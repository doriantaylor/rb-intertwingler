require 'intertwingler/handler'

require 'rack/mock_request' # for env_for

# This is the engine. This is the thing that is run.
class Intertwingler::Engine < Intertwingler::Handler

  class Dispatcher
    def self.configure engine
    end

    def initialize engine, handlers
    end

    # find a handler for the request and return the response
    def dispatch req
    end

  end

  private

  # XXX this is wack af
  WRAPPER = eval 'Rack::Lint::Wrapper::InputWrapper' rescue nil
  if WRAPPER and not WRAPPER.method_defined? :set_encoding
    WRAPPER.define_method :set_encoding do |encoding, opt = nil|
      @input.set_encoding encoding, opt if @input.respond_to? :set_encoding
      self
    end
  end

  ITCV = Intertwingler::Vocab::ITCV

  public

  # Find all the subjects in the graph that are an `itcv:Engine`.
  #
  # @param repo [RDF::Queryable, Intertwingler::GraphOps] the repository.
  #
  # @return [Array] all engine instances.
  #
  def self.locate repo
    repo.all_of_type ITCV.Engine
  end

  # Resolve the engine and all of its handlers and transforms and
  # queues and such out of the graph.
  def self.configure repo, subject, resolvers: []
    # step 1: find the instance of itcv:Engine that has the same base
    # URI as the resolvers and initialize.
    me = self.new resolvers, subject: subject

    # step 2: find the handlers and load them.
    me.refresh_handlers

    # step 3: construct the transform queues and their contents. (note
    # the queues are apt to reuse transforms, and the transform
    # handler instances could very well already be in the handler
    # stack.)
    me.refresh_queues
  end

  # Refresh the handler stack associated with this engine.
  #
  # @return [self]
  #
  def refresh_handlers
    list = resolver.repo.objects_for(subject,
      ITCV['handler-list'], only: :resource).first
    unless list
      self
    end
  end

  # Refresh the transform queues associated with this engine.
  #
  # @return [self]
  #
  def refresh_queues

    self
  end

  # Initialize the engine.
  #
  # @param resolvers [Array<Intertwingler::Resolver>] the necessary resolvers.
  #
  def initialize resolvers, subject: nil, handlers: [], transforms: {}
    # ensure resolvers are an array
    resolvers = resolvers.respond_to?(:to_a) ? resolvers.to_a : [resolvers]
    # set authority map
    @authorities = (@resolvers = resolvers).reduce({}) do |h, r|
      r.authorities.each { |a| h[a] = r }
      h
    end

    # XXX here is where we would have each of the handlers disgorge
    # its manifest so we don't have to poll them all with each request
    handlers  = handlers.respond_to?(:to_a) ? handlers.to_a : [handlers]
    @handlers = handlers.map do |handler|
      handler, args = (handler.respond_to?(:to_a) ? handler.to_a : [handler])
      args ||= {}
      handler.new self, **args
    end

    # create transform harness from handlers
  end

  attr_reader :resolvers

  # No-op to overwrite `engine` member.
  #
  # @return [self] this _is_ the engine.
  #
  def engine; self; end

  # Get the {Intertwingler::Resolver} for the given request.
  #
  # @param req [Rack::Request, URI, RDF::URI] the request (URI).
  #
  # @return [Intertwingler::Resolver, nil] the resolver, maybe
  #
  def resolver_for req
    req = RDF::URI(req.url) if req.respond_to? :url
    @authorities[req.authority.downcase] if req.respond_to? :authority
  end

  # Get the resolver's graph for the given request.
  #
  # @param req [Rack::Request, URI, RDF::URI] the request (URI).
  #
  # @return [RDF::Repository] the graph.
  #
  def repo_for req
    resolver = resolver_for(req) or return
    resolver.repo
  end

  # Using the current request as a basis, fake up a new request with
  # the given URI.
  #
  # @param req [Rack::Request] the current request.
  # @param uri [URI, RDF::URI] the new URI
  # @param method [Symbol, #to_sym] the request method (GET)
  # @param headers [Hash, #to_h] overriding request headers
  # @param body [#each, #call] a new body
  #
  # @return [Rack::Request] a new request
  #
  def dup_request req, uri, method: nil, headers: {}, body: nil
    # coerce the URI just so we can flatten it again so we can parse again
    uri = Intertwingler::Resolver.coerce_resource uri, as: :uri

    # override the method (maybe)
    method ||= req.request_method
    # same deal with with the body
    body ||= req.env['rack.input']

    # fake up an environment
    env = req.env.merge Rack::MockRequest.env_for uri.to_s,
      method: method.to_s.strip.upcase, script_name: req.script_name

    # bored with the discussion happening in
    # https://github.com/rack/rack/pull/2115 so just gonna do this
    body.set_encoding(Encoding::BINARY) if body.respond_to? :set_encoding
    env['rack.input'] = body

    # supplant rack.errors which will be wrong
    env['rack.errors'] = req.env['rack.errors']

    # correct (non-standard??) REQUEST_URI which will also be wrong if it exists
    env['REQUEST_URI'] = uri.request_uri.b if env.key? 'REQUEST_URI'

    # now overwrite the headers
    headers.each do |hdr, val|
      hdr = hdr.to_s.strip.upcase.tr_s(?-, ?_)
      hdr = "HTTP_#{hdr}" unless hdr.start_with? 'HTTP_'
      val = val.join ', ' if val.is_a? Array
      env[hdr] = val
    end

    # et voilà
    Rack::Request.new env
  end

  def replace_response_body resp, body
    # why oh why no body=
    Rack::Response[resp.status, resp.headers, body]
  end

  # Fake up a request and run the main handler. Returns the
  # (potentially wrapped) body. Will throw an error if the response is
  # anything but `200 OK`.
  def fetch req, uri: nil, method: :GET, headers: {}, body: nil
    # start with a new request when there is a different URI
    req = dup_request req, uri, method: method,
      headers: headers, body: body if uri

    # XXX actually do the thing
  end

  # This is the master handler that runs the engine and marshals all
  # other handlers and transforms.
  #
  # @param req [Rack::Request]
  #
  # @return [Rack::Response]
  #
  def handle req
    # cache the original request
    orig = req

    # XXX handle OPTIONS *

    # get the uri as given (hostname/authority may be an alias)
    uri = URI(req.url)

    # implicit 404 if we can't locate the resolver
    resolver = resolver_for(uri) or
      return Rack::Response[404, {}, ['not found lol']]

    # split out the path parameters
    uri, *pp = resolver.split_pp uri, parse: true

    # cut a per-request instance of the transform harness
    # transforms = @transform_harness.dup

    # this would actually be a 404 if it couldn't resolve one of them.
    # blow up unless transforms.construct_queue(:addressable, pp)

    # XXX TODO normalize query parameters (à la Params::Registry)

    # resolve URI and mint a new request if necessary
    if subject = resolver.uuid_for(uri, as: :uri)
      uri = URI(req.base_url) + subject.uuid
      req = dup_request orig, uri
    end

    # run all request transforms

    # req = transforms.run_queue :request, req

    # transforms can signal that they manipulate the request wholesale
    # by only accepting and returning message/http, meaning that if
    # they accept/return anything *else*, we know only to send the
    # body / manipulate the request/response accordingly.

    # XXX TODO

    # always assume the worst, then you can only be pleasantly surprised.
    resp = Rack::Response[404, {}, []]

    # poll content handlers until something returns non-404/405
    begin
      # XXX this should be replaced with the manifest system
      @handlers.each do |h|
        resp = h.handle req

        break unless [404, 405].include? resp.status
      end

    rescue Intertwingler::Handler::AnyButSuccess => e
      resp = e.response
    rescue Exception => e
      resp = Rack::Response[500,
                            { 'content-type' => 'text/plain' }, [e.message]]
    end

    # run all response transforms

    # early-run transforms

    # resp = transforms.run_queue :early, resp

    # resp = transforms.run_queue :addressable, resp

    # So here is a situation: I want to make it so direct requests to
    # the content-addressable store are not transformed (except
    # optionally by addressable transforms). These are easy enough to
    # identify via `/.well-known/ni/{algo}/{hash}` URIs (plus I repeat
    # the `ni:` URI in the etag). One solution would be an early-run
    # response transform that if successful clears the current queue
    # *and* the late-run queue. We already know we need to *add*
    # things to queues; we should also be able to *remove* things from
    # queues, as well as empty them completely.
    #
    # Another solution would be to *construct* queues-of-queues on the
    # fly (which is already being done with the addressable
    # transforms), and you start off with a single queue which is
    # empty except for this one test, and if the test is successful
    # (or rather, the test is *negative*), *then* it switches tracks
    # to the ordinary queue of queues. I am less sanguine about this
    # one; I think what I want is to be able to designate three of the
    # four queues (request and the early-run/late-run response queues)
    # by URI and then assign them. The addressable response queue will
    # always need to be constructed on the fly (indeed piece by piece)
    # because there will be identifiers in the path parameters
    # potentially associated with more than one transform (e.g.
    # analogous operations for different content types). So while it's
    # a good idea to content-negotiate *all* the transforms, we need
    # to do the addressable ones in particular, run each one, see what
    # type it outputs, then see what the response type is before
    # content-negotiating the next one.
    #
    # While the ordinary non-addressable queues can just ignore any
    # transforms whose input/output specs don't match the payload
    # and/or requestor's Accept: header, an addressable queue *has* to
    # attempt to run its entire contents. It also has to maintain the
    # explicit sequence in which it was enqueued (while the other ones
    # can get by with a quasi-topological sort). This means that any
    # non-matching transform in the addressable queue can blow up the
    # response (either with a 406 or 415 internally which should be
    # translated into a 409 for public consumption).

    # addressable transforms
    pp.each do |x|
      warn x.inspect
    end

    # late-run transforms

    # return the response
    resp
  end

end
