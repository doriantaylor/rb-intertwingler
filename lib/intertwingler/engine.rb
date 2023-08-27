require 'intertwingler/handler'

require 'rack/mock_request' # for env_for

# This is the engine. This is the thing that is run.
class Intertwingler::Engine < Intertwingler::Handler

  private

  # XXX all this is lame af
  WRAPPER = eval 'Rack::Lint::Wrapper::InputWrapper' rescue nil
  if WRAPPER and not WRAPPER.method_defined? :set_encoding
    WRAPPER.define_method :set_encoding do |encoding, opt = nil|
      @input.set_encoding encoding, opt if @input.respond_to? :set_encoding
      self
    end
  end

  public

  # Initialize the engine.
  #
  # @param resolvers [Array<Intertwingler::Resolver>] the necessary resolvers.
  #
  def initialize resolvers, handlers: [], transforms: {}
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
      method: method.to_s.strip.upcase, input: body

    # now overwrite the headers
    headers.each do |hdr, val|
      hdr = hdr.to_s.strip.upcase.tr_s(?-, ?_)
      hdr = "HTTP_#{hdr}" unless hdr.start_with? 'HTTP_'
      val = val.join ', ' if val.is_a? Array
      env[hdr] = val
    end

    Rack::Request.new env
  end

  def replace_resp_body resp, body
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

    # XXX TODO normalize query parameters (à la Params::Registry)

    # resolve URI and mint a new request if necessary
    if subject = resolver.uuid_for(uri, as: :uri)
      uri = URI(req.base_url) + subject.uuid
      req = dup_request orig, uri
    end

    # run all request transforms

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
    rescue Intertwingler::Handler::Unsuccessful => e
      resp = e.response
    rescue Exception => e
      resp = Rack::Response[500,
        { 'content-type' => 'text/plain' }, [e.message]]
    end

    # run all response transforms

    # early-run transforms

    # addressable transforms
    pp.each do |x|
      warn x.inspect
    end

    # late-run transforms

    # return the response
    resp
  end
end
