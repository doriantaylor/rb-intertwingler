require 'intertwingler/handler'
require 'intertwingler/transform'
require 'intertwingler/util/clean'
require 'intertwingler/error'

require 'params/registry'

require 'rack/mock_request' # for env_for

# This is the engine. This is the thing that is run.
class Intertwingler::Engine < Intertwingler::Handler

  # The dispatcher has one job, which is to dispatch an incoming
  # request to the correct handler.
  class Dispatcher

    # Give us a new dispatcher.
    #
    # @param engine [Intertwingler::Engine]
    # @param urns [Array<Intertwingler::RubyURN>]
    #
    def initialize engine, *urns
      @engine   = engine
      @handlers = {}
      @queues   = {}

      add(*urns)
    end

    attr_reader :engine

    # Clear out the handlers.
    #
    # @return [self]
    #
    def clear
      @handlers.clear

      self
    end

    # Add handlers by `urn:x-ruby:` identifier. These identifiers are
    # expected to represent references to {Intertwingler::Handler}
    # subclasses and any initialization parameters. This method
    # attempts to change directory to {Intertwingler::Engine#home}
    # before initializing the handlers, so that if any parameters
    # contain file system paths, they are resolved relative to it.
    #
    # @note The original working directory is unconditionally restored
    #  before the method completes.
    #
    # @param urns [Array<Intertwingler::RubyURN, RDF::URI, #to_s>]
    #  anything coercible into an {Intertwingler::RubyURN}.
    #
    # @return [self]
    #
    def add *urns
      resolver = @engine.resolver
      home     = @engine.home
      oldpwd   = Pathname.getwd

      begin
        # mkay first change directories
        Dir.chdir home

        # now do the thing
        urns.each do |urn|
          urn = resolver.coerce_resource urn, as: :uri
          # only attempt to load the right scheme
          next unless urn.is_a? Intertwingler::RubyURN

          unless @handlers[urn]
            cls = urn.object
            raise Intertwingler::Error::Config,
              "#{cls} is not a subclass of Intertwingler::Handler" unless
              cls.is_a? Class and cls.ancestors.include? Intertwingler::Handler

            params = urn.q_component_hash

            # XXX in here is where we would do the manifest stuff
            @handlers[urn] = cls.new(engine, **params)
          end
        end
      ensure
        # mkay change directories back
        Dir.chdir oldpwd
      end

      # why the hell not
      self
    end

    # Find the appropriate handler for the request and return the response.
    #
    # @param req [Rack::Request] The request to which the response is
    #  dispatched.
    # @param transforms [Intertwingler::Transform::Harness] A
    #  duplicated transform harness scoped to the request.
    # @param run [false, true] whether to run the response transforms.
    #
    # @return [Rack::Response]
    #
    def dispatch req, transforms: nil, run: false
      # we always want this as a symbol
      method = req.request_method.to_sym
      # uuid = engine.resolver.uuid_for

      # strategy:
      #
      # * explicit configuration (uri -> method -> handler):
      #   * terminal (ie exact match)
      #   * container (ie path prefix match)
      # * empirical cache (de facto terminal paths)
      # * poll the handlers (that haven't already been attempted)

      # check the empirical cache

      # if a response from a cached entry returns 404/405 then remove it

      # XXX right now we just iterate through the handlers until one
      # returns something other than 404 or 405
      @handlers.each do |urn, handler|
        begin
          resp = handler.handle req
        rescue => e
          # quit now in case this blows up
          # XXX do something smarter here
          return Rack::Response[500, {}, [e.message]]
        end

        # any other statusc and the handler is considered to have 'handled'
        next if [404, 405].include? resp.status

        if transforms
          transforms.response_head = handler.queue if handler.queue
          return run ? transforms.run_response(req, resp) : resp
        else
          return resp
        end
      end

      # default non-response
      Rack::Response[404, {}, []]
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
  def self.configure repo: nil, subject: nil, resolver: nil,
      authority: nil, home: nil
    # you either need a resolver, or a repo + { subject or authority }

    if resolver
      self.new resolver: resolver, home: home
    elsif repo
      if subject
        self.new repo: repo, subject: subject, home: home
      elsif authority
        resolver = Intertwingler::Resolver.configure repo, authority: authority
        self.new resolver: resolver, home: home
      else
        raise Intertwingler::Error::Config,
          'A repository must be accompanied by an authority or a subject URI.'
      end
    else
      raise Intertwingler::Error::Config,
        'Configuration requires either a resolver, or a repository plus ' +
        'either an authority or a subject URI.'
    end
  end

  # Refresh the handler stack associated with this engine.
  #
  # @return [self]
  #
  def refresh_handlers

    # get the list of handlers
    list = @repo.objects_for(subject,
      ITCV['handler-list'], only: :resource).sort.first
    list = list ? RDF::List.new(subject: list, graph: @repo).to_a : []
    list += @repo.objects_for(@subject, ITCV.handler, only: :resource).sort

    # wipe out the contents of the dispatcher first
    @dispatcher.clear.add(*list)

    self
  end

  # Refresh the transform queues associated with this engine.
  #
  # @return [self]
  #
  def refresh_transforms

    self
  end

  private

  def subject_from_resolver resolver
    subjects = resolver.repo.subjects_for(
      ITCV.resolver, resolver.subject, only: :resource)
    case subjects.count
    when 0 then raise Intertwingler::Error::Config,
        "No engine found associated with resolver #{resolver.subject}"
    when 1 then return subjects.first
    else raise Intertwingler::Error::Config,
        'Multiple engines found associated with %s: %s' %
        [resolver.subject, subjects.join(', ')]
    end
  end

  def resolver_from_subject
    resolvers = @repo.objects_for(@subject, ITCV.resolver, only: :resource)
    case resolvers.count
    when 0 then raise Intertwingler::Error::Config,
        "No resolvers associated with engine #{@subject}."
    when 1 then return Intertwingler::Resolver.configure @repo,
        subject: resolvers.first
    else raise Intertwingler::Error::Config, 'Multiple resolvers for %s: %s' %
        [@subject, resolvers.join(', ')]
    end
  end

  public

  # Initialize the engine. You _must_ pass in _either_ a `subject`
  # _and_ a `repo` parameter, (from which a resolver will be derived),
  # _or_ a `resolver` parameter (which contains a subject and a
  # repository), but not both. The `home` parameter will default to
  # the current working directory if not otherwise specified.
  #
  # @param repo [Intertwingler::GraphOps] The RDF repository.
  # @param subject [RDF::URI, URI, #to_s] a URI or string suitable as
  #  a subject.
  # @param resolver [Intertwingler::Resolver] the associated resolver,
  #  if present.
  # @param home [Pathname, #to_s] a home directory for the engine from
  #  which relative file system paths are resolved. Uses the current
  #  working directory by default.
  #
  def initialize repo: nil, subject: nil, resolver: nil, home: nil
    # step 1: the basics
    if resolver
      @resolver = resolver
      @repo     = resolver.repo
      @subject  = subject_from_resolver resolver
    elsif repo && subject
      @repo     = repo
      @subject  = Intertwingler::Resolver.coerce_resource subject
      @resolver = resolver_from_subject
    else
      raise Intertwingler::Error::Config,
        'Must initialize with either a resolver or a repository and subject.'
    end

    @home = Pathname(home.to_s).expand_path

    @registry   = Intertwingler::Params.new self
    @dispatcher = Dispatcher.new self
    @transforms = Intertwingler::Transform::Harness.new self

    # step 2: find the handlers and load them. (incidentally, this
    # returns `self`.)
    refresh_handlers

    # step 3: construct the transform queues and their contents. (note
    # the queues are apt to reuse transforms, and the transform
    # handler instances could very well already be in the handler
    # stack.) NB this also returns `self`.
    refresh_transforms
  end

  attr_reader :subject, :resolver, :repo, :home,
    :dispatcher, :transforms, :registry
  alias_method :id, :subject

  # No-op to overwrite `engine` member.
  #
  # @return [self] this _is_ the engine.
  #
  def engine; self; end

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
  def dup_request req, uri: nil, method: nil, headers: {}, body: nil
    # coerce the URI just so we can flatten it again so we can parse again
    uri = Intertwingler::Resolver.coerce_resource(uri || req.url, as: :uri)

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
      hdr = "HTTP_#{hdr}" unless /^(?:HTTP_|(?:CONTENT_(?:LENGTH|TYPE)))$/ =~ hdr
      val = val.join ', ' if val.is_a? Array
      env[hdr] = val
    end

    # et voilà
    Rack::Request.new env
  end

  # Because {Rack::Response} has no `#body=` method, this does what it
  # says on the tin: it returns a new {Rack::Response} with the
  # supplied body, and any replacement headers.
  #
  # @param resp [Rack::Response] the response in question
  # @param body [IO, #call] a suitable body, see Rack spec document.
  # @param headers [Hash] a hash suitable to be a header set; see Rack
  #  spec.
  #
  # @return [Rack::Response] the new response.
  #
  def replace_response_body resp, body, headers: {}

    headers = resp.headers.merge headers

    # why oh why no body=
    Rack::Response[resp.status, headers, body]
  end

  # Fake up a request and run the main handler. Returns the
  # (potentially wrapped) body. Will throw an error if the response is
  # anything but `200 OK`.
  def fetch req, uri: nil, method: :GET, headers: {}, body: nil
    # start with a new request when there is a different URI
    req = dup_request req, uri: uri, method: method,
      headers: headers, body: body if uri

    resp = dispatcher.dispatch req

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
      req = dup_request orig, uri: uri
    end

    # duplicate transforms so state changes get wiped after the request
    transforms = transforms.dup

    # transforms can signal that they manipulate the request wholesale
    # by only accepting and returning message/http, meaning that if
    # they accept/return anything *else*, we know only to send the
    # body / manipulate the request/response accordingly.

    # always assume the worst, then you can only be pleasantly surprised.
    resp = Rack::Response[404, {}, []]

    begin
      # set the addressable queue
      transforms.set_addressable(*pp) # XXX this may raise

      # run the request queue
      req = transforms.run_request req

      # this should tell the transform harness which handler was used
      # and therefore if it has an initial response queue that differs
      # from the default
      resp = dispatcher.dispatch req, transforms: transforms, run: true

      # this can do all sorts of things; it can blow up, it can redirect…
    rescue Intertwingler::Handler::AnyButSuccess => e
      resp = e.response
    # rescue StandardError => e
    #   resp = Rack::Response[500,
    #     { 'content-type' => 'text/plain' }, [e.inspect]]
    end

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

    # return the response
    resp
  end

end
