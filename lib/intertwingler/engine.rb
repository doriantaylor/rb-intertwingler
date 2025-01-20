require 'intertwingler/handler'
require 'intertwingler/params'
require 'intertwingler/transform'
require 'intertwingler/util/clean'
require 'intertwingler/error'
require 'intertwingler/loggable'

require 'params/registry'

require 'rack/mock_request' # for env_for

# This is the engine. This is the thing that is run.
class Intertwingler::Engine < Intertwingler::Handler
  include Intertwingler::Loggable

  ITCV = Intertwingler::Vocab::ITCV
  TFO  = Intertwingler::Vocab::TFO

  # The dispatcher has one job, which is to dispatch an incoming
  # request to the correct handler.
  class Dispatcher
    include Intertwingler::GraphOps::Addressable

    private

    # https://www.iana.org/assignments/http-methods/http-methods.xhtml
    HTTP = %i[ACL BASELINE-CONTROL BIND CHECKIN CHECKOUT CONNECT COPY DELETE
              GET HEAD LABEL LINK LOCK MERGE MKACTIVITY MKCALENDAR MKCOL
              MKREDIRECTREF MKWORKSPACE MOVE OPTIONS ORDERPATCH PATCH POST
              PRI PROPFIND PROPPATCH PUT REBIND REPORT SEARCH TRACE UNBIND
              UNCHECKOUT UNLINK UNLOCK UPDATE UPDATEREDIRECTREF VERSION-CONTROL]

    def repo ; engine.repo ; end
    def subject ; engine.subject; end

    # It turns out that HTTP request methods are case-_sensitive_, but
    # the standard ones just happen to be all uppercase.
    def coerce_http_method arg
      unless arg.nil?
        arg = arg.to_s
        arg = arg.upcase if HTTP.include? arg.upcase.to_sym
        arg = arg.to_sym
      end

      arg
    end

    # Load a handler (if not already loaded).
    #
    # @param uri [URI, RDF::URI] the handler or instance URI
    # @param chdir [false, true] whether to change directories to the
    #  engine home.
    #
    # @raise [Intertwingler::Error::Config] when the URI represents
    #  something other than an {Intertwingler::Handler} subclass or
    #  invocation thereof.
    #
    # @return [nil, Intertwingler::Handler] `nil` if the URI does not
    #  resolve, {Intertwingler::Handler} instance otherwise.
    #
    def load_handler uri, chdir: false
      resolver = engine.resolver
      subject  = resolver.coerce_resource uri, as: :rdf
      uri      = URI(subject.to_s)

      # already cached, gitouttahere
      return @handlers[uri] if @handlers[uri]

      repo  = engine.repo
      types = repo.types_for subject # get these just the once

      # either this is an itcv:Handler or an itcv:Instance. if it's a
      # handler it must be a `urn:x-ruby:`; if it's an instance it
      # doesn't matter what it is BUT if it is a ruby URN it should
      # get its parameters merged too.
      #
      # if the subject is an itcv:Instance:
      if repo.type_is? types, ITCV.Instance
        # * find the `itcv:of` (this must be an itcv:Handler and
        #   `urn:x-ruby:`)
        handler = repo.objects_for(subject, ITCV.of, only: :resource).sort.first
        # * from there, find all tfo:parameter statements
        preds = repo.objects_for handler, TFO.parameter, only: :resource
        # * parse the associated parameter specs XXX MAYBE LATER LOL
        # * go back and harvest/parse the parameters from the subject
        # * merge with any parameters in the URN

        params = preds.map do |pred|
          # either we get it from the label
          if key = repo.objects_for(
            pred, RDF::Vocab::DC11.identifier,
            only: :literal, datatype: RDF::XSD.token).first
            key = key.last.object
          elsif pred.to_s.downcase.start_with? 'urn:x-ruby:'
            key = URI(pred.to_s).identifier
          else
            key = pred.to_s # uh i guess?
          end

          # we want to separate out the list designations vs the scalars
          lists   = []
          scalars = []
          repo.objects_for(subject, pred).each do |o|
            # check if this is a list; coerce if so
            if list = repo.as_list(o)
              lists << list.to_a.map do |x|
                x.literal? ? x.object : x.uri? ? URI(x.to_s) : x.to_s
              end
            else
              scalars << (o.literal? ? o.object : o.uri? ? URI(o.to_s) : o.to_s)
            end
          end

          # only return something
          unless lists.empty? and scalars.empty?
            val = if lists.empty?
                    scalars.length == 1 ? scalars.first : scalars
                  else
                    lists.sort { |a, b| b.length <=> a.length }.first
                  end

            [key.to_sym, val]
          end
        end.compact.to_h

        # reassign the handler
        handler = URI(handler.to_s)
        # merge in any parameters
        params = (handler.is_a?(Intertwingler::RubyURN) ?
                  handler.q_component_hash : {}).merge params
        # load the class from the handler
        cls = handler.object
      elsif repo.type_is? types, ITCV.Handler
        #
        # otherwise:
        # * do what's already here
        #
        params = uri.q_component_hash
        # load t
        cls = uri.object
      else
        raise Intertwingler::Error::Config,
          "#{subject} is neither Handler nor Instance (#{types.join(', ')})"
      end

      # warn params.inspect

      raise Intertwingler::Error::Config,
        "#{cls} is not a subclass of Intertwingler::Handler" unless
        cls.is_a? Class and cls.ancestors.include? Intertwingler::Handler
                  # get the initialization params from the URN

      begin
        if chdir
          oldpwd = Pathname.getwd
          Dir.chdir engine.home
        end

        @handlers[uri] = cls.new(engine, **params)
      ensure
        Dir.chdir oldpwd if chdir
      end

      @handlers[uri]
    end

    public

    # Configure the dispatcher out of the graph.
    #
    # @param engine [Intertwingler::Engine]
    #
    # @return [Intertwingler::Engine::Dispatcher]
    #
    def self.configure engine
      new(engine).refresh
    end

    # Give us a new (potentially empty) dispatcher.
    #
    # @param engine [Intertwingler::Engine]
    # @param handlers [Array<RDF::URI, Intertwingler::RubyURN>]
    #
    def initialize engine, handlers: []
      @engine     = engine
      @handlers   = {} # this is just urn:x-ruby -> handler instance
      @routes     = {} # this is uri -> method -> handler (or other way i dunno)
      @transforms = Intertwingler::Transform::Harness.new self

      handlers.each { |handler| add handler }
    end

    attr_reader :engine, :transforms

    def refresh
      # give us the list of handlers or otherwise sort any directly-attached
      list = blanks(ITCV['handler-list']).sort.first
      list = list ? RDF::List.new(subject: list, graph: repo).to_a : []
      list += resources(ITCV.handler).sort

      add(*list)

      # the transforms are encapsulated
      @transforms.refresh

      self
    end

    # Clear out the handlers and routes.
    #
    # @return [self]
    #
    def clear
      @handlers.clear
      @routes.clear

      self
    end

    private

    # Set up the route but don't process any of the input and don't
    # try to load the handler so we don't recurse.
    #
    # @param path [String] URI path
    # @param urn [Intertwingler::RubyURN] the handler URN
    # @param handler [Intertwingler::Handler] the handler instance
    # @param method [nil, Symbol, Array<nil, Symbol>] the request method(s)
    #
    # @return
    #
    def add_route_internal path, urn, handler, method
      # There is furthermore the issue of request methods. We're
      # putting the request method in the second rung of the data
      # structure rather than the first because they're symbols and
      # they're small, whereas URIs are relatively big, and we would
      # likely have a copy for each request method. The spec also
      # requires that any resource that answers to GET should also
      # answer to HEAD, and we also have to account for unspecified
      # methods which we will just assign to `nil`.
      #
      # Note as well methods are case *sensitive* (cf RFC9110 §9.1),
      # however the standard methods are all uppercase.
      method = method.respond_to?(:to_a) ? method.to_a : [method]
      method = method.map { |m| coerce_http_method m }
      method += %i[GET HEAD] unless (method & %i[GET HEAD]).empty?
      # (nil stands for all methods so specific methods are redundant)
      method = [nil] if method.empty? or method.include? nil
      method.uniq!

      # now add a mapping for each request method
      route = @routes[path] ||= {}
      method.each { |m| (route[m] ||= {})[urn] = handler }

      # might as well return the handler instance
      handler
    end

    public

    # Associate a specific URI and set of request methods with a
    # handler instance.
    #
    # @note This method is in lieu of the handler manifest
    #  infrastructure, so we can map routes directly to transform
    #  handlers without having to poll the whole list of them.
    #
    # @param uri [URI, RDF::URI] the target URI (inside the handler)
    # @param handler [URI, RDF::URI] the handler URI
    # @param method [Symbol, Array<Symbol>, nil] request methods, if
    #  applicable
    #
    def add_route uri, handler, method: nil
      # really thinking the resolver should have something like
      # "durable" vs "dereferenceable" rather than just `uri_for` and
      # `uuid_for`, mainly so we can handle durable URIs other than
      # UUIDs.
      resolver = engine.resolver
      uri      = resolver.coerce_resource uri,     as: :uri
      handler  = resolver.coerce_resource handler, as: :uri

      # okay so here is a potential issue: which URI do we put in the
      # key? if we want this to have the effect that URL paths work
      # like people expect them to (ie entire subtrees mapped to
      # a given handler), then we're going to have to iron this out.
      #
      # My instinct is to put the UUID where possible, but problem is
      # "containers" also get assigned UUIDs, and their container-ness
      # is not clearly defined. Plus there is no reason why we
      # shouldn't be able to have one handler handle a URI path that
      # is "under" a path handled by a different handler.
      #
      # Note as well that UUID URNs get translated to the single path
      # segment `/<uuid>` if they don't have a human-readable overlay.
      # _Also_ note that ci:canonical overlay URIs can terminate with
      # a slash even though the resource is not a "container" per se.
      #
      # The other thing to consider is that a handler matching a route
      # is not dispositive. It's just a _candidate_. It may 404 or
      # 405, and then the dispatcher is supposed to try the next-best
      # handler (which indeed may be in the same route).

      path = if uri.respond_to? :uuid
               uri.uuid.downcase.freeze
             elsif uri.path
               uri, _ = resolver.split_pp uri
               uri.path.delete_prefix(?/).freeze
             else
               # XXX not sure about this one
               uri.to_s.freeze
             end

      # get the handler instance (or die trying)
      instance = load_handler handler, chdir: true

      add_route_internal path, handler, instance, method
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
    # @note Not sure if this method is going to stick around since I
    #  wrote it before I started thinking about more complex routing.
    #
    # @note 2024-02-12 yeah I don't like this method anymore.
    #
    # @param urns [Array<Intertwingler::RubyURN, RDF::URI, #to_s>]
    #  anything coercible into an {Intertwingler::RubyURN}.
    # @param queues [true, false] also register any transformation
    #  queues associated with a given handler.
    #
    # @return [self]
    #
    def add *urns, path: nil, queues: true
      resolver = engine.resolver
      home     = engine.home
      oldpwd   = Pathname.getwd

      begin
        # mkay first change directories
        Dir.chdir home

        # now do the thing
        urns.each do |urn|
          # XXX all this might be dumb
          urn = resolver.coerce_resource urn, as: :uri
          handler = load_handler(urn) or next
          # handlers added this way (XXX are we just making a mess?)
          add_route_internal '', urn, handler, nil

          # run this here unconditionally
          if queues
            queue = @engine.repo.objects_for(RDF::URI(urn.to_s),
              Intertwingler::Vocab::ITCV.queue, only: :resource).sort.first
            # warn queue
            # transforms.register? queue if queue
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
    # @param subrequest [false, true] if true, skip the transforms.
    #
    # @return [Rack::Response] the response.
    #
    def dispatch req, subrequest: false
      resolver = engine.resolver
      method   = req.request_method.to_sym # we always want this as a symbol
      uri, *pp = resolver.split_pp req.url # split out the path parameters
      uri      = resolver.uri_for uri, as: :uri # canonicalize the uri
      uuid     = engine.resolver.uuid_for uri, as: :uri # also get the uuid

      # default response
      resp = Rack::Response[404,
        { 'content-type' => 'text/plain' }, ['legit nothing bro']]

      # strategy:
      #
      # * explicit configuration (uri -> method -> handler):
      #   * terminal (ie exact match)
      #   * container (ie path prefix match)
      # * empirical cache (de facto terminal paths)
      # * poll the handlers (that haven't already been attempted)

      # check the empirical cache XXX MAYBE LATER LOL
      # if a response from a cached entry returns 404/405 then remove it

      # build up a list of paths to check. this will do explicit
      # checks on uuids and paths and then shorter and shorter paths
      # until the empty string
      paths = []
      paths << uuid.uuid if uuid
      if uri.path
        path = uri.path.sub(/^\/+/, '') # XXX do we really wanna do this?
        paths << path # as-is
        # minus leading and trailing slashes
        ps = path.split(/\/+/)
        until ps.empty?
          ps.pop
          paths << (ps + ['']).join(?/)
        end
      end

      # warn paths.inspect

      # now build up a list of candidate handlers. first we try the
      # full stack of paths with the actual request method, then we
      # try the same with the wildcard (nil).
      candidates = []
      [method, nil].each do |m|
        paths.each do |a|
          h = @routes.fetch(a, {})[m] or next
          h.each do |pair|
            candidates << pair unless candidates.include? pair
          end
        end
      end

      # warn candidates.map(&:first).inspect

      # an empty list of handlers means nothing to see here
      return resp if candidates.empty?

      warn "dispatcher #{subrequest ? '(subrequest) ' : ''}sees content type: #{req.content_type.inspect}"

      # the transform harness may return an empty chain; that's fine
      unless subrequest
        chain = transforms.request_chain
        req   = chain.run req
      end

      hurn = nil

      candidates.each do |urn, handler|
        hurn = urn

        begin
          # we're adding a smidge of logic here to not supplant a 405 with a 404
          tmp  = handler.handle req
          resp = tmp unless resp.status == 405 and tmp.status == 404
        rescue Intertwingler::Handler::AnyButSuccess => e
          resp = e.response
        rescue => e
          # quit now in case this blows up
          # XXX do something smarter here
          return Intertwingler::Handler::Error::Server.new(
            e.message + e.backtrace.join("\n")).response
        end

        # All response codes besides 404 and 405 are considered
        # authoritative. (XXX a 404 followed by a 405 followed by a
        # 404 will be represented as a 404, rather than 405, though
        # a 405 kinda tells you more than a 404 does.)
        break unless [404, 405].include? resp.status
      end

      unless subrequest
        # generate the response chain with addressable queue
        chain = chain.response_chain hurn, pp: pp
        resp  = chain.run req, resp
      end

      resp
    end

  end # END Dispatcher

  private

  # XXX this is wack af
  WRAPPER = eval 'Rack::Lint::Wrapper::InputWrapper' rescue nil
  if WRAPPER and not WRAPPER.method_defined? :set_encoding
    WRAPPER.define_method :set_encoding do |encoding, opt = nil|
      @input.set_encoding encoding, opt if @input.respond_to? :set_encoding
      self
    end
  end

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
  def initialize repo: nil, subject: nil, resolver: nil, home: nil, log: nil
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
    @log  = log || resolver.log

    @registry   = Intertwingler::Params.configure self
    @dispatcher = Dispatcher.new self

    # step 2: find the handlers and load them. (incidentally, this
    # returns `self`.)
    # step 3: construct the transform queues and their contents. (note
    # the queues are apt to reuse transforms, and the transform
    # handler instances could very well already be in the handler
    # stack.) NB this also returns `self`.

    # XXX i don't actually want to refresh in the constructor
    @dispatcher.refresh
  end

  attr_reader :subject, :resolver, :repo, :home,
    :dispatcher, :transforms, :registry, :log
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

    log.debug headers.inspect

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

    # XXX handle OPTIONS *

    # cut a per-request instance of the transform harness
    # transforms = @transform_harness.dup

    # this would actually be a 404 if it couldn't resolve one of them.
    # blow up unless transforms.construct_queue(:addressable, pp)

    # XXX TODO normalize query parameters (à la Params::Registry)

    # transforms can signal that they manipulate the request wholesale
    # by only accepting and returning message/http, meaning that if
    # they accept/return anything *else*, we know only to send the
    # body / manipulate the request/response accordingly.

    # always assume the worst, then you can only be pleasantly surprised.
    resp = Rack::Response[404, {}, []]

    begin
      # this should tell the transform harness which handler was used
      # and therefore if it has an initial response queue that differs
      # from the default
      resp = dispatcher.dispatch req

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
