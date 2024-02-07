require 'rdf'
require 'rdf/vocab'
require 'intertwingler/vocab/tfo'
require 'intertwingler/graphops'
require 'intertwingler/util'
require 'set'
require 'mimemagic'
require 'http/negotiate'
require 'time'

require 'intertwingler/handler'
require 'http/negotiate'

# This class represents the metadata about an individual service
# endpoint for a transformation function, and registers its
# parameters. It is meant to be congruent with the
# [`tfo:Function`](https://vocab.methodandstructure.com/transformation#Function)
# class in the [Transformation Functions
# Ontology](https://vocab.methodandstructure.com/transformation#).
class Intertwingler::Transform
  include Intertwingler::GraphOps::Addressable

  private

  TFO = Intertwingler::Vocab::TFO
  CI  = Intertwingler::Vocab::CI
  OWL = RDF::OWL

  def repo; harness.resolver.repo; end

  def coerce_params params
    Intertwingler::Params.validate(params || {})
  end

  def coerce_types types
    types ||= []
    types = [types] unless types.is_a? Array
    types.map { |t| t.is_a?(MimeMagic) ? t : MimeMagic[t.to_s] }
  end

  def gather_accepts_returns raw: false, predicate: TFO.accepts
    literals = []
    lists    = []

    objects(predicate).each do |o|
      if o.literal?
        literals << o
      elsif o.blank?
        lists << RDF::List.new(graph: repo, subject: o).to_a
      end
    end

    # this is mainly to give us consistent results
    out = (lists.sort.flatten + literals.sort).uniq
    # raw as in raw literals
    raw ? out : out.map { |o| MimeMagic[o.to_s] }
  end

  def fetch_types predicate
    literals(predicate, datatype: TFO['content-type']).map do |x|
      MimeMagic[x.to_s]
    end
  end

  public

  # this is meant to be called from a harness
  def self.configure harness, subject
    # haul together all the pieces

    # give us the precious
    new(harness, subject).refresh
  end

  # Initialize the representation of the transform.
  #
  # @param harness [Intertwingler::Transform::Harness] backreference
  #  to the harness (and by extension all the state data)
  # @param subject [RDF::URI, URI] the subject URI
  # @param implementation [RDF::URI, URI] the implementation URI
  # @param params [Hash, Array, nil] any parameters to the
  # @param accepts [Array<String,MimeMagic>] media types to accept
  # @param returns [Array<String,MimeMagic>] media types to return
  #
  def initialize harness, subject, implementation: nil,
      params: {}, accepts: [], returns: []
    @harness = harness
    resolver = harness.resolver
    @subject = resolver.coerce_resource subject
    @impl    = resolver.coerce_resource implementation if implementation

    # we have to coerce params/accepts/returns
    @params  = harness.registry[@subject] = coerce_params params
    @accepts = coerce_types accepts
    @returns = coerce_types returns
  end

  attr_reader :harness, :subject, :params
  alias_method :id, :subject

  # Return a dereferenceable URI for the transform. This is just a
  # shorthand for `harness.resolver.uri_for subject`.
  #
  # @param as [:uri, :rdf, :str] coerce the return value to some other
  #  type.
  #
  # @return [URI, RDF::URI] the subject URI.
  #
  def uri as: :uri
    @harness.resolver.uri_for @subject, as: as
  end

  # Return the implementation URI as something that can be passed a
  # request (or not).
  #
  # @param as [:uri, :rdf, :str] cast the result to a type.
  #
  # @return [RDF::URI, URI] the address of the implementation.
  #
  def implementation as: :uri
    @harness.resolver.coerce_resource @impl, as: as if @impl
  end

  # Refresh the state of the transform associated with the subject in
  # the graph.
  #
  # @return [self]
  #
  def refresh

    # get the implementation
    @impl = resources(TFO.implementation).select do |o|
      /^urn:x-ruby:/i.match? o.to_s
    end.sort.first

    # add the implementation to the dispatcher (via engine via
    # harness; will be a noop if it's already in there)

    # get the params. XXX this isn't exactly right. it should get the
    # first tfo:parameter-list (there could be more than one of those,
    # so it should disambiguate) and then look at tfo:parameter
    # (though sorting those is an issue) if there are any that aren't
    # defined in the list.

    params = if pl = blanks(TFO['parameter-list']).sort.first
               RDF::List.new(subject: pl, graph: repo).to_a.uniq
             else
               resources(TFO.parameter).sort
             end

    @params = harness.registry[subject] = params

    # get the accepts
    @accepts = fetch_types TFO.accepts

    # get the returns
    @returns = fetch_types TFO.returns

    # get the preferred serialization
    @prefers = fetch_types(TFO.prefers).sort.first

    self
  end

  # Process a set of parameters.
  #
  # @param values [Object] can't remember what the type is
  #
  # @return [Params::Registry::Instance]
  def process values
    @params.process values
  end

  ### BELOW THIS IS HANDLER/QUEUE STUFF

  class ParamError < ::ArgumentError
    # XXX TODO something coherent about which parameters were wrong
    # (and irreparable by the transform) and a hint at what would fix
    # them, preferably something machine-actionable so any prose can
    # be localized (in addition to machine-actionable being generally
    # good).
  end

  # A partial invocation is a collection of scalar parameter values
  # *without* the message body input, such that all that is missing to
  # complete the invocation is the input body itself. These are used
  # to "curry" the transformation functions, for some interpretation
  # of the term.
  #
  # Queues can accommodate either {Intertwingler::Transform}s or
  # {Intertwingler::Transform::Partial}s. We use the latter when we
  # want to statically configure a set of scalar parameters for a
  # given transform. Indeed, the only time we would invoke this
  # class's constructor directly is in the construction of addressable
  # queues, when the parameters would be read off the request-URI.
  #
  class Partial
    include Intertwingler::GraphOps::Addressable

    private

    def repo; transform.harness.repo; end

    public

    def self.configure transform, subject = nil, **rest
      new(transform, subject: subject, **rest).refresh
    end

    def initialize transform, subject: nil, slug: nil, aliases: [], values: {}
      @transform = transform
      resolver   = transform.harness.resolver
      @subject   = resolver.coerce_resource subject if subject
      @slug      = slug.to_s.to_sym if slug
      @aliases   = aliases.map { |a| a.to_s.to_sym }.sort.uniq
      @params    = transform.process(values) unless values.empty?
    end

    attr_reader :transform, :subject, :params

    def refresh
      if @subject
        # get slug
        @slug = literals(
          CI['canonical-slug']).sort.map { |o| o.to_s.to_sym }.first

        # get aliases
        @aliases = literals(CI.slug).sort.map { |a| a.to_s.to_sym } - [@slug]

        # get properties
        @params = transform.process transform.params.keys.reduce({}) do |hash, k|
          hash[k] = objects(k, entail: false).map do |o|
            o.literal? ? o.object : o.to_s
          end

          hash
        end
      end

      self
    end

    # @!attribute [r] harness
    #  @return [Intertwingler::Transform::Harness]
    def harness ; transform.harness ; end

  end

  # This is a union type to represent transforms in addressable queues
  # that have the same name. It is possible that two or more
  # transforms (represented by path parameters) have the same
  # identifiers and arity, but different accept/return types. It may
  # not be known which is viable until the response handler has been
  # called. This class affords a superposition of transformation
  # functions that can be determined when the necessary information is
  # available.
  #
  # @note We anticipate this situation to arise when two functions do
  #  analogous things to different types, or otherwise, e.g., when the
  #  name of two completely _different_ functions is lexically
  #  identical when spelled in another language.
  class Union < Partial
  end

  # This is intended to mirror `tfo:Invocation`, which is just a
  # record of a particular invocation of a particular function with
  # particular parameters and a particular input, which should always
  # yield a particular output. There is almost no role for this beyond
  # resolving cached transformations.
  class Invocation < Partial
  end

  # This class represents an ordered collection of transforms. "Queue"
  # is a slight misnomer as it's more like a _bag_ of transforms that
  # will be sorted topologically when given a request, if not given an
  # explicit sequence upon initialization. An explicit sequence
  # supersedes the topological sort; unordered transforms will be
  # ignored when it is present. An ordinary transform queue will
  # attempt to run everything in its (potentially
  # dynamically-determined) sequence, ignoring those transforms that
  # do not match the intersection of least one available return type,
  # and the content of the client's `Accept` header.
  #
  #
  # I call the unordered incarnation of a transform queue a _bag_
  # rather than a _set_ because some transforms may not match the
  # payload or the request's `Accept:` header and will thus not be
  # run. Conversely, two or more transforms can be *punned* into the
  # same (human-readable) identifier, implying they do analogous
  # things for different content-types.
  class Queue
    include Intertwingler::GraphOps::Addressable

    # A strict queue differs from an ordinary queue insofar as *all*
    # transforms in the queue *must* be run. This means a strict queue
    # is liable to generate an error if *any* of its constituent
    # transforms don't match the payload, *including* (and
    # *especially*) transforms that are intended to process the result
    # of a previous one. In general it only makes sense for a strict
    # queue to be addressable, but we won't prevent you from using
    # strict queues in other places.
    class Strict < self
      #
      def can_serve! uri, type
        raise "nope can't serve, lol"
      end
    end

    # An addressable queue is a strict queue that is run in the
    # addressable phase of response transforms, typically sandwiched
    # between an early queue and a late queue. If more than one
    # addressable queue is specified in the chain of transformation
    # queues, then only the *last* addressable queue will be treated
    # as such, while any preceding addressable queues will be treated
    # as ordinary strict queues. This will probably be undesirable, so
    # ensure that only one addressable queue is present in the chain.
    class Addressable < Strict
    end

    private

    def repo; harness.engine.repo; end

    public

    # Configure the queue out of the graph.
    #
    # @param harness [Intertwingler::Transform::Harness]
    # @param subject [RDF::URI]
    #
    # @return [Intertwingler::Transform::Queue]
    #
    def self.configure harness, subject
      new(harness, subject).refresh
    end

    # Initialize a (potentially empty) queue.
    #
    # @param harness [Intertwingler::Transform::Harness]
    # @param subject [RDF::URI]
    # @param transforms [Array<Intertwingler::Transform,
    #  Intertwingler::Transform::Partial>]
    # @param next [nil, RDF::URI]
    #
    def initialize harness, subject, transforms: [], next: nil
      @harness    = harness
      @subject    = subject
      @transforms = transforms || []
      @next       = binding.local_variable_get :next # next is a keyword

    end

    attr_reader :harness, :subject
    alias_method :id, :subject

    # Refresh the queue against the graph.
    #
    # @return [self]
    #
    def refresh
      # deal with list
      if list = blanks(TFO['member-list']).sort.first
        list = RDF::List.new(graph: repo, subject: list).to_a
        @transforms = list.map do |member|
          harness.resolve member, queue: false, partial: true, refresh: true
        end
      else
        @transforms = []
      end

      # deal with next queue
      if qnext = resources(TFO.next).sort.first
        qnext = harness.resolve qnext,
          transform: false, partial: false, refresh: true
        # XXX here is where we should check for cycles
        @next = qnext
      else
        @next = nil
      end

      self
    end

    # Push
    #
    # @param member [Intertwingler::Transform,
    #  Intertwingler::Transform::Partial, RDF::URI]
    #
    def push member
      # XXX this may blow up?
      member = harness.resolve member
      @transforms << member
    end

    alias_method :<<, :push

    # Determine if the queue is strict.
    #
    # @return [false, true]
    #
    def strict?
      is_a? Strict
    end

    # Determine if the queue is addressable.
    #
    # @return [false, true]
    #
    def addressable?
      is_a? Addressable
    end

    private

    def prep_request
    end

    def prep_response
    end

    # Test if the transform accepts the `Content-Type`. Intended to
    # provide a hook to subclasses to raise an error if false.
    def can_serve! transform, type
      transform.accepts? type
    end

    public

    # Run this queue and return the altered message. Include the
    # response in the second argument if this is a response queue.
    #
    # @note {::Rack::Response} has no reference back to the
    #  {::Rack::Request} that it responds to, so the request is always needed.
    #
    # @param req [Rack::Request] the request, upon which to base the subrequest.
    # @param resp [Rack::Response, nil] the response, to use in response queues.
    #
    # @return [Rack::Request, Rack::Response] the resulting HTTP message.
    #
    def run req, resp = nil
      # get some shortcuts
      engine     = harness.engine
      resolver   = engine.resolver
      dispatcher = engine.dispatcher

      # for each transform in the queue
      # first we test if it accepts the message body (or is message/http)

      # we gin up a POST subrequest for its uri + params if applicable

      # we use the dispatcher to run the subrequest
      # we shuck off the response body from the subrequest (TODO full http msg)
      # if there is a subsequent transform, we repeat
      # if not we try the next queue
      # if no queues left, we return the message with the new body

      # XXX okay here is where we whine if we run the whole queue and
      # e.g. the Accept: header does not match

      # if this is a response transform then resp will be non-nil
      out  = resp || req
      body = out.body # note the body is different depending on req or resp
      type = out.get_header( # so is this and that is dumb af
        out.is_a?(Rack::Request) ? 'CONTENT_TYPE' : 'content-type') ||
        'application/octet-stream'
      type = MimeMagic[type].canonical || MimeMagic['application/octet-stream']

      # transforms could be transforms or they could be partials; in
      # the case that they're partials, we want to pull the parameters
      # out and append them to the uri
      @transforms.each do |transform|
        if transform.is_a? Intertwingler::Transform::Partial
          params    = transform.params.content.to_h slugs: true
          transform = transform.function
        else
          params = {}
        end

        # we just override this in strict
        next unless can_serve! transform, type

        # this already should be a uuid but eh
        uuid = resolver.uuid_for transform.subject, as: :uri
        # first check if the transform matches typewise
        # XXX TODO handle `message/http`; not doing that one yet lol
        uri = req.base_url + uuid.uuid

        uri.query = URI.encode_www_form(params) unless params.empty?

        subreq  = engine.dup_request req, uri, method: :POST,
          headers: { 'content-type' => type.to_s }, body: body
        subresp = dispatcher.dispatch subreq

        # XXX any failure in here is bad except 304 which is considered a noop

        # reassign content type
        type = MimeType[subresp.get_header 'content-type']

        # reassign body
        body = subresp.body

        # here is where we would break if this was an error or redirect

        # XXX content-encoding content-language etc etc
      end

      # we shouldn't do this if nothing has been run
      if resp
        out = engine.replace_response_body out, body,
          headers: { 'content-type' => type }
      else
        out = engine.dup_request req, body: body
      end

      out
    end

  end

  # A queue chain is like a disposable queue of queues. When a chain
  # is created (e.g., with each HTTP request), it collects its
  # constituent queues and creates shallow copies, enabling them to be
  # manipulated over the course of the request without damaging the
  # originals. The basic {Intertwingler::Transform::Chain} is an
  # abstract class, with the (marginally) more specialized request and
  # response chains building off of it.
  #
  class Chain

    # Initialize a new queue chain. The `head` is the leading queue in
    # the chain. We successively call `next` on the queues to build up
    # a sequence and check for cycles.
    #
    # @param harness [Intertwingler::Transform::Harness] the transform
    #  harness that has all the master copies of everything.
    # @param head [RDF::URI, Intertwingler::Transform::Queue] the
    #  initial queue to traverse to construct the chain.
    #
    def initialize harness, head
      @harness = harness
      @queues  = [harness.resolve(head).dup]
      while queue = queue.next
        @queues << queue.dup
      end
    end

    # Run the queues in the chain over the message and get back the
    # transformed message.
    #
    # @note Unfortunately {Rack::Response} does not include a
    #  reference to the {Rack::Request} that called it, so we have to
    #  pass in both even if we only need one.
    #
    # @param request  [Rack::Request] the HTTP request.
    # @param response [nil, Rack::Request] the HTTP response, if
    # applicable.
    #
    # @return [Rack::Request, Rack::Response] the transformed message.
    #
    def run request, response = nil
      message = response || request
      @queues.each do |q|
        message = q.run request, response do |event|
          # do stuff with side effects
        end
      end

      message
    end

    # A request chain differs from a response chain insofar as it
    # stores up state from request transforms to pass on to the
    # response transforms. A factory method {#response_chain} then
    # produces the appropriate chain to complete the response
    # transforms.
    class Request < self

      # Transform the request.
      #
      # @param message [Rack::Request] an HTTP request.
      #
      # @return [Rack::Request] the transformed request.
      #
      def run request
        super request
      end

      # Return the appropriate response chain for the given handler.
      #
      # @param handler [RDF::URI] the address of the handler selected
      #  by the dispatcher.
      #
      # @return [Intertwingler::Transform::Chain::Response] the response chain.
      #
      def response_chain handler
        head = harness.queue_for handler
        Intertwingler::Transform::Chain::Response.new @harness, head
      end
    end

    # A response chain extends the abstract chain class with methods
    # used to manipulate
    class Response < self

      # Transform the response.
      #
      # @param message [Rack::Response] an HTTP response.
      #
      # @return [Rack::Response] the transformed response.
      #
      def run response
        super response
      end

      # Determine if the chain contains an addressable queue.
      #
      # @return [false, true] whether or not the chain has an
      #  addressable queue.
      #
      def has_addressable?
      end

      # Set the addressable queue in the chain.
      def set_addressable *pp
      end
    end
  end

  # The transformation harness is the part of the
  # {Intertwingler::Engine} responsible for organizing and running the
  # sequence of {Transform}s that manipulate the HTTP message on its
  # way into, and out from, a {Intertwingler::Handler}. The proximate
  # entities managed by the harness are transformation {Queue}s:
  #
  # * A queue can reference a subsequent queue (which can potentially
  #   cycle, which would be an error).
  # * Queues contain either {Transform}s, or {Partial} invocations.
  # * Every valid partial invocation _must_ point to a transform.
  # * Transforms may reference an {Intertwingler::Params::Group}
  #   (keyed by the same subject URI), which in turn may share
  #   {Intertwingler::Params::Template} instances with other groups.
  # * Transforms _must_ reference an {Intertwingler::Handler}, which
  #   supplies the actual implementation, and therefore _must_ respond
  #   to the same (resolved) URI as the transform itself.
  # * Handlers may specify their own queues; lather, rinse, repeat.
  #
  # The engine's configuration record specifies entry points for both
  # request and response queues. Other entry points (for response
  # queues only) can be found attached to handler configurations. The
  # queue records are then traversed to build up the entire structure.
  #
  # @note Since its internal state may change over the lifetime of a
  #  request, the harness must be duplicated at the beginning of every
  #  request, and that duplicate is the copy that ought to be used.
  #
  # @note XXX: this thing will never get used outside of the context
  #  of the dispatcher, so perhaps it should either be attached to the
  #  _dispatcher_ instead of the _engine_. Another option is to just
  #  merge this with the dispatcher, or the dispatcher with it.
  #
  class Harness
    include Intertwingler::GraphOps::Addressable

    # Inititalize the harness and populate it with configuration from
    # the graph.
    #
    # @param engine [Intertwingler::Engine]
    #
    # @return [Intertwingler::Transform::Harness]
    #
    def self.configure engine
      new(engine).refresh
    end

    # Initialize an empty harness. This will do nothing and it must be
    # populated separately.
    #
    # @param engine [Intertwingler::Engine]
    #
    def initialize engine, queues: [], request_head: nil, response_head: nil
      # the engine
      @engine  = engine
      resolver = engine.resolver

      # maps
      @queues     = {}
      @transforms = {}
      @partials   = {}

      # queue heads
      @request_head  = resolver.coerce_resource request_head  if request_head
      @response_head = resolver.coerce_resource response_head if response_head

      # chains, keyed by queue head
      @request  = {}
      @response = {}
    end

    # Refresh (recursively) the configuration in the graph.
    #
    # @return [self]
    #
    def refresh
      @request_head  = resources(TFO['request-queue']).sort.first
      @response_head = resources(TFO['response-queue']).sort.first

      # this is probably enough to get things started, actually
      [@request_head, @response_head].each do |q|
        resolve q, transforms: false, partials: false, refresh: true
      end

      #

      self
    end

    private

    # just making a mess here really

    def resolve_queue uri, state, force: nil
    end

    def resolve_transform uri, state, force: nil
      # check for associated handler implementation
      impl = repo.objects_for(uri,
        TFO.implementation, only: :resource).sort.first
      raise unless impl
    end

    def resolve_partial uri, state, force: nil
    end

    def resolve_handler uri, state, force: nil
      engine.dispatcher.add uri, queues: false
    end

    def resolve_one uri, state, force: nil
      ts = repo.asserted_types uri
      if repo.type_is?(ts, TFO.Partial) and !repo.type_is?(ts, TFO.Invocation)
        resolve_partial uri, state, force: force
      elsif repo.type_is? ts, TFO.Transform
        resolve_transform uri, state, force: force
      elsif repo.type_is? ts, TFO.Queue
        resolve_queue uri, state, force: force
      end
    end

    public

    def queue_for uri, refresh: false
    end

    def transform_for uri, partials: true, refresh: false
    end

    def partial_for uri, refresh: false
    end

    def resolve uri, queues: true, transforms: true, partials: true,
        refresh: false
      uri = engine.resolver.coerce_resource uri
    end

    # Duplicate the harness and the queues but keep everything else.
    def dup
      # dup the queues but not their contents
      queues = []

      # start a new instance
      self.class.new engine, queues: queues,
        request_head: @request_head, response_head: @response_head
    end

    attr_reader :engine, :request_head, :response_head

    # @!attribute [rw] request_head
    #  @note The URI *must* already be known to the harness. The chain
    #   of queues will be recalculated on assignment.
    #  @return [RDF::URI] The initial *request* queue's URI.
    #  @raise [ArgumentError] on assignment, if it doesn't like your URI.
    #
    def request_head= uri
      if uri
        uri = engine.resolver.coerce_resource uri
        register? uri
        @request_head = uri
      end
    end

    # @!attribute [rw] response_head
    #  @note The URI *must* already be known to the harness. The chain
    #   of queues will be recalculated on assignment.
    #  @return [RDF::URI] The initial *response* queue's URI.
    #  @raise [ArgumentError] on assignment, if it doesn't like your URI.
    #
    def response_head= uri
      if uri
        uri = engine.resolver.coerce_resource uri
        register? uri
        @response_head = uri
      end
    end

    # @!attribute [r] repo
    #  @return [RDF::Repository] The graph repository from the engine.
    def repo
      @engine.repo
    end

    # @!attribute [r] subject
    #  @return [RDF::URI] The subject URI of the engine.
    def subject
      @engine.subject
    end


    # @!attribute [r] resolver
    #  @return [Intertwingler::Resolver] The resolver from the engine.
    def resolver
      @engine.resolver
    end

    # @!attribute [r] dispatcher
    #  @return [Intertwingler::Engine::Dispatcher] The dispatcher from
    #   the engine.
    def dispatcher
      @engine.dispatcher
    end

    # Set the contents of the addressable queue based on harvested
    # path parameters. This will attempt to resolve the
    # transformations and their parameters.
    def set_addressable *pairs
    end

    # Run the request queues.
    def run_request req
      @request.each { |queue| req = queue.run req }

      req
    end

    # Run the response queues.
    def run_response req, resp
      @response.each { |queue| resp = queue.run req, resp }

      resp
    end

  end

  # This is the actual transform _handler_ class, that includes a number
  class Handler < Intertwingler::Handler

    private

    NUMBER_RE = /^[+-]?(?=\.?\d)\d*\.?\d*(?:[Ee][+-]?\d+)?\z/
    TOKEN_RE  = /[^\x0-\x20\x7f-\xff()<>@,;:\\"\/\[\]?={}]+/n # srsly??
    MTYPE_RE  = /^#{TOKEN_RE}\/#{TOKEN_RE}/on

    # subclasses contain a hard-coded URI map that never changes
    URI_MAP = {}.freeze

    def uri_map uuid
      (self.class.const_get(:URI_MAP) || {})[uuid]
    end

    def representation
      self.class.const_get :REPRESENTATION or raise NotImplementedError,
        'a REPRESENTATION must be defined in the class'
    end

    public

    def handle req
      # first we check if the request method is POST; if not this is over quickly
      return Rack::Response[405, {}, []] unless req.request_method.to_sym == :POST

      # request must have a content type or return 409
      type = req.content_type or
        return Rack::Response[409, {}, ['Missing Content-Type header']]
      type = MimeMagic[type].canonical # XXX do we preserve type parameters??

      # give us a default response
      resp = Rack::Response[404, {}, []]

      # get the resolver for this request or bail out
      resolver = resolver_for(req) or return resp

      uri  = URI(resolver.preproc req.url) # annoying that isn't already a URI
      uuid = resolver.split_pp(uri).first.path.split(?/)[1].downcase

      # match the function
      func, accept, variants = uri_map uuid

      # 404 unless we have a function
      return resp unless func

      # XXX this next bit is where we would have the thing like Params::Registry

      # XXX well dingdong we now have Params::Registry so maybe use it??

      # harvest params from uri
      params = URI.decode_www_form(uri.query.to_s).reduce({}) do |hash, pair|
        k, v = pair
        # XXX UNKNOWN KEYS SHOULD BE IGNORED
        k = k.strip.downcase.tr_s(?-, ?_).to_sym
        v = NUMBER_RE === v ? v.to_f : v
        (hash[k] ||= []) << v
        hash
      end

      # check request content type and return 415 if no match
      return Rack::Response[415, {}, ["Unsupported type #{type}"]] unless
        type.lineage.any? { |t| accept.include? t.to_s }

      variants = variants.reduce({}) do |hash, v|
        v = MimeMagic[v].canonical
        ([v.canonical] + v.aliases).each { |t| hash[t] = { type: v.to_s } }

        hash
      end

      # we want to do surgery to Accept:
      headers = HTTP::Negotiate.parse_headers req

      # this will guarantee that if the request accepts a certain type
      if ahdr = headers[:type]
        ahdr.keys.each do |t|
          q = ahdr[t][:q]
          lin = MimeMagic[t].lineage.reject do |x|
            %w[text/plain application/octet-stream].include? x
          end
          lin.each_with_index do |mt, i|
            # this will slightly decrement the score a little more each time
            (ahdr[mt.to_s] = {})[:q] = 0.999 ** i * q unless ahdr.key? mt.to_s
          end
        end
      end

      # okay this is gonna be a weird one: we can have input like
      # `major/*` and `*/*` which i'm inclined to just leave alone. we
      # want to canonicalize any asserted types plus add the type
      # lineage all the way up to application/octet-stream

      # here we do the content negotiation
      # 406 unless types match up
      rtype = HTTP::Negotiate.negotiate headers, variants

      # XXX this would be helpful if it would say what variants it *does* have
      return Rack::Response[406, {}, []] unless rtype

      # duplicate the request
      req = Rack::Request.new req.env.dup

      # set the accept header to the only one
      req.set_header 'HTTP_ACCEPT', "#{rtype.to_s}, */*;q=0"

      # okay now we actually run the thing
      begin
        # it is so dumb you can't just set the body
        req.env['rack.input'] = representation.coerce req.body, type: type.to_s

        # run the transform, get back the body
        out = send func, req, params

        # note `out` can be nil which should be interpreted as 304
        return Rack::Response[304, {}, []] unless out

        # this should be it
        return Rack::Response[200, { 'content-type' => out.type.to_s }, out]

      rescue Intertwingler::Transform::ParamError
        return Rack::Response[409, {}, []]
      rescue Intertwingler::Handler::Redirect => r
        # umm i dunno
        # r.location
        warn r
      rescue Intertwingler::Handler::Error => r
        # umm i dunno
        # r.location
        return r.response
      end
    end
  end

end

# XXX EVERYTHING BELOW THIS LINE IS OLD AND I AM NOT SURE HOW MUCH I
# WANT TO CHOP UP FOR PARTS YET. SO JUST IGNORE IT MKAY
#
# This class encapsulates a specification for an individual
# transformation function, including its parameter spec, accepted and
# returned types, identity, and implementation.
#
class Intertwingler::NotTransform
  # mkay basically this transformation function stuff got too hairy to
  # just do ad-hoc so i guess i'm doing this now

  private

  TFO = Intertwingler::Vocab::TFO

  def self.numeric_objects repo, subject, predicate, entail: false
    repo.objects_for(
      subject, predicate, entail: entail, only: :literal
    ).reduce([]) do |a, o|
      a << o.object if o.object.is_a? Numeric
      a
    end.sort
  end

  def self.gather_params repo, subject
    params = {}
    repo.objects_for(subject, TFO.parameter,
                     entail: false, only: :resource).each do |ps|
      param = params[ps] ||= {}

      # slug/identifier
      if id = repo.objects_for(
        ps, RDF::Vocab::DC.identifier, only: :literal).sort.first
        param[:id] = id.value.to_sym
      end

      # rdfs:range
      range = repo.objects_for(ps, RDF::RDFS.range, only: :resource)
      param[:range] = range.to_set unless range.empty?

      # default = Intertwingler::Util
      param[:default] = repo.objects_for(ps, TFO.default)

      # cardinalities
      param[:minc] = 0
      param[:maxc] = Float::INFINITY

      if c0 = numeric_objects(repo, ps, RDF::OWL.cardinality).first
        param[:minc] = param[:maxc] = c0
      else
        if c1 = numeric_objects(repo, ps, RDF::OWL.minCardinality).first
          param[:minc] = c1
        end
        if c2 = numeric_objects(repo, ps, RDF::OWL.maxCardinality).first
          param[:maxc] = c2
        end
      end
    end

    params
  end

  def self.gather_accepts_returns repo, subject, raw: false, returns: false
    literals = []
    lists    = []
    pred     = TFO[returns ? 'returns' : 'accepts']

    repo.query([subject, pred, nil]).objects.each do |o|
      if o.literal?
        literals << o
      else
        lists << RDF::List.from(repo, o).to_a
      end
    end
    # this is mainly to give us consistent results
    out = (lists.sort.flatten + literals.sort).uniq
    # raw as in raw literals
    raw ? out : out.map(&:value)
  end

  protected

  # Initialize the implementation. Does nothing in the base
  # class. Return value is ignored.
  #
  # @param harness [Intertwingler::Transform::Harness] the harness
  #
  def init_implementation harness
  end

  public

  # Resolve a transform out of the repository. Optionally supply a
  # block to resolve any implementation associated with the transform.
  #
  # @param harness [Intertwingler::Transform::Harness] the harness
  # @param subject [RDF::Resource]
  def self.resolve harness, subject
    # noop
    return subject if subject.is_a? self

    repo = harness.repo

    asserted = repo.objects_for subject, RDF.type, only: :resource

    return if (asserted & repo.all_related(TFO.Transform)).empty?

    params = gather_params repo, subject

    plist = if pl = repo.objects_for(
              subject, TFO['parameter-list'], only: :resource
            ).sort.first
              RDF::List.from(repo, pl).to_a
            else
              params.keys.sort
            end

    accepts = gather_accepts_returns repo, subject
    returns = gather_accepts_returns repo, subject, returns: true

    tclass = self

    # XXX this is all dumb but it has to be this way for now

    if impl = repo.objects_for(subject,
      TFO.implementation, only: :uri).sort.first

      case impl.to_s
      when /^file:/i then
        # XXX redo this later
        if /xsl/i.match? MimeMagic.by_path(impl.path.to_s).to_s
          tclass = Intertwingler::Transform::XSLT
        end
      when /^urn:x-ruby:(.*)$/i then
        cn = $1
        begin
          cs = Object.const_get cn
          tclass = cs
        rescue NameError, e
          raise NotImplementedError,
            "Could not locate implementation for #{impl}!"
        end
      end
    end

    tclass.new subject, params: params, param_list: plist, accepts: accepts,
      returns: returns, implementation: impl, harness: harness
  end

  def self.coerce_params params
    # this idiom is everywhere
    params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v]) unless v.nil?
    end
  end

  attr_reader :subject

  # Initialize a transform from data.
  # @param subject [RDF::Resource]
  # @param harness [Intertwingler::Transform::Harness]
  # @param params [Hash]
  # @param param_list [Array]
  # @param accepts [Array]
  # @param returns [Array]
  # @param implementation [RDF::Resource]
  #
  def initialize subject, harness: nil, params: {}, param_list: [],
      accepts: %w[*/*], returns: %w[*/*], implementation: nil
    @subject = subject.dup.freeze
    @params  = params.freeze
    @plist   = (param_list.empty? ? params.keys.sort : param_list.dup).freeze
    @pcache  = params.map { |k, v| [v[:id], k] }.to_h.freeze
    @accepts = (accepts.respond_to?(:to_a) ? accepts.to_a : [accepts]).freeze
    @returns = (returns.respond_to?(:to_a) ? returns.to_a : [returns]).freeze
    @impl    = implementation.freeze

    # initialize the implementation
    init_implementation harness
  end

  # Return the identifier of the implementation.
  #
  # @return [RDF::URI]
  #
  def implementation
    @impl
  end

  # True if this transform is *actually* implemented.
  #
  # @return [false, true]
  #
  def implemented?
    false
  end

  # True if the transform accepts the given Content-Type.
  #
  # @param type [String] the content type to test
  # @return [false, true] wh
  #
  def accepts? type
    # construct the variants: this gives us a stack of all the types
    # all the way up to the top, then turns it into a hash of faux
    # variants. this will ensure the negotiate algorithm will return a
    # value if the transform function can handle the type, even if it
    # does not explicitly mention it (e.g. if the transform specifies
    # it accepts application/xml and you hand it application/xhtml+xml)
    variants = MimeMagic.new(type).lineage.map do |t|
      # the key can be anything as long as it's unique since it ends
      # up as a hash
      [t.to_s, [1, t.to_s]]
    end.to_h

    # construct the pseudo-header
    accept = @accepts.dup
    accept << '*/*;q=0' unless accept.include? '*/*'
    accept = { Accept: accept.join(', ') }

    # we only care *if* this returns something, not *what*
    !!HTTP::Negotiate.negotiate(accept, variants)
  end

  # Return the parameter list, or a sorted list of parameter keys in lieu
  #
  # @return [Array]
  #
  def keys
    # XXX this should be unique to begin with. what is going on here?
    # tests mysteriously started failing and the output was duplicated
    @plist.uniq
  end

  # Retrieve a parameter spec, either by its fully-qualified URI or
  # its `dct:identifier`.
  #
  # @param key [RDF::Resource,Symbol,String] the parameter URI or its identifier
  # @return [Hash] the parameter spec
  #
  def [](key)
    out = case key
          when RDF::Resource then @params[key]
          when Symbol then @params[@pcache[key]]
          when String
            @params[@pcache[key.to_sym]] || @params[RDF::URI(key)]
          end
    # add the key to the group
    out.merge({ uri: key }) if out
  end

  # XXX kill this
  def lint params
    raise ArgumentError, "params must be a hash, not #{params.class}" unless
      params.is_a? Hash
    params.keys.sort == keys
  end

  # Return the validated parameters or raise an exception.
  #
  # @param params [Hash] the hash of parameters
  # @param symbols [false, true] whether the keys should be symbols or URIs
  # @param defaults [true, false] whether to supplant the defaults
  # @param silent [false, true] return nil rather than raise if true
  # @return [Hash] the validated parameters
  #
  def validate params, symbols: false, defaults: true, silent: false
    # duplicate so we can delete from it
    params = params.dup
    out = {}

    # note the instance variable vs the argument
    @params.each do |k, spec|
      v = params.delete(k) || params.delete(spec[:id]) || []
      v = (v.respond_to?(:to_a) ? v.to_a : [v]).map do |v|
        case v
        when RDF::Term then v
        when URI then RDF::URI(v.to_s)
        when nil then RDF::nil
        else
          range = spec[:range] || []
          if r = range.select(&:datatype?) and !r.empty?
            r = r.to_a.sort
            "multiple ranges; arbitrarily picking #{r.first}" if
              r.size > 1
            RDF::Literal(v, datatype: r.first)
          elsif v.is_a? String and r = range.reject(&:datatype?) and !r.empty?
            if m = /^_:(.+)$/.match(v)
              RDF::Node(m[1])
            else
              RDF::URI(v)
            end
          else
            RDF::Literal(v)
          end
        end
      end

      # XXX one day we should check types but not today

      # give us the default(s) then
      v = spec[:default].dup if v.empty? and spec[:default]

      # but we *will* check the cardinality
      minc = spec.fetch :minc, 0
      maxc = spec.fetch :maxc, Float::INFINITY

      raise ArgumentError, "Parameter #{k} must have at least"\
        " #{minc} value#{minc == 1 ? '' : ?s }" if v.size < minc
      raise ArgumentError, "Parameter #{k} must have at most"\
        " #{maxc} value#{maxc == 1 ? '' : ?s }" if v.size > maxc
      # XXX if cardinality == 1 should we set v to v.first? dunno

      # now overwrite k
      k = spec[:id] || k.to_s if symbols

      out[k] = v unless !defaults and v == spec[:default]
    end

    # if params are not empty then this is an error
    unless params.empty?
      return if silent
      raise ArgumentError,
        "Unrecognized parameters #{params.keys.join ', '}"
    end

    out
  end

  # Check the parameters and apply the function, then check the
  # output. Parameters are checked with {#validate} for key
  # resolution, cardinality, range, and type.
  #
  # @param input [String,IO,#to_s,#read] Something bytelike
  # @param params [Hash,Intertwingler::Transform::Partial] the instance parameters
  # @param parsed [Object] the already-parsed object, if applicable
  # @param type [String] the content-type of the input
  # @param accept [String] a string in the form of an Accept header
  # @yieldparam output [String,IO] the output
  # @yieldparam parseout [Object] the parsed output, if applicable
  # @return [#to_s, Object] the serialized output (and parsed if applicable)
  #
  def apply input, params = {}, parsed: nil,
      type: 'application/octet-stream', accept: '*/*', &block
    raise NotImplementedError, "Transform #{@id} is not implemented!" unless
      implemented?

    # XXX validate accept or explode
    mimetypes = HTTP::Negotiate.negotiate({ Accept: accept },
      @returns.map { |t| [t, [1, t]] }.to_h, all: true) or return

    # this will succeed or explode
    params = validate params, symbols: true

    # run the transform
    out, parseout = execute input, parsed, params

    # bail out if nothing was returned
    return unless out

    # now run the block if present
    block.call out, parseout if block

    # return it to the caller
    [out, parseout]
  end

  # This class implements a cache for partial transformation function
  # applications, which bundle transforms with a set of instance
  # parameters under a reusable identity.
  class PartialCache
    private

    def coerce_params params
      Intertwingler::Transform.coerce_params params
    end

    public

    # Initialize the cache with all partials pre-loaded.
    #
    # @param harness [Intertwingler::Transform::Harness] the transform harness
    # @return [Intertwingler::Transform::PartialCache] the instance
    #
    def self.load harness
      new(harness).load
    end

    attr_reader :harness

    # Initialize an empty cache.
    # @param harness [Intertwingler::Transform::Harness] the parent harness.
    #
    def initialize harness
      @harness    = harness
      @cache      = {}
      @mapping    = {}
      @transforms = {}
    end

    # Load an initialized partial cache.
    #
    # @return [self] daisy-chainable self-reference
    #
    def load
      repo.subjects_for(RDF.type, TFO.Partial).each do |s|
        resolve subject: s
      end

      # return self to daisy-chain
      self
    end

    def partials
      @cache.keys.select { |x| x.is_a? RDF::Resource }
    end

    def repo
      @harness.repo
    end

    def transforms
      @transforms.dup
    end

    # Retrieve a Partial from the cache based on its
    def get transform, params
      ts = case transform
           when Intertwingler::Transform then transform.subject
           when RDF::URI
             # XXX transforms resolved here may not get implemented
             transform = Intertwingler::Transform.resolve @repo, transform
             transform.subject
           else
             raise ArgumentError, "Don't know what to do with #{transform}"
           end

      # return direct cache entry if transform is really the subject
      return @cache[ts] if @cache.key?

      # otherwise return the mapping
      @mapping[transform][coerce_params params]
    end

    # Resolves a partial either by subject or by transform + parameter
    # set.
    #
    # @param subject [RDF::URI] The subject URI of the partial
    # @param transform [RDF::URI,Intertwingler::Transform] the transform
    # @param params [Hash] an instance of parameters
    # @return [Intertwingler::Transform::Partial]
    #
    def resolve subject: nil, transform: nil, params: {}
      if subject
        if subject.is_a? Intertwingler::Transform::Partial
          # snag the transform
          transform = @harness.resolve(subject.transform) or
            raise 'Could not resolve the transform associated with ' +
            subject.subject

          # mkay now add this to the cache
          t = @mapping[transform.subject] ||= {} # lol got all that?
          @cache[subject.subject] ||= t[subject.params] ||= subject
        else
          # resolve the partial
          partial = @cache[subject] || Intertwingler::Transform::Partial.resolve(
            @harness, subject: subject) or return

          # initialize the mapping if not present
          t = @mapping[partial.transform.subject] ||= {}

          # off we go
          @cache[subject] ||= t[partial.params] ||= partial
        end
      elsif transform
        transform = @harness.resolve transform unless
          transform.is_a? Intertwingler::Transform

        params = transform.validate params, defaults: false

        # note the *presence* of the key means the cache item has been
        # checked already; its *value* may be nil
        t = @mapping[transform.subject] ||= {}
        return t[params] if t.key? params

        # try to resolve the partial
        partial = Intertwingler::Transform::Partial.resolve(@harness,
          transform: transform, params: params) or return

        # update the caches
        @cache[partial.subject] = t[params] = partial
      end
    end
  end

  # This class is the main harness for holding all the transforms and
  # operating over them. This is the primary interface through which
  # we manipulate transforms.
  class Harness

    attr_reader :partials, :repo, :root

    # Create a new harness instance.
    #
    # @param repo [RDF::Repository] the repository to find RDF data
    # @param root [String,Pathname] the root directory for implementations
    #
    def initialize repo, root
      raise ArgumentError,
        "repo is #{repo.class}, not an RDF::Repository" unless
        repo.is_a? RDF::Repository
      @repo = repo
      @root = Pathname(root).expand_path
      raise ArgumentError, "Root #{@root} does not exist" unless
        @root.directory? and @root.readable?
      @cache    = {}
      @partials = Intertwingler::Transform::PartialCache.new self
    end

    # Bootstrap all the transforms.
    #
    # @param repo [RDF::Repository] the repository to find RDF data
    # @param root [String,Pathname] the root directory for implementations
    # @return [Intertwingler::Transform::Harness] the harness instance
    def self.load repo, root
      self.new(repo, root).load
    end

    # Load transforms into an existing instance
    # @return [Array] the transforms
    def load
      @repo.subjects_for(
        RDF.type, TFO.Transform, only: :resource
      ).each do |subject|
        resolve subject
      end

      # return self so we can daisy-chain
      self
    end

    # Return all cached Transform identities.
    #
    # @return [Array] the URIs of known Transforms
    #
    def transforms
      @cache.keys.sort
    end

    # Resolve a Transform based on its URI.
    #
    # @param subject [RDF::Resource] the identifier for the transform.
    # @return [Intertwingler::Transform] the Transform, if present.
    #
    def resolve subject
      return @cache[subject] if @cache[subject]
      # XXX raise???
      transform =
        Intertwingler::Transform.resolve(self, subject) or return
      @cache[subject] = transform
    end

    # Resolve a Partial based on either its subject URI or the
    # transform-params pair.
    #
    # @param subject [RDF::Resource] the Partial's subject
    # @param transform [RDF::Resource,Intertwingler::Transform] the transform
    # @param params [Hash] an instance of parameters
    # @return [Intertwingler::Transform::Partial] the Partial, if present
    #
    def resolve_partial subject: nil, transform: nil, params: nil
      partials.resolve subject: subject, transform: transform, params: params
    end

    # Resolve a total function application record based on either its
    # subject URI, a transform-params pair, or a Partial.
    #
    # @param subject [RDF::Resource] the Application's subject
    # @param transform [RDF::Resource,Intertwingler::Transform] the Transform
    # @param params [Hash] an instance of parameters
    # @param partial [RDF::Resource,Intertwingler::Transform::Partial] a Partial
    # @return [Intertwingler::Transform::Application] the Application, if present
    #
    def resolve_application subject: nil, transform: nil, params: {},
        partial: nil, input: nil, output: nil
      Intertwingler::Transform::Application.resolve self, subject: subject,
        transform: transform, params: params, partial: partial,
        input: input, output: output
    end

    # Returns true if the Application with the given subject URI
    # matches either the transform-params pair, or a partial.
    #
    # @param subject [RDF::Resource,Intertwingler::Transform::Application]
    #   the application
    # @param transform [RDF::Resource,Intertwingler::Transform] the transform
    # @param params [Hash] an instance of parameters
    # @param partial [RDF::Resource,Intertwingler::Transform::Partial] a partial
    # @return [true, false] whether or not the application matches
    #
    def application_matches? subject, transform: nil, params: {}, partial: nil

      # unbundle the params; partial overrides transform+params
      if partial
        partial   = resolve_partial partial unless
          partial.is_a? Intertwingler::Transform::Partial
        transform = partial.transform
        params    = partial.params
      else
        transform = resolve transform unless
          transform.is_a? Intertwingler::Transform
        params = transform.validate params
      end

      if subject.is_a? Intertwingler::Transform::Application
        return true if partial and subject.completes? partial
        return true if
          subject.transform == transform and subject.matches? params
      else
        # this should say, try matching the partial if there is one
        # to match, otherwise attempt to directly match the transform
        return true if partial and repo.has_statement?(
          RDF::Statement(subject, TFO.completes, partial.subject))

        if repo.has_statement?(
          RDF::Statement(subject, TFO.transform, transform.subject))
          testp = transform.keys.map do |p|
            o = repo.query([subject, p, nil]).objects.uniq.sort
            o.empty? ? nil : [p, o]
          end.compact.to_h

          # this will clear any explicit declarations of defaults
          testp = transform.validate testp, defaults: false, silent: true
          # true means it matches
          return testp == params
        end
      end

      false
    end
  end

  class Partial
    # Resolve a partial function application with the given parameters.
    #
    # @param harness [Intertwingler::Transform::Harness] the harness
    # @param subject [RDF::Resource] the identity of the partial
    # @param transform [RDF::Resource] the identity of the transform
    # @param params [Hash] key-value pairs
    def self.resolve harness, subject: nil, transform: nil, params: {}
      raise ArgumentError, 'Must supply either a subject or a transform' unless
        subject or transform

      repo = harness.repo

      # coerce the transform to a Transform object if it isn't already
      if transform
        transform = harness.resolve(transform) or
          return unless transform.is_a?(Intertwingler::Transform)
      elsif subject.is_a? RDF::URI
        # locate the transform if given the subject
        transform = repo.objects_for(
          subject, TFO.transform, only: :resource).first or return
        transform = harness.resolve(transform) or return
        warn transform
      end

      # obtain the subject for the given parameters
      if subject
        params = {}
        transform.keys.each do |p|
          o = repo.query([subject, p, nil]).objects.uniq.sort
          params[p] = o unless o.empty?
        end
      else
        params = transform.validate params, symbols: false, defaults: false

        candidates = RDF::Query.new do
          # XXX we should sort parameters by longest value since
          # longer values will probably be less common; anyway this is
          # gonna all need to be rethought
          params.each { |p, objs| objs.each { |o| pattern [:s, p, o] } }
          pattern [:s, RDF.type, TFO.Partial]
          pattern [:s, TFO.transform, transform.subject]

          # add any remaining parameters
          # XXX this actually messes up; we don't want this
          # (transform.keys - params.keys.sort).each { |r| pattern [:s, r, nil] }
        end.execute(repo).map { |sol| [sol[:s], {}] }.to_h

        # warn "yo #{transform.subject} #{params} #{candidates}"

        # this is ruby being cheeky
        candidates.select! do |s, ps|
          transform.keys.each do |p|
            o = repo.query([s, p, nil]).objects.uniq.sort
            ps[p] = o unless o.empty?
          end
          ps == params
        end

        return if candidates.empty?

        # sort it so we always get the same thing
        subject = candidates.keys.sort.first
        params  = candidates[subject]
      end

      self.new subject, transform, params
    end

    attr_reader :subject, :transform

    def initialize subject, transform, params = {}
      raise ArgumentError, 'transform must be a Transform' unless
        transform.is_a? Intertwingler::Transform
      @subject   = subject
      @transform = transform
      @params    = transform.validate params unless
        params.is_a? Intertwingler::Transform::Partial
    end

    def [](key)
      @params[key]
    end

    def keys
      @params.keys
    end

    def params
      @params.dup
    end

    def matches? params
      @params == @transform.validate(params)
    end

    def ===(other)
      return false unless other.is_a? Intertwingler::Transform::Partial
      transform == other.transform and matches? other.params
    end

    def ==(other)
      self === other and subject == other.subject
    end
  end

  # A record of a transformation function application.
  # @note "Application" as in to "apply" a function, not an "app".
  class Application < Partial
    # Resolve a particular function Application from the repository.
    # Either resolve by subject, or resolve by a transform + parameter
    # + input set. Applications that complete Partials will be
    # automatically resolved.
    #
    # @param harness [Intertwingler::Transform::Harness] the harness
    # @param subject [RDF::Resource] the subject
    # @param transform [RDF::Resource,Intertwingler::Transform] the transform
    # @param params [Hash] an instance of parameters
    # @param input [RDF::Resource] the Application's input
    # @param output [RDF::Resource] the Application's output
    #
    # @return [Intertwingler::Transform::Application] the Application, if present
    #
    def self.resolve harness, subject: nil, transform: nil, params: {},
        partial: nil, input: nil, output: nil
      # either a subject or transform + input + output? + params?

      repo     = harness.repo
      partials = harness.partials

      if subject
        # noop
        return subject if subject.is_a? self

        # okay partial
        partial = repo.objects_for(
          subject, TFO.completes, only: :resource).sort.first

        if partial
          tmp = partials.resolve(subject: partial) or
            raise "Could not find partial #{partial}"
          partial   = tmp
          transform = partial.transform
        else
          transform = repo.objects_for(
            subject, TFO.transform, only: :resource).sort.first or
            raise "Could not find a transform for #{subject}"
          tmp = harness.resolve(transform) or
            raise "Could not find transform #{transform}"
          transform = tmp

          params = transform.validate

          # get params
          params = {}
          transform.keys.each do |p|
            o = repo.query([subject, p, nil]).objects.uniq.sort
            params[p] = o unless o.empty?
          end
        end

        # get inputs and outputs
        input  = repo.objects_for(
          subject, TFO.input,  only: :resource).sort.first
        output = repo.objects_for(
          subject, TFO.output, only: :resource).sort.first

        raise 'Data must have both input and output' unless input and output
      elsif input and ((transform and params) or partial)

        # XXX dispatch on partial only? smart? dumb?
        if partial
          transform = partial.transform
          params    = partial.params
        else
          # do transform
          t = harness.resolve(transform) or
            raise "Could not resolve transform #{transform}"
          transform = t

          # coerce/validate params
          params = transform.validate params, defaults: false

          # do partial
          partial = partials.resolve transform: transform, params: params
        end

        # collect function application receipts
        candidates = RDF::Query.new do
          # note that there is no cost-based optimization so we write
          # these in the order of least to most cardinality
          pattern [:t, TFO.output, output] if output
          pattern [:t, TFO.input,  input]
        end.execute(repo).map { |sol| sol[:t] }.compact.uniq.select do |s|
          # this should say, try matching the partial if there is one
          # to match, otherwise attempt to directly match the transform
          if partial and repo.has_statement?(
            RDF::Statement(s, TFO.completes, partial.subject))
            true
          elsif repo.has_statement?(
            RDF::Statement(s, TFO.transform, transform.subject))
            testp = transform.keys.map do |p|
              o = repo.query([s, p, nil]).objects.uniq.sort
              o.empty? ? nil : [p, o]
            end.compact.to_h

            testp = transform.validate testp, defaults: false, silent: true
            testp == params
          end
        end.compact.uniq.sort

        return if candidates.empty?

        if candidates.size == 1
          subject = candidates.first
        else
          # now we have the unlikely case that there are two identical
          # records so we just sort em first by end date, then by
          # start date, then lexically
          subject = candidates.map do |s|
            st, et = %i[startedAtTime endedAtTime].map do |p|
              repo.query([s, RDF::Vocab::PROV[p], nil]).map do |stmt|
                dt = stmt.object.object
                dt if dt.is_a? DateTime
              end.compact.sort.last
            end
            [s, st, et]
          end.sort do |a, b|
            # first check latest end-time, then check latest start-time
            c = a[2] && b[2] ? b[2] <=> a[2] : 0
            # if those two yield nothing, then sort lexically i guess
            (c == 0 && a[1] && b[1]) ? b[1] <=> a[1] : a[0] <=> b[0]
          end.first.first
        end
      else
        raise ArgumentError,
          'must have either a subject or transform + params + input'
      end

      # don't forget the output
      output ||= repo.objects_for(
        subject, TFO.output, only: :resource).sort.first

      new subject, transform, input, output, partial || params

    end

    attr_reader :input, :output, :completes

    # Create a new function application from whole cloth.
    #
    # @param subject [RDF::Resource]
    # @param transform [RDF::Resource] the identifier for the transform
    # @param input  [RDF::Resource] the identifier for the input
    # @param output [RDF::Resource] the identifier for the output
    # @param params [Hash, Intertwingler::Transform::Partial] the parameters
    #   or partial application that is completed
    def initialize subject, transform, input, output, params = {},
        start: nil, stop: nil
      # params may be a partial
      super subject, transform, params

      @input     = input
      @output    = output
      @completes = params if params.is_a? Intertwingler::Transform::Partial
      @start     = start
      @stop      = stop
    end

    # Returns the function application as an array of triples.
    def to_triples
      out = [] # .extend RDF::Enumerable
      s = @subject
      out << [s, RDF.type, TFO.Application]

      if @start
        start = @start.is_a?(RDF::Literal) ? @start : RDF::Literal(@start)
        out << [s, RDF::Vocab::PROV.startedAtTime, start]
      end

      if @stop
        stop = @stop.is_a?(RDF::Literal) ? @stop : RDF::Literal(@stop)
        out << [s, RDF::Vocab::PROV.endedAtTime, stop]
      end

      if @completes
        out << [s, TFO.completes, @completes.subject]
      else
        out << [s, TFO.transform, transform.subject]
        pdup = transform.validate params, defaults: false, silent: true
        pdup.each do |k, vals|
          vals.each { |v| out << [s, k, v] }
        end
      end

      out.map { |triples| RDF::Statement(*triples) }
    end

    def [](key)
      # note complete is
      (@completes || @params)[key]
    end

    def keys
      (@completes || @params).keys
    end

    def params
      @completes ? @completes.params : @params.dup
    end

    def transform
      @completes ? @completes.transform : @transform
    end

    def completes? partial
      @completes and partial and @completes == partial
    end

    def matches? params
      return @completes.matches? params if @completes
      super params
    end

    def ===(other)
      return false unless other.is_a? Application
      return false unless @input == other.input and @output == other.output

      # now the comparand is either the partial or us
      cmp = @completes || self

      # and this should do it
      other.transform == cmp.transform and other.matches? cmp.params
   end
  end

  # XXX everything below this line is trash

  def match_params repo, candidate, params = {}
    # overwrite normalized params
    params = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    struct = {}
    params.keys.each do |p|
      repo.query([candidate, p, nil]) do |stmt|
        x = struct[stmt.predicate] ||= Set.new
        x << stmt.object
      end
    end
  end

  # Resolve a transformation application function in the repository
  # with the given inputs and outputs.
  #
  # XXX note that this thing in its current state will not distinguish
  # between two different function applications that happen to map the
  # same input to the same output, but with different scalar
  # parameters. For example, the `subtree` function could be given two
  # different XPath queries but return the same subtree.
  #
  def resolve_transformation repo, transform, input, output = nil,
      graph: nil, params: {}, partials: {}

    # overwrite normalized params XXX replace this with something real
    params = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    # first we check the cache of partials to see if there is one that
    # matches our parameters. we want to use trasns
    partial = partials.values.select do |p|
      p.transform == transform and p.matches? params
    end.sort.first

    # find the partial if there is one
    unless partial
      partial = Partial.resolve transform: transform, params: params
      # argh this isn't right; it should be partials[transform][params]
      partials[partial.subject] = partial if partial
    end

    # collect function application receipts
    candidates = RDF::Query.new do
      # note that there is no cost-based optimization so we write
      # these in the order of least to most cardinality
      pattern [:t, TFO.output, output]
      pattern [:t, TFO.input,  input]
    end.execute(repo).map { |sol| sol[:t] }.compact.uniq.select do |s|
      repo.has_statement?(
        RDF::Statement(s, TFO.transform, transform)) or
        partial && repo.has_statement?(
        RDF::Statement(s, TFO.completes, partial))
    end.compact.uniq

    # first will be nil if this is empty so voila
    return candidates.first unless candidates.size > 1

    # now we have the unlikely case that there are two identical records
    candidates.map do |s|
      st, et = %i[startedAtTime endedAtTime].map do |p|
        repo.query([s, RDF::Vocab::PROV[p], nil]) do |stmt|
          dt = stmt.object.object
          dt if dt.is_a? DateTime
        end.compact.sort.last
      end
      [s, st, et]
    end.sort do |a, b|
      # first check latest end-time, then check latest start-time
      c = a[2] && b[2] ? b[2] <=> a[2] : 0
      # if those two yield nothing, then sort lexically i guess
      (c == 0 && a[1] && b[1]) ? b[1] <=> a[1] : a[0] <=> b[0]
    end.first.first
  end

  def record_application repo, transform, input, output, start, finish,
      partial: false, graph: nil, subject: nil, params: {}
  end

  # get transform
  def get_partial_transform repo, function, params = {}
    temp = {}
    RDF::Query.new do
      pattern [:s, RDF.type, TFO.Partial]
      pattern [:s, TFO.transform, function]
      params.keys.each { |k| pattern [:s, k, nil] }
    end.execute(repo).each do |sol|
      t = temp[sol[:s]] ||= {}
      params.keys.each do |k|
        # make these a set for now cause we don't care about the
        t[k] = Set.new(repo.query([sol[:s], k, nil]).objects)
      end
    end

    # now we imagine massaging the candidates' parameters so they
    # match the input (eg sets/arrays or whatever)

    # (in this case the input params are made to match the retrieved params)
    newp = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    # sort this because we want it to return the same thing every time
    # if there are multiples for some reason
    temp.keys.sort.each do |k|
      # do a cheaper comparison first
      next unless temp[k].keys.sort == params.keys.sort
      #
      return k if temp[k] == newp
    end

    nil
  end

  class XPath < self
    protected

    def execute input, parsed = nil, params
      xpath  = params.fetch(:xpath, []).first or raise
      prefix = params.fetch(:prefix, []).map do |x|
        x.value.split(/\s*:\s*/, 2)
      end.to_h.transform_keys(&:to_sym)
      reindent = (params.fetch(:reindent).first || RDF::Literal(true)).object

      begin
        parsed ||= Nokogiri.XML input
      rescue Nokogiri::SyntaxError
        # XXX i dunno, raise?
        return
      end

      doc = Intertwingler::Util.subtree parsed,
        xpath.value, prefixes: prefix, reindent: reindent

      return unless doc

      [doc.to_xml, doc]
    end

    public

    def implemented?
      true
    end
  end

  class XSLT < self
    protected

    def init_implementation harness
      root = harness.root
      raise ArgumentError,
        "Need a root to initialize the implementation" unless root
      root = Pathname(root).expand_path unless root.is_a? Pathname
      raise ArgumentError, "#{root} is not a readable directory" unless
        root.directory? and root.readable?

      # XXX this assumes this is a file URI but so far that is the
      # only way we get here
      filename = root + implementation.path
      raise ArgumentError, "#{filename} is not a readable file" unless
        filename.file? and filename.readable?
      @sheet = Nokogiri::XSLT(filename.read)
    end

    def execute input, parsed = nil, params
      begin
        parsed ||= Nokogiri.XML input
      rescue Nokogiri::SyntaxError
        # XXX i dunno, raise?
        return
      end

      # XXX do we wanna allow params?
      out = @sheet.transform parsed

      # now return string and still-parsed
      [@sheet.serialize(out), out]
    end

    public

    def implemented?
      true
    end

  end
end
