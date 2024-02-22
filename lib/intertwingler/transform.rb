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

  ITCV = Intertwingler::Vocab::ITCV
  TFO  = Intertwingler::Vocab::TFO
  CI   = Intertwingler::Vocab::CI
  OWL  = ::RDF::OWL

  def engine ; harness.dispatcher.engine ; end

  def repo; engine.repo; end

  def resolver ; engine.resolver ; end

  def registry ; engine.registry ; end

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
    @subject = resolver.coerce_resource subject
    @impl    = resolver.coerce_resource implementation if implementation

    # we have to coerce params/accepts/returns
    @params  = registry[@subject] = coerce_params params
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
    resolver.coerce_resource @impl, as: as if @impl
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

    # note overloaded assignments do not return values so you can't
    # just daisy-chain these two statements
    registry[subject] = params
    @params = registry[subject]

    # get the accepts
    @accepts = fetch_types TFO.accepts

    # get the returns
    @returns = fetch_types TFO.returns

    # get the preferred serialization
    @prefers = fetch_types(TFO.prefers).sort.first

    self
  end

  # Determine if the _transform_ accepts the message body's content
  # type.
  #
  # @param type [#to_s] the message body content type
  #
  # @return [false, true] whether the transform can parse it.
  #
  def accepts? type
    type     = MimeMagic[type.to_s.downcase]
    headers  = { Accept: @accepts.map { |t| t.to_s.downcase } }
    variants = { type.to_s => { type: type.to_s } }

    headers[:Accept] << type.to_s if !headers[:Accept].include?(type.to_s) and
      headers[:Accept].any? { |t| type.descendant_of? t }
    # don't forget the magic bullet
    headers[:Accept] << '*/*;q=0'

    # warn "headers: #{headers.inspect}, variants: #{variants.inspect}"

    # warn "negotiated: #{HTTP::Negotiate.negotiate(headers, variants).inspect}"

    !!HTTP::Negotiate.negotiate(headers, variants)
  end

  # Determine if the transform is capable of producing content the
  # _caller_ will accept.
  #
  # @param types [Array<#to_s>] content types, either an `Accept:`
  #  header or split into an array.
  #
  # @return [false, true] whether the transform can satisfy the caller.
  #
  def returns? *types
    headers  = { Accept: types }
    variants = @returns.map do |t|
      t = t.to_s.downcase
      [t, { type: t }]
    end.to_h

    !!HTTP::Negotiate.negotiate(headers, variants)
  end

  # Determine in one shot if the transform can process the message.
  #
  # @param header [#to_s, Array<#to_s>] the `Accept:` header, either
  #  whole or split into an array.
  # @param type [#to_s] the message body `Content-Type`.
  #
  def can_process? header, type
    returns?(header) and accepts?(type)
  end

  # Process a set of parameters.
  #
  # @param values [Object] can't remember what the type is
  #
  # @return [Params::Registry::Instance]
  def process values
    # warn @params.inspect
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

    def repo; transform.harness.dispatcher.engine.repo; end

    def resolver ; transform.harness.dispatcher.engine.resolver ; end

    public

    def self.configure harness, subject, **rest
      # get transform URI
      transform = harness.dispatcher.engine.repo.objects_for(
        subject, TFO.transform, only: :resource).sort.first

      raise Intertwingler::Error::Config,
        "partial #{subject} has no transform" unless transform

      # get transform object
      transform = harness.resolve transform

      new(transform, subject: subject, **rest).refresh
    end

    def initialize transform, subject: nil, slug: nil, aliases: [], values: {}
      @transform = transform
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
        @params = transform.process(
          transform.params.keys.reduce({}) do |hash, k|
            hash[k] = objects(k, entail: false).map do |o|
              o.literal? ? o.object : o.to_s
            end

            hash
          end)
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

    def repo; harness.dispatcher.engine.repo; end

    def initialize_copy *args
      # warn 'sup lol'
      @transforms = @transforms.dup
      super
    end

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

    attr_reader :harness, :subject, :next
    alias_method :id, :subject

    # Refresh the queue against the graph.
    #
    # @return [self]
    #
    def refresh
      # deal with list
      if list = blanks(TFO['member-list']).sort.first
        list = ::RDF::List.new(graph: repo, subject: list).to_a
        @transforms = list.map do |member|
          harness.resolve member, queues: false, partials: true, refresh: true
        end
      else
        @transforms = []
      end

      # deal with next queue
      if qnext = resources(TFO.next).sort.first
        # qnext = harness.resolve qnext,
        #   transforms: false, partials: false, refresh: true
        # XXX here is where we should check for cycles
        @next = qnext
      else
        @next = nil
      end

      self
    end

    # This is like Array#push.
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

    # Test if the transform accepts the `Content-Type`. Intended to
    # provide a hook to subclasses to raise an error if false.
    def can_serve! transform, type
      # XXX change this when we have a regime we can stand
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
      dispatcher = harness.dispatcher
      engine     = dispatcher.engine
      resolver   = engine.resolver
      log        = engine.log

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
      type = out.content_type || 'application/octet-stream'
      # type = out.get_header( # so is this and that is dumb af
      #   out.is_a?(Rack::Request) ? 'CONTENT_TYPE' : 'content-type') ||
      #   'application/octet-stream'

      type = MimeMagic[type].canonical || MimeMagic['application/octet-stream']

      log.debug "#{resp ? 'response' : 'request'} type #{type.inspect}"

      # don't screw around dupping the message if there are no transforms
      return out if @transforms.empty?

      # transforms could be transforms or they could be partials; in
      # the case that they're partials, we want to pull the parameters
      # out and append them to the uri
      @transforms.each_with_index do |transform, i|
        if transform.is_a? Intertwingler::Transform::Partial
          params    = transform.params.to_h
          transform = transform.transform
        else
          params = {}
        end

        # we just override this in strict
        next unless can_serve! transform, type

        # this already should be a uuid but eh
        uuid = resolver.uuid_for transform.subject, as: :uri
        # first check if the transform matches typewise
        # XXX TODO handle `message/http`; not doing that one yet lol
        uri = URI(req.base_url) + uuid.uuid

        uri.query = URI.encode_www_form(params) unless params.empty?

        log.debug "about to POST #{type} to #{uri}"

        subreq  = engine.dup_request req, uri: uri, method: :POST,
          headers: { 'content-type' => type.to_s }, body: body
        subresp = dispatcher.dispatch subreq, subrequest: true

        # here is where we would break if this was an error or redirect
        unless subresp.successful?
          # XXX any failure in here is bad except 304 which is considered a noop
          next if subresp.status == 304

          # basically no matter what the situation is here it's a
          # misconfiguration
          raise Intertwingler::Handler::Error::Server,
            "Transform #{uuid} returned error #{subresp.status}: #{subresp.body}"

          # XXX HANDLE REDIRECTS WITH DEPTH LIMIT/CYCLE DETECTION: a
          # redirect that is to the same URI but different
          # parameters/values is legitimate and should bubble up IF
          # AND ONLY IF the queue is addressable, otherwise we should
          # silently follow the redirects (and blow up if it loops).

        end

        # warn "dios mio: " + subresp.inspect

        # reassign content type
        if type = subresp.get_header('content-type')
          type  = MimeMagic[type]
          btype = subresp.body.type if subresp.body.respond_to? :type
          type = btype if btype and btype != type and btype.descendant_of? type
          # warn "btype: #{btype.inspect} #{btype.descendant_of? type}"
        else
          raise Intertwingler::Handler::Error::Server,
            'Transform #{uuid} did not return a Content-Type header'
        end

        # reassign body
        body = subresp.body

        # XXX content-encoding content-language etc etc
      end

      # these headers are the same for either request or response
      hdr = { 'content-type' => type.to_s, 'content-length' => nil }
      hdr['content-length'] = body.size.to_s if body.respond_to? :size

      # we shouldn't do this if nothing has been run
      if resp
        out = engine.replace_response_body out, body, headers: hdr
      else
        out = engine.dup_request req, body: body, headers: hdr
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
    def initialize harness, queue = nil
      @harness = harness
      @queues  = {}

      if queue and qobj = harness.resolve(queue)
        @queues[queue] = qobj.dup
        while queue = qobj.next
          qobj = harness.resolve(queue)

          raise Intertwingler::Error::Config,
            "Cycle detected in queue #{queue}" if @queues.key? queue
          @queues[queue] = qobj.dup
        end
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
    #  applicable.
    #
    # @return [Rack::Request, Rack::Response] the transformed message.
    #
    def run request, response = nil
      message = response || request

      @queues.values.each do |q|
        message = q.run request, response do |event|
          # do stuff with side effects
        end

        # don't forget to reassign or this hands the unchanged one down the line
        if response
          response = message
        else
          request = message
        end

        # warn "in chain loop (#{q.subject}): #{message.content_type}"
      end

      message
    end

    # A request chain differs from a response chain insofar as it
    # stores up state from request transforms to pass on to the
    # response transforms. A factory method {#response_chain} then
    # produces the appropriate chain to complete the response
    # transforms.
    class Request < self

      # Apply all the request queues to the request (and collect any
      # side effects).
      #
      # @param request [Rack::Request] an HTTP request.
      #
      # @return [Rack::Request] the transformed request.
      #
      def run request
        super request
      end

      # Return the appropriate response chain for the given handler.
      #
      # @param handler [RDF::URI, URI] the address of the handler selected
      #  by the dispatcher.
      # @param pp [nil, Array] optional path parameters
      #
      # @return [Intertwingler::Transform::Chain::Response] the response chain.
      #
      def response_chain handler, pp: nil
        head = @harness.queue_head_for handler
         warn "generating response chain for #{handler} lol: #{head.inspect}"
        Intertwingler::Transform::Chain::Response.new @harness, head,
          pp: pp, insertions: @insertions
      end
    end

    # A response chain extends the abstract chain class with methods
    # used to manipulate insertion events and addressable queues.
    class Response < self

      # Initialize a response chain.
      #
      # @param harness [Intertwingler::Transform::Harness]
      # @param head [RDF::URI]
      # @param pp [nil, Array]
      # @param insertions [nil, Array]
      #
      def initialize harness, head, pp: nil, insertions: nil
        super harness, head
        set_addressable(*pp) if pp
        apply_insertions insertions if insertions
      end

      # Transform the response.
      #
      # @param request [Rack::Request] an HTTP request.
      # @param response [Rack::Response] an HTTP response.
      #
      # @return [Rack::Response] the transformed response.
      #
      def run request, response
        super request, response
      end

      # Determine if the chain contains an addressable queue.
      #
      # @return [false, true] whether or not the chain has an
      #  addressable queue.
      #
      def has_addressable?
        @queues.values.any? { |q| q.addressable? }
      end

      # Set the addressable queue in the chain.
      def set_addressable *pp
        warn "path parameters: #{pp}"
        self
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

    private

    def repo ; dispatcher.engine.repo ; end
    def subject ; dispatcher.engine.subject ; end
    def resolver ; dispatcher.engine.resolver ; end

    public

    # Inititalize the harness and populate it with configuration from
    # the graph.
    #
    # @param dispatcher [Intertwingler::Engine::Dispatcher]
    #
    # @return [Intertwingler::Transform::Harness]
    #
    def self.configure dispatcher
      new(dispatcher).refresh
    end

    # Initialize an empty harness. This will do nothing and it must be
    # populated separately.
    #
    # @param dispatcher [Intertwingler::Engine::Dispatcher]
    # @param queues [Array]
    # @param request_head [RDF::URI] the default request queue head
    # @param response_head [RDF::URI] the default response queue head
    #
    def initialize dispatcher, queues: [], request_head: nil, response_head: nil
      # the dispatcher
      @dispatcher = dispatcher

      # maps
      @queues      = {}
      @transforms  = {}
      @partials    = {}
      @handler_map = {}

      # queue heads
      @request_head  = resolver.coerce_resource request_head  if request_head
      @response_head = resolver.coerce_resource response_head if response_head
    end

    # Refresh (recursively) the configuration in the graph.
    #
    # @return [self]
    #
    def refresh
      @request_head  = resources(ITCV['request-queue']).sort.first
      @response_head = resources(ITCV['response-queue']).sort.first

      # this is probably enough to get things started, actually
      qs = [@request_head, @response_head]

      while q = qs.shift
        # warn "resolving queue #{q}"
        qobj = resolve q, transforms: false, partials: false, refresh: true
        qs << qobj.next if
          qobj.next and !qs.include?(qobj.next) and !@queues.key?(qobj.next)
      end

      #

      self
    end

    private

    # just making a mess here really

    def resolve_queue uri, state, force: nil
      return @queues[uri] if @queues.key? uri and !force
      @queues[uri] = Intertwingler::Transform::Queue.configure self, uri
    end

    def resolve_transform uri, state, force: nil
      return @transforms[uri] if @transforms.key? uri and !force

      tfi = @transforms[uri] = Intertwingler::Transform.configure self, uri

      # XXX this will not be necessary with manifest protocol; the
      # dispatcher will just ask for the handler's manifest.
      dispatcher.add_route uri, tfi.implementation, method: :POST

      tfi
    end

    def resolve_partial uri, state, force: nil
      return @partials[uri] if @partials.key? uri and !force

      @partials[uri] = Intertwingler::Transform::Partial.configure self, uri
    end

    def resolve_handler uri, state, force: nil
      engine.dispatcher.add uri, queues: false
      if queue = repo.objects_for(uri, ITCV.queue, only: :resource).sort.first
        queue = resolve_queue queue, state
        @handlers[uri] = queue
      end
    end

    def resolve_one uri, state, force: nil
      ts = repo.asserted_types uri
      if repo.type_is?(ts, TFO.Partial) and !repo.type_is?(ts, TFO.Invocation)
        resolve_partial uri, state, force: force
      elsif repo.type_is? ts, TFO.Function
        resolve_transform uri, state, force: force
      elsif repo.type_is? ts, TFO.Queue
        resolve_queue uri, state, force: force
      else
        raise ArgumentError,
          "Not sure what to do with #{uri} of type #{ts.join ?,}"
      end
    end

    public

    def resolve uri, queues: true, transforms: true, partials: true,
        refresh: false
      uri = resolver.coerce_resource uri
      state = {}
      resolve_one uri, state
    end

    attr_reader :dispatcher, :request_head, :response_head
    # @!attribute [r] dispatcher
    #  @return [Intertwingler::Engine::Dispatcher] The dispatcher from
    #   the engine.

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

    # Return the request transform chain.
    #
    # @return [Intertwingler:Transform::Chain::Request]
    #
    def request_chain
      # warn "request head: #{request_head.inspect}"
      Intertwingler::Transform::Chain::Request.new self, request_head
    end

    def queue_head_for handler
      handler = resolver.coerce_resource handler
      @handler_map[handler] || response_head
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

      # warn "from inside transform handler: #{type}"

      # give us a default response
      resp = Rack::Response[404, {}, []]

      # get the resolver for this request or bail out
      resolver = engine.resolver

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

      urn = RDF::URI('urn:uuid:' + uuid)
      if pgroup = engine.registry[urn]
        # warn "params: #{params.inspect}"
        params = pgroup.process params
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

        # warn "still inside: #{out.type}"

        # this should be it
        return Rack::Response[200, {
          'content-type'   => out.type.to_s,
          'content-length' => out.size.to_s }, out]

      rescue Intertwingler::Transform::ParamError
        return Rack::Response[409, {}, []]
      rescue Intertwingler::Handler::Redirect => r
        # umm i dunno
        # r.location
        return r.response
      rescue Intertwingler::Handler::Error => r
        # umm i dunno
        # r.location
        return r.response
      end
    end
  end

end
