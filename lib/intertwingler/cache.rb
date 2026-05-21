require_relative 'storable'
require_relative 'loggable'
require_relative 'field'
require_relative 'representation'

require 'digest'
require 'uri/ni'
require 'zlib'

# This is a toy cache index. Its job is to map cache keys to
# cryptographic digests. It uses Intertwingler's onboard
# content-addressable store to handle the blobs of message bodies, and
# deals with headers and other metadata on its own.
#
# One thing we have to avoid in this design is sending response bodies
# _back_ to the content-addressable store they just came from. Take
# for instance the planned case of a faux-static handler which
# resolves human-readable URIs to content-addressed blobs. Top
# candidate for the moment is RFC9530 headers, followed by a
# `Content-Location` or `ETag` containing an RFC6920 address.
# Realistically we can check all of these.
#
# @note I'm not sure if this belongs as an {Intertwingler::Handler} or
#  not, since it could conceivably have the same interface as one.
#
class Intertwingler::Cache
  include Intertwingler::Storable
  include Intertwingler::Loggable

  private

  # request methods show up here if they are cacheable in principle
  # and the value refers to whether the request body is to be
  # considered in the cache.
  METHODS = { GET: false, HEAD: false, QUERY: true }
  SAFE = Set[*%w[GET HEAD OPTIONS PRI PROPFIND QUERY REPORT SEARCH TRACE]]
  # dunno if we need this but we'll chuck it in to save having to go get it
  # IDEM = Set[*%w[ACL BASELINE-CONTROL BIND CHECKIN CHECKOUT COPY DELETE
  #                LABEL LINK MERGE MKACTIVITY MKCALENDAR MKCOL MKREDIRECTREF
  #                MKWORKSPACE MOVE ORDERPATCH PROPPATCH PUT REBIND UNBIND
  #                UNCHECKOUT UNLINK UNLOCK UPDATE UPDATEREDIRECTREF
  #                VERSION-CONTROL]]

  # list of response codes that are okay to cache irrespective of
  # `Cache-Control` or `Expires` header, followed by list of codes
  # that are unconditionally never okay to cache. note that 206 would
  # be in the OK bucket if we cached ranges but lol @ that
  CACHE_OK = Set[200, 203, 204, 300, 301, 404, 405, 410, 414, 501]
  # these responses are never cacheable (except maybe 206 one day)
  NO_CACHE = Set[206, 401, 407, 412, 416, 503]

  # been typing this a lot lol
  F = Intertwingler::Field

  # Determine whether the request is authenticated.
  #
  # @param req [Rack::Request, Hash, false, true, nil] the request or
  #  environment or pass-through value
  #
  # @return [false, true] whether the request is authenticated
  #
  def authed? req
    user = case req
           when Rack::Request then req.env['REMOTE_USER']
           when Hash then req['REMOTE_USER']
           when false, true then return req
           when nil then return false
           else
             raise ArgumentError, "Can't act on a #{req.class}"
           end

    # coerce to string; empty means no
    !user.to_s.strip.empty?
  end

  # Determine if the response is public.
  #
  # @param resp [Rack::Response] the response
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated (or the request itself)
  #
  # @return [false, true] whether the response is public
  #
  def public? resp, authed: false
    # assume the response is private if the request is authenticated
    # and the response's `Cache-Control` is not explicitly public;
    # likewise assume it is private if the request is not
    # authenticated and the response is explicitly private

    # get the cache-control header from the response
    cc = F['cache-control'][resp] || {}
    authed?(authed) ? cc.key?(:public) : !cc.key?(:private)
  end

  # Determine if the response is private.
  #
  # @param resp [Rack::Response] the response
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated (or the request itself)
  #
  # @return [false, true] whether the response is private
  #
  def private? resp, authed: false
    !public?(resp, authed: authed)
  end

  # Get an absolute `Content-Location` header for the response, if
  # present. Turn it into an RFC6920 URI if possible.
  #
  # @param resp [Rack::Response] the response
  # @param uri [URI, #to_s, Rack::Request] where to get the base URI from
  #
  def absolute_cl resp, base
    base = req.url if base.is_a? Rack::Request
    base = URI(base.to_s) unless base.is_a? URI

    if cl = F['content-location'][resp, base]
      # now we resolve
      if out.scheme.downcase.start_with? 'http' and
          m = %r{^/+\.well-known/+ni/+([^/]+)/+(.*)}.match(out.path)
        algo, hash = m.captures
        return URI("ni:///#{algo};#{hash}") if URI::NI.valid_algo? algo
      end

      out
    end
  end

  # Return a harmonized set of `Cache-Control` values for the message.
  #
  # @param message [Rack::Request, Rack::Response]
  # @param reqt [nil, Time] time request was issued
  # @param respt [nil, Time] time response was received
  #
  # @return [Hash] cache-control directives
  #
  def cache_control message, authed: false, reqt: nil, respt: nil
    case message
    when Rack::Request
      # get both `Cache-Control` and obsolete `Pragma` header
      cc, pragma = %w[cache-control pragma].map do |hdr|
        F[hdr][message] || {}
      end

      # cache-control supersedes pragma
      pragma.slice(:'no-cache').merge cc
    when Rack::Response
      authed = authed?(authed)
      # expires header
      respd = F['date'][message]
      exp   = F['expires'][message]
      cc    = F['cache-control'][message] || {}
    else
      raise ArgumentError,
        "Rack::Request or Rack::Response only, not #{message.class}"
    end
  end

  # Validate that the request (from the client) is cacheable.
  #
  # @param req [Rack::Request] the request
  #
  # @return [false, true] whether the request is cacheable
  #
  def cacheable_req? req, cc = nil
    # method
    return false unless METHODS.key? req.request_method.to_sym

    cc ||= cache_control req

    # get both `Cache-Control` and obsolete `Pragma` header
    cc, pragma = %w[cache-control pragma].map do |hdr|
      F[hdr][req] || {}
    end

    # cache-control supersedes pragma
    cc = pragma.slice(:'no-cache').merge cc

    %i[no-cache no-store].any? { |k| cc.key? k } ? false : true
  end

  # Validate that the response (from the origin) is cacheable.
  #
  # @param resp [Rack::Response] the response
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated, or just pass in the request itself
  #
  # @return [false, true] whether the response is cacheable
  #
  def cacheable_resp? resp, authed: false
    # response code
    return false if NO_CACHE.include?(resp.status)
    # headers
  end

  # Determine if the request method is *safe*, per RFC9110.
  #
  # @param meth [#to_s, Rack::Request] the request method or the
  #  entire request
  #
  # @return [false, true]
  #
  def safe? meth
    meth = meth.request_method if meth.is_a? Rack::Request
    SAFE.include?(meth.to_s)
  end

  # Determine if the request method is *not safe*, per RFC9110.
  #
  # @param meth [#to_s, Rack::Request] the request method or the
  #  entire request
  #
  # @return [false, true]
  #
  def unsafe? meth
    meth = meth.request_method if meth.is_a? Rack::Request
    !SAFE.include?(meth.to_s)
  end

  # def modified? req, resp
  # end

  # def stale? resp
  # end

  # Determine whether the response can be served stale, optionally
  # given the request, a reference time (which defaults to now), an
  # age (if not specified in the `Age` header in the presumably-cached
  # response), and a cache directive to focus on.
  #
  # @note This will always return true if the response is still fresh.
  #
  # @param resp [Rack::Response] the (presumably cached) response
  # @param request [Rack::Request] the concomitant request
  # @param directive [Symbol, #to_sym] a particular directive
  # @param time [Time] a reference time
  # @param age [Integer, nil] an age, in seconds, which supplants `Age`
  #
  def serve_stale? resp, request: nil, directive: nil, time: Time.now, age: nil
    age ||= F['age'][resp] || 0
    # if the age is zero then it's ipso facto fresh
    return true if age == 0

    pub = public? resp, authed: request

    if directive
    else
    end
  end

  # Unconditionally fetch a response, assuming the request has already
  # been verified as cacheable.
  #
  # This metehod
  #
  # @param req [Rack::Request] the request to match
  #
  # @return [Array(Integer, Hash, URI::NI), nil] the parts
  #  for the cached response
  #
  def fetch_internal req
    raise NotImplementedError, 'drivers must implement `fetch_internal`'
  end

  # Unconditionally cache a response, assuming both it and the
  # concomitant request have already been verified as cacheable.
  #
  # @param req [Rack::Request] the request to match
  # @param resp [Rack::Response] the response to store (maybe)
  #
  # @return [Rack::Response] the response passed in as `resp`
  #
  def cache_internal req, resp
    raise NotImplementedError, 'drivers must implement `cache_internal`'
    # store the body

    # XXX note the response could indicate that its body already
    # exists in the store, eg by a Content-Location with an rfc6920
    # address, or etag, or rfc9530 Content-Digest/Repr-Digest headers

    # construct the cache key

    # construct the cache metadata payload

    # actually store the body payload?

    # return the response
  end

  # Freshen the response timestamps and other elements on 304 responses.
  #
  def freshen_internal req
    raise NotImplementedError, 'drivers must implement `expire_internal`'
  end

  # Forcibly expire an entry, either by full request or by URI.
  #
  # @param arg [Rack::Request, URI, String] the key to expire
  #
  # @return [void]
  #
  def expire_internal arg
    raise NotImplementedError, 'drivers must implement `expire_internal`'
  end

  # Cache (and optionally execute) an upstream response.
  #
  # @param req [Rack::Request] the request to match
  # @param resp [Rack::Response, nil] the response to store (maybe)
  # @param reqt [nil, Time] the time (from our point of view) the
  #  request was initiated or received
  # @param respt [nil, Time] the time (from our point of view) the
  #  response to the concomitant request was received
  # @param force [false, true] whether to force the cache to store an
  #  un-cacheable request/response
  # @param block [Proc, #call] the origin/upstream request handler
  # @yieldparam req [Rack::Request] the same request
  # @yieldreturn [Rack::Response] the response
  #
  # @raise [ArgumentError] if the response is missing or the block
  #  does not return one
  #
  # @return [Rack::Response] the response passed in as `resp`
  #
  def maybe_cache req, resp = nil, reqt: nil, respt: nil, force: false, &block
    if resp
      nil
    elsif block
      # if a response is passed in then it's a cache entry, and respt
      # is its own response time
      if resp
        cached = resp
        cachet = respt || Time.now
        respt  = nil # unset because we need a new one
      end

      # get the time of the (sub?)request
      reqt ||= Time.now

      # execute the origin (or at least upstream) handler
      resp = block.call req

      raise ArgumentError,
        "Block must return a Rack::Response, not #{resp.class}" unless
        resp.is_a? Rack::Response

      # get the time of the response
      respt ||= Time.now
    else
      raise ArgumentError, 'either supply an explicit response or a block'
    end

    # this is an error of course
    if resp.status >= 400
      # handle `stale-if-error` cache directive
      if cached && serve_stale?(
        cached, request: req, directive: :'stale-if-error')
        log.debug(
          "Serving stale cache on error for #{req.request_method} #{req.url}")
        return cached
      end
    elsif force || cacheable_resp?(resp)
      log.debug "Caching #{req.request_method} #{req.url}"
      # resp may get replaced during processing so we return it
      return cache_internal req, resp
    elsif unsafe? req
      # rfc9111 §4.4 must expire an entry after successful unsafe method
      log.debug "Expiring cache for #{req.url} after #{req.request_method}"
      expire req.url
    end

    # freshen the age if we get here

    resp
  end

  # Emit a 504 Gateway Timeout error for `only-if-cached`.
  #
  # @param text [nil, #to_s] the message text.
  #
  # @return [Rack::Response] the 504 response
  #
  def error_504 text = nil
    text ||= 'Cached request not found.'
    headers = {
      'content-type'     => 'text/plain',
      'content-language' => 'en',
      'content-length'   => text.to_s.b.size,
    }
    Rack::Response[504, headers, [text]]
  end

  public

  # Initialize the cache index.
  #
  # @param engine [Intertwingler::Engine] a back-reference to the engine
  # @param options [Hash] downstream options
  #
  # @return [void]
  #
  def initialize store: nil, log: nil, driver: :LMDB, **options
    raise ArgumentError, "`store` must be a Store::Digest" unless
      store.is_a? Store::Digest
    @store = init_store store
    @log   = log

    # warn self.store
    # warn options

    # bolt on the driver or whatever
    mod = /^(?:Driver)?::/.match?(driver.to_s) ? driver : "Driver::#{driver}"
    raise ArgumentError, "No driver #{mod}" unless self.class.const_defined? mod
    mod = self.class.const_get(mod)

    # initialize
    self.log.debug "Loading cache driver #{mod}"

    extend mod

    bootstrap **options
  end

  # Attempt to cache the response associated with a request.
  #
  # @note here is where it would be really handy to have something
  #  like perl's `HTTP::Message` but that implemented the rack spec.
  #
  # @param req [Rack::Request] the request to match
  # @param resp [Rack::Response, nil] the response to store (maybe)
  # @param force [false, true] whether to force the cache to store an
  #  un-cacheable request/response
  # @param block [Proc, #call] the origin/upstream request handler
  # @yieldparam req [Rack::Request] the same request
  # @yieldreturn [Rack::Response] the origin response
  #
  # @return [Rack::Response] the response passed in as `resp`
  #
  def cache req, resp = nil, force: false, &block
    # validate the request and response as cacheable
    unless cacheable_req? req
      resp ||= block.call req
      return resp
    end

    # this will return a Rack::Response
    maybe_cache req, resp, force: force, &block
  end

  # Attempt to fetch a cache entry based on a given request. Returns
  # `nil` if a fresh cache entry can't be resolved.
  #
  # @param req [Rack::Request] the request in question.
  # @param block [Proc, #call] the origin/upstream request handler
  # @yieldparam req [Rack::Request] the same request
  # @yieldreturn [Rack::Response] the origin response
  #
  # @return [Rack::Response, nil] the cached response (maybe)
  #
  def fetch req, &block
    # get cache-control header from request
    reqcc = cache_control req
    m = req.request_method
    miss = "Cache miss on #{m} #{req.url}:"

    # bail out early if request is not cacheable
    unless cacheable_req? req
      log.debug "#{miss} request not cacheable"
      # 504 only-if-cached
      return error_504 if reqcc.key? :'only-if-cached'
      return
    end

    # this should also take care of expiration
    status, headers, cl, cachet = fetch_internal(req)
    unless status
      log.debug "#{miss} not found"
      # XXX only-if-cached
      return error_504 if reqcc.key? :'only-if-cached'
      return
    end

    # ok things that need to happen here:
    # * ensure we get a handle on the response body if applicable
    # * if the body is missing from the store, we consider that a
    #   cache miss and bail
    # we should get the body at this point to make sure we have it
    if cl.is_a? URI::NI
      if blob = store.get(cl)
        unless body = blob.content
          log.warn "#{miss} #{cl} appears deleted"
          return
        end
      else
        log.warn "#{miss} store is missing #{cl}"
        return
      end
    else
      body = []
    end

    log.debug "Cache hit on #{m} #{req.url} with #{cl}"

    # may as well construct the response at this point
    resp = Rack::Response[status, headers.transform_values(&:to_s), body]

    # the response is fresh, we can return it
    return resp if freshness >= 0

    # nothing else to do without a block to call
    unless block
      log.warn "Stale cache entry for #{m} #{req.url} and no block given"
      return
    end

    # okay we have a stale response and a block, so the zeroth thing
    # we do is doctor the request to have an `If-Modified-Since`
    # header which should either be the Last-Modified of the cache entry, unless
    # the request already has one which is newer (although i'm
    # thinking if it did we wouldn't even get here)
    subreq = req.dup
    srt = [cachet, F['last-modified'][resp], F['if-modified-since'][req]].last
    subreq.set_header 'HTTP_IF_MODIFIED_SINCE', cachet.httpdate

    # first we test if `stale-while-revalidate` is under the threshold
    # and then we run the validating request in a thread while
    # returning the cached one
    if serve_stale?(resp, request: req, directive: :'stale-while-revalidate', time: cachet)
      # run the validation request in a thread
      Thread.new { maybe_cache subreq, resp, respt: cachet, &block }
      return resp
    end

    # finally we test if `must-revalidate` or `proxy-revalidate` (if
    # cached response is public); if the response produces an error we
    # can check if `stale-if-error` doesn't exceed the timeout
    if serve_stale?(resp, request: req, directive: :'must-revalidate', time: cachet)
      # (resp and block together mean resp is the existing cache)
      return maybe_cache subreq, resp, respt: cachet, &block
    end

    nil # yes we have no bananas
  end

  # Forcibly expire an entry, either by full request or by URI.
  #
  # @param arg [Rack::Request, URI, String] the key to expire
  #
  # @return [void]
  #
  def expire arg
    expire_internal arg
  end

  # Attempt to fetch a cache entry, or otherwise run the block.
  #
  # @param req [Rack::Request] the request
  # @param block [Proc, #call] the origin/upstream request handler
  # @yieldparam req [Rack::Request] the same request
  # @yieldreturn [Rack::Response] the response if not cached
  #
  # @return [Rack::Response] the response, either from cache or the block
  #
  def fetch_or_cache req, &block
    # attempt to fetch the cache
    if resp = fetch(req, &block)
      # get cache-control header from the response
      cc = F['cache-control'][resp] || {}

      # XXX yo okay so we should deal with must-revalidate and
      # stale-while-revalidate by grabbing eg etag and last-modified
      # headers from the cached response to inform if-none-match and
      # if-modified-since

      # also if we happen to be in the window of
      # stale-while-revalidate then we can just ship the cached
      # response and fork off a thread to retrieve it

      return resp
    elsif only_if_cached? req
      return Rack::Response[504]
    else
      #
      maybe_cache req, &block
    end
  end

  # This is a little toy class that encapsulates the cache key digest
  # state, which gets bifurcated twice (authenticated vs not; with
  # `Vary` headers vs not).
  #
  class KeyState
    private

    def set_initial_state
      # get the hash for the request body:

      # get the request method
      meth = @req.request_method

      # we change the cache key input by one bit if this is the case
      if cc = F['cache-control'][@req]
        @no_transform = cc.key? :'no-transform'
      end

      # get the normalized request-URI
      uri = Intertwingler::Resolver.coerce_resource(@req.url, as: :uri)
      @ustate = (Digest::SHA256.new << uri.normalize.to_s)

      # now we should get the state
      state = @states.first

      # if the request is other than HEAD/GET:
      unless METHODS.fetch(meth.to_sym, false)

        # check to see if there's a `Content-Location` request header
        cl = F['content-location'][@req]

        # if the Content-Location contains an RFC6920 URI, then we
        # will just use that. if that fails or if otherwise it's
        # missing (likely), scan the request body i guess.

        unless cl.is_a? URI::NI
          body = @req.body

          raise TypeError,
            'request body should be a Store::Digest::Object::IOWrapper, not %s' %
            body.class unless body.is_a? Store::Digest::Object::IOWrapper

          cl = body.object[:"sha-256"]
        end

        # add the body digest to the state
        state << "#{cl.algorithm}\x1e#{cl.digest}"
      end

      # we do this to vary the key so transformed responses don't get
      # served out from `no-transform` requests
      state << [meth, uri.normalize.to_s].join(@no_transform ? "\x1d" : "\x1e")

      # if there is a user, do the initial bifurcation
      if user = @req.env['REMOTE_USER'] and not user.strip.empty?
        (@states[1] = state.dup) << "\x1e#{user}"
      end
    end

    public

    # Initialize the state object.
    #
    # @param req [Rack::Request] the request to be turned into a cache key
    # @param vary [nil, Array<String>] request headers to `Vary`
    #
    def initialize req, vary: nil
      @states = [Digest::SHA256.new, nil, nil, nil]
      @req    = req

      # just parcel this out
      set_initial_state

      # this will just invoke the accessor
      self.vary = vary if vary.is_a?(Array) && !vary.empty?
    end

    # Determine if the encapsulated request is authenticated and
    # thus whether we should attempt a private cache lookup.
    #
    # @return [false, true]
    #
    def authed?
      # if this is set then the request is authenticated
      !!@states[1]
    end

    # Return the URI state
    def uri_state
      @ustate.dup
    end

    # Set which headers were found in the response's `Vary` to be
    # used for the request.
    #
    # @param headers [Array<String>]
    #
    def vary= headers
      # set empty list by default
      headers ||= []

      headers = F['vary'].new(headers) || [] unless
        headers.is_a? Array

      # empty these out if there are no vary headers
      if headers.empty?
        @states[2] = @states[3] = nil
        return
      end

      # XXX FUTURE ME: rack 3.3 will have `headers` (which was
      # relevant here but no longer is)
      reqh = headers.each_with_object([]) do |name, a|
        if v = F[name][req]
          a << [v.field_name, v.to_s]
        end
      end.sort { |a, b| a.first <=> b.first }.to_h

      # now serialize them
      serialized = reqh.map { |k, v| "#{k}: {#v}" }.join("\x1f")

      st = [0]
      st << 1 if authed?

      st.each do |i|
        (@states[2 | i] = @states[i].dup) << "\x1e#{serialized}"
      end
    end

    # Get one of the state variants
    #
    # @param authed [false, true]
    # @param vary [nil, false, true, Array<String>]
    #
    # @return [Digest::SHA256, nil] a duplicate of the hash state,
    #  if it exists.
    #
    def state authed: false, vary: nil
      # overwrite new vary states
      if vary.is_a? Array
        self.vary = vary
        vary = false if vary.empty?
      end

      # construct the index this fun way
      i = authed ? 1 : 0
      j = vary   ? 2 : 0

      # pick which hash state we want and return a duplicate
      @states[i | j].dup
    end

    # Return a fresh SHA-256 state.
    #
    # @return [Digest::Instance]
    #
    def self.state
      Digest::SHA256.new
    end

    # Coerce a URI and return a SHA-256 digest state of it.
    #
    # @param uri [URI, RDF::URI, #to_s] the URI
    #
    # @return [Digest::Instance]
    #
    def self.uri_state uri
      uri = Intertwingler::Resolver.coerce_resource uri, as: :uri
      state << uri.normalize.to_s
    end
  end

  # The driver module is just a namespace for cache drivers.
  module Driver
    # Okay so this is super simple: we begin with a key-value store
    # with *one* table keyed by sha256 hash, and two kinds of entry:
    #
    # * "signposts" whose job it is to encode the contents of the
    #   `Vary` header,
    # * "terminals" which actually contain the cache index.
    #
    # The cache key is composed of the sha256 of the following entities:
    #
    # * message body (an empty message body is the initial sha256 state)
    # * request method
    # * fully-qualified, normalized request URI (minus scheme?)
    # * optionally `REMOTE_USER`
    # * optionally, request headers found in `Vary`.
    #
    # These fields, minus the body hash state, are delimited by the
    # ASCII record separator character (0x1e).
    #
    # (The reason why the message body goes first is because if we
    # know its hash already, we can just begin with that, and we won't
    # have to waste resources rehashing it on the end of the key.)
    #
    # The hash state is built up incrementally, and the state is saved
    # at certain checkpoints. For instance, we save a checkpoint
    # before adding the `REMOTE_USER`, and again before we add the
    # `Vary` headers. This enables us to test multiple keys
    # efficiently.
    #
    # The algorithm to generate the hash key and perform the lookup
    # goes like this:
    #
    # * add the binary SHA-256 hash of the message body, if present.
    # * add the request method (these are case-sensitive!) to the hash.
    # * if the request had `no-transform` in the `Cache-Control`
    #   header, add `\x1d`. Otherwise, add `\x1e`.
    #   (cached transformed responses should not surface by accident)
    # * normalize the request-URI (using the `Host` header) and add it.
    # * stash the hash state at this time.
    # * add `\x1e` and the value of `REMOTE_USER` if present.
    # * stash this state (for private cache entries).
    # * try to look up the "private" cache key first, if present,
    #   then the "public" one.
    # * if there is a hit at this point, determine if the record is a
    #   signpost or a terminal.
    # * if it is a signpost, extract the list of `Vary` header names.
    # * select the relevant headers from the request (which could be zero)
    # * normalize the headers, alphabetize and serialize them, using
    #   `Name: value` for the pairs, and `\x1f` between them.
    # * add `\x1e`.
    # * add the serialized headers (which may be the empty
    #   string) to whichever variant of the hash matched the signpost.
    # * now try looking up the new key. if it hits, that's the
    #   terminal entry for that resource.
    #
    # (Note the original plan was to actually hash the message body
    # first for the initial state, as I mistakenly thought you could
    # resume a SHA-256 hash. It turns out you can't, on purpose.)
    #
    # (Note as well that `KeyState` proactively generates up to all
    # four hashes based on the request object and `Vary` headers.)
    #
    # Now we discuss cache entry layouts.
    #
    # The first byte of the entry will consist of control flags. The
    # first bit in that byte determines whether the entry is a
    # signpost (1) or terminal (0). The rest of the bits flag aspects
    # of the terminal record. On a signpost record, the only things
    # that follow are an unsigned 32-bit integer to represent a count
    # of terminal headers, and the list of `Vary` headers, delineated
    # by `\x1f`. These may be compressed. (We rely on the GZip header
    # to indicate compression.)
    #
    # Here are the flags for the control byte:
    #
    # 0. record is a "signpost"
    # 1. body hash present
    # 2. `no-transform` flag (from the *response*)
    # 3. `immutable` flag
    # 4. `last-modified` timestamp present
    # 5. `max-age` delta present
    # 6. `stale-while-revalidate` delta present
    # 7. `stale-if-error` delta present
    #
    # For the terminal entries, these are the remaining fields:
    #
    # * Response code (unsigned short; we may steal up to 7 bits from it)
    # * Time of response (signed quad)
    # * Last-Modified time (signed quad; if control flag)
    # * `max-age` delta (unsigned long; if control flag; negotiated)
    # * `stale-while-revalidate` delta (unsigned long; if control flag)
    # * `stale-if-error` delta (unsigned long; if control flag)
    # * sha256 of response body (32 bytes; if control flag)
    # * serialized response headers (rest of record; may be gzipped)
    #
    # For the terminal entries, the next two bytes represent an
    # unsigned short (which we may eventually steal up to six or
    # seven bits from) containing the status code, followed by a
    # 64-bit timestamp (overkill?) representing the response time
    # measured locally the origin response, and another
    # representing.  Following those are uint32 deltas whose
    # presence are contingent on bits in the control flags for
    # `max-age`/`Expires`/`s-maxage`, `stale-while-revalidate`,
    # `stale-if-error`, followed by the sha-256 hash of the response
    # body, if present. the rest of the record is the serialized
    # header set (which may be compressed).
    #
    # The count in the signpost is for cache eviction. If you retrieve
    # a terminal record that turns out to be expired, you delete it
    # and update the concomitant signpost with a decremented count. If
    # the count (after decrementing) is zero, delete the signpost too.
    #
    # Once we have resolved the terminal entry, we can actually do
    # some fkn logic to it.

    # These methods are applicable to any key-value store.
    #
    module KVStore
      private

      # record flag fields
      IS_SIGNPOST  = 1 << 0
      BODY_PRESENT = 1 << 1
      NO_TRANSFORM = 1 << 2
      IMMUTABLE    = 1 << 3
      LAST_MOD_P   = 1 << 4
      MAX_AGE_P    = 1 << 5
      STALE_REV_P  = 1 << 6
      STALE_ERR_P  = 1 << 7

      # how long a string has to be before we try to compress it
      GZIP_LIMIT = 128

      # Pack a signpost record.
      #
      # @param count [Integer] the count of terminal records
      # @param vary [Array, String] the contents of the response `Vary` header
      #
      # @raise [ArgumentError] if `Vary` is empty we don't need a signpost
      #
      # @return [String] the packed record
      #
      def pack_signpost count, vary
        unless vary.is_a? Array
          vary = vary.to_s.strip
          raise ArgumentError, 'Vary header must be non-empty' if vary.empty?
          vary = F['vary'][vary]
        end
        raise ArgumentError, 'Vary contents must be non-empty' if vary.empty?

        vary = vary.join "\x1f"
        vary = Zlib.gzip(vary) if vary.size > GZIP_LIMIT

        [IS_SIGNPOST, count, vary].pack 'CLa*'
      end

      # Unpack a signpost record.
      #
      # @param record [String] the packed record
      #
      # @return [Array(Integer, Integer, Array<String>)] the unpacked record
      #
      def unpack_signpost record
        ctrl, count, vary = record.unpack 'Cla*'
        vary = Zlib.gunzip(vary) if vary.start_with?("\x1f\x8b")
        vary = vary.split "\x1f"

        [ctrl, count, vary]
      end

      # Detect if a record is a signpost.
      #
      # @param record [String] a packed record
      #
      # @return [false, true] whether it's a signpost
      #
      def signpost? record
        # get the control byte
        ctrl = record.is_a?(Integer) ? record : record.to_s.bytes.first
        # now check it
        (ctrl & IS_SIGNPOST).nonzero?
      end

      # Detect if a terminal record is expired.
      #
      # @param record [String]
      #
      # @return [false, true]
      #
      def rec_expired? record, now: nil
        now ||= Time.now
      end

      # Pack the headers into a string; gzip them if they are too long.
      #
      # @note We just pack them as-is; they don't need to be normalized.
      #
      # @param headers [Hash, Rack::Response] the headers or response
      #
      # @return [String] the packed headersnderstand
      #
      def pack_headers headers
        headers = headers.headers if headers.is_a? Rack::Response

        # we can use a colon to delineate headers and values
        out = headers.map { |k, v| "#{k}: #{v}" }.join("\x1f")
        (out.size > GZIP_LIMIT) ? Zlib.gzip(out) : out
      end

      # Unpack headers from the record; uncompress them if need be.
      #
      # @param string [String] the fragment of record that is headers
      #
      # @return [Hash] the headers
      #
      def unpack_headers string
        string = Zlib.gunzip(string) if string.start_with? "\x1f\x8b"
        string.split("\x1f").map { |pair| pair.split(/\s*:\s*/, 2) }.to_h
      end

      # Pack a "terminal" record.
      #
      # @param status [Integer] the response status
      # @param headers [Hash] the response headers
      # @param body [URI::NI] the sha-256 URI for the body
      # @param time [Time] the response time
      # @param authed [false, true] whether the entry is private
      #
      # @return [String] the record
      #
      def pack_terminal status, headers, body, time: Time.now, authed: false
        fmt   = +'CSq'
        ctrl  = 0 # control byte
        out   = []

        # we use the date header for reference or the response time
        # if it's missing
        dt = F['date'][headers] || time
        delta = time - dt

        if lm = F['last-modified'][headers]
          ctrl |= LAST_MOD_P
          fmt << ?q
          out << (lm + delta).to_i
        end

        # now we get the actual cache parameters
        cc = F['cache-control'][headers] || {}
        ctrl |= NO_TRANSFORM if cc.key? :'no-transform'
        ctrl |= IMMUTABLE    if cc.key? :immutable

        max = if !authed and cc[:'s-maxage'].is_a?(Integer)
                cc[:'s-maxage']
              elsif cc[:'max-age'].is_a?(Integer)
                cc[:'max-age']
              elsif ex = F['expires'][headers] and ex > dt
                (ex - dt).round
              elsif lm and lm < dt
                # 10% of last-modified to date/response/now
                ((dt - lm) * 0.1).round
              end

        if max && max >= 0
          ctrl |= MAX_AGE_P
          fmt << ?L
          out << max
        end

        { 'stale-while-revalidate': STALE_REV_P,
         'stale-if-error': STALE_ERR_P }.each do |d, flag|
          if cc[d].is_a?(Integer) && cc[d] >= 0
            ctrl |= flag
            fmt << ?L
            out << cc[d]
          end
        end

        # the penultimate thing is the body hash
        if body.is_a? URI::NI
          ctrl |= BODY_PRESENT
          fmt << 'a32'
          out << body.digest
        end

        # finally, headers
        fmt << 'a*'
        out << pack_headers(headers)
        

        ([ctrl, status, time.to_i] + out).pack fmt
      end

      # Unpack a "terminal" record.
      #
      # @param record [String]
      #
      # @return [Array(Integer, Hash, URI::NI)] status, headers, body URI
      #
      def unpack_terminal record
        ctrl, rest = record.unpack 'Ca*'
        raise ArgumentError, 'must be a terminal record' if signpost? ctrl

        # here are our little
        cc = {
          'no-transform':           (ctrl & NO_TRANSFORM).nonzero?,
          immutable:                (ctrl & IMMUTABLE).nonzero?,
          'must-understand':        (ctrl & M_UNDERSTAND).nonzero?,
          'max-age':                (ctrl & MAX_AGE_P).nonzero?,
          'stale-while-revalidate': (ctrl & STALE_REV_P).nonzero?,
          'stale-if-error':         (ctrl & STALE_ERR_P).nonzero?,
        }

        # this is the pack format
        fmt = +'SQ'
        %i[max-age stale-while-revalidate stale-if-error].each do |s|
          fmt << ?L if cc[s]
        end

        fmt << 'a32' if (ctrl & BODY_PRESENT).nonzero?
        fmt << 'a*'

        status, date, *rest = rest.unpack fmt

        %i[max-age stale-while-revalidate stale-if-error].each do |s|
          cc[s] = rest.shift if cc[s]
        end

        body, headers = rest

        body = URI("ni:///sha-256;#{[body].pack(m0).tr('+/', '-_').delete(?=)}")

        headers = unpack_headers headers

        [status, headers, body]
      end
    end

    public

    # We're sticking to LMDB because we're using it in the RDF store
    # and {Store::Digest}.
    module LMDB
      include KVStore

      require 'lmdb'

      private

      # driver-specific installation
      def bootstrap dir: 'cache', mapsize: 2**27
        dir = Pathname(dir).expand_path
        dir.mkpath unless dir.exist?

        @env = ::LMDB.new dir, mapsize: mapsize
        @index = @env.database 'index', create: true
        @uris  = @env.database 'uris',  create: true, dupsort: true
      end

      def cache_internal req, resp
        # construct the hash key state
        vary  = F['vary'][resp]
        state = KeyState.new req, vary: vary

        # we use authed key if the request is authenticated, unless
        # the response is explicitly `public`
        authed = private?(resp, authed: req)

        # if vary is nonempty we create a signpost record

        # we unconditionally create a terminal record; the only
        # difference is whether the lookup key is

        # stash the response body (unless it has a `Content-Location`
        # with an `ni:` URI, in which case it's already stashed)
        cl = absolute_cl resp, req.url

        unless cl.is_a? URI::NI
          # this turns the body from anything weird into something less weird
          body = Intertwingler::Representation::BodyWrap[resp.body]

          if ct = F['content-type'][resp]
            ct = ct.to_s # get content-type
          end

          if ce = F['content-encoding'][resp]
            ce = ce.value # get content-encoding
          end

          if lm = F['last-modified'][resp]
            lm = lm.value # get last-modified
          end

          obj = store.add body, type: ct, encoding: ce, mtime: lm
          cl  = obj[:"sha-256"]

          # replace the response ONLY if not an Intertwingler::Representation
          resp = Rack::Response[resp.status, resp.headers.to_h, obj.content]
        end

        # okay NOW
        @env.transaction do
          # create the terminal record
          tk  = state.state(authed: authed, vary: !!vary).digest
          tv  = pack_terminal resp, cl
          inc = !index.key?(tk) # determine if adding or updating
          @index.put tk, tv

          # put the terminal key in the uri index
          uk = state.uri_state.digest
          @uris.put? uk, tk

          if vary
            sk = state.state(authed: authed,  vary: false)

            count = inc ? 1 : 0
            if sv = @index.get(sk)
              count += count_signpost sv
            end

            sv = pack_signpost count, vary

            @index.put sk, sv
            # put the signpost key in the uri index
            @uris.put? uk, sk
          end
        end

        # we need to return the response because we may have overwritten it
        resp
      end

      #
      #
      # @return [nil, Array(Integer, Hash, Store::Digest::Object)]
      #
      def fetch_internal req
        state = KeyState.new req
        ukey  = state.uri_state.digest

        @env.transaction do |txn|
          if authed = state.authed?
            # first attempt to get a private entry
            key = state.state(authed: true).digest
            # try public record
            authed = false unless raw = @index.get(key)
          end

          # attempt to get the public record
          raw ||= @index.get(key = state.state.digest)

          # okay there really isn't anything here
          break if raw.nil? or raw.empty?

          if is_sp = signpost?(raw)
            count, vary = unpack_signpost raw
            tkey = state.state(authed: authed, vary: vary).digest
            raw  = @index.get tkey

            # if this is nil then there's nothing here
            break unless raw
          else
            tkey  = key
            count = 0
            vary  = nil
          end

          # first we measure if the response is objectively stale,
          # then we determine if the client will accept it. the
          # client may reject a record that's still fresh, or
          # accept a record that's stale, depending on the
          # request's `Cache-Control` parameters.

          # if the response is still fresh and the client will
          # accept it, we could actually return 304 here

          # if the response is stale then expire the record

          # if 


          # all expired is stale but not all stale is expired

          if rec_expired? raw
            # delete the expired terminal
            @index.delete tkey
            @uris.delete ukey, key

            # now we decrement the refcount
            if key != tkey
              if count > 1
                sp = pack_signpost count - 1, vary
                @index.put key, sp
              else
                @index.delete key
                @uris.delete? ukey, key
              end
            end

            # now we break out
            break
          end

          # return the terminal record
          unpack_terminal raw
        end
      end
    end

    def expire_internal arg
      if arg.is_a? Rack::Request
        state = KeyState.new arg
        ukey = state.uri_state.digest
      else
        ukey = KeyState.uri_state(arg).digest
      end

      # XXX honestly it's probably best to just obliterate the whole
      # URI until we can determine a way to disambiguate between
      # individual responses or even if it makes sense to

      # if state
      #   @env.transaction do
      #     [false, true].each do |auth|
      #       key = state.state(authed: auth).digest
      #       raw = @index.get key
      #       if signpost?(raw)
      #         vary
      #         @uris.delete? ukey, tkey
      #       end
      #       @index.delete? key
      #       @uris.delete? ukey, key
      #     end
      #   end
      # else
      @env.transaction do
        @uris.each_value(ukey) { |ikey| @index.delete? ikey }
        @uris.delete? ukey
      end
      # end
    end

    # we can do redis or whatever later lol
  end

end
