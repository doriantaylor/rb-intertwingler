require_relative 'error'
require_relative 'storable'
require_relative 'loggable'
require_relative 'field'
require_relative 'representation'

require 'digest'
require 'uri/ni'
require 'zlib'
require 'time'

# This is a toy cache index. Its job is to map cache keys to
# cryptographic digests. It uses a content-addressable store,
# {Store::Digest}, to handle the blobs of message bodies, and deals
# with headers and other metadata on its own.
#
# The role of a cache is to mediate between the _downstream_ client
# and the _upstream_ server, either or both of which can conceivably
# be other caches. We will refer to them, however, as they are in the
# simplest case, as _client_ and _origin_, respectively. Whether a
# response is _cached_ (notwithstanding a `no-store` `Cache-Control`
# directive in the request) is up to the origin, and whether a cached
# response is _served_ is ultimately up to the client—as long as the
# request does not exceed the limitations set by the origin.
#
# The theory underpinning this separation of concerns is that the
# origin did _work_ to produce a response: even if one particular
# client request signals that _it_ doesn't want a cached response, a
# subsequent request (or client) _might_. Furthermore, with the more
# resource-consumptive aspect of the system de-duplicated through a
# content-addressable store (indeed, potentially the very same one as
# the origin), the additional overhead of preemptive caching is
# negligible.
#
# Per [the index](https://www.iana.org/assignments/http-cache-directives/http-cache-directives.xhtml):
#
# |Directive               |Request|Response|Type  |
# |------------------------|-------|--------|------|
# |`immutable`             |       | *      |      |
# |`max-age`               | *     | *      | int  |
# |`max-stale`             | *     |        | int  |
# |`min-fresh`             | *     |        | int  |
# |`must-revalidate`       |       | *      |      |
# |`must-understand`       |       | *      |      |
# |`no-cache`              | *     | *      | str* |
# |`no-store`              | *     | *      |      |
# |`no-transform`          | *     | *      |      |
# |`only-if-cached`        | *     |        |      |
# |`private`               |       | *      | str† |
# |`proxy-revalidate`      |       | *      |      |
# |`public`                |       | *      |      |
# |`s-maxage`              |       | *      | int  |
# |`stale-if-error`        | *     | *      | int  |
# |`stale-while-revalidate`|       | *      | int  |
#
# > \* Header fields to be replaced by `304` responses
# >
# > † Header fields that must never be shared
#
# @note One thing we have to avoid in this design is sending response
#  bodies _back_ to the content-addressable store they just came
#  from. Take for instance the planned case of a faux-static handler
#  which resolves human-readable URIs to content-addressed blobs. Top
#  candidate for the moment is RFC9530 headers, followed by a
#  `Content-Location` or `ETag` containing an RFC6920 address.
#  Realistically we can check all of these.
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

  # Ensure the argument resolves to a parsed `Cache-Control` header.
  #
  # @param msg [Rack::Request, Rack::Response, Hash, #to_s] smome
  #  argument or other
  #
  # @return [Hash] the `Cache-Control` header
  #
  def coerce_cache_control msg
    return msg if msg.is_a?(Hash) && msg.keys.all? { |k| k.is_a? Symbol }

    F['cache-control'][msg]&.value || {}
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
  # Coalesces in `Pragma` for requests, and for responses it folds in
  # `Expires` and `Last-Modified`.
  #
  # @param message [Rack::Request, Rack::Response, Hash] the full
  #  message or response header set in lieu
  # @param time [Time] when the message was retrieved
  # @param stored [Time] when the response was stored in the cache, if
  #  different
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated, or just pass in the request to test
  # @param default [false, true, Integer] a default value for max-age
  #
  # @return [Hash] cache-control directives
  #
  def flatten_cache_control message, time = Time.now, stored: time,
      authed: false, default: false
    case message
    when Rack::Request
      # get both `Cache-Control` and obsolete `Pragma` header
      cc, pragma = %w[cache-control pragma].map do |hdr|
        F[hdr][message]&.value || {}
      end

      # cache-control supersedes pragma
      pragma.slice(:'no-cache').merge cc
    when Rack::Response, Hash
      # get the cache-control header
      cc  = coerce_cache_control message
      # determine if response is public
      pub = public? message, authed: authed

      if max = max_age(message, time, stored: stored, authed: authed, cc: cc)
        cc[:'s-maxage'] = max if public?(message, authed: authed)
        cc[:'max-age']  = max
      end

      cc
    else
      raise ArgumentError,
        "Rack::Request or Rack::Response (or Hash) only, not #{message.class}"
    end
  end

  # Returns a definitive `max-age` for the HTTP message (either
  # request or response), or `nil` if one can't be derived. Attempts
  # candidates in this order:
  #
  # * `s-maxage` if the response is public (and syntactically valid)
  # * `max-age` otherwise (ditto syntax)
  # * `Expires` header relative to `Date` (or `stored` time in lieu)
  # * `Last-Modified` heuristic if the response is
  #   heuristically-cacheable, delta adjusted
  #
  # @note A `Hash` as the first argument is assumed to be _response_
  #  headers, unless all the keys are `Symbol`s, in which case it is
  #  assumed to be the `Cache-Control` header itself.
  #
  # @param message [Rack::Request, Rack::Response, Hash] the full
  #  message or response header set in lieu
  # @param time [Time] when the message was retrieved
  # @param stored [Time] when the response was stored in the cache, if
  #  different
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated, or just pass in the request to test
  # @param default [false, true, Integer] a default value for max-age
  # @param cc [Hash] an already-parsed `Cache-Control` header, if one
  #  is available
  #
  # @return [Integer, nil] the canonical `max-age`, or `nil` if one
  #  can't be resolved.
  #
  def max_age message, time = Time.now, stored: time,
      authed: false, default: false, cc: nil

    case message
    when Rack::Request
      cc ||= F['cache-control'][message]&.value || {}
      cc[:'max-age'] if cc[:'max-age'].is_a? Numeric
    when Rack::Response, Hash
      # get definitive `Cache-Control` header
      cc ||= coerce_cache_control message

      # fake up an empty hash for the headers if just cc was passed in
      message = {} if message == cc

      # coalesce the max-ages
      max = cc[:'s-maxage'] if public?(cc, authed: authed) &&
        cc[:'s-maxage'].is_a?(Numeric) && cc[:'s-maxage'] >= 0

      max ||= cc[:'max-age'] if
        cc[:'max-age'].is_a?(Numeric) && cc[:'max-age'] >= 0

      unless max
        stored ||= time ||= Time.now
        delta = time - stored
        date  = F['date'][message]&.value || stored
        dd    = stored - date
        lm    = F['last-modified'][message]&.value if
          message.respond_to?(:status) && CACHE_OK.include?(message.status)

        if exp = F['expires'][message]&.value
          # note `Expires` is gonna be relative to `Date`
          max = exp > date ? exp - date : 0
        elsif lm && lm.to_i > 0
          # so is `Last-Modified` but we're calculating
          # heuristic freshness against `now`.
          max = date + dd - lm
          max = max > 0 ? max * 0.1 : 0
        elsif default
          max = default.is_a(Numeric) ? default : @defaults[:'max-age']
        end
      end

      max.round if max
    else
      raise ArgumentError,
        "Rack::Request or Rack::Response (or Hash) only, not #{message.class}"
    end
  end

  # Fetch the components of the response, assuming the request has
  # already been verified as cacheable in principle.
  #
  # @note This method should always return a record if one is found in
  #  the cache, and leave it to {#fetch} to negotiate whether the
  #  client will accept it or if it must be revalidated. However, if
  #  the response _is_ stale, this method is responsible for evicting
  #  the record from the cache index.
  #
  # @param req [Rack::Request] the request to match
  # @param time [Time] the time the request was received
  #
  # @return [Array(Integer, Hash, URI::NI), nil] the parts
  #  for the cached response
  #
  def fetch_internal req, time: Time.now
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

  # Freshen the response timestamps and other elements on 304
  # responses. Updates response time, headers, etc.
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
  def maybe_cache req, resp = nil, time: Time.now, &block
    # before we begin:
    #
    # * if there is both a `resp` and a block, treat it like a
    #   (re)validation request.
    #   * `time` is assumed to be the timestamp of the entry.
    # * if there is only a `resp`, then `time` is treated as the
    #   response time.
    # * if there is only a `block`, then `time` is the request time.
    # * if there is neither a `resp` or a `block`, this is an error.
    #
    # the procedure:
    #
    # * unless there is a block:
    #   * raise an error if there is no `resp` argument
    #   * if `resp` is cacheable, update `resp` with a new cached version
    #   * RETURN `resp` (potentially a no-op)
    # * assign `resp` to `cached` if it exists
    # * duplicate the request in case it gets manipulated
    # * if cached, set the `If-Modified-Since` header of the request
    #   to the `httpdate` serialization of `time`
    #   * (XXX do we clear out the other `If-*` headers?)
    #     * (i wanna say yes??)
    # * run the block with the subrequest and collect the response
    # * if the request was unsafe and the response was successful,
    #   expire all cache entries for the request-URI (RFC9111 §4.4)
    # * if the response is cacheable, update `resp` with a new cached version
    #   * (make sure to pass in the original request for the cache key)
    # * if the response was a (re)validation:
    #   * if the response is a 304:
    #     * freshen the cache entry for the (original) request
    #     * if `no-cache` or `must-revalidate` is present, update
    #       any headers that need updating and RETURN the cached response
    #   * if the response is an error:
    #     * if `stale-if-error` is present and still valid and
    #       `no-cache` is not, RETURN the cached response
    #     * (note `stale-if-error` holds "regardless of other freshness
    #       information", and only covers 500, 502, 503, 504, per RFC5861 §4)
    # * RETURN the origin response

    unless block
      raise ArgumentError,
        'must have at least either a resp or block' unless resp

      resp = cache_internal(req, resp) if cacheable_resp?(resp, request: req)

      return resp
    end

    # preamble

    cached = resp # this will obviously stay nil if `resp` is nil
    subreq = req.dup
    subreq.set_header('HTTP_IF_MODIFIED_SINCE', time.httpdate) if cached

    # get the origin response
    begin
      resp = block.call subreq
    rescue Intertwingler::Error::HTTPStatus => e
      resp = e.response
    end

    # nuke if bad lol
    expire_internal(req.url) if unsafe?(req) and resp.successful?

    # add to cache
    resp = cache_internal(req, resp) if cacheable_resp?(resp, request: req)

    # now deal with revalidation
    if cached
      # shuffle around some values
      stored = time
      time   = Time.now in: ?Z

      raise ArgumentError, 'cached entry must have an Age header' unless
        age = F['age'][cached].value

      # attempt to get the cache-control headers from both responses
      ccc = F['cache-control'][cached]&.value || {}
      rcc = F['cache-control'][resp]&.value   || {}

      if resp.status == 304
        # freshen the index
        freshen_internal req, resp
        # assemble the set of headers to replace (RFC9110 §15.4.5;
        # note the bit about how 304 MUST replace these headers)
        nc = ccc.merge(rcc)[:'no-cache'].to_s.strip.downcase.split
        nh = (
          %w[content-location date etag vary cache-control expires] + nc).to_set
        # we can just do that lol
        cached.headers.merge!(resp.headers.slice(*nh))
        return cached
      end

      # deal with stale-if-error

      if [500, 502, 503, 504].include?(resp.status)
        max = max_age cached, time, stored: stored, authed: req, default: true
        sie = rcc[:'stale-if-error']&.value || 0
        tmp = ccc[:'stale-if-error']&.value || 0
        sie = tmp if tmp < sie

        # this says age greater than max-age but less than
        return cached if age > max && age <= (max + sie)
      end
    end

    resp # aaand the response
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

  DEFAULTS = {
    'max-age': 5, # we want stuff to expire fairly quickly by default
    'stale-while-revalidate': 0, # we probably don't want to do this by default
    'stale-if-error': 0, # ditto
  }.freeze

  public

  # Initialize the cache index.
  #
  # @param engine [Intertwingler::Engine] a back-reference to the engine
  # @param options [Hash] downstream options
  #
  # @return [void]
  #
  def initialize store: nil, log: nil, driver: :LMDB, defaults: nil, **options
    raise ArgumentError, "`store` must be a Store::Digest" unless
      store.is_a? Store::Digest
    @store    = init_store store
    @log      = log
    @defaults = DEFAULTS.merge((defaults || {}).transform_keys(&:to_sym))

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

  # Determine whether the request is authenticated.
  #
  # @param request [Rack::Request, Hash, false, true, nil] the request or
  #  environment or pass-through value
  #
  # @return [false, true] whether the request is authenticated
  #
  def authed? request
    user = case request
           when FalseClass, TrueClass then return request
           when nil then return false
           when Rack::Request then request.env['REMOTE_USER']
           when Hash then request['REMOTE_USER']
           else
             raise ArgumentError, "Can't act on a #{request.class}"
           end

    # coerce to string; empty means no
    !user.to_s.strip.empty?
  end

  # Determine if the response is public.
  #
  # @param response [Rack::Response, Hash] the response, or response
  #  headers, or `Cache-Control` header
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated (or the request itself)
  #
  # @return [false, true] whether the response is public
  #
  def public? response, authed: false
    # if we're handed a hash with only symbol keys, assume it's
    # already the parsed cache control
    cc = coerce_cache_control response

    # assume the response is private if the request is authenticated
    # and the response's `Cache-Control` is not explicitly public;
    # likewise assume it is private if the request is not
    # authenticated and the response is explicitly private

    # get the cache-control header from the response
    authed?(authed) ? cc.key?(:public) : !cc.key?(:private)
  end

  # Determine if the response is private.
  #
  # @param response [Rack::Response, Hash] the response, or response
  #  headers, or `Cache-Control` header
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated (or the request itself)
  #
  # @return [false, true] whether the response is private
  #
  def private? response, authed: false
    !public?(response, authed: authed)
  end

  # Validate that the request (from the client) is cacheable.
  #
  # @param req [Rack::Request] the request
  #
  # @return [false, true] whether the request is cacheable

  def cacheable_req? req
    # we can't cache if the method is not supported;
    # also note http methods are actually case-sensitive
    return false unless METHODS.key? req.request_method.to_sym

    # obtain cache-control header (no need to fold in `Pragma:
    # no-cache`) since it turns out the meaning of `no-cache` is
    # actually subtler than "don't cache"
    cc = F['cache-control'][req]&.value || {}

    # we can't cache if the client explicitly says not to
    return false if cc.key?(:'no-store')

    # we can't cache if max-age is zero
    return false if cc[:'max-age'] == 0

    # otherwise it's cacheable i guess
    true
  end

  # Validate that the response (from the origin) is cacheable.
  #
  # @param resp [Rack::Response] the response
  # @param authed [false, true, Rack::Request] whether the request is
  #  authenticated, or just pass in the request itself
  #
  # @return [false, true] whether the response is cacheable
  #
  def cacheable_resp? resp, request: nil, authed: request, time: Time.now
    # these response codes are always a no
    return false if NO_CACHE.include?(resp.status)
    # headers
    cc = flatten_cache_control resp, authed: authed
    return false if cc[:'max-age'] == 0

    # XXX RFC9111 §5.2.2.4 says an (unqualified) no-cache in the
    # response indicates that the response must not be reused without
    # being revalidated

    # false if any of no-store, s-maxage=0 (if shared), max-age=0,
    # expires in the past, age header exists and exceeds max-age

    # we can't cache a private response if not logged in
    return false if cc.key?(:private) and not req.env['REMOTE_USER']

    true
  end

  # Determines whether a cache entry is strictly _stale_, meaning its
  # age exceeds its max-age
  #
  # @param response [Rack::Response, Hash] the response or its headers
  # @param time [Time] the request time (defaults to now)
  # @param request [Rack::Request] the request
  # @param authed [false, true] a flag in lieu of the request if authenticated
  # @param stored [Time] when the cache entry was stored
  # @param default [false, true, Integer] a default max-=a
  #
  # @return [false, true, nil] whether the response is stale, `nil` if
  #  indeterminate.
  #
  def stale? response, time = Time.now, stored: time,
      request: nil, authed: request, default: false

    # get the age of the resource
    age = time - stored

    authed = authed?(authed || request)

    # obtain max_age
    max = max_age resp, time, stored: stored, authed: authed, default: default

    # indeterminate
    return unless max

    # no request to compare
    return age > max unless request

    # okay _now_ we compare against the request
    cc    = F['cache-control'][response]&.value || {}
    rcc   = F['cache-control'][request]&.value || {}
    rmax  = rcc[:'max-age'] || max
    max   = rmax if rmax < max
    swr   = cc[:'stale-while-revalidate'] || 0
    max_s = rcc[:'max-stale'] || 0
    min_f = rcc[:'min-fresh'] || 0

    #
    age > ((max - min_f).clamp(0..) + [max_s, swr].max)
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
    return maybe_cache(req, resp, force: force, &block) if
      force || cacheable_req?(req)

    resp || block.call(req)
  end

  # Attempt to fetch a cache entry based on a given request. Returns
  # `nil` if a fresh cache entry can't be resolved.
  #
  # @param req [Rack::Request] the request in question
  # @param time [Time] the time of the request
  # @param block [Proc, #call] the origin/upstream request handler
  # @yieldparam req [Rack::Request] the same request
  # @yieldreturn [Rack::Response] the origin (or cached) response
  #
  # @return [Rack::Response, nil] the cached response (maybe)
  #
  def fetch req, time: Time.now, &block
    # pretty straightforward here when called with a block:
    #
    # * check if the request is cacheable (request method OK, `no-store`
    #   not set, `max-age - min-fresh + max-stale` greater than zero)
    #   * if so, try to fetch the response out of cache
    #   * if found:
    #     * immediately determine objective response freshness
    #       * get response age relative to THIS request's timestamp
    #       * minimum of client `max-age` and harmonized server `max-age`
    #     * determine if the client will accept the cached response
    #       * `min-fresh`, minimum of (`max-stale`, `stale-while-revalidate`)
    #       * (`stale-while-revalidate` and `stale-if-error` shall be
    #         clipped at `max-stale` and calculated relative to `min-fresh`)
    #     * determine if the response does NOT need synchronous revalidation
    #       * if `stale-while-revalidate` is in force, split off a
    #         thread to revalidate the cache entry
    #       * RETURN the cached response
    #     * now perform revalidation:
    #       * `no-cache` on either client or server
    #         * set `If-Modified-Since` to the timestamp of the cache entry
    #         * (304 from upstream means a cached response can be sent if fresh)
    #         * (`no-cache="Foo Bar"` from server means replace those headers)
    #       * `must-revalidate`
    #         * (cached response is stale and must be revalidated before used)
    #         * (`stale-if-error` only makes sense here as far as i can tell)
    #         * (`stale-if-error` should also re-test
    #       * (fall through the flow if client rejects cached response)
    #     * determine if response needs *a*synchronous revalidation
    #       (ie `stale-while-revalidate`)
    #     * return a (cached or potentially revalidated) response if OK
    # * RETURN 504 at this point if request specifies `only-if-cached`
    # * try to request the response from the origin
    #   * (this implies attempting to re-cache it)
    #
    # If there is no block, all attempts to contact the origin
    # naturally return nil.

    m = req.request_method
    u = req.url

    rcc = F['cache-control'][req]&.value || {}

    if cacheable_req? req
      # attempt to get cache entry
      resp, stored = fetch_internal req

      # we have a cached entry
      if resp
        # get response age vs time
        age = time - stored
        # may as well set the header now
        resp.set_header('age', age.to_s)

        # get the definitive `max-age` from the response
        max = max_age resp, time, stored: stored, authed: req, default: true

        # determine if request will accept a cached response
        cc    = F['cache-control'][resp]&.value || {}
        rcc   = F['cache-control'][req]&.value || {}
        rmax  = rcc[:'max-age'] || max
        max   = rmax if rmax < max
        swr   = cc[:'stale-while-revalidate'] || 0
        max_s = rcc[:'max-stale'] || 0
        min_f = rcc[:'min-fresh'] || 0
        serve = age <= ((max - min_f).clamp(0..) + [max_s, swr].max)

        if serve
          log.debug "Cache hit on #{m} #{u}"
          # determine if we don't have to revalidate synchronously
          if !rcc.key?(:'no-cache') ||
              cc.slice(*%i[no-cache must-revalidate]).empty?

            # swr may be less than max-stale so we test it again
            if block and age < ((max - min_f).clamp(0..) + swr)
              log.debug "Asynchronously revalidating #{m} #{u}"
              Thread.new { maybe_cache req, &block }
            end

            # return cached response
            return resp
          end

          # okay now revalidate
          if block
            log.debug "Revalidating #{m} #{u}"
            return maybe_cache req, resp, time: stored, &block
          end
        end
      end

      return unless block

      return maybe_cache req, time: time, &block
    end

    log.debug "Cache miss on #{m} #{u}: request is not cacheable"

    return error_504 if rcc.key? :'only-if-cached'

    block.call req if block
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

  # This is a little toy class that encapsulates the cache key digest
  # state, which gets bifurcated twice (authenticated vs not; with
  # `Vary` headers vs not).
  #
  class KeyState
    private

    NULL_BODY = URI::NI.compute ''

    def set_initial_state
      # get the hash for the request body:

      # get the request method
      meth = @req.request_method

      # we change the cache key input by one bit if this is the case
      if cc = F['cache-control'][@req]
        @no_transform = cc.key? :'no-transform'
      end

      # get the normalized request-URI
      uri = Intertwingler::Util::Clean.coerce_resource(@req.url, as: :uri)
      @ustate = (Digest::SHA256.new << uri.normalize.to_s)

      # now we should get the state
      state = @states.first

      # if the request is other than HEAD/GET:
      unless METHODS.fetch(meth.to_sym, false)

        # check to see if there's a `Content-Location` request header
        cl = F['content-location'][@req]&.value

        # if the Content-Location contains an RFC6920 URI, then we
        # will just use that. if that fails or if otherwise it's
        # missing (likely), scan the request body i guess.

        unless cl.is_a? URI::NI
          if body = @req.body
            # i *could* make this code responsible for the conversion
            # but i don't want to
            raise TypeError,
              'request body should be a Store::Digest::Entry by now, not %s' %
              body.class unless body.is_a? Store::Digest::Entry

            cl = body[:"sha-256"]
          else
            cl = NULL_BODY.dup
          end
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
      uri = Intertwingler::Util::Clean.coerce_resource uri, as: :uri
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
        vary = vary.split("\x1f").map(&:strip)

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

      # Return the subset of response headers suitable for caching.
      #
      # @param headers [Hash, Rack::Response] a hash of HTTP headers
      #
      # @return [Hash] the pruned subset
      #
      def prune_headers headers
        # first make sure
        headers = headers.headers if headers.is_a? Rack::Response

        headers.except(*%w[connection keep-alive proxy-connection
                           proxy-authenticate proxy-authentication-info
                           proxy-authorization te transfer-encoding upgrade])
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
        headers = prune_headers headers

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
        date = F['date'][headers]&.value || time
        delta = time - date

        if lm = F['last-modified'][headers]&.value
          ctrl |= LAST_MOD_P
          fmt << ?q
          out << (lm + delta).to_i
        end

        # now we get the actual cache parameters
        cc = F['cache-control'][headers]&.value || {}
        ctrl |= NO_TRANSFORM if cc.key? :'no-transform'
        ctrl |= IMMUTABLE    if cc.key? :immutable

        # we need a definitive max-age delta
        max = if !authed and cc[:'s-maxage'].is_a?(F)
                cc[:'s-maxage'].value
              elsif cc[:'max-age'].is_a?(F)
                cc[:'max-age'].value
              elsif expires = F['expires'][headers]&.value and expires > date
                (expires - date).round
              elsif lm and lm < date
                # 10% of last-modified to date/response/now
                ((date - lm) * 0.1).round
              end

        if max && max >= 0
          ctrl |= MAX_AGE_P
          fmt << ?L
          out << max
        end

        # XXX it may turn out that none of these make any sense to
        # pull out of the header set because as of 2026-05-21 i
        # haven't found a way to use them before they're headers
        {
          'stale-while-revalidate': STALE_REV_P,
          'stale-if-error':         STALE_ERR_P,
        }.each do |d, flag|
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

        # LOL RFC9111 §5.2.2.4
        except = cc[:'no-cache'] ?
          cc[:'no-cache'].to_s.strip.split.map(&:downcase): []

        # finally, headers
        fmt << 'a*'
        out << pack_headers(headers.except(*except))

        ([ctrl, status, time.to_i] + out).pack fmt
      end

      # Unpack a "terminal" record.
      #
      # @param record [String] the raw record bytes
      # @param time   [Time] the reference time
      #
      # @return [Array(Integer, Hash, URI::NI, Time)] status, headers,
      #  body URI, and cache entry time
      #
      def unpack_terminal record, time = Time.now, &block
        ctrl, rest = record.unpack 'Ca*'
        raise ArgumentError, 'must be a terminal record' if signpost? ctrl

        # initial format
        fmt = +'Sq'
        fields = %i[status time]

        # go through this mapping and add fields to list and pack format
        {
          LAST_MOD_P   => [?q, :'last-modified'],
          MAX_AGE_P    => [?L, :'max-age'],
          STALE_REV_P  => [?L, :'stale-while-revalidate'],
          STALE_ERR_P  => [?L, :'stale-if-error'],
          BODY_PRESENT => ['a32', :body],
        }.each do |flag, pair|
          if (ctrl & flag).nonzero?
            fmt    << pair.first
            fields << pair.last
          end
        end

        # we add the packed headers last
        fmt    << 'a*'
        fields << :headers

        # aaand now we snag all the fields and format them. first we
        # unpack everything into a hash; reuse `fields`, why not
        fields = fields.zip(rest.unpack fmt).to_h

        # eh might as well do these even though we aren't using em
        # right now
        %i[no-transform immutable].zip(
          [NO_TRANSFORM, IMMUTABLE]).each do |k, v|
          fields[k] = (ctrl & v).nonzero?
        end

        # XXX i mean, here is where all those extracted cache fields
        # would be useful because you could determine whether to
        # continue with eg unpacking the headers or resolving the body
        if block
          # * prepare something to hand the block
          # * don't do any more work
          # return unless block.call arg
        end

        # …buuut anyway let's just unpack those headers
        fields[:headers] = unpack_headers fields[:headers]
        # we're gonna overwrite the age header with our age
        fields[:headers]['age'] = (time - fields[:time]).round

        fields[:body] =
          URI("ni:///sha-256;#{[body].pack(m0).tr('+/', '-_').delete(?=)}") if
          fields.key? :body

        # note `fields[:time]` is not the same as `time`
        fields.values_at :status, :headers, :body, :time
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
        vary  = F['vary'][resp]&.value
        state = KeyState.new req, vary: vary

        # we use authed key if the request is authenticated, unless
        # the response is explicitly `public`
        priv = private?(resp, authed: req)

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
          tk  = state.state(authed: priv, vary: !!vary).digest
          tv  = pack_terminal resp.status, resp.headers, cl
          inc = !index.key?(tk) # determine if adding or updating
          @index.put tk, tv

          # put the terminal key in the uri index
          uk = state.uri_state.digest
          @uris.put? uk, tk

          if vary
            sk = state.state(authed: priv,  vary: false)

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

      # Resolve a terminal record from a key state.
      #
      # @param state [Intertwingler::Cache::KeyState] a key state
      #
      # @return [nil, Array(String, String, Integer, Array, String)]
      #  if the record is found: the terminal key, raw record,
      #  signpost count, `Vary` headers, and signpost key.
      #
      def resolve_terminal state
        # first attempt to get a private record
        if authed = state.authed?
          tkey = state.state(authed: true).digest
          # try public record
          authed = false unless raw = @index[tkey]
        end

        # attempt to get the public record
        raw ||= @index[tkey = state.state.digest]

        # okay there really isn't anything here
        return if raw.nil? or raw.empty?

        if signpost?(raw)
          skey = tkey
          count, vary = unpack_signpost raw
          tkey = state.state(authed: authed, vary: vary).digest
          raw  = @index[tkey]

          # if this is nil then there's nothing here
          return if raw.nil? or raw.empty?
        else
          skey  = tkey
          count = 0
          vary  = nil
        end

        [tkey, raw, count, vary, skey]
      end

      # @see {Intertwingler::Cache#fetch_internal} for documentation
      #
      # @return [nil, Array(Integer, Hash, Store::Digest::Entry)]
      #
      def fetch_internal req, time: Time.now
        state = KeyState.new req

        @env.transaction do |txn|
          tkey, raw, count, vary, skey = resolve_terminal state

          if tkey
            # XXX THIS IS A HUGE PREMATURE OPTIMIZATION
            #
            # first we measure if the response is objectively stale,
            # then we determine if the client will accept it. the
            # client may reject a record that's still fresh, or
            # accept a record that's stale, depending on the
            # request's `Cache-Control` parameters.

            # if the response is still fresh and the client will
            # accept it, we could actually return 304 here

            # out = unpack_terminal(raw) do |lol|
            #   # smuggle stuff out?
            # end

            # okay back to being normal
            warn state

            status, headers, body, ctime = unpack_terminal raw

            # all expired is stale but not all stale is expired

            # if the response is stale then expire the record
            if stale?(headers, time, authed: state.authed?, stored: ctime)
              # get the key for the whole uri
              ukey = state.uri_state.digest

              # delete the expired terminal
              @index.delete tkey
              @uris.delete ukey, tkey

              # now we decrement the refcount
              if skey != tkey
                if count > 1
                  @index[skey] = pack_signpost(count - 1, vary)
                else
                  @index.delete skey
                  @uris.delete? ukey, skey
                end
              end
            end

            # return the terminal record
            [status, headers, body, ctime]
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

      FRESHEN_EXCEPT = %w[content-type content-length
                          content-encoding content-location]

      def freshen_internal req, resp, time: nil
        raise ArgumentError,
          "response must be 304, not #{resp.status}" unless resp.status == 304

        time ||= Time.now

        state = KeyState.new req

        @env.transaction do
          # resolve terminal record (or return because it's already been nuked)
          key, raw = resolve_terminal state

          if key
            # get the raw record
            status, headers, cl, _ = unpack_terminal raw

            # merge the headers
            headers.merge! resp.headers.except(*FRESHEN_EXCEPT)

            raw = pack_terminal status, headers,
              time: time, authed: state.authed?

            @index[key] = raw
          end
        end
      end
    end

    # we can do redis or whatever later lol
  end

end
