require_relative 'storable'

require 'strscan'
require 'digest'
require 'uri/ni'

# This is a toy cache index. Its job is to map cache keys to
# cryptographic digests. It uses Intertwingler's opaque blob
# resolution infrastructure to store and retrieve the blobs, but
# handles its own metadata.
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

  # This module is for parsing parametrized headers like `Accept`.
  #
  module HeaderParser
    # scan a token
    TOKEN = /[!#$%&'*+\-.^_`|~0-9A-Za-z]+/
    # optional whitespace
    OWS   = /[ \t]*/
    FLOAT = /^[+-]?(?=\.?\d)\d*\.?\d*(?:[Ee][+-]?\d+)?\z/

    # Scan a quoted string.
    #
    # @param ss [StringScanner] the string scanner instance
    #
    # @raise [ArgumentError] the grammar is malformed.
    #
    # @return [String] the erstwhile-quoted string.
    #
    def self.scan_quoted ss
      ss.scan(/"/) or return
      out = String.new
      until ss.eos?
        if (chunk = ss.scan(/[\t \x21\x23-\x5b\x5d-\x7e\x80-\xff]+/n))
          out << chunk
        elsif ss.scan(/\\/)
          ch = ss.scan(/[\t \x21-\x7e\x80-\xff]/n) or
            raise ArgumentError, "invalid escape at #{ss.pos}"
          out << ch
        elsif ss.scan(/"/)
          return out
        else
          peek = ss.peek(10).inspect
          raise ArgumentError,
            "invalid character in quoted string at #{ss.pos}: #{peek}"
        end
      end
      raise ArgumentError, 'unterminated quoted string'
    end

    # Scan an individual parameter value.
    #
    # @param ss [StringScanner] the string scanner instance
    #
    # @raise [ArgumentError] the value is malformed.
    #
    # @return [String] the erstwhile-quoted string.
    #
    def self.scan_value ss
      value = ss.scan TOKEN
      return value.downcase.to_sym if value

      scan_quoted ss
    end

    # Scan an individual parameter pair.
    #
    # @param ss [StringScanner] the string scanner instance
    #
    # @return [Array(Symbol, Object)] the parameter name and its value
    #
    def self.scan_param ss
      name = ss.scan(TOKEN) or return

      ss.skip OWS

      ss.scan(/=/) or
        raise ArgumentError, "expected '=' after parameter name #{name.inspect}"

      ss.skip OWS
      value = scan_value(ss) or
        raise ArgumentError, "expected value for parameter #{name.inspect}"

      name  = name.downcase.to_sym
      value = value.to_s.to_f if value.is_a?(Symbol) && FLOAT.match?(value.to_s)

      [name, value]
    end

    # Scan a member of the list
    #
    # @param ss [StringScanner] the string scanner instance
    #
    # @return [Array(Symbol,Hash)] the
    #
    def self.scan_member ss
      ss.skip OWS
      value = scan_value(ss) or return nil
      params = {}

      while ss.skip(/[ \t]*;[ \t]*/) && !ss.eos?
        # peek ahead — trailing semicolon with no parameter is tolerated
        break unless ss.check TOKEN

        name, val = scan_param ss
        params[name] = val
      end

      ss.skip OWS

      [value, params]
    end

    # Parse a ranked header value (e.g. `Accept`)
    #
    # @param value [String] the header value
    #
    # @return [Array]
    #
    def self.parse value
      ss = StringScanner.new value
      members = []
      loop do
        ss.skip OWS
        break if ss.eos?

        member = scan_member ss
        members << member if member

        ss.skip OWS
        break unless ss.scan /,/
      end
      members
    end

    # Normalize a parsed header
    #
    # @param values [Array<Array(Symbol,Hash{Symbol=>Object})>]
    # @param strip [false, true]
    # @param cmp [Proc] an optional comparator function
    #
    # @yieldparam a [Array(Symbol,Hash{Symbol=>Object})] comparand A
    # @yieldparam b [Array(Symbol,Hash{Symbol=>Object})] comparand B
    #
    # @yieldreturn [Integer] preferably -1, 0, or 1
    #
    # @return [Array<Array(Symbol,Hash{Symbol=>Object})>]
    #
    def self.normalize values, strip: false, &cmp
      cmp ||= -> a, b do
      end
    end

    # Serialize a parsed header to a string.
    def self.serialize values, &cmp
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

      # get the normalized request-URI
      uri = Intertwingler::Resolver.coerce_resource(@req.url, as: :uri)

      # now we should get the state
      state = @states.first

      # if the request is other than HEAD/GET:
      unless %w[HEAD GET].include? meth

        # check to see if there's a `Content-Location` request header
        if cl = @req.get_header('HTTP_CONTENT_LOCATION')
          # resolve the content-location against the request-URI
          cl = uri + cl
        end

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
        state << "#{cl.digest}#{cl.algorithm}\x1e"
      end

      state << [uri.normalize.to_s, meth].join("\x1e")

      # if there is a user, do the initial bifurcation
      if user = @req.env['REMOTE_USER'] and not user.strip.empty?
        (@states[1] = @states.first.dup) << "\x1e#{user}"
      end
    end

    public

    # Initialize the state object.
    #
    # @param cache [Intertwingler::Cache] obligatory backlink to cache
    # @param req [Rack::Request] the request to be turned into a cache key
    # @param vary [nil, Array<String>] request headers to `Vary`
    #
    def initialize cache, req, vary: nil
      @cache  = cache
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

    # Set which headers were found in the response's `Vary` to be
    # used for the request.
    #
    # @param headers [Array<String>]
    #
    def vary= headers
      # check input
      raise ArgumentError, "headers must be an array" unless
        headers.is_a? Array

      # empty these out if there are no vary headers
      if headers.empty?
        @states[2] = @states[3] = nil
        return
      end

      # map the canonical request headers to cgi/rack env (eyeroll)
      # XXX FUTURE ME: rack 3.3 will have `headers`
      mapping = headers.map do |h|
        rh = h.upcase.tr(?-, ?_)

        # these would be weird to be in here but w/e
        rh = "HTTP_#{rh}" unless /^CONTENT_(?:TYPE|LENGTH)$/ =~ rh

        [h.downcase, rh]
      end.to_h

      # give us the reverse
      rev = mapping.invert

      # take subset of headers
      reqh = req.env.slice(*mapping.values).map do |k, v|
        # normalize request headers
        [rev[k], v]
      end.sort.to_h

      # now serialize them
      serialized = reqh.map { |k, v| "#{k}\x1f{#v}" }.join("\x1e")

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
    # * add `\x1e`.
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
    # * If the request had `no-transform` in the `Cache-Control`
    #   header, add `\x1d`. Otherwise, add `\x1e`.
    # * add the serialized headers (which may be the empty
    #   string) to whichever variant of the hash matched the signpost.
    # * now try looking up the new key. if it hits, that's the
    #   terminal entry for that resource.
    #
    # (Note the original plan was to actually hash the message body
    # first for the initial state, as I mistakenly thought you could
    # resume a SHA-256 hash. It turns out you can't, on purpose.)
    #
    # Now we discuss cache entry layouts.
    #
    # The first byte of the entry will consist of control flags. The
    # first bit in that byte determines whether the entry is a
    # signpost (1) or terminal (0). The rest of the bits flag aspects
    # of the terminal record. On a signpost record, the only thing
    # that follows is the list of `Vary` headers, delineated by
    # `\x1f`. These may be compressed. (We rely on the GZip header to
    # indicate compression.)
    #
    # Here are the flags for the control byte:
    #
    # 0. record is a "signpost"
    # 1. body hash present
    # 2. `no-transform` flag in the *response*
    # 3. `immutable` flag
    # 4. `must-understand` flag
    # 5. `max-age` delta present
    # 6. `stale-while-revalidate` delta present
    # 7. `stale-if-error` delta present
    #
    # For the terminal entries, the next two bytes represent an
    # unsigned short (which we may eventually steal up to six or seven
    # bits from) containing the status code, followed by a 64-bit
    # timestamp (overkill?) representing the original `Date` header of
    # the origin response. Following that are uint32 deltas whose
    # presence are contingent on bits in the control flags for
    # `max-age`/`Expires`/`s-maxage`, `stale-while-revalidate`,
    # `stale-if-error`, followed by the sha-256 hash of the response
    # body, if present. the rest of the record is the serialized
    # header set (which may be compressed).
    #
    # Once we have resolved the terminal entry, we can actually do
    # some fkn logic to it.

    # We're sticking to LMDB because we're using it in the RDF store
    # and {Store::Digest}.
    module LMDB
      require 'lmdb'

      private

      # record flag fields
      IS_SIGNPOST  = 1 << 0
      BODY_PRESENT = 1 << 1
      NO_TRANSFORM = 1 << 2
      IMMUTABLE    = 1 << 3
      M_UNDERSTAND = 1 << 4
      MAX_AGE_P    = 1 << 5
      STALE_REV_P  = 1 << 6
      STALE_ERR_P  = 1 << 7

      # driver-specific installation
      def bootstrap dir: 'cache', mapsize: 2**27
        dir = Pathname(dir).expand_path
        dir.mkpath unless dir.exist?

        @env = ::LMDB.new dir, mapsize: mapsize
        @index = @env.database 'index', create: true
      end

      def vary_headers val
        return unless val

        # lol get all that?
        val.to_s.strip.split(/\s*,\s*/).map do |v|
          v = v.upcase.tr ?-, ?_
        end.sort.map do |v|
          # of course it would be incredibly cursed if the Vary header
          # *did* contain these, but i'm not here to judge
          %w[CONTENT_TYPE CONTENT_LENGTH].include?(v) ? v : "HTTP_#{v}"
        end
      end

      #
      def construct_key_state req, state: nil, vary: nil
        if state
          raise ArgumentError, 'state must be a Digest::SHA256' unless
            state.is_a? Digest::SHA256
        else
          # start fresh
          state = Digest::SHA256.new

          # add the main key elements

          # first we do the request body which we either get from
          # `Content-Location: ni:///…` or hash ourselves

          # XXX what do we do about `Content-Encoding` in requests?

          # if the request is HEAD, store GET
        end

        # now append the vary stuff
        if vary.is_a? Array and !vary.empty?
        end

        state
      end

      def cache_internal req, resp
        # construct the hash key(s)

        state = construct_key_state req
        vary  = vary_headers resp.get_header 'Vary'

        # stash the response body (unless it has a `Content-Location`
        # with an `ni:` URI, in which case it's already stashed)
      end

      # @return [nil, Array(Integer, Hash, Store::Digest::Object)]
      #
      def fetch_internal req
        state = construct_key_state req
        key   = state.dup.digest

        @index.transaction do
          raw = @index.get key
          break if rec.nil? or rec.empty?
          ctrl, rest = rec.unpack 'Ca*'

          # now we test if it's a signpost or not
          if (ctrl & IS_SIGNPOST).nonzero?
            # retrieve the vary headers

            # select the headers from the request
            # normalize and serialize
            # add them to the hash state
            # look up terminal record
            # (if it isn't there that's an error)
          end

          # now we construct the response

          # test for presence of deltas in control byte; adjust pack
          # template accordingly

          # unpack the rest of the record

          # if the record is expired, delete it (them) and return

          # if the body hash is present, turn it into an `ni:` URI and
          # dereference it

          # if it's missing that counts as a cache miss; delete the record(s)

          # now unpack the status and headers and turn them into a hash
          # (test for gzip header)

          # [status, headers, body]
        end
      end
    end

    # we can do redis or whatever later lol
  end

  private

  # this data structure (theoretically) controls whether the request
  # method is cacheable and what headers to consider (although i guess
  # the Vary header in the response is ultimately supposed to control
  # that)
  REQS = {
    GET:   [false],
    QUERY: [true],
  }
  REQS[:HEAD] = REQS[:GET]

  # Validate that the request (from the client) is cacheable.
  #
  # @param req [Rack::Request] the request
  #
  # @return [false, true] whether the request is cacheable
  #
  def cacheable_req? req
    # method
    # headers
  end

  # Validate that the response (from the origin) is cacheable.
  #
  # @param resp [Rack::Response] the response
  #
  # @return [false, true] whether the response is cacheable
  #
  def cacheable_resp? resp
    # response code
    # headers
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
    # store the body

    # XXX note the response could indicate that its body already
    # exists in the store, eg by a Content-Location with an rfc6920
    # address, or etag, or rfc9530 Content-Digest/Repr-Digest headers

    # construct the cache key

    # construct the cache metadata payload

    # actually store the body payload?

    # return the response
    resp
  end

  # Normalize an HTTP header.
  #
  # @param name  [String, #to_s] the header name
  # @param value [String, #to_s] the header value
  #
  # @return [String] the normalized header value
  #
  def normalize_header name, value
    #
  end

  # Normalize all the headers for an HTTP message.
  #
  # @param message [Rack::Request, Rack::Response, #to_h]
  #
  # @return [Hash] the normalized header set
  #
  def normalize_headers message
    #
  end

  public

  # Initialize the cache index.
  #
  # @param engine [Intertwingler::Engine] a back-reference to the engine
  # @param options [Hash] downstream options
  #
  # @return [void]
  #
  def initialize store: nil, driver: :LMDB, **options
    raise ArgumentError, "`store` must be a Store::Digest" unless
      store.is_a? Store::Digest
    @store = init_store store

    # warn self.store
    # warn options

    # bolt on the driver or whatever
    mod = /^(?:Driver)?::/.match?(driver.to_s) ? driver : "Driver::#{driver}"
    raise ArgumentError, "No driver #{mod}" unless self.class.const_defined? mod
    mod = self.class.const_get(mod)

    # warn mod

    extend mod

    bootstrap **options
  end

  # Attempt to cache the response associated with a request.
  #
  # @note here is where it would be really handy to have something
  #  like perl's `HTTP::Message` but that implemented the rack spec.
  #
  # @param req [Rack::Request] the request to match
  # @param resp [Rack::Response] the response to store (maybe)
  # @param force [false, true] whether to force the cache to store an
  #  un-cacheable request/response
  #
  # @return [Rack::Response] the response passed in as `resp`
  #
  def cache req, resp, force: false
    # validate the request and response as cacheable
    return resp unless cacheable_req?(req) && cacheable_resp?(resp) || force

    cache_internal req, resp
  end

  # Attempt to fetch a cache entry based on a given request. Returns
  # `nil` if a fresh cache entry can't be resolved.
  #
  # @param req [Rack::Request] the request in question.
  #
  # @return [Rack::Response, nil] the cached response (maybe)
  #
  def fetch req
    # validate the request as cacheable
    return unless cacheable_req? req

    # look for private candidates first

    # otherwise public

    # file through candidates

    # process conditionals

    fetch_internal req
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
    if resp = fetch(req)
      return resp
    end

    # execute the origin/upstream handler
    resp = block.call req

    raise ArgumentError,
      "Block must return a Rack::Response, not #{resp.class}" unless
      resp.is_a? Rack::Response

    # only store the response if cacheable, duh
    cache_internal req, resp if cacheable_resp? resp

    resp
  end
end
