require_relative 'engine'

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
  # mkay basic facts about cache

  # we need the cache to store the *exact* headers of the original
  # response, minus headers that make no sense to cache
  #
  # probably wouldn't hurt to normalize them as well, also gzip or whatever

  private

  REQ_HDR = %i[Accept Accept-Language Accept-Charset Accept-Encoding]

  HEADERS = %i[Content-Type Content-Encoding Content-Language]

  # this data structure (theoretically) controls whether the request
  # method is cacheable and what headers to consider (although i guess
  # the Vary header in the response is ultimately supposed to control
  # that)
  REQS = {
    GET:   [false],
    QUERY: [true],
  }
  REQS[:HEAD] = REQS[:GET]

  # Validate that the request is cacheable.
  #
  # @param req [Rack::Request] the request
  #
  # @return [false, true] whether the request is cacheable
  #
  def cacheable_req? req
    # method
    # headers
  end

  # Validate that the response is cacheable.
  #
  # @param resp [Rack::Response] the response
  #
  # @return [false, true] whether the response is cacheable
  #
  def cacheable_resp? resp
    # response code
    # headers
  end

  # Unconditionally store a response, assuming both it and the
  # concomitant request have already been verified as cacheable.
  #
  # @param req [Rack::Request] the request to match
  # @param resp [Rack::Response] the response to store (maybe)
  #
  # @return [Rack::Response] the response passed in as `resp`
  #
  def store_internal req, resp
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

  public

  # Initialize the cache index.
  #
  # @param engine [Intertwingler::Engine] a back-reference to the engine
  # @param options [Hash] downstream options
  #
  # @return [void]
  #
  def initialize engine, driver: :LMDB, **options
    # bolt on the driver or whatever
  end

  # Store the response associated with a request.
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
  def store req, resp, force: false
    # validate the request and response as cacheable
    return resp unless cacheable_req?(req) && cacheable_resp?(resp) || force

    store_internal req, resp
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
  def fetch_or_store req, &block
    # attempt to fetch the cache
    if resp = fetch(req)
      return resp
    end

    # execute the origin/upstream handler
    resp = block.call req

    raise ArgumentError,
      "Block must return a Rack::Response, not resp.class" unless
      resp.is_a? Rack::Response

    # only store the response if cacheable, duh
    return resp unless cacheable_resp? resp

    store_internal req, resp
  end

  # The driver module is just a namespace for cache drivers.
  module Driver
    # mkay we need to have a chitchat about just what the hell goes
    # into this database.
    #
    # I mean, it's a key-value store where the key is the cache key
    # and the value is the respective hash locations of the headers
    # and body.
    #
    # The cache key can vary (user, Vary: headers, request body) on
    # top of the minimal cache key which consists of the request
    # method and URI. We'll do a scheme where we hash the full cache
    # key for the main index, and then we have a secondary index with
    # the minimal (unhashed, as it is likely to be shorter, or is it?)
    # key. The latter will tell us how the full cache key can vary.
    #
    # Actually no, because if the response is public, *that's* the
    # response. If it's private, we do the same, but we attach the
    # request's user.
    #
    # okay so the cache stores *responses* which may or may not
    # contain `Vary:` headers
    #
    #
    #
    # of course if we *don't* supply the headers in the Vary
    #
    # so i'm thinking we have


    #
    # cache key -> (header set, body)
    #
    # we begin with a minimal cache key which stores options for
    # composite cache keys
    #
    # expires, flags (has user, has vary, has request body);
    #  * user if user,
    #  * vary headers + header hash if vary
    #  * request body hash if body
    #  * response header hash
    #  * response body hash (may be null)

    # nah we should just do minimal cache key to all the ways it can
    # vary, then a binary sha256 -> (sha256, sha256) for the response
    # headers and body

    # minimal cache key is method + request-uri
    #
    # minimal cache key -> (expiry vary user varyhash reqbodyhash)
    #
    # next is user for private cache
    #
    # next is request headers present in the response's Vary: header
    #
    # minimal cache key -> variants ()
    # composite cache key
    # cache key -> internal key
    # internal key -> body
    # internal key -> headers

    # We're sticking to LMDB because we're using it in the RDF store
    # and {Store::Digest}.
    module LMDB
    end

    # we can do redis or whatever later lol
  end
end
