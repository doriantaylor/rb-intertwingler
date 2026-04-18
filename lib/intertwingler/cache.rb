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
    # * hash the message body (if present), for the initial state.
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
    #   `Name: value` for the pairs, and \x1f between them.
    # * add `\x1e` and the serialized headers (which may be the empty
    #   string) to whichever variant of the hash matched the signpost.
    # * now try looking up the new key. if it hits, that's the
    #   terminal entry for that resource.
    #
    # Now we discuss cache entry layouts.
    #
    # The first byte of the entry will consist of control flags. The
    # MSB determines whether the entry is a signpost (1) or terminal
    # (0). The rest of the bits flag aspects of the terminal
    # record. On a signpost record, the only thing that follows is the
    # list of `Vary` headers, delineated by `\x1f`. These may be
    # compressed.
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
      # actually fuck it here's how we're gonna do it:
      #
      # * one table; key is sha256(body + method + normalized full uri + maybe user)
      #
      # * if there are Vary: headers, the basic entry should indicate
      #   which ones they are
      #
      # * second lookup: sha256(body + method + uri + user + normalized Vary headers)
      #
      # * payload is (compressed) response header set plus hash
      #   identifier of representation

      # keep the hash around and just add to it for the second lookup
      # to save (the minuscule amount of) reprocessing

    end

    # we can do redis or whatever later lol
  end
end
