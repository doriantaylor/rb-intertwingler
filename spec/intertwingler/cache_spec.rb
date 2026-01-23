require 'spec_helper'

describe Intertwingler::Cache do
  # XXX note i am organizing this by storage and retrieval for now but
  # they will likely be intermixed because how are you gonna test this
  # thing otherwise lol

  # STORAGE

  # do not store a response to any request method other than GET (HEAD?) and QUERY

  # do not store if `no-cache` is present in response (for now)

  # do not store if `no-store` is present (in either request or response)

  # do not store if `private` is present and request is not authenticated

  # do not store errors (for now)

  # do not store redirects (for now)

  # do not store incomplete responses (for now; 206/Content-Range header)

  # do not store 304 responses

  # do not store if response is not 200 and `must-understand` is set

  # do not store if `max-age=0`

  # do not store if not authenticated and `s-maxage=0`

  # do not store if freshness cannot be determined
  # * `Expires` header
  # * `s-maxage` directive (and no authentication)
  # * `max-age` directive
  # * heuristic

  # (ensure freshness is calculated in that order)

  # store a response

  # store a private response

  # store a varied response

  # store a varied private response

  # store a QUERY with request body

  # expect a cache miss with different request body

  # RETRIEVAL

  # do not retrieve if request has `no-cache` or `no-store`

  # do not retrieve if `must-revalidate` (for now; no point)

  # (obviously) do not retrieve if no match

  # do not complete retrieval if entry can't satisfy directives
  # * `max-age`
  # * `min-fresh`
  # * `max-stale`

  # do not complete retrieval if stale

  # return 304 not modified if not newer than `If-Modified-Since`

  # return 304 not modified if `ETag` matches

  # return 503 if `only-if-cached` and response not present in cache

  # get that object back

  # get the object with a user

  # a subsequent response with a `public` directive to an identical
  # (authenticated) request supersede any `private` entry

end
