require 'spec_helper'
require 'pathname'
require 'tmpdir'
require 'rack'
require 'rack/mock'

require 'intertwingler/cache'

BASE = Pathname(Dir.tmpdir).expand_path + 'intertwingler-cache'

describe Intertwingler::Cache::KeyState do
  it 'should work lol' do
    env = Rack::MockRequest.env_for 'http://lol.dongs/hurr'
    # env['REMOTE_USER'] = 'deuce@jerkcity.com'
    req = Rack::Request.new env

    state = Intertwingler::Cache::KeyState.new req

    expect(state.authed?).to be_falsey
  end
end

describe Intertwingler::Cache do

  before :context do
    @store = Store::Digest.new dir: BASE + 'cas', mapsize: 2**27
  end

  # after :context do
  #   @cache = nil
  #   @store = nil
  #   FileUtils.rm_rf BASE
  # end

  # are we cargo-culting here?
  subject { @cache ||= Intertwingler::Cache.new store: @store, dir: BASE }

  it 'should store a simple cacheable GET request' do
    env = Rack::MockRequest.env_for 'http://lol.dongs/hurr'
    req = Rack::Request.new env

    resp = subject.fetch req do |r|
      Rack::Response[200, { 'content-type' => 'text/plain' }, ['duhh']]
    end

  end

  # XXX note i am organizing this by storage and retrieval for now but
  # they will likely be intermixed because how are you gonna test this
  # thing otherwise lol

  # STORAGE

  # do not store a response to any request method other than GET (HEAD?) and QUERY

  # do not store if `no-store` is present (in either request or response)

  # storing (confusingly) OK if `no-cache` is present in response

  # do not store if `private` is present and request is not authenticated

  # do not store errors (for now)

  # do not store redirects (for now)

  # do not store incomplete responses (for now; 206/Content-Range header)

  # never store 304 responses

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

  # store a different key if *request* is `no-transform`

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

  # a subsequent, newer response with a `public` directive to an identical
  # (authenticated) request supersede any `private` entry

end
