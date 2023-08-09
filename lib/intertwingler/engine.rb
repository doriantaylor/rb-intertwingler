require 'intertwingler/handler'

# This is the engine. This is the thing that is run.
class Intertwingler::Engine < Intertwingler::Handler

  private

  public

  # Initialize the engine.
  #
  # @param resolvers [Array<Intertwingler::Resolver>] the necessary resolvers.
  #
  def initialize resolvers, handlers: [], transforms: {}
    @handlers = handlers

    super resolvers
  end

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
  def new_request req, uri, method: :GET, headers: {}, body: nil
    env = req.env
  end

  # Fake up a request and run
  # the main handler. Returns the (potentially wrapped) body. Will
  # throw an error if the response is anything but `200 OK`.
  def fetch req, uri, method: :GET, headers: {}, body: nil
    subreq = new_request req, uri, method: method, body: body
  end

  # This is the master handler that runs the engine and marshals all
  # other handlers and transforms.
  #
  # @param req [Rack::Request]
  #
  # @return [Rack::Response]
  #
  def handle req
    # cache the original request
    orig = req

    # XXX TODO normalize query parameters (à la Params::Registry)

    # 

    # resolve URI

    # run all request transforms

    # transforms can signal that they manipulate the request wholesale
    # by only accepting and returning message/http, meaning that if
    # they accept/return anything *else*, we know only to send the
    # body / manipulate the request/response accordingly.

    # XXX TODO

    # always assume the worst, then you can only be pleasantly surprised.
    resp = Rack::Response[404, {}, []]

    # poll content handlers until something returns non-404
    begin
      @handlers.each do |h|
        resp = h.handle req
        break unless resp.status == 404
      end
    rescue Exception => e
      resp = Rack::Response[500,
        { 'Content-Type' => 'text/plain' }, [e.message]]
    end

    # run all response transforms

    # pre

    # path parameters

    # post

    # return the response
    resp
  end
end
