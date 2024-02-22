require 'intertwingler/error'
require 'rack/request'
require 'rack/response'

# Everything in {Intertwingler} is a handler.
#
class Intertwingler::Handler

  # do this to declare the symbol
  class ::Intertwingler::Engine < self
  end

  # This is the abstract parent Exception class that acts as an escape
  # hatch for responses that are something _other_ than 200-series,
  # i.e. they are not-successful (albeit not strictly _unsuccessful_)
  # responses.
  class AnyButSuccess < Exception
    def initialize message, status: nil
      @status = status

      super message
    end

    attr_reader  :status
    alias_method :code, :status

    def response
      Rack::Response[status, { 'content-type' => 'text/plain' }, [message]]
    end
  end

  # Redirects are an example of not-successful-yet-not-unsuccessful responses.
  class Redirect < AnyButSuccess
    # Make a new redirect "exception"
    #
    # @param message [#to_s] the error message
    # @param status [Integer] the response code
    # @param location [URI, RDF::URI, #to_s, nil]
    # @param as [:uri, :rdf] URI coercion type
    #
    def initialize message, status: nil, location: nil, as: :uri
      @location =
        Intertwingler::Resolver.coerce_resource location, as: as if location
      super message, status || 302
    end

    attr_reader :location

    def response
      hdr = {}
      hdr['location'] = location.to_s if location
      Rack::Response[status, hdr, [message]]
    end
  end

  class Error < AnyButSuccess
    def response
      Rack::Response[status, { 'content-type' => 'text/plain' },
        [message, (backtrace || []).join("\n")]]
    end

    class Client < self
      def initialize message, status: nil
        super message, status: status || 403
      end
    end
    class Server < self
      def initialize message, status: nil
        super message, status: status || 500
      end
    end
  end

  # Handle a {Rack::Request}. Return a {Rack::Response}.
  #
  # @param req [Rack::Request] the request.
  #
  # @return [Rack::Response] the response.
  #
  def handle req
    raise NotImplementedError, 'Subclasses must implement their own `handle`'
  end

  # Handle a Rack request from the wire.
  #
  # @param env [Hash, Rack::Request] the Rack environment or request.
  #
  # @return [Array<(Integer, Hash, #each)>] the response.
  #
  def call env
    # XXX maybe wrap this or put it in a base class i dunno
    req = env.is_a?(Rack::Request) ? env : Rack::Request.new(env)

    handle(req).finish
  end

  # Initialize a handler.
  #
  # @param engine [Intertwingler::Engine]
  # @param args [Hash{Symbol => Object}]
  #
  def initialize engine, **args
    raise ArgumentError, 'engine must be an Intertwingler::Engine' unless
      engine.is_a? ::Intertwingler::Engine
    @engine = engine
  end

  attr_reader :engine

  # Get the {Intertwingler::Resolver} for the given request.
  #
  # @return [Intertwingler::Resolver, nil] the resolver, maybe
  #
  def resolver
    @engine.resolver
  end

  # Get the resolver's graph for the given request.
  #
  # @return [RDF::Repository] the graph.
  #
  def repo
    @engine.repo
  end

  # Get the engine's logger.
  #
  # @return [Logger] the logger object.
  #
  def log
    @engine.log
  end
end
