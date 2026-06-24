require 'intertwingler/version'

module Intertwingler::Error

  class Config < ArgumentError
  end

  # This is the abstract parent {::Exception} class that acts as an escape
  # hatch for responses that are something _other_ than 200-series,
  # i.e. they are not-successful (albeit not strictly _unsuccessful_)
  # responses.
  class HTTPStatus < RuntimeError
    STATUS = nil

    def initialize message, status: nil
      @status = status || self.class.const_get(:STATUS)

      super message
    end

    attr_reader  :status
    alias_method :code, :status

    def response
      Rack::Response[status, { 'content-type' => 'text/plain' }, [message]]
    end
  end

  # Redirects are an example of not-successful-yet-not-unsuccessful responses.
  class Redirect < HTTPStatus
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
      super message, status: status || 302
    end

    attr_reader :location

    def response
      hdr = { 'content-type' => 'text/plain' }
      hdr['location'] = location.to_s if location
      Rack::Response[status, hdr, StringIO.new(message)]
    end
  end

  # This is the superclass of HTTP errors.
  #
  class HTTPError < HTTPStatus
    def response
      Rack::Response[status, { 'content-type' => 'text/plain' },
        [message, (backtrace || []).join("\n")]]
    end
  end

  class ClientError < HTTPError
    def initialize message, status: nil
      super message, status: status
    end

    class BadRequest < self
      STATUS = 400
    end

    class Forbidden < self
      STATUS = 403
    end

    class NotFound < self
      STATUS = 404
    end

    class NotAllowed < self
      STATUS = 405

      attr_reader :request_method

      def initialize message, status: 405, method: 'GET'
        @request_method = method

        super message, status: status
      end
    end

    class Conflict < self
      STATUS =  409
    end
  end

  class ServerError < HTTPError
    STATUS = 500
    def initialize message, status: nil
      super message, status: status || 500
    end

    class NotImplemented < self
      STATUS = 501
    end
  end

end
