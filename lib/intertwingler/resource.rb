require 'intertwingler/handler'

# This class encapsulates an individual (presumably generated) HTTP
# resource. It sits between an {Intertwingler::Handler} and whatever
# code generates the response's representation, and handles any
# preprocessing e.g. of query parameters or the request body. This
# class itself is an abstract superclass and is not meant to be used
# directly. Subclasses of this class are intended to be instantiated
# by a handler, when the handler itself is instantiated or otherwise
# refreshed.
class Intertwingler::Resource

  private

  # from https://www.iana.org/assignments/http-methods/http-methods.xhtml
  METHODS = (<<~METH).strip.split.map(&:to_sym)
  ACL BASELINE-CONTROL BIND CHECKIN CHECKOUT CONNECT COPY DELETE GET
  HEAD LABEL LINK LOCK MERGE MKACTIVITY MKCALENDAR MKCOL MKREDIRECTREF
  MKWORKSPACE MOVE OPTIONS ORDERPATCH PATCH POST PRI PROPFIND PROPPATCH
  PUT REBIND REPORT SEARCH TRACE UNBIND UNCHECKOUT UNLINK UNLOCK UPDATE
  UPDATEREDIRECTREF VERSION-CONTROL
  METH

  public

  attr_reader :handler, :uri

  # @!attribute [r] handler
  #  @return [Intertwingler::Handler] the associated handler
  #
  # @!attribute [r] uri
  #  @return [RDF::URI] the (durable, canonical) URI of the resource

  # Initialize a new resource object.
  #
  def initialize handler, uri, **args
    @handler = handler
    @uri     = resolver.uuid_for uri
  end

  # @!attribute [r] engine
  #  @return [Intertwingler::Engine] shortcut for the handler's engine
  def engine ; @handler.engine ; end

  # @!attribute [r] resolver
  #  @return [Intertwingler::Resolver] shortcut for the engine's resolver
  def resolver ; engine.resolver ; end

  # @!attribute [r] repo
  #  @return [Intertwingler::GraphOps] shortcut for the resolver's graph
  def repo ; resolver.repo ; end

  # Call the resource with the given request method. Include headers,
  # a body, and query parameters when applicable. After preprocessing,
  # the call is forwarded to a method defined in the subclass
  #
  # @param method [Symbol, #to_sym, #to_s] the request method
  # @param params [Hash] query parameters, semi-processed by the handler
  # @param headers [Hash] any relevant request headers
  # @param body [nil, #read, #call, #to_s] something that can pass for a request body
  #
  # @raise [Intertwingler::Handler::Redirect] when the response needs
  #  to be redirected
  # @raise [Intertwingler::Handler::Error] when there is a client or
  #  server error
  #
  # @return [Rack::Response] the response to pass upstream
  #
  def call method, params: {}, headers: {}, body: nil
    to_call = method
    # set the method name to `http_whatever` for unregistered methods
    to_call = "http_#{to_call}" unless METHODS.include? method.to_s.strip.to_sym
    # normalize the request method to a ruby method
    to_call = to_call.to_s.strip.downcase.tr_s(?-, ?_).to_sym

    begin
      send to_call, params: params, headers: headers, body: body
    rescue NoMethodError
      raise Intertwingler::Handler::Error::NotAllowed.new(
        "This resource does not respond to #{method} requests.", method: method)
    end
  end

end
