require 'intertwingler/handler'
require 'intertwingler/graphops'

# This class encapsulates an individual (presumably generated) HTTP
# resource. It sits between an {Intertwingler::Handler} and whatever
# code generates the response's representation, and handles any
# preprocessing e.g. of query parameters or the request body. This
# class itself is an abstract superclass and is not meant to be used
# directly. Subclasses of this class are intended to be instantiated
# by a handler, when the handler itself is instantiated or otherwise
# refreshed.
#
# To use this class, subclass it and create a method with the same
# name as the request method except transliterated to
# `.downcase.tr_s(?-, ?_)` (so `GET` becomes `get`, or
# `VERSION-CONTROL` becomes `version_control`).  The method _must_
# return a {::Rack::Response} and _may_ raise an
# {Intertwingler::Handler::AnyButSuccess}.
class Intertwingler::Resource
  include Intertwingler::GraphOps::Addressable

  private

  # from https://www.iana.org/assignments/http-methods/http-methods.xhtml
  #
  # any (instance) methods with these names (modulo `.downcase.tr_s(?-,?_)`)
  # are automatically routed; otherwise http method FOO is represented by
  # `def http_foo`…
  #
  METHODS = (<<~METH).strip.split.map(&:to_sym)
  ACL BASELINE-CONTROL BIND CHECKIN CHECKOUT CONNECT COPY DELETE GET
  HEAD LABEL LINK LOCK MERGE MKACTIVITY MKCALENDAR MKCOL MKREDIRECTREF
  MKWORKSPACE MOVE OPTIONS ORDERPATCH PATCH POST PRI PROPFIND PROPPATCH
  PUT REBIND REPORT SEARCH TRACE UNBIND UNCHECKOUT UNLINK UNLOCK UPDATE
  UPDATEREDIRECTREF VERSION-CONTROL
  METH

  SUBJECT = nil

  public


  # @!attribute [r] handler
  #  @return [Intertwingler::Handler] the associated handler
  #
  attr_reader :handler, :subject

  # @!attribute [r] subject
  #  @return [RDF::URI] the (durable, canonical) URI of the resource
  #
  def self.subject
    const_get :SUBJECT
  end

  # Initialize a new resource object.
  #
  def initialize handler, subject = nil, **args
    @handler = handler
    @subject = resolver.uuid_for(subject || self.class.subject, verify: false)
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
  def call method, uri, params: {}, headers: {}, body: nil
    # keep the original method untouched
    to_call = method.dup
    # set the method name to `http_whatever` for unregistered methods
    to_call = "http_#{to_call}" unless METHODS.include? method.to_s.strip.to_sym
    # normalize the request method to a ruby method
    to_call = to_call.to_s.strip.downcase.tr_s(?-, ?_).to_sym

    raise Intertwingler::Handler::Error::NotAllowed.new(
      "This resource does not respond to #{method} requests.",
      method: method) unless respond_to? to_call

    # warn engine.registry.groups.inspect

    # warn "inside resource: #{uri}"

    # handle the params XXX MAY RAISE
    # instance = engine.registry[subject].process params
    instance = engine.registry.process params || uri.query, defaults: true

    # warn instance.inspect

    # this will already be wrapped in a rescue block upstream
    send to_call, uri, params: instance, headers: headers, body: body
  end

end
