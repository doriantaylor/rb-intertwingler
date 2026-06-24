require 'intertwingler/error'
require 'rack/request'
require 'rack/response'
require 'store/digest/object'

# Everything in {Intertwingler} is a handler.
#
class Intertwingler::Handler

  # do this to declare the symbol
  class ::Intertwingler::Engine < self
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

    # XXX DO WE WANT THIS HERE??
    if forwarded = req.env['HTTP_FORWARDED']
      # we only care about the first one
      forwarded = forwarded.strip.downcase.split(/\s*,\s*/).first
      forwarded = forwarded.split(/\s*;\s*/).map do |pair|
        # XXX we should really parse this properly but echhh
        k, v = pair.gsub(/['"]/, '').split(/\s*=\s*/, 2)
        [k.to_sym, v]
      end.to_h

      req.env['HTTP_HOST']   = forwarded[:host] if forwarded[:host]
      req.env['REMOTE_ADDR'] = forwarded[:for]  if forwarded[:for]
    elsif forwarded = req.env['HTTP_X_FORWARDED_HOST']
      fwdfor   = req.env['HTTP_X_FORWARDED_FOR']
      fwdproto = req.env['HTTP_X_FORWARDED_PROTO']

      req.env['HTTP_HOST']   = forwarded
      req.env['REMOTE_ADDR'] = fwdfor if fwdfor
      if /https/i =~ fwdproto.to_s
        req.env['HTTPS'] = 'on'
      end
    end

    handle(req).finish
  end

  # Normalize a set of request headers into something that can be
  # counted on downstream.
  #
  # @note This method is 100% provisional.
  #
  # @param req [Rack::Request] a Rack request.
  # @param as_symbols [false, true] whether to coerce keys to symbols
  # @param split [false, true] whether to split multi-valued headers
  #
  # @return [Hash] the normalized header set
  #
  def normalize_headers req, as_symbols: false, split: false
    req.env.select do |k|
      %w[CONTENT_TYPE CONTENT_LENGTH].include?(k) or k.start_with? 'HTTP'
    end.reduce({}) do |hash, pair|
      key = pair.first.downcase.delete_prefix('http_').tr_s(?_, ?-)
      key = key.to_sym if as_symbols
      val = pair.last
      val = val.split(/\s*,+\s*/) if split
        hash[key] = val
      hash
    end
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
