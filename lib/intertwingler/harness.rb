require 'intertwingler/handler'
require 'intertwingler/resolver'
require 'intertwingler/engine'
require 'intertwingler/loggable'
require 'pathname'

# This is the multiplexing harness introduced to partition the
# bootstrapping configuration
class Intertwingler::Harness < Intertwingler::Handler
  include Intertwingler::Loggable

  # Create a new instance of the harness.
  #
  # @param mapping [Hash{String=>RDF::Repository}] The relation
  #  mapping authorities (domains) to RDF repositories.
  #
  def initialize mapping, home: nil, log: nil, jwt: {}
    @home = home
    @log  = log

    if jwt and not jwt.empty?
      begin
        log.debug "Enabling user set by JWT"
        require 'jwt'
        require 'jwt-eddsa' if %w[ED25519].include? jwt[:algorithm]
        @jwt = jwt
      rescue LoadError => e
        if e.path == 'eddsa'
          warn "The 'rbnacl' gem is required for ED25519."
        else
          warn "You have a JWT configured but no 'jwt' gem installed."
        end

        raise e
      end
    end

    @engines = mapping.reduce({}) do |hash, pair|
      authority, repo = pair
      # get the resolver for the authority
      resolver = Intertwingler::Resolver.configure repo,
        authority: authority, log: self.log # note this is to call Loggable

      # from there, load the engine
      engine = Intertwingler::Engine.configure resolver: resolver, home: home

      # map the domain aliases as well
      ([resolver.base] + resolver.aliases).each do |uri|
        hash[uri.authority] = engine if /^https?$/i.match? uri.scheme
      end

      hash
    end

  end

  private

  BEARER = /^\s*Bearer\s+([0-9A-Za-z_-]+(?:\.[0-9A-Za-z_-]+)*)/

  public

  attr_reader :engines, :home

  # Dispatch the request to the appropriate engine.
  #
  #
  def handle req
    # read off the Host: header
    authority = req.get_header('HTTP_HOST').to_s.strip.downcase
    # get an override map for the authority otherwise assign itself
    # authority = @override.fetch authority.to_s.strip.downcase, authority

    # match the authority to an engine or otherwise 404
    engine = @engines[authority] or return Rack::Response[404, {}, []]

    # log.debug "Authorization: #{req.env['HTTP_AUTHORIZATION']}"
    # log.debug 'Bearer matches' if BEARER.match? req.env['HTTP_AUTHORIZATION']
    # log.debug @jwt.inspect

    # handle jwt
    if @jwt and bearer = req.env['HTTP_AUTHORIZATION'] and
        m = BEARER.match(bearer)

      token = m.captures.first

      key, algo = @jwt.values_at :secret, :algorithm

      begin
        obj = JWT.decode(token, key, true, { algorithm: algo })
      rescue JWT::DecodeError => e
        return [409, {}, ["Could not decode JWT (#{token}), #{e} (#{e.class})"]]
      rescue e
        warn e.message
        return [500, {}, ["Server error (check logs)"]]
      end

      # XXX better logging ???
      if obj and obj.first.is_a? Hash and principal = obj.first['sub']
        req.env['REMOTE_USER'] = principal
        log.debug "Retrieved #{req.env['REMOTE_USER']} from JWT"
      else
        log.debug "JWT: #{obj.inspect}"
      end
    end

    # forward request to engine
    engine.handle req
  end
end
