require 'intertwingler/handler'
require 'intertwingler/resolver'
require 'intertwingler/engine'
require 'pathname'

# This is the multiplexing harness introduced to partition the
# bootstrapping configuration
class Intertwingler::Harness < Intertwingler::Handler

  # Create a new instance of the harness.
  #
  # @param mapping [Hash{String=>RDF::Repository}] The relation
  #  mapping authorities (domains) to RDF repositories.
  #
  def initialize mapping, home: nil
    @home = home
    @engines = mapping.reduce({}) do |hash, pair|
      authority, repo = pair
      # get the resolver for the authority
      resolver = Intertwingler::Resolver.configure repo, authority: authority

      # from there, load the engine
      engine = Intertwingler::Engine.configure resolver: resolver, home: home

      # map the domain aliases as well
      ([resolver.base] + resolver.aliases).each do |uri|
        hash[uri.authority] = engine if /^https?$/i.match? uri.scheme
      end

      hash
    end

  end

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

    # forward request to engine
    engine.handle req
  end
end
