require 'intertwingler/handler'
require 'intertwingler/resolver'

# This is the multiplexing harness introduced to partition the
# bootstrapping configuration
class Intertwingler::Harness < Intertwingler::Handler

  # Create a new instance of the harness.
  #
  # @param mapping [Hash{String=>RDF::Repository}] The relation
  #  mapping authorities (domains) to RDF repositories.
  #
  def initialize **mapping
    # get the intersection of resolvers with authorities

    resolvers = mapping.reduce({}) do |hash, pair|
      authority, repo = pair
      resolver = Intertwingler::Resolver.configure repo, authority: authority

      ([resolver.base] + resolver.aliases).each do |uri|
        hash[uri.authority] = resolver if uri.scheme.downcase.start_with? 'http'
      end

      hash
    end

    warn resolvers.inspect

    # from there, load the engines

    # map the domain aliases as well

    # (the engines take it from there)
  end

  # Dispatch the request to the appropriate engine.
  def handle req
    # read off the Host: header

    # match the authority to an engine or otherwise 404

    # forward request to engine
  end
end
