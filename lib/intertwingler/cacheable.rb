require_relative 'types'
require_relative 'storable'
require_relative 'cache'

# Add HTTP cache functionality to a handler. Implies
# {Intertwingler::Storable}.
#
module Intertwingler::Cacheable
  include Intertwingler::Storable

  private

  # Initialize the onboard {Intertwingler::Cache} instance.
  #
  # @note this is only meant to be run in the constructor
  #
  # @param cache [Intertwingler::Cache, Hash, nil] the instance or its config
  #
  # @return [Intertwingler::Cache]
  #
  def init_cache cache
    # XXX now wondering if i actually want to do this
    return cache if cache.is_a? Intertwingler::Cache

    raise ArgumentError, 'cache must be Hash if defined' unless
      cache.nil? or cache.is_a? Hash
    # fill in defaults
    cache = Intertwingler::Types::CacheConfig[cache || {}]

    # make path absolute (but only if the driver has one)
    cache[:dir] = @home + cache[:dir] if @home && cache[:dir]

    # gimme da caccchhhe
    Intertwingler::Cache.new store: store, **cache
  end

  public

  attr_reader :cache
end
