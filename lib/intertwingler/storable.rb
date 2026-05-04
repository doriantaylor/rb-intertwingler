require 'store/digest'
require 'intertwingler/types'
require 'intertwingler/cache'

# Add content-addressable store functionality to a handler.
#
module Intertwingler::Storable
  private

  # Initialize the onboard {Store::Digest} instance.
  #
  # @note this is only meant to be run in the constructor.
  #
  # @param store [Store::Digest, Hash, nil] the instance or its config
  #
  # @return [Store::Digest]
  #
  def init_store store
    return store if store.is_a? Store::Digest

    raise ArgumentError, 'store must be Hash if defined' unless
      store.nil? or store.is_a? Hash

    # fill in the defaults
    store = Intertwingler::Types::StoreConfig[store || {}]

    # make path absolute if the driver has one
    store[:dir] = @home + store[:dir] if @home && store[:dir]

    # aand here's the store
    @store = Store::Digest.new **store
  end

  public

  attr_reader :store
end

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
    return cache if cache.is_a? Intertwingler::Cache

    raise ArgumentError, 'cache must be Hash if defined' unless
      cache.nil? or cache.is_a? Hash
    # fill in defaults
    cache = Intertwingler::Types::CacheConfig[cache || {}]

    # make path absolute (but only if the driver has one)
    cache[:dir] = @home + cache[:dir] if @home && cache[:dir]

    # gimme da caccchhhe
    @cache = Intertwingler::Cache.new store: @store, **cache
  end

  public

  attr_reader :cache
end
