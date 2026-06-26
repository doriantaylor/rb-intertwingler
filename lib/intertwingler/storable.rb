require 'store/digest'
require_relative 'types'

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

    # try this and see if it does anything; it was on before though
    # and didn't make a difference (and it still doesn't)
    # store[:notls] = true

    # aand here's the store
    Store::Digest.new **store
  end

  public

  attr_reader :store
end
