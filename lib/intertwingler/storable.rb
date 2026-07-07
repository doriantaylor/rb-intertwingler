require_relative 'types'
require_relative 'field'
require 'store/digest'

require 'rack/request'
require 'rack/response'

# Add content-addressable store functionality to a class.
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

  # Add a body to the store.
  #
  # @param message [Rack::Request, Rack::Response] the HTTP message
  #
  # @return [Store::Digest::Entry]
  #
  def add_body message
    body = message.body || ''
    if body.is_a? Store::Digest::Entry
      body.add store
    else
      ct = Intertwingler::Field['content-type'][message].value
      ce = Intertwingler::Field['content-encoding'][message].value
      body = store.add body, type: ct, encoding: ce
    end
  end

  public

  attr_reader :store

  # Replace the HTTP message with one that has a
  # {Store::Digest::Entry} in the message body.
  #
  # @param message [Rack::Request, Rack::Response] the message
  #
  # @return [Rack::Request, Rack::Response] the altered message
  #
  def store_message message
    case message
    when Rack::Request
      # for some reason this needs an explicit transaction because it
      # tries to write to a read-only one somehow
      store.transaction do
        return message if message.body.is_a?(Store::Digest::Entry) &&
          store.has?(message.body)

        env = message.env.dup
        env['rack.input'] = add_body message
        Rack::Request.new env
      end
    when Rack::Response
      store.transaction do
        return message if message.body.is_a?(Store::Digest::Entry) &&
          store.has?(message.body)

        body = add_body message
        Rack::Response[message.status, message.headers, body]
      end
    else
      raise TypeError, "Can't manipulate an object of type #{message.class}"
    end
  end
end
