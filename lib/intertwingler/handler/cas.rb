require 'intertwingler/handler'
require 'store/digest'
require 'store/digest/http'

class Intertwingler::Handler::CAS < Intertwingler::Handler
  def initialize engine, **options
    super

    @store = Store::Digest.new(**options)
    @proxy = Store::Digest::HTTP.new(@store, base: @engine.resolver.base)
  end

  def handle req

    if body = req.body
      File.open('/tmp/wtf.lol', 'wb') { |fh| fh << body.read }
      body.seek 0 if body.respond_to? :seek
    end

    # XXX handle OPTIONS *
    @proxy.handle req
  end
end
