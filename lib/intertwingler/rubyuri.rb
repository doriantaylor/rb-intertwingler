require 'intertwingler/version' # for the
require 'uri'

class Intertwingler::RubyURI < URI::Generic
  def path
    opaque.split(?;).first
  end

  def module
    opaque.split(?;, 2).last
  end
end

URI.register_scheme 'RUBY', Intertwingler::RubyURI
