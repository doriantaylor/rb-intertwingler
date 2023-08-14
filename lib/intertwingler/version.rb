# might as well do this here because this module is guaranteed to get loaded
require 'mimemagic'

module Intertwingler
  VERSION = '0.1.5'

  # XXX this is not strictly correct but good enough for now
  [
   ['text/n3', %w(n3 ttl nt), %w(text/plain), [[0..256, '@prefix']]],
   ['application/x-vnd.sass', %w(sass), %w(text/plain), []],
   ['application/x-vnd.sass.scss', %w(scss), %w(text/css), []],
  ].each do |magic|
    MimeMagic.add magic[0], extensions: magic[1],
      parents: magic[2], magic: magic[3]
  end
end
