# bring in the namespace
require 'intertwingler/version'

require 'mimemagic'
# XXX this is not strictly correct but good enough for now, also
# application/x-www-form-urlencoded is not in the mime types (and thus
# has no `canonical)`, so that was fun to debug (oh look turns out
# multipart/form-data isn't in there either)
[
 ['application/x-www-form-urlencoded', [], %w(text/plain), []],
 ['multipart/form-data', [], %w(application/octet-stream), []],
 ['text/n3', %w(n3 ttl nt), %w(text/plain), [[0..256, '@prefix']]],
 ['application/x-vnd.sass', %w(sass), %w(text/plain), []],
 ['application/x-vnd.sass.scss', %w(scss), %w(text/css), []],
].each do |magic|
  MimeMagic.add magic[0], extensions: magic[1],
    parents: magic[2], magic: magic[3]
end

# bring in the patients
require 'intertwingler/util/clean'
require 'intertwingler/util/messy'

# 2021-12-27: Here's the plan for this thing:
#
# * rename {Intertwingler::Util} to {Intertwingler::Util::Messy}
#
# * create {Intertwingler::Util::Clean} which will eventually be the new
#   {Intertwingler::Util}
#
# * create a temporary {Intertwingler::Util} that yokes `Clean` and `Messy`
#   back together
#
# * move all genuine bona fide *stateless* utility functions that are
#   used in more than one place to {Intertwingler::Util::Clean}
#
# * refactor `Messy` until it ceases to exist
#
# * rename `Clean` to {Intertwingler::Util}
#
module Intertwingler::Util
  include Clean
  include Messy

  # Look, it was easier to write an LRU cache than figure out which
  # off-the-shelf one to use.
  #
  class LRU < Hash
    attr_accessor :capacity

    def initialize default = nil, capacity: nil
      @capacity = capacity || Float::INFINITY
      super default
    end

    def [] key
      if key? key
        # delete the key from the contents
        value = delete key
        self[key] = value
      end
    end

    def []= key, value
      n = size - capacity

      keys.take(n).each { |k| delete k } if n > 0

      super key, value
    end

    def fetch key, default = nil, &block
      return self[key] if key? key
      return block.call key if block
      return default

    end

    def fetch_values *keys, &block
      keys.map { |k| fetch k, &block }
    end

    def values_at *keys
      fetch_values(*keys)
    end

    def to_h
      dup
    end
  end
end
