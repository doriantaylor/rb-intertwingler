require 'intertwingler/version'
require 'mimemagic'

# janky bolt-on MimeMagic
class Intertwingler::MimeMagic < ::MimeMagic

  # XXX this is not strictly correct but good enough for now
  [
    ['text/n3', %w(n3 ttl nt), %w(text/plain), [[0..256, '@prefix']]],
    ['application/x-vnd.sass', %w(sass), %w(text/plain), []],
    ['application/x-vnd.sass.scss', %w(scss), %w(text/css), []],
  ].each do |magic|
    self.add magic[0], extensions: magic[1], parents: magic[2],
      magic: magic[3]
  end

  def self.binary? thing
    sample = nil

    # get some stuff out of the IO or get a substring
    if thing.is_a? IO
      pos = thing.tell
      thing.seek 0, 0
      sample = thing.read 100
      thing.seek pos
    else
      sample = thing.to_s[0,100]
    end

    # consider this to be 'binary' if empty
    return true if sample.length == 0
    # control codes minus ordinary whitespace
    /[\x0-\x8\xe-\x1f\x7f]/.match? sample.b
  end

  def self.default_type thing
    new(self.binary?(thing) ? 'application/octet-stream' : 'text/plain')
  end

  def self.by_extension io
    super(io) || default_type(io)
  end

  def self.by_path io
    super(io) || default_type(io)
  end

  def self.by_magic io
    super(io) || default_type(io)
  end

  def self.all_by_magic io
    out = super(io)
    out.empty? ? [default_type(io)] : out
  end

  def initialize string
    # XXX in some formulations (quoted strings) this will be wrong obvs
    string, *params = string.split(/\s*;+\s*/)
    @params = params.map { |x| x.split(/\s*=\s*/, 2) } unless params.empty?
    super string
  end

  def inspect
    out = @type
    out = [out, @params.map { |x| x.join ?= }].join ?; if
      @params and !@params.empty?
    %q[<%s "%s">] % [self.class, out]
  end

  # fetches immediate parent types
  def parents
    out = ::MimeMagic::TYPES.fetch(type, [nil, []])[1].map { |x| self.class.new x }
    # add this unless we're it
    out << self.class.new('application/octet-stream') unless
      type.downcase == 'application/octet-stream'
    out
  end

  # fetches all supertypes
  def lineage
    ([self] + parents.map { |t| t.lineage }.flatten).uniq
  end

  alias_method :ancestor_types, :lineage

  private

  def method_missing id
    @type.send id
  end

end
