require 'intertwingler/version' # for the symbol

require 'forwardable'
require 'mimemagic'
require 'tempfile'

# This class is a cheap knockoff of a
# {https://en.wikipedia.org/wiki/Monad_(functional_programming) monad}
# for handling successive transformations to a given resource
# representation within the same process. The goal is to mitigate the
# amount of times a (potentially large) object gets serialized and
# reparsed. It works by wrapping a Rack body (either request or
# response, it works on both), and associating it with a parser
# (via parser-specific subclasses).
#
# 
# This is a base class for what are called "representations" in [the
# Fielding dissertation](https://www.ics.uci.edu/~fielding/pubs/dissertation/top.htm).
#
# In REST parlance, a resource is a relation between a set of one or
# more identifiers (URIs) and a set of one or more representations. A
# representation has (or rather *is*) a segment of bytes of finite
# length, of a given [content-type](https://www.iana.org/assignments/media-types/).
# If applicable, a representation also may have a [character
# set](https://www.iana.org/assignments/character-sets/) and a
# content-encoding (i.e. compression).
#
# 
#
# Of additional interest to us is the fact that a resource's
# representation has to *come* from somewhere: say a file system or
# analogous blob storage. Otherwise, it has to be generated.
#
# Finally, we want representations to be amenable to *transformation*,
# and we want those transformations to be *composable*. Within the
# confines of a running process (and therefore, programming language),
# it is advantageous, where applicable, to have, in addition to the
# byte segment, the parsed representation ready for manipulation,
# rather than the expensive proposition of executing successive
# parsing and serialization operations.
#
# > A couple asides here, since I don't know where else to put them:
# > one is that having multiple in-memory copies of a representation
# > (e.g. the results of successive transformations) is going to be
# > crazy wasteful memory-wise. We could therefore consider the
# > in-memory representation as a *mutable* execution context for
# > transformation (pseudo)functions. In other words, each function
# > nominally *returns* the in-memory representation, but it's
# > actually applying successive operations to the same chunk of
# > memory. The role of the representation object would therefore be
# > to record which transformations have been applied.
# >
# > Another concern here is that a number of (?? i took a shower and
# > lost the thread) (was it that we're going to want to apply a
# > sequence of stock transformations to a number of content types?)
#
# We want to have an "origin" object (pattern? function?) that
# coalesces the possible places where a representation might be
# found. Origins can be things like directories on the ordinary file
# system, repositories under version control (e.g. git),
# content-addressable stores, or even other servers (e.g. reverse
# proxy, in which case it wouldn't be an "origin" as much as a "next
# hop"/"go fish" scenario). For purely-generated ("transparent")
# representations, we could have an origin function that constructs
# and emits a representation from scratch.
#
# > Indeed, all origins can be modeled as functions that take a URI
# > plus `Accept-*` headers (and potentially a few other parameters
# > like version) and resolve them to a representation.
#
# Any sufficiently complex system is going to have at least one origin
# and likely more than one. Some origins will be amenable to
# *manifests* (so you know what's on them). Some will be able to
# respond to requests for variants e.g. by `Content-Type`,
# `Content-Language`, `Content-Encoding`, as well as character set,
# and version. Preference for origin may vary depending on the
# resource in question or any of the `Accept-*` headers. We could
# imagine a successive process of elimination that tests each origin
# for variants, to which interesting return codes are 401, 403, 404,
# 406, or of course 200. Ranged requests are probably not the best
# idea, but they might be okay. Redirects are definitely off-limits.
#
# 1. use 
#
class Intertwingler::Representation
  extend Forwardable

  # just enough io methods
  def_delegators :io,
    :each, :read, :gets, :seek, :pos, :tell, :length, :size, :flush, :close

  private

  # subclasses should set this
  OBJECT_CLASS = nil
  DEFAULT_TYPE = 'application/octet-stream'.freeze
  VALID_TYPES  = [DEFAULT_TYPE].freeze

  # make a temp file with presets
  def tempfile
    Tempfile.new 'repr-', encoding: Encoding::BINARY
  end

  def coerce_io_like obj
    # just give us the io if it's one of ours
    return obj.io if obj.is_a? Intertwingler::Representation

    if obj.respond_to? :each
      return obj if obj.is_a? IO or
        %i[getc read seek close].all? { |m| obj.respond_to? m }
      # this would be where we upgrade the IO object to a seekable thing
      io = tempfile
      obj.each { |x| io << x }
      return io
    elsif objs.respond_to? :call
      # okay then it's a rack streaming response body
      obj.call(io = tempfile)
      return io
    end

    raise ArgumentError, "object of #{obj.class} is not IO-ey enough"
  end

  def coerce_type type
    # this syntax sugar will automatically noop for MimeMagic objects
    type = MimeMagic[type]

    raise ArgumentError, "#{type} is not a valid type" unless
      valid_types.any? { |t| type.descendant_of? t }

    type
  end

  def coerce_rfc5646 language
    # i dunno i don't really feel like being smarter than this
    language.to_s.downcase.strip.gsub(/[[:space:]_]/, ?-).tr_s(?-, ?-)
  end

  def coerce_charset charset
    # i dunno right yet
    charset.to_s.downcase.strip
  end

  def parse io
    raise NotImplementedError,
      'subclasses must implement private method `parse`'
  end

  def serialize obj, target
    raise NotImplementedError,
      'subclasses must implement private method `serialize`'
  end

  public

  def self.object_class
    const_get :OBJECT_CLASS
  end

  def self.default_type
    const_get :DEFAULT_TYPE
  end

  def self.valid_types
    const_get(:VALID_TYPES).map { |t| MimeMagic[t] }
  end

  def object_class
    self.class.object_class
  end

  def default_type
    self.class.default_type
  end

  def valid_types
    self.class.valid_types
  end

  attr_reader :type
  attr_accessor :language, :charset

  def initialize obj, type: nil, language: nil, charset: nil, **options
    oc = object_class # call this once cause self.class is slow

    raise NotImplementedError, 'Subclasses need an OBJECT_CLASS' unless oc

    if obj.is_a? oc
      @object = obj
    else
      @io = coerce_io_like obj
    end

    @type     = coerce_type(type || cl.default_type)
    @language = coerce_rfc5646 language if language
    @charset  = coerce_charset charset  if charset
  end

  def self.coerce io, type: nil, language: nil, charset: nil, **options
    if io.is_a? self
      # this might be dumb?
      io.type     = type     if type
      io.language = language if language
      io.charset  = charset  if charset
      return io
    end
    new io, type: type, language: language, charset: charset, **options
  end

  def type= newtype
    newtype = coerce_type newtype

    # if this is different we're converting so we need to parse the io
    # if we haven't already
    if @type != newtype
      @type = newtype

      if @io
        @io.seek 0 if @io.respond_to? :seek
        @object ||= parse @io
      end
    end

    @type
  end

  def io
    warn "hi lol #{caller}"

    if @object
      @io = serialize @object, tempfile
      @io.seek 0 if @io.respond_to? :seek
      @object = nil
    end

    @io
  end

  def io= obj
    @object = nil
    @io = coerce_io_like obj
  end

  def object= obj
    cls = object_class # do this because self.class is slow
    raise ArgumentError,
      "object must be a #{cls}, not #{obj.class}" unless object.is_a? cls

    # wipe out the stale io
    @io = nil if @io
    @object = obj
  end

  # Return the in-memory representation of the object.
  def object
    @object ||= parse @io
  end

end
