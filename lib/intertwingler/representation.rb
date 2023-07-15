require 'intertwingler/version'

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
  include Enumerable

  private

  # subclasses should set this
  OBJECT_CLASS = nil

  def assert_io_like obj
    raise ArgumentError, "object of #{obj.class} is not IO-ey enough" unless
      IO === obj or obj.respond_to? :each
    obj
  end

  def coerce_type type
    return type if type.is_a? Intertwingler::MimeMagic
    Intertwingler::MimeMagic.new type.to_s
  end

  def coerce_rfc5646 language
    # i dunno i don't really feel like being smarter than this
    language.to_s.downcase.strip.gsub(/[[:space:]_]/, ?-).tr_s(?-, ?-)
  end

  def coerce_charset charset
    # i dunno right yet
    charset.to_s.downcase.strip
  end

  DEFAULT_TYPE = 'application/octet-stream'.freeze

  public

  def self.default_type
    DEFAULT_TYPE
  end

  attr_reader :io, :type, :language, :charset

  def initialize io, type: DEFAULT_TYPE, language: nil, charset: nil, **options
    raise NotImplementedError, 'Subclasses need an OBJECT_CLASS' unless OBJECT_CLASS
    @io       = assert_io_like io
    @type     = coerce_type(type || DEFAULT_TYPE)
    @language = coerce_rfc5646 language if language
    @charset  = coerce_charset charset  if charset
  end

  def each &block
    raise NotImplementedError, 'subclasses must implement each'
  end

  # Return the in-memory representation of the object.
  def object
    raise NotImplementedError, 'subclasses must implement object'
  end

  # Nokogiri is for HTML/XML.
  class Nokogiri < self
  end

  # Vips is used for raster images.
  class Vips < self
  end
end
