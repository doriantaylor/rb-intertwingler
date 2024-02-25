require 'intertwingler/version' # for the namespace
require 'uri/urn'

# This is a class for representing Ruby modules as URIs. It is
# probably more appropriate for this to be a URI rather than a URN,
# and it was not my first choice to do so, but changes to the URI
# module that ships with Ruby has made it impossible to register a
# scheme that contains a hyphen like `x-ruby`, which is the convention
# for non-standard schemes. Nevertheless this module tries to conform
# to RFC 8141 (with some provisional corrections to its proximate
# dependency).
#
# The canonical form `urn:x-ruby:module/path;Constant::Name?=p1=v1&p2=v2`
# contains enough information to load a module, and return whatever
# constant it points to, along with (not necessarily but what you
# could interpret as) initialization parameters.
#
# Note that there are no constraints for the kinds of path expressions
# or identifiers other than being syntactically valid. If, for
# example, an attacker had access to your file system, they could use
# this system to load a malicious Ruby module. Path expressions of the
# form `foo/bar` must necessarily resolve to something in the module
# search path, but `require` can take absolute and relative paths, so
# the scenario is potentially very mcuh like the Log4J
# vulnerability. I have yet to decide what to do about it.
class Intertwingler::RubyURN < URI::URN::Generic

  private

  # these are just character classes so we can merge them into actual regexes
  UNRESERVED = %[-0-9A-Za-z._~].freeze
  SUB_DELIMS = %[!$&'()*+,;=].freeze
  MY_SUBS    = %[!$&'()*+,=].freeze # same minus semicolon
  PCHAR_STR  = %[[#{UNRESERVED}#{SUB_DELIMS}:@]|%[0-9A-Fa-f]{2}].freeze
  MY_PCHAR   = %[[#{UNRESERVED}#{MY_SUBS}:@]|%[0-9A-Fa-f]{2}].freeze

  # like this one
  PCHAR  = /(?:#{PCHAR_STR})/o
  NID    = /[0-9A-Za-z][0-9A-Za-z-]{,30}[0-9A-Za-z]/o
  NSS    = /(?:#{PCHAR_STR})(?:#{PCHAR_STR}|\/)*/o
  RQ     = /(?:#{PCHAR_STR})(?:#{PCHAR_STR}|[\/\?])*/o
  FRAG   = /(?:#{PCHAR_STR}|[\/\?])*/o
  OPAQUE = /\A(#{NID}):(#{NSS})(?:\?\+(#{RQ})?)?(?:\?\=(#{RQ})?)?\z/o
  CONST  = /[A-Z]\w*(?:::[A-Z]\w*)*/o
  PATH   = /(?:\/*(?:#{MY_PCHAR})+(?:\/+(?:#{MY_PCHAR})+)*)/o
  MY_NSS = /\A(?:(#{PATH})(?:;(#{CONST})?)?|;(#{CONST}))\z/o

  def check_nss value
    MY_NSS.match? value or raise URI::InvalidComponentError,
      "Invalid urn:x-ruby: NSS: #{value}"
  end

  def check_opaque value
    out = OPAQUE.match(value) or raise URI::InvalidComponentError,
      "Invalid opaque value for URN: #{value}"
    out
  end

  def query_hash_for which
    URI.decode_www_form(which.to_s).reduce({}) do |hash, pair|
      key, value = pair
      key = key.to_sym

      if hash.key? key
        hash[key] = [hash[key]] unless hash[key].is_a? Array
        hash[key] << value
      else
        hash[key] = value
      end

      hash
    end
  end

  public

  attr_reader :r_component, :q_component
  alias_method :query, :q_component

  # Nothing to see here, just `URI.new`.
  #
  # @param arg [Array] arguments from the `URI` constructor
  #
  def initialize *arg
    super

    # not sure what the point of this is
    self.opaque = @opaque if arg[-1]
    # it's missing the fragment though
    self.fragment = arg[8] if arg[8]
  end

  # Retrieve the opaque component which is the NSS plus any R and/or Q
  # components.
  #
  # @return [String] the opaque component.
  #
  def opaque
    out = [nid, nss].join ?:
    out << '?+' + r_component if r_component
    out << '?=' + q_component if q_component
    out
  end

  # Set the opaque component.
  #
  # @param value [String] the new opaque value.
  #
  # @return [String] helpfully, what you just passed in.
  #
  def opaque= value
    nid, nss, r, q   = check_opaque(value).captures
    self.nid         = nid
    self.nss         = nss
    self.r_component = r
    self.q_component = q

    value
  end

  # Retrieve the module path.
  #
  # @return [String] said module path.
  #
  def path
    URI.decode_www_form_component nss.split(?;).first
  end

  # Set the module path.
  #
  # @param value [String] the new module path.
  #
  # @return [String] helpfully, what you just passed in.
  #
  def path= value
    _, rest = nss.split ?;, 2
    self.nss = [URI.encode_www_form_component(value.to_s), rest].join ?;
  end

  # Retrieve the R-component as a hash. Keys get transformed into
  # symbols. Multiple values for a given key will get collated into an
  # array.
  #
  # @return [Hash{Symbol => (String,Array<String>)}] the R-component.
  #
  def r_component_hash
    query_hash_for r_component
  end

  # Ditto Q-component.
  #
  # @return [Hash{Symbol => (String,Array<String>)}] the R-component.
  #
  def q_component_hash
    query_hash_for q_component
  end

  # Set the R-component.
  #
  # @param value [String] the new R-compoenent.
  #
  # @return [String] helpfully, what you just passed in.
  #
  def r_component= value
    return @r_component = nil unless value
    /\A#{RQ}\z/o.match? value or raise URI::InvalidComponentError,
      "Invalid r-component value for URN: #{value}"
    @r_component = value
  end

  # Set the Q-component.
  #
  # @param value [String] the new Q-compoenent.
  #
  # @return [String] helpfully, what you just passed in.
  #
  def q_component= value
    return @q_component = nil unless value
    /\A#{RQ}\z/o.match? value or raise URI::InvalidComponentError,
      "Invalid q-component value for URN: #{value}"
    @q_component = value
  end

  alias_method :query=, :q_component=

  # Serialize the URN to a string.
  #
  # @return [String] said URN.
  #
  def to_s
    out = [scheme, opaque].join ?:
    out << ?# + fragment if fragment
    out
  end

  # it is highly annoying that string coercion only recognizes `to_str`
  alias_method :to_str, :to_s

  # Return the string representation of the constant that will be
  # returned from #object.
  #
  # @return [String] the constant name.
  #
  def constant
    URI.decode_www_form_component nss.split(?;, 2).last
  end

  # Apply `require` to the module path, if present.
  #
  # @note **This performs no security checks.**
  #
  # @raise [LoadError] if loading the module fails.
  #
  # @return [false, true] the result of `require`.
  #
  def require
    # wtf i guess pry overloads Kernel.require
    super path if path and !path.empty?
  end

  # This will #require the #path and then `eval` whatever's in #constant.
  #
  # @raise [LoadError] if loading the module fails.
  # @raise [URI::InvalidComponentError] if the constant is invalid.
  # @raise [NameError] if the constant isn't in the execution context.
  #
  # @return [Object] whatever constant was named in the URN.
  #
  def object
    self.require # this may raise a LoadError
    ref = URI.decode_www_form_component nss.split(?;, 2).last.split(/[?#]/).first
    raise URI::InvalidComponentError, "#{ref} is not a valid constant" unless
      /\A\p{Lu}\p{Word}*(?:::\p{Lu}\p{Word}*)*\z/.match? ref
    # this may raise a NameError
    @constant ||= eval ref
  end
end

module URI::URN
  # even though double-@ variables are outmoded, at least this lets
  # you properly represent NIDs (cf https://github.com/ruby/uri/issues/89)
  @@nids['X-RUBY'] = Intertwingler::RubyURN

  # XXX WTF, GUY??? This replaces URI::URN.new that has an
  # incomprehensibly baroque (and wrong) regex that fails to resolve
  # the `x-ruby` NID, instead of just using `split` like a normal person.
  def self.new *arg
    nid = arg[6].to_s.split(?:).first
    @@nids.fetch(nid.to_s.upcase, Generic).new(*arg)
  end
end
