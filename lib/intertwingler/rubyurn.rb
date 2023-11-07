require 'intertwingler/version' # for the
require 'uri/urn'

class Intertwingler::RubyURN < URI::URN::Generic

  private

  # these are just character classes so we can merge them into actual regexes
  UNRESERVED = %[-0-9A-Za-z._~].freeze
  SUB_DELIMS = %[!$&'()*+,;=].freeze
  PCHAR_STR  = %[[#{UNRESERVED}#{SUB_DELIMS}:@]|%[0-9A-Fa-f]{2}].freeze

  # like this one
  PCHAR  = /(?:#{PCHAR_STR})/o
  NID    = /[0-9A-Za-z][0-9A-Za-z-]{,30}[0-9A-Za-z]/o
  NSS    = /(?:#{PCHAR_STR})(?:#{PCHAR_STR}|\/)*/o
  RQ     = /(?:#{PCHAR_STR})(?:#{PCHAR_STR}|[\/\?])*/o
  FRAG   = /(?:#{PCHAR_STR}|[\/\?])*/o
  OPAQUE = /\A(#{NID}):(#{NSS})(?:\?\+(#{RQ})?)?(?:\?\=(#{RQ})?)?\z/o
  CONST  = /[A-Z]\w*(?:::[A-Z]\w*)*/o
  MY_NSS = /\A([#{UNRESERVED}!$&'()*+,=\/]+)?;(#{CONST})\z/o

  def check_nss value
    MY_NSS.match? value or raise URI::InvalidURIComponentError,
      "Invalid URN NSS: #{value}"
  end

  def check_opaque value
    out = OPAQUE.match(value) or raise URI::InvalidURIComponentError,
      "Invalid opaque value for URN: #{value}"
    out
  end

  def query_params which
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

  def r_component_hash
    query_params r_component
  end

  def q_component_hash
    query_params q_component
  end

  def initialize *arg
    super

    # not sure what the point of this is
    self.opaque = @opaque if arg[-1]
    # it's missing the fragment though
    self.fragment = arg[8] if arg[8]
  end

  def r_component= value
    return @r_component = nil unless value
    /\A#{RQ}\z/o.match? value or raise URI::InvalidURIComponentError,
      "Invalid r-component value for URN: #{value}"
    @r_component = value
  end

  def q_component= value
    return @q_component = nil unless value
    /\A#{RQ}\z/o.match? value or raise URI::InvalidURIComponentError,
      "Invalid r-component value for URN: #{value}"
    @q_component = value
  end

  alias_method :query=, :q_component=

  def opaque
    out = [nid, nss].join ?:
    out << '?+' + r_component if r_component
    out << '?=' + q_component if q_component
    out
  end

  def opaque= value
    nid, nss, r, q   = check_opaque(value).captures
    self.nid         = nid
    self.nss         = nss
    self.r_component = r
    self.q_component = q

    value
  end

  def path
    URI.decode_www_form_component nss.split(?;).first
  end

  def path= value
    _, rest = nss.split ?;, 2
    self.nss = [URI.encode_www_form_component(value), rest].join ?;
  end

  def to_s
    out = [scheme, opaque].join ?:
    out << ?# + fragment if fragment
    out
  end

  def require
    super path if path and !path.empty?
  end

  def constant
    nss.split(?;, 2).last
  end

  def object
    self.require # this may raise 
    ref = URI.decode_www_form_component nss.split(?;, 2).last.split(/[?#]/).first
    raise URI::Error, "#{ref} is not a valid constant" unless
      /^\p{Lu}\p{Word}*(?::\p{Lu}\p{Word}*)*$/.match? ref
    @constant ||= eval ref
  end
end

module URI::URN
  @@nids['X-RUBY'] = Intertwingler::RubyURN

  # XXX WTF, GUY???
  def self.new *arg
    nid = arg[6].to_s.split(?:).first
    @@nids[nid.to_s.upcase].new(*arg)
  end
end
