require_relative 'resolver'
require 'rack/request'
require 'strscan'
require 'uri'
require 'set'

# This is a set of classes that represent HTTP header/trailer field
# values of various structures. It serves the dual purpose of input
# sanitation and normalization, and a consistent,
# cryptographically-hashable, serialized representation for given
# input semantics.
#
# This base class also contains a number of primitives for parsing
# field values.
#
# @note These classes only consider RFC9110 and RFC9111 (for
#  `Cache-Control`), as well as RFC8288 for the `Link` header. They do
#  not consider the structured fields in RFC9651.
#
class Intertwingler::Field
  private

  # scan a token
  TOKEN = /[!#$%&'*+\-.^_`|~0-9A-Za-z]+/
  # optional whitespace
  OWS   = /[ \t]*/
  FLOAT = /^[+-]?(?=\.?\d)\d*\.?\d*(?:[Ee][+-]?\d+)?\z/

  def parse! ; end

  public

  # Scan a quoted string.
  #
  # @param ss [StringScanner] the string scanner instance
  #
  # @raise [ArgumentError] the grammar is malformed.
  #
  # @return [String] the erstwhile-quoted string.
  #
  def self.scan_quoted ss
    ss.scan(/"/) or return
    out = String.new
    until ss.eos?
      if (chunk = ss.scan(/[\t \x21\x23-\x5b\x5d-\x7e\x80-\xff]+/n))
        out << chunk
      elsif ss.scan(/\\/)
        ch = ss.scan(/[\t \x21-\x7e\x80-\xff]/n) or
          raise ArgumentError, "invalid escape at #{ss.pos}"
        out << ch
      elsif ss.scan(/"/)
        return out
      else
        peek = ss.peek(10).inspect
        raise ArgumentError,
          "invalid character in quoted string at #{ss.pos}: #{peek}"
      end
    end
    raise ArgumentError, 'unterminated quoted string'
  end

  # Scan an individual parameter value.
  #
  # @param ss [StringScanner] the string scanner instance
  #
  # @raise [ArgumentError] the value is malformed.
  #
  # @return [String] the erstwhile-quoted string.
  #
  def self.scan_value ss
    value = ss.scan TOKEN
    return value.downcase.to_sym if value

    scan_quoted ss
  end

  # Scan an individual parameter pair.
  #
  # @param ss [StringScanner] the string scanner instance
  #
  # @return [Array(Symbol, Object)] the parameter name and its value
  #
  def self.scan_param ss
    name = ss.scan(TOKEN) or return

    ss.skip OWS

    ss.scan(/=/) or
      raise ArgumentError, "expected '=' after parameter name #{name.inspect}"

    ss.skip OWS
    value = scan_value(ss) or
      raise ArgumentError, "expected value for parameter #{name.inspect}"

    name  = name.downcase.to_sym
    value = value.to_s.to_f if value.is_a?(Symbol) && FLOAT.match?(value.to_s)

    [name, value]
  end

  # Scan the full parameter set.
  #
  # @param ss [StringScanner] the string scanner instance
  #
  # @return [Hash{Symbol=>Object}] the parameter set
  #
  def self.scan_params ss
    ss.skip OWS

    params = {}

    while ss.skip(/[ \t]*;[ \t]*/) && !ss.eos?
      # peek ahead — trailing semicolon with no parameter is tolerated
      break unless ss.check TOKEN

      name, val = scan_param ss
      params[name] = val
    end

    ss.skip OWS

    params
  end

  # Scan a member of the list
  #
  # @param ss [StringScanner] the string scanner instance
  #
  # @return [Array(Symbol,Hash)] the
  #
  def self.scan_member ss
    ss.skip OWS

    value  = scan_value(ss) or return
    params = scan_params ss

    [value, params]
  end

  # Parse a ranked header value (e.g. `Accept`)
  #
  # @param value [String] the header value
  #
  # @return [Array]
  #
  def self.parse value
    ss = StringScanner.new value
    members = []
    loop do
      ss.skip OWS
      break if ss.eos?

      member = scan_member ss
      members << member if member

      ss.skip OWS
      break unless ss.scan /,/
    end
    members
  end

  # Instantiate a parsed HTTP field.
  #
  # @param value [String, #to_s] the initial field value
  # @param uri [URI, RDF::URI, String, nil] the request-URI, if
  #  applicable
  # @param message [Rack::Request, Rack::Response] the entire
  #  associated HTTP message, if applicable
  #
  # @return [void]
  #
  def initialize value, uri: nil, message: nil
    @original = value.to_s.dup.freeze # keep the original string
    @message  = message               # associated http message
    @value    = nil                   # make sure something is there

    # we get a base URI either directly or from a request message
    @uri = if uri
             Intertwingler::Resolver.coerce_resource uri, as: :uri
           elsif message.is_a? ::Rack::Request
             URI(message.url.to_s)
           end

    # then we parse
    parse!
  end

  attr_reader :original, :uri, :message, :value

  # Serialize the field to a normalized string.
  #
  def to_s
    raise NotImplementedError, 'Each subclass must implement its own `to_s`.'
  end

  # Return a consistent human-readable representation for debugging.
  #
  def inspect
    "<#{self.class} \"#{to_s}\">"
  end

  # Leaves header value untouched, except for `#strip` around the
  # entire string.
  #
  class Verbatim < self
    private

    def parse!
      @value = @original.strip
    end

    public

    # Applies `#strip` to the original string.
    #
    def to_s
      @value
    end
  end

  # A handful of headers (like `Content-Length`) are just numbers.
  #
  class NonNegativeInteger < self

    private

    def parse!
      raise ArgumentError, "#{@original} is not numeric" unless
        /^\s*\d+\s*$/ =~ @original

      @value = @original.strip.to_i
    end

    public

    def to_s
      @value.to_s
    end

    def inspect
      "<#{self.class} (#{to_s})>"
    end
  end

  # Another handful of headers (`Location`, `Content-Location`) are URIs.
  #
  class URI < self
    private

    def parse!
      begin
        @value = (@uri ? @uri + value : ::URI.new(value)).normalize
      rescue ::URI::Error => e
        # re-raise with a different message
        raise ArgumentError, e.message
      end
    end

    public

    # Returns a URI relative to the base if initialized with one,
    # otherwise absolute.
    #
    def to_s
      (@uri ? @uri.route_to(@value) : @value).to_s
    end
  end

  # A media type is (unfortunately) not a token.
  #
  class MediaType < self
    private

    def parse!
      @value = self.class.scan_element StringScanner.new(@original)
    end

    public

    # Scan an elementary header value.
    #
    # @param scanner [StringScanner] the string scanner instance
    #
    # @return [Array(String, Hash)] the parsed type
    #
    def self.scan_element scanner
      type = scanner.scan /#{TOKEN}\/#{TOKEN}/
      scanner.skip OWS
      params = scan_params scanner

      [type.downcase, params]
    end

    # Serialize a normalized media type.
    #
    # @param type [String] the media type
    # @param params [Hash{Symbol=>Object}, nil] its parameters
    #
    def self.serialize type, params
      params ||= {}

      out = [type] + params.slice(*params.keys.sort).map do |k, v|
        v = case v
            when Float then (v == v.truncate ? v.to_i : v).to_s
            when String then '"%s"' % v.gsub(/[\x22\x5c]/, '\\&')
            else v.to_s
            end

        "#{k}=#{v}"
      end

      out.join ?;
    end

    # Serialize the media type stored as the value.
    #
    # @return [String]
    #
    def to_s
      self.class.serialize(*@value)
    end
  end

  # Field value gets split into an array on commas, modulo quoted
  # strings. Tokens get normalized to lower case, and that's about it.
  #
  class List < self
    private

    def parse!
      @value = @original.strip.split /\s*,+\s*/
    end

    public

    def to_s
      @value.join ', '
    end
  end

  # Field value elements are de-duplicated and sorted lexically on top
  # of being normalized.
  #
  class Set < List
    private

    def parse!
      super
      @value = @value.to_set
    end

    public

    def to_s
      @value.sort.join ', '
    end
  end

  # Field values consist of a `key` with optional `=value` pairs
  # (which may be a quoted string), such as `Cache-Control`.
  #
  class Pairs < List
  end

  class ParamPairs < Pairs
  end

  # Weighted header values like `Accept` have a `q` parameter from 0 to
  # 1 (which when omitted implies 1) among other possible parameters.
  #
  class Weighted < Set

    private

    # Normalize a parsed header
    #
    # @param values [Array<Array(Symbol,Hash{Symbol=>Object})>]
    # @param strip [false, true]
    # @param cmp [Proc] an optional comparator function
    #
    # @yieldparam a [Array(Symbol,Hash{Symbol=>Object})] comparand A
    # @yieldparam b [Array(Symbol,Hash{Symbol=>Object})] comparand B
    #
    # @yieldreturn [Integer] preferably -1, 0, or 1
    #
    # @return [Array<Array(Symbol,Hash{Symbol=>Object})>]
    #
    def self.normalize values, strip: false, &cmp
      cmp ||= -> a, b do
      end
    end
    def parse!
    end
  end

  # The `Accept` header is weighted and with parameters, and has a special
  #
  class Accept < Weighted
    def parse!
    end

    def to_s
    end
  end

  # Special case for the `Link` header, which strictly speaking is a
  # _set_, with elements between angle brackets `<>` and parameters
  # that can be quoted strings in both keys _and_ values.
  #
  class Link < Set
  end

  # `Authorization` headers also have special contents.
  #
  class Credentials < self
  end

  private

  # The `Vary` response header is used in content negotiation to
  # indicate which *request* headers affect the representation. Such a
  # dir can indicate *any* combination of (request) headers, and so a
  # cache ought to dutifully incorporate those headers and their values
  # into its lookup key. The problem is that many HTTP field values can
  # vary *lexically* enormously while all saying exactly the same
  # thing. To produce an effective (cryptographic) cache key, then, we
  # have to parse, sanitize, normalize, and ultimately reserialize the
  # header values.  This has several implications:
  #
  # * The request-handling processes actually *use* the data structures
  #   that result from parsing the header values, and so there is value
  #   in having them already parsed, sanitized, and ready to use.
  # * While parsing message headers (both from requests and from
  #   upstream responses), we are likely to encounter erroneous values,
  #   which will either be a mistake, garbage, or a deliberate
  #   attack. As such, there is value in handling these scenarios
  #   appropriately.
  #
  # HTTP field values cluster into a number of types — far fewer than
  # the total number of fields (255 as of this writing), but many more
  # than just a handful. Many of the types are highly structured.
  # `Protocol semantics dictate that fields (both headers and trailers)
  # with cardinalities greater than 1 are equivalent to concatenating
  # multiple single-cardinality fields together with commas (strictly
  # `/\s*,\s*/`).
  #
  # (Note: strictly speaking it is a protocol error for single-
  # cardinality fields to contain more than one value — or otherwise for
  # there to be more than one instance of a given single-cardinality
  # field. Such errors are nevertheless easily recoverable, by
  # instituting a policy to pick either the first conforming value — or
  # the last one — and discarding the others. There are good arguments
  # for either option.)
  #
  # There are therefore a number of primitives:
  #
  # * Non-negative integer (0..)
  # * token
  # * token=[value] (mandatory, most of the time)
  # * token[=value] (optional, as in `Cache-Control`)
  # * opaque string with parameters (as in `Content-Type`, `Accept`)
  # * token[=value ; parameters] (as in `Expect`)
  # * rfc5322 date (commas are part of the grammar)
  # * URI (same)
  # * product (as in `Server`, `User-Agent`)
  # * credentials (as in numerous auth headers)
  # * `Link` header is its own beast
  # * so is (`Set-`)`Cookie`
  # * (there are more; TBD)
  #
  # Some fields with N≥1 cardinality (such as `Content-Encoding`)
  # represent actual sequences of values, while most represent sets.
  # Those with parameters can be represented as a Hash with the token as
  # the key and the parameters as the value. Some of the parametrized
  # values have a *weight* which can be used as a lexical sorting
  # criterion (`Accept` is actually the only weighted set where the
  # elements have true parameters; for the rest the weight is the only
  # valid parameter.)

  HEADERS = {
    'accept'                    => Accept,
    'accept-charset'            => Weighted,
    'accept-encoding'           => Weighted,
    'accept-language'           => Weighted,
    'accept-ranges'             => Set,
    'age'                       => NonNegativeInteger, # 9111
    'allow'                     => Set,
    'authentication-info'       => Pairs,
    'authorization'             => Credentials,
    'cache-control'             => Pairs, # 9111
    'connection-info'           => Set,
    'content-encoding'          => List,
    'content-language'          => Set,
    'content-length'            => NonNegativeInteger,
    'content-location'          => URI,
    'content-range'             => Verbatim, # special
    'content-type'              => MediaType,
    'date'                      => Date,
    'etag'                      => Verbatim, # special
    'expect'                    => ParamPairs,
    'expires'                   => Date, # 9111
    'from'                      => Verbatim, # special (email)
    'host'                      => Verbatim, # special (authority)
    'if-match'                  => Set,
    'if-modified-since'         => Date,
    'if-none-match'             => Set,
    'if-range'                  => Verbatim, # special
    'if-unmodified-since'       => Date,
    'last-modified'             => Date,
    'location'                  => URI,
    'max-forwards'              => NonNegativeInteger,
    'pragma'                    => ParamPairs, # 9111; deprecated
    'proxy-authenticate'        => Credentials, # a challenge is the same
    'proxy-authentication-info' => Pairs,
    'proxy-authorization'       => Credentials,
    'range'                     => Verbatim, # this is special too
    'referer'                   => URI,
    'retry-after'               => Verbatim, # special
    'server'                    => Verbatim, # "products" actually
    'te'                        => Weighted,
    'trailer'                   => Set, # List?
    'upgrade'                   => Set, # List?
    'user-agent'                => Verbatim, # "products" also
    'vary'                      => Set,
    'via'                       => Verbatim, # special
    'www-authenticate'          => Credentials, # "challenge" is the same
  }

  HDR_RE = %r{^(CONTENT_(?:TYPE|LENGTH))|
    HTTP_([0-9A-Z_]+)|([A-Za-z](?:-?[0-9A-Za-z]+)*)\Z}x

  public

  # Select the appropriate header class
  #
  def self.[] header
    # normalize the header
    m = HDR_RE.match(header.to_s) or
      raise ArgumentError, "header #{header} not found"

    # normalized header
    nh = m.captures.detect { |x| !x.nil? }.downcase.tr(?_, ?-)

    # default to verbatim
    HEADERS.fetch nh, Verbatim
  end
end
