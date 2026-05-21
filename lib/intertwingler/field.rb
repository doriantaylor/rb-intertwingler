require_relative 'resolver'
require 'rack/request'
require 'set'
require 'strscan'
require 'time'
require 'uri'

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
  # We introduce an error class that doesn't do much besides
  # disambiguate from a generic {ArgumentError}.
  #
  class ParseError < ArgumentError
  end

  private

  # scan a token
  TOKEN = /[!#$%&'*+\-.^_`|~0-9A-Za-z]+/
  # optional whitespace
  OWS   = /[ \t]*/
  FLOAT = /^[+-]?(?=\.?\d)\d*\.?\d*(?:[Ee][+-]?\d+)?\z/

  def parse! ; end

  public

  # Coerce the (potentially unknown) field name to CGI/{Rack::Request}
  # format.
  #
  # @note Rack 3.3 will FINALLY have consistent field names in
  #  requests/responses.
  #
  # @param name [String] the field name
  #
  # @return [String] the converted name
  #
  def self.to_req name
  end

  # Coerce the (potentially unknown) field name to the standard HTTP/2
  # (lower-case) format used in {Rack::Response}.
  #
  # @param name [String] the field name
  #
  # @return [String] the converted name
  #
  def self.to_resp name
  end

  # Scan a quoted string.
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @raise [ArgumentError] the grammar is malformed.
  #
  # @return [String] the erstwhile-quoted string.
  #
  def self.scan_quoted scanner
    scanner.scan(/"/) or return
    out = String.new
    until scanner.eos?
      if (chunk = scanner.scan(/[\t \x21\x23-\x5b\x5d-\x7e\x80-\xff]+/n))
        out << chunk
      elsif scanner.scan(/\\/)
        ch = scanner.scan(/[\t \x21-\x7e\x80-\xff]/n) or
          raise ParseError, "invalid escape at #{scanner.pos}"
        out << ch
      elsif scanner.scan(/"/)
        return out
      else
        peek = scanner.peek(10).inspect
        raise ParseError,
          "invalid character in quoted string at #{scanner.pos}: #{peek}"
      end
    end
    raise ParseError, 'unterminated quoted string'
  end

  # Scan an individual parameter value.
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @raise [ParseError] the value is malformed.
  #
  # @return [String] the erstwhile-quoted string.
  #
  def self.scan_value scanner
    value = scanner.scan TOKEN
    return value.downcase.to_sym if value

    scan_quoted scanner
  end

  # Scan an individual parameter pair.
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @return [Array(Symbol, Object)] the parameter name and its value
  #
  def self.scan_param scanner
    name = scanner.scan(TOKEN) or return

    scanner.skip OWS

    scanner.scan(/=/) or
      raise ParseError, "expected '=' after parameter name #{name.inspect}"

    scanner.skip OWS
    value = scan_value(scanner) or
      raise ParseError, "expected value for parameter #{name.inspect}"

    name  = name.downcase.to_sym
    value = value.to_s.to_f if value.is_a?(Symbol) && FLOAT.match?(value.to_s)

    [name, value]
  end

  # Scan the full parameter set.
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @return [Hash{Symbol=>Object}] the parameter set
  #
  def self.scan_params scanner
    scanner.skip OWS

    params = {}

    while scanner.skip(/[ \t]*;[ \t]*/) && !scanner.eos?
      # peek ahead — trailing semicolon with no parameter is tolerated
      break unless scanner.check TOKEN

      name, val = scan_param scanner
      params[name] = val
    end

    scanner.skip OWS

    params
  end

  # Scan for a (parametrized) header element
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @return [Array(Symbol,Hash)] the
  #
  def self.scan_element scanner
    scanner.skip OWS

    value  = scan_value(scanner) or return
    params = scan_params scanner

    [value, params]
  end

  # Scan all elements in a list
  #
  # @param scanner [StringScanner] the string scanner instance
  #
  # @return [Array] the elements
  #
  def self.scan_elements scanner
    elements = []

    loop do
      scanner.skip OWS
      break if scanner.eos?

      elem = scan_element scanner
      elements << elem if elem

      scanner.skip OWS
      break unless scanner.scan /,/
    end

    elements
  end

  # Parse a ranked header value (e.g. `Accept`)
  #
  # @param value [String] the header value
  #
  # @return [Array]
  #
  def self.parse value
    ss = StringScanner.new value
    scan_elements ss
  end

  # Return a quoted string with escapes.
  #
  # @param string [String,#to_s] the input string
  #
  # @return [String] a string in double quotes with any internal
  #  double quotes (and backslashes) escaped.
  #
  def self.quote_string string
    '"%s"' % string.to_s.gsub(/[\x22\x5c]/, '\\&')
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
           elsif message.is_a? ::Hash
             @message = Rack::Request.new message
             URI(@message.url.to_s)
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
      @value.freeze
    end
  end

  # A handful of headers (like `Content-Length`) are just numbers.
  #
  class NonNegativeInteger < self

    private

    def parse!
      raise ParseError, "#{@original} is not a recognizable integer" unless
        /^\s*\d+\s*$/ =~ @original

      @value = @original.strip.to_i
    end

    public

    # This just takes the parsed value instead of the original.
    #
    def to_s
      @value.to_s.freeze
    end

    # Overriding `#inspect` to get rid of the quotation marks.
    #
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
        orig = @original.strip
        @value = (@uri ? @uri + orig : ::URI.new(orig)).normalize
      rescue ::URI::Error => e
        # re-raise with a different message
        raise ParseError, e.message
      end
    end

    public

    # Returns a URI relative to the base if initialized with one,
    # otherwise absolute.
    #
    def to_s
      (@uri ? @uri.route_to(@value) : @value).to_s
    end

    # Overriding `#inspect` to get rid of the quotation marks.
    #
    def inspect
      "<#{self.class} #{to_s}>"
    end
  end

  # XXX add http date parsing
  #
  class Date < Verbatim
    def parse!
      @value = Time.httpdate @original
    end

    def to_s
      @value.httpdate
    end

    def inspect
      "<#{self.class} (#{@value.iso8601})>"
    end
  end

  # A media type is (unfortunately) not a token.
  #
  class MediaType < self
    private

    def parse!
      elem = self.class.scan_element StringScanner.new(@original)
      elem.last.delete :q # there should not be a q key
      @value = elem
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

      # we include q here for Accept
      keys = params.keys.sort - [:q]
      keys << :q if params.key? :q

      out = [type] + params.slice(*keys).map do |k, v|
        v = case v
            when Float then (v == v.truncate ? v.to_i : v).to_s
            when String then quote_string v
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
      @value = self.class.scan_elements(StringScanner.new @original).compact
    end

    public

    def self.scan_element scanner
      elem = scanner.scan(TOKEN) or return
      elem.downcase.to_sym
    end

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
    def parse!
      ss = StringScanner.new @original
      @value = self.class.scan_elements(ss).to_h
    end

    # We get a special `scan_element` here
    #
    def self.scan_element scanner
      name = scanner.scan(TOKEN) or return
      scanner.skip OWS

      value = if scanner.scan(/=/)
        scanner.skip OWS

        scan_value scanner
      end

      name  = name.downcase.to_sym
      value = value.to_s.to_i if /^\d+$/.match? value.to_s

      [name, value]
    end

    def to_s
      @value.map { |k, v| v ? "#{k}=#{v}" : k.to_s }.sort.join ', '
    end
  end

  class ParamPairs < Pairs
    def self.scan_element scanner
      # it's weird that this is the default; we should probably move it.
      Intertwingler::Field.scan_element scanner
    end
  end

  # Weighted header values like `Accept` have a `q` parameter from 0 to
  # 1 (which when omitted implies 1) among other possible parameters.
  #
  class Weighted < ParamPairs

    private

    def parse!
      ss = StringScanner.new @original
      @value = self.class.scan_elements(ss).each_with_object({}) do |pair, h|
        q = (pair.last[:q] || 1.0).to_f.clamp(0.0, 1.0).round 3
        h[pair.first] = q
      end
    end

    public

    def to_s
      keys = @value.keys.sort do |a, b|
        c = @value[b] <=> @value[a]
        c == 0 ? a <=> b : c
      end

      keys.map do |k|
        @value[k] == 1.0 ? k.to_s : "#{k};q=#{@value[k]}"
      end.join ', '
    end
  end

  # The `Accept` header is weighted and the elements also have
  # parameters. We permit the `q` values to be mixed in with those
  # parameters on parse but we serialize with them always at the end.
  #
  class Accept < Weighted
    private

    # we override here to preserve
    def parse!
      ss = StringScanner.new @original
      @value = self.class.scan_elements(ss).each_with_object({}) do |pair, h|
        q = (pair.last.delete(:q) || 1.0).to_f.clamp(0.0, 1.0).round 3
        (h[pair.first] ||= []) << [pair.last, q]
      end
    end

    public

    # This just wraps {MediaType.scan_element}.
    #
    def self.scan_element scanner
      # c wut we did thar???
      MediaType.scan_element scanner
    end

    def to_s
      @value.map { |k, v| [k].product v }.flatten(1).sort do |a, b|
        c = b.last.last <=> a.last.last # weight
        c = c == 0 ? a <=> b : c # type identifier
        c == 0 ? a.last.first.to_s <=> b.last.first.to_s : c # params
      end.map do |pair|
        type, pair     = pair
        params, weight = pair
        params = params.merge({ q: weight }) if weight != 1.0
        MediaType.serialize type, params
      end.join ', '
    end
  end

  # Special case for the `Link` header, which strictly speaking is a
  # _set_, with elements between angle brackets `<>` and parameters
  # that can be quoted strings in both keys _and_ values.
  #
  class Link < Set
    # def self.scan_element scanner
    #   # < link > followed by params
    # end
  end

  # `Authorization` headers also have special contents.
  #
  class Credentials < Verbatim
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

  FIELDS = {
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
    'pragma'                    => Pairs, # 9111; deprecated
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

  REQ_CTCL = /(CONTENT_(?:TYPE|LENGTH))/
  REQ_HDR  = /^#{REQ_CTCL}|HTTP_([0-9A-Z_]+)\Z/o
  RESP_HDR = /^([A-Za-z](?:-?[0-9A-Za-z]+)*)\Z/o
  HDR_RE   = %r{(?:#{REQ_HDR}|#{RESP_HDR})}o

  def self.dispatch name
    @fields ||= {}

    # normalize the field name
    name = to_http name

    @fields[name] ||= Class.new(FIELDS.fetch name, Verbatim) do
      define_singleton_method(:field_name) { name.freeze }
    end
  end

  public

  # Select the appropriate field class for the given name.
  #
  # @param arg [String, ]
  #
  def self.[] *args
    # on-the-fly fields
    return from(*args[0,2]) if respond_to?(:field_name)
    dispatch args.first
  end

  # Coerce a 
  #
  def self.from message, uri = nil
    val = case message
          when String then return new(message)
          when Rack::Request
            uri ||= message.url
            message.get_header(to_req_env field_name)
          when Hash
            # we want rack\..* or FOO_BAR
            if message.keys.any? { |k| /^(rack\..+|[A-Z]+(?:_[0-9A-Z]+)+)$/ }
              message = Rack::Request.new message
              uri ||= message.url
              message.get_header(to_req_env field_name)
            else
              message[to_http field_name]
            end
          when Rack::Response then message.get_header(to_http field_name)
          else
            raise ArgumentError,
              "cannot handle message of type #{message.class}"
          end

    if [Rack::Request, Rack::Response].any? { |c| message.is_a? c }
      mobj = message
    end

    new(val, message: mobj, uri: uri) if val
  end

  def self.to_req_env name
    name = name.to_s.strip.upcase.tr ?-, ?_
    raise ParseError, "Field name must not be empty" if name.empty?
    /^#{REQ_CTCL}\Z/o.match?(name) ? name : "HTTP_#{name}"
  end

  def self.to_http name
    raise ParseError, "invalid field name #{name}" unless
      m = HDR_RE.match(name.to_s.strip)
    m.captures.detect { |x| !x.nil? }.downcase.tr(?_, ?-)
  end

  def self.inspect
    return "#{ancestors[1]}(#{field_name})" if respond_to? :field_name
    super
  end

  # Return a string representation of the class
  def self.to_s
    inspect
  end

  def field_name
    self.class.field_name
  end

end
