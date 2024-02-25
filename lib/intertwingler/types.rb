require 'intertwingler/resolver'

require 'dry-schema'
require 'uri'
require 'pathname'
require 'mimemagic'

require_relative 'rubyurn'

module Intertwingler
  # XXX pop this out into its own module when we're ready
  module Types
    include Dry::Types()

    Dry::Types.define_builder :hash_default do |type|
      reqd = type.keys.select &:required?

      if reqd.empty?
        return type.default({}.freeze) unless type.keys.empty?
      else
        return type.default(reqd.map { |k| [k.name, k.value] }.to_h.freeze) if
          reqd.all?(&:default?)
      end

      type
    end

    # XXX THIS IS A BAD SOLUTION TO THE URI PROBLEM
    Dry::Schema::PredicateInferrer::Compiler.infer_predicate_by_class_name false

    private

    HOSTRE =
      /[0-9a-z](?:[-0-9a-z]*[0-9a-z])?(?:\.[0-9a-z](?:[-0-9a-z]*[0-9a-z])?)*/i
    AUTHRE = /#{HOSTRE}(:\d+)?/o

    UNITS = { nil => 1 }
    'kmgtpe'.split('').each_with_index do |x, i|
      UNITS[x] = 1000 ** (i + 1)
      UNITS[x.upcase] = 1024 ** (i + 1)
    end
    UNITS.freeze

    public

    # @!group Atoms

    # A relative path name.
    RelativePathname = Types.Constructor(::Pathname) { |x| Pathname(x) }

    # A path name that actually exists on the drive.
    ExtantPathname = Types.Constructor(::Pathname) do |x|
      out = Pathname(x).expand_path
      dir = out.dirname
      raise Dry::Types::CoercionError, "#{dir} does not exist" unless
        out.exist? || dir.exist?

      out
    end

    # A path name that is actually writable by the process.
    WritablePathname = Types.Constructor(::Pathname) do |x|
      out = Pathname(x)
      dir = out.expand_path.dirname
      raise Dry::Types::CoercionError, "#{dir} is not writable" unless
        dir.writable?
      raise Dry::Types::CoercionError, "#{out} can't be overwritten" if
        out.exist? and !out.writable?
      out
    end

    # A normalized symbol. Takes its input first to a string, then
    # strips it, lowercases it, and transforms whitespace and hyphens
    # to underscores.
    NormSym = Symbol.constructor do |k|
      k.to_s.strip.downcase.tr_s(' _-', ?_).to_sym
    end

    # A byte count. Use optional suffixes `K`, `M`, `G`, `T`, `P`, `E`
    # to raise to the appropriate power of two. Use lower-case
    # suffixes for powers of ten.
    Bytes = Integer.constructor do |x|
      m = /\A\s*(\d+)([kmgtpeKMGTPE])?\s*\Z/s.match x.to_s
      raise Dry::Types::CoercionError, "#{x} not a viable byte size" unless m

      factor, unit = m.captures
      factor.to_i * UNITS[unit]
    end

    # A hostname is of course a string constrained like `foo.bar.com`.
    Hostname  = String.constrained(format: /^#{HOSTRE}$/o).constructor do |k|
      # XXX you have to do this instead of just say :downcase
      k.downcase
    end

    Port = Coercible::Integer.constrained(gt: 0, lt: 65536)

    # An authority differs from a hostname in that it can have a port
    # number separated by a colon.
    Authority = String.constrained(format: /^#{AUTHRE}$/o).constructor do |k|
      k.downcase
    end

    # A media type, e.g. `text/plain`.
    MediaType = Types::Constructor(MimeMagic).constrained(
      format: /^[^\/]+\/[^\/]+$/)

    # An unprocessed CURIE or IRI, i.e. prior to prefix expansion.
    CURIEOrIRIString = String.constrained format: URI.regexp

    # A "negatable" CURIE/IRI borrows from SPARQL's invert property notation.
    # Coerces `^foo:bar` to a pair of the form `["foo:bar", true]`.
    NegatableCURIEOrIRI = Array.constructor do |x|
      m = /^(\^)?(#{URI.regexp})/o.match(x) or raise Dry::Types::CoercionError,
        "#{x} is not a viable CURIE/IRI path"
      out = m.captures[0,2].reverse
      out[-1] = !!out.last # coerce that to a boolean
      out
    end

    URI = Types.Constructor(::URI) do |x|
      begin
        out = ::URI.parse(x)
      rescue ::URI::InvalidURIError => e
        raise Dry::Types::CoercionError, e
      end

      out
    end

    # A single RDF vocabulary
    Vocab = Types::Constructor(RDF::Vocabulary) do |vocab|
      Intertwingler::Resolver.sanitize_vocab vocab
    end

    RubyURN = URI.constrained format: /\Aurn:x-ruby:/i

    # @!group Molecules

    # A hash where the keys are normalized symbols.
    # @note XXX it is fucking stupid that you have to do this.
    SymbolHash = Hash.constructor do |h|
      h = {} if h.nil?
      raise Dry::Types::CoercionError,
        "#{h.inspect} is a #{h.class}, not a Hash" unless h.is_a? ::Hash
      h.transform_keys { |k| NormSym[k] }
    end

    # RDF vocabularies
    Vocabs = SymbolHash.constructor do |x|
      Intertwingler::Resolver.sanitize_prefixes x, nonnil: true
    end

    # this is the harness configuration

    LibsConfig = SymbolHash.schema path?: Array.of(RelativePathname),
      preload?: Array.of(RubyURN)

    GraphConfig = SymbolHash.schema driver?: RubyURN,
      init?: Array.of(ExtantPathname)

    StaticConfig = SymbolHash.schema target: ExtantPathname

    DomainConfig = SymbolHash.schema graph?: GraphConfig, static?: StaticConfig

    # DevConfig = SymbolHash.schema map?: Hash.map(Authority, Authority)

    HarnessConfig = SymbolHash.schema host?: Hostname, port?: Port,
      libs?: LibsConfig, graph?: GraphConfig,
      authorities?: Hash.map(Hostname, DomainConfig) #,
    # development?: DevConfig

  end
end
