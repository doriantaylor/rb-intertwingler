require 'rdf/sak/resolver'
require 'rdf/sak/mimemagic'

require 'dry-schema'
require 'uri'
require 'pathname'

module RDF::SAK
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
      /[0-9a-z](?:[-0-9a-z]*[0-9a-z])(?:\.[0-9a-z](?:[-0-9a-z]*[0-9a-z])?)*/i
    AUTHRE = /#{HOSTRE}(:\d+)?/o

    UNITS = { nil => 1 }
    'kmgtpe'.split('').each_with_index do |x, i|
      j = i + 1
      UNITS[x] = 1000 ** j
      UNITS[x.upcase] = 1024 ** j
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

    # An authority differs from a hostname in that it can have a port
    # number separated by a colon.
    Authority = String.constrained(format: /^#{AUTHRE}$/o).constructor do |k|
      k.downcase
    end

    # A media type, e.g. `text/plain`.
    MediaType = Types::Constructor(RDF::SAK::MimeMagic).constrained(
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

    # A single RDF vocabulary
    Vocab = Types::Constructor(RDF::Vocabulary) do |vocab|
      RDF::SAK::Resolver.sanitize_vocab vocab
    end

    # @!group Molecules

    # A hash where the keys are normalized symbols.
    # @note XXX it is fucking stupid that you have to do this.
    SymbolHash = Hash.constructor do |h|
      h.transform_keys { |k| NormSym[k] }
    end

    # A plugin has two mandatory keys: `driver` and `options`.
    Plugin = SymbolHash.schema(driver: String, options: SymbolHash)

    # A sequence of plugins
    PluginSeq = Array.of Plugin

    # A map of plugins
    NamedPlugins = Hash.map(NormSym, Plugin)

    # RDF vocabularies
    Vocabs = SymbolHash.constructor do |x|
      RDF::SAK::Resolver.sanitize_prefixes x, nonnil: true
    end

    # A sequence of RDF types that are always directly addressable via HTTP
    Documents = Array.of CURIEOrIRIString

    # A mapping of RDF types that are canonically fragments if
    # connected to a directly-addressable resource via a predicate
    Fragments = Hash.map(CURIEOrIRIString, Array.of(NegatableCURIEOrIRI))

    # A presentation transform
    Transform = SymbolHash.schema(name: String, params?: SymbolHash)

    # Multiple presentation transforms
    Transforms = Hash.map(MediaType, Array.of(Transform))

    # A site-specific record
    Site = SymbolHash.schema(
      aliases?:    Array.of(Authority),
      home?:       ExtantPathname,
      graph?:      Plugin,
      sources?:    PluginSeq,
      surfaces?:   NamedPlugins,
      vocab?:      Vocab,
      prefixes?:   Vocabs,
      documents?:  Documents,
      fragments?:  Fragments,
      transforms?: Transforms,
    )

    # Multiple sites
    Sites = Hash.map(Hostname, Site)
  end

  # Config
  Config = Types::Site.schema(sites: Types::Sites)
end
