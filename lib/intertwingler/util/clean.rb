# bring in the namespace
require 'intertwingler/version'

require 'rdf'
require 'uri'

# do this so we don't have to indent all to hell
module Intertwingler::Util; end

module Intertwingler::Util::Clean
  private

  URI_COERCIONS = {
    nil   => -> t { t.to_s.strip },
    false => -> t { t.to_s.strip },
    uri:     -> t { t.is_a?(URI) ? t : URI(t.to_s.strip) },
    rdf:     -> t {
      return t if t.is_a? RDF::Resource
      t = t.to_s.strip
      t.start_with?('_:') ? RDF::Node(t.delete_prefix '_:') : RDF::URI(t)
    },
    term:   -> t {
      # if it's a vocab term check if it's also a vocab, otherwise noop
      if t.is_a? RDF::Vocabulary::Term
        tt = RDF::Vocabulary.find t
        t = tt if tt and tt.to_uri == t.to_uri
        # skull emoji
        t = RDF::RDFV if t == RDF

        return t
      end

      # noop if it's a vocab
      if t.is_a? RDF::Vocabulary
        # not taking any chances
        t = RDF::RDFV if t == RDF
        return t
      end

      # turn to uri or bnode if neither
      unless t.is_a? RDF::Resource
        t = t.to_s.strip
        t = t.start_with?('_:') ? RDF::Node(t.delete_prefix '_:') : RDF::URI(t)
      end

      # try to resolve it to a vocab term
      if t.uri?
        t = (RDF::Vocabulary.find_term(t) rescue t) || t
        tt = RDF::Vocabulary.find t
        t = tt if tt and tt.to_uri == t.to_uri

        # ugh this thing
        t = RDF::RDFV if t == RDF
      end

      t
    },
    vocab:  -> t {
      raise NotImplementedError, 'lol vocab coercion not implemented'
    },
  }

  URI_COERCION_TYPES = {
    nil    => String,
    false  => String,
    uri:   URI,
    rdf:   RDF::URI,
    term:  RDF::Vocabulary::Term,
    vocab: RDF::Vocabulary,
  }

  public

  def assert_uri_coercion coerce
    if coerce
      coerce = coerce.to_s.to_sym if coerce.respond_to? :to_s
      raise ArgumentError, "coerce must be in #{URI_COERCIONS.keys}" unless
        URI_COERCIONS.key?(coerce)
    end
    coerce
  end

  # assertions and coercions

  # Coerce the argument into a resource, either {URI} or {RDF::URI}
  # (or {RDF::Node}). The type can be specified
  #
  # @param arg [#to_s, URI, RDF::URI, RDF::Node] the argument to
  #  coerce into a resource
  # @param as [:rdf, :uri, :term, false, nil] how to coerce the result
  # @yieldparam arg [String] the argument for further processing
  # @yieldreturn [#to_s] the preprocessed argument
  #
  # @return [RDF::URI, URI, RDF::Vocabulary::Term, RDF::Vocabulary, String]
  #
  def coerce_resource arg, as: :rdf, base: nil, &block
    # noop if this is already done
    return arg if as and arg.is_a? URI_COERCION_TYPES[as]

    arg = arg.to_s.strip

    if arg.start_with? '_:' and as
      # override the coercion if this is a blank node
      as = :rdf
    elsif arg.start_with?(?#) and
        uuid = UUID::NCName.from_ncname(arg.delete_prefix(?#), format: :urn)
      return URI_COERCIONS[as].call uuid
    elsif block
      arg = block.call arg
      return if arg.nil?
    end

    URI_COERCIONS[as].call arg
  end

  # Apply #coerce_resource to each element of an array, returning an
  # array of resources. If `arg` can't be turned into an array, it
  # will be wrapped in one. Returns an array of whatever type `as:` is
  # set to return.
  #
  # @param arg [#to_a, #to_s, URI, RDF::URI, RDF::Node] the thing(s)
  #  to coerce
  # @param as [:rdf, :uri, :term, false, nil] how to coerce the
  #  result(s)
  #
  # @return [Array<RDF::URI, URI, RDF::Vocabulary::Term,
  #  RDF::Vocabulary, String>] the coerced elements
  #
  def coerce_resources arg, as: :rdf, &block
    # note nil.to_a is []
    (arg.respond_to?(:to_a) ? arg.to_a : [arg]).map do |c|
      coerce_resource c, as: as, &block
    end.compact
  end

  def assert_term term
    raise ArgumentError, "term must be an RDF::Value" unless
      term.is_a? RDF::Value
    term
  end

  # Assert that the given argument is an {RDF::Resource}.
  #
  # @param term [RDF::Resource] the input being tested
  # @param message [String] an overriding error message (not sure if
  #  dumb)
  # @param blank [true, false] whether to permit blank nodes
  # @param vocab [false, true] whether to try to resolve the term
  #  using {RDF::Vocabulary.find_term}
  #
  # @raise [ArgumentError] if the input is not an {RDF::Resource}
  #
  # @return [RDF::Resource] the argument
  #
  def assert_resource term,
      message = "Term must be a resource, not #{term.inspect}",
      blank: true, vocab: false
    raise ArgumentError, message unless
      term.is_a?(blank ? RDF::Resource : RDF::URI)

    term = (RDF::Vocabulary.find_term(term) rescue term) || term if
      vocab and term.uri? and not term.is_a? RDF::Vocabulary::Term

    term
  end

  # Normalize (and assert) the input as an array of {RDF::Resource}s.
  #
  # @param terms [RDF::Resource, Array<RDF::Resource>] the term(s)
  # @param blank [true, false] whether to allow blank nodes ({RDF::Node})
  # @param empty [true, false] whether the list can be empty
  # @param unique [true, false] whether to remove duplicates
  # @param vocab [false, true] whether to try to resolve the terms
  #  using {RDF::Vocabulary.find_term}
  #
  # @raise [ArgumentError] if the input contains non-{RDF::Resource}s
  #
  # @return [Array<RDF::Resource>] the normalized array of resources
  #
  def assert_resources terms, blank: true, empty: true,
      unique: true, vocab: false
    # normalize to array
    terms = [] if terms.nil?
    terms = terms.respond_to?(:to_a) ? terms.to_a.dup : [terms]

    # ensure everything is a resource (or uri)
    tc = blank ? RDF::Resource : RDF::URI
    if bad = terms.detect { |t| !t.is_a? tc }
      # XXX i know i know but too lazy to do a proper error message
      raise ArgumentError, "Term(s) must be an #{tc}, not #{bad.class}"
    end

    # ditto lol
    raise ArgumentError, "Need at least one term" if terms.empty? and !empty

    # resolve to vocabulary terms
    terms = terms.map do |t|
      t.uri? ? (RDF::Vocabulary.find_term t rescue t) || t : t
    end if vocab

    # prune out any duplicates
    terms.uniq! if unique

    # et voilà
    terms
  end

  # Test that the hash is a struct
  #
  # @param struct [Hash] (we hope)
  #
  # @return [Hash] the struct
  #
  def assert_struct struct
    if struct.is_a? Hash and struct.all? do |pair|
        pair.first.is_a? RDF::URI and
          pair.last.is_a? Array and pair.last.all? { |x| x.is_a? RDF::Value }
      end
      return struct
    end
    raise ArgumentError, "struct is not valid: #{struct.inspect}"
  end

  # Find a subset of a struct for a given set of predicates,
  # optionally inverting to give the objects as keys and predicates as
  # values.
  #
  # @param struct [Hash]
  # @param preds  [RDF::URI, #to_a]
  # @param entail [true, false] whether to entail the predicate(s)
  # @param invert [true, false] whether to invert the resulting hash
  #
  # @return [Hash] the selected subset (which could be empty)
  #
  def find_in_struct struct, preds, entail: false, invert: false
    raise ArgumentError, 'preds must not be nil' if preds.nil?
    preds = preds.respond_to?(:to_a) ? preds.to_a : [preds]
    preds = predicate_set preds if entail

    struct = struct.select { |p, _| preds.include? p }

    invert ? invert_struct(struct) : struct
  end

  # Turns any data structure containing {RDF::Term} objects into a
  # flat set thereof. If `:uri` is true, then any {RDF::Literal}
  # objects are mined for their datatypes and those are returned
  # alongside other {RDF::URI}s instead.
  #
  # @param struct [RDF::Term, #to_a] the struct
  # @param uris [false, true] whether to prune out all but {RDF::URI}
  #  objects
  #
  # @return [Set<RDF::Term, RDF::URII>] The set of terms
  #
  def smush_struct struct, uris: false
    out = Set[]

    if struct.is_a? RDF::Term
      if uris
        case
        when struct.literal?
          out << struct.datatype if struct.datatype?
        when struct.uri? then out << struct
        end
      else
        out << struct
      end
    elsif struct.respond_to? :to_a
      out |= struct.to_a.map do |s|
        smush_struct(s, uris: uris).to_a
      end.flatten.to_set
    end

    out
  end

  # Invert a given struct so that `{ predicate => Set[object] }`
  # becomes `{ object => Set[predicate] }`. Optionally run a block in
  # the inner loop. If the block returns an `Array`, the first two
  # values will be assigned to the predicate and object in the
  # returned inverted struct. Return an explicit `nil` in the block to
  #
  # @param struct [Hash{RDF::Resource => Set}] a structure containing
  #  the predicates and objects for a given subject.
  # @yieldparam predicate [RDF::Resource] the predicate of the statement
  # @yieldparam object    [RDF::Value] the object of the statement
  # @yieldreturn          [nil, Array] an optional predicate-object pair.
  #
  # @return [Hash] the inverted struct.
  #
  def invert_struct struct, &block
    nodes = {}

    struct.each do |p, v|
      v.each do |o|
        # copy the predicate so we don't overwrite it
        pi = p

        if block
          tmp = block.call pi, o
          # assign block return if it has one
          pi, o = *tmp if tmp.is_a? Array
        end

        # now assign to output
        nodes[o] ||= Set.new
        nodes[o] << pi
      end
    end

    nodes
  end

  private

  # anything that could possibly be construed as whitespace
  WS_RE = /[\s\u{0085 00a0 1680 2028 2029 202f 205f 3000}\u2000-\u200a]+/

  public

  def normalize_space string
    string.gsub(WS_RE, ' ').strip
  end

  # Sanitize a term as an {::RDF::Vocabulary}.
  #
  # @param term [#to_s,RDF::URI,URI] the term to sanitize.
  # @param cache [Hash] an optional cache.
  #
  # @return [RDF::Vocabulary]
  #
  def sanitize_vocab vocab, cache: nil
  # def self.sanitize_vocab vocab, cache: nil
    cache = {} unless cache.is_a? Hash
    # 2022-05-18 XXX THIS IS A CLUSTERFUCK
    #
    # what we want is the official vocab if it exists, an
    # on-the-fly vocab if it doesn't, and to use RDF::RDFV
    # instead of RDF if it shows up
    #
    # we notice that bibo:status/ resolves to bibo: with .find
    # so we need to check if the uri is the same before accepting it
    vocab = RDF::URI(vocab) unless vocab.is_a? RDF::URI
    vocab = if cache[vocab.to_s]
              cache[vocab.to_s]
            elsif vocab.is_a?(Class) and
                vocab.ancestors.include?(RDF::Vocabulary)
              vocab # arrrrghhh
            elsif vv = RDF::Vocabulary.find(vocab) # XXX SLOW AF hence cache
              vv.to_uri == vocab ? vv : Class.new(RDF::Vocabulary(vocab))
            else
              Class.new(RDF::Vocabulary(vocab))
            end
    # GRRRR the punning on RDF messes things up so we have to replace it
    vocab = RDF::RDFV if vocab == RDF

    cache[vocab.to_s] = vocab
  end

  # Return a hash mapping a set of RDF prefixes to their vocabularies.
  #
  # @param prefixes [Hash, #to_h, String, #to_s] the input prefixes
  # @param downcase [true, false] whether to normalize key symbolss to downcase
  # @param nonnil [false, true] whether to remove the nil prefix
  # @param cache [Hash] an optional cache for the slowness
  #
  # @return [Hash{Symbol=>RDF::Vocabulary}] sanitized prefix map
  #
  def sanitize_prefixes prefixes, downcase: true, nonnil: false, cache: nil
  # def self.sanitize_prefixes prefixes, downcase: true, nonnil: false, cache: nil
    prefixes = {} unless prefixes         # noop prefixes
    cache    = {} unless cache.is_a? Hash # noop cache

    # turn raw text from a `prefix` attribute into a hash
    if !prefixes.respond_to?(:to_h) && prefixes.respond_to?(:to_s)
      prefixes = prefixes.to_s.strip.split.each_slice(2).map do |k, v|
        [k.split(?:).first, v]
      end.to_h
    end

    raise ArgumentError, 'prefixes must be a hash' unless
      prefixes.is_a? Hash or prefixes.respond_to? :to_h

    prefixes = prefixes.to_h.map do |k, v|
      unless k.nil?
        k = k.to_s.strip
        if k.empty?
          k = nil
        else
          k.downcase! if downcase
          k = k.to_sym
        end
      end
      [k, sanitize_vocab(v, cache: cache)] if (k or !nonnil) and v
    end.compact.to_h

    prefixes
  end

  extend self
end
