# bring in the namespace
require 'rdf/sak/version'

# do this so we don't have to indent all to hell
module RDF::SAK::Util
end

module RDF::SAK::Util::Clean
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
      return t if t.is_a? RDF::Vocabulary::Term
      unless t.is_a? RDF::Resource
        t = t.to_s.strip
        t = t.start_with?('_:') ? RDF::Node(t.delete_prefix '_:') : RDF::URI(t)
      end

      t = (RDF::Vocabulary.find_term(t) rescue t) || t if t.uri?
      t
    },
  }

  public

  # assertions and coercions

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
    terms = terms.respond_to?(:to_a) ? terms.to_a : [terms]

    # ensure everything is a resource (or uri)
    tc = blank ? RDF::Resource : RDF::URI
    if bad = terms.detect { |t| !t.is_a? tc }
      # XXX i know i know but too lazy to do a proper error message
      raise ArgumentError, "Term(s) must be an #{tc}, not #{bad.class}"
    end

    # ditto lol
    raise ArgumentError, "Need at least one term" if terms.empty? and !empty

    # resolve to vocabulary terms
    terms.map! do |t|
      t.uri? ? (RDF::Vocabulary.find_term t rescue t) || t : t
    end if vocab

    # prune out any duplicates
    terms.uniq! if unique

    # et voilà
    terms
  end

  def assert_struct struct
  end

  def coerce_resource arg, base = nil, as: :rdf
  end

  # turns any data structure into a set of nodes
  def smush_struct struct, uris: false
    out = Set.new

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

  def invert_struct struct
    nodes = {}

    struct.each do |p, v|
      v.each do |o|
        nodes[o] ||= Set.new
        nodes[o] << p
      end
    end

    nodes
  end

  extend self
end
