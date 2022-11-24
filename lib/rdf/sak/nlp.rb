# -*- coding: utf-8 -*-
require 'rdf/sak/version' # initialize the symbols

require 'lemmatizer'
require 'engtagger'

private

# XXX why does this feel super familiar
POS_MAP = {
  noun: %i[xnn nnp nnps nns],
  verb: %i[vb vbd vbg vbn vbp vbz],
  adj:  %i[jj jjr jjs],
  adv:  %i[rb rbr rbs rp],
}.reduce({}) do |hash, pair|
  target = pair.first
  pair.last.each { |v| hash[v] = pair.first }
  hash
end.freeze

public

# This is the *extremely* lightweight NLP functionality. Goals:
#
# * Identify N-grams in the corpus that can be candidates for terms
#   (concepts etc) and/or labels (of other entities).
#
# * Generate the raw material for `ci:mentions` relations from
#   enclosing text segments (documents, fragments thereof) to entities
#   (concepts, people, places, things).
#
# Since we want arbitrarily long N-grams, and we want to remember
# fairly accurately where they came from, we want to take a document
# to an array (of arrays) of segments, or rather a hash of arrays of
# segments, keyed by fragment ID. (This implicitly should happen after
# IDs have been assigned to fragments through some other process.)
# Conceivably we can identify document fragments down to the paragraph
# but in practice the innermost sections will probably do. (Figures,
# tables, blockquotes and asides should also be identified.)
#
# Assuming we are beginning with markup that distinguishes between
# block and inline elements, we drill down to the bottom-most blocks
# and then we recursively process the inlines. These can nest
# arbitrarily deeply but what we want is a flat list of text segments
# to pass to the sentence segmenter. Not all inlines are
# equal. Consider:
#
# * inlines that are always considered separate segments
#   (e.g. definitions, abbreviations, quotations, variables, code)
#
# * inlines that are never considered separate segments (e.g. links)
#
# * inlines that are concatenated to adjacent segments if there is no
#   whitespace on either side of the join (e.g. emphasis; consider
#   `<em>un</em>believable!` should end up as one segment)
#
# Once we have a flat array of strings (per identifiable document
# fragment), we pass those through the sentence segmenter to get
# sentences. From there we can split the sentences into clauses, which
# is thankfully regexable, as commas, (semi)colons, dashes etc are
# less ambiguous than periods or question/exclamation marks. This
# final result is what we send to the tokenizer (if we want, we can
# also remove stop words), and ultimately count as N-grams.
#
# The intermediate product is a mapping from an identified text
# segment (eg a document or section thereof) to a word, its frequency
# in the segment, along with a mapping of that word to the words found
# to its immediate left or right (including nil) and the frequency
# they are found adjacent to one another. From this basic element, we
# can construct arbitrarily long N-grams and just say something like
# P(ABC) = P(AB)P(BC) (which probably breaks all sorts of rules but it
# will be good enough for what we are trying to do, which is to
# display a sorted list of candidates and match them to known terms).
# This should be something we can punt out as JSON and ship around;
# it's gonna be too hairy as RDF.
#
# ```json
# {
#   "fragment-31337": {
#     "count": 123,
#     "words": {
#       "Foo": {
#         "lemma": "foo"
#         "count": 12,
#         "left":  { "": 12 },
#         "right": { "": 8, "Bar": 3, "bar": 1 }
#       },
#       "Bar": {
#         "lemma": "bar",
#         "count": 3,
#         "left":  { "Foo": 3 },
#         "right": { "": 3 }
#       }
#     }
#   }
# }
# ```
# ... etc. 
#
# Oh also I suppose we can generate TF-IDF scores or whatever with
# that data too.
#
# OK so other stuff: say we have a set of extracted terms and we want
# to compare it with a concept scheme w want to 
#
module RDF::SAK::NLP
  # This class encapsulates a cache of SKOS concepts (either a concept
  # scheme, a collection, an ordered collection, or just a bundle of
  # concepts) and organizes them by label
  class TermCache
    # initialize from a scheme or collection
    def self.from_scheme repo, subject
    end

    # Concepts can be either a hash of the form `{ subject => struct }`
    # or just an array of subjects with a repo
    def initialize concepts, repo: nil
    end

    # Match a label (or labels) to one or more items in the cache.
    def match label, fuzzy: false
      # step zero: coerce label to array of nfkc strings
      # step 1: sort labels from longest to shortest
      # now we go: exact match, lemmatized, normalized and lemmatized
    end
  end

  private

  # https://html.spec.whatwg.org/#usage-summary
  HARVEST_DEFAULT = {
    'http://www.w3.org/1999/xhtml' => %i[
      dfn abbr span var kbd samp code q cite data time mark].freeze
  }.freeze

  public

  # Recurse into an X(HT?)ML document, harvesting a given set of tags
  # for a given namespace. Returns an array of arrays of the form
  # `[:name, "text", "alt"]`, which can be manipulated by a
  # block. Note the block gets the element itself prepended to the
  # array for further processing.
  #
  # @param node [Nokogiri::XML::Node] the origin node
  # @param mapping [Hash] A mapping of namespaces to arrays of tags
  # @yieldparam text [String] the element's (flattened) text
  # @yieldparam alt  [String, nil] the element's alternate text
  #   (currently hard-coded as the `title` attribute)
  # @yieldparam name [Symbol] the element's local name
  # @yieldparam node [Nokogiri::XML::Element] the current element
  # @yieldreturn [Array] a potentially modified array of inputs
  # @return [Array] an array of arrays
  #
  def harvest_tags node, mapping: HARVEST_DEFAULT, &block

    out = []

    if node.element?
      ns   = node.namespace.respond_to?(:href) ? node.namespace.href : nil
      name = node.name.to_sym
      if mapping[ns] and mapping[ns].include?(name)
        text = node.text.strip
        text = text.empty? ? nil : text # make this nil if empty
        alt  = node[:title]             # XXX maybe parametrize this?

        # only run the block/append if there is something there
        if text or alt
          out << (block ? block.call(text, alt, name, node) : [text, alt, name])
        end
      end
    end

    # recurse lol
    out + node.children.map do |c|
      harvest_tags c, mapping: mapping, &block
    end.flatten(1) # shuck off the first layer of array
  end

  def pre_segment element
  end

  def segment doc
  end

  # this is dumb but whatever

  def lemmatize text, type = nil
    # XXX parameters for these? lol
    tag = @@tagger ||= EngTagger.new
    lem = @@lemma  ||= Lemmatizer.new

    tag.tag_pairs(text.strip).map do |pair|
      word, t = pair
      unless %i[pos pp sym].any? { |s| s == t }
        # if the word is merely capitalized we downcase it (XXX maybe
        # do something smarter like check if more than 50% of the
        # characters are uppercase rather than just the first one; ie
        # more than half, the thing is an acronym)
        word.downcase! if word == word.downcase.capitalize
        lem.lemma word, POS_MAP[t] || type
      end
    end.compact.join ' '
  end

  # make these instance methods available to the module
  extend self
end
