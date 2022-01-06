require 'rdf/sak/graphops'

module RDF::SAK
  # This class is intended to be a caching URI (and URI-adjacent)
  # resolver, intended to persist only as long as it needs to, as the
  # cache is not very sophisticated.
  class Resolver

    private

    def self.coerce_terms arg
    end

    def coerce_repo repo
      
    end

    def coerce_resource arg, as: :rdf
      # XXX we will move this out of util
      RDF::SAK::Util.coerce_resource arg, @base, as: as
    end

    def coerce_resources arg, as: :rdf
      (arg.respond_to?(:to_a) ? arg.to_a : [arg]).map do |c|
        c = RDF::SAK::Util.coerce_resource(c, @base, as: as) or next
        # we try to turn these into vocab terms
        (RDF::Vocabulary.find_term c rescue c) || c
      end.compact
    end

    public

    # Create a new URI resolver.
    #
    # @param repo [RDF::Repository] where we get our data from
    # @param base [URI, RDF::URI] base _URL_ (as in dereferenceable)
    # @param aliases [Array<URI, RDF::URI>] alternative base URLs to
    #  be treated as equivalent in lookups
    # @param prefixes [Hash{Symbol, nil => RDF::Term}] the prefix map
    # @param documents [RDF::URI, Array<RDF::URI>] the RDF classes we
    #  consider to be "documents"
    # @param frag_map [Hash{RDF::URI => Array<RDF::URI, Array(RDF::URI, true)>}]
    #  a mapping of RDF classes to (inverse?) predicates for deriving
    #  fragments
    #
    def initialize repo, base, aliases: [], prefixes: {},
        documents: RDF::Vocab::FOAF.Document, frag_map: {}

      # set the base uri; store it as as a URI rather than RDF::URI
      @base    = coerce_resource  base,    as: :uri
      @aliases = coerce_resources aliases, as: :uri

      # register the classes we consider to be "documents"
      @docs = coerce_resources(documents).to_set
      @docs = (@docs.respond_to?(:to_a) ? @docs.to_a : [@docs]).map do |c|
        c = Util.coerce_resource(c, base) or next
        (RDF::Vocabulary.find_term c rescue c) || c
      end.compact.to_set

      # register the mapping of the form { RDFClass => [p1, p2..pn] }
      # where pn is either a predicate or a pair signifying the
      # predicate should be evaluated inversely
      @frags = frag_map.dup.freeze

      # cache of subjects in the graph
      @subjects = {}
      # cache of URIs (not necessarily UUIDs) to host documents
      # (UUIDs), or nils where the URIs are themselves full documents
      @hosts = {}
      # uri -> uuid cache
      @uuids = {}
      # uuid -> uri cache
      @uris = {}
    end

    # Clear the resolver's caches but otherwise keep its configuration.
    #
    # @return [true] constant true return value that can be ignored
    #
    def flush
      # empty em all out
      [@subjects, @hosts, @uuids, @uris].each(&:clear)

      # this is a throwaway result mainly intended to mask what would
      # otherwise return an empty `@uris`
      true
    end

    # Return the UUID(s) associated with the subject. May return
    # `nil` or be a no-op.
    #
    # @param subject [URI, RDF::URI, #to_s] the URI to resolve
    # @param scalar [true, false] whether to return only one UUID if
    #  more than one is resolved; always returns an array if false
    # @param verify [true, false] whether any UUID found in the input
    #  should be resolved against the graph
    # @param as [:rdf, :uri, :str] coerce the output to either
    #  {RDF::URI}, {URI}, or string literal, respectively
    # @param published [false, true] whether to constrain the
    #  UUID resolution to published documents only
    # @param noop [false, true] whether to return `nil` or otherwise
    #  just echo back the input if a UUID can't be resolved
    #
    # @return [URI, RDF::URI, Array<URI, RDF::URI>, nil]
    #
    def uuid_for subject, scalar: true, verify: true, as: :rdf,
        published: false, noop: false
      subject = coerce_resource subject, @base
      # XXX we will eventually move `canonical_uuid` to this class
      # because it always needs a repo and a base URI anyway
      Util.canonical_uuid 
    end

    # Return the (hopefully dereferenceable) URI(s) associated with
    # the subject. This will always return something even if it is a
    # no-op, such as URIs that are valid but not present in the graph.
    # A resource determined to be a document fragment will be resolved
    # to its host document and appended as a fragment identifier.
    # Blank nodes are skolemized, resolved to `/.well-known/genid/...`
    # if they can't be resolved to a host document. UUIDs (in
    # canonical or UUID-NCName form) are resolved (again, if not
    # fragments) as a single path segment off the base (`/<uuid>`).
    # CURIEs are expanded into their respective terms.
    #
    # @param subject [RDF::URI, RDF::Node, URI, String] URI, blank
    #  node, UUID, or CURIE to be resolved
    # @param as [:rdf, :uri] coerce the output to one or the other form
    # @param relative [false, true] return relative to base
    #
    # @return [URI, RDF::URI, Array<URI, RDF::URI>] the URI(s)
    #
    def uri_for subject, as: :rdf, relative: false, roundtrip: false
      # XXX we will eventually move `canonical_uri` to this class
      # because it always needs a repo and a base URI anyway
      RDF::SAK::Util.canonical_uri @repo, subject
    end

    # Returns the "host document" of a given subject (or `nil` if the
    # subject is not a fragment)
    #
    # @return [URI, RDF::URI, nil] the host document, if any
    #
    def host_doc subject, unique: true, as: :rdf, published: false
      subject = uuid_for subject, noop: true
    end

    # Abbreviate one or more URIs into one or more CURIEs if we
    # can. Will through if `noop:` is true, or if false, return `nil`
    # for any URI that can't be abbreviated this way.
    #
    # @param subject [URI, RDF::URI, #to_s, Array<URI, RDF::URI, #to_s>]
    #  the subject URI(s) to abbreviate
    # @param scalar [true, false] always returns an array if false;
    #  ignored if passed an array
    # @param noop [true, false] whether to leave the input alone if it
    #  can't abbreviate
    # @param sort [true, false] whether to sort the resulting array;
    #  meaningless if `scalar` is true
    #
    # @return [String, Array<String>, nil] the CURIE(s) 
    #
    def abbreviate subject, scalar: true, noop: true, sort: true
    end

    # Determine whether the subject is "published", which canonically
    # translates to whether querying `?subject bibo:status
    # bs:published .` returns something. If `circulated` is true, it
    # will also consider `ci:circulated` as "published". If `retired`
    # is false (the default), the presence of `ci:retired` will
    # short-circuit the other tests and return false. When `retired`
    # is true, the presence of `ci:retired` is ignored. If `indexed`
    # is true, the presence of `?subject ci:indexed false .` will
    # cause this to return false.
    #
    # @param subject [RDF::Term] the resource to look up
    # @param circulated [false, true] whether to also count
    #  `ci:circulated` as "published"
    # @param retired [false, true] whether to compute the status even
    #  if `ci:retired` is in the statuses
    # @param indexed [false, true] whether to compute the status even
    #  if `ci:indexed` is false
    #
    # @return [false, true] whether or not the subject is "published"
    #
    def published? subject, circulated: false, retired: false, indexed: false
    end

    # this thing needs its own souped-up struct_for because of
    # separation of concerns

    # 
    def struct_for subject, rev: false, only: [], entail: false,
        uuids: false, canon: false
    end
  end
end
