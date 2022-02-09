require 'rdf/sak/graphops'
require 'rdf/sak/uril/clean'

require 'uri'
require 'uuidtools'
require 'uuid/ncname'

module RDF::SAK
  # This class is intended to be a caching URI (and URI-adjacent)
  # resolver, intended to persist only as long as it needs to, as the
  # cache is not very sophisticated.
  class Resolver
    include RDF::SAK::Util::Clean

    private

    def sanitize_prefixes prefixes, nonnil = false
      raise ArgumentError, 'prefixes must be a hash' unless
        prefixes.is_a? Hash or prefixes.respond_to? :to_h
      prefixes = prefixes.to_h.map do |k, v|
        k = k.to_s.to_sym if k
        v = RDF::Vocabulary.find(v.to_s) || RDF::URI(v.to_s) if
          v and not v.is_a?(RDF::URI)
        [k, v]
      end.to_h

      prefixes.reject! { |k, v| k.nil? || v.nil? } if nonnil
      prefixes
    end

    public

    attr_reader :repo, :base, :aliases, :prefixes

    # Create a new URI resolver.
    #
    # @param repo [RDF::Repository] where we get our data from
    # @param base [URI, RDF::URI] base _URL_ (as in dereferenceable)
    # @param aliases [Array<URI, RDF::URI>] alternative base URLs to
    #  be treated as equivalent in lookups
    # @param prefixes [Hash{Symbol, nil => RDF::Term}] the prefix map
    #
    def initialize repo, base, aliases: [], prefixes: {}
      @repo = repo
      raise ArgumentError, 'repo must be RDF::Queryable' unless
        repo.is_a? RDF::Queryable

      # set the base uri; store it as as a URI rather than RDF::URI
      @base     = coerce_resource   base,    as: :uri
      @aliases  = coerce_resources  aliases, as: :uri
      @prefixes = sanitize_prefixes prefixes

      # cache of subjects in the graph so we only look them up once
      @subjects = {}
      # cache of URIs (likely but not necessarily UUIDs) to host
      # documents (UUIDs), or nils where the URIs are themselves full
      # documents
      @hosts = {}
      # uri -> uuid cache
      @uuids = {}
      # uuid -> uri cache
      @uris  = {}
    end

    # Clear the resolver's caches but otherwise keep its configuration.
    #
    # @return [true] constant true return value that can be ignored
    #
    def flush
      # empty em all out
      [@subjects, @hosts, @uuids, @uris].each(&:clear)

      # this is a throwaway result mainly intended to mask what would
      # otherwise return the array
      true
    end

    private

    R3986   = /^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$/
    SF      = /[^[:alpha:][:digit:]\/\?%@!$&'()*+,:;=._~-]/n
    RFC3986 =
      /^(?:([^:\/?#]+):)?(?:\/\/([^\/?#]*))?([^?#]+)?(?:\?([^#]*))?(?:#(.*))?$/
    SEPS = [['', ?:], ['//', ''], ['', ''], [??, ''], [?#, '']].freeze

    public

    # Preprocess a candidate (absolute or relative) URI to produce a
    # valid URI string with the minimally valid set of escaped
    # characters. That is, characters that are unnecessarily
    # percent-encoded (`%XX`) are turned back to their ASCII
    # originals, while ensuring that characters that absolutely _must_
    # be encoded, remain as such.
    #
    # @param uri [#to_s] the URI, absolute or relative, treated as a
    #  string
    # @param extra [#to_s] a set of additional characters to be
    #  escaped, treated as a {Regexp} character class
    #
    # @return [String] the preprocessed URI string
    # 
    def preproc uri, extra = ''
      # take care of malformed escapes
      uri = uri.to_s.b.gsub(/%(?![0-9A-Fa-f]{2})/n, '%25')

      # escape the extras if they exist
      uri.gsub!(/([#{Regexp.quote extra}])/) { |s| '%%%02X' % s.ord } unless
        extra.empty?

      # we want the minimal amount of escaping so we split out the separators
      out = ''
      parts = RFC3986.match(uri).captures
      parts.each_index do |i|
        next if parts[i].nil?
        out << SEPS[i].first
        out << parts[i].b.gsub(SF) { |s| '%%%02X' % s.ord }
        out << SEPS[i].last
      end

      # make sure escaped hex is upper case like the rfc says
      out.gsub(/(%[0-9A-Fa-f]{2})/, &:upcase)
    end

    alias_method :preprocess, :preproc

    # Given a URI as input, split any path parameters out of the last
    # path segment. Works the same way as #split_pp.
    #
    # @param uri [URI,#to_s] The URI to extract parameters from
    # @param only [false, true] whether to only return the parameters
    # @return [Array] (See description)
    #
    def split_pp uri, only: false
      begin
        u = (uri.is_a?(URI) ? uri : URI(uri_pp uri.to_s)).normalize

      rescue URI::InvalidURIError => e
        # these stock error messages don't even tell you what the uri is
        raise URI::InvalidURIError, "#{e.message} (#{uri.to_s})"
      end

      return only ? [] : [uri] unless u.path
      uri = u

      ps = uri.path.split ?/, -1
      pp = ps.pop.split ?;, -1
      bp = (ps + [pp.shift]).join ?/
      uri = uri.dup

      begin
        uri.path = bp
      rescue URI::InvalidURIError => e
        # these stock error messages don't even tell you what the uri is
        m = e.message
        raise URI::InvalidURIError, "#{m} (#{uri.to_s}, #{bp})"
      end

      return pp if only
      [uri] + pp
    end

    # Given a URI as input, split any query parameters into an array of
    # key-value pairs. If `:only` is true, this will just return the
    # pairs. Otherwise it will prepend the query-less URI to the array,
    # and can be captured with an idiom like `uri, *qp = split_qp uri`.
    #
    # @param uri [RDF::URI, URI, #to_s] The URI to extract parameters from
    # @param only [false, true] whether to only return the parameters
    #
    # @return [Array] (See description)
    #
    def split_qp uri, only: false, as: :uri
      uri = coerce_resource uri, as: as
      if uri.query
        qp = URI::decode_www_form(uri.query)
        return qp if only
        uri.query = nil
        [uri] + qp
      elsif only
        []
      else
        [uri]
      end
    end

    # Get the fragment, or otherwise the last non-empty path segment
    # of the URI. Returns `nil` if the URI is not the kind that has a
    # path.
    #
    # @param uri [URI, RDF::URI, #to_s] the URI input
    #
    # @return [String, nil] the slug, if present
    #
    def terminal_slug uri
      uri = coerce_resource uri, as: :uri
      # 
      return unless uri.respond_to? :path
      if f = uri.fragment and not f.empty?
        return f
      elsif p = uri.path
        # we reuse p a bunch but whatever we're dynamically typed here
        if p = /^\/+(.*?)\/*$/.match(p)
          if p = p[1].split(/\/+/).last
            # we need to escape colons or consumers will think it's absolute
            return preproc(p.split(/;+/).first || '', ':')
          end
        end        
      end
      '' # can't remember why but the default return value is empty string
    end

    private

    UUID_ONLY = /\b([0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8})\b/i
    UUID_RE   = /^(?:urn:uuid:)?#{UUID_ONLY}$/i
    UUID_PATH = /^\/+#{UUID_ONLY}/

    # lol ruby booleans do not coerce to integers so here we are
    BITS = { nil => 0, false => 0, true => 1 }.freeze

    public

    # Return the UUID(s) associated with the subject. May return
    # `nil` or be a no-op, if specified.
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
    def uuid_for uri, scalar: true, verify: true, as: :rdf,
        published: false, circulated: false, noop: false
      # this ensures the input is an absolute RDF::URI
      orig = uri = coerce_resource uri, @base

      # do an initial check here to determine if we already have a UUID
      unless uri.is_a? RDF::Node
        # we make a URI object so we can test it easier
        tu = URI(preproc uri).normalize

        # if we find a uuid in the path, we extract it
        if tu.path && !tu.fragment &&
            UUID_RE.match?(uu = tu.path.delete_prefix(?/))
          tu = URI('urn:uuid:' + uu.downcase)
        end

        # unconditionally overwrite the URI
        uri = RDF::URI(tu.to_s)

        # now we check for a compact UUID fragment or UUID URN
        if tu.fragment and
            (uu = UUID::NCName.from_ncname(tu.fragment, validate: true))
          # this is the special case that the fragment is a compact uuid
          uu = RDF::URI("urn:uuid:#{uu}")
          if !verify or @subjects[uu] ||= @repo.has_subject?(uu)
            return scalar ? uu : [uu]
          end
        elsif tu.respond_to? :uuid
          # in this case the URI is already a UUID, so now we check
          # if it's a subject
          if !verify or @subjects[uri] ||= @repo.has_subject?(uri)
            return scalar ? uri : [uri]
          end
        end
      end

      # return our result from cache if present
      if out = @uuids[orig]
        return scalar ? out.first : out
      end

      # this (assuming the input has a path) will give us a stack of
      # URIs containing successively fewer path parameters (if there
      # are no path parameters or no path, then there will only be the
      # one URI).
      uris = if uri.respond_to? :path and uri.path.start_with? ?/
               # split off path parameters
               uu, *pp = split_pp uri
               if pp.empty?
                 [uri] # no path parameters; this is a noop
               else
                 uu = RDF::URI(uu.to_s)
                 bp = uu.path # base path
                 # this counts down from all parameters to zero
                 (0..pp.length).to_a.reverse.map do |i|
                   u = uu.dup
                   u.path = ([bp] + pp.take(i)).join(?;)
                   u # uri with the first 0..i path parameters
                 end
               end
             else
               [uri] # URI does not have a path
             end

      # prior to other criteria, we sort UUID candidates by two
      # dimensions: exact match on a URI (i.e., where the candidate is
      # a resource vs whether it is just a slug), and canonicality
      # (i.e., whether the relation has been demarcated as canonical).
      # This list shows how the ranks map to bits, and then integers,
      # so they can be compared with an ordinary <=> operator.
      #
      # * (00) exact & canonical == 0,
      # * (01) exact == 1,
      # * (10) inexact & canonical == 2,
      # * (11) inexact == 3.
      #
      # Subsequent comparison criteria include whether the resource
      # is considered "published", and its latest associated date.

      # obtain the raw candidates for our stack of URIs using the
      # ci:canonical/owl:sameAs mechanism, ie exact match
      sa = @repo.property_set [RDF::SAK::CI.canonical,
        RDF::SAK::CI.alias, RDF::OWL.sameAs]
      candidates = nil
      uris.each do |u|
        # this will give us a hash where the keys are 
        candidates = @repo.subjects_for(sa, u, entail: false) do |s, f|
          # skip non-uuid subjects
          next unless UUID_RE.match? s
          [s, {
            # we xor this because BITS[true] ^ 1 == 0, and 0 < 1
            rank: BITS[f.include? RDF::SAK::CI.canonical] ^ 1,
            published: @repo.published?(s, circulated: circulated),
            mtime: @repo.dates_for(s).last || DateTime.new }]
        end.compact.to_h

        # this is a funny way to say quit on the first match
        nnbreak unless candidates.empty?
      end

      # after we have checked the URI(s) verbatim against the graph,
      # but before we start checking slugs, we can try to harvest some
      # host documents, assuming out URI has a fragment.
      hosts = if uri.uri? and uri.fragment and not uri.fragment.empty?
                tmp = uri.dup
                tmp.fragment = nil
                h = canonical_uuid tmp, scalar: false, published: published,
                  circulated: circulated
                # a fragment URI for which the non-fragment part does
                # not resolve to a UUID should likewise not resolve
                # (XXX: or should it?)
                return scalar ? nil : [] if h.empty?
                h # the hosts
              end

      # okay *now* do the slugs
      slug = terminal_slug uri
      if slug and slug != ''
        exact = uri == coerce_resource(slug)
        sl = [RDF::SAK::CI['canonical-slug'], RDF::SAK::CI.slug]
        [RDF::XSD.string, RDF::XSD.token].each do |t|
          repo.subjects_for(sl, RDF::Literal(slug, datatype: t)) do |s, f|
            # skip non-uuid subjects
            next unless UUID_RE.match? s
            entry = candidates[s] ||= {
              rank: 0b11,
              published: @repo.published?(s, circulated: circulated),
              mtime: @repo.dates_for(s).last || DateTime.new }
            # reset the rank if it is a lower number (higher rank)
            rank  = (BITS[exact] << 1 | BITS[f.include? sl.first]) ^ 0b11
            entry[:rank] = rank if rank < entry[:rank]
          end
        end
      end

      # okay now that we have the candidates, let's make sure, e.g.,
      # that the fragment actually maps to a host
      if hosts
        # XXX jklol this needs to be implemented
      end

      # here is where we go sniffing for replacements. we turn the
      # candidates hash into an array to iterate over it because we
      # mess with the actual candidates hash in the loop
      candidates.to_a.each do |k, v|
        # find any replacements
        reps = @repo.replacements_for(k, published: published) - [k]

        # 
        unless reps.empty?
          v[:replaced] = true
          reps.each do |r|
            c = candidates[r] ||= {
              rank: v[:rank], published: @repo.published?(r),
              mtime: @repo.dates_for(r).last || v[:mtime] || DateTime.new }
            
            # adjust rank and modification time of the replacement to
            # that of the replaced if they are more favourable
            c[:rank]  = v[:rank]  if v[:rank]  < c[:rank]
            c[:mtime] = v[:mtime] if v[:mtime] > c[:mtime]
          end
        end
      end

      # now we can remove all unpublished candidates if the context is
      # published
      candidates.select! do |_, v|
        !v[:replaced] && (published ? v[:published] : true)
      end

      out = candidates.sort do |a, b|
        # we are mainly interested in the structs we generated
        as, bs = [a, b].map(&:last)

        # check publication status (contingent), rank, then modification time
        c = published ? BITS[bs[:published]] <=> BITS[as[:published]] : 0
        c = as[:rank]  <=> bs[:rank]  if c == 0
        c = bs[:mtime] <=> as[:mtime] if c == 0

        # finally compare lexically if none of the others resolve
        c == 0 ? a.first <=> b.first : c
      end.map(&:first).compact

      # cache if there is something to cache
      @uuids[orig] = out unless out.empty?

      # return the first (ie most preferred) UUID
      scalar ? out.first : out
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
    def uri_for term, scalar: true, as: :rdf, relative: false,
        roundtrip: false, slugs: false, fragments: true
      term = coerce_resource term

      # harvest uuid out of term if present
      uuid = case
             when m = UUID_RE.match(term.to_s)
               RDF::URI("urn:uuid:#{m.captures.first.downcase}")
             when term.respond_to?(:fragment) &&
                 v = UUID::NCName.valid?(term.fragment)
               RDF::URI(UUID::NCName.from_ncname(
                 term.fragment, version: v, format: :urn))
             end

      # now we do the round trip if called for
      term = roundtrip ? uuid_for(uuid || term) : uuid

      # give us the host uri if available
      hosturi = if uuid
                  # XXX what do we do about explicit graphs? also published?
                  host = @repo.host_for uuid
                  # note function-level scope of hosturi
                  uri_for(host, slugs: true) if host
                end

      # create an appropriate map function depending on whether there
      # is a host URI so the condition is only tested once
      umap = if hosturi
               lambda do |o|
                 h = hosturi.dup
                 h.fragment = o.value
                 h
               end
             else
               lambda { |o| @base + o.value }
             end

      # generate a comparator proc
      cmp = @repo.cmp_uri prioritize: [@base] + @aliases

      # obtain a sorted list of primary URIs (those identified by
      # ci:canonical and ci:canonical-slug)
      primary = @repo.objects_for(
        term, RDF::SAK::CI.canonical, only: :resource).sort(&cmp)
      if term.uri? and (host or slugs) and (primary.empty? or not scalar)
        primary += @repo.objects_for(term, RDF::SAK::CI['canonical-slug'],
          only: :literal, datatype: RDF::XSD.token).map(&umap).sort(&cmp)
      end

      secondary = []
      if primary.empty? or not unique
        secondary = @repo.objects_for(subject,
          [RDF::OWL.sameAs, RDF::SAK::CI['alias-for']],
          entail: false, only: :resource).sort(&cmp)
        if subject.uri? and (slugs or host)
          secondary += @repo.objects_for(
            subject, RDF::SAK::CI.slug, entail: false,
            only: :literal, datatype: RDF::XSD.token).map(&umap).sort(&cmp)   
        end
      end

      # in the final case append the UUID to the base
      uri = URI(preproc term)
      if uri.respond_to? :uuid
        if hosturi
          h = hosturi.dup
          h.fragment = UUID::NCName.to_ncname uri.uuid
          secondary << RDF::URI(h.to_s)
        else
          u = @base.clone
          u.query = u.fragment = nil
          u.path = ?/ + uri.uuid
          secondary << RDF::URI(u.to_s)
        end
      end

      # 
      out = (primary + secondary).uniq

      # eliminate fragment URIs unless explicitly allowed
      unless fragment
        tmp = out.reject(&:fragment)
        out = tmp unless tmp.empty?
      end

      # turn these into URIs if the thing says so
      out.map! { |u| URI(preproc u.to_s) } if as == :uri

      scalar ? out.first : out 
    end

    # Resolve a CURIE to a full URI using the embedded prefix map,
    # with optional overrides. Multiple values, including CURIE
    # strings containing spaces, will be split and expanded out
    # individually. Safe CURIEs (as in encased in square brackets) are
    # handled appropriately.
    #
    # @param curie [String, Array<String>] one or more CURIEs
    # @param as [:rdf, :uri, :term, false, nil] coercion types
    # @param scalar [true, false] whether to return a single value
    # @param base [URI, RDF::URI] overriding base URI
    # @param prefixes [Hash{Symbol, nil => RDF::Vocabulary}]
    #  overriding prefix map, if needed
    #
    # @return [URI, RDF::URI, Array<URI, RDF::URI>, nil]
    #
    def resolve_curie curie, as: :term, scalar: true, base: nil, prefixes: {}
      # override the base if present
      base = base ? coerce_resource(base, as: :uri) : @base

      # smush together any overriding prefixes
      prefixes = @prefixes.merge(sanitize_prefixes prefixes)

      out = (curie.respond_to?(:to_a) ? curie.to_a : [curie]).map do |c|
        RDF::SAK::Util.normalize_space(c).split
      end.flatten.compact.map do |c|
        prefix, slug = /^\[?(?:([^:]+):)?(.*?)\]?$/.match(c).captures
        prefix = prefix.to_sym if prefix
        tmp = if prefixes[prefix]
                (((term || prefix) ? prefixes[prefix] : base) + slug).to_s
              else
                noop ? c : nil
              end

        tmp ? coerce_resource(tmp, as: as) : tmp
      end

      scalar ? out.first : out
    end

    # Abbreviate one or more URIs into one or more CURIEs if we
    # can. Will through if `noop:` is true, or if false, return `nil`
    # for any URI that can't be abbreviated this way.
    #
    # @param term [URI, RDF::URI, #to_s, Array<URI, RDF::URI, #to_s>]
    #  the URI(s) to abbreviate
    # @param scalar [true, false] always returns an array if false;
    #  ignored if passed an array
    # @param noop [true, false] whether to leave the input alone if it
    #  can't abbreviate
    # @param sort [true, false] whether to sort the resulting array;
    #  meaningless if `scalar` is true
    #
    # @return [String, Array<String>, nil] the CURIE(s) in question
    #
    def abbreviate term, as: :rdf, scalar: true,
        noop: true, sort: true, prefixes: {}

      term = coerce_resources term
      as   = assert_uri_coercion as

      # this returns a duplicate that we can mess with
      prefixes = @prefixes.merge(sanitize_prefixes prefixes)

      rev = prefixes.invert

      term.map! do |t|
        t = t.to_s
        slug = nil # we want this value to be nil if no match and !noop

        # try matching each prefix URI from longest to shortest
        rev.sort { |a, b| b.first.length <=> a.first.length }.each do |uri, pfx|
          slug = t.delete_prefix uri.to_s
          # this is saying the URI either doesn't match or abbreviates to ""
          if slug == t or pfx.nil? && slug.empty?
            slug = nil
          else
            # it's already a slug so we add a prefix if there is one
            slug = '%s:%s' % [pfx, slug] unless pfx.nil?
            break # we have our match
          end
        end

        # at this point slug is either an abbreviated term or nil, so:
        slug ||= t if noop

        coerce_resource(slug, as: as)
      end

      # only sort if noop is set
      term.sort! if noop && sort

      scalar ? term.first : term
    end

    # Return the subset of prefixes, in the form of a `{ foo: "bar" }`
    # {Hash}, that cover, to the extent of available prefix mappings,
    # the set of terms passed in. The terms can be embedded in any
    # kind of data structure that can be flattened into an {Array}.
    # Elements not belonging to the {URI} (which are coerced),
    # {RDF::URI}, and {RDF::Literal} (from which datatypes are
    # harvested) classes are ignored. The `nil` key can be interpreted
    # as the `vocab` for the given scope.
    #
    # @param terms [Array<RDF::URI, RDF::Literal, URI>] an array (or
    #  something that can ultimately be turned _into_ an array) of
    #  terms
    #
    # @return [Hash{Symbol, nil => RDF::Vocabulary::Term}] the prefix subset
    #
    def prefix_subset terms
      # sniff out all the URIs and datatypes
      terms = smush_struct terms, uris: true

      # now we abbreviate all the resources
      pfx = abbreviate(
        terms.to_a, noop: false, sort: false).uniq.compact.map do |p|
        p.split(?:).first.to_sym
      end.uniq.to_set

      # now we return the subset
      @prefixes.select { |k, _| pfx.include? k.to_sym }
    end

    # this thing needs its own souped-up struct_for because of
    # separation of concerns

    #
    def struct_for subject, rev: false, only: [], entail: false,
        uuids: false, canon: false
      struct = @repo.struct_for subject
    end

    # Returns the "host document" of a given subject (or `nil` if the
    # subject is not a fragment)
    #
    # @return [URI, RDF::URI, nil] the host document, if any
    #
    def host_for subject, unique: true, as: :rdf, published: false
      subject = uuid_for subject, noop: true
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

    # Generate a random (version 4 in RFC 4122) UUID URN.
    #
    # @param as [:rdf, :uri, false, nil] how to coerce the result
    #
    # @return [RDF::URI, URI] the UUID URN
    #
    def uuidv4 as: :rdf
      coerce_resource(UUIDTools::UUID.random_create.to_uri, as: as)
    end
  end
end
