require 'intertwingler/error'
require 'intertwingler/graphops'
require 'intertwingler/vocab'
require 'intertwingler/loggable'
require 'intertwingler/util/clean'

require 'uri'
require 'uuidtools'
require 'uuid/ncname'
require 'digest'
require 'base64'

# This class is intended to be a caching URI (and URI-adjacent)
# resolver, intended to persist only as long as it needs to, as the
# cache is not very sophisticated.
class Intertwingler::Resolver
  include Intertwingler::Loggable
  include Intertwingler::Util::Clean

  private

  ITCV = Intertwingler::Vocab::ITCV
  TFO  = Intertwingler::Vocab::TFO
  XSD  = RDF::XSD
  SH   = RDF::Vocab::SH

  def self.configure_one repo, subject, log: nil
    # 0) assert that the subject is indeed a resource
    subject = coerce_resource subject

    # 1) get site under management and aliases
    base    = repo.objects_for(subject, ITCV.manages, only: :uri).sort.first
    aliases = repo.objects_for(subject, ITCV.alias, only: :uri).sort

    # 2) get prefixes and vocab
    prefixes = repo.objects_for(
      subject, ITCV.prefix, only: :resource).reduce({}) do |h, n|
      pfx = repo.objects_for(n, SH.prefix, only: :literal).sort.first
      uri = repo.objects_for(n,
        SH.namespace, only: :literal, datatype: XSD.anyURI).sort.first
      h[pfx.value.to_sym] = uri.value if pfx && uri

      h
    end
    vocab = repo.objects_for(subject, ITCV.vocab).select do |o|
      o.iri? or o.literal? && o.datatype? && o.datatype == XSD.anyURI
    end.sort.first

    prefixes[nil] = vocab.value if vocab


    # 3) get document and fragment specifications
    documents = repo.objects_for(subject, ITCV.document, only: :uri)
    fragments = repo.objects_for(
      subject, ITCV[:'fragment-list'], only: :resource)
    unless fragments.empty?
      fragments = RDF::List.new(
        subject: fragments.first, graph: repo).to_a.map do |f|
        # fragment class
        c = repo.objects_for(f, ITCV[:'fragment-spec'], only: :uri)
        # via shacl property path
        v = repo.objects_for(f, ITCV.via, only: :resource).map do |path|
          repo.process_shacl_path path
        end

        # host class
        h = repo.objects_for(f, ITCV[:'host-class'], only: :uri)
        # exceptions to host
        e = repo.objects_for(f, ITCV[:'except-class'], only: :uri)

        [c, v, h, e]
      end
    end

    self.new repo, base, aliases: aliases, prefixes: prefixes,
      subject: subject, documents: documents, fragments: fragments, log: log
  end

  # this is dumb but we have both URI and RDF::URI in the mix
  def set_alias uri, base
    # this will work whether it's URI or RDF::URI
    return uri unless /^https?$/i.match? uri.scheme and
      base and base.respond_to? :authority and
      authorities.include? base.authority.downcase

    # we have to dup in case it's frozen
    uri = uri.dup
    uri.scheme = base.scheme
    uri.host   = base.host
    uri.port   = base.port

    uri
  end

  public

  # Return a hash mapping a set of RDF prefixes to their vocabularies.
  #
  # @param prefixes [Hash, #to_h] the input prefixes
  # @param nonnil [false, true] whether to remove the nil prefix
  #
  # @return [Hash{Symbol=>RDF::Vocabulary}] sanitized prefix map
  #
  def sanitize_prefixes prefixes, nonnil: false
    Intertwingler::Util::Clean.sanitize_prefixes prefixes,
      nonnil: nonnil, cache: @vocabs
  end

  def self.locate repo
    repo.all_of_type Intertwingler::Vocab::ITCV.Resolver
  end

  def self.configure repo, subject: nil, authority: nil, log: nil
    if subject
      return subject.map { |s| configure_one repo, s} if subject.is_a? Array

      configure_one repo, subject
    elsif authority
      candidates = %w[http https].map do |scheme|
        base = RDF::URI("#{scheme}://#{authority}/")
        repo.subjects_for(ITCV.manages, base, only: :resource)
      end.flatten.uniq

      case candidates.size
      when 1 then return configure_one repo, candidates.first
      when 0 then raise Intertwingler::Error::Config,
          "No resolver found for #{authority}"
      else raise Intertwingler::Error::Config,
          'Multiple resolvers identified for %s: %s' %
          [authority, candidates.join(', ')]
      end
    else
      locate(repo).map { |s| configure_one repo, r, log: log }
    end
  end

  attr_reader :repo, :base, :aliases, :prefixes, :id, :documents, :fragments
  alias_method :subject, :id

  # Create a new URI resolver.
  #
  # @param repo [RDF::Repository] where we get our data from
  # @param base [URI, RDF::URI] base _URL_ (as in dereferenceable)
  # @param aliases [Array<URI, RDF::URI>] alternative base URLs to
  #  be treated as equivalent in lookups
  # @param prefixes [Hash{Symbol, nil => RDF::Term}] the prefix map
  #
  def initialize repo, base, aliases: [], prefixes: {}, subject: nil,
      documents: [], fragments: [], log: nil

    @repo = repo
    raise ArgumentError, 'repo must be RDF::Queryable' unless
      repo.is_a? RDF::Queryable

    # set the base uri; store it as as a URI rather than RDF::URI
    @base     = coerce_resource   base,    as: :uri
    @aliases  = coerce_resources  aliases, as: :uri
    @prefixes = sanitize_prefixes prefixes
    @id       = coerce_resource subject if subject

    @documents = documents.to_set
    @fragments = fragments # this is in order

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
    # map uri.to_s to rdf vocab
    @vocabs = {}

    @log = log
  end

  # Clear the resolver's caches but otherwise keep its configuration.
  #
  # @return [true] constant true return value that can be ignored
  #
  def flush
    # empty em all out
    [@subjects, @hosts, @uuids, @uris, @vocabs].each(&:clear)

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

  # Return the set of URI authorities (host-port pairs) associated
  # with this resolver, beginning with the base and following with
  # the aliases.
  #
  # @return [Array<String>] the authorities
  #
  def authorities
    ([@base] + @aliases).select { |u| u.respond_to? :authority }.map do |u|
      u.authority.to_s.downcase
    end.uniq
  end

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
  def self.preproc uri, extra = ''
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

  define_method :preproc, self.singleton_method(:preproc).to_proc

  alias_method :preprocess, :preproc

  # Given a URI as input, split any path parameters out of the last
  # path segment. Works the same way as #split_pp.
  #
  # @param uri [URI, #to_s] The URI to extract parameters from
  # @param only [false, true] whether to only return the parameters
  # @param parse [false, true] whether to parse the path parameters
  # @return [Array] (See description)
  #
  def self.split_pp uri, only: false, parse: false
    begin
      u = (uri.is_a?(URI) ? uri : URI(preproc uri.to_s)).normalize

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

    pp = pp.map do |param|
      k, v = param.split ?=, 2
      v = v.split ?, if v
      [k, v || []]
    end if parse

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

  define_method :split_pp, self.singleton_method(:split_pp).to_proc

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

  define_singleton_method :coerce_resource,
    Intertwingler::Util::Clean.method(:coerce_resource).unbind

  # Coerce the argument into a resource, either {URI} or {RDF::URI}
  # (or {RDF::Node}). The type can be specified
  #
  # @param arg [#to_s, URI, RDF::URI, RDF::Node] the argument to
  #  coerce into a resource
  # @param as [:rdf, :uri, :term, false, nil] how to coerce the result
  #
  # @return [RDF::URI, URI, RDF::Vocabulary::Term, RDF::Vocabulary, String]
  #
  def coerce_resource arg, as: :rdf
    # again self.class is suuuuuuuuper slow
    Intertwingler::Util::Clean.coerce_resource arg, as: as do |arg|
      begin
        @base ? @base.merge(preproc arg.to_s.strip) : arg
      rescue URI::InvalidURIError => e
        warn "attempted to coerce #{arg} which turned out to be invalid: #{e}"
        nil
      end
    end
  end

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
    orig = uri = coerce_resource uri

    # do an initial check here to determine if we already have a UUID
    unless uri.is_a? RDF::Node
      # swap in our canonical scheme/authority
      if %w[http https].include? uri.scheme.to_s.downcase and
          uri.authority and authorities.include? uri.authority.downcase
        uri = uri.dup # duplicate because frozen sometimes
        uri.scheme    = @base.scheme
        uri.authority = @base.authority
      end

      # we make a URI object so we can test it easier
      tu = URI(preproc uri).normalize

      # if we find a uuid in the path, we extract it
      if tu.path && !tu.fragment &&
          UUID_RE.match?(uu = tu.path.delete_prefix(?/))
        tu = URI('urn:uuid:' + uu.downcase)
      end

      # unconditionally overwrite the URI
      uri = RDF::URI(tu.to_s)

      # warn "hmm #{uri}"

      # now we check for a compact UUID fragment or UUID URN
      if tu.fragment and
          (uu = UUID::NCName.from_ncname(tu.fragment, validate: true))
        # this is the special case that the fragment is a compact uuid
        uu = RDF::URI("urn:uuid:#{uu}")
        if !verify or @subjects[uu] ||= @repo.has_subject?(uu)
          uu = coerce_resource uu, as: as
          return scalar ? uu : [uu]
        end
      elsif tu.respond_to? :uuid
        # in this case the URI is already a UUID, so now we check
        # if it's a subject
        if !verify or @subjects[uri] ||= @repo.has_subject?(uri)
          uri = coerce_resource uri, as: as
          return scalar ? uri : [uri]
        end
      end
    end

    # warn "test #{uri}"

    # return our result from cache if present
    if out = @uuids[orig]
      out = coerce_resources out, as: as
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
    sa = @repo.property_set [Intertwingler::Vocab::CI.canonical,
                             Intertwingler::Vocab::CI.alias, RDF::OWL.sameAs]
    candidates = nil
    uris.each do |u|
      # this will give us a hash where the keys are
      candidates = @repo.subjects_for(sa, u, entail: false) do |s, f|
        # skip non-uuid subjects
        next unless UUID_RE.match? s
        [s, {
          # we xor this because BITS[true] ^ 1 == 0, and 0 < 1
             rank: BITS[f.include? Intertwingler::Vocab::CI.canonical] ^ 1,
             published: @repo.published?(s, circulated: circulated),
             ctime: @repo.dates_for(s,
               predicate: RDF::Vocab::DC.created).last || DateTime.new,
             mtime: @repo.dates_for(s).last || DateTime.new }]
      end.compact.to_h

      # this is a funny way to say quit on the first match
      break unless candidates.empty?
    end

    # after we have checked the URI(s) verbatim against the graph,
    # but before we start checking slugs, we can try to harvest some
    # host documents, assuming out URI has a fragment.
    hosts = if uri.uri? and uri.fragment and not uri.fragment.empty?
              tmp = uri.dup
              tmp.fragment = nil
              h = uuid_for tmp, scalar: false, published: published,
                circulated: circulated, noop: noop
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
      sl = [Intertwingler::Vocab::CI['canonical-slug'],
            Intertwingler::Vocab::CI.slug]
      [RDF::XSD.string, RDF::XSD.token].each do |t|
        repo.subjects_for(sl, RDF::Literal(slug, datatype: t)) do |s, f|
          # skip non-uuid subjects
          next unless UUID_RE.match? s
          entry = candidates[s] ||= {
            rank: 0b11,
            published: @repo.published?(s, circulated: circulated),
            ctime: @repo.dates_for(s,
              predicate: RDF::Vocab::DC.created).last || DateTime.new,
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
            ctime: @repo.dates_for(r, predicate: RDF::Vocab::DC.created).last ||
              v[:ctime] || DateTime.new,
            mtime: @repo.dates_for(r).last || v[:mtime] || DateTime.new }

          # adjust rank and modification time of the replacement to
          # that of the replaced if they are more favourable
          c[:rank]  = v[:rank]  if v[:rank]  < c[:rank]
          c[:mtime] = v[:mtime] if v[:mtime] > c[:mtime]
          c[:ctime] = v[:ctime] if v[:ctime] > c[:ctime]
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
      ax, bx = [a, b].map(&:last)

      # check publication status (contingent), rank, then modification time
      c = published ? BITS[bs[:published]] <=> BITS[as[:published]] : 0
      c = ax[:rank]  <=> bx[:rank]  if c == 0
      c = bx[:mtime] <=> ax[:mtime] if c == 0
      c = bx[:ctime] <=> ax[:ctime] if c == 0

      # finally compare lexically if none of the others resolve
      c == 0 ? a.first <=> b.first : c
    end.map(&:first).compact

    if out.empty?
      # ensure we return noop
      out << orig if noop
    else
      # cache if there is something to cache
      @uuids[orig] = out
    end

    # make these into uri objects if requested
    out = coerce_resources out, as: as

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
  # @param roundtrip [false, true] resolve UUID first
  # @param slugs [false, true] attempt to resolve from slugs as well
  # @param fragments [true, false] resolve fragment URIs
  # @param local [false, true] only return URIs with the same
  #  authority as the base
  # @param via [URI, RDF::URI] a base URI, e.g. one of the aliases
  #
  # @return [URI, RDF::URI, Array<URI, RDF::URI>] the URI(s)
  #
  def uri_for term, scalar: true, as: :rdf, relative: false, roundtrip: false,
      slugs: false, fragments: true, local: false, via: nil
    term = coerce_resource term

    if via
      via = coerce_resource via
      via = nil unless authorities.include? via.authority.downcase
    end

    # harvest uuid out of term if present
    uuid = case
           when m = UUID_RE.match(term.to_s)
             RDF::URI("urn:uuid:#{m.captures.first.downcase}")
           when term.respond_to?(:fragment) &&
               v = UUID::NCName.valid?(term.fragment)
             RDF::URI(UUID::NCName.from_ncname(
               term.fragment, version: v, format: :urn))
           end

    # warn "wat #{uuid} -> #{term}"

    # now we do the round trip if called for
    if tmp = roundtrip ? uuid_for(uuid || term) : uuid
      term = tmp
    else
      term = coerce_resource term, as: as
      term = set_alias term, via
      return scalar ? term : [term]
    end

    # give us the host uri if available
    hosturi = if uuid
                # XXX what do we do about explicit graphs? also published?
                host = host_for uuid
                # note function-level scope of hosturi
                uri_for(host, slugs: true, via: via) if host
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
    cmp = @repo.cmp_resource prioritize: [@base] + @aliases

    # obtain a sorted list of primary URIs (those identified by
    # ci:canonical and ci:canonical-slug)
    primary = @repo.objects_for(term,
      Intertwingler::Vocab::CI.canonical, only: :resource).sort(&cmp)
    if term.uri? and (host or slugs) and (primary.empty? or not scalar)
      primary += @repo.objects_for(term,
        Intertwingler::Vocab::CI['canonical-slug'],
        only: :literal, datatype: RDF::XSD.token).map(&umap).sort(&cmp)
    end

    secondary = []
    if primary.empty? or not scalar
      secondary = @repo.objects_for(term,
        [RDF::OWL.sameAs, Intertwingler::Vocab::CI['alias-for']],
        entail: false, only: :resource).sort(&cmp)
      if term.uri? and (slugs or host)
        secondary += @repo.objects_for(term,
          Intertwingler::Vocab::CI.slug, entail: false,
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
    unless fragments
      tmp = out.reject(&:fragment)
      out = tmp unless tmp.empty?
    end

    # eliminate non-local URIs
    out.select! do |u|
      /^https?$/i.match? u.scheme and u.authority == base.authority
    end if local

    # turn these into URIs if the thing says so
    out.map! do |u|
      u = set_alias u, via
      u = URI(preproc u.to_s) if as == :uri
      u
    end

    scalar ? out.first : out
  end

  # XXX 2022-05-17 NOTE THAT Intertwingler::Util::resolve_curie is more
  # complex than this; it assumes you can hand it stuff from
  # existing markup, so this is kinda the lite version


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
  def self.resolve_curie curie,
      as: :term, scalar: true, base: nil, prefixes: {}, noop: false
    prefixes = { rdf: RDF::RDFV }.merge(
      Intertwingler::Util::Clean.sanitize_prefixes prefixes)

    out = (curie.respond_to?(:to_a) ? curie.to_a : [curie]).map do |c|
      Intertwingler::Util::Clean.normalize_space(c.to_s).split
    end.flatten.compact.map do |c|
      prefix, slug = /^\[?(?:([^:]+):)?(.*?)\]?$/.match(c).captures
      prefix = prefix.to_sym if prefix
      tmp = if v = prefixes[prefix]
              # note that we will need another resolve_curie for
              # dealing with markup
              case v
              when RDF::Vocabulary then v[slug]
              when RDF::URI then v + slug
              else RDF::URI(v.to_s + slug)
              end
            else
              noop ? c : nil
            end

      tmp ? coerce_resource(tmp, as: as) : tmp
    end.compact

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
  def resolve_curie curie, as: :term, scalar: true, base: nil,
      prefixes: {}, noop: false
    # override the base if present
    base = base ? coerce_resource(base, as: :uri) : @base

    # smush together any overriding prefixes
    prefixes = @prefixes.merge(sanitize_prefixes prefixes)

    self.class.resolve_curie curie, as: as, scalar: scalar,
      base: base, prefixes: prefixes, noop: noop
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
  def abbreviate term, scalar: true, noop: true, sort: true,
      prefixes: nil, vocab: nil, cache: nil

    term  = coerce_resources term
    as    = assert_uri_coercion as
    cache = {} unless cache.is_a? Hash

    # why was this not already like this?
    prefixes ||= self.prefixes.dup

    # this returns a duplicate that we can mess with
    if vocab
      vocab = coerce_resource vocab, as: :term
      prefixes[nil] = vocab
    elsif prefixes.key? nil
      prefixes[nil] = coerce_resource prefixes[nil], as: :term
    end

    # only do this if there's something to do, cause it's expensive
    # XXX also figure out a sensible way to cache this move
    prefixes = sanitize_prefixes prefixes unless
      prefixes.empty? or (prefixes.size == 1 and prefixes.key? nil)

    # okay now merge
    prefixes = @prefixes.merge prefixes

    # note since hash key order is preserved this will clobber any
    # explicit namespace prefix for the vocab
    rev = prefixes.invert

    term.map! do |t|
      t = t.to_s
      slug = nil # we want this value to be nil if no match and !noop

      # try matching each prefix URI from longest to shortest
      rev.sort do |a, b|
        b.first.to_uri.to_s.length <=> a.first.to_uri.to_s.length
      end.each do |vocab, pfx|
        # this will start us off with the terminating slug
        slug = t.delete_prefix vocab.to_s
        # warn [slug, pfx].inspect
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

      slug
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
    pfx = abbreviate(terms.to_a, noop: false,
                     sort: false, scalar: false).compact.map do |c|
      c = /^(?:([^:]+):)?/.match(c).captures.first
      c ? c.to_sym : c
    end.uniq.to_set

    # now we return the subset
    @prefixes.select { |k, _| pfx.include? k }
  end

  # Determine if a path ends with a slash (modulo path parameters).
  #
  # @param uri [#to_s] the URI path
  #
  # @return [false, true] whether the path ends with a slash.
  #
  def slash? uri
    uri = coerce_resource uri, as: :uri
    uri.respond_to?(:path) and /\/(?:;[^\/]*)?$/.match? uri.path
  end

  # Clean any dodginess (`//`, `.`, `..`) out of the path, including
  # path parameters. Unlike {Pathname#cleanpath} it preserves the
  # trailing slash if one is present. Returns either the cleaned
  # path or an array of segments. Returns nil (or empty array) if
  # the URI does not respond to `#path`.
  #
  # @param uri [URI, RDF::URI, #to_s] the URI
  # @param scalar [true, false] whether to return a string or array
  # @param slash [true, false] whether to preserve a trailing slash
  #
  # @return [String, Array<String>, nil] the cleaned path
  #
  def clean_path uri, scalar: true, slash: true
    uri = coerce_resource uri, as: :uri

    # bail out if this isn't the kind of uri that has a path
    return scalar ? nil : [] unless uri.respond_to? :path

    orig = uri.path

    ps = orig.split(/\/+/).map do |x|
      /^([^;]*)(?:;.*)?$/.match x
    end.compact.reduce([]) do |a, x|
      x = x.captures.first
      case x
      when ''   then nil
      when ?.   then nil
      when '..' then a.pop
      else a << x
      end
      a
    end

    return ps unless scalar

    path = ps.join ?/
    path << ?/ if slash and slash? orig
    path
  end

  # Test if a URI path contains a UUID in its first (and only)
  # segment and return it as a UUID URN.
  #
  # @param uri [URI, RDF::URI, #to_s] the URI
  # @param as [:rdf, :uri, false, nil] how to coerce the result
  #
  # @return [nil, URI, RDF::URI, String] the UUID, possibly coerced.
  #
  def uuid_path uri, as: :rdf
    uri = coerce_resource uri, as: :uri
    if uri.respond_to?(:path) and m = UUID_PATH.match(uri.path)
      uuid = m.captures.first.downcase

      if as
        uuid = "urn:uuid:#{uuid}"
        return coerce_resource uuid, as: as
      end

      return uuid
    end
    # nil
  end

  # XXX 2022-03-16 A BUNCH OF THIS STUFF WE SHOULD IGNORE I THINK

  # this thing needs its own souped-up struct_for because of
  # separation of concerns

  #
  def struct_for subject, graph: nil, only: nil, rev: false, inverses: false,
      uuids: false, canon: false, &block
    @repo.struct_for subject, graph: graph, only: only, rev: rev,
      inverses: inverses do |term|
        if term.iri?
          if uuids
            term = uuid_for term, verify: false, noop: true
          elsif canon
            # XXX revisit this, we may want to parametrize
            term = uri_for term
          end
        end
        block ? block.call(term) : term
      end
  end

  # Returns the "host document" of a given subject (or `nil` if the
  # subject is not a discernible fragment).
  #
  # @param subject [RDF::URI, URI] the subject to resolve
  # @param graph [RDF::URI, Array<RDF::URI>] named graph(s), if any
  # @param published [false, true, :circulated] whether to limit
  #  candidates to `published` resources
  # @param noop [false, true] returns `subject` if no host document found
  # @param as [:rdf, :uri] optional type coercion
  #
  # @return [URI, RDF::URI, nil] the host document, if any
  #
  def host_for subject, graph: nil, published: true, noop: false, as: :rdf
    subject = uuid_for subject, noop: true

    # XXX feed in our own fragment spec
    host = @repo.host_for subject, graph: graph, published: published,
      noop: noop, documents: @documents, fragments: @fragments

    # not sure if this is necessary
    host ||= subject if noop

    # XXX THIS NEEDS TO BE HERE
    return unless host

    coerce_resource host, as: as
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
    raise NotImplementedError
  end

  # Detect if something is a UUID.
  #
  # @param uuid [#to_s] the thing you think is a uuid
  #
  # @return [false, true]
  #
  def uuid? uuid
    UUID_ONLY.match? uuid.to_s
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

  # Get the canonical request URI from a {Rack::Request}.
  #
  # @param req [Rack::Request] a request object
  # @param as [:rdf, :uri, false, nil] how to coerce the result
  #
  # @return [URI, RDF::URI, nil] the request-URI
  #
  def request_uri req, as: :rdf
    rb = URI(preproc req.base_url)
    return unless authorities.include? rb.authority
    coerce_resource(base + req.env['REQUEST_URI'], as: as)
  end

  # Returns a stable fragment identifier for a given sequence of
  # terms. Passing in a base URI will return it with the fragment
  # appended. Setting `expand` to a true value will return the
  # fragment as a UUID URN. The `as` parameter coerces the output like
  # it does elsewhere in this module, but is ignored for the compact
  # (default) representation.
  #
  # The stable fragment identifier takes a space-concatenated string
  # of a list of RDF terms and returns a [compact
  # UUID](https://datatracker.ietf.org/doc/html/draft-taylor-uuid-ncname)
  # [version
  # 8](https://datatracker.ietf.org/doc/html/rfc9562#section-5.8)
  # consisting of the token `SF0-`, standing for "stable fragment
  # revision 0", followed by the first 96 bits of a SHA-256 hash of
  # the input string constructed from the RDF terms. The `expand`
  # parameter will return the equivalent UUID URN, or a `base` URI can
  # be passed where a copy will be returned with the fragment
  # appended.
  #
  # @param terms [RDF::Value] the RDF terms to hash
  # @param expand [false, true] whether to return a UUID URN
  # @param base [RDF::URI, URI] a base URI to which the fragment will
  #  be appended
  # @param as [:rdf, :uri, false, nil] how to coerce the result
  #
  # @return [String, RDF::URI, URI] the fragment (or URI with fragment)
  #
  def stable_fragment *terms, expand: false, base: nil, as: :rdf
    # first make sure these are the correct representatoin
    terms = terms.flatten.map { |t| coerce_resource(t).to_sxp }

    raise ArgumentError, "Term list cannot be empty" if terms.empty?

    # generate a string of terms separated by spaces
    input = terms.join ' '

    # generate a sha256 hash truncated to 96 bits
    hash = Digest::SHA256.digest(input).slice 0, 12

    # base64 encode that baby
    b64 = Base64.urlsafe_encode64 hash

    # I… is version 8 and …I is the variant 0b1000 (with the last two
    # bits masked); `SF0-` (48 5d 3e) stands for stable fragment rev 0
    # plus a hyphen because we're not animals
    frag = "ISF0-%sI" % b64

    if base
      base = coerce_resource(base, as: as).dup
      base.fragment = frag
      return base
    end

    if expand
      out = UUID::NCName.from_ncname frag, format: :urn
      return coerce_resource out, as: as
    end

    # XXX what if we want to change the radix? don't care for now
    frag
  end

  # Return a new random (v4) UUID URN.
  #
  # @return [RDF::URI, URI, String] the UUID URN
  #
  def random_uuid as: :rdf
    coerce_resource UUIDTools::UUID.random_create.to_uri, as: as
  end
end
