# -*- coding: utf-8 -*-
require 'intertwingler/version'

# basic stuff
require 'stringio'
require 'pathname'
require 'tempfile'
require 'yaml' # apparently no longer stock
require 'mimemagic'

# rdf stuff
require 'uri'
require 'uri/urn'
require 'cgi/util'
require 'rdf'
require 'rdf/turtle'
require 'rdf/vocab'
require 'rdf/reasoner'
require 'rdf/rdfa'
# this done broke lol: rdf-n3 -> sxp ~1.3 vs everything else -> sxp ~2.0
# require 'linkeddata'

# my stuff
require 'xml-mixup'
require 'md-noko'
require 'uuid-ncname'
require 'intertwingler/graphops'
require 'intertwingler/resolver'
require 'intertwingler/document'

require 'intertwingler/util'
require 'intertwingler/nlp'

# ontologies, mine in particular
require 'intertwingler/vocab/ci'
require 'intertwingler/vocab/ibis'
# others not included in rdf.rb
require 'intertwingler/vocab/pav'
require 'intertwingler/vocab/scovo'
require 'intertwingler/vocab/qb'
require 'intertwingler/vocab/adms'

# 2021-12-09 REFACTORING NOTES: This thing is an utter mess and needs
# a friggin overhaul. Here is the proposed new layout:
#
# * URI resolver: this handles all things URI, which also means CURIE
#   and prefix mapping and subsetting, and includes all the caches and
#   crap it needs to do its job.
#
# * Individual document resources: a unified interface for either
#   parsing and manipulating, or outright generating concrete
#   representations of resources, (X|HT)ML in particular.
#
#   * Specific generators include: Atom feeds, Google site maps, SKOS
#     concept schemes (and collections), indexes of external links,
#     qb:DataSet resources...
#
# * Storage drivers: Resolve URIs to actual files (or something else);
#   read and write in the usual fashion.
#
# * Main context for handling global configuration.
#
# * Command-line interface and shell.
#
# * Web (Rack) application.
#
# * General-purpose crawler and HTTP client (caching reverse proxy?).
#
# * Statistics and feature-gathering infrastructure (RDFa/JSON-LD/
#   microdata sponger, word counts, marked-up terminology, NLP...);
#   maybe some kind of unified interface onto this?
#
# * Other stuff that really belongs as an RDF::Repository subclass and
#   should(?) eventually get kicked up to RDF.rb (subject_for,
#   object_for, etc).

module Intertwingler

  # The context class parses the configuration and yokes together the
  # {Intertwingler::Resolver} (which itself depends on an {RDF::Repository}
  # augmented by {Intertwingler::GraphOps}), plus the {Intertwingler::Source}
  # and {Intertwingler::Surface} instances specified in the configuration.
  # We (potentially via a `Surface`) query {Intertwingler::Context} to get
  # {Intertwingler::Document} objects to be rendered to the surface.
  #
  # @note The {Intertwingler::Document} infrastructure is pending an
  #  overhaul to the {Intertwingler::Representation} and
  #  {Intertwingler::Transformation} pipeline.
  class Context
    include XML::Mixup
    include Util

    private

    CI = Intertwingler::Vocab::CI

    # RDF::Reasoner.apply(:rdfs, :owl)

    G_OK = [RDF::Repository, RDF::Dataset, RDF::Graph].freeze
    C_OK = [Pathname, IO, String].freeze

    def coerce_to_path_or_io obj
      return obj if obj.is_a? IO
      return obj.expand_path if obj.is_a? Pathname
      raise "#{obj.inspect} is not stringable" unless obj.respond_to? :to_s
      Pathname(obj.to_s).expand_path
    end

    def coerce_graph graph = nil, type: nil
      # begin with empty graph
      out = RDF::Repository.new

      return out unless graph
      return graph if G_OK.any? { |c| graph.is_a? c }

      # now turn into an array
      graph = [graph] unless graph.is_a? Array

      graph.each do |g|
        raise 'Graph must be some kind of RDF::Graph or RDF data file' unless
          C_OK.any? { |c| g.is_a? c } || g.respond_to?(:to_s)

        opts = {}
        opts[:content_type] = type if type

        if g.is_a? Pathname
          opts[:filename] = g.expand_path.to_s
          g = g.open
        elsif g.is_a? File
          opts[:filename] = g.path
        end

        g = StringIO.new(g.to_s) unless g.is_a? IO
        reader = RDF::Reader.for(opts) do
          g.rewind
          sample = g.read 1000
          g.rewind
          sample
        end or raise "Could not find an RDF::Reader for #{opts[:content_type]}"

        reader = reader.new g, **opts
        reader.each_statement do |stmt|
          out << stmt
        end
      end

      out
    end

    def normalize_hash h
      return h unless h.is_a? Hash
      out = {}
      h.each do |k, v|
        out[k.to_s.to_sym] = v.is_a?(Hash) ? normalize_hash(v) :
          v.respond_to?(:to_a) ? v.to_a.map { |x| normalize_hash x } : v
      end
      out
    end

    def coerce_config config
      # start by resolving paths against the working directory
      root = Pathname.getwd

      # config must either be a hash or a file name/pathname/io object
      unless config.respond_to? :to_h
        # when in rome
        require 'yaml'
        config = if config.is_a? IO
                   YAML.load config
                 else
                   config = Pathname.new(config).expand_path
                   # replace root with the path the config is found in
                   root = config.dirname
                   YAML.load_file config
                 end
      end

      config = normalize_hash config

      # finally, replace the root with whatever the root entry is
      # inside the config itself
      root = config[:root] = Pathname(config[:root]).expand_path(root) if
        config[:root]

      # config MUST have source and target dirs
      raise 'Config must have :source, :target, and :private directories' unless
        ([:source, :target, :private] - config.keys).empty?
      [:source, :target].each do |path|
        dir = config[path] = Pathname.new(config[path]).expand_path(root)
        raise "#{dir} is not a readable directory" unless
          dir.directory? && dir.readable?
      end
      raise "Target directory #{config[:target]} is not writable" unless
        config[:target].writable?
      raise "Source and target directories are the same: #{config[:source]}" if
        config[:source] == config[:target]

      # we try to create the private directory
      config[:private] = config[:target] + config[:private]
      if config[:private].exist?
        raise "#{config[:private]} is not a readable/writable directory" unless
          [:directory?, :readable?, :writable?].all? do |m|
          config[:private].send m
        end
      else
        config[:private].mkpath
      end

      # config MAY have graph location(s) but we can test this other
      # ways, same goes for base URI
      if config[:graph]
        g = config[:graph]
        g = [g] unless g.is_a? Array
        config[:graph] = g.map { |x| Pathname(x).expand_path root }
      end

      # deal with prefix map
      config[:prefixes] =
        Intertwingler::Util::Clean.sanitize_prefixes config[:prefixes] if
        config[:prefixes]

      # XXX 2022-03-17 gotta get rid of or otherwise resolve all these curie
      # resolution thingies (or ultimately just come up with a better
      # strategy for config)

      # deal with duplicate map
      pfx = config[:prefixes] || {}
      if dups = config[:duplicate]
        base = Intertwingler::Resolver.coerce_resource config[:base], as: :uri
        if dups.is_a? Hash
          config[:duplicate] = dups.map do |ruri, preds|
            preds = [preds] unless preds.is_a? Array
            preds.map! do |p|
              Intertwingler::Resolver.resolve_curie p, prefixes: pfx
            end
            [RDF::URI((base + ruri.to_s).to_s), Set.new(preds)]
          end.to_h
        end
      end

      # rewrite maps
      config[:maps] = {} unless config[:maps].is_a? Hash
      %w(rewrite redirect gone).each do |type|
        config[:maps][type.to_sym] ||= ".#{type}.map"
      end

      # "document" maps, ie things that are never fragments
      config[:documents] = [] unless config[:documents].is_a? Array
      config[:documents].map! do |d|
        Intertwingler::Resolver.resolve_curie d, prefixes: pfx
      end.compact
      config[:documents] << RDF::Vocab::FOAF.Document if
        config[:documents].empty?

      # fragment maps
      config[:fragment] = {} unless config[:fragment].is_a? Hash
      config[:fragment] = config[:fragment].map do |frag, paths|
        paths = (paths.respond_to?(:to_a) ? paths.to_a : [paths]).map do |path|
          Intertwingler::GraphOps.parse_property_path path, pfx
        end
        [[Intertwingler::Resolver.resolve_curie(frag.to_s, prefixes: pfx)], paths]
      end

      config
    end

    public

    attr_reader :config, :graph, :resolver

    alias_method :repo, :graph

    # Initialize a context.
    #
    # @param graph
    # @param base
    # @param config
    # @param type
    #
    # @return [Intertwingler::Context] the new context object.

    def initialize graph: nil, base: nil, config: nil, type: nil
      # RDF::Reasoner.apply(:rdfs, :owl)

      @config = coerce_config config

      graph ||= @config[:graph] if @config[:graph]
      base  ||= @config[:base]  if @config[:base]

      # warn base.inspect

      @graph = coerce_graph graph, type: type
      @graph.document_types = @config[:documents]
      @graph.fragment_spec  = @config[:fragment]

      @resolver = Intertwingler::Resolver.new @graph, base,
        prefixes: @config[:prefixes], aliases: @config[:aliases],
        documents: @graph.document_types, fragments: @graph.fragment_spec

      @ucache = RDF::Util::Cache.new(-1)
      @scache = {} # wtf rdf util cache doesn't like booleans
    end

    # Get the base URI.
    #
    # @return [RDF::URI] the base URI.
    #
    def base
      @resolver.base
    end

    # Get the prefix mappings from the configuration.
    #
    # @return [Hash{Symbol=>RDF::Vocabulary}] The prefixes
    #
    def prefixes
      @resolver.prefixes
    end

    # XXX KILL AFTER THIS LINE TO NEXT MARKER

    # Abbreviate a set of terms against the registered namespace
    # prefixes and optional default vocabulary, or otherwise return a
    # string representation of the original URI.

    # @param term [RDF::Term]
    # @param prefixes [Hash]
    #
    # @return [String]
    #
    def abbreviate term, **args
      @resolver.abbreviate term, **args
    end

    # Obtain a key-value structure for the given subject, optionally
    # constraining the result by node type (:resource, :uri/:iri,
    # :blank/:bnode, :literal)
    #
    # @param subject of the inquiry
    # @param rev map in reverse
    # @param only one or more node types
    # @param uuids coerce resources to if possible
    #
    # @return [Hash]
    #
    def struct_for subject, **args
      # note that this is the resolver one not the repo one
      @resolver.struct_for subject, **args
    end

    # Obtain everything in the graph that is an `rdf:type` of something.
    #
    # @return [Array]
    #
    def all_types
      @graph.all_types
    end

    # Obtain every subject that is rdf:type the given type or its subtypes.
    #
    # @param rdftype [RDF::Term, #to_a]
    #
    # @return [Array]
    #
    def all_of_type rdftype, exclude: []
      @graph.all_of_type rdftype, exclude: exclude
    end

    # Obtain all and only the rdf:types directly asserted on the subject.
    #
    # @param subject [RDF::Resource]
    # @param type [RDF::Term, :to_a]
    #
    # @return [Array]
    #
    def asserted_types subject, struct: nil
      @graph.types_for subject, struct: struct
    end

    # Determine whether a subject is a given `rdf:type`.
    #
    # @param repo [RDF::Queryable] the repository
    # @param subject [RDF::Resource] the resource to test
    # @param type [RDF::Term] the type to test the subject against
    # @param struct [Hash] an optional predicate-object set
    # @return [true, false]
    #
    def rdf_type? subject, type, struct: nil
      @graph.rdf_type? subject, type, struct: struct
    end

    # Returns subjects from the graph with entailment.
    #
    # @param predicate
    # @param object
    # @param entail
    # @param only
    #
    # @return [RDF::Resource]
    #
    def subjects_for predicate, object, entail: true, only: [], &block
      @graph.subjects_for predicate, object, entail: entail, only: only, &block
    end

    # Returns objects from the graph with entailment.
    #
    # @param subject
    # @param predicate
    # @param entail
    # @param only
    # @param datatype
    #
    # @return [RDF::Term]
    #
    def objects_for subject, predicate,
        entail: true, only: [], datatype: nil, repo: @graph, &block
      @graph.objects_for subject, predicate, entail: entail, only: only,
        datatype: datatype, &block
    end

    # Find the terminal replacements for the given subject, if any exist.
    #
    # @param subject
    # @param published indicate the context is published
    #
    # @return [Set]
    #
    def replacements_for subject, published: true
      @graph.replacements_for subject, published: published
    end

    # Obtain dates for the subject as instances of Date(Time). This is
    # just shorthand for a common application of `objects_for`.
    #
    # @param subject
    # @param predicate
    # @param datatype
    #
    # @return [Array] of dates
    def dates_for subject, predicate: RDF::Vocab::DC.date,
        datatype: [RDF::XSD.date, RDF::XSD.dateTime]
      @graph.dates_for subject, predicate: predicate, datatype: datatype
    end

    # Obtain any specified MIME types for the subject. Just shorthand
    # for a common application of `objects_for`.
    #
    # @param subject
    # @param predicate
    # @param datatype
    #
    # @return [Array] of internet media types
    #
    def formats_for subject, predicate: RDF::Vocab::DC.format,
        datatype: [RDF::XSD.token]
      @graph.formats_for subject, predicate: predicate, datatype: datatype
    end

    # Assuming the subject is a thing that has authors, return the
    # list of authors. Try bibo:authorList first for an explicit
    # ordering, then continue to the various other predicates.
    #
    # @param subject [RDF::Resource]
    # @param unique  [false, true] only return the first author
    # @param contrib [false, true] return contributors instead of authors
    #
    # @return [RDF::Value, Array]
    #
    def authors_for subject, unique: false, contrib: false
      @graph.authors_for subject, unique: unique, contrib: contrib
    end

    # Obtain the most appropriate label(s) for the subject's type(s).
    # Returns one or more (depending on the `unique` flag)
    # predicate-object pairs in order of preference.
    #
    # @param subject [RDF::Resource]
    # @param unique [true, false] only return the first pair
    # @param type [RDF::Term, Array] supply asserted types if already retrieved
    # @param lang [nil] not currently implemented (will be conneg)
    # @param desc [false, true] retrieve description instead of label
    # @param alt  [false, true] retrieve alternate instead of main
    #
    # @return [Array] either a predicate-object pair or an array of pairs.
    #
    def label_for subject, candidates: nil, unique: true, type: nil,
        lang: nil, desc: false, alt: false
      @graph.label_for subject,
        unique: unique, lang: lang, desc: desc, alt: alt, struct: candidates
    end

    # holy cow this is actually a lot of stuff:

    # turn markdown into xhtml (via md-noko)

    # turn html into xhtml (trivial)

    # generate triples from ordinary (x)html structure

    # map vanilla (x)html metadata to existing graph (ie to get resource URIs)

    # pull triples from rdfa

    # stuff rdfa into rdfa-less xhtml

    # basic nlp detection of terms + text-level markup (dfn, abbr...)

    # markdown round-tripping (may as well store source in md if possible)

    # add title attribute to all links

    # add alt attribute to all images

    # segmentation of composite documents into multiple files

    # aggregation of simple documents into composites

    # generate backlinks

    # - resource (ie file) generation -

    # generate indexes of people, groups, and organizations

    # generate indexes of books, not-books, and other external links

    # KILL OK
    def head_links subject, struct: nil, nodes: nil, prefixes: {},
        ignore: [], uris: {}, labels: {}, vocab: nil, rev: []

      raise 'ignore must be Array or Set' unless
        [Array, Set].any? { |c| ignore.is_a? c }

      rev = rev.respond_to?(:to_a) ? rev.to_a : [rev]

      struct ||= @graph.struct_for subject
      nodes  ||= @graph.invert_struct struct

      # XXX is this smart?
      revs = {}
      @graph.subjects_for(rev, subject, only: :resource) do |s, ps|
          revs[s] ||= Set.new
          revs[s] |= ps
      end

      # make sure these are actually URI objects not RDF::URI
      uris = uris.transform_values { |v| URI(@resolver.preproc v.to_s) }
      uri  = uris[subject] || @resolver.uri_for(subject, as: :uri, slugs: true)

      # make ignore more robust
      ignore = ignore.uniq.select(&:iri?).map do |i|
        [i, @resolver.uuid_for(i, noop: true, verify: false)]
      end.flatten.to_set

      # warn ignore.sort.inspect

      # output
      links = []

      { false => nodes, true => revs }.each do |reversed, obj|
        obj.reject { |n, _| ignore.include?(n) || !n.uri? }.each do |k, v|
          # first nuke rdf:type, that's never in there
          v = v.dup.delete RDF.type unless reversed
          next if v.empty?

          unless uris[k]
            cu = @resolver.uri_for k, slugs: true
            uris[k] = URI(@resolver.preproc(cu || k.to_s))
          end

          # munge the url and make the tag
          ru  = uri.route_to(uris[k])
          ln  = { nil => :link, href: ru.to_s }
          ln[reversed ? :rev : :rel] = @resolver.abbreviate v.to_a,
            scalar: false, vocab: vocab

          # add the title
          if lab = labels[k]
            ln[:title] = lab[1].to_s
          end

          # add type attribute
          unless (mts = formats_for k).empty?
            ln[:type] = mts.first.to_s

            if ln[:type] =~ /(java|ecma)script/i ||
                !(v.to_set & Set[RDF::Vocab::DC.requires]).empty?
              ln[:src] = ln.delete :href
              # make sure we pass in an empty string so there is a closing tag
              ln.delete nil
              ln[['']] = :script
            end
          end

          # warn ln.inspect

          # finally add the link
          links << ln
        end
      end

      links.sort! do |a, b|
        # sort by rel, then by href
        # warn a.inspect, b.inspect
        s = 0
        [nil, :rel, :rev, :href, :title].each do |k|
          s = a.fetch(k, '').to_s <=> b.fetch(k, '').to_s
          break if s != 0
        end
        s
      end

      links
    end

    # KILL OK
    def head_meta subject, struct: nil, nodes: nil, prefixes: {},
        ignore: [], meta_names: {}, vocab: nil, lang: nil, xhtml: true

      raise 'ignore must be Array or Set' unless
        [Array, Set].any? { |c| ignore.is_a? c }

      struct ||= @graph.struct_for subject
      nodes  ||= @graph.invert_struct struct

      ignore = ignore.to_set

      meta = []
      nodes.select { |n| n.literal? && !ignore.include?(n) }.each do |k, v|
        rel  = @resolver.abbreviate v.to_a, vocab: vocab
        tag  = { nil => :meta, property: rel, content: k.to_s }

        lang = (k.language? && k.language != lang ? k.language : nil) ||
          (k.datatype == RDF::XSD.string && lang ? '' : nil)
        if lang
          tag['xml:lang'] = lang if xhtml
          tag[:lang] = lang
        end

        tag[:datatype] =
          @resolver.abbreviate k.datatype, vocab: XHV if k.datatype?
        tag[:name] = meta_names[k] if meta_names[k]

        meta << tag
      end

      meta.sort! do |a, b|
        s = 0
        [:about, :property, :datatype, :content, :name].each do |k|
          # warn a.inspect, b.inspect
          s = a.fetch(k, '').to_s <=> b.fetch(k, '').to_s
          break if s != 0
        end
        s
      end

      meta
    end

    # KILL OK
    def generate_backlinks subject, published: true, ignore: nil
      uri = @resolver.uri_for(
        subject, as: :uri, slugs: true) || URI(@resolver.preproc subject)

      ignore ||= Set.new
      raise 'ignore must be amenable to a set' unless ignore.respond_to? :to_set
      ignore = ignore.to_set
      nodes  = {}
      labels = {}
      types  = {}
      @graph.query([nil, nil, subject]).each do |stmt|
        next if ignore.include?(sj = stmt.subject)
        preds = nodes[sj] ||= Set.new
        preds << (pr = stmt.predicate)
        types[sj]  ||= @graph.asserted_types sj
        labels[sj] ||= @graph.label_for sj
        labels[pr] ||= @graph.label_for pr
      end

      # prune out
      nodes.select! { |k, _| @graph.published? k } if published

      return if nodes.empty?

      # the block gets propagated to the comparands
      lcmp = @graph.cmp_label &:first

      li = nodes.sort(&lcmp).map do |rsrc, preds|
        cu  = @resolver.uri_for(rsrc, as: :uri) or next
        lab = labels[rsrc] || [nil, rsrc]
        lp  = @resolver.abbreviate(lab.first) if lab.first
        ty  = @resolver.abbreviate(types[rsrc]) if types[rsrc]

        { [{ [{ [lab[1].to_s] => :span, property: lp }] => :a,
          href: uri.route_to(cu), typeof: ty,
          rev: @resolver.abbreviate(preds) }] => :li }
      end.compact

      { [{ li => :ul }] => :nav }
    end

    # KILL OK
    def generate_twitter_meta subject
      Intertwingler::Document.twitter_meta @resolver, subject
    end

    # generate skos concept schemes

    def generate_audience_csv file = nil, published: true

      require 'csv'
      file = coerce_to_path_or_io file if file
      lab = {}

      @concepts ||= @graph.all_related(RDF::Vocab::SKOS.Concept).to_set

      out = @graph.all_documents(published: published, external: false,
                                 exclude: RDF::Vocab::FOAF.Image).map do |s|
        u = @resolver.uri_for s
        x = @graph.struct_for s
        c = x[RDF::Vocab::DC.created] ? x[RDF::Vocab::DC.created][0] : nil
        _, t = @graph.label_for s, struct: x
        _, d = @graph.label_for s, struct: x, desc: true

        # # audience(s)
        # a = objects_for(s, RDF::Vocab::DC.audience).map do |au|
        #   next lab[au] if lab[au]
        #   _, al = label_for au
        #   lab[au] = al
        # end.map(&:to_s).sort.join '; '

        # # explicit non-audience(s)
        # n = objects_for(s, Intertwingler::Vocab::CI['non-audience']).map do |au|
        #   next lab[au] if lab[au]
        #   _, al = label_for au
        #   lab[au] = al
        # end.map(&:to_s).sort.join '; '

        # audience and non-audience
        a, n = [RDF::Vocab::DC.audience, CI['non-audience']].map do |ap|
          @graph.objects_for(s, ap).map do |au|
            next lab[au] if lab[au]
            _, al = label_for au
            lab[au] = al
          end.map(&:to_s).sort.join '; '
        end

        # concepts???
        concepts = [RDF::Vocab::DC.subject, CI.introduces,
                    CI.assumes, CI.mentions].map do |pred|
          @graph.objects_for(s, pred, only: :resource).map do |o|
            con = @graph.objects_for(o, RDF.type).to_set & @concepts
            next if con.empty?
            next lab[o] if lab[o]
            _, ol = label_for o
            lab[o] = ol
          end.compact.map(&:to_s).sort.join '; '
        end

        [s, u, c, t, d, a, n].map(&:to_s) + concepts
      end.sort { |a, b| a[2] <=> b[2] }

      out.unshift ['ID', 'URL', 'Created', 'Title', 'Description', 'Audience',
        'Non-Audience', 'Subject', 'Introduces', 'Assumes', 'Mentions']

      if file
        # don't open until now
        file = file.expand_path.open('wb') unless file.is_a? IO

        csv = CSV.new file
        out.each { |x| csv << x }
        file.flush
      end

      out
    end

    CSV_PRED = {
      audience:    RDF::Vocab::DC.audience,
      nonaudience: CI['non-audience'],
      subject:     RDF::Vocab::DC.subject,
      assumes:     CI.assumes,
      introduces:  CI.introduces,
      mentions:    CI.mentions,
    }

    def ingest_csv file
      file = coerce_to_path_or_io file

      require 'csv'

      # key mapper
      km = { uuid: :id, url: :uri }
      kt = -> (k) { km[k] || k }

      # grab all the concepts and audiences

      audiences = {}
      all_of_type(CI.Audience).map do |c|
        s = struct_for c

        # homogenize the labels
        lab = [false, true].map do |b|
          @graph.label_for(c, struct: s, unique: false, alt: b).map { |x| x[1] }
        end.flatten.map { |x| x.to_s.strip.downcase }

        # we want all the keys to share the same set
        set = nil
        lab.each { |t| set = audiences[t] ||= set || Set.new }
        set << c
      end

      concepts = {}
      all_of_type(RDF::Vocab::SKOS.Concept).map do |c|
        s = struct_for c

        # homogenize the labels
        lab = [false, true].map do |b|
          @graph.label_for(c, struct: s, unique: false, alt: b).map { |x| x[1] }
        end.flatten.map { |x| x.to_s.strip.downcase }

        # we want all the keys to share the same set
        set = nil
        lab.each { |t| set = concepts[t] ||= set || Set.new }
        set << c
      end

      data = CSV.read(file, headers: true,
                      header_converters: :symbol).map do |o|
        o = o.to_h.transform_keys(&kt)
        s = @resolver.uuid_for(o.delete :id) or next

        # LOLOL wtf

        # handle audience
        [:audience, :nonaudience].each do |a|
          if o[a]
            o[a] = o[a].strip.split(/\s*[;,]+\s*/, -1).map do |t|
              if t =~ /^[a-z+-]+:[^[:space:]]+$/
                u = RDF::URI(t)
                @resolver.uuid_for(u) || u
              elsif audiences[t.downcase]
                audiences[t.downcase].to_a
              end
            end.flatten.compact.uniq
          else
            o[a] = []
          end
        end

        # handle concepts
        [:subject, :introduces, :assumes, :mentions].each do |a|
          if o[a]
            o[a] = o[a].strip.split(/\s*[;,]+\s*/, -1).map do |t|
              if t =~ /^[a-z+-]+:[^[:space:]]+$/
                u = RDF::URI(t)
                @resolver.uuid_for(u) || u
              elsif concepts[t.downcase]
                concepts[t.downcase].to_a
              end
            end.flatten.compact.uniq
          else
            o[a] = []
          end

        end

        CSV_PRED.each do |sym, pred|
          o[sym].each do |obj|
            @graph << [s, pred, obj]
          end
        end

        [s, o]
      end.compact.to_h
      data
    end

    # KILL OK
    def generate_sitemap published: true
      urls = {}

      # do feeds separately
      feeds = all_of_type RDF::Vocab::DCAT.Distribution
      #feeds.select! { |f| published? f } if published
      feeds.each do |f|
        uri = @resolver.uri_for(f)
        f = generate_atom_feed f, published: published, related: feeds
        mt = f.at_xpath('/atom:feed/atom:updated[1]/text()',
          { atom: 'http://www.w3.org/2005/Atom' })
        urls[uri] = { [{ [uri.to_s] => :loc }, { [mt] => :lastmod }] => :url }
      end

      # build up hash of urls
      all_internal_docs(published: published).each do |doc|
        next if asserted_types(doc).include? RDF::Vocab::FOAF.Image
        uri  = @resolver.uri_for(doc)
        next unless uri.authority && @base && uri.authority == base.authority
        mods = objects_for(doc, [RDF::Vocab::DC.created,
          RDF::Vocab::DC.modified, RDF::Vocab::DC.issued],
          datatype: RDF::XSD.dateTime).sort
        nodes = [{ [uri.to_s] => :loc }]
        nodes << { [mods[-1].to_s] => :lastmod } unless mods.empty?
        urls[uri] = { nodes => :url }
      end

      urls = urls.sort.map { |_, v| v }

      markup(spec: { urls => :urlset,
        xmlns: 'http://www.sitemaps.org/schemas/sitemap/0.9' }).document
    end

    def write_sitemap published: true
      sitemap = generate_sitemap published: published
      file    = @config[:sitemap] || '.well-known/sitemap.xml'
      target  = @config[published ? :target : :private]
      target.mkpath unless target.directory?

      fn = target + file
      fn.dirname.mkpath unless fn.dirname.directory?

      fh = fn.open(?w)
      sitemap.write_to fh
      fh.close
    end

    def reload_graph
      resolver.flush
      graph.clear
      graph.flush_cache

      @config[:graph].each { |f| graph.load f }

      graph
    end

    def write_turtle file
      file = Pathname(file).expand_path.open('wb') unless file.is_a? IO
      # warn file
      RDF::Writer.for(:turtle).open(file, prefixes: resolver.prefixes) do |w|
        # warn graph.size
        w << graph
      end
      file.close unless file.closed?
    end

    # generate atom feed

    # KILL OK
    def all_internal_docs published: true, exclude: []
      # find all UUIDs that are documents
      docs = all_of_type(RDF::Vocab::FOAF.Document,
        exclude: exclude).select { |x| x =~ /^urn:uuid:/ }

      # prune out all but the published documents if specified
      if published
        p = RDF::Vocab::BIBO.status
        o = RDF::Vocabulary.find_term(
          'http://purl.org/ontology/bibo/status/published')
        docs = docs.select do |s|
          @graph.has_statement? RDF::Statement(s, p, o)
        end
      end

      docs
    end

    # KILL OK
    def indexed? subject
      subject = @resolver.coerce_resource subject
      indexed = objects_for subject, Intertwingler::Vocab::CI.indexed,
        only: :literal, datatype: RDF::XSD.boolean
      # assume the subject is indexed but we are looking for explicit
      # presence of `false` (ie explicit `false` overrides explicit
      # `true`). values like ci:indexed -> "potato" are ignored.
      indexed.empty? or indexed.none? { |f| f.object == false }
    end

    # KILL OK
    def generate_atom_feed id, published: true, related: []
      raise 'ID must be a resource' unless id.is_a? RDF::Resource
      Intertwingler::Document::Feed.new(@resolver, id, published: published)
    end

    # MOVE TO Surface::DocumentRoot
    def write_feeds type: RDF::Vocab::DCAT.Distribution, published: true
      feeds = all_of_type type
      target = @config[published ? :target : :private]
      feeds.each do |feed|
        tu  = URI(feed.to_s)
        doc = generate_atom_feed feed, published: published, related: feeds
        fh  = (target + "#{tu.uuid}.xml").open('w')
        doc.write_to fh
        fh.close
      end
    end

    # generate sass palettes

    # MOVE TO Surface::DocumentRoot

    # generate rewrite map(s)
    def generate_rewrite_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, published: published
      base = @resolver.base
      umap = {}
      rwm  = {}

      docs.each do |doc|
        next unless doc.uri?
        tu = umap[doc] = URI(doc.to_s)
        next unless tu.respond_to?(:uuid)

        @resolver.uri_for(doc, scalar: false, slugs: true).each do |cu|
          # must be http(s)
          next unless cu.scheme.to_s.downcase.start_with? 'http'

          # skip fragments
          next if cu.fragment

          # skip external links obvs
          # XXX THIS DOES NOT WORK
          # next unless base.route_to(cu).relative?

          # this should though
          next unless cu.host == base.host

          # skip /uuid form
          cp = cu.request_uri.delete_prefix '/'
          next if tu.uuid == cp

          # XXX
          # next if @graph.replacements_for(doc, published: published).empty?

          (rwm[cp] ||= []) << doc
        end
      end

      # ranking hash
      r = {}

      pslug = @config[:private].basename

      rwm.transform_values do |uuids|
        uuid = if uuids.size == 1
                 umap[uuids.first]
               else
                 # otherwise we need to figure out what the right one is
                 uuids.each do |u|
                   r[u] ||= @graph.ranking_data_for u, ints: true
                 end

                 u = uuids.sort do |a, b|
                   c = r[a][:replaced]   <=> r[b][:replaced]
                   c = r[a][:retired]    <=> r[b][:retired]    if c == 0
                   c = r[b][:published]  <=> r[a][:published]  if c == 0
                   c = r[b][:circulated] <=> r[a][:circulated] if c == 0
                   c = r[b][:mtime]      <=> r[a][:mtime]      if c == 0
                   c = r[b][:ctime]      <=> r[a][:ctime]      if c == 0

                   # okay this is us giving up
                   c = a.to_s <=> b.to_s if c == 0

                   c # finally
                 end.first

                 umap[u]
               end

        # note this is a pathname
        ((@graph.published?(RDF::URI(uuid.to_s)) ? '' : pslug) + uuid.uuid).to_s
      end
    end

    # give me all UUIDs of all documents, filter for published if
    # applicable
    #
    # find the "best" (relative) URL for the UUID and map the pair
    # together
    def generate_uuid_redirect_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, fragments: true,
        published: published
      base = @resolver.base

      # keys are /uuid, values are
      out = {}
      docs.each do |doc|
        next unless doc.uri?
        tu = URI(doc.to_s)
        cu = @resolver.uri_for doc, as: :uri
        next unless tu.respond_to?(:uuid) and cu.respond_to?(:request_uri)
        next if tu.fragment

        # skip /uuid form
        cp = cu.request_uri.delete_prefix '/'
        next if cu.host == base.host && tu.uuid == cp

        # all redirect links are absolute
        out[tu.uuid] = cu.to_s
      end
      out
    end

    # find all URIs/slugs that are *not* canonical, map them to slugs
    # that *are* canonical
    def generate_slug_redirect_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, fragments: true,
        published: published
      base = @resolver.base

      # for redirects we collect all the docs, plus all their URIs,
      # separate canonical from the rest

      # actually an easy way to do this is just harvest all the
      # multi-addressed docs, remove the first one, then ask for the
      # canonical uuid back,

      fwd = {}
      rev = {}
      out = {}

      docs.each do |doc|
        next unless doc.uri?
        uris  = @resolver.uri_for doc, scalar: false, slugs: true, as: :uri
        canon = uris.shift
        next unless canon.respond_to? :request_uri
        # XXX not sure i want to do this
        # next if canon.fragment

        # cache the forward direction
        fwd[URI(doc.to_s)] = canon

        # warn uris.inspect

        uris.each do |uri|
          next unless uri.respond_to? :request_uri
          next if canon == uri
          next unless base.route_to(uri).relative?
          next if uri.fragment

          # warn "#{canon} <=> #{uri}"

          requri = uri.request_uri.delete_prefix '/'
          next if requri.empty?
          # XXX why would i want to eliminate UUIDs?
          # next if requri == '' ||
          #  requri =~ /^[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$/

          # cache the reverse direction
          rev[uri] = requri
        end
      end

      # warn fwd, rev

      rev.each do |uri, requri|
        doc = @resolver.uuid_for uri, published: published, as: :uri
        if doc and fwd[doc] and fwd[doc] != uri
          # warn "#{doc.inspect} -> #{fwd[doc]} (ex #{uri})"
          out[requri] = fwd[doc].to_s
        end
      end

      out
    end

    # you know what, it's entirely possible that these ought never be
    # called individually and the work to get one would duplicate the
    # work of getting the other, so maybe just do 'em both at once

    def generate_redirect_map published: false, docs: nil
      generate_uuid_redirect_map(published: published, docs: docs).merge(
        generate_slug_redirect_map(published: published, docs: docs))
    end

    def generate_gone_map published: false, docs: nil
      # published is a no-op for this one because these docs are by
      # definition not published
      docs ||= @graph.reachable published: false
      p    = RDF::Vocab::BIBO.status
      base = @resolver.base
      out  = {}
      docs.select { |s|
        @graph.has_statement? RDF::Statement(s, p, CI.retired) }.each do |doc|
        canon = @resolver.uri_for doc, as: :uri
        next unless base.route_to(canon).relative?
        canon = canon.request_uri.delete_prefix '/'
        # value of the gone map doesn't matter
        out[canon] = canon
      end

      out
    end

    # private?

    def map_location type
      # find file name in config
      fn = @config[:maps][type] or return

      # concatenate to target directory
      @config[:target] + fn
    end

    # private?

    def write_map_file location, data
      # open file
      File.open(location, ?w) do |fh|
        data.sort.each { |k, v| fh.write "#{k}\t#{v}\n" }
      end # return value is return value from close
    end

    # public again

    def write_rewrite_map published: false, docs: nil
      data = generate_rewrite_map published: published, docs: docs
      loc  = map_location :rewrite
      write_map_file loc, data
    end

    def write_redirect_map published: false, docs: nil
      data = generate_redirect_map published: published, docs: docs
      loc  = map_location :redirect
      write_map_file loc, data
    end

    def write_gone_map published: false, docs: nil
      data = generate_gone_map published: published, docs: docs
      loc  = map_location :gone
      write_map_file loc, data
    end

    def write_maps published: true, docs: nil
      docs ||= @graph.all_documents external: false,
        published: false, fragments: true
      # slug to uuid (internal)
      write_rewrite_map docs: docs
      # uuid/slug to canonical slug (308)
      write_redirect_map docs: docs
      # retired slugs/uuids (410)
      write_gone_map docs: docs
      true
    end

    # MOVE TO Surface::DocumentRoot
    def generate_xml_catalog published: false, docs: nil, path: ?., extra: {}
      path = Pathname(path)

      # make a bunch of mappings
      data = generate_rewrite_map(
        published: published, docs: docs
      ).merge(extra).map do |uri, file|
        uri = self.base + uri
        file = Pathname(file)
        file = Pathname("#{file}.xml") if file.extname.empty?
        file = path + file if file == file.basename
        { nil => :uri, name: uri, uri: file }
      end

      # set up the markup spec
      spec = [
        { '#doctype' => ['catalog', "-//OASIS//DTD XML Catalogs V1.0//EN" ] },
        { '#catalog' => data,
         xmlns: 'urn:oasis:names:tc:entity:xmlns:xml:catalog' }
      ]

      # out the door
      markup(spec: spec).document
    end

    # whoops lol we forgot the book list

    def reading_lists published: true
      out = all_of_type RDF::Vocab::SiocTypes.ReadingList
      return out unless published
      out.select { |r| published? r }
    end

    def add_xslt doc
      # gotta do this because the generator separates these out now
      if xf = @config[:transform]
        dtd = doc.children.first
        doc = markup(
          spec: { '#pi' => 'xml-stylesheet', type: 'text/xsl', href: xf },
          before: dtd).document
      end

      doc
    end

    def generate_reading_list subject, published: true
      add_xslt Intertwingler::Document::ReadingList.new(
        resolver, subject, published: published, backlinks: true).doc
    end

    def write_reading_lists published: true
      reading_lists(published: published).each do |rl|
        tu  = URI(rl.to_s)
        states = [true]
        states << false unless published
        states.each do | state|
          doc = generate_reading_list rl, published: state
          dir = @config[state ? :target : :private]
          (dir + "#{tu.uuid}.xml").open('wb') { |fh| doc.write_to fh }
        end
      end
    end

    def generate_stats published: true
      all_of_type(QB.DataSet).map do |s|
        [s, add_xslt(Intertwingler::Document::Stats.new(
          @resolver, s, published: published))]
      end.to_h
    end

    def write_stats published: true
      target = @config[published ? :target : :private]
      target.mkpath unless target.directory?
      generate_stats(published: published).each do |uu, doc|
        bn = URI(uu.to_s).uuid + '.xml'
        fh = (target + bn).open ?w
        doc.write_to fh
        fh.flush
        fh.close
      end
    end

    # XXX OKAY THESE ARE HELLA TEMPORARY; THEIR LAYOUTS SHOULD REALLY
    # BE GOVERNED BY LOUPE OR SOMETHING

    def generate_address_book subject, published: true
      add_xslt Intertwingler::Document::AddressBook.new(
        resolver, subject, published: published, backlinks: true).doc
    end

    def write_address_book subject, published: true
      write_any_index subject, published: published do
        generate_address_book subject, published: published
      end
    end

    def write_address_books published: true
      all_of_type(RDF::Vocab::SiocTypes.AddressBook).each do |a|
        write_address_book a, published: published
      end
    end

    # This will generate an (X)HTML+RDFa page containing either a
    # SKOS concept scheme or a collection, ordered or otherwise.
    #
    # XXX later on we should consider conneg for languages
    #
    #
    def generate_concept_scheme subject, published: true, fragment: false
      # stick some logic here to sort out what kind of thing it is
      # (concept scheme, collection, ordered collection)
      add_xslt Intertwingler::Document::ConceptScheme.new(
        resolver, subject, published: published, backlinks: true).doc
    end

    def write_any_index subject, published: true, &block
      return if published and !@graph.published?(subject)
      subject = @resolver.uuid_for(subject) or return

      uuid = URI(subject.to_s)
      states = [true]
      states << false unless published
      states.each do |state|
        doc = block.call subject, published: state
        dir = @config[state ? :target : :private]
        dir.mkdir unless dir.exist?
        (dir + "#{uuid.uuid}.xml").open('wb') { |fh| doc.write_to fh }
      end
    end

    def write_concept_scheme subject, published: true
      return if published and !@graph.published?(subject)
      subject = @resolver.uuid_for(subject) or return

      uuid = URI(subject.to_s)
      states = [true]
      states << false unless published
      states.each do |state|
        doc = generate_concept_scheme subject, published: state
        dir = @config[state ? :target : :private]
        dir.mkdir unless dir.exist?
        (dir + "#{uuid.uuid}.xml").open('wb') { |fh| doc.write_to fh }
      end
    end

    def write_concept_schemes published: true
      # XXX i should really standardize these `write_whatever` thingies
      schemes = [RDF::Vocab::SKOS.ConceptScheme, RDF::Vocab::SKOS.Collection]
      all_of_type(schemes).each do |list|
        write_concept_scheme list, published: published
      end
    end

    # - io stuff -

    # Locate the file in the source directory associated with the given URI.
    #
    # @param [RDF::URI, URI, :to_s] the URI requested
    #
    # @return [Pathname] of the corresponding file or nil if no file was found.
    #
    def locate uri
      uri  = @resolver.coerce_resource uri
      base = @resolver.base

      tu = URI(uri.to_s) # copy of uri for testing content
      unless tu.scheme == 'urn' and tu.nid == 'uuid'
        raise "could not find UUID for #{uri}" unless
          uuid = @resolver.uuid_for(uri)
        tu = URI(uri = uuid)
      end

      # xxx bail if the uri isn't a subject in the graph

      candidates = [@config[:source] + tu.uuid]

      # try all canonical URIs
      (@resolver.uri_for uri, scalar: false, slugs: true).each do |u|
        # warn u.inspect
        u = URI(u.to_s)
        # warn "#{u.hostname} #{base.hostname}".inspect
        next unless u.hostname == base.hostname
        p = CGI.unescape u.path[/^\/*(.*?)$/, 1]
        candidates.push(@config[:source] + p)
      end

      # warn "candidates: #{candidates.inspect}"

      files = candidates.uniq.map do |c|
        Pathname.glob(c.to_s + '{,.*,/index{,.*}}')
      end.reduce(:+).reject do |x|
        x.directory? or
          /.*(?:markdown|(?:x?ht|x)ml).*/i !~ MimeMagic.by_path(x).to_s
      end.uniq

      # warn files.inspect

      # XXX implement negotiation algorithm
      return files.first

      # return the filename from the source
      # nil
    end

    # Visit (open) the document at the given URI.
    #
    # @param uri [RDF::URI, URI, :to_s]
    #
    # @return [Intertwingler::Context::Document] or nil

    def visit uri
      uri  = @resolver.uuid_for(uri) or return
      path = locate uri
      return unless path
      Document.new self, uri, uri: @resolver.uri_for(uri), doc: path
    end

    # resolve documents from source
    def resolve_documents
      src = @config[:source]
      out = []
      src.find do |f|
        Find.prune if f.basename.to_s[0] == ?.
        next if f.directory?
        out << f
      end

      out
    end

    def resolve_file path
      return unless path.file?
      path = Pathname('/') + path.relative_path_from(@config[:source])
      base = URI(@base.to_s)
      uri  = base + path.to_s

      #warn "trying #{uri}"

      until (out = @resolver.uuid_for uri)
        # iteratively strip off
        break if uri.path.end_with? '/'

        dn = path.dirname
        bn = path.basename '.*'

        # try index first
        if bn.to_s == 'index'
          p = dn.to_s
          p << '/' unless p.end_with? '/'
          uri = base + p
        elsif bn == path.basename
          break
        else
          path = dn + bn
          uri = base + path.to_s
        end

        # warn "trying #{uri}"
      end

      out
    end

    # Determine whether the URI represents a published document.
    #
    # @param uri
    #
    # @return [true, false]
    #
    def published? uri, circulated: false, retired: false, indexed: false
      if uuid = @resolver.uuid_for(uri)
        @graph.published? uuid, circulated: circulated,
          retired: retired, indexed: indexed
      end
    end

    # Find a destination pathname for the document
    #
    # @param uri
    # @param published
    #
    # @return [Pathname]
    #
    def target_for uri, published: false
      uri = @resolver.coerce_resource uri
      uri = @resolver.uuid_for uri
      target = @config[@graph.published?(uri) && published ? :target : :private]

      # target is a pathname so this makes a pathname
      target + "#{URI(uri.to_s).uuid}.xml"
    end

    # read from source

    # write (manipulated (x|x?ht)ml) back to source

    # write public and private variants to target

    def write_xhtml published: true
    end

    # write modified rdf

    def sponge_docs published: false, docs: nil
      docs ||= all_internal_docs published: published

      out = RDF::Repository.new

      docs.each do |s|
        next unless s = @resolver.uuid_for(s)
        next unless doc = visit(s)
        warn s
        out << doc.sponge
      end

      out
    end

    # bulk scan for terms
    def scan_terms types: [RDF::Vocab::SKOS.Concept, RDF::Vocab::FOAF.Agent],
        published: false, docs: nil
      docs ||= all_internal_docs published: published

      # we make a temporary repository
      # XXX maybe only select the target types?
      scratch = RDF::Repository.new
      @graph.query([nil, RDF.type, nil]) do |stmt|
        scratch << @graph.query([stmt.subject, nil, nil]) if
          type_is? stmt.object, types
      end

      # we also start with a pool of terms as they appear in the text
      pool = {}

      # we iterate over the docs
      docs.each do |s|
        # obtain the content
        doc = visit(s) or next

        # note the objective here is to get the statements in the rdfa
        # that are not already in the graph

        # remove garbage from the <head> from the working version; it
        # was thrown in there ad-hoc
        html = doc.doc.dup 1
        html.xpath(
          '/html:html/html:head/*[not(self::html:title|self::html:base)]',
          Intertwingler::Util::XPATHNS).each(&:unlink)

        # slurp up any rdfa, swapping in canonical uuids; note that if
        # we're doing this here then we assume they are authoritative
        # and don't check them against the graph
        RDF::RDFa::Reader.new(html, host_language: :xhtml5,
                              version: :'rdfa1.1').each do |stmt|
          if stmt.subject.iri? and
              su = @resolver.uuid_for(stmt.subject, verify: false)
            stmt.subject = su
          end
          if stmt.object.iri? and
              ou = @resolver.uuid_for(stmt.object, verify: false)
            stmt.object = ou
          end

          scratch << stmt
        end

        # we want to construct a net (hash) where the keys are all
        # acceptable lexical representations of all the terms, and the
        # values are hashes containing those representations  entities

        # okay now scan the doc for terms
        doc.scan_inlines do |subject, text, attrs, elem|
          # step zero: normalize the damn thing
          text = normalize_space text

          # inline alternate text or nil if none
          title = attrs[%i[content aria-label title].detect { |a| attrs[a] }]
          title = nil if title == text

          # okay we have three grades of label: pref, alt, hidden

          # pref is always going to be the most differentiated lexical
          # representation of whatever concept; we begin by assuming
          # that the text in the `text` position is the preferred one
          pref = text.dup

          # alt is going to be stuff like synonyms and abbreviations
          # that can be linked to an expanded term
          alt = Set[]

          # things like plurals and possessives, as well as normalized
          # lowercase representations of proper nouns go into hidden
          hidden = Set[]

          # do the possessives now
          if m = /^(.+?)['\u2019]s?$/.match(pref)
            hidden << pref
            pref = m.captures.first.strip
          end

          # proper noun?
          proper = false

          type = RDF::Vocab::SKOS.Concept

          case elem.name.to_sym
          when :abbr
            # the term is definitely an abbreviation if it is in an
            # <abbr> tag

            proper = true

            # do it this way rather than lemmatize
            if m = /^(.+?)e?s$/.match(pref)
              hidden << pref
              pref = m.captures.first.strip
            end

            # abbr title means the abbr text is the alt
            if title
              alt << pref
              pref = title
            end
          when :dfn
            # the term is definitely a concept if it is in a <dfn> tag
            if title
              hidden << pref # note this is hidden not alt
              pref = title
            end
          when -> x { x == :span and elem.attributes.empty? }
            # if it is in a <span> tag with exactly zero attributes,
            # then it is something like an agent
            proper = true
            type = RDF::Vocab::FOAF.Agent
          when -> x { x == :span and (title || '').split.include? pref }
            # same deal but use the title instead
            hidden << pref
            pref   = title
            proper = true
            type   = RDF::Vocab::FOAF.Agent
          else
            # unset pref to signal no more operations
            pref = nil
            type = nil
          end

          if pref
            words = pref.split
            # we can be confident the term is a proper noun if it is
            # more than one word and the first and last are capitalized
            proper = true if words.length > 2 and
              words.any? { |w| /\p{Upper}/.match w }
            if proper
              tmp = pref.downcase
              hidden << tmp if tmp != pref
            else
              # if we haven't decided conclusively that the term is a
              # proper noun (XXX THIS IS WHERE A TAGGER WOULD BE HANDY)
              # then we can go ahead and downcase and lemmatize it
              pref = pref.downcase
              tmp  = doc.lemmatize pref
              if tmp != pref
                hidden << pref
                pref = tmp
              end
            end

            # all the candidate keys
            t = Set[pref] | alt | hidden
            k = pool.keys.detect { |k| t.include? k }
            res = k ? pool[k] : {
              pref: pref, type: type, alt: Set[], hidden: Set[], refs: Set[] }

            # move the key to an alt if the existing one is shorter
            if res[:pref].length < pref.length
              res[:alt] << res[:pref]
              res[:pref] = pref
            end

            # now merge alt and hidden
            res[:alt]    |= alt
            res[:hidden] |= hidden

            # now add these in case we missed them
            t |= res[:alt] | res[:hidden]

            t.each { |k| pool[k] ||= res }

            # get the subject for this node
            if s = doc.subject_for(elem, coerce: :rdf)
              s = @resolver.uuid_for(s, verify: false) || s
              res[:refs] << s
            end
          end

        end
      end

      # okay NOW we want to construct a net of all the concepts etc,
      # where the keys are normalized, lowercased strings, and the
      # values are *sets* of resources
      entries    = {}
      candidates = {}

      scratch.query([nil, RDF.type, nil]) do |stmt|
        next unless type_is? stmt.object, types
        s = stmt.subject
        entry  = entries[s] = { id: s}
        struct = entry[:struct] = struct_for s, only: :literal
        pref   = entry[:pref] = (label_for(s, repo: scratch,
                                           candidates: struct) || []).last
        alts   = entry[:alts] = (label_for s, repo: scratch, unique: false,
                                 candidates: struct, alt: true).map(&:last)

        # we want all the labels
        labels = alts.dup
        labels.unshift pref if pref
        labels.uniq.each do |label|
          lstr = label.value.downcase.strip
          next if lstr.empty?
          mapping = candidates[lstr] ||= {}
          mapping[s] = entry
        end
      end

      out = pool.values.uniq
      out.each do |entry|
        entry[:id] = uuidv4
      end

      out
    end

    # csv output format:
    #
    # * first row: uri (uuid), type, preflabel, description, seealso
    # * second row: if missing the uuid in the first column then it is
    #   assumed columns 2 through N are altlabels
    # * third row: same deal but for hidden labels
    # * fourth row: same deal but for inverse references
    #
    # the next record starts when there is a uri in the first column
    #
    # @return [Array] suitable for csv output

    def scan_to_csv types: [RDF::Vocab::SKOS.Concept, RDF::Vocab::FOAF.Agent],
        published: false, docs: nil
      terms = scan_terms types: types, published: published, docs: docs

      out = [
        ['ID', 'Type', 'Preferred Label', 'Description', 'See Also']
      ]

      terms.sort do |a, b|
        c = a[:type] <=> b[:type]
        c == 0 ? a[:pref].downcase <=> b[:pref].downcase : c
      end.each do |term|
        out << [term[:id].to_s, @resolver.abbreviate(term[:type]), term[:pref].to_s]
        %i[alt hidden refs].each do |sym|
          out << (term[sym].empty? ? [] :
                  ([nil] + term[sym].to_a.map(&:to_s).sort))
        end
      end

      out
    end

    def ingest_concept_csv rows
      # temporary graph
      out = RDF::Repository.new

      subject = nil
      ctr = 0
      rows.each do |row|
        row = row.fields if row.respond_to? :fields
        row.map! do |c|
          c = c.to_s.strip
          c.empty? ? nil : c
        end

        if row.first
          subject = RDF::URI(row.first)

          if row[1] # type
            type = @resolver.resolve_curie row[1], scalar: true, as: :rdf
            out << [subject, RDF.type, type]
          end
          if row[2] # preflabel
            pref = RDF::Literal(row[2], language: 'en')
            out << [subject, RDF::Vocab::SKOS.prefLabel, pref]
          end
          if row[4] # description
            desc = RDF::Literal(row[3], language: 'en')
            out << [subject, RDF::Vocab::SKOS.definition, desc]
          end
          #
          row.drop(4).compact.each do |c|
            c = RDF::URI(c)
            out << [subject, RDF::RDFS.seeAlso, c]
          end

          ctr = 1
        elsif ctr == 1 # alt labels
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::Literal(c, language: 'en')
            out << [subject, RDF::Vocab::SKOS.altLabel, c]
          end
        elsif ctr == 2 # hidden labels
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::Literal(c, language: 'en')
            out << [subject, RDF::Vocab::SKOS.hiddenLabel, c]
          end
        elsif ctr == 3 # inverse refs
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::URI(c)
            out << [c, RDF::Vocab::DC.references, subject]
          end
        end
      end

      out
    end

    # - internet stuff -

    # verify external links for upness

    # collect triples for external links

    # fetch references for people/companies/concepts/etc from dbpedia/wikidata

    # - document context class -

    class Document
      include XML::Mixup
      include Util
      include NLP

      private

      C_OK = [Nokogiri::XML::Node, IO, Pathname].freeze

      public

      attr_reader :context, :doc, :uuid, :uri

      def initialize context, uuid, doc: nil, uri: nil, mtime: nil
        raise 'context must be a Intertwingler::Context' unless
          context.is_a? Intertwingler::Context
        raise "uuid must be an RDF::URI, not #{uuid.class}" unless
          uuid.is_a? RDF::URI and uuid.to_s.start_with? 'urn:uuid:'

        doc ||= context.locate uuid
        raise 'doc must be Pathname, IO, or Nokogiri node' unless
          C_OK.any? { |c| doc.is_a? c } || doc.respond_to?(:to_s)

        # set some instance variables
        @context = context
        @uuid    = uuid
        @mtime   = mtime || doc.respond_to?(:mtime) ? doc.mtime : Time.now
        @target  = context.target_for uuid

        # now process the document

        # turn the document into an XML::Document
        if doc.is_a? Nokogiri::XML::Node
          # a node that is not a document should be wrapped with one
          unless doc.is_a? Nokogiri::XML::Document
            d = doc.dup 1
            doc = Nokogiri::XML::Document.new
            doc << d
          end
        else
          type = nil

          # pathnames turned into IO objects
          if doc.is_a? Pathname
            type = MimeMagic.by_path doc
            doc  = doc.open # this may raise if the file isn't there
          end

          # squash everything else to a string
          doc = doc.to_s unless doc.is_a? IO

          # check type by content
          type ||= MimeMagic.by_magic(doc)

          # can you believe there is a special bookmarks mime type good grief
          type = MimeMagic['text/html'] if
            type == 'application/x-mozilla-bookmarks'

          # now we try to parse the blob
          if type.to_s =~ /xml/i
            doc = Nokogiri.XML doc
          elsif type == 'text/html'
            # if the detected type is html, try it as strict xml first
            attempt = nil
            begin
              attempt = Nokogiri.XML doc, nil, nil, (1 << 11) # NONET
            rescue Nokogiri::XML::SyntaxError
              # do not wrap this a second time; let it fail if it's gonna
              tmp = Nokogiri.HTML doc
              attempt = Nokogiri::XML::Document.new
              attempt << tmp.root.dup(1)
            end
            doc = attempt
          elsif type.to_s =~ /^text\/(?:plain|(?:x-)?markdown)/i
            # just assume plain text is markdown
            doc = ::MD::Noko.new.ingest doc
          else
            raise "Don't know what to do with #{uuid} (#{type})"
          end
        end

        # now fix the namespaces for mangled html documents
        root   = doc.root
        if root.name == 'html'
          unless root.namespace
            # clear this off or it will be duplicated in the output
            root.remove_attribute('xmlns')
            # now generate a new ns object
            ns = root.add_namespace(nil, XHTMLNS)
            # *now* scan the document and add the namespace declaration
            root.traverse do |node|
              if node.element? && node.namespace.nil?
                # downcasing the name may be cargo culting; need to check
                # node.name = node.name.downcase # yup it is
                node.namespace = ns
              end
            end
          end

          # also add the magic blank doctype declaration if it's missing
          unless doc.internal_subset
            doc.create_internal_subset('html', nil, nil)
          end
        end

        # aaand set some more instance variables

        @uri = URI(uri || @resolver.uri_for(uuid))

        # voilà
        @doc = doc
      end

      # KILL OK

      def subject_for node = nil,
          prefixes: @resolver.prefixes, base: @uri, coerce: :rdf
        node ||= doc.root
        prefixes = @resolver.prefixes.merge(
          get_prefixes(node, coerce: :term).filter { |k, _| k })
        Intertwingler::Util.subject_for node,
          prefixes: @resolver.prefixes, base: base, coerce: coerce
      end

      # KILL OK

      # proxy for graph published
      def published?
        @graph.published? @uuid
      end

      #KILL OK

      def base_for node = nil
        node ||= @doc
        doc  = node.document
        base = @uri.to_s
        if doc.root.name.to_sym == :html
          b = doc.at_xpath(
            '(/html:html/html:head/html:base[@href])[1]/@href',
            { html: XHTMLNS }).to_s.strip
          base = b if URI(b).absolute?
        elsif b = doc.at_xpath('ancestor-or-self::*[@xml:base][1]/@xml:base')
          b = b.to_s.strip
          base = b if URI(b).absolute?
        end

        URI(base)
      end

      # notice these are only RDFa attributes that take URIs
      RDFA_ATTR  = %i[about resource typeof].freeze
      LINK_ATTR  = %i[href src data action longdesc].freeze
      LINK_XPATH = ('.//html:*[not(self::html:base)][%s]' %
        (LINK_ATTR + RDFA_ATTR).map { |a| "@#{a.to_s}" }.join('|')).freeze

      def rewrite_links node = @doc, uuids: {}, uris: {}, rdfa: true, &block
        res   = @context.resolver
        base  = base_for node
        out   = []
        cache = {}
        names = rdfa ? LINK_ATTR + RDFA_ATTR : LINK_ATTR
        xpath = ('.//html:*[not(self::html:base)][%s]' %
          names.map { |a| "@#{a.to_s}" }.join(?|))
        node.xpath(xpath, { html: XHTMLNS }).each do |elem|
          (names - [:typeof]).each do |attr|
            attr = attr.to_s
            next unless elem.has_attribute? attr

            # XXX grr bnodes
            next if elem[attr].strip.start_with? '_:'

            # warn "trying raw #{elem[attr].strip}"

            # GRRRR URI::URN (or rather URI) is way way too anal
            begin
              tmp = uri_pp(elem[attr].strip)
              abs = base.merge tmp
            rescue URI::InvalidComponentError
              next
            end

            # fix e.g. http->https
            if abs.host == @uri.host and abs.scheme != @uri.scheme
              tmp          = @uri.dup
              tmp.path     = abs.path
              tmp.query    = abs.query
              tmp.fragment = abs.fragment
              abs          = tmp
            end

            # harvest query string
            pp = split_pp abs, only: true

            abs = RDF::URI(abs.to_s)

            # warn "first #{abs.inspect}"

            # round-trip to uuid and back if we can
            #if uuid = uuids[abs] ||= res.uuid_for(abs, verify: false)
            if uuid = res.uuid_for(abs, verify: false)
              abs = cache[abs] ||= res.uri_for(uuid, roundtrip: true)
            else
              abs = cache[abs] ||= res.uri_for(abs, roundtrip: true)
            end

            # warn "then #{abs.inspect}"

            # reinstate the path parameters
            if !pp.empty? && split_pp(abs, only: true).empty?
              abs = abs.dup
              abs.path = ([abs.path] + pp).join(?;)
            end

            # warn "trying #{abs}"
            elem[attr] = @uri.host == abs.host ?
              @uri.route_to(abs.to_s).to_s : abs.to_s
            # warn "that (#{abs} -> #{elem[attr]}) worked lol"
            out << abs
          end

          # warn "now trying block"
          block.call elem if block
          # warn "block worked"
        end

        # uhh is there any better return value here?
        out.uniq
      end

      # sponge the document for rdfa
      def triples_for
      end

      OBJS = [:href, :src].freeze

      # ancestor node always with (@property and not @content) and
      # not @resource|@href|@src unless @rel|@rev
      LITXP = ['(ancestor::*[@property][not(@content)]',
        '[not(@resource|@href|@src) or @rel|@rev])[1]' ].join('').freeze
      # note parentheses cause the index to be counted from the root

      def vocab_for node
        if node[:vocab]
          vocab = node[:vocab].strip
          return nil if vocab == ''
          vocab = RDF::URI(vocab)
          return RDF::Vocabulary.find_term(vocab) rescue vocab
        end
        parent = node.parent
        vocab_for parent if parent and parent.element?
      end

      def prefixes_for node, prefixes = {}
        prefixes = prefixes.transform_keys(&:to_sym);

        # start with namespaces
        pfx = node.namespaces.select do |k, _|
          k.start_with? 'xmlns:'
        end.transform_keys do |k|
          k.delete_prefix('xmlns:').to_sym
        end
        # then add @prefix overtop of the namespaces
        if node[:prefix]
          x = node[:prefix].strip.split(/\s+/)
          a = []
          b = []
          x.each_index { |i| (i % 2 == 0 ? a : b).push x[i] }
          a.map! { |x| x.gsub(/:.*/, '').to_sym }
          # if the size is uneven the values will be nil, so w drop em
          pfx.merge! a.zip(b).to_h.reject { |_, v| v.nil? }
        end

        # since we're ascending the tree, input takes precedence
        prefixes = pfx.transform_values do |v|
          v = RDF::URI(v)
          RDF::Vocabulary.find_term(v) rescue v
        end.merge prefixes

        if node.parent and node.parent.element?
          prefixes_for node.parent, prefixes
        else
          prefixes
        end
      end

      # backlink structure
      def generate_backlinks published: true, ignore: nil
        @context.generate_backlinks @uuid, published: published, ignore: ignore
      end

      # goofy twitter-specific metadata
      def generate_twitter_meta
        @context.generate_twitter_meta @uuid
      end

      def transform_xhtml published: true, rehydrate: false, rescan: false, sponge: false
        # before we do any more work make sure this is html
        doc  = @doc.dup 1
        body = doc.at_xpath(
          '/html:body|/html:html/html:body[1]', { html: XHTMLNS }) or return

        resolver = @context.resolver
        repo     = @context.graph

        # sponge initially
        sponge doc, repo: repo if sponge

        # XXX KILL THIS
        Intertwingler::Util::Messy.rehydrate body, resolver, rescan: rescan do |cs|
          # warn cs.inspect
          unless cs.empty?
            cs.select! do |k, v|
              repo.type_is?(v[:types],
                [RDF::Vocab::SKOS.Concept, RDF::Vocab::FOAF.Agent
                ])
            end
            # XXX TODO make this logic better: if there are still
            # candidates, sort by preferred predicate for given type
            cs.keys.sort.first
          end
        end if rehydrate

        # eliminate comments
        doc.xpath('//comment()[not(ancestor::html:script)]',
          { html: XHTMLNS }).each(&:unlink)

        # initial stuff
        struct    = resolver.struct_for @uuid, uuids: true, canon: true
        # rstruct   = @context.struct_for @uuid, uuids: true, rev: true
        resources = {}
        literals  = {}
        ufwd      = {} # uuid -> uri
        urev      = {} # uri  -> uuid
        datatypes = Set.new
        types     = Set.new
        authors   = repo.authors_for @uuid
        title     = repo.label_for @uuid, struct: struct
        desc      = repo.label_for @uuid, struct: struct, desc: true

        # rewrite content
        title = title[1] if title
        desc  = desc[1]  if desc

        # `struct` and `rstruct` will contain all the links and
        # metadata for forward and backward neighbours, respectively,
        # which we need to mine (predicates, classes, datatypes) for
        # prefixes among other things.

        inv = repo.invert_struct struct do |p, o|
          if o.literal?
            literals[o] ||= Set.new
            literals[o] << p

            # collect the datatype
            datatypes << o.datatype if o.has_datatype?
          else
            # normalize URIs
            if o.to_s.downcase.start_with? 'urn:uuid:'
              ufwd[o] ||= resolver.uri_for o
            elsif cu = resolver.uuid_for(o)
              o = urev[o] ||= cu
            end

            # collect the resource
            resources[o] ||= Set.new
            resources[o] << p

            # add to type
            types << o if p == RDF::RDFV.type
          end

          nil # so we don't accidentally pollute the output
        end
        urev.merge! ufwd.invert

        labels = resources.keys.map do |k|
          # turn this into a pair which subsequently gets turned into a hash
          [k, repo.label_for(k) ]
        end.to_h

        #warn labels

        # handle the title
        title ||= RDF::Literal('')
        tm = { '#title' => title,
          property: resolver.abbreviate(literals[title].to_a, vocab: XHV) }
        if tl = title.language
          tm['xml:lang'] = tl # if xmlns
          tm['lang'] = tl
        elsif tdt = title.datatype and tdt != RDF::XSD.string
          tm[:datatype] = resolver.abbreviate(tdt)
        end

        # we accumulate a record of the links in the body so we know
        # which ones to skip in the head
        bodylinks = rewrite_links body do |elem|
          vocab = elem.at_xpath('ancestor-or-self::*[@vocab][1]/@vocab')
          vocab = uri_pp(vocab.to_s) if vocab

          if elem.key?('href') or elem.key?('src')
            # warn [@uri, elem['href'] || elem['src']].inspect
            vu = uri_pp(elem['href'] || elem['src'])
            ru = RDF::URI(@uri.merge(vu))
            # bodylinks[urev[ru] || ru] = true

            if rel = resources[urev[ru] || ru]
              elem['rel'] =
                (resolver.abbreviate rel, vocab: vocab, scalar: false).join ' '
            end

            label = labels[urev[ru] || ru]
            if label and (!elem.key?('title') or elem['title'].strip == '')
              elem['title'] = label[1].to_s
            end
          end
        end


        # and now we do the head
        links = @context.head_links @uuid, struct: struct, nodes: resources,
          prefixes: @context.prefixes, ignore: bodylinks, labels: labels,
          vocab: XHV, rev: Intertwingler::Vocab::CI.document

        # we want to duplicate links from particular subjects (eg the root)
        (@context.config[:duplicate] || {}).sort do |a, b|
          a.first <=> b.first
        end.each do |s, preds|

          o = {}
          u = ufwd[s] ||= resolver.uuid_for s
          s = urev[u] ||= resolver.uri_for u if u
          f = {}

          # do not include this subject as these links are already included!
          next if u == @uuid

          # gather up the objects, then gather up the predicates

          repo.objects_for u || s, preds, only: :resource do |obj, rel|
            # XXX do not know why += |= etc does not work
            x = resolver.uuid_for(obj) || obj
            urev[x] ||= resolver.uri_for x
            y = o[x] ||= Set.new
            o[x] = y | rel
            f[x] = repo.formats_for x
          end

          srel = @uri.route_to((u ? urev[u] || s : s).to_s)

          # now collect all the other predicates
          o.keys.each do |obj|
            hrel = @uri.route_to((urev[obj] || obj).to_s)
            o[obj] |= repo.query([u || s, nil, obj]).predicates.to_set
            rels = resolver.abbreviate o[obj].to_a, vocab: XHV
            ln = { nil => :link, about: srel, rel: rels, href: hrel }
            ln[:type] = f[obj].first if f[obj]

            # add to links
            links << ln
          end
        end

        meta = []

        # include author names as old school meta tags
        authors.each do |a|
          name  = labels[urev[a] || a] or next
          datatypes.add name[0] # a convenient place to chuck this
          prop  = resolver.abbreviate(name[0])
          name  = name[1]
          about = @uri.route_to((ufwd[a] || a).to_s)
          tag   = { nil => :meta, about: about.to_s, name: :author,
                   property: prop, content: name.to_s }

          if name.has_datatype? and name.datatype != RDF::XSD.string
            tag[:datatype] = resolver.abbreviate(name.datatype)
          elsif name.has_language?
            tag['xml:lang'] = tag[:lang] = name.language
          end
          meta.push tag
        end

        literals.each do |k, v|
          next if k == title
          rel = resolver.abbreviate v.to_a, vocab: XHV
          elem = { nil => :meta, property: rel, content: k.to_s }
          elem[:name] = :description if k == desc

          if k.has_datatype?
            datatypes.add k.datatype # so we get the prefix
            elem[:datatype] = resolver.abbreviate k.datatype, vocab: XHV
          end

          meta.push(elem)
        end

        meta.sort! do |a, b|
          s = 0
          [:about, :property, :datatype, :content, :name].each do |k|
            # warn a.inspect, b.inspect
            s = a.fetch(k, '').to_s <=> b.fetch(k, '').to_s
            break if s != 0
          end
          s
        end

        # don't forget style tag
        style = doc.xpath('/html:html/html:head/html:style', { html: XHTMLNS })

        # prepare only the prefixes we need to resolve the data we need
        rsc = resolver.abbreviate(
          (struct.keys + resources.keys + datatypes.to_a + types.to_a).uniq,
          noop: false, scalar: false).map do |x|
          next if x.nil?
          x.split(?:).first.to_sym
        end.compact.to_set

        # XXX we need to also capture prefixes that were in the markup
        # but had the prefix removed or otherwise no prefix expansion given

        # XXX do this better
        rdfa = %w[about resource typeof rel rev property datatype]
        # rxp = ".//*[%s]/@*" % rdfa.map { |a| "@#{a}" }.join(?|)
        rxp = rdfa.map { |a| ".//*/@%s" % a }.join(?|)

        rsc += doc.xpath(rxp).map do |a|
          # get rid of safe-curies
          a.value.strip.gsub(/^\[([^\]]+)\]$/, '\1').split.map do |s|
            s.split(?:).first.to_sym
          end
        end.compact.flatten.to_set

        pfx = resolver.prefixes.select do |k, _|
          rsc.include? k
        end.transform_values { |v| v.to_s }

        body = body.dup 1
        body = { id: UUID::NCName.to_ncname_64(@uuid.to_s.dup, version: 1),
          about: '', '#body' => body.children.to_a }
        body[:typeof] = resolver.abbreviate(types.to_a, vocab: XHV) unless
          types.empty?

        # XXX deal with the qb:Observation separately (just nuke it for now)
        extra = generate_twitter_meta || []
        if bl = generate_backlinks(published: published)#,
          # ignore: @context.graph.query(
          # [nil, CI.document, @uuid]).subjects.to_set)
          extra << { [bl] => :object }
        end

        # and now for the document
        xf  = @context.config[:transform]
        doc = xhtml_stub(
          base: @uri, prefix: pfx, vocab: XHV, lang: 'en', title: tm,
          link: links, meta: meta, style: style, transform: xf,
          extra: extra, body: body).document

        # goddamn script tags and text/html
        doc.xpath('//html:script[@src][not(node())]',
          { html: XHTMLNS }).each do |script|
          script << doc.create_text_node('')
        end

        # sponge the generated data back lol
        sponge doc, repo: repo if sponge

        doc
      end

      # Actually write the transformed document to the target
      #
      # @param published [true, false]
      # @param rehydrate
      #
      # @return [Array] pathname(s) written
      def write_to_target published: true, rehydrate: false, rescan: false, sponge: false

        # in all cases we write to private target
        states = [false]
        # document has to be publishable
        states.push true if published && @context.published?(@uuid)

        ok = []
        states.each do |state|
          target = @context.config[state ? :target : :private]

          # XXX this is dumb; it should do something more robust if it
          # fails
          doc = transform_xhtml(published: state, rehydrate: rehydrate,
                                rescan: rescan, sponge: sponge) or next

          begin
            fh   = Tempfile.create('xml-', target)
            path = Pathname(fh.path)

            # write the doc to the target
            doc.write_to fh
            fh.close

            uuid = URI(@uuid.to_s)
            newpath = path.dirname + "#{uuid.uuid}.xml"
            ok.push newpath

            File.chmod(0644, path)
            File.rename(path, newpath)
            File.utime(@mtime, @mtime, newpath)
          rescue Exception => e
            # XXX this should only rescue a specific class of errors
            warn e.class, e
            File.unlink path if path.exist?
          end
        end

        ok
      end

      CURIES     = %w[about resource typeof rel rev property datatype].freeze
      CURIE_TAGS = ('//*[%s][not(ancestor::*[@property and not(@content)])]' %
        CURIES.map { |a| "@#{a}" }.join(?|)).freeze

      # Sponge the RDFa out of the document.
      #
      # @return [RDF::Repository] the statements found in the document.
      #
      def sponge doc = @doc, uri = @uri, repo: nil, overwrite: false
        repo ||= RDF::Repository.new

        # remove garbage from the <head> from the working version; it
        # was thrown in there ad-hoc
        html = doc.dup 1

        # XXX maybe get rid of this?
        # html.xpath(
        #   '/html:html/html:head/*[not(self::html:title|self::html:base)]',
        #   Intertwingler::Util::XPATHNS).each(&:unlink)

        res = @context.resolver

        # gather up all of the values of the rdfa attributes
        prefixes = {}
        found = html.xpath(CURIE_TAGS, Intertwingler::Util::XPATHNS).map do |elem|
          prefixes = Intertwingler::Util::Messy.get_prefixes(elem).merge prefixes
          CURIES.map { |a| elem[a].to_s.strip.split }
        end.flatten.reject { |c| /:(?=\/\/)/.match? c }.map do |c|
          c = /^(?:([^:]+):)?/.match(c).captures.first
          c ? c.to_sym : c
        end.to_set

        # merge any found prefixes with our prefixes
        prefixes = res.prefixes.merge(prefixes).select do |k, _|
          found.include? k and not k.nil?
        end

        # warn found.inspect
        # warn prefixes.inspect

        # warn prefixes.sort.inspect

        # warn found.inspect, res.prefixes.keys.to_set.inspect, prefixes.inspect

        html.root[:prefix] = flatten_attr prefixes.transform_values(&:to_s)

        # this prevents what i think was causing the first section to
        # be incorrectly identified with the document
        if body = html.at_xpath('/html:html/html:body', XPATHNS)
          body[:about] = '' unless %w[about resource typeof].any? do |a|
            body.key? a
          end
        end

        # warn html.root[:prefix]

        # warn html.to_s

        # if the document has no rdfa

        # slurp up any rdfa, swapping in canonical uuids; note that if
        # we're doing this here then we assume they are authoritative
        # and don't check them against the graph
        RDF::RDFa::Reader.new(html, base_uri: @uri.to_s, host_language: :xhtml5,
                              version: :'rdfa1.1').each do |stmt|
          warn stmt.inspect
          # just throw this out; it causes trouble
          next if stmt.predicate == Intertwingler::Vocab::CI.canonical

          s, o = stmt.subject, stmt.object

          stmt.subject = res.uuid_for s,
            verify: false, published: false, noop: true if s.iri?
          stmt.object  = res.uuid_for o,
            verify: false, published: false, noop: true if o.iri?

          # warn stmt

          # warn o if stmt.object.nil?

          repo << stmt
        end

        repo
      end

      def scan_inlines &block
        # we're using the static method because it is stateless
        Intertwingler::Util.scan_inlines @doc, base: @uri, &block
      end

    end
  end
end
