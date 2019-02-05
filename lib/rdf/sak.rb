# -*- coding: utf-8 -*-
require 'rdf/sak/version'

# basic stuff
require 'stringio'
require 'pathname'
require 'tempfile'
require 'mimemagic'

# rdf stuff
require 'uri'
require 'uri/urn'
require 'rdf'
require 'rdf/reasoner'
require 'linkeddata'

# cli stuff
require 'commander'

# my stuff
require 'xml-mixup'
require 'md-noko'
require 'uuid-ncname'

# ontologies, mine in particular
require 'rdf/sak/ci'
require 'rdf/sak/ibis'
# others not included in rdf.rb
require 'rdf/sak/pav'

module RDF::SAK

  # janky bolt-on MimeMagic
  class MimeMagic < ::MimeMagic

    # XXX this is not strictly correct but good enough for now
    [
      ['text/n3', %w(n3 ttl nt), %w(text/plain), [[0..256, '@prefix']]],
    ].each do |magic|
      self.add magic[0], extensions: magic[1], parents: magic[2],
        magic: magic[3]
    end

    def self.binary? thing
      sample = nil

      # get some stuff out of the IO or get a substring
      if thing.is_a? IO
        pos = thing.tell
        thing.seek 0, 0
        sample = thing.read 100
        thing.seek pos
      else
        sample = thing.to_s[0,100]
      end

      # consider this to be 'binary' if empty
      return true if sample.length == 0
      # control codes minus ordinary whitespace
      sample.b =~ /[\x0-\x8\xe-\x1f\x7f]/ ? true : false
    end

    def self.default_type thing
      new(self.binary?(thing) ? 'application/octet-stream' : 'text/plain')
    end

    def self.by_extension io
      super(io) || default_type(io)
    end

    def self.by_path io
      super(io) || default_type(io)
    end

    def self.by_magic io
      super(io) || default_type(io)
    end

    def self.all_by_magic io
      out = super(io)
      out.length > 0 ? out : [default_type(io)]
    end

  end

  class Context
    include XML::Mixup

    private

    RDF::Reasoner.apply(:rdfs, :owl)

    # okay labels: what do we want to do about them? poor man's fresnel!

    # basic structure is an asserted base class corresponding to a
    # ranked list of asserted predicates. to the subject we first
    # match the closest class, then the closest property.

    # if the instance data doesn't have an exact property mentioned in
    # the spec, it may have an equivalent property or subproperty we
    # may be able to use. we could imagine a scoring system analogous
    # to the one used by CSS selectors, albeit using the topological
    # distance of classes/predicates in the spec versus those in the
    # instance data.

    # think about dcterms:title is a subproperty of dc11:title even
    # though they are actually more like equivalent properties;
    # owl:equivalentProperty is not as big a conundrum as
    # rdfs:subPropertyOf. 

    # if Q rdfs:subPropertyOf P then S Q O implies S P O. this is
    # great but property Q may not be desirable to display.

    # it may be desirable to be able to express properties to never
    # use as a label, such as skos:hiddenLabel

    # consider ranked alternates, sequences, sequences of alternates.
    # (this is what fresnel does fyi)

    STRINGS = {
      RDF::RDFS.Resource => {
        label: [
          # main
          [RDF::Vocab::SKOS.prefLabel, RDF::RDFS.label,
            RDF::Vocab::DC.title, RDF::Vocab::DC11.title, RDF::RDFV.value],
          # alt
          [RDF::Vocab::SKOS.altLabel, RDF::Vocab::DC.alternative],
        ],
        desc: [
          # main will be cloned into alt
          [RDF::Vocab::DC.description, RDF::Vocab::DC11.description,
            RDF::RDFS.comment, RDF::Vocab::SKOS.note],
        ],
      },
      RDF::Vocab::FOAF.Document => {
        label: [
          # main
          [RDF::Vocab::DC.title, RDF::Vocab::DC11.title],
          # alt
          [RDF::Vocab::BIBO.shortTitle, RDF::Vocab::DC.alternative],
        ],
        desc: [
          # main
          [RDF::Vocab::BIBO.abstract, RDF::Vocab::DC.abstract,
            RDF::Vocab::DC.description, RDF::Vocab::DC11.description],
          # alt
          [RDF::Vocab::BIBO.shortDescription],
        ],
      },
      RDF::Vocab::FOAF.Agent => {
        label: [
          # main (will get cloned into alt)
          [RDF::Vocab::FOAF.name],
        ],
        desc: [
          # main cloned into alt
          [RDF::Vocab::FOAF.status],
        ],
      },
    }
    STRINGS[RDF::OWL.Thing] = STRINGS[RDF::RDFS.Resource]

    # note this is to_a because "can't modify a hash during iteration"
    # which i guess is sensible, so we generate a set of pairs first
    STRINGS.to_a.each do |type, struct|
      struct.values.each do |lst|
        # assert a whole bunch of stuff
        raise 'STRINGS content must be an array of arrays' unless
          lst.is_a? Array
        raise 'Spec must contain 1 or 2 Array elements' if lst.empty?
        raise 'Spec must be array of arrays of terms' unless
          lst.all? { |x| x.is_a? Array and x.all? { |y|
            RDF::Vocabulary.find_term(y) } }

        # prune this to two elements (not that there should be more than)
        lst.slice!(2, lst.length) if lst.length > 2

        # pre-fill equivalent properties
        lst.each do |preds|
          # for each predicate, find its equivalent properties

          # splice them in after the current predicate only if they
          # are not already explicitly in the list
          i = 0
          loop do
            equiv = preds[i].entail(:equivalentProperty) - preds
            preds.insert(i + 1, *equiv) unless equiv.empty?

            i += equiv.length + 1
            break if i >= preds.length
          end

          # this just causes too many problems otherwise
          # preds.map! { |p| p.to_s }
        end

        # duplicate main predicates to alternatives
        lst[1] ||= lst[0]
      end

      # may as well seed equivalent classes so we don't have to look them up
      type.entail(:equivalentClass).each do |equiv|
        STRINGS[equiv] ||= struct
      end

      # tempting to do subclasses too but it seems pretty costly in
      # this framework; save it for the clojure version
    end

    G_OK = [RDF::Repository, RDF::Dataset, RDF::Graph].freeze
    C_OK = [Pathname, IO, String].freeze

    def coerce_graph graph = nil, type: nil
      return RDF::Repository.new unless graph
      return graph if G_OK.any? { |c| graph.is_a? c }
      raise 'Graph must be some kind of RDF::Graph or RDF data file' unless
        C_OK.any? { |c| graph.is_a? c } || graph.respond_to?(:to_s)

      opts = {}
      opts[:content_type] = type if type

      if graph.is_a? Pathname
        opts[:filename] = graph.expand_path.to_s
        graph = graph.open
      elsif graph.is_a? File
        opts[:filename] = graph.path
      end

      graph = StringIO.new(graph.to_s) unless graph.is_a? IO
      reader = RDF::Reader.for(opts) do
        graph.rewind
        sample = graph.read 1000
        graph.rewind
        sample
      end or raise "Could not find an RDF::Reader for #{opts[:content_type]}"

      out = RDF::Repository.new

      reader = reader.new graph, **opts
      reader.each_statement do |stmt|
        out << stmt
      end

      out
    end

    def normalize_hash h
      out = {}
      h.each do |k, v|
        out[k.to_s.to_sym] = v.is_a?(Hash) ? normalize_hash(v) : v
      end
      out
    end

    def coerce_config config
      # config must either be a hash or a file name/pathname/io object
      unless config.respond_to? :to_h
        require 'parseconfig'
        cf = config.is_a?(IO) ? config : Pathname.new(config).expand_path
        config = ParseConfig.new(cf).params
      end

      config = normalize_hash config

      # config MUST have source and target dirs
      raise 'Config must have :source, :target, and :private directories' unless
        ([:source, :target, :private] - config.keys).empty?
      [:source, :target].each do |path|
        dir = config[path] = Pathname.new(config[path]).expand_path
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
      if config[:graph] and config[:graph].is_a? String
        config[:graph] = Pathname.new(config[:graph]).expand_path
      end

      # deal with prefix map
      if config[:prefixes]
        config[:prefixes] = config[:prefixes].transform_values do |p|
          # we have to wrap this in case it fails
          begin
            RDF::Vocabulary.find_term(p) || RDF::URI(p)
          rescue
            RDF::URI(p)
          end
        end
      end

      config
    end

    SCHEME_RANK = { https: 0, http: 1 }

    def cmp_resource a, b, www: nil
      raise 'Comparands must be instances of RDF::Value' unless
        [a, b].all? { |x| x.is_a? RDF::Value }

      # URI beats non-URI
      if a.uri?
        if b.uri?
          # https beats http beats other
          as = a.scheme.downcase.to_sym
          bs = b.scheme.downcase.to_sym
          cmp = SCHEME_RANK.fetch(as, 2) <=> SCHEME_RANK.fetch(bs, 2)

          # bail out early
          return cmp unless cmp == 0

          # this would have returned if the schemes were different, as
          # such we only need to test one of them
          if [:http, :https].any?(as) and not www.nil?
            # if www is non-nil, prefer www or no-www depending on
            # truthiness of `www` parameter
            pref = [false, true].zip(www ? [1, 0] : [0, 1]).to_h
            re = /^(?:(www)\.)?(.*?)$/

            ah = re.match(a.host.to_s.downcase)[1,2]
            bh = re.match(b.host.to_s.downcase)[1,2]

            # compare hosts sans www
            cmp = ah[1] <=> bh[1]
            return cmp unless cmp == 0

            # now compare presence of www
            cmp = pref[ah[0] == 'www'] <=> pref[bh[0] == 'www']
            return cmp unless cmp == 0

            # if we're still here, compare the path/query/fragment
            re = /^.*?\/\/.*?(\/.*)$/
            al = re.match(a.to_s)[1].to_s
            bl = re.match(b.to_s)[1].to_s

            return al <=> bl
          end

          return a <=> b
        else
          return -1
        end
      elsif b.uri?
        return 1
      else
        return a <=> b
      end
    end
    
    # rdf term type tests
    NTESTS = { uri: :"uri?", blank: :"node?", literal: :"literal?" }.freeze
    NMAP   = ({ iri: :uri, bnode: :blank }.merge(
      [:uri, :blank, :literal].map { |x| [x, x] }.to_h)).freeze

    def coerce_node_spec spec
      spec = [spec] unless spec.respond_to? :to_a
      spec = spec - [:resource] + [:uri, :blank] if spec.include? :resource
      spec = NMAP.values_at(*spec).reject(&:nil?).uniq
      spec = NTESTS.keys if spec.length == 0
      spec
    end

    def node_matches? node, spec
      spec.any? { |k| node.send NTESTS[k] }
    end

    AUTHOR  = [RDF::SAK::PAV.authoredBy, RDF::Vocab::DC.creator,
      RDF::Vocab::DC11.creator, RDF::Vocab::PROV.wasAttributedTo]
    CONTRIB = [RDF::SAK::PAV.contributedBy, RDF::Vocab::DC.contributor,
      RDF::Vocab::DC11.contributor]
    [AUTHOR, CONTRIB].each do |preds|
      i = 0
      loop do
        equiv = preds[i].entail(:equivalentProperty) - preds
        preds.insert(i + 1, *equiv) unless equiv.empty?
        i += equiv.length + 1
        break if i >= preds.length
      end

      preds.freeze
    end

    def term_list terms
      terms = terms.respond_to?(:to_a) ? terms.to_a : [terms]
      terms.uniq.map { |t| RDF::Vocabulary.find_term t }.compact
    end

    def coerce_resource arg
      return arg if arg.is_a? RDF::URI

      blank = /^_:(.*)/.match arg.to_s
      return RDF::Node blank[1] if blank

      arg = URI(@base.to_s).merge arg.to_s

      RDF::URI(arg.to_s)
    end

    def coerce_uuid_urn arg
      # if this is an ncname then change it
      if ([URI, RDF::URI] & arg.class.ancestors).empty? &&
          arg.respond_to?(:to_s)
        arg = arg.to_s

        # coerce ncname to uuid
        arg = UUID::NCName::from_ncname(arg, version: 1) if arg =~
          /^[A-P](?:[0-9A-Z_-]{20}|[2-7A-Z]{24})[A-P]$/i

        # now the string is either a UUID or it isn't
        
        arg = "urn:uuid:#{arg}" unless arg.start_with? 'urn:uuid:'
      else
        arg = arg.class.new arg.to_s.downcase unless arg == arg.to_s.downcase
      end

      raise 'not a UUID' unless
        arg.to_s =~ /^urn:uuid:[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$/

      arg = coerce_resource arg
    end

    public

    attr_reader :config, :graph, :base

    # Initialize a context.
    #
    # @param graph
    # @param base
    # @param config
    # @param type
    #
    # @return [RDF::SAK::Context] the new context object.

    def initialize graph: nil, base: nil, config: nil, type: nil
      # RDF::Reasoner.apply(:rdfs, :owl)

      @config = coerce_config config

      graph ||= @config[:graph] if @config[:graph]
      base  ||= @config[:base]  if @config[:base]
      
      @graph  = coerce_graph graph, type: type
      @base   = RDF::URI.new base.to_s if base
    end

    # Get the prefix mappings from the configuration.
    #
    # @return [Hash]

    def prefixes
      @config[:prefixes] || {}
    end

    # Abbreviate a set of terms against the registered namespace
    # prefixes and optional default vocabulary, or otherwise return a
    # string representation of the original URI.
    
    # @param 
    # @param
    #
    # @return

    def abbreviate term, vocab: nil
      vocab  = coerce_resource(vocab).to_s if vocab
      scalar = true

      if term.respond_to? :to_a
        term   = term.to_a
        scalar = false
      else
        term = [term]
      end

      rev = prefixes.invert.transform_keys { |k| k.to_s }

      term.map! do |t|
        t  = t.to_s
        vt = t.delete_prefix(vocab) if vocab
        if vt.nil? or vt == t or vt.match?(/:/)
          rev.sort { |a, b| b.length <=> a.length }.each do |k, v|
            if (vt = t.delete_prefix(k)) != t
              vt = '%s:%s' % [v, vt]
              break
            else
              vt = nil
            end
          end
        end
        vt || t
      end

      scalar ? term[0] : term.sort
    end

    # Obtain a key-value structure for the given subject, optionally
    # constraining the result by node type (:resource, :uri/:iri,
    # :blank/:bnode, :literal)
    #
    # @param subject
    # @param rev
    # @param only
    #
    # @return [Hash]

    def struct_for subject, rev: false, only: []
      only = coerce_node_spec only

      rsrc = {}
      pattern = rev ? [nil, nil, subject] : [subject, nil, nil]
      @graph.query(pattern).each do |stmt|
        # this will skip over any term not matching the type
        node = rev ? stmt.subject : stmt.object
        next unless node_matches? node, only
        p = RDF::Vocabulary.find_term(stmt.predicate) || stmt.predicate
        o = rsrc[p] ||= []
        o.push node
      end

      # XXX in here we can do fun stuff like filter/sort by language/datatype
      rsrc.values.each { |v| v.sort! }

      rsrc
    end

    # Obtain everything in the graph that is an `rdf:type` of something.
    # 
    # @return [Array]

    def all_types
      @graph.query([nil, RDF.type, nil]).collect do |s|
        s.object
      end.uniq
    end

    # Obtain every subject that is rdf:type the given type or its subtypes.
    #
    # @param rdftype [RDF::Term]
    #
    # @return [Array]

    def all_of_type rdftype
      t = RDF::Vocabulary.find_term(rdftype) or raise "No type #{rdftype.to_s}"
      out = []
      (all_types & all_related(t)).each do |type|
        out += @graph.query([nil, RDF.type, type]).collect { |s| s.subject }
      end
      
      out.uniq
    end

    # Obtain everything that is an owl:equivalentClass or
    # rdfs:subClassOf the given type.
    #
    # @param rdftype [RDF::Term]
    #
    # @return [Array]

    def all_related rdftype
      t = RDF::Vocabulary.find_term(rdftype) or raise "No type #{rdftype.to_s}"
      q = [t] # queue
      c = {}  # cache

      while term = q.shift
        # add term to cache
        c[term] = term
        # entail equivalent classes
        term.entail(:equivalentClass).each do |ec|
          # add equivalent classes to queue (if not already cached)
          q.push ec unless c[ec]
          c[ec] = ec unless ec == term
        end
        # entail subclasses
        term.subClass.each do |sc|
          # add subclasses to queue (if not already cached)
          q.push sc unless c[sc]
          c[sc] = sc unless sc == term
        end
      end

      # smush the result 
      c.keys
    end

    # Obtain a stack of types for an asserted initial type or set
    # thereof. Returns an array of arrays, where the first is the
    # asserted types and their inferred equivalents, and subsequent
    # elements are immediate superclasses and their equivalents. A
    # given URI will only appear once in the entire structure.
    #
    # @param rdftype [RDF::Term, :to_a]
    #
    # @return [Array]

    def type_strata rdftype
      # first we coerce this to an array
      if rdftype.respond_to? :to_a
        rdftype = rdftype.to_a
      else
        rdftype = [rdftype]
      end
      
      # now squash and coerce
      rdftype = rdftype.uniq.map { |t| RDF::Vocabulary.find_term t }.compact

      # bail out early
      return [] if rdftype.count == 0

      # essentially what we want to do is construct a layer of
      # asserted classes and their inferred equivalents, then probe
      # the classes in the first layer for subClassOf assertions,
      # which will form the second layer, and so on.

      queue  = [rdftype]
      strata = []
      seen   = Set.new

      while qin = queue.shift
        qwork = []

        qin.each do |q|
          qwork << q # entail doesn't include q
          qwork += q.entail(:equivalentClass)
        end

        # grep and flatten
        qwork = qwork.map { |t| RDF::Vocabulary.find_term t }.compact.uniq
        qwork -= seen.to_a
        seen |= qwork

        # push current layer out
        strata.push qwork if qwork.length > 0
     
        # now deal with subClassOf
        qsuper = []
        qwork.each { |q| qsuper += q.subClassOf }

        # grep and flatten this too
        qsuper = qsuper.map { |t| RDF::Vocabulary.find_term t }.compact.uniq
        qsuper -= seen.to_a
        seen |= qsuper

        # same deal, conditionally push the input queue
        queue.push qsuper if qsuper.length > 0
      end

      # voila
      strata
    end

    # Obtain all and only the rdf:types directly asserted on the subject.
    # 
    # @param subject [RDF::Resource]
    # @param type [RDF::Term, :to_a]
    #
    # @return [Array]

    def asserted_types subject, type = nil
      asserted = nil
      if type
        type = type.respond_to?(:to_a) ? type.to_a : [type]
        asserted = type.select { |t| t.is_a? RDF::Value }.map do |t|
          RDF::Vocabulary.find_term t
        end
      end
      asserted ||= @graph.query([subject, RDF.type, nil]).collect do |st|
        RDF::Vocabulary.find_term st.object
      end
      asserted.select { |t| t && t.uri? }.uniq
    end

    # Obtain the "best" dereferenceable URI for the subject.
    # Optionally returns all candidates.
    # 
    # @param subject [RDF::Resource]
    # @param unique [true, false] flag for unique return value
    # @param rdf    [true, false] flag to specify RDF::URI vs URI 
    #
    # @return [RDF::URI, URI, Array]

    def canonical_uri subject, unique: true, rdf: true
      subject = coerce_resource subject
      out = []

      [CI.canonical, RDF::OWL.sameAs].each do |p|
        o = @graph.query([subject, p, nil]).objects # collect { |st| st.object }
        out += o.sort { |a, b| cmp_resource(a, b) }
      end

      # try to generate 
      if out.empty? and subject.uri?
        uri = ::URI.parse subject.to_s
        if @base and uri.respond_to? :uuid
          b = @base.clone
          b.query = b.fragment = nil
          b.path = '/' + uri.uuid
          out << RDF::URI.new(b.to_s)
        end
      end

      out.map! { |u| URI(u.to_s) } unless rdf

      unique ? out.first : out.uniq
    end

    # Obtain the canonical UUID for the given URI
    #
    # @param uri [RDF::URI, URI, to_s]
    # @param unique [true, false]
    # 
    # @return [RDF::URI, Array]

    def canonical_uuid uri, unique: true
      uri = coerce_resource uri
      tu  = ::URI.parse uri.to_s
      # first we check if this is a subject uuid
      if tu.scheme == 'urn' and tu.nid == 'uuid'
        return unique ? uri : [uri] if @graph.first([uri, nil, nil])
      end

      # next we test for direct relation via ci:canonical and owl:sameAs
      candidates = ([CI.canonical, RDF::OWL.sameAs].reduce([]) do |a, p|
        a += @graph.query([uri, p, nil]).objects
      end + @graph.query(
        [nil, RDF::OWL.sameAs, uri]).subjects).uniq.select do |x|
        x.to_s.start_with? 'urn:uuid:'
      end.sort

      # return if we find anything
      return unique ? candidates[0] : candidates unless candidates.empty?

      # if the supplied URI has a path, we test the path slugs
      if tu.hierarchical?
        # construct a set of resources for each segment in the path,
        # testing first against ci:canonical-slug, then ci:slug
        segs = Pathname(tu.path).cleanpath.to_s.split(/\/+/).drop 1
        sets = segs.map do |segment|
          s = []
          [CI['canonical-slug'], CI.slug, RDF::Vocab::DC.identifier,
            RDF::Vocab::DC11.identifier].each do |p|
            [RDF::XSD.token, RDF::XSD.string].each do |t|
              s += @graph.query(
                [nil, p, RDF::Literal(segment, datatype: t)]).subjects
            end
          end
          Set.new s
        end

        # (at this point if there is not at least one resource in each
        # set, we return nil)
        return unique ? nil : [] if sets.empty? or sets.any? { |x| x.empty? }

        # all resources in the last column for which there is an
        # unbroken path from front to back are candidates

        candidates = []
        if sets.length == 1
          # shortcut
          candidates = sets[0].to_a
        else
          # build up a list of links between the ith and ith + 1
          # resource(s) matching each path segment slug; check in both
          # directions
          links = {}
          (1 .. sets.length).each do |i|
            sets[i - 1].each do |a|
              links[a] ||= Set.new [a]
              sets[i].each do |b|
                next if links[a].include? b
                [[a, nil, b], [b, nil, a]].each do |q|
                  if @graph.first(q)
                    links[a] << b
                    next
                  end
                end
              end
            end

            # this is the intersection of the preceding segment
            # candidate's links with the current segment's candidates
            c = links.values_at(*sets[i-1]).reduce(Set.new, :union) & sets[i]
            # if the set is empty, the pseudohierarchy is broken
            break if c.empty?

            # if we have made it to the end then this intersection is
            # our set of candidates
            candidates = c.to_a if i == sets.length
          end
        end

        return unique ? nil : [] if candidates.empty?

        # if the call was for unique, sort the UUIDs lexically (for now)
        # and return the first one
        candidates.sort!
        unique ? candidates[0] : candidates
      end
    end

    # Obtain the objects for a given subject-predicate pair.
    #
    # @param subject [RDF::Resource]
    # @param predicate [RDF::URI]
    # @param entail [false, true]
    # @param only [:uri, :iri, :resource, :blank, :bnode, :literal]
    # @param datatype [RDF::Term]
    #
    # @return [Array]

    def objects_for subject, predicate, entail: true, only: [], datatype: nil
      raise 'Subject must be a resource' unless subject.is_a? RDF::Resource
      predicate = predicate.respond_to?(:to_a) ? predicate.to_a : [predicate]
      raise 'Predicate must be some kind of term' unless
        predicate.all? { |p| p.is_a? RDF::URI }

      predicate = predicate.map { |x| RDF::Vocabulary.find_term x }.compact

      only = coerce_node_spec only

      datatype = (
        datatype.respond_to?(:to_a) ? datatype.to_a : [datatype]).compact
      raise 'Datatype must be some kind of term' unless
        datatype.all? { |p| p.is_a? RDF::URI }

      # XXX we really should wrap this up huh
      if entail
        i = 0
        loop do
          equiv = predicate[i].entail(:equivalentProperty) - predicate
          predicate.insert(i + 1, *equiv) unless equiv.empty?
          i += equiv.length + 1
          break if i >= predicate.length
        end
      end

      out = []
      predicate.each do |p|
        @graph.query([subject, p, nil]).each do |stmt|
          o = stmt.object

          # warn o

          # make sure it's in the spec
          next unless node_matches? o, only

          # constrain output
          next if o.literal? and
            !(datatype.empty? or datatype.include?(o.datatype))

          out.push o
        end
      end

      out.uniq
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

    def authors_for subject, unique: false, contrib: false
      authors = []

      # try the author list
      lp = [RDF::Vocab::BIBO[contrib ? :contributorList : :authorList]]
      lp += lp[0].entail(:equivalentProperty) # XXX cache this
      lp.each do |pred|
        o = @graph.first_object([subject, pred, nil])
        next unless o
        # note this use of RDF::List is not particularly well-documented
        authors += RDF::List.new(subject: o, graph: @graph).to_a
      end

      # now try various permutations of the author/contributor predicate
      unsorted = []
      preds = contrib ? CONTRIB : AUTHOR
      preds.each do |pred|
        unsorted += @graph.query([subject, pred, nil]).collect { |x| x.object }
      end

      authors += unsorted.uniq.sort { |a, b| label_for(a) <=> label_for(b) }

      unique ? authors.first : authors.uniq
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

    def label_for subject, candidates: nil, unique: true, type: nil,
        lang: nil, desc: false, alt: false
      return unless subject and subject.is_a? RDF::Value and subject.resource?
      # get type(s) if not supplied
      asserted = asserted_types subject, type

      # get all the inferred types by layer; add default class if needed
      strata = type_strata asserted
      strata.push [RDF::RDFS.Resource] if
        strata.length == 0 or not strata[-1].include?(RDF::RDFS.Resource)

      # get the key-value pairs for the subject
      candidates ||= struct_for subject, only: :literal

      seen  = {}
      accum = []
      strata.each do |lst|
        lst.each do |cls|
          next unless STRINGS[cls] and
            preds = STRINGS[cls][desc ? :desc : :label][alt ? 1 : 0]
          # warn cls
          preds.each do |p|
            # warn p.inspect
            next unless vals = candidates[p]
            vals.each do |v|
              pair = [p, v]
              accum.push(pair) unless seen[pair]
              seen[pair] = true
            end
          end
        end
      end

      # try that for now
      accum[0]

      # what we want to do is match the predicates from the subject to
      # the predicates in the label designation

      # get label predicate stack(s) for RDF type(s)

      # get all predicates in order (use alt stack if doubly specified)

      # filter out desired language(s)

      # XXX note we will probably want to return the predicate as well
    end

    SKOS_HIER = [
      {
        element: :subject,
        pattern: -> c, p { [nil, p, c] },
        preds: [RDF::Vocab::SKOS.broader,  RDF::Vocab::SKOS.broaderTransitive],
      },
      {
        element: :object,
        pattern: -> c, p { [c, p, nil] },
        preds: [RDF::Vocab::SKOS.narrower, RDF::Vocab::SKOS.narrowerTransitive],
      }
    ]
    SKOS_HIER.each do |struct|
      # lol how many times are we gonna cart this thing around
      preds = struct[:preds]
      i = 0
      loop do
        equiv = preds[i].entail(:equivalentProperty) - preds
        preds.insert(i + 1, *equiv) unless equiv.empty?
        i += equiv.length + 1;
        break if i >= preds.length
      end
    end

    def sub_concepts concept, extra: []
      raise 'Concept must be exactly one concept' unless
        concept.is_a? RDF::Resource
      extra = term_list extra

      # we need an array for a queue, and a set to accumulate the
      # output as well as a separate 'seen' set
      queue = [concept]
      seen  = Set.new queue.dup
      out   = seen.dup

      # it turns out that the main SKOS hierarchy terms, while not
      # being transitive themselves, are subproperties of transitive
      # relations which means they are as good as being transitive.

      while c = queue.shift
        SKOS_HIER.each do |struct|
          elem, pat, preds = struct.values_at(:element, :pattern, :preds)
          preds.each do |p|
            @graph.query(pat.call c, p).each do |stmt|
              # obtain hierarchical element
              hierc = stmt.send elem

              # skip any further processing if we have seen this concept
              next if seen.include? hierc
              seen << hierc

              next if !extra.empty? and !extra.any? do |t|
                @graph.has_statement? RDF::Statement.new(hierc, RDF.type, t)
              end

              queue << hierc
              out   << hierc
            end
          end
        end
      end

      out.to_a.sort
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

    # generate skos concept schemes

    # generate atom feed

    #

    def generate_atom_feed id, published: true
      raise 'ID must be a resource' unless id.is_a? RDF::Resource

      feed = struct_for id

      # find all UUIDs that are documents
      docs = all_of_type(RDF::Vocab::FOAF.Document).select do |x|
        x =~ /^urn:uuid:/
      end

      # prune out all but the published documents if specified
      if published
        p = RDF::Vocab::BIBO.status
        o = RDF::Vocabulary.find_term(
          'http://purl.org/ontology/bibo/status/published')
        docs = docs.select do |s|
          @graph.has_statement? RDF::Statement(s, p, o)
        end
      end

      # now we create a hash keyed by uuid containing the metadata
      authors = {}
      dates   = {}
      entries = {}
      docs.each do |uu|
        # basically make a jsonld-like structure
        #rsrc = struct_for uu

        # get id (got it already duh)
        
        # get canonical link
        # canon = ((rsrc[CI.canonical] || []) +
        #   (rsrc[RDF::OWL.sameAs] || [])).find(-> { uu }) { |x| x.resource? }
        # canon = URI.parse canon.to_s

        canon = URI.parse(canonical_uri(uu).to_s)
        
        xml = { '#entry' => [
          { '#link' => nil, rel: :alternate, href: canon, type: 'text/html' },
          { '#id' => uu.to_s }
        ] }

        # get published date first
        published = (objects_for uu,
                     [RDF::Vocab::DC.issued, RDF::Vocab::DC.created],
                     datatype: RDF::XSD.dateTime)[0]
        
        # get latest updated date
        updated = (objects_for uu, RDF::Vocab::DC.modified,
                   datatype: RDF::XSD.dateTime).sort[-1]
        updated ||= published || RDF::Literal::DateTime.new(DateTime.now)
        dates[uu] = updated = Time.parse(updated.to_s).utc

        xml['#entry'].push({ '#updated' => updated.iso8601 })
        if published
          published = Time.parse(published.to_s).utc
          xml['#entry'].push({ '#published' => published.iso8601 })
          dates[uu] = published
        end

        # get author(s)
        al = []
        authors_for(uu).each do |a|
          unless authors[a]
            n = label_for a
            x = authors[a] = { '#author' => [{ '#name' => n[1].to_s }] }

            hp = @graph.first_object [a, RDF::Vocab::FOAF.homepage, nil]
            hp ||= canonical_uri a
            
            x['#author'].push({ '#uri' => hp.to_s }) if hp
          end

          al.push authors[a]
        end

        xml['#entry'] += al unless al.empty?

        # get title (note unshift)
        if (t = label_for uu)
          xml['#entry'].unshift({ '#title' => t[1].to_s })
        end
        
        # get abstract
        if (d = label_for uu, desc: true)
          xml['#entry'].push({ '#summary' => d[1].to_s })
        end
        
        entries[uu] = xml
      end

      # note we overwrite the entries hash here with a sorted array
      entries = entries.values_at(
        *entries.keys.sort { |a, b| dates[a] <=> dates[b] })
      # ugggh god forgot the asterisk and lost an hour

      # now we punt out the file

      preamble = [
        # { '#title' =>
        # { id = id.to_s },
        
      ]

      markup(spec: { '#feed' => preamble + entries,
        xmlns: 'http://www.w3.org/2005/Atom' }).document
    end

    # generate sass palettes

    # generate rewrite map(s)
    def generate_rewrite_map published: true
    end

    # give me all UUIDs of all documents, filter for published if
    # applicable
    # 
    # find the "best" (relative) URL for the UUID and map the pair
    # together
    def generate_uuid_redirect_map published: true
      
    end

    # find all URIs/slugs that are *not* canonical, map them to slugs
    # that *are* canonical
    def generate_slug_redirect_map published: true
    end

    # you know what, it's entirely possible that these ought never be
    # called individually and the work to get one would duplicate the
    # work of getting the other, so maybe just do 'em both at once

    def generate_redirect_map published: true
      generate_uuid_redirect_map(published) +
        generate_slug_redirect_map(published)
    end

    def generate_gone_map published: true
    end

    def generate_rewrite_maps published: true
      # slug to uuid (internal)
      # uuid/slug to canonical slug (308)
      # retired slugs/uuids (410)
    end

    # - io stuff -

    # Locate the file in the source directory associated with the given URI.
    #
    # @param [RDF::URI, URI, :to_s] the URI requested
    # 
    # @return [Pathname] of the corresponding file or nil if no file was found.

    def locate uri
      uri = coerce_resource uri

      base = URI(@base.to_s)

      tu = URI(uri) # copy of uri for testing content
      unless tu.scheme == 'urn' and tu.nid == 'uuid'
        raise "could not find UUID for #{uri}" unless uuid = canonical_uuid(uri)
        tu = URI(uri = uuid)
      end

      # xxx bail if the uri isn't a subject in the graph

      candidates = [@config[:source] + tu.uuid]
      (canonical_uri uri, unique: false).each do |u|
        u = URI(u.to_s)
        next unless u.hostname == base.hostname
        p = u.path[/^\/*(.*?)$/, 1]
        candidates.push(@config[:source] + p)
      end

      files = candidates.uniq.map do |c|
        Pathname.glob(c.to_s + '{,.*,/index{,.*}}')
      end.reduce(:+).uniq

      # XXX implement negotiation algorithm
      return files[0]

      # return the filename from the source
      # nil
    end

    # Visit (open) the document at the given URI.
    # 
    # @param uri [RDF::URI, URI, :to_s]
    # 
    # @return [RDF::SAK::Context::Document] or nil

    def visit uri
      path = locate uri
      return unless path
      Document.new self, path, uri, uri: canonical_uri(uri)
    end

    # read from source

    # write (manipulated (x|x?ht)ml) back to source

    # write public and private variants to target

    def write_xhtml published: true
    end

    # write modified rdf

    # - internet stuff -

    # verify external links for upness

    # collect triples for external links

    # fetch references for people/companies/concepts/etc from dbpedia/wikidata

    # - document context class -

    class Document
      include XML::Mixup

      private

      C_OK = [Nokogiri::XML::Node, IO, Pathname].freeze

      XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze

      public

      attr_reader :doc, :uuid

      def initialize context, doc, uuid, uri: nil
        raise 'context must be a RDF::SAK::Context' unless
          context.is_a? RDF::SAK::Context
        raise 'doc must be Pathname, IO, or Nokogiri node' unless
          C_OK.any? { |c| doc.is_a? c } || doc.respond_to?(:to_s)
        raise 'uuid must be an RDF::URI' unless
          uuid.is_a? RDF::URI and uuid.to_s.start_with? 'urn:uuid:'

        @context = context
        @uuid    = uuid

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
            type = RDF::SAK::MimeMagic.by_path doc
            doc  = doc.open # this may raise if the file isn't there
          end

          # squash everything else to a string
          doc = doc.to_s unless doc.is_a? IO

          # check type by content
          type ||= RDF::SAK::MimeMagic.by_magic(doc)

          # can you believe there is a special bookmarks mime type good grief
          type = 'text/html' if type == 'application/x-mozilla-bookmarks'

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
          elsif type =~ /^text\/(?:plain|(?:x-)?markdown)/i
            # just assume plain text is markdown
            doc = ::MD::Noko.new.ingest doc
          else
            raise "Don't know what to do with #{type}"
          end
        end

        # now fix the namespaces for mangled html documents
        root = doc.root
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

        @uri = URI(uri || @context.canonical_uri(uuid))

        # voilà
        @doc = doc
      end

      def base_for doc: nil
        doc ||= @doc
        base = @uri
        if doc.root.name.to_sym == :html
          base = doc.at_xpath(
            '(/html:html/html:head/html:base[@href])[1]/@href',
            { html: XHTMLNS })
        elsif doc.root['xml:base']
          base = doc.root['xml:base']
        end

        URI(base)
      end

      # notice these are only RDFa attributes that take URIs
      RDFA_ATTR  = [:about, :resource, :typeof].freeze
      LINK_ATTR  = [:href, :src, :data, :action, :longdesc].freeze
      LINK_XPATH = ('//html:*[not(self::html:base)][%s]' %
        (LINK_ATTR + RDFA_ATTR).map { |a| "@#{a.to_s}" }.join('|')).freeze

      def rewrite_links
        base  = base_for
        count = 0
        cache = {}
        @doc.xpath(LINK_XPATH, { html: XHTMLNS }).each do |elem|
          LINK_ATTR.each do |attr|
            attr = attr.to_s
            next unless elem.has_attribute? attr

            abs = base.merge elem[attr].strip
            if abs.host == @uri.host and abs.scheme != @uri.scheme
              tmp          = @uri.dup
              tmp.path     = abs.path
              tmp.query    = abs.query
              tmp.fragment = abs.fragment
              abs          = tmp
            end
            abs = cache[abs] ||= @context.canonical_uri(abs, rdf: false) || abs
            elem[attr] = @uri.route_to(abs).to_s
            count += 1
          end
        end

        count
      end

      # sponge the document for rdfa
      def triples_for
      end

      # give us the rdf subject of the node itself
      def subject_for node: nil, is_ancestor: false
        node ||= @doc.root
        raise 'Node must be an element' unless
          node.is_a? Nokogiri::XML::Element

        # first we check for an ancestor element with @property and no
        # @content; if we find one then we reevaluate with that
        # element as the starting point
        if n = node.at_xpath('(ancestor::*[@property][not(@content)])[1]')
          return subject_for n
        end

        subject = nil
        is_root = !node.parent or node.parent.document?

        # if parent is false we first check for an @about
        if is_ancestor
          [:resource, :href, :src].each do |attr|
            if node.key? attr
              subject = node[attr]
              break
            end
          end

          subject = node[:about] if subject.nil? and node.key? :about
        else
          # 
          subject = node[:about] if node.key? :about
          # now we need to check @inlist and @typeof

          # @typeof is easy because if it's there and @about isn't
          # then the subject is a bnode (we'll generate-id(@typeof)
          # and derive the bnode ID from that)

          # @inlist on the other hand is hard, because we want the
          # actual list node

        end

        # XXX actually no we don't want this. subject definitions on
        # the current node should only ever come from @about or blank
        # nodes if a @typeof or @inlist is present. the only time
        # @resource/@href/@src ought to be considered is when parent
        # is true (ie we are scanning an ancestor of the initial
        # node).

        # also maybe we should memoize this shit

        # then we check @resource, @href, @src
        if subject.nil? and is_ancestor
          [:resource, :href, :src].each do |attr|
            if node.key? attr
              subject = node[attr]
              break
            end
          end

          # if parent is true, *now* we check for @about
          if subject.nil? and node.key? :about
            subject = node[:about]
          end
        end

        # if there is still no viable subject, check if the element is
        # the root, and if so, treat it as if there is an empty @about
        # (ie use base href)
        subject = @uri if subject.nil? and is_root

        # if there is still no viable subject, then before proceeding,
        # check if this is an (x)html head or body element, in which
        # case short-circuit to the parent node
        if subject.nil? and [:head, :body].include? node.name.to_sym
          subject_for node.parent, true
        end

        # (blank node time)

        # if there are none of those, we check for a @typeof

        # if there is no @typeof, check for @inlist

        return subject if subject

        # if we couldn't find anything suitable as a subject, recurse to
        # the parent node

        subject_for(node.parent, true) if node.parent and node.parent.element?
      end

      # backlink structure
      def generate_backlinks published: true
        
      end

      def write_to_target published: true
        uuid = URI(@uuid.to_s)
        # obtain target directory
        target = @context.config[published ? :target : :private]
        struct = @context.struct_for @uuid

        resources = {}
        literals  = {}
        uuids     = {}
        types     = Set.new
        title     = (@context.label_for @uuid, candidates: struct)[1]

        struct.each do |k, v|
          v.each do |x|
            if x.literal?
              literals[x] ||= Set.new
              literals[x].add k
            else
              resources[x] ||= Set.new
              resources[x].add k
              # harvest proper URIs
              uuids[x] ||= @context.canonical_uri(x) if
                x.to_s.start_with? 'urn:uuid:'
              # add to type
              types.add x if k == RDF::RDFV.type
            end
          end
        end
        urev = uuids.invert

        labels = resources.keys.map do |k|
          [k, @context.label_for(k) || []]
        end.to_h

        xhv = 'http://www.w3.org/1999/xhtml/vocab#'

        # XXX abbreviae
        #tp    = (literals[title] || []).map { |p| ns.abbreviate(p) }.sort
        tdt   = @context.abbreviate(title.datatype)
        tl    = title.language
        title = { '#title' => title,
          property: @context.abbreviate(literals[title].to_a, vocab: xhv) }
        if tl
          title['xml:lang'] = tl # if xmlns
          title['lang'] = tl
        elsif tdt
          title[:datatype] = tdt
        end

        body = @doc.at_css('body').dup 1
        # attrs = body.attribute_nodes.map { |a| a
        body = { '#body' => body.children.to_a }

        # XXX abbreviate
        body[:typeof] = @context.abbreviate(types.to_a, vocab: xhv) unless
          types.empty?

        pfx = @context.prefixes.transform_values { |v| v.to_s }

        doc = xhtml_stub(
          base: @uri, prefix: pfx, vocab: xhv, lang: 'en', title: title, 
          transform: '/transform', body: body).document

        doc.xpath(LINK_XPATH, { html: XHTMLNS }).each do |elem|
          vocab = elem.at_xpath('ancestor-or-self::*[@vocab][1]/@vocab')
          vocab = URI(vocab.to_s) if vocab
          if elem.key?('href') or elem.key?('src')
            vu = elem['href'] || elem['src']
            tu = RDF::URI(@uri.merge(vu))
            label = labels[urev[tu] || tu]
            if label and (!elem.key?('title') or elem['title'].strip == '')
              elem['title'] = label[1].to_s
            end
          end
        end

        # we assume the directory has already been verified to be
        # present and writable, so try opening a write handle
        begin
          fh = Tempfile.create('xml', target)
          path = Pathname(fh.path)

          # write the doc to the target
          doc.write_to fh
          fh.close

          newpath = path.dirname + "#{uuid.uuid}.xml"

          File.chmod(0644, path)
          File.rename(path, newpath)
        rescue
          File.unlink path
          return
        end

        newpath
      end
    end
  end

  class CLI
    # This is a command-line interface

    include XML::Mixup
    include Commander::Methods

    # bunch of data declarations etc we don't want to expose
    private

    # actual methods
    public

    # constructor

    # configuration:

    # directories: source, target, private
    # files (or file names): graph, rewrite_map, redirect_map, gone_map
    # URIs: base, aliases

    def initialize config: {}
    end

    # vestigial

    def run
      run!
    end
  end
end
