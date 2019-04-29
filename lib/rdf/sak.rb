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

# my stuff
require 'xml-mixup'
require 'md-noko'
require 'uuid-ncname'

# ontologies, mine in particular
require 'rdf/sak/ci'
require 'rdf/sak/ibis'
# others not included in rdf.rb
require 'rdf/sak/pav'

# uhh why don't we just cool it with this mkay

# XXX REMOVE THIS ONCE REASONER >0.5.2 RELEASED
module RDF::Reasoner::RDFS
  def self.included(mod)
    mod.add_entailment :subClassOf, :_entail_subClassOf
    mod.add_entailment :subClass, :_entail_subClass
    mod.add_entailment :subProperty, :_entail_subProperty
    mod.add_entailment :subPropertyOf, :_entail_subPropertyOf
    mod.add_entailment :domain, :_entail_domain
    mod.add_entailment :range, :_entail_range
  end

  def descendant_property_cache
    @@descendant_property_cache ||= RDF::Util::Cache.new(-1)
  end

  def subProperty_cache
    @@subProperty_cache ||= RDF::Util::Cache.new(-1)
  end

  def subProperty
    raise RDF::Reasoner::Error,
      "#{self} Can't entail subProperty" unless property?
    vocabs = [self.vocab] + self.vocab.imported_from
    subProperty_cache[self] ||= vocabs.map do |v|
      Array(v.properties).select do |p|
        p.property? && Array(p.subPropertyOf).include?(self)
      end
    end.flatten.compact
  end

  def _entail_subProperty
    case self
    when RDF::URI, RDF::Node
      unless property?
        yield self if block_given?
        return Array(self)
      end

      terms = descendant_property_cache[self] ||= (
        Array(self.subProperty).map do |c|
          c._entail_subProperty rescue c
        end.flatten + Array(self)).compact

      terms.each {|t| yield t } if block_given?
      terms
    else []
    end
  end
end

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

    UUID_RE = /^(?:urn:uuid:)?([0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8})$/i

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
      # config must either be a hash or a file name/pathname/io object
      unless config.respond_to? :to_h
        # when in rome
        require 'yaml'
        config = if config.is_a? IO
                   YAML.load config
                 else 
                   YAML.load_file Pathname.new(config).expand_path
                 end
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
      if config[:graph]
        g = config[:graph]
        g = [g] unless g.is_a? Array
        config[:graph] = g.map { |x| Pathname.new(x).expand_path }
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

      # rewrite maps
      config[:maps] = {} unless config[:maps].is_a? Hash
      %w(rewrite redirect gone).each do |type|
        config[:maps][type.to_sym] ||= ".#{type}.map"
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

    # 
    def coerce_node_spec spec, rev: false
      spec = [spec] unless spec.respond_to? :to_a
      spec = spec - [:resource] + [:uri, :blank] if spec.include? :resource
      raise 'Subjects are never literals' if rev and spec.include? :literal

      spec = NMAP.values_at(*spec).reject(&:nil?).uniq
      spec = NTESTS.keys if spec.empty?
      spec.delete :literal if rev
      spec.uniq
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

    def abbreviate term, vocab: nil, noop: true, sort: true
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
          rev.sort { |a, b| b[0].length <=> a[0].length }.each do |k, v|
            if (vt = t.delete_prefix(k)) != t
              vt = '%s:%s' % [v, vt]
              break
            else
              vt = nil
            end
          end
        end
        vt ||= t if noop
        vt
      end

      # only sort if noop is set
      term.sort! if noop && sort

      scalar ? term[0] : term
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
        out += @graph.query([nil, RDF.type, type]).subjects
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
        o = @graph.query([subject, p, nil]).objects
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

    # Find the terminal replacements for the given subject, if any exist.
    # 
    #
    #
    # @param subject
    # @param published indicate the context is published
    #
    # @return [Set]
    def replacements_for subject, published: true
      subject = coerce_resource subject

      # `seen` is a hash mapping resources to publication status and
      # subsequent replacements. it collects all the resources in the
      # replacement chain in :fwd (replaces) and :rev (replaced-by)
      # members, along with a boolean :pub. `seen` also performs a
      # duty as cycle-breaking sentinel.

      seen  = {}
      queue = [subject]
      while (test = queue.shift)
        # fwd is "replaces", rev is "replaced by"
        entry = seen[test] ||= {
          pub: published?(test), fwd: Set.new, rev: Set.new }
        queue += (
          subjects_for(RDF::Vocab::DC.replaces, subject) +
            objects_for(subject, RDF::Vocab::DC.isReplacedBy, only: :resource)
        ).uniq.map do |r| # r = replacement
          next if seen.include? r
          seen[r] ||= { pub: published?(r), fwd: Set.new, rev: Set.new }
          seen[r][:fwd] << test
          entry[:rev] << r
          r
        end.compact.uniq
      end

      # if we're calling from a published context, we return the
      # (topologically) last published resource(s), even if they are
      # replaced ultimately by unpublished resources.
      
      out = seen.map { |k, v| v[:rev].empty? ? k : nil }.compact - [subject]

      # now we modify `out` based on the publication status of the context
      if published
        pubout = out.select { |o| seen[o][:pub] }
        # if there is anything left after this, return it
        return pubout unless pubout.empty?
        # now we want to find the penultimate elements of `seen` that
        # are farthest along the replacement chain but whose status is
        # published

        # start with `out`, take the union of their :fwd members, then
        # take the subset of those which are published. if the result
        # is empty, repeat. (this is walking backwards through the
        # graph we just walked forwards through to construct `seen`)
        loop do
          # XXX THIS NEEDS A TEST CASE
          out = seen.values_at(*out).map { |v| v[:fwd] }.reduce(:+).to_a
          break if out.empty?
          pubout = out.select { |o| seen[o][:pub] }
          return pubout unless pubout.empty?
        end
      end

      out

    end

    # Get the last non-empty path segment of the URI
    #
    # @param uri
    #
    # @return [String]

    def terminal_slug uri
      uri = coerce_resource uri
      if p = uri.path
        if p = /^\/+(.*?)\/*$/.match(p)
          if p = p[1].split(/\/+/)[-1]
            return p.split(/;+/)[0] || ''
          end
        end        
      end
      ''
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
      objects_for(subject, predicate, only: :literal, datatype: datatype) do |o|
        o.object
      end.sort.uniq
    end

    # Obtain the canonical UUID for the given URI
    #
    # @param uri [RDF::URI, URI, to_s]
    # @param unique [true, false]
    # 
    # @return [RDF::URI, Array]

    # it is so lame i have to do this
    BITS = { nil => 0, false => 0, true => 1 }

    def soon_canonical_uuid uri, unique: true, published: false
      # make sure this is actually a uri
      uri = coerce_resource uri
      tu  = URI(uri.to_s)

      # now check if it's a uuid
      if tu.respond_to? :uuid
        # if it's a uuid, check that we have it as a subject
        # if we have it as a subject, return it
        return uri if @context.graph.has_subject? uri
        # note i don't want to screw around right now dealing with the
        # case that a UUID might not itself be canonical
      end

      # otherwise we proceed:

      # goal: return the most "appropriate" UUID for the given URI

      # rank (0 is higher):
      # * (00) exact & canonical == 0,
      # * (01) exact == 1,
      # * (10) inexact & canonical == 2,
      # * (11) inexact == 3. 

      # collect the candidates by URI
      sa = predicate_set [RDF::SAK::CI.canonical, RDF::OWL.sameAs]
      candidates = subjects_for(sa, uri, entail: false) do |s, f|
        # there is no #to_i for booleans and also we xor this number
        [s, { rank: BITS[f.include?(RDF::SAK::CI.canonical)] ^ 1,
          published: published?(s), mtime: dates_for(s).last || DateTime.new }]
      end.to_h

      # now collect by slug
      if (slug = terminal_slug uri) != ''
        exact = uri == coerce_resource(slug) # slug represents exact match
        sl = [RDF::SAK::CI['canonical-slug'], RDF::SAK::CI.slug]
        [RDF::XSD.string, RDF::XSD.token].each do |t|
          subjects_for(sl, RDF::Literal(slug, datatype: t)) do |s, f|
            # default to lowest rank if this candidate is new
            entry = candidates[s] ||= { rank: 0b11, published: published?(s),
              mtime: dates_for(s).last || DateTime.new }
            # true is 1 and false is zero so we xor this too
            rank  = (BITS[exact] << 1 | BITS[f.include?(sl[0])]) ^ 0b11
            # now amend the rank if we have found a better one
            entry[:rank] = rank if rank < entry[:rank]
          end
        end
      end

      # scan all the candidates for replacements and remove any
      # candidates that have been replaced
      candidates.to_a.each do |k, v|
        # note that 
        reps = replacements_for(k, published: published) - [k]
        unless reps.empty?
          v[:replaced] = true
          reps.each do |r|
            c = candidates[r] ||= { rank: v[:rank], published: published?(r),
              mtime: dates_for(r).last || v[:mtime] || DateTime.new }
            # we give the replacement the rank and mtime of the
            # resource being replaced if it scores better
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

      # now we sort by rank and date; the highest-ranking newest
      # candidate is the one

      out = candidates.sort do |a, b|
        _, va = a
        _, vb = b
        cb = published ? BITS[vb[:published]] <=> BITS[va[:published]] : 0
        cr = va[:rank] <=> vb[:rank]
        cb == 0 ? cr == 0 ? vb[:mtime] <=> va[:mtime] : cr : cb
      end.map { |x| x.first }.compact

      unique ? out.first : out

      # an exact match is better than an inexact one

      # a canonical match is better than non-canonical

      # note this is four bits: exact, canon(exact), inexact, canon(inexact)
      # !canon(exact) should rank higher than canon(inexact)

      # unreplaced is better than replaced

      # newer is better than older (though no reason an older item
      # can't replace a newer one)

      # published is better than not, unless the context is
      # unpublished and an unpublished document replaces a published one
    end

    def canonical_uuid uri, unique: true
      uri = coerce_resource uri
      tu  = ::URI.parse uri.to_s
      # first we check if this is a subject uuid
      if tu.scheme == 'urn' and tu.nid == 'uuid'
        return unique ? uri : [uri] if @graph.first([uri, nil, nil])
      elsif (m = UUID_RE.match(tu.path.delete_prefix('/')))
        xu = RDF::URI('urn:uuid:' + m[1])
        return unique ? xu : [xu] if @graph.first([xu, nil, nil])
      end

      # next we test for direct relation via ci:canonical and owl:sameAs
      candidates = ([CI.canonical, RDF::OWL.sameAs].reduce([]) do |a, p|
        a += @graph.query([nil, p, uri]).subjects
      end + @graph.query(
        [uri, RDF::OWL.sameAs]).objects).uniq.select do |x|
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

    def predicate_set predicates, seen: Set.new
      predicates = Set[predicates] if predicates.is_a? RDF::URI
      unless predicates.is_a? Set
        raise "predicates must be a set" unless predicates.respond_to? :to_set
        predicates = predicates.to_set
      end

      # shortcut
      return predicates if predicates.empty?

      raise 'predicates must all be RDF::URI' unless predicates.all? do |p|
        p.is_a? RDF::URI
      end

      # first we generate the set of equivalent properties for the given
      # properties
      predicates += predicates.map do |p|
        p.entail :equivalentProperty
      end.flatten.to_set

      # then we take the resulting set of properties and
      # compute their subproperties
      subp = Set.new
      (predicates - seen).each do |p|
        subp += p.subProperty.flatten.to_set
      end

      # uhh this whole "seen" business might not be necessary
      predicates + predicate_set(subp - predicates - seen, seen: predicates)
    end

    # Returns subjects from the graph with entailment.
    #
    # @param predicate
    # @param object
    # @param entail
    # @param only
    #
    # @return [RDF::Resource]
    def subjects_for predicate, object, entail: true, only: []
      raise 'Subject must be a Term' unless object.is_a? RDF::Term
      predicate = predicate.respond_to?(:to_a) ? predicate.to_a : [predicate]
      raise 'Predicate must be some kind of term' unless
        predicate.all? { |p| p.is_a? RDF::URI }

      only = coerce_node_spec only, rev: true

      predicate = predicate.map { |x| RDF::Vocabulary.find_term x }.compact
      predicate = predicate_set predicate if entail

      out  = {}
      revp = Set.new
      predicate.each do |p|
        @graph.query([nil, p, object]).subjects.each do |s|
          next unless node_matches? s, only

          entry = out[s] ||= [Set.new, Set.new]
          entry[0] << p
        end

        # do this here while we're at it
        unless object.literal?
          revp += p.inverseOf.to_set
          revp << p if p.type.include? RDF::OWL.SymmetricProperty
        end
      end

      unless object.literal?
        revp = predicate_set revp if entail

        revp.each do |p|
          @graph.query([object, p, nil]).objects.each do |o|
            next unless node_matches? o, only

            entry = out[o] ||= [Set.new, Set.new]
            entry[1] << p
          end
        end
      end

      # run this through a block to get access to the predicates
      return out.map { |p, v| yield p, *v } if block_given?

      out.keys
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

      # fluff this out 
      predicate = predicate_set predicate if entail

      out = {}
      predicate.each do |p|
        @graph.query([subject, p, nil]).objects.each do |o|

          # make sure it's in the spec
          next unless node_matches? o, only

          # constrain output
          next if o.literal? and
            !(datatype.empty? or datatype.include?(o.datatype))

          entry = out[o] ||= [Set.new, Set.new]
          entry[0] << p
        end
      end

      # now we do the reverse
      unless only == [:literal]
        # generate reverse predicates
        revp = Set.new
        predicate.each do |p|
          revp += p.inverseOf.to_set
          revp << p if p.type.include? RDF::OWL.SymmetricProperty
        end
        revp = predicate_set revp if entail

        # now scan 'em
        revp.each do |p|
          @graph.query([nil, p, subject]).subjects.each do |s|
            next unless node_matches? s, only
            # no need to check datatype; subject is never a literal

            entry = out[s] ||= [Set.new, Set.new]
            entry[1] << p
          end
        end
      end

      # run this through a block to get access to the predicates
      return out.map { |p, v| yield p, *v } if block_given?

      out.keys
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

    def audiences_for uuid, proximate: false, invert: false
      p = invert ? CI['non-audience'] : RDF::Vocab::DC.audience
      return @graph.query([uuid, p, nil]).objects if proximate

      out = []
      @graph.query([uuid, p, nil]).objects.each do |o|
        out += sub_concepts o
      end

      out
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
    def all_internal_docs published: true
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
      
      docs
    end

    def generate_atom_feed id, published: true, related: []
      raise 'ID must be a resource' unless id.is_a? RDF::Resource

      # prepare relateds
      raise 'related must be an array' unless related.is_a? Array
      related -= [id]

      # feed = struct_for id

      faudy = audiences_for id
      faudn = audiences_for id, invert: true
      faudy -= faudn

      docs = all_internal_docs published: published

      # now we create a hash keyed by uuid containing the metadata
      authors = {}
      titles  = {}
      dates   = {}
      entries = {}
      latest  = nil
      docs.each do |uu|
        # basically make a jsonld-like structure
        #rsrc = struct_for uu

        indexed = objects_for uu, RDF::SAK::CI.indexed, only: :literal
        next if !indexed.empty? and indexed.any? { |f| f == false }

        # get id (got it already duh)
        
        # get audiences
        audy = audiences_for uu, proximate: true
        audn = audiences_for uu, proximate: true, invert: true

        #warn "#{faudy.to_s} & #{faud"

        skip = false
        if audy.empty?
          # an unspecified audience implies "everybody", but if the
          # feed's audience *is* specified, then it's not for everybody
          skip = true unless faudy.empty?
        else
          # if document audience matches feed non-audience, disqualify
          skip = true unless (faudn & audy).empty?

          # absence of an explicit feed audience implies "everybody"
          if faudy.empty?
            # if document audience minus feed non-audience has
            # members, re-qualify
            skip = false unless (audy - faudn).empty?
          else
            # if document audience matches feed audience, re-qualify
            skip = false unless (faudy & audy).empty?
          end
        end

        # if document non-audience matches feed audience, re-disqualify
        skip = true if !(audn.empty? || faudy.empty?) && !(faudy & audn).empty?

        next if skip

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
        updated = Time.parse(updated.to_s).utc
        latest = updated if !latest or latest < updated

        xml['#entry'].push({ '#updated' => updated.iso8601 })

        if published
          published = Time.parse(published.to_s).utc
          xml['#entry'].push({ '#published' => published.iso8601 })
          dates[uu] = [published, updated]
        else
          dates[uu] = [updated, updated]
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
          titles[uu] = t[1].to_s
          xml['#entry'].unshift({ '#title' => t[1].to_s })
        else
          titles[uu] = uu.to_s
        end
        
        # get abstract
        if (d = label_for uu, desc: true)
          xml['#entry'].push({ '#summary' => d[1].to_s })
        end
        
        entries[uu] = xml
      end

      # note we overwrite the entries hash here with a sorted array
      entrycmp = -> a, b {
        # first we sort by published date
        p = dates[a][0] <=> dates[b][0]
        # if the published dates are the same, sort by updated date
        u = dates[a][1] <=> dates[b][1]
        # to break any ties, finally sort by title
        p == 0 ? u == 0 ? titles[a] <=> titles[b] : u : p }
      entries = entries.values_at(
        *entries.keys.sort { |a, b| entrycmp.call(a, b) })
      # ugggh god forgot the asterisk and lost an hour

      # now we punt out the doc

      preamble = [
        { '#id' => id.to_s },
        { '#updated' => latest.iso8601 },
        { '#generator' => 'RDF::SAK', version: RDF::SAK::VERSION,
          uri: "https://github.com/doriantaylor/rb-rdf-sak" },
        { nil => :link, rel: :self, type: 'application/atom+xml',
          href: canonical_uri(id) },
        { nil => :link, rel: :alternate, type: 'text/html',
          href: @base },
      ] + related.map do |r|
        { nil => :link, rel: :related, type: 'application/atom+xml',
         href: canonical_uri(r) }
      end

      if (t = label_for id)
        preamble.unshift({ '#title' => t[1].to_s })
      end

      if (r = @graph.first_literal [id, RDF::Vocab::DC.rights, nil])
        rh = { '#rights' => r.to_s, type: :text }
        rh['xml:lang'] = r.language if r.has_language?
        preamble.push rh
      end

      markup(spec: { '#feed' => preamble + entries,
        xmlns: 'http://www.w3.org/2005/Atom' }).document
    end

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

    # generate rewrite map(s)
    def generate_rewrite_map published: false, docs: nil
      docs ||= all_internal_docs published: published
      base = URI(@base.to_s)
      rwm  = {}
      docs.each do |doc|
        tu = URI(doc.to_s)
        cu = canonical_uri doc, rdf: false
        next unless tu.respond_to?(:uuid) and cu.respond_to?(:request_uri)

        # skip external links obvs
        next unless base.route_to(cu).relative?

        # skip /uuid form
        cp = cu.request_uri.delete_prefix '/'
        next if cu.host == base.host and tu.uuid == cp
        
        rwm[cp] = tu.uuid
      end

      rwm
    end

    # give me all UUIDs of all documents, filter for published if
    # applicable
    # 
    # find the "best" (relative) URL for the UUID and map the pair
    # together
    def generate_uuid_redirect_map published: false, docs: nil
      docs ||= all_internal_docs published: published

      base = URI(@base.to_s)

      # keys are /uuid, values are 
      out = {}
      docs.each do |doc|
        tu = URI(doc.to_s)
        cu = canonical_uri doc, rdf: false
        next unless tu.respond_to?(:uuid) and cu.respond_to?(:request_uri)

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
      docs ||= all_internal_docs published: published

      out = {}

      docs.each do |doc|
        uris  = canonical_uri doc, unique: false, rdf: false
        if uris.length > 1
          canon = uris.shift
          next unless canon.respond_to? :request_uri

          uris.each do |uri|
            next unless uri.respond_to? :request_uri
            next if canon == uri
            uri = uri.request_uri.delete_prefix '/'

            # remove uuids as we already have them
            next if uri =~ /^[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$/
            out[uri] = canon.to_s
          end
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
      docs ||= all_internal_docs published: false
      p    = RDF::Vocab::BIBO.status
      base = URI(@base.to_s)
      out  = {}
      docs.select { |s|
        @graph.has_statement? RDF::Statement(s, p, CI.retired) }.each do |doc|
        canon = canonical_uri doc, rdf: false
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
      fh = File.new location, 'w'
      data.sort.each { |k, v| fh.write "#{k}\t#{v}\n" }
      fh.close # return value is return value from close
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
      docs ||= all_internal_docs published: false
      # slug to uuid (internal)
      write_rewrite_map docs: docs
      # uuid/slug to canonical slug (308)
      write_redirect_map docs: docs
      # retired slugs/uuids (410)
      write_gone_map docs: docs
      true
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
      end.reduce(:+).reject do |x|
        x.directory? or
          RDF::SAK::MimeMagic.by_path(x).to_s !~ /.*(?:x?ht|x)ml.*/i
      end.uniq

      warn files

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
      uri  = canonical_uuid uri
      path = locate uri
      return unless path
      Document.new self, uri, uri: canonical_uri(uri), doc: path
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

      until (out = canonical_uuid uri)
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
    def published? uri, circulated: false
      uri = coerce_resource uri
      candidates = objects_for(
        uri, RDF::Vocab::BIBO.status, only: :resource).to_set

      test = Set[RDF::Vocab::BIBO['status/published']]
      test << RDF::SAK::CI.circulated if circulated

      # warn candidates, test, candidates & test

      !(candidates & test).empty?
    end

    # Find a destination pathname for the document
    #
    # @param uri
    # @param published
    # 
    # @return [Pathname]
    def target_for uri, published: false
      uri = coerce_resource uri
      uri = canonical_uuid uri
      target = @config[published?(uri) && published ? :target : :private]

      # target is a pathname so this makes a pathname
      target + "#{URI(uri.to_s).uuid}.xml"
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

      XHV = 'http://www.w3.org/1999/xhtml/vocab#'
      QF  = /^([^?#]*)(?:\?([^#]*))?(?:#(.*?))?$/
      SF  = /[^[:alpha:][:digit:]\/\?:@!$&'()*+,;=._~-]/

      # need this URI preprocessor because uri.rb is dumb
      def uri_pp uri
        m = QF.match uri.to_s
        out = m[1]
        [[2, ??], [3, ?#]].each do |i, c|
          next if m[i].nil?
          clean = m[i].gsub(SF) { |s| sprintf('%X', s.ord) }
          out += c + clean
        end

        out
      end

      public

      attr_reader :doc, :uuid

      def initialize context, uuid, doc: nil, uri: nil, mtime: nil
        raise 'context must be a RDF::SAK::Context' unless
          context.is_a? RDF::SAK::Context
        raise 'uuid must be an RDF::URI' unless
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
            raise "Don't know what to do with #{uuid} (#{type})"
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

        # aaand set some more instance variables

        @uri = URI(uri || @context.canonical_uri(uuid))

        # voilà
        @doc = doc
      end

      # proxy for context published
      def published?
        @context.published? @uuid
      end

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
      RDFA_ATTR  = [:about, :resource, :typeof].freeze
      LINK_ATTR  = [:href, :src, :data, :action, :longdesc].freeze
      LINK_XPATH = ('.//html:*[not(self::html:base)][%s]' %
        (LINK_ATTR + RDFA_ATTR).map { |a| "@#{a.to_s}" }.join('|')).freeze

      def rewrite_links node = @doc, &block
        base  = base_for node
        count = 0
        cache = {}
        node.xpath(LINK_XPATH, { html: XHTMLNS }).each do |elem|
          LINK_ATTR.each do |attr|
            attr = attr.to_s
            next unless elem.has_attribute? attr

            abs = base.merge uri_pp(elem[attr].strip)
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

          block.call elem if block
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

      # goofy twitter-specific metadata
      def generate_twitter_meta
        # get author
        author = @context.authors_for(@uuid, unique: true) or return

        # get author's twitter account
        twitter = @context.objects_for(author, RDF::Vocab::FOAF.account,
                                       only: :resource).select do |t|
          t.to_s =~ /twitter\.com/
        end.sort.first or return
        twitter = URI(twitter.to_s).path.split(/\/+/)[1]
        twitter = ?@ + twitter unless twitter.start_with? ?@

        # get title
        title = @context.label_for(@uuid) or return

        out = [
          { nil => :meta, name: 'twitter:card', content: :summary },
          { nil => :meta, name: 'twitter:site', content: twitter },
          { nil => :meta, name: 'twitter:title', content: title[1].to_s }
        ]

        # get abstract
        if desc = @context.label_for(@uuid, desc: true)
          out.push({ nil => :meta, name: 'twitter:description',
            content: desc[1].to_s })
        end

        # get image (foaf:depiction)
        img = @context.objects_for(@uuid, RDF::Vocab::FOAF.depiction,
                                   only: :resource)
        unless img.empty?
          img = img[0].to_s
          out.push({ nil => :meta, name: 'twitter:image', content: img })
          out[0][:content] = :summary_large_image
        end

        # return the appropriate xml-mixup structure
        out
      end

      def transform_xhtml published: true
        # before we do any more work make sure this is html
        doc  = @doc.dup 1
        body = doc.at_xpath('//html:body[1]', { html: XHTMLNS }) or return

        # initial stuff
        struct    = @context.struct_for @uuid
        resources = {}
        literals  = {}
        uuids     = {}
        datatypes = Set.new
        types     = Set.new
        authors   = @context.authors_for(@uuid)
        title     = @context.label_for @uuid, candidates: struct
        desc      = @context.label_for @uuid, candidates: struct, desc: true

        # rewrite content
        title = title[1] if title
        desc  = desc[1]  if desc

        struct.each do |k, v|
          v.each do |x|
            if x.literal?
              literals[x] ||= Set.new
              literals[x].add k

              # collect the datatype
              datatypes.add x.datatype if x.has_datatype?
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
          # turn this into a pair which subsequently gets turned into a hash
          [k, @context.label_for(k) ]
        end.to_h

        # warn labels

        # handle the title
        title ||= RDF::Literal('')
        tm = { '#title' => title,
          property: @context.abbreviate(literals[title].to_a, vocab: XHV) }
        if tl = title.language
          tm['xml:lang'] = tl # if xmlns
          tm['lang'] = tl
        elsif tdt = title.datatype and tdt != RDF::XSD.string
          tm[:datatype] = @context.abbreviate(tdt)
        end

        bodylinks = {}
        rewrite_links body do |elem|
          vocab = elem.at_xpath('ancestor-or-self::*[@vocab][1]/@vocab')
          vocab = uri_pp(vocab.to_s) if vocab

          if elem.key?('href') or elem.key?('src')
            vu = uri_pp(elem['href'] || elem['src'])
            tu = RDF::URI(@uri.merge(vu))
            bodylinks[urev[tu] || tu] = true

            if rel = resources[urev[tu] || tu]
              elem['rel'] = (@context.abbreviate rel, vocab: vocab).join ' '
            end

            label = labels[urev[tu] || tu]
            if label and (!elem.key?('title') or elem['title'].strip == '')
              elem['title'] = label[1].to_s
            end
          end
        end

        links = []
        resources.reject { |k, _| bodylinks[k] }.each do |k, v|
          v = v.dup.delete RDF::RDFV.type
          next if v.empty?
          rel = @context.abbreviate v.to_a, vocab: XHV
          k   = @uri.route_to(uri_pp (uuids[k] || k).to_s)
          name = :link
          attr = :href
          unless (v.to_set & Set[RDF::Vocab::DC.requires]).empty?
            name = :script
            attr = :src
          end
          links.push({ nil => name, rel: rel, attr => k.to_s })
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

        meta = []

        # include author names as old school meta tags
        authors.each do |a|
          name  = labels[urev[a] || a] or next
          datatypes.add name[0] # a convenient place to chuck this
          prop  = @context.abbreviate(name[0])
          name  = name[1]
          about = @uri.route_to((uuids[a] || a).to_s)
          tag   = { nil => :meta, about: about.to_s, name: :author,
                   property: prop, content: name.to_s }

          if name.has_datatype? and name.datatype != RDF::XSD.string
            tag[:datatype] = @context.abbreviate(name.datatype)
          elsif name.has_language?
            tag['xml:lang'] = tag[:lang] = name.language
          end
          meta.push tag
        end

        literals.each do |k, v|
          next if k == title
          rel = @context.abbreviate v.to_a, vocab: XHV
          elem = { nil => :meta, property: rel, content: k.to_s }
          elem[:name] = :description if k == desc

          if k.has_datatype?
            datatypes.add k.datatype # so we get the prefix
            elem[:datatype] = @context.abbreviate k.datatype, vocab: XHV
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

        body = body.dup 1
        body = { '#body' => body.children.to_a, about: '' }
        body[:typeof] = @context.abbreviate(types.to_a, vocab: XHV) unless
          types.empty?

        # prepare only the prefixes we need to resolve the data we need
        rsc = @context.abbreviate(
          (struct.keys + resources.keys + datatypes.to_a + types.to_a).uniq,
          noop: false).map do |x|
          next if x.nil?
          x.split(?:)[0].to_sym
        end.select { |x| not x.nil? }.to_set

        pfx = @context.prefixes.select do |k, _|
          rsc.include? k
        end.transform_values { |v| v.to_s }


        # extra = [
        # ] + generate_twitter_meta

        # and now for the document
        xf  = @context.config[:transform]
        doc = xhtml_stub(
          base: @uri, prefix: pfx, vocab: XHV, lang: 'en', title: tm, 
          link: links, meta: meta, extra: generate_twitter_meta || [],
          transform: xf, body: body).document
      end

      # Actually write the transformed document to the target
      #
      # @param published [true, false] 
      #
      # @return [Array] pathname(s) written
      def write_to_target published: true

        # in all cases we write to private target
        states = [false]
        # document has to be publishable
        states.push true if published && @context.published?(@uuid)

        ok = []
        states.each do |state|
          target = @context.config[state ? :target : :private]

          # XXX this is dumb; it should do something more robust if it
          # fails
          doc = transform_xhtml(published: state) or next

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
          rescue
            # XXX this should only rescue a specific class of errors
            File.unlink bad
          end
        end

        ok
      end

    end
  end
end
