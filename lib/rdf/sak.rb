# -*- coding: utf-8 -*-
require 'rdf/sak/version'

# basic stuff
require 'stringio'
require 'pathname'
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
require 'rdf/sak/ci'
require 'rdf/sak/ibis'

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
            RDF::Vocab::DC.title, RDF::Vocab::DC11.title],
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

    RDF::Reasoner.apply(:rdfs, :owl)

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

    public

    attr_reader :graph

    def initialize graph: nil, config: nil, type: nil
      RDF::Reasoner.apply(:rdfs, :owl)
      @graph = coerce_graph graph, type: type
    end

    def struct_for subject, only: []
      only = coerce_node_spec only

      rsrc = {}
      @graph.query([subject, nil, nil]).each do |stmt|
        # this will skip over any term not matching the type
        next unless node_matches? stmt.object, only
        p = RDF::Vocabulary.find_term(stmt.predicate) || stmt.predicate
        o = rsrc[p] ||= []
        o.push stmt.object
      end

      # XXX in here we can do fun stuff like filter/sort by language/datatype
      rsrc.each do |k, v|
        v.sort!
      end

      rsrc
    end

    def all_types
      @graph.query([nil, RDF.type, nil]).collect do |s|
        s.object
      end.uniq
    end

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

    def all_of_type rdftype
      t = RDF::Vocabulary.find_term(rdftype) or raise "No type #{rdftype.to_s}"
      out = []
      (all_types & all_related(t)).each do |type|
        out += @graph.query([nil, RDF.type, type]).collect { |s| s.subject }
      end
      
      out.uniq
    end

    def type_strata rdftype
      # first we coerce this to an array
      if rdftype.respond_to? :to_a
        rdftype = rdftype.to_a
      else
        rdftype = [rdftype]
      end
      
      # now squash
      rdftype.uniq!

      # bail out early
      return [] if rdftype.count == 0

      # essentially what we want to do is construct a layer of
      # asserted classes and their inferred equivalents, then probe
      # the classes in the first layer for subClassOf assertions,
      # which will form the second layer, and so on.

      queue  = [rdftype]
      strata = []
      seen   = {}

      while qin = queue.shift
        qwork = []

        qin.each do |q|
          qwork << q # entail doesn't include q
          qwork += q.entail(:equivalentClass)
        end

        # grep and flatten
        qwork = qwork.select { |t| t.uri? and not seen[t] }.uniq

        # push current layer out
        strata.push qwork if qwork.length > 0
     
        # now deal with subClassOf
        qsuper = []
        qwork.each do |q|
          seen[q] = true
          qsuper += q.subClassOf
        end

        # grep and flatten this too
        qsuper = qsuper.select { |t| t.uri? and not seen[t] }.uniq

        # same deal, conditionally push the input queue
        queue.push qsuper if qsuper.length > 0
      end

      # voila
      strata
    end

    def canonical_uri subject, unique: true
      out = []

      [CI.canonical, RDF::OWL.sameAs].each do |p|
        o = @graph.query([subject, p, nil]).collect { |st| st.object }
        out += o.sort { |a, b| cmp_resource(a, b) }
      end

      unique ? out.first : out.uniq
    end

    def label_for subject, unique: true, type: nil, lang: nil, alt: false
      # get type(s) if not supplied
      asserted = nil
      if type
        type = type.respond_to?(:to_a) ? type.to_a : [type]
        asserted = type.select { |t| t.is_a? RDF::Value }
      end
      asserted ||= @graph.query([subject, RDF.type, nil]).collect do |st|
        RDF::Vocabulary.find_term st.object
      end
      asserted.select! { |t| t && t.uri? }
      asserted.uniq!

      # get all the inferred types by layer; add default class if needed
      strata = type_strata asserted
      strata.push [RDF::RDFS.Resource] if
        strata.length == 0 or not strata[-1].include?(RDF::RDFS.Resource)

      # get the key-value pairs for the subject
      candidates = struct_for subject, only: :literal

      seen  = {}
      accum = []
      strata.each do |lst|
        lst.each do |cls|
          next unless STRINGS[cls] and
            preds = STRINGS[cls][:label][alt ? 1 : 0]
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

    # segmentation of composite documents into multiple files

    # aggregation of simple documents into composites

    # generate backlinks

    # - resource (ie file) generation -

    # generate indexes of people, groups, and organizations

    # generate indexes of books, not-books, and other external links

    # generate skos concept schemes

    # generate atom feed

    def generate_atom_feed id, published: true
      # 

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
      entries = {}
      docs.each do |uu|
        # basically make a jsonld-like structure
        rsrc = struct_for uu

        # get id (got it already duh)
        
        # get canonical link
        canon = ((rsrc[CI.canonical] || []) +
          (rsrc[RDF::OWL.sameAs] || [])).find(-> { uu }) { |x| x.resource? }
        canon = URI.parse canon.to_s
        
        entries[uu] = canon

        # get title
        # get abstract
        # get author 
        # get created date
        # get latest updated date
      end

      # now we sort by creation (not update) date

      # now we punt out the file

      entries
    end

    # generate sass palettes

    # generate rewrite map(s)

    def generate_rewrite_maps published: true
      # slug to uuid (internal)
      # uuid/slug to canonical slug (308)
      # retired slugs/uuids (410)
    end

    # - io stuff -

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
      private

      C_OK = [Nokogiri::XML::Node, IO, Pathname].freeze

      public

      attr_reader :doc

      def initialize context, doc
        raise 'context must be a RDF::SAK::Context' unless
          context.is_a? RDF::SAK::Context
        raise 'doc must be Pathname, IO, or Nokogiri node' unless
          C_OK.any? { |c| doc.is_a? c } || doc.respond_to?(:to_s)

        @context = context

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
          if type =~ /xml/i
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
            ns = root.add_namespace(nil, 'http://www.w3.org/1999/xhtml')
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

        # voilà
        @doc = doc
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

    def initialize
    end

    # vestigial

    def run
      run!
    end
  end
end
