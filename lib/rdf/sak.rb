# -*- coding: utf-8 -*-
require 'rdf/sak/version'

# basic stuff
require 'stringio'
require 'pathname'
require 'mimemagic'

# rdf stuff
require 'rdf'
require 'rdf/reasoner'
require 'linkeddata'

# cli stuff
require 'commander'

# my stuff
require 'xml-mixup'
require 'md-noko'

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

    private

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

    public

    attr_reader :graph

    def initialize graph: nil, config: nil, type: nil
      RDF::Reasoner.apply(:rdfs, :owl)
      @graph = coerce_graph graph, type: type
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

    def generate_atom_feed private: False
      # find all things that are documents
    end

    # generate sass palettes

    # generate rewrite map(s)

    def generate_rewrite_maps private: False
      # slug to uuid (internal)
      # uuid/slug to canonical slug (308)
      # retired slugs/uuids (410)
    end

    # - io stuff -

    # read from source

    # write (manipulated (x|x?ht)ml) back to source

    # write public and private variants to target

    def write_xhtml private: False
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
