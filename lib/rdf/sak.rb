require 'rdf/sak/version'
require 'pathname'
require 'mimemagic'
require 'commander'
require 'xml-mixup'
require 'md-noko'

module RDF::SAK

  class Context

    def initialize graph: nil, config: nil
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

    # - document class -

    class Document
      private

      C_OK = [Nokogiri::XML::Node, IO, Pathname].freeze

      DERR = {
        
      }

      def is_binary thing
        sample = nil
        if thing.is_a? IO
          pos = thing.tell
          thing.seek 0, 0
          sample = thing.read 100
          thing.seek pos
        else
          sample = thing.to_s[0,100]
        end

        sample.b =~ /[\x0-\x8\xe-\x1f\x7f]/ ? true : false
      end

      def default_type thing
        is_binary(thing) ? 'application/octet-stream' : 'text/plain'
      end

      public

      attr_reader :doc

      def initialize context, doc
        raise "doc must be Pathname, IO, or Nokogiri node" unless
          C_OK.any? { |c| doc.is_a? c } || doc.respond_to?(:to_s)

        @context = context

        # turn the document into an XML::Document
        if doc.is_a? Nokogiri::XML::Node
          unless doc.is_a? Nokogiri::XML::Document
            d = doc.dup 1
            doc = Nokogiri::XML::Document.new
            doc << d
          end
        else
          type = nil

          if doc.is_a? Pathname
            type = MimeMagic.by_path doc
            doc  = doc.open
          end

          doc = doc.to_s unless doc.is_a? IO

          type ||= MimeMagic.by_magic(doc) || default_type(doc)

          # can you believe there is a special bookmarks mime type good grief
          type = 'text/html' if type == 'application/x-mozilla-bookmarks'

          # if the detected type is html, try it as strict xml first
          if type =~ /xml/i
            doc = Nokogiri.XML doc
          elsif type == 'text/html'
            # try xml first
            attempt = nil
            begin
              # try to parse strictn
              attempt = Nokogiri.XML doc, nil, nil, (1 << 11) # NONET
            rescue Nokogiri::XML::SyntaxError
              tmp = Nokogiri.HTML doc
              attempt = Nokogiri::XML::Document.new
              attempt << tmp.root.dup(1)
            end
            doc = attempt
          elsif type =~ /^text/i
            doc = ::MD::Noko.new.ingest doc
          else
            raise "Don't know what to do with #{type}"
          end
        end

        root = doc.root
        if root.name == 'html'
          unless root.namespace
            # clear this off or it will be duplicated in the output
            root.remove_attribute('xmlns')
            # now generate a new ns object
            ns = root.add_namespace(
              nil, 'http://www.w3.org/1999/xhtml')
            # *now* scan the document and add the namespace declaration
            root.traverse do |node|
              if node.element? && node.namespace.nil?
                node.name = node.name.downcase
                node.namespace = ns
              end
            end
          end

          unless doc.internal_subset
            doc.create_internal_subset('html', nil, nil)
          end
        end

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
