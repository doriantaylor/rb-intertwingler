require 'rdf/sak/version'

require 'xml-mixup'

require 'commander'

module RDF::SAK

  class Context
    # holy cow this is actually a lot of stuff:

    # turn markdown into xhtml (via md-noko)

    # turn html into xhtml (trivial)

    # generate triples from ordinary (x)html structure

    # map vanilla (x)html metadata to existing graph (ie to get URIs)

    # pull triples from rdfa

    # stuff rdfa into rdfa-less xhtml

    # basic nlp detection of terms + text-level markup (dfn, abbr...)

    # markdown round-tripping (may as well store source in md if possible)

    # add link titles

    # segmentation of composite documents into multiple files

    # aggregation of simple documents into composites

    # generate backlinks

    # generate indexes of people, groups, and organizations

    # generate indexes of books, not-books, and other external links

    # generate skos concept schemes

    # generate atom feed

    # generate sass palettes

    # generate rewrite map

    # read from source

    # write (manipulated (x|x?ht)ml) back to source

    # write to target

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
