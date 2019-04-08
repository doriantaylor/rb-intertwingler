require 'rdf/sak'
require 'xml-mixup'
require 'commander'

module RDF::SAK
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
