require 'rdf/sak/version'

require 'xml-mixup'

require 'commander'

module RDF::SAK

  class Context
  end

  class CLI
    include XML::Mixup
    include Commander::Methods

    private

    public

    def initialize
    end

    def run
      run!
    end
  end
end
