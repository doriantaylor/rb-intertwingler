# ensure this is loaded
require 'rdf'

module Intertwingler
  module Vocab
    %i[ADMS CI CGTO IBIS ITCV PAV PM QB SCOVO TFO].each do |sym|
      autoload sym, "intertwingler/vocab/#{sym.to_s.downcase}.rb"
    end

    def self.load_vocabs
      constants.each { |c| const_get c }
    end
  end
end
