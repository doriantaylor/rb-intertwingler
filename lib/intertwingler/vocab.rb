# ensure this is loaded
require 'rdf'

module Intertwingler
  module Vocab
    %i[ADMS CI IBIS ITCV PAV QB SCOVO TFO].each do |sym|
      autoload sym, "intertwingler/vocab/#{sym.to_s.downcase}.rb"
    end
  end
end
