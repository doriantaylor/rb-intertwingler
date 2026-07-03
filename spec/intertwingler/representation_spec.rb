require 'spec_helper'

require 'intertwingler/representation'
require 'nokogiri'

describe Intertwingler::Representation do
  context 'xml bodies' do
    it 'should parse an xml document' do
      xml = Nokogiri::XML '<hlagh/>'

      repr = Intertwingler::Representation::Nokogiri.new xml

      expect(repr.object.root).to_not be_nil
      expect(repr.object.root.name).to eq('hlagh')


      warn repr.type
    end
  end

  context 'image bodies' do
    it 'should ingest a png' do
      # warn Pathname.getwd
      path = Pathname('example/flow-diagram.png')
      repr = Intertwingler::Representation::Vips.new path, type: 'image/png'

      expect(repr.object).to be_instance_of(Vips::Image)

      expect(repr.size).to eq(182013)

      before = repr[:'sha-256']

      repr.type = 'image/webp'

      after = repr[:'sha-256']

      expect(before).to_not eq(after)

      # this should scan
      # warn repr.size

      # File.open('/tmp/fart.avif', 'wb') { |fh| fh << repr.read }
    end
  end
end
