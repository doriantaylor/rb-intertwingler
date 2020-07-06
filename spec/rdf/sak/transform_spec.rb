require 'spec_helper'

require 'rdf'
require 'rdf/turtle'
require 'rdf/sak/transform'

RSpec.describe RDF::SAK::Transform do

  root = Pathname(Dir.getwd)

  VOCAB = RDF::Vocabulary.new 'tag:makethingsmakesense.com,2020:transform/'

  repo = RDF::Repository.new
  repo.load root + 'example/transforms.ttl'
  
  TRANSFORMS = %w[subtree cleanup].map { |t| VOCAB[t].to_uri }
  PARAMS = [VOCAB.prefix, VOCAB.reindent, VOCAB.xpath]

  let(:harness) { RDF::SAK::Transform::Harness.load repo, root }

  context 'harness behaviour' do
    it 'initializes' do
      expect(harness).to be_a RDF::SAK::Transform::Harness
    end

    it 'loads all transforms' do
      expect(harness.transforms.sort).to eql TRANSFORMS.sort
      xf = harness.transforms.map { |t| harness.resolve t }
      expect(xf.select(&:implemented?).size).to eql xf.size
    end

    it 'loads partials' do
      partials = harness.partials
      expect(partials).to be_a RDF::SAK::Transform::PartialCache
    end
  end

  context 'resolving transforms' do
    it 'finds a transform in the graph' do
      transform = harness.resolve VOCAB.subtree
      expect(transform).to be_a RDF::SAK::Transform
      expect(transform.keys.sort).to eql PARAMS
      impl = RDF::URI('urn:x-ruby:RDF::SAK::Transform::XPath')
      expect(transform.implementation).to eql impl
    end

    it 'resolves an XSLT implementation' do
      transform = RDF::SAK::Transform.resolve harness, VOCAB.cleanup
      expect(transform).to be_a RDF::SAK::Transform::XSLT
    end

  end

  context 'resolving partials' do
    it 'should find a partial with given parameters' do
      partial = harness.resolve_partial transform: VOCAB.subtree,
        params: { xpath: '//html:main[1]',
                  prefix: 'html:http://www.w3.org/1999/xhtml' }
      expect(partial).to be_a RDF::SAK::Transform::Partial
    end

    it 'should not find the partial if only some of the parameters are given' do
      nothing = harness.resolve_partial transform: VOCAB.subtree,
        params: { xpath: '//html:main[1]' }
      expect(nothing).to be nil
    end
  end

  context 'resolving applications' do
    input =
      RDF::URI('ni:///sha-256;0GHHmDtxh9CRZttXdr-cX78u72auS2P-O6tDXxvz2kU')
    it 'resolves an application' do
      application = harness.resolve_application transform: VOCAB.subtree,
        input: input, params: { xpath: '//html:main[1]',
                               prefix: 'html:http://www.w3.org/1999/xhtml' }
      expect(application).to be_a RDF::SAK::Transform::Application
      expect(application.transform).to be_a RDF::SAK::Transform
    end
  end

  context 'applying transforms' do

    it 'resolves the transform' do
      transform = harness.resolve VOCAB.subtree
      expect(transform).to_not be_nil
    end

    it 'applies a transform against parameters' do
      transform = harness.resolve VOCAB.subtree

      params = {
        VOCAB.xpath  => RDF::Literal('//html:main[1]'),
        VOCAB.prefix => RDF::Literal('html:http://www.w3.org/1999/xhtml'),
      }

      input = (root + 'example/matches.xhtml').open

      output, parseout = transform.apply input, params
      expect(parseout).to be_a Nokogiri::XML::Document
      expect(parseout.root.name).to eql 'main'
    end

    it 'applies a transform against a partial' do
    end
  end

  context 'applying XSLT transforms' do

    it 'resolves the transform' do
      transform = harness.resolve VOCAB.cleanup
      expect(transform).to_not be_nil
    end

    it 'applies the transform to the input' do
      transform = harness.resolve VOCAB.cleanup

      input = (root + 'example/matches.xhtml').open

      output, parseout = transform.apply input
      expect(parseout).to be_a Nokogiri::XML::Document
      expect(parseout.root.name).to eql 'main'
    end
  end

end
