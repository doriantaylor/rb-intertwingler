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

  context 'resolving transforms' do
    it 'finds a transform in the graph' do
      transform = RDF::SAK::Transform.resolve repo, VOCAB.subtree, root: root
      expect(transform).to be_a RDF::SAK::Transform
      expect(transform.keys.sort).to eql PARAMS
      impl = RDF::URI('urn:x-ruby:RDF::SAK::Transform::XPath')
      expect(transform.implementation).to eql impl
    end

    it 'resolves an XSLT implementation' do
      transform = RDF::SAK::Transform.resolve repo, VOCAB.cleanup, root: root
      expect(transform).to be_a RDF::SAK::Transform::XSLT
    end

    it 'loads all transforms' do
      harness = RDF::SAK::Transform::Harness.load repo, root
      expect(harness.transforms.sort).to eql TRANSFORMS.sort
      xf = harness.transforms.map { |t| harness.resolve t }
      expect(xf.select(&:implemented?).size).to eql xf.size
    end
  end

  # context 'resolving partials' do
  # end

  # context 'resolving applications' do
  # end

  context 'applying transforms' do
    harness = RDF::SAK::Transform::Harness.load repo, root
    transform = harness.resolve VOCAB.subtree

    it 'resolves the transform' do
      expect(transform).to_not be_nil
    end

    it 'applies a transform against parameters' do
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
    harness = RDF::SAK::Transform::Harness.load repo, root
    transform = harness.resolve VOCAB.cleanup

    it 'resolves the transform' do
      expect(transform).to_not be_nil
    end

    it 'applies the transform to the input' do
      input = (root + 'example/matches.xhtml').open

      output, parseout = transform.apply input
      expect(parseout).to be_a Nokogiri::XML::Document
      expect(parseout.root.name).to eql 'main'
    end
  end

end
