# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from http://purl.org/NET/scovo#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <http://purl.org/NET/scovo#>
  #   #
  #   class SCOVO < RDF::StrictVocabulary
  #     # a statistical dataset
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Dataset
  #
  #     # a dimension of a statistical data item
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Dimension
  #
  #     # a statistical data item
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Item
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :dataset
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :datasetOf
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :dimension
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :max
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :min
  #
  #   end
  SCOVO = Class.new(RDF::StrictVocabulary("http://purl.org/NET/scovo#")) do

    # Class definitions
    term :Dataset,
      comment: "a statistical dataset".freeze,
      label: "Dataset".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :Dimension,
      comment: "a dimension of a statistical data item".freeze,
      label: "Dimension".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :Item,
      comment: "a statistical data item".freeze,
      label: "Item".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]

    # Property definitions
    property :dataset,
      domain: "scovo:Item".freeze,
      label: "belongs to dataset".freeze,
      range: "scovo:Dataset".freeze,
      type: "rdf:Property".freeze
    property :datasetOf,
      domain: "scovo:Dataset".freeze,
      label: "is the dataset of".freeze,
      range: "scovo:Item".freeze,
      type: "rdf:Property".freeze
    property :dimension,
      domain: "scovo:Item".freeze,
      label: "has a dimension".freeze,
      range: "scovo:Dimension".freeze,
      type: "rdf:Property".freeze
    property :max,
      domain: "scovo:Dimension".freeze,
      label: "has a maximum range value".freeze,
      type: "rdf:Property".freeze
    property :min,
      domain: "scovo:Dimension".freeze,
      label: "has a minimum range value".freeze,
      type: "rdf:Property".freeze

    RDF::Vocabulary.register :qb, self if
      RDF::Vocabulary.respond_to? :register
  end
end
