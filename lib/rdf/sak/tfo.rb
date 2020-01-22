# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://privatealpha.com/ontology/transformation/1#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://privatealpha.com/ontology/transformation/1#>
  #   class TFO < RDF::StrictVocabulary
  #   end
  class TFO < RDF::StrictVocabulary("https://privatealpha.com/ontology/transformation/1#")

    # Ontology definition
    ontology :"https://privatealpha.com/ontology/transformation/1#",
      "bibo:uri": "tfo:".freeze,
      comment: %(This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.).freeze,
      "dc:created": "2014-06-05T03:06:58Z".freeze,
      "dc:creator": "https://doriantaylor.com/person/dorian-taylor#me".freeze,
      "dc:description": "This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.".freeze,
      "dc:modified": "2020-01-20T06:05:13Z".freeze,
      "dc:references": ["https://www.iana.org/assignments/media-types/media-types.xhtml".freeze, "https://www.w3.org/TR/prov-o/".freeze, "https://www.w3.org/TR/rdf-schema/".freeze, "https://www.w3.org/TR/xmlschema-2/".freeze],
      "dc:subject": "tfo:".freeze,
      "dc:title": "Transformation Functions Ontology".freeze,
      "foaf:primaryTopic": "tfo:".freeze,
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "tfo".freeze,
      isDefinedBy: "tfo:".freeze,
      "owl:sameAs": ["https://privatealpha.com/ontology/permutation/1".freeze, "https://privatealpha.com/ontology/permutation/1#".freeze],
      type: ["bibo:Webpage".freeze, "owl:Ontology".freeze]

    # Class definitions
    term :Application,
      comment: %(This class represents an application of a transformation function, connecting a specific input and scalar parameters with its output.).freeze,
      label: "Application".freeze,
      subClassOf: "prov:Activity".freeze,
      type: "owl:Class".freeze
    term :Parameter,
      comment: %(This class provides a specification for a parameter in a given function.).freeze,
      label: "Parameter".freeze,
      subClassOf: ["rdf:Property".freeze, term(
          allValuesFrom: "tfo:Application".freeze,
          onProperty: "rdfs:domain".freeze
        )],
      type: "owl:Class".freeze
    term :ParameterList,
      comment: %(This class represents a list with the restriction that its members be tfo:Parameter nodes.).freeze,
      label: "ParameterList".freeze,
      subClassOf: ["rdf:List".freeze, term(
          allValuesFrom: "tfo:Parameter".freeze,
          onProperty: "rdf:first".freeze
        ), term(
          allValuesFrom: "tfo:ParameterList".freeze,
          onProperty: "rdf:rest".freeze
        )],
      type: "owl:Class".freeze
    term :Transform,
      comment: %(This class provides a specification for a transformation function.).freeze,
      label: "Transform".freeze,
      subClassOf: "prov:SoftwareAgent".freeze,
      type: "owl:Class".freeze

    # Property definitions
    property :accepts,
      comment: %(Specifies the list of content-types, in order of preference, that the function can process.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "accepts".freeze,
      range: term(
          unionOf: list("tfo:content-type".freeze, "rdf:List".freeze)
        ),
      type: "owl:ObjectProperty".freeze
    property :implementation,
      comment: %(URI to the implementation of the function.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "implementation".freeze,
      range: "rdfs:Resource".freeze,
      "skos:usageNote": "The URI is meant to be dereferenced by an internal implementation, e.g. file:, jar:, or an idiosyncratic scheme like urn:x-python:.".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
    property :input,
      comment: %(Specifies the resource that was the input of the transformation function.).freeze,
      domain: "tfo:Application".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "input".freeze,
      range: "rdfs:Resource".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
    property :output,
      comment: %(Specifies the resource that was the output of the transformation function.).freeze,
      domain: "tfo:Application".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "output".freeze,
      range: "rdfs:Resource".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
    property :parameter,
      comment: %(Binds a parameter object to its function.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "parameter".freeze,
      range: "tfo:Parameter".freeze,
      "skos:usageNote": "Parameters can either be supplied to the function as key-value pairs or sequentially.".freeze,
      type: "owl:ObjectProperty".freeze
    property :"parameter-list",
      comment: %(Specifies the sequence of parameters when the invocation method of the function is sequential.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "parameter-list".freeze,
      range: "tfo:ParameterList".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
    property :returns,
      comment: %(Specifies the list of content-types, in order of preference, that the function is capable of returning.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "returns".freeze,
      range: term(
          unionOf: list("tfo:content-type".freeze, "rdf:List".freeze)
        ),
      type: "owl:ObjectProperty".freeze
    property :transform,
      comment: %(Specifies the transform associated with this particular application).freeze,
      domain: "tfo:Application".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "transform".freeze,
      range: "tfo:Transform".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]

    # Datatype definitions
    term :"content-type",
      comment: %(A literal that represents a content-type such as that which is found in the HTTP Accept: or Content-Type: header.).freeze,
      isDefinedBy: "tfo:".freeze,
      label: "content-type".freeze,
      "owl:onDatatype": "xsd:token".freeze,
      "owl:withRestrictions": list(term(
          "xsd:pattern": "^([!\#$%&'\\*\\+\\-.^_`|~0-9-A-Za-z]+)(?:/[!\#$%&'\\*\\+\\-.^_`|~0-9-A-Za-z]+)?)$".freeze
        )),
      "rdfs:seeAlso": ["https://tools.ietf.org/html/rfc7230#section-3.2.6".freeze, "https://tools.ietf.org/html/rfc7231#section-5.3.2".freeze],
      type: "rdfs:Datatype".freeze
  end
end
