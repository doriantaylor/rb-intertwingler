# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://privatealpha.com/ontology/transformation/1#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://privatealpha.com/ontology/transformation/1#>
  #   #
  #   # Transformation Functions Ontology
  #   #
  #   # This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.
  #   #
  #   # This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.
  #   class TFO < RDF::StrictVocabulary
  #     # This class represents an application of a transformation function, connecting a specific input and scalar parameters with its output.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Application
  #
  #     # This class represents the set of transformation functions that operate exclusively over (HTML/XML) markup.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :MarkupTransform
  #
  #     # This class provides a specification for a parameter in a given function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Parameter
  #
  #     # This class represents a list with the restriction that its members be tfo:Parameter nodes.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :ParameterList
  #
  #     # This class represents a partial application of a transformation function, affording the encapsulation and re-use of existing parameters.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Partial
  #
  #     # This class provides a specification for a transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Transform
  #
  #     # Specifies the list of content-types, in order of preference, that the function can process.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :accepts
  #
  #     # Identifies a tfo:Partial function that this tfo:Application completes.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :completes
  #
  #     # URI to the implementation of the function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :implementation
  #
  #     # Specifies the resource that was the input of the transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :input
  #
  #     # Specifies the resource that was the output of the transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :output
  #
  #     # Binds a parameter object to its function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :parameter
  #
  #     # Specifies a SHACL prefix declaration for complementing any associated XPath expression.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :prefix
  #
  #     # Specifies the list of content-types, in order of preference, that the function is capable of returning.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :returns
  #
  #     # Specifies the transform associated with this particular application
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :transform
  #
  #     # A case-insensitive regular expression.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :iregexp
  #
  #     # A regular expression.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :regexp
  #
  #     # An XPath expression.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :xpath
  #
  #   end
  TFO = Class.new(RDF::StrictVocabulary("https://privatealpha.com/ontology/transformation/1#")) do

    # Ontology definition
    ontology :"https://privatealpha.com/ontology/transformation/1#",
      "bibo:uri": "tfo:".freeze,
      comment: %(This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.).freeze,
      "dc:created": "2014-06-05T03:06:58Z".freeze,
      "dc:creator": "https://doriantaylor.com/person/dorian-taylor#me".freeze,
      "dc:description": "This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.".freeze,
      "dc:modified": ["2020-01-20T06:05:13Z".freeze, "2020-04-11T02:51:52Z".freeze, "2020-06-10T18:27:35Z".freeze],
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
      subClassOf: "tfo:Partial".freeze,
      type: "owl:Class".freeze
    term :MarkupTransform,
      comment: %(This class represents the set of transformation functions that operate exclusively over \(HTML/XML\) markup.).freeze,
      label: "MarkupTransform".freeze,
      subClassOf: "tfo:Transform".freeze,
      type: "owl:Class".freeze
    term :Parameter,
      comment: %(This class provides a specification for a parameter in a given function.).freeze,
      label: "Parameter".freeze,
      subClassOf: ["rdf:Property".freeze, term(
          allValuesFrom: "tfo:Partial".freeze,
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
    term :Partial,
      comment: %(This class represents a partial application of a transformation function, affording the encapsulation and re-use of existing parameters.).freeze,
      label: "Partial".freeze,
      subClassOf: "prov:Activity".freeze,
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
    property :"by-uri",
      comment: %(Specifies a regular expression for matching against URIs.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "by-uri".freeze,
      range: "tfo:regexp".freeze,
      type: "owl:DatatypeProperty".freeze
    property :"by-xpath",
      comment: %(Specifies an XPath expression for matching against markup \(HTML/XML\) content.).freeze,
      domain: "tfo:MarkupTransform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "by-xpath".freeze,
      range: "tfo:xpath".freeze,
      type: "owl:DatatypeProperty".freeze
    property :completes,
      comment: %(Identifies a tfo:Partial function that this tfo:Application completes.).freeze,
      domain: "tfo:Application".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "completes".freeze,
      range: "tfo:Partial".freeze,
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
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
    property :"not-by-uri",
      comment: %(Specifies a regular expression for anti-matching against URIs.).freeze,
      domain: "tfo:Transform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "not-by-uri".freeze,
      range: "tfo:regexp".freeze,
      type: "owl:DatatypeProperty".freeze
    property :"not-by-xpath",
      comment: %(Specifies an XPath expression for anti-matching against markup \(HTML/XML\) content.).freeze,
      domain: "tfo:MarkupTransform".freeze,
      isDefinedBy: "tfo:".freeze,
      label: "not-by-xpath".freeze,
      range: "tfo:xpath".freeze,
      type: "owl:DatatypeProperty".freeze
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
    property :prefix,
      comment: %(Specifies a SHACL prefix declaration for complementing any associated XPath expression.).freeze,
      domain: "tfo:MarkupTransform".freeze,
      label: "prefix".freeze,
      note: %(
            Note that the existing sh:declare property has a domain of owl:Ontology so we can't use it for this.
          ).freeze,
      range: "sh:PrefixDeclaration".freeze,
      type: "owl:ObjectProperty".freeze
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
      domain: "tfo:Partial".freeze,
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
    term :iregexp,
      comment: %(A case-insensitive regular expression.).freeze,
      label: "iregexp".freeze,
      note: %(
          Note that we do not intend to provide the full complement of flags for regular expressions like m, s, or x.
        ).freeze,
      "owl:onDatatype": "xsd:string".freeze,
      subClassOf: "tfo:regexp".freeze,
      type: "rdfs:Datatype".freeze
    term :regexp,
      comment: %(A regular expression.).freeze,
      label: "regexp".freeze,
      "owl:onDatatype": "xsd:string".freeze,
      "skos:usageNote": "\n          We do not specify a flavour, but in practice we should assume PCRE or ECMA-262.\n        ".freeze,
      type: "rdfs:Datatype".freeze
    term :xpath,
      comment: %(An XPath expression.).freeze,
      label: "xpath".freeze,
      "owl:onDatatype": "xsd:string".freeze,
      type: "rdfs:Datatype".freeze

    RDF::Vocabulary.register :tfo, self if
      RDF::Vocabulary.respond_to? :register
  end
end
