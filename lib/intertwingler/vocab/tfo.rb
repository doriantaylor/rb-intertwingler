# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/transformation#
require 'rdf'
module Intertwingler
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/transformation#>
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
  #     # Specifies one or more default values for a parameter.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :default
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
  TFO = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/transformation#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/transformation#",
      comment: {en: "This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages."},
      "http://purl.org/dc/terms/created": "2014-06-05T03:06:58Z",
      "http://purl.org/dc/terms/creator": "https://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/description": {en: "This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages."},
      "http://purl.org/dc/terms/modified": ["2020-01-20T06:05:13Z", "2020-04-11T02:51:52Z", "2020-06-10T18:27:35Z", "2020-07-03T04:49:40Z"],
      "http://purl.org/dc/terms/references": ["https://www.iana.org/assignments/media-types/media-types.xhtml", "https://www.w3.org/TR/prov-o/", "https://www.w3.org/TR/rdf-schema/", "https://www.w3.org/TR/xmlschema-2/"],
      "http://purl.org/dc/terms/subject": "https://vocab.methodandstructure.com/transformation#",
      "http://purl.org/dc/terms/title": {en: "Transformation Functions Ontology"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/transformation#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "tfo",
      "http://www.w3.org/2002/07/owl#sameAs": ["https://privatealpha.com/ontology/permutation/1", "https://privatealpha.com/ontology/permutation/1#", "https://privatealpha.com/ontology/transformation/1", "https://privatealpha.com/ontology/transformation/1#"],
      "http://xmlns.com/foaf/0.1/primaryTopic": "https://vocab.methodandstructure.com/transformation#",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Application,
      comment: {en: "This class represents an application of a transformation function, connecting a specific input and scalar parameters with its output."},
      label: {en: "Application"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Partial",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :MarkupTransform,
      comment: {en: "This class represents the set of transformation functions that operate exclusively over (HTML/XML) markup."},
      label: {en: "MarkupTransform"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Transform",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Parameter,
      comment: {en: "This class provides a specification for a parameter in a given function."},
      label: {en: "Parameter"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", term(
          allValuesFrom: "https://vocab.methodandstructure.com/transformation#Partial",
          onProperty: "http://www.w3.org/2000/01/rdf-schema#domain"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :ParameterList,
      comment: {en: "This class represents a list with the restriction that its members be tfo:Parameter nodes."},
      label: {en: "ParameterList"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#List", term(
          allValuesFrom: "https://vocab.methodandstructure.com/transformation#Parameter",
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/transformation#ParameterList",
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Partial,
      comment: {en: "This class represents a partial application of a transformation function, affording the encapsulation and re-use of existing parameters."},
      label: {en: "Partial"},
      subClassOf: "http://www.w3.org/ns/prov#Activity",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Transform,
      comment: {en: "This class provides a specification for a transformation function."},
      label: {en: "Transform"},
      subClassOf: "http://www.w3.org/ns/prov#SoftwareAgent",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :accepts,
      comment: {en: "Specifies the list of content-types, in order of preference, that the function can process."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "accepts"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#content-type", "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        ),
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"by-uri",
      comment: {en: "Specifies a regular expression for matching against URIs."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "by-uri"},
      range: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"by-xpath",
      comment: {en: "Specifies an XPath expression for matching against markup (HTML/XML) content."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "by-xpath"},
      range: "https://vocab.methodandstructure.com/transformation#xpath",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :completes,
      comment: {en: "Identifies a tfo:Partial function that this tfo:Application completes."},
      domain: "https://vocab.methodandstructure.com/transformation#Application",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "completes"},
      range: "https://vocab.methodandstructure.com/transformation#Partial",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :default,
      comment: {en: "Specifies one or more default values for a parameter."},
      domain: "https://vocab.methodandstructure.com/transformation#Parameter",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "default"},
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :implementation,
      comment: {en: "URI to the implementation of the function."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "The URI is meant to be dereferenced by an internal implementation, e.g. file:, jar:, or an idiosyncratic scheme like urn:x-python:."},
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "implementation"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :input,
      comment: {en: "Specifies the resource that was the input of the transformation function."},
      domain: "https://vocab.methodandstructure.com/transformation#Application",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "input"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"not-by-uri",
      comment: {en: "Specifies a regular expression for anti-matching against URIs."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "not-by-uri"},
      range: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"not-by-xpath",
      comment: {en: "Specifies an XPath expression for anti-matching against markup (HTML/XML) content."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "not-by-xpath"},
      range: "https://vocab.methodandstructure.com/transformation#xpath",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :output,
      comment: {en: "Specifies the resource that was the output of the transformation function."},
      domain: "https://vocab.methodandstructure.com/transformation#Application",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "output"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :parameter,
      comment: {en: "Binds a parameter object to its function."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Parameters can either be supplied to the function as key-value pairs or sequentially."},
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "parameter"},
      range: "https://vocab.methodandstructure.com/transformation#Parameter",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"parameter-list",
      comment: {en: "Specifies the sequence of parameters when the invocation method of the function is sequential."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "parameter-list"},
      range: "https://vocab.methodandstructure.com/transformation#ParameterList",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :prefix,
      comment: {en: "Specifies a SHACL prefix declaration for complementing any associated XPath expression."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      label: {en: "prefix"},
      note: {en: "\n            Note that the existing sh:declare property has a domain of owl:Ontology so we can't use it for this.\n          "},
      range: "http://www.w3.org/ns/shacl#PrefixDeclaration",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :returns,
      comment: {en: "Specifies the list of content-types, in order of preference, that the function is capable of returning."},
      domain: "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "returns"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#content-type", "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        ),
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :transform,
      comment: {en: "Specifies the transform associated with this particular application"},
      domain: "https://vocab.methodandstructure.com/transformation#Partial",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "transform"},
      range: "https://vocab.methodandstructure.com/transformation#Transform",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]

    # Datatype definitions
    term :"content-type",
      comment: {en: "A literal that represents a content-type such as that which is found in the HTTP Accept: or Content-Type: header."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": ["https://tools.ietf.org/html/rfc7230#section-3.2.6", "https://tools.ietf.org/html/rfc7231#section-5.3.2"],
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#token",
      "http://www.w3.org/2002/07/owl#withRestrictions": list(term(
          "http://www.w3.org/2001/XMLSchema#pattern": "^([!\#$%&'\\*\\+\\-.^_`|~0-9-A-Za-z]+)(?:/[!\#$%&'\\*\\+\\-.^_`|~0-9-A-Za-z]+)?)$"
        )),
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "content-type"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :iregexp,
      comment: {en: "A case-insensitive regular expression."},
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      label: {en: "iregexp"},
      note: {en: "\n          Note that we do not intend to provide the full complement of flags for regular expressions like m, s, or x.\n        "},
      subClassOf: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :regexp,
      comment: {en: "A regular expression."},
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "\n          We do not specify a flavour, but in practice we should assume PCRE or ECMA-262.\n        "},
      label: {en: "regexp"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :xpath,
      comment: {en: "An XPath expression."},
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      label: {en: "xpath"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"

    RDF::Vocabulary.register :tfo, self if
      RDF::Vocabulary.respond_to? :register
  end
end
