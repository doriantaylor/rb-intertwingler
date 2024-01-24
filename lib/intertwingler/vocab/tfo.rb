# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/transformation#
require 'rdf'
module Intertwingler::Vocab
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/transformation#>
  #   #
  #   # Transformation Functions Ontology
  #   #
  #   # This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.
  #   #
  #   # This document describes functions which transform HTTP representations, i.e., the actual literal payloads of HTTP messages.
  #   class TFO < RDF::StrictVocabulary
  #     # An addressable queue is a strict queue intended to hold transforms that have been invoked through the URL.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :AddressableQueue
  #
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Application
  #
  #     # This class provides a basic mechanism to yoke a set of transformation functions together.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Bundle
  #
  #     # This class provides a specification for a transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Function
  #
  #     # A function list is a list that only holds tfo:Function or tfo:Partial entities.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :FunctionList
  #
  #     # An insertion is a pre-packaged event that manipulates a per-request instance of a transformation queue. When the result of the transform being run matches the condition, the contents of the tfo:Insertion are inserted into the target queue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Insertion
  #
  #     # This class represents an invocation of a transformation function, connecting a specific input and scalar parameters with its output.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Invocation
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
  #     # A queue is a collection of tfo:Function (and/or tfo:Partial) elements, organized either by explicit sequence (via tfo:member-list), or by dynamic sorting at runtime.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Queue
  #
  #     # A strict queue is one for which all its elements must be executed, unlike an ordinary queue which only has to attempt to run its contents.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :StrictQueue
  #
  #     # Specifies the list of content-types, in order of preference, that the function can process.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :accepts
  #
  #     # Identifies a tfo:Partial function that this tfo:Invocation completes.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :completes
  #
  #     # An HTTP status code for which the insertion is triggered.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :condition
  #
  #     # Specifies one or more default values for a parameter.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :default
  #
  #     # Explicitly specifies a transform that must go first.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :first
  #
  #     # Specifies one or more tfo:Function that the subject must follow in a queue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :follows
  #
  #     # URI to the implementation of the function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :implementation
  #
  #     # Specifies the resource that was the input of the transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :input
  #
  #     # Explicitly specifies a transform that must go last.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :last
  #
  #     # Denotes a member of a queue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :member
  #
  #     # Specifies the next queue to run after this one.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :next
  #
  #     # Specifies the resource that was the output of the transformation function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :output
  #
  #     # Binds a parameter object to its function.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :parameter
  #
  #     # Specifies one or more tfo:Function that the subject must precede in a queue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :precedes
  #
  #     # Specifies the tfo:content-type the transform prefers to emit in lieu of a preference specified by the request. May be a list.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :prefers
  #
  #     # Specifies a SHACL prefix declaration for complementing any associated XPath expression.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :prefix
  #
  #     # Specifies the list of content-types, in order of preference, that the function is capable of returning.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :returns
  #
  #     # A target queue for the insertion.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :target
  #
  #     # Specifies the transform associated with this particular application
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :transform
  #
  #     # A tfo:Function can trigger an tfo:Insertion event on a certain condition (e.g., successful completion).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :triggers
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
      "http://purl.org/dc/terms/modified": ["2020-01-20T06:05:13Z", "2020-04-11T02:51:52Z", "2020-06-10T18:27:35Z", "2020-07-03T04:49:40Z", "2023-08-27T00:30:09Z", "2023-09-06T20:54:02Z", "2023-10-09T19:04:07Z"],
      "http://purl.org/dc/terms/references": ["https://www.iana.org/assignments/media-types/media-types.xhtml", "https://www.w3.org/TR/prov-o/", "https://www.w3.org/TR/rdf-schema/", "https://www.w3.org/TR/xmlschema-2/"],
      "http://purl.org/dc/terms/subject": "https://vocab.methodandstructure.com/transformation#",
      "http://purl.org/dc/terms/title": {en: "Transformation Functions Ontology"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/transformation#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "tfo",
      "http://www.w3.org/2002/07/owl#imports": "http://www.w3.org/ns/dcat#",
      "http://www.w3.org/2002/07/owl#sameAs": ["https://privatealpha.com/ontology/permutation/1", "https://privatealpha.com/ontology/permutation/1#", "https://privatealpha.com/ontology/transformation/1", "https://privatealpha.com/ontology/transformation/1#"],
      "http://xmlns.com/foaf/0.1/primaryTopic": "https://vocab.methodandstructure.com/transformation#",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :AddressableQueue,
      comment: {en: "An addressable queue is a strict queue intended to hold transforms that have been invoked through the URL."},
      label: {en: "AddressableQueue"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#StrictQueue",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Application,
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Bundle,
      comment: {en: "This class provides a basic mechanism to yoke a set of transformation functions together."},
      label: {en: "Bundle"},
      subClassOf: ["http://www.w3.org/ns/dcat#Catalog", "http://www.w3.org/ns/prov#SoftwareAgent"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Function,
      comment: {en: "This class provides a specification for a transformation function."},
      label: {en: "Function"},
      subClassOf: ["http://www.w3.org/ns/dcat#DataService", "http://www.w3.org/ns/prov#Entity"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :FunctionList,
      comment: {en: "A function list is a list that only holds tfo:Function or tfo:Partial entities."},
      label: {en: "FunctionList"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#List", term(
          allValuesFrom: "https://vocab.methodandstructure.com/transformation#FunctionList",
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        ), term(
          allValuesFrom: term(
            unionOf: list("https://vocab.methodandstructure.com/transformation#Function", "https://vocab.methodandstructure.com/transformation#Partial")
          ),
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Insertion,
      comment: {en: "An insertion is a pre-packaged event that manipulates a per-request instance of a transformation queue. When the result of the transform being run matches the condition, the contents of the tfo:Insertion are inserted into the target queue."},
      label: {en: "Insertion"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Queue",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Invocation,
      comment: {en: "This class represents an invocation of a transformation function, connecting a specific input and scalar parameters with its output."},
      equivalentClass: "https://vocab.methodandstructure.com/transformation#Application",
      label: {en: "Invocation"},
      note: {en: "\n            This class supersedes tfo:Application, which has been deprecated due to being too easily confused with a software application versus the application of a function.\n          "},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Partial",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :MarkupTransform,
      comment: {en: "This class represents the set of transformation functions that operate exclusively over (HTML/XML) markup."},
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      label: {en: "MarkupTransform"},
      note: {en: "\n            This class has been deprecated since it only made sense in 2020 when this vocabulary was being used in a different context.\n          "},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Function",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Parameter,
      comment: {en: "This class provides a specification for a parameter in a given function."},
      label: {en: "Parameter"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", term(
          allValuesFrom: term(
            unionOf: list("https://vocab.methodandstructure.com/transformation#Partial", "https://vocab.methodandstructure.com/transformation#Bundle")
          ),
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
    term :Queue,
      comment: {en: "A queue is a collection of tfo:Function (and/or tfo:Partial) elements, organized either by explicit sequence (via tfo:member-list), or by dynamic sorting at runtime."},
      label: {en: "Queue"},
      note: {en: "\n            Given that tfo:Invocation is a subclass of tfo:Partial, there is nothing in principle preventing the former from being introduced into a queue. If this happens, we ignore any tfo:input or tfo:output statemments associated with the application of the function.\n          "},
      subClassOf: "http://www.w3.org/ns/prov#Activity",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :StrictQueue,
      comment: {en: "A strict queue is one for which all its elements must be executed, unlike an ordinary queue which only has to attempt to run its contents."},
      label: {en: "StrictQueue"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Queue",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :accepts,
      comment: {en: "Specifies the list of content-types, in order of preference, that the function can process."},
      domain: "http://www.w3.org/ns/dcat#DataService",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "accepts"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#content-type", "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        ),
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"by-uri",
      comment: {en: "Specifies a regular expression for matching against URIs."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "by-uri"},
      range: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"by-xpath",
      comment: {en: "Specifies an XPath expression for matching against markup (HTML/XML) content."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "by-xpath"},
      range: "https://vocab.methodandstructure.com/transformation#xpath",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :completes,
      comment: {en: "Identifies a tfo:Partial function that this tfo:Invocation completes."},
      domain: "https://vocab.methodandstructure.com/transformation#Invocation",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "completes"},
      range: "https://vocab.methodandstructure.com/transformation#Partial",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :condition,
      comment: {en: "An HTTP status code for which the insertion is triggered."},
      domain: "https://vocab.methodandstructure.com/transformation#Insertion",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "condition"},
      range: "http://www.w3.org/2011/http#StatusCode",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :default,
      comment: {en: "Specifies one or more default values for a parameter."},
      domain: "https://vocab.methodandstructure.com/transformation#Parameter",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "default"},
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :first,
      comment: {en: "Explicitly specifies a transform that must go first."},
      domain: "https://vocab.methodandstructure.com/transformation#Queue",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "first"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#Function", "https://vocab.methodandstructure.com/transformation#Partial")
        ),
      subPropertyOf: "https://vocab.methodandstructure.com/transformation#member",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :follows,
      comment: {en: "Specifies one or more tfo:Function that the subject must follow in a queue."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      inverseOf: "https://vocab.methodandstructure.com/transformation#precedes",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "follows"},
      range: "https://vocab.methodandstructure.com/transformation#Function",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :implementation,
      comment: {en: "URI to the implementation of the function."},
      domain: "https://vocab.methodandstructure.com/transformation#Bundle",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "The URI is meant to be dereferenced by an internal implementation, e.g. file:, jar:, or an idiosyncratic scheme like urn:x-python:."},
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "implementation"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :input,
      comment: {en: "Specifies the resource that was the input of the transformation function."},
      domain: "https://vocab.methodandstructure.com/transformation#Invocation",
      "http://www.w3.org/2000/01/rdf-schema#PropertyOf": "http://www.w3.org/ns/prov#used",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "input"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :last,
      comment: {en: "Explicitly specifies a transform that must go last."},
      domain: "https://vocab.methodandstructure.com/transformation#Queue",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "last"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#Function", "https://vocab.methodandstructure.com/transformation#Partial")
        ),
      subPropertyOf: "https://vocab.methodandstructure.com/transformation#member",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :member,
      comment: {en: "Denotes a member of a queue."},
      domain: "https://vocab.methodandstructure.com/transformation#Queue",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "member"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#Function", "https://vocab.methodandstructure.com/transformation#Partial")
        ),
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"member-list",
      comment: {en: "Denotes an explicit member list for a queue."},
      domain: "https://vocab.methodandstructure.com/transformation#Queue",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "member-list"},
      range: "https://vocab.methodandstructure.com/transformation#FunctionList",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :next,
      comment: {en: "Specifies the next queue to run after this one."},
      domain: "https://vocab.methodandstructure.com/transformation#Queue",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "next"},
      range: "https://vocab.methodandstructure.com/transformation#Queue",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"not-by-uri",
      comment: {en: "Specifies a regular expression for anti-matching against URIs."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "not-by-uri"},
      range: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"not-by-xpath",
      comment: {en: "Specifies an XPath expression for anti-matching against markup (HTML/XML) content."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "not-by-xpath"},
      range: "https://vocab.methodandstructure.com/transformation#xpath",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :output,
      comment: {en: "Specifies the resource that was the output of the transformation function."},
      domain: "https://vocab.methodandstructure.com/transformation#Invocation",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "output"},
      range: "http://www.w3.org/2000/01/rdf-schema#Resource",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :parameter,
      comment: {en: "Binds a parameter object to its function."},
      domain: "http://www.w3.org/ns/dcat#DataService",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Parameters can either be supplied to the function as key-value pairs or sequentially."},
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "parameter"},
      range: "https://vocab.methodandstructure.com/transformation#Parameter",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"parameter-list",
      comment: {en: "Specifies the sequence of parameters when the invocation method of the function is sequential."},
      domain: "http://www.w3.org/ns/dcat#DataService",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "parameter-list"},
      range: "https://vocab.methodandstructure.com/transformation#ParameterList",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :precedes,
      comment: {en: "Specifies one or more tfo:Function that the subject must precede in a queue."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      inverseOf: "https://vocab.methodandstructure.com/transformation#follows",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "precedes"},
      range: "https://vocab.methodandstructure.com/transformation#Function",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :prefers,
      comment: {en: "Specifies the tfo:content-type the transform prefers to emit in lieu of a preference specified by the request. May be a list."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "prefers"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#content-type", "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        ),
      subPropertyOf: "https://vocab.methodandstructure.com/transformation#returns",
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty", "http://www.w3.org/2002/07/owl#FunctionalProperty"]
    property :prefix,
      comment: {en: "Specifies a SHACL prefix declaration for complementing any associated XPath expression."},
      domain: "https://vocab.methodandstructure.com/transformation#MarkupTransform",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "prefix"},
      note: {en: "\n            Note that the existing sh:declare property has a domain of owl:Ontology so we can't use it for this.\n          "},
      range: "http://www.w3.org/ns/shacl#PrefixDeclaration",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :returns,
      comment: {en: "Specifies the list of content-types, in order of preference, that the function is capable of returning."},
      domain: "http://www.w3.org/ns/dcat#DataService",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "returns"},
      range: term(
          unionOf: list("https://vocab.methodandstructure.com/transformation#content-type", "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        ),
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :target,
      comment: {en: "A target queue for the insertion."},
      domain: "https://vocab.methodandstructure.com/transformation#Insertion",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "target"},
      range: "https://vocab.methodandstructure.com/transformation#Queue",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :transform,
      comment: {en: "Specifies the transform associated with this particular application"},
      domain: "https://vocab.methodandstructure.com/transformation#Partial",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "transform"},
      range: "https://vocab.methodandstructure.com/transformation#Function",
      subPropertyOf: "https://vocab.methodandstructure.com/transformation#input",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :triggers,
      comment: {en: "A tfo:Function can trigger an tfo:Insertion event on a certain condition (e.g., successful completion)."},
      domain: "https://vocab.methodandstructure.com/transformation#Function",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "triggers"},
      range: "https://vocab.methodandstructure.com/transformation#Insertion",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"

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
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "iregexp"},
      note: {en: "\n          Note that we do not intend to provide the full complement of flags for regular expressions like m, s, or x.\n        "},
      subClassOf: "https://vocab.methodandstructure.com/transformation#regexp",
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :regexp,
      comment: {en: "A regular expression."},
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "\n          We do not specify a flavour, but in practice we should assume PCRE or ECMA-262.\n        "},
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "regexp"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :xpath,
      comment: {en: "An XPath expression."},
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      isDefinedBy: "https://vocab.methodandstructure.com/transformation#",
      label: {en: "xpath"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"

    RDF::Vocabulary.register :tfo, self if
      RDF::Vocabulary.respond_to? :register
  end
end
