# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/intertwingler#
require 'rdf'
module Intertwingler::Vocab
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/intertwingler#>
  #   #
  #   # Intertwingler Configuration Vocabulary
  #   #
  #   # This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine.
  #   #
  #   # This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine.
  #   class ITCV < RDF::StrictVocabulary
  #     # An itcv:Engine is the (as in the only one per site) specialized itcv:Handler that is responsible for marshalling all other handlers and transforms.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Engine
  #
  #     # An itcv:FragmentList is a list of fragments and only fragments.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :FragmentList
  #
  #     # A fragment specifier tells a resolver to treat a particular class of resource as a document fragment, rather than a full document, as well how to relate said fragment to a host document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :FragmentSpecifier
  #
  #     # An itcv:Handler is the basic unit of functionality in Intertwingler. Handlers are microservices that serve one or more URIs via one or more HTTP request methods.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Handler
  #
  #     # An itcv:HandlerList is a list of handlers and only handlers.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :HandlerList
  #
  #     # An itcv:Instance represents an instantiated handler object. It exists as a vehicle for initialization parameters to be specified in configuration data.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Instance
  #
  #     # An itcv:PathList is a list of itcv:paths.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :PathList
  #
  #     # An itcv:Engine has an itcv:Resolver for each website under its management. Resolvers map the dereferenceable—yet perishable—Web addresses to more durable identifiers like UUIDs and cryptographic hashes, as well as determine how to treat certain classes of resource, i.e., whether sovereign document or fragment thereof.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Resolver
  #
  #     # An itcv:Transform is a special-purpose itcv:Handler that encapsulates a set of actual transformation functions, identified by their URIs, that interact via HTTP POST.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Transform
  #
  #     # Denotes, ultimately, an HTTP Host: header that is an acceptable substitute for the authority under management.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :alias
  #
  #     # Denotes a class which is always to be treated as a stand-alone document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :document
  #
  #     # Specifies an rdfs:Class, e.g. a subclasswhich is explicitly not a host.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :except
  #
  #     # A target class of an itcv:FragmentSpecifier.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :fragment
  #
  #     # This property relates a handler to the engine.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :handler
  #
  #     # Specifies a class of host document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :host
  #
  #     # Denotes the base URI under management from this resolver.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :manages
  #
  #     # This property relates an itcv:Instance to the itcv:Handler to which it is intended to supply parameters.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :of
  #
  #     # This is a prefix declaration borrowed from SHACL.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :prefix
  #
  #     # This property generically relates a transform queue to a handler without specifying what else to do with it.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :queue
  #
  #     # This property maps the itcv:Engine to an itcv:Resolver.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :resolver
  #
  #     # Identifies a fragment specifier over which the subject is intended to take precedence.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :supersedes
  #
  #     # Specifies the relationship between a fragment and its host document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :via
  #
  #     # Defines the null prefix vocabulary, e.g. for use with RDFa.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :vocab
  #
  #     # This literal datatype is a human-readable representation of a byte count, with magnitude suffixes.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :bytes
  #
  #     # This literal datatype represents a local file system path which may be absolute, relative, or relative to home (~[user]).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :path
  #
  #   end
  ITCV = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/intertwingler#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/intertwingler#",
      comment: {en: "This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine."},
      "http://purl.org/dc/terms/created": "2023-09-06T21:14:10Z",
      "http://purl.org/dc/terms/creator": "http://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/description": {en: "This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine."},
      "http://purl.org/dc/terms/modified": ["2023-10-09T19:04:07Z", "2023-10-29T19:43:48Z", "2023-11-18T19:28:38Z", "2024-11-08T05:06:04Z"],
      "http://purl.org/dc/terms/references": "https://github.com/doriantaylor/rb-intertwingler",
      "http://purl.org/dc/terms/subject": "https://intertwingler.net/",
      "http://purl.org/dc/terms/title": {en: "Intertwingler Configuration Vocabulary"},
      "http://purl.org/ontology/bibo/status": "http://purl.org/ontology/bibo/status/draft",
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/intertwingler#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "itcv",
      "http://www.w3.org/1999/xhtml/vocab#contents": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#index": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#top": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#up": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/2002/07/owl#imports": ["http://www.w3.org/ns/shacl#", "https://vocab.methodandstructure.com/transformation#"],
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      "http://xmlns.com/foaf/0.1/primaryTopic": ["https://intertwingler.net/", "https://vocab.methodandstructure.com/intertwingler#"],
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Engine,
      comment: {en: "An itcv:Engine is the (as in the only one per site) specialized itcv:Handler that is responsible for marshalling all other handlers and transforms."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Engine"},
      subClassOf: "https://vocab.methodandstructure.com/intertwingler#Handler",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :FragmentList,
      comment: {en: "An itcv:FragmentList is a list of fragments and only fragments."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "FragmentList"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#List", term(
          allValuesFrom: term(
            unionOf: list("https://vocab.methodandstructure.com/intertwingler#FragmentList", term(
              "http://www.w3.org/2002/07/owl#hasValue": list()
            ))
          ),
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :FragmentSpecifier,
      comment: {en: "A fragment specifier tells a resolver to treat a particular class of resource as a document fragment, rather than a full document, as well how to relate said fragment to a host document."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "FragmentSpecifier"},
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Handler,
      comment: {en: "An itcv:Handler is the basic unit of functionality in Intertwingler. Handlers are microservices that serve one or more URIs via one or more HTTP request methods."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Handler"},
      subClassOf: "https://vocab.methodandstructure.com/transformation#Bundle",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :HandlerList,
      comment: {en: "An itcv:HandlerList is a list of handlers and only handlers."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "HandlerList"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#List", term(
          allValuesFrom: "https://vocab.methodandstructure.com/intertwingler#Handler",
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
        ), term(
          allValuesFrom: term(
            unionOf: list("https://vocab.methodandstructure.com/intertwingler#HandlerList", term(
              "http://www.w3.org/2002/07/owl#hasValue": list()
            ))
          ),
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Instance,
      comment: {en: "An itcv:Instance represents an instantiated handler object. It exists as a vehicle for initialization parameters to be specified in configuration data."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Instance"},
      type: "http://www.w3.org/2002/07/owl#Class"
    term :PathList,
      comment: {en: "An itcv:PathList is a list of itcv:paths."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "PathList"},
      subClassOf: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#List", term(
          allValuesFrom: "https://vocab.methodandstructure.com/intertwingler#path",
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
        ), term(
          allValuesFrom: term(
            unionOf: list("https://vocab.methodandstructure.com/intertwingler#PathList", term(
              "http://www.w3.org/2002/07/owl#hasValue": list()
            ))
          ),
          onProperty: "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Resolver,
      comment: {en: "An itcv:Engine has an itcv:Resolver for each website under its management. Resolvers map the dereferenceable—yet perishable—Web addresses to more durable identifiers like UUIDs and cryptographic hashes, as well as determine how to treat certain classes of resource, i.e., whether sovereign document or fragment thereof."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Resolver"},
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Transform,
      comment: {en: "An itcv:Transform is a special-purpose itcv:Handler that encapsulates a set of actual transformation functions, identified by their URIs, that interact via HTTP POST."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://vocab.methodandstructure.com/transformation#Transform",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Transform"},
      note: {en: "\n            The itcv:Transform class is different from the tfo:Transform insofar as the former is a handler, a potentially stand-alone microservice that bundles together a set of individual service endpoints, while the latter describes the individual service endpoints themselves.\n          "},
      subClassOf: "https://vocab.methodandstructure.com/intertwingler#Handler",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :alias,
      comment: {en: "Denotes, ultimately, an HTTP Host: header that is an acceptable substitute for the authority under management."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "alias"},
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :document,
      comment: {en: "Denotes a class which is always to be treated as a stand-alone document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "document"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :except,
      comment: {en: "Specifies an rdfs:Class, e.g. a subclasswhich is explicitly not a host."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "except"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :fragment,
      comment: {en: "A target class of an itcv:FragmentSpecifier."},
      domain: ["https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier", "https://vocab.methodandstructure.com/intertwingler#Resolver"],
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "fragment"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"fragment-list",
      comment: {en: "Denotes an ordered list of itcv:FragmentSpecifiers that describes how a given class is to be treated as a fragment of another document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "fragment-list"},
      range: "https://vocab.methodandstructure.com/intertwingler#FragmentList",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"fragment-spec",
      comment: {en: "Denotes an itcv:FragmentSpecifier that describes how a given class is to be treated as a fragment of another document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "fragment-spec"},
      range: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :handler,
      comment: {en: "This property relates a handler to the engine."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Engine",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "handler"},
      range: "https://vocab.methodandstructure.com/intertwingler#Handler",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"handler-list",
      comment: {en: "This property relates an ordered list of handlers to the engine."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Engine",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "handler-list"},
      range: "https://vocab.methodandstructure.com/intertwingler#HandlerList",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :host,
      comment: {en: "Specifies a class of host document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "host"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :manages,
      comment: {en: "Denotes the base URI under management from this resolver."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "manages"},
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :of,
      cardinality: {en: "1"},
      comment: {en: "This property relates an itcv:Instance to the itcv:Handler to which it is intended to supply parameters."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Instance",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "of"},
      range: "https://vocab.methodandstructure.com/intertwingler#Handler",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :prefix,
      comment: {en: "This is a prefix declaration borrowed from SHACL."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "prefix"},
      range: "http://www.w3.org/ns/shacl#PrefixDeclaration",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :queue,
      comment: {en: "This property generically relates a transform queue to a handler without specifying what else to do with it."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Handler",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "queue"},
      range: "https://vocab.methodandstructure.com/transformation#Queue",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"request-queue",
      comment: {en: "The engine, which inherits the relation itcv:queue, also has a queue for request transforms."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Engine",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "request-queue"},
      range: "https://vocab.methodandstructure.com/transformation#Queue",
      subPropertyOf: "https://vocab.methodandstructure.com/intertwingler#queue",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :resolver,
      comment: {en: "This property maps the itcv:Engine to an itcv:Resolver."},
      domain: ["https://vocab.methodandstructure.com/intertwingler#Engine", "https://vocab.methodandstructure.com/intertwingler#Resolver"],
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "resolver"},
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"response-queue",
      comment: {en: "This property denotes the entry point for the response transform queue associated with the itcv:Handler. It also specifies the default queue associated with the itcv:Engine."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Handler",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "response-queue"},
      range: "https://vocab.methodandstructure.com/transformation#Queue",
      subPropertyOf: "https://vocab.methodandstructure.com/intertwingler#queue",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :supersedes,
      comment: {en: "Identifies a fragment specifier over which the subject is intended to take precedence."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "supersedes"},
      range: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :via,
      comment: {en: "Specifies the relationship between a fragment and its host document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://www.w3.org/TR/shacl/#property-paths",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "via"},
      note: {en: "Rather than type out a formal definition of the range of this property, note that it is intended to be the same range as a SHACL property path (the SHACL people didn't formally define this either)."},
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :vocab,
      comment: {en: "Defines the null prefix vocabulary, e.g. for use with RDFa."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "vocab"},
      range: "http://www.w3.org/2001/XMLSchema#anyURI",
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty", "http://www.w3.org/2002/07/owl#FunctionalProperty"]

    # Datatype definitions
    term :bytes,
      comment: {en: "This literal datatype is a human-readable representation of a byte count, with magnitude suffixes."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://en.wikipedia.org/wiki/Orders_of_magnitude_(data)",
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#token",
      "http://www.w3.org/2002/07/owl#withRestrictions": list(term(
          "http://www.w3.org/2001/XMLSchema#pattern": "^\\d+[KMGTPEkmgtpe]?$"
        )),
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Magnitude suffixes use the convention of lowercase for base 10 and uppercase for base 2."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "bytes"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"
    term :path,
      comment: {en: "This literal datatype represents a local file system path which may be absolute, relative, or relative to home (~[user])."},
      "http://www.w3.org/2002/07/owl#onDatatype": "http://www.w3.org/2001/XMLSchema#string",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "path"},
      type: "http://www.w3.org/2000/01/rdf-schema#Datatype"

    RDF::Vocabulary.register :itcv, self if
      RDF::Vocabulary.respond_to? :register
  end
end
