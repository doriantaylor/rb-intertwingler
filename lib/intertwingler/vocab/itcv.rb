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
  #     # An itcv:Engine is the (as in the only one per instance) specialized itcv:Handler that is responsible for marshalling all other handlers and transforms.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Engine
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
  #     # An itcv:Engine has an itcv:Resolver for each website under its management. Resolvers map the dereferenceable—yet perishable—Web addresses to more durable identifiers like UUIDs and cryptographic hashes, as well as determine how to treat certain classes of resource, i.e., whether sovereign document or fragment thereof.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Resolver
  #
  #     # An itcv:Transform is a special-purpose itcv:Handler that encapsulates a set of actual transformation functions, identified by their URIs, that interact via HTTP POST.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Transform
  #
  #     # Denotes a class which is always to be treated as a stand-alone document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :document
  #
  #     # Denotes an itcv:FragmentSpecifier that describes how a given class is to be treated as a fragment of another document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :fragment
  #
  #     # This property relates a handler to the engine.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :handler
  #
  #     # Denotes the base URI under management from this resolver.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :manages
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
  #   end
  ITCV = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/intertwingler#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/intertwingler#",
      comment: {en: "This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine."},
      "http://purl.org/dc/terms/created": "2023-09-06T21:14:10Z",
      "http://purl.org/dc/terms/creator": "http://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/description": {en: "This document specifies the on-line configuration vocabulary for Intertwingler, a dense hypermedia engine."},
      "http://purl.org/dc/terms/references": "https://github.com/doriantaylor/rb-intertwingler",
      "http://purl.org/dc/terms/subject": "https://intertwingler.net/",
      "http://purl.org/dc/terms/title": {en: "Intertwingler Configuration Vocabulary"},
      "http://purl.org/ontology/bibo/status": "http://purl.org/ontology/bibo/status/draft",
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/intertwingler#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "itcv",
      "http://www.w3.org/2002/07/owl#imports": ["http://www.w3.org/ns/shacl#", "https://vocab.methodandstructure.com/transformation#"],
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      "http://xmlns.com/foaf/0.1/primaryTopic": ["https://intertwingler.net/", "https://vocab.methodandstructure.com/intertwingler#"],
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Engine,
      comment: {en: "An itcv:Engine is the (as in the only one per instance) specialized itcv:Handler that is responsible for marshalling all other handlers and transforms."},
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "Engine"},
      subClassOf: "https://vocab.methodandstructure.com/intertwingler#Handler",
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
      note: {en: "\n          The itcv:Transform class is different from the tfo:Transform insofar as the former is a handler, a potentially stand-alone microservice that bundles together a set of individual service endpoints, while the latter describes the individual service endpoints themselves.\n        "},
      subClassOf: "https://vocab.methodandstructure.com/intertwingler#Handler",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :document,
      comment: {en: "Denotes a class which is always to be treated as a stand-alone document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "document"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :fragment,
      comment: {en: "Denotes an itcv:FragmentSpecifier that describes how a given class is to be treated as a fragment of another document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "fragment"},
      range: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"fragment-class",
      comment: {en: "The target class of an itcv:FragmentSpecifier."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "fragment-class"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
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
    property :"host-class",
      comment: {en: "Specifies a class of host document."},
      domain: "https://vocab.methodandstructure.com/intertwingler#FragmentSpecifier",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "host-class"},
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :manages,
      comment: {en: "Denotes the base URI under management from this resolver."},
      domain: "https://vocab.methodandstructure.com/intertwingler#Resolver",
      isDefinedBy: "https://vocab.methodandstructure.com/intertwingler#",
      label: {en: "manages"},
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

    RDF::Vocabulary.register :itcv, self if
      RDF::Vocabulary.respond_to? :register
  end
end
