# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/graph-tool#
require 'rdf'
module Intertwingler::Vocab
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/graph-tool#>
  #   #
  #   # Collaborative Graph Tool Ontology
  #   #
  #   # Ontologies like the IBIS vocabulary are intended to only convey their essential semantic content. There are nevertheless additional constructs, that do not belong in the core vocabulary, that need to be expressed in order to fully operationalize the information it describes as a piece of user-facing software. Such constructs include the users of the environment, and the graphical representation of the network itself, from colour palette to the relative (or absolute) geometry of the individual nodes.
  #   # @version 0.2
  #   class CGTO < RDF::StrictVocabulary
  #     # An error is a type of cgto:View the tool can send the user when something is wrong. It is more suitable to send as a response body than http:Response, which would be problematically self-referential.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Error
  #
  #     # An index relates a cgto:Space to a set of cgto:Inventory resources, via a set of cgto:Summary resources.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Index
  #
  #     # An inventory is a de facto grouping of resources according to some criterion.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Inventory
  #
  #     # A graph tool needs a shared logical space to lay out its universe of discourse.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Space
  #
  #     # A cgto:State relates elements of persistent application state, e.g. from the perspective of the user.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :State
  #
  #     # A summary indexes the available cgto:Inventory resources.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Summary
  #
  #     # A partially or fully computed geometric rendering of a (sub)graph, or the structured information needed to do so.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :View
  #
  #     # A window represents a slice of another container, e.g. for pagination.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Window
  #
  #     # Specifies whether the inventory includes resources that are asserted from the parameters (assumed to be true).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :asserted
  #
  #     # Specifies the owl:Class found in the rdf:type of the subjects.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :class
  #
  #     # This property picks out the resource that is currently in focus to the user.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :focus
  #
  #     # Connects the cgto:Space to its cgto:Index of resources.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :index
  #
  #     # Specifies whether the inventory includes resources that are inferred from the parameters, irrespective of whether they are asserted.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :inferred
  #
  #     # This property relates a cgto:State to the foaf:Agent associated with it.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :owner
  #
  #     # Specifies the property (predicate) in the statements from which subject and object resources are enumerated.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :property
  #
  #     # This property relates a cgto:State to the foaf:Agent associated with it.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :state
  #
  #     # Connects the cgto:Space to its cgto:Index of resources.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :summary
  #
  #     # This property relates the cgto:Index to the foaf:Agent that reprsents the current user.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :user
  #
  #     # Relates the space to a view of said space.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :view
  #
  #     # Relates a collection to an arbitrary window over the collection.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :window
  #
  #   end
  CGTO = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/graph-tool#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/graph-tool#",
      comment: {en: "Ontologies like the IBIS vocabulary are intended to only convey their essential semantic content. There are nevertheless additional constructs, that do not belong in the core vocabulary, that need to be expressed in order to fully operationalize the information it describes as a piece of user-facing software. Such constructs include the users of the environment, and the graphical representation of the network itself, from colour palette to the relative (or absolute) geometry of the individual nodes."},
      "http://purl.org/dc/terms/created": "2022-12-04T20:12:02Z",
      "http://purl.org/dc/terms/creator": "https://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/modified": ["2023-01-22T02:09:02Z", "2023-12-12T21:48:55Z", "2024-10-06T19:16:59Z", "2025-02-23T01:28:27Z", "2025-02-28T14:20:07Z", "2025-06-28T22:27:24Z"],
      "http://purl.org/dc/terms/title": {en: "Collaborative Graph Tool Ontology"},
      "http://purl.org/ontology/bibo/status": "http://purl.org/ontology/bibo/status/draft",
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/graph-tool#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "cgto",
      "http://purl.org/vocab/vann/preferredNamespaceUri": "https://vocab.methodandstructure.com/graph-tool#",
      "http://www.w3.org/1999/xhtml/vocab#contents": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#index": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#top": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#up": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/2002/07/owl#imports": ["http://purl.org/linked-data/cube#", "http://purl.org/linked-data/cube#", "http://rdfs.org/sioc/ns#", "http://www.w3.org/1999/xhtml/vocab#", "http://www.w3.org/2000/01/rdf-schema#"],
      "http://www.w3.org/2002/07/owl#versionInfo": "0.2",
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      "http://www.w3.org/ns/shacl#namespace": "https://vocab.methodandstructure.com/graph-tool#",
      "http://www.w3.org/ns/shacl#prefix": "cgto",
      "http://xmlns.com/foaf/0.1/primaryTopic": "https://vocab.methodandstructure.com/graph-tool#",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Error,
      comment: {en: "An error is a type of cgto:View the tool can send the user when something is wrong. It is more suitable to send as a response body than http:Response, which would be problematically self-referential."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Error",
      subClassOf: "https://vocab.methodandstructure.com/graph-tool#View",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Index,
      comment: {en: "An index relates a cgto:Space to a set of cgto:Inventory resources, via a set of cgto:Summary resources."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Index",
      subClassOf: ["http://rdfs.org/sioc/ns#Container", "http://www.w3.org/2000/01/rdf-schema#Container"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Inventory,
      comment: {en: "An inventory is a de facto grouping of resources according to some criterion."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Inventory",
      subClassOf: ["http://rdfs.org/sioc/ns#Container", "http://www.w3.org/2000/01/rdf-schema#Container"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Space,
      comment: {en: "A graph tool needs a shared logical space to lay out its universe of discourse."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Space",
      subClassOf: "http://rdfs.org/sioc/ns#Space",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :State,
      comment: {en: "A cgto:State relates elements of persistent application state, e.g. from the perspective of the user."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "State",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Summary,
      comment: {en: "A summary indexes the available cgto:Inventory resources."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Summary",
      subClassOf: "http://purl.org/linked-data/cube#DataSet",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :View,
      comment: {en: "A partially or fully computed geometric rendering of a (sub)graph, or the structured information needed to do so."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "View",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Window,
      comment: {en: "A window represents a slice of another container, e.g. for pagination."},
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "Window",
      subClassOf: ["http://rdfs.org/sioc/ns#Container", "http://www.w3.org/2000/01/rdf-schema#Container"],
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :asserted,
      comment: {en: "Specifies whether the inventory includes resources that are asserted from the parameters (assumed to be true)."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "asserted",
      range: "http://www.w3.org/2001/XMLSchema#boolean",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"asserted-object-count",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "asserted-object-count",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: ["http://purl.org/linked-data/cube#MeasureProperty", "http://www.w3.org/2002/07/owl#DatatypeProperty"]
    property :"asserted-objects",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "asserted-objects",
      range: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      type: ["http://purl.org/linked-data/cube#AttributeProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"asserted-subject-count",
      comment: {en: "Specifies the number of subject resources asserted to relate according to the given criterion."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "asserted-subject-count",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: ["http://purl.org/linked-data/cube#MeasureProperty", "http://www.w3.org/2002/07/owl#DatatypeProperty"]
    property :"asserted-subjects",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "asserted-subjects",
      range: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      type: ["http://purl.org/linked-data/cube#AttributeProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"by-class",
      comment: {en: "Specifies the index of inventories organized by rdfs:Class."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Index",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "by-class",
      range: "https://vocab.methodandstructure.com/graph-tool#Summary",
      subPropertyOf: "https://vocab.methodandstructure.com/graph-tool#summary",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"by-property",
      comment: {en: "Connects the cgto:Space to its cgto:Index of resources."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Index",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "by-property",
      range: "https://vocab.methodandstructure.com/graph-tool#Summary",
      subPropertyOf: "https://vocab.methodandstructure.com/graph-tool#summary",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :class,
      comment: {en: "Specifies the owl:Class found in the rdf:type of the subjects."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "class",
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: ["http://purl.org/linked-data/cube#DimensionProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"first-window",
      comment: {en: "Points to the first window over a collection."},
      domain: "http://www.w3.org/2000/01/rdf-schema#Container",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "first-window",
      range: "https://vocab.methodandstructure.com/graph-tool#Window",
      subPropertyOf: ["http://www.w3.org/1999/xhtml/vocab#first", "https://vocab.methodandstructure.com/graph-tool#window"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :focus,
      comment: {en: "This property picks out the resource that is currently in focus to the user."},
      domain: "https://vocab.methodandstructure.com/graph-tool#State",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#focus-of",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "focus",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"focus-of",
      comment: {en: "This property relates the focus back to the user state."},
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#focus",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "focus-of",
      range: "https://vocab.methodandstructure.com/graph-tool#State",
      type: ["http://www.w3.org/2002/07/owl#InverseFunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"in-domain-of",
      comment: {en: "The parameter specifying the property that resources selected into the inventory are in the domain of."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "in-domain-of",
      range: "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"in-range-of",
      comment: {en: "The parameter specifying the property that resources selected into the inventory are in the range of."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "in-range-of",
      range: "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :index,
      comment: {en: "Connects the cgto:Space to its cgto:Index of resources."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Space",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "index",
      range: "https://vocab.methodandstructure.com/graph-tool#Index",
      subPropertyOf: ["http://rdfs.org/sioc/ns#space_of", "http://www.w3.org/1999/xhtml/vocab#index"],
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :inferred,
      comment: {en: "Specifies whether the inventory includes resources that are inferred from the parameters, irrespective of whether they are asserted."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "inferred",
      range: "http://www.w3.org/2001/XMLSchema#boolean",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"inferred-object-count",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "inferred-object-count",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: ["http://purl.org/linked-data/cube#MeasureProperty", "http://www.w3.org/2002/07/owl#DatatypeProperty"]
    property :"inferred-objects",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "inferred-objects",
      range: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      type: ["http://purl.org/linked-data/cube#AttributeProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"inferred-subject-count",
      comment: {en: "Specifies the number of subject resources inferred to relate according to the given criterion."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "inferred-subject-count",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: ["http://purl.org/linked-data/cube#MeasureProperty", "http://www.w3.org/2002/07/owl#DatatypeProperty"]
    property :"inferred-subjects",
      comment: {en: ""},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "inferred-subjects",
      range: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      type: ["http://purl.org/linked-data/cube#AttributeProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"instance-of",
      comment: {en: "The parameter specifying the classes that resources selected into the inventory are instances of."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Inventory",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "instance-of",
      range: "http://www.w3.org/2000/01/rdf-schema#Class",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :owner,
      comment: {en: "This property relates a cgto:State to the foaf:Agent associated with it."},
      domain: "https://vocab.methodandstructure.com/graph-tool#State",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#state",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "owner",
      note: {en: "This property and its relatives originally related to foaf:OnlineAccount—and may again—although for operational reasons this was inconvenient at the time (2025-03-12)."},
      range: "http://xmlns.com/foaf/0.1/Agent",
      type: ["http://www.w3.org/2002/07/owl#InverseFunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :property,
      comment: {en: "Specifies the property (predicate) in the statements from which subject and object resources are enumerated."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "property",
      range: "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property",
      type: ["http://purl.org/linked-data/cube#DimensionProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :state,
      comment: {en: "This property relates a cgto:State to the foaf:Agent associated with it."},
      domain: "http://xmlns.com/foaf/0.1/Agent",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#owner",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "state",
      range: "https://vocab.methodandstructure.com/graph-tool#State",
      type: ["http://www.w3.org/2002/07/owl#InverseFunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :summary,
      comment: {en: "Connects the cgto:Space to its cgto:Index of resources."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Index",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "summary",
      range: "https://vocab.methodandstructure.com/graph-tool#Summary",
      subPropertyOf: "http://rdfs.org/sioc/ns#parent_of",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :user,
      comment: {en: "This property relates the cgto:Index to the foaf:Agent that reprsents the current user."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Index",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#owner",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "user",
      range: "http://xmlns.com/foaf/0.1/Agent",
      type: ["http://www.w3.org/2002/07/owl#InverseFunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :view,
      comment: {en: "Relates the space to a view of said space."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Space",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#view-of",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "view",
      range: "https://vocab.methodandstructure.com/graph-tool#View",
      subPropertyOf: "http://rdfs.org/sioc/ns#space_of",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"view-of",
      comment: {en: "Relates a view back to its cgto:Space."},
      domain: "https://vocab.methodandstructure.com/graph-tool#View",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#view",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "view-of",
      range: "https://vocab.methodandstructure.com/graph-tool#Space",
      subPropertyOf: "http://rdfs.org/sioc/ns#has_space",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :window,
      comment: {en: "Relates a collection to an arbitrary window over the collection."},
      domain: "http://www.w3.org/2000/01/rdf-schema#Container",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#window-of",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "window",
      range: "https://vocab.methodandstructure.com/graph-tool#Window",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"window-of",
      comment: {en: "Points a window to the collection it slices."},
      domain: "https://vocab.methodandstructure.com/graph-tool#Window",
      inverseOf: "https://vocab.methodandstructure.com/graph-tool#window",
      isDefinedBy: "https://vocab.methodandstructure.com/graph-tool#",
      label: "window-of",
      range: "http://www.w3.org/2000/01/rdf-schema#Container",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"

    # Extra definitions
    term :"resources-by-class",
      comment: {en: "This structure describes a data set that tabulates subject resources of a certain rdf:type."},
      "http://purl.org/linked-data/cube#component": [term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#inferred-subjects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#subjects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#dimension": "https://vocab.methodandstructure.com/graph-tool#class",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#asserted-subject-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#inferred-subject-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        )],
      label: "resources-by-class",
      type: "http://purl.org/linked-data/cube#DataStructureDefinition"
    term :"resources-by-property",
      comment: {en: "This structure describes a data set that tabulates both subject and object resources by property (predicate)."},
      "http://purl.org/linked-data/cube#component": [term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#asserted-objects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#asserted-subjects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#inferred-objects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#attribute": "https://vocab.methodandstructure.com/graph-tool#inferred-subjects",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#dimension": "https://vocab.methodandstructure.com/graph-tool#property",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#asserted-object-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#asserted-subject-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#inferred-object-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/graph-tool#inferred-subject-count",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        )],
      label: "resources-by-property",
      type: "http://purl.org/linked-data/cube#DataStructureDefinition"

    RDF::Vocabulary.register :cgto, self if
      RDF::Vocabulary.respond_to? :register
  end
end
