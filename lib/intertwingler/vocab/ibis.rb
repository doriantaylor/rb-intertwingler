# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/ibis#
require 'rdf'
module Intertwingler::Vocab
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/ibis#>
  #   #
  #   # IBIS (bis) Vocabulary
  #   #
  #   # IBIS
  #   #
  #   # This document specifies a vocabulary for describing an IBIS (issue-based information system).
  #   # @version 0.6
  #   # @see http://www.cs.hut.fi/Opinnot/T-93.850/2005/Papers/gIBIS1988-conklin.pdf
  #   # @see https://web.archive.org/web/20120606063823/http://hyperdata.org/xmlns/ibis/
  #   # @see https://vocab.methodandstructure.com/ibis.rdf
  #   # @see https://vocab.methodandstructure.com/ibis.n3
  #   # @see http://dublincore.org/documents/dcmi-terms/
  #   # @see http://www.w3.org/TR/prov-o/
  #   # @see https://vocab.methodandstructure.com/process-model#
  #   # @see http://www.cc.gatech.edu/~ellendo/rittel/rittel-issues.pdf
  #   # @see http://en.wikipedia.org/wiki/Issue-Based_Information_System
  #   class IBIS < RDF::StrictVocabulary
  #     # An Argument is a type of Issue that explicitly supports or refutes a Position.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Argument
  #
  #     # ibis:Entity is the abstract superclass of from which the more specific entities are derived.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Entity
  #
  #     # An ibis:Entity can be marked ibis:Invariant to denote that it has been deemed outside of the influence of the agents in the system, i.e., something to be steered around.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Invariant
  #
  #     # An Issue is a state of affairs, claimed by one or more Agents to either be a misfit itself, or affecting some other Issue or Position.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Issue
  #
  #     # A network of issues, positions, and arguments.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Network
  #
  #     # A Position asserts a moral, ethical, pragmatic, or similar kind of assertion, typically identifying what, if anything, should be done about an Issue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Position
  #
  #     # A State can be understood as a snapshot of a system at a given time, such as before or after an event.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :State
  #
  #     # The subject is an issue concerning the object, which can be any resource.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :concerns
  #
  #     # An Agent can endorse a concept without having initially mentioned or advanced it, and without any additional comment.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :endorses
  #
  #     # The subject is a more generic form of the object.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :generalizes
  #
  #     # Indicates a subject argument that opposes an object position.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :opposes
  #
  #     # Indicates an issue that raises doubt on a belief.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :questions
  #
  #     # Indicates when a concept replaces another concept of the same type.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :replaces
  #
  #     # Indicates a position that responds to the subject issue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :response
  #
  #     # The subject is a more specific form of the object.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :specializes
  #
  #     # Indicates when the subject belief suggests the object issue.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :suggests
  #
  #     # Indicates a subject argument that supports an object position.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :supports
  #
  #   end
  IBIS = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/ibis#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/ibis#",
      comment: {en: "This document specifies a vocabulary for describing an IBIS (issue-based information system)."},
      "http://purl.org/dc/terms/created": "2012-12-11T22:22:53-08:00",
      "http://purl.org/dc/terms/creator": "https://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/modified": ["2012-12-12T16:04:50-08:00", "2014-02-24T21:14:13Z", "2018-02-22T03:39:14Z", "2019-03-24T22:37:22Z", "2023-12-18T02:25:31Z", "2024-01-05T04:05:24Z", "2025-04-10T02:30:11Z"],
      "http://purl.org/dc/terms/references": "http://www.w3.org/2004/02/skos/core#Concept",
      "http://purl.org/dc/terms/title": {en: "IBIS (bis) Vocabulary"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/ibis#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "ibis",
      "http://www.w3.org/1999/xhtml/vocab#contents": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#index": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#top": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#up": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": ["http://dublincore.org/documents/dcmi-terms/", "http://en.wikipedia.org/wiki/Issue-Based_Information_System", "http://www.cc.gatech.edu/~ellendo/rittel/rittel-issues.pdf", "http://www.cs.hut.fi/Opinnot/T-93.850/2005/Papers/gIBIS1988-conklin.pdf", "http://www.w3.org/TR/prov-o/", "https://vocab.methodandstructure.com/ibis.n3", "https://vocab.methodandstructure.com/ibis.rdf", "https://vocab.methodandstructure.com/process-model#", "https://web.archive.org/web/20120606063823/http://hyperdata.org/xmlns/ibis/"],
      "http://www.w3.org/2002/07/owl#imports": "http://www.w3.org/2004/02/skos/core#",
      "http://www.w3.org/2002/07/owl#versionInfo": "0.6",
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "IBIS"},
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Argument,
      comment: {en: "An Argument is a type of Issue that explicitly supports or refutes a Position."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/ibis#Position",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "An Argument need not only relate in scope to another Argument, but it must only be replaced by another argument."},
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Argument"},
      subClassOf: ["https://vocab.methodandstructure.com/ibis#Issue", term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Argument",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaced-by",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Argument",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaces",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Entity,
      comment: {en: "ibis:Entity is the abstract superclass of from which the more specific entities are derived."},
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Entity"},
      subClassOf: "http://www.w3.org/2004/02/skos/core#Concept",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Invariant,
      comment: {en: "An ibis:Entity can be marked ibis:Invariant to denote that it has been deemed outside of the influence of the agents in the system, i.e., something to be steered around."},
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Invariant"},
      subClassOf: "https://vocab.methodandstructure.com/ibis#Entity",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Issue,
      comment: {en: "An Issue is a state of affairs, claimed by one or more Agents to either be a misfit itself, or affecting some other Issue or Position."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/ibis#Position",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Issue"},
      subClassOf: ["https://vocab.methodandstructure.com/ibis#State", term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Issue",
          onProperty: "http://www.w3.org/2004/02/skos/core#broaderTransitive",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Issue",
          onProperty: "http://www.w3.org/2004/02/skos/core#narrowerTransitive",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Issue",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaced-by",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Issue",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaces",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Network,
      comment: {en: "A network of issues, positions, and arguments."},
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Network"},
      subClassOf: "http://www.w3.org/2004/02/skos/core#ConceptScheme",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Position,
      comment: {en: "A Position asserts a moral, ethical, pragmatic, or similar kind of assertion, typically identifying what, if anything, should be done about an Issue."},
      "http://www.w3.org/2002/07/owl#disjointWith": ["https://vocab.methodandstructure.com/ibis#Argument", "https://vocab.methodandstructure.com/ibis#Issue"],
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "Position"},
      subClassOf: ["https://vocab.methodandstructure.com/ibis#Entity", term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Position",
          onProperty: "http://www.w3.org/2004/02/skos/core#broaderTransitive",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Position",
          onProperty: "http://www.w3.org/2004/02/skos/core#narrowerTransitive",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Position",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaced-by",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        ), term(
          allValuesFrom: "https://vocab.methodandstructure.com/ibis#Position",
          onProperty: "https://vocab.methodandstructure.com/ibis#replaces",
          type: "http://www.w3.org/2002/07/owl#Restriction"
        )],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :State,
      comment: {en: "A State can be understood as a snapshot of a system at a given time, such as before or after an event."},
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: "State",
      note: {en: "A State is distinct from a particular instant, but it is analogous to it. At the time of observation, a State is either true or false."},
      subClassOf: "https://vocab.methodandstructure.com/ibis#Entity",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :"concern-of",
      comment: {en: "The subject is an issue concerning the object, which can be any resource."},
      inverseOf: "https://vocab.methodandstructure.com/ibis#concerns",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "concern-of"},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :concerns,
      comment: {en: "The subject is an issue concerning the object, which can be any resource."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#concern-of",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "concerns"},
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"endorsed-by",
      comment: {en: "A concept can be endorsed by an Agent without said Agent having mentioned or advanced it initially, and without any additional comment."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#endorses",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "endorsed by"},
      note: {en: "This term, along with ibis:endorses, enables an Agent to signal its agreement with a concept. To signal disagreement, explain why with an ibis:Argument that ibis:opposes the concept."},
      range: "http://xmlns.com/foaf/0.1/Agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :endorses,
      comment: {en: "An Agent can endorse a concept without having initially mentioned or advanced it, and without any additional comment."},
      domain: "http://xmlns.com/foaf/0.1/Agent",
      inverseOf: "https://vocab.methodandstructure.com/ibis#endorsed-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "endorses"},
      note: {en: "This term, along with ibis:endorsed-by, enables an Agent to signal its agreement with a concept. To signal disagreement, explain why with an ibis:Argument that ibis:opposes the concept."},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :generalizes,
      comment: {en: "The subject is a more generic form of the object."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#specializes",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "generalizes"},
      note: {en: "The equivalent property skos:narrower asserts that the object is narrower than the subject, while the subject of ibis:generalizes is more general than the object."},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#narrower",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"opposed-by",
      comment: {en: "Indicates a subject position opposed by an object argument."},
      domain: "https://vocab.methodandstructure.com/ibis#Position",
      inverseOf: "https://vocab.methodandstructure.com/ibis#opposes",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "opposed-by"},
      range: "https://vocab.methodandstructure.com/ibis#Argument",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :opposes,
      comment: {en: "Indicates a subject argument that opposes an object position."},
      domain: "https://vocab.methodandstructure.com/ibis#Argument",
      inverseOf: "https://vocab.methodandstructure.com/ibis#opposed-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "opposes"},
      range: "https://vocab.methodandstructure.com/ibis#Position",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"questioned-by",
      comment: {en: "Indicates a belief called into question by an issue."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#questions",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "questioned-by"},
      range: "https://vocab.methodandstructure.com/ibis#Issue",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#suggests",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :questions,
      comment: {en: "Indicates an issue that raises doubt on a belief."},
      domain: "https://vocab.methodandstructure.com/ibis#Issue",
      inverseOf: "https://vocab.methodandstructure.com/ibis#questioned-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "questions"},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#suggested-by",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"replaced-by",
      comment: {en: "Indicates when a concept is replaced by another concept of the same type."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#replaces",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "replaced-by"},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: ["http://purl.org/dc/terms/isReplacedBy", "http://www.w3.org/2004/02/skos/core#semanticRelation"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :replaces,
      comment: {en: "Indicates when a concept replaces another concept of the same type."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#replaced-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "replaces"},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: ["http://purl.org/dc/terms/replaces", "http://www.w3.org/2004/02/skos/core#semanticRelation"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"responds-to",
      comment: {en: "Indicates an issue to which the subject position responds."},
      domain: "https://vocab.methodandstructure.com/ibis#Position",
      inverseOf: "https://vocab.methodandstructure.com/ibis#response",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "responds-to"},
      range: "https://vocab.methodandstructure.com/ibis#Issue",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :response,
      comment: {en: "Indicates a position that responds to the subject issue."},
      domain: "https://vocab.methodandstructure.com/ibis#Issue",
      inverseOf: "https://vocab.methodandstructure.com/ibis#responds-to",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "response"},
      range: "https://vocab.methodandstructure.com/ibis#Position",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :specializes,
      comment: {en: "The subject is a more specific form of the object."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#generalizes",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "specializes"},
      note: {en: "The equivalent property skos:broader asserts that the object is broader than the subject, while the subject of ibis:specializes is more specific than the object."},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#broader",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"suggested-by",
      comment: {en: "Indicates when the subject issue is suggested by the object belief."},
      domain: "https://vocab.methodandstructure.com/ibis#Issue",
      inverseOf: "https://vocab.methodandstructure.com/ibis#suggests",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "suggested-by"},
      range: "https://vocab.methodandstructure.com/ibis#Entity",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :suggests,
      comment: {en: "Indicates when the subject belief suggests the object issue."},
      domain: "https://vocab.methodandstructure.com/ibis#Entity",
      inverseOf: "https://vocab.methodandstructure.com/ibis#suggested-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "suggests"},
      range: "https://vocab.methodandstructure.com/ibis#Issue",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"supported-by",
      comment: {en: "Indicates a subject position supported by an object argument."},
      domain: "https://vocab.methodandstructure.com/ibis#Position",
      inverseOf: "https://vocab.methodandstructure.com/ibis#supports",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "supported-by"},
      range: "https://vocab.methodandstructure.com/ibis#Argument",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :supports,
      comment: {en: "Indicates a subject argument that supports an object position."},
      domain: "https://vocab.methodandstructure.com/ibis#Argument",
      inverseOf: "https://vocab.methodandstructure.com/ibis#supported-by",
      isDefinedBy: "https://vocab.methodandstructure.com/ibis#",
      label: {en: "supports"},
      range: "https://vocab.methodandstructure.com/ibis#Position",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"

    RDF::Vocabulary.register :ibis, self if
      RDF::Vocabulary.respond_to? :register
  end
end
