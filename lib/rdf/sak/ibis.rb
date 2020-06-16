# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://privatealpha.com/ontology/ibis/1#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://privatealpha.com/ontology/ibis/1#>
  #   class IBIS < RDF::StrictVocabulary
  #   end
  class IBIS < RDF::StrictVocabulary("https://privatealpha.com/ontology/ibis/1#")

    # Ontology definition
    ontology :"https://privatealpha.com/ontology/ibis/1#",
      "bibo:uri": "ibis:".freeze,
      comment: %(This document specifies a vocabulary for describing an IBIS \(issue-based information system\).).freeze,
      "dc:created": "2012-12-11T22:22:53-08:00".freeze,
      "dc:creator": "http://doriantaylor.com/person/dorian-taylor#me".freeze,
      "dc:modified": ["2012-12-12T16:04:50-08:00".freeze, "2014-02-24T21:14:13Z".freeze, "2018-02-22T03:39:14Z".freeze],
      "dc:title": "IBIS (bis) Vocabulary".freeze,
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "ibis".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "IBIS".freeze,
      "owl:imports": "skos:".freeze,
      "owl:versionInfo": "0.3".freeze,
      "rdfs:seeAlso": ["http://dublincore.org/documents/dcmi-terms/".freeze, "http://en.wikipedia.org/wiki/Issue-Based_Information_System".freeze, "http://hyperdata.org/xmlns/ibis/".freeze, "http://www.cc.gatech.edu/~ellendo/rittel/rittel-issues.pdf".freeze, "http://www.cs.hut.fi/Opinnot/T-93.850/2005/Papers/gIBIS1988-conklin.pdf".freeze, "http://www.w3.org/TR/prov-o/".freeze, "https://privatealpha.com/ontology/ibis/1.n3".freeze, "https://privatealpha.com/ontology/ibis/1.rdf".freeze, "https://privatealpha.com/ontology/process-model/1#".freeze],
      type: ["bibo:Webpage".freeze, "owl:Ontology".freeze]

    # Class definitions
    term :Argument,
      comment: %(An Argument is a type of Issue that explicitly supports or refutes a Position.).freeze,
      isDefinedBy: "ibis:".freeze,
      label: "Argument".freeze,
      "owl:disjointWith": "ibis:Position".freeze,
      "skos:usageNote": "An Argument need not only relate in scope to another Argument, but it must only be replaced by another argument.".freeze,
      subClassOf: ["ibis:Issue".freeze, term(
          allValuesFrom: "ibis:Argument".freeze,
          onProperty: "ibis:replaced-by".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Argument".freeze,
          onProperty: "ibis:replaces".freeze,
          type: "owl:Restriction".freeze
        )],
      type: "owl:Class".freeze
    term :Invariant,
      comment: %(An Issue or Position can be marked Invariant to denote that it has been deemed outside of the influence of the Agents in the system, i.e., something to be steered around.).freeze,
      isDefinedBy: "ibis:".freeze,
      label: "Invariant".freeze,
      subClassOf: "skos:Concept".freeze,
      type: "owl:Class".freeze
    term :Issue,
      comment: %(An Issue is a state of affairs, claimed by one or more Agents to either be a misfit itself, or affecting some other Issue or Position.).freeze,
      isDefinedBy: "ibis:".freeze,
      label: "Issue".freeze,
      "owl:disjointWith": "ibis:Position".freeze,
      subClassOf: ["skos:Concept".freeze, term(
          allValuesFrom: "ibis:Issue".freeze,
          onProperty: "ibis:replaced-by".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Issue".freeze,
          onProperty: "ibis:replaces".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Issue".freeze,
          onProperty: "skos:broaderTransitive".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Issue".freeze,
          onProperty: "skos:narrowerTransitive".freeze,
          type: "owl:Restriction".freeze
        )],
      type: "owl:Class".freeze
    term :Network,
      comment: %(A network of issues, positions, and arguments.).freeze,
      isDefinedBy: "ibis:".freeze,
      label: "Network".freeze,
      subClassOf: "skos:ConceptScheme".freeze,
      type: "owl:Class".freeze
    term :Position,
      comment: %(A Position asserts a moral, ethical, pragmatic, or similar kind of assertion, typically identifying what, if anything, should be done about an Issue.).freeze,
      isDefinedBy: "ibis:".freeze,
      label: "Position".freeze,
      "owl:disjointWith": ["ibis:Argument".freeze, "ibis:Issue".freeze],
      subClassOf: ["skos:Concept".freeze, term(
          allValuesFrom: "ibis:Position".freeze,
          onProperty: "ibis:replaced-by".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Position".freeze,
          onProperty: "ibis:replaces".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Position".freeze,
          onProperty: "skos:broaderTransitive".freeze,
          type: "owl:Restriction".freeze
        ), term(
          allValuesFrom: "ibis:Position".freeze,
          onProperty: "skos:narrowerTransitive".freeze,
          type: "owl:Restriction".freeze
        )],
      type: "owl:Class".freeze

    # Property definitions
    property :concerns,
      comment: %(The subject is an issue concerning the object, which can be any resource.).freeze,
      domain: "ibis:Issue".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "concerns".freeze,
      range: "owl:Thing".freeze,
      type: "owl:ObjectProperty".freeze
    property :"endorsed-by",
      comment: %(A concept can be endorsed by an Agent without said Agent having mentioned or advanced it initially, and without any additional comment.).freeze,
      domain: "skos:Concept".freeze,
      inverseOf: "ibis:endorses".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "endorsed by".freeze,
      note: %(This term, along with ibis:endorses, enables an Agent to signal its agreement with a concept. To signal disagreement, explain why with an ibis:Argument that ibis:opposes the concept.).freeze,
      range: "foaf:Agent".freeze,
      type: "owl:ObjectProperty".freeze
    property :endorses,
      comment: %(An Agent can endorse a concept without having initially mentioned or advanced it, and without any additional comment.).freeze,
      domain: "foaf:Agent".freeze,
      inverseOf: "ibis:endorsed-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "endorses".freeze,
      note: %(This term, along with ibis:endorsed-by, enables an Agent to signal its agreement with a concept. To signal disagreement, explain why with an ibis:Argument that ibis:opposes the concept.).freeze,
      range: "skos:Concept".freeze,
      type: "owl:ObjectProperty".freeze
    property :generalizes,
      comment: %(The subject is a more generic form of the object.).freeze,
      equivalentProperty: "skos:narrower".freeze,
      inverseOf: "ibis:specializes".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "generalizes".freeze,
      note: %(The equivalent property skos:narrower asserts that the object is narrower than the subject, while the subject of ibis:generalizes is more general than the object.).freeze,
      type: "owl:ObjectProperty".freeze
    property :"opposed-by",
      comment: %(Indicates a subject position opposed by an object argument.).freeze,
      domain: "ibis:Position".freeze,
      inverseOf: "ibis:opposes".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "opposed by".freeze,
      range: "ibis:Argument".freeze,
      type: "owl:ObjectProperty".freeze
    property :opposes,
      comment: %(Indicates a subject argument that opposes an object position.).freeze,
      domain: "ibis:Argument".freeze,
      inverseOf: "ibis:opposed-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "opposes".freeze,
      range: "ibis:Position".freeze,
      type: "owl:ObjectProperty".freeze
    property :"questioned-by",
      comment: %(Indicates a belief called into question by an issue.).freeze,
      domain: "skos:Concept".freeze,
      inverseOf: "ibis:questions".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "questioned by".freeze,
      range: "ibis:Issue".freeze,
      type: "owl:ObjectProperty".freeze
    property :questions,
      comment: %(Indicates an issue that raises doubt on a belief.).freeze,
      domain: "ibis:Issue".freeze,
      inverseOf: "ibis:questioned-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "questions".freeze,
      range: "skos:Concept".freeze,
      type: "owl:ObjectProperty".freeze
    property :"replaced-by",
      comment: %(Indicates when a concept is replaced by another concept of the same type.).freeze,
      domain: "skos:Concept".freeze,
      inverseOf: "ibis:replaces".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "replaced by".freeze,
      range: "skos:Concept".freeze,
      subPropertyOf: "dc:isReplacedBy".freeze,
      type: "owl:ObjectProperty".freeze
    property :replaces,
      comment: %(Indicates when a concept replaces another concept of the same type.).freeze,
      domain: "skos:Concept".freeze,
      inverseOf: "ibis:replaced-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "replaces".freeze,
      range: "skos:Concept".freeze,
      subPropertyOf: "dc:replaces".freeze,
      type: "owl:ObjectProperty".freeze
    property :"responds-to",
      comment: %(Indicates an issue to which the subject position responds.).freeze,
      domain: "ibis:Position".freeze,
      inverseOf: "ibis:response".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "responds to".freeze,
      range: "ibis:Issue".freeze,
      type: "owl:ObjectProperty".freeze
    property :response,
      comment: %(Indicates a position that responds to the subject issue.).freeze,
      domain: "ibis:Issue".freeze,
      inverseOf: "ibis:responds-to".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "response".freeze,
      range: "ibis:Position".freeze,
      type: "owl:ObjectProperty".freeze
    property :specializes,
      comment: %(The subject is a more specific form of the object.).freeze,
      equivalentProperty: "skos:broader".freeze,
      inverseOf: "ibis:generalizes".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "specializes".freeze,
      note: %(The equivalent property skos:broader asserts that the object is broader than the subject, while the subject of ibis:specializes is more specific than the object.).freeze,
      type: "owl:ObjectProperty".freeze
    property :"suggested-by",
      comment: %(Indicates when the subject issue is suggested by the object belief.).freeze,
      domain: "ibis:Issue".freeze,
      inverseOf: "ibis:suggests".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "suggested by".freeze,
      range: "skos:Concept".freeze,
      type: "owl:ObjectProperty".freeze
    property :suggests,
      comment: %(Indicates when the subject belief suggests the object issue.).freeze,
      domain: "skos:Concept".freeze,
      inverseOf: "ibis:suggested-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "suggests".freeze,
      range: "ibis:Issue".freeze,
      type: "owl:ObjectProperty".freeze
    property :"supported-by",
      comment: %(Indicates a subject position supported by an object argument.).freeze,
      domain: "ibis:Position".freeze,
      inverseOf: "ibis:supports".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "supported by".freeze,
      range: "ibis:Argument".freeze,
      type: "owl:ObjectProperty".freeze
    property :supports,
      comment: %(Indicates a subject argument that supports an object position.).freeze,
      domain: "ibis:Argument".freeze,
      inverseOf: "ibis:supported-by".freeze,
      isDefinedBy: "ibis:".freeze,
      label: "supports".freeze,
      range: "ibis:Position".freeze,
      type: "owl:ObjectProperty".freeze

    RDF::Vocabulary.register :ibis, self if
      RDF::Vocabulary.respond_to? :register
  end
end
