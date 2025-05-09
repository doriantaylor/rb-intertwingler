# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/process-model#
require 'rdf'
module Intertwingler::Vocab
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/process-model#>
  #   #
  #   # A Process Model Ontology
  #   #
  #   # This vocabulary encodes a model for expressing generic business processes. Its purpose is to provide a language and exchange format for software applications designed to facilitate project management.
  #   # @version 0.2
  #   class PM < RDF::StrictVocabulary
  #     # A pm:Action specializes an ev:Event in that it is performed by (at least one) real, living foaf:Person.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Action
  #
  #     # A pm:Goal extends an ibis:Issue by way of being explicitly desired by a foaf:Agent.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Goal
  #
  #     # A method specifies an abstract sequence of events.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Method
  #
  #     # A pm:Target connects a qualitative pm:Goal to a specific pm:Task, and a concrete, quantitative allocation of resources.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Target
  #
  #     # A pm:Task specializes a pm:Action in that it has one or more pm:Goal, and connects a pm:Method of execution with a responsible foaf:Person who will carry it out.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Task
  #
  #     # The purpose of a task is to achieve a goal. Every pm:Task specified in this vocabulary must also specify the pm:Goal they are intended to achieve.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :achieves
  #
  #     # By anchoring a pm:Goal to a pm:Target, we give it a concrete budget and deadline.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :anchors
  #
  #     # Every pm:Target has to have a quantitative budget of resources assigned to it. It need not be denominated in currency (e.g. it could be person-hours).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :budget
  #
  #     # A pm:Action may have an ibis:State as a context.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :context
  #
  #     # An ibis:State lends context to a pm:Action.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :contextualizes
  #
  #     # An ibis:State the pm:Action depends on to be actionable.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :dependency
  #
  #     # Every pm:Target must have a due date, or expiry date after which the target is no longer viable.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :due
  #
  #     # A valid pm:Target must initiate exactly one pm:Task to carry it out.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :initiates
  #
  #     # A pm:Action is an instance of a pm:Method.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :instance
  #
  #     # A pm:Action may involve one or more foaf:Agent.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :involves
  #
  #     # A pm:Action can identify an associated pm:Method which will be used to complete it.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :method
  #
  #     # Every pm:Action has an outcome, which is some kind of ibis:State.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :outcome
  #
  #     # A performer is a real, live (at the time of performance) person who performs a task.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :performer
  #
  #     # 
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :performs
  #
  #     # When an ibis:State is the outcome of a pm:Action.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :process
  #
  #     # A recipient of a task is a (real, live at the time of receipt) person who receives and approves its product or products.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :recipient
  #
  #     # The person responsible for a task may not be the one performing it, but they are the one accountable for its completion.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :responsible
  #
  #     # The status of a Task at any instant is a State.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :status
  #
  #     # This property narrows the domain and range of ev:sub_event to pm:Task.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :subtask
  #
  #     # 
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :supertask
  #
  #     # A variant is an alternate method of performing the same action.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :variant
  #
  #     # A foaf:Agent may want a pm:Goal.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :wants
  #
  #     # The task is aborted.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :ABORTED
  #
  #     # The task is complete.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :COMPLETE
  #
  #     # The task is stalled.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :STALLED
  #
  #   end
  PM = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/process-model#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/process-model#",
      comment: {en: "This vocabulary encodes a model for expressing generic business processes. Its purpose is to provide a language and exchange format for software applications designed to facilitate project management."},
      "http://purl.org/dc/terms/created": "2009-07-23T00:56:12Z",
      "http://purl.org/dc/terms/creator": "http://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/modified": ["2014-02-06T22:26:21Z", "2017-04-06T22:24:06Z", "2020-03-08T18:58:53Z", "2025-04-09T23:18:53Z"],
      "http://purl.org/dc/terms/title": {en: "A Process Model Ontology"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/process-model#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "pm",
      "http://purl.org/vocab/vann/preferredNamespaceUri": "https://vocab.methodandstructure.com/process-model#",
      "http://www.w3.org/1999/xhtml/vocab#contents": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#index": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#top": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#up": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/2002/07/owl#imports": ["http://purl.org/NET/c4dm/event.owl#", "http://www.w3.org/2004/02/skos/core#", "http://www.w3.org/ns/prov#", "https://vocab.methodandstructure.com/ibis#"],
      "http://www.w3.org/2002/07/owl#versionInfo": "0.2",
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      "http://www.w3.org/ns/shacl#namespace": "https://vocab.methodandstructure.com/process-model#",
      "http://www.w3.org/ns/shacl#prefix": "pm",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Action,
      comment: {en: "A pm:Action specializes an ev:Event in that it is performed by (at least one) real, living foaf:Person."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/ibis#State",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "Action",
      subClassOf: ["http://purl.org/NET/c4dm/event.owl#Event", "http://www.w3.org/ns/prov#Activity"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Goal,
      comment: {en: "A pm:Goal extends an ibis:Issue by way of being explicitly desired by a foaf:Agent."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "Goal",
      note: {en: "A pm:Goal is not actionable itself, although a pm:Task, which is a pm:Action, must achieve at least one pm:Goal. At the time of observation, pm:Goal, like its superclass, ibis:Issue, is binary: either resolved or unresolved. This entails that a pm:Goal is not expressed in terms of quantitative results or deadlines. That is the job of a pm:Target."},
      subClassOf: "https://vocab.methodandstructure.com/ibis#Issue",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Method,
      comment: {en: "A method specifies an abstract sequence of events."},
      editorialNote: {en: "This class is provisional. It isn't entirely clear that we need a distinct pm:Method class, since pm:Tasks and the like could be appropriated as prototypes of methods."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/ibis#State",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "Method",
      subClassOf: "https://vocab.methodandstructure.com/process-model#Action",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Target,
      comment: {en: "A pm:Target connects a qualitative pm:Goal to a specific pm:Task, and a concrete, quantitative allocation of resources."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "Target",
      note: {en: "The reason why pm:Target is distinct from pm:Goal is because we can imagine more than one competing candidate pm:Task with different cost and effort profiles. Without this information, any goal—or target, for that matter—is merely aspirational. A target is therefore intended to marry a particular goal to a particular concrete strategy for achieving it."},
      subClassOf: "https://vocab.methodandstructure.com/process-model#Goal",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Task,
      comment: {en: "A pm:Task specializes a pm:Action in that it has one or more pm:Goal, and connects a pm:Method of execution with a responsible foaf:Person who will carry it out."},
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Since pm:Task is a descendant of ev:Event, we can use the ev:time relation to specify a time:Interval to record the time taken to complete a task."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "Task",
      subClassOf: ["https://vocab.methodandstructure.com/ibis#Position", "https://vocab.methodandstructure.com/process-model#Action"],
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :"achieved-by",
      comment: {en: "A pm:Goal is achieved by at least one candidate pm:Task."},
      domain: "https://vocab.methodandstructure.com/process-model#Goal",
      inverseOf: "https://vocab.methodandstructure.com/process-model#achieves",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "achieved-by",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#semanticRelation",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :achieves,
      comment: {en: "The purpose of a task is to achieve a goal. Every pm:Task specified in this vocabulary must also specify the pm:Goal they are intended to achieve."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      inverseOf: "https://vocab.methodandstructure.com/process-model#achieved-by",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "achieves",
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: ["http://www.w3.org/2004/02/skos/core#semanticRelation", "https://vocab.methodandstructure.com/process-model#outcome"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"anchored-by",
      comment: {en: "A pm:Goal is anchored to reality by a pm:Target."},
      domain: "https://vocab.methodandstructure.com/process-model#Goal",
      inverseOf: "https://vocab.methodandstructure.com/process-model#anchors",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "anchored-by",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#generalizes",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :anchors,
      comment: {en: "By anchoring a pm:Goal to a pm:Target, we give it a concrete budget and deadline."},
      domain: ["https://vocab.methodandstructure.com/process-model#Target", "https://vocab.methodandstructure.com/process-model#Task"],
      inverseOf: "https://vocab.methodandstructure.com/process-model#anchored-by",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "anchors",
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#specializes",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :budget,
      cardinality: "1",
      comment: {en: "Every pm:Target has to have a quantitative budget of resources assigned to it. It need not be denominated in currency (e.g. it could be person-hours)."},
      domain: "https://vocab.methodandstructure.com/process-model#Target",
      editorialNote: {en: "It is not clear, prior to implementation, if we should keep the budget as a raw quantity, or create some kind of resource envelope object."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://www.youtube.com/watch?v=fiSjxr8xG6A",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "budget",
      range: "http://www.w3.org/2001/XMLSchema#decimal",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :context,
      comment: {en: "A pm:Action may have an ibis:State as a context."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "The pm:context and pm:outcome properties can be used to chain one pm:Action to another."},
      inverseOf: "https://vocab.methodandstructure.com/process-model#contextualizes",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "context",
      range: "https://vocab.methodandstructure.com/ibis#State",
      subPropertyOf: ["http://purl.org/NET/c4dm/event.owl#factor", "http://www.w3.org/2004/02/skos/core#semanticRelation", "http://www.w3.org/ns/prov#used"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :contextualizes,
      comment: {en: "An ibis:State lends context to a pm:Action."},
      domain: "https://vocab.methodandstructure.com/ibis#State",
      inverseOf: "https://vocab.methodandstructure.com/process-model#context",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "contextualizes",
      range: "https://vocab.methodandstructure.com/process-model#Action",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :dependency,
      comment: {en: "An ibis:State the pm:Action depends on to be actionable."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      inverseOf: "https://vocab.methodandstructure.com/process-model#dependency-of",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "dependency",
      range: "https://vocab.methodandstructure.com/ibis#State",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#suggests",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :due,
      cardinality: "1",
      comment: "Every pm:Target must have a due date, or expiry date after which the target is no longer viable.",
      domain: "https://vocab.methodandstructure.com/process-model#Target",
      editorialNote: {en: "I am contemplating changing this to pm:expires to refer to the value-based process model."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://www.youtube.com/watch?v=fiSjxr8xG6A",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "due"},
      range: "http://www.w3.org/2001/XMLSchema#dateTime",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :"initiated-by",
      comment: {en: "This connects a selected pm:Task to the pm:Target that initiated it."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      inverseOf: "https://vocab.methodandstructure.com/process-model#initiates",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "initiated-by",
      range: "https://vocab.methodandstructure.com/process-model#Target",
      type: ["http://www.w3.org/2002/07/owl#InverseFunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :initiates,
      cardinality: "1",
      comment: {en: "A valid pm:Target must initiate exactly one pm:Task to carry it out."},
      domain: "https://vocab.methodandstructure.com/process-model#Target",
      inverseOf: "https://vocab.methodandstructure.com/process-model#initiated-by",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "initiates",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :instance,
      comment: {en: "A pm:Action is an instance of a pm:Method."},
      domain: "https://vocab.methodandstructure.com/process-model#Method",
      inverseOf: "https://vocab.methodandstructure.com/process-model#method",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "instance",
      range: "https://vocab.methodandstructure.com/process-model#Action",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"involved-in",
      comment: {en: "A foaf:Agent may be involved in a pm:Task"},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "involved-in",
      range: "http://xmlns.com/foaf/0.1/Agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :involves,
      comment: {en: "A pm:Action may involve one or more foaf:Agent."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      inverseOf: "https://vocab.methodandstructure.com/process-model#involved-in",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "involves",
      range: "http://xmlns.com/foaf/0.1/Agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :method,
      comment: {en: "A pm:Action can identify an associated pm:Method which will be used to complete it."},
      domain: ["https://vocab.methodandstructure.com/process-model#Action", "https://vocab.methodandstructure.com/process-model#Task"],
      inverseOf: "https://vocab.methodandstructure.com/process-model#instance",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "method",
      range: "https://vocab.methodandstructure.com/process-model#Method",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :outcome,
      comment: {en: "Every pm:Action has an outcome, which is some kind of ibis:State."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      inverseOf: "https://vocab.methodandstructure.com/process-model#outcome",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "outcome",
      range: "https://vocab.methodandstructure.com/ibis#State",
      subPropertyOf: ["http://purl.org/NET/c4dm/event.owl#product", "http://www.w3.org/ns/prov#generated"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :performer,
      comment: {en: "A performer is a real, live (at the time of performance) person who performs a task."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      inverseOf: "https://vocab.methodandstructure.com/process-model#performs",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "performer",
      range: "http://xmlns.com/foaf/0.1/Person",
      subPropertyOf: ["http://purl.org/NET/c4dm/event.owl#agent", "http://www.w3.org/ns/prov#wasAttributedTo", "https://vocab.methodandstructure.com/process-model#involves"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :performs,
      comment: {en: ""},
      domain: "http://xmlns.com/foaf/0.1/Person",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "performs",
      range: "https://vocab.methodandstructure.com/process-model#Action",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :process,
      comment: {en: "When an ibis:State is the outcome of a pm:Action."},
      domain: "https://vocab.methodandstructure.com/ibis#State",
      inverseOf: "https://vocab.methodandstructure.com/process-model#outcome",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "process",
      range: "https://vocab.methodandstructure.com/process-model#Action",
      subPropertyOf: "http://www.w3.org/ns/prov#wasGeneratedBy",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :recipient,
      comment: {en: "A recipient of a task is a (real, live at the time of receipt) person who receives and approves its product or products."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      inverseOf: "https://vocab.methodandstructure.com/process-model#responsible-for",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "recipient",
      range: "http://xmlns.com/foaf/0.1/Person",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"recipient-of",
      comment: {en: ""},
      domain: "http://xmlns.com/foaf/0.1/Person",
      inverseOf: "https://vocab.methodandstructure.com/process-model#recipient",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "recipient-of",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :responsible,
      comment: {en: "The person responsible for a task may not be the one performing it, but they are the one accountable for its completion."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      inverseOf: "https://vocab.methodandstructure.com/process-model#responsible-for",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "responsible",
      range: "http://xmlns.com/foaf/0.1/Person",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"responsible-for",
      comment: {en: ""},
      domain: "http://xmlns.com/foaf/0.1/Person",
      inverseOf: "https://vocab.methodandstructure.com/process-model#responsible",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "responsible-for",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :status,
      comment: {en: "The status of a Task at any instant is a State."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "status",
      range: "https://vocab.methodandstructure.com/ibis#State",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :subtask,
      comment: {en: "This property narrows the domain and range of ev:sub_event to pm:Task."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "subtask",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      subPropertyOf: ["http://purl.org/NET/c4dm/event.owl#sub_event", "https://vocab.methodandstructure.com/ibis#generalizes"],
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :supertask,
      comment: {en: ""},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      inverseOf: "https://vocab.methodandstructure.com/process-model#subtask",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "supertask",
      range: "https://vocab.methodandstructure.com/process-model#Task",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#specializes",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :variant,
      comment: {en: "A variant is an alternate method of performing the same action."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "variant",
      range: "https://vocab.methodandstructure.com/process-model#Action",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related",
      type: ["http://www.w3.org/2002/07/owl#ObjectProperty", "http://www.w3.org/2002/07/owl#SymmetricProperty"]
    property :"wanted-by",
      comment: {en: "The identifying feature of a pm:Goal is that somebody wants it to happen."},
      domain: "https://vocab.methodandstructure.com/process-model#Goal",
      inverseOf: "https://vocab.methodandstructure.com/process-model#wants",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "wanted-by",
      range: "http://xmlns.com/foaf/0.1/Agent",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#endorsed-by",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :wants,
      comment: {en: "A foaf:Agent may want a pm:Goal."},
      domain: "http://xmlns.com/foaf/0.1/Agent",
      inverseOf: "https://vocab.methodandstructure.com/process-model#wanted-by",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: "wants",
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#endorses",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"

    # Extra definitions
    term :ABORTED,
      comment: {en: "The task is aborted."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "ABORTED"},
      type: "https://vocab.methodandstructure.com/ibis#State"
    term :COMPLETE,
      comment: {en: "The task is complete."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "COMPLETE"},
      type: "https://vocab.methodandstructure.com/ibis#State"
    term :"IN-PROGRESS",
      comment: {en: "The task is in progress."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "IN-PROGRESS"},
      type: "https://vocab.methodandstructure.com/ibis#State"
    term :STALLED,
      comment: {en: "The task is stalled."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "STALLED"},
      type: "https://vocab.methodandstructure.com/ibis#State"
    term :"ch-classes",
      "http://purl.org/dc/terms/references": ["http://motools.sourceforge.net/event/event.html#term_Event", "https://www.w3.org/TR/prov-o/#Activity"]

    RDF::Vocabulary.register :pm, self if
      RDF::Vocabulary.respond_to? :register
  end
end
