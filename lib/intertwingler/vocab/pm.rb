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
  #   class PM < RDF::StrictVocabulary
  #     # An Action specializes an Event in that it is performed by (at least one) real, living Person.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Action
  #
  #     # A Goal extends a State by way of being explicitly desired by an Agent.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Goal
  #
  #     # A method specifies an abstract sequence of events.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Method
  #
  #     # A State can be understood as a snapshot of a system at a given time, such as before or after an event.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :State
  #
  #     # A Target connects an abstract Goal to a specific Task, budget and deadline.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Target
  #
  #     # A Task specializes an Action in that it has one or more Goals, and connects a Method of execution with a responsible Person who will carry it out.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Task
  #
  #     # The purpose of a task is to achieve a goal. All tasks specified in this vocabulary must also specify the goal they are intended to achieve.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :achieves
  #
  #     # By anchoring a Goal to a Target, we give it a concrete budget and deadline.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :anchors
  #
  #     # All Targets require a quantitative budget.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :budget
  #
  #     # The context of an Action is a State.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :context
  #
  #     # A State the Action depends on to be actionable.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :dependency
  #
  #     # A foaf:Agent may desire a Goal.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :desires
  #
  #     # All Targets must have a due date.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :due
  #
  #     # A valid target must initiate exactly one task to carry it out.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :initiates
  #
  #     # All Tasks must identify an associated Method which will be used to complete them.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :method
  #
  #     # All Actions have an outcome, which is some kind of State.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :outcome
  #
  #     # A performer is a real, live (at the time of performance) person who performs a task.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :performer
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
  #     # This property narrows the domain and range of ev:sub_event to Tasks.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :subtask
  #
  #     # A variant is an alternate method of performing the same action.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :variant
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
      "http://purl.org/dc/terms/modified": ["2014-02-06T22:26:21Z", "2017-04-06T22:24:06Z", "2020-03-08T18:58:53Z"],
      "http://purl.org/dc/terms/title": {en: "A Process Model Ontology"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/process-model#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "pm",
      "http://purl.org/vocab/vann/preferredNamespaceUri": "https://vocab.methodandstructure.com/process-model#",
      "http://www.w3.org/1999/xhtml/vocab#contents": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#index": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#top": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/1999/xhtml/vocab#up": "https://vocab.methodandstructure.com/",
      "http://www.w3.org/2002/07/owl#imports": ["http://purl.org/NET/c4dm/event.owl#", "http://www.w3.org/2004/02/skos/core#", "https://vocab.methodandstructure.com/ibis#"],
      "http://www.w3.org/ns/rdfa#usesVocabulary": "http://www.w3.org/1999/xhtml/vocab#",
      "http://www.w3.org/ns/shacl#namespace": "https://vocab.methodandstructure.com/process-model#",
      "http://www.w3.org/ns/shacl#prefix": "pm",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      type: ["http://purl.org/ontology/bibo/Specification", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Action,
      comment: {en: "An Action specializes an Event in that it is performed by (at least one) real, living Person."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/process-model#State",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "Action"},
      subClassOf: "http://purl.org/NET/c4dm/event.owl#Event",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Goal,
      comment: {en: "A Goal extends a State by way of being explicitly desired by an Agent."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "Goal"},
      note: {en: "A Goal is not actionable itself, although Tasks, which are Actions, must achieve at least one Goal. At the time of observation, a Goal, like its superclass, State, is either true or false. This entails that a Goal is not expressed in terms of quantitative results or deadlines. That is the job of a Target."},
      subClassOf: ["https://vocab.methodandstructure.com/ibis#Issue", "https://vocab.methodandstructure.com/process-model#State"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Method,
      comment: {en: "A method specifies an abstract sequence of events."},
      editorialNote: {en: "This class is provisional. It isn't entirely clear that we need a distinct Method class, since Tasks and the like could be appropriated as prototypes of methods."},
      "http://www.w3.org/2002/07/owl#disjointWith": "https://vocab.methodandstructure.com/process-model#State",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "Method"},
      subClassOf: "https://vocab.methodandstructure.com/process-model#Action",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :State,
      comment: {en: "A State can be understood as a snapshot of a system at a given time, such as before or after an event."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "State"},
      note: {en: "A State is distinct from a particular instant, but it is analogous to it. At the time of observation, a State is either true or false."},
      subClassOf: "http://www.w3.org/2004/02/skos/core#Concept",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Target,
      comment: {en: "A Target connects an abstract Goal to a specific Task, budget and deadline."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "Target"},
      note: {en: "The logical separation of a Target from a Goal makes it possible to speak of a Goal in abstract terms, and generate several equivalent candidate Tasks, each with one or more candidate Methods, which may achieve it."},
      subClassOf: "https://vocab.methodandstructure.com/process-model#Goal",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Task,
      comment: {en: "A Task specializes an Action in that it has one or more Goals, and connects a Method of execution with a responsible Person who will carry it out."},
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Since pm:Task is a descendant of ev:Event, we can use the ev:time relation to specify a time:Interval to record the time taken to complete a task."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "Task"},
      subClassOf: ["https://vocab.methodandstructure.com/ibis#Position", "https://vocab.methodandstructure.com/process-model#Action"],
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :achieves,
      comment: {en: "The purpose of a task is to achieve a goal. All tasks specified in this vocabulary must also specify the goal they are intended to achieve."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "achieves"},
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: "https://vocab.methodandstructure.com/process-model#outcome",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :anchors,
      comment: {en: "By anchoring a Goal to a Target, we give it a concrete budget and deadline."},
      domain: ["https://vocab.methodandstructure.com/process-model#Target", "https://vocab.methodandstructure.com/process-model#Task"],
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "anchors"},
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#specializes",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :budget,
      cardinality: "1",
      comment: {en: "All Targets require a quantitative budget."},
      domain: ["https://vocab.methodandstructure.com/process-model#Target", "https://vocab.methodandstructure.com/process-model#Task"],
      editorialNote: {en: "It is not clear, prior to implementation, if we should keep the budget as a raw quantity, or create some kind of resource envelope object."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "budget"},
      range: "http://www.w3.org/2001/XMLSchema#decimal",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :context,
      comment: {en: "The context of an Action is a State."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "The pm:context and pm:outcome properties can be used to chain pm:Actions together."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "context"},
      range: "https://vocab.methodandstructure.com/process-model#State",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#factor",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :dependency,
      comment: {en: "A State the Action depends on to be actionable."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "dependency"},
      range: "https://vocab.methodandstructure.com/process-model#State",
      subPropertyOf: "https://vocab.methodandstructure.com/ontology/ibis/1#suggests",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :desires,
      comment: {en: "A foaf:Agent may desire a Goal."},
      domain: "http://xmlns.com/foaf/0.1/Agent",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "desires"},
      range: "https://vocab.methodandstructure.com/process-model#Goal",
      subPropertyOf: "https://vocab.methodandstructure.com/ibis#endorses",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :due,
      cardinality: "1",
      comment: {en: "All Targets must have a due date."},
      domain: ["https://vocab.methodandstructure.com/process-model#Target", "https://vocab.methodandstructure.com/process-model#Task"],
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "due"},
      range: "http://www.w3.org/2001/XMLSchema#dateTime",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :initiates,
      cardinality: "1",
      comment: {en: "A valid target must initiate exactly one task to carry it out."},
      domain: "https://vocab.methodandstructure.com/process-model#Target",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "initiates"},
      range: "https://vocab.methodandstructure.com/process-model#Task",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :method,
      comment: {en: "All Tasks must identify an associated Method which will be used to complete them."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "method"},
      range: "https://vocab.methodandstructure.com/process-model#Method",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :outcome,
      comment: {en: "All Actions have an outcome, which is some kind of State."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "outcome"},
      range: "https://vocab.methodandstructure.com/process-model#State",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#product",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :performer,
      comment: {en: "A performer is a real, live (at the time of performance) person who performs a task."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "performer"},
      range: "http://xmlns.com/foaf/0.1/Person",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :recipient,
      comment: {en: "A recipient of a task is a (real, live at the time of receipt) person who receives and approves its product or products."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "recipient"},
      range: "http://xmlns.com/foaf/0.1/Person",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :responsible,
      comment: {en: "The person responsible for a task may not be the one performing it, but they are the one accountable for its completion."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "responsible"},
      range: "http://xmlns.com/foaf/0.1/Person",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#agent",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :status,
      comment: {en: "The status of a Task at any instant is a State."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "status"},
      range: "https://vocab.methodandstructure.com/process-model#State",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :subtask,
      comment: {en: "This property narrows the domain and range of ev:sub_event to Tasks."},
      domain: "https://vocab.methodandstructure.com/process-model#Task",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "subtask"},
      range: "https://vocab.methodandstructure.com/process-model#Task",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#sub_event",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :variant,
      comment: {en: "A variant is an alternate method of performing the same action."},
      domain: "https://vocab.methodandstructure.com/process-model#Action",
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "variant"},
      range: "https://vocab.methodandstructure.com/process-model#Action",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#sub_event",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"

    # Extra definitions
    term :ABORTED,
      comment: {en: "The task is aborted."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "ABORTED"},
      type: "https://vocab.methodandstructure.com/process-model#State"
    term :COMPLETE,
      comment: {en: "The task is complete."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "COMPLETE"},
      type: "https://vocab.methodandstructure.com/process-model#State"
    term :"IN-PROGRESS",
      comment: {en: "The task is in progress."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "IN-PROGRESS"},
      type: "https://vocab.methodandstructure.com/process-model#State"
    term :STALLED,
      comment: {en: "The task is stalled."},
      isDefinedBy: "https://vocab.methodandstructure.com/process-model#",
      label: {en: "STALLED"},
      type: "https://vocab.methodandstructure.com/process-model#State"
    term :"ch-classes",
      "http://purl.org/dc/terms/references": "http://motools.sourceforge.net/event/event.html#term_Event"

    RDF::Vocabulary.register :pm, self if
      RDF::Vocabulary.respond_to? :register
  end
end
