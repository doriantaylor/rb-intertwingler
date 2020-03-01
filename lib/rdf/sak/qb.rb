# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from http://purl.org/linked-data/cube#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <http://purl.org/linked-data/cube#>
  #   class QB < RDF::StrictVocabulary
  #   end
  class QB < RDF::StrictVocabulary("http://purl.org/linked-data/cube#")

    # Ontology definition
    ontology :"http://purl.org/linked-data/cube#",
      comment: %(This vocabulary allows multi-dimensional data, such as statistics, to be published in RDF. It is based on the core information model from SDMX \(and thus also DDI\).).freeze,
      "dc:contributor": [term(
          "foaf:mbox": "arofan.gregory@earthlink.net".freeze
        ), term(
          "foaf:mbox": "dave@epimorphics.com".freeze
        ), term(
          "foaf:mbox": "ian@epimorphics.com".freeze
        ), term(
          "foaf:mbox": "jeni@jenitennison.com".freeze
        ), term(
          "foaf:mbox": "richard@cyganiak.de".freeze
        )],
      "dc:created": "2010-07-12".freeze,
      "dc:license": "http://www.opendatacommons.org/licenses/pddl/1.0/".freeze,
      "dc:modified": ["2010-11-27".freeze, "2013-03-02".freeze, "2013-07-26".freeze],
      "dc:title": "Vocabulary for multi-dimensional (e.g. statistical) data publishing".freeze,
      label: "The data cube vocabulary".freeze,
      "owl:versionInfo": "0.2".freeze,
      type: "owl:Ontology".freeze

    # Class definitions
    term :Attachable,
      comment: %(Abstract superclass for everything that can have attributes and dimensions).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Attachable (abstract)".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :AttributeProperty,
      comment: %(The class of components which represent attributes of observations in the cube, e.g. unit of measurement).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Attribute property".freeze,
      "owl:disjointWith": "qb:MeasureProperty".freeze,
      subClassOf: "qb:ComponentProperty".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :CodedProperty,
      comment: %(Superclass of all coded ComponentProperties).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Coded property".freeze,
      subClassOf: "qb:ComponentProperty".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :ComponentProperty,
      comment: %(Abstract super-property of all properties representing dimensions, attributes or measures).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Component property (abstract)".freeze,
      subClassOf: "rdf:Property".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :ComponentSet,
      comment: %(Abstract class of things which reference one or more ComponentProperties).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Component set".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :ComponentSpecification,
      comment: %(Used to define properties of a component \(attribute, dimension etc\) which are specific to its usage in a DSD.).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Component specification".freeze,
      subClassOf: "qb:ComponentSet".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :DataSet,
      comment: %(Represents a collection of observations, possibly organized into various slices, conforming to some common dimensional structure.).freeze,
      equivalentClass: "http://purl.org/NET/scovo#Dataset".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Data set".freeze,
      subClassOf: "qb:Attachable".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :DataStructureDefinition,
      comment: %(Defines the structure of a DataSet or slice).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Data structure definition".freeze,
      subClassOf: "qb:ComponentSet".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :DimensionProperty,
      comment: %(The class of components which represent the dimensions of the cube).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Dimension property".freeze,
      "owl:disjointWith": "qb:MeasureProperty".freeze,
      subClassOf: ["qb:CodedProperty".freeze, "qb:ComponentProperty".freeze],
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :HierarchicalCodeList,
      comment: %(Represents a generalized hierarchy of concepts which can be used for coding. The hierarchy is defined by one or more roots together with a property which relates concepts in the hierarchy to thier child concept .  The same concepts may be members of multiple hierarchies provided that different qb:parentChildProperty values are used for each hierarchy.).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Hierarchical Code List".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :MeasureProperty,
      comment: %(The class of components which represent the measured value of the phenomenon being observed).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Measure property".freeze,
      "owl:disjointWith": ["qb:AttributeProperty".freeze, "qb:DimensionProperty".freeze],
      subClassOf: "qb:ComponentProperty".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :Observation,
      comment: %(A single observation in the cube, may have one or more associated measured values).freeze,
      equivalentClass: "http://purl.org/NET/scovo#Item".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Observation".freeze,
      subClassOf: "qb:Attachable".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :ObservationGroup,
      comment: %(A, possibly arbitrary, group of observations.).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Observation Group".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :Slice,
      comment: %(Denotes a subset of a DataSet defined by fixing a subset of the dimensional values, component properties on the Slice).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Slice".freeze,
      subClassOf: ["qb:Attachable".freeze, "qb:ObservationGroup".freeze],
      type: ["owl:Class".freeze, "rdfs:Class".freeze]
    term :SliceKey,
      comment: %(Denotes a subset of the component properties of a DataSet which are fixed in the corresponding slices).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "Slice key".freeze,
      subClassOf: "qb:ComponentSet".freeze,
      type: ["owl:Class".freeze, "rdfs:Class".freeze]

    # Property definitions
    property :attribute,
      comment: %(An alternative to qb:componentProperty which makes explicit that the component is a attribute).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "attribute".freeze,
      range: "qb:AttributeProperty".freeze,
      subPropertyOf: "qb:componentProperty".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :codeList,
      comment: %(gives the code list associated with a CodedProperty).freeze,
      domain: "qb:CodedProperty".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "code list".freeze,
      range: term(
          unionOf: list("skos:ConceptScheme".freeze, "skos:Collection".freeze, "qb:HierarchicalCodeList".freeze)
        ),
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :component,
      comment: %(indicates a component specification which is included in the structure of the dataset).freeze,
      domain: "qb:DataStructureDefinition".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "component specification".freeze,
      range: "qb:ComponentSpecification".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :componentAttachment,
      comment: %(Indicates the level at which the component property should be attached, this might an qb:DataSet, qb:Slice or qb:Observation, or a qb:MeasureProperty.).freeze,
      domain: "qb:ComponentSpecification".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "component attachment".freeze,
      range: "rdfs:Class".freeze,
      type: "rdf:Property".freeze
    property :componentProperty,
      comment: %(indicates a ComponentProperty \(i.e. attribute/dimension\) expected on a DataSet, or a dimension fixed in a SliceKey).freeze,
      domain: "qb:ComponentSet".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "component".freeze,
      range: "qb:ComponentProperty".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :componentRequired,
      comment: %(Indicates whether a component property is required \(true\) or optional \(false\) in the context of a DSD. Only applicable
    to components correspond to an attribute. Defaults to false \(optional\).).freeze,
      domain: "qb:ComponentSpecification".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "component required".freeze,
      range: "xsd:boolean".freeze,
      type: ["owl:DatatypeProperty".freeze, "rdf:Property".freeze]
    property :concept,
      comment: %(gives the concept which is being measured or indicated by a ComponentProperty).freeze,
      domain: "qb:ComponentProperty".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "concept".freeze,
      range: "skos:Concept".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :dataSet,
      comment: %(indicates the data set of which this observation is a part).freeze,
      domain: "qb:Observation".freeze,
      equivalentProperty: "http://purl.org/NET/scovo#dataset".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "data set".freeze,
      range: "qb:DataSet".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :dimension,
      comment: %(An alternative to qb:componentProperty which makes explicit that the component is a dimension).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "dimension".freeze,
      range: "qb:DimensionProperty".freeze,
      subPropertyOf: "qb:componentProperty".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :hierarchyRoot,
      comment: %(Specifies a root of the hierarchy. A hierarchy may have multiple roots but must have at least one.).freeze,
      domain: "qb:HierarchicalCodeList".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :measure,
      comment: %(An alternative to qb:componentProperty which makes explicit that the component is a measure).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "measure".freeze,
      range: "qb:MeasureProperty".freeze,
      subPropertyOf: "qb:componentProperty".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :measureDimension,
      comment: %(An alternative to qb:componentProperty which makes explicit that the component is a measure dimension).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "measure dimension".freeze,
      range: "qb:DimensionProperty".freeze,
      subPropertyOf: "qb:componentProperty".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :measureType,
      comment: %(Generic measure dimension, the value of this dimension indicates which measure \(from the set of measures in the DSD\) is being given by the obsValue \(or other primary measure\)).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "measure type".freeze,
      range: "qb:MeasureProperty".freeze,
      type: ["qb:DimensionProperty".freeze, "rdf:Property".freeze]
    property :observation,
      comment: %(indicates a observation contained within this slice of the data set).freeze,
      domain: "qb:ObservationGroup".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "observation".freeze,
      range: "qb:Observation".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :observationGroup,
      comment: %(Indicates a group of observations. The domain of this property is left open so that a group may be attached to different resources and need not be restricted to a single DataSet).freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "observation group".freeze,
      range: "qb:ObservationGroup".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :order,
      comment: %(indicates a priority order for the components of sets with this structure, used to guide presentations - lower order numbers come before higher numbers, un-numbered components come last).freeze,
      domain: "qb:ComponentSpecification".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "order".freeze,
      range: "xsd:int".freeze,
      type: ["owl:DatatypeProperty".freeze, "rdf:Property".freeze]
    property :parentChildProperty,
      comment: %(Specifies a property which relates a parent concept in the hierarchy to a child concept.).freeze,
      domain: "qb:HierarchicalCodeList".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "parent-child property".freeze,
      range: "rdf:Property".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :slice,
      comment: %(Indicates a subset of a DataSet defined by fixing a subset of the dimensional values).freeze,
      domain: "qb:DataSet".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "slice".freeze,
      range: "qb:Slice".freeze,
      subPropertyOf: "qb:observationGroup".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :sliceKey,
      comment: %(indicates a slice key which is used for slices in this dataset).freeze,
      domain: "qb:DataStructureDefinition".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "slice key".freeze,
      range: "qb:SliceKey".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :sliceStructure,
      comment: %(indicates the sub-key corresponding to this slice).freeze,
      domain: "qb:Slice".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "slice structure".freeze,
      range: "qb:SliceKey".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
    property :structure,
      comment: %(indicates the structure to which this data set conforms).freeze,
      domain: "qb:DataSet".freeze,
      isDefinedBy: "http://purl.org/linked-data/cube".freeze,
      label: "structure".freeze,
      range: "qb:DataStructureDefinition".freeze,
      type: ["owl:ObjectProperty".freeze, "rdf:Property".freeze]
  end
end
