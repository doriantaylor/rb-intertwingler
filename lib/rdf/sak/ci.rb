# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://vocab.methodandstructure.com/content-inventory#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://vocab.methodandstructure.com/content-inventory#>
  #   #
  #   # A Content Inventory Vocabulary
  #   #
  #   # This vocabulary defines a number of concepts peculiar to content strategy which are not accounted for by other vocabularies.
  #   # @version 0.14
  #   class CI < RDF::StrictVocabulary
  #     # This is an explicit document abstract/executive summary class, intended to belong to BIBO, which appears to be abandonware.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Abstract
  #
  #     # An action, as its name implies, is meant to represent something a person or other agent ought to do to a document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Action
  #
  #     # In general there is no programmatic way to tell whether a resource is an advertisement, since advertisements on the Web look (to a machine) like any other resource. This is intended to be a decorator class to indicate that the subject is an advertisement. It can therefore be combined with other classes such as foaf:Image, or bibo:AudioVisualDocument.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Advertisement
  #
  #     # This is an explicit document appendix class, intended to belong to BIBO, which appears to be abandonware.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Appendix
  #
  #     # An audience represents the set of people who are the intended recipients of the resource. This class is at once an agent class as well as a conceptual entity, capable of being mixed into a SKOS concept scheme.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Audience
  #
  #     # This is intended to represent a generic block-level segment, such as a paragraph, list, figure, or table.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Block
  #
  #     # In order to merge a document, we must define the target to which it ought to be merged. This class is identical to an Action, save for such a property.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Merge
  #
  #     # This is an explicit document section (i.e., sub-chapter) class.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Section
  #
  #     # Identifies a variable which can be embedded into a document and assigned an rdf:value.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Variable
  #
  #     # Relates a document to an action to take.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :action
  #
  #     # Denotes an alternate URI for the subject resource. It extends owl:sameAs insofar as asserting that the object is somehow less canonical than the subject.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :alias
  #
  #     # The document assumes the audience is familiar with this concept, and may not mention it explicitly.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :assumes
  #
  #     # A block count is conceptually similar to a word or section count, though it counts the total of elements in the document considered to be text blocks, such as paragraphs, tables, lists and figures. It is suited for document types that have no concept of (semantic) sections, such as HTML. The purpose of this measurement is to provide a sort of ratio to the word count, to glean how well-proportioned the document is.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :blocks
  #
  #     # Asserts the canonical URI of the subject resource, i.e., the one you always want to publish in content or redirect Web requests to.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :canonical
  #
  #     # This indicates the number of characters in a document, with punctuation and the XPath normalize-space function applied. Note this is characters, not bytes.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :characters
  #
  #     # The document explicitly depicts this concept (or other entity).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :depicts
  #
  #     # Document Reference
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :document
  #
  #     # This property specifies an embedded resource, such as an image, which is visible on the document's canvas.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :embed
  #
  #     # The number of embeds in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :embeds
  #
  #     # This property relates an Audience to a SKOS concept that members of the audience are known to actively avoid or regard with contempt. This relation is intended to represent the complement of values.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :eschews
  #
  #     # The document evokes the given concept without mentioning it explicitly.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :evokes
  #
  #     # This property relates an Audience to a specific foaf:Person who is an exemplar of the audience.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :exemplar
  #
  #     # This property specifies form target, which may or may not be visible to the user.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :form
  #
  #     # The number of forms in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :forms
  #
  #     # This indicates the number of images in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :images
  #
  #     # This property specifies a related resource which is not directly visible to the user.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :include
  #
  #     # The number of links pointing at the specified resource.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :indegree
  #
  #     # This is a boolean value to indicate whether or not a resource ought to be indexed. It does not necessarily ascribe a policy: an absence of an explicit true value does not necessarily imply the resource ought not be indexed, but the presence of a false value probably should.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :indexed
  #
  #     # The document defines, describes, or otherwise introduces the audience to this concept.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :introduces
  #
  #     # This property specifies an ordinary hyperlink, which is visible on the document's canvas.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :link
  #
  #     # The number of lists in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :lists
  #
  #     # Maximum
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :max
  #
  #     # Mean
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :mean
  #
  #     # The median of a population 
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :median
  #
  #     # The document explicitly mentions this concept.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :mentions
  #
  #     # The smallest observation in the sample.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :min
  #
  #     # The number of links emanating from the specified resource.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :outdegree
  #
  #     # Denotes the primary variant that concretely represents the resource.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :primary
  #
  #     # Denotes a resource that is a concrete representation of the subject, which assumed to be more abstract.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :representation
  #
  #     # The number of scripts in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :scripts
  #
  #     # Standard Deviation
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :sd
  #
  #     # For document types that have a concrete representation of sections, this property may be used to capture their sum.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :sections
  #
  #     # The slug is a text token which represents either the full path or terminal path segment of an HTTP(S) URL by which a resource can be located. This property is mainly for the purpose of archiving old or alternative URL paths in a content inventory, for such tasks as generating URL rewriting maps.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :slug
  #
  #     # The number of stylesheets in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :stylesheets
  #
  #     # The number of tables in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :tables
  #
  #     # Specify the URI of the target resource into which this document should be merged.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :target
  #
  #     # This property relates an Audience to a SKOS concept that members of the audience are known to comprehend, and thereby do not need any additional explanation.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :understands
  #
  #     # This property relates an Audience to a SKOS concept that members of the audience are known to value, that is, to find meaningful in an axiological sense.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :values
  #
  #     # Denotes a resource that is a variant of a concrete representation of the subject, which assumed to be more abstract.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :variant
  #
  #     # The number of videos in the document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :videos
  #
  #     # This indicates the number of words in a document, similar to the familiar function in a word processor. The exact method of counting words may vary by document type, language etc., and is thus out of scope from this document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :words
  #
  #     # The document is available for select people to see, but not published in the strict literal sense.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :circulated
  #
  #     # The document is confidential and not for publication.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :confidential
  #
  #     # The document contains no content.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :empty
  #
  #     # The document has been started, but is clearly not finished.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :incomplete
  #
  #     # The content of this document is factually wrong.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :incorrect
  #
  #     # Keep this document to which this is associated; make no changes to it at this time.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :keep
  #
  #     # The resource is a landing page from some other medium (e.g. e-mail, television, billboard). This status is a hint to automated systems which would otherwise orphan or retire a landing page with no inbound links.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :landing
  #
  #     # The content of this document was correct and relevant at one point, but external circumstances have caused it to lapse in relevance or factual accuracy.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :obsolete
  #
  #     # The resource is not explicitly pending or removed from publication, however it has managed to be disconnected from the rest of the site: There is no path to it from a landing page, and it is not a landing page on its own. That is to say that the resource either has no inbound links, or if it does, those links are from other resources that are in the same situation. Documents which are only linked from retired documents are also considered orphans.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :orphan
  #
  #     # Proofread this document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :proofread
  #
  #     # Remove all references to this document and consign it to the archive.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :retire
  #
  #     # The document has been explicitly retired by an editor or curator, but still exists in the archive.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :retired
  #
  #     # Revise or restructure this document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :revise
  #
  #     # Rewrite this document from scratch.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :rewrite
  #
  #     # Split this document into multiple pieces.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :split
  #
  #     # The resource at the subject address is unavailable for reasons other than explicit retirement, e.g. HTTP 404 or 403, or going out of print.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :unavailable
  #
  #   end
  CI = Class.new(RDF::StrictVocabulary("https://vocab.methodandstructure.com/content-inventory#")) do

    # Ontology definition
    ontology :"https://vocab.methodandstructure.com/content-inventory#",
      comment: {en: "This vocabulary defines a number of concepts peculiar to content strategy which are not accounted for by other vocabularies."},
      "http://purl.org/dc/terms/created": "2012-01-23T11:52:00-08:00",
      "http://purl.org/dc/terms/creator": "https://doriantaylor.com/person/dorian-taylor#me",
      "http://purl.org/dc/terms/modified": ["2012-12-11T22:22:00-08:00", "2014-02-06T14:10:00-08:00", "2015-02-03T14:39:00-08:00", "2017-04-06T15:24:00-07:00", "2018-10-06T16:23:52Z", "2019-03-05T23:38:59Z", "2019-04-07T16:36:10Z", "2019-04-18T01:01:09Z", "2019-07-07T22:10:55Z", "2019-07-10T22:28:06Z", "2019-07-21T23:05:32Z", "2019-09-04T20:27:32Z", "2020-01-26T05:02:30Z", "2020-04-24T23:16:20Z", "2020-04-30T01:05:51Z", "2020-06-29T02:24:58Z", "2020-07-04T01:24:22Z", "2020-11-13T03:27:35Z", "2021-05-17T17:57:27Z", "2022-10-05T10:36:04Z", "2022-11-02T19:19:18Z"],
      "http://purl.org/dc/terms/references": ["http://en.wikipedia.org/wiki/Content_strategy", "http://en.wikipedia.org/wiki/Five-number_summary", "http://en.wikipedia.org/wiki/Mean", "http://en.wikipedia.org/wiki/Standard_deviation", "http://vocab.org/frbr/core", "http://vocab.org/frbr/extended", "http://www.w3.org/TR/vocab-data-cube/", "http://www.w3.org/TR/vocab-data-cube/#ref_qb_DataSet", "https://www.w3.org/TR/prov-o/", "https://www.w3.org/TR/vocab-data-cube/#ref_qb_DataStructureDefinition", "https://www.w3.org/TR/vocab-data-cube/#ref_qb_DimensionProperty", "https://www.w3.org/TR/vocab-data-cube/#ref_qb_MeasureProperty", "https://www.w3.org/TR/vocab-data-cube/#ref_qb_Observation"],
      "http://purl.org/dc/terms/subject": "https://vocab.methodandstructure.com/content-inventory#",
      "http://purl.org/dc/terms/title": {en: "A Content Inventory Vocabulary"},
      "http://purl.org/ontology/bibo/uri": "https://vocab.methodandstructure.com/content-inventory#",
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "ci",
      "http://www.w3.org/1999/xhtml/vocab#license": "http://creativecommons.org/licenses/by/2.5/ca/",
      "http://www.w3.org/2002/07/owl#imports": ["http://purl.org/NET/c4dm/event.owl#", "http://purl.org/dc/terms/", "http://purl.org/linked-data/cube#", "http://purl.org/ontology/bibo/", "http://www.w3.org/2001/XMLSchema#", "http://www.w3.org/2002/07/owl#", "http://www.w3.org/2004/02/skos/core#", "http://xmlns.com/foaf/0.1/"],
      "http://www.w3.org/2002/07/owl#versionInfo": "0.14",
      "http://xmlns.com/foaf/0.1/primaryTopic": "https://vocab.methodandstructure.com/content-inventory#",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      type: ["http://purl.org/ontology/bibo/Webpage", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Abstract,
      comment: {en: "This is an explicit document abstract/executive summary class, intended to belong to BIBO, which appears to be abandonware."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Abstract",
      subClassOf: "http://purl.org/ontology/bibo/DocumentPart",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Action,
      comment: {en: "An action, as its name implies, is meant to represent something a person or other agent ought to do to a document."},
      "http://www.w3.org/2004/02/skos/core#usageNote": {en: "Being a subclass of an event, a ci:Action can have agents, factors, products, places and times ascribed to it."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Action",
      subClassOf: "http://purl.org/NET/c4dm/event.owl#Event",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Advertisement,
      comment: {en: "In general there is no programmatic way to tell whether a resource is an advertisement, since advertisements on the Web look (to a machine) like any other resource. This is intended to be a decorator class to indicate that the subject is an advertisement. It can therefore be combined with other classes such as foaf:Image, or bibo:AudioVisualDocument."},
      "http://www.w3.org/2004/02/skos/core#example": {en: "@prefix bibo: <http://purl.org/ontology/bibo/> .\n@prefix dct:  <http://purl.org/dc/terms/> .\n@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n@prefix ci:   <https://vocab.methodandstructure.com/content-inventory#> .\n\n<https://example.club/17-mindblowing-ways-to-write-listicles> a bibo:Article ;\n  dct:title \"17 Mindblowing Ways to Write Listicles!\"@en ;\n  dct:hasPart <https://adtech.biz/assets/punch-the-monkey> .\n\n<https://adtech.biz/assets/punch-the-monkey> a foaf:Image, ci:Advertisement ;\n  dct:title \"Punch The Monkey And WIN!\#@$!%%^!\"@en ."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Advertisement",
      subClassOf: "http://xmlns.com/foaf/0.1/Document",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Appendix,
      comment: {en: "This is an explicit document appendix class, intended to belong to BIBO, which appears to be abandonware."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://github.com/structureddynamics/Bibliographic-Ontology-BIBO/pull/17",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Appendix",
      subClassOf: "http://purl.org/ontology/bibo/DocumentPart",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Audience,
      comment: {en: "An audience represents the set of people who are the intended recipients of the resource. This class is at once an agent class as well as a conceptual entity, capable of being mixed into a SKOS concept scheme."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": ["http://purl.org/dc/terms/audience", "http://www.w3.org/ns/org#Role"],
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Audience",
      subClassOf: ["http://purl.org/dc/terms/AgentClass", "http://www.w3.org/2004/02/skos/core#Concept"],
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Block,
      comment: {en: "This is intended to represent a generic block-level segment, such as a paragraph, list, figure, or table."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Block",
      subClassOf: "http://purl.org/ontology/bibo/DocumentPart",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Merge,
      comment: {en: "In order to merge a document, we must define the target to which it ought to be merged. This class is identical to an Action, save for such a property."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Merge",
      subClassOf: "https://vocab.methodandstructure.com/content-inventory#Action",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Section,
      comment: {en: "This is an explicit document section (i.e., sub-chapter) class."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Section",
      subClassOf: "http://purl.org/ontology/bibo/DocumentPart",
      type: "http://www.w3.org/2002/07/owl#Class"
    term :Variable,
      comment: {en: "Identifies a variable which can be embedded into a document and assigned an rdf:value."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://www.w3.org/1999/02/22-rdf-syntax-ns#value",
      "http://www.w3.org/2004/02/skos/core#example": {en: "\n            Consider the markup:\n            <p about=\"https://example.biz/ratecard#\">Shingy's bill-out rate is <span rel=\"dct:references\">\n  <var about=\"#rate\" typeof=\"ci:Variable\" property=\"rdf:value\" datatype=\"xsd:decimal\">100.00</var>\n  <var about=\"#unit\" typeof=\"ci:Variable\" property=\"rdf:value\" datatype=\"xsd:token\">USD</var>\n</span> per hour.</p>\n            …which when parsed will produce the statements:\n            @base <https://example.biz/ratecard#> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n@prefix dct: <http://purl.org/dc/terms/> .\n@prefix ci:  <https://vocab.methodandstructure.com/content-inventory#> .\n\n<> dct:references <#rate>, <#unit> .\n\n<#rate> a ci:Variable ; rdf:value \"100.00\"^^xsd:decimal .\n<#unit> a ci:Variable ; rdf:value \"USD\"^^xsd:token .\n            \n          "},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "Variable",
      type: "http://www.w3.org/2002/07/owl#Class"

    # Property definitions
    property :action,
      comment: {en: "Relates a document to an action to take."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "action",
      range: "https://vocab.methodandstructure.com/content-inventory#Action",
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#factor_of",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :alias,
      comment: {en: "Denotes an alternate URI for the subject resource. It extends owl:sameAs insofar as asserting that the object is somehow less canonical than the subject."},
      inverseOf: "https://vocab.methodandstructure.com/content-inventory#alias-for",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "alias",
      subPropertyOf: "http://www.w3.org/2002/07/owl#sameAs",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"alias-for",
      comment: {en: "Denotes that the subject is the alias URI, and the object is more canonical (though not necessarily the most canonical)."},
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://vocab.methodandstructure.com/content-inventory#canonical",
      inverseOf: "https://vocab.methodandstructure.com/content-inventory#alias",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "alias-for",
      subPropertyOf: "http://www.w3.org/2002/07/owl#sameAs",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :assumes,
      comment: {en: "The document assumes the audience is familiar with this concept, and may not mention it explicitly."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/educationLevel",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "assumes",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "http://purl.org/dc/terms/references",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"aware-of",
      comment: {en: "This property relates an Audience to a SKOS concept that is likely to be in the orbit of the audience's members: they are aware that the concept exists, although they may not necessarily understand it."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Audience",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "aware-of",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :blocks,
      comment: {en: "A block count is conceptually similar to a word or section count, though it counts the total of elements in the document considered to be text blocks, such as paragraphs, tables, lists and figures. It is suited for document types that have no concept of (semantic) sections, such as HTML. The purpose of this measurement is to provide a sort of ratio to the word count, to glean how well-proportioned the document is."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "blocks",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :canonical,
      comment: {en: "Asserts the canonical URI of the subject resource, i.e., the one you always want to publish in content or redirect Web requests to."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "canonical",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#alias-for",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"canonical-slug",
      comment: {en: "This is the canonical slug associated with the resource, and should be populated with the slug which is actually in use."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "canonical-slug",
      range: "http://www.w3.org/2001/XMLSchema#string",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#slug",
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty", "http://www.w3.org/2002/07/owl#FunctionalProperty"]
    property :characters,
      comment: {en: "This indicates the number of characters in a document, with punctuation and the XPath normalize-space function applied. Note this is characters, not bytes."},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://www.w3.org/TR/xpath/#function-normalize-space",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "characters",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :depicts,
      comment: {en: "The document explicitly depicts this concept (or other entity)."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://xmlns.com/foaf/0.1/depicts",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "depicts",
      note: {en: "This term is identical in meaning to foaf:depicts except that the latter constrains its domain to images only, whereas this can relate any kind of document. The range of this property is also left open, to accommodate any kind of resource being depicted."},
      subPropertyOf: "http://purl.org/dc/terms/references",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :"desired-outcome",
      comment: {en: "This property is intended to indicate what the document is supposed to do—what material effect it is supposed to produce. It is intentionally open-ended, and as such can point to something like a skos:Concept, another document, or a literal string of text describing the outcome."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      "http://www.w3.org/2004/02/skos/core#example": {en: "@prefix bibo: <http://purl.org/ontology/bibo/> .\n@prefix dct:  <http://purl.org/dc/terms/> .\n@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n@prefix ci:   <https://vocab.methodandstructure.com/content-inventory#> .\n@prefix eg:   <https://backoffice.example.club/concepts/> .\n\n# we can extend our article metadata the following way:\n\n<https://example.club/17-mindblowing-ways-to-write-listicles> a bibo:Article ;\n  dct:title \"17 Mindblowing Ways to Write Listicles!\"@en ;\n  ci:desired-outcome eg:maximize-clicks .\n\n# and create a corresponding resource to unambiguously identify the goal:\n\neg:maximize-clicks a skos:Concept ;\n  skos:prefLabel \"Maximize Clicks\"@en ;\n  skos:description \"Moar clicks means moar monies.\"@en .\n            "},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "desired-outcome",
      subPropertyOf: "http://purl.org/dc/terms/type",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :document,
      comment: {en: "Document Reference"},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "document",
      range: "http://xmlns.com/foaf/0.1/Document",
      type: "http://purl.org/linked-data/cube#DimensionProperty"
    property :embed,
      comment: {en: "This property specifies an embedded resource, such as an image, which is visible on the document's canvas."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "embed",
      subPropertyOf: "http://purl.org/dc/terms/hasPart",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :embeds,
      comment: {en: "The number of embeds in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "embeds",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :eschews,
      comment: {en: "This property relates an Audience to a SKOS concept that members of the audience are known to actively avoid or regard with contempt. This relation is intended to represent the complement of values."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Audience",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://vocab.methodandstructure.com/content-inventory#values",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "eschews",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :evokes,
      comment: {en: "The document evokes the given concept without mentioning it explicitly."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "evokes",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#assumes",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :exemplar,
      comment: {en: "This property relates an Audience to a specific foaf:Person who is an exemplar of the audience."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Audience",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "exemplar",
      range: "http://xmlns.com/foaf/0.1/Person",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#example",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :form,
      comment: {en: "This property specifies form target, which may or may not be visible to the user."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "form",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#link",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :forms,
      comment: {en: "The number of forms in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "forms",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :"fragment-of",
      comment: {en: "This property asserts that the subject should be treated as a fragment of the document it relates to."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "fragment-of",
      range: "http://xmlns.com/foaf/0.1/Document",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :"high-quartile",
      comment: {en: "Third Quartile"},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Quartile",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "high-quartile",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :images,
      comment: {en: "This indicates the number of images in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "images",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :include,
      comment: {en: "This property specifies a related resource which is not directly visible to the user."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "include",
      subPropertyOf: "http://purl.org/dc/terms/requires",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :indegree,
      comment: {en: "The number of links pointing at the specified resource."},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "indegree",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :indexed,
      comment: {en: "This is a boolean value to indicate whether or not a resource ought to be indexed. It does not necessarily ascribe a policy: an absence of an explicit true value does not necessarily imply the resource ought not be indexed, but the presence of a false value probably should."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "indexed",
      range: "http://www.w3.org/2001/XMLSchema#boolean",
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty", "http://www.w3.org/2002/07/owl#FunctionalProperty"]
    property :introduces,
      comment: {en: "The document defines, describes, or otherwise introduces the audience to this concept."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "introduces",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#mentions",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :link,
      comment: {en: "This property specifies an ordinary hyperlink, which is visible on the document's canvas."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "link",
      subPropertyOf: "http://purl.org/dc/terms/references",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :lists,
      comment: {en: "The number of lists in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "lists",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :"low-quartile",
      comment: {en: "Equivalent to the bottom quarter, or 25th percentile, of the observed data."},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Quartile",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "low-quartile",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :max,
      comment: {en: "Maximum"},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Sample_maximum",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "max",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :mean,
      comment: {en: "Mean"},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Mean",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "mean",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :median,
      comment: {en: "The median of a population "},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Median",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "median",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :mentions,
      comment: {en: "The document explicitly mentions this concept."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "mentions",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "http://purl.org/dc/terms/references",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :min,
      comment: {en: "The smallest observation in the sample."},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Sample_minimum",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "min",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :"non-audience",
      comment: {en: "This property complements dct:audience insofar as enabling the author or editor to designate a set of entities who are explicitly not the intended audience of the document."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/audience",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "non-audience",
      range: "http://purl.org/dc/terms/AgentClass",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :outdegree,
      comment: {en: "The number of links emanating from the specified resource."},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "outdegree",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :primary,
      comment: {en: "Denotes the primary variant that concretely represents the resource."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "primary",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#variant",
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :representation,
      comment: {en: "Denotes a resource that is a concrete representation of the subject, which assumed to be more abstract."},
      "http://www.w3.org/2002/07/owl#deprecated": "true",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "representation",
      subPropertyOf: "http://purl.org/dc/terms/hasFormat",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :scripts,
      comment: {en: "The number of scripts in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "scripts",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :sd,
      comment: {en: "Standard Deviation"},
      domain: "http://purl.org/linked-data/cube#Observation",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Standard_deviation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "sd",
      range: "http://www.w3.org/2001/XMLSchema#number",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :sections,
      comment: {en: "For document types that have a concrete representation of sections, this property may be used to capture their sum."},
      domain: "http://xmlns.com/foaf/0.1/Document",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "sections",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :slug,
      comment: {en: "The slug is a text token which represents either the full path or terminal path segment of an HTTP(S) URL by which a resource can be located. This property is mainly for the purpose of archiving old or alternative URL paths in a content inventory, for such tasks as generating URL rewriting maps."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "slug",
      range: "http://www.w3.org/2001/XMLSchema#string",
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty"
    property :stylesheets,
      comment: {en: "The number of stylesheets in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "stylesheets",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :tables,
      comment: {en: "The number of tables in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "tables",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :target,
      comment: {en: "Specify the URI of the target resource into which this document should be merged."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Merge",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "target",
      range: "http://xmlns.com/foaf/0.1/Document",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :understands,
      comment: {en: "This property relates an Audience to a SKOS concept that members of the audience are known to comprehend, and thereby do not need any additional explanation."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Audience",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/educationLevel",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "understands",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "https://vocab.methodandstructure.com/content-inventory#aware-of",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :values,
      comment: {en: "This property relates an Audience to a SKOS concept that members of the audience are known to value, that is, to find meaningful in an axiological sense."},
      domain: "https://vocab.methodandstructure.com/content-inventory#Audience",
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://vocab.methodandstructure.com/content-inventory#eschews",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "values",
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :variant,
      comment: {en: "Denotes a resource that is a variant of a concrete representation of the subject, which assumed to be more abstract."},
      equivalentProperty: "https://vocab.methodandstructure.com/content-inventory#representation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "variant",
      subPropertyOf: "http://purl.org/dc/terms/hasFormat",
      type: "http://www.w3.org/2002/07/owl#ObjectProperty"
    property :videos,
      comment: {en: "The number of videos in the document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "videos",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"
    property :words,
      comment: {en: "This indicates the number of words in a document, similar to the familiar function in a word processor. The exact method of counting words may vary by document type, language etc., and is thus out of scope from this document."},
      domain: "http://purl.org/linked-data/cube#Observation",
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "words",
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
      type: "http://purl.org/linked-data/cube#MeasureProperty"

    # Extra definitions
    term :circulated,
      comment: {en: "The document is available for select people to see, but not published in the strict literal sense."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "circulated",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :confidential,
      comment: {en: "The document is confidential and not for publication."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "confidential",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :empty,
      comment: {en: "The document contains no content."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "empty",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :incomplete,
      comment: {en: "The document has been started, but is clearly not finished."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "incomplete",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :incorrect,
      comment: {en: "The content of this document is factually wrong."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "incorrect",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :keep,
      comment: {en: "Keep this document to which this is associated; make no changes to it at this time."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "keep",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :landing,
      comment: {en: "The resource is a landing page from some other medium (e.g. e-mail, television, billboard). This status is a hint to automated systems which would otherwise orphan or retire a landing page with no inbound links."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "landing",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :obsolete,
      comment: {en: "The content of this document was correct and relevant at one point, but external circumstances have caused it to lapse in relevance or factual accuracy."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "obsolete",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :orphan,
      comment: {en: "The resource is not explicitly pending or removed from publication, however it has managed to be disconnected from the rest of the site: There is no path to it from a landing page, and it is not a landing page on its own. That is to say that the resource either has no inbound links, or if it does, those links are from other resources that are in the same situation. Documents which are only linked from retired documents are also considered orphans."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "orphan",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :proofread,
      comment: {en: "Proofread this document."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "proofread",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :retire,
      comment: {en: "Remove all references to this document and consign it to the archive."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "retire",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :retired,
      comment: {en: "The document has been explicitly retired by an editor or curator, but still exists in the archive."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "retired",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :revise,
      comment: {en: "Revise or restructure this document."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "revise",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :rewrite,
      comment: {en: "Rewrite this document from scratch."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "rewrite",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :split,
      comment: {en: "Split this document into multiple pieces."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "split",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :"tentative-merge",
      comment: {en: "Merge this document into some other document, though unspecified at this time as to which."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "tentative-merge",
      type: ["https://vocab.methodandstructure.com/content-inventory#Action", "https://vocab.methodandstructure.com/content-inventory#Merge"]
    term :unavailable,
      comment: {en: "The resource at the subject address is unavailable for reasons other than explicit retirement, e.g. HTTP 404 or 403, or going out of print."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "unavailable",
      type: "http://purl.org/ontology/bibo/DocumentStatus"
    term :"update-metadata",
      comment: {en: "Update the metadata of this document, such as keywords, audience, etc."},
      isDefinedBy: "https://vocab.methodandstructure.com/content-inventory#",
      label: "update-metadata",
      type: "https://vocab.methodandstructure.com/content-inventory#Action"
    term :"words-and-blocks",
      comment: {en: "A set of descriptive statistics pertaining to the number of words per block of text in a given document."},
      "http://purl.org/linked-data/cube#component": [term(
          "http://purl.org/linked-data/cube#dimension": "https://vocab.methodandstructure.com/content-inventory#document",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#blocks",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#characters",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#embeds",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#forms",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#high-quartile",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#images",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#low-quartile",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#max",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#mean",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#median",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#min",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#scripts",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#sd",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#sections",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#stylesheets",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#tables",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#videos",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://vocab.methodandstructure.com/content-inventory#words",
          type: "http://purl.org/linked-data/cube#ComponentSpecification"
        )],
      label: "words-and-blocks",
      type: "http://purl.org/linked-data/cube#DataStructureDefinition"
  end
end
