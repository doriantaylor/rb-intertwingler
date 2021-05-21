# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://privatealpha.com/ontology/content-inventory/1#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://privatealpha.com/ontology/content-inventory/1#>
  #   #
  #   # A Content Inventory Vocabulary
  #   #
  #   # This vocabulary defines a number of concepts peculiar to content strategy which are not accounted for by other vocabularies.
  #   # @version 0.14
  #   class CI < RDF::StrictVocabulary
  #     # An action, as its name implies, is meant to represent something a person or other agent ought to do to a document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Action
  #
  #     # In general there is no programmatic way to tell whether a resource is an advertisement, since advertisements on the Web look (to a machine) like any other resource. This is intended to be a decorator class to indicate that the subject is an advertisement. It can therefore be combined with other classes such as foaf:Image, or bibo:AudioVisualDocument.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Advertisement
  #
  #     # An audience represents the set of people who are the intended recipients of the resource. This class is at once an agent class as well as a conceptual entity, capable of being mixed into a SKOS concept scheme.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Audience
  #
  #     # In order to merge a document, we must define the target to which it ought to be merged. This class is identical to an Action, save for such a property.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Merge
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
  CI = Class.new(RDF::StrictVocabulary("https://privatealpha.com/ontology/content-inventory/1#")) do

    # Ontology definition
    ontology :"https://privatealpha.com/ontology/content-inventory/1#",
      comment: "This vocabulary defines a number of concepts peculiar to content strategy which are not accounted for by other vocabularies.".freeze,
      "http://purl.org/dc/terms/created": "2012-01-23T11:52:00-08:00".freeze,
      "http://purl.org/dc/terms/creator": "https://doriantaylor.com/person/dorian-taylor#me".freeze,
      "http://purl.org/dc/terms/modified": ["2012-12-11T22:22:00-08:00".freeze, "2014-02-06T14:10:00-08:00".freeze, "2015-02-03T14:39:00-08:00".freeze, "2017-04-06T15:24:00-07:00".freeze, "2018-10-06T16:23:52Z".freeze, "2019-03-05T23:38:59Z".freeze, "2019-04-07T16:36:10Z".freeze, "2019-04-18T01:01:09Z".freeze, "2019-07-07T22:10:55Z".freeze, "2019-07-10T22:28:06Z".freeze, "2019-07-21T23:05:32Z".freeze, "2019-09-04T20:27:32Z".freeze, "2020-01-26T05:02:30Z".freeze, "2020-04-24T23:16:20Z".freeze, "2020-04-30T01:05:51Z".freeze, "2020-06-29T02:24:58Z".freeze, "2020-07-04T01:24:22Z".freeze, "2020-11-13T03:27:35Z".freeze, "2021-05-17T17:57:27Z".freeze],
      "http://purl.org/dc/terms/references": ["http://en.wikipedia.org/wiki/Content_strategy".freeze, "http://en.wikipedia.org/wiki/Five-number_summary".freeze, "http://en.wikipedia.org/wiki/Mean".freeze, "http://en.wikipedia.org/wiki/Standard_deviation".freeze, "http://vocab.org/frbr/core".freeze, "http://vocab.org/frbr/extended".freeze, "http://www.w3.org/TR/vocab-data-cube/".freeze, "http://www.w3.org/TR/vocab-data-cube/#ref_qb_DataSet".freeze, "https://www.w3.org/TR/prov-o/".freeze, "https://www.w3.org/TR/vocab-data-cube/#ref_qb_DataStructureDefinition".freeze, "https://www.w3.org/TR/vocab-data-cube/#ref_qb_DimensionProperty".freeze, "https://www.w3.org/TR/vocab-data-cube/#ref_qb_MeasureProperty".freeze, "https://www.w3.org/TR/vocab-data-cube/#ref_qb_Observation".freeze],
      "http://purl.org/dc/terms/subject": "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      "http://purl.org/dc/terms/title": "A Content Inventory Vocabulary".freeze,
      "http://purl.org/ontology/bibo/uri": "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "ci".freeze,
      "http://www.w3.org/1999/xhtml/vocab#license": "http://creativecommons.org/licenses/by/2.5/ca/".freeze,
      "http://www.w3.org/2002/07/owl#imports": ["http://purl.org/NET/c4dm/event.owl#".freeze, "http://purl.org/dc/terms/".freeze, "http://purl.org/linked-data/cube#".freeze, "http://purl.org/ontology/bibo/".freeze, "http://www.w3.org/2001/XMLSchema#".freeze, "http://www.w3.org/2002/07/owl#".freeze, "http://www.w3.org/2004/02/skos/core#".freeze, "http://xmlns.com/foaf/0.1/".freeze],
      "http://www.w3.org/2002/07/owl#versionInfo": "0.14".freeze,
      "http://xmlns.com/foaf/0.1/primaryTopic": "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      type: ["http://purl.org/ontology/bibo/Webpage".freeze, "http://www.w3.org/2002/07/owl#Ontology".freeze]

    # Class definitions
    term :Action,
      comment: "An action, as its name implies, is meant to represent something a person or other agent ought to do to a document.".freeze,
      "http://www.w3.org/2004/02/skos/core#usageNote": "Being a subclass of an event, a ci:Action can have agents, factors, products, places and times ascribed to it.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "Action".freeze,
      subClassOf: "http://purl.org/NET/c4dm/event.owl#Event".freeze,
      type: "http://www.w3.org/2002/07/owl#Class".freeze
    term :Advertisement,
      comment: "In general there is no programmatic way to tell whether a resource is an advertisement, since advertisements on the Web look (to a machine) like any other resource. This is intended to be a decorator class to indicate that the subject is an advertisement. It can therefore be combined with other classes such as foaf:Image, or bibo:AudioVisualDocument.".freeze,
      "http://www.w3.org/2004/02/skos/core#example": "@prefix bibo: <http://purl.org/ontology/bibo/> .\n@prefix dct:  <http://purl.org/dc/terms/> .\n@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n@prefix ci:   <https://privatealpha.com/ontology/content-inventory/1#> .\n\n<https://example.club/17-mindblowing-ways-to-write-listicles> a bibo:Article ;\n  dct:title \"17 Mindblowing Ways to Write Listicles!\"@en ;\n  dct:hasPart <https://adtech.biz/assets/punch-the-monkey> .\n\n<https://adtech.biz/assets/punch-the-monkey> a foaf:Image, ci:Advertisement ;\n  dct:title \"Punch The Monkey And WIN!\#@$!%%^!\"@en .".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "Advertisement".freeze,
      subClassOf: "http://xmlns.com/foaf/0.1/Document".freeze,
      type: "http://www.w3.org/2002/07/owl#Class".freeze
    term :Audience,
      comment: "An audience represents the set of people who are the intended recipients of the resource. This class is at once an agent class as well as a conceptual entity, capable of being mixed into a SKOS concept scheme.".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": ["http://purl.org/dc/terms/audience".freeze, "http://www.w3.org/ns/org#Role".freeze],
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "Audience".freeze,
      subClassOf: ["http://purl.org/dc/terms/AgentClass".freeze, "http://www.w3.org/2004/02/skos/core#Concept".freeze],
      type: "http://www.w3.org/2002/07/owl#Class".freeze
    term :Merge,
      comment: "In order to merge a document, we must define the target to which it ought to be merged. This class is identical to an Action, save for such a property.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "Merge".freeze,
      subClassOf: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze,
      type: "http://www.w3.org/2002/07/owl#Class".freeze
    term :Variable,
      comment: "Identifies a variable which can be embedded into a document and assigned an rdf:value.".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://www.w3.org/1999/02/22-rdf-syntax-ns#value".freeze,
      "http://www.w3.org/2004/02/skos/core#example": "\n            Consider the markup:\n            <p about=\"https://example.biz/ratecard#\">Shingy's bill-out rate is <span rel=\"dct:references\">\n  <var about=\"#rate\" typeof=\"ci:Variable\" property=\"rdf:value\" datatype=\"xsd:decimal\">100.00</var>\n  <var about=\"#unit\" typeof=\"ci:Variable\" property=\"rdf:value\" datatype=\"xsd:token\">USD</var>\n</span> per hour.</p>\n            …which when parsed will produce the statements:\n            @base <https://example.biz/ratecard#> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n@prefix dct: <http://purl.org/dc/terms/> .\n@prefix ci:  <https://privatealpha.com/ontology/content-inventory/1#> .\n\n<> dct:references <#rate>, <#unit> .\n\n<#rate> a ci:Variable ; rdf:value \"100.00\"^^xsd:decimal .\n<#unit> a ci:Variable ; rdf:value \"USD\"^^xsd:token .\n            \n          ".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "Variable".freeze,
      type: "http://www.w3.org/2002/07/owl#Class".freeze

    # Property definitions
    property :action,
      comment: "Relates a document to an action to take.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "action".freeze,
      range: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze,
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#factor_of".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :alias,
      comment: "Denotes an alternate URI for the subject resource. It extends owl:sameAs insofar as asserting that the object is somehow less canonical than the subject.".freeze,
      inverseOf: "https://privatealpha.com/ontology/content-inventory/1#alias-for".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "alias".freeze,
      subPropertyOf: "http://www.w3.org/2002/07/owl#sameAs".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :"alias-for",
      comment: "Denotes that the subject is the alias URI, and the object is more canonical (though not necessarily the most canonical).".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://privatealpha.com/ontology/content-inventory/1#canonical".freeze,
      inverseOf: "https://privatealpha.com/ontology/content-inventory/1#alias".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "alias-for".freeze,
      subPropertyOf: "http://www.w3.org/2002/07/owl#sameAs".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :assumes,
      comment: "The document assumes the audience is familiar with this concept, and may not mention it explicitly.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/educationLevel".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "assumes".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "http://purl.org/dc/terms/references".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :"aware-of",
      comment: "This property relates an Audience to a SKOS concept that is likely to be in the orbit of the audience's members: they are aware that the concept exists, although they may not necessarily understand it.".freeze,
      domain: "https://privatealpha.com/ontology/content-inventory/1#Audience".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "aware-of".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :blocks,
      comment: "A block count is conceptually similar to a word or section count, though it counts the total of elements in the document considered to be text blocks, such as paragraphs, tables, lists and figures. It is suited for document types that have no concept of (semantic) sections, such as HTML. The purpose of this measurement is to provide a sort of ratio to the word count, to glean how well-proportioned the document is.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "blocks".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :canonical,
      comment: "Asserts the canonical URI of the subject resource, i.e., the one you always want to publish in content or redirect Web requests to.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "canonical".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#alias-for".freeze,
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty".freeze, "http://www.w3.org/2002/07/owl#ObjectProperty".freeze]
    property :"canonical-slug",
      comment: "This is the canonical slug associated with the resource, and should be populated with the slug which is actually in use.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "canonical-slug".freeze,
      range: "http://www.w3.org/2001/XMLSchema#string".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#slug".freeze,
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty".freeze, "http://www.w3.org/2002/07/owl#FunctionalProperty".freeze]
    property :characters,
      comment: "This indicates the number of characters in a document, with punctuation and the XPath normalize-space function applied. Note this is characters, not bytes.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://www.w3.org/TR/xpath/#function-normalize-space".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "characters".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :depicts,
      comment: "The document explicitly depicts this concept (or other entity).".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://xmlns.com/foaf/0.1/depicts".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "depicts".freeze,
      note: "This term is identical in meaning to foaf:depicts except that the latter constrains its domain to images only, whereas this can relate any kind of document. The range of this property is also left open, to accommodate any kind of resource being depicted.".freeze,
      subPropertyOf: "http://purl.org/dc/terms/references".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :"desired-outcome",
      comment: "This property is intended to indicate what the document is supposed to do—what material effect it is supposed to produce. It is intentionally open-ended, and as such can point to something like a skos:Concept, another document, or a literal string of text describing the outcome.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      "http://www.w3.org/2004/02/skos/core#example": "@prefix bibo: <http://purl.org/ontology/bibo/> .\n@prefix dct:  <http://purl.org/dc/terms/> .\n@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n@prefix ci:   <https://privatealpha.com/ontology/content-inventory/1#> .\n@prefix eg:   <https://backoffice.example.club/concepts/> .\n\n# we can extend our article metadata the following way:\n\n<https://example.club/17-mindblowing-ways-to-write-listicles> a bibo:Article ;\n  dct:title \"17 Mindblowing Ways to Write Listicles!\"@en ;\n  ci:desired-outcome eg:maximize-clicks .\n\n# and create a corresponding resource to unambiguously identify the goal:\n\neg:maximize-clicks a skos:Concept ;\n  skos:prefLabel \"Maximize Clicks\"@en ;\n  skos:description \"Moar clicks means moar monies.\"@en .\n            ".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "desired-outcome".freeze,
      subPropertyOf: "http://purl.org/dc/terms/type".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :document,
      comment: "Document Reference".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "document".freeze,
      range: "http://xmlns.com/foaf/0.1/Document".freeze,
      type: "http://purl.org/linked-data/cube#DimensionProperty".freeze
    property :embed,
      comment: "This property specifies an embedded resource, such as an image, which is visible on the document's canvas.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "embed".freeze,
      subPropertyOf: "http://purl.org/dc/terms/hasPart".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :embeds,
      comment: "The number of embeds in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "embeds".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :eschews,
      comment: "This property relates an Audience to a SKOS concept that members of the audience are known to actively avoid or regard with contempt. This relation is intended to represent the complement of values.".freeze,
      domain: "https://privatealpha.com/ontology/content-inventory/1#Audience".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://privatealpha.com/ontology/content-inventory/1#values".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "eschews".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :evokes,
      comment: "The document evokes the given concept without mentioning it explicitly.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "evokes".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#assumes".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :form,
      comment: "This property specifies form target, which may or may not be visible to the user.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "form".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#link".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :forms,
      comment: "The number of forms in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "forms".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :"fragment-of",
      comment: "This property asserts that the subject should be treated as a fragment of the document it relates to.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "fragment-of".freeze,
      range: "http://xmlns.com/foaf/0.1/Document".freeze,
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty".freeze, "http://www.w3.org/2002/07/owl#ObjectProperty".freeze]
    property :"high-quartile",
      comment: "Third Quartile".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Quartile".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "high-quartile".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :images,
      comment: "This indicates the number of images in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "images".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :include,
      comment: "This property specifies a related resource which is not directly visible to the user.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "include".freeze,
      subPropertyOf: "http://purl.org/dc/terms/requires".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :indegree,
      comment: "The number of links pointing at the specified resource.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "indegree".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :indexed,
      comment: "This is a boolean value to indicate whether or not a resource ought to be indexed. It does not necessarily ascribe a policy: an absence of an explicit true value does not necessarily imply the resource ought not be indexed, but the presence of a false value probably should.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "indexed".freeze,
      range: "http://www.w3.org/2001/XMLSchema#boolean".freeze,
      type: ["http://www.w3.org/2002/07/owl#DatatypeProperty".freeze, "http://www.w3.org/2002/07/owl#FunctionalProperty".freeze]
    property :introduces,
      comment: "The document defines, describes, or otherwise introduces the audience to this concept.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "introduces".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#mentions".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :link,
      comment: "This property specifies an ordinary hyperlink, which is visible on the document's canvas.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "link".freeze,
      subPropertyOf: "http://purl.org/dc/terms/references".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :lists,
      comment: "The number of lists in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "lists".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :"low-quartile",
      comment: "Equivalent to the bottom quarter, or 25th percentile, of the observed data.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Quartile".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "low-quartile".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :max,
      comment: "Maximum".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Sample_maximum".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "max".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :mean,
      comment: "Mean".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Mean".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "mean".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :median,
      comment: "The median of a population ".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Median".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "median".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :mentions,
      comment: "The document explicitly mentions this concept.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "mentions".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "http://purl.org/dc/terms/references".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :min,
      comment: "The smallest observation in the sample.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Sample_minimum".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "min".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :"non-audience",
      comment: "This property complements dct:audience insofar as enabling the author or editor to designate a set of entities who are explicitly not the intended audience of the document.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/audience".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "non-audience".freeze,
      range: "http://purl.org/dc/terms/AgentClass".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :outdegree,
      comment: "The number of links emanating from the specified resource.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "outdegree".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :primary,
      comment: "Denotes the primary variant that concretely represents the resource.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "primary".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#variant".freeze,
      type: ["http://www.w3.org/2002/07/owl#FunctionalProperty".freeze, "http://www.w3.org/2002/07/owl#ObjectProperty".freeze]
    property :representation,
      comment: "Denotes a resource that is a concrete representation of the subject, which assumed to be more abstract.".freeze,
      "http://www.w3.org/2002/07/owl#deprecated": "true".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "representation".freeze,
      subPropertyOf: "http://purl.org/dc/terms/hasFormat".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :scripts,
      comment: "The number of scripts in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "scripts".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :sd,
      comment: "Standard Deviation".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://en.wikipedia.org/wiki/Standard_deviation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "sd".freeze,
      range: "http://www.w3.org/2001/XMLSchema#number".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :sections,
      comment: "For document types that have a concrete representation of sections, this property may be used to capture their sum.".freeze,
      domain: "http://xmlns.com/foaf/0.1/Document".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "sections".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :slug,
      comment: "The slug is a text token which represents either the full path or terminal path segment of an HTTP(S) URL by which a resource can be located. This property is mainly for the purpose of archiving old or alternative URL paths in a content inventory, for such tasks as generating URL rewriting maps.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "slug".freeze,
      range: "http://www.w3.org/2001/XMLSchema#string".freeze,
      type: "http://www.w3.org/2002/07/owl#DatatypeProperty".freeze
    property :stylesheets,
      comment: "The number of stylesheets in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "stylesheets".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :tables,
      comment: "The number of tables in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "tables".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :target,
      comment: "Specify the URI of the target resource into which this document should be merged.".freeze,
      domain: "https://privatealpha.com/ontology/content-inventory/1#Merge".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "target".freeze,
      range: "http://xmlns.com/foaf/0.1/Document".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :understands,
      comment: "This property relates an Audience to a SKOS concept that members of the audience are known to comprehend, and thereby do not need any additional explanation.".freeze,
      domain: "https://privatealpha.com/ontology/content-inventory/1#Audience".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://purl.org/dc/terms/educationLevel".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "understands".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "https://privatealpha.com/ontology/content-inventory/1#aware-of".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :values,
      comment: "This property relates an Audience to a SKOS concept that members of the audience are known to value, that is, to find meaningful in an axiological sense.".freeze,
      domain: "https://privatealpha.com/ontology/content-inventory/1#Audience".freeze,
      "http://www.w3.org/2000/01/rdf-schema#seeAlso": "https://privatealpha.com/ontology/content-inventory/1#eschews".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "values".freeze,
      range: "http://www.w3.org/2004/02/skos/core#Concept".freeze,
      subPropertyOf: "http://www.w3.org/2004/02/skos/core#related".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :variant,
      comment: "Denotes a resource that is a variant of a concrete representation of the subject, which assumed to be more abstract.".freeze,
      equivalentProperty: "https://privatealpha.com/ontology/content-inventory/1#representation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "variant".freeze,
      subPropertyOf: "http://purl.org/dc/terms/hasFormat".freeze,
      type: "http://www.w3.org/2002/07/owl#ObjectProperty".freeze
    property :videos,
      comment: "The number of videos in the document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "videos".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :words,
      comment: "This indicates the number of words in a document, similar to the familiar function in a word processor. The exact method of counting words may vary by document type, language etc., and is thus out of scope from this document.".freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "words".freeze,
      range: "http://www.w3.org/2001/XMLSchema#nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze

    # Extra definitions
    term :circulated,
      comment: "The document is available for select people to see, but not published in the strict literal sense.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "circulated".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :confidential,
      comment: "The document is confidential and not for publication.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "confidential".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :empty,
      comment: "The document contains no content.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "empty".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :incomplete,
      comment: "The document has been started, but is clearly not finished.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "incomplete".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :incorrect,
      comment: "The content of this document is factually wrong.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "incorrect".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :keep,
      comment: "Keep this document to which this is associated; make no changes to it at this time.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "keep".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :landing,
      comment: "The resource is a landing page from some other medium (e.g. e-mail, television, billboard). This status is a hint to automated systems which would otherwise orphan or retire a landing page with no inbound links.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "landing".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :obsolete,
      comment: "The content of this document was correct and relevant at one point, but external circumstances have caused it to lapse in relevance or factual accuracy.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "obsolete".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :orphan,
      comment: "The resource is not explicitly pending or removed from publication, however it has managed to be disconnected from the rest of the site: There is no path to it from a landing page, and it is not a landing page on its own. That is to say that the resource either has no inbound links, or if it does, those links are from other resources that are in the same situation. Documents which are only linked from retired documents are also considered orphans.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "orphan".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :proofread,
      comment: "Proofread this document.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "proofread".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :retire,
      comment: "Remove all references to this document and consign it to the archive.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "retire".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :retired,
      comment: "The document has been explicitly retired by an editor or curator, but still exists in the archive.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "retired".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :revise,
      comment: "Revise or restructure this document.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "revise".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :rewrite,
      comment: "Rewrite this document from scratch.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "rewrite".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :split,
      comment: "Split this document into multiple pieces.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "split".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :"tentative-merge",
      comment: "Merge this document into some other document, though unspecified at this time as to which.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "tentative-merge".freeze,
      type: ["https://privatealpha.com/ontology/content-inventory/1#Action".freeze, "https://privatealpha.com/ontology/content-inventory/1#Merge".freeze]
    term :unavailable,
      comment: "The resource at the subject address is unavailable for reasons other than explicit retirement, e.g. HTTP 404 or 403, or going out of print.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "unavailable".freeze,
      type: "http://purl.org/ontology/bibo/DocumentStatus".freeze
    term :"update-metadata",
      comment: "Update the metadata of this document, such as keywords, audience, etc.".freeze,
      isDefinedBy: "https://privatealpha.com/ontology/content-inventory/1#".freeze,
      label: "update-metadata".freeze,
      type: "https://privatealpha.com/ontology/content-inventory/1#Action".freeze
    term :"words-and-blocks",
      comment: "A set of descriptive statistics pertaining to the number of words per block of text in a given document.".freeze,
      "http://purl.org/linked-data/cube#component": [term(
          "http://purl.org/linked-data/cube#dimension": "https://privatealpha.com/ontology/content-inventory/1#document".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#blocks".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#characters".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#embeds".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#forms".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#high-quartile".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#images".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#low-quartile".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#max".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#mean".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#median".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#min".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#scripts".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#sd".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#sections".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#stylesheets".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#tables".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#videos".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "https://privatealpha.com/ontology/content-inventory/1#words".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        )],
      label: "words-and-blocks".freeze,
      type: "http://purl.org/linked-data/cube#DataStructureDefinition".freeze

    RDF::Vocabulary.register :ci, self if
      RDF::Vocabulary.respond_to? :register
  end
end
