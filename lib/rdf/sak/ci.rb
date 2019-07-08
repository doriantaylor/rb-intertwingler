# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from https://privatealpha.com/ontology/content-inventory/1#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <https://privatealpha.com/ontology/content-inventory/1#>
  #   class CI < RDF::StrictVocabulary
  #   end
  class CI < RDF::StrictVocabulary("https://privatealpha.com/ontology/content-inventory/1#")

    # Ontology definition
    ontology :"https://privatealpha.com/ontology/content-inventory/1#",
      "bibo:uri": "ci:".freeze,
      comment: %(This vocabulary defines a number of concepts peculiar to content strategy which are not accounted for by other vocabularies.).freeze,
      "dc:created": "2012-01-23T11:52:00-08:00".freeze,
      "dc:creator": "https://doriantaylor.com/person/dorian-taylor#me".freeze,
      "dc:modified": ["2012-12-11T22:22:00-08:00".freeze, "2014-02-06T14:10:00-08:00".freeze, "2015-02-03T14:39:00-08:00".freeze, "2017-04-06T15:24:00-07:00".freeze, "2018-10-06T16:23:52Z".freeze, "2019-03-05T23:38:59Z".freeze, "2019-04-07T16:36:10Z".freeze, "2019-04-18T01:01:09Z".freeze, "2019-07-07T22:10:55Z".freeze],
      "dc:references": ["http://en.wikipedia.org/wiki/Content_strategy".freeze, "http://en.wikipedia.org/wiki/Five-number_summary".freeze, "http://en.wikipedia.org/wiki/Mean".freeze, "http://en.wikipedia.org/wiki/Standard_deviation".freeze, "http://publishing-statistical-data.googlecode.com/svn/trunk/specs/src/main/html/cube.html#ref_qb_DataStructureDefinition".freeze, "http://publishing-statistical-data.googlecode.com/svn/trunk/specs/src/main/html/cube.html#ref_qb_DimensionProperty".freeze, "http://publishing-statistical-data.googlecode.com/svn/trunk/specs/src/main/html/cube.html#ref_qb_MeasureProperty".freeze, "http://publishing-statistical-data.googlecode.com/svn/trunk/specs/src/main/html/cube.html#ref_qb_Observation".freeze, "http://vocab.org/frbr/core".freeze, "http://vocab.org/frbr/extended".freeze, "http://www.w3.org/TR/vocab-data-cube/".freeze, "http://www.w3.org/TR/vocab-data-cube/#ref_qb_DataSet".freeze],
      "dc:subject": "ci:".freeze,
      "dc:title": "A Content Inventory Vocabulary".freeze,
      "foaf:primaryTopic": "ci:".freeze,
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "ci".freeze,
      isDefinedBy: "ci:".freeze,
      "owl:imports": ["bibo:".freeze, "dc:".freeze, "foaf:".freeze, "http://purl.org/NET/c4dm/event.owl#".freeze, "http://purl.org/linked-data/cube#".freeze],
      "owl:versionInfo": "0.8".freeze,
      type: ["bibo:Webpage".freeze, "owl:Ontology".freeze],
      "xhv:license": "http://creativecommons.org/licenses/by/2.5/ca/".freeze

    # Class definitions
    term :Action,
      comment: %(An action, as its name implies, is meant to represent something a person or other agent ought to do to a document.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "Action".freeze,
      subClassOf: "http://purl.org/NET/c4dm/event.owl#Event".freeze,
      type: "owl:Class".freeze
    term :Audience,
      comment: %(An audience represents the set of people who are the intended recipients of the resource. This class is at once an agent class as well as a conceptual entity, capable of being mixed into a SKOS concept scheme.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "Audience".freeze,
      "rdfs:seeAlso": ["dc:audience".freeze, "org:Role".freeze],
      subClassOf: ["dc:AgentClass".freeze, "skos:Concept".freeze],
      type: "owl:Class".freeze
    term :Merge,
      comment: %(In order to merge a document, we must define the target to which it ought to be merged. This class is identical to an Action, save for such a property.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "Merge".freeze,
      subClassOf: "ci:Action".freeze,
      type: "owl:Class".freeze

    # Property definitions
    property :action,
      comment: %(use this to signal an action to take with the document in question.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "action".freeze,
      range: "ci:Action".freeze,
      subPropertyOf: "http://purl.org/NET/c4dm/event.owl#factor_of".freeze,
      type: "owl:ObjectProperty".freeze
    property :alias,
      comment: %(This is an alternate URI for the subject resource. It is simply meant to annotate a resource with another address. It differs from owl:sameAs in that it does not imply that <a> = <b> as well as <b> = <a>.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "alias".freeze,
      type: "owl:ObjectProperty".freeze
    property :assumes,
      comment: %(The document assumes the audience is familiar with this concept, and may not mention it explicitly.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "assumes".freeze,
      range: "skos:Concept".freeze,
      "rdfs:seeAlso": "dc:educationLevel".freeze,
      subPropertyOf: "dc:references".freeze,
      type: "owl:ObjectProperty".freeze
    property :"aware-of",
      comment: %(This property relates an Audience to a SKOS concept that is likely to be in the orbit of the audience's members: they are aware that the concept exists, although they may not necessarily understand it.).freeze,
      domain: "ci:audience".freeze,
      isDefinedBy: "ci:".freeze,
      label: "aware-of".freeze,
      range: "skos:Concept".freeze,
      subPropertyOf: "skos:related".freeze,
      type: "owl:ObjectProperty".freeze
    property :blocks,
      comment: %(A block count is conceptually similar to a word or section count, though it counts the total of elements in the document considered to be text blocks, such as paragraphs, tables, lists and figures. It is suited for document types that have no concept of \(semantic\) sections, such as HTML. The purpose of this measurement is to provide a sort of ratio to the word count, to glean how well-proportioned the document is.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "blocks".freeze,
      range: "xsd:nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :canonical,
      comment: %(This is the canonical URI of the subject resource, i.e., the one you always want to publish in content or redirect Web requests to.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "canonical".freeze,
      subPropertyOf: ["ci:alias".freeze, "owl:sameAs".freeze],
      type: ["owl:FunctionalProperty".freeze, "owl:ObjectProperty".freeze]
    property :"canonical-slug",
      comment: %(This is the canonical slug associated with the resource, and should be populated with the slug which is actually in use.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "canonical-slug".freeze,
      range: "xsd:string".freeze,
      subPropertyOf: "ci:slug".freeze,
      type: ["owl:DatatypeProperty".freeze, "owl:FunctionalProperty".freeze]
    property :characters,
      comment: %(This indicates the number of characters in a document, with punctuation and the XPath normalize-space function applied. Note this is characters, not bytes.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "characters".freeze,
      range: "xsd:nonNegativeInteger".freeze,
      "rdfs:seeAlso": "http://www.w3.org/TR/xpath/#function-normalize-space".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :"desired-outcome",
      comment: %(This property is intended to indicate what the document is supposed to do—what material effect it is supposed to produce. It is intentionally open-ended, and as such can point to something like a skos:Concept, another document, or a literal string of text describing the outcome.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "desired-outcome".freeze,
      subPropertyOf: "dc:type".freeze,
      type: "owl:ObjectProperty".freeze
    property :disdains,
      comment: %(This property relates an Audience to a SKOS concept that members of the audience are known to actively eschew or regard with contempt. This relation is intended to represent the complement of values.).freeze,
      domain: "ci:audience".freeze,
      isDefinedBy: "ci:".freeze,
      label: "disdains".freeze,
      range: "skos:Concept".freeze,
      "rdfs:seeAlso": "ci:values".freeze,
      subPropertyOf: "skos:related".freeze,
      type: "owl:ObjectProperty".freeze
    property :document,
      comment: %(Document Reference).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "document".freeze,
      range: "foaf:Document".freeze,
      type: "http://purl.org/linked-data/cube#DimensionProperty".freeze
    property :embed,
      comment: %(This property specifies an embedded resource which is visible in the subject's user interface.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "embed".freeze,
      subPropertyOf: "dc:hasPart".freeze,
      type: "owl:ObjectProperty".freeze
    property :"high-quartile",
      comment: %(Third Quartile).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "high-quartile".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Quartile".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :indegree,
      comment: %(The number of links pointing at the specified resource.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "indegree".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :indexed,
      comment: %(This is a boolean value to indicate whether or not a resource ought to be indexed. It does not necessarily ascribe a policy: an absence of an explicit true value does not necessarily imply the resource ought not be indexed, but the presence of a false value probably should.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "indexed".freeze,
      range: "xsd:boolean".freeze,
      type: ["owl:DatatypeProperty".freeze, "owl:FunctionalProperty".freeze]
    property :introduces,
      comment: %(The document defines, describes, or otherwise introduces the audience to this concept.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "introduces".freeze,
      range: "skos:Concept".freeze,
      subPropertyOf: "ci:mentions".freeze,
      type: "owl:ObjectProperty".freeze
    property :link,
      comment: %(This property specifies a linked resource which is visible in the subject's user interface.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "link".freeze,
      subPropertyOf: "dc:references".freeze,
      type: "owl:ObjectProperty".freeze
    property :"low-quartile",
      comment: %(Equivalent to the bottom quarter, or 25th percentile, of the observed data.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "low-quartile".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Quartile".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :max,
      comment: %(Maximum).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "max".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Sample_maximum".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :mean,
      comment: %(Mean).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "mean".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Mean".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :median,
      comment: %(The median of a population ).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "median".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Median".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :mentions,
      comment: %(The document explicitly mentions this concept.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "mentions".freeze,
      range: "skos:Concept".freeze,
      subPropertyOf: "dc:references".freeze,
      type: "owl:ObjectProperty".freeze
    property :min,
      comment: %(The smallest observation in the sample.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "min".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Sample_minimum".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :"non-audience",
      comment: %(This property complements dct:audience insofar as enabling the author or editor to designate a set of entities who are explicitly not the intended audience of the document.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "non-audience".freeze,
      range: "dc:AgentClass".freeze,
      "rdfs:seeAlso": "dc:audience".freeze,
      type: "owl:ObjectProperty".freeze
    property :outdegree,
      comment: %(The number of links emanating from the specified resource.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "outdegree".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Directed_graph#Indegree_and_outdegree".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :sd,
      comment: %(Standard Deviation).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "sd".freeze,
      range: "xsd:number".freeze,
      "rdfs:seeAlso": "http://en.wikipedia.org/wiki/Standard_deviation".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :sections,
      comment: %(For document types that have a concrete representation of sections, this property may be used to capture their sum.).freeze,
      domain: "foaf:Document".freeze,
      isDefinedBy: "ci:".freeze,
      label: "sections".freeze,
      range: "xsd:nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze
    property :slug,
      comment: %(The slug is a text token which represents either the full path or terminal path segment of an HTTP\(S\) URL by which a resource can be located. This property is mainly for the purpose of archiving old or alternative URL paths in a content inventory, for such tasks as generating URL rewriting maps.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "slug".freeze,
      range: "xsd:string".freeze,
      type: "owl:DatatypeProperty".freeze
    property :target,
      comment: %(Specify the URI of the target resource into which this document should be merged.).freeze,
      domain: "ci:Merge".freeze,
      isDefinedBy: "ci:".freeze,
      label: "target".freeze,
      range: "foaf:Document".freeze,
      type: "owl:ObjectProperty".freeze
    property :understands,
      comment: %(This property relates an Audience to a SKOS concept that members of the audience are known to comprehend, and thereby do not need any additional explanation.).freeze,
      domain: "ci:audience".freeze,
      isDefinedBy: "ci:".freeze,
      label: "understands".freeze,
      range: "skos:Concept".freeze,
      "rdfs:seeAlso": "dc:educationLevel".freeze,
      subPropertyOf: "ci:aware-of".freeze,
      type: "owl:ObjectProperty".freeze
    property :values,
      comment: %(This property relates an Audience to a SKOS concept that members of the audience are known to value, that is, to find meaningful in an axiological sense.).freeze,
      domain: "ci:audience".freeze,
      isDefinedBy: "ci:".freeze,
      label: "values".freeze,
      range: "skos:Concept".freeze,
      "rdfs:seeAlso": "ci:disdains".freeze,
      subPropertyOf: "skos:related".freeze,
      type: "owl:ObjectProperty".freeze
    property :words,
      comment: %(This indicates the number of words in a document, similar to the familiar function in a word processor. The exact method of counting words may vary by document type, language etc., and is thus out of scope from this document.).freeze,
      domain: "http://purl.org/linked-data/cube#Observation".freeze,
      isDefinedBy: "ci:".freeze,
      label: "words".freeze,
      range: "xsd:nonNegativeInteger".freeze,
      type: "http://purl.org/linked-data/cube#MeasureProperty".freeze

    # Extra definitions
    term :circulated,
      comment: %(The document is available for select people to see, but not published in the strict literal sense.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "circulated".freeze,
      type: "bibo:DocumentStatus".freeze
    term :confidential,
      comment: %(The document is confidential and not for publication.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "confidential".freeze,
      type: "bibo:DocumentStatus".freeze
    term :empty,
      comment: %(The document contains no content.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "empty".freeze,
      type: "bibo:DocumentStatus".freeze
    term :incomplete,
      comment: %(The document has been started, but is clearly not finished.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "incomplete".freeze,
      type: "bibo:DocumentStatus".freeze
    term :incorrect,
      comment: %(The content of this document is factually wrong.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "incorrect".freeze,
      type: "bibo:DocumentStatus".freeze
    term :keep,
      comment: %(Keep this document to which this is associated; make no changes to it at this time.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "keep".freeze,
      type: "ci:Action".freeze
    term :landing,
      comment: %(The resource is a landing page from some other medium \(e.g. e-mail, television, billboard\). This status is a hint to automated systems which would otherwise orphan or retire a landing page with no inbound links.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "landing".freeze,
      type: "bibo:DocumentStatus".freeze
    term :obsolete,
      comment: %(The content of this document was correct and relevant at one point, but external circumstances have caused it to lapse in relevance or factual accuracy.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "obsolete".freeze,
      type: "bibo:DocumentStatus".freeze
    term :orphan,
      comment: %(The resource is not explicitly pending or removed from publication, however it has managed to be disconnected from the rest of the site: There is no path to it from a landing page, and it is not a landing page on its own. That is to say that the resource either has no inbound links, or if it does, those links are from other resources that are in the same situation. Documents which are only linked from retired documents are also considered orphans.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "orphan".freeze,
      type: "bibo:DocumentStatus".freeze
    term :proofread,
      comment: %(Proofread this document.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "proofread".freeze,
      type: "ci:Action".freeze
    term :retire,
      comment: %(Remove all references to this document and consign it to the archive.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "retire".freeze,
      type: "ci:Action".freeze
    term :retired,
      comment: %(The document has been explicitly retired by an editor or curator, but still exists in the archive.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "retired".freeze,
      type: "bibo:DocumentStatus".freeze
    term :revise,
      comment: %(Revise or restructure this document.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "revise".freeze,
      type: "ci:Action".freeze
    term :rewrite,
      comment: %(Rewrite this document from scratch.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "rewrite".freeze,
      type: "ci:Action".freeze
    term :split,
      comment: %(Split this document into multiple pieces.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "split".freeze,
      type: "ci:Action".freeze
    term :"tentative-merge",
      comment: %(Merge this document into some other document, though unspecified at this time as to which.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "tentative-merge".freeze,
      type: "ci:Merge".freeze
    term :unavailable,
      comment: %(The resource at the subject address is unavailable for reasons other than explicit retirement, e.g. HTTP 404 or 403, or going out of print.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "unavailable".freeze,
      type: "bibo:DocumentStatus".freeze
    term :"update-metadata",
      comment: %(Update the metadata of this document, such as keywords, audience, etc.).freeze,
      isDefinedBy: "ci:".freeze,
      label: "update-metadata".freeze,
      type: "ci:Action".freeze
    term :"words-and-blocks",
      comment: %(A set of descriptive statistics pertaining to the number of words per block of text in a given document.).freeze,
      "http://purl.org/linked-data/cube#component": [term(
          "http://purl.org/linked-data/cube#dimension": "ci:document".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:blocks".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:characters".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:high-quartile".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:low-quartile".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:max".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:mean".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:median".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:min".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:sd".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:sections".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        ), term(
          "http://purl.org/linked-data/cube#measure": "ci:words".freeze,
          type: "http://purl.org/linked-data/cube#ComponentSpecification".freeze
        )],
      label: "words-and-blocks".freeze,
      type: "http://purl.org/linked-data/cube#DataStructureDefinition".freeze
  end
end
