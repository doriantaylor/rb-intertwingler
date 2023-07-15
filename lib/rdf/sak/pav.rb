# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from http://purl.org/pav/
require 'rdf'
module Intertwingler
  # @!parse
  #   # Vocabulary for <http://purl.org/pav/>
  #   class PAV < RDF::StrictVocabulary
  #   end
  class PAV < RDF::StrictVocabulary("http://purl.org/pav/")

    # Ontology definition
    ontology :"http://purl.org/pav/",
      comment: %(PAV is a lightweight ontology for tracking Provenance, Authoring and Versioning. PAV specializes the W3C provenance ontology PROV-O in order to describe authorship, curation and digital creation of online resources.

          This ontology describes the defined PAV properties and their usage. Note that PAV does not define any explicit classes or domain/ranges, as every property is meant to be used directly on the described online resource.

          Cite as: Paolo Ciccarese, Stian Soiland-Reyes, Khalid Belhajjame, Alasdair JG Gray, Carole Goble, Tim Clark \(2013\): PAV ontology: provenance, authoring and versioning. Journal of biomedical semantics 4 \(1\), 37. doi:10.1186/2041-1480-4-37
        ).freeze,
      "dc11:contributor": ["Alasdair J G Gray".freeze, "Khalid Belhajjame".freeze, "Marco Ocana".freeze, "Simon Jupp".freeze],
      "dc11:creator": ["Paolo Ciccarese".freeze, "Stian Soiland-Reyes".freeze],
      "dc11:description": ["PAV supplies terms for distinguishing between the different roles of the agents contributing content in current web based systems: contributors, authors, curators and digital artifact creators. The ontology also provides terms for tracking provenance of digital entities that are published on the web and then accessed, transformed and consumed. In order to support broader interoperability, PAV specializes the general purpose W3C PROV provenance model (PROV-O).\n\nPAV distinguishes between the data related to the digital artifact - named Provenance - and those related to the actual knowledge creation and therefore to the intellectual property aspects – named Authoring. The Versioning axis describes the evolution of digital entities in time.\n\nUsing PAV, descriptions can define the authors that originate or gave existence to the work that is expressed in the digital resource (pav:authoredBy); curators (pav:curatedBy) who are content specialists responsible for shaping the expression in an appropriate format, and contributors (super-property pav:contributedBy) that provided some help in conceiving the resource or in the expressed knowledge creation/extraction.\n\nThese provenance aspects can be detailed with dates using pav:curatedOn, pav:authoredOn, etc. Further details about the creation activities, such as different authors contributing specific parts of the resource at different dates are out of scope for PAV and should be defined using vocabularies like PROV-O and additional intermediate entities to describe the different states.\n\nFor resources based on other resources, PAV allows specification of direct retrieval (pav:retrievedFrom), import through transformations (pav:importedFrom) and sources that were merely consulted (pav:sourceAccessedAt). These aspects can also define the agents responsible using pav:retrievedBy, pav:importedBy and pav:sourceAccessedBy.\n\nVersion number of a resource can be given with pav:version, the previous version of the resource with pav:previousVersion, and any other earlier versions with pav:hasEarlierVersion. Unversioned, 'mutable' resources can specify their current version as a snapshot resource using pav:hasCurrentVersion and list the earlier versions using pav:hasVersion.\n\nThe creation of the digital representation (e.g. an RDF graph or a .docx file) can in many cases be different from the authorship of the content/knowledge, and in PAV this digital creation is specified using pav:createdBy, pav:createdWith and pav:createdOn.\n\nPAV specializes terms from W3C PROV-O (prov:) and DC Terms (dcterms:), however these ontologies are not OWL imported as PAV can be used independently. The \"is defined by\" links indicate where those terms are included from. See http://www.w3.org/TR/prov-o and http://dublincore.org/documents/2012/06/14/dcmi-terms/ for more details. See http://purl.org/pav/mapping/dcterms For a comprehensive SKOS mapping to DC Terms.\n\nPAV 2 is based on PAV 1.2 but in a different namespace ( http://purl.org/pav/ ). Terms compatible with 1.2 are indicated in this ontology using owl:equivalentProperty.\n\nThe ontology IRI http://purl.org/pav/ always resolve to the latest version of PAV 2. Particular versionIRIs such as http://purl.org/pav/2.1 can be used by clients to force imports of a particular version - note however that all terms are defined directly in the http://purl.org/pav/ namespace.\n\nThe goal of PAV is to provide a lightweight, straight forward way to give the essential information about authorship, provenance and versioning, and therefore these properties are described directly on the published resource. As such, PAV does not define any classes or restrict domain/ranges, as all properties are applicable to any online resource.\n\n--\n\nCopyright 2008-2014 Massachusetts General Hospital; Harvard Medical School; Balboa Systems; University of Manchester\n\nLicensed under the Apache License, Version 2.0 (the \"License\"); you may not use this file except in compliance with the License.  You may obtain a copy of the License at\n\n    http://www.apache.org/licenses/LICENSE-2.0\n\nUnless required by applicable law or agreed to in writing, software distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.\n".freeze, "http://pav-ontology.googlecode.com/svn/branches/2.3/images/pav-overview.svg".freeze],
      "dc:contributor": ["http://orcid.org/0000-0001-6938-0820".freeze, "http://orcid.org/0000-0002-0643-3144".freeze, "http://orcid.org/0000-0002-5711-4872".freeze, "http://www.paolociccarese.info/foaf.rdf#marco-ocana".freeze],
      "dc:creator": ["http://orcid.org/0000-0001-9842-9718".freeze, "http://orcid.org/0000-0002-5156-2703".freeze],
      "dc:format": "application/rdf+xml".freeze,
      "dc:issued": "2014-08-28T15:00:00Z".freeze,
      "dc:language": "en".freeze,
      "dc:license": "http://www.apache.org/licenses/LICENSE-2.0".freeze,
      "dc:modified": "2014-08-28T14:41:00Z".freeze,
      "dc:publisher": "http://www.mindinformatics.org/".freeze,
      "dc:title": "PAV - Provenance, Authoring and Versioning".freeze,
      "foaf:homepage": "pav:home".freeze,
      "foaf:isPrimaryTopicOf": ["http://arxiv.org/abs/1304.7224".freeze, "http://dx.doi.org/10.1186/2041-1480-4-37".freeze],
      label: "Provenance, Authoring and Versioning (PAV)".freeze,
      "owl:backwardCompatibleWith": ["http://purl.org/pav/2.0/".freeze, "http://purl.org/pav/authoring/2.0/".freeze, "http://purl.org/pav/provenance/2.0/".freeze, "http://purl.org/pav/versioning/2.0/".freeze, "pav:2.1".freeze, "pav:2.2".freeze],
      "owl:incompatibleWith": "http://swan.mindinformatics.org/ontologies/1.2/pav.owl".freeze,
      "owl:priorVersion": "pav:2.2".freeze,
      "owl:versionIRI": "pav:2.3".freeze,
      "owl:versionInfo": "2.3.1".freeze,
      "prov:has_provenance": "pav:provenance.ttl".freeze,
      "rdfs:seeAlso": ["http://code.google.com/p/pav-ontology/".freeze, "http://pav-ontology.googlecode.com/svn/trunk/1.2/pav.owl".freeze, "https://code.google.com/p/pav-ontology/wiki/Versions".freeze, "pav:doc".freeze, "prov:".freeze],
      type: "owl:Ontology".freeze

    # Property definitions
    property :authoredBy,
      comment: %(An agent that originated or gave existence to the work that is expressed by the digital resource.

The author of the content of a resource may be different from the creator of the resource representation \(although they are often the same\). See pav:createdBy for a discussion.

pav:authoredBy is more specific than its superproperty dct:creator - which might or might not be interpreted to also cover the creation of the representation of the artifact.

The author is usually not a software agent \(which would be indicated with pav:createdWith, pav:createdBy or pav:importedBy\), unless the software actually authored the content itself; for instance an artificial intelligence algorithm which authored a piece of music or a machine learning algorithm that authored a classification of a tumor sample.

The date of authoring can be expressed using pav:authoredOn - note however in the case of multiple authors that there is no relationship in PAV identifying which agent contributed when or what. If capturing such lineage is desired, it should be additionally expressed using PROV relationships like prov:qualifiedAttribution or prov:wasGeneratedBy.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/authoredBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Authored by".freeze,
      "rdfs:seeAlso": ["pav:authoredOn".freeze, "pav:createdBy".freeze],
      subPropertyOf: ["dc:creator".freeze, "pav:contributedBy".freeze],
      type: "owl:ObjectProperty".freeze
    property :authoredOn,
      comment: %(The date this resource was authored.

pav:authoredBy gives the authoring agent.

Note that pav:authoredOn is different from pav:createdOn, although they are often the same. See pav:createdBy for a discussion.

This property is normally used in a functional way, indicating the last time of authoring, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Authored on".freeze,
      "rdfs:seeAlso": ["pav:authoredBy".freeze, "pav:createdBy".freeze, "pav:createdOn".freeze],
      subPropertyOf: "pav:contributedOn".freeze,
      type: "owl:DatatypeProperty".freeze
    property :contributedBy,
      comment: %(The resource was contributed to by the given agent.

Specifies an agent that provided any sort of help in conceiving the work that is expressed by the digital artifact.

Contributions can take many forms, of which PAV define the subproperties pav:authoredBy and pav:curatedBy; however other specific roles could also be specified by pav:contributedBy or custom subproperties, such as illustrating, investigating or managing the underlying data source. Contributions can additionally be expressed in detail using prov:qualifiedAttribution and prov:hadRole.

Note that pav:contributedBy identifies only agents that contributed to the work, knowledge or intellectual property, and not agents that made the digital artifact or representation \(pav:createdBy\), thus the considerations for software agents is similar to for pav:authoredBy and pav:curatedBy.

pav:contributedBy is more specific than its superproperty dct:contributor - which might or might not be interpreted to also cover contributions to making the representation of the artifact.


The date of contribution can be expressed using pav:contributedOn - note however in the case of multiple contributors that there is no relationship in PAV identifying which agent contributed when or what. If capturing such lineage is desired, it should be additionally expressed using PROV relationships like prov:qualifiedAttribution or prov:wasGeneratedBy.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/contributedBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Contributed by".freeze,
      "rdfs:seeAlso": ["pav:contributedOn".freeze, "pav:createdBy".freeze],
      subPropertyOf: ["dc:contributor".freeze, "prov:wasAttributedTo".freeze],
      type: "owl:ObjectProperty".freeze
    property :contributedOn,
      comment: %(The date this resource was contributed to.

pav:contributedBy provides the agent\(s\) that contributed.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Contributed on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": "pav:contributedBy".freeze,
      type: "owl:DatatypeProperty".freeze
    property :createdAt,
      comment: %(The geo-location of the agents when creating the resource \(pav:createdBy\). For instance  a photographer takes a picture of the Eiffel Tower while standing in front of it.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Created at".freeze,
      "rdfs:seeAlso": "pav:createdBy".freeze,
      type: "owl:ObjectProperty".freeze
    property :createdBy,
      comment: %(An agent primary responsible for making the digital artifact or resource representation.

This property is distinct from forming the content, which is indicated with pav:contributedBy or its subproperties; pav:authoredBy, which identifies who authored the knowledge expressed by this resource; and pav:curatedBy, which identifies who curated the knowledge into its current form.

pav:createdBy is more specific than its superproperty dct:creator - which might or might not be interpreted to cover this creator.

For instance, the author wrote 'this species has bigger wings than normal' in his log book. The curator, going through the log book and identifying important knowledge, formalizes this as 'locus perculus has wingspan > 0.5m'. The creator enters this knowledge as a digital resource in the knowledge system, thus creating the digital artifact \(say as JSON, RDF, XML or HTML\).

A different example is a news article. pav:authoredBy indicates the journalist who wrote the article. pav:contributedBy can indicate the artist who added an illustration. pav:curatedBy can indicate the editor who made the article conform to the news paper's style. pav:createdBy can indicate who put the article on the web site.

The software tool used by the creator to make the digital resource \(say Protege, Wordpress or OpenOffice\) can be indicated with pav:createdWith.

The date the digital resource was created can be indicated with pav:createdOn.

The location the agent was at when creating the digital resource can be made using pav:createdAt.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/createdBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Created by".freeze,
      "rdfs:seeAlso": ["pav:authoredBy".freeze, "pav:createdAt".freeze, "pav:createdOn".freeze, "pav:curatedBy".freeze],
      subPropertyOf: ["dc:creator".freeze, "prov:wasAttributedTo".freeze],
      type: "owl:ObjectProperty".freeze
    property :createdOn,
      comment: %(The date of creation of the resource representation.

The agents responsible can be indicated with pav:createdBy.

This property is normally used in a functional way, indicating the time of creation, although PAV does not formally restrict this. pav:lastUpdateOn can be used to indicate minor updates that did not affect the creating date.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/createdOn".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Created on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": "pav:createdBy".freeze,
      type: "owl:DatatypeProperty".freeze
    property :createdWith,
      comment: %(The software/tool used by the creator \(pav:createdBy\) when making the digital resource, for instance a word processor or an annotation tool. A more independent software agent that creates the resource without direct interaction by a human creator should instead should instead by indicated using pav:createdBy.
).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Created with".freeze,
      "rdfs:seeAlso": "pav:createdBy".freeze,
      subPropertyOf: "prov:wasAttributedTo".freeze,
      type: "owl:ObjectProperty".freeze
    property :curatedBy,
      comment: %(Specifies an agent specialist responsible for shaping the expression in an appropriate format. Often the primary agent responsible for ensuring the quality of the representation.

The curator may be different from the author \(pav:authoredBy\) and creator of the digital resource \(pav:createdBy\).

The curator may in some cases be a software agent, for instance text mining software which adds hyperlinks for recognized genome names.

The date of curating can be expressed using pav:curatedOn - note however in the case of multiple curators that there is no relationship in PAV identifying which agent contributed when or what. If capturing such lineage is desired, it should be additionally expressed using PROV relationships like prov:qualifiedAttribution or prov:wasGeneratedBy.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/curatedBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Curated by".freeze,
      "rdfs:seeAlso": ["pav:createdBy".freeze, "pav:curatedOn".freeze],
      subPropertyOf: "pav:contributedBy".freeze,
      type: "owl:ObjectProperty".freeze
    property :curatedOn,
      comment: %(The date this resource was curated.

pav:curatedBy gives the agent\(s\) that performed the curation.

This property is normally used in a functional way, indicating the last curation date, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Curated on".freeze,
      "rdfs:seeAlso": "pav:curatedBy".freeze,
      subPropertyOf: "pav:contributedOn".freeze,
      type: "owl:DatatypeProperty".freeze
    property :curates,
      comment: %(Provided for backwards compatibility. Use instead the inverse pav:curatedBy.).freeze,
      inverseOf: "pav:curatedBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Curates".freeze,
      "owl:deprecated": "true".freeze,
      type: "owl:ObjectProperty".freeze
    property :derivedFrom,
      comment: %(Derived from a different resource.

Derivation conserns itself with derived knowledge. If this resource has the same content as the other resource, but has simply been transcribed to fit a different model \(like XML -> RDF or SQL -> CVS\), use pav:importedFrom. If a resource was simply retrieved, use pav:retrievedFrom. If the content has however been further refined or modified, pav:derivedFrom should be used.

Details about who performed the derivation \(e.g. who did the refining or modifications\) may be indicated with pav:contributedBy and its subproperties.
).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Derived from".freeze,
      "rdfs:seeAlso": ["pav:importedFrom".freeze, "pav:previousVersion".freeze],
      subPropertyOf: "prov:wasDerivedFrom".freeze,
      type: "owl:ObjectProperty".freeze
    property :hasCurrentVersion,
      comment: %(This resource has a more specific, versioned resource with equivalent content.

This property is intended for relating a non-versioned or abstract resource to a single snapshot that can be used as a permalink to indicate the current version of the content.

For instance, if today is 2013-12-25, then a News page can indicate a corresponding snapshot resource which will refer to the news as they were of 2013-12-25.

    <http://news.example.com/> pav:hasCurrentVersion <http://news.example.com/2013-12-25/> .

"Equivalent content" is a loose definition, for instance the snapshot resource might include additional information to indicate it is a snapshot, and is not required to be immutable.

Other versioned resources indicating the content at earlier times MAY be indicated with the superproperty pav:hasVersion, one of which MAY be related to the current version using pav:hasCurrentVersion:

    <http://news.example.com/2013-12-25/> pav:previousVersion <http://news.example.com/2013-12-24/> .
    <http://news.example.com/> pav:hasVersion <http://news.example.com/2013-12-23/> .

Note that it might be confusing to also indicate pav:previousVersion from a resource that has hasCurrentVersion relations, as such a resource is intended to be a long-living "unversioned" resource. The PAV ontology does however not formally restrict this, to cater for more complex scenarios with multiple abstraction levels.

Similarly, it would normally be incorrect to indicate a pav:hasCurrentVersion from an older version; instead the current version would be found by finding the non-versioned resource that the particular resource is a version of, and then its current version.

This property is normally used in a functional way, although PAV does not formally restrict this.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Has current version".freeze,
      subPropertyOf: "pav:hasVersion".freeze,
      type: "owl:ObjectProperty".freeze
    property :hasEarlierVersion,
      comment: %(This versioned resource has an earlier version.

Any earlier version of this resource can be indicated with pav:hasEarlierVersion, e.g.:

<http://example.com/v4> pav:hasEarlierVersion <http://example.com/v2> ;
    pav:hasEarlierVersion <http://example.com/v1> .


The subproperty pav:previousVersion SHOULD be used if the earlier version is the direct ancestor of this version.

<http://example.com/v4> pav:previousVersion <http://example.com/v3> .


This property is transitive, so it should not be necessary to repeat the earlier versions of an earlier version. A chain of previous versions can be declared using the subproperty pav:previousVersion, implying that the previous previous version is also an earlier version. It might however still be useful to declare an earlier version explicitly, for instance because it is an earlier version of high relevance or because the complete chain of pav:previousVersion is not available.


To indicate that this version is a snapshot of a more general, non-versioned resource, e.g. "Weather Today" vs. "Weather Today on 2013-12-07", see pav:hasVersion.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Has earlier version".freeze,
      subPropertyOf: "prov:alternateOf".freeze,
      type: ["owl:AsymmetricProperty".freeze, "owl:ObjectProperty".freeze, "owl:TransitiveProperty".freeze]
    property :hasVersion,
      comment: %(This resource has a more specific, versioned resource.

This property is intended for relating a non-versioned or abstract resource to several versioned resources, e.g. snapshots. For instance, if there are two snapshots of the News page, made on 23rd and 24th of December, then:

    <http://news.example.com/> pav:hasVersion <http://news.example.com/2013-12-23/> ;
        pav:hasVersion <http://news.example.com/2013-12-24/> .

If one of the versions has somewhat the equivalent content to this resource \(e.g. can be used as a permalink for this resource\), then it should instead be indicated with the subproperty pav:hasCurrentVersion:

    <http://news.example.com/> pav:hasCurrentVersion <http://news.example.com/2013-12-25/> .

To order the versions, use pav:previousVersion:

    <http://news.example.com/2013-12-25/> pav:previousVersion <http://news.example.com/2013-12-24/> .
    <http://news.example.com/2013-12-24/> pav:previousVersion <http://news.example.com/2013-12-23/> .

Note that it might be confusing to also indicate pav:previousVersion from a resource that has pav:hasVersion relations, as such a resource is intended to be a long-living "unversioned" resource. The PAV ontology does however not formally restrict this, to cater for more complex scenarios with multiple abstraction levels.

pav:hasVersion is a subproperty of dcterms:hasVersion to more strongly define this hierarchical pattern. It is therefore also a subproperty of pav:generalizationOf \(inverse of prov:specializationOf\).

To indicate the existence of other, non-hierarchical kind of editions and adaptations of this resource that are not versioned snapshots \(e.g. Powerpoint slides has a video recording version\), use instead dcterms:hasVersion or prov:alternateOf.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Has version".freeze,
      "rdfs:seeAlso": ["pav:hasCurrentVersion".freeze, "pav:previousVersion".freeze],
      subPropertyOf: ["dc:hasVersion".freeze, "prov:generalizationOf".freeze],
      type: "owl:ObjectProperty".freeze
    property :importedBy,
      comment: %(An entity responsible for importing the data.

The importer is usually a software entity which has done the transcription from the original source.

Note that pav:importedBy may overlap with pav:createdWith.

The source for the import should be given with pav:importedFrom. The time of the import should be given with pav:importedOn.

See pav:importedFrom for a discussion of import vs. retrieve vs. derived.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/importedBy".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Imported by".freeze,
      "rdfs:seeAlso": "pav:importedFrom".freeze,
      subPropertyOf: "prov:wasAttributedTo".freeze,
      type: "owl:ObjectProperty".freeze
    property :importedFrom,
      comment: %(The original source of imported information.

Import means that the content has been preserved, but transcribed somehow, for instance to fit a different representation model by converting formats. Examples of import are when the original was JSON and the current resource is RDF, or where the original was an document scan, and this resource is the plain text found through OCR.

The imported resource does not have to be complete, but should be consistent with the knowledge conveyed by the original resource.

If additional knowledge has been contributed, pav:derivedFrom would be more appropriate.

If the resource has been copied verbatim from the original representation \(e.g. downloaded\), use pav:retrievedFrom.

To indicate which agent\(s\) performed the import, use pav:importedBy. Use pav:importedOn to indicate when it happened. ).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/importedFromSource".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Imported from".freeze,
      "rdfs:seeAlso": ["pav:derivedFrom".freeze, "pav:importedBy".freeze, "pav:importedOn".freeze, "pav:retrievedFrom".freeze],
      subPropertyOf: "prov:wasDerivedFrom".freeze,
      type: "owl:ObjectProperty".freeze
    property :importedOn,
      comment: %(The date this resource was imported from a source \(pav:importedFrom\).

Note that pav:importedOn may overlap with pav:createdOn, but in cases where they differ, the import time indicates the time of the retrieval and transcription of the original source, while the creation time indicates when the final resource was made, for instance after user approval.

This property is normally used in a functional way, indicating the first import date, although PAV does not formally restrict this. If the resource is later reimported, this should instead be indicated with pav:lastRefreshedOn.

The source of the import should be given with pav:importedFrom. The agent that performed the import should be given with pav:importedBy.

See pav:importedFrom for a discussion about import vs. retrieval.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/importedOn".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Imported on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:importedBy".freeze, "pav:importedFrom".freeze],
      type: "owl:DatatypeProperty".freeze
    property :lastRefreshedOn,
      comment: %(The date of the last re-import of the resource. This property is used in addition to pav:importedOn if this version has been updated due to a re-import. If the re-import created a new resource rather than refreshing an existing resource, then instead use pav:importedOn together with pav:previousVersion.

This property is normally used in a functional way, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/importedLastOn".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Last refreshed on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:importedBy".freeze, "pav:importedFrom".freeze, "pav:importedOn".freeze, "pav:previousVersion".freeze],
      type: "owl:DatatypeProperty".freeze
    property :lastUpdateOn,
      comment: %(The date of the last update of the resource. An update is a change which did not warrant making a new resource related using pav:previousVersion, for instance correcting a spelling mistake.

This property is normally used in a functional way, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/lastUpdateOn".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Last updated on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:createdOn".freeze, "pav:previousVersion".freeze],
      type: "owl:DatatypeProperty".freeze
    property :previousVersion,
      comment: %(The previous version of a resource in a lineage. For instance a news article updated to correct factual information would point to the previous version of the article with pav:previousVersion. If however the content has significantly changed so that the two resources no longer share lineage \(say a new article that talks about the same facts\), they can instead be related using pav:derivedFrom.

This property is normally used in a functional way, although PAV does not formally restrict this. Earlier versions which are not direct ancestors of this resource may instead be provided using the superproperty pav:hasEarlierVersion.

A version number of this resource can be provided using the data property pav:version.

To indicate that this version is a snapshot of a more general, non-versioned resource, e.g. "Weather Today" vs. "Weather Today on 2013-12-07", see pav:hasVersion.

Note that it might be confusing to indicate pav:previousVersion from a resource that also has pav:hasVersion or pav:hasCurrentVersion relations, as such resources are intended to be a long-living and "unversioned", while pav:previousVersion is intended for use between permalink-like "snapshots" arranged in a linear history.  ).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/previousVersion".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Previous version".freeze,
      "rdfs:seeAlso": ["pav:derivedFrom".freeze, "pav:hasVersion".freeze, "pav:version".freeze],
      subPropertyOf: ["pav:hasEarlierVersion".freeze, "prov:wasRevisionOf".freeze],
      type: "owl:ObjectProperty".freeze
    property :providedBy,
      comment: %(The original provider of the encoded information \(e.g. PubMed, UniProt, Science Commons\).

The provider might not coincide with the dct:publisher, which would describe the current publisher of the resource. For instance if the resource was retrieved, imported or derived from a source, that source was published by the original provider. pav:providedBy provides a shortcut to indicate that original provider on the new resource.  ).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Provided by".freeze,
      "rdfs:seeAlso": "dc:publisher".freeze,
      type: "owl:ObjectProperty".freeze
    property :retrievedBy,
      comment: %(An entity responsible for retrieving the data from an external source.

The retrieving agent is usually a software entity, which has done the retrieval from the original source without performing any transcription.

The source that was retrieved should be given with pav:retrievedFrom. The time of the retrieval should be indicated using pav:retrievedOn.

See pav:importedFrom for a discussion of import vs. retrieve vs. derived.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Retrieved by".freeze,
      "rdfs:seeAlso": "pav:importedFrom".freeze,
      subPropertyOf: "prov:wasAttributedTo".freeze,
      type: "owl:ObjectProperty".freeze
    property :retrievedFrom,
      comment: %(The URI where a resource has been retrieved from.

The retrieving agent is usually a software entity, which has done the retrieval from the original source without performing any transcription.

Retrieval indicates that this resource has the same representation as the original resource. If the resource has been somewhat transformed, use pav:importedFrom instead.

The time of the retrieval should be indicated using pav:retrievedOn. The agent may be indicated with pav:retrievedBy.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Retrieved from".freeze,
      "rdfs:seeAlso": ["pav:retrievedBy".freeze, "pav:retrievedOn".freeze],
      subPropertyOf: "prov:wasDerivedFrom".freeze,
      type: "owl:ObjectProperty".freeze
    property :retrievedOn,
      comment: %(The date the source for this resource was retrieved.

The source that was retrieved should be indicated with pav:retrievedFrom. The agent that performed the retrieval may be specified with pav:retrievedBy.

This property is normally used in a functional way, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Retrieved on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:retrievedBy".freeze, "pav:retrievedFrom".freeze],
      type: "owl:DatatypeProperty".freeze
    property :sourceAccessedAt,
      comment: %(The resource is related to a given source which was accessed or consulted \(but not retrieved, imported or derived from\). This access can be detailed with pav:sourceAccessedBy and pav:sourceAccessedOn.

For instance, a curator \(pav:curatedBy\) might have consulted figures in a published paper to confirm that a dataset was correctly pav:importedFrom the paper's supplementary CSV file.

Another example: I can access the page for tomorrow weather in Boston \(http://www.weather.com/weather/tomorrow/Boston+MA+02143\)  and I can blog ‘tomorrow is going to be nice’. The source does not make any claims about the nice weather, that is my interpretation; therefore the blog post has pav:sourceAccessedAt the weather page. ).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Source accessed at".freeze,
      "rdfs:seeAlso": ["pav:importedFrom".freeze, "pav:retrievedFrom".freeze, "pav:sourceAccessedBy".freeze, "pav:sourceAccessedOn".freeze, "pav:sourceLastAccessedOn".freeze],
      subPropertyOf: "prov:wasInfluencedBy".freeze,
      type: "owl:ObjectProperty".freeze
    property :sourceAccessedBy,
      comment: %(The resource is related to a source which was accessed or consulted
by the given agent. The source\(s\) should be specified using pav:sourceAccessedAt, and the time with pav:sourceAccessedOn.

For instance, the given agent could be a curator \(also pav:curatedBy\) which consulted figures in a published paper to confirm that a dataset was correctly pav:importedFrom the paper's supplementary CSV file.).freeze,
      isDefinedBy: "pav:".freeze,
      label: "Source accessed by".freeze,
      "rdfs:seeAlso": "pav:sourceAccessedAt".freeze,
      type: "owl:ObjectProperty".freeze
    property :sourceAccessedOn,
      comment: %(The resource is related to a source which was originally accessed or consulted on the given date as part of creating or authoring the resource. The source\(s\) should be specified using pav:sourceAccessedAt.

For instance, if the source accessed described the weather forecast for the next day, the time of source access can be crucial information.

This property is normally used in a functional way, although PAV does not formally restrict this. If the source is subsequently checked again \(say to verify validity\), this should be indicated with pav:sourceLastAccessedOn.

In the case multiple sources being accessed at different times or by different agents, PAV does not distinguish who accessed when what. If such details are required, they may be provided by additionally using prov:qualifiedInfluence.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: ["http://swan.mindinformatics.org/ontologies/1.2/pav/sourceAccessedOn".freeze, "http://swan.mindinformatics.org/ontologies/1.2/pav/sourceFirstAccessedOn".freeze],
      isDefinedBy: "pav:".freeze,
      label: "Source accessed on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:createdAt".freeze, "pav:sourceAccessedAt".freeze, "pav:sourceAccessedBy".freeze, "pav:sourceLastAccessedOn".freeze],
      type: "owl:DatatypeProperty".freeze
    property :sourceLastAccessedOn,
      comment: %(The resource is related to a source which was last accessed or consulted on the given date. The source\(s\) should be specified using pav:sourceAccessedAt. Usage of this property indicates that the source has been checked previously, which the initial time should be indicated with pav:sourceAccessedOn.

This property can be useful together with pav:lastRefreshedOn or pav:lastUpdateOn in order to indicate a re-import or update, but could also be used alone, for instance when a source was simply verified and no further action was taken for the resource.

This property is normally used in a functional way, although PAV does not formally restrict this.

The value is of type xsd:dateTime, for instance "2013-03-26T14:49:00+01:00"^^xsd:dateTime. The timezone information \(Z for UTC, +01:00 for UTC+1, etc\) SHOULD be included unless unknown. If the time \(or parts of time\) is unknown, use 00:00:00Z. If the day/month is unknown, use 01-01, for instance, if we only know September 1983, then use "1983-09-01T00:00:00Z"^^xsd:dateTime.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/sourceLastAccessedOn".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Source last accessed on".freeze,
      range: "xsd:dateTime".freeze,
      "rdfs:seeAlso": ["pav:createdAt".freeze, "pav:sourceAccessedAt".freeze, "pav:sourceAccessedBy".freeze],
      type: "owl:DatatypeProperty".freeze
    property :version,
      comment: %(The version number of a resource. This is a freetext string, typical values are "1.5" or "21". The URI identifying the previous version can be provided using prov:previousVersion.

This property is normally used in a functional way, although PAV does not formally restrict this.).freeze,
      equivalentProperty: "http://swan.mindinformatics.org/ontologies/1.2/pav/versionNumber".freeze,
      isDefinedBy: "pav:".freeze,
      label: "Version".freeze,
      range: "xsd:string".freeze,
      "rdfs:seeAlso": "pav:previousVersion".freeze,
      type: "owl:DatatypeProperty".freeze

    RDF::Vocabulary.register :pav, self if
      RDF::Vocabulary.respond_to? :register
  end
end
