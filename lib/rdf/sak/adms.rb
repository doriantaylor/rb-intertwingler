# -*- encoding: utf-8 -*-
# frozen_string_literal: true
# This file generated automatically using rdf vocabulary format from http://www.w3.org/ns/adms#
require 'rdf'
module RDF::SAK
  # @!parse
  #   # Vocabulary for <http://www.w3.org/ns/adms#>
  #   #
  #   # Asset Description Metadata Schema (ADMS)
  #   #
  #   # Asset Description Metadata Schema (ADMS)
  #   #
  #   # This file specifies the set of RDF classes and properties used in ADMS
  #   #
  #   # This is the RDF encoding of the Asset Description Metadata Schema, originally developed under the European Union's ISA Programme and further developed by the W3C Government Linked Data Working Group. It re-uses terms from several other vocabularies, notably Dublin Core, with elements of SKOS, FOAF and more. 
  #   class ADMS < RDF::StrictVocabulary
  #     # An Asset is an abstract entity that reflects the intellectual content of the asset and represents those characteristics of the asset that are independent of its physical embodiment. This abstract entity combines the FRBR entities work (a distinct intellectual or artistic creation) and expression (the intellectual or artistic realization of a work). Assets can be versioned. Every time the intellectual content of an asset changes, the result is considered to be a new asset that can be linked to previous and next versions of the Asset. The physical embodiment of an Asset is called an Asset Distribution. A particular Asset may have zero or more Asset Distributions..
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Asset
  #
  #     # An Asset Distribution represents a particular physical embodiment of an Asset, which is an example of the FRBR entity manifestation (the physical embodiment of an expression of a work). An Asset Distribution is typically a downloadable computer file (but in principle it could also be a paper document) that implements the intellectual content of an Asset. A particular Asset Distribution is associated with one and only one Asset, while all Distributions of an Asset share the same intellectual content in different physical formats. Asset Distributions themselves are not versioned.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :AssetDistribution
  #
  #     # An Asset Repository is a system or service that provides facilities for storage and maintenance of descriptions of Assets and Asset Distributions, and functionality that allows users to search and access these descriptions. An Asset Repository will typically contain descriptions of several Assets and related Asset Distributions.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :AssetRepository
  #
  #     # This class is based on the UN/CEFACT Identifier complex type defined in See Section 5.8 of Core Components Data Type Catalogue Version 3.1 (http://www.unece.org/fileadmin/DAM/cefact/codesfortrade/CCTS/CCTS-DTCatalogueVersion3p1.pdf) In RDF this is expressed using the following properties: - the content string should be provided using skos:notation, datatyped with the identifier scheme (inclduing the version number if appropriate); - use dcterms:creator to link to a class describing the agency that manages the identifier scheme or adms:schemaAgency to provide the name as a literal. Although not part of the ADMS conceptual model, it may be useful to provide further properties to the Identifier class such as dcterms:created to provide the date on which the identifier was issued.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :Identifier
  #
  #     # adms:identifier is used to link any resource to an instance of adms:Identifier which is its range. N.B. it is not appropriate to use dcterms:identifer to link to the Identifier class as its range is rdfs:Literal. ADMS uses this to provide any identifier for the Asset.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :identifier
  #
  #     # Links to an Asset that is contained in the Asset being described, e.g. when there are several vocabularies defined in a single document.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :includedAsset
  #
  #     # Links an Asset to its adms:InteroperabilityLevel. Since this is encoded using skos:Concept, that is the defined range for this property.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :interoperabilityLevel
  #
  #     # A link to the current or latest version of the Asset.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :last
  #
  #     # A link to the next version of the Asset.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :next
  #
  #     # A link to the previous version of the Asset.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :prev
  #
  #     # Links an adms:AssetDistribution to a skos:Concept that is its adms:RepresentationTechnique.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :representationTechnique
  #
  #     # Links to a sample of an Asset (which is itself an Asset).
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :sample
  #
  #     # The name of the agency responsible for issuing the identifier
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :schemeAgency
  #
  #     # Links to the status of the Asset or Asset Distribution in the context of a particular workflow process. Since Status is defined using a skos:Concept, that is the defined range for this property.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :status
  #
  #     # A schema according to which the Asset Repository can provide data about its content, e.g. ADMS
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :supportedSchema
  #
  #     # Links Assets that are translations of each other.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :translation
  #
  #     # A description of changes between this version and the previous version of the Asset.
  #     # @return [RDF::Vocabulary::Term]
  #     attr_reader :versionNotes
  #
  #   end
  ADMS = Class.new(RDF::StrictVocabulary("http://www.w3.org/ns/adms#")) do

    # Ontology definition
    ontology :"http://www.w3.org/ns/adms#",
      comment: {en: "This file specifies the set of RDF classes and properties used in ADMS"},
      "http://purl.org/dc/terms/abstract": {en: "ADMS is a profile of DCAT, used to describe semantic assets (or just 'Assets'), defined as highly reusable metadata (e.g. xml schemata, generic data models) and reference data (e.g. code lists, taxonomies, dictionaries, vocabularies) that are used for eGovernment system development."},
      "http://purl.org/dc/terms/creator": [term(
          "http://schema.org/affiliation": "http://www.w3.org/data#W3C",
          "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://philarcher.org/foaf.rdf#me",
          "http://xmlns.com/foaf/0.1/homepage": "http://www.w3.org/People/all#phila",
          "http://xmlns.com/foaf/0.1/name": "Phil Archer"
        ), term(
          "http://schema.org/affiliation": term(
            "http://xmlns.com/foaf/0.1/homepage": "http://ies.jrc.ec.europa.eu/DE",
            "http://xmlns.com/foaf/0.1/name": "European Commission, Joint Research Centre "
          ),
          "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://andrea-perego.name/foaf/#me",
          "http://xmlns.com/foaf/0.1/homepage": "http://andrea-perego.name/",
          "http://xmlns.com/foaf/0.1/name": "Andrea Perego"
        ), term(
          "http://www.w3.org/2000/01/rdf-schema#seeAlso": "http://makxdekkers.com/makxdekkers.rdf#me",
          "http://xmlns.com/foaf/0.1/homepage": "http://makxdekkers.com/",
          "http://xmlns.com/foaf/0.1/name": "Makx Dekkers"
        )],
      "http://purl.org/dc/terms/description": "This is the RDF encoding of the Asset Description Metadata Schema, originally \n    developed under the European Union's ISA Programme and further developed by the W3C Government Linked Data Working Group.\n    It re-uses terms from several other vocabularies, notably Dublin Core, with elements of SKOS, FOAF and more. ",
      "http://purl.org/dc/terms/modified": ["2013-05-24", "2013-09-16", "2013-12-21", "2015-07-22"],
      "http://purl.org/dc/terms/title": {en: "Asset Description Metadata Schema (ADMS)"},
      "http://purl.org/vocab/vann/preferredNamespacePrefix": "adms",
      "http://purl.org/vocab/vann/preferredNamespaceUri": "http://www.w3.org/ns/adms#",
      "http://purl.org/vocommons/voaf#specializes": "http://www.w3.org/ns/dcat#",
      "http://www.w3.org/2007/05/powder-s#describedby": "http://www.w3.org/TR/vocab-adms",
      "http://xmlns.com/foaf/0.1/maker": term(
          "http://xmlns.com/foaf/0.1/homepage": "http://www.w3.org/2011/gld/",
          "http://xmlns.com/foaf/0.1/name": "Government Linked Data WG, based on output from the ADMS Working Group"
        ),
      label: {en: "Asset Description Metadata Schema (ADMS)"},
      type: ["http://purl.org/vocommons/voaf#Vocabulary", "http://www.w3.org/2002/07/owl#Ontology"]

    # Class definitions
    term :Asset,
      comment: {en: "An Asset is an abstract entity that reflects the intellectual content of the asset and represents those characteristics of the asset that are independent of its physical embodiment. This abstract entity combines the FRBR entities work (a distinct intellectual or artistic creation) and expression (the intellectual or artistic realization of a work). Assets can be versioned. Every time the intellectual content of an asset changes, the result is considered to be a new asset that can be linked to previous and next versions of the Asset. The physical embodiment of an Asset is called an Asset Distribution. A particular Asset may have zero or more Asset Distributions.."},
      "http://purl.org/dc/terms/identifier": "adms:Asset",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "Asset"},
      subClassOf: "http://www.w3.org/ns/dcat#Dataset",
      type: "http://www.w3.org/2000/01/rdf-schema#Class"
    term :AssetDistribution,
      comment: {en: "An Asset Distribution represents a particular physical embodiment of an Asset, which is an example of the FRBR entity manifestation (the physical embodiment of an expression of a work). An Asset Distribution is typically a downloadable computer file (but in principle it could also be a paper document) that implements the intellectual content of an Asset. A particular Asset Distribution is associated with one and only one Asset, while all Distributions of an Asset share the same intellectual content in different physical formats. Asset Distributions themselves are not versioned."},
      "http://purl.org/dc/terms/identifier": "adms:AssetDistribution",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "Asset Distribution"},
      subClassOf: "http://www.w3.org/ns/dcat#Distribution",
      type: "http://www.w3.org/2000/01/rdf-schema#Class"
    term :AssetRepository,
      comment: {en: "An Asset Repository is a system or service that provides facilities for storage and maintenance of descriptions of Assets and Asset Distributions, and functionality that allows users to search and access these descriptions. An Asset Repository will typically contain descriptions of several Assets and related Asset Distributions."},
      "http://purl.org/dc/terms/identifier": "adms:AssetRepository",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "Asset Repository"},
      subClassOf: "http://www.w3.org/ns/dcat#Catalog",
      type: "http://www.w3.org/2000/01/rdf-schema#Class"
    term :Identifier,
      comment: "This class is based on the UN/CEFACT Identifier complex type defined in See Section 5.8 of Core Components Data Type Catalogue Version 3.1 (http://www.unece.org/fileadmin/DAM/cefact/codesfortrade/CCTS/CCTS-DTCatalogueVersion3p1.pdf) In RDF this is expressed using the following properties: - the content string should be provided using skos:notation, datatyped with the identifier scheme (inclduing the version number if appropriate); - use dcterms:creator to link to a class describing the agency that manages the identifier scheme or adms:schemaAgency to provide the name as a literal. Although not part of the ADMS conceptual model, it may be useful to provide further properties to the Identifier class such as dcterms:created to provide the date on which the identifier was issued.",
      "http://purl.org/dc/terms/identifier": "adms:Identifier",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "Identifier"},
      type: "http://www.w3.org/2000/01/rdf-schema#Class"

    # Property definitions
    property :identifier,
      comment: {en: "adms:identifier is used to link any resource to an instance of adms:Identifier which is its range. N.B. it is not appropriate to use dcterms:identifer to link to the Identifier class as its range is rdfs:Literal. ADMS uses this to provide any identifier for the Asset."},
      "http://purl.org/dc/terms/identifier": "adms:identifier",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "identifier"},
      range: "http://www.w3.org/ns/adms#Identifier",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :includedAsset,
      comment: {en: "Links to an Asset that is contained in the Asset being described, e.g. when there are several vocabularies defined in a single document."},
      domain: "http://www.w3.org/ns/adms#Asset",
      "http://purl.org/dc/terms/identifier": "adms:includedAsset",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "included asset"},
      range: "http://www.w3.org/ns/adms#Asset",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :interoperabilityLevel,
      comment: {en: "Links an Asset to its adms:InteroperabilityLevel. Since this is encoded using skos:Concept, that is the defined range for this property."},
      domain: "http://www.w3.org/ns/adms#Asset",
      "http://purl.org/dc/terms/identifier": "adms:interoperabilityLevel",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "interoperability level"},
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :last,
      comment: {en: "A link to the current or latest version of the Asset."},
      "http://purl.org/dc/terms/identifier": "adms:last",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "last"},
      subPropertyOf: "http://www.w3.org/1999/xhtml/vocab#last",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :next,
      comment: {en: "A link to the next version of the Asset."},
      "http://purl.org/dc/terms/identifier": "adms:next",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "next"},
      subPropertyOf: "http://www.w3.org/1999/xhtml/vocab#next",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :prev,
      comment: {en: "A link to the previous version of the Asset."},
      "http://purl.org/dc/terms/identifier": "adms:prev",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "prev"},
      subPropertyOf: "http://www.w3.org/1999/xhtml/vocab#prev",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :representationTechnique,
      comment: {en: "Links an adms:AssetDistribution to a skos:Concept that is its adms:RepresentationTechnique."},
      "http://purl.org/dc/terms/identifier": "adms:representationTechnique",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "representation technique"},
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :sample,
      comment: {en: "Links to a sample of an Asset (which is itself an Asset)."},
      domain: "http://www.w3.org/ns/adms#Asset",
      "http://purl.org/dc/terms/identifier": "adms:sample",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "sample"},
      range: "http://www.w3.org/ns/adms#Asset",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :schemeAgency,
      comment: {en: "The name of the agency responsible for issuing the identifier"},
      domain: "http://www.w3.org/ns/adms#Identifier",
      "http://purl.org/dc/terms/identifier": "adms:schemeAgency",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "scheme agency"},
      range: "http://www.w3.org/2000/01/rdf-schema#Literal",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#DatatypeProperty"]
    property :status,
      comment: "Links to the status of the Asset or Asset Distribution in the context of a particular workflow process. Since Status is defined using a skos:Concept, that is the defined range for this property.",
      "http://purl.org/dc/terms/identifier": "adms:status",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "status"},
      range: "http://www.w3.org/2004/02/skos/core#Concept",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :supportedSchema,
      comment: {en: "A schema according to which the Asset Repository can provide data about its content, e.g. ADMS"},
      "http://purl.org/dc/terms/identifier": "adms:supportedSchema",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "supported schema"},
      range: "http://www.w3.org/ns/adms#Asset",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :translation,
      comment: {en: "Links Assets that are translations of each other."},
      "http://purl.org/dc/terms/identifier": "adms:translation",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "translation"},
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#ObjectProperty"]
    property :versionNotes,
      comment: {en: "A description of changes between this version and the previous version of the Asset."},
      "http://purl.org/dc/terms/identifier": "adms:versionNotes",
      isDefinedBy: "http://www.w3.org/TR/vocab-adms",
      label: {en: "version info"},
      range: "http://www.w3.org/2000/01/rdf-schema#Literal",
      type: ["http://www.w3.org/1999/02/22-rdf-syntax-ns#Property", "http://www.w3.org/2002/07/owl#DatatypeProperty"]

    RDF::Vocabulary.register :ci, self if
      RDF::Vocabulary.respond_to? :register
  end
end
