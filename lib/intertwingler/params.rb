require 'intertwingler/engine'
require 'intertwingler/vocab/ci'
require 'intertwingler/vocab/tfo'
require 'intertwingler/vocab/itcv'
require 'params/registry'

# This is an {Intertwingler}-specific adaptation of the more generic
# {Params::Registry}. In particular, it adds functionality for
# configuring parameters—and groups thereof—out of the graph.
#
# The main motivating factor for this configuration is that while I
# wanted to make it _possible_ for {Params::Registry} to source its
# configuration data from RDF, I didn't want doing so to be
# _necessary_. Intertwingler, however, _only_ sources its
# configuration from RDF, so any RDF configuration business belongs in
# Intertwingler for the time being.
#
# To complete the functionality that reads parameter specs out of the
# RDF graph, we need a mapping that goes from
# [XSD](https://www.w3.org/TR/xmlschema-2/) and/or
# [RDF](https://www.w3.org/TR/rdf-schema/#ch_literal) literal datatypes to
# {Params::Registry::Types}, which this module provides.
#
# @note While {Params::Registry} offers quite a bit of control for
#  parameter sets, at the time of this writing we are only interested
#  in the simple, single-cardinality, scalar parameters that decorate
#  [`tfo:Function`](https://vocab.methodandstructure.com/transformation#Function)
#  entities. When this behaviour changes, this note will be removed.
#
# @note It is not clear at the time of this writing how a global
#  parameter ordering would be configured (e.g., in the graph), or
#  even if that's a desirable thing. Currently the only user of this
#  registry are the transform infrastructure, and their parameters
#  would get registered one group at a time with no consideration for
#  a global sequence. Presumably other handlers would as well.
#
class Intertwingler::Params < Params::Registry

  # This is the group class with additional functionality for fetching
  # configuration from the graph.
  class Group < ::Params::Registry::Group

    # This assignor autovivifies the template from the graph.
    #
    # @note This may be dumb.
    #
    # @param id [Object] the template's canonical identifier.
    # @param spec [Hash{Symbol => Object}, Params::Registry::Template, nil]
    #  the template specification.
    #
    # @return [Params::Registry::Template] the new template
    #
    def []= id, spec
      spec = registry.templates[id] ||
        registry.template_class.new(registry, id) if spec == id or spec.nil?

      super id, spec
    end

    # Refresh the group and its constituent parameters.
    #
    # @return [self]
    #
    def refresh
      templates.each { |t| t.refresh }

      self
    end
  end

  # This is the template class with additional functionality for fetching
  # configuration from the graph.
  #
  # @note The mapping of XSD tpes
  #
  class Template < ::Params::Registry::Template

    include Intertwingler::GraphOps::Addressable

    private

    T   = ::Params::Registry::Types
    CI  = Intertwingler::Vocab::CI
    TFO = Intertwingler::Vocab::TFO
    XSD = RDF::Vocab::XSD

    # XXX need a solution for object properties, relative URIs, also
    # (compact) UUIDs.
    MAPPING = {
      nil                    => T::NormalizedString,
      RDF::RDFS.Literal      => T::String,
      RDF::RDFV.langString   => T::String,
      XSD.string             => T::String,
      XSD.token              => T::Token,
      XSD.integer            => T::DecimalInteger,
      XSD.negativeInteger    => T::NegativeInteger,
      XSD.positiveInteger    => T::PositiveInteger,
      XSD.nonNegativeInteger => T::NonNegativeInteger,
      XSD.nonPositiveInteger => T::NonPositiveInteger,
      XSD.date               => T::Date,
      XSD.dateTime           => T::Time,
    }

    def repo; registry.engine.repo; end

    # This post-init hook will refresh the template if it has no
    # configuration data.
    #
    def post_init
      refresh if blank?
    end

    public

    # Refresh the template from the graph. Currently manages `slug`,
    # `aliases`, `type` (mapped from XSD), and cardinality.
    #
    # @note This method does not configure the full feature set of
    #  {Params::Registry::Template}, because there is currently no
    #  ontology that represents it.
    #
    # @return [self]
    #
    def refresh

      if slug = literals(CI['canonical-slug']).sort.first
        # i guess this is what we do? lol
        @slug = slug.object.to_s.to_sym
      end

      @aliases = (
        literals(CI.slug).map { |a| a.object.to_s.to_sym } - [slug]).sort.uniq

      # XXX this may be subtler
      @type = MAPPING.fetch(
        resources(RDF::RDFS.range).sort.first, T::NormalizedString)

      # cardinality
      if c1 = numeric_literals(RDF::OWL.cardinality).sort.first
        @min = @max = c1.object
      else
        c1 = numeric_literals(RDF::OWL.minCardinality).sort.first
        c2 = numeric_literals(RDF::OWL.maxCardinality).sort.last
        @min = c1.object.to_i if c1
        @max = c2.object.to_i if c2
      end

      # we return self cause there's nothing here to see
      self
    end

  end

  # This constructor extends its parent {Params::Registry#initialize}
  # by prepending a mandatory `engine` parameter.
  #
  # @param engine [Intertwingler::Engine] for accessing various useful things.
  # @param templates [Hash] the hash of template specifications.
  # @param groups [Hash, Array] the hash of groups.
  # @param complement [Object, Hash] the identifier for the parameter
  #  for complementing composites, or otherwise a partial specification.
  #
  def initialize engine, templates: nil, groups: nil, complement: nil
    @engine = engine

    super templates: templates, groups: groups, complement: complement
  end

  # @!attribute [r] engine
  #  @return [Intertwingler::Engine] the engine.
  #
  attr_reader :engine

  def group_class; Group; end

  def template_class; Template; end

  def self.validate params
    (Params::Registry::Types::Array|Params::Registry::Types::TemplateMap)[params]
  end

end
