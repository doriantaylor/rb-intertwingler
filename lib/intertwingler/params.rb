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
class Intertwingler::Params < Params::Registry

  private

  T   = Params::Registry::Types
  XSD = RDF::Vocab::XSD

  MAPPING = {
    nil                    => T::String,
    XSD.string             => T::String,
    XSD.token              => T::Token,
    XSD.integer            => T::DecimalInteger,
    XSD.negativeInteger    => T::NegativeInteger,
    XSD.positiveInteger    => T::PositiveInteger,
    XSD.nonNegativeInteger => T::NonNegativeInteger,
    XSD.nonPositiveInteger => T::NonPositiveInteger,
  }

  public

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

  

end
