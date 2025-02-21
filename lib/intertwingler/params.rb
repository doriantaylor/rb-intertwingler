require 'intertwingler/engine'
require 'intertwingler/graphops'
require 'intertwingler/types'
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

  private

  T   = ::Params::Registry::Types
  I   = Intertwingler::Types
  CI  = Intertwingler::Vocab::CI
  TFO = Intertwingler::Vocab::TFO
  XSD = RDF::Vocab::XSD

  public

  # This is the group class with additional functionality for fetching
  # configuration from the graph.
  class Group < ::Params::Registry::Group
    include Intertwingler::GraphOps::Addressable

    private

    def repo ; registry.engine.repo ; end

    public

    alias_method :subject, :id

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

    # Refresh the group and (optionally) its constituent parameters.
    #
    # @param cascade [true, false] whether to cascade into the templates
    #
    # @return [self]
    #
    def refresh! cascade: true
      # warn subject.inspect

      # only do this if we're a graph buddy
      return self unless subject.is_a? RDF::URI

      # fetch the parameters out of the graph

      # XXX TODO better negotiation between ordered and unordered
      # (eg subtract ordered from unordered then sort unordered and
      # append it to the end of ordered? something like that?)
      params = if pl = blanks(TFO['parameter-list']).sort.first
                 RDF::List.new(subject: pl, graph: repo).to_a.uniq
               else
                 resources(TFO.parameter).sort
               end

      # now use the overloaded bulk assign
      # templates = params unless params.empty?

      #warn templates.inspect

      # do the templates
      params.each do |uri|
        if template = self[uri]
          template.refresh!
        else
          # this will trigger it
          self[uri] = uri
        end
      end

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

    # XXX need a solution for object properties, relative URIs, also
    # (compact) UUIDs.
    MAPPING = {
      nil                    => T::NormalizedString,
      RDF::RDFS.Literal      => T::String,
      RDF::RDFV.langString   => T::String,
      XSD.string             => T::String,
      XSD.token              => T::Token,
      XSD.boolean            => T::Bool,
      XSD.integer            => T::DecimalInteger,
      XSD.negativeInteger    => T::NegativeInteger,
      XSD.positiveInteger    => T::PositiveInteger,
      XSD.nonNegativeInteger => T::NonNegativeInteger,
      XSD.nonPositiveInteger => T::NonPositiveInteger,
      XSD.date               => T::Date,
      XSD.dateTime           => T::Time,
      RDF::RDFV.List         => T::List,
      RDF::RDFV.Bag          => T::Set,
      TFO.Range              => T::Range,
      TFO[:term]             => I::Term,  # XXX '#term' is an api method
    }

    # this is to actually parse the defaults out of the graph
    COMPOSITES = {
      RDF::RDFV.List => -> subject {
        RDF::List.new(subject: subject, graph: repo).to_a.map do |x|
          # convert the literals
          x.literal? ? x.object : x
        end
      },
      RDF::RDFV.Bag => -> subject {
        repo.objects_for(subject, RDF::RDFS.member).map do |x|
          # convert the literals
          x.literal? ? x.object : x
        end.to_set
      },
      TFO.Range => -> subject {
        # XXX we won't mess with open ranges and infimum/supremum right now
        lo = repo.objects_for(subject, TFO.low,  only: :literal).sort.first
        hi = repo.objects_for(subject, TFO.high, only: :literal).sort.first

        # convert the literals if not nil
        lo = lo.object if lo
        hi = hi.object if hi

        Range.new lo, hi
      },
    }

    # XXX we are doing this because apparently the type instances
    # don't compare, which is unbelievably fucking annoying
    ROOTS = [TFO.Range, RDF::RDFV.Bag, RDF::RDFV.List]

    # this could have been simple, but no
    UNWIND = {
      T::Range => -> value { value.minmax },
      T::Set   => -> value { value.to_a.sort },
      T::List  => -> value { value },
    }

    def load_composite subject
      # get the domains of the predicates from the struct
      types = (
        repo.types_for(subject) + repo.struct_for(subject).keys.select do |p|
          p.respond_to? :domain
        end.map { |p| p.domain }.flatten).uniq

      candidates = COMPOSITES.keys.reverse & types

      instance_exec subject, &COMPOSITES[candidates.first] unless
        candidates.empty?
    end

    def repo; registry.engine.repo; end

    # This post-init hook will refresh the template if it has no
    # configuration data.
    #
    def post_init
      refresh! if blank?
    end

    public

    alias_method :subject, :id

    # Refresh the template from the graph. Currently manages `slug`,
    # `aliases`, `type` (mapped from XSD), cardinality, `empty`, and
    # `shift`. Will also determine if the entity is composite.
    #
    # @note Still outstanding are `format`, `depends`, `conflicts`,
    #  `consumes` `preproc`, `universe`, `complement`, `unwind`,
    #  and `reverse`.
    #
    # @note This method does not configure the full feature set of
    #  {Params::Registry::Template}, because there is currently no
    #  determination on how the `universe` and `complement` members
    #  ought to be implemented; likewise stateful type coercions
    #  (i.e., coercions that may change when something within a
    #  running instance of Intertwingler changes).
    #
    # @return [self] because what else do you return
    #
    def refresh!
      # bail out because nothing here to work with otherwise
      return super unless subject.is_a? RDF::URI

      if slug = literals(CI['canonical-slug']).sort.first
        # i guess this is what we do? lol
        @slug = slug = slug.object.to_s.to_sym
      end

      @aliases = (
        literals(CI.slug).map { |a| a.object.to_s.to_sym } - [slug]).sort.uniq

      if type? TFO.Composite
        comp = resources(RDF::RDFS.range).sort.first || RDF::RDFV.Bag
        # root = ROOTS.detect(-> { RDF::RDFV.Bag }) { |c| repo.type_is? comp, c }

        @composite = MAPPING.fetch(comp, T::Set)
        @unwfunc = UNWIND[@composite]
        # warn @unwfunc.inspect
        @type = MAPPING.fetch(
          resources(TFO.element).sort.first, T::NormalizedString)
        # XXX COMPOSITE DEFAULT ???
        if d = resources(TFO.default).sort.first
          @default = load_composite d
        end
      else
        # XXX this may be subtler
        @type = MAPPING.fetch(
          resources(RDF::RDFS.range).sort.first, T::NormalizedString)

        # XXX is there some less stupid way of doing this
        @default = if d = literals(TFO.default).sort.first
                     d.object
                   end
      end

      # XXX deal with terms a non-stupid way: the problem is we need
      # state from the resolver ie the prefix mapping to deal
      if @type and @type == I::Term
        # we need to do surgery to preproc and format; note these are
        # instance_exec'd
        @preproc = -> x, _ {
          r = registry.engine.resolver
          x.map { |t| r.resolve_curie t, noop: true }
        }
        @format  = -> x { registry.engine.resolver.abbreviate x, noop: true }
      end

      # we need an unwind for trms

      # cardinality
      if c1 = numeric_literals(RDF::OWL.cardinality).sort.first
        @min = @max = c1.object.to_i
      else
        c1 = numeric_literals(RDF::OWL.minCardinality).sort.first
        c2 = numeric_literals(RDF::OWL.maxCardinality).sort.last
        @min = c1.object.to_i if c1
        @max = c2.object.to_i if c2
      end

      # empty/multi-value behaviour
      @empty = if em = literals(TFO.empty, datatype: XSD.boolean).sort.first
                 T::Bool[em.object]
               end
      @shift = if sh = literals(TFO.shift, datatype: XSD.boolean).sort.first
                 T::Bool[sh.object]
               end

      # we return self (well, `super` does) cause there's nothing here to see
      super
    end

  end

  def refresh!

    # do the templates
    super

    # do the groups
    groups.each { |g| g.refresh! cascade: false }

    self
  end

  def self.configure engine
    repo   = engine.repo
    props  = repo.property_set [TFO.parameter, TFO['parameter-list']]
    groups = props.map do |p|
      repo.query([nil, p, nil]).subjects
    end.flatten.select(&:iri?)

    me = self.new engine

    groups.each { |g| me.configure_group g }

    me
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

  def configure_group id, params = []
    # this is dumb because dumb
    unless group = self[id]
      self[id] = params
      group = self[id]
    end

    group.refresh!
  end
end
