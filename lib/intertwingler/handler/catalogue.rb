require 'intertwingler/vocab'
require 'intertwingler/handler'
require 'intertwingler/document'
require 'xml/mixup'
require 'rdf/rdfxml'

# This is a `GET` handler for what I'm calling "_catalogue_ resources"
# (sorry Americans). Its purpose is to tell us things like what's in
# the graph. Applications like the IBIS tool need a principled yet
# "dumb" way to access this kind of information in order to operate.
#
# The main goal of these catalogues is effectively to stand in for a
# handful of useful SPARQL queries (with inferencing) so a Web front
# end could construct a serviceable interactive user interface without
# having a lot of machinery written in JavaScript (much of which, like
# a _reasoner_, still does not exist at the time of this writing).
#
# Here are some specific questions the front end would want to ask the
# back end if it was setting up a user interface:
#
# * what classes are available to the application?
# * given a class (or classes), what properties have it (them) as a domain?
# * what properties have it (them) as a range?
# * given a property, what other resources in the graph…
#   * …are directly in its domain?
#   * …are in its domain by inference?
#   * …are directly in its range?
#   * …are in its range by inference?
#   * …are already subjects (directly and by inference)?
#   * …are already objects (directly and by inference)?
#
# The application's front end _should_ be able to answer these
# questions just by following links from wherever it currently is. It
# should not need to know any URLs in advance (except class and
# predicate identifiers, obviously, but it shouldn't need to know the
# URLs of any instance resources).
#
# Here is a sample chain of resources:
#
# ```turtle
# # how the actual entry point itself is done is negotiable
#
# ?wherever xhv:index ?idx .
#
# ?idx a cgto:Index ;
#   cgto:class-summary ?sc ;
#   cgto:property-summary ?sp .
#
# # (cgto:Summary rdfs:subClassOf qb:DataSet .)
#
# ?sc a cgto:Summary ;
#   qb:structure cgto:resources-by-class .
#
# # cgto:resources-by-class and cgto:resources-by-property are both
# # instances of qb:DataStructureDefinition
#
# # observations have datasets, not the other way around, so:
#
# ?cobs a qb:Observation ;
#   qb:dataSet ?sc ;
#   cgto:class ?class ;
#   cgto:asserted-instances ?ai ;
#   cgto:asserted-instances-count ?aic ;
#   cgto:inferred-instances ?ii ;
#   cgto:inferred-instances-count ?iic .
#
# # the predicate one will look something like this
#
# ?sp a cgto:Summary
#   qb:structure cgto:resources-by-property .
#
# # this is gonna be ugly cause there are 16 permutations of
# # asserted/inferred/subject/object/in-domain/in-range/resources/counts
#
# ?pobs a qb:Observation ;
#   qb:dataSet ?sp ;
#   cgto:property ?property ;
#   cgto:asserted-subjects ?as ;         # XXX
#   cgto:asserted-subject-count ?asc ;   # XXX
#   cgto:inferred-subjects ?is ;         # XXX
#   cgto:inferred-subject-count ?isc ;   # XXX
#   cgto:asserted-objects ?ao ;          # XXX
#   cgto:asserted-object-count ?aoc ;    # XXX
#   cgto:inferred-objects ?io ;          # XXX
#   cgto:inferred-object-count ?ioc ;    # XXX
#   cgto:asserted-domain ?ad ;
#   cgto:asserted-domain-count ?adc ;
#   cgto:inferred-domain ?id ;
#   cgto:inferred-domain-count ?idc ;
#   cgto:asserted-range ?ar ;
#   cgto:asserted-range-count ?arc ;
#   cgto:inferred-range ?ir ;
#   cgto:inferred-range-count ?irc .
#
# # ?as is asserted subjects; ?r1 is whatever resource being named
#
# ?as a cgto:Inventory ;
#   dct:hasPart ?r1 . # , ...
# ```
#
# > In the IBIS tool I kind of unthinkingly added the `subjects`/
# > `objects` construct, but on further reflection I'm wondering how
# > useful it is. For starters, I never actually used it myself.
# > Second, it's redundant, at least from the point of view of any
# > discovery because it's not useful to know that this or that
# > resource is the subject or object of this or that predicate
# > outside of the context of the node of the other side of the
# > predicate (which, if you had access to the node, is information
# > you would already have), _except_ perhaps that aggregates will
# > tell you something about the usage of those particular predicates.
#
class Intertwingler::Handler::Catalogue < Intertwingler::Handler

  private

  CGTO = Intertwingler::Vocab::CGTO
  QB   = Intertwingler::Vocab::QB
  XSD  = RDF::XSD

  # start with reverse mapping because it's easier to type
  URI_REV = {
    index:          'f4792b48-92d8-4dcb-ae8a-c17199601cb9',
    all_classes:    '4ab10425-d970-4280-8da2-7172822929ea',
    all_properties: '611ed2d0-1544-4e0b-a4db-de942e1193e2',
    inventory:      'bf4647be-7b02-4742-b482-567022a8c228',
    me:             'fe836b6d-11ef-48ef-9422-1747099b17ca',
    all_vocabs:     '13e45ee1-0b98-4d4b-9e74-a83a09e85030',
  }.transform_values { |v| RDF::URI("urn:uuid:#{v.freeze}") }
  # XXX "graduate" the URI_MAP construct from transforms to ordinary handlers?
  URI_MAP = URI_REV.invert

  TYPE_MAP = {
    index:          CGTO.Index,
    all_classes:    CGTO.Summary,
    all_properties: CGTO.Summary,
    inventory:      CGTO.Inventory,
    me:             nil, # dunno yet lol
  }

  # ditto parameter names, which shooould come to us from upstream as
  # compact uuids but we resolve them back to boring old symbols
  PARAM_REV = {
    instance_of:  '67cf8149-face-4781-8fbd-07aa17cd2c6c',
    in_domain_of: '61a04a58-045c-4747-b9aa-cceb95d0fcde',
    in_range_of:  '5d798f1f-a0da-4824-92dc-fe2155bf30e8',
    asserted:     '342ce7f9-9ebe-44cd-87b8-8be9e2e1a778',
    inferred:     'e52d9137-f1e1-4611-8ca3-01d9786b3dd6',
  }.transform_values { |v| RDF::URI("urn:uuid:#{v.freeze}") }
  PARAM_MAP = PARAM_REV.invert

  # lol, this is a bastard thing to do
  def my_uuid
    # this gets the name of the immediate caller
    sym = caller_locations(1, 1).first.base_label.to_sym
    # this gets the uuid urn from the map
    URI_REV.fetch sym # will raise a KeyError if missing
  end

  # XXX maybe make Intertwingler::Document a mixin? iunno

  def linkt target, **args
    Intertwingler::Document.link_tag resolver, target, **args
  end

  def litt label, **args
    Intertwingler::Document.literal_tag resolver, label, **args
  end

  # specific resources:

  # Returns a meta-catalogue (`cgto:Index` that links to summaries).
  # What I characterize as "almost static".
  #
  # @param base [URI] the user-facing subject URI
  # @param args [Hash] throwaway keyword arguments for parity with
  #  other methods
  #
  # @return [Array] `<body>` contents to {XML::Mixup}
  #
  def index base, **args
    # note the keys are method names not URL slugs
    {
      all_classes:    CGTO['by-class'],
      all_properties: CGTO['by-property'],
    }.map do |method, pred|
      uu   = URI_REV.fetch method # trigger a KeyError if missing
      uri  = resolver.uri_for uu, slugs: true, as: :uri
      src  = resolver.base.route_to uri
      # we could hard-code these i suppose but this affords changing them
      rel  = resolver.abbreviate pred
      type = resolver.abbreviate CGTO.Summary

      # XXX do we want alternate representations of this??
      { { [''] => :script, type: 'application/xhtml+xml',
         src: src, typeof: type } => :section, rel: rel }
    end
  end

  # XXX CAN WE USE THIS IN GRAPHOPS PERHAPS?

  VOCABS = [RDF::RDFV] + RDF::Vocabulary.to_a.drop(1)

  def self.generate_stack properties: false
    if properties
      mth = :property?
      eqv = :equivalentProperty
      sup = :subPropertyOf
    else
      mth = :class?
      eqv = :equivalentClass
      sup = :subClassOf
    end

    VOCABS.reduce({}) do |hash, vocab|
    vocab.each do |term|
      # no blank nodes or other clutter
      next unless term.uri? and term.respond_to? mth and term.send mth
      # check if an equivalent class has already been entered into the hash
      equivs = term.entail(eqv).to_set
      record = hash.values_at(*equivs).compact.first || [Set[], Set[]]
      if properties
        record.push Set[], Set[] if record.length == 2
        record[2].merge term.domain.select { |t| t.uri? }
        record[3].merge term.range.select  { |t| t.uri? }
      end
      # add ourselves
      equivs << term
      record[0].merge equivs
      equivs.each do |e|
        # get subclasses as no guarantee they were already got
        record[1].merge e.send(sup).select(&:uri?) if e.respond_to? sup
        # now link it up
        hash[e] ||= record
      end
    end
    hash
  end

  end

  PREFIXES = RDF::Vocabulary.vocab_map.map do |prefix, struct|
    [prefix, struct[:class] || RDF::Vocabulary.find(struct[:uri])]
  end.to_h

  # first we're gonna need to pull all the classes that we know about
  CLASSES    = generate_stack
  PROPERTIES = generate_stack properties: true
  DOMAINS    = {}
  RANGES     = {}

  # populate domains and ranges such that the keys are types
  PROPERTIES.each do |prop, record|
    { 2 => DOMAINS, 3 => RANGES }.each do |index, mapping|
      record[index].each { |type| (mapping[type] ||= Set[]) << prop }
    end
  end

  PROPLIST = (%w[asserted inferred].product %w[domain range]).map do |p|
    Intertwingler::Vocab::CGTO[p.join ?-]
  end

  def generic_set_for stack, term, include: false
    out   = Set[]
    queue = [term]

    while term = queue.shift
      pair = stack[term] or next
      # add the equivalents
      out |= pair.first
      # append supers to the queue before adding them to out
      queue += (pair.last - out).to_a
      # okay now add the supers
      out |= pair.last
    end

    out -= [term] unless include

    out
  end

  def class_set_for type, include: false
    generic_set_for CLASSES, type, include: include
  end

  def property_set_for prop, include: false
    generic_set_for PROPERTIES, prop, include: include
  end

  # all classes (cgto:Summary table)
  #
  # @param base [URI] the user-facing subject URI
  # @param asserted [true, false] whether to include asserted types
  # @param inferred [true, false] whether to include inferred types
  #
  # @return [Hash] an {XML::Mixup} representation of a table
  #
  def all_classes base, asserted: true, inferred: true
    # we also need what's going on in the inventory down there so we
    # can present the counts

    prefixes = PREFIXES.merge resolver.prefixes

    # log.debug "fart lol"

    # our product is something shaped like { type => [asserted, inferred] }
    # counts = CLASSES.keys.map { |k| [k, [0, 0]] }.to_h
    counts = {}
    abbrs  = {}

    # then we scan the whole graph for ?s a ?t statements and
    repo.query([nil, RDF.type, nil]).each do |stmt|
      type = stmt.object
      # only resources pls
      next unless type.uri?

      abbrs[type] ||= resolver.abbreviate(type, prefixes: prefixes)

      # there may not be one already
      (counts[type] ||= [0, 0])[0] += 1

      # now do inferred
      class_set_for(type).each do |inferred|
        # doyy
        abbrs[inferred] ||= resolver.abbreviate(inferred, prefixes: prefixes)

        (counts[inferred] ||= [0, 0])[1] += 1
      end
    end

    # log.debug abbrs.select {|_, v| v.nil? }.inspect

    obst = resolver.abbreviate(QB.Observation)

    # okay now we can sort it
    obs  = 0
    body = counts.sort do |a, b|
      # log.debug({ a.first => abbrs[a.first], b.first => abbrs[b.first]}).inspect
      abbrs[a.first] <=> abbrs[b.first]
    end.map do |type, record|
      row = "o.%d" % obs += 1
      abt = "##{row}"
      tt = resolver.abbreviate type.type, prefixes: prefixes if
        type.respond_to? :type
      asserted = litt(
        RDF::Literal(record.first, datatype: XSD.nonNegativeInteger),
        about: abt, property: CGTO['asserted-subject-count'])
      inferred = litt(
        RDF::Literal(record.last,  datatype: XSD.nonNegativeInteger),
        about: abt, property: CGTO['inferred-subject-count'])
      st = resolver.abbreviate CGTO.Index
      cols = [
        { linkt(type, typeof: tt, label: abbrs[type] || type) => :th },
        { linkt('', rel: CGTO['asserted-subjects'],
                typeof: st, label: asserted) => :td },
        { linkt('', rel: CGTO['inferred-subjects'],
                typeof: st, label: inferred) => :td },
      ]

      { cols => :tr, id: row, about: abt, typeof: obst }
    end

    { [
      { { [
        { ['Class'] => :th },
        { ['Asserted Subjects'] => :th },
        { ['Inferred Subjects'] => :th },
      ] => :tr } => :thead },
      { body => :tbody, rev: resolver.abbreviate(QB.dataSet) }
      ] => :table }
  end

  # all properties (cgto:Summary table)
  #
  # * all properties with domain T
  # * all properties with range T
  #
  def all_properties base, asserted: true, inferred: true

    prefixes = PREFIXES.merge resolver.prefixes

    # property => asserted (domain, range), inferred (domain, range)
    counts = {} # our product { p => [ad, ar, id, ir] }
    abbrs  = {} # properties to CURIEs

    repo.query([nil, RDF.type, nil]).each do |stmt|
      type = stmt.object
      next unless type.uri?

      # XXX there is probably a cheaper way to do this, loopwise

      # this one give us the initial asserted/inferred offsets
      [Set[type], class_set_for(type)].each_with_index do |types, i|
        # this index gives us the initial domain/range offsets
        [DOMAINS, RANGES].each_with_index do |mapping, j|
          # this will increment the correct slot
          slot = i << 1 | j
          types.each do |t|
            mapping.fetch(t, []).each do |prop|
              abbrs[prop] ||= resolver.abbreviate prop, prefixes: prefixes
              (counts[prop] ||= [0, 0, 0, 0])[slot] += 1
            end
          end
        end
      end

    end

    body = counts.sort do |a, b|
      abbrs[a.first] <=> abbrs[b.first]
    end.map do |prop, record|
      cols = [{ linkt(prop, label: abbrs[prop]) => :th }]

      PROPLIST.each_with_index do |prop, i|
        # we need the property and the count
        cp = RDF::URI(prop.to_s + '-count')
        cv = RDF::Literal(record[i], datatype: XSD.nonNegativeInteger)
        cols << { linkt('', rel: prop, property: cp, label: cv) => :td }
      end

      { cols => :tr }
    end

    { [
      { [
        { [
          { ['Property'] => :th, rowspan: 2 },
          { ['Asserted'] => :th, colspan: 2 },
          { ['Inferred'] => :th, colspan: 2 },
          ] => :tr },
        { [
          { ['In Domain'] => :th },
          { ['In Range']  => :th },
          { ['In Domain'] => :th },
          { ['In Range']  => :th }
          ] => :tr },
        ] => :thead },
      {
        body => :tbody,
        rev: resolver.abbreviate(QB.dataSet, prefixes: prefixes)
      } ] => :table }
  end

  # all of these should have asserted/inferred variants; could even do
  # a parameter for both so all four combinations (well, three of the
  # four since asserted=false&inferred=false would give you nothing)
  # are available.

  # Return an inventory of resources found in the graph. Three
  # parameters narrow the set returned:
  #
  # * `instance_of`: resources returned must be instances of these classes
  # * `in_domain_of`: resources returned must be instances of classes
  #   in the _domain_ of these properties
  # * `in_range_of`: resources returned must be instances of classes
  #   in the _range_ of these properties
  #
  # The set of classes produced by all three of these parameters is
  # unioned together. If the `inferred` flag is set, both the
  # properties and classes are expanded out to their equivalents and
  # subproperties/classes. If all three parameters (`instance_of`,
  # `in_domain_of`, `in_range_of`) are empty, this resource should
  # just disgorge the entire set of subjects in the graph.
  #
  # If `inferred` is true, then the equivalents and
  # subclasses/properties are tested as well. If `asserted` is false,
  # then asserted classes, as well as classes directly asserted in the
  # domains and ranges of properties, are _subtracted_ from the result
  # set. If both `asserted` and `inferred` are false, this will raise
  # a `409 Conflict`.
  #
  # The result set has the potential to be enormous, so we use the
  # `boundary` parameter to paginate it.
  #
  # The document body returned by this function contains a single
  # `<ol>` followed by a `<nav>` containing up to four pagination
  # links (first, previous, next, last). It represents a
  # `cgto:Inventory` and relates to its members via `dct:hasPart`.
  # The sequence of members is not enforced in the data, but rather is
  # derived from sorting {Intertwingler::GraphOps#label_for}.
  #
  # @param base [RDF::URI] the requested URI
  # @param instance_of [RDF::URI, Array<RDF::URI>] a set of
  #  constraining RDF classes
  # @param in_domain_of [RDF::URI, Array<RDF::URI>] a set of
  #  constraining properties by domain
  # @param in_range_of [RDF::URI, Array<RDF::URI>] a set of
  #  constraining properties by range
  # @param asserted [true, false] whether to include subjects whose
  #  directly asserted types match the constraints
  # @param inferred [false, true] whether to include subjects whose
  #  types are inferred from equivalents or subclasses/properties
  #
  # @raise [Intertwingler::Handler::Redirect] when e.g. the parameters
  #  are wrong but fixable internally
  # @raise [Intertwingler::Handler::Conflict] when e.g. the parameters
  #  are wrong but only fixable by the user
  #
  # @return [Array] the body to pass into {XML::Mixup}
  #
  def inventory base, instance_of: nil, in_domain_of: nil, in_range_of: nil,
      asserted: true, inferred: false, boundary: [1, 1000]
    # step zero: bail with a conflict error if both asserted and
    # inferred are false

    types = Set[*(instance_of || [])]


    # flatten out 
    if inferred
    elsif !asserted
      # neither asserted nor inferred: raise a 409
      raise Intertwingler::Handler::Conflict,
        'at least one of `asserted` or `inferred` must be true'
    end

    # resource, types, label(p, o)

    li = []

    # do instance of

    # first we need a list of types
    # then we need a list of ?s a ?t
    # then we sort the list
    # then we segment it

    [{ li => :ol, start: start }, { nav => :nav }]
  end


  # a _me_ resource that echoes back `REMOTE_USER` among other things
  def me base, **args
    # XXX we can do this later lol
  end

  def all_vocabs base, **args
    io = StringIO.new '', 'w+', encoding: Encoding::BINARY

    prefixes = resolver.prefixes.transform_values(&:to_uri)

    RDF::Writer.for(:rdf).new(io, base_uri: base, prefixes: prefixes) do |writer|
      resolver.prefixes.values.each do |vocab|
        repo.query({ graph_name: vocab.to_uri }).each do |stmt|
          # warn stmt.to_triple.inspect
          writer << stmt.to_triple
        end
      end
    end

    # require 'pry'
    # binding.pry

    Rack::Response[200, {
      'content-type'   => 'application/rdf+xml',
      'content-length' => io.size,
    }, io]
  end

  def xhtml_response uri, body
    ttag = 'hi lol'

    doc = XML::Mixup.xhtml_stub(
      base: uri, title: ttag, content: body
    ).document

    str = doc.to_xml.b

    Rack::Response[200, {
      'content-type'   => 'application/xhtml+xml',
      'content-length' => str.length.to_s,
    }, StringIO.new(str, ?r, encoding: Encoding::BINARY)]
  end

  #
  #
  #
  #
  # @
  #
  def dispatch subject, uri
    return unless method = URI_MAP[subject]

    body = send(method, URI(uri.to_s))

    case body
    when nil then nil
    when Hash, Array then xhtml_response uri, body
    when Rack::Response then body
    else nil
    end

  end

  public

  # General-purpose dispatch handler.
  #
  # @param req [Rack::Request] the request object
  #
  # @return [Rack::Response] a response
  #
  def handle req
    # complain unless this is a GET or HEAD

    # get the uri
    uri  = RDF::URI(req.url)
    # orig = uri.dup

    # clip off the query
    # query = uri.query
    uri.query = nil

    # uuid in here??
    subject = resolver.uuid_for uri, verify: false

    # get uuid or return 404
    return Rack::Response[404, {
      'content-type' => 'text/plain',
    }, ['no catalogue']] unless subject

    # run the dispatch or return 404
    resp = dispatch subject, uri

    return Rack::Response[404, {
      'content-type' => 'text/plain',
    }, ['no mapping']] unless resp

    resp
  end
end
