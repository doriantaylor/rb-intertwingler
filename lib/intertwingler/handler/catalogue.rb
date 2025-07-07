require 'intertwingler/vocab'
require 'intertwingler/handler'
require 'intertwingler/resource'
require 'intertwingler/document'
require 'intertwingler/representation/nokogiri'
require 'xml/mixup'
require 'rdf/rdfxml'
require 'http/negotiate'

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
  FOAF = RDF::Vocab::FOAF
  XSD  = RDF::XSD
  XHV  = RDF::Vocab::XHV

  public

  class Resource < Intertwingler::Resource
    # XXX maybe make Intertwingler::Document a mixin? iunno

    # all of these should have asserted/inferred variants; could even do
    # a parameter for both so all four combinations (well, three of the
    # four since asserted=false&inferred=false would give you nothing)
    # are available.

    private


    def linkt target, **args
      Intertwingler::Document.link_tag resolver, target, **args
    end

    def litt label, **args
      Intertwingler::Document.literal_tag resolver, label, **args
    end

    # Flatten a hash representation of a `Cache-Control` header to a string.
    def flatten_cc hdr

      hdr = hdr.map do |k, v|
        v.nil? ? k.to_s : "#{k}=#{v}"
      end.join(', ') if hdr.is_a? Hash

      hdr.to_s
    end

    def xhtml_response body,
        uri: nil, label: nil, prefixes: nil, typeof: nil, cache: nil
      uri      ||= resolver.uri_for subject, slugs: true
      label    ||= repo.label_for(subject, noop: true).reverse

      if prefixes
        # get at a prefix subset
        prefixes = resolver.prefixes.slice(*prefixes) if prefixes.is_a? Array
      else
        prefixes = resolver.prefixes
      end

      vocab ||= prefixes[nil] || resolver.prefixes[nil]

      label[1] = resolver.abbreviate label.last, prefixes: prefixes if
        label and label.is_a? Array and label.last.is_a? RDF::URI

      attr = {}
      if typeof
        typeof = resolver.abbreviate typeof, prefixes: prefixes if
          [Array, RDF::URI, URI].detect { |c| typeof.is_a? c }
        attr[:typeof] = typeof
      end

      doc = XML::Mixup.xhtml_stub(
        base: uri, title: label, content: body,
        prefix: prefixes, vocab: vocab, attr: attr
      ).document

      rep = Intertwingler::Representation::Nokogiri.new doc,
        type: 'application/xhtml+xml'

      hdr = {
        'content-type'   => rep.type,
        'content-length' => rep.size.to_s,
      }

      hdr['cache-control'] = flatten_cc(cache) if cache

      Rack::Response[200, hdr, rep]
    end

    # this is more or less copied from perl
    #
    # @note `params[key]` is assumed to be a {::Range}
    #
    # @param params [Params::Registry::Instance] the parsed parameters
    # @param key    [Object] the key to pick out of the parameters
    # @param items  [Integer] the total number of items to paginate
    # @param step   [Integer] the default step size
    #
    # @return [Hash{Symbol=>Params::Registry::Instance}] the relevant
    #  parameter set
    #
    def pagination_params params, key, items, step: 100
      min, max = params[key].minmax

      min ||= 1
      max ||= (min - 1) + step

      rows = max - (min - 1)
      # page = (min - 1) / rows # this is never used
      last = items / rows + 1
      ceil = last * rows

      # the return value
      out = {}

      if min > 1
        pmin = min - rows < 1 ? 1 : min - rows
        pmax = max - rows < rows ? rows : max - rows

        prev = out[:prev] = params.dup
        prev[key] = [pmin, pmax]

        if ceil > rows
          x = out[:first] = params.dup
          x[key] = [1, rows]
        end
      end

      if min - 1 + rows < ceil
        nmin = min + rows
        nmax = (min - 1) + (rows * 2)

        n = out[:next] = params.dup
        n[key] = [nmin, nmax]

        if ceil > rows
          last = out[:last] = params.dup
          last[key] = [(ceil - rows + 1), ceil]
        end
      end

      out
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

    # this is all the prefixes we know about
    PREFIXES = RDF::Vocabulary.vocab_map.map do |prefix, struct|
      [prefix, struct[:class] || RDF::Vocabulary.find(struct[:uri])] unless
        prefix == :rdfv # XXX HACK this fucking thing out
    end.compact.to_h

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

    PROPLIST = (%w[asserted inferred].product %w[subjects objects]).map do |p|
      Intertwingler::Vocab::CGTO[p.join ?-]
    end

    def abbr term, multi = false
      # XXX this will no doubt make bugs lol
      @prefixes ||= PREFIXES.merge resolver.prefixes
      resolver.abbreviate term,
        scalar: !multi, prefixes: @prefixes, cache: (@curies ||= {})
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

    # XXX yes i know there's (MIME) :type and (RDF) :typeof. sue me.
    def finalize body, uri: nil,
        prefixes: nil, typeof: nil, type: nil, cache: nil
      # XXX should we get this from the request??
      uri ||= resolver.uri_for subject, slugs: true

      case body
      when nil then nil
      when String
        ct = type || 'application/octet-stream'
        # engine.log.debug body
        hdr = { 'content-length' => body.b.length, 'content-type' => ct }
        hdr.merge!({ 'cache-control' => flatten_cc(cache) }) if cache
        Rack::Response[200, hdr, StringIO.new(body)]
      when Hash, Array
        xhtml_response body, uri: uri,
          prefixes: prefixes, typeof: typeof, cache: cache
      when Rack::Response then body
      else nil
      end
    end

    public

    def cacheable?
      true
    end

    def get uri, params: {}, headers: {}, user: nil, body: nil
      raise NotImplementedError, 'you should really implement this method, lol'
    end

  end

  # Returns a meta-catalogue (`cgto:Index` that links to summaries).
  # What I characterize as "almost static".
  #
  # @param base [URI] the user-facing subject URI
  # @param args [Hash] throwaway keyword arguments for parity with
  #  other methods
  #
  # @return [Array] `<body>` contents to {XML::Mixup}
  #
  class Index < Resource
    SUBJECT = RDF::URI('urn:uuid:f4792b48-92d8-4dcb-ae8a-c17199601cb9')

    private

    # XXX lol one day we will localize
    LABELS = {
      CGTO.user           => 'User',
      CGTO.state          => 'States',
      CGTO['by-class']    => 'By Class',
      CGTO['by-property'] => 'By Property',
    }
    # (actually hate to be that guy but could probably do l10n in a transform)

    def generate_jsonld uri, user

      preds = {
        '4ab10425-d970-4280-8da2-7172822929ea' => CGTO['by-class'],
        '611ed2d0-1544-4e0b-a4db-de942e1193e2' => CGTO['by-property'],
      }.map do |uu, pred|
        uu = RDF::URI("urn:uuid:#{uu}")
        href = resolver.uri_for uu, slugs: true, as: :uri, via: uri

        [abbr(pred),
         { "@id": uri.route_to(href), "@type": abbr(CGTO.Summary) }]
      end.to_h

      terms = Set[]

      if user
        # resolve the raw http user to something in the graph
        unless user.is_a? RDF::URI
          user = resolver.preproc user.to_s
          if user.include? ?@
            # XXX we are assuming there is no query string crap on
            # this email and we are making the executive decision to
            # downcase it here
            user = RDF::URI("mailto:#{user.downcase}")
          else
            # XXX change this to something less ad-hoc
            user = RDF::URI("urn:x-user-id:#{user}")
          end
        end
        agent = repo.subjects_for(FOAF.account, user).sort.first

        if agent
          at = repo.types_for agent
          au = resolver.uri_for agent, slugs: true, as: :uri, via: uri
          ap, ao = repo.label_for agent

          terms |= [at, ap, ao].flatten.compact

          # add cgto:user
          arec = { "@id": au.to_s, "@type": abbr(at, true)}
          if ap
            lrec = if ao.language?
                     { "@value": ao.value, "@language": ao.language }
                   elsif ao.datatype?
                     { "@value": ao.value, "@datatype": abbr(ao.datatype) }
                   else
                     ao.value
                   end
            arec[abbr(ap)] = lrec
          end

          preds[abbr(CGTO.user)] = arec

          # now do states

          states = repo.subjects_for(CGTO.owner, agent).sort.map do |st|
            u = resolver.uri_for st, slugs: true, as: :uri, via: uri
            { "@id": u.to_s, type: abbr(CGTO.State) }
          end
          preds[abbr(CGTO.state)] = states unless states.empty?
        end
      end

      pfx = resolver.prefixes.slice(
        :cgto, :rdf, :rdfs, :xsd, :foaf, :xhv, nil).merge(
      resolver.prefix_subset(terms)).map do |k, v|
        [(k.nil? ? :@vocab : k), v.to_s]
      end.sort { |a, b| a.first.to_s <=> b.first.to_s }.to_h

      types = (repo.types_for(subject) + [CGTO.Index]).uniq

      {
        "@context": {
          "@base": uri,
        }.merge(pfx),
        "@id": '',
        "@type": abbr(types, true),
      }.merge(preds)
    end

    # lol this one actually uses the user
    DISPATCH = {
      'application/ld+json' => -> uri, params, user {
        jsonld = generate_jsonld uri, user

        finalize(jsonld.to_json, type: 'application/ld+json',
                 cache: { "no-cache": nil })
      },
      'application/xhtml+xml' => -> uri, params, user {
        # user, state, by-class, by-property

        dd = []

        # XXX PERHAPS THIS WHOLE USER BUSINESS SHOULD BE PARCELED OUT??
        if user
          # resolve the user
          unless user.is_a? RDF::URI
            user = resolver.preproc user.to_s
            if user.include? ?@
              # XXX we are assuming there is no query string crap on
            # this email and we are making the executive decision to
              # downcase it here
              user = RDF::URI("mailto:#{user.downcase}")
            else
              # XXX change this to something less ad-hoc
              user = RDF::URI("urn:x-user-id:#{user}")
            end
          end

          # uu = resolver.uri_for user, slugs: true, as: :uri, via: uri
          # ut = repo.types_for user
          # up, uo = repo.label_for user, noop: true

          # dd << { '#dt' => LABELS[CGTO.user] }
          # dd << { '#dd' => linkt(uu, base: uri, rel: CGTO.user,
          #                        typeof: ut, property: up, label: uo) }

          # XXX don't get rid of this quite yet

          # this is the actual user we want
          agent = repo.subjects_for(FOAF.account, user).sort.first

          if agent
            at = repo.types_for agent
            au = resolver.uri_for agent, slugs: true, as: :uri, via: uri
            ap, ao = repo.label_for agent

            dd << { '#dt' => LABELS[CGTO.user] }
            dd << { '#dd' => linkt(au, base: uri, rel: CGTO.user,
                                   typeof: at, property: ap, label: ao) }

            # resolve the user's state object

            # ruh roh, didn't think about this: the user could conceivably
            # have multiple state objects because the same site (moreover
            # the same rdf store) could have multiple app instances

            states = repo.subjects_for(CGTO.owner, agent).sort
            unless states.empty?
              dd << { '#dt' => LABELS[CGTO.state] }
              states.each do |s|
                su = resolver.uri_for s, slugs: true, as: :uri, via: uri
                st = repo.types_for s

                # doubtful but whatever
                lp, lo = repo.label_for s, noop: true
                dd << { '#dd' => linkt(su, rel: CGTO.state, typeof: st,
                                       property: lp, label: lo )}
              end
            end
          end
        end

        out = []
        out << { dd => :dl } unless dd.empty?

        # link to the two summaries
        type = resolver.abbreviate CGTO.Summary

        # note the keys are method names not URL slugs
        out += {
          '4ab10425-d970-4280-8da2-7172822929ea' => CGTO['by-class'],
          '611ed2d0-1544-4e0b-a4db-de942e1193e2' => CGTO['by-property'],
        }.map do |uu, pred|
          uu = RDF::URI("urn:uuid:#{uu}")
          href = resolver.uri_for uu, slugs: true, as: :uri, via: uri
          src  = uri.route_to href
          # we could hard-code these i suppose but this affords changing them
          rel  = resolver.abbreviate pred
          # XXX do we want alternate representations of this??
          { { [''] => :script, type: 'application/xhtml+xml',
             src: src, typeof: type } => :section, rel: rel }
        end

        finalize out, uri: uri, prefixes: %i[cgto dct], typeof: CGTO.Index,
        cache: { "no-cache": nil }
      }
    }
    # make sure we cover our bases
    %w[text/html application/xml].each do |t|
      DISPATCH[t] = DISPATCH['application/xhtml+xml']
    end

    # meh i had something way more clever before
    VARIANTS = DISPATCH.map do |k, v|
      [k, { type: k }]
    end.to_h

    public

    def cacheable?
      false
    end


    def get uri, params: {}, headers: {}, user: nil, body: nil
      unless variant = HTTP::Negotiate.negotiate(headers, VARIANTS)
        return Rack::Response[
          406, { 'content-type' => 'text/plain' }, ['no variant chosen']]
      end

      instance_exec uri, params, user, &DISPATCH[variant]
    end
  end

  # all classes (cgto:Summary table)
  #
  # @param base [URI] the user-facing subject URI
  # @param asserted [true, false] whether to include asserted types
  # @param inferred [true, false] whether to include inferred types
  #
  # @return [Hash] an {XML::Mixup} representation of a table
  #
  class AllClasses < Resource
    SUBJECT = RDF::URI('urn:uuid:4ab10425-d970-4280-8da2-7172822929ea')

    private

    def generate_body uri, params, &block
      # we also need what's going on in the inventory down there so we
      # can present the counts

      prefixes = PREFIXES.merge resolver.prefixes

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

      href = resolver.uri_for(
        RDF::URI('urn:uuid:bf4647be-7b02-4742-b482-567022a8c228'),
        slugs: true, via: uri)

      # okay now we can sort it
      counts.sort do |a, b|
        # log.debug({ a.first => abbrs[a.first], b.first => abbrs[b.first]}).inspect
        abbrs[a.first] <=> abbrs[b.first]
      end.map do |type, record|
        # XXX we have decided to make the sequence number into stable
        # fragments so we can point back to them
        row = resolver.stable_fragment subject, type

        yp = {
          id:         row,
          about:      "##{row}",
          class:      type,
          asserted:   record.first,
          inferred:   record.last,
          # this is the class of the class, which will be either
          # rdfs:Class or owl:Class, or potentially something else we
          # haven't seen yet
          class_type: resolver.abbreviate(
            type.respond_to?(:type) ? type.type : RDF::RDFS.Class,
            prefixes: prefixes, scalar: false)
        }

        # asserted and inferred params
        ap = params.dup
        ap[:"instance-of"] = Set[type]
        ip = ap.dup
        ip[:inferred] = true

        yp[:a_params] = ap
        yp[:i_params] = ip
        yp[:a_href] = uri.route_to(ap.make_uri href).to_s
        yp[:i_href] = uri.route_to(ip.make_uri href).to_s

        instance_exec yp, &block
      end
    end

    # decided to do json-ld variants by hand because the (x)html is
    # way too slow to generate after a POST. this is just a warmup
    # btw; the real gnarly one is `all-resources`.
    DISPATCH = {
      'application/ld+json' => -> uri, params {
        prefixes = PREFIXES.merge resolver.prefixes

        obst = resolver.abbreviate QB.Observation, prefixes: prefixes
        invt = resolver.abbreviate CGTO.Inventory, prefixes: prefixes
        clsp = resolver.abbreviate CGTO.class, prefixes: prefixes
        nint = resolver.abbreviate XSD.nonNegativeInteger, prefixes: prefixes

        preds = %w[asserted inferred].product(%w[s -count]).reduce({}) do |h, x|
          slug = '%s-subject%s' % x
          z = CGTO[slug]
          (h[x.first.to_sym] ||= []) <<
            resolver.abbreviate(z, prefixes: prefixes)
          h
        end

        # collect the prefixes from the abbreviated terms
        pfx = Set[:cgto, :qb, :xsd]

        body = generate_body uri, params do |yp|
          yp[:class_type].each do |t|
            p = /^([^:]+):/.match(t)&.captures&.first&.to_sym
            pfx << p if p
          end

          o = {
            "@id": yp[:about],
            "@type": obst,
            clsp => { "@id": yp[:class].to_s, "@type": yp[:class_type] },
            preds[:asserted].last  => {
              "@value": yp[:asserted], "@datatype": nint },
            preds[:inferred].last  => {
              "@value": yp[:inferred], "@datatype": nint },
          }
          o[preds[:asserted].first] = { "@id": yp[:a_href], "@type": invt } if
            yp[:asserted] > 0
          o[preds[:inferred].first] = { "@id": yp[:i_href], "@type": invt } if
            yp[:inferred] > 0
          o # rly?
        end

        # XXX uhh yo do we wanna do other backlinks?
        index = resolver.uri_for(
          RDF::URI('urn:uuid:f4792b48-92d8-4dcb-ae8a-c17199601cb9'),
          slugs: true, via: uri)
        bcls = resolver.abbreviate CGTO['by-class'], prefixes: prefixes
        qbs  = resolver.abbreviate QB.structure, prefixes: prefixes

        out = {
          "@context": {
            "@version": 1.1,
            "@base": uri,
          }.merge(prefixes.slice(*pfx.to_a.sort).transform_values(&:to_s)),
          "@id": '',
          "@type": resolver.abbreviate(CGTO.Summary, prefixes: prefixes),
          qbs => {
            "@id": abbr(CGTO['resources-by-class']),
            "@type": abbr(QB.DataStructureDefinition),
          },
          "@reverse": {
            "qb:dataSet": body,
            bcls => {
              "@id": uri.route_to(index).to_s,
              "@type": abbr(CGTO.Index),
            }
          }
        }

        finalize(out.to_json, type: 'application/ld+json',
                 cache: { public: nil, "max-age": 5 })
      },
      'application/xhtml+xml' => -> uri, params {
        prefixes = PREFIXES.merge resolver.prefixes

        obst = resolver.abbreviate QB.Observation, prefixes: prefixes
        st   = resolver.abbreviate CGTO.Inventory, prefixes: prefixes

        abbrs = {}

        body = generate_body uri, params do |yp|
          type = abbrs[yp[:class]] ||=
            resolver.abbreviate(yp[:class], prefixes: prefixes) || yp[:class]
          asserted = litt(
            RDF::Literal(yp[:asserted], datatype: XSD.nonNegativeInteger),
            about: yp[:about], property: CGTO['asserted-subject-count'])
          inferred = litt(
            RDF::Literal(yp[:inferred],  datatype: XSD.nonNegativeInteger),
            about: yp[:about], property: CGTO['inferred-subject-count'])
          cols = [ "\n",
            { linkt(yp[:class], rel: CGTO.class, typeof: yp[:class_type],
                    label: type) => :th }, "\n",
            { linkt(yp[:a_href], rel: CGTO['asserted-subjects'],
                    typeof: st, label: asserted) => :td }, "\n",
            { linkt(yp[:i_href], rel: CGTO['inferred-subjects'],
                    typeof: st, label: inferred) => :td }, "\n",
          ].map { |c| [c, "\n"] }.flatten(1)

          [ { cols => :tr, id: yp[:id], about: yp[:about], typeof: obst }, "\n"]
        end

        cache = { public: nil, "max-age": 5 }

        finalize({ [ "\n",
          { { [ "\n",
            { ['Class'] => :th }, "\n",
            { ['Asserted Subjects'] => :th }, "\n",
            { ['Inferred Subjects'] => :th }, "\n",
          ] => :tr } => :thead }, "\n",
          { body => :tbody, rev: resolver.abbreviate(QB.dataSet) }, "\n",
        ] => :table }, uri: uri, prefixes: prefixes,
                 typeof: CGTO.Summary, cache: cache)
      }
    }
    # make sure we cover our bases
    %w[text/html application/xml].each do |t|
      DISPATCH[t] = DISPATCH['application/xhtml+xml']
    end

    # meh i had something way more clever before
    VARIANTS = DISPATCH.map do |k, v|
      [k, { type: k }]
    end.to_h

    public

    def get uri, params: {}, headers: {}, user: nil, body: nil
      unless variant = HTTP::Negotiate.negotiate(headers, VARIANTS)
        return Rack::Response[
          406, { 'content-type' => 'text/plain' }, ['no variant chosen']]
      end

      instance_exec uri, params, &DISPATCH[variant]
    end
  end

  # all properties (cgto:Summary table)
  #
  # * all properties with domain T
  # * all properties with range T
  #
  class AllProperties < Resource
    SUBJECT = RDF::URI('urn:uuid:611ed2d0-1544-4e0b-a4db-de942e1193e2')

    private

    def generate_body uri, params, &block
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

      # XXX we should figure out a way to resolve these if the subject
      # UUIDs get overridden
      href = resolver.uri_for(
        RDF::URI('urn:uuid:bf4647be-7b02-4742-b482-567022a8c228'),
        slugs: true, via: uri)

      counts.sort do |a, b|
        abbrs[a.first] <=> abbrs[b.first]
      end.map do |prop, record|
        row = resolver.stable_fragment subject, prop

        # generate all the row-specific data
        yp = {
          id: row,
          about: "##{row}",
          prop: prop,
          prop_type: resolver.abbreviate(
            prop.respond_to?(:type) ? prop.type : RDF::RDFV.Property,
            prefixes: prefixes, scalar: false),
        }.merge(%i[asserted inferred].product(
          %i[domain range]).reduce({}) do |h, pair|
                  # lol this is crazy person shit

                  # param/link stuff
                  x = params.dup
                  x["in-#{pair.last}-of".to_sym] = Set[prop]
                  x[:inferred] = (pair.first == :inferred)
                  u = uri.route_to(x.make_uri href)

                  # now we do the counts
                  z = { asserted: :a_href, inferred: :i_href }
                  i = z.keys.index pair.first
                  j = %i[domain range].index pair.last

                  # warn "#{i} #{j} #{record}"

                  # this is our product
                  (h[pair.first] ||= [])[j]    = record[i << 1 | j]
                  (h[z[pair.first]] ||= [])[j] = u

                  h # return accumulator
                end)

        instance_exec yp, &block
      end
    end

    DISPATCH = {
      'application/ld+json' => -> uri, params {

        prefixes = PREFIXES.merge resolver.prefixes

        obst = resolver.abbreviate QB.Observation, prefixes: prefixes
        invt = resolver.abbreviate CGTO.Inventory, prefixes: prefixes
        prop = resolver.abbreviate CGTO.property, prefixes: prefixes
        nint = resolver.abbreviate XSD.nonNegativeInteger, prefixes: prefixes

        abbrs = {}

        # collect the prefixes from the abbreviated terms
        pfx = Set[:cgto, :qb, :xsd]

        body = generate_body uri, params do |yp|
          yp[:prop_type].each do |t|
            p = /^([^:]+):/.match(t)&.captures&.first&.to_sym
            pfx << p if p
          end

          o = {
            "@id": yp[:about],
            "@type": obst,
            prop => { "@id": yp[:prop].to_s, "@type": yp[:prop_type] },
          }
          # add to it
          %i[asserted inferred].product([0, 1]).each do |slug, i|
            # property base, numeric property
            pb = '%s-%s' % [slug, %w[subject object][i]]
            np = abbrs[CGTO["#{pb}-count"]] ||=
              resolver.abbreviate(CGTO["#{pb}-count"], prefixes: prefixes)

            # numeric value
            nv = yp[slug][i]
            o[np] = { "@value": nv, "@datatype": nint }

            if nv > 0
              # href property, href value
              hp = abbrs[CGTO["#{pb}s"]] ||=
                resolver.abbreviate(CGTO["#{pb}s"], prefixes: prefixes)
              hv = yp[{ asserted: :a_href, inferred: :i_href }[slug]][i]
              o[hp] = { "@id": hv, "@type": invt }
            end
          end

          o # the observation
        end

        # XXX uhh yo do we wanna do other backlinks?
        index = resolver.uri_for(
          RDF::URI('urn:uuid:f4792b48-92d8-4dcb-ae8a-c17199601cb9'),
          slugs: true, via: uri)
        bprp = resolver.abbreviate CGTO['by-property'], prefixes: prefixes
        qbd  = resolver.abbreviate QB.dataSet, prefixes: prefixes
        qbs  = resolver.abbreviate QB.structure, prefixes: prefixes

        out = {
          "@context": {
            "@version": 1.1,
            "@base": uri,
          }.merge(prefixes.slice(*pfx.to_a.sort).transform_values(&:to_s)),
          "@id": '',
          "@type": resolver.abbreviate(CGTO.Summary, prefixes: prefixes),
          qbs => resolver.abbreviate(
            CGTO['resources-by-property'], prefixes: prefixes),
          "@reverse": {
            qbd => body,
            bprp => uri.route_to(index).to_s,
          }
        }

        finalize(out.to_json, type: 'application/ld+json',
                 cache: { public: nil, "max-age": 5 })
      },
      'application/xhtml+xml' => -> uri, params {
        prefixes = PREFIXES.merge resolver.prefixes

        obst = resolver.abbreviate QB.Observation, prefixes: prefixes
        invt = resolver.abbreviate CGTO.Inventory, prefixes: prefixes
        prop = resolver.abbreviate CGTO.property,  prefixes: prefixes

        # collect curies
        abbrs = {}
        # collect the prefixes from the abbreviated terms
        pfx = Set[:cgto, :qb, :xsd]

        # why didn't we just translate this to the same thing? i have no idea

        body = generate_body uri, params do |yp|
          yp[:prop_type].each do |t|
            p = /^([^:]+):/.match(t)&.captures&.first&.to_sym
            pfx << p if p
          end

          pabbr = abbrs[yp[:prop]] ||=
            resolver.abbreviate yp[:prop], prefixes: prefixes

          cols = [{ '#th' =>
                   linkt(yp[:prop], typeof: yp[:prop_type], rel: prop,
                         label: pabbr, prefixes: prefixes)}]

          # warn yp.inspect

          PROPLIST.each_with_index do |hp, i|
            # we need the property and the count
            cp = RDF::URI(hp.to_s.chop + '-count')

            # mask and shift down
            j = i & 3 >> 1

            c = yp[%i[asserted inferred][j]][i & 1]
            h = yp[%i[a_href     i_href][j]][i & 1]

            args = { prefixes: prefixes, property: cp }
            (c == 0) ? args[:name] = 'td' : args[:about] = yp[:about]

            cv = litt(RDF::Literal(c, datatype: XSD.nonNegativeInteger), **args)

            cols << ( (c > 0) ? {
              '#td' => linkt(h, rel: hp, label: cv, typeof: invt) } : cv)
          end

          { cols => :tr, id: yp[:id], about: yp[:about], typeof: obst }
        end

        cache = { public: nil, "max-age": 5 }

        finalize({ [
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
          } ] => :table }, uri: uri, prefixes: prefixes,
                 typeof: CGTO.Summary, cache: cache)
      },
    }
    # make sure we cover our bases
    %w[text/html application/xml].each do |t|
      DISPATCH[t] = DISPATCH['application/xhtml+xml']
    end

    # meh i had something way more clever before
    VARIANTS = DISPATCH.map do |k, v|
      [k, { type: k }]
    end.to_h

    public

    def get uri, params: {}, headers: {}, user: nil, body: nil
      unless variant = HTTP::Negotiate.negotiate(headers, VARIANTS)
        return Rack::Response[
          406, { 'content-type' => 'text/plain' }, ['no variant chosen']]
      end

      instance_exec uri, params, &DISPATCH[variant]
    end

  end

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
  #  do not match when round-tripped or otherwise require adjustment
  # @raise [Intertwingler::Handler::Conflict] when e.g. the parameters
  #  are wrong but only fixable by the user
  #
  # @return [Array] the body to pass into {XML::Mixup}
  #
  class Inventory < Resource
    SUBJECT = RDF::URI('urn:uuid:bf4647be-7b02-4742-b482-567022a8c228')

    private

    def expand resource
      x = { RDF.type => repo.types_for(resource) }
      lp, lo = repo.label_for resource
      x[lp] = [lo] if lp
      x
    end

    # XXX we want to just do the work of pulling and sorting these
    # only once per GET on a given graph state. need a structure
    # that's like: { host => parameter set => [mtime, content] } .
    # content is something like [subject, types, label pred, label].
    # we won't screw around with language (at this time). if the
    # modification time is stale, we nuke and replace. otherwise we
    # just take a slice for the window.
    #
    # note that the caching here is purely internal; from the outside
    # you'll just see a 304. this is for the entire sequence which
    # (potentially unwisely) is held in ram
    #
    def full_list params

      # do the terms first because we need them
      terms = %i[instance-of in-domain-of in-range-of].map do |k|
        [k, (params[k] || []).map do |t|
          resolver.resolve_curie t, noop: true
        end.to_set]
      end.to_h

      # i'm not sure if the host is even necessary because every engine
      # has its own handler stack; oh well whatever we'll leave it in
      # scache = @subjects ||= {}
      qcache = (@qcache ||= {})[resolver.base] ||= {}

      # so we actually just make the cache key the actual thing
      key = terms.values +
        [params[:asserted] || true, params[:inferred] || false]

      entry = qcache[key]

      # fetch the store mtime if it's available
      cm = (entry || []).first
      rm = begin
             # arrrgh RDF::Graph
             rp = repo.respond_to?(:data) ? repo.data : rp
             rp.mtime if rp.respond_to? :mtime
           end

      engine.log.debug "cache mtime: #{cm.inspect} repo mtime: #{rm.inspect}"

      # if the store mtime is nil or newer than the cache (or the
      # mtime of the cache itself is nil), retrieve the records, sort,
      # and cache them
      unless rm && cm && rm <= cm
        # okay now we bring this in line with the original code
        terms.transform_values! { |t| [t, Set[]] }

        alts = [0]
        alts << 1 if params[:inferred]

        # step 1: map domain and range properties to asserted types
        { "in-domain-of": :domain, "in-range-of": :range}.each do |k, m|
          # add to inferred
          terms[k][1] |= repo.property_set terms[k][0] if params[:inferred]

          # ohh this adds instances of the domain/range, duh
          alts.each do |v|
            terms[:"instance-of"][v] |= terms[k][v].map do |t|
            t.respond_to?(m) ? t.send(m) : nil
            end.flatten.compact.select(&:iri?)
          end
        end

        # *now* this adds to the inferencing to the types (if requested)
        terms[:"instance-of"][1] |=
          repo.type_strata(terms[:"instance-of"][0], descend: true) if
          params[:inferred]

        # collect the resources into a set for now
        resources = {}

        if terms.values.flatten.reduce(&:|).empty?
          # just fetch everything
          %i[subjects objects].each do |m|
            repo.send(m).select(&:iri?).each do |r|
              resources[r] ||= expand r
            end
          end
        else
          # there is something in at least one of these, so we filter

          # let's get all the types that are actually in the graph
          all = repo.all_types

          alts.each do |v|
            # do the properties
            { "in-domain-of": :subjects, "in-range-of": :objects }.each do |k, m|
              # XXX this is wrong; actually what this should do is
              # something like p.domain
              terms[k][v].each do |p|
                repo.query([nil, p, nil]).send(m).select(&:iri?).each do |r|
                  resources[r] ||= expand r
                end
              end
            end

            #
            (terms[:"instance-of"][v] & all).each do |t|
              repo.query([nil, RDF.type, t]).subjects.select(&:iri?).each do |r|
                resources[r] ||= expand r
              end
            end
          end
        end

        # but we want to use it as a cache for this comparator
        lcmp = repo.cmp_label cache: resources, nocase: true, &:first

        # so now we sort and do the final transformation
        resources = resources.sort(&lcmp).to_h.transform_values do |v|
          [v[RDF.type]] + (v.except(RDF.type).first || []).flatten
        end

        # and now we store
        entry = [rm, resources]
        qcache[key] = entry if rm
      end

      entry.last
    end

    def generate_body uri, params, &block
      # do the boundary
      boundary = Range.new(*params[:boundary].minmax.map { |x| x - 1 })

      # duh this is a hash
      list = full_list params
      len  = list.length

      # engine.log.debug "list length: #{len}, slice: #{boundary}"

      # aduh try to_a first before slicing
      list.to_a.slice(boundary).map do |s, rest|
        # engine.log.debug(rest)
        types, lp, lo = rest

        su = (@uricache ||= {})[s] ||=
          uri.route_to(resolver.uri_for s, slugs: true, via: uri)

        yp = { about: su, length: len }
        yp[:type] = types if types && !types.empty?
        if lp
          yp[:lprop] = lp
          yp[:label] = lo
        end

        instance_exec yp, &block
      end
    end

    DISPATCH = {
      'application/ld+json' => -> uri, params {
        prefixes = PREFIXES.merge resolver.prefixes

        terms = Set[]

        # get the body
        length = nil
        body = generate_body uri, params do |yp|
          # may as well smuggle out the total length now
          length ||= yp[:length]

          # add these to terms
          terms |= yp.values_at(:type, :lprop, :label).flatten.compact

          o = {
            "@id": yp[:about],
            "@type": abbr(yp[:type], true),
          }

          if yp[:lprop] and yp[:label] and yp[:label].literal?
            v, ln, dt = %i[value language datatype].map do |m|
              yp[:label].send m
            end

            o[abbr(yp[:lprop])] = if ln
                                  { "@value": v, "@language": ln }
                                elsif dt
                                  { "@value": v, "@datatype": abbr(dt) }
                                else
                                  v
                                end
          end

          o # mah gerd
        end

        # okay now we wrap the body in the inventory
        np = params.dup
        np[:boundary] = nil
        inv = uri.route_to(np.make_uri uri)

        inventory = {
          "@id": inv.to_s,
          "@type": abbr(CGTO.Inventory),
          abbr(CGTO.asserted) => params[:asserted],
          abbr(CGTO.inferred) => params[:inferred],
        }
        %i[instance-of in-domain-of in-range-of].each do |slug|
          inventory[abbr(CGTO[slug])] = params[slug].to_a.sort.map do |c|
            { "@id": abbr(c) }
          end unless params[slug].empty?
        end
        inventory[abbr(RDF::RDFS.member)] = body

        pfx = prefixes.slice(:cgto, :rdf, :rdfs, :xhv, nil).merge(
          resolver.prefix_subset(terms)).map do |k, v|
          [(k.nil? ? :@vocab : k), v.to_s]
        end.sort { |a, b| a.first.to_s <=> b.first.to_s }.to_h

        # and finally we wrap the inventory in the window
        window = {
          "@context": {
            "@version": 1.1,
            "@base": uri,
          }.merge(pfx),
          "@id": '',
          "@type": abbr(CGTO.Window),
          abbr(CGTO['window-of']) => inventory,
        }
        # do the paginations
        pp = pagination_params params, :boundary, length
        pp.each do |k, v|
          ku = uri.route_to(v.make_uri uri, defaults: :boundary)
          window[abbr(XHV[k])] = { "@id": ku.to_s, "@type": abbr(CGTO.Window) }
        end

        finalize(window.to_json, type: 'application/ld+json',
                 cache: { public: nil, "max-age": 5 })
      },
      'application/xhtml+xml' => -> uri, params {
        prefixes = PREFIXES.merge resolver.prefixes

        mem = abbr(RDF::RDFS.member)

        terms = Set[]

        length = nil
        li = generate_body uri, params do |yp|
          # smuggle out the total length
          length ||= yp[:length]

          terms |= yp.values_at(:type, :lprop, :label).flatten.compact

          # href = uri.route_to yp[:about]
          href = yp[:about]

          # lol watch out, `types` is a valid mixed-in method
          types = abbr(yp[:type], true)

          lp, lo = yp.values_at :lprop, :label

          a = linkt href, rel: mem,
          typeof: types, label: lp ? litt(lo, property: lp) : lo

          { '#li' => a }
        end

        pp  = pagination_params params, :boundary, length

        # this is trash but we're using a dynamic language sooo
        abt = params.dup
        abt[:boundary] = nil
        abt = abt.make_uri uri

        nav = { first: 'First', prev: 'Previous',
               next: 'Next', last: 'Last'}.map do |k, label|
          if pp[k]
            ku = pp[k].make_uri uri, defaults: :boundary
            { '#li' => linkt(uri.route_to(ku), rel: XHV[k], label: label) }
          else
            { '#li' => label }
          end
        end

        pfx = prefixes.slice(
          :cgto, :rdf, :rdfs, :xhv).merge resolver.prefix_subset(terms)

        # XXX don't forget backlinks

        finalize [{
          li => :ol, start: params[:boundary].begin + 1, resource: abt,
          rel: abbr(CGTO['window-of']),
          rev: abbr(CGTO[:window]),
          typeof: abbr(CGTO.Inventory),
        }, { '#nav' => { '#ul' =>  nav } }],
        uri: params.make_uri(uri, defaults: :boundary),
        prefixes: pfx, typeof: CGTO.Window
      },
    }
    # make sure we cover our bases
    %w[text/html application/xml].each do |t|
      DISPATCH[t] = DISPATCH['application/xhtml+xml']
    end

    # meh i had something way more clever before
    VARIANTS = DISPATCH.map do |k, v|
      [k, { type: k }]
    end.to_h

    public

    def get uri, params: {}, headers: {}, user: nil, body: nil
      # The job of this thing is to list individual resources, in a
      # consistent order, with "best" label if applicable. Passing no
      # (rather, default) parameters will yield a paginated list of
      # all subjects in the graph. Results can be filtered first with
      # an `instance-of` parameter which represents a set of classes,
      # and `in-domain-of`/`in-range-of` parameters, which represent
      # sets of properties. Each of these sets must be non-empty if it
      # is to be included in the filtering, however the results
      # returned will correspond to the union of all sets.
      #
      # Results can further be adjusted by using the boolean
      # parameters `asserted` and `inferred`, although it is a 409
      # Conflict error if both of these are false.
      #
      # ###

      # warn "wat lol #{uri.inspect}"

      # step zero: bail with a conflict error if both asserted and
      # inferred are false
      raise Intertwingler::Handler::Error::Conflict,
        'At least one of asserted or inferred parameters must be true' unless
        params[:asserted] or params[:inferred]

      # engine.log.debug "#{uri} #{params.to_s}"

      # step 0.25: redirect
      base = params.make_uri uri, defaults: :boundary
      # engine.log.debug "#{base} <=> #{uri}"
      raise Intertwingler::Handler::Redirect.new(
        'Redirecting to window', status: 308, location: base) if
        base.to_s != uri.to_s

      unless variant = HTTP::Negotiate.negotiate(headers, VARIANTS)
        return Rack::Response[
          406, { 'content-type' => 'text/plain' }, ['no variant chosen']]
      end

      instance_exec uri, params, &DISPATCH[variant]
    end
  end

  # this is kind of a user state record which we still have to figure out
  class Me < Resource
    SUBJECT = RDF::URI('urn:uuid:fe836b6d-11ef-48ef-9422-1747099b17ca')

    def get uri, params: {}, headers: {}, user: nil, body: nil
    end
  end

  class AllVocabs < Resource
    SUBJECT = RDF::URI('urn:uuid:13e45ee1-0b98-4d4b-9e74-a83a09e85030')

    def get uri, params: {}, headers: {}, user: nil, body: nil
      io = StringIO.new '', 'w+', encoding: Encoding::BINARY
      prefixes = resolver.prefixes.transform_values(&:to_uri)

      # huh
      base = resolver.coerce_resource uri

      RDF::Writer.for(:rdf).new(io, base_uri: base, prefixes: prefixes) do |writer|
        resolver.prefixes.values.each do |vocab|

          engine.log.debug "serializing #{vocab}"

          vocab.each_statement do |stmt|

          # repo.query({ graph_name: vocab.to_uri }).each do |stmt|
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

  end

  private

  MANIFEST = [Index, AllClasses, AllProperties,
              Inventory, Me, AllVocabs].map { |c| [c.subject, c] }.to_h

  def get_headers req
    req.env.select do |k|
      %w[CONTENT_TYPE CONTENT_LENGTH].include? k or k.start_with? 'HTTP_'
    end.transform_keys { |k| k.delete_prefix('HTTP_').downcase.tr(?_, ?-) }
  end

  public

  def initialize engine, **args
    # do whatever the superclass does
    super engine, **args

    # add the manifest
    @manifest = MANIFEST.transform_values { |v| v.new self }
  end

  attr_reader :manifest

  # General-purpose dispatch handler.
  #
  # @param req [Rack::Request] the request object
  #
  # @return [Rack::Response] a response
  #
  def handle req
    # complain unless this is a GET or HEAD

    # get the uri
    uri  = URI(req.url)
    orig = uri.dup

    # clip off the query
    query = uri.query || ''
    uri.query = nil

    # uuid in here??
    subject  = resolver.uuid_for uri, verify: false
    resource = @manifest[subject] if subject

    # get uuid or return 404
    return Rack::Response[404, {
      'content-type' => 'text/plain',
    }, ['no catalogue']] unless subject and resource

    # okay NOW check if it's cacheable
    if resource.cacheable? and repo.respond_to?(:mtime) and
        ims = req.get_header('HTTP_IF_MODIFIED_SINCE')
      ims = (Time.httpdate(ims) rescue Time.at(0)).utc
      lm  = repo.mtime
      return Rack::Response[304, {}, []] if lm.to_i <= ims.to_i
    end

    # stupid rack doesn't have this field
    user = req.env['REMOTE_USER']

    log.debug "found user #{user}" if user

    # XXX this may raise an Intertwingler::Handler::AnyButSuccess
    begin
      resp = resource.call req.request_method, orig, params: query,
        headers: normalize_headers(req), user: user, body: req.body
    rescue Intertwingler::Handler::AnyButSuccess => e
      return e.response
    end

    # resp can be nil so 404 if it is
    return Rack::Response[404, {
      'content-type' => 'text/plain',
    }, ['no mapping']] unless resp

    # add last-modified header to the response now that we know we have one
    resp['last-modified'] ||= repo.mtime.httpdate if repo.respond_to? :mtime

    resp
  end
end
