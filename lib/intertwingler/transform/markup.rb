require 'intertwingler/transform'

# most of the actual substantive code lives in this
require 'intertwingler/document'

# the representation
require 'intertwingler/representation/nokogiri'

# these are also handy
require 'xml-mixup'
require 'md-noko'
require 'tidy_ffi'
require 'rdf/rdfa'

# This class will probably be the template for
# {Intertwingler::Transform} going forward.
# {Intertwingler::Transform::Harness} is probably still useful for
# parts, as is the stuff for resolving transforms and parameters and
# partials and such.
#
# It took an extremely long time to settle on a design for the
# transforms. Originally I was imagining one class per transform, but
# hadn't completely worked out how they were going to be invoked.
# Around early July 2023 is when I decided to make everything an
# {Intertwingler::Handler} which interfaces exclusively by URL, and it
# made more sense to bundle transformation functions thematically,
# because they end up dealing with the same media types and therefore
# relying on the same dependencies. More to the point, the individual
# transforms are tiny pieces of code and there are a lot of them, so
# having a separate class for each is unnecessary clutter.
#
# As mentioned elsewhere, a transform responds exclusively to `POST`
# requests, and has a limited range of media types that it accepts and
# emits. Additional parameters are passed in via the URI query string.
class Intertwingler::Transform::Markup < Intertwingler::Transform::Handler
  private

  # XXX do we actually need this rather than a `representation` method?
  REPRESENTATION = Intertwingler::Representation::Nokogiri

  # This map routes URIs (UUIDs as single path segment) to internal
  # methods. We use UUIDs to decouple the identifiers that may show up
  # in configuration or path parameters from what we call the functions
  # internally. This structure also contains the accept and return types,
  # i.e., matching what comes in on the request's `Content-Type` header,
  # and offering results according to the request's `Accept` header,
  # respectively.

  URI_MAP = {
    '8307ac09-670b-48b9-b08d-3eacc1f51f43' => [:parse_markdown,
      %w[text/markdown], %w[application/xhtml+xml text/html] ],
    'ca069a8a-dd73-423d-b4c2-77777c049f36' => [:tidy,
      %w[text/html application/xhtml+xml], %w[text/html application/xhtml+xml] ],
    '6d7a49e8-5b33-44f2-b7c9-588816b98a04' => [:fix_xml_content_type,
      %w[application/xml text/xml], %w[application/xml application/atom+xml
        application/rdf+xml application/xhtml+xml image/svg+xml]] ,
    '46be5c11-fbcb-4dfc-a486-9ac3344a0308' => [:strip_comments,
      %w[application/xml text/html], %w[application/xml text/html] ],
    'b15e1970-9d1f-4ed1-b6cc-2a382d804dda' => [:rewrite_head,
      %w[text/html application/xhtml+xml], %w[text/html application/xhtml+xml] ],
    '937b4f68-b29e-4d54-84ce-144348248686' => [:repair_rdfa,
      %w[application/xml text/html], %w[application/xml text/html] ],
    'cdd7b9fb-c43a-48d1-a6ae-6a53f19146a4' => [:rehydrate,
      %w[application/xhtml+xml text/html], %w[application/xhtml+xml text/html] ],
    '775636e5-51c1-41e8-a5c8-eb1173f67735' => [:add_social_meta,
      %w[application/xhtml+xml text/html], %w[application/xhtml+xml text/html] ],
    '3ea4bc29-a5bb-410c-b78f-a327da5aa24c' => [:add_statements,
      %w[application/xhtml+xml text/html], %w[application/xhtml+xml text/html] ],
    '89a34334-956d-4122-92ee-29ae80fa558b' => [:add_backlinks,
      %w[application/xhtml+xml text/html], %w[application/xhtml+xml text/html] ],
    '582b691a-6e57-4704-aaab-17ab25a30527' => [:rewrite_links,
      %w[application/xml text/html], %w[application/xml text/html] ],
    '12c028fc-d780-4496-ab82-5b8fd82c5d65' => [:mangle_mailto,
      %w[application/xml text/html], %w[application/xml text/html] ],
    '06bb8a91-7257-476d-96f4-601f7dbdbf5d' => [:amazon_tag,
      %w[application/xml text/html], %w[application/xml text/html] ],
    '7713493a-617a-4980-80a5-3eaa9e441da1' => [:normalize_prefixes,
      %w[application/xml text/html], %w[application/xml text/html] ],
    'fb680265-b62f-4fb5-af35-52f18c186d01' => [:stylesheet_pi,
      %w[application/xml], %w[application/xml] ],
    '8c7dd87f-c236-4218-8028-e6c8692d7846' => [:reindent,
      %w[application/xml text/html], %w[application/xml text/html] ],
  }.freeze

  XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze
  SVGNS   = 'http://www.w3.org/2000/svg'.freeze
  ATOMNS  = 'http://www.w3.org/2005/Atom'.freeze
  XSLTNS  = 'http://www.w3.org/1999/XSL/Transform'.freeze

  # These are internal methods that actually perform the
  # transformations. since by this point we have everything we need from
  # the request, we can dispense with it and just operate over the body.
  # Parameters that have come in on the query string

  # XXX redcarpet accepts a string and returns a representation
  def parse_markdown req, params
    loc  = req.get_header 'HTTP_CONTENT_LOCATION'
    body = req.body
    doc  = MD::Noko.new.ingest body, loc
    body = representation.coerce doc, type: 'application/xhtml+xml'

    body
  end

  # same deal with tidy actually
  def tidy req, params
  end

  # idea here is we test root element name AND namespace, then just
  # namespace, then just root element.
  NAMESPACES = {
    [nil, ATOMNS]         => 'application/atom+xml',
    [nil, RDF::RDFV.to_s] => 'application/rdf+xml',
    [:rss, nil]           => 'application/rss+xml',
    [nil, XHTMLNS]        => 'application/xhtml+xml',
    [nil, SVGNS]          => 'image/svg+xml',
  }

  XPATHNS = { html: XHTMLNS }.freeze

  D = Intertwingler::Document

  public

  # I would rather not do this this way, but it's expedient. Any
  # header manipulation other than `Content-Type` should really be
  # done in a `message/http` transform (which aren't implemented yet).
  def fix_xml_content_type req, params
    body = req.body
    doc  = body.object
    type = body.type

    if root = doc.root
      name = root.name.to_sym
      ns   = root.namespace.href if root.namespace
      type = if ns
               NAMESPACES[[name, ns]] || NAMESPACES[[nil, ns]] || type
             else
               NAMESPACES[[name, nil]] || type
             end

      engine.log.debug "changed type fromm #{body.type} to #{type}"

      body.type = type
    end

    # XXX we are smuggling out the type and i would prefer not to do it this way
    body
  end

  def strip_comments req, params
    body = req.body
    doc  = body.object

    engine.log.debug "stripping comments lol"

    # easy peasy
    doc.xpath('//comment()').each { |c| c.unlink }

    # add back to invalidate
    body.object = doc
    body
  end

  HTML_HEAD_OTHER = '*[not(%s)]' %
    (%w[title base link meta script style].map do |x|
         'self::html:%s|self::%s' % [x, x]
       end.join ?|)

  # mkay the question is what do we actually want this to _do_? given
  # that it's in the milieu of a bunch of other things that are going
  # to do a bunch of other operations to various parts of the document
  #
  def rewrite_head req, params
    loc = subject_from req

    engine.log.debug "rewriting head on #{loc}"

    doc = req.body.object

    html = doc.root
    head = doc.at_xpath('(/html:html/html:head|/html/head)[1]', XPATHNS)
    body = doc.at_xpath('(/html:html/html:body|/html/body)[1]', XPATHNS)

    # you know what? just do this for now, i don't want to think about
    # repairing busted markup
    return req.body unless html and head and body

    subject = resolver.uuid_for loc, noop: true

    # we need separate prefix sets for head and body lol
    pfx  = resolver.prefixes
    hpfx = pfx.merge(D.get_prefixes head, coerce: :term)
    bpfx = pfx.merge(D.get_prefixes body, coerce: :term)

    # source may not have a head (but we aren't dealing with that rn)
    if head
      # fetch any existing nodes
      title  = head.xpath('(html:title|title)[1]', XPATHNS).map(&:unlink)
      base   = head.xpath('(html:base|base)[1]', XPATHNS).map(&:unlink)
      link   = head.xpath('html:link|link', XPATHNS).map(&:unlink)
      meta   = head.xpath('html:meta|meta', XPATHNS).map(&:unlink)
      script = head.xpath('html:script|script', XPATHNS).map(&:unlink)
      style  = head.xpath('html:style|style', XPATHNS).map(&:unlink)
      other  = head.xpath(HTML_HEAD_OTHER, XPATHNS).map(&:unlink)
      # nuke remaining child nodes
      head.children.each(&:unlink)

      # do title
      lp, lo = resolver.repo.label_for subject
      if title && !title.empty?
        title = title.first
        head << "\n"
        head << title
        if lp and (!title[:property] or title[:property].empty?)
          title[:property] = resolver.abbreviate lp, prefixes: hpfx
          title[:datatype] = resolver.abbreviate lo.datatype, prefixes: hpfx if
            lo.datatype?
          title[title.namespace ? 'xml:lang' : 'lang'] = lo.language if
            lo.language?
          title.content = lo.value
        end
      else
        spec = D.literal_tag resolver, lo, property: lp, name: :title,
          prefixes: hpfx
        XML::Mixup.markup parent: head, spec: ["\n", spec]
      end

      # base href is request uri but just add it, don't touch it
      if base && !base.empty?
        base = base.first
        head << "\n"
        head << base
      else
        base = XML::Mixup.markup parent: head,
          spec: ["\n", { '#base' => nil, href: loc.to_s }]
      end

      # next is all links
      link.each   { |el| head << "\n"; head << el }
      # next is all metas
      meta.each   { |el| head << "\n"; head << el }
      # next is all scripts
      script.each { |el| head << "\n"; head << el }
      # next is all inline stylesheets
      style.each  { |el| head << "\n"; head << el }
      # next is all other
      other.each  { |el| head << "\n"; head << el }
      # add a newline for the subsequent indenter
      head << "\n"
    end

    # body set id to canonical slug or compact uuid if not already set
    unless body[:id]
      slugs = resolver.repo.objects_for(
        subject, CI['canonical-slug'], only: :literal, datatype: RDF::XSD.token)
      if slugs.empty?
        # only do this if it's a uuid
        body[:id] = UUID::NCName.to_ncname_64(subject.to_s) if
          subject.start_with? 'urn:uuid:'
      else
        body[:id] = slugs.sort.first.value
      end
    end

    # body set about="" if not already set
    body[:about] = '' unless body[:about]
    # body unconditionally set types
    types = resolver.repo.types_for subject
    body[:typeof] = resolver.abbreviate(
      types, scalar: false, prefixes: bpfx).sort.join(' ') unless types.empty?

    # XXX do something about this one day lol
    req.body.object = doc
    req.body
  end

  RDFA_ATTRS = %w[about typeof rel rev property resource datatype].freeze
  RDFA_XPATH = ('//*[%s]' % RDFA_ATTRS.map { |a| "@#{a}" }.join(?|)).freeze

  # not sure what i actually intended this to do; probably scan for
  # missing prefixes or something
  def repair_rdfa req, params
    engine.log.debug "repairing rdfa lol"

    doc = req.body.object

    ns = doc.root&.namespace&.href

    return req.body unless [XHTMLNS, SVGNS, ATOMNS].include? ns or
      %w[html svg rss].include? doc.root.name

    missing = Set[]

    # scan the document looking for:
    # @about @typeof @rel @rev @property @resource @datatype
    doc.xpath(RDFA_XPATH).each do |elem|
      pfx = D.get_prefixes elem
      # engine.log.debug pfx.inspect
      RDFA_ATTRS.each do |a|
        if elem.key? a
          elem[a].strip.split(/\s+/).map { |x| x.split ?:, 2 }.each do |p, s|
            # swap if nil
            s, p = p, s unless s
            p = p.to_sym if p
            missing << p unless
              %i[http https urn ni mailto javascript].include?(p) || pfx[p]
          end
        end
      end
    end

    engine.log.debug "Found missing prefixes: #{missing.inspect}"

    fix = resolver.prefixes.slice(*missing.to_a.sort).transform_values(&:to_s)
    if missing.size > fix.keys.size
      rest = RDF::Vocabulary.vocab_map.slice(
        *(missing.to_a.sort - fix.keys)).transform_values { |v| v[:uri] }
      fix.merge! rest

      engine.log.debug "Still missing #{missing - fix.keys}" if
        missing.size > fix.keys.size
    end

    # okay now we merge with the root
    fix = D.get_prefixes(doc.root).merge fix

    # these are the attributes that can have curies; we basically scan
    # these for missing prefixes, then collect those up and rewrite
    # the outermost prefix declaration

    # if we find unprefixed terms and there is no vocab, assume it's
    # xhv

    if vocab = fix.delete(nil)
      doc.root[:vocab] ||= vocab
    elsif missing.include? nil
      doc.root[:vocab] = RDF::Vocab::XHV.to_s
    end

    doc.root[:prefix] = fix.sort do |a, b|
      a.first <=> b.first
    end.map { |k, v| "#{k}: #{v}" }.join(' ')

    req.body.object = doc
    req.body
  end

  # this one relinks
  def rehydrate req, params
    loc = subject_from req
    doc = req.body.object

    engine.log.debug "rehydrating lol"

    @lemmas ||= {}

    # Intertwingler::Document.rehydrate resolver, doc, base: loc, cache: @lemmas

    req.body.object = doc
    req.body
  end

  # what would be really sneaky is to do this exclusively based on
  # rdfa and never look at the graph
  def add_social_meta req, params
    # add schema dot org
    # add ogp
    # add twitter meta
    engine.log.debug "adding social media metadata lol"
    req.body
  end

  private

  BLOCK_ELEMS = %w[body main article section aside nav header footer
             blockquote figure figcaption search div[not(parent::html:dl)]]
  SUBJECT_XPATH = (
    %w[/html:html/html:body|/html:html/html:body//html:*
       [@id|@about|@resource|@href|@src]] +
      ['[%s]' % BLOCK_ELEMS.map { |e| "self::html:#{e}" }.join(?|)] +
      ['[not(ancestor-or-self::*[@property and not(@content|@datetime)])]']
  ).join.freeze

  CI  = Intertwingler::Vocab::CI
  TFO = Intertwingler::Vocab::TFO

  # this is used to grab the rdfa; it will return the graph or nil
  def get_rdfa req, role = CI.links

    resolver = engine.resolver

    # don't get this unless we have a subject, since it autovivifies
    # if it isn't already in there
    doc = req.body.object

    # we should have already determined this is an html document
    return unless body = doc.at_xpath(
      '/html:html/html:body|/html/body', XPATHNS)

    slug = role.to_s.delete_prefix(role.vocab.to_s)

    # bail out if there are already backlinks in here
    return if
      doc.xpath(".//*[contains(@role, '#{slug}')]").any? do |node|
        pfx = {
          nil => RDF::Vocab::XHV
        }.merge Intertwingler::Document.get_prefixes(node, coerce: :term)

        resolver.resolve_curies(
          node['role'], prefixes: pfx, noop: true).include? role
      end

    # XXX THESE CIRCUMVENT A DEFICIENCY IN THE RDFA PARSER <= 3.3.0
    version = /RDFa 1.0/ =~ (
      doc.children.detect { |c| c.is_a? ::Nokogiri::XML::DTD } ||
        doc.root.attribute('version')).to_s ? :"rdfa1.0" : :"rdfa1.1"
    hlang = doc.root&.namespace&.href == 'http://www.w3.org/1999/xhtml' ?
      :xhtml5 : :html5

    # this is the graph embedded in the document
    RDF::RDFa::Reader.new(
      doc, version: version, host_language: hlang)
  end

  def add_ambi_links req, reverse: false
    loc = subject_from req

    resolver = engine.resolver

    role = CI[reverse ? :backlinks : :links]

    # return unless subject = resolver.uuid_for(loc)
    # return unless rdfa    = get_rdfa(req, role)

    # okay apparently this was costing something so we'll cache it
    uuids = {}

    subject = uuids[loc] = resolver.uuid_for(loc)
    engine.log.debug "subject: #{subject}"
    return unless subject

    rdfa    = get_rdfa(req, role)
    engine.log.debug "rdfa: #{rdfa}"
    return unless rdfa


    # first we isolate the relevant statements
    graph = RDF::Repository.new
    rdfa.each_triple do |s, p, o|
      # only subjects that are IRIs obvs
      next unless s.iri?

      # test if we're dealing with the subject or a fragment
      stest = s.dup
      stest.fragment = nil
      next unless stest == loc

      # remap these to their uuids so as not to confuse
      s, p, o = [s, p, o].map do |x|
        x.iri? ? (uuids[x] ||= resolver.uuid_for(x, noop: true)) : x
      end

      # add to graph
      graph << [s, p, o]
    end

    # engine.log.debug(graph.to_turtle, prefixes: resolver.prefixes)
    uris = uuids.invert

    # collect the labels and a label-type struct for the comparand
    labels   = {}
    labtypes = {}

    # graph may be empty or not even contain the subject
    subjects = graph.subjects.dup
    subjects << subject unless subjects.include? subject

    # we'll construct these special inverted structs by hand
    structs = subjects.reduce({}) do |hash, resource|
      q = [resource, nil, nil].send(reverse ? :reverse : :to_a)

      resolver.repo.query(q).each do |stmt|
        # we don't want to duplicate existing statements; note
        # whatever's coming out of the repo is going to have a graph
        # name and we don't need to know/care what it is, so we use
        # has_triples? rather than has_statement?
        tmts = resolver.repo.invert_statement stmt
        next if !stmt.subject.iri? or graph.has_triple?(stmt.to_triple) or
          (tmts and graph.has_triple? tmts.to_triple)

        # c wut i did thar?
        s, p, o = stmt.to_triple.send(reverse ? :reverse : :to_a)

        # okay now remap to routable URIs
        [s, p, o].select(&:iri?).each do |x|
          uris[x] ||= resolver.uri_for(x, slugs: true, fragments: true)
        end

        [s, o].each do |uuid|
          # get the labels; note we still use stmt.object for the query/test
          if uuid.iri?
            uri = uris[uuid]
            st  = labtypes[uri] ||= {}
            st[RDF.type] ||= resolver.repo.types_for uuid
            unless labels[uri]
              lp, lo = resolver.repo.label_for uuid, noop: true
              labels[uri] ||= [lp, lo]
              (st[lp] ||= []) << lo if lp
            end
          end
        end

        # build the inverted struct
        ((hash[uris[s]] ||= {})[uris[o] || o] ||= []) << p
      end
      hash
    end

    # engine.log.debug labtypes.keys.inspect
    ssz = structs.reduce(0) { |i, pair| i + pair.last.size }
    engine.log.debug "graph: #{graph.size}; structs: #{ssz}"

    # okay this is our actual product
    links, metas = [], []

    rel = reverse ? :rev : :rel

    # now let's create the nav structure
    structs.each do |s, struct|
      # only make the ul if there are links
      if struct.keys.any?(&:iri?)
        # give us a lael comparator
        lcmp = resolver.repo.cmp_label cache: labtypes

        li = struct.keys.select(&:iri?).sort(&lcmp).map do |o|
          ps = struct[o]
          lp, lo = labels[o]
          # engine.log.debug "#{o} -> #{labtypes[o].inspect}"
          a = Intertwingler::Document.link_tag resolver, o,
            rel => ps, typeof: labtypes[o][RDF.type], property: lp, label: lo
          ["\n", { '#li' => a }]
        end.flatten(1) << "\n"

        links += ["\n", { '#ul' => li, about: s }] unless li.empty?
      end

      # do the metas unconditionally; just reuse lcmp for that
      lcmp = resolver.repo.cmp_literal &:first

      struct.select { |k, _| k.literal? }.sort(&lcmp).each do |o, ps|
        metas += [Intertwingler::Document.literal_tag(
          resolver, o, property: ps, name: :meta), "\n"]
      end
    end

    # now let's get a fresh copy to munge
    doc = req.body.object.dup

    # uhh we had graph.terms in here but it hauled in way too much
    # stuff (it still hauls in too much stuff); take the
    # all-classes/all-properties catalogue resource: they list every
    # class and property but don't actually abbreviate them, so those
    # shouldn't count. we only need prefixes for the terms *we* add.
    tpfx = [labtypes, labels, structs, role].reduce({}) do |hash, x|
      hash.merge resolver.prefix_subset(x)
    end

    # now merge with the document
    pfx = Intertwingler::Document.get_prefixes(doc.root).merge tpfx

    # now reset
    doc.root['prefix'] = XML::Mixup.flatten_attr(pfx.reject { |k, _| k.nil? })
    doc.root['vocab']  = pfx[nil] if pfx.key? nil

    if !reverse and !metas.empty? and
        head = doc.at_xpath('/html:html/html:head', XPATHNS)
      XML::Mixup.markup parent: head, spec: metas
    end

    if !links.empty? and body = doc.at_xpath('/html:html/html:body', XPATHNS)
      role = resolver.abbreviate role, prefixes: pfx
      XML::Mixup.markup parent: body,
        spec: [{ '#nav' => links + ["\n"], role: role }, "\n"]
    end

    # oh btw we should repair the prefixes in the root; we have
    # something for that right?

    doc
  end

  public

  # Add a `<nav role="ci:links">` block containing all links not
  # otherwise expressed in the document, as well as adding `<meta>`
  # tags to the `<head>` to do the same for literals.
  #
  # @param req [Rack::Request]
  # @param params [Params::Registry::Instance]
  #
  # @return [Intertwingler::Representation::Nokogiri] the request body.
  #
  def add_statements req, params
    loc = subject_from req

    engine.log.debug "adding orphaned links/statements to #{loc}"

    # that's all, huh
    return req.body unless doc = add_ambi_links(req)

    # now we do the dumb thing where we have to coerce it
    req.body.object = doc
    req.body
  end

  # We do backlinks in a transform because that way it nets stuff like
  # static content, where you don't know what the backlinks even are,
  # and if you did, it would be a pain in the ass to maintain them. We
  # tag the containing element with `role="ci:backlinks"` which also
  # leaves an exit for something else to supply them.
  #
  # okay so the problem is we need to do backlinks for not just the
  # document, but anything the document embeds. we propose to use
  # `id` attributes that match fragment identifiers in the embedded
  # graph data. so an element that has an `id` attribute *and* an
  # `about`/`resource`/`href`/`src` attribute is our de facto
  # container element. note that the root `<html>` element, the
  # `<head>` and its descendants, and the `<body>` all implicitly
  # take the document (or rather the content of `<base href="…">`)
  # as the subject unless modified (e.g. with `about`).
  #
  # with those exceptions then, we can scan the document for
  # container elements. note that some container elements
  # (e.g. `<tr>`) will have a content model that does not permit
  # plunking a `<nav>` down into it, and so another strategy should
  # be chosen.
  #
  # again, if an element containing the role `ci:backlinks` is
  # already present in the subtree, we assume the origin or some
  # other transform upstream has already done the job for us for
  # that subject URI/container element.
  #
  # another thing to watch out for is the empty fragment, which is
  # distinct from the document. that can't have an `id` but it *can*
  # have an e.g. `about`. so the solution here would be to find the
  # outermost one (and if there are multiple outermost ones, we pick
  # either the first or last one of them).
  #
  # PROCEDURE: scan every node that asserts a new subject for its
  # children (or has an id), perhaps with an xpath like
  # SUBJECT_XPATH above:
  #
  # /html:html/html:body|/html:html/html:body//html:*
  #  [@id|@about|@resource|@href|@src]
  #  [not(ancestor-or-self::*[@property and not(@content|@datetime)])]
  #
  # (should also add [self::sectioning-element|self::html:a])
  #
  # no wait a second, `<a>` can't have `<a>` descendants (which is
  # dumb frankly) so that rules it out
  #
  # valid elements look like: body, main, article, section, aside,
  # nav, header, footer, blockquote, figure, search,
  # div[not(parent::dl)]
  #
  # actually, do we *want* to include `href` and `src`? we are only
  # interested in block elements that don't have them (although <a>
  # can be coopted into one come to think of it).
  #
  # okay so here is the strategy: find the most "appropriate"
  # element for each subject:
  #
  # * first check if `@id` matches `@resource|@href|@src|@about` (in
  #   that order); if so this is it.
  # * otherwise if `@id` is present and matches the subject in an
  #   ancestor, this is it.
  # * otherwise if no node with `@id` is found, we want the
  #   "biggest" one that asserts the subject:
  #   * closer to the root ranks higher than farther down
  #   * closer to the front ranks higher than farther back
  #
  # subjects with zero elements appropriate for containing a block
  # of backlinks can be collated at the bottom as the last element
  # of the `<body>`.
  #
  def add_backlinks req, params
    loc = subject_from req

    engine.log.debug "adding backlinks to #{loc}"

    # XXX NOTE that there is an Intertwingler::Document.backlinks but
    # i did not use it here because it's fkn broken; if anything move
    # the add_ambi_links thing to Intertwingler::Document

    return req.body unless doc = add_ambi_links(req, reverse: true)

    # again, reassigning the object resets the faux-nad
    req.body.object = doc

    req.body
  end

  # rewrite uuids and crap to their http(s or whatever other scheme)
  # counterparts
  def rewrite_links req, params
    # engine.resolver_for params[:subject]
    engine.log.debug "rewriting links lol"

    if loc = req.get_header('HTTP_CONTENT_LOCATION')
      loc = resolver.coerce_resource loc, as: :uri
    end

    engine.log.debug "rewrite: #{loc}"

    # get a duplicate of the document
    doc = req.body.object.dup

    # no return because this operates in situ
    Intertwingler::Document.rewrite_links engine.resolver, doc, base: loc

    # (* URIs should be empty, slug/query/fragment, full path, or
    # absolute. no screwing around with ../ or //authority.host/)

    # reassign to invalidate any serialized version
    # (XXX THIS IS DUMB IS THERE A BETTER WAY TO DO THIS???)
    req.body.object = doc

    req.body
  end

  # mangle mailto: URIs according to house style
  def mangle_mailto req, params
    engine.log.debug "mangling mailto: lol"
    doc = req.body.object
    if Intertwingler::Document.html? doc
      doc = doc.dup
      doc.xpath("//*[@href|@about|@resource]/@*" +
                "[starts-with(normalize-space(.), 'mailto:')]").each do |attr|
        address = resolver.coerce_resource(attr.value.strip, as: :uri)
        lp, dom = address.to.split(?@, 2).map do |x|
          URI::encode_uri_component x
        end

        headers = address.headers.reduce({}) do |h, pair|
          h[pair.first.downcase] = URI::decode_uri_component pair.last
          h
        end

        htxt = case
               when headers.empty? then ''
               when headers.count == 1 && headers.key?('subject')
                 "'%s'" % headers['subject'].gsub(
                   /\G([^\\']*(?:\\\\)*(?!\\)')/, "\\1\\\\'")
               else
                 "{%s}" % headers.map do |k, v|
                   v = v.gsub(/\G([^\\']*(?:\\\\)*(?!\\)')/, "\\1\\\\'")
                   "#{k}:'#{v}'"
                 end.join(?,)
               end
        htxt = ?, + URI::encode_uri_component(htxt) unless htxt.empty?


        attr.value = %q[javascript:sendMail('%s','%s'%s)] % [dom, lp, htxt]
        elem = attr.parent
        if elem.children.count == 1 && elem.children.first.text?
          # XXX this is dumb lol, make it better
          text = elem.children.first
          tmp = text.content
          if tmp.include? address
            tmp.sub(/#{address}/,
                    ["\u202e", dom.reverse, "\u202d",
                     "\u1202e", ?@, lp.reverse, "\u202d"].join(''))
            text.content = tmp
          end
        end
      end

      req.body.object = doc
    end
    req.body
  end

  # traverse the document body looking for amazon links, add `?tag=youknowwho`
  def amazon_tag req, params
    engine.log.debug "amazon taggin', lol"
    req.body
  end

  # read the RDFa and prune unnecessary prefix declarations, also
  # bundle them all up to the outermost bit
  def normalize_prefixes req, params
    engine.log.debug "normalizing rdfa prefixes lol"
    req.body
  end

  # add a stylesheet processing instruction to the top of the document
  def stylesheet_pi req, params
    # warn "lol stylesheet pi: #{params.inspect}"
    body = req.body
    doc  = body.object

    # nothing to do if this is the case
    return body unless doc.root

    # don't do this to other stylesheets
    return body if doc.root&.namespace&.href == XSLTNS

    # this is the raw content-location header
    subject = subject_from req

    # coerce Params::Registry::Instance to plain hash
    params = params.to_h

    r = engine.resolver

    # if a parameter is passed in explicitly, use that
    if params[:href]
      template = params[:href]
      mimetype = params[:type]
    elsif subject = r.uuid_for(subject)
      # otherwise rummage around the graph and find the best-matching
      # template associated with the subject

      # there won't be many of these, though this can stand to be cached
      #
      # XXX alTHOUGH this and the next stanza could probably be merged
      # because this doesn't get used a second time
      candidates = r.repo.all_of_type(CI.Template).map do |t|
        out = {
          resource: r.repo.objects_for(t, CI.resource),
          class:    r.repo.objects_for(t, CI.class)
        }

        [t, out]
      end.to_h

      # now we rearrange; XXX note if there are collisions, this will
      # blow away duplicates arbitrarily
      resources, classes = candidates.reduce([{}, {}]) do |accumulator, pair|
        t = pair.first
        rsrc, cls = pair.last.values_at(:resource, :class)
        rsrc.each { |r| accumulator.first[r] = t }
        cls.each  { |c| accumulator.last[c]  = t }

        accumulator
      end

      # first attempt to match the subject exactly (unlikely but w/e)
      unless template = resources[subject]
        # welp that was easy, let's track down the best match by type
        types = r.repo.types_for subject
        types << RDF::RDFS.Resource if types.empty? # not sure if dumb
        tc = {} # type_is? score cache

        template = classes.select do |k, _|
          x = r.repo.type_is? types, k
          # engine.log.debug "#{types.join ', '} -> #{k} score: #{x}"
          tc[k] ||= x if x
        end.sort do |a, b|
          # engine.log.debug("#{b.first} (#{tc[b.first]}) <=> #{a.first} (#{tc[a.first]})");
          tc[a.first] <=> tc[b.first]
        end.map(&:last).first
      end
    else
      template = params[:default]
    end

    if template
      engine.log.debug template.inspect

      # ensure this is in fact a stable identifier
      uu = r.uuid_for template

      # rewrite the template as a routable address
      template = r.uri_for template, slugs: true, as: :rdf
      ruri = RDF::URI(req.url) # RDF::URI has authority= but URI does not

      # deal with domain aliasing
      template = r.as_alias template, ruri

      # what are we looking at here
      engine.log.debug template.inspect

      if !mimetype and uu and not params[:href]
        mimetype = r.repo.objects_for(
          uu, RDF::Vocab::DC.format,
          only: :literal, datatype: TFO[:"content-type"]
        ).sort.first
        mimetype = mimetype.object.to_s.downcase if mimetype
      end

      # assign default and override because of browsers wah wah boo hoo
      mimetype ||= 'text/xsl'
      mimetype = 'text/xsl' if mimetype == 'application/xslt+xml'

      # assign back to the params cause they're in a hash already lol
      params[:href] = template
      params[:type] = mimetype

      # okay now we actually do the surgery; first we remove anything
      # that's already there
      doc.xpath("/processing-instruction('xml-stylesheet')").each do |c|
        c.unlink
      end

      # then we find the node we're gonna stick this in front of
      node = doc.external_subset || doc.internal_subset || doc.root

      engine.log.debug "adding stylesheet PI: #{params[:href]}"

      # et voilà
      XML::Mixup.markup before: node,
        spec: { '#pi' => 'xml-stylesheet' }.merge(params.slice :type, :href)
    end

    # goose the body by assigning the doc
    body.object = doc
    body
  end

  # normalize the indentation
  def reindent req, params
    body = req.body
    doc  = body.object

    return body if doc.root&.namespace&.href == XSLTNS

    engine.log.debug "reindenting lol"

    # this reindents in place
    Intertwingler::Document.reindent doc

    # spur the invalidation of the embedded io object
    body.object = doc

    # warn body.inspect

    body
  end

end
