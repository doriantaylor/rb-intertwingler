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
    [nil, 'http://www.w3.org/2005/Atom'] =>
      'application/atom+xml',
    [nil, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'] =>
      'application/rdf+xml',
    [:rss, nil] => 'application/rss+xml',
    [nil, 'http://www.w3.org/1999/xhtml'] =>
      'application/xhtml+xml',
    [nil, 'http://www.w3.org/2000/svg'] =>
      'image/svg+xml',
  }

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

  def rewrite_head req, params
    engine.log.debug "rewriting head lol"

    req.body
  end

  # not sure what i actually intended this to do; probably scan for
  # missing prefixes or something
  def repair_rdfa req, params
    engine.log.debug "repairing rdfa lol"
    req.body
  end

  # this one relinks
  def rehydrate req, params
    #req.body.object
    #Intertwingler::Document.rehydrate
    engine.log.debug "rehydrating lol"
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

  public

  # We do backlinks in a transform because that way it nets stuff like
  # static content, where you don't know what the backlinks even are,
  # and if you did, it would be a pain in the ass to maintain them. We
  # tag the containing element with `role="ci:backlinks"` which also
  # leaves an exit for something else to supply them.
  #
  def add_backlinks req, params
    # have i mentioned it's remarkable that Rack::Request has a
    # different regime for header names than Rack::Response?
    loc = req.get_header 'HTTP_CONTENT_LOCATION'

    raise Intertwingler::Handler::Error::Conflict.new(
      'Transform must have a Content-Location header', method: :POST) unless loc

    # make sure we're coercing a string
    loc = RDF::URI(loc.to_s)

    engine.log.debug "adding backlinks to #{loc}"

    resolver = engine.resolver

    return req.body unless subject = resolver.uuid_for(loc)

    # don't get this unless we have a subject, since it autovivifies
    # if it isn't already in there
    doc = req.body.object

    # we should have already determined this is an html document
    return req.body unless
      body = doc.at_xpath('/html:html/html:body|/html/body',
      { html: 'http://www.w3.org/1999/xhtml' })

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

    # mapping = doc.xpath(SUBJECT_XPATH, XPATHNS).reduce({}) do |hash, elem|
    #   Intertwingler::Document.subject_for resolver,
    #   hash
    # end

    # XXX 2025-03-17 CHEAP OUT FOR NOW
    #
    # we are gonna just collate all the resources at the bottom

    # anyway snag all the subjects already present in the document and
    # turn them into a mapping

    # XXX THESE CIRCUMVENT A DEFICIENCY IN THE RDFA PARSER <= 3.3.0
    version = /RDFa 1.0/ =~ (
      doc.children.detect { |c| c.is_a? ::Nokogiri::XML::DTD } ||
        doc.root.attribute('version')).to_s ? :"rdfa1.0" : :"rdfa1.1"
    hlang = doc.root&.namespace&.href == 'http://www.w3.org/1999/xhtml' ?
      :xhtml5 : :html5

    subjects = ([loc] + RDF::RDFa::Reader.new(
      doc, version: version, host_language: hlang).map do |stmt|
      [stmt.subject, stmt.object]
    end.flatten).uniq.select do |t|
      if t.iri?
        t = resolver.as_alias t.dup, loc
        engine.log.debug "FOUND TERM #{t}"
        t.fragment = nil
        t == loc
      end
    end.sort { |a, b| a <=> b }.reduce({}) do |hash, s|
      hash[resolver.uuid_for s, noop: true] = s
      # case s.fragment
      # when nil
      #   # this is gonna be the <body> unless it isn't for some reason
      # when ''
      #   # this is gonna be like `about="#"` or something
      # else
      #   # this *should* be an `id` but the markup might not have one
      # end
      # # scan the document
      hash
    end

    engine.log.debug subjects.inspect

    # bail out if there are already backlinks in here
    return req.body if
      doc.xpath(".//*[contains(@role, 'backlinks')]").any? do |node|
        pfx = {
          nil => RDF::Vocab::XHV
        }.merge Intertwingler::Document.get_prefixes(node, coerce: :term)

        resolver.resolve_curies(
          node['role'], prefixes: pfx, noop: true).include? CI.backlinks
      end

    # this will collect all the terms involved in the backlinks so we
    # can amend the prefixes/vocab
    terms = Set[]

    return req.body unless links = Intertwingler::Document.backlinks(
      resolver, subject, resources: subjects.keys,
      base: loc, published: false) do |*args|
        args = args.map do |a|
          a.respond_to?(:to_a) ? a.to_a : a
        end.flatten.compact.uniq.map do |t|
          t.literal? && t.datatype? ? t.datatype : t.uri? ? t : nil
        end.compact

        terms |= args
      end

    # obtain prefixes from terms
    tpfx = resolver.prefix_subset terms

    if pfx = doc.root['prefix']
      # chop up into an array
      # engine.log.debug pfx.inspect
      pfx = pfx.strip.split
      # engine.log.debug pfx.length
      # collect into a hash
      h = {}
      until (pair = pfx.slice! 0, 2).empty?
        h[pair.first.chop] = pair.last
      end
      # engine.log.debug h.inspect
      # normalize it
      pfx = resolver.sanitize_prefixes h, nonnil: true
    else
      pfx = {}
    end

    pfx.merge! tpfx

    # engine.log.debug pfx.inspect

    doc.root['prefix'] = XML::Mixup.flatten_attr(pfx.reject { |k, _| k.nil? })
    doc.root['vocab']  = pfx[nil] if pfx.key? nil

    # decided the magic word to include backlinks in a way that
    # can be picked up by a transform but still comply with the
    # spec is <noscript>.
    XML::Mixup.markup parent: body, spec: links

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
    doc = body.object

    # coerce Params::Registry::Instance to plain hash
    params = params.to_h

    # resolve this if need be
    if params[:href]
      r = engine.resolver
      href = params[:href] = r.uri_for params[:href]
      ruri = RDF::URI(req.url) # RDF::URI has authority= but URI does not

      if r.authorities.include? href.authority and
          r.authorities.include? ruri.authority
        href.authority = ruri.authority
        href.scheme    = ruri.scheme
      end

    end

    if doc.root
      doc.xpath("/processing-instruction('xml-stylesheet')").each do |c|
        c.unlink
      end

      node = doc.external_subset || doc.internal_subset || doc.root

      engine.log.debug "adding stylesheet PI: #{params[:href]}"

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

    engine.log.debug "reindenting lol"

    # this reindents in place
    Intertwingler::Document.reindent doc

    # spur the invalidation of the embedded io object
    body.object = doc

    # warn body.inspect

    body
  end

end
