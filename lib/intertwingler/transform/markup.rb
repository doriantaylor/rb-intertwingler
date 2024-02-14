require 'intertwingler/transform'

# most of the actual substantive code lives in this
require 'intertwingler/document'

# the representation
require 'intertwingler/representation/nokogiri'

# these are also handy
require 'xml-mixup'
require 'md-noko'
require 'tidy_ffi'

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

  # These are internal methods that actually perform the
  # transformations. since by this point we have everything we need from
  # the request, we can dispense with it and just operate over the body.
  # Parameters that have come in on the query string

  # XXX redcarpet accepts a string and returns a representation
  def parse_markdown req, params
    body = req.body
    doc  = MD::Noko.new.ingest body
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
      warn "changed type fromm #{body.type} to #{type}"
      body.type = type
    end

    # XXX we are smuggling out the type and i would prefer not to do it this way
    body
  end

  def strip_comments req, params
    body = req.body
    doc  = body.object

    # easy peasy
    doc.xpath('//comment()').each { |c| c.unlink }

    # add back to invalidate
    body.object = doc
    body
  end

  def rewrite_head req, params
    warn "rewriting head lol"
    req.body
  end

  # not sure what i actually intended this to do; probably scan for
  # missing prefixes or something
  def repair_rdfa req, params
    warn "repairing rdfa lol"
    req.body
  end

  # this one relinks
  def rehydrate req, params
    #req.body.object
    #Intertwingler::Document.rehydrate
    warn "rehydrating lol"
    req.body
  end

  # what would be really sneaky is to do this exclusively based on
  # rdfa and never look at the graph
  def add_social_meta req, params
    # add schema dot org
    # add ogp
    # add twitter meta
    warn "adding social media metadata lol"
    req.body
  end

  # stick a wad of backlinks everywhere they fit
  def add_backlinks req, params
    # backlinks = Intertwingler::Document.backlinks
    # XML::Mixup.markup
    warn "adding backlinks lol"
    req.body
  end

  # rewrite uuids and crap to their http(s or whatever other scheme)
  # counterparts
  def rewrite_links req, params
    # engine.resolver_for params[:subject]
    warn "rewriting links lol"
    req.body
  end

  # mangle mailto: URIs according to house style
  def mangle_mailto req, params
    warn "mangling mailto: lol"
    req.body
  end

  # traverse the document body looking for amazon links, add `?tag=youknowwho`
  def amazon_tag req, params
    warn "amazon taggin', lol"
    req.body
  end

  # read the RDFa and prune unnecessary prefix declarations, also
  # bundle them all up to the outermost bit
  def normalize_prefixes req, params
    warn "normalizing rdfa prefixes lol"
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
    params[:href] = engine.resolver.uri_for params[:href], as: :uri if
      params[:href]

    if doc.root
      doc.xpath("/processing-instruction('xml-stylesheet')").each do |c|
        c.unlink
      end

      node = doc.external_subset || doc.internal_subset || doc.root

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

    warn "reindenting lol"

    # this reindents in place
    Intertwingler::Document.reindent doc

    # spur the invalidation of the embedded io object
    body.object = doc

    # warn body.inspect

    body
  end

end
