require 'intertwingler/handler'
require 'intertwingler/transform'

# 
require 'intertwingler/document'

# the representation
require 'intertwingler/representation/nokogiri'

# This class is not quite figured out
class Intertwingler::Transform::Markup < Intertwingler::Handler

private

# This map routes URIs (UUIDs as single path segment) to internal methods.

URI_MAP = {
  '8307ac09-670b-48b9-b08d-3eacc1f51f43' => [:parse_markdown,
    %w[text/markdown], %w[application/xhtml+xml text/html] ],
  'ca069a8a-dd73-423d-b4c2-77777c049f36' => [:tidy,
    %w[text/html application/xhtml+xml], %w[text/html application/xhtml+xml] ],
  '46be5c11-fbcb-4dfc-a486-9ac3344a0308' => [:strip_comments,
    %w[application/xml text/html], %w[application/xml text/html] ],
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
}

# these are internal methods that actually perform the transformations.

# same deal with redcarpet actually
def parse_markdown body, params
end

# XXX tidy returns a string so it need not be in this
def tidy body, params
end

def strip_comments body, params
end

def repair_rdfa body, params
end

def rehydrate body, params
end

def add_social_meta body, params
end

def add_backlinks body, params
end

def rewrite_links body, params
end

def mangle_mailto body, params
end

def amazon_tag body, params
end

def normalize_prefixes body, params
end

def stylesheet_pi body, params
end

def reindent body, params
end

public

def handle req
  # first we check if the request method is POST; if not this is over quickly
  return Rack::Response[405, {}, []] unless req.request_method.to_sym == :POST

  # give us a default response
  resp = Rack::Response[404, {}, []]

  # get the resolver for this request
  resolver = resolver_for req

  uri  = req.url
  uuid = resolver.split_pp(url).first.split(?/)[1].downcase

  # match the function
  func, atypes, rtypes = URI_MAP[uuid]

  # 404 unless we have a function
  return resp unless func

  # here we do the content negotiation
  # 406 unless types match up

  # okay now we actually run the thing
  begin
    body = req.body.is_a?(Intertwingler::Representation::Nokogiri) ? req.body : Intertwingler::Representation::Nokogiri.new(req.body)
    # note out can be nil which should be interpreted as 304
    out = send func, req.body, params
  rescue Intertwingler::Transform::ParamError
    return Rack::Response[409, {}, []]
  end
end

end
