# rack? sure

require 'rdf/sak/util'
require 'rack'
require 'rack/request'
require 'rack/response'
require 'http/negotiate'
require 'uuidtools'

class RDF::SAK::Console
  private

  # http errors we care about
  ERROR = {
    403 => -> uri, req {
    },
    404 => -> uri, req {
      # nothing here, make something?
    },
    405 => -> uri, req {
    },
    406 => -> uri, req {
    },
  }

  # content-negotiating static handler
  STATIC = -> uri, req {
  }

  DISPATCH = {
    RDF::SAK::Util::UUID_PATH => {
      GET: -> uri, req {
        # first check if the subject is present; if not then 404

        # this could include being an object

        # do content negotiation; hive off non-xhtml responses

        # lol caching fuhgedaboudit; you would need metadata about
        # metadata about resources and how that changes over time
        # which wouldn't be impossible but also wouldn't be reliable
        # without baking it into the quad store

        # generate the response (here is where loupe would be handy)
        doc = RDF::SAK::Util.generate graph, subject, headers: req.headers
        xml = doc.to_xml

        # return 200
        [200, xml, {
          'Content-Type'   => 'application/xhtml+xml;charset=utf-8',
          'Content-Length' => xml.b.length.to_s,
        }]
      },
      POST: -> uri, req {
        # negotiate input

        # modify graph

        # return 303 to itself
        [303, nil, { 'Location' => uri.to_s }]
      },
      DELETE: -> uri, req {
        # nuke all statements to and from this subject
        # return 204
        [204, nil, {}]
      },
    },
    # internut hoem paeg maybe just show all the (non-blank) subjects
    # in the graph? iunno how about subject (with label), outbound
    # links collated by type?
    ?/ => {
      GET: -> uri, req {
      },
    },
    # this is the /me resource that tells the ui about who is looking at it
    '/me' => {
      GET: -> uri, req {
      },
    },
    # these are utility resources so the ui has some content to work with
    '/classes' => {
      GET: -> uri, req {
        # needs at least one in-domain-of or in-range-of query
        # parameter or it will disgorge everything it knows lol

        # althouuugh that might not actually be that big a deal in
        # practice; how many classes can there be (spoiler: lots)
      },
    },
    '/properties' => {
      GET: -> uri, req {
        # this on the other hand, well, there are always gonna be way
        # more properties than classes so it should probably be pruned

        # so say by default it returns all known properties with an
        # unspecified domain; then you can specify one or more classes
        # in domain= (and range= for parity although this might be dumb)
      },
    }
  }.transform_values do |methods|
    # copies GET to HEAD but only if there is a GET (and no HEAD already)
    methods[:GET] ? { HEAD: methods[:GET] }.merge(methods) : methods
  end.freeze

  def dispatch uri, req
    # match uri or 404
    _, methods = DISPATCH.detect { |test, _| test === uri.request_uri }

    # match method or 405
    handler = methods ?
      methods.fetch(req.request_method.to_sym, ERROR[405]) : STATIC

    # run the handler, whatever that may be
    resp = instance_exec uri, req, handler
    resp = Rack::Response[*resp] unless resp.is_a? Rack::Response
    # XXX maybe nuke the body if it is a HEAD request? ehh
    resp
  end

  public

  # document root
  # 
  def initialize
  end

  def call env
    # normalize the environment in the case of ssl tomfoolery
    env['HTTPS'] = 'on' if env.key? 'REQUEST_SCHEME' and
      env['REQUEST_SCHEME'].to_s.strip.downcase == 'https'
    req  = Rack::Request.new env
    uri  = URI(req.base_url) + env['REQUEST_URI']

    # here is where we would rewrite the request i guess

    # dispatch
    resp = dispatch uri, req

    # here is where we would rewrite the response i guess

    resp.finish
  end
end
