# rack? sure

require 'rdf/sak/util'

require 'rack'
require 'rack/request'
require 'rack/response'
require 'http/negotiate'
require 'uuidtools'
require 'pathname'

class RDF::SAK::Console

  class Request < Rack::Request
    # this should be in vanilla Rack::Request
    def full_uri
      URI(base_url) + env['REQUEST_URI']
    end
    
  end

  class Response < Rack::Response

    private

    # mapping xml namespaces to content types
    TYPES = {
      nil => {
        html: 'application/xhtml+xml',
      },
      'http://www.w3.org/1999/xhtml' => {
        nil => 'application/xhtml+xml',
      },
    }

    def resolve_type node
      doc  = node.is_a?(Nokogiri::XML::Document) ? node : node.document
      node = doc.root
      ns   = node.namespace.href if node.namespace
      type = TYPES.key?(ns) ?
        TYPES[ns].fetch(node.name.to_sym, TYPES[ns][nil]) : 'application/xml'

      "#{type};charset=#{(doc.encoding || 'utf-8').downcase}"
    end

    public

    def initialize body, status, headers
      body    ||= ''
      status  ||= 501 # Not Implemented lol
      headers ||= {}

      if body.is_a? Nokogiri::XML::Document
        # XXX failure modes in here like there not being a root
        headers[Rack::CONTENT_TYPE] = resolve_type body
      
        body = body.to_xml
        # XXX why is there no explicit set?
        headers[Rack::CONTENT_LENGTH] = body.b.length.to_s
      end
      
      super body, status, headers
    end
  end

  private

  # http errors we care about
  ERROR = {
    403 => -> req {
    },
    404 => -> req {
      # nothing here, make something?
      
      doc = context.xhtml_stub().document
    },
    405 => -> req {
    },
    406 => -> req {
    },
  }

  # content-negotiating static handler
  STATIC = -> req {
    
  }

  DISPATCH = {
    RDF::SAK::Util::UUID_PATH => {
      GET: -> req {
        uri   = req.full_uri
        match = RDF::SAK::Util::UUID_PATH.match uri.request_uri

        subject = context.canonical_uuid match.captures.first

        # first check if the subject is present; if not then 404

        # this could include being an object

        # do content negotiation; hive off non-xhtml responses

        # lol caching fuhgedaboudit; you would need metadata about
        # metadata about resources and how that changes over time
        # which wouldn't be impossible but also wouldn't be reliable
        # without baking it into the quad store

        # generate the response (here is where loupe would be handy)
        doc = RDF::SAK::Util.generate_doc graph, subject, base: base,
         langs: req.accept_language.to_h, prefixes: context.prefixes
        
        # return 200
        [200, {}, doc]
      },
      POST: -> req {
        # negotiate input

        # modify graph

        # return 303 to itself
        [303, nil, { 'Location' => req.full_uri.to_s }]
      },
      DELETE: -> req {
        # nuke all statements to and from this subject
        # return 204
        [204, {}, nil]
      },
    },
    # internut hoem paeg maybe just show all the (non-blank) subjects
    # in the graph? iunno how about subject (with label), outbound
    # links collated by type?
    ?/ => {
      GET: -> req {
        subjects = graph.subjects.sort.select &:uri?
        body = subjects.map(&:to_s).join "\n"
        [200, { 'Content-Type' => 'text/plain' }, body]
      },
    },
    # this is the /me resource that tells the ui about who is looking at it
    '/me' => {
      GET: -> req {
      },
    },
    # these are utility resources so the ui has some content to work with
    '/classes' => {
      GET: -> req {
        # needs at least one in-domain-of or in-range-of query
        # parameter or it will disgorge everything it knows lol

        # althouuugh that might not actually be that big a deal in
        # practice; how many classes can there be (spoiler: lots)
      },
    },
    '/properties' => {
      GET: -> req {
        # this on the other hand, well, there are always gonna be way
        # more properties than classes so it should probably be pruned

        # so say by default it returns all known properties with an
        # unspecified domain; then you can specify one or more classes
        # in domain= (and range= for parity although this might be dumb)
      },
    },
    /^\/.*/ => {
      GET: -> req {
      },
    },
  }.transform_values do |methods|
    # copies GET to HEAD but only if there is a GET (and no HEAD already)
    methods[:GET] ? { HEAD: methods[:GET] }.merge(methods) : methods
  end.freeze

  def dispatch req
    uri = req.full_uri

    # match uri or 404; stash the match data while we're at it
    _, methods = DISPATCH.detect { |test, _| test === uri.request_uri }

    # match method or 405
    handler = methods ?
      methods.fetch(req.request_method.to_sym, ERROR[405]) : STATIC

    # run the handler, whatever that may be
    resp = instance_exec(req, &handler).to_a
    # XXX what if there is not one of these? lol

    body = resp.last

    if body.is_a? Nokogiri::XML::Document
      if btag = body.at_xpath(
        '/html:html/html:head[1]/html:base[1]', RDF::SAK::Util::XPATHNS)
        buri = RDF::URI(btag['href'])
        if buri.authority == base.authority
          buri.scheme    = uri.scheme
          buri.authority = RDF::URI(uri.to_s).authority
          btag['href'] = buri.to_s
        end
      end
    end

    resp = Response[*resp.to_a]
    
    # XXX maybe nuke the body if it is a HEAD request? ehh i think
    # that happens already
    resp
  end

  public

  attr_reader :context

  # Initialize a new Web console.
  #
  # @param context [RDF::SAK::Context] let's just be lazy for now
  #
  def initialize context
    @context = context
  end

  # Returns the RDF graph (repository, not quad graph identifier).
  #
  # @return [RDF::Repository]
  #
  def graph
    @context.graph
  end

  # Returns the base URI (if configured, which it should be).
  #
  # @return [RDF::URI]
  #
  def base
    @context.base
  end

  # Returns the configured prefix mapping
  #
  # @return [Hash]
  #
  def prefixes
    @context.prefixes
  end

  # Run the response.
  #
  # @param env [Hash] the #Rack environment
  # 
  def call env
    # normalize the environment in the case of ssl tomfoolery
    env['HTTPS'] = 'on' if env.key? 'REQUEST_SCHEME' and
      env['REQUEST_SCHEME'].to_s.strip.downcase == 'https'
    req = Request.new env

    # here is where we would rewrite the request i guess

    # you know and maybe resolve it to an actual handler or something
    resp = dispatch req

    # here is where we would rewrite the response i guess

    # aand kick it out the door
    resp.finish
  end
end
