require 'rack/request'
require 'rack/response'

# for generated
require 'intertwingler/document'
require 'stringio'
require 'uri'

# for filesystem
require 'mimemagic'
require 'http-negotiate'

# for cas
require 'store/digest/http'

class Intertwingler::Handler
  # do this to declare the symbol
  class ::Intertwingler::Engine < self
  end

  # This is the abstract parent Exception class that acts as an escape
  # hatch for responses that are something _other_ than 200-series,
  # i.e. they are not-successful (albeit not strictly _un_successful)
  # responses.
  class NotSuccess < Exception
    def initialize message, status: nil
      @status = status

      super message
    end

    attr_reader :status
    alias_method :code, :status

    def response
      Rack::Response[status, { 'content-type' => 'text/plain' }, [message]]
    end
  end

  # Redirects are an example of not-successful-yet-not-unsuccessful responses.
  class Redirect < NotSuccess
    # Make a new redirect "exception"
    #
    # @param message [#to_s] the error message
    # @param status [Integer] the response code
    # @param location [URI, RDF::URI, #to_s, nil]
    # @param as [:uri, :rdf] URI coercion type
    #
    def initialize message, status: nil, location: nil, as: :uri
      @location =
        Intertwingler::Resolver.coerce_resource location, as: as if location
      super message, status || 302
    end

    attr_reader :location

    def response
      hdr = {}
      hdr['Location'] = location.to_s if location
      Rack::Response[status, hdr, [message]]
    end
  end

  class Error < NotSuccess
    class Client < self
    end
    class Server < self
    end
  end

  # Handle a {Rack::Request}. Return a {Rack::Response}.
  #
  # @param req [Rack::Request] the request.
  #
  # @return [Rack::Response] the response.
  #
  def handle req
    raise NotImplementedError, 'Subclasses must implement their own `handle`'
  end

  # Handle a Rack request from the wire.
  #
  # @param env [Hash, Rack::Request] the Rack environment or request.
  #
  # @return [Array<(Integer, Hash, #each)>] the response.
  #
  def call env
    # XXX maybe wrap this or put it in a base class i dunno
    req = env.is_a?(Rack::Request) ? env : Rack::Request.new(env)

    handle(req).finish
  end

  # Initialize a handler.
  #
  # @param engine [Intertwingler::Engine]
  # @param args [Hash{Symbol => Object}]
  #
  def initialize engine, **args
    raise ArgumentError, 'engine must be an Intertwingler::Engine' unless
      engine.is_a? ::Intertwingler::Engine
    @engine = engine
  end

  attr_reader :engine

  # Get the {Intertwingler::Resolver} for the given request.
  #
  # @param req [Rack::Request, URI, RDF::URI] the request (URI).
  #
  # @return [Intertwingler::Resolver, nil] the resolver, maybe
  #
  def resolver_for req
    @engine.resolver_for req
  end

  # Get the resolver's graph for the given request.
  #
  # @param req [Rack::Request, URI, RDF::URI] the request (URI).
  #
  # @return [RDF::Repository] the graph.
  #
  def repo_for req
    @engine.repo_for req
  end

  # This is a toy content handler for serving content-negotiated files
  # directly from the file system. It is intended to be for residual
  # interoperability with long-standing expectations about how a
  # website ought to work, eventually to be supplanted by more
  # sophisticated methods of storing opaque resources. This handler
  # only responds to `GET` and `HEAD` methods, as it assumes to be
  # situated in an ecosystem where other request methods are handled
  # by other handlers. This handler does limited resolution to and
  # from request-URIs of the form `/{uuid}`, and will likewise perform
  # limited redirections from non-canonical request-URIs to canonical
  # ones (except if the original request is `/{uuid}`). The
  # `If-Modified-Since` request header will be honoured with a `304
  # Not Modified` response when appropriate. Content negotiation is
  # similar to Apache's `mod_negotiation`, insofar as content types
  # are derived from (possibly multiple) file extensions. Where this
  # handler departs from Apache is that it will serve (a negotiated)
  # `/dir.ext` (i.e., `/dir` with no trailing slash) in the presence
  # of a `/dir/index.ext`, while Apache does the opposite.
  #
  # This content handler provides rudimentary security in the form of
  # checking that symbolic links resolve inside the document root, and
  # declining to serve dotfiles (files that begin with a period `.`).
  # Otherwise, ordinary files system permissions apply. This handler
  # does not generate directory indexes.
  #
  # Error responses are expected to be handled downstream, though this
  # handler will produce the correct ones: 403 for attempts to request
  # unreadable files 405 for methods other than `GET` or `HEAD`, 406
  # for incompatible `Accept*` headers to variants, and 404 when no
  # variants can be found. If a resource is present in the graph but
  # not on the file system, the handler can be configured to return
  # `410 Gone` (though this may conflict with attempts to poll other
  # content handlers). The error response bodies are minimal, intended
  # for debugging purposes.
  #
  class FileSystem < self

    # Initialize a handler with parameters.
    #
    # @param resolvers [Array<Intertwingler::Resolver>] the URI resolver(s)
    # @param roots [Pathname, #to_s, Array<Pathname, #to_s>] the document root(s)
    # @param indices [#to_s, Array<#to_s>] slugs to use for directory index
    #
    def initialize engine, roots: nil, indices: %w[index].freeze
      # coerce document root(s)
      @roots = (roots.respond_to?(:to_a) ? roots.to_a : [roots]).map do |r|
        Pathname(r).expand_path.realpath
      end

      @indices = indices.respond_to?(:to_a) ? indices.to_a : [indices]

      super engine
    end

    attr_reader :roots, :indices

    # XXX do we wanna do method methods? is this dumb?
    # def GET req
    # end

    # Handle the request.
    #
    # @param req [Rack::Request] the request
    #
    # @return [Rack::Response] the response
    #
    def handle req
      # XXX do the thing i said on stream about mapping request
      # methods to actual methods so you don't have to keep typing the
      # 405 thing

      # step zero return 405 unless GET or HEAD (or OPTIONS but that
      # is a special case)
      return Rack::Response[405, {}, []] unless
        %w[HEAD GET].include? req.request_method

      # basically what we want is this thing to do as little work as
      # it can get away with since most of the URI resolution will be
      # done upstream, but still be robust enough to run as a
      # standalone content-negotiating static filesystem handler, but
      # also not heavily duplicate any redirection or access control
      # behaviour. that said, it should not follow symlinks outside
      # the document root, or try to serve raw directories, or things
      # like dotfiles that would otherwise be readable.

      # general strategy is to build up a list of candidates and then
      # eliminate them

      # * we start with the actual URI that was requested, which may
      #   also be the UUID (or at least *a* UUID)
      # * then we get the UUID (if we didn't have it already)
      # * then we get the subset of `uri_for` on this scheme/authority
      #   (that we don't already have)

      resolver = resolver_for req

      # determine if the requested path terminates with a slash (~ parameters)
      slash = resolver.slash? req.path

      path = resolver.clean_path(req.path, slash: false).delete_prefix ?/

      # preemptively check if the request-uri is /{uuid}, otherwise get uuid
      is_uuid = !!(uuid = resolver.uuid_path path, as: :uri)
      uuid ||= resolver.uuid_for path, as: :uri

      paths = []

      roots.each do |root|
        if uuid
          paths << root + uuid.uuid
          paths << root + path
          paths += resolver.uri_for(uuid, scalar: false, as: :uri,
                                  slugs: true, fragments: false, local: true).reduce([]) do |a, u|
            next a if resolver.uuid_path u
            a << root + resolver.clean_path(u, slash: false).delete_prefix(?/)
          end
          paths.uniq!
        else
          # who knows maybe there's a thing on the file system
          # XXX maybe make this verboten if it's not in the graph??
          paths << root + path
        end
      end

      re = /^#{roots.map { |r| Regexp.quote r.to_s }.join ?|}\//o

      # we'll just make a big chonkin' hash of variants which we can
      # use for the negotiation and afterwards
      variants = paths.reduce({}) do |h, p|

        # don't do this if this is the root
        unless r = roots.include?(p)
          dn, bn = p.split
          dn.glob("#{bn}{,.*}").each do |x|
            if stat = x.stat rescue nil
              next if stat.directory?
              type = MimeMagic.by_path(x).to_s
              incl = re.match? x.realpath.to_s
              h[x] ||= { dir: false, stat: stat, type: type, included?: incl }
            end
          end
        end

        @indices.each do |i|
          p.glob("#{i}{,.*}").each do |x|
            if stat = x.stat rescue nil
              next if stat.directory?
              type = MimeMagic.by_path(x).to_s
              incl = re.match? x.realpath.to_s
              h[x] ||= { dir: true, stat: stat, type: type, included?: incl }
            end
          end
        end unless !r and resolver.uuid? bn

        h
      end

      # if there are no variants then this is a genuine 404
      return Rack::Response[404, {}, []] if variants.empty?

      # okay now subsequently process the variants
      variants.transform_values! do |val|
        stat = val[:stat]
        qs = 1.0

        # the perl CatalystX::Action::Negotiate one does some
        # twiddling here; not sure if i wanna copy it
        # if val[:dir]
        # else
        # end

        # this i thought was clever: you demote the variant to
        # oblivion so if it gets selected anyway you know to return a
        # 403 rather than eliminating it and having to return 404
        ok = stat.file? and stat.readable? and val[:included?]
        qs /= 100.0 unless ok

        val.merge(
          { weight: qs, size: stat.size, mtime: stat.mtime.getgm, ok: ok })
      end

      # warn variants

      # now we actually perform the negotiation and get our selected variant
      if selected = HTTP::Negotiate.negotiate(req, variants)
        var = variants[selected]

        # warn paths.inspect
        # warn selected

        # test if readable
        return Rack::Response[403, {}, []] unless var[:ok]

        # test if uri matches requested
        # redirect if requested uri was not just a uuid

        # test mtime
        if ims = req.get_header('HTTP_IF_MODIFIED_SINCE')
          ims = (Time.httpdate(ims) rescue Time.at(0)).getgm
          # warn "mtime: #{var[:mtime]} (#{var[:mtime].to_i}), IMS: #{ims} (#{ims.to_i}), lt: #{var[:mtime] < ims}, cmp: #{var[:mtime] <=> ims}"
          # return not modified if the variant is *older* than ims
          # XXX TIL Time objects can be equal but not
          return Rack::Response[304, {}, []] if var[:mtime].to_i <= ims.to_i
        end

        return Rack::Response[200, {
          'content-type'   => var[:type],
          'content-length' => var[:size].to_s, # rack should do this
          'last-modified'  => var[:mtime].httpdate,
        }, selected.open]
      end

      # there were variants but none were chosen so 406
      Rack::Response[406, {}, []]
    end

  end

  class Generated < self

    def handle req

      resolver = resolver_for(req) or return Rack::Response[404, {}, ['not found lol']]

      # warn req.url.inspect
      # warn resolver.base.inspect

      uri = RDF::URI(req.url)

      orig = uri.dup

      # XXX lol
      uri.authority = resolver.base.authority if
        /(spigot|localhost):9292/i.match? uri.authority
      uri.scheme = 'https' if uri.scheme == 'http'

      # warn uri

      # resolve subject
      subject = resolver.uuid_for uri

      # bail out if this doesn't return anything

      return Rack::Response[404, {
        'content-type' => 'text/plain',
      }, ['lol fail']] unless subject

      # okay now we see if there are any sub-handlers that will take this request

      # types  = repo.types_for subject
      # strata = repo.type_strata types

      # otherwise we fall back to the main handler

      doc = Intertwingler::Document.generate_doc resolver, subject

      # XXX nuke this later
      if base = doc.at_xpath('/html:html/html:head/html:base',
                             { html: 'http://www.w3.org/1999/xhtml' })
        href = RDF::URI(base['href'])
        href.scheme = orig.scheme
        href.authority = orig.authority
        base['href'] = href.to_s
      end

      str = doc.to_xml.b

      # warn 'ouate de phoque'

      Rack::Response[200, {
        'content-type'   => 'application/xhtml+xml',
        'content-length' => str.length.to_s,
      }, StringIO.new(str, ?r, encoding: Encoding::BINARY)]
    end
  end

  class ContentAddressable < self
    def initialize engine
    end
  end
end
