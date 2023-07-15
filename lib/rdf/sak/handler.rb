require 'rack/request'
require 'rack/response'

# for generated
require 'rdf/sak/document'
require 'stringio'
require 'uri'

# for filesystem
require 'rdf/sak/mimemagic'
require 'http-negotiate'

# for cas
require 'store/digest/http'

class RDF::SAK::Handler
  def initialize resolver, **args
    @resolver = resolver
  end

  attr_reader :resolver

  # Get the resolver's graph
  #
  # @return [RDF::Repository] the graph.
  #
  def repo
    @resolver.repo
  end

  # Handle a {Rack::Request}. Return a {Rack::Response}.
  #
  # @param req [Rack::Request] the request.
  #
  # @return [Rack::Response] the response.
  #
  def handle req
    raise NotImplementedError, 'Subclasses must implement `handle`'
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
    # @param resolver [RDF::SAK::Resolver] the URI resolver
    # @param root [Pathname, #to_s] the document root
    # @param indices [Array<#to_s>] slugs to use for directory index
    #
    def initialize resolver, root: nil, indices: %w[index].freeze
      @root    = Pathname(root).expand_path.realpath
      @indices = indices

      super resolver
    end

    attr_reader :root, :indices

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

      # determine if the requested path terminates with a slash (~ parameters)
      slash = resolver.slash? req.path

      path = resolver.clean_path(req.path, slash: false).delete_prefix ?/

      # preemptively check if the request-uri is /{uuid}, otherwise get uuid
      is_uuid = !!(uuid = resolver.uuid_path path, as: :uri)
      uuid ||= resolver.uuid_for path, as: :uri

      paths = []

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

      # we'll just make a big chonkin' hash of variants which we can
      # use for the negotiation and afterwards
      variants = paths.reduce({}) do |h, p|
        re = /^#{Regexp.quote root.to_s}\//o

        # don't do this if this is the root
        unless r = p == root
          dn, bn = p.split
          dn.glob("#{bn}{,.*}").each do |x|
            if stat = x.stat rescue nil
              type = RDF::SAK::MimeMagic.by_path(x).to_s
              incl = re.match? x.realpath.to_s
              h[x] = { dir: false, stat: stat, type: type, included?: incl }
            end
          end
        end

        @indices.each do |i|
          p.glob("#{i}{,.*}").each do |x|
            if stat = x.stat rescue nil
              type = RDF::SAK::MimeMagic.by_path(x).to_s
              incl = re.match? x.realpath.to_s
              h[x] = { dir: true, stat: stat, type: type, included?: incl }
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
        if val[:dir]
        else
        end

        # this i thought was clever: you demote the variant to
        # oblivion so if it gets selected anyway you know to return a
        # 403 rather than eliminating it and having to return 404
        ok = stat.file? and stat.readable? and val[:included?]
        qs /= 100.0 unless ok

        val.merge({ weight: qs, size: stat.size, mtime: stat.mtime, ok: ok })
      end

      # now we actually perform the negotiation and get our selected variant
      if selected = HTTP::Negotiate.negotiate(req, variants)
        var = variants[selected]

        warn paths.inspect
        warn selected

        # test if readable
        return Rack::Response[403, {}, []] unless var[:ok]

        # test if uri matches requested
        # redirect if requested uri was not just a uuid

        # test mtime

        return Rack::Response[200, {
          'Content-Type'   => var[:type],
          'Content-Length' => var[:size].to_s, # rack should do this
          'Last-Modified'  => var[:mtime].mtime.httpdate,
        }, selected.open]
      end

      # there were variants but none were chosen so 406
      Rack::Response[406, {}, []]
    end

  end

  class Generated < self

    def handle req

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
        'Content-Type' => 'text/plain',
      }, ['lol fail']] unless subject

      # okay now we see if there are any sub-handlers that will take this request

      # types  = repo.types_for subject
      # strata = repo.type_strata types

      # otherwise we fall back to the main handler

      doc = RDF::SAK::Document.generate_doc resolver, subject

      # XXX nuke this later
      if base = doc.at_xpath('/html:html/html:head/html:base',
        { html: 'http://www.w3.org/1999/xhtml' })
        href = RDF::URI(base['href'])
        href.scheme = orig.scheme
        href.authority = orig.authority
        base['href'] = href.to_s
      end

      str = doc.to_xml

      # warn 'ouate de phoque'

      Rack::Response[200, {
        'Content-Type'   => 'application/xhtml+xml',
        'Content-Length' => str.b.length.to_s,
      }, StringIO.new(str, ?r)]
    end
  end
end
