require 'rdf/sak/mimemagic'

require 'http-negotiate'
#
require 'store-digest'

# A _source_ is where (opaque) representations of information
# resources originate, either in whole or in part (from the point of
# view of this system). The most familiar kind of source is the file
# system. Other species of sources include content addressable
# storage, as well as version control, and reverse proxies. A source
# (at least in this context) is a read-only component with two salient
# functions:
#
# * `locate`: given a URI and a set of `Accept-*` headers, perform
#   content negotiation and return the "best" internal locator.
#
# * `visit`: return an IO object associated with the locator, along
#   with at least the following metadata:
#   * content (aka MIME) type
#   * modification time (or something that can pass for it)
#
# Other sources may elect to include other metadata.
#
class RDF::SAK::Source
  # A {NotAcceptable} error is intended to be raised when content
  # negotiation fails, i.e., when at least one variant is found in the
  # source but the rules preclude selecting any one of them. This is
  # to distinguish from the variant not being found at all. This error
  # is named after and intended to correspond to the HTTP response
  # 406, and so bears the same name, even though a name like
  # "negotiation failed" or even "no proposal chosen" would be more
  # descriptive.
  class NotAcceptable < RuntimeError
    # XXX do we want to do anything cute in here??
  end

  # Initialize the source.
  #
  # @param resolver [RDF::SAK::Resolver] the URI resolver
  # @param options [Hash] catch-all for keyword options
  #
  def initialize resolver, **options
    @resolver = resolver
  end

  # Locate the "best" resource representation for a given URI/header
  # set. Returns `nil` if a URI can't be found. Otherwise, it will
  # return either the internal URI that positively identifies the
  # representation, or the pair of this plus the content-type. Raises
  # {NotAcceptable} if variants are found but not selected.
  #
  # @param uri [URI, RDF::URI, #to_s] the URI (will be coerced)
  # @param headers [Hash, Rack::Request, #env, #to_h] a header set
  # @param pair [false, true] whether to return a pair including the
  #  content-type of the selected variant or just the variant itself.
  #
  # @raise [NotAcceptable] The operation may fail to select a variant.
  #
  # @note There is currently no concept of access control in sources;
  #  the requestor is assumed to be permitted to retrieve the content.
  #
  # @return [nil, URI, (URI, String)] URI or URI/content-type pair
  #
  def locate uri, headers: {}, pair: false
    raise NotImplementedError
  end

  # Return a minimalist structure containing information suitable for
  # downstream processing, including the modification time, the
  # content type, and the content {IO} object itself. Keys include but
  # are not limited to:
  #
  # * `:content` — an {IO} object containing the payload,
  # * `:mtime` — the modification time (as a {Time} object),
  # * `:type` — the content-type, as a string.
  #
  # Other drivers may include other metadata in their results, like
  # the (natural) language, the encoding (compression), and character
  # set (where applicable; not to be confused with encoding).
  #
  # @param uri [URI, RDF::URI, #to_s] the URI (will be coerced)
  # @param headers [Hash, Rack::Request, #env, #to_h] a header set
  #
  # @raise [NotAcceptable] The operation may fail to select a variant.
  #
  # @note We may eventually move to something like {Dry::Types} for
  #  the return value to impose some validation on it, but not quite yet.
  #
  # @return [Hash] a hash containing the information described above.
  #
  def visit uri, headers: {}
    raise NotImplementedError
  end

  class FileSystem < RDF::SAK::Source
    # XXX THIS MIGHT NEED TO BE TUNED
    HEADERS = {
      'Accept' => %w[
        application/xhtml+xml application/xml text/html;q=0.8
        text/markdown;q=0.5 */*;q=0.1].join(', ').freeze
    }.freeze

    def initialize resolver, dir: nil, **options
      # coerce dir to pathname
      raise ArgumentError, 'root directory must be defined' unless dir
      @dir = Pathname(dir).expand_path
      raise ArgumentError, 'dir must be a readable directory' unless
        dir.readable? and dir.directory?

      # XXX do we want a default language?

      super
    end

    def locate uri, headers: HEADERS, pair: false
      uri  = @resolver.coerce_resource uri
      base = @resolver.base

      tu = URI(uri.to_s) # copy of uri for testing content
      unless tu.scheme == 'urn' and tu.nid == 'uuid'
        raise "could not find UUID for #{uri}" unless
          uuid = @resolver.uuid_for(uri)
        tu = URI(uri = uuid)
      end

      # xxx bail if the uri isn't a subject in the graph

      candidates = [@dir + tu.uuid]

      # try all canonical URIs
      (@resolver.uri_for uri, scalar: false, slugs: true).each do |u|
        # warn u.inspect
        u = URI(u.to_s)
        # warn "#{u.hostname} #{base.hostname}".inspect
        next unless u.hostname == base.hostname
        p = CGI.unescape u.path[/^\/*(.*?)$/, 1]
        candidates.push(@dir + p)
      end

      variants = candidates.uniq.map do |c|
        Pathname.glob(c.to_s + '{,.*,/index{,.*}}')
      end.reduce(:+).select { |x| x.file? && x.readable? }.map do |x|
        [x, { type: RDF::SAK::MimeMagic.by_path(x).to_s, size: x.size }]
      end.to_h

      # XXX in some future we can imagine telling the difference
      # between strictly nonexistent and not readable
      return if variants.empty?

      # et voila
      chosen = HTTP::Negotiate.negotiate(headers, variants) or
        raise NotAcceptable
      # XXX do we wanna make this optional? or do something else?
      pair ? [chosen, variants[chosen][:type]] : chosen
    end

    def visit uri, headers: {}
      path, type = locate(uri, headers: headers)
      return unless path

      mtime = path.mtime

      # XXX i suppose this could fail
      io = path.open

      # this is the minimum set that should be returned
      return {
        content:  io,
        mtime:    mtime,
        type:     type,
        # charset:  charset,
        # encoding: encoding,
        # language: lang,
      }
    end
  end

  # XXX break this out
  class ContentAddressable < RDF::SAK::Source
    def initialize resolver, store: nil, **options
      @store = store || Store::Digest.new(**options)

      super
    end
  end

  # XXX TODO LOL
  class VersionControl < RDF::SAK::Source
    # URI could have branch and version parameters?

    # struct returned from visit could have vcs-specific metadata

    class Git < VersionControl
    end
  end

  # XXX ReverseProxy

end
