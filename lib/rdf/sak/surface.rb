require 'rdf/sak/version'

# A _surface_ is where {RDF::SAK::Representation representations} of
# _information resources_ are _projected_. The behaviour of a surface
# is intentionally opaque, and as such its main functionality is
# accessed through the `call` method. Whatever is on the other side of
# that is assumed to know what to do with it.
class RDF::SAK::Surface

  attr_reader context

  # Instantiate the surface.
  #
  # @param context [RDF::SAK::Context] the configuration context
  # @param options [Hash] dummy keyword options
  #
  def initialize context, **options
    @context = context
  end

  # No-op `call` that should be overridden in a subclass.
  def call *args, **options, &block
    raise NotImplementedError
  end

  # This is the document-root surface, intended for a very particular
  # flavour of static website generation.
  #
  # * Files are deposited in the root as `<uuid>.<ext>`.
  # * Three `RewriteMap`s are deposited alongside, respectively to
  #   handle rewrites, redirections, and `410 Gone` responses.
  # * Private files (how you determine access is up to you) are
  #   deposited in the private directory (which is configured by
  #   default to be "under" the root but could be elsewhere).
  # * Content negotiation (`Options +MultiViews` in Apache; whatever
  #   equivalent in `nginx` or IIS) is assumed to be enabled.
  #
  class DocumentRoot < Surface

    private

    PRIVATE = '.private'.freeze

    public

    def initialize context, dir: nil, private: PRIVATE, **options
      raise ArgumentError, 'dir must be string or Pathname' unless dir
      @dir = (dir.is_a? Pathname ? dir : Pathname(dir)).expand_path
      raise ArgumentError, "#{dir} must be a readable directory" unless
        @dir.directory? and dir.readable?
      # we deal with it this way because `private` is a keyword
      @private = @dir + binding.local_variable_get(:private)

      super
    end

    # Write a single resource to the document root.
    #
    # @param resource [RDF::SAK::Document, RDF::URI, URI, #to_s] the resource we
    #  want to write
    # @param published [true, false] whether to write the published
    #  version, if it exists
    # @param rehydrate [false, true] whether to run the rehydrate operation
    # @param rescan [false, true] whether to rescan the document
    # @param sponge [false, true] whether to sponge the RDFa into the graph
    #
    # @return [Array] the path(s) written to disk.
    #
    # @note The `rehydrate`, `rescan` and `sponge` parameters are
    #  probably unnecessary here.
    #
    # @note While we're at it, is that really the most sensible return value?
    #
    def write resource, published: true, rehydrate: false,
        rescan: false, sponge: false
      unless resource.is_a? RDF::SAK::Document
        begin
          resource = @context.visit resource
        rescue RDF::SAK::Source::NotAcceptable
          warn "No variant found for #{uri}"
          return
        end

        return unless resource
      end

      states = [false]
      states << true if published && resource.published?

      ok = []
      states.each do |state|
        target = state ? @dir : @private

        # XXX this only handles RDF::SAK::Document objects; we will
        # need to rethink this for the move to the
        # RDF::SAK::Representation regime (which should have a unified
        # interface for serialization no matter what the payload is).
        # This is fine for now though.

        doc = resource.transform(published: state, rehydrate: rehydrate,
          rescan: rescan, sponge: sponge) or next

        begin
          fh   = Tempfile.create('xml-', target)
          path = Pathname(fh.path)

          # write the doc to the target
          doc.write_to fh
          fh.close

          uuid = URI(resource.uuid.to_s)
          newpath = path.dirname + "#{uuid.uuid}.xml"
          ok << newpath

          # XXX do we wanna include umask??
          File.chmod 0644, path
          File.rename path, newpath
          File.utime resource.mtime, resource.mtime, newpath
        rescue Exception => e
          # XXX this should only rescue a specific class of errors
          # XXX ps do something more intelligent here
          warn e.class, e
          File.unlink path if path.exist?
        end
      end
    end

    def call *args, **options, &block
      # get all the documents

      # write each one out
      # call the block
    end
  end

  # This is literally a rack app.
  class Rack < Surface
  end
end
