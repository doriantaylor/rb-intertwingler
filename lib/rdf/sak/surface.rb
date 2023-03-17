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

    def initialize context, dir: nil, private_dir: PRIVATE, **options
      raise ArgumentError, 'dir must be string or Pathname' unless dir
      @dir = (dir.is_a? Pathname ? dir : Pathname(dir)).expand_path
      raise ArgumentError, "#{dir} must be a readable directory" unless
        @dir.directory? and dir.readable?
      @private = @dir + private_dir

      super
    end

    # Write a single URI to the document root.
    def write uri, published: true
      begin
        doc = @context.visit uri
      rescue RDF::SAK::Source::NotAcceptable
        return
      end

      doc = doc.transform
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
