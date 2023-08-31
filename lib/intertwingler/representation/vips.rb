require 'intertwingler/representation'

require 'vips'
require 'vips/sourcecustom'
require 'vips/targetcustom'

require 'stringio'

class Intertwingler::Representation::Vips < Intertwingler::Representation
  private

  OBJECT_CLASS = ::Vips::Image

  DEFAULT_TYPE = 'image/png'.freeze
  VALID_TYPES  = %w[application/pdf] +
    %w[avif gif heic heif jpeg jp2 jxl png tiff webp x-portable-anymap
           x-portable-bitmap x-portable-graymap x-portable-pixmap].map do |t|
    "image/#{t}".freeze
  end.freeze

  public

  def each &block
    io.each(&block)
  end

  def io
    # just return the input unless something has been done to it
    return @io unless @object

    # screw it we'll blit to memory and do stringio for now
    # target = ::Vips::TargetCustom.new
    # string = ''.b # binary string lol
    # target.on_write { |bytes| string << bytes }
    target = ::Vips::Target.new_to_memory

    @object.write_to_target target, ".#{type.extensions.first}"
    # this is weird like what about race conditions or something
    string = target.get 'blob'

    StringIO.new string
  end

  def object
    # if it isn't already parsed, we parse it
    unless @object
      if @io.respond_to? :fileno and @io.fileno
        # if there's a file descriptor just use it, don't screw around
        src = ::Vips::Source.new_from_descriptor @io.fileno
      else
        # this is weird
        src = ::Vips::SourceCustom.new
        src.on_read do |len|
          warn "reading #{len} bytes"
          @io.read len
        end

        src.on_seek do |offset, whence|
          warn "seeking #{offset} #{whence}"
          @io.seek offset, whence
        end
      end

      # XXX i imagine this can crash

      # whatever
      @object = ::Vips::Image.new_from_source src, ''
    end

    # now return it
    @object
  end
end
