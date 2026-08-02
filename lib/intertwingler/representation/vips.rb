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

  def parse io
    # warn "hurrr #{io.inspect}"
    if io.respond_to?(:stat) && io.stat.file? && io.respond_to?(:fileno)
      # seek and ye shall find
      io.seek 0 if io.respond_to? :seek

      # if there's a file descriptor just use it, don't screw around
      src = ::Vips::Source.new_from_descriptor io.fileno
    else
      # this is weird
      src = ::Vips::SourceCustom.new
      src.on_read do |len|
        # warn "reading #{len} bytes"
        io.read len
      end

      src.on_seek do |offset, whence|
        # warn "seeking #{offset} #{whence}"
        io.seek offset, whence
      end
    end

    ::Vips::Image.new_from_source src, ''
  end

  def serialize obj, target = tempfile
    fd_ok = target.respond_to?(:stat) &&
      target.stat.file? && target.respond_to?(:fileno)

    if fd_ok
      # warn "got here wtf lolol #{target} #{target.fileno}"
      tgt = ::Vips::Target.new_to_descriptor target.fileno
    else
      # warn "okay i'm here on the custom target"
      tgt = ::Vips::TargetCustom.new

      tgt.on_write do |bytes|
        # warn "sup #{bytes.size} bytes"
        target.write bytes
      end

      tgt.on_finish do
        # warn "sup done lol"
        if fd_ok && target.respond_to?(:fsync)
          target.fsync
        elsif target.respond_to? :flush
          target.flush rescue nil
        end
      end
    end

    # warn target.inspect

    # warn "type: #{type}, extensions: #{type.extensions}"

    # warn type.extensions.first
    ext = type.extensions.first
    # ext = 'heif'

    obj.write_to_target tgt, ".#{ext}"

    # culta da cargo
    # target.fsync rescue nil  if target.respond_to? :fsync
    # target.flush rescue nil  if target.respond_to? :flush
    # target.seek 0 rescue nil if target.respond_to? :seek

    target
  end

  public

end
