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
    if io.respond_to? :fileno and io.fileno
      # seek and ye shall find
      io.seek 0 if io.respond_to? :seek

      # if there's a file descriptor just use it, don't screw around
      src = ::Vips::Source.new_from_descriptor io.fileno
    else
      # this is weird
      src = ::Vips::SourceCustom.new
      src.on_read do |len|
        warn "reading #{len} bytes"
        io.read len
      end

      src.on_seek do |offset, whence|
        warn "seeking #{offset} #{whence}"
        io.seek offset, whence
      end
    end

    ::Vips::Image.new_from_source src, ''
  end

  def serialize obj, target
    if target.respond_to? :fileno and target.fileno
      tgt = ::Vips::Target.new_to_descriptor target.fileno
    else
      tgt = ::Vips::TargetCustom.new
      tgt.on_write { |bytes| target << bytes }
      tgt.on_finish do
        if target.respond_to? :fsync
          target.fsync
        elsif target.respond_to? :flush
          target.flush
        end
      end
    end

    obj.write_to_target tgt, ".#{type.extensions.first}"

    # culta da cargo
    target.fsync  if target.respond_to? :fsync
    target.flush  if target.respond_to? :flush
    target.seek 0 if target.respond_to? :seek

    target
  end

  public

end
