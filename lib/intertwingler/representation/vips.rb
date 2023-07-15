require 'intertwingler/representation'
require 'vips'
require 'vips/sourcecustom'
require 'vips/targetcustom'
require 'stringio'

class Intertwingler::Representation::Vips < Intertwingler::Representation
  private

  OBJECT_CLASS = Vips::Image

  public

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

  def each &block
    io.each block
  end

  def object
    # if it isn't already parsed, we parse it
    unless @object
      # this is weird
      #src = ::Vips::SourceCustom.new
      #src.on_read { |len| warn "reading #{len} bytes"; @io.read len }
      #src.on_seek { |offset, whence| warn "seeking #{offset} #{whence}"; @io.seek offset, whence }
      src = ::Vips::Source.new_from_descriptor @io.to_i

      # XXX i imagine this can crash

      # whatever
      @object = ::Vips::Image.new_from_source src, ''
    end

    # now return it
    @object
  end
end
