require 'intertwingler/representation'
require 'intertwingler/document'
require 'stringio'

class Intertwingler::Representation::Nokogiri < Intertwingler::Representation
  private

  OBJECT_CLASS = Nokogiri::XML::Node

  public

  def io
    return @io unless @object

    out = StringIO.new

    @object.write_to out

    out
  end

  def object
    unless @object
      @object = Intertwingler::Document.coerce_doc io
    end

    @object
  end
end
