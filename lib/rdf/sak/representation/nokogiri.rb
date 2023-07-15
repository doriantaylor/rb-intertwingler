require 'rdf/sak/representation'
require 'rdf/sak/document'
require 'stringio'

class RDF::SAK::Representation::Nokogiri < RDF::SAK::Representation
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
      @object = RDF::SAK::Document.coerce_doc io
    end

    @object
  end
end
