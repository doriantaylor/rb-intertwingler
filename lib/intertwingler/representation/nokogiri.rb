require 'intertwingler/representation'

require 'intertwingler/document' # this will import nokogiri

require 'stringio'

class Intertwingler::Representation::Nokogiri < Intertwingler::Representation
  private

  OBJECT_CLASS = ::Nokogiri::XML::Node
  DEFAULT_TYPE = 'application/xml'.freeze
  VALID_TYPES  = %w[text/html application/xml].freeze

  public

  # 
  def each &block
    io.each(&block)
  end

  def io
    return @io unless @object

    out = StringIO.new '', 'w+'

    @object.write_to out

    out.seek 0

    out
  end

  def object
    unless @object
      io.seek 0 if io.respond_to? :seek
      @object = Intertwingler::Document.coerce_doc io
    end

    @object
  end
end
