require 'intertwingler/representation'

require 'intertwingler/document' # this will also import nokogiri

# require 'nokogiri'
require 'stringio'

class Intertwingler::Representation::Nokogiri < Intertwingler::Representation
  private

  OBJECT_CLASS = ::Nokogiri::XML::Node
  DEFAULT_TYPE = 'application/xml'.freeze
  VALID_TYPES  = %w[text/html application/xml].freeze

  def tempfile
    StringIO.new +''
  end

  def parse io
    warn "parsing #{io.class} #{io.inspect}"

    # rewind going in
    io.rewind if io.respond_to? :rewind

    html = /^text\/html/i.match?(type.to_s)
    parser = html ? ::Nokogiri::HTML5::Document : ::Nokogiri::XML::Document
    # defaults are sane i think
    out = parser.parse io

    # rewind going out
    io.rewind if io.respond_to? :rewind

    out
  end

  def serialize obj, target
    # warn "serializing for #{caller.inspect}"
    # warn "serializing #{obj.inspect} to #{target.inspect}"
    html = /html/i.match?(type.to_s)
    html ? obj&.root&.namespace&.href == 'http://www.w3.org/1999/xhtml' ?
      obj.write_xhtml_to(target) :
      obj.write_html_to(target) :
      obj.write_to(target)

    target
  end

  public

  def inspect
    if root = object.root
      root = "<#{root.name}>"
    else
      root = root.inspect
    end
    "<#{self.class} type: #{type}, object: #{object.class} (#{root})>"
  end

  # def each &block
  #   io.each(&block)
  # end


  # def parse io
  #   if io.respond_to? 

  # def io
  #   return @io unless @object
  #   # XXX we want this to be
  #   out = StringIO.new ''.b, 'wb+'

  #   @object.write_to out

  #   out.seek 0

  #   @io = out
  # end

  # def object
  #   unless @object
  #     io.seek 0 if io.respond_to? :seek
  #     @object = Intertwingler::Document.coerce_doc io
  #   end

  #   @object
  # end
end
