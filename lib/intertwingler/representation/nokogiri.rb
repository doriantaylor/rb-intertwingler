require 'intertwingler/representation'

# require 'intertwingler/document' # this will also import nokogiri

require 'nokogiri'
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
    io.rewind rescue nil if io.respond_to? :rewind

    out
  end

  def serialize obj, target = tempfile
    # warn "serializing for #{caller.inspect}"
    # warn "serializing #{obj.inspect} to #{target.inspect}"
    html = /html/i.match?(type.to_s)
    html ? obj&.root&.namespace&.href == 'http://www.w3.org/1999/xhtml' ?
      obj.write_xhtml_to(target) :
      obj.write_html_to(target) :
      obj.write_to(target)

    target.rewind rescue nil if target.respond_to? :rewind

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

end

