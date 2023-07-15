require 'intertwingler/version'
require 'set'
require 'descriptive_statistics'
require 'nokogiri'

class Intertwingler::DocStats < Nokogiri::XML::SAX::Document
  private

  MAYBE  = %i[dt dd li td th caption figcaption]
  SKIP   = %i[html head title base link meta script]
  BLOCKS = Set.new(%i[body p h1 h2 h3 h4 h5 h6 ul ol pre dl main header footer
    article section aside figure nav div noscript blockquote form hr
    table fieldset address] + MAYBE).freeze
  SECTIONS = Set.new(%i[body article section]).freeze
  IMAGES   = Set.new(%i[img picture]).freeze
  VIDEOS   = Set.new(%i[video]).freeze
  EMBEDS   = Set.new(%i[embed object iframe])
  COUNTS = {
    sections: %i[body article section header footer nav aside],
    images:   %i[img picture],
    videos:   %i[video],
    embeds:   %i[embed object iframe],
    tables:   %i[table],
    lists:    %i[ul ol dl],
    forms:    %i[form],
    scripts:  %i[script],
    sheets:   %i[style],
  }.transform_values { |v| Set.new v }.freeze
  
  
  NODEXP = '/html:html/html:body[not(*)]|/html:html/html:body//*[not(*)]'.freeze
  XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze
  XPATHNS = { html: XHTMLNS }.freeze

  # ok listen up fools here is the new html document stats algo:

  # okay we want to count characters, words, blocks, and sections, as
  # well as gather stats on words per block (and probably blocks per section)

  # the problem is we don't want to count blocks that only contain other blocks

  # we also don't want to count the text of sub-blocks in a superordinate block

  # there are also quasi-blocks that we may not ordinarily count,
  # except if they themselves contain two or more adjacent
  # blocks. (examples: li, th/td, h1-6, caption/figcaption)

  # count the block only if it contains text and inline elements (and
  # only count the text and inline elements)

  # if 

  # we can also 

  # use xpath to find all the leaf node elements
  # 

  def pretend_sax node
    case node.type
    when Nokogiri::XML::Node::DOCUMENT_NODE
      # if node is a document run begin and end document and then run
      # for children
      start_document
      node.children.each { |c| pretend_sax c }
      end_document
    when Nokogiri::XML::Node::ELEMENT_NODE
      # if node is an element run begin and end element and run for children
      prefix, uri = if ns = node.namespace
                      [ns.prefix, ns.href]
                    end
      ns    = node.namespace_scopes.map { |n| [ns.prefix, ns.href] }
      attrs = node.attribute_nodes.map do |a|
        an = a.name
        an = "#{a.namespace.prefix}:#{an}" if
          a.namespace and a.namespace.prefix
        [an, a.content]
      end
      start_element_namespace node.name, attrs, prefix, uri, ns
      node.children.each { |c| pretend_sax c }
      end_element_namespace node.name, prefix, uri
    when Nokogiri::XML::Node::TEXT_NODE
      characters node.content
    when Nokogiri::XML::Node::CDATA_SECTION_NODE
      cdata_block node.content
    end
  end

  def do_block name
    if BLOCKS.include? name.to_sym
      w = @text.strip.split
      t = w.join ' '

      unless w.empty?
        words = w.length
        @counts[:chars]  += t.length
        @counts[:words]  += words
        @counts[:blocks] += 1
        @wpb   << words
        @stack << t
        @text  = ''
      end
    end
  end

  def clear_text
    @text = ''
  end
  
  public

  attr_reader :chars, :words, :blocks

  def start_element_namespace name, attrs = [], prefix = nil, uri = nil, ns = []
    unless uri != XHTMLNS or SKIP.include? name.to_sym
      @on = true 
      do_block name
    end
  end

  def end_element_namespace name, prefix = nil, uri = nil
    if uri == XHTMLNS
      SKIP.include?(name.to_sym) ? clear_text : do_block(name)
      COUNTS.each do |type, set|
        @counts[type] += 1 if set.include? name.to_sym
      end
      @counts[:sections] -= 1 if name == 'body'
      @on = false if name == 'body'
    end
  end

  def characters string
    @text += string if @on
  end

  def cdata_block string
    characters string
  end

  # @return [Float] mean of words per block
  def mean
    @wpb.mean
  end

  # @return [Float] standard deviation of words per block
  def sd
    @wpb.standard_deviation
  end

  # @return 
  def quartiles
    [0, 25, 50, 75, 100].map { |pct| @wpb.percentile(pct) }
  end

  def counts
    @counts.dup.freeze
  end

  def initialize
    @on     = false
    @text   = ''
    @stack  = [] # XXX i don't think we use this one
    @wpb    = []
    @counts = %i[chars words blocks sections images videos embeds
      tables lists forms scripts sheets].map { |k| [k, 0] }.to_h
  end

  def scan doc
    if doc.is_a? Nokogiri::XML::Node
      pretend_sax doc
    else
      parser = Nokogiri::XML::SAX::Parser.new self
      parser.parse doc
    end

    self
  end

  def self.scan doc
    new.scan doc
  end

  def to_h
    { mean: mean, sd: sd, quartiles: quartiles }.merge counts
  end

  def to_rdf uri: nil, subject: nil
  end
end
