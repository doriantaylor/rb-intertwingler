require 'rdf/sak/version'
require 'set'

class RDF::SAK::DocStats
  private

  MAYBE  = %i[dt dd li td th caption figcaption]
  BLOCKS = Set.new(%i[body p h1 h2 h3 h4 h5 h6 ul ol pre dl main header footer
    article section aside figure nav div noscript blockquote form hr
    table fieldset address] + MAYBE).freeze

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
  

  public

  def initialize
    @text      = ''
    @stack     = []
    @wpb       = []
    @sections  = 0
    @blocks    = 0
    @words     = 0
    @chars     = 0
    @mean      = nil
    @sd        = nil
    @quantiles = Array.new 5 # nils
  end

  def scan doc
    # we get a list of 'leaf' elements
    doc.xpath(NODEXP, XPATHNS).each do
      # - collect text node(s) (there will be no child elements)
      # - ascend upward until you find a BLOCK
      # - scan child elements, subtracting 'seen' (and descendant text nodes)
      # - subtract 'seen' elements
      # - stop when you hit <body>
    end 
  end

  def self.scan doc
    new.scan doc
  end
end
