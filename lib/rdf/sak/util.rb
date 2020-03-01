# -*- coding: utf-8 -*-
require 'rdf/sak/version'

require 'uri'
require 'uri/urn'
require 'rdf'
require 'set'
require 'uuid-ncname'

module RDF::SAK::Util
  private

  R3986   = /^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$/
  QF      = /^([^?#]*)(?:\?([^#]*))?(?:#(.*?))?$/
  SF      = /[^[:alpha:][:digit:]\/\?%@!$&'()*+,;=._~-]/

  XPATH = {
    htmlbase: proc {
      x = ['ancestor-or-self::html:html[1]/' \
        'html:head[html:base[@href]][1]/html:base[@href][1]/@href']
      (x << x.first.gsub('html:', '')).join ?| }.call,
    xmlbase: 'ancestor-or-self::*[@xml:base][1]/@xml:base',
    lang: 'normalize-space((%s)[last()])' %
      %w[lang xml:lang].map do |a|
      'ancestor-or-self::*[@%s][1]/@%s' % [a,a]
    end.join(?|),
    literal: '(ancestor::*[@property][not(@content)]' \
      '[not(@resource|@href|@src) or @rel|@rev])[1]',
    leaves: 'descendant::html:section[not(descendant::html:section)]' \
      '[not(*[not(self::html:script)])]',
    headers: './*[1][%s]//text()' %
      (1..6).map { |x| "self::html:h#{x}" }.join(?|),
    modernize: ([
      "//html:div[*[1][#{(1..6).map { |i| 'self::html:h%d' % i }.join ?|}]]"] +
        { div: %i[section figure], blockquote: :note,
         table: :figure, img: :figure }.map do |k, v|
        (v.is_a?(Array) ? v : [v]).map do |cl|
          "//html:#{k}[contains(concat(' ', " \
            "normalize-space(@class), ' '), ' #{cl} ')]"
        end
      end.flatten).join(?|),
    dehydrate: '//html:a[count(*)=1][html:dfn|html:abbr|html:span]',
    rehydrate: %w[//html:dfn
      //html:abbr[not(parent::html:dfn)] //html:span].join(?|) +
      '[not(parent::html:a)]'
  }

  URI_COERCIONS = {
    nil   => -> t { t.to_s },
    false => -> t { t.to_s },
    uri:     -> t { URI.parse t.to_s },
    rdf:     -> t {
      t = t.to_s
      t.start_with?('_:') ? RDF::Node.new(t.drop_prefix '_:') : RDF::URI(t) },
  }

  def sanitize_prefixes prefixes, nonnil = false
    raise ArgumentError, 'prefixes must be a hash' unless
      prefixes.is_a? Hash or prefixes.respond_to? :to_h
    prefixes = prefixes.to_h.map do |k, v|
      [k ? k.to_s.to_sym : nil, v ? v.to_s : nil]
    end.to_h

    prefixes.reject! { |k, v| k.nil? || v.nil? } if nonnil
    prefixes
  end

  def assert_uri_coercion coerce
    if coerce
      coerce = coerce.to_s.to_sym if coerce.respond_to? :to_s
      raise 'coerce must be either :uri or :rdf' unless
        %i[uri rdf].include?(coerce)
    end
    coerce
  end

  def assert_xml_node node
    raise 'Argument must be a Nokogiri::XML::Element' unless
      node.is_a? Nokogiri::XML::Element
    node
  end

  def internal_subject_for node, prefixes: nil, base: nil, coerce: nil,
      is_ancestor: false

    # note we assign these AFTER the literal check or it will be wrong
    prefixes ||= get_prefixes node

    base ||= get_base node
    base = coerce_resource base, as: :uri unless base

    # answer a bunch of helpful questions about this element
    subject = nil
    parent  = node.parent
    ns_href = node.namespace.href if node.namespace
    up_ok   = %i[rel rev].none? { |a| node.key? a }
    is_root = !parent or parent.document?
    special = /^(?:[^:]+:)?(?:head|body)$/i === node.name and
      (ns_href == 'http://www.w3.org/1999/xhtml' or
      /^(?:[^:]+:)?html$/xi === parent.name)

    # if the node is being inspected as an ancestor to the
    # original node, we have to check it backwards.
    if is_ancestor
      # ah right @resource gets special treatment
      if subject = node[:resource]
        subject = resolve_curie subject,
          prefixes: prefixes, base: base, scalar: true
      else
        # then check @href and @src
        %i[href src].each do |attr|
          if node.key? attr
            # merge with the root and return it
            subject = base + node[attr]
            break
          end
        end
      end

      return coerce_resource subject, as: coerce if subject

      # note if we are being called with is_ancestor, that means
      # the original node (or indeed any of the nodes previously
      # tested) have anything resembling a resource in them. this
      # means @rel/@rev should be ignored, and we should keep
      # looking for a subject.
    end

    if node[:about]

      subject = resolve_curie node[:about],
        prefixes: prefixes, base: base, scalar: true

      # ignore coercion
      return subject if subject.is_a? RDF::Node

    elsif is_root
      subject = base
    elsif special
      subject = subject_for_internal parent
    elsif node[:resource]
      # XXX resolve @about against potential curie
      subject = resolve_curie node[:resource], prefixes: prefixes, base: base
    elsif node[:href]
      subject = base + node[:href]
    elsif node[:src]
      subject = base + node[:src]
    elsif node[:typeof]
      # bnode the typeof attr

      # note we return bnodes irrespective of the rdf flag
      return RDF::Node('id-%016x' % node.attributes['typeof'].pointer_id)
    elsif node[:inlist]
      # bnode the inlist attr
      return RDF::Node('id-%016x' % node.attributes['inlist'].pointer_id)
    elsif (parent[:inlist] && %i[href src].none? { |a| parent.key? a }) ||
        (is_ancestor && !up_ok)
      # bnode the element
      return RDF::Node('id-%016x' % node.pointer_id)
      # elsif node[:id]
    else
      subject = subject_for_internal parent, is_ancestor: true
    end

    coerce_resource subject, as: coerce if subject
  end

  MODERNIZE = {
    div: -> e {
      if e.classes.include? 'figure'
        e.remove_class 'figure'
        e.name = 'figure' unless e.parent.name == 'figure'
      else
        e.remove_class 'section'
        e.name = 'section'
      end
    },
    blockquote: -> e {
      e.remove_class 'note'
      e.name = 'aside'
      e['role'] = 'note'
    },
    table: -> e {
      e.remove_class 'figure'
      unless e.parent.name == 'figure'
        inner = e.dup
        markup replace: e, spec: { [inner] => :figure }
      end
    },
    img: -> e {
      e.remove_class 'figure'
      unless e.parent.name == 'figure'
        inner = e.dup
        markup replace: e, spec: { [inner] => :figure }
      end
    },
  }

  public

  XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze
  XPATHNS = { html: XHTMLNS }.freeze
  XHV     = 'http://www.w3.org/1999/xhtml/vocab#'.freeze

  ######## URI STUFF ########

  # Preprocess a URI string so that it can be handed to +URI.parse+
  # without crashing.
  #
  # @param uri [#to_s] The URI string in question
  # @return [String] The sanitized (appropriately escaped) URI string
  def uri_pp uri
    m = QF.match uri.to_s
    out = m[1]
    [[2, ??], [3, ?#]].each do |i, c|
      next if m[i].nil?
      clean = m[i].gsub(SF) { |s| sprintf('%X', s.ord) }
      out += c + clean
    end

    out
  end

  # Given a URI as input, split any query parameters into an array of
  # key-value pairs. If +:only+ is true, this will just return the
  # pairs. Otherwise it will prepend the query-less URI to the array,
  # and can be captured with an idiom like +uri, *qp = split_qp uri+.
  #
  # @param uri [URI,#to_s] The URI to extract parameters from
  # @param only [false, true] whether to only return the parameters
  # @return [Array] (See description)
  #
  def split_qp uri, only: false
    uri = URI(uri_pp uri.to_s) unless uri.is_a? URI
    qp  = URI::decode_www_form(uri.query)
    return qp if only
    uri.query = nil
    [uri] + qp
  end

  # Given a URI as input, split any path parameters out of the last
  # path segment. Works the same way as #split_pp.
  #
  # @param uri [URI,#to_s] The URI to extract parameters from
  # @param only [false, true] whether to only return the parameters
  # @return [Array] (See description)
  #
  def split_pp uri, only: false
    u = (uri.is_a?(URI) ? uri : URI(uri_pp uri.to_s)).normalize
    return only ? [] : [uri] unless u.path
    uri = u

    ps = uri.path.split '/', -1
    pp = ps.pop.split ';', -1
    bp = (ps + [pp.shift]).join '/'
    uri = uri.dup
    uri.path = bp
    return pp if only
    [uri] + pp
  end

  # Coerce a stringlike argument into a URI. Raises an exception if
  # the string can't be turned into a valid URI. Optionally resolves
  # against a +base+, and the coercion can be tuned to either URI or
  # RDF::URI via +:as+.
  #
  # @param arg [URI, RDF::URI, #to_s] The input string
  # @param base [URI, RDF::URI, #to_s] The optional base URI
  # @param as [:rdf, :uri, nil] The optional coercion type
  # @return [URI, RDF::URI, String]
  #
  def coerce_resource arg, base = nil, as: :rdf
    as = assert_uri_coercion as
    return arg if as and arg.is_a?({ uri: URI, rdf: RDF::URI }[as])
    raise ArgumentError, 'arg must be stringable' unless arg.respond_to? :to_s

    arg = arg.to_s.strip

    if arg.start_with? '_:' and as
      # override the coercion if this is a blank node
      as = :rdf
    elsif base
      begin
        arg = (base.is_a?(URI) ? base : URI(uri_pp base.to_s.strip)).merge arg
      rescue URI::InvalidURIError => e
        warn "attempted to coerce #{arg} which turned out to be invalid: #{e}"
        return
      end
    end

    URI_COERCIONS[as].call arg
  end

  # Coerce a stringlike argument into a UUID URN. Will
  def coerce_uuid_urn arg
    # if this is an ncname then change it
    if ([URI, RDF::URI] & arg.class.ancestors).empty? &&
        arg.respond_to?(:to_s)
      arg = arg.to_s

      # coerce ncname to uuid
      arg = UUID::NCName::from_ncname(arg, version: 1) if arg =~
        /^[A-P](?:[0-9A-Z_-]{20}|[2-7A-Z]{24})[A-P]$/i

      # now the string is either a UUID or it isn't
      arg = "urn:uuid:#{arg}" unless arg.start_with? 'urn:uuid:'
    else
      arg = arg.class.new arg.to_s.downcase unless arg == arg.to_s.downcase
    end

    raise ArgumentError, 'not a UUID' unless
      arg.to_s =~ /^urn:uuid:[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$/

    arg = coerce_resource arg
  end

  # Resolve a string or array or attribute node containing one or more
  # terms/CURIEs against a set of prefixes. The CURIE can be a string,
  # Nokogiri::XML::Attr, or an array thereof. Strings are stripped and
  # split on whitespace. +:prefixes+ and +:base+ can be supplied or
  # gleaned from +:refnode+, which itself can be gleaned if +curie+ is
  # a Nokogiri::XML::Attr. Returns an array of (attempted) resolved
  # terms unless +:scalar+ is true, in which case only the first URI
  # is returned. When +:noop+ is true, this method will always return
  # a value. Can coerce results to either RDF::URI or URI objects.
  #
  # @note +:vocab+ overrides, and is the same as supplying
  #  +prefix[nil]+. It is only meaningful when +:term+ (i.e., when we
  #  expect the input to be an RDFa term) is true.
  #
  # @param curie [#to_s, Nokogiri::XML::Attr,Array] One or more CURIEs
  # @param prefixes [#to_h] The hash of prefixes (nil key is equivalent
  #  to vocab)
  # @param vocab [nil,#to_s] An optional base URI
  # @param refnode [nil, Nokogiri::XML::Element] A reference node for resolution
  # @param term [false, true] Whether to treat the input as an RDFa _term_
  # @param noop [true, false] Whether to skip if the CURIE can't be resolved
  # @param scalar [false, true] Whether to return a scalar value
  # @param coerce [nil, :rdf, :uri] Desired type coercion for the output
  #
  # @return [nil,URI,RDF::URI,Array<nil,URI,RDF::URI>]
  #
  def resolve_curie curie, prefixes: {}, vocab: nil, base: nil,
      refnode: nil, term: false, noop: true, scalar: false, coerce: nil
    prefixes = sanitize_prefixes prefixes

    raise 'coerce must be either :uri or :rdf' if coerce and
      not %i[uri rdf].include?(coerce)

    # coerce curie to its value and set refnode if not present
    if curie.is_a? Nokogiri::XML::Attr
      refnode ||= curie.parent
      curie = curie.value.strip.split
    elsif curie.respond_to? :to_a
      curie = curie.to_a
      raise ArgumentError,
        'if curie is an array, it has to be all strings' unless
        curie.all? { |x| x.respond_to? :to_s }
      curie = curie.map { |x| x.to_s.strip.split }.flatten
    else
      raise ArgumentError, 'curie must be stringable' unless
        curie.respond_to? :to_s
      curie = curie.to_s.strip.split
    end

    if refnode
      raise ArgumentError, 'refnode must be an element' unless
        refnode.is_a? Nokogiri::XML::Element
      prefixes = get_prefixes refnode if prefixes.empty?
    end

    # now we overwrite the vocab
    if vocab
      raise ArgumentError, 'vocab must be stringable' unless
        vocab.respond_to? :to_s
      prefixes[nil] = vocab.to_s.strip
    end

    out = curie.map do |c|
      prefix, slug = /^\[?(?:([^:]+):)?(.*?)\]?$/.match(c).captures
      prefix = prefix.to_sym if prefix
      tmp = if prefixes[prefix]
              prefixes[prefix] + slug
            else
              noop ? c : nil
            end
      tmp && coerce ? URI_COERCIONS[coerce].call(tmp) : tmp
    end

    scalar ? out.first : out
  end

  # Abbreviate one or more URIs into one or more CURIEs if we
  # can. Will through if +noop:+ is true, or if false, return nil for
  # any URI that can't be abbreviated this way. Takes a hash of
  # prefix-URI mappings where the keys are assumed to be symbols or
  # +nil+ to express the current vocabulary, which can be overridden
  # via +vocab:+.
  #
  # @note Only +noop: true+ can be guaranteed to return a value.
  #
  # @param term [Array<#to_s>, #to_s] the term(s)
  # @param prefixes [Hash<Symbol,nil>, #to_h] the prefix mappings
  # @param vocab [#to_s] current vocabulary, overrides +prefixes[nil]+
  # @param noop [true, false] whether or not to pass terms through
  # @param sort [true, false] whether or not to sort (only if +noop:+)
  # @return [String, nil, Array<String,nil>] the (maybe) abbreviated term(s)
  #
  def abbreviate term, prefixes: {}, vocab: nil, noop: true, sort: true
    # this returns a duplicate that we can mess with
    prefixes = sanitize_prefixes prefixes

    # sanitize vocab
    raise ArgumentError, 'vocab must be nil or stringable' unless
      vocab.nil? or vocab.respond_to? :to_s
    prefixes[nil] = vocab.to_s if vocab
    scalar = true

    term = if term.respond_to? :to_a
             scalar = false
             term.to_a
           else [term]; end

    rev = prefixes.invert

    term.map! do |t|
      t = t.to_s
      slug = nil # we want this value to be nil if no match and !noop

      # try matching each prefix URI from longest to shortest
      rev.sort { |a, b| b.first.length <=> a.first.length }.each do |uri, pfx|
        slug = t.delete_prefix uri
        # this is saying the URI either doesn't match or abbreviates to ""
        if slug == t or pfx.nil? && slug.empty?
          slug = nil
        else
          # it's already a slug so we add a prefix if there is one
          slug = '%s:%s' % [pfx, slug] unless pfx.nil?
          break # we have our match
        end
      end

      # at this point slug is either an abbreviated term or nil, so:
      slug ||= t if noop
      slug
    end

    # only sort if noop is set
    term.sort! if noop && sort

    scalar ? term.first : term
  end

  ######## RDFA/XML STUFF ########

  # Returns the base URI from the perspective of the given element.
  # Can optionally be coerced into either a URI or RDF::URI. Also
  # takes a default value.
  #
  # @param elem [Nokogiri::XML::Node] the context element
  # @param default [nil, #to_s] the default URI
  # @param coerce [nil, :uri, :rdf] the coercion scheme, if any
  # @return [nil, String, URI, RDF::URI] the context's base URI
  def get_base elem, default: nil, coerce: nil
    assert_uri_coercion coerce

    if elem.document?
      elem = elem.root
      return unless elem
    end

    # get the xpath
    xpath = (elem.namespace && elem.namespace.href == XHTMLNS or
      elem.at_xpath('/html')) ? :htmlbase : :xmlbase

    # now we go looking for the attribute
    if base = elem.at_xpath(XPATH[xpath], XPATHNS)
      base = base.value.strip
    else
      base = default.to_s.strip if default
    end

    # clear it out if it's the empty string
    base = nil if base and base.empty?

    # eh that's about all the input sanitation we're gonna get
    base && coerce ? URI_COERCIONS[coerce].call(base) : base
  end

  # Given an X(HT)ML element, returns a hash of prefixes of the form
  # +{ prefix: "vocab" }+, where the current +@vocab+ is represented
  # by the +nil+ key. An optional +:traverse+ parameter can be set to
  # +false+ to prevent ascending the node tree. Any XML namespace
  # declarations are superseded by the +@prefix+ attribute. Returns
  # any +@vocab+ declaration found as the +nil+ key.
  #
  # @note The +descend: true+ parameter assumes we are trying to
  #  collect all the namespaces in use in the entire subtree, rather
  #  than resolve any particular CURIE. As such, the _first_ prefix
  #  mapping in document order is preserved over subsequent/descendant
  #  ones.
  #
  # @param elem [Nokogiri::XML::Node] The context element
  # @param traverse [true, false] whether or not to traverse the tree
  # @param coerce [nil, :rdf, :uri] a type coercion for the URIs, if any
  # @param descend [false, true] go _down_ the tree instead of up
  # @return [Hash] Depending on +:traverse+, either all prefixes
  #  merged, or just the ones asserted in the element.
  def get_prefixes elem, traverse: true, coerce: nil, descend: false
    coerce = assert_uri_coercion coerce

    # deal with a common phenomenon
    elem = elem.root if elem.is_a? Nokogiri::XML::Document

    # get namespace definitions first
    prefix = elem.namespaces.reject do |k, _| k == 'xmlns'
    end.transform_keys { |k| k.split(?:)[1].to_sym }

    # now do the prefix attribute
    if elem.key? 'prefix'
      # XXX note this assumes largely that the input is clean
      elem['prefix'].strip.split.each_slice(2) do |k, v|
        pfx = k.split(?:)[0] or next # otherwise error
        prefix[pfx.to_sym] = v
      end
    end

    # encode the vocab as the null prefix
    if vocab = elem['vocab']
      vocab.strip!
      # note that a specified but empty @vocab means kill any existing vocab
      prefix[nil] = vocab.empty? ? nil : vocab
    end

    # don't forget we can coerce
    prefix.transform_values! { |v| COERCIONS[coerce].call v } if coerce

    # don't proceed if `traverse` is false
    return prefix unless traverse

    # save us having to recurse in ruby by using xpath implemented in c
    xpath = '%s::*[namespace::*|@prefix|@vocab]' %
      (descend ? :descendant : :ancestor)
    elem.xpath(xpath).each do |e|
      # this will always merge our prefix on top irrespective of direction
      prefix = get_prefix(e, traverse: false, coerce: coerce).merge prefix
    end

    prefix
  end

  # Given an X(HT)ML element, return the nearest RDFa _subject_.
  # Optionally takes +:prefix+ and +:base+ parameters which override
  # anything found in the document tree.
  #
  # @param node [Nokogiri::XML::Element] the node
  # @param prefixes [Hash] Prefix mapping. Overrides derived values.
  # @param base [#to_s,URI,RDF::URI] Base URI, overrides as well.
  # @param coerce [nil, :rdf, :uri] the coercion regime
  #
  # @return [URI,RDF::URI,String] the subject
  #
  def subject_for node, prefixes: nil, base: nil, coerce: :rdf
    assert_xml_node node
    coerce = assert_uri_coercion coerce

    if n = node.at_xpath(XPATH[:literal])
      return internal_subject_for n,
        prefixes: prefixes, base: base, coerce: coerce
    end

    internal_subject_for node, prefixes: prefixes, base: base, coerce: coerce
  end

  def modernize doc
    doc.xpath(XPATH[:modernize], XPATHNS).each do |e|
      # gotta instance_exec because `markup` is otherwise unbound
      instance_exec e, &MODERNIZE[e.name.to_sym]
    end
  end

  # Strip all the links surrounding and RDFa attributes off
  # +dfn+/+abbr+/+span+ tags. Assuming a construct like +<a
  # rel="some:relation" href="#..." typeof="skos:Concept"><dfn
  # property="some:property">Term</dfn></a>+ is a link to a glossary
  # entry, this method returns the term back to an undecorated state
  # (+<dfn>Term</dfn>+).

  def dehydrate doc
    doc.xpath(XPATH[:dehydrate], XPATHNS).each do |e|
      e = e.replace e.elements.first.dup
      %w[about resource typeof rel rev property datatype].each do |a|
        e.delete a if e.key? a
      end
    end
  end

  # Scan all the +dfn+/+abbr+/+span+ tags in the document that are not
  # already wrapped in a link. This method scans the text (or
  # +@content+) of each element and compares it to the contents of the
  # graph. If the process locates a subject, it will use that subject
  # as the basis of a link. if there are zero subjects, or more than
  # one, then the method executes a block which can be used to pick
  # (e.g., via user interface) a definite subject or otherwise add one.

  # (maybe add +code+/+kbd+/+samp+/+var+/+time+ one day too)

  def rehydrate doc, graph, &block
    doc.xpath(XPATH[:rehydrate], XPATHNS).each do |e|
      lang = e.xpath(XPATH[:lang]).to_s.strip
      # dt   = e['datatype'] # XXX no datatype rn
      text = (e['content'] || e.xpath('.//text()').to_a.join).strip

      # now we have the literal
      lit = [RDF::Literal(text)]
      lit.unshift RDF::Literal(text, language: lang) unless lang.empty?

      # candidates
      cand = {}
      lit.map do |t|
        graph.query(object: t).to_a
      end.flatten.each do |x|
        y = cand[x.subject] ||= {}
        (y[:stmts] ||= []) << x
        y[:types]  ||= graph.query([x.subject, RDF.type, nil]).objects.sort
      end

      # if there's only one candidate, this is basically a noop
      chosen = cand.keys.first if cand.size == 1

      # call the block to reconcile any gaps or conflicts
      if block_given? and cand.size != 1
        # the block is expected to return one of the candidates or
        # nil. we call the block with the graph so that the block can
        # manipulate its contents.
        chosen = block.call cand, graph
        raise ArgumentError, 'block must return nil or a term' unless
          chosen.nil? or chosen.is_a? RDF::Term
      end

      if chosen
        # we assume this has been retrieved from the graph
        cc = cand[chosen]
        unless cc
          cc = cand[chosen] = {}
          cc[:stmts] = graph.query([chosen, nil, lit[0]]).to_a.sort
          cc[:types] = graph.query([chosen, RDF.type, nil]).objects.sort
          # if either of these are empty then the graph was not
          # appropriately populated
          raise 'Missing a statement relating #{chosen} to #{text}' if
            cc[:stmts].empty?
        end

        # we should actually probably move any prefix/vocab/xmlns
        # declarations from the inner node to the outer one (although
        # in practice this will be an unlikely configuration)
        pfx = get_prefixes e

        # here we have pretty much everything except for the prefixes
        # and wherever we want to actually link to.

        inner = e.dup
        spec  = { [inner] => :a, href: '' }
        # we should have types
        spec[:typeof] = abbreviate cc[:types], prefixes: pfx unless
          cc[:types].empty?

        markup replace: e, spec: spec
      end
    end
    # return maybe the elements that did/didn't get changed?
  end

  ######## RENDERING STUFF ########

  # Given a structure of the form +{ predicate => [objects] }+,
  # rearrange the structure into one more amenable to rendering
  # RDFa. Returns a hash of the form +{ resources: { r1 => Set[p1, pn]
  # }, literals: { l1 => Set[p2, pm] }, types: Set[t1, tn], datatypes:
  # Set[d1, dn] }+. This inverted structure can then be conveniently
  # traversed to generate the RDFa. An optional block lets us examine
  # the predicate-object pairs as they go by.
  #
  # @param struct [Hash] The struct of the designated form
  # @yield [p, o] An optional block is given the predicate-object pair
  # @return [Hash] The inverted structure, as described.
  #
  def prepare_collation struct, &block
    resources = {}
    literals  = {}
    datatypes = Set.new
    types     = Set.new

    struct.each do |p, v|
      v.each do |o|
        block.call p, o if block

        if o.literal?
          literals[o] ||= Set.new
          literals[o].add p
          # collect the datatype
          datatypes.add o.datatype if o.has_datatype?
        else
          if  p == RDF::RDFV.type
            # separate the type
            types.add o
          else
            # collect the resource
            resources[o] ||= Set.new
            resources[o].add p
          end
        end
      end
    end

    { resources: resources, literals: literals,
      datatypes: datatypes, types: types }
  end

  # Given a hash of prefixes and an array of nodes, obtain the the
  # subset of prefixes that abbreviate the nodes. Scans RDF URIs as
  # well as RDF::Literal datatypes.
  #
  # @param prefixes [#to_h] The prefixes, of the form +{ k: "v" }+
  # @param nodes [Array<RDF::Term>] The nodes to supply
  # @return [Hash] The prefix subset
  def prefix_subset prefixes, nodes
    prefixes = sanitize_prefixes prefixes, true

    raise 'nodes must be arrayable' unless nodes.respond_to? :to_a

    # sniff out all the URIs and datatypes
    resources = Set.new
    nodes.each do |n|
      next unless n.is_a? RDF::Term
      if n.literal? && n.datatype?
        resources << n.datatype
      elsif n.uri?
        resources << n
      end
    end

    # now we abbreviate all the resources
    pfx = abbreviate(resources.to_a,
      prefixes: prefixes, noop: false, sort: false).uniq.compact.map do |p|
      p.split(?:).first.to_sym
    end.uniq.to_set

    # now we return the subset 
    prefixes.select { |k, _| pfx.include? k.to_sym }
  end

  # turns any data structure into a set of nodes
  def smush_struct struct
    out = Set.new

    if struct.is_a? RDF::Term
      out << struct
    elsif struct.respond_to? :to_a
      out |= struct.to_a.map { |s| smush_struct(s).to_a }.flatten.to_set
    end

    out
  end

  def invert_struct struct
    nodes = {}

    struct.each do |p, v|
      v.each do |o|
        nodes[o] ||= Set.new
        nodes[o] << p
      end
    end

    nodes
  end

  def title_tag predicates, content,
      prefixes: {}, vocab: nil, lang: nil, xhtml: true

    # begin with the tag
    tag = { '#title' => content.to_s,
      property: abbreviate(predicates, prefixes: prefixes, vocab: vocab) }

    # we set the language if it exists and is different from the
    # body OR if it is xsd:string we set it to the empty string
    lang = (content.language? && content.language != lang ?
      content.language : nil) || (content.datatype == RDF::XSD.string &&
      lang ? '' : nil)
    if lang
      tag['xml:lang'] = lang if xhtml
      tag[:lang] = lang
    end
    if content.datatype? && content.datatype != RDF::XSD.string
      tag[:datatype] = abbreviate(content.datatype,
        prefixes: prefixes, vocab: vocab)
    end

    tag
  end

  ######## MISC STUFF ########

  # Obtain everything that is an owl:equivalentClass or
  # rdfs:subClassOf the given type.
  #
  # @param rdftype [RDF::Term]
  #
  # @return [Array]

  def all_related rdftype
    t = RDF::Vocabulary.find_term(rdftype) or raise "No type #{rdftype.to_s}"
    q = [t] # queue
    c = {}  # cache

    while term = q.shift
      # add term to cache
      c[term] = term

      # keep this from tripping up
      next unless term.uri? and term.respond_to? :class?

      # entail equivalent classes
      term.entail(:equivalentClass).each do |ec|
        # add equivalent classes to queue (if not already cached)
        q.push ec unless c[ec]
        c[ec] = ec unless ec == term
      end

      # entail subclasses
      term.subClass.each do |sc|
        # add subclasses to queue (if not already cached)
        q.push sc unless c[sc]
        c[sc] = sc unless sc == term
      end
    end

    # smush the result 
    c.keys
  end

  # duplicate instance methods as module methods
  extend self
end
