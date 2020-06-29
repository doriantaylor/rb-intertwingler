require 'rdf'
require 'rdf/sak/util'
require 'time'
require 'nokogiri'
require 'xml-mixup'

class RDF::SAK::Document
  include XML::Mixup
  include RDF::SAK::Util

  private

  XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze
  XPATHNS = { html: XHTMLNS }
  XHV = 'http://www.w3.org/1999/xhtml/vocab#'.freeze

  # notice these are only RDFa attributes that take URIs
  RDFA_ATTR  = [:about, :resource, :typeof].freeze
  LINK_ATTR  = [:href, :src, :data, :action, :longdesc].freeze
  LINK_XPATH = ('.//html:*[not(self::html:base)][%s]' %
    (LINK_ATTR + RDFA_ATTR).map { |a| "@#{a.to_s}" }.join('|')).freeze

  OBJS = [:href, :src].freeze
      
  # ancestor node always with (@property and not @content) and
  # not @resource|@href|@src unless @rel|@rev
  LITXP = ['(ancestor::*[@property][not(@content)]',
    '[not(@resource|@href|@src) or @rel|@rev])[1]' ].join('').freeze
  # note parentheses cause the index to be counted from the root

  public

  attr_reader :repo, :subject, :doc, :base, :prefixes

  # Initialize a document context. 
  def initialize repo, doc, subject: nil, base: nil, resolve: nil,
      prefixes: {}, transform: nil, scache: {}, ucache: {}
    # coerce the document
    doc = case doc
          when Nokogiri::XML::Document then doc
          when Nokogiri::XML::Node then Nokogiri::XML::Document.new << doc.dup
          when String, IO, File, Pathname then Nokogiri.XML doc
          else
            raise ArgumentError, "Not sure what to do with #{doc.class}"
          end

    # we only try this if there is a subject defined, obvs
    base ||= RDF::SAK::Util.canonical_uri repo, subject, rdf: false if subject

    @repo      = repo
    @subject   = subject
    @doc       = doc
    @base      = URI(base.to_s) if base # note this is a vanilla URI
    @resolve   = RDF::URI(resolve.to_s) if resolve # note this is an RDF::URI
    @prefixes  = prefixes
    @transform = transform
    @scache    = scache
    @ucache    = ucache
  end

  def canonical_uuid uri, unique: true, published: false
    RDF::SAK::Util.canonical_uuid @repo, uri, base: @base,
      unique: unique, published: published, scache: @scache, ucache: @ucache
  end

  def canonical_uri subject,
      unique: true, rdf: true, slugs: false, fragment: false
    RDF::SAK::Util.canonical_uri @repo, subject, base: @base,
      unique: unique, rdf: rdf, slugs: slugs, fragment: fragment
  end

  def cmp_label a, b, labels: nil, supplant: true, reverse: false
    RDF::SAK::Util.cmp_label @repo, a, b,
      labels: labels, supplant: supplant, reverse: reverse
  end

  def asserted_types subject, type = nil
    RDF::SAK::Util.asserted_types @repo, subject, type
  end

  def subjects_for predicate, object, entail: true, only: []
    RDF::SAK::Util.subjects_for @repo, predicate, object,
      entail: entail, only: only
  end

  def objects_for subject, predicate, entail: true, only: [], datatype: nil
    RDF::SAK::Util.objects_for @repo, subject, predicate,
      entail: entail, only: only, datatype: datatype
  end

  def struct_for subject, rev: false, only: [], uuids: false, canon: false
    RDF::SAK::Util.struct_for @repo, subject,
      rev: rev, only: only, uuids: uuids, canon: canon,
      ucache: @ucache, scache: @scache
  end

  def label_for subject, candidates: nil, unique: true, type: nil,
      lang: nil, desc: false, alt: false
    RDF::SAK::Util.label_for @repo, subject, candidates: candidates,
      unique: unique, type: type, lang: lang, desc: desc, alt: alt
  end

  def formats_for subject, predicate: RDF::Vocab::DC.format,
      datatype: [RDF::XSD.token]
    RDF::SAK::Util.formats_for @repo, subject,
      predicate: predicate, datatype: datatype
  end

  def authors_for subject, unique: false, contrib: false
    RDF::SAK::Util.authors_for @repo, subject, unique: unique, contrib: contrib
  end

  # proxy for context published
  def published? subject = nil
    return RDF::SAK::Util.published? @repo, subject, base: @base if subject
    @published ||= RDF::SAK::Util.published? @repo, @subject, base: @base
  end

  def abbreviate term, prefixes: @prefixes,
      vocab: nil, noop: true, sort: true
    super term, prefixes: prefixes || {}, vocab: vocab, noop: noop, sort: sort
  end

  def base_for node = nil
    node ||= @doc
    doc  = node.document
    base = URI(@base.to_s)
    if doc.root.name.to_sym == :html
      b = doc.at_xpath(
        '(/html:html/html:head/html:base[@href])[1]/@href', XPATHNS
      ).to_s.strip
      b = URI(b)
      
      base = b if b.absolute?
    elsif b = doc.root.at_xpath('ancestor-or-self::*[@xml:base][1]/@xml:base')
      b = URI(b.to_s.strip)
      base = b if b.absolute?
    end


    # warn({ orig_base: @base, resolve: resolve, base: base}.inspect)

    # warn %i[scheme host port].map { |s| [s, base.send(s) == resolve.send(s)] }.to_h.inspect

    # rewrite if aliased
    if @resolve and resolve = URI(@resolve.to_s) and
        %i[scheme host port].all? { |s| base.send(s) == resolve.send(s) }
      tmp        = base.dup
      tmp.scheme = @base.scheme
      tmp.host   = @base.host
      tmp.port   = @base.port
      base       = tmp.normalize
    end

    base
  end

  def rewrite_links node = @doc, uuids: {}, uris: {}, &block
    base  = base_for node
    if be = node.at_xpath('(/html:html/html:head/html:base[@href])[1]', XPATHNS)
      be[:href] = base.to_s if base.to_s != be[:href]
    end
    count = 0
    node.xpath(LINK_XPATH, XPATHNS).each do |elem|
      LINK_ATTR.each do |attr|
        attr = attr.to_s
        next unless elem.has_attribute? attr

        abs = base.merge uri_pp(elem[attr].strip)

        # bail out if this isn't http(s)
        next if abs.scheme and !%w[http https].include? abs.scheme.downcase

        # fix e.g. http->https
        if abs.host == @base.host and abs.scheme != @base.scheme
          tmp          = @base.dup
          tmp.path     = abs.path
          tmp.query    = abs.query
          tmp.fragment = abs.fragment
          abs          = tmp
        end

        # harvest path parameters
        pp = split_pp abs, only: true

        # coerce to rdf
        abs = RDF::URI(abs.to_s)

        # make an aliased copy we use to look up the uuid
        aliased = if @resolve
                    tmp = abs.dup
                    tmp.scheme    = @resolve.scheme
                    tmp.authority = @resolve.authority if @resolve.authority
                    tmp
                  else
                    abs
                  end

        # warn "aliased #{abs} to #{aliased}" if @resolve


        # round-trip to uuid and back if we can
        if uuid = uris[abs] ||= canonical_uuid(aliased)
          abs = uuids[uuid] ||= canonical_uri(uuid)
        elsif cu = canonical_uri(abs)
          # otherwise just find the canonical uri
          abs = cu
        end

        # reinstate the path parameters
        if !pp.empty? && split_pp(abs, only: true).empty?
          abs = abs.dup
          abs.path = ([abs.path] + pp).join(';')
        end

        elem[attr] = @base.route_to(abs.to_s).to_s
        count += 1
      end

      block.call elem if block
    end

    count
  end

  # sponge the document for rdfa
  def triples_for
  end

  def vocab_for node
    if node[:vocab]
      vocab = node[:vocab].strip
      return nil if vocab == ''
      return vocab
    end
    parent = node.parent
    vocab_for parent if parent and parent.element?
  end

  def prefixes_for node, prefixes = {}
    # start with namespaces
    pfx = node.namespace_declarations.filter(&:prefix).map do |n|
      [n.prefix.to_sym, n.href]
    end.to_h
    
    # then add @prefix overtop of the namespaces
    if node[:prefix]
      x = node[:prefix].strip.split(/\s+/)
      a = []
      b = []
      x.each_index { |i| (i % 2 == 0 ? a : b).push x[i] }
      a.map!(&:to_sym)
      # if the size is uneven the values will be nil, so w drop em
      pfx.merge! a.zip(b).to_h.reject { |_, v| v.nil? }
    end

    # since we're ascending the tree, input takes precedence
    prefixes = pfx.merge prefixes
      
    if node.parent and node.parent.element?
      prefixes_for(node.parent, prefixes)
    else
      prefixes
    end
  end

  # give us the rdf subject of the node itself
  def subject_for node = nil, rdf: false, is_ancestor: false
    node ||= @doc.root
    raise 'Node must be an element' unless
      node.is_a? Nokogiri::XML::Element

    # first we check for an ancestor element with @property and no
    # @content; if we find one then we reevaluate with that
    # element as the starting point
    if n = node.at_xpath(LITXP)
      return subject_for n
    end

    # answer a bunch of helpful questions about this element
    subject = nil
    base    = base_for node
    parent  = node.parent
    ns_href = node.namespace.href if node.namespace
    up_ok   = %i{rel rev}.none? { |a| node[a] }
    is_root = !parent or parent.document?
    special = /^(?:[^:]+:)?(?:head|body)$/i === node.name and
      (ns_href == XHTMLNS or /^(?:[^:]+:)?html$/xi === parent.name)

    # if the node is being inspected as an ancestor to the
    # original node, we have to check it backwards.
    if is_ancestor
      # ah right @resource gets special treatment
      if subject = node[:resource]
        subject.strip!
        if m = /^\[(.*?)\]$/.match(subject)
        end
      else
        OBJS.each do |attr|
          if node[attr]
            # merge with the root and return it
            subject = base + node[attr]
            break
          end
        end
      end

      return rdf ? RDF::URI(subject.to_s) : subject

      # note if we are being called with is_ancestor, that means
      # the original node (or indeed any of the nodes previously
      # tested) have anything resembling a resource in them. this
      # means @rel/@rev should be ignored, and we should keep
      # looking for a subject.
    end

    if node[:about]
          
      if m = /^_:(.*)$/.match(node[:about])
        return RDF::Node(m[1])
      end

      # XXX resolve @about against potential curie
      subject = base + node[:about]

    elsif is_root
      subject = base
    elsif special
      subject = subject_for parent
    elsif node[:resource]
      # XXX resolve @about against potential curie
      subject = base + node[:resource]
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
    elsif (parent[:inlist] && OBJS.none? { |a| parent[a] }) ||
        (is_ancestor && !up_ok)
      # bnode the element
      return RDF::Node('id-%016x' % node.pointer_id)
      # elsif node[:id]
    else
      subject = subject_for parent, is_ancestor: true
    end

    rdf ? RDF::URI(subject.to_s) : URI(subject.to_s)

  end

  # backlink structure
  def generate_backlinks published: true, struct: nil,
      ignore: nil, pattern: nil, terse: false
    uri    = canonical_uri(subject, rdf: false) || URI(uri_pp subject)
    ignore = case ignore
             when nil then Set.new
             when Proc then ignore
             when -> x { x.respond_to? :to_set } then ignore = ignore.to_set
             else 
               raise 'ignore must be either a proc or amenable to a set' 
             end
    nodes  = {}
    labels = {}
    types  = {}

    if struct
      struct.each do |p, subjects|
        subjects.each do |s|
          case ignore
          when Proc then next if ignore.call s, p
          when Set  then next if ignore.include? s
          end
          preds = nodes[s] ||= Set.new
          preds << p
          types[s]  ||= asserted_types s
          labels[s] ||= label_for s
          labels[p] ||= label_for p unless terse
        end
      end
    else
      @repo.query([nil, nil, subject]).each do |stmt|
        s = stmt.subject
        case ignore
        when Proc then next if ignore.call stmt
        when Set  then next if ignore.include? s
        end
        preds = nodes[s] ||= Set.new
        preds << (p = stmt.predicate)
        types[s]  ||= asserted_types s
        labels[s] ||= label_for s
        labels[p] ||= label_for p unless terse
      end
    end

    # prune out nonmatching
    nodes.select! { |k, _| pattern.match? k.to_s } if
      pattern and pattern.is_a? Regexp

    # prune out unpublished
    nodes.select! { |k, _| published? k } if published
      
    return if nodes.empty?

    if terse
      nodes.map do |rsrc, preds|
        cu   = canonical_uri(rsrc, rdf: false) or next
        lab  = labels[rsrc] || [nil, rsrc]
        link = { nil => :link, rel: '', href: uri.route_to(cu),
          rev: abbreviate(preds)  }
        link[:typeof] = abbreviate(types[rsrc]) if types[rsrc]
        link[:title]  = lab.last if lab.last
        link
      end.compact
    else
      li = nodes.sort do |a, b|
        cmp_label a.first, b.first, labels: labels
      end.map do |rsrc, preds|
        cu  = canonical_uri(rsrc, rdf: false) or next
        lab = labels[rsrc] || [nil, rsrc]
        lp  = abbreviate(lab.first) if lab.first
        ty  = abbreviate(types[rsrc]) if types[rsrc]
        
        { [{ [{ [lab[1].to_s] => :span, property: lp }] => :a, typeof: ty,
          href: uri.route_to(cu), rev: abbreviate(preds) }] => :li }
      end.compact

      { [{ li => :ul }] => :nav }
    end
  end

  # goofy twitter-specific metadata
  def generate_twitter_meta
    # get author
    author = authors_for(subject, unique: true) or return

    return unless author.is_a? RDF::Resource

    # get author's twitter account
    twitter = objects_for(author, RDF::Vocab::FOAF.account,
      only: :resource).select { |t| t.to_s =~ /twitter\.com/
    }.sort.first or return
    twitter = URI(twitter.to_s).path.split(/\/+/)[1]
    twitter = ?@ + twitter unless twitter.start_with? ?@

    # get title
    title = label_for(subject) or return

    out = [
      { nil => :meta, name: 'twitter:card', content: :summary },
      { nil => :meta, name: 'twitter:site', content: twitter },
      { nil => :meta, name: 'twitter:title', content: title[1].to_s }
    ]

    # get abstract
    if desc = label_for(subject, desc: true)
      out.push({ nil => :meta, name: 'twitter:description',
        content: desc[1].to_s })
    end

    # get image (foaf:depiction)
    img = objects_for(subject, RDF::Vocab::FOAF.depiction, only: :resource)
    unless img.empty?
      img = img[0].to_s
      out.push({ nil => :meta, name: 'twitter:image', content: img })
      out[0][:content] = :summary_large_image
    end

    # return the appropriate xml-mixup structure
    out
  end

  def transform_xhtml published: true, titles: false
    # before we do any more work make sure this is html
    doc  = @doc.dup 1
    body = doc.at_xpath('//html:body[1]', XPATHNS) || doc.root

    # eliminate comments
    doc.xpath('//comment()[not(ancestor::html:script)]', XPATHNS).each do |c|
      c.unlink
    end

    # initial stuff
    struct    = struct_for @subject, uuids: true, canon: true
    rstruct   = struct_for @subject, uuids: true, canon: true, rev: true
    resources = {}
    literals  = {}
    ufwd      = {} # uuid => uri
    urev      = {} # uri  => uuid
    datatypes = Set.new
    types     = Set.new
    authors   = authors_for @subject
    title     = label_for @subject, candidates: struct
    desc      = label_for @subject, candidates: struct, desc: true

    # warn struct

    # rewrite content
    title = title[1] if title
    desc  = desc[1]  if desc

    # `struct` and `rstruct` will contain all the links and
    # metadata for forward and backward neighbours, respectively,
    # which we need to mine (predicates, classes, datatypes) for
    # prefixes among other things.

    struct.each do |p, v|
      v.each do |o|
        if o.literal?
          literals[o] ||= Set.new
          literals[o].add p

          # collect the datatype
          datatypes.add o.datatype if o.has_datatype?
        else
          # normalize URIs
          if o.to_s.start_with? 'urn:uuid:'
            ufwd[o] ||= canonical_uri o
          elsif cu = urev[o] || canonical_uuid(o)
            o = urev[o] ||= cu
          end

          # collect the resource
          resources[o] ||= Set.new
          resources[o].add p

          # add to type
          types.add o if p == RDF::RDFV.type 
        end
      end
    end

    urev.merge! ufwd.invert

    labels = resources.keys.map do |k|
      # turn this into a pair which subsequently gets turned into a hash
      [k, label_for(k) ]
    end.to_h

    #warn labels

    # handle the title
    title ||= RDF::Literal('')
    tm = { '#title' => title,
      property: abbreviate(literals[title].to_a, vocab: XHV) }
    if tl = title.language
      tm['xml:lang'] = tl # if xmlns
      tm['lang'] = tl
    elsif tdt = title.datatype and tdt != RDF::XSD.string
      tm[:datatype] = abbreviate(tdt)
    end

    # we accumulate a record of the links in the body so we know
    # which ones to skip in the head
    bodylinks = {}
    rewrite_links body, uuids: ufwd, uris: urev do |elem|
      vocab = elem.at_xpath('ancestor-or-self::*[@vocab][1]/@vocab')
      vocab = uri_pp(vocab.to_s) if vocab

      if elem.key?('href') or elem.key?('src')
        vu = uri_pp(elem['href'] || elem['src'])
        ru = RDF::URI(@base.merge(vu))
        bodylinks[urev[ru] || ru] = true

        if rel = resources[urev[ru] || ru]
          elem['rel'] = (abbreviate rel, vocab: vocab).join ' '
        end

        label = labels[urev[ru] || ru]
        if titles and label and
            (!elem.key?('title') or elem['title'].strip == '')
          elem['title'] = label[1].to_s
        end
      end
    end

    # and now we do the head
    links = []
    resources.reject { |k, _| bodylinks[k] }.each do |k, v|
      v = v.dup.delete RDF::RDFV.type
      next if v.empty?
      mts = formats_for k

      # warn k, v.inspect

      # warn k, mts.inspect

      rel = abbreviate v.to_a, vocab: XHV
      ru  = @base.route_to(uri_pp (ufwd[k] || k).to_s)
      ln  = { nil => :link, rel: rel, href: ru.to_s }
      if (label = labels[urev[k] || k])
        ln[:title] = label[1].to_s
      end

      # add type=lol/wut
      ln[:type] = mts.first.to_s unless mts.empty?

      if !ln[:type] and v.include?(RDF::Vocab::XHV.stylesheet)
        ln[:type] = 'text/css'
      elsif ln[:type] =~ /(java|ecma)script/i or
          v.include?(RDF::Vocab::DC.requires)
        ln[nil]  = :script
        ln[:src] = ln.delete :href
        ln[:type] ||= 'text/javascript'
      end
      links.push ln
    end

    links.sort! do |a, b|
      # sort by rel, then by href
      # warn a.inspect, b.inspect
      s = 0
      [nil, :rel, :rev, :href, :title].each do |k|
        s = a.fetch(k, '').to_s <=> b.fetch(k, '').to_s
        break if s != 0
      end
      s
    end

    # we want to duplicate links from particular subjects (eg the root)
    (@duplicate || {}).sort do |a, b|
      a.first <=> b.first
    end.each do |s, preds|

      o = {}
      u = ufwd[s] ||= canonical_uuid s
      s = urev[u] ||= canonical_uri u if u
      f = {}

      # do not include this subject as these links are already included!
      next if u == @subject

      # gather up the objects, then gather up the predicates

      objects_for u || s, preds, only: :resource do |obj, rel|
        # XXX do not know why += |= etc does not work
        x = canonical_uuid(obj) || obj
        urev[x] ||= canonical_uri x
        y = o[x] ||= Set.new
        o[x] = y | rel
        f[x] = formats_for x
      end

      srel = @base.route_to((u ? urev[u] || s : s).to_s)

      # now collect all the other predicates
      o.keys.each do |obj|
        hrel = @base.route_to((urev[obj] || obj).to_s)
        o[obj] |= @repo.query([u || s, nil, obj]).predicates.to_set
        rels = abbreviate o[obj].to_a, vocab: XHV
        ln = { nil => :link, about: srel, rel: rels, href: hrel }
        ln[:type] = f[obj].first if f[obj]

        # add to links
        links << ln
      end
    end

    meta = []

    # include author names as old school meta tags
    authors.each do |a|
      name  = labels[urev[a] || a] or next
      datatypes.add name[0] # a convenient place to chuck this
      prop  = abbreviate(name[0])
      name  = name[1]
      about = @base.route_to((ufwd[a] || a).to_s)
      tag   = { nil => :meta, about: about.to_s, name: :author,
        property: prop, content: name.to_s }

      if name.has_datatype? and name.datatype != RDF::XSD.string
        tag[:datatype] = abbreviate(name.datatype)
      elsif name.has_language?
        tag['xml:lang'] = tag[:lang] = name.language
      end
      meta.push tag
    end

    literals.each do |k, v|
      next if k == title
      rel = abbreviate v.to_a, vocab: XHV
      elem = { nil => :meta, property: rel, content: k.to_s }
      elem[:name] = :description if k == desc

      if k.has_datatype?
        datatypes.add k.datatype # so we get the prefix
        elem[:datatype] = abbreviate k.datatype, vocab: XHV
      end

      meta.push(elem)
    end

    meta.sort! do |a, b|
      s = 0
      [:about, :property, :datatype, :content, :name].each do |k|
        # warn a.inspect, b.inspect
        s = a.fetch(k, '').to_s <=> b.fetch(k, '').to_s
        break if s != 0
      end
      s
    end

    # don't forget style tag
    style = doc.xpath('/html:html/html:head/html:style', { html: XHTMLNS })

    body = body.dup 1
    body = { '#body' => body.children.to_a, about: '' }
    body[:typeof] = abbreviate(types.to_a, vocab: XHV) unless
      types.empty?

    

    # prepare only the prefixes we need to resolve the data we need
    rsc = abbreviate(
      (struct.keys + resources.keys + datatypes.to_a +
        types.to_a + rstruct.to_a.flatten).uniq, noop: false).map do |x|
      next if x.nil?
      x.split(?:)[0].to_sym
    end.reject(&:nil?).to_set

    warn rsc

    pfx = prefixes.select do |k, _|
      rsc.include? k
    end.transform_values { |v| v.to_s }

    # XXX deal with the qb:Observation separately (just nuke it for now)
    extra = generate_twitter_meta || []
    bl_op = begin
              bads = @repo.query(
                [nil, RDF::SAK::CI.document, @subject]).subjects.to_set
              nope = %w[top contents index].map { |x| RDF::Vocab::XHV[x] }
              lambda { |s, p| bads.include? s or nope.include? p }
            end
    if bl = generate_backlinks(
      published: published, pattern: /^urn:uuid:/, terse: true,
      struct: rstruct, ignore: bl_op)
      extra << bl #{ [bl] => :object }
    end

    # and now for the document
    xf  = @transform
    doc = xhtml_stub(
      base: @base, prefix: pfx, vocab: XHV, lang: 'en', title: tm,
      link: links, meta: meta, style: style, transform: xf,
      extra: extra, body: body).document

    # goddamn script tags and text/html
    doc.xpath('//html:script[@src][not(node())]', XPATHNS).each do |script|
      script << doc.create_text_node('')
    end

    doc
  end

  

end
