require 'rdf'
require 'rdf/vocab'
require 'intertwingler/util/clean'
require 'intertwingler/nlp'
require 'intertwingler/ci'
require 'intertwingler/qb'
require 'time'
require 'stringio'
require 'nokogiri'
require 'md-noko'
require 'uuid-ncname'
require 'xml-mixup'

# This is the base class for (X)HTML+RDFa documents. It is a temporary
# situation intended to absorb the dissolution of
# {Intertwingler::Context::Document} and {Intertwingler::Util::Messy}. It will
# eventually get replaced by the much more robust
# {Intertwingler::Representation} paradigm, which, among other things, will
# be able to handle _non_-markup resources like images and such.
class Intertwingler::Document
  include XML::Mixup
  include Intertwingler::NLP
  include Intertwingler::Util::Clean

  private

  # this grabs a subset of the class methods defined herein and turns
  # 'em into instance methods, based on their argument signature
  def self.bind_instance_methods
    # `false` here gives us only the defined methods, not the inherited ones
    methods(false).each do |m|
      # obtain the method code itself
      proc = method m
      # get the argspec
      params = proc.parameters
      # a lot of methods use the resolver
      if params.first == [:req, :resolver]
        if params[1] == [:req, :subject]
          # a large subset of those follow with the subject
          define_method m do |*args, **opts, &block|
            proc.call @resolver, @subject, *args, **opts, &block
          end
        else
          # others do something else
          define_method m do |*args, **opts, &block|
            proc.call @resolver, *args, **opts, &block
          end
        end
      elsif params.first == [:req, :elem]
        # the rest operate over xml nodes and stuff
        define_method m do |*args, **opts, &block|
          proc.call(*args, **opts, &block)
        end
      end
    end
  end

  public

  class Feed < Intertwingler::Document
    def generate published: true
      # get related feeds
      related = @repo.objects_for(
        @subject, RDF::RDFS.seeAlso, only: :uri) - [@subject]

      # feed audiences
      faudy = @repo.audiences_for @subject
      faudn = @repo.audiences_for @subject, invert: true
      faudy -= faudn

      warn 'feed %s has audiences %s and non-audiences %s' %
        [@subject, faudy.inspect, faudn.inspect]

      docs = @repo.all_documents external: false, published: published

      # now we create a hash keyed by uuid containing the metadata
      authors = {}
      titles  = {}
      dates   = {}
      entries = {}
      latest  = nil
      docs.each do |uu|
        # basically make a jsonld-like structure
        #rsrc = struct_for uu

        # skip unless the entry is indexed
        next unless @repo.indexed? uu

        # get audiences
        audy = @repo.audiences_for uu, proximate: true
        audn = @repo.audiences_for uu, proximate: true, invert: true
        audy -= audn

        warn 'doc %s has audiences %s and non-audiences %s' %
          [uu, audy.inspect, audn.inspect]

        # we begin by assuming the document is *not* included in the
        # feed, and then we try to prove otherwise
        skip = true
        if audy.empty?
          # if both the feed and the document audiences are
          # unspecified, then include it, as both are for "everybody"
          skip = false if faudy.empty?
        elsif faudy.empty?
          # if the feed is for everybody but the document is for
          # somebody in particular, only include it if its audience is
          # in the list of the feed's non-audiences
          skip = false unless (audy - faudn).empty?
        else
          # now we deal with the case where both the feed and the
          # document have explicit audiences

          # if the document hasn't been explicitly excluded by the
          # feed, then include it if it has explicitly been included
          skip = false if !(audy - faudn).empty? && !(faudy & audy).empty?
        end

        next if skip

        canon = @resolver.uri_for uu, as: :uri

        xml = { '#entry' => [
          { '#link' => nil, rel: :alternate, href: canon, type: 'text/html' },
          { '#id' => uu.to_s }
        ] }

        # get published date first
        published = (objects_for uu,
                     [RDF::Vocab::DC.issued, RDF::Vocab::DC.created],
                     datatype: RDF::XSD.dateTime)[0]

        # get latest updated date
        updated = (objects_for uu, RDF::Vocab::DC.modified,
                   datatype: RDF::XSD.dateTime).sort[-1]
        updated ||= published || RDF::Literal::DateTime.new(DateTime.now)
        updated = Time.parse(updated.to_s).utc
        latest = updated if !latest or latest < updated

        xml['#entry'].push({ '#updated' => updated.iso8601 })

        if published
          published = Time.parse(published.to_s).utc
          xml['#entry'].push({ '#published' => published.iso8601 })
          dates[uu] = [published, updated]
        else
          dates[uu] = [updated, updated]
        end

        # get author(s)
        al = []
        @repo.authors_for(uu).each do |a|
          unless authors[a]
            n = @repo.label_for a
            x = authors[a] = { '#author' => [{ '#name' => n[1].to_s }] }

            if hp = @repo.objects_for(
              a, RDF::Vocab::FOAF.homepage, only: :resource).sort.first
              hp = @resolver.uri_for hp
            end

            hp ||= @resolver.uri_for a

            x['#author'].push({ '#uri' => hp.to_s }) if hp
          end

          al.push authors[a]
        end

        xml['#entry'] += al unless al.empty?

        # get title (note unshift)
        if (t = @repo.label_for uu)
          titles[uu] = t[1].to_s
          xml['#entry'].unshift({ '#title' => t[1].to_s })
        else
          titles[uu] = uu.to_s
        end

        # get abstract
        if (d = @repo.label_for uu, desc: true)
          xml['#entry'].push({ '#summary' => d[1].to_s })
        end

        entries[uu] = xml
      end

      # note we overwrite the entries hash here with a sorted array
      entrycmp = -> a, b {
        # first we sort by published date
        p = dates[a][0] <=> dates[b][0]
        # if the published dates are the same, sort by updated date
        u = dates[a][1] <=> dates[b][1]
        # to break any ties, finally sort by title
        p == 0 ? u == 0 ? titles[a] <=> titles[b] : u : p }
      entries = entries.values_at(
        *entries.keys.sort { |a, b| entrycmp.call(a, b) })
      # ugggh god forgot the asterisk and lost an hour

      # now we punt out the doc

      preamble = [
        { '#id' => @subject.to_s },
        { '#updated' => latest.iso8601 },
        { '#generator' => 'Intertwingler', version: Intertwingler::VERSION,
          uri: "https://github.com/doriantaylor/rb-rdf-sak" },
        { nil => :link, rel: :self, type: 'application/atom+xml',
          href: @resolver.uri_for(@subject) },
        { nil => :link, rel: :alternate, type: 'text/html',
          href: @resolver.base },
      ] + related.map do |r|
        { nil => :link, rel: :related, type: 'application/atom+xml',
         href: @resolver.uri_for(r) }
      end

      if (t = @repo.label_for @subject)
        preamble.unshift({ '#title' => t.last.to_s })
      end

      if (r = @repo.first_literal [@subject, RDF::Vocab::DC.rights, nil])
        rh = { '#rights' => r.to_s, type: :text }
        rh['xml:lang'] = r.language if r.has_language?
        preamble.push rh
      end

      markup(spec: { '#feed' => preamble + entries,
        xmlns: 'http://www.w3.org/2005/Atom' }).document
    end
  end

  class SiteMap < Intertwingler::Document

    def generate published: true
      base = resolver.base
      urls = {}

      @graph.all_documents(external: false, published: published).each do |doc|
        next if @graph.rdf_type? doc, RDF::Vocab::FOAF.Image
        uri = @resolver.uri_for doc
        next unless uri.authority && base && uri.authority == base.authority
        mtime = @graph.dates_for(doc).last
        nodes = [{ [uri.to_s] => :loc }]
        nodes << { [mtime.to_s] => :lastmod } if mtime
        urls[uri] = { nodes => :url }
      end

      urls = urls.sort.map { |_, v| v }

      markup(spec: { urls => :urlset,
        xmlns: 'http://www.sitemaps.org/schemas/sitemap/0.9' }).document
    end
  end

  class Stats < Intertwingler::Document

    def generate published: true
      base  = @resolver.uri_for @subject, as: :uri
      types = @resolver.abbreviate @graph.asserted_types(@subject)
      title = if t = @graph.label_for(@subject)
                [t[1].to_s, @resolver.abbreviate(t[0])]
              end
      cache = {}
      @repo.subjects_for(
        Intertwingler::Vocab::QB.dataSet, @subject, only: :resource).each do |o|
        if d = @repo.objects_for(o, Intertwingler::Vocab::CI.document, only: :resource).first
          if !published or @repo.published?(d)
            # include a "sort" time that defaults to epoch zero
            c = cache[o] ||= {
              doc: d, stime: Time.at(0).getgm, struct: @repo.struct_for(o) }

            if t = @repo.label_for(d)
              c[:title] = t
            end
            if a = @repo.label_for(d, desc: true)
              c[:abstract] = a
            end
            if ct = @repo.objects_for(d,
              RDF::Vocab::DC.created, datatype: RDF::XSD.dateTime).first
              c[:stime] = c[:ctime] = ct.object.to_time.getgm
            end
            if mt = @repo.objects_for(d,
              RDF::Vocab::DC.modified, datatype:RDF::XSD.dateTime)
              c[:mtime] = mt.map { |m| m.object.to_time.getgm }.sort
              c[:stime] = c[:mtime].last unless mt.empty?
            end
          end
        end
      end

      # sort lambda closure
      sl = -> a, b do
        x = cache[b][:stime] <=> cache[a][:stime]
        return x unless x == 0
        x = cache[b][:ctime] <=> cache[a][:ctime]
        return x unless x == 0
        ta = cache[a][:title] || Array.new(2, cache[a][:uri])
        tb = cache[b][:title] || Array.new(2, cache[b][:uri])
        ta[1].to_s <=> tb[1].to_s
      end

      rows = []
      cache.keys.sort(&sl).each do |k|
        c = cache[k]
        href = base.route_to @resolver.uri_for(c[:doc], as: :uri)
        dt = @resolver.abbreviate @graph.asserted_types(c[:doc])
        uu = URI(k.to_s).uuid
        nc = UUID::NCName.to_ncname uu, version: 1
        tp, tt = c[:title] || []
        ab = if c[:abstract]
               { [c[:abstract][1].to_s] => :th, about: href,
                property: @resolver.abbreviate(c[:abstract].first) }
             else
               { [] => :th }
             end

        td = [{ { { [tt.to_s] => :span,
                   property: @resolver.abbreviate(tp) } => :a,
                 rel: 'ci:document', href: href } => :th },
              ab,
              { [c[:ctime].iso8601] => :th, property: 'dct:created',
               datatype: 'xsd:dateTime', about: href, typeof: dt },
              { c[:mtime].reverse.map { |m| { [m.iso8601] => :span,
                 property: 'dct:modified', datatype: 'xsd:dateTime' } } => :th,
               about: href
              },
             ] + DSD_SEQ.map do |f|
          h = []
          x = { h => :td }
          p = Intertwingler::Vocab::CI[f]
          if y = c[:struct][p] and !y.empty?
            h << y = y.first
            x[:property] = @resolver.abbreviate p
            x[:datatype] = @resolver.abbreviate y.datatype if y.datatype?
          end
          x
        end
        rows << { td => :tr, id: nc, about: "##{nc}", typeof: 'qb:Observation' }
      end

      # XXX add something to the vocab so this can be controlled in the data
      xf = config.dig(:stats, :transform) || config[:transform]

      xhtml_stub(base: base, title: title, transform: xf, attr: {
        id: UUID::NCName.to_ncname_64(s.to_s), about: '', typeof: types },
                 prefix: @resolver.prefixes, content: {
                   [{ [{ [{ ['About'] => :th, colspan: 4 },
                          { ['Counts'] => :th, colspan: 4 },
                          { ['Words per Block'] => :th, colspan: 7 }] => :tr },
                       { TH_SEQ => :tr } ] => :thead },
                    { rows => :tbody, rev: 'qb:dataSet' }] => :table }).document
    end

  end

  # This class is for things like SKOS concept schemes, rosters,
  # reading lists, etc.
  class Index < Intertwingler::Document

    # we'll make this a class method
    def self.alphabetized_list resolver, subject, fwd: nil, rev: nil,
        published: true, preamble: RDF::Vocab::DC.description,
        transform: nil, &block
      raise ArgumentError,
        'We need a block to render the markup! it is not optional!' unless block

      repo = resolver.repo

      # plump these out
      fwd = fwd ? fwd.respond_to?(:to_a) ? fwd.to_a : [fwd] : []
      rev = rev ? rev.respond_to?(:to_a) ? rev.to_a : [rev] : []

      raise ArgumentError, 'Must have either a fwd or a rev defined' if
        fwd.empty? and rev.empty?

      # first we get them, then we sort them
      frels = {} # forward relations
      rrels = {} # reverse relations
      alpha = {} # alphabetical map
      seen  = {} # flat map

      # a little metaprogramming to get forward and reverse
      [[fwd, [frels, rrels], :objects_for,  [subject, fwd]],
       [rev, [rrels, frels], :subjects_for, [rev, subject]]
      ].each do |pa, rel, meth, args|
        next if pa.empty?

        send meth, *args, only: :resource do |n, pfwd, prev|
          # skip if we've already got this (eg with fwd then reverse)
          next if seen[n]
          # make a dummy so as not to incur calling published? multiple times
          seen[n] = {}
          # now we set the real struct and get the label
          st = seen[n] = repo.struct_for n, inverses: true

          # now check if it's published
          next if published and
            repo.rdf_type?(n, RDF::Vocab::FOAF.Document, struct: st) and
            not repo.published?(n)

          # now the relations to the subject
          rel.map { |r| r[n] ||= Set.new }
          rel.first[n] |= pfwd
          rel.last[n]  |= prev

          lab = (repo.label_for(n, struct: st) || [nil, n]).last.value.strip

          lab.gsub!(/\A[[:punct:][:space:]]*(?:An?|The)[[:space:]]+/i, '')

          # get the index character which will be unicode, hence these
          # festive character classes
          char = if match = /\A[[:punct:][:space:]]*
                   (?:([[:digit:]])|([[:word:]])|([[:graph:]]))
                   /x.match(lab)
                   match[2] ? match[2].upcase : ?#
                 else
                  ?#
                 end

          # add { node => struct } under this heading
          (alpha[char] ||= {})[n] = st
        end
      end

      # up until now we didn't need this; also add it to seen
      struct = seen[subject] ||= repo.struct_for subject, inverses: true

      # obtain the base and prefixes and generate the node spec
      base = resolver.uri_for subject, as: :rdf, slugs: true
      spec = alpha.sort { |a, b| a.first <=> b.first }.map do |key, structs|
        # sort these and run the block

        sections = structs.sort do |a, b|
          if a.nil? or b.nil?
            raise "#{key.inspect} => #{structs.inspect}"
          end
          al = repo.label_for(a.first, noop: true, struct: a.last).last
          bl = repo.label_for(b.first, noop: true, struct: b.last).last
          al.value.upcase <=> bl.value.upcase
        end.map do |s, st|
          # now we call the block
          fr = frels[s].to_a if frels[s] and !frels[s].empty?
          rr = rrels[s].to_a if rrels[s] and !rrels[s].empty?
          # XXX it may be smart to just pass all the structs in
          block.call s, fr, rr, st, base, seen
        end.compact

        { ([{[key] => :h2 }] + sections) => :section }
      end

      # now we get the page metadata
      pfx   = resolver.prefix_subset(seen).transform_values(&:to_s)
      abs   = repo.label_for(subject, struct: struct, desc: true)
      mn    = abs ? { abs.last => :description } : {} # not an element!
      meta  = head_meta(resolver, subject, struct: struct, meta_names: mn,
                        vocab: XHV) + twitter_meta(resolver, subject)
      links = head_links resolver, subject, struct: struct, vocab: XHV,
        ignore: seen.keys, rev: Intertwingler::Vocab::CI.document
      types = resolver.abbreviate repo.types_for(subject, struct: struct)
      title = if t = repo.label_for(subject, struct: struct)
                [t.last.to_s, resolver.abbreviate(t.first)]
              end

      if abs
        para = { [abs.last.to_s] => :p,
          property: resolver.abbreviate(abs.first) }
        para['xml:lang'] = abs.last.language if abs.last.language?
        para[:datatype]  = abs.last.datatype if abs.last.datatype?
        spec.unshift para
      end

      xhtml_stub(base: base, title: title, transform: transform,
        prefix: pfx, vocab: XHV, link: links, meta: meta,
        attr: { id: UUID::NCName.to_ncname_64(subject.to_s.dup),
          about: '', typeof: types }, content: spec
      ).document
    end

    bind_instance_methods
  end

  class AddressBook < Index
    # XXX do we use this mechanism or something else
    CLASSES = [RDF::Vocab::SiocTypes.AddressBook]

    def generate published: true
      # stick some logic here to sort out what kind of thing it is
      # (concept scheme, collection, ordered collection)

      # run this once
      rels = {
        broader: 'Has Broader',
        narrower: 'Has Narrower',
        related: 'Has Related' }.map do |k, v|
        [@repo.property_set(RDF::Vocab::SKOS[k]), v]
      end

      skospreds = rels.map(&:first).reduce(Set[]) { |s, a| s | a }.freeze

      ucache = {}
      scache = {}
      struct = @repo.struct_for subject
      neighbours = { subject => struct }

      types = @repo.types_for(subject, struct: struct)

      if @graph.type_is?(types, RDF::Vocab::SKOS.OrderedCollection)
        # sort
        list = if head = @repo.objects_for(
                 subject, RDF::Vocab::SKOS.memberList, only: :blank).sort.first
                 RDF::List.new(subject: head, graph: @repo).to_a.map do |s|
                   next if published and not @repo.published?(s)
                   neighbours[s] ||= {}
                   { [] => :script, type: 'application/xhtml+xml',
                    src: @resolver.uri_for(s, slugs: true) }
                 end.compact
               end

        spec = [{ list => :article, inlist: '',
                rel: @resolver.abbreviate(RDF::Vocab::SKOS.memberList) }]

        abs   = @repo.label_for(subject, struct: struct, desc: true)
        mn    = abs ? { abs.last => :description } : {} # not an element!
        meta  = head_meta(subject, struct: struct, meta_names: mn,
                          vocab: XHV) + twitter_meta(subject)
        links = head_links subject, struct: struct, vocab: XHV,
          ignore: neighbours.keys
        title = if t = @graph.label_for(subject, struct: struct)
                  [t.last.to_s, @resolver.abbreviate(t.first)]
                end
        if abs
          para = { [abs.last.to_s] => :p,
                  property: @resolver.abbreviate(abs.first) }
          para['xml:lang'] = abs.last.language if abs.last.language?
          para[:datatype]  = abs.last.datatype if abs.last.datatype?
          spec.unshift para
        end

        pfx = @resolver.prefix_subset neighbours

        xhtml_stub(base: base, title: title, transform: @transform,
          prefix: pfx, vocab: XHV, link: links, meta: meta,
          attr: { id: UUID::NCName.to_ncname_64(subject, version: 1),
                 about: '', typeof: @resolver.abbreviate(types) },
          content: spec).document
      elsif @graph.type_is?(types, RDF::Vocab::SKOS.Collection)
        # make a flat list of just transclusions
        alphabetized_list subject, fwd: RDF::Vocab::SKOS.member,
          published: published,
          ucache: ucache, scache: scache do |s, fp, rp, struct, base, seen|
            # just return the sections i guess lol
            { [''] => :script, type: 'application/xhtml+xml',
              src: @resolver.uri_for(s, slugs: true),
              rel: @resolver.abbreviate(fp),
              typeof: @resolver.abbreviate(
                @graph.types_for(s, struct: struct)) }
        end
      elsif @graph.type_is?(types, RDF::Vocab::SKOS.ConceptScheme)
        # note i'm not sure about this whole 'seen' business

        # WOOF lol
        alphabetized_list subject, rev: RDF::Vocab::SKOS.inScheme,
          published: published,
          ucache: ucache, scache: scache do |s, fp, rp, struct, base, seen|

          # may as well bag this while we're at it
          neighbours[s] ||= struct

          # sequence of elements beginning with the heading
          el = begin
                 lp, lo = @graph.label_for(s, struct: struct)
                 if lp
                   [literal_tag(lo, name: :h3, property: lp,
                                prefixes: @resolver.prefixes)]
                 else
                   [{ [s.to_s] => :h3 }]
                 end
               end

          # now we do definitions
          cmp = @graph.cmp_term
          el += @graph.find_in_struct(struct, RDF::Vocab::SKOS.definition,
                               entail: true, invert: true).sort do |a, b|
              cmp.(a.first, b.first)
          end.map do |o, ps|
            rel = @resolver.abbreviate(ps)
            if o.uri?
              { { [''] => :script, type: 'application/xhtml+xml',
                 src: @resolver.uri_for(o, slugs: true) } => :p, rel: rel }
            else
              para = { [o.value] => :p, property: rel }
              para['xml:lang'] = para[:lang] = o.language if o.language?
              para[:datatype] = o.datatype if o.datatype?
              para
            end.compact
          end

          # do alternate labels
          dl = @graph.find_in_struct(struct, RDF::Vocab::SKOS.altLabel,
                              entail: true, invert: true).sort do |a, b|
            a.first <=> b.first
          end.map do |o, ps|
            next unless o.literal?

            dd = { [o.value] => :dd, property: @resolver.abbreviate(ps) }
            dd[:'xml:lang'] = dd[:lang] = o.language if o.language?
            dd[:datatype] = o.datatype if o.datatype?
            dd
          end.compact
          dl.unshift({ ['Also known as'] => :dt }) unless dl.empty?

          # do relations
          dl += rels.map do |pred, dt|
            # this will give us a map of neighbours to the predicates
            # actually used to relate them
            objs = @graph.find_in_struct struct, pred, invert: true
            # plump up the structs
            objs.keys.each do |k|
              # neighbours[k] goes first or it is short circuited
              neighbours[k] ||= seen[k] ||= @resolver.struct_for(k,
                uuids: true, inverses: true)
            end

            # only show relations to concepts in this scheme
            objs.select! do |k, _|
              x = @graph.find_in_struct neighbours[k],
                RDF::Vocab::SKOS.inScheme, entail: true, invert: true
              x.key? subject
            end

            unless objs.empty?
              lcmp = @graph.cmp_label
              [{ [dt] => :dt }] + objs.sort do |a, b|
                lcmp.(a.first, b.first)
              end.map do |o, ps|
                # XXX this is where i would like canonical_uri to
                # just "know" to do this (also this will fail if
                # this is not a uuid)
                id = UUID::NCName.to_ncname_64(o.value.dup, version: 1)
                olp, olo = @graph.label_for(o, struct: neighbours[o])
                href = base.dup
                href.fragment = id
                { link_tag(href, rel: ps, base: base,
                  typeof: @graph.asserted_types(o, struct: struct),
                  property: olp, label: olo,
                  prefixes: @resolver.prefixes ) => :dd }
              end
            end
          end.compact

          # do backreferences

          op = @graph.query([nil, nil, s]).to_a.select do |stmt|
            sj = stmt.subject

            if sj.uri? and sj != subject
              # XXX there is probably a better way to do this
              ref_ok = if neighbours[sj]
                         np = neighbours[sj].keys.to_set - skospreds -
                           [RDF::Vocab::SKOS.member,
                            RDF::Vocab::SKOS.hasTopConcept]
                         no = neighbours[sj].values_at(*np.to_a).flatten.uniq
                         !np.empty? and no.include? s
                       else
                         true
                       end
              ref_ok and (!published or published?(sj))
            end
          end.reduce({}) do |hash, stmt|
            sj = stmt.subject
            seen[sj] ||= neighbours[sj] ||= @graph.struct_for(sj, inverses: true)
            unless skospreds.include? stmt.predicate
              (hash[sj] ||= []) << stmt.predicate
            end
            hash
          end

          # warn s, op

          unless op.empty?
            dl << { ['Referenced By'] => :dt }
            op.sort do |a, b|
              al = (label_for(a.first,
                candidates: neighbours[a.first]) || [a.first]).last
              bl = (label_for(b.first,
                candidates: neighbours[b.first]) || [b.first]).last
              al.value.upcase <=> bl.value.upcase
            end.each do |sj, ps|
              st   = neighbours[sj]
              href = @resolver.uri_for sj, slugs: true
              olp, olo = @graph.label_for(sj, struct: st)

              dl << { link_tag(href, rev: ps, base: base,
                typeof: @graph.asserted_types(sj, struct: st),
                property: olp, label: olo,
                prefixes: @resolver.prefixes) => :dd }
            end
          end

          # add see also
          sa = @graph.find_in_struct(
            struct, RDF::RDFS.seeAlso, invert: true).each do |o, _|
            seen[o] ||= neighbours[o] ||= @graph.struct_for(o, inverses: true)
          end

          unless sa.empty?
            dl << { ['See Also'] => :dt }
            lcmp = @graph.cmp_label cache: neighbours
            sa.keys.sort(&lcmp).each do |o|
              href = @resolver.uri_for o, slugs: true
              if st = neighbours[o]
                olp, olo = @graph.label_for(o, struct: st)
                dl << { link_tag(href, rel: sa[o], base: base,
                  typeof: @graph.types_for(o, struct: st),
                  property: olp, label: olo,
                  prefixes: @resolver.prefixes) => :dd }
              else
                dl << { link_tag(href, rel: ps, base: base) => :dd }
              end
            end
          end

          # add to set
          el << { dl => :dl } unless dl.empty?

          # id  = UUID::NCName.to_ncname_64(s.value.dup, version: 1)
          id = @resolver.uri_for s, slugs: true
          id = id.fragment || UUID::NCName.to_ncname_64(s.value.dup, version: 1)
          sec = { el => :section, id: id, resource: "##{id}" }
          if typ = @graph.asserted_types(s, struct: struct)
            sec[:typeof] = @resolver.abbreviate typ
          end
          sec[:rel] = @resolver.abbreviate(fp) if rp
          sec[:rev] = @resolver.abbreviate(rp) if rp
          sec
        end
      else
        raise ArgumentError,
          "Subject #{subject} must be some kind of SKOS entity"
      end
    end
  end

  # This will generate an (X)HTML+RDFa page containing either a
  # SKOS concept scheme or a collection, ordered or otherwise.
  #
  # XXX later on we should consider conneg for languages
  #
  #
  class ConceptScheme < Index
    CLASSES = [RDF::Vocab::SKOS.ConceptScheme, RDF::Vocab::SKOS.Collection]
  end

  class ReadingList < Index
    CLASSES = [RDF::Vocab::SiocTypes.ReadingList]

    private

    # XXX THIS LOOKS SUSPECT. yeah. like does this predate `generate_list`?

    LISTOP = -> opmap, seen, published do
      # we just sort this so it's consistent
      opmap.sort { |a, b| a.first <=> b.first }.map do |o, preds|
        li = RDF::List.new(subject: o, graph: @repo).to_a.map do |item|
          next if seen[item] or (published and @repo.published?(item))
          st = seen[item] = @repo.struct_for item
          lp, lo = (@repo.label_for(item, struct: st) || [nil, item])
          typ = @repo.types_for item, struct: st
          #a = { [lab.last] => :span, about: item, typeof: abbreviate(typ),
          #  property: abbreviate(lab.first) }
          a = [lo.value]
          dd = { a => :li, about: item }
          dd[:typeof] = @resolver.abbreviate(typ) if typ
          # literal stuff
          dd[:property]  = @resolver.abbreviate(lp) if lp
          dd[:datatype]  = @resolver.abbreviate(lo.datatype) if lo.datatype?
          dd['xml:lang'] = lo.language if lo.language?
          dd
        end.compact
        { { li => :ul, rel: @resolver.abbreviate(preds), inlist: '' } => :dd }
      end.compact
    end

    REGOP  = -> opmap, seen, published do
      opmap.map { |x| x + [struct_for(x.first, uuids: true)] }.sort do |a, b|
        al = (@repo.label_for(a.first, struct: a.last) || [nil, a.first])
        bl = (@repo.label_for(b.first, struct: b.last) || [nil, b.first])
        al.last.value <=> bl.last.value
      end.map do |item, preds, struct|
        next if seen[item] or (published and @repo.published?(item))
        st = seen[item] = @repo.struct_for item
        lab = @repo.label_for item, struct: st
        typ = @repo.types_for item, struct: st
        a = { [lab.last] => :span, about: item,
             typeof: @respolver.abbreviate(typ),
             property: @resolver.abbreviate(lab.first) }
        { a => :dd, rel: @resolver.abbreviate(preds) }
      end.compact
    end

    DLSPEC = {
      'By:' => {
        RDF::Vocab::BIBO.authorList      => LISTOP,
        RDF::Vocab::DC.creator           => REGOP,
      },
      'With:' => {
        RDF::Vocab::BIBO.contributorList => LISTOP,
        RDF::Vocab::DC.contributor       => REGOP,
      },
      'Edited by:' => {
        RDF::Vocab::BIBO.editorList      => LISTOP,
        RDF::Vocab::BIBO.editor          => REGOP,
      },
      'Translated by:' => {
        RDF::Vocab::BIBO.translator      => REGOP,
      },
    }

    AUTHOR_SPEC = [
      ['By:', [RDF::Vocab::BIBO.authorList, RDF::Vocab::DC.creator]],
      ['With:', [RDF::Vocab::BIBO.contributorList, RDF::Vocab::DC.contributor]],
      ['Edited by:', [RDF::Vocab::BIBO.editorList, RDF::Vocab::BIBO.editor]],
      ['Translated by:', [RDF::Vocab::BIBO.translator]],
    ].freeze

    public

    def generate published: true

      here = Set[@subject]

      # uggh put these somewhere
      preds = {
        hp:    @graph.predicate_set(RDF::Vocab::DC.hasPart),
        sa:    @graph.predicate_set(RDF::RDFS.seeAlso),
        canon: @graph.predicate_set([RDF::OWL.sameAs, CI.canonical]),
        ref:   @graph.predicate_set(RDF::Vocab::DC.references),
        al:    @graph.predicate_set(RDF::Vocab::BIBO.contributorList),
        cont:  @graph.predicate_set(RDF::Vocab::DC.contributor),
      }

      alphabetized_list fwd: RDF::Vocab::DC.hasPart,
        published: published do |s, fp, rp, struct, base, seen|

        here << s

        # let's just do this first
        kids = []
        sec  = { kids => :section, resource: s.value }
        if types = @repo.types_for(s, struct: struct)
          sec[:typeof] = @resolver.abbreviate(types)
        end
        sec[:rel] = @resolver.abbreviate(fp) if fp
        sec[:rev] = @resolver.abbreviate(rp) if rp

        lp, lo = @repo.label_for(s, struct: struct)
        lh = { property: @resolver.abbreviate(lp) }
        lh[:datatype]  = lo.datatype if lo.datatype?
        lh['xml:lang'] = lo.language if lo.language?

        # rdfs:seeAlso -> amazon (or whatever) link
        sa = @repo.find_in_struct(struct, RDF::RDFS.seeAlso, invert: true)
        if sa and !sa.empty?
          sao, sap = sa.sort { |a, b| a.first <=> b.first }.first
          sap = @resolver.abbreviate sap

          # lol add amazon affil tag
          # if /^(www\.)?amazon\./i.match? sao.host and
          #     amzn = @config.dig(:plugin, :amazon)
          #   qv = (sao.query_values(Array) || []).reject { |x| x.first == 'tag' }
          #   qv << ['tag', amzn]
          #   sao = sao.dup
          #   sao.query_values = qv
          # end

          span = { [lo.value] => :span, about: s.value }.merge lh
          kids << { { span => :a,
            rel: @resolver.abbreviate(sap), href: sao.value } => :h3 }
        else
          kids << { [lo.value] => :h3 }.merge(lh)
        end


        # now
        dli = DLSPEC.map do |dtl, ops|
          lseen = {}

          dd = ops.map do |p, op|
            opmap = @repo.find_in_struct struct, p, invert: true
            instance_exec(opmap, lseen, published, &op)
          end.flatten(1)

          seen.merge! lseen

          [{ [dtl] => :dt }] + dd unless dd.empty?
        end.compact.flatten(1)

        # now do references

        # everything that references either the subject or one of its seeAlsos
        # XXX think of a better way to accumulate
        structs = {}
        refs = ([s] + sa.keys + sa.keys.map { |k|
            @repo.subjects_for(preds[:canon], k, only: :uri, entail: false)
          }).flatten.uniq.map do |u|
          # get the subjects that point
          @repo.subjects_for(preds[:ref], u, only: :uri) do |rs, pfwd, prev|
            # maybe this structure will correctly communicate upstream
            unless structs[rs] or (published and !@repo.published?(rs))
              structs[rs] = @repo.struct_for(rs)
              [rs, [pfwd, prev]]
            end
          end.compact.to_h
        end.reduce({}) { |hout, hin| hout.merge! hin }.to_a.sort do |a, b|
          sa = structs[a.first]
          sb = structs[b.first]
          al = (@repo.label_for(a.first, struct: sa) || [nil, a.first])
          bl = (@repo.label_for(a.first, struct: sb) || [nil, b.first])
        #   warn [al.last.to_s.upcase, bl.last.to_s.upcase].inspect
          al.last.to_s.upcase <=> bl.last.to_s.upcase
        end.map do |k, v|
          st = structs[k]
          pfwd, prev = *v
          uri = @resolver.uri_for k, as: :uri
          lp, lo = (@repo.label_for(k, candidates: st) || [nil, k])

          if %w[http https].include? uri.scheme
            span = if lp
                     x = { [lo.to_s] => :span,
                          property: @resolver.abbreviate(lp) }
                     x[:datatype]  = lo.datatype if lo.datatype?
                     x['xml:lang'] = lo.language if lo.language?
                     x
                   else
                     [lo.to_s]
                   end
            x = { span => :a, href: base.route_to(uri) }
            if typ = @repo.types_for(k, struct: st)
              x[:typeof] = @resolver.abbreviate(typ)
            end
            x[:rev] = @resolver.abbreviate(pfwd) unless pfwd.empty?
            x[:rel] = @resolver.abbreviate(prev) unless prev.empty?
            { x => :dd }
          else
            x = { [lo.to_s] => :dd, resource: uri }
            if typ = @repo.types_for(k, struct: st)
              x[:typeof] = @resolver.abbreviate(typ)
            end
            x[:rev] = @resolver.abbreviate(pfwd) unless pfwd.empty?
            x[:rel] = @resolver.abbreviate(prev) unless prev.empty?
            x
          end
        end

        # warn refs.inspect

        unless refs.empty?
          refs.unshift({ ['Referenced by:'] => :dt })
          dli += refs
        end

        kids << { dli => :dl } unless dli.empty?

        sec
      end
    end
  end

  private

  # XXX DO SOMETHING COOLER HERE
  CLASS_MAP = {
    RDF::Vocab::SiocTypes.AddressBook => Intertwingler::Document::AddressBook,
    RDF::Vocab::SKOS.ConceptScheme    => Intertwingler::Document::ConceptScheme,
    RDF::Vocab::SKOS.Collection       => Intertwingler::Document::ConceptScheme,
    RDF::Vocab::SiocTypes.ReadingList => Intertwingler::Document::ReadingList,
  }

  # Default `generate` method generates the doc from triples
  def generate published: true
    generate_doc
  end

  public

  # Coerce an input into a {Nokogiri::XML::Document}.
  #
  # @param [IO, Pathname, Nokogiri::XML::Node, #to_s] whatever this is
  #
  # @return [Nokogiri::XML::Document] the parsed document.
  #
  def self.coerce_doc doc
    # turn the document into an XML::Document
    if doc.is_a? Nokogiri::XML::Node
      # a node that is not a document should be wrapped with one
      unless doc.is_a? Nokogiri::XML::Document
        d = doc.dup 1
        doc = Nokogiri::XML::Document.new
        doc << d
      end
    else
      type = nil

      # pathnames turned into IO objects
      if doc.is_a? Pathname
        type = Intertwingler::MimeMagic.by_path doc
        doc  = doc.open # this may raise if the file isn't there
      end

      # squash everything else to a string
      doc = doc.to_s unless [IO, StringIO].any? { |c| doc_is_a? c }

      # check type by content
      type ||= Intertwingler::MimeMagic.by_magic(doc)

      # can you believe there is a special bookmarks mime type good grief
      type = 'text/html' if type == 'application/x-mozilla-bookmarks'

      # now we try to parse the blob
      if type.to_s =~ /xml/i
        doc = Nokogiri.XML doc
      elsif type == 'text/html'
        # if the detected type is html, try it as strict xml first
        attempt = nil
        begin
          attempt = Nokogiri.XML doc, nil, nil, (1 << 11) # NONET
        rescue Nokogiri::XML::SyntaxError
          # do not wrap this a second time; let it fail if it's gonna
          tmp = Nokogiri.HTML doc
          attempt = Nokogiri::XML::Document.new
          attempt << tmp.root.dup(1)
        end
        doc = attempt
      elsif type.to_s =~ /^text\/(?:plain|(?:x-)?markdown)/i
        # just assume plain text is markdown
        doc = ::MD::Noko.new.ingest doc
      else
        raise "Don't know what to do with #{uuid} (#{type})"
      end
    end

    # now fix the namespaces for mangled html documents
    root   = doc.root
    if root.name == 'html'
      unless root.namespace
        # clear this off or it will be duplicated in the output
        root.remove_attribute('xmlns')
        # now generate a new ns object
        ns = root.add_namespace(nil, XHTMLNS)
        # *now* scan the document and add the namespace declaration
        root.traverse do |node|
          if node.element? && node.namespace.nil?
            # downcasing the name may be cargo culting; need to check
            # node.name = node.name.downcase # yup it is
            node.namespace = ns
          end
        end
      end

      # also add the magic blank doctype declaration if it's missing
      unless doc.internal_subset
        doc.create_internal_subset('html', nil, nil)
      end
    end

    doc
  end

  # Initialize the document, dispatching to the correct subclass.
  #
  # @param resolver [Intertwingler::Resolver] the resolver
  # @param subject [RDF::URI] the subject URI
  #
  # @return [Intertwingler::Document]
  #
  def self.new resolver, subject, **args
    if self == Intertwingler::Document
      # snag the graph
      repo = resolver.repo

      # find us some types
      types = args[:type] = args[:type] ? resolver.coerce_resources(args[:type]) :
        repo.types_for(subject)

      # this will return us either the first matching subclass or self
      # by using a dummy lambda in the "not found" arg of `detect`
      cls = CLASS_MAP.detect(-> { [self] }) do |pair|
        repo.type_is? types, pair.first
      end.last

      # this is apparently how you do this according to
      # https://blog.appsignal.com/2018/08/07/ruby-magic-changing-the-way-ruby-creates-objects.html
      instance = cls.allocate
      instance.send :initialize, resolver, subject, **args
      instance
    else
      super
    end
  end

  attr_reader :resolver, :subject, :repo, :uri, :doc

  # Initialize the document.
  #
  # @param resolver [Intertwingler::Resolver] the resolver
  # @param subject [RDF::URI] the subject URI
  # @param uri [RDF::URI, URI] the routable URI
  # @param doc
  #
  def initialize resolver, subject, uri: nil, doc: nil, mtime: nil,
      type: nil, lang: nil
    @resolver = resolver
    @repo     = resolver.repo
    @subject  = subject
    @uri      = uri ? resolver.coerce_resource(uri, as: :uri) :
      resolver.uri_for(subject, as: :uri)

    # obtain the types (uh why? lol)
    @types = type ? resolver.coerce_resources(type) : @repo.types_for(subject)

    # if a document is handed in, we read it, otherwise we generate it
    @doc = doc ? self.class.coerce_doc(doc) : generate
  end

  # Transform the document and return it.
  def transform
    root = doc.root
    ns   = root.namespace && root.namespace.href
    return doc.dup unless root.name == 'html' or ns == XHTMLNS
    # we strip off the <head> and generate a new one
  end

  # Transform the document and replace it internally.
  def transform!
    @doc = transform
  end

  # these can all get converted into transformation functions

  def self.head_links resolver, subject, struct: nil, nodes: nil, prefixes: {},
      ignore: [], uris: {}, labels: {}, vocab: nil, rev: []
    repo = resolver.repo

    raise 'ignore must be Array or Set' unless
      [Array, Set].any? { |c| ignore.is_a? c }

    rev = rev.respond_to?(:to_a) ? rev.to_a : [rev]

    struct ||= repo.struct_for subject
    nodes  ||= repo.invert_struct struct

    # XXX is this smart?
    revs = {}
    repo.subjects_for(rev, subject, only: :resource) do |s, ps|
      revs[s] ||= Set.new
      revs[s] |= ps
    end

    # make sure these are actually URI objects not RDF::URI
    uris = uris.transform_values { |v| URI(resolver.preproc v.to_s) }
    uri  = uris[subject] || resolver.uri_for(subject, as: :uri, slugs: true)

    # make ignore more robust
    ignore = ignore.uniq.select(&:iri?).map do |i|
      [i, resolver.uuid_for(i, noop: true, verify: false)]
    end.flatten.to_set

    # warn ignore.sort.inspect

    # output
    links = []

    { false => nodes, true => revs }.each do |reversed, obj|
      obj.reject { |n, _| ignore.include?(n) || !n.uri? }.each do |k, v|
        # first nuke rdf:type, that's never in there
        v = v.dup.delete RDF.type unless reversed
        next if v.empty?

        unless uris[k]
          cu = resolver.uri_for k, slugs: true
          uris[k] = URI(resolver.preproc(cu || k.to_s))
        end

        # munge the url and make the tag
        ru  = uri.route_to(uris[k])
        ln  = { nil => :link, href: ru.to_s }
        ln[reversed ? :rev : :rel] = resolver.abbreviate v.to_a,
          scalar: false, vocab: vocab

        # add the title
        if lab = labels[k]
          ln[:title] = lab[1].to_s
        end

        # add type attribute
        unless (mts = @repo.formats_for k).empty?
          ln[:type] = mts.first.to_s

          if ln[:type] =~ /(java|ecma)script/i ||
              !(v.to_set & Set[RDF::Vocab::DC.requires]).empty?
            ln[:src] = ln.delete :href
            # make sure we pass in an empty string so there is a closing tag
            ln.delete nil
            ln[['']] = :script
          end
        end

        # warn ln.inspect

        # finally add the link
        links << ln
      end
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

    links
  end

  def self.head_meta resolver, subject, struct: nil, nodes: nil, prefixes: {},
      ignore: [], meta_names: {}, vocab: nil, lang: nil, xhtml: true

    raise 'ignore must be Array or Set' unless
      [Array, Set].any? { |c| ignore.is_a? c }

    repo = resolver.repo

    struct ||= repo.struct_for subject
    nodes  ||= repo.invert_struct struct

    ignore = ignore.to_set

    meta = []
    nodes.select { |n| n.literal? && !ignore.include?(n) }.each do |k, v|
      rel  = resolver.abbreviate v.to_a, vocab: vocab
      tag  = { nil => :meta, property: rel, content: k.to_s }

      lang = (k.language? && k.language != lang ? k.language : nil) ||
        (k.datatype == RDF::XSD.string && lang ? '' : nil)
      if lang
        tag['xml:lang'] = lang if xhtml
        tag[:lang] = lang
      end

      tag[:datatype] = resolver.abbreviate k.datatype, vocab: XHV if k.datatype?
      tag[:name] = meta_names[k] if meta_names[k]

      meta << tag
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

    meta
  end

  def self.twitter_meta resolver, subject
    repo = resolver.repo

    # get author
    author = repo.authors_for(subject, unique: true) or return []

    # get author's twitter account
    twitter = repo.objects_for(author, RDF::Vocab::FOAF.account,
      only: :resource).select { |t| t.to_s =~ /twitter\.com/
    }.sort.first or return []
    twitter = URI(twitter.to_s).path.split(/\/+/)[1]
    twitter = ?@ + twitter unless twitter.start_with? ?@

    # get title
    title = repo.label_for(subject) or return []

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
    img = repo.objects_for(
      subject, RDF::Vocab::FOAF.depiction, only: :resource)
    unless img.empty?
      img = resolver.uri_for img.first
      out.push({ nil => :meta, name: 'twitter:image', content: img })
      out.first[:content] = :summary_large_image
    end

    # return the appropriate xml-mixup structure
    out
  end

  # transform non-ogp to ogp
  def self.ogp resolver, subject
  end

  # transform non-sdo to sdo
  def self.sdo resolver, subject
  end

  def self.backlinks resolver, subject, published: true, ignore: nil
    repo = resolver.repo

    uri = @resolver.uri_for(
      subject, as: :uri, slugs: true) || URI(@resolver.preproc subject)

    ignore ||= Set.new
    raise 'ignore must be amenable to a set' unless ignore.respond_to? :to_set
    ignore = ignore.to_set
    nodes  = {}
    labels = {}
    types  = {}
    repo.query([nil, nil, subject]).each do |stmt|
      next if ignore.include?(sj = stmt.subject)
      preds = nodes[sj] ||= Set.new
      preds << (pr = stmt.predicate)
      types[sj]  ||= repo.types_for sj
      labels[sj] ||= repo.label_for sj
      labels[pr] ||= repo.label_for pr
    end

    # prune out unpublished resources if we are relegating to published
    nodes.select! { |k, _| repo.published? k } if published

    return if nodes.empty?

    lcmp = repo.cmp_label

    li = nodes.sort(&lcmp).map do |rsrc, preds|
      cu  = resolver.uri_for(rsrc, as: :uri) or next
      lab = labels[rsrc] || [nil, rsrc]
      lp  = resolver.abbreviate(lab.first) if lab.first
      ty  = resolver.abbreviate(types[rsrc]) if types[rsrc]

      { [{ [{ [lab[1].to_s] => :span, property: lp }] => :a,
        href: uri.route_to(cu), typeof: ty,
        rev: resolver.abbreviate(preds) }] => :li }
    end.compact

    { [{ li => :ul }] => :nav }
  end

  def add_rdfa
  end

  def add_section_ids
  end

  # METHODS THAT ONLY TOUCH MARKUP

  # Return true if the element is (X)HTML.
  #
  # @param elem [Nokogiri::XML::Node] the node/element to test
  #
  # @return [false, true]
  #
  def html? elem
    # make sure we find an element
    elem = elem.document? ? elem.root : elem.element? ? elem : elem.parent
    ns = elem.namespace && elem.namespace.href
    ns == XHTMLNS or !ns && elem.name.downcase == 'html'
  end

  # Normalize the indentation of the document.
  #
  # @param elem [Nokogiri::XML::Node] where to begin reindenting
  # @param depth [Integer], where to start the indentation
  # @param indent [String], the indentation string to use
  #
  # @return [Nokogiri::XML::Node] the node in `elem`
  #
  def reindent elem, depth = 0, indent = '  '
    kids = elem.children
    if kids and child = kids.first
      loop do
        if child.element?
          # recurse into the element
          reindent child, depth + 1, indent
        elsif child.text?
          text = child.content || ''

          # optional horizontal whitespace followed by at least
          # one newline (we don't care what kind), followed by
          # optional horizontal or vertical whitespace
          preamble = !!text.gsub!(/\A[ \t]*[\r\n]+\s*/, '')

          # then we don't care what's in the middle, but hey let's get
          # rid of dos newlines because we can always put them back
          # later if we absolutely have to
          text.gsub!(/\r+/, '')

          # then optionally any whitespace followed by at least
          # another newline again, followed by optional horizontal
          # whitespace and then the end of the string
          epilogue = !!text.gsub!(/\s*[\r\n]+[ \t]*\z/, '')

          # if we prune these off we'll have a text node that is
          # either the empty string or it isn't (note we will only
          # register an epilogue if the text has some non-whitespace
          # in it, because otherwise the first regex would have
          # snagged everything, so it's probably redundant)

          # if it's *not* empty then we *prepend* indented whitespace
          if preamble and !text.empty?
            d = depth + (child.previous ? 1 : 0)
            text = "\n" + (indent * d) + text
          end

          # then we unconditionally *append*, (modulo there being a
          # newline in the original at all), but we have to check by
          # how much: if this is *not* the last node then depth + 1,
          # otherwise depth
          if preamble or epilogue
            d = depth + (child.next ? 1 : 0)
            text << "\n" + (indent * d)
          end

          child.content = text
        end

        break unless child = child.next
      end
    end

    elem
  end

  # Isolate an element into a new document.
  #
  # @param elem [Nokogiri::XML::Node] the root of the subtree
  # @param xpath [String] the XPath statement
  # @param reindent [false, true] whether to reindent the result
  # @param prefixes [Hash] XPath prefix/namespace map
  #
  # @return [Nokogiri::XML::Node, nil]
  #
  def subtree elem, xpath = '/*', reindent: true, prefixes: {}
    # at this time we shouldn't try to do anything cute with the xpath
    # even though it is attractive to want to prune out prefixes

    # how about we start with a noop
    return doc.root.dup if xpath == '/*'

    begin
      nodes = doc.xpath xpath, prefixes
      return unless
        nodes and nodes.is_a?(Nokogiri::XML::NodeSet) and !nodes.empty?
      out = Nokogiri::XML::Document.new
      out << nodes.first.dup
      reindent out.root if reindent
      out
    rescue Nokogiri::SyntaxError
      return
    end
  end

  private

  public

  # Returns the base URI from the perspective of the given element.
  # Can optionally be coerced into either a URI or RDF::URI. Also
  # takes a default value.
  #
  # @param elem [Nokogiri::XML::Node] the context element
  # @param default [nil, #to_s] the default URI
  # @param coerce [nil, :uri, :rdf] the coercion scheme, if any
  #
  # @return [nil, String, URI, RDF::URI] the context's base URI
  #
  def get_base elem, default: nil, coerce: nil
    coerce = assert_uri_coercion coerce

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
        pfx = k.split(?:).first or next # otherwise error
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
    prefix.transform_values! do |v|
      v ? URI_COERCIONS[coerce].call(v) : v
    end if coerce

    # don't proceed if `traverse` is false
    return prefix unless traverse

    # save us having to recurse in ruby by using xpath implemented in c
    xpath = '%s::*[namespace::*|@prefix|@vocab]' %
      (descend ? :descendant : :ancestor)
    elem.xpath(xpath).each do |e|
      # this will always merge our prefix on top irrespective of direction
      prefix = get_prefixes(e, traverse: false, coerce: coerce).merge prefix
    end

    prefix
  end

  private

  def assert_uri_coercion coerce
    if coerce
      coerce = coerce.to_s.to_sym if coerce.respond_to? :to_s
      raise 'coerce must be either :uri or :rdf' unless
        URI_COERCIONS.keys.include?(coerce)
    end
    coerce
  end

  def assert_xml_node node
    raise 'Argument must be a Nokogiri::XML::Element' unless
      node.is_a? Nokogiri::XML::Element
    node
  end

  def internal_subject_for node, prefixes: nil, base: nil, as: nil,
      is_ancestor: false

    raise ArgumentError, 'Elements only' unless node.element?

    # note we assign these AFTER the literal check or it will be wrong
    prefixes ||= get_prefixes node

    # document base is different from supplied base
    base  = @resolver.coerce_resource base, as: :uri if base
    dbase = @resolver.coerce_resource(get_base(node) || base, as: :uri)

    # ???
    base ||= dbase

    # answer a bunch of helpful questions about this element
    subject = nil
    parent  = node.parent
    ns_href = node.namespace.href if node.namespace
    up_ok   = %w[rel rev].none? { |a| node.key? a }
    is_root = !(parent && parent.element?)
    special = /^(?:[^:]+:)?(?:head|body)$/i === node.name and
      (ns_href == 'http://www.w3.org/1999/xhtml' or
      /^(?:[^:]+:)?html$/xi === parent.name)

    # if the node is being inspected as an ancestor to the
    # original node, we have to check it backwards.
    if is_ancestor
      # ah right @resource gets special treatment
      if subject = node[:resource]
        subject = @resolver.resolve_curie subject, term: false,
          prefixes: prefixes, base: dbase, scalar: true
      else
        # then check @href and @src
        %w[href src].each do |attr|
          if node.key? attr
            # merge with the root and return it
            subject = dbase + node[attr]
            break
          end
        end
      end

      return @resolver.coerce_resource subject, as: as if subject

      # note if we are being called with is_ancestor, that means
      # the original node (or indeed any of the nodes previously
      # tested) have anything resembling a resource in them. this
      # means @rel/@rev should be ignored, and we should keep
      # looking for a subject.
    end

    if node[:about]
      subject = @resolver.resolve_curie node[:about], prefixes: prefixes,
        base: dbase, term: true, scalar: true

      # ignore coercion
      return subject if subject.is_a? RDF::Node

    elsif is_root
      # note this is parameter base not document base
      subject = base
    elsif special
      # same deal here
      subject = internal_subject_for parent, base: base
    elsif node[:resource]
      # XXX resolve @about against potential curie
      subject = @resolver.resolve_curie node[:resource], prefixes: prefixes,
        base: dbase, term: true, scalar: true
    elsif node[:href]
      # XXX 2021-05-30 you can't just use this; you have to find a rel
      # or rev that isn't itself disrupted by about/resource/href/src
      # or typeof/inlist. you already figured this out for the xslt
      # rdfa query engine so go look there.
      subject = dbase + node[:href]
    elsif node[:src]
      subject = dbase + node[:src]
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
    elsif parent.element?
      subject = internal_subject_for parent,
        base: base || dbase, is_ancestor: true
    else
      raise "this should never get here"
    end

    @resolver.coerce_resource subject, as: as if subject
  end

  public

  # Given an X(HT)ML element, return the nearest RDFa _subject_.
  # Optionally takes +:prefix+ and +:base+ parameters which override
  # anything found in the document tree.
  #
  # @param node [Nokogiri::XML::Element] the node
  # @param prefixes [Hash] Prefix mapping. Overrides derived values.
  # @param base [#to_s,URI,RDF::URI] Base URI, overrides as well.
  # @param as [nil, :rdf, :uri] the coercion regime
  #
  # @return [URI,RDF::URI,String] the subject
  #
  def subject_for elem, prefixes: nil, base: nil, as: :rdf
    assert_xml_node elem
    as = assert_uri_coercion as

    if n = elem.at_xpath(XPATH[:literal])
      return internal_subject_for n, prefixes: prefixes, base: base, as: as
    end

    internal_subject_for elem, prefixes: prefixes, base: base, as: as
  end

  # Return the language in scope for the current (X|HT)ML element.
  #
  # @param node [Nokogiri::XML::Element]
  # @return [nil, String] the RFC3066 language tag
  #
  def lang_for elem
    lang = elem.lang || elem['lang']
    if lang
      return if lang.strip.empty?
      return lang.strip.downcase.tr(?_, ?-)
    end
    lang_for elem.parent if
      elem.element? and elem.parent and elem.parent.element?
  end

  # this replaces divs and stuff with html5 elements
  def modernize elem
    elem.xpath(XPATH[:modernize], XPATHNS).each do |e|
      # gotta instance_exec because `markup` is otherwise unbound
      instance_exec e, &MODERNIZE[e.name.to_sym]
    end
  end

  # Recurse into an X(HT?)ML document, harvesting inline elements that
  # may contain terminology. Returns an array of arrays of the form
  # `[subject, text, lang, datatype, alts]`, which can be manipulated
  # by a block. Note the block also gets the element as its last
  # argument.
  #
  # @param node [Nokogiri::XML::Node] the origin node
  # @param mapping [Hash] A mapping of namespaces to arrays of tags
  # @yieldparam text [String] the element's (flattened) text
  # @yieldparam alt  [String, nil] the element's alternate text
  #   (currently hard-coded as the `title` attribute)
  # @yieldparam name [Symbol] the element's local name
  # @yieldparam node [Nokogiri::XML::Element] the current element
  # @yieldreturn [Array] a potentially modified array of inputs
  # @return [Array] an array of arrays
  #
  def scan_inlines elem, prefixes: nil, base: nil, coerce: :rdf, &block
    elem.xpath(XPATH[:rehydrate], XPATHNS).map do |e|
      # extract some useful bits from the thing
      subject = subject_for e, prefixes: prefixes, base: base, coerce: coerce
      text    = (e.content || '').strip
      attrs   = %w[href title aria-label content datetime value].map do |a|
        if e.key? a and !(v = e[a].strip).empty?
          [a.to_sym, v]
        end
      end.compact.to_h

      # nothing to see here, move along
      next if text.empty? and attrs.empty?

      # conditionally set the language
      lang = lang_for e
      attrs[:lang] = lang if lang

      # note we only add the datatype now so that test above works
      attrs[:datatype] = e[:datatype] if e[:datatype]

      # run the block if there is one
      if block
        block.call subject, text, attrs, e
      else
        # otherwise
        [subject, text, attrs, e.name.to_sym]
      end
    end.compact.uniq
  end

  # Remove all `<head>` content aside from `<title>` and `<base>`;
  # revert all links to their canonical UUIDs (where applicable).
  #
  # @param elem [Nokogiri::XML::Node]
  #
  # @return [Nokogiri::XML::Node]
  #
  def sanitize elem

    # and away we go
    out = elem.dup

    out.xpath(XPATH[:sanitize], XPATHNS).each do |e|
      # XXX this shouldn ot be a problem post-refactor
      base  = base_for e, @uri
      attrs = %i[about resource] + URL_ELEMS.fetch(e.name.to_sym, [])

      attrs.each do |a|
        next unless e.key? a.to_s
        uri = base + e[a].strip

        # warn "#{e.name} #{a} #{uri}"

        e[a] = @resolver.uuid_for uri, noop: true, as: :str
      end
    end

    out
  end

  # Strip all the links surrounding and RDFa attributes off
  # `dfn`/`abbr`/`span` tags. Assuming a construct like `<a
  # rel="some:relation" href="#..." typeof="skos:Concept"><dfn
  # property="some:property">Term</dfn></a>` is a link to a glossary
  # entry, this method returns the term back to an undecorated state
  # (`<dfn>Term</dfn>`).
  #
  # @param elem [Nokogiri::XML::Node]
  #
  def dehydrate elem
    elem.xpath(XPATH[:dehydrate], XPATHNS).each do |e|
      e = e.replace e.elements.first.dup
      %w[about resource typeof rel rev property datatype].each do |a|
        e.delete a if e.key? a
      end
    end

    elem
  end

  def rehydrate elem, base: nil, cache: {}, rescan: false, &block
    # collect all the literals
    @repo.each_object do |o|
      lemma = Intertwingler::NLP.lemmatize o.value
      (cache[lemma.downcase] ||= Set.new) << o if o.literal?
    end

    node.xpath(XPATH[:rehydrate], XPATHNS).each do |e|
      # split the xpath up so it isn't as costly to run
      next if e.at_xpath(XPATH[:rh_filter], XPATHNS)

      lang = e.xpath(XPATH[:lang]).to_s.strip.downcase
      # dt   = e['datatype'] # not used currently

      # deal with <time> element XXX should also deal with XMLLiteral
      text = (e.name == 'time' && e['datetime'] ||
              e['content'] || e.content).strip

      # now we have the literals actually in the graph
      lit = cache[Intertwingler::NLP.lemmatize(text).downcase] or next
      lit = lit.to_a.sort do |a, b|
        c = 0
        if lang
          ac = a.language? && a.language.downcase == lang ? -1 : 0
          bc = b.language? && b.language.downcase == lang ? -1 : 0
          c = ac <=> bc
        end

        if c == 0
          c = b.value.length <=> a.value.length # prefer longer strings
          c == 0 ? a.value <=> b.value : c # otherwise lexical sort
        else
          c
        end
      end

      # candidates
      cand = {}
      lit.map { |t| @repo.query([nil, nil, t]).to_a }.flatten.each do |x|
        y = cand[x.subject] ||= {}
        (y[:stmts] ||= []) << x
        y[:types]  ||= @repo.query([x.subject, RDF.type, nil]).objects.sort
      end

      # passing a block to this method enables e.g. interactive
      # control over which candidates, if any, get applied to the tag.
      if block_given?
        # the block is expected to return one of the candidates or
        # nil. we call the block with the graph so that the block can
        # manipulate its contents.
        chosen = block.call cand, e
        raise ArgumentError, 'block must return nil or a term' unless
          chosen.nil? or chosen.is_a? RDF::Term
      elsif !cand.empty?
        # y'know some kind of deterministic differentiation mechanism
        # would be useful here but i can't think of one
        chosen = cand.keys.first
      end

      if chosen
        # we assume this has been retrieved from the graph
        cc = cand[chosen]
        unless cc
          cc = cand[chosen] = {}
          cc[:stmts] = @repo.query([chosen, nil, lit.first]).to_a.sort
          cc[:types] = @repo.query([chosen, RDF.type, nil]).objects.sort
          # if either of these are empty then the graph was not
          # appropriately populated
          raise 'Missing a statement relating #{chosen} to #{text}' if
            cc[:stmts].empty?
        end

        # we should actually probably move any prefix/vocab/xmlns
        # declarations from the inner node to the outer one (although
        # in practice this will be an unlikely configuration)
        pfx   = get_prefixes e
        ebase = get_base e, default: base

        # find the subject for this node
        subject = subject_for(e, prefixes: pfx, base: ebase)
        preds = if subject
                  su = @resolver.uuid_for(subject) || subject
                  pp = @repo.query([su, nil, chosen]).predicates.uniq

                  if pp.empty?
                    pp << Intertwingler::Vocab::CI.mentions
                    pp.each { |p| graph << [su, p, chosen] } if rescan
                  end

                  warn "#{su} #{pp.inspect} #{chosen}"

                  pp
                else
                  []
                end

        # here we have pretty much everything except for the prefixes
        # and wherever we want to actually link to.

        inner = e.dup
        spec  = { [inner] => :a, href: chosen.to_s }
        spec[:rel] = resolver.abbreviate preds, prefixes: pfx unless preds.empty?
        # we should have types
        spec[:typeof] = resolver.abbreviate cc[:types], prefixes: pfx unless
          cc[:types].empty?

        markup replace: e, spec: spec
      end
    end
    # return maybe the elements that did/didn't get changed?
  end

  # these all need the resolver

  def self.rewrite_links resolver, elem, base: nil
  end

  def self.title_tag resolver, predicates, content,
      prefixes: {}, vocab: nil, lang: nil, xhtml: true

    # begin with the tag
    tag = { '#title' => content.to_s, property: resolver.abbreviate(
      predicates, prefixes: prefixes, vocab: vocab) }

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
      tag[:datatype] = resolver.abbreviate(
        content.datatype, prefixes: prefixes, vocab: vocab)
    end

    tag
  end

  # Generate a tag in the XML::Mixup spec format that contains a
  # single literal. Defaults to `:span`.
  #
  # @param value [RDF::Term] the term to be represented
  # @param name  [Symbol, String] the element name
  # @param property [RDF::URI, Array] the value of the `property=` attribute
  # @param text [String] literal text (puts value in `content=`)
  # @param prefixes [Hash] prefixes we should know about for making CURIEs
  #
  # @return [Hash] the element spec
  #
  def self.literal_tag resolver, value, name: :span, property: nil, text: nil,
      prefixes: {}, vocab: nil
    # literal text content if different from the value
    content = if value.literal? and text and text != value.value
                value.value
              end

    out = { [text || value.value] => name }
    out[:content]  = content if content
    out[:property] = resolver.abbreviate(
      property, prefixes: prefixes, vocab: vocab) if property

    # almost certain this is true, but not completely
    if value.literal?
      out['xml:lang'] = value.language if value.language?
      out[:datatype]  = resolver.abbreviate(
        value.datatype, prefixes: prefixes, vocab: vocab) if value.datatype?
    end

    # note you can do surgery to this otherwise
    out
  end

  # Generate a tag in the XML::Mixup spec format that contains a
  # single text link. Defaults to `:a`. Provides the means to include
  # a label relation.
  #
  # @param target [RDF::URI]
  #
  # @return [Hash] the element spec
  #
  def self.link_tag resolver, target, rel: nil, rev: nil, href: nil, about: nil,
      typeof: nil, label: nil, property: nil, name: :a, placeholder: nil,
      base: nil, prefixes: nil, vocab: nil

    # * target is href= by default
    # * if we supply an href=, target becomes resource=
    if href
      resource = target
    else
      href = target
    end

    # make a relative uri but only if we have a base, otherwise don't bother
    if base
      href = href.is_a?(URI) ? href : URI(resolver.preproc href.to_s)
      base = base.is_a?(URI) ? base : URI(resolver.preproc base.to_s)
      href = base.route_to(href)
    end

    # construct the label tag/relation
    ltag = if property and label.is_a? RDF::Literal
             literal_tag resolver, label, property: property,
               prefixes: prefixes, vocab: vocab
            else
              [label.to_s]
            end

    # make the element with the bits we know for sure
    out = { ltag => name, href: href }

    # make the attributes
    { rel: rel, rev: rev, about: about,
     typeof: typeof, resource: resource }.each do |attr, term|
      out[attr] = resolver.abbreviate term,
        prefixes: prefixes, vocab: vocab if term
    end

    out
  end

  # Generate an (X)HTML+RDFa list from what is assumed to be a bnode
  #
  # @param repo  [RDF::Repository]
  # @param list  [RDF::Term]
  # @param base  [RDF::URI, URI]
  # @param langs [#to_a, String]
  # @param rel   [RDF::Term, #to_a]
  # @param rev   [RDF::Term, #to_a]
  # @return [Hash]
  #
  def self.generate_list resolver, list, base: nil, langs: [],
      rel: nil, rev: nil, prefixes: {}, ncache: Set.new, ordered: true
    repo = resolver.repo

    list = RDF::List.new(subject: list) unless list.is_a? RDF::List

    ol = { inlist: '' }
    if rel
      # the presence of rel= or rev= mean the subject has to go in
      # resource= instead of about=
      ol[:rel]      = resolver.abbreviate rel, prefixes: prefixes
      ol[:rev]      = resolver.abbreviate rev, prefixes: prefixes if rev
      ol[:resource] = resolver.abbreviate list.subject, prefixes: prefixes
    elsif rev
      ol[:rev]      = resolver.abbreviate rev, prefixes: prefixes
      ol[:resource] = resolver.abbreviate list.subject, prefixes: prefixes
    else
      ol[:about] = resolver.abbreviate list.subject, prefixes: prefixes
    end

    strings = []

    li = list.to_a.map do |item|
      case item
      when RDF::Literal
        strings << item.value.strip
        literal_tag resolver, item, name: :li, prefixes: prefixes
      when RDF::Resource
        ts = repo.struct_for, item
        tt = repo.types_for item, struct: ts
        labp, labo = repo.label_for item, struct: ts, type: tt
        # XXX labp might actually be more than one predicate, never
        # thought of that

        # get everything into the cache
        ncache |= repo.smush_struct ts
        ncache |= tt.to_set
        ncache << labp
        ncache << labo

        # append to strings
        strings << (labo || item).value.strip

        href = resolver.uri_for(item, base: base) || item
        tag = link_tag href, base: base, prefixes: prefixes,
          property: labp, label: labo, typeof: tt
        { '#li' => tag }
      when RDF::Node
        frag, fstr = generate_fragment resolver, item, base: base, name: :li,
          ncache: ncache, prefixes: prefixes, langs: langs, wrap_list: true
        # append all the strings in the fragment
        strings << fstr
        frag
      end
    end

    # now finish off with the tag name and don't forget the meta
    [ol.merge({ "##{ordered ? ?o : ?u}l" => li }), strings.join(' ').strip]
  end


  # Generate an (X)HTML fragment in XML::Mixup spec format. The
  # fragment takes the form of a root node which is intended to
  # represent the subject. The presence of `rel=` or `rev=` attributes
  # will cause the subject to show up in `resource=` rather than
  # `about=`. Adjacent resources are represented as `<a>` elements
  # which get their asserted types and default (long) labels resolved,
  # and these are collated with the adjacent literals to produce a
  # list which is sorted according to configured criteria. Predicates
  # are rolled up into `rel=`, `rev=`, and `property=` attributes.
  # `rdf:XMLLiteral` terms are parsed and interwoven into the
  # markup. Blank nodes are collected at the bottom of the list as
  # (potentially recursively) embedded subtrees, sorted (for now) by
  # node ID, unless there is a cycle, in which case the cycle is
  # broken.
  #
  # ```
  # <name about="#subject" typeof="my:Type">
  #   <member rel="some:resource other:predicate">
  #     <a href="/wherever" typeof="another:Type">
  #       <span property="my:label" xml:lang="en">A link</span>
  #     </a>
  #   </member>
  #   <member property="some:literal" datatype="a:dt">A literal</member>
  #   <member rel="another:relation" resource="_:blank">
  #     <member property="lol:embedded">this recurses..</member>
  #   </member>
  #   <ol rel="some:list" resource="_:lol" inlist="">
  #     <li datatype="list:literal">foo</li>
  #     <li>...(fragment recurses)</li>
  #   </ol>
  # </name>
  # ```
  #
  # @note Collating properties might actually turn out to be dumb, and
  #  instead what I should be doing is grouping by property (and an
  #  intermediate sort by property label), but that will result in
  #  redundancies in the meat of the markup. The goal with this
  #  generator is really just to get the data onto the page where it
  #  can be picked up and manipulated by some downstream
  #  processor. Any more sophisticated markup generation on this side
  #  is going to have to be controlled by something like Loupe.
  #
  # @param repo [RDF::Repository]
  # @param subject [RDF::Resource, RDF::Node]
  # @param struct [Hash, nil]
  # @param base [RDF::URI, URI]
  # @param langs [Hash, Array, String] a representation of `Accept-Language`
  # @param rel [RDF::Resource, Array, nil]
  # @param rev [RDF::Resource, Array, nil]
  # @param prefixes [Hash]
  # @param tag [Symbol]
  # @param ptag [Symbol] the html tag
  # @param otag [Symbol]
  # @param pskip [#to_set] a set of _edges_ (not nodes) to skip
  # @param oskip [#to_set] a set of _nodes_ (not edges) to skip
  # @param wrap_list [false, true] whether to wrap a list with an element
  # @return [Array] pair containing the markup spec and the string value
  #
  def self.generate_fragment resolver, subject, struct: nil, base: nil, langs: [],
      rel: nil, rev: nil, prefixes: {}, ncache: Set.new,
      tag: :div, ptag: :div, otag: :div, pskip: [], oskip: [], wrap_list: false

    repo = resolver.repo

    # we need to collate the strings
    strings = []

    ncache << subject if ncache

    # determine if subject is a list and return early
    if repo.query([subject, RDF.first, nil]).first
      if wrap_list
        out, lstr = generate_list resolver, subject, base: base, ncache: ncache,
          langs: langs, prefixes: prefixes
        out = { "##{name}" => out }

        # append list strings to meta
        strings << lstr

        # any rel or rev will be part of this element then
        out[:rel] = resolver.abbreviate rel, prefixes: prefixes if rel
        out[:rev] = resolver.abbreviate rel, prefixes: prefixes if rev

        return [out, strings.join(' ').strip]
      else
        # otherwise just pass it along
        out, lstr = generate_list resolver, subject, base: base, ncache: ncache,
          langs: langs, rel: rel, rev: rev, prefixes: prefixes
        strings << lstr

        return [out, strings.join(' ').strip]
      end
    end

    # okay now we get to the actual thing
    struct ||= repo.struct_for subject, base: base

    ncache |= repo.smush_struct struct

    # what we're probably gonna want to do then is get all the labels
    # for all the URI references as well as the string values of any
    # embedded fragments; literals are going to be their own labels

    pscore = struct.map { |p, os| [p, os.count] }.to_h
    nodes  = repo.invert_struct(struct).map do |o, ps|
      next if oskip.include?(o) or !(pskip.to_set & ps).empty?

      pmax = ps.map { |x| pscore[ps] }.max
      m = t = nil
      case o
      when RDF::Literal
        m = literal_tag resolver, o, name: otag, prefixes: prefixes, property: ps
        t = o.value.strip
      when RDF::Resource
        ts = repo.struct_for o
        tt = repo.types_for o, struct: ts

        labp, labo = repo.label_for o, struct: ts

        href = resolver.uri_for(o) || o

        m = { "##{otag}" => link_tag(resolver, href, base: base,
          prefixes: prefixes, property: labp, label: labo, typeof: tt, rel: ps) }

        t = (labo || o).value.strip

      when RDF::Node
        m, t = generate_fragment resolver, o, base: base, tag: otag, rel: ps
      end
      [o, pmax, t, m]
    end.compact.sort do |a, b|
      ao, ap, at, _ = a
      bo, bp, bt, _ = b
      c = bp <=> ap
      c = at.downcase <=> bt.downcase if c == 0
      c = at <=> bt if c == 0
      c = ao <=> bo if c == 0
      c
    end.map do |o, _, t, m|
      strings << t
      [o, m]
    end.to_h

    out = { "##{tag}" => nodes.values }
    out[:typeof] = resolver.abbreviate(
      struct[RDF.type], prefixes: prefixes) if struct[RDF.type]
    out[:rel] = resolver.abbreviate(rel, prefixes: prefixes) if rel
    out[:rev] = resolver.abbreviate(rev, prefixes: prefixes) if rev

    # we actually want to return some metadata along with this, in
    # particular the fragment's string value (ie the concatenation of
    # all the text nodes)

    [out, strings.join(' ').strip]
  end

  # Generate a rudimentary (X)HTML document based on a subject node.
  #
  # Properties with `owl:inverseOf` relations are resolved and flipped
  # around, as are instances of `owl:SymmetricProperty`.  Reverse
  # relations that can't be resolved this way are put in `<link>`
  # elements in the `<head>` (with the `title=` attribute set to the
  # short label for handy downstream rendering). In the case that the
  # reverse adjacent is a blank node, an effort is made to resolve the
  # nearest non-blank resources and place their addresses in `href=`
  # while the blank node goes into `resource=`. An attempt is also
  # made to determine the `<title>` (using #label_for). What remains
  # is passed to #generate_fragment.
  #
  # @param resolver [Intertwingler::Resolver]
  # @param subject
  # @param struct
  # @param langes
  # @param prefixes
  # @param vocab
  # @param title
  #
  # @return [Nokogiri::XML::Document] the document
  #
  def self.generate_doc resolver, subject, struct: nil, langs: [],
      prefixes: {}, vocab: nil, title: false

    repo = resolver.repo

    # we will need to cache nodes and properties
    ncache = Set.new
    # pcache = Set.new

    # compute the struct
    struct = repo.struct_for subject, inverses: true

    # get the content of the title
    labp, labo = repo.label_for subject, struct: struct

    ncache |= repo.smush_struct struct
    ncache << labp
    ncache << labo

    # initialize the skips
    pskip = [RDF.type]
    oskip = []

    if title
      pskip += labp
      oskip << labo.dup
    end

    # generate what should be the request-uri
    uri = resolver.uri_for subject

    # otherwise the body is just a special kind of fragment
    body, _ = generate_fragment resolver, subject, struct: struct, base: uri,
      prefixes: prefixes, langs: langs, ncache: ncache,
      tag: :body, ptag: nil, otag: :p, pskip: pskip, oskip: oskip

    # warn ncache.inspect

    pfx = resolver.prefix_subset ncache

    # generate the title
    ttag = title_tag resolver, labp, labo, prefixes: prefixes if labo

    XML::Mixup.xhtml_stub(
      base: uri, prefix: pfx, vocab: vocab, title: ttag, body: body
    ).document
  end

  bind_instance_methods
end
