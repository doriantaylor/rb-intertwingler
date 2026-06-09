require 'intertwingler/engine'
require 'intertwingler/field'
require 'rack'
require 'rack/mock'
require 'time'

# This is the static site generator harness I should have written
# years ago instead of jerking around in `pry`, lol.
#
class Intertwingler::Static

  # initialize with engine
  def initialize engine, target, private: Pathname('.private')
    @engine  = engine
    @target  = target
    @priv    = binding.local_variable_get(:private)
  end

  attr_reader :engine, :source, :target

  def log
    engine.log
  end

  def resolver
    engine.resolver
  end

  def repo
    resolver.repo
  end

  def get uri, accept: nil, time: nil
    uuid = resolver.uuid_for(uri) or return
    uri  = resolver.uri_for(uuid || uri, as: :uri, slugs: true, fragments: true)

    return unless resolver.base.route_to(uri).relative?

    # warn "#{uuid} -> #{uri}"
    env  = Rack::MockRequest.env_for uri.to_s
    env['REQUEST_URI'] = uri.request_uri # for some reason not included?
    env['HTTP_CACHE_CONTROL'] = 'no-store'
    env['HTTP_ACCEPT'] = %w[application/xhtml+xml application/xml
                            text/html;q=0.8 */*;q=0.5].join(', ')
    env['HTTP_IF_MODIFIED_SINCE'] = time.httpdate if time

    req = Rack::Request.new env

    engine.handle req
  end

  # write an individual resource

  # write all resources

  # write feeds

  # write indices

  # write site map

  # write rewrite maps
  def write_one subject, time: Time.now
    subject = resolver.uuid_for(subject) or return

    uuid = resolver.coerce_resource subject, as: :uri

    # get types for output
    # types = repo.types_for subject

    mtime = target.glob("#{uuid.uuid}.*").map do |f|
      break unless f.file?
      f.stat.mtime
    end.compact.max

    # simulate GETs to everything
    resp = get(subject, time: mtime) or return

    if resp.successful?
      lm = Intertwingler::Field['last-modified'][resp]&.value || time
      bn = uuid.uuid
      ct = MimeMagic[resp.content_type]
      bn += ct.extensions.empty? ? '' : ".#{ct.extensions.first}"
      path = target + bn

      if path.exist? and path.stat.mtime >= lm
        log.info "skipping #{path}: #{lm}"
        return
      end

      body = Intertwingler::Representation::BodyWrap.coerce resp.body

      path.open('wb') do |fh|
        while buf = body.read(8192)
          fh << buf
        end
      end

      # touch the file to last-modified
      path.utime lm, lm

      log.debug "wrote #{bn}"
    elsif resp.redirect? || resp.status == 304 # i guess 304 isn't a redirect
      log.info "#{resp.status}: (#{subject}) #{resp.content_type} -> #{resp.body.inspect}"
    else
      log.error "#{resp.status}: (#{subject}) #{resp.content_type} -> #{resp.body.inspect}"
    end

    # not sure what this should return
    resp
  end

  # Write everything in the space to the file system (may take a while).
  def write_all published: nil, &block
    now = Time.now
    # gather up list of everything
    docs = repo.all_documents.each { |subject| write_one subject, time: now }

    warn "got #{docs.size} documents lol"

  end

  # @!group stuff rescued from `intertwingler.rb`

  # bulk scan for terms
  def scan_terms types: [RDF::Vocab::SKOS.Concept, RDF::Vocab::FOAF.Agent],
      published: false, docs: nil
    docs ||= all_internal_docs published: published

    # we make a temporary repository
    # XXX maybe only select the target types?
    scratch = RDF::Repository.new
    @graph.query([nil, RDF.type, nil]) do |stmt|
      scratch << @graph.query([stmt.subject, nil, nil]) if
        type_is? stmt.object, types
    end

    # we also start with a pool of terms as they appear in the text
    pool = {}

    # we iterate over the docs
    docs.each do |s|
      # obtain the content
      doc = visit(s) or next

      # note the objective here is to get the statements in the rdfa
      # that are not already in the graph

      # remove garbage from the <head> from the working version; it
      # was thrown in there ad-hoc
      html = doc.doc.dup 1
      html.xpath(
        '/html:html/html:head/*[not(self::html:title|self::html:base)]',
        Intertwingler::Util::XPATHNS).each(&:unlink)

      # slurp up any rdfa, swapping in canonical uuids; note that if
      # we're doing this here then we assume they are authoritative
      # and don't check them against the graph
      RDF::RDFa::Reader.new(html, host_language: :xhtml5,
                            version: :'rdfa1.1').each do |stmt|
        if stmt.subject.iri? and
            su = @resolver.uuid_for(stmt.subject, verify: false)
          stmt.subject = su
        end
        if stmt.object.iri? and
            ou = @resolver.uuid_for(stmt.object, verify: false)
          stmt.object = ou
        end

        scratch << stmt
      end

      # we want to construct a net (hash) where the keys are all
      # acceptable lexical representations of all the terms, and the
      # values are hashes containing those representations  entities

      # okay now scan the doc for terms
      doc.scan_inlines do |subject, text, attrs, elem|
        # step zero: normalize the damn thing
        text = normalize_space text

        # inline alternate text or nil if none
        title = attrs[%i[content aria-label title].detect { |a| attrs[a] }]
        title = nil if title == text

        # okay we have three grades of label: pref, alt, hidden

        # pref is always going to be the most differentiated lexical
        # representation of whatever concept; we begin by assuming
        # that the text in the `text` position is the preferred one
        pref = text.dup

        # alt is going to be stuff like synonyms and abbreviations
        # that can be linked to an expanded term
        alt = Set[]

        # things like plurals and possessives, as well as normalized
        # lowercase representations of proper nouns go into hidden
        hidden = Set[]

        # do the possessives now
        if m = /^(.+?)['\u2019]s?$/.match(pref)
          hidden << pref
          pref = m.captures.first.strip
        end

        # proper noun?
        proper = false

        type = RDF::Vocab::SKOS.Concept

        case elem.name.to_sym
        when :abbr
          # the term is definitely an abbreviation if it is in an
          # <abbr> tag

          proper = true

          # do it this way rather than lemmatize
          if m = /^(.+?)e?s$/.match(pref)
            hidden << pref
            pref = m.captures.first.strip
          end

          # abbr title means the abbr text is the alt
          if title
            alt << pref
            pref = title
          end
        when :dfn
          # the term is definitely a concept if it is in a <dfn> tag
          if title
            hidden << pref # note this is hidden not alt
            pref = title
          end
        when -> x { x == :span and elem.attributes.empty? }
          # if it is in a <span> tag with exactly zero attributes,
          # then it is something like an agent
          proper = true
          type = RDF::Vocab::FOAF.Agent
        when -> x { x == :span and (title || '').split.include? pref }
          # same deal but use the title instead
          hidden << pref
          pref   = title
          proper = true
          type   = RDF::Vocab::FOAF.Agent
        else
          # unset pref to signal no more operations
          pref = nil
          type = nil
        end

        if pref
          words = pref.split
          # we can be confident the term is a proper noun if it is
          # more than one word and the first and last are capitalized
          proper = true if words.length > 2 and
            words.any? { |w| /\p{Upper}/.match w }
          if proper
            tmp = pref.downcase
            hidden << tmp if tmp != pref
          else
            # if we haven't decided conclusively that the term is a
            # proper noun (XXX THIS IS WHERE A TAGGER WOULD BE HANDY)
            # then we can go ahead and downcase and lemmatize it
            pref = pref.downcase
            tmp  = doc.lemmatize pref
            if tmp != pref
              hidden << pref
              pref = tmp
            end
          end

          # all the candidate keys
          t = Set[pref] | alt | hidden
          k = pool.keys.detect { |k| t.include? k }
          res = k ? pool[k] : {
            pref: pref, type: type, alt: Set[], hidden: Set[], refs: Set[] }

          # move the key to an alt if the existing one is shorter
          if res[:pref].length < pref.length
            res[:alt] << res[:pref]
            res[:pref] = pref
          end

          # now merge alt and hidden
          res[:alt]    |= alt
          res[:hidden] |= hidden

          # now add these in case we missed them
          t |= res[:alt] | res[:hidden]

          t.each { |k| pool[k] ||= res }

          # get the subject for this node
          if s = doc.subject_for(elem, coerce: :rdf)
            s = @resolver.uuid_for(s, verify: false) || s
            res[:refs] << s
          end
        end

      end
    end

    # okay NOW we want to construct a net of all the concepts etc,
    # where the keys are normalized, lowercased strings, and the
    # values are *sets* of resources
    entries    = {}
    candidates = {}

    scratch.query([nil, RDF.type, nil]) do |stmt|
      next unless type_is? stmt.object, types
      s = stmt.subject
      entry  = entries[s] = { id: s}
      struct = entry[:struct] = struct_for s, only: :literal
      pref   = entry[:pref] = (label_for(s, repo: scratch,
                                         candidates: struct) || []).last
      alts   = entry[:alts] = (label_for s, repo: scratch, unique: false,
                               candidates: struct, alt: true).map(&:last)

      # we want all the labels
      labels = alts.dup
      labels.unshift pref if pref
      labels.uniq.each do |label|
        lstr = label.value.downcase.strip
        next if lstr.empty?
        mapping = candidates[lstr] ||= {}
        mapping[s] = entry
      end
    end

    out = pool.values.uniq
    out.each do |entry|
      entry[:id] = uuidv4
    end

    out
  end

  # csv output format:
  #
  # * first row: uri (uuid), type, preflabel, description, seealso
  # * second row: if missing the uuid in the first column then it is
  #   assumed columns 2 through N are altlabels
  # * third row: same deal but for hidden labels
  # * fourth row: same deal but for inverse references
  #
  # the next record starts when there is a uri in the first column
  #
  # @return [Array] suitable for csv output

  def scan_to_csv types: [RDF::Vocab::SKOS.Concept, RDF::Vocab::FOAF.Agent],
      published: false, docs: nil
    terms = scan_terms types: types, published: published, docs: docs

    out = [
      ['ID', 'Type', 'Preferred Label', 'Description', 'See Also']
    ]

    terms.sort do |a, b|
      c = a[:type] <=> b[:type]
        c == 0 ? a[:pref].downcase <=> b[:pref].downcase : c
      end.each do |term|
        out << [term[:id].to_s, @resolver.abbreviate(term[:type]), term[:pref].to_s]
        %i[alt hidden refs].each do |sym|
          out << (term[sym].empty? ? [] :
                  ([nil] + term[sym].to_a.map(&:to_s).sort))
        end
      end

      out
    end

    def ingest_concept_csv rows
      # temporary graph
      out = RDF::Repository.new

      subject = nil
      ctr = 0
      rows.each do |row|
        row = row.fields if row.respond_to? :fields
        row.map! do |c|
          c = c.to_s.strip
          c.empty? ? nil : c
        end

        if row.first
          subject = RDF::URI(row.first)

          if row[1] # type
            type = @resolver.resolve_curie row[1]
            out << [subject, RDF.type, type]
          end
          if row[2] # preflabel
            pref = RDF::Literal(row[2], language: 'en')
            out << [subject, RDF::Vocab::SKOS.prefLabel, pref]
          end
          if row[4] # description
            desc = RDF::Literal(row[3], language: 'en')
            out << [subject, RDF::Vocab::SKOS.definition, desc]
          end
          #
          row.drop(4).compact.each do |c|
            c = RDF::URI(c)
            out << [subject, RDF::RDFS.seeAlso, c]
          end

          ctr = 1
        elsif ctr == 1 # alt labels
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::Literal(c, language: 'en')
            out << [subject, RDF::Vocab::SKOS.altLabel, c]
          end
        elsif ctr == 2 # hidden labels
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::Literal(c, language: 'en')
            out << [subject, RDF::Vocab::SKOS.hiddenLabel, c]
          end
        elsif ctr == 3 # inverse refs
          ctr += 1
          row.drop(1).compact.each do |c|
            c = RDF::URI(c)
            out << [c, RDF::Vocab::DC.references, subject]
          end
        end
      end

      out
    end

  def generate_audience_csv file = nil, published: true

      require 'csv'
      file = coerce_to_path_or_io file if file
      lab = {}

      @concepts ||= @graph.all_related(RDF::Vocab::SKOS.Concept).to_set

      out = @graph.all_documents(published: published, external: false,
                                 exclude: RDF::Vocab::FOAF.Image).map do |s|
        u = @resolver.uri_for s
        x = @graph.struct_for s
        c = x[RDF::Vocab::DC.created] ? x[RDF::Vocab::DC.created][0] : nil
        _, t = @graph.label_for s, struct: x
        _, d = @graph.label_for s, struct: x, desc: true

        # # audience(s)
        # a = objects_for(s, RDF::Vocab::DC.audience).map do |au|
        #   next lab[au] if lab[au]
        #   _, al = label_for au
        #   lab[au] = al        file = file.expand_path.open('wb') unless file.is_a? IO

        csv = CSV.new file
        out.each { |x| csv << x }
        file.flush
      end

      out
    end

    CSV_PRED = {
      audience:    RDF::Vocab::DC.audience,
      nonaudience: CI['non-audience'],
      subject:     RDF::Vocab::DC.subject,
      assumes:     CI.assumes,
      introduces:  CI.introduces,
      mentions:    CI.mentions,
    }

    def ingest_csv file
      file = coerce_to_path_or_io file

      require 'csv'

      # key mapper
      km = { uuid: :id, url: :uri }
      kt = -> (k) { km[k] || k }

      # grab all the concepts and audiences

      audiences = {}
      all_of_type(CI.Audience).map do |c|
        s = struct_for c

        # homogenize the labels
        lab = [false, true].map do |b|
          @graph.label_for(c, struct: s, unique: false, alt: b).map { |x| x[1] }
        end.flatten.map { |x| x.to_s.strip.downcase }

        # we want all the keys to share the same set
        set = nil
        lab.each { |t| set = audiences[t] ||= set || Set.new }
        set << c
      end

      concepts = {}
      all_of_type(RDF::Vocab::SKOS.Concept).map do |c|
        s = struct_for c

        # homogenize the labels
        lab = [false, true].map do |b|
          @graph.label_for(c, struct: s, unique: false, alt: b).map { |x| x[1] }
        end.flatten.map { |x| x.to_s.strip.downcase }

        # we want all the keys to share the same set
        set = nil
        lab.each { |t| set = concepts[t] ||= set || Set.new }
        set << c
      end

      data = CSV.read(file, headers: true,
                      header_converters: :symbol).map do |o|
        o = o.to_h.transform_keys(&kt)
        s = @resolver.uuid_for(o.delete :id) or next

        # LOLOL wtf

        # handle audience
        [:audience, :nonaudience].each do |a|
          if o[a]
            o[a] = o[a].strip.split(/\s*[;,]+\s*/, -1).map do |t|
              if t =~ /^[a-z+-]+:[^[:space:]]+$/
                u = RDF::URI(t)
                @resolver.uuid_for(u) || u
              elsif audiences[t.downcase]
                audiences[t.downcase].to_a
              end
            end.flatten.compact.uniq
          else
            o[a] = []
          end
        end

        # handle concepts
        [:subject, :introduces, :assumes, :mentions].each do |a|
          if o[a]
            o[a] = o[a].strip.split(/\s*[;,]+\s*/, -1).map do |t|
              if t =~ /^[a-z+-]+:[^[:space:]]+$/
                u = RDF::URI(t)
                @resolver.uuid_for(u) || u
              elsif concepts[t.downcase]
                concepts[t.downcase].to_a
              end
            end.flatten.compact.uniq
          else
            o[a] = []
          end

        end

        CSV_PRED.each do |sym, pred|
          o[sym].each do |obj|
            @graph << [s, pred, obj]
          end
        end

        [s, o]
      end.compact.to_h
      data
    end

  # @!group map-related

  # generate rewrite map(s)
    def generate_rewrite_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, published: published
      base = @resolver.base
      umap = {}
      rwm  = {}

      docs.each do |doc|
        next unless doc.uri?
        tu = umap[doc] = URI(doc.to_s)
        next unless tu.respond_to?(:uuid)

        @resolver.uri_for(doc, scalar: false, slugs: true).each do |cu|
          # must be http(s)
          next unless cu.scheme.to_s.downcase.start_with? 'http'

          # skip fragments
          next if cu.fragment

          # skip external links obvs
          # XXX THIS DOES NOT WORK
          # next unless base.route_to(cu).relative?

          # this should though
          next unless cu.host == base.host

          # skip /uuid form
          cp = cu.request_uri.delete_prefix '/'
          next if tu.uuid == cp

          # XXX
          # next if @graph.replacements_for(doc, published: published).empty?

          (rwm[cp] ||= []) << doc
        end
      end

      # ranking hash
      r = {}

      pslug = @config[:private].basename

      rwm.transform_values do |uuids|
        uuid = if uuids.size == 1
                 umap[uuids.first]
               else
                 # otherwise we need to figure out what the right one is
                 uuids.each do |u|
                   r[u] ||= @graph.ranking_data_for u, ints: true
                 end

                 u = uuids.sort do |a, b|
                   c = r[a][:replaced]   <=> r[b][:replaced]
                   c = r[a][:retired]    <=> r[b][:retired]    if c == 0
                   c = r[b][:published]  <=> r[a][:published]  if c == 0
                   c = r[b][:circulated] <=> r[a][:circulated] if c == 0
                   c = r[b][:mtime]      <=> r[a][:mtime]      if c == 0
                   c = r[b][:ctime]      <=> r[a][:ctime]      if c == 0

                   # okay this is us giving up
                   c = a.to_s <=> b.to_s if c == 0

                   c # finally
                 end.first

                 umap[u]
               end

        # note this is a pathname
        ((@graph.published?(RDF::URI(uuid.to_s)) ? '' : pslug) + uuid.uuid).to_s
      end
    end

    # give me all UUIDs of all documents, filter for published if
    # applicable
    #
    # find the "best" (relative) URL for the UUID and map the pair
    # together
    def generate_uuid_redirect_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, fragments: true,
        published: published
      base = @resolver.base

      # keys are /uuid, values are
      out = {}
      docs.each do |doc|
        next unless doc.uri?
        tu = URI(doc.to_s)
        cu = @resolver.uri_for doc, as: :uri
        next unless tu.respond_to?(:uuid) and cu.respond_to?(:request_uri)
        next if tu.fragment

        # skip /uuid form
        cp = cu.request_uri.delete_prefix '/'
        next if cu.host == base.host && tu.uuid == cp

        # all redirect links are absolute
        out[tu.uuid] = cu.to_s
      end
      out
    end

    # find all URIs/slugs that are *not* canonical, map them to slugs
    # that *are* canonical
    def generate_slug_redirect_map published: false, docs: nil
      docs ||= @graph.all_documents external: false, fragments: true,
        published: published
      base = @resolver.base

      # for redirects we collect all the docs, plus all their URIs,
      # separate canonical from the rest

      # actually an easy way to do this is just harvest all the
      # multi-addressed docs, remove the first one, then ask for the
      # canonical uuid back,

      fwd = {}
      rev = {}
      out = {}

      docs.each do |doc|
        next unless doc.uri?
        uris  = @resolver.uri_for doc, scalar: false, slugs: true, as: :uri
        canon = uris.shift
        next unless canon.respond_to? :request_uri
        # XXX not sure i want to do this
        # next if canon.fragment

        # cache the forward direction
        fwd[URI(doc.to_s)] = canon

        # warn uris.inspect

        uris.each do |uri|
          next unless uri.respond_to? :request_uri
          next if canon == uri
          next unless base.route_to(uri).relative?
          next if uri.fragment

          # warn "#{canon} <=> #{uri}"

          requri = uri.request_uri.delete_prefix '/'
          next if requri.empty?
          # XXX why would i want to eliminate UUIDs?
          # next if requri == '' ||
          #  requri =~ /^[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$/

          # cache the reverse direction
          rev[uri] = requri
        end
      end

      # warn fwd, rev

      rev.each do |uri, requri|
        doc = @resolver.uuid_for uri, published: published, as: :uri
        if doc and fwd[doc] and fwd[doc] != uri
          # warn "#{doc.inspect} -> #{fwd[doc]} (ex #{uri})"
          out[requri] = fwd[doc].to_s
        end
      end

      out
    end

    # you know what, it's entirely possible that these ought never be
    # called individually and the work to get one would duplicate the
    # work of getting the other, so maybe just do 'em both at once

    def generate_redirect_map published: false, docs: nil
      generate_uuid_redirect_map(published: published, docs: docs).merge(
        generate_slug_redirect_map(published: published, docs: docs))
    end

    def generate_gone_map published: false, docs: nil
      # published is a no-op for this one because these docs are by
      # definition not published
      docs ||= @graph.reachable published: false
      p    = RDF::Vocab::BIBO.status
      base = @resolver.base
      out  = {}
      docs.select { |s|
        @graph.has_statement? RDF::Statement(s, p, CI.retired) }.each do |doc|
        canon = @resolver.uri_for doc, as: :uri
        next unless base.route_to(canon).relative?
        canon = canon.request_uri.delete_prefix '/'
        # value of the gone map doesn't matter
        out[canon] = canon
      end

      out
    end

    # private?

    def map_location type
      # find file name in config
      fn = @config[:maps][type] or return

      # concatenate to target directory
      @config[:target] + fn
    end

    # private?

    def write_map_file location, data
      # open file
      File.open(location, ?w) do |fh|
        data.sort.each { |k, v| fh.write "#{k}\t#{v}\n" }
      end # return value is return value from close
    end

    # public again

    def write_rewrite_map published: false, docs: nil
      data = generate_rewrite_map published: published, docs: docs
      loc  = map_location :rewrite
      write_map_file loc, data
    end

    def write_redirect_map published: false, docs: nil
      data = generate_redirect_map published: published, docs: docs
      loc  = map_location :redirect
      write_map_file loc, data
    end

    def write_gone_map published: false, docs: nil
      data = generate_gone_map published: published, docs: docs
      loc  = map_location :gone
      write_map_file loc, data
    end

    def write_maps published: true, docs: nil
      docs ||= @graph.all_documents external: false,
        published: false, fragments: true
      # slug to uuid (internal)
      write_rewrite_map docs: docs
      # uuid/slug to canonical slug (308)
      write_redirect_map docs: docs
      # retired slugs/uuids (410)
      write_gone_map docs: docs
      true
    end

    # MOVE TO Surface::DocumentRoot
    def generate_xml_catalog published: false, docs: nil, path: ?., extra: {}
      path = Pathname(path)

      # make a bunch of mappings
      data = generate_rewrite_map(
        published: published, docs: docs
      ).merge(extra).map do |uri, file|
        uri = self.base + uri
        file = Pathname(file)
        file = Pathname("#{file}.xml") if file.extname.empty?
        file = path + file if file == file.basename
        { nil => :uri, name: uri, uri: file }
      end

      # set up the markup spec
      spec = [
        { '#doctype' => ['catalog', "-//OASIS//DTD XML Catalogs V1.0//EN" ] },
        { '#catalog' => data,
         xmlns: 'urn:oasis:names:tc:entity:xmlns:xml:catalog' }
      ]

      # out the door
      markup(spec: spec).document
    end
end
