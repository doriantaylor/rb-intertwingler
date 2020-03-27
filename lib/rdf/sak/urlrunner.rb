require 'net/http'
require 'concurrent'
require 'concurrent-edge'

require 'uri'
require 'uri/urn/uuid'
require 'time'

require 'rdf'
require 'rdf/rdfa'
#require 'rdf/vocab'
require 'rdf/vocab/dc'
require 'rdf/vocab/prov'
require 'rdf/sak/ci'
require 'rdf/sak/tfo'
require 'tidy_ffi'
require 'uuidtools'
require 'nokogiri'
require 'crass'

class RDF::SAK::URLRunner
  private

  UA = 'RDF::SAK::URLRunner/0.1'.freeze
  TIDY_OPTS = {
    wrap:             0,
    numeric_entities: true,
    tidy_mark:        false,
    output_xhtml:     true,
    custom_tags:      'inline',
  }.freeze

  TIDY_U  = RDF::URI('urn:x-dummy:tidy').freeze
  XHTMLNS = 'http://www.w3.org/1999/xhtml'.freeze
  TTL     = "normalize-space(/html:html/html:head/html:title" \
    "[normalize-space(.) != ''][1])".freeze
  JLD     = "//html:script[text()]" \
    "[normalize-space(@type) = 'application/ld+json']".freeze
  QF      = /^([^?#]*)(?:\?([^#]*))?(?:#(.*?))?$/.freeze
  SF      = /[^[:alpha:][:digit:]\/\?%@!$&'()*+,;=._~-]/.freeze
  TOKEN   = /^([^\x0-\x20()<>@,;:\\"\/\[\]?=\x7f-\xff]+)$/n.freeze
  LANG_RE = /^[A-Za-z]+(?:[_-]+[0-9A-Za-z]+)*$/.freeze

  # xpath for various linkages
  XPATHNS  = {
    html:  XHTMLNS,
    svg:   'http://www.w3.org/2000/svg',
    atom:  'http://www.w3.org/2005/Atom',
    xlink: 'http://www.w3.org/1999/xlink',
  }
  XBASE_XP = 'ancestor-or-self::*[@xml:base][1]/@xml:base'.freeze
  HTML_XP  = %w[*[not(self::html:base)][@href]/@href
              *[@src]/@src object/@data *[@srcset]/@srcset
              form/@action].map { |e| '//html:%s' % e }.join(?|).freeze
  ATOM_XP  = %w[uri content/@src category/@scheme generator/@uri icon id
              link/@href logo].map { |e| '//atom:%s' % e }.join(?|).freeze
  RSS_XP   = %w[image docs source/@url enclosure/@url
              guid comments].map { |e| '//%s' % e }.join(?|).freeze
  XLINK_XP = '//*/@xlink:href'.freeze

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

  def css_urls css
    out = []
    case css
    when Array
      css.each { |c| out += find_urls(c) }
    when Hash
      if css[:node] == :url
        out << css[:value]
      else
        out += find_urls(css.values)
      end
    end

    out.uniq.compact
  end

  def title_term title
    title = title.first if title.is_a? Array
    return unless title

    text = title.content.strip
    unless text.empty?
      lang = if title.lang and LANG_RE.match? title.lang.strip
               title.lang.strip.downcase.tr_s '_-', ?-
             end
      return RDF::Literal(text, language: lang)
    end
  end

  # sponge cartridges should add triples and return all the resolved
  # (RDF::)URIs to the caller
  SPONGE = {
    'application/xhtml+xml': -> content, uri {
      rs = RDF::URI(uri.to_s)

      # lol we have done this a million times before
      if title = content.xpath('/html:html/html:head/html:title', XPATHNS)
        if title = title_term(title)
          @repo << [rs, RDF::Vocab::DC.title, title]
        end
      end

      content.xpath(HTML_XP, XPATHNS).map do |node|
        # html will always be an attribute
        pred = case node.parent.name
               when 'script' then RDF::Vocab::DC.requires
               when 'a', 'link', 'area', 'form' then RDF::Vocab::DC.references
               else
                 RDF::Vocab::DC.hasPart
               end

        # srcset gets special treatment
        objs = if node.name == 'srcset'
                 node.content.strip.split(/\s*,\s+/).map do |u|
                   u.split.first.strip
                 end
               else
                 [node.content.strip]
               end.map { |u| RDF::URI((uri + u).to_s) }

        # phew
        objs.each { |o| @repo << [rs, pred, o] }
      end.flatten # don't forget to flatten
    },
    'application/atom+xml':  -> content, uri {
      rs = RDF::URI(uri.to_s)

      if title = content.xpath('/atom:feed/atom:title', XPATHNS)
        if title = title_term(title)
          @repo << [rs, RDF::Vocab::DC.title, title]
        end
      end

      content.xpath(ATOM_XP, XPATHNS).map do |node|
        o = RDF::URI((uri + node.content.strip).to_s)
        @repo << [rs, RDF::Vocab::DC.references, o]
        o
      end
    },
    'application/x-rss+xml': -> content, uri {
      rs = RDF::URI(uri.to_s)

      if title = content.xpath('/rss/channel/title', XPATHNS)
        if title = title_term(title)
          @repo << [rs, RDF::Vocab::DC.title, title]
        end
      end

      content.xpath(RSS_XP, XPATHNS).map do |node|
        o = RDF::URI((uri + node.content.strip).to_s)
        @repo << [rs, RDF::Vocab::DC.references, o]
        o
      end
    },
    'image/svg+xml': -> content, uri {
      if title = content.xpath('//svg:title', XPATHNS)
        if title = title_term(title)
          @repo << [rs, RDF::Vocab::DC.title, title]
        end
      end

      content.xpath(XLINK_XP, XPATHNS).map do |node|
        o = RDF::URI((uri + node.content.strip).to_s)
        @repo << [rs, RDF::Vocab::DC.references, o]
        o
      end
    },
    'text/css': -> content, uri {
      rs  = RDF::URI(uri.to_s)
      css = Crass.parse content
      css_urls(css).map do |u|
        ro = RDF::URI((uri + u).to_s)
        @repo << [rs, RDF::Vocab::DC.requires, ro]
        ro
      end
    }
  }.freeze

  XMLMAP = {
    nil => {
      'rss'  => 'application/x-rss+xml',
      'html' => 'text/html',
    },
    XHTMLNS                       => 'application/xhtml+xml',
    'http://www.w3.org/2005/Atom' => 'application/atom+xml',
    'http://www.w3.org/2000/svg'  => 'image/svg+xml',
  }.freeze

  def xml_type doc
    r = doc.root
    if r and x = XMLMAP[r.namespace]
      if x.is_a? Hash
        return x[r.name] if x[r.name]
      else
        return x
      end
    end
    'application/xml'
  end

  public

  # NS = {
  #   rdf:  RDF::RDFV,
  #   rdfs: RDF::RDFS,
  #   owl:  RDF::OWL,
  #   xsd:  RDF::XSD,
  #   xhv:  RDF::Vocab::XHV,
  #   http: RDF::Vocabulary.new('http://www.w3.org/2011/http#'),
  #   vann: RDF::Vocabulary.new('http://purl.org/vocab/vann/'),
  #   skos: RDF::Vocab::SKOS,
  #   dcat: RDF::Vocab::DCAT,
  #   so:   RDF::Vocab::SCHEMA,
  #   dct:  RDF::Vocab::DC,
  #   ci:   RDF::SAK::CI,
  #   ogp:  RDF::Vocab::OG,
  #   foaf: RDF::Vocab::FOAF,
  #   org:  RDF::Vocab::ORG,
  #   bibo: RDF::Vocab::BIBO,
  #   qb:   RDF::Vocabulary.new('http://purl.org/linked-data/cube#'),
  # }.freeze
  
  def initialize store: nil, repo: nil, ua: nil, ignore: nil, traverse: nil
    @store  = store
    @repo   = repo
    # @urls   = Concurrent::Array.new
    @jobs   = Concurrent::Array.new
    @seen   = Concurrent::Map.new

    # @fchan  = Concurrent::Promises::Channel.new 10
    # @schan  = Concurrent::Promises::Channel.new 10
    # @tchan  = Concurrent::Promises::Channel.new 10

    @fthrot = Concurrent::Throttle.new 5

    # @done   = Concurrent::Cancellation.new

    @ua     = ua ? ua.to_s : UA
    @ignore = ignore ? ignore.respond_to?(:to_a) ? ignore.to_a : [ignore] : []
    @traverse = traverse ?
      traverse.respond_to?(:to_a) ? traverse.to_a : [traverse] : []
    # other stuff

    #Signal.trap(:INT) { warn 'FART'; @done.origin.resolve }
  end

  def fetch url, redir = 10
    # `return` in a lambda is okay but in a block you have to use
    # `break`, and it complains if you do that

    url = URI(url.to_s) unless url.is_a? URI
    url.normalize!

    # XXX apparently you can't just *return* a fulfilled future, you
    # have to assign it to something.
    bailout = Concurrent::Promises.fulfilled_future nil
    return bailout unless %w[http https].include? url.scheme

    # nuke the fragment
    url.fragment = nil

    # make sure we don't do this a second time
    return bailout if seen? url
    @seen[url.to_s] = url

    ru = RDF::URI(url.to_s)

    # obtain last-modified from object
    q = RDF::Query.new { pattern [ru, RDF::Vocab::DC.modified, :m] }

    ims = q.execute(@repo).map do |s|
      s[:m].object.to_time.getgm if
        s[:m].literal? and s[:m].object.respond_to? :to_time
    end.compact.sort { |a, b| b <=> a }.first

    # XXX this is a little too low-level, don't you think?
    http = Net::HTTP.new(url.hostname, url.port)
    http.continue_timeout = 10
    http.open_timeout     = 30
    http.read_timeout     = 10
    http.write_timeout    = 10
    http.use_ssl          = url.is_a?(URI::HTTPS)
    http.start

    hdr  = { 'User-Agent' => @ua, 'Connection' => 'close' }
    hdr['If-Modified-Since'] = ims.rfc2822 if ims
    req  = Net::HTTP::Get.new url, hdr
    resp = http.request req
    http.finish

    case resp
    when Net::HTTPSuccess
      Concurrent::Promises.fulfilled_future(resp)
    when Net::HTTPNotModified
      warn "Already seen #{url}"
      bailout
    when Net::HTTPRedirection
      raise Net::HTTPClientException.new "Too many redirects (#{redir})",
        resp if redir <= 0
      unless dest = resp['location']
        raise Net::HTTPBadResponse.new(
          "Redirect on #{url} missing Location header", resp)
      end

      dest = (url + dest).normalize

      @repo << [ru, RDF::SAK::CI.canonical, RDF::URI(dest.to_s)]

      raise Net::HTTPClientException.new "Loop detected on #{url}",
        resp if url == dest

      fetch(dest, redir - 1)
    else
      raise Net::HTTPClientException.new "Response failed #{url}", resp
    end
  end

  def store resp
    return unless resp

    now     = Time.now.getgm # we mint a single "now" to use everywhere
    date    = if d = resp['Date']
                # who knows what weird shit is coming off the wire
                d.gsub!(/^([^,]*(?:,[^,]*)?)(?:\s*,.*)?$/, "\\1")
                Time.httpdate(d).getgm rescue now
              else
                now # rfc says server MUST send a date header, but does it?
              end
    mtime   = if lm = resp['Last-Modified']
                # lol god two Last-Modified headers no that's not messed up
                lm.gsub!(/^([^,]*(?:,[^,]*)?)(?:\s*,.*)?$/, "\\1")
                delta = now - date
                      
                Time.httpdate(lm).getgm + delta rescue nil
              end
    lang    = if resp['Content-Language']
                # same friggin deal
                resp['Content-Language'].strip.split('\s*,+\s*').first
              end
    charset = nil
    if type = resp['Content-Type']
      # and again, wtf
      type = type.split(/\s*,+\s*/).first || 'application/octet-stream'
      type, *params = type.strip.split(/\s*;\s*/).reject(&:empty?)
      params = params.map do |p|
        p.split(/\s*=\s*/, 2)
      end.reject { |p| p.length < 2 }.to_h.transform_keys(&:downcase)

      charset = params['charset'] if TOKEN.match? params['charset']
    end

    
    # obj = @store.add resp.body, strict: false,
    #   type: type, charset: charset, language: lang, mtime: mtime

    s  = RDF::URI(resp.uri.to_s) # - the subject
    cs = RDF::Changeset.new      # - a receptacle for statements

    cs << [s, RDF::Vocab::DC.modified, obj.mtime.getgm]
    #cs << [s, RDF::Vocab::DC.hasVersion, RDF::URI(obj[:"sha-256"].to_s)]

    cs.apply @repo

    obj
    #nil
  end

  def tidy obj
    if obj and /html/i.match? obj.type
      # obtain original digest uri
      oldu = RDF::URI(obj[:"sha-256"].to_s)

      # first let's detect if this job has already been done
      RDF::Query.new do
        pattern [:a, RDF::SAK::TFO.input, oldu]
        pattern [:a, RDF::SAK::TFO.transform, TIDY_U]
        pattern [:a, RDF::SAK::TFO.output, :n]
      end.execute(@repo).map do |s|
        # can't completely trust what's in the repo so check then convert
        URI(s[:n].to_s) if s[:n].uri? and s[:n].scheme.downcase == 'ni'
      end.compact.each do |n|
        # just because it's in the rdf doesn't mean it's in the store
        out = @store.get n
        return out if out and !out.deleted? # don't return an empty record
      end

      # tidy the object and reinsert it back into the store as xhtml
      start = Time.now.getgm
      if clean = TidyFFI::Tidy.clean(obj.content.read, TIDY_OPTS)
        newobj = @store.add clean, mtime: obj.mtime,
          type: 'application/xhtml+xml', language: obj.language,
          charset: obj.charset, encoding: obj.encoding
        stop = Time.now.getgm
        newu = RDF::URI(newobj[:"sha-256"].to_s)

        q = RDF::Query.new do
          pattern [:a, RDF::SAK::TFO.input, oldu]
          pattern [:a, RDF::SAK::TFO.output, newu]
        end

        if q.execute(@repo).empty?
          s  = RDF::URI(UUIDTools::UUID.random_create.to_uri)
          cs = RDF::Changeset.new
          cs << [s, RDF.type, RDF::SAK::TFO.Application]
          cs << [s, RDF::Vocab::PROV.startedAtTime, start]
          cs << [s, RDF::Vocab::PROV.endedAtTime, stop]
          cs << [s, RDF::SAK::TFO.transform, TIDY_U]
          cs << [s, RDF::SAK::TFO.input, oldu]
          cs << [s, RDF::SAK::TFO.output, newu]

          cs.apply @repo
        end

        newobj
      end
    end      
  end

  def sponge obj
    return unless obj
    #if obj and /xml/.match? obj.type

    # get rdf stuff
    ru  = RDF::URI(obj[:"sha-256"].to_s)
    uri = RDF::Query.new do
      pattern [:b, RDF::SAK::TFO.output, ru]
      pattern [:b, RDF::SAK::TFO.transform, TIDY_U]
      pattern [:b, RDF::SAK::TFO.input, :a]
      pattern [:s, RDF::Vocab::DC.hasVersion, :a]
    end.execute(@repo) + RDF::Query.new do
      pattern [:s, RDF::Vocab::DC.hasVersion, ru]
    end.execute(@repo).uniq.map do |sol|
      u = sol[:s]
      u if u.uri? and u.scheme and %w[http https].include? u.scheme.downcase
    end.compact.first

    uuri    = URI(uri ? uri_pp(uri.to_s) : ru)
    content = obj.content
    sponge  = SPONGE[obj.type]

    if /xml/.match? type
      content = Nokogiri.XML(content, uuri.to_s)
      unless sponge
        type   = xml_type(content)
        sponge = SPONGE[type] || SPONGE['image/svg+xml'] # svg is just xlink
      end
    end

    if sponge
      instance_exec(content, uuri, &sponge).compact.uniq.each do |link|
        enqueue link if traverse? link
      end
    end

  end

  def enqueue uri
    uri = URI(uri_pp uri.to_s).normalize

    return if seen?    uri
    return if ignored? uri

    warn "enqueuing #{uri}"

    @fthrot.future(uri) do |u|
      fr = fetch u
      fr.on_rejection { |reason| warn "fetch fail: #{reason.inspect}" }
      fs = fr.then do |resp|
        begin
          store resp
        rescue Exception => e
          warn e
        end
      end
      fs.on_rejection { |reason| warn "store fail: #{reason.inspect}" }
      ft = fs.then do |obj|
        if obj and /html/i.match? obj.type
          warn "tidying #{obj[:"sha-256"]}"
          tidy obj
        else
          obj
        end
      end
      ft.on_rejection { |reason| warn "tidy fail: #{reason.inspect}" }
      ft.then { |obj| sponge obj }
    end.on_rejection { |reason| warn "throttle fail: #{reason.inspect}" }
  end

  def seen? uri
    !!@seen[uri.to_s]
  end

  def traverse? uri
    return if @traverse.empty?

    re = /(?:^|\.)#{@traverse.map { |t| Regexp.escape t }.join ?|}$/o
    re.match? uri.host
  end

  def ignored? uri
    return if @ignore.empty?

    re = /(?:^|\.)#{@ignore.map { |t| Regexp.escape t }.join ?|}$/o
    re.match? uri.host
  end

  def run urls, shuffle: false
    # ingest URLs and prune 
    urls = urls.map { |u| URI(uri_pp u).normalize }.reject { |u| ignored? u }

    # optionally randomize
    urls.shuffle! if shuffle

    # now add the queue
    urls.map { |url| enqueue url }.map(&:wait)
    
    # while job = @jobs.shift
    #   job.wait
    # end

    self
  end
end


if __FILE__ == $0
  require 'pathname'
  require 'rdf/turtle'
  require 'rdf/lmdb'
  require 'store/digest'
  require 'commander'

  Commander.configure do
    program :name, 'URLRunner'
    program :version, '0.0.0'
    program :description, 'snarf a bunch of URLs for great justice'

    command :process do |c|
      c.syntax = 'process [options] [urls]'
      c.description = 'just do the friggin urls already'

      c.option '-c', '--csv FILE',
        'a CSV file containing URLs in the first column'
      c.option '-i', '--ignore DOMAIN[,DOMAIN...]', Array,
        'domain(s) to ignore'
      c.option '-t', '--traverse DOMAIN[,DOMAIN...]', Array,
        'traverse links within hypermedia documents'
      c.option '-p', '--print', 'print the results when finished'
      c.option '-A', '--user-agent STRING', 'override the User-Agent string'
      c.option '--shuffle', 'shuffle the list of URLs'

      # wtf why did i have to do this
      c.option '-s', '--store DIR', 'Directory for digest store'
      c.option '-r', '--rdf DIR', 'Directory for RDF store'

      c.action do |args, options|
        raise ArgumentError, 'Store and RDF directories are required' unless
          options.store and options.rdf
        repo  = RDF::LMDB::Repository.new options.rdf, mapsize: 2**27
        store = Store::Digest.new dir: options.store, mapsize: 2**27

        urls = args.dup

        if options.csv
          require 'csv'
          csv = Pathname(options.csv).expand_path
          raise ArgumentError, "CSV file #{csv} is not readable" unless
            csv.file? and csv.readable?
          urls += CSV.read(csv).map(&:first).compact
        end

        RDF::SAK::URLRunner.new(
          repo: repo, store: store, ua: options.user_agent,
          ignore: options.ignore, traverse: options.traverse
        ).run urls, shuffle: options.shuffle

        if options.print
          print repo.dump :turtle #, prefixes: URLRunner::NS
        end
      end
    end

    # XXX these should work wtf
    #global_option '-s', '--store DIR', 'Directory for digest store'
    #global_option '-r', '--rdf DIR', 'Directory for RDF store'

    default_command :process

  end
end
