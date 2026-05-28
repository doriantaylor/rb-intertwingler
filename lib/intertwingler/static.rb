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

  def get uri, accept: nil
    uuid = resolver.uuid_for(uri) or return
    uri  = resolver.uri_for(uuid || uri, as: :uri, slugs: true, fragments: true)

    return unless resolver.base.route_to(uri).relative?
    
    # warn "#{uuid} -> #{uri}"
    env  = Rack::MockRequest.env_for uri.to_s
    env['REQUEST_URI'] = uri.request_uri # for some reason not included?
    env['HTTP_CACHE_CONTROL'] = 'no-store'

    req = Rack::Request.new env

    engine.handle req
  end

  # write an individual resource

  # write all resources

  # write feeds

  # write indices

  # write site map

  # write rewrite maps

  # Write everything in the space to the file system (may take a while).
  def write_all published: nil, &block
    now = Time.now
    # gather up list of everything
    docs = repo.all_documents.each do |subject|
      subject = resolver.uuid_for(subject) or next

      uuid = resolver.coerce_resource subject, as: :uri

      # get types for output
      # types = repo.types_for subject

      # simulate GETs to everything
      resp = get(subject) or next

      if resp.successful?
        lm = Intertwingler::Field['last-modified'][resp]&.value || now
        bn = uuid.uuid
        ct = MimeMagic[resp.content_type]
        bn += ct.extensions.empty? ? '' : ".#{ct.extensions.first}"
        path = target + bn

        if path.exist? and path.stat.mtime >= lm
          log.info "skipping #{path}: #{lm}"
          next
        end

        body = Intertwingler::Representation::BodyWrap.coerce resp.body

        path.open('wb') do |fh|
          while buf = body.read(8192)
            fh << buf
          end
        end

        path.utime lm, lm

        log.debug "wrote #{bn}"
      else
        log.error "#{resp.status}: (#{subject}) #{resp.content_type} -> #{resp.body.inspect}"
      end
      # write to target
      # if resp.suc
    end
    warn "got #{docs.size} documents lol"

    # write 
  end
end
