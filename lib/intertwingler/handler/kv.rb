require 'intertwingler/handler'
require 'rdf/kv'

# This is a `POST` handler for {RDF::KV}, which will likely be
# supplanted by a request transform that converts RDF-KV protocol data
# to an LD-Patch request, and a concomitant `PATCH` handler. So don't
# get too comfy with it.
#
class Intertwingler::Handler::KV < Intertwingler::Handler

  private

  ERR415 =
    'Request must be application/x-www-form-urlencoded or multipart/form-data'

  public

  # Handle a POST request that complies with the RDF-KV protocol.
  #
  # @param req [Rack::Request]
  #
  # @return [Rack::Response]
  #
  def handle req
    # we only respond to post
    return Rack::Response[405, {}, []] unless req.request_method == 'POST'

    warn "content-type: #{req.content_type.to_s}"

    # we only respond to ordinary web forms
    return Rack::Response[
      415, { 'content-type' => 'text/plain' }, [ERR415]] unless
      %w[application/x-www-form-urlencoded
         multipart/form-data].include? req.content_type.to_s.downcase

    subject = RDF::URI(req.url)

    kv = RDF::KV.new subject: subject, prefixes: resolver.prefixes,
      callback: -> term do
        log.debug "TERM: #{term.inspect}"
        if term.iri?
          # XXX THIS SHOULD PROBABLY BE LESS DUMB
          resolver.uuid_for(term) || term rescue term
          # anyway note the parentheses.
        else
          term
        end
      end

    # XXX WATCH OUT THIS MIGHT SILENTLY THROW AWAY DATA
    req.POST.each { |k, v| log.debug "POST #{k} => #{v}" }

    begin
      # generate the changeset
      cs = kv.process req.POST

      log.debug "inserts: #{cs.inserts} deletes: #{cs.deletes}"

      log.debug "graph size: #{repo.size}"

      # apply it to the graph
      cs.apply repo

      log.debug "graph size: #{repo.size}"

      # XXX we should figure out a way to hook up a rider or otherwise
      # smuggle callback functions in; that would entail coming up
      # with a more robust solution for configuring handlers though.
    rescue Exception => e
      log.error e.full_message
      return Rack::Response[409, {
        'content-type' => 'text/plain',
        'content-length' => e.full_message.b.length.to_s }, [e.full_message]]
    end

    # XXX y'know there could be some cross-site wankery in this,
    # requests that redirect to other sites or something, but so what?

    # now we redirect to self or whatever the new subject is
    redir = resolver.uri_for kv.subject, slugs: true, via: subject
    engine.log.debug "RDF::KV redirecting to #{redir}"
    Rack::Response[303, { 'location' => redir.to_s }, []]
  end
end
