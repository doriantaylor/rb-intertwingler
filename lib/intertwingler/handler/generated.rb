require 'intertwingler/handler'

# for generated
require 'intertwingler/document'
require 'stringio'
require 'uri'

class Intertwingler::Handler::Generated < Intertwingler::Handler

  private

  public

  def handle req

    # yaww
    if repo.respond_to?(:mtime) and
        ims = req.get_header('HTTP_IF_MODIFIED_SINCE')
      ims = (Time.httpdate(ims) rescue Time.at(0)).utc
      lm  = repo.mtime
      return Rack::Response[304, {}, []] if lm.to_i <= ims.to_i
    end

    # warn req.url.inspect
    # warn resolver.base.inspect

    uri = RDF::URI(req.url)

    orig = uri.dup

    # XXX lol
    uri.authority = resolver.base.authority if
      /(spigot|localhost):9292/i.match? uri.authority
    uri.scheme = 'https' if uri.scheme == 'http'

    # warn uri

    # resolve subject
    subject = resolver.uuid_for uri

    # bail out if this doesn't return anything

    return Rack::Response[404, {
      'content-type' => 'text/plain',
    }, ['lol fail']] unless subject

    # okay now we see if there are any sub-handlers that will take this request

    # types  = repo.types_for subject
    # strata = repo.type_strata types

    # otherwise we fall back to the main handler

    # doc = Intertwingler::Document.generate_doc resolver, subject,
    #   prefixes: engine.resolver.prefixes
    generator = Intertwingler::Document.new resolver, subject

    doc = generator.doc

    # XXX nuke this later
    if base = doc.at_xpath('/html:html/html:head/html:base',
                           { html: 'http://www.w3.org/1999/xhtml' })
      href = RDF::URI(base['href'])
      href.scheme = orig.scheme
      href.authority = orig.authority
      base['href'] = href.to_s
    end

    str = doc.to_xml.b

    # warn 'ouate de phoque'

    hdrs = {
      'content-type'   => 'application/xhtml+xml',
      'content-length' => str.length.to_s,
    }
    hdrs['last-modified'] = repo.mtime.httpdate if repo.respond_to? :mtime

    Rack::Response[200, hdrs, StringIO.new(str, ?r, encoding: Encoding::BINARY)]
  end
end
