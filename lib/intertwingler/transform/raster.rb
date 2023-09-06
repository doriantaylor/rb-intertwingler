require 'intertwingler/transform'

# the representation
require 'intertwingler/representation/vips'

class Intertwingler::Transform::Raster < Intertwingler::Transform
  private

  REPRESENTATION = Intertwingler::Representation::Vips

  # these are all the formats that vips will write
  OUT = %w[avif gif heic heif jpeg jp2 jxl png tiff webp x-portable-anymap
           x-portable-bitmap x-portable-graymap x-portable-pixmap].map do |t|
    "image/#{t}".freeze
  end.freeze

  # we are not averse to reading pdfs
  IN = (%w[application/pdf] + OUT).freeze

  URI_MAP = {
    '4c817a44-005d-48cb-83be-d962604cddda' => [:convert,    IN, OUT],
    'deb428cb-2f88-4726-98ea-d4b8d4589f17' => [:crop,       IN, OUT],
    '5842e610-c5d3-46cd-8ec6-c1c64bf44d3a' => [:scale,      IN, OUT],
    'a3fd7171-ecaf-4f2a-a396-ebddf1b65eb4' => [:desaturate, IN, OUT],
    '7beb24fb-9708-4fd5-861a-1b2aaa45d46e' => [:posterize,  IN, OUT],
    'e43dc4b8-20e1-4739-9150-c1842d64eb5d' => [:knockout,   IN, OUT],
    'f77a0a45-2ba6-4a3b-8291-9eaae2a80a82' => [:brightness, IN, OUT],
    '973172a1-261b-4621-b27b-98d660e87544' => [:contrast,   IN, OUT],
    '2fe3049b-bc1e-496f-9abc-dafa45746ef5' => [:gamma,      IN, OUT],
  }.freeze

  def accept_header req
    accept =
      req.get_header('HTTP_ACCEPT').to_s.split(/\s*,+\s*/).first or return

    MimeMagic[accept]
  end

  # do nothing but convert
  def convert req, params

    body = req.body

    # this will have been set

    accept = accept_header req

    # this fast-tracks to 304 upstream
    return if body.type == accept

    # setting the body type (if different) invalidates the io
    body.type = accept

    body
  end

  # crops the image by xywh
  def crop req, params
    # XXX sanitize params
    x, y, width, height = params.values_at(:x, :y, :width, :height).map do |x|
      x.first.to_i
    end

    body = req.body
    img  = body.object
    img  = img.crop x, y, width, height

    body.type   = accept_header req
    body.object = img
    body
  end

  # scales the image down
  def scale req, params
    # XXX sanitize params
    # width, height = params.values_at :width, :height

    body = req.body
    img  = body.object
    img  = img.thumbnail_image params[:width].first.to_i

    body.type   = accept_header req
    body.object = img
    body
  end

  # parameterless desaturate; maybe we roll it into brightness/contrast? iunno
  def desaturate req, params

    body = req.body
    img  = body.object
    img  = img.colourspace :b_w

    body.type   = accept_header req
    body.object = img
    body
  end

  # flatten colours out
  def posterize req, params
    # okay so apparently posterize involves chonking out the colours;
    # the parameter is the number of steps

    body.type   = accept_header req
    body.object = img
    body
  end

  # knock out a colour ± radius around it
  def knockout req, params
    # this one is gonna be tough and require some thought
    raise Intertwingler::Handler::Error::Server.new(
      'Transform `knockout` not implemented', status: 501)
  end

  # adjust brightness
  def brightness req, params
    raise Intertwingler::Handler::Error::Server.new(
      'Transform `brightness` not implemented', status: 501)
  end

  # adjust contrast
  def contrast req, params
    raise Intertwingler::Handler::Error::Server.new(
      'Transform `contrast` not implemented', status: 501)
  end

  # adjust gamma
  def gamma req, params
    raise Intertwingler::Handler::Error::Server.new(
      'Transform `gamma` not implemented', status: 501)
  end

  # TODO rotate (arbitrary with speedup for 45-degree increments,
  # alpha channel), flip (h, v), gaussian blur

  public
end
