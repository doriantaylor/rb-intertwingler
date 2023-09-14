require 'intertwingler/transform'

# all this is gonna do is run the sass thinger. this one will need
# access to the engine to run subrequests. actually since active sass
# development has moved to dart this could be an early candidate for
# reverse-proxying to a handler written in dart.
class Intertwingler::Transform::Sass < Intertwingler::Transform::Handler

end
