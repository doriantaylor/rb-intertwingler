require 'intertwingler/transform'

# load this anyway cause we'll return it
require 'intertwingler/representation/nokogiri'

# this is gonna be mainly stuff like markdown->html and tidy since
# they both ingest only text, but we could consider other fun
# plain-text transforms too.
class Intertwingler::Transform::Text < Intertwingler::Transform::Handler

end
