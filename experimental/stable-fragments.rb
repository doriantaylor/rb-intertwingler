require 'rdf'
require 'rdf/vocab'
require 'intertwingler/vocab'
require 'uuidtools'
require 'uuid-ncname'

# This is a little side quest to figure out stable fragment identifiers
# based on RFC9562 §5.5 (UUID v8): https://datatracker.ietf.org/doc/html/rfc9562
#
# What we _want_ are fragment identifiers that can be
# deterministically generated based on a set of immutable facts about
# a particular (RDF) subject. By "immutable" we mean if you change the
# assertion then you change the resource, so we're talking about the
# kinds of values that would constitute a (potentially composite)
# primary key in a conventional SQL database.
#
# Why do we want fragment identifiers instead of fully-fledged
# document identifiers? Well, because the resources they identify are
# small and there are a lot of them. My specific use case is
# `qb:Observation`, but other classes of entities apply.
#
# The desideratum is an identifier that can be derived from known
# facts, that isn't too long (lexically), that nevertheless has enough
# entropy that we won't get collisions. The typical strategy for
# something like this is to use a cryptographic hash, because it takes
# an arbitrary input and returns a fixed-length string of bytes.
#
# Here we note that we already have the compact UUID format in play
# (https://datatracker.ietf.org/doc/html/draft-taylor-uuid-ncname) for
# fragment identifiers, so I have a strong bias to using it for this
# as well. The Base64 UUID-NCName variant produces a symbol that is 22
# characters long, which is long, but not unusably long. The UUID
# specification, furthermore, lays out two methods of deriving
# identifiers by cryptographic hash (MD5 and a truncated SHA1). Both
# of these hash functions are obsolete, but that doesn't matter as
# much for this application as does the method of computing them.
# Specifically, RFC4122 (over)prescribes what goes into the hash as
# input.
#
# RFC9652 defines UUID version 8 which is a fully user-defined
# identifier. This means we can compute a hash over whatever we like.
# Current hash algorithms (SHA256 and up), are of course much longer
# than a UUID. However, since the input domain to the hashes _itself_
# is highly constrained (whatever the solution will be is some small
# collection of RDF terms), it will be _incredibly_ unlikely to
# produce an input within those constraints that will cause a
# collision in the output in even a heavily-truncated hash.
#
# There is furthermore the matter of picking out these identifiers as
# such. I am inclined to set aside some of the 128 (really 122) bits
# of real estate to do so. My particular inclination is to _spell_
# something — something short — at the front of the identifier, and
# use the rest for hash data. UUID-NCNames move the masked bits out to
# be "bookends" of the identifier, and sequence them using the same
# alphabet as Base32/Base64 (they are four bits apiece the first 16
# positions are the same for both encodings). If we mask the remaining
# two bits in the "variant" field (itself useful as a signal that this
# is what we are doing) then we have 120 bits remaining. If we
# truncate the hash to 96 bits, that leaves 24 bits, or four Base64
# characters, to spell something with. We will table the decision on
# precisely _what_ to spell for now, after we have determined how the
# hash is to be computed.
#
# Consider the following SPARQL query:
#
# ```
# PREFIX qb:   <http://purl.org/linked-data/cube#>
# PREFIX cgto: <https://vocab.methodandstructure.com/graph-tool#>
#
# SELECT DISTINCT ?s ?d ?c
# WHERE {
#   ?s a qb:Observation ;
#     qb:dataSet ?d .
#     cgto:class ?c .
# }
# ```
#
# In this example, the variables ?d and ?c would be known, and thus
# bound. The CURIEs can of course be expanded to their canonical URI
# counterparts. The wrinkle is that the subject ?s is the thing we're
# trying to compute. A cryptographic hash requires a definite string
# of bytes as input, so what we need here is a rule for crafting
# precisely what input is to be hashed.
#
# > The type assertion `?s a qb:Observation` is there for the clarity
# > of the example, and is immaterial to computing the hash.
#
# BELOW THIS LINE == IN PROGRESS
#
# While the paradigm case is data cube `qb:Observation`s, the method
# of constructing the hash input string should be generic enough that
# other situations can use it
#
#
# we may be able to represent this with SHACL

# what if a subproperty/equivalent property (ie same meaning,
# different URI) is being used?

# what if the observation is in more than one dataset? well, we are
# _approaching_ this from the context of the dataset in question.

# what if the dimension property (`cgto:class` in the example) has
# more than one value? i mean really they shouldn't
