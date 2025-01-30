require 'rdf'
require 'rdf/turtle'
require 'rdf/vocab'
require 'intertwingler/vocab'
require 'uuidtools'
require 'uuid-ncname'
require 'digest'
require 'base64'

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
# > of the example, and would just be a liability when computing the
# > hash.
#
# While the paradigm case is data cube `qb:Observation`s, the method
# of constructing the hash input string should be generic and
# versatile enough that it can be used in other situations.
#
# > What if a subproperty/equivalent property (ie same meaning,
# > different URI) is being used?
#
# I am loath to put anything into the hash input that _could_ vary but
# that doesn't _have_ to vary. Therein lies tears. Examples:
#
# * subclasses/equivalent classes
# * subproperties/equivalent properties
# * any other statements that don't unambiguously identify a subject
#
# > What if the observation is in more than one dataset?
#
# We are _approaching_ this from the context of the dataset in
# question, so the identity of the dataset we care about is _known_.
# That said, if an observation is in more than one dataset then it may
# make sense to mint a random UUID for it and thus any custom v8 UUID
# derived from its members would be considered secondary.
#
# > This means we will need some kind of ranking system for durable
# > identifiers, which is a good idea anyway, in case a subject has
# > more than one of them.
#
# My inclination is that the function just takes a list of RDF terms
# rather than a set of statements or fragment of SPARQL or something.
# However, the terms should be sequenced _as if_ they were the result
# of a SPARQL query. Sort of. Suppose the dimension property (in this
# example, `cgto:class`) has more than one value. (It shouldn't, but a
# similar construct might.) Just take the set of values and sort them
# lexically. Then go to the next set of values and do the same. This
# will give you a flattened list that should give repeatable results.
#
# Say then that we took the clauses from the SPARQL above in order,
# our input is a serialization of the values for `?d` and `?c`, which
# may look like this:
#
# ```
# <urn:uuid:3ebd018a-149a-4d62-8a84-8f3dc2cd58d8> <http://www.w3.org/2004/02/skos/core#Concept>
# ```
#
# …where the first term represents `?d` and the second represents
# `?c` for the given `?s`, which is the identifier we are trying to
# compute.
#
# > We will table the matter of Unicode normalization for now and just
# > assume NFC or something (https://www.unicode.org/reports/tr15/).
# > Some literals may also require additional escaping, e.g. things
# > like double quotes which are part of the serialization syntax.
#
# Suppose, furthermore, that there is more than one dimension
# property, or plainly more than one property to consider. Or if a
# property (potentially to be) asserted is a
# subproperty/equivalent/inverse etc of the one in the spec.
#
# The algorithm should be something like, for a given list of
# identifying properties (sorted if not given an explicit order), get
# the (unique) values and sort them. Then concatenate this together
# into a list. Null values/empty sets should be illegal (or
# nonsensical because these values will be known given the context).
#
# > We may be able to repurpose bits of SHACL to represent these rules.
#
# Having decided to have the bulk of the identifier consist of the
# first 96 bits of a SHA256 hash, and we have furthermore decided that
# the contents of the hash will be a space-separated string of RDF
# terms (serialized to their NTriples representations), we need to
# consider what the remaining 24 bits should say. What I'm thinking
# here is some sort of fixed code that indicates what kind of
# identifier this is. My proposal here is something like `SF0-` in the
# Base64 representation, which stands for stable fragment revision
# zero, followed by a separator. The three bytes, when transformed,
# will be `48 5d 3e`.
#
# Thus, an identifier given the inputs above will look like
# `ISF0-gYTySpDxALMNjWJ4I`, which translates into an equivalent UUID
# that looks like `485d3e81-84f2-84a9-80f1-00b30d8d6278`

def make_stable_fragment *terms
  # generate a string of terms separated by spaces
  input = terms.flatten.map(&:to_sxp).join(' ')

  # warn input

  # generate a sha256 hash truncated to 96 bits
  hash = Digest::SHA256.digest(input).slice 0, 12

  # base64 encode that baby
  b64 = Base64.urlsafe_encode64 hash

  # I… is version 8 and …I is the variant 0b1000 (with the last two
  # bits masked); `SF0-` (48 5d 3e) stands for stable fragment rev 0
  "ISF0-%sI" % b64
end

if $0 == __FILE__
  require 'pry'

  # start with some imaginary subject
  s = RDF::URI("urn:uuid:3ebd018a-149a-4d62-8a84-8f3dc2cd58d8")

  # say we want the observation over skos:Concepts
  x = make_stable_fragment s, RDF::Vocab::SKOS.Concept

  # look ma, valid uuid
  u = UUIDTools::UUID.parse UUID::NCName.from_ncname x

  # aand rdf subject
  r = RDF::URI(u.to_uri)

  # here, poke around
  binding.pry
end
