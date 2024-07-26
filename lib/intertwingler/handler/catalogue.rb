require 'intertwingler/handler'

# This is a `GET` handler for what I'm calling "_catalogue_ resources"
# (sorry Americans). Its purpose is to tell us things like what's in
# the graph. Applications like the IBIS tool need a principled yet
# "dumb" way to access this kind of information in order to operate.
#
# The main goal of these catalogues is effectively to stand in for a
# handful of useful SPARQL queries (with inferencing) so a Web front
# end could construct a serviceable interactive user interface without
# having a lot of machinery written in JavaScript (much of which, like
# a _reasoner_, still does not exist at the time of this writing).
#
# Here are some specific questions the front end would want to ask the
# back end if it was setting up a user interface:
#
# * what classes are available to the application?
# * given a class (or classes), what properties have it (them) as a domain?
# * what properties have it (them) as a range?
# * given a property, what other resources in the graph…
#   * …are directly in its domain?
#   * …are in its domain by inference?
#   * …are directly in its range?
#   * …are in its range by inference?
#   * …are already subjects (directly and by inference)?
#   * …are already objects (directly and by inference)?
#
# The application's front end _should_ be able to answer these
# questions just by following links from wherever it currently is. It
# should not need to know any URLs in advance (except class and
# predicate identifiers, obviously, but it shouldn't need to know the
# URLs of any instance resources).
#
# Here is a sample chain of resources:
#
# ```turtle
# # how the actual entry point itself is done is negotiable
#
# ?wherever xhv:index ?idx .
#
# ?idx a cgto:Index ;
#   cgto:class-summary ?sc ;
#   cgto:property-summary ?sp .
#
# # (cgto:Summary rdfs:subClassOf qb:DataSet .)
#
# ?sc a cgto:Summary ;
#   qb:structure cgto:resources-by-class .
#
# # cgto:resources-by-class and cgto:resources-by-property are both
# # instances of qb:DataStructureDefinition
#
# # observations have datasets, not the other way around, so:
#
# ?cobs a qb:Observation ;
#   qb:dataSet ?sc ;
#   cgto:class ?class ;
#   cgto:asserted-instances ?ai ;
#   cgto:asserted-instances-count ?aic ;
#   cgto:inferred-instances ?ii ;
#   cgto:inferred-instances-count ?iic .
#
# # the predicate one will look something like this
#
# ?sp a cgto:Summary
#   qb:structure cgto:resources-by-property .
#
# # this is gonna be ugly cause there are 16 permutations of
# # asserted/inferred/subject/object/in-domain/in-range/resources/counts
#
# ?pobs a qb:Observation ;
#   qb:dataSet ?sp ;
#   cgto:property ?property ;
#   cgto:asserted-subjects ?as ;         # XXX
#   cgto:asserted-subject-count ?asc ;   # XXX
#   cgto:inferred-subjects ?is ;         # XXX
#   cgto:inferred-subject-count ?isc ;   # XXX
#   cgto:asserted-objects ?ao ;          # XXX
#   cgto:asserted-object-count ?aoc ;    # XXX
#   cgto:inferred-objects ?io ;          # XXX
#   cgto:inferred-object-count ?ioc ;    # XXX
#   cgto:asserted-domain ?ad ;
#   cgto:asserted-domain-count ?adc ;
#   cgto:inferred-domain ?id ;
#   cgto:inferred-domain-count ?idc ;
#   cgto:asserted-range ?ar ;
#   cgto:asserted-range-count ?arc ;
#   cgto:inferred-range ?ir ;
#   cgto:inferred-range-count ?irc .
#
# # ?as is asserted subjects; ?r1 is whatever resource being named
#
# ?as a cgto:Inventory ;
#   dct:hasPart ?r1 . # , ...
# ```
#
# > In the IBIS tool I kind of unthinkingly added the `subjects`/
# > `objects` construct, but on further reflection I'm wondering how
# > useful it is. For starters, I never actually used it myself.
# > Second, it's redundant, at least from the point of view of any
# > discovery because it's not useful to know that this or that
# > resource is the subject or object of this or that predicate
# > outside of the context of the node of the other side of the
# > predicate (which, if you had access to the node, is information
# > you would already have), _except_ perhaps that aggregates will
# > tell you something about the usage of those particular predicates.
#
class Intertwingler::Handler::Catalogue < Intertwingler::Handler

  private

  # specific resources:

  # meta-catalogue (cgto:Index that links to summaries)

  # all classes (cgto:Summary table)

  # all properties (cgto:Summary table)

  # all of these should have asserted/inferred variants; could even do
  # a parameter for both so all four combinations (well, three of the
  # four since asserted=false&inferred=false would give you nothing)
  # are available.

  # all instances of type T

  # all properties with domain T

  # all properties with range T

  # all resources in domain of P

  # all resources in range of P

  # a _me_ resource that echoes back `REMOTE_USER` among other things

  public

  def handle req
  end
end
