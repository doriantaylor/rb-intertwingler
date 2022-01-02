# bring in the namespace
require 'rdf/sak/version'

# bring in the patients
require 'rdf/sak/util/clean'
require 'rdf/sak/util/messy'

# 2021-12-27: Here's the plan for this thing:
#
# * rename {RDF::SAK::Util} to {RDF::SAK::Util::Messy}
#
# * create {RDF::SAK::Util::Clean} which will eventually be the new
#   {RDF::SAK::Util}
#
# * create a temporary {RDF::SAK::Util} that yokes `Clean` and `Messy`
#   back together
#
# * move all genuine bona fide *stateless* utility functions that are
#   used in more than one place to {RDF::SAK::Util::Clean}
#
# * refactor `Messy` until it ceases to exist
#
# * rename `Clean` to {RDF::SAK::Util}
#
module RDF::SAK::Util
  include Clean
  include Messy
end
