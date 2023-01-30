require 'spec_helper'

###
#
# okay since there is not really anywhere better to do this, here is
# the tentative testing plan:
#
# 1. test the extended graph operations
#
# 2. test the URI resolver
#
# 3. test the document parser (RDF::SAK::Document::Parsed)
#
# 4. test the markup analysis/surgery
#
#   4.1 augmenting structure/addressability
#
#   4.2 NLP/term harvesting
#
# 5. test the markup generators
#
#   5.1 common traits
#
#     5.1.1 generated document fragments
#
#     5.1.2 seo/social media metadata
#
#     5.1.3 backlinks
#
#   5.2 concept scheme/collection
#
#   5.3 document stats
#
#   5.4 reading list/"books mentioned"
#
#   5.5 social graph/"people mentioned"
#
#   5.6 annotation set
#
# 6. test the target(s)
#
#   6.1 config harness
#
#   6.2 filesystem target
#
#   6.3 web app target
#
# 7. test the transforms
#
# 8. test the scraper
#
# 9. test the CLI
#
###

describe RDF::SAK do
  it 'has a version number' do
    expect(RDF::SAK::VERSION).not_to be nil
  end

  it 'does something useful' do
    expect(false).to eq(true)
  end
end
