require 'spec_helper'

require 'rdf/sak/resolver'

RSpec.describe RDF::SAK::Resolver do

  context 'stateless methods' do
    # preproc
    # split_pp
    # split_qp
    # terminal_slug
  end

  context 'uuid<->uri resolution methods' do
    # uuid_for
    # uri_for
  end

  context 'curie methods' do
    # resolve_curie
    # abbreviate
    # prefix_subset
  end

  context 'proxy methods??' do
    # still not sure if we even wanna do this: graph proxy methods
    # that also do the uuid<->uri resolution. like, do we care? is it
    # more cumbersome than helpful?

    # struct_for
    # host_for
    # published?
  end
end
