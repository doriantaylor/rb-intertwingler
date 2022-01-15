require 'spec_helper'

require 'rdf'
require 'rdf/sak/graphops'

RSpec.describe RDF::SAK::GraphOps do
  context 'stateless behaviour' do
    # type_strata
    # all_related (basically a shorthand for type_strata descend: true)
    # type_is?
    # property_set (née predicate_set)
    # symmetric?
  end

  context 'basic extended (entailing) graph queries' do
    # objects_for
    # subjects_for
  end

  context 'comparator function factories' do
    # cmp_literal
    # cmp_label
    # cmp_resource
    # cmp_term
  end

  context 'shorthands' do
    # types_for
    # rdf_type?
    # all_types
    # all_of_type

    # these return literals:
    # label_for
    # authors_for

    # these return non-nodes:
    # dates_for
    # formats_for

    # these return routing data:
    # published?
    # indexed?
    # host_for
    # replacements_for
  end

  context 'resource-centric interface' do
    # resource factory method
    # resource properties
  end
end
