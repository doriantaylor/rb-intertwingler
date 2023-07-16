require "bundler/gem_tasks"
require "rspec/core/rake_task"

RSpec::Core::RakeTask.new(:spec)

task :default => :spec

#desc 'Generate Vocabularies'
#task :gen_vocabs => %w(ci).map { |v| "lib/intertwingler/#{v}.rb" }

# XXX turn this into a rake task at some point :P

task :vocabs do
  require 'rdf/vocab/writer'

  # ehh we can circle back on this one
  vocabs = {
    ADMS: 'http://www.w3.org/ns/adms#',
    CI:   'https://vocab.methodandstructure.com/content-inventory#',
    IBIS: 'https://vocab.methodandstructure.com/ibis#',
    PAV:  'http://purl.org/pav/',
    QB:   'http://purl.org/linked-data/cube#',
    # SCOVO: 'http://purl.org/NET/scovo#', # this one is dead
    TFO:  'https://vocab.methodandstructure.com/transformation#',
  }
end

# rdf serialize --uri 'https://vocab.methodandstructure.com/content-inventory#' --output-format vocabulary --module-name Intertwingler::Vocab --class-name CI -o lib/intertwingler/vocab/ci.rb --strict 'https://vocab.methodandstructure.com/content-inventory#'
