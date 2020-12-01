# -*- mode: enh-ruby -*-
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'rdf/sak/version'

Gem::Specification.new do |spec|
  spec.name          = 'rdf-sak'
  spec.version       = RDF::SAK::VERSION
  spec.authors       = ['Dorian Taylor']
  spec.email         = ['code@doriantaylor.com']
  spec.license       = 'Apache-2.0'
  spec.homepage      = 'https://github.com/doriantaylor/rb-rdf-sak'
  spec.summary       = 'Swiss-Army Knife for RDF(a) markup generation'
  spec.description   = <<-DESC

  DESC

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/})
  end
  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  # ruby
  spec.required_ruby_version = "~> 2.0"

  # dev/test dependencies
  spec.add_development_dependency 'bundler', '>= 2.1.4'
  spec.add_development_dependency 'rake',    '>= 13.0'
  spec.add_development_dependency 'rspec',   '>= 3.9'

  # stuff we use
  spec.add_runtime_dependency 'rdf',              '>= 3.1.7'
  spec.add_runtime_dependency 'rdf-reasoner',     '>= 0.6.2' # my patch
  spec.add_runtime_dependency 'linkeddata',       '>= 3.1.2'
  spec.add_runtime_dependency 'uri-urn',          '>= 0.0.3'
  spec.add_runtime_dependency 'sassc',            '>= 2.2.1'
  spec.add_runtime_dependency 'shared-mime-info', '>= 0.2.0'
  spec.add_runtime_dependency 'commander',        '>= 4.4.5' # my patch
  spec.add_runtime_dependency 'mimemagic',        '>= 0.3.4'

  # stuff for urlrunner
  spec.add_runtime_dependency 'concurrent-ruby',      '>= 1.1.6'
  spec.add_runtime_dependency 'concurrent-ruby-edge', '>= 0.6.0'
  spec.add_runtime_dependency 'tidy_ffi',             '>= 1.0.0'
  spec.add_runtime_dependency 'crass',                '>= 1.0.6'
  spec.add_runtime_dependency 'uuidtools',            '>= 2.1.5'
  # spec.add_runtime_dependency 'uri-urn',              '>= 0.0.3'

  # stuff for docstats
  spec.add_runtime_dependency 'descriptive_statistics', '>= 2.5.1'

  # stuff i wrote
  spec.add_runtime_dependency 'xml-mixup',      '>= 0.1.10'
  spec.add_runtime_dependency 'uuid-ncname',    '>= 0.2.2'
  spec.add_runtime_dependency 'md-noko',        '>= 0.1.0'
  spec.add_runtime_dependency 'http-negotiate', '>= 0.1.0'

end
