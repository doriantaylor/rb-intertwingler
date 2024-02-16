# -*- mode: enh-ruby -*-
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'intertwingler/version'

Gem::Specification.new do |spec|
  spec.name          = 'intertwingler'
  spec.version       = Intertwingler::VERSION
  spec.authors       = ['Dorian Taylor']
  spec.email         = ['code@doriantaylor.com']
  spec.license       = 'Apache-2.0'
  spec.homepage      = 'https://github.com/doriantaylor/rb-intertwingler'
  spec.summary       = 'An engine for dense hypermedia.'
  spec.description   = <<~DESC

  DESC

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features|example|experimental)/})
  end
  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = %w[lib]

  # ruby
  spec.required_ruby_version = '>= 3.1'

  # dev/test dependencies
  spec.add_development_dependency 'bundler', '>= 2.4'
  spec.add_development_dependency 'rake',    '>= 13.1'
  spec.add_development_dependency 'rspec',   '>= 3.12'

  # stuff we use pretty universally
  spec.add_runtime_dependency 'dry-schema',     '>= 1.13.3'
  spec.add_runtime_dependency 'http-negotiate', '>= 0.2.2' # mine
  spec.add_runtime_dependency 'linkeddata',     '>= 3.1.2'
  spec.add_runtime_dependency 'mimemagic',      '>= 0.5.3' # my patch
  spec.add_runtime_dependency 'rack',           '~> 3'
  spec.add_runtime_dependency 'rackup',         '~> 2'
  spec.add_runtime_dependency 'rdf-reasoner',   '>= 0.9.0'
  spec.add_runtime_dependency 'sparql',         '>= 3.3.0'
  spec.add_runtime_dependency 'uri-urn',        '>= 0.0.3'
  spec.add_runtime_dependency 'uuid-ncname',    '>= 0.4'   # mine
  spec.add_runtime_dependency 'uuidtools',      '>= 2.1.5'

  # stuff for handlers/transforms
  spec.add_runtime_dependency 'md-noko',           '>= 0.1.0'  # mine
  spec.add_runtime_dependency 'params-registry',   '>= 0.1.7'  # mine
  spec.add_runtime_dependency 'sassc',             '>= 2.2.1'
  spec.add_runtime_dependency 'store-digest',      '>= 0.1.4'  # mine
  spec.add_runtime_dependency 'store-digest-http', '>= 0.1.1'  # mine
  spec.add_runtime_dependency 'vips',              '>= 8.12.2'

  spec.add_runtime_dependency 'xml-mixup',         '>= 0.1.16' # mine

  # stuff for cli
  spec.add_runtime_dependency 'thor',         '>= 1.2.2'
  spec.add_runtime_dependency 'tty-markdown', '>= 0.7.2'
  spec.add_runtime_dependency 'tty-reader',   '>= 0.9.0' # my patch
  spec.add_runtime_dependency 'tty-prompt',   '>= 0.23.1'

  # stuff for urlrunner
  spec.add_runtime_dependency 'concurrent-ruby',      '>= 1.1.6'
  spec.add_runtime_dependency 'concurrent-ruby-edge', '>= 0.6.0'
  spec.add_runtime_dependency 'crass',                '>= 1.0.6'
  spec.add_runtime_dependency 'tidy_ffi',             '>= 1.0.0'

  # stuff for docstats
  spec.add_runtime_dependency 'descriptive_statistics', '>= 2.5.1'
  spec.add_runtime_dependency 'engtagger',              '>= 0.3.2'
  spec.add_runtime_dependency 'lemmatizer',             '>= 0.2.2'
end
