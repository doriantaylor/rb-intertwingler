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
  spec.add_development_dependency 'bundler', '~> 2.0.2'
  spec.add_development_dependency 'rake',    '~> 10.0'
  spec.add_development_dependency 'rspec',   '~> 3.0'

  # stuff we use
  spec.add_runtime_dependency 'rdf',              '~> 3.0.2'
  spec.add_runtime_dependency 'linkeddata',       '~> 3.0.1'
  spec.add_runtime_dependency 'uri-urn',          '~> 0.0.3'
  spec.add_runtime_dependency 'sassc',            '~> 2.0.1'
  spec.add_runtime_dependency 'shared-mime-info', '~> 0.2.0'
  spec.add_runtime_dependency 'commander',        '~> 4.4.5' # my patch
  spec.add_runtime_dependency 'mimemagic',        '~> 0.3.3'

  # stuff i wrote
  spec.add_runtime_dependency 'xml-mixup',   '~> 0.1.10'
  spec.add_runtime_dependency 'uuid-ncname', '~> 0.2.2'
  spec.add_runtime_dependency 'md-noko',     '~> 0.1.0'
  
end
