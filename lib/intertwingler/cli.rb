# frozen_string_literal: true
require 'thor'
require 'intertwingler/version'
require 'pathname'
require 'yaml'


# The Intertwingler command line tool.
class Intertwingler::CLI < Thor
  private

  # 
  CONFIG_HOME = '~/.intertwingler'
  CONFIG_FILE = 'intertwingler.conf'
  ENV_HOME    = 'INTERTWINGLER_HOME'
  ENV_CONFIG  = 'INTERTWINGLER_CONFIG'

  def config_file home: true
    # get the literal value
    file = Pathname(options[:config] || ENV[ENV_CONFIG] || CONFIG_FILE)

    return config_home(file: false) + file if home and file == file.basename

    file.expand_path
  end

  def config_home file: true

    tests = [options[:home], ENV[ENV_HOME], CONFIG_HOME]
    tests.insert(2, config_file(home: false).dirname) if file

    # warn tests.inspect

    Pathname(tests.detect { |t| t }).expand_path
  end

  public

  class_option :home, aliases: %i[-H],
    desc: 'Configuration home (default: ~/.intertwingler)'
  class_option :config, aliases: %i[-C],
    desc: 'Configuration file (default: $INTERTWINGLER_HOME/intertwingler.conf)'

  # def self.start args = [], opts = {}, config = {}
  #   super
  # end

  def initialize args = [], opts = {}, config = {}
    super

    warn config_home
    warn config_file

  end

  # detect state directory

  # first command line, then env, then test ./, then ~/

  desc :init, 'Initialize an Intertwingler base configuration.'
  option :store, aliases: %i[-s], type: :string,
    desc: 'Descriptor for the RDF store'
  option :rdf, aliases: %i[-r], type: :string, repeatable: true,
    desc: 'Read in an RDF file'

  def init
    warn 'initializing'
    # detect terminal
    # if no state dir found then initialize one
    # * quad store driver and options (urn:x-ruby?)
    # * domains to expose
    # * rdf files to initialize with
    # later on we can configure handlers and transforms and shit
  end

  desc :engine, 'Run the Intertwingler engine.'
  option :host, aliases: %i[-h], type: :string, default: '127.0.0.1',
    desc: 'Bind to particular host address'
  option :port, aliases: %i[-p], type: :numeric, default: 10101,
    desc: 'Bind to a particular TCP port'
  option :domain, aliases: %i[-d], type: :string, repeatable: true,
    desc: 'Answer only to this domain'
  option :detach, aliases: %i[-z], type: :boolean, default: false,
    desc: 'Detach process to background'
  option :pid, aliases: %i[-P], type: :string,
    desc: 'Create a PID file when detached'

  def engine
    # we imagine detecting whether the configuration has been initialized
    unless false
      return invoke :init
    end

    require 'intertwingler/engine'
    require 'rack'

    warn 'sup'

    # initialize quad store
    # repo = whatever

    # initialize Intertwingler::Engine
    # engine = Intertwingler::Engine.configure repo

    # initialize rack server
  end

  default_command :engine

end
