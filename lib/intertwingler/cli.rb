# frozen_string_literal: true
require 'thor'
require 'intertwingler/version'
require 'rdf'

require 'pathname'
require 'yaml'


# The Intertwingler command line tool.
class Intertwingler::CLI < Thor
  private

  # configuration location defaults
  CONFIG_HOME = '~/.intertwingler'
  CONFIG_FILE = 'intertwingler.conf'
  ENV_HOME    = 'INTERTWINGLER_HOME'
  ENV_CONFIG  = 'INTERTWINGLER_CONFIG'

  # Return the full path to the configuration home directory. This is
  # the default directory for Intertwingler state data, including the
  # base configuration file. Candidate order goes: command line, the
  # `$INTERTWINGLER_HOME` environment variable, any explicit directory
  # specified for the config file, and finally, the default, which is
  # `~/.intertwingler`.
  #
  # @param file [true, false] whether to splice the path of the
  #  config file into the search, which may be the current directory.
  #
  # @return [Pathname] the configuration home directory.
  #
  def config_home file: true
    tests = [options[:home], ENV[ENV_HOME], CONFIG_HOME]

    if file
      # same deal re environment
      return @config_home if @config_home

      # if the config file was given with an absolute path,
      # its containing directory takes precedence over the default.
      tests.insert(2, config_file(home: false).dirname)

      return @config_home = Pathname(tests.detect { |t| t }).expand_path
    end

    Pathname(tests.detect { |t| t }).expand_path
  end

  # Return the full path of the base configuration file, which stores
  # the residual configuration data which can't be stored in the graph
  # (such as where to find the graph containing the rest of the
  # configuration). As with the configuration home,
  #
  # @param home [true, false] whether to prepend `config_home`
  #
  # @return [Pathname] the location of the configuration file.
  #
  def config_file home: true
    # we want to bake this once because it's reading off the environment
    return @config_file if home && @config_file

    # check the command line, then the environment, then the default
    file = Pathname(options[:config] || ENV[ENV_CONFIG] || CONFIG_FILE)

    # note the directory defaults to ./
    home && file == file.basename ?
      @config_file = config_home(file: false) + file : file.expand_path
  end

  public

  class_option :home, aliases: %i[-H],
    desc: 'Configuration home (default: ~/.intertwingler)'
  class_option :config, aliases: %i[-C],
    desc: 'Configuration file (default: $INTERTWINGLER_HOME/intertwingler.conf)'

  def initialize args = [], opts = {}, config = {}
    super

    require 'pry'
    binding.pry

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
