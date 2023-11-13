2# frozen_string_literal: true
require 'intertwingler/types'
require 'rdf'
require 'thor'

require 'pathname'
require 'psych'

require 'tty-prompt'
require 'tty-markdown'

# The Intertwingler command line tool.
class Intertwingler::CLI < Thor

  class Prompt < TTY::Prompt

    THEME = {
      link: :bright_blue,
      em: %i[bright_white],
      blockquote: :bright_magenta,
    }

    #
    def say_md message = '', **options
      mdopts = options.fetch :markdown, {}
      mdopts[:theme] ||= THEME # sub in a theme if there isn't one
      mdopts[:theme] = mdopts[:theme].merge THEME # *now* merge
      options = options.except :markdown
      say TTY::Markdown.parse(message, **mdopts), **options
    end
  end

  private

  # configuration location defaults
  CONFIG_HOME = '~/.intertwingler'
  CONFIG_FILE = 'intertwingler.conf'
  ENV_HOME    = 'INTERTWINGLER_HOME'
  ENV_CONFIG  = 'INTERTWINGLER_CONFIG'

  # other defaults
  STORE = 'urn:x-ruby:rdf/lmdb;RDF::LMDB::Repository?=dir=rdf&mapsize=128M'
  HOST  = '127.0.0.1'
  PORT  = 10101

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

  # honestly i am surprised this kind of thing isn't part of the yaml module
  def munge_keys hash, symbol: false
    hash.reduce({}) do |out, pair|
      key, value = pair
      if key.respond_to? :to_s
        key = key.to_s
        key = key.to_sym if symbol
      end

      value = case value
              when Hash then munge_keys value, symbol: symbol
              when -> x { x.respond_to? :map }
                value.map do |v|
                  v.is_a?(Hash) ? munge_keys(v, symbol: symbol) : v
                end
              else value
              end

      out[key] = value

      out
    end
  end

  # this should really be part of thor
  def interactive?
    [$stdin, $stdout].all?(&:tty?)
  end

  # 
  CTRL_W = %[[:space:]/\\'+=_.,:;-]
  CW_RE  = /\A.*?[#{CTRL_W}]\z/o

  public

  class_option :home, aliases: %i[-H],
    desc: 'Configuration home (default: ~/.intertwingler)'
  class_option :config, aliases: %i[-C],
    desc: 'Configuration file (default: $INTERTWINGLER_HOME/intertwingler.conf)'

  # Note that {Thor} objects get (re)initialized with every command
  # invocation, so calling {Thor#invoke} on one of these commands will
  # actually create a new one.
  def initialize args = [], opts = {}, config = {}
    super

    unless @raw_config
      begin
        @raw_config = Psych.load_file config_file, symbolize_names: true
      rescue Psych::SyntaxError
        warn "There is a problem with the syntax of #{config_file}. " +
          "You'll need to either fix the syntax or rerun the initialization."
        exit 1
      end if config_file.exist? and config_file.readable?
    end

    # warn config_file
  end

  # detect state directory

  # first command line, then env, then test ./, then ~/

  desc :init, 'Initialize an Intertwingler base configuration.'
  option :store, aliases: %i[-s], type: :string,
    desc: 'Descriptor for the RDF store'
  option :rdf, aliases: %i[-r], type: :string, repeatable: true,
    desc: 'Read in an RDF file'

  def init
    # detect terminal
    unless interactive?
      say 'Sorry, we only do interactive setups for now.'
      exit 1
    end

    prompt = Prompt.new help_color: :cyan, interrupt: :exit

    prompt.reader.on(:keyctrl_a) do |event|
      event.line.move_to_start
    end

    prompt.reader.on(:keyctrl_e) do |event|
      event.line.move_to_end
    end

    prompt.reader.on(:keyctrl_u) do |event|
      line = event.line
      line.move_to_start
      line.delete line.text.length
    end

    prompt.reader.on(:keyctrl_w) do |event|
      line = event.line

      # /\A.*?([[:space:]\/,;._-]+[^[:space:]\/,;._+=-]

      if m = /.*(\/[^\/]*\/*)\z/.match(line)
        chars = m.captures.first.length
        event.line.left   chars
        event.line.delete chars
      end
    end

    prompt.reader.completion_handler = -> path do
      # get this as something manipulable
      path = Pathname(path.strip).expand_path

      dir, file = path.directory? ? [path, ''] : path.split

      dir.glob("#{file}*").select do |c|
        c.readable? and (c.directory? or (c.file? and
          %w[.ttl .rdf .jsonld].include? c.extname.downcase))
      end.map { |x| x.to_s + (x.directory? ? ?/ : '') }
    end

    prompt.say 'Initializing...'
    # TODO maybe someday
    # prompt.say "Taking defaults from #{config_file}..." if @raw_config

    # Collect the following prompts into a structure:
    config = prompt.collect do
      # Store (category mandatory):
      # * quad store driver and options (default RDF::LMDB relative to config home)
      # * RDF files to initialize with (tab completion?)
      #
      prompt.say_md(<<~EOS)

_Intertwingler_ uses a provisional `x-ruby` URN scheme (also known as a
NID) to identify pluggable modules. These take the following form:


```
urn:x-ruby:module/path;Class::Name?=constructor=parameter&other=param

```


In this case, the class identified by the URN is expected to be a
subclass of `RDF::Repository`. This defines how and where the graph
data is stored.

EOS
      key :graph do
        key(:driver).ask 'Driver to use', required: true, default: STORE
        prompt.say_md(<<~EOS)

When an empty graph is initialized, we can load it with the contents
of one or more RDF files.

EOS
        path = Pathname(?.).expand_path
        until prompt.no? 'Add an RDF file to initialize the graph with?'
          path = prompt.ask(?>, value: path.to_s + ?/) or break
          path = Pathname(path) # okay *now*

          if path.file? && path.readable? or !prompt.no?(
            "#{path} doesn't seem to be a readable file. Are you sure?")
            key(:init).values.add_answer path.to_s
            path = path.parent
          end
        end
      end

      # Engine (category optional, default yes):
      # * host (default localhost)
      # * port (default 10101)
      # * domains to expose (default empty == all of them)
      #
      if prompt.yes? 'Configure the main Intertwingler engine?'
        key :engine do
          key(:host).ask 'Host:', default: HOST
          key(:port).ask 'Port:', default: PORT
          prompt.say <<~EOS

The engine's default behaviour is to configure a resolver for every domain
found in the graph. You can limit the DNS domains the engine answers to.

EOS
          domain = prompt.ask 'Add an explicit domain? (leave blank to skip):'
          if domain
            key(:domains).values.add_answer domain
            while (domain = prompt.ask 'Add another? (leave blank if not):')
              key(:domains).values.add_answer domain
            end
          end
        end
      end

      # Static (category optional, default no):
      # * target directory (required)
      unless prompt.no? 'Configure the static site generator?'
        key :static do
          key(:target).ask 'Target directory: ', value: Dir.getwd
        end
      end
    end

    # if resulting configuration is (semantically) identical to
    # existing config file contents, exit.
    if config_file.exist?
      if @raw_config && @raw_config == config
        prompt.warn "An identical configuration already exists! " +
          "No point in overwriting."
        exit 0
      else
        prompt.warn "Overwriting #{config_file}."
      end
    end

    # otherwise, write out a new one
    #
    # (if no state dir found then `mkpath` one)

    unless config_file.parent.exist?
      begin
        config_file.parent.mkpath
      rescue Errno::EPERM
        prompt.error "Can't create directory #{config_file.parent}."
        exit 1
      end
    end

    config_file.open(?w) do |fh|
      Psych.dump munge_keys(config), fh
    end

    # and finished.
    #
    # …later on we can configure handlers and transforms and shit
  end

  desc :engine, 'Run the Intertwingler engine.'
  option :host, aliases: %i[-h], type: :string,
    desc: "Bind to particular host address (#{HOST})"
  option :port, aliases: %i[-p], type: :numeric,
    desc: "Bind to a particular TCP port (#{PORT})"
  option :domain, aliases: %i[-d], type: :string, repeatable: true,
    desc: 'Answer only to this domain'
  option :detach, aliases: %i[-z], type: :boolean, default: false,
    desc: 'Detach process to background'
  option :pid, aliases: %i[-P], type: :string,
    desc: 'Create a PID file when detached'

  def engine
    # we imagine detecting whether the configuration has been initialized
    unless @raw_config
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
