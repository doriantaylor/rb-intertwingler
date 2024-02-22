1# frozen_string_literal: true
require 'intertwingler/types'
require 'rdf/repository'
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

    private

    def x_md which, message = '', **options
      mdopts = options.fetch :markdown, {}
      mdopts[:theme] ||= THEME # sub in a theme if there isn't one
      mdopts[:theme] = mdopts[:theme].merge THEME # *now* merge
      options = options.except :markdown
      send which, TTY::Markdown.parse(message, **mdopts), **options
    end

    public

    # This is just #{TTY::Prompt#say} but with Markdown. Not sure why
    # this isn't just built in.
    def say_md message = '', **options
      x_md :say, message, **options
    end

    def warn_md message = '', **options
      x_md :error, message, **options
    end

    def error_md message = '', **options
      x_md :error, message, **options
    end

    # XXX not sure if i want this here
    def collect_graph_config message: nil
      # gotta overwrite as a local variable or it will fail method resolution
      prompt = self

      prompt.collect do
        key(:driver).ask 'Driver to use',
          default: STORE unless prompt.no? 'Specify graph driver?'

        prompt.say_md(message) if message

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

      return @config_home = Pathname(tests.compact.first).expand_path
    end

    # note we don't want to cache in this case
    Pathname(tests.compact.first).expand_path
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
    out = file == file.basename ?
      config_home(file: false) + file : file.expand_path

    # default behaviour is to cache
    home ? @config_file = out : out
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

  def init_repo urn
    repo = @repo_urn[urn] or raise ArgumentError,
      "there should be a repo at #{urn}"

    if repo.empty?
      require 'rdf/turtle'

      default = base_config.dig(:graph, :driver) == urn
      files = {}

      # if this repository is the default
      if default and i = base_config.dig(:graph, :init)
        files[nil] = i
      end

      base_config.fetch(:authorities, {}).each do |domain, config|
        driver = config.dig :graph, :driver
        init   = config.dig :graph, :init

        files[domain] = init if
          init and (driver.nil? && default || driver == urn)
      end

      files.each do |domain, fs|
        if interactive?
          msg = domain ? "RDF files for #{domain}" : 'default RDF files'
          prompt.say "Loading #{msg}…"
        end

        fs.each do |f|
          prompt.say "#{f} …" if interactive?
          repo.load f
        end
      end

    end

    repo
  end

  def load_repo urn
    raise ArgumentError, 'Expecting an Intertwingler::RubyURN' unless
      urn.is_a? Intertwingler::RubyURN
    cls = urn.object
    raise TypeError,
      "URN refers to #{cls} which is not an RDF::Repository" unless
      cls.ancestors.include? RDF::Repository
    oldpwd = Dir.getwd
    begin
      Dir.chdir config_home

      # this can still raise, of course
      repo = cls.new(**urn.q_component_hash)
    ensure
      Dir.chdir oldpwd
    end

    repo
  end

  # eh deal with this later
  CTRL_W = %[[:space:]/\\'+=_.,:;-]
  CW_RE  = /\A.*?[#{CTRL_W}]\z/o

  public

  class_option :home, aliases: %i[-H],
    desc: 'Configuration home (default: ~/.intertwingler)'
  class_option :config, aliases: %i[-C],
    desc: 'Configuration file (default: $INTERTWINGLER_HOME/intertwingler.conf)'

  no_commands do

    # Note that {Thor} objects get (re)initialized with every command
    # invocation, so calling {Thor#invoke} on one of these commands will
    # actually create a new one.
    def initialize args = [], opts = {}, config = {}
      super

      # XXX figure out how to force the invocation of `init` (with
      # subsequent exit) if there is no config (EXCEPT if the config
      # was passed in through the environment or command line, in which case
      # complain

      unless @base_config
        begin
          @raw_config  = Psych.load_file config_file
          @base_config = Intertwingler::Types::HarnessConfig[@raw_config]
        rescue Psych::SyntaxError
          prompt.error_md <<~EOS
There is a problem with the syntax of #{config_file}. You'll need to
either fix the syntax or rerun the initialization.

EOS
          exit 1
        rescue Dry::Types::CoercionError
          prompt.error_md <<~EOS
The structure of the configuration file #{config_file} appears to be malformed.
EOS
        end if config_file.exist? and config_file.readable?
      end

      # graph repositories by domain and by urn
      @repos    = {}
      @repo_urn = {}

      # warn config_file
    end

    attr_reader :base_config

    def prompt
      # TODO maybe propagate/merge parameters in a sensible way
      @prompt ||= Prompt.new help_color: :cyan, interrupt: :exit
    end

    def repo_for authority
      authority = authority.to_s.strip.downcase

      return @repo_urn[@repos[authority]] if @repos.key? authority

      raise RuntimeError,
        "Can't access repo without base config" unless base_config

      urn = base_config.dig(:authorities, authority, :graph, :driver)
      unless urn
        urn = base_config.dig(:graph, :driver)
        default = true
      end

      raise Intertwingler::ConfigError,
        'No default driver specified' unless urn

      @repo_urn[urn] ||= load_repo urn

      # collect all the authorities for which this is the repo
      base_config.fetch(:authorities, {}).reduce([]) do |out, pair|
        driver = pair.last.dig(:graph, :driver)
        out << pair.first if default && driver.nil? or driver == urn
        out
      end.each { |auth| @repos[auth] = urn }

      # XXX this is a bit spaghetti-ey
      init_repo urn
    end

    def authorities
      base_config.fetch(:authorities, {}).keys.map do |k|
        [k, repo_for(k)]
      end.to_h
    end

    # end of no_commands region
  end

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

    prompt = self.prompt # copy prompt

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
    # prompt.say "Taking defaults from #{config_file}..." if base_config

    # Collect the following prompts into a structure:
    config = prompt.collect do
      if prompt.yes? 'Specify the host and port for the application?'
        key(:host).ask 'Host:', default: HOST
        key(:port).ask 'Port:', default: PORT
      end

      if prompt.yes? 'Configure a global default graph database?'
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
        gconf = prompt.collect_graph_config(message: <<~EOS) if prompt

When an empty graph is initialized, we can load it with the contents
of one or more RDF files.

EOS
        key(:graph).add_answer gconf if gconf and !gconf.empty?
      end

      if prompt.yes? 'Add domain-specific configuration?'
        tmp = {}
        while domain = prompt.ask('Domain name (leave blank to stop):')
          subcol = create_collector.call do
            gconf = prompt.collect_graph_config
            key(:graph).add_answer gconf if gconf and !gconf.empty?

            unless prompt.no? 'Configure the static site generator?'
              key :static do
                key(:target).ask 'Target directory: ', value: Dir.getwd
              end
            end

          end

          # this little incantation is to ensure a null value rather
          # than an empty hash.
          tmp[domain] = subcol ? subcol.empty? ? nil : subcol : nil
        end

        key(:authorities).add_answer tmp unless tmp.empty?
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
    return invoke :init unless base_config

    # require 'pry'
    # binding.pry

    require 'rack'
    require 'rackup'
    require 'intertwingler/harness'

    require 'logger'

    # XXX
    log = Logger.new $stderr

    # give us a harness
    harness = Intertwingler::Harness.new authorities,
      home: config_home, log: log

    # initialize rack server
    Rackup::Server.start({
      app: harness,
      # server: ...
      # environment: ...
      daemonize: options[:detach],
      Host: options[:host] || base_config[:host],
      Port: options[:port] || base_config[:port],
    })
  end

  desc :pry, 'run pry lol'
  def pry
    require 'intertwingler/harness'
    require 'pry'
    binding.pry
  end

  desc :load, 'Load one or more RDF files.'
  def load
  end

  desc :dump, 'Dump the graph database to a file.'
  def load
  end

  desc :nuke, 'Wipe out the contents of the graph database.'
  def nuke
    # we imagine detecting whether the configuration has been initialized
    unless base_config
      warn "Can't nuke anything if there isn't anything to nuke."
      exit 1
    end

    unless interactive?
      warn "Can't nuke except for interactively."
      exit 1
    end

    unless prompt.no? 'Really?'
      require 'intertwingler/harness'

      # give us a harness
      harness = Intertwingler::Harness.new authorities, home: config_home

      harness.engines.values.each do |engine|
        engine.repo.clear
      end

      say "\u{2622}\u{FE0F} \u{1F480} \u{2622}\u{FE0F}"
    end
  end

  default_command :engine

end
