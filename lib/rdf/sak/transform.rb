require 'rdf'
require 'rdf/sak/tfo'
require 'rdf/sak/util'
require 'set'
require 'mimemagic'

# This class encapsulates a specification for an individual
# transformation function, including its parameter spec, accepted and
# returned types, identity, and implementation.
#
class RDF::SAK::Transform
  # mkay basically this transformation function stuff got too hairy to
  # just do ad-hoc so i guess i'm doing this now

  private

  def self.numeric_objects repo, subject, predicate, entail: false
    RDF::SAK::Util.objects_for(repo, subject, predicate, entail: entail,
      only: :literal).map(&:object).select { |c| c.is_a? Numeric }.sort
  end

  def self.gather_params repo, subject
    params = {}
    RDF::SAK::Util.objects_for(repo, subject, RDF::SAK::TFO.parameter,
                               entail: false, only: :resource).each do |ps|
      param = params[ps] ||= {}

      # slug/identifier
      if id = RDF::SAK::Util.objects_for(
        repo, ps, RDF::Vocab::DC.identifier, only: :literal).sort.first
        param[:id] = id.value.to_sym
      end

      # rdfs:range
      range = RDF::SAK::Util.objects_for(
        repo, ps, RDF::RDFS.range, only: :resource)
      param[:range] = range.to_set unless range.empty?

      # default = RDF::SAK::Util
      param[:default] = RDF::SAK::Util.objects_for(
        repo, ps, RDF::SAK::TFO.default)

      # cardinalities
      param[:minc] = 0
      param[:maxc] = Float::INFINITY

      if c0 = numeric_objects(repo, ps, RDF::OWL.cardinality).first
        param[:minc] = param[:maxc] = c0
      else
        if c1 = numeric_objects(repo, ps, RDF::OWL.minCardinality).first
          param[:minc] = c1
        end
        if c2 = numeric_objects(repo, ps, RDF::OWL.maxCardinality).first
          param[:maxc] = c2
        end
      end
    end

    params
  end

  def self.gather_accepts_returns repo, subject, raw: false, returns: false
    literals = []
    lists    = []
    pred     = RDF::SAK::TFO[returns ? 'returns' : 'accepts']
    repo.query([subject, pred, nil]).objects.each do |o|
      if o.literal?
        literals << o
      else
        lists << RDF::List.from(repo, o).to_a
      end
    end
    # this is mainly to give us consistent results
    out = (lists.sort.flatten + literals.sort).uniq
    # raw as in raw literals
    raw ? out : out.map(&:value)
  end

  protected

  # This does nothing in the base class.
  def init_implementation repo, root
  end

  public

  # Resolve a transform out of the repository. Optionally supply a
  # block to resolve any implementation associated with the transform.
  #
  # @param repo [RDF::Queryable]
  # @param subject [RDF::Resource]
  # @param root
  # @yieldparam implementation [RDF::Resource] the implementation
  #  identifier for the resolver
  # @yieldreturn [Proc] the implementation
  def self.resolve repo, subject, root: nil, &block
    # noop
    return subject if subject.is_a? self

    root ||= Pathname(Dir.getwd)

    asserted = RDF::SAK::Util.objects_for repo, subject,
      RDF.type, only: :resource

    return if
      (asserted & RDF::SAK::Util.all_related(RDF::SAK::TFO.Transform)).empty?

    params = gather_params repo, subject

    plist = if pl = RDF::SAK::Util.objects_for(repo, subject,
              RDF::SAK::TFO['parameter-list'], only: :resource).sort.first
              RDF::List.from(repo, pl).to_a
            else
              params.keys.sort
            end

    accepts = gather_accepts_returns repo, subject
    returns = gather_accepts_returns repo, subject, returns: true

    tclass = self

    if impl = RDF::SAK::Util.objects_for(repo, subject,
      RDF::SAK::TFO.implementation, only: :uri).sort.first
      case impl.to_s
      when /^file:/i then
        # XXX redo this later
        if /xsl/i.match? MimeMagic.by_path(impl.path.to_s).to_s
          tclass = RDF::SAK::Transform::XSLT
        end
      when /^urn:x-ruby:(.*)$/i then
        cn = $1
        begin
          cs = Object.const_get cn
          tclass = cs
        rescue NameError, e
          raise NotImplementedError,
            "Could not locate implementation for #{impl}!"
        end
      end
    end

    tclass.new subject, params: params, param_list: plist, accepts: accepts,
      returns: returns, implementation: impl, repo: repo, root: root
  end

  def self.coerce_params params
    # this idiom is everywhere
    params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v]) unless v.nil?
    end
  end

  attr_reader :subject

  # Initialize a transform from data.
  # @param subject [RDF::Resource]
  # @param params [Hash]
  # @param param_list [Array]
  # @param accepts [Array]
  # @param returns [Array]
  # @param implementation [RDF::Resource]
  # @yield code to execute when called
  #
  def initialize subject, params: {}, param_list: [],
      accepts: %w[*/*], returns: %w[*/*], implementation: nil,
      repo: nil, root: nil
    @subject = subject.dup.freeze
    @params  = params.freeze
    @plist   = (param_list.empty? ? params.keys.sort : param_list.dup).freeze
    @pcache  = params.map { |k, v| [v[:id], k] }.to_h.freeze
    @accepts = (accepts.respond_to?(:to_a) ? accepts.to_a : [accepts]).freeze
    @returns = (returns.respond_to?(:to_a) ? returns.to_a : [returns]).freeze
    @impl    = implementation.freeze

    # initialize the implementation
    init_implementation repo, root
    
    # @code    = block
  end

  # Return the identifier of the implementation.
  #
  # @return [RDF::URI]
  #
  def implementation
    @impl
  end

  # True if this transform is *actually* implemented.
  #
  # @return [false, true]
  #
  def implemented?
    false
  end
  
  # Return the parameter list, or a sorted list of parameter keys in lieu
  #
  # @return [Array]
  #
  def keys
    @plist
  end

  # Retrieve a parameter spec, either by its fully-qualified URI or
  # its `dct:identifier`.
  #
  # @param key [RDF::Resource,Symbol,String] the parameter URI or its identifier
  # @return [Hash] the parameter spec
  #
  def [](key)
    out = case key
          when RDF::Resource then @params[key]
          when Symbol then @params[@pcache[key]]
          when String
            @params[@pcache[key.to_sym]] || @params[RDF::URI(key)]
          end
    # add the key to the group
    out.merge({ uri: key }) if out
  end

  # XXX kill this
  def lint params
    raise ArgumentError, "params must be a hash, not #{params.class}" unless
      params.is_a? Hash
    params.keys.sort == keys
  end

  # Return the validated parameters or raise an exception.
  #
  # @param params [Hash] the hash of parameters
  # @return [Hash] the validated parameters
  #
  def validate params, symbols: true
    # duplicate so we can delete from it
    params = params.dup
    out = {}

    # note the instance variable vs the argumenet
    @params.each do |k, spec|
      v = params.delete(k) || params.delete(spec[:id]) || []
      v = (v.respond_to?(:to_a) ? v.to_a : [v]).map do |v|
        case v
        when RDF::Term then v
        when URI then RDF::URI(v.to_s)
        when nil then RDF::nil
        else
          RDF::Literal(v)
        end
      end

      # XXX one day we should check types but not today

      # give us the default(s) then
      v = spec[:default].dup if v.empty? and spec[:default]

      # but we *will* check the cardinality
      minc = spec.fetch :minc, 0
      maxc = spec.fetch :maxc, Float::INFINITY

      raise ArgumentError, "Parameter #{k} must have at least"\
        " #{minc} value#{minc == 1 ? '' : ?s }" if v.size < minc
      raise ArgumentError, "Parameter #{k} must have at most"\
        " #{maxc} value#{maxc == 1 ? '' : ?s }" if v.size > maxc
      # XXX if cardinality == 1 should we set v to v.first? dunno

      # now overwrite k
      k = spec[:id] || k.to_s if symbols

      out[k] = v
    end

    # if params are not empty then this is an error
    raise ArgumentError,
      "Unrecognized parameters #{params.keys.join ', '}" unless params.empty?

    out
  end

  # Check the parameters and apply the function, then check the
  # output. Parameters are checked with {#validate} for key
  # resolution, cardinality, range, and type.
  #
  # @param input [String,IO,#to_s,#read] Something bytelike
  # @param params [Hash,RDF::SAK::Transform::Partial] the instance parameters
  # @param parsed [Object] the already-parsed object, if applicable
  # @param type [String] the content-type of the input
  # @param accept [String] a string in the form of an Accept header
  # @yieldparam output [String,IO] the output
  # @yieldparam parseout [Object] the parsed output, if applicable
  # @return
  #
  def apply input, params = {}, parsed: nil,
      type: 'application/octet-stream', accept: '*/*', &block
    raise NotImplementedError, "Transform #{@id} is not implemented!" unless
      implemented?

    # this will succeed or explode
    params = validate params

    # XXX validate accept or explode

    # run the transform
    out, parseout = execute input, parsed, params

    # bail out if nothing was returned
    return unless out

    # now run the block if present
    block.call out, parseout if block

    # return it to the caller
    [out, parseout]
  end

  # Cache for partial transform function applications, 
  class PartialCache
    # this is a cache for partial function applications so we don't
    # have to keep going back to the rdf graph
    private

    def coerce_params params
      RDF::SAK::Transform.coerce_params params
    end

    public

    def initialize
      @cache      = {}
      @mapping    = {}
      @transforms = {}
    end

    def transforms
      @transforms.dup
    end

    # "transform" may also be the subject
    def get transform, params
      transform = transform.subject if transform.is_a? RDF::SAK::Transform

      # return direct cache entry if transform is really the subject
      return @cache[transform] if @cache.key? transform

      # otherwise return the mapping
      @mapping[transform][coerce_params params]
    end

    # the second argument 
    def resolve repo, subject: nil, transform: nil, params: {}
      if subject
        if subject.is_a? RDF::SAK::Transform::Partial
          # snag the transform
          transform = @transforms[subject.transform] ||=
            RDF::SAK::Transform.resolve(repo, subject.transform) or
            raise 'Could not resolve the transform associated with ' + 
            subject.subject      

          # mkay now add this to the cache
          t = @mapping[subject.transform] ||= {}
          @cache[subject.subject] ||= t[subject.params] ||= subject
        else
          # resolve the partial
          par = @cache[subject] ||
            RDF::SAK::Transform::Partial.resolve(repo, subject: subject) or
            return

          # register the transform
          @transforms[par.transform] ||=
            RDF::SAK::Transform.resolve(repo, par.transform)

          # initialize the mapping if not present
          t = @mapping[par.transform] ||= {}

          # off we go
          @cache[subject] ||= t[par.params] ||= par
        end
      elsif transform
        params = coerce_params params

        unless transform.is_a? RDF::SAK::Transform
          if @transforms[transform]
            transform = @transforms[transform]
          else
            transform = RDF::SAK::Transform.resolve(repo, transform) or return
            @transforms[transform.subject] = transform
          end
        end

        # note the *presence* of the key means the cache item has been
        # checked already; its *value* may be nil
        t = @mapping[transform.subject] ||= {}
        return t[params] if t.key? params
        
        par = t[params] = RDF::SAK::Transform::Partial.resolve(repo,
          transform: transform, params: params) or return
        @cache[par.subject] = par
      end
    end
  end

  # This class is a harness for holding all the transforms and
  # operating over them.
  class Harness
    # Create a new harness instance.
    #
    # @param repo [RDF::Repository] the repository to find RDF data
    # @param root [String,Pathname] the root directory for implementations
    def initialize repo, root, partials: nil
      raise ArgumentError,
        "repo is #{repo.class}, not an RDF::Repository" unless
        repo.is_a? RDF::Repository
      @repo = repo
      @root = Pathname(root).expand_path
      raise ArgumentError, "Root #{@root} does not exist" unless
        @root.directory? and @root.readable?
      @cache    = {}
      @partials = partials.is_a?(RDF::SAK::Transform::PartialCache) ? partials :
        RDF::SAK::Transform::PartialCache.new
    end

    # Bootstrap all the transforms.
    #
    # @param repo [RDF::Repository] the repository to find RDF data
    # @param root [String,Pathname] the root directory for implementations
    # @return [RDF::SAK::Transform::Harness] the harness instance
    def self.load repo, root
      self.new(repo, root).load
    end

    # Load transforms into an existing instance
    # @return [Array] the transforms
    def load
      RDF::SAK::Util.subjects_for(@repo, RDF.type,
        RDF::SAK::TFO.Transform, only: :resource).each do |id|
        # do what the algorithm says
        resolve id
      end

      # return self so we can daisy-chain
      self
    end

    def transforms
      @cache.keys
    end

    # Resolve a transform based on its ID
    def resolve id
      return @cache[id] if @cache[id]
      # XXX raise???
      transform  = RDF::SAK::Transform.resolve(@repo, id, root: @root) or
        return nil
      @cache[id] = transform
    end
  end

  class Partial
    # Resolve a partial function application with the given parameters.
    #
    # @param repo [RDF::Repository] the repository to draw from
    # @param subject [RDF::Resource] the identity of the partial
    # @param transform [RDF::Resource] the identity of the transform
    # @param params [Hash] key-value pairs 
    def self.resolve repo, subject: nil, transform: nil, params: {}
      raise ArgumentError, 'Must supply either a subject or a transform' unless
        subject or transform

      # coerce the transform if it isn't already
      if transform
        transform = RDF::SAK::Transform.resolve(repo, transform) or
          return unless transform.is_a?(RDF::SAK::Transform)
      elsif subject.is_a? RDF::URI
        # locate the transform if given the subject
        transform = RDF::SAK::Util.objects_for(repo, subject,
          RDF::SAK::TFO.transform, only: :resource).first or return
        transform = RDF::SAK::Transform.resolve(repo, transform) or
          return
        warn transform
      end

      # obtain the subject for the given parameters
      if subject
        params = {}
        transform.keys.each do |p|
          o = Set.new(repo.query([subject, p, nil]).objects)
          params[p] = o unless o.empty?
        end
      else
        params = RDF::SAK::Transform.coerce_params params

        candidates = RDF::Query.new do
          # XXX we should sort parameters by longest value since
          # longer values will probably be less common; anyway this is
          # gonna all need to be rethought
          params.each { |p, objs| objs.each { |o| pattern [:s, p, o] } }
          pattern [:s, RDF.type, RDF::SAK::TFO.Partial]
          pattern [:s, RDF::SAK::TFO.transform, transform.subject]

          # add any remaining parameters
          # XXX this actually messes up; we don't want this
          # (transform.keys - params.keys.sort).each { |r| pattern [:s, r, nil] }
        end.execute(repo).map { |sol| [sol[:s], {}] }.to_h

        # warn "yo #{transform.subject} #{params} #{candidates}"

        # this is ruby being cheeky
        candidates.select! do |s, ps|
          transform.keys.each do |p|
            o = Set.new(repo.query([s, p, nil]).objects)
            ps[p] = o unless o.empty?
          end
          ps == params
        end

        return if candidates.empty?

        # sort it so we always get the same thing
        subject = candidates.keys.sort.first
        params  = candidates[subject]
      end

      self.new subject, transform.subject, params
    end

    attr_reader :subject, :transform

    def initialize subject, transform, params = {}
      @subject   = subject
      @transform = transform.is_a?(RDF::SAK::Transform) ?
        transform.subject : transform
      @params    = params.dup unless params.is_a? RDF::SAK::Transform::Partial
    end

    def [](key)
      @params[key]
    end

    def keys
      @params.keys
    end

    def params
      @params.dup
    end

    def matches? params
      params = params.transform_values do |v|
        Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
      end

      @params == params
    end

    def ===(other)
      return false unless other.is_a? RDF::SAK::Transform::Partial
      transform == other.transform and matches? other.params
    end

    def ==(other)
      self === other and subject == other.subject
    end
  end

  # A record of a transformation function application.
  # @note "Application" as in to "apply" a function, not an "app".
  class Application < Partial
    # Resolve a particular function application from the repository
    # 
    def self.resolve repo, subject: nil, transform: nil,
        input: nil, output: nil, params: {}, partials: nil
      # either a subject or transform + input + output? + params?

      partials ||= RDF::SAK::Transform::PartialCache.new
      partial = nil

      if subject
        # noop
        return subject if subject.is_a? self

        # okay partial
        partial = repo.query([subject, RDF::SAK::TFO.completes,
          nil]).objects.select(&:uri?).sort.first
        if partial
          partial = partials.resolve(repo, subject: partial) or
            raise "Could not find partial #{partial}"
          transform = partial.transform
        else
          transform = repo.query([subject, RDF::SAK::TFO.transform,
            nil]).objects.select(&:uri?).sort.first or
            raise "Could not find a transform for #{subject}"

          t = RDF::SAK::Transform.resolve(repo, transform) or
            raise "Could not find transform #{transform}"
          transform = t

          # get params
          params = {}
          transform.keys.each do |p|
            o = Set.new(repo.query([subject, p, nil]).objects) 
            params[p] = o unless o.empty?
          end
        end

        # get inputs and outputs
        input  = repo.query([subject, RDF::SAK::TFO.input,
          nil]).objects.select(&:uri?).sort.first
        output = repo.query([subject, RDF::SAK::TFO.output,
          nil]).objects.select(&:uri?).sort.first

        raise 'Data must have both input and output' unless input and output

      elsif transform and input and params
        # do params
        params = RDF::SAK::Transform.coerce_params params

        # do transform
        t = RDF::SAK::Transform.resolve(repo, transform) or
          raise "Could not resolve transform #{transform}"
        transform = t

        # do partial
        partial = partials.resolve(repo, transform: transform, params: params)

        # collect function application receipts
        candidates = RDF::Query.new do
          # note that there is no cost-based optimization so we write
          # these in the order of least to most cardinality
          pattern [:t, RDF::SAK::TFO.output, output] if output
          pattern [:t, RDF::SAK::TFO.input,  input]
        end.execute(repo).map { |sol| sol[:t] }.compact.uniq.select do |s|
          partial && repo.has_statement?(
            RDF::Statement(s, RDF::SAK::TFO.completes, partial.subject)) or
            repo.has_statement?(
            RDF::Statement(s, RDF::SAK::TFO.transform, transform.subject))
        end.compact.uniq.sort

        # we only check the params if we don't have a partial
        unless partial
          # XXX uh i dunno if we need to keep candidate params around
          cp = {}
          candidates.select! do |s|
            ap = cp[s] ||= {}
            transform.keys.each do |p|
              o = Set.new(repo.query([s, p, nil]).objects)
              ap[p] = o unless o.empty?
            end
            ap == params
          end
        end

        return if candidates.empty?

        if candidates.size == 1
          subject = candidates.first
        else
          # now we have the unlikely case that there are two identical
          # records so we just sort em first by end date, then by
          # start date, then lexically
          subject = candidates.map do |s|
            st, et = %i[startedAtTime endedAtTime].map do |p|
              repo.query([s, RDF::Vocab::PROV[p], nil]).map do |stmt|
                dt = stmt.object.object
                dt if dt.is_a? DateTime
              end.compact.sort.last
            end
            [s, st, et]
          end.sort do |a, b|
            # first check latest end-time, then check latest start-time
            c = a[2] && b[2] ? b[2] <=> a[2] : 0
            # if those two yield nothing, then sort lexically i guess
            (c == 0 && a[1] && b[1]) ? b[1] <=> a[1] : a[0] <=> b[0]
          end.first.first
        end
      else
        raise ArgumentError,
          'must have either a subject or transform+input+params'
      end

      # don't forget the output
      output ||= repo.query(
        [subject, RDF::SAK::TFO.output, nil]
      ).objects.select(&:uri?).sort.first

      new subject, transform, input, output, partial || params

    end

    attr_reader :input, :output, :completes

    # Create a new function application from whole cloth.
    #
    # @param subject [RDF::Resource]
    # @param transform [RDF::Resource] the identifier for the transform
    # @param input  [RDF::Resource] the identifier for the input
    # @param output [RDF::Resource] the identifier for the output
    # @param params [Hash, RDF::SAK::Transform::Partial] the parameters
    #   or partial application that is completed
    def initialize subject, transform, input, output, params = {}
      # params may be a partial
      super subject, transform, params

      @input     = input
      @output    = output
      @completes = params if params.is_a? Partial
    end

    def [](key)
      # note complete is
      (@completes || @params)[key]
    end

    def keys
      (@completes || @params).keys
    end

    def params
      @completes ? @completes.params : @params.dup
    end

    def transform
      @completes ? @completes.transform : @transform
    end

    def matches? params
      return @completes.matches? params if @completes
      super params
    end

    def ===(other)
      return false unless other.is_a? Application
      return false unless @input == other.input and @output == other.output

      # now the comparand is either the partial or us
      cmp = @completes || self

      # and this should do it
      other.transform == cmp.transform and other.matches? cmp.params
   end
  end

  # XXX everything below this line is trash

  def match_params repo, candidate, params = {}
    # overwrite normalized params
    params = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    struct = {}
    params.keys.each do |p|
      repo.query([candidate, p, nil]) do |stmt|
        x = struct[stmt.predicate] ||= Set.new
        x << stmt.object
      end
    end
  end

  # Resolve a transformation application function in the repository
  # with the given inputs and outputs.
  #
  # XXX note that this thing in its current state will not distinguish
  # between two different function applications that happen to map the
  # same input to the same output, but with different scalar
  # parameters. For example, the `subtree` function could be given two
  # different XPath queries but return the same subtree.
  #
  def resolve_transformation repo, transform, input, output = nil,
      graph: nil, params: {}, partials: {}

    # overwrite normalized params XXX replace this with something real
    params = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    # first we check the cache of partials to see if there is one that
    # matches our parameters. we want to use trasns
    partial = partials.values.select do |p|
      p.transform == transform and p.matches? params
    end.sort.first

    # find the partial if there is one
    unless partial
      partial = Partial.resolve transform: transform, params: params
      # argh this isn't right; it should be partials[transform][params]
      partials[partial.subject] = partial if partial
    end

    # collect function application receipts
    candidates = RDF::Query.new do
      # note that there is no cost-based optimization so we write
      # these in the order of least to most cardinality
      pattern [:t, RDF::SAK::TFO.output, output]
      pattern [:t, RDF::SAK::TFO.input,  input]
    end.execute(repo).map { |sol| sol[:t] }.compact.uniq.select do |s|
      repo.has_statement?(
        RDF::Statement(s, RDF::SAK::TFO.transform, transform)) or
        partial && repo.has_statement?(
        RDF::Statement(s, RDF::SAK::TFO.completes, partial))
    end.compact.uniq

    # first will be nil if this is empty so voila
    return candidates.first unless candidates.size > 1

    # now we have the unlikely case that there are two identical records
    candidates.map do |s|
      st, et = %i[startedAtTime endedAtTime].map do |p|
        repo.query([s, RDF::Vocab::PROV[p], nil]) do |stmt|
          dt = stmt.object.object
          dt if dt.is_a? DateTime
        end.compact.sort.last
      end
      [s, st, et]
    end.sort do |a, b|
      # first check latest end-time, then check latest start-time
      c = a[2] && b[2] ? b[2] <=> a[2] : 0
      # if those two yield nothing, then sort lexically i guess
      (c == 0 && a[1] && b[1]) ? b[1] <=> a[1] : a[0] <=> b[0]
    end.first.first
  end

  def record_application repo, transform, input, output, start, finish,
      partial: false, graph: nil, subject: nil, params: {}
  end

  # get transform
  def get_partial_transform repo, function, params = {}
    temp = {}
    RDF::Query.new do
      pattern [:s, RDF.type, RDF::SAK::TFO.Partial]
      pattern [:s, RDF::SAK::TFO.transform, function]
      params.keys.each { |k| pattern [:s, k, nil] }
    end.execute(repo).each do |sol|
      t = temp[sol[:s]] ||= {}
      params.keys.each do |k|
        # make these a set for now cause we don't care about the 
        t[k] = Set.new(repo.query([sol[:s], k, nil]).objects)
      end
    end

    # now we imagine massaging the candidates' parameters so they
    # match the input (eg sets/arrays or whatever)

    # (in this case the input params are made to match the retrieved params)
    newp = params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v])
    end

    # sort this because we want it to return the same thing every time
    # if there are multiples for some reason
    temp.keys.sort.each do |k|
      # do a cheaper comparison first
      next unless temp[k].keys.sort == params.keys.sort
      # 
      return k if temp[k] == newp
    end

    nil
  end

  class XPath < RDF::SAK::Transform
    protected

    def execute input, parsed = nil, params
      xpath  = params.fetch(:xpath, []).first or raise
      prefix = params.fetch(:prefix, []).map do |x|
        x.value.split(/\s*:\s*/, 2)
      end.to_h.transform_keys(&:to_sym)
      reindent = (params.fetch(:reindent).first || RDF::Literal(true)).object

      begin
        parsed ||= Nokogiri.XML input
      rescue Nokogiri::SyntaxError
        # XXX i dunno, raise?
        return
      end

      doc = RDF::SAK::Util.subtree parsed,
        xpath.value, prefixes: prefix, reindent: reindent

      return unless doc

      [doc.to_xml, doc]
    end

    public

    def implemented?
      true
    end
  end

  class XSLT < RDF::SAK::Transform
    protected

    def init_implementation _, root
      raise ArgumentError,
        "Need a root to initialize the implementation" unless root
      root = Pathname(root).expand_path unless root.is_a? Pathname
      raise ArgumentError, "#{root} is not a readable directory" unless
        root.directory? and root.readable?

      # XXX this assumes this is a file URI but so far that is the
      # only way we get here
      filename = root + implementation.path
      raise ArgumentError, "#{filename} is not a readable file" unless 
        filename.file? and filename.readable?
      @sheet = Nokogiri::XSLT(filename.read)
    end

    def execute input, parsed = nil, params
      begin
        parsed ||= Nokogiri.XML input
      rescue Nokogiri::SyntaxError
        # XXX i dunno, raise?
        return
      end

      # XXX do we wanna allow params?
      out = @sheet.transform parsed

      # now return string and still-parsed
      [@sheet.serialize(out), out]
    end

    public

    def implemented?
      true
    end

  end
end
