require 'rdf'
require 'rdf/sak/util'
require 'rdf/sak/tfo'
require 'set'

class RDF::SAK::Transform
  # mkay basically this transformation function stuff got too hairy to
  # just do ad-hoc so i guess i'm doing this now

  def self.resolve repo, subject
    # noop
    return subject if subject.is_a? self

    asserted = RDF::SAK::Util.objects_for repo, subject,
      RDF.type, only: :resource

    return if
      (asserted & RDF::SAK::Util.all_related(RDF::SAK::TFO.Transform)).empty?

    # note we won't screw around with cardinality or whatever here
    params = {}
    repo.query([subject, RDF::SAK::TFO.parameter, nil]) do |stmt|
      p = stmt.object
      next unless p.uri?
      params[p] = repo.query(
        [p, RDF::RDFS.range, nil]).objects.filter(&:uri?).sort.first
    end

    new subject, params
  end

  def self.coerce_params params
    # this idiom is everywhere
    params.transform_values do |v|
      Set.new(v.respond_to?(:to_a) ? v.to_a : [v]) unless v.nil?
    end
  end

  attr_reader :subject

  def initialize subject, params = {}
    @subject = subject
    @params  = params
  end

  def keys
    @params.keys.sort
  end

  def [](key)
    @params[key]
  end

  def lint params
    params.keys.sort == keys
  end

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

  class Partial
    def self.resolve repo, subject: nil, transform: nil, params: {}
      raise ArgumentError, 'Must supply either a subject or a transform' unless
        subject or transform

      # coerce the transform if it isn't already
      if transform and !transform.is_a?(RDF::SAK::Transform)
        transform = RDF::SAK::Transform.resolve(transform) or return
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
        candidates.filter! do |s, ps|
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

  class Application < Partial
    # note this is an application of a transform, not an "app"

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
          nil]).objects.filter(&:uri?).sort.first
        if partial
          partial = partials.resolve(repo, subject: partial) or
            raise "Could not find partial #{partial}"
          transform = partial.transform
        else
          transform = repo.query([subject, RDF::SAK::TFO.transform,
            nil]).objects.filter(&:uri?).sort.first or
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
          nil]).objects.filter(&:uri?).sort.first
        output = repo.query([subject, RDF::SAK::TFO.output,
          nil]).objects.filter(&:uri?).sort.first

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
        end.execute(repo).map { |sol| sol[:t] }.compact.uniq.filter do |s|
          partial && repo.has_statement?(
            RDF::Statement(s, RDF::SAK::TFO.completes, partial.subject)) or
            repo.has_statement?(
            RDF::Statement(s, RDF::SAK::TFO.transform, transform.subject))
        end.compact.uniq.sort

        # we only check the params if we don't have a partial
        unless partial
          # XXX uh i dunno if we need to keep candidate params around
          cp = {}
          candidates.filter! do |s|
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
      else
        raise ArgumentError,
          'must have either a subject or transform+input+params'
      end

      # don't forget the output
      output ||= repo.query(
        [subject, RDF::SAK::TFO.output, nil]
      ).objects.filter(&:uri?).sort.first

      new subject, transform, input, output, partial || params

    end

    attr_reader :input, :output, :completes

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
    partial = partials.values.filter do |p|
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
    end.execute(repo).map { |sol| sol[:t] }.compact.uniq.filter do |s|
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

end
