require 'intertwingler/version'

require 'mimemagic'
require 'set'
require 'rdf'
require 'rdf/vocab'
require 'rdf/reasoner'
require 'intertwingler/resolver'
require 'intertwingler/util/clean'

# load up my vocabs before reasoner is applied
require 'intertwingler/vocab/ci'
require 'intertwingler/vocab/tfo'
require 'intertwingler/vocab/ibis'

# also third-party vocabs not found in RDF::Vocab
require 'intertwingler/vocab/pav'
require 'intertwingler/vocab/qb'
require 'intertwingler/vocab/scovo'

module Intertwingler

  # gotta make sure this gets run
  RDF::Reasoner.apply(:rdfs, :owl)

  # This is to attach inferencing operations directly to the
  # repository instead of bolting it on every time like a schmuck

  # this module bolts functionality onto RDF::Repository

  module GraphOps
    include Intertwingler::Util::Clean

    private

    # rdf term type tests
    NTESTS = { uri: :"uri?", blank: :"node?", literal: :"literal?" }.freeze
    NMAP   = { iri: :uri, bnode: :blank }.merge(
      ([%i[uri blank literal]] * 2).transpose.to_h)

    # if the instance data doesn't have an exact property mentioned in
    # the spec, it may have an equivalent property or subproperty we
    # may be able to use. we could imagine a scoring system analogous
    # to the one used by CSS selectors, albeit using the topological
    # distance of classes/predicates in the spec versus those in the
    # instance data.

    # think about dcterms:title is a subproperty of dc11:title even
    # though they are actually more like equivalent properties;
    # owl:equivalentProperty is not as big a conundrum as
    # rdfs:subPropertyOf.

    # if Q rdfs:subPropertyOf P then S Q O implies S P O. this is
    # great but property Q may not be desirable to display.

    # it may be desirable to be able to express properties to never
    # use as a label, such as skos:hiddenLabel

    # consider ranked alternates, sequences, sequences of alternates.
    # (this is what fresnel does fyi)

    LABELS = {
      RDF::RDFS.Resource => {
        label: [
          # main
          [RDF::Vocab::SKOS.prefLabel, RDF::RDFS.label,
           RDF::Vocab::DC.title, RDF::Vocab::DC11.title, RDF::RDFV.value],
          # alt
          [RDF::Vocab::SKOS.altLabel, RDF::Vocab::DC.alternative,
           RDF::Vocab::SKOS.hiddenLabel],
        ],
        desc: [
          # main will be cloned into alt
          [RDF::Vocab::DC.abstract, RDF::Vocab::DC.description,
           RDF::Vocab::DC11.description, RDF::RDFS.comment,
           RDF::Vocab::SKOS.note],
        ],
      },
      RDF::Vocab::FOAF.Document => {
        label: [
          # main
          [RDF::Vocab::DC.title, RDF::Vocab::DC11.title],
          # alt
          [RDF::Vocab::BIBO.shortTitle, RDF::Vocab::DC.alternative],
        ],
        desc: [
          # main
          [RDF::Vocab::BIBO.abstract, RDF::Vocab::DC.abstract,
           RDF::Vocab::DC.description, RDF::Vocab::DC11.description],
          # alt
          [RDF::Vocab::BIBO.shortDescription],
        ],
      },
      RDF::Vocab::FOAF.Agent => {
        label: [
          # main (will get cloned into alt)
          [RDF::Vocab::FOAF.name],
        ],
        desc: [
          # main cloned into alt
          [RDF::Vocab::FOAF.status],
        ],
      },
    }
    LABELS[RDF::OWL.Thing] = LABELS[RDF::RDFS.Resource]

    # Coerce a node spec into a canonical form. Node specs in
    # #subjects_for and #objects_for methods are arrays of symbols `:uri`,
    # `:blank`, `:literal`, and then the synonyms `:resource` (shorthand
    # for `[:uri, :blank]`), `:iri` for `:uri`, and `:bnode` (for
    # `:blank`).
    #
    # @param spec [Symbol, Array<Symbol>] the node spec
    # @param rev [false, true] whether the node spec is to be applied to
    #  a subject node rather than an object node
    #
    def coerce_node_spec spec, rev: false
      spec = [] if spec.nil?
      spec = spec.respond_to?(:to_a) ? spec.to_a : [spec]
      spec = spec - [:resource] + [:uri, :blank] if spec.include? :resource
      raise 'Subjects are never literals' if rev and spec.include? :literal

      # normalize out the synonyms
      spec = NMAP.values_at(*spec).compact.uniq

      # give us some nice defaults if this is still empty
      if spec.empty?
        spec = NTESTS.keys
        # make sure this doesn't end up in here if we're looking at subjects
        spec.delete :literal if rev
      end

      # et voilà
      spec.uniq
    end

    # Determine whether a given node matches a node spec.
    def node_matches? node, spec
      spec.any? { |k| node.send NTESTS[k] }
    end

    # this ensures languages are something we can use
    def coerce_languages languages
      languages = languages.respond_to?(:to_a) ? languages.to_a : [languages]
      languages.map { |lang| lang.to_s.strip.tr_s(?_, ?-).downcase }.uniq
    end

    # this tells us if the literal's language is in our given set
    def is_language? literal, languages
      return false unless literal.literal? and lang = literal.language
      languages = coerce_languages languages
      lang = lang.to_s.strip.tr_s(?_, ?-).downcase

      languages.include? lang
    end

    # this gives us a set of inverse (and symmetric) properties for
    # the given input
    def invert_semantic properties, entail: false
      properties = assert_resources properties, empty: false

      inverted = properties.map do |p|
        if icache[p]
          icache[p]
        else
          # inverse properties are available by entailment
          set = p.respond_to?(:inverseOf) ? p.inverseOf.to_set : Set[]
          # symmetric properties go in as-is
          set << p if symmetric? p
          icache[p] = set
        end
      end.reduce :|

      # warn properties.inspect, inverted.inspect

      # don't forget to entail
      entail ? property_set(inverted) : inverted
    end

    # this gives us a label spec we can use
    def process_labels struct
      out = {}

      struct.each do |type, spec|
        raise ArgumentError,
          "keys need to be RDF::URI, not #{type.class}" unless
          type.is_a? RDF::URI
        raise ArgumentError,
          "spec needs to be a Hash, not #{spec.inspect}" unless
          spec.is_a? Hash

        ospec = out[type] ||= {}
        spec.each do |variant, pair|
          raise ArgumentError,
            "variant needs to be :label or :desc, not #{variant}" unless
            %i[label desc].include? variant
          raise ArgumentError,
            "struct[#{type}][#{variant}] must be an array of arrays" unless
            pair.is_a? Array and !pair.empty? and
            pair.all? { |x| x.is_a? Array }

          # truncate list to two elements (main, alt)
          pair = pair[0, 2]

          vspec = ospec[variant] ||= [[], []]

          pair.each_index do |i|
            preds = pair[i].dup

            raise ArgumentError,
              "specify at least one property" if preds.empty?
            raise ArgumentError,
              "property list needs to be RDF::URIs" unless
              preds.all? { |p| p.is_a? RDF::URI }

            j = 0
            loop do
              # obtain equivalent properties we don't already have
              equiv = preds[j].entail(:equivalentProperty) - preds
              # splice them into the existing predicates
              preds.insert(j + 1, *equiv) unless equiv.empty?
              # skip over what we just added
              j += equiv.length + 1
              # stop when we have run off the end
              break if j >= preds.length
            end

            # now we prepend the new predicates and squash them down;
            # we're prepending because this may already exist via the
            # equivalent class bit a few lines down
            vspec[i].unshift(*preds)
            vspec[i].uniq!
          end

          # copy main to alt if alt is missing
          if pair.length < 2
            vspec[1].unshift(*vspec[0])
            vspec[1].uniq!
          end

        end

        # add any equivalent classes
        type.entail(:equivalentClass).each { |equiv| out[equiv] ||= spec }
      end

      out.freeze
    end

    public

    # Returns the input term coerced to an RDF::Vocabulary::Term.
    #
    # @param term [URI, RDF::URI, RDF::Vocabulary::Term, #to_s] the term
    # @param strict [false, true] whether to strictly enforce the conversion
    #
    # @return [RDF::Vocabulary::Term, RDF::URI, nil]
    #
    def coerce_term term, uri: false, strict: false
      out = tcache[coerce_resource(term).to_s] ||=
        coerce_resource term, as: :term

      return if uri and !out.uri?

      return if strict and [RDF::Vocabulary::Term, RDF::Vocabulary].none? do |c|
        out.is_a? c
      end

      out
    end

    # Retrieve the current structure being used to govern how labels
    # are resolved against subjects.
    #
    # @return [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  the label structure
    #
    def label_spec
      @labels ||= process_labels LABELS
    end

    # Set a new structure for determining how labels are resolved.
    # This is a hash where the keys are RDF types, and the values are
    # hashes containing the keys `:label` and `:desc`, whose values
    # are arrays containing one or two values (for main and alternate;
    # if alternate is missing then main will be used instead), which
    # themselves are arrays containing one or more RDF properties, in
    # order of preference for the given class.
    #
    # @param spec [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  what a mouthful
    #
    # @return [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  the same label structure, but normalized
    #
    def label_spec= spec
      @labels = process_labels spec
    end

    # Get the objects for a given subject-predicate pair. Either of
    # `predicate` or `graph` can be array-able, in which case the
    # Cartesian product will be evaluated. If `entail` is
    # true (the default), the predicate(s) will be expanded out into
    # the full set of properties via `rdfs:subPropertyOf` and
    # `owl:equivalentProperty` relations, as well as `owl:inverseOf`
    # and `owl:SymmetricProperty` entailments.  Passing in a `graph`
    # will constrain the search to one or more named graphs, otherwise
    # all graphs are queried.
    #
    # @param subject [RDF::Resource] the subject
    # @param predicate [RDF::URI, Array<RDF::URI>] the predicate(s)
    # @param graph [RDF::Resource, Array<RDF::Resource>] the graph(s)
    # @param entail [true, false] whether to entail
    # @param only [Symbol, Array<Symbol>] limit to certain node specs
    # @param language [String, Symbol, Array<String, Symbol>]
    #  constrain literals to these languages
    # @param datatype [RDF::URI, Array<RDF::URI>] constrain literals
    #  to these datatypes
    # @param swap [false, true] noop for argument parity with #subject_for
    #
    # @yieldparam object [RDF::Resource, RDF::Literal] the object
    # @yieldparam rel [Set<RDF::URI>] predicates pointing toward this
    #  object from the subject
    # @yieldparam rev [Set<RDF::URI>] predicates pointing *out* of
    #  this object to the subject
    #
    # @return [Array] the resulting objects, or otherwise the
    #  aggregate results of the block.
    #
    def objects_for subject, predicate, graph: nil, entail: true,
        only: [], language: nil, datatype: nil, swap: false, &block
      # XXX you know what might be smart? fine-tuning the entailment
      # so it also does owl:sameAs on nodes (even the graph),
      only = coerce_node_spec only

      subject   = assert_resource  subject
      predicate = assert_resources predicate, blank: false, empty: true
      datatype  = assert_resources datatype,  blank: false
      graph     = assert_resources graph

      language = (language.respond_to?(:to_a) ? language.to_a : [language])

      # entail all the predicates
      predicate = property_set predicate if entail

      # do the reverse predicates once now instead of recomputing them
      # with every graph
      revp = invert_semantic predicate, entail: entail unless only == [:literal]

      # warn "wat #{revp} #{only}"

      # add a single nil graph for triple semantics
      graph << nil if graph.empty?

      # add a single nil predicate for wildcards
      predicate << nil if predicate.empty?

      # okay go
      out = graph.reduce({}) do |out, g|
        predicate.each do |p|
          query([subject, p, nil, g]).objects.each do |o|
            # ignore statement objects that don't match the spec
            next unless node_matches?(o, only)

            # filter out the literals
            if o.literal?
              # ignore statement objects that don't match the language
              next unless language.empty? or is_language? o, language
              # ignore statement objects that don't match the datatype
              next unless datatype.empty? or datatype.include? o.datatype
            end

            entry = out[o] ||= [Set[], Set[]]
            entry.first << p
          end
        end

        # now we do the reverse
        unless only == [:literal]
          revp.each do |p|
            query([nil, p, subject, g]).subjects.each do |s|
              next unless node_matches? s, only
              # note of course the subject is never going to be a
              # literal so we don't have to check for languages/datatypes
              entry = out[s] ||= [Set[], Set[]]
              entry.last << p
            end
          end
        end

        # pass out back out
        out
      end

      # *here* is where the block gets called, on node, preds out, preds in
      return out.map { |node, preds| block.call node, *preds } if block

      # otherwise we just return the accumulated objects
      out.keys
    end

    # Get the subjects for a given predicate-object pair. Behaves just
    # like #objects_for in many respects, without the provisions for
    # literals (as subjects never are). The named parameter, `:swap`,
    # will reorder the positional parameters from the order they
    # appear in RDF statements (`predicate`, `object`) to always
    # beginning with the node on the end of the statement (`object`,
    # `predicate`). This helps mitigate certain acrobatics needed in
    # contexts when both #subject_for and #object_for are called
    # together. Note as well that `:only` does not accept `:literal`
    # as a node spec, since subjects can never be literals.
    #
    # @param predicate [RDF::URI] the statement predicate(s) (or the
    #  object if `:swap` is true)
    # @param object [RDF::Resource, RDF::Literal] the object (or the
    #  predicates if `:swap` is true)
    # @param graph [RDF::Resource, Array<RDF::Resource>] the graph(s)
    # @param entail [true, false] whether to entail
    # @param only [Symbol, Array<Symbol>] limit to certain node specs
    # @param swap [false, true] swap positional parameters as described
    #
    # @yieldparam subject [RDF::Resource, RDF::Literal] the object
    # @yieldparam rel [Set<RDF::URI>] predicates pointing from this
    #  subject to the object
    # @yieldparam rev [Set<RDF::URI>] predicates pointing from the
    #  object to this subject
    #
    # @return [Array] the resulting subjects, or otherwise the
    #  aggregate results of the block.
    #
    def subjects_for predicate, object, graph: nil,
        entail: true, only: [], swap: false, &block
      # change the order of the positional parameters because this was
      # a bad idea to do it this way at the outset
      predicate, object = object, predicate if swap

      only = coerce_node_spec only, rev: true

      predicate = assert_resources predicate, blank: false, empty: true
      object    = assert_term      object
      graph     = assert_resources graph

      predicate = property_set predicate if entail

      # note that this is a slightly different regime than object_for
      revp = invert_semantic predicate, entail: entail unless object.literal?

      # add nil graph to the array for triple semantics
      graph << nil if graph.empty?

      predicate << nil if predicate.empty?

      # okay go
      out = graph.reduce({}) do |out, g|
        predicate.each do |p|
          query([nil, p, object, g]).subjects.each do |s|
            next unless node_matches? s, only
            # we don't need to do any fussy testing of literals in here
            entry = out[s] ||= [Set[], Set[]]
            entry.first << p
          end
        end

        # again our criterion for processing subjects is different
        unless object.literal?
          revp.each do |p|
            query([object, p, nil, g]).objects.each do |o|
              next unless node_matches? o, only
              entry = out[o] ||= [Set[], Set[]]
              entry.last << p
            end
          end
        end

        out
      end

      # process the block if we have one
      return out.map { |node, preds| block.call node, *preds } if block

      # otherwise just give back the subjects
      out.keys
    end

    # Obtain a key-value structure for the given subject, optionally
    # constraining the result by node type (:resource, :uri/:iri,
    # :blank/:bnode, :literal)
    #
    # @param subject [RDF::Resource] the subject of the inquiry
    # @apram graph [RDF::Resource, Array<RDF::Resource>] named graph(s)
    # @param rev [false, true] generate a struct from inbound links
    # @param only [Symbol, Array<Symbol>] one or more node types
    # @param inverses [false, true] whether to include
    #  inverse/symmetric properties
    #
    # @yieldparam node [RDF::Resource, RDF::Literal] a node to be manipulated
    # @yieldreturn [RDF::Resource, RDF::Literal] the transformed node
    #
    # @return [Hash{RDF::URI=>Array<RDF::Resource,RDF::Literal>] the struct
    #
    def struct_for subject, graph: nil, only: nil,
        rev: false, inverses: false, &block
      only    = coerce_node_spec only
      subject = assert_resource  subject
      graph   = assert_resources graph

      graph << nil if graph.empty?

      rsrc = {}
      pattern = rev ? [nil, nil, subject] : [subject, nil, nil]
      graph.each do |g|
        query(pattern, graph_name: g) do |stmt|
           node = rev ? stmt.subject : stmt.object
           next unless node_matches? node, only

           node = block.call node if block

           p = coerce_term stmt.predicate
           (rsrc[p] ||= []) << node
        end

        if inverses and only != [:literal]
          pattern = rev ? [subject, nil, nil] : [nil, nil, subject]
          query(pattern, graph_name: g) do |stmt|
            node = rev ? stmt.object : stmt.subject
            next unless node_matches? node, only

            node = block.call node if block

            invert_semantic(stmt.predicate).each do |inverse|
              (rsrc[inverse] ||= []) << node
            end
          end
        end
      end

      # make sure these are clean before shipping em out
      cmp = cmp_term
      rsrc.values.each { |v| v.sort!(&cmp).uniq! }
      rsrc
    end

    # Obtain all and only the `rdf:type`s directly asserted on the subject.
    #
    # @param subject [RDF::Resource]
    # @param type [RDF::Term, :to_a] override searching for type(s) and
    #  just return what is passed in (XXX why did i do this?)
    # @param struct [Hash] pull from an attribute-value hash rather than
    #  the graph
    #
    # @return [Array<RDF::URI>] the types asserted on the subject
    #
    def types_for subject, graph: nil, entail: false, struct: nil
      if struct
        assert_struct struct
        return (struct[RDF.type] || []).select(&:uri?)
      end

      objects_for subject, RDF.type, graph: graph, entail: entail, only: :uri
    end

    alias_method :asserted_types, :types_for

    # Obtain the most appropriate label(s) for the subject's type(s).
    # Returns one or more (depending on the `unique` flag)
    # predicate-object pairs in order of preference.
    #
    # @param subject [RDF::Resource, RDF::Literal] the subject (or the
    #  label itself)
    # @param unique [true, false] only return the first pair
    # @param type [RDF::Term, Array] supply asserted types if already
    #  retrieved
    # @param lang [nil, String, Symbol, Array<String, Symbol>] not
    #  currently implemented (will be conneg)
    # @param desc [false, true] retrieve description instead of label
    # @param alt  [false, true] retrieve alternate instead of main
    #
    # @return [Array<(RDF::URI, RDF::Literal)>, Array<Array<(RDF::URI,
    #  RDF::Literal)>>] either a predicate-object pair or an array of
    #  pairs.
    #
    def label_for subject, graph: nil, entail: true, unique: true,
        lang: nil, desc: false, alt: false, noop: false, struct: nil

      # a literal is its own label
      if subject.is_a? RDF::Literal
        # do this for return value parity
        return unique ? [nil, subject] : [[nil, subject]]
      end

      subject = assert_resource  subject
      graph   = assert_resources graph

      # get the asserted types
      asserted = types_for subject, graph: graph, struct: struct

      # get the full type stratum if we're entailing, otherwise fake
      # up a single layer for the loop below
      strata = entail ? type_strata(asserted) : [asserted]
      strata << [RDF::RDFS.Resource] unless
        strata.flatten.include? RDF::RDFS.Resource

      struct ||= struct_for subject, graph: graph, only: :literal
      seen  = Set[]
      accum = []

      strata.each do |types|
        types.each do |type|
          next unless preds = (label_spec.dig(
            type, desc ? :desc : :label) || [])[alt ? 1 : 0]
          preds.each do |p|
            next unless vals = struct[p]
            vals.each do |v|
              next unless v.literal?
              pair = [p, v]
              accum << pair unless seen.include? pair
              seen << pair
            end

            # XXX TODO sort vals
          end
        end
      end

      accum << [nil, subject] if noop and accum.empty?

      unique ? accum.first : accum.uniq
    end

    private

    AUTHOR  = [Intertwingler::Vocab::PAV.authoredBy, RDF::Vocab::DC.creator,
      RDF::Vocab::DC11.creator, RDF::Vocab::PROV.wasAttributedTo]
    CONTRIB = [Intertwingler::Vocab::PAV.contributedBy, RDF::Vocab::DC.contributor,
      RDF::Vocab::DC11.contributor]
    AUTHOR_LIST  = [RDF::Vocab::BIBO.authorList]
    CONTRIB_LIST = [RDF::Vocab::BIBO.contributorList]
    [AUTHOR, CONTRIB, AUTHOR_LIST, CONTRIB_LIST].each do |preds|
      i = 0
      loop do
        # note we are not using property_set or objects_for because we
        # *only* want equivalentProperty entailment, not subproperties
        equiv = preds[i].entail(:equivalentProperty) - preds
        preds.insert(i + 1, *equiv) unless equiv.empty?
        i += equiv.length + 1
        break if i >= preds.length
      end

      preds.freeze
    end

    public

    # Return an ordered list of authors (or contributors) for a given
    # subject. Tries `bibo:authorList` (or `bibo:contributorList`)
    # first before going on to `dct:creator` etc. Any unsorted authors
    # not listed in an explicit order are sorted by name (label).
    #
    # @param subject [RDF::Resource] the entity whose authors we are
    #  looking for
    # @param graph [nil, RDF::Resource] a named graph identifier
    # @param unique [false, true] whether to return only one value
    # @param contrib [false, true] whether to list contributors
    #  instead of authors
    #
    # @return [RDF::Term, Array<RDF::Term>] the author(s)
    #
    def authors_for subject, graph: nil, unique: false, contrib: false
      subject = assert_resource subject

      authors = []

      # try the author list;
      (contrib ? CONTRIB_LIST : AUTHOR_LIST).each do |pred|
        o = first_object([subject, pred, nil])
        next unless o
        # note this use of RDF::List is not particularly well-documented
        authors += RDF::List.new(subject: o, graph: self).to_a
      end

      # now try various permutations of the author/contributor predicate
      unsorted = (contrib ? CONTRIB : AUTHOR).reduce([]) do |u, pred|
        u + query([subject, pred, nil]).objects
      end

      # XXX maybe pass in some parameters to this??
      lcmp = cmp_label

      # sort unsorted according to labels and then append to any
      # explicitly sorted list
      authors += unsorted.uniq.sort(&lcmp)

      # note "unique" just means give me the first author; there may
      # be duplicates from authors being in both lists so we still `uniq`
      unique ? authors.first : authors.uniq
    end

    # Return the terminal replacements (as in, replacements that
    # themselves have not been replaced) for the given subject, if
    # any.
    #
    # @param subject [RDF::Resource] the entity whose replacements we
    #  are looking for
    # @param graph [nil, RDF::Resource] a named graph identifier
    # @param published [false, true] whether to constrain the search
    #  to published resources
    # @param published [true, false]
    # @param noop [false, true] whether to return unconditionally
    #
    # @return [Array<RDF::Resource>] the replacements, if any
    #
    def replacements_for subject, graph: nil, published: true, noop: false
      # XXX TODO this thing needs to be coded to handle fragments;
      # still not sure what to do about fragments

      subject = assert_resource subject
      graph   = assert_resources graph

      # `seen` is a hash mapping resources to publication status and
      # subsequent replacements. it collects all the resources in the
      # replacement chain in :fwd (replaces) and :rev (replaced-by)
      # members, along with a boolean :pub. `seen` also performs a
      # duty as cycle-breaking sentinel.

      seen  = {}
      queue = [subject]
      while (test = queue.shift)
        # fwd is "replaces", rev is "replaced by"
        entry = seen[test] ||= {
          pub: published?(test), fwd: Set[], rev: Set[] }
        queue += (
          subjects_for(RDF::Vocab::DC.replaces, subject, graph: graph) +
            objects_for(subject, RDF::Vocab::DC.isReplacedBy, graph: graph,
            only: :resource)).uniq.map do |r| # r = replacement
          next if seen.include? r
          # we preemptively create a structure
          seen[r] ||= { pub: published?(r), fwd: Set[], rev: Set[] }
          seen[r][:fwd] << test
          entry[:rev] << r
          r
        end.compact.uniq
      end

      # if we're calling from a published context, we return the
      # (topologically) last published resource(s), even if they are
      # replaced ultimately by unpublished resources.

      out = seen.map { |k, v| v[:rev].empty? ? k : nil }.compact - [subject]

      # now we modify `out` based on the publication status of the context
      if published
        pubout = out.select { |o| seen[o][:pub] }
        # if there is anything left after this, return it
        return pubout unless pubout.empty?
        # now we want to find the penultimate elements of `seen` that
        # are farthest along the replacement chain but whose status is
        # published

        # start with `out`, take the union of their :fwd members, then
        # take the subset of those which are published. if the result
        # is empty, repeat. (this is walking backwards through the
        # graph we just walked forwards through to construct `seen`)
        loop do
          # XXX THIS NEEDS A TEST CASE
          out = seen.values_at(*out).map { |v| v[:fwd] }.reduce(:+).to_a
          break if out.empty?
          pubout = out.select { |o| seen[o][:pub] }
          return pubout unless pubout.empty?
        end
      end

      out
    end

    # Return the dates associated with the subject.
    #
    # @param subject [RDF::Resource] the entity whose replacements we
    #  are looking for
    # @param graph [nil, RDF::Resource] a named graph identifier
    # @param predicate [RDF::URI, Array<RDF::URI>] the predicate(s) to check
    # @param datatype [RDF::URI, Array<RDF::URI>] the datatype(s) to check
    #
    # @return [Array<Date>] the date(time)s, if any
    #
    def dates_for subject, graph: nil, predicate: RDF::Vocab::DC.date,
        datatype: [RDF::XSD.dateTime, RDF::XSD.date]
      objects_for(subject, predicate, graph: graph,
        datatype: datatype, only: :literal) do |o|
        o.object
      end.select { |d| d.is_a? Date }.sort.uniq
    end

    # Return the dates associated with the subject.
    #
    # @param subject [RDF::Resource] the entity whose replacements we
    #  are looking for
    # @param graph [nil, RDF::Resource] a named graph identifier
    # @param predicate [RDF::URI, Array<RDF::URI>] the predicate(s) to check
    # @param datatype [RDF::URI, Array<RDF::URI>] the datatype(s) to check
    #
    # @return [Array<RDF::Literal>] the dates, if any
    #
    def formats_for subject, graph: nil, predicate: RDF::Vocab::DC.format,
        datatype: [RDF::XSD.token]
      objects_for(subject, predicate,
                  graph: graph, datatype: datatype, only: :literal) do |o|
        t = o.object.to_s.strip.downcase
        /\//.match?(t) ? MimeMagic.new(t) : nil
      end.compact.sort.uniq
    end

    # Return a Hash containing common values useful for ranking
    # subjects. Includes whether the
    #
    # @param subject [RDF::Resource] a subject node
    # @param graph [nil, RDF::Resource] an optional graph identifier
    # @param date [nil,Date,DateTime] a default date
    # @param ints [false, true] whether to represent booleans as integers
    #
    # @return [Hash] a data structure for ranking
    #
    def ranking_data_for subject, graph: nil, date: nil, ints: false
      date ||= DateTime.new

      out = {
       published:  published?(subject, graph: graph, circulated: false),
       circulated: published?(subject, graph: graph, circulated: true),
       replaced:   replaced?(subject, graph: graph),
       retired:    retired?(subject, graph: graph),
       ctime: dates_for(subject, graph: graph,
         predicate: RDF::Vocab::DC.created).last || date,
       mtime: dates_for(subject, graph: graph).last || date,
      }

      # convert to integers lol
      if ints
        # XXX H88888888 >:|
        bits = { false => 0, true => 1 }

        %i[published circulated replaced retired].each do |k|
          # love me some mutable data structures
          out[k] = bits[out[k]]
        end
      end

      out
    end

    private

    # the fragment spec determines the kinds of types which are
    # always considered fragments (rather than full documents)

    # fragment specs take the form { type => { predicate => reversed? } }
    FRAGMENTS = {
      RDF::RDFS.Resource => {
        RDF::Vocab::FOAF.isPrimaryTopicOf => false,
        RDF::Vocab::DC.hasPart            => true,
        RDF::Vocab::DC.isPartOf           => false,
      }.freeze,
      # we explicitly add this class because it overrides the fact
      # that foaf:Document etc are otherwise never fragments
      RDF::Vocab::BIBO.DocumentPart => {
        RDF::Vocab::DC.hasPart            => true,
        RDF::Vocab::DC.isPartOf           => false,
      }.freeze,
    }.freeze

    # these are all the types unambiguously considered to be "documents"
    DOCUMENTS = [RDF::Vocab::FOAF.Document].freeze

    def expand_documents docs
      docs = assert_resources docs, blank: false, empty: false, vocab: true
      type_strata docs, descend: true
    end

    def expand_fragments spec
      out = {}

      # create an initial queue of rdf types
      typeq = spec.keys
      i = 0
      while i < typeq.length do
        type = typeq[i]
        # we don't want to do this for base types; just pass them through
        if [RDF::RDFS.Resource, RDF::OWL.Thing].include? type
          out[type] = spec[type]
          next i += 1
        end
        # get all the equivalent and subtypes minus those explicitly defined
        tequiv = type_strata(type, descend: true) - typeq
        # splice these in to the queue
        typeq.insert(i + 1, *tequiv)

        # note it is not strictly necessary to splice at point because
        # these queues are kind of punned as to their function: both
        # as a queue and as a set of 'seen' terms. since the terms are
        # operated on all at once, the order doesn't matter and they
        # could just as easily be tacked onto the end and there should
        # be no substantive difference in the behaviour of this routine.

        # prepare the predicate spec
        ps = {}
        # now do a queue of predicates
        predq = spec[type].keys
        j = 0
        while j < predq.length do
          pred = predq[j]
          rev  = spec[type][pred]

          # get the equivalent properties
          pequiv = property_set(pred).to_a.sort - predq
          # do the same thing with the predicates
          predq.insert(j + 1, *pequiv)

          # bulk-assign spec with the asserted predicate first
          ([pred] + pequiv).each { |p| ps[p] = rev }

          # increment plus whatever was in the equivalents
          j += 1 + pequiv.length
        end

        # bulk assign type spec, same deal
        ([type] + tequiv).each { |t| out[t] = ps }

        # increment plus whatever was in the equivalents
        i += 1 + tequiv.length
      end

      # chuck out an expando'd copy of what was handed in
      out
    end

    public

    # Returns the spec by which document fragments are determined.
    # Takes the form of a hash where the keys are RDF types and the
    # values are also hashes where the keys are predicates, and the
    # values are flags as to whether the predicate is to be taken as
    # a reverse relation.
    #
    # @return [Hash{RDF::URI=>Hash{RDF::URI=>false,true}}]
    #
    def fragment_spec
      @fragments ||= expand_fragments FRAGMENTS
    end

    # Set a new fragment spec.
    #
    # @param spec [Hash{RDF::URI=>Hash{RDF::URI=>false,true}}] a
    #  fragment spec
    #
    # @return [Hash{RDF::URI=>Hash{RDF::URI=>false,true}}] an
    #  expando'd spec, with all equivalent classes and properties
    #  dereferenced.
    #
    def fragment_spec= spec
      @fragments = expand_fragments spec
    end

    private

    def host_for_internal subject, seen = Set[],
        dtypes = nil, graph: [], published: false, circulated: false
      # caching manoeuvre
      key = [subject.to_s, graph.sort, published]
      return hcache[key] if hcache.key? key

      # we begin by looking for an explicit designation
      host = objects_for(subject, Intertwingler::Vocab::CI['fragment-of'],
        graph: graph, only: :resource).first

      # get all the classes but the two basic ones that will net everything
      ft = fragment_spec.keys - [RDF::RDFS.Resource, RDF::OWL.Thing]
      dtypes ||= document_types(fragments: true) & all_types

      types = types_for subject, graph: graph
      isdoc = type_is? types, dtypes
      frags = type_is? types, ft

      # this condition is true if there is no explicit host document
      # and the subject itself is not a document type or explicit fragment type
      unless host or (isdoc and not frags)
        # attempt to find a list head (although not sure what we do if
        # there are multiple list heads)
        head = subjects_for(
          RDF.first, subject, graph: graph, only: :blank).sort.first
        head = list_head(head, graph: graph) if head
        # get an ordered set of [predicate, revp] pairs
        preds = fragment_spec.map do |type, pv|
          # this will give us
          score = type_is?(types, type) or next
          [score, pv.to_a]
        end.compact.sort do |a, b|
          # this will sort the mappings by score
          a.first <=> b.first
        end.map(&:last).flatten(1).uniq

        # accumulate candidate hosts and filter them
        tab = {}
        pab = {}
        pmap = {}
        hosts = preds.reduce([]) do |a, pair|
          pred, rev = pair
          px = pmap[pred] ||= property_set pred
          if rev
            # reverse relations include list heads
            a += subjects_for px, subject,
              graph: graph, entail: false, only: :resource
            a += subjects_for px, head,
              graph: graph, entail: false, only: :resource if head
          else
            a += objects_for subject, px, entail: false, only: :resource
          end
          a # <-- the accumulator
        end.uniq.reject do |h|
          ((tab[h] ||= types_for h) & dtypes).empty?
        end.sort do |a, b|
          # sort by publication status
          pa, pb = [a, b].map do |x|
            pab[x] ||= (published?(x, circulated: circulated) ? -1 : 0)
          end
          c = pa <=> pb
          # sort by priority in config (ish; ordering could be better)
          if c == 0
            # warn "#{tab[a]} <=> #{tab[b]}"
            pa, pb = [a, b].map do |x|
              tab[x].map { |y| dtypes.index(y) || Float::INFINITY }.min
            end
            c = pa <=> pb
          end
          # XXX TODO maybe sort by date? i unno
          # sort lexically if it's a tie
          c == 0 ? a <=> b : c
        end

        # the first one will be our baby
        if host = hosts.first and not seen.include? host
          parent = host_for_internal host, seen | Set[host], dtypes,
            graph: graph, published: published, circulated: circulated
          host = parent if parent
        end
      end

      hcache[key] = host
    end

    # XXX YO MAYBE REIN IN THE CACHES? lol

    # host document cache
    def hcache
      @hcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # term cache
    def tcache
      @tcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # type strata cache
    def tscache
      @tscache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # type strata descending cache
    def tdcache
      @tdcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # property set cache
    def pcache
      @pcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # inverseOf cache
    def icache
      @icache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # equivalents cache
    def eqcache
      @eqcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # subproperty/class cache
    def sbcache
      @sbcache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    # superproperty/class cache
    def sucache
      @sucache ||= Intertwingler::Util::LRU.new capacity: cache_limit
    end

    public

    def cache_limit
      @cache_limit ||= Float::INFINITY
    end

    def cache_limit= limit
      hcache.capacity  = limit
      tcache.capacity  = limit
      tscache.capacity = limit
      tdcache.capacity = limit
      pcache.capacity  = limit
      icache.capacity  = limit
      eqcache.capacity = limit
      sbcache.capacity = limit
      sucache.capacity = limit
      @cache_limit     = limit
    end

    def flush_cache
      hcache.clear
      tcache.clear
      tscache.clear
      tdcache.clear
      pcache.clear
      icache.clear
      eqcache.clear
      sbcache.clear
      sucache.clear
      nil
    end

    # Retrieve a host document for a suspected document fragment, if
    # it exists; null otherwise (or itself if `:noop` is true).
    #
    # @param subject [RDF::Resource] the subject we think may be a fragment
    # @param graph [RDF::Resource, Array<RDF::Resource>] filter search
    #  by optional named graph(s)
    # @param published [false, true, :circulated] only consider
    #  published (or circulated) documents
    # @param noop [false, true] return the subject if there is no host
    #
    # @return [nil, RDF::Resource] the host document, if any
    #
    def host_for subject, graph: nil, published: true, noop: false
      subject = assert_resource  subject
      graph   = assert_resources graph

      # smuggle in 'circulated' flag
      circulated = case published when :circulated then published = true
                   else false
                   end

      host = host_for_internal subject, graph: graph,
        published: published, circulated: circulated

      # return the noop
      noop ? host || subject : host
    end

    # Return true if the subject is a fragment of another document.
    #
    # @param subject [RDF::Resource] the subject we think may be a fragment
    # @param graph [RDF::Resource, Array<RDF::Resource>] filter search
    #  by optional named graph(s)
    # @param published [false, true, :circulated] only consider
    #  published (or circulated) documents
    #
    # @return [false, true] whether the resource is a fragment
    #
    def fragment? subject, graph: nil, published: true
      !!host_for(subject, graph: graph, published: published)
    end

    # Retrieve the RDF types considered to be "documents".
    #
    # @return [Array<RDF::URI>] all recognized document types
    #
    def document_types fragments: false
      @documents ||= expand_documents DOCUMENTS
      fragments ? @documents : @documents - fragment_spec.keys
    end

    # Set the RDF types that are recognized as "documents".
    #
    # @param types [RDF::URI, Array<RDF::URI>] said types
    #
    # @return [Array<RDF::URI>] all recognized document types
    #
    def document_types= types
      @documents = expand_documents types
    end

    # Return all subjects in the graph that conform to the configured
    # notion of a "document".
    #
    # @param internal [true, false] whether to include internal documents
    # @param external [true, false] whether to include external documents
    # @param published [true, false] whether to limit to published documents
    # @param fragments [false, true] whether to include document fragments
    # @param exclude [Array<RDF::Resource>] RDF types to exclude from selection
    #
    # @return [Array] the documents
    #
    def all_documents internal: true, external: true,
        published: false, fragments: false, exclude: []
      docs = all_of_type document_types, exclude: exclude
      docs.reject! do |d|
        d.to_s.downcase.start_with? 'urn:uuid:'
      end unless internal
      docs.reject! do |d|
        not d.to_s.downcase.start_with? 'urn:uuid:'
      end unless external
      docs.select! { |d| published? d } if published
      docs.reject! { |d| fragment?(d) } unless fragments
      docs
    end

    # Determine whether a resource is considered to be indexed, which
    # all resources implicitly are. A statement `?s ci:indexed false`
    # indicates otherwise.
    #
    # @param subject [RDF::Resource] the subject to inspect
    # @param graph [RDF::Resource, Array<RDF::Resource>] named
    #  graph(s), if any
    # @param explicit [false, true] whether to stipulate that
    #  resources must be explicitly indexed
    # @param fragments [true, false] whether to resolve host documents
    #  of document fragments
    #
    # @return [false, true] whether the subject is indexed
    #
    def indexed? subject, graph: nil, explicit: false, fragments: true
      subject = assert_resource  subject
      graph   = assert_resources graph

      # test the host document if fragments are allowed
      subject = host_for subject, graph: graph, published: false,
        noop: true if fragments

      # get the value of ci:indexed
      ix = objects_for(subject, Intertwingler::Vocab::CI.indexed, graph: graph,
        only: :literal, datatype: RDF::XSD.boolean).first

      # if there was a value then return it, otherwise it's false if
      # explicit or true if implicit
      ix ? ix.object : explicit ? false : true
    end

    # Determine whether the subject is "published", which canonically
    # translates to whether querying `?subject bibo:status
    # bs:published .` returns something. If `circulated` is true, it
    # will also consider `ci:circulated` as "published". If `retired`
    # is false (the default), the presence of `ci:retired` will
    # short-circuit the other tests and return false. When `retired`
    # is true, the presence of `ci:retired` is ignored. If `indexed`
    # is true, the presence of `?subject ci:indexed false .` will
    # cause this to return false.
    #
    # @param subject [RDF::Resource] the subject to inspect
    # @param graph [RDF::Resource, Array<RDF::Resource>] named
    #  graph(s), if any
    # @param circulated [false, true] whether to consider
    #  `ci:circulated` as well as `bs:published`
    # @param retired [false, true] whether to _include_ resources that
    #  are `ci:retired`
    # @param indexed [false, true] whether to _omit_ resources that
    #  are `ci:indexed false`
    #
    # @return [true, false] whether the subject is published
    #
    def published? subject, graph: nil, circulated: false, retired: false,
        indexed: false

      # if this is a fragment then we test the host document. note
      # that we are not filtering published documents here because we
      # want to test whatever comes out of this
      if host = host_for(subject, graph: graph, published: false)
        return published? host, graph: graph,
          circulated: circulated, retired: retired, indexed: indexed
      end

      #
      if indexed
        ix = objects_for(subject, Intertwingler::Vocab::CI.indexed, graph: graph,
          only: :literal, datatype: RDF::XSD.boolean).first
        return false if ix and ix.object == false
      end

      # obtain the statuses for the given subject
      candidates = objects_for(
        subject, RDF::Vocab::BIBO.status, only: :resource).to_set

      # bail out if the subject has been retired
      return false if !retired and candidates.include? Intertwingler::Vocab::CI.retired

      # set up a test set of statuses
      test = Set[RDF::Vocab::BIBO['status/published']]
      test << Intertwingler::Vocab::CI.circulated if circulated

      # if this isn't empty then we're "published"
      !(candidates & test).empty?
    end

    # Returns whether the subject has a `bibo:status` of `ci:retired`.
    #
    # @param subject [RDF::Resource] the subject to inspect
    # @param graph [RDF::Resource, Array<RDF::Resource>] named
    #  graph(s), if any
    #
    # @return [false, true] whether the subject is retired
    #
    def retired? subject, graph: nil
      objects_for(subject, RDF::Vocab::BIBO.status,
        graph: graph, only: :resource).include?(Intertwingler::Vocab::CI.retired)
    end

    # Returns whether the subject is dct:isReplacedBy some other node.
    #
    # @param subject [RDF::Resource] the subject to inspect
    # @param graph [RDF::Resource, Array<RDF::Resource>] named
    #  graph(s), if any
    # @param published [false, true] replacements must be published
    #
    # @return [false, true] whether the subject is retired
    #
    def replaced? subject, graph: nil, published: false
      # get proximate replacement minus self
      candidates = subjects_for(RDF::Vocab::DC.replaces, subject,
        graph: graph, only: :resource) - [subject]

      # remove the unpublished ones
      candidates.select! { |c| published? c } if published

      # if empty then false
      !candidates.empty?
    end

    # Obtain the head of a list for a given list subject. Note that
    # any named graph passed in must be a _singular_ graph, since
    # while blank node lists that span multiple named graphs are
    # _possible_, it isn't clear how to handle them.
    #
    # @param subject [RDF::Node] a node in the list
    # @param graph [nil, RDF::URI] an optional named graph
    #
    # @return [RDF::Node, nil] the head node or nothing
    #
    def list_head subject, graph: nil
      subject = assert_resource subject
      graph   = assert_resource graph

      # the current spot in the list
      nodes = [subject]

      # append the list nodes to the array until we run out
      while tmp = query(
        [nil, RDF.rest, nodes.last, graph]).subjects.select(&:node?).sort.first
        nodes << tmp
      end

      # the last one is the head of the list
      nodes.last
    end

    # Determine whether a subject is a given `rdf:type`.
    #
    # @param subject [RDF::Resource] the resource to test
    # @param type [RDF::Resource, Array<RDF::Resource>] the type(s) to
    #  test the subject against
    # @param struct [Hash{RDF::URI=>Array<RDF::Value>}] an optional
    #  predicate-object structure of cached values
    #
    # @return [true, false] whether or not the subject is of the type(s)
    #
    def rdf_type? subject, type, struct: nil, graph: nil
      asserted = types_for subject, graph: graph, struct: struct
      !!type_is?(asserted, type)
    end

    # Return all RDF types (that is, all `?t` for `?s rdf:type ?t`)
    # present in the graph.
    #
    # @param graph [RDF::URI, Array<RDF::URI>] constrain search to
    #  named graph(s)
    #
    # @return [Array<RDF::Resource>] the types
    #
    def all_types graph: nil
      graph = assert_resources graph
      graph << nil if graph.empty?
      graph.map { |g| query([nil, RDF.type, nil, g]).objects }.flatten.uniq
    end

    # Return all subjects in the graph of (a) given type(s). Takes an
    # optional block for access to the exact type and graph. Returns
    # an array of subjects, or an array of whatever the block returns.
    #
    # @param rdftype [RDF::URI, Array<RDF::URI>] the type(s) to check
    # @param graph [nil, RDF::URI, Array<RDF::URI>] named graph(s) to
    #  search, or nil for everything
    # @param exclude [RDF::URI, Array<RDF::URI>] explicit RDF types to
    #  exclude
    #
    # @yieldparam subject [RDF::Resource] the subject
    # @yieldparam type [RDF::Resource] the asserted type of the subject
    # @yieldparam graph [nil, RDF::URI] the graph where the statement
    #  was found
    #
    # @return [Array<RDF::Resource>] the subjects of the given type(s)
    #
    def all_of_type rdftype, graph: nil, exclude: [], &block
      rdftype = assert_resources rdftype
      graph   = assert_resources graph
      exclude = assert_resources exclude
      # get all the RDF types in the graph(s)
      out = []

      # if this is an empty array we get nothing back
      graph << nil if graph.empty?

      all_types(graph: graph).each do |t|
        next unless type_is? t, rdftype
        next if !exclude.empty? and type_is? t, exclude
        out += graph.map do |g|
          query([nil, RDF.type, t, g]).subjects.map do |s|
            block ? block.call(s, t, g) : s
          end
        end.flatten(1)
        # only flatten the first layer, the second is concatenated
      end

      out.sort.uniq
    end

    # Obtain equivalents for a given term, which can either be a class
    # or a property. This method wraps a cache around `term.entail`,
    # which is slow.
    #
    # @param term [RDF::Vocabulary::Term] input term
    #
    # @return [Array<RDF::Vocabulary::Term>] the equivalent terms
    #
    def equivs_for term
      # completely short-circuit
      return eqcache[term.to_s] if eqcache.key? term.to_s

      # first off we do the cached resolution
      term = coerce_term(term, uri: true) or return []

      entailment = case
                   when !term.is_a?(RDF::Vocabulary::Term)
                     nil
                   when term.property? then :equivalentProperty
                   when term.class? then :equivalentClass
                   else
                     nil
                   end

      # note we want the deep entailment for this one
      equivs = [term]
      equivs += term.entail(entailment).map do |t|
        coerce_term t, uri: true
      end.compact.uniq if entailment

      # return the equivalents
      equivs.each { |e| eqcache[e.to_s] = equivs }
    end

    # Obtain (shallow) subclasses/properties for a given term, which
    # can either be a class or a property. May be empty. Also cached.
    #
    # @param term [RDF::Vocabulary::Term] input term
    #
    # @return [Array<RDF::Vocabulary::Term>] the subordinate terms
    #
    def subs_for term
      # completely short-circuit
      return sbcache[term.to_s] if sbcache.key? term.to_s

      term = coerce_term(term, uri: true) or return []

      entailment = case
                   when !term.is_a?(RDF::Vocabulary::Term)
                     nil
                   when term.property? then :subProperty
                   when term.class? then :subClass
                   else
                     nil
                   end

      # get the equivalents
      equivs = equivs_for term

      # now get the adjacents
      terms = if entailment
                equivs.map do |equiv|
                  if equiv.is_a? RDF::Vocabulary::Term
                    # warn "#{equiv} => #{equiv.respond_to?(entailment)}"
                    equiv.send(entailment).map { |t| coerce_term t, uri: true }
                  end
                end.flatten.compact.uniq
              else
                []
              end

      # add to the cache
      equivs.each { |t| sbcache[t.to_s] = terms }

      terms
    end

    # Obtain (shallow) superclasses/properties for a given term, which
    # can either be a class or a property. May be empty. Also cached.
    #
    # @param term [RDF::Vocabulary::Term] input term
    #
    # @return [Array<RDF::Vocabulary::Term>] the superordinate terms
    #
    def supers_for term
      # completely short-circuit
      return sucache[term.to_s] if sucache.key? term.to_s

      term = coerce_term(term, uri: true) or return []

      entailment = case
                   when !term.is_a?(RDF::Vocabulary::Term)
                     nil
                   when term.property? then :subPropertyOf
                   when term.class? then :subClassOf
                   else
                     nil
                   end

      # get the equivalents
      equivs = equivs_for term

      # now get the adjacents
      terms = if entailment
                equivs.map do |equiv|
                  if equiv.is_a? RDF::Vocabulary::Term
                    equiv.send(entailment).map { |t| coerce_term t, uri: true }
                  end
                end.flatten.compact.uniq
              else
                []
              end

      # add to the cache
      equivs.each { |t| sbcache[t.to_s] = terms }

      terms
    end

    # Obtain a stack of RDF types for an asserted initial type or set
    # thereof. Returns an array of arrays, where the first is the
    # asserted types and their inferred equivalents, and subsequent
    # elements are immediate superclasses and their equivalents. A given
    # URI will only appear once in the entire structure. When `descend`
    # is set, the resulting array will be flat.
    #
    # @param rdftype [RDF::Term, :to_a] the type(s) to inspect
    # @param descend [false, true] descend instead of ascend
    #
    # @return [Array<Array<RDF::URI>>] the type stratum
    #
    def type_strata rdftype, descend: false
      rdftype = assert_resources rdftype, vocab: true

      return [] if rdftype.empty?

      qmeth  = descend ? :subs_for : :supers_for # inheritance direction
      strata = []
      queue  = [rdftype]

      while terms = queue.shift
        # do equivalent classes
        terms = terms.map { |t| equivs_for t }.flatten.uniq

        # only add to the strata if there is a there there
        seen  = strata.flatten
        layer = terms.reject { |t| seen.include? t }
        strata << layer unless layer.empty?

        # now do hierarchically adjacent terms
        hier  = terms.map { |t| send qmeth, t }.flatten.uniq
        queue << hier unless hier.empty?
      end

      descend ? strata.flatten : strata
    end

    # Obtain everything that is an `owl:equivalentClass` or
    # `rdfs:subClassOf` the given type. Equivalent to running
    # #type_strata with `descend: true`.
    #
    # @param rdftype [RDF::URI, Array<RDF::URI>] the initial type(s)
    #
    # @return [Array<RDF::URI>] the related types
    #
    def all_related rdftype
      type_strata rdftype, descend: true
    end

    # Determine whether one or more `rdf:Class` entities is transitively
    # an `rdfs:subClassOf` or `owl:equivalentClass` of one or more
    # reference types. Returns the subclass "distance" of the "nearest"
    # reference type from the given type(s), or nil if none match.
    #
    # @param type [RDF::Resource, #to_a] the type(s) we are interested in
    # @param reftype [RDF::Resource, #to_a] the reference type(s) to
    #   check against
    #
    # @return [nil, Integer]
    #
    def type_is? type, reftype
      # coerce reftype to an array if it isn't already
      reftype = assert_resources reftype, blank: false, vocab: true
      return if reftype.empty?

      # generate types, including optionally base classes if they aren't
      # already present in the strata (this will be automatically
      types = type_strata type
      bases = [RDF::RDFS.Resource, RDF::OWL.Thing,
               RDF::Vocab::SCHEMA.Thing] - types.flatten
      # put the base classes in last if there are any left after subtracting
      types << bases unless bases.empty?


      # this will return the "distance" of the matching type from what
      # was asserted, starting at 0 (which in ruby is true!) or false
      types.find_index { |ts| !(ts & reftype).empty? }
    end

    # Determine whether a property is symmetric.
    #
    # @param property [RDF::URI] the property to be tested
    #
    # @return [true, false] whether or not the property is symmetric
    #
    def symmetric? property
      property = assert_resource property, vocab: true rescue return false

      if property.uri? and property.respond_to?(:type)
        type = type_strata(property.type).flatten
        return type.include? RDF::OWL.SymmetricProperty
      end

      false
    end

    # Obtain all the properties that are equivalent to or
    # subproperties of the given propert(y|ies).
    #
    # @param properties [RDF::URI, Array<RDF::URI>] said propert(y|ies)
    #
    # @return [Set<RDF::URI>] said equivalent/subproperties
    #
    def property_set properties
      properties = assert_resources(
        properties, blank: false, vocab: true).to_set

      return properties if properties.empty?

      # if there are cache entries for all the properties in the set,
      # union them together and return it
      properties.map do |p|
        if pcache[p]
          pcache[p]
        else
          # get all the equivalents, again note we can't trust this output
          # completely so we filter for URIs
          ep = Set[p] | p.entail(:equivalentProperty).select(&:uri?).to_set
          ep.map do |e|
            # get the subproperties of each
            sp = e.respond_to?(:entail) ?
              e.entail(:subProperty).flatten.to_set : Set[]
            pe = pcache[e] || Set[]
            pe |= (ep | sp)
            pcache[e] = pe
            # warn pcache[e]
            pe
          end.reduce :|
        end
      end.reduce :|
    end

    # as i will invariably trip over this
    alias_method :predicate_set, :property_set

    private

    # we want https to come before http; one could imagine doing this
    # for all sorts of other applicable protocol schemes
    SCHEME_RANK = { https: 0, http: 1 }

    public

    # Generate a closure that can be passed into {Enumerable#sort} for
    # sorting URIs that would be found in the wild.
    #
    # @param reverse [false, true] whether to reverse the sort
    # @param www [nil, true, false] whether the subdomain 'www' should
    #  always be prioritized (true) when comparing HTTP(S) URLs, or
    #  always deprioritized (false), or compared normally (nil).
    # @param prioritize [URI, RDF::URI, Array<URI, RDF::URI>] URIs
    #  that are relative to these will be put in front of other URIs.
    # @param blankfirst [false, true] whether blank nodes should
    #  always go before proper URIs, or after them.
    # @yieldparam comparand [Object] the comparand to be transformed
    # @yieldreturn cleaned [Object] the transformed comparand
    #
    # @return [Proc] the comparison function
    #
    def cmp_resource reverse: false, www: nil, prioritize: [],
        blankfirst: false, &block
      # index tables for 'www' and blank-first preferences
      wpref = [false, true].zip(www ? [1, -1] : [-1, 1]).to_h
      bpref = [false, true].zip(blankfirst ? [1, 0] : [0, 1]).to_h
      cache = {}

      # this will create a hash (which of course in ruby are ordered)
      # where the keys are arrays of inverted hostnames, with any
      # leading 'www' removed (if the related parameter is non-nil)
      # and the values are arrays of matching URIs, again preserving
      # the order.
      prioritize = coerce_resources(prioritize, as: :uri).reduce({}) do |a, u|
        u = u.normalize
        h = u.host.to_s.split(?.).reverse

        # we don't care about matching www with these
        h.pop if h.last == 'www' and not www.nil?
        # make sure the URI's host agrees with this
        u.host = h.reverse.join ?.
        if u.fragment
          u.fragment = ''
        elsif u.query
          u.query = ''
        else
          # clip off any terminating path segment that isn't a /
          u.path = /^(.*\/)[^\/]*$/.match(u.path).captures.first
        end

        # conditionally initialize *and* conditionally add the URI
        entry = a[h] ||= []
        entry << u unless entry.include? u
        a # the initial hash
      end

      # this closure returns a priority score for a host/URI; i broke
      # it out because the main cmp lambda was getting too insane
      priscore = lambda do |h, u|
        return prioritize.size unless pri = prioritize[h]
        # knowing that we match a host in the priority list, we need
        # to find the URL in the list that "best matches" the
        # comparand. its index will be added as a decimal point to the
        # containing index, so the form will be major.minor. match
        # grades can be characterized thusly:
        #
        # * exact match (path, query, fragment)
        # * match path and query, different fragment
        # * match path, different query
        # * partial match on path
        #
        # more path segments matched are better than fewer, but a
        # `../` anywhere is disqualifying

        # XXX TODO the actual URI comparison is going to be a tricky
        # computation because we want the *lowest* index in `pri` that
        # matches the *most* of the URI (ie so a higher index will be
        # preferred over a lower one if it accounts for more path
        # segments/query/fragment/whatever) and i don't feel like
        # writing that logic right now.

        # anyway we'll just return the host's index for now
        prioritize.keys.index h
      end

      # return a closure that can be passed into Enumerable#sort
      lambda do |a, b|
        raise 'Comparands must be instances of RDF::Resource' unless
          [a, b].all? { |x| x.is_a? RDF::Resource }

        # let's not forget to reverse
        a, b = b, a if reverse

        # queue up the comparisons; lol gotta love ruby for being able
        # to do bastard stuff like this
        a, b = [a, b].map do |x|
          # this will return the cache entry or otherwise create it
          cache[x] ||= begin
                         # preprocess if necessary
                         x = block.call x if block

                         o = { id: x }
                         if x.uri?
                           # get version as URI object
                           o[:uri]  = u = URI(x.to_s).normalize
                           o[:rank] = bpref[false]
                           # get scheme and rank; the fallback number
                           # only needs to be bigger than matches
                           o[:scheme] = s = x.scheme.downcase.to_s.to_sym
                           o[:srank]  = SCHEME_RANK.fetch s, SCHEME_RANK.size

                           # get host into an ideally comparable form
                           # (note this will not work for IPs)
                           o[:host] = h = u.host.to_s.split(?.).reverse
                           o[:www]  = 0 # this needs a default
                           unless www.nil?
                             # wpref has already been set in the
                             # preamble; h.pop will only be evaluated
                             # if the test is true; the double bang
                             # coerces the expression to a boolean
                             # which are the keys in wpref (and 'www'
                             # will be coerced to true)
                             o[:www] = wpref[!!(h.last == 'www' && h.pop)]
                           end

                           # now we see if the host is on our priority
                           # list (or get a default high value otherwise)
                           o[:hrank] = priscore.call h, u
                         elsif o.blank?
                           o[:rank] = bpref[true]
                         else
                            # this just needs to be higher than 0 or
                            # 1, but honestly this shouldn't get run
                           o[:rank] = 2
                         end
                         o # the new compound comparand
                       end
        end

        # that's the setup, now for the comparison

        # warn [a, b].inspect

        # this will put all URIs either before or after all blanks
        c = a[:rank] <=> b[:rank]
        return c if c != 0

        # if one is a uri here then they are both uris
        if a[:id].uri?
          c = a[:srank]  <=> b[:srank] # pull out the scheme rank
          c = a[:scheme] <=> b[:scheme] if c == 0

          return c if c != 0

          # if one is hierarchical they are both hierarchical
          if a[:id].hier?
            # test priority rank
            c = a[:hrank] <=> b[:hrank]

            # compare (reversed and segmented) hosts
            c = a[:host] <=> b[:host] if c == 0

            # if the hosts are the same, test the www policy
            c = a[:www] <=> b[:www] if c == 0

            # XXX upgrade this for finer-grained control
            c = a[:uri] <=> b[:uri] if c == 0
          end
        end

        # XXX c.to_i because the <=> here can return nil which is a nono
        c.to_i == 0 ? a[:id].to_s <=> b[:id].to_s : c.to_i
      end
    end

    # Generate a closure that can be passed into {Enumerable#sort} for
    # sorting literals according to the given policy.
    #
    # @param reverse [false, true] whether to reverse the sort
    # @param datatype [RDF::URI, Array<RDF::URI>] preference for datatype(s)
    # @param language [String, Symbol, Array<String, Symbol>]
    #  `Accept-Language`/RFC5456 language tag(s)
    # @param nocase [true, false] whether to sort case-sensitive
    # @param longer [false, true] whether to sort longer strings
    #  before shorter strings where the longer string begins with the
    #  shorter string
    # @yieldparam comparand [Object] the comparand to be transformed
    # @yieldreturn cleaned [Object] the transformed comparand
    #
    # @return [Proc] the comparison function
    #
    def cmp_literal reverse: false, datatype: nil, language: nil,
        nocase: true, longer: false, longer_raw: nil,
        resources_first: false, &block
      datatype = assert_resources datatype, blank: false
      language = coerce_languages language

      # map the result of literal? to something we can compare
      cmp_rsrc = [false, true].zip(resources_first ? [0, 1] : [1, 0]).to_h

      lambda do |a, b|
        # step zero: preprocess comparands
        a, b = [a, b].map(&block) if block
        # first flip if reverse
        a, b = b, a if reverse
        # then detect if both are literal
        c = cmp_rsrc[a.literal?] <=> cmp_rsrc[b.literal?]

        return c if c != 0

        # at this point these are both literal so only need to test one
        if a.literal?
          # then detect if both are the same type
          unless datatype.empty?
            ad = datatype.index(a.datatype) || Float::INFINITY
            bd = datatype.index(b.datatype) || Float::INFINITY

            c = ad <=> bd

            return c if c != 0
          end

          # then detect if both are the same language
          if [a, b].any?(&:language?) and !language.empty?
            al, bl = [a, b].map do |x|
              x = x.language.to_s.downcase.tr_s(?_, ?-)
              language.index(x) || Float::INFINITY
            end

            c = al <=> bl

            return c if c != 0
          end

          # unconditionally return the longer string
          unless longer_raw.nil?
            al, bl = a.value.strip.length, b.value.strip.length
            al, bl = bl, al unless longer_raw?
            c = al <=> bl
            return c if c != 0
          end

          # at this point we're comparing values
          a, b = [a, b].map do |x|
            x = x.value
            # optionally squash to lower case
            nocase ? x.downcase : x
          end

          # then detect if one string starts with the other, and if it
          # does, what to do with it
          len = [a, b].map(&:length).min
          if a[0, len] == b[0, len]
            c = (a.length <=> b.length) * (longer ? -1 : 1)

            return c if c != 0
          end
        end

        # then finally do a lexical comparison
        a.to_s <=> b.to_s
      end
    end

    # Generate a closure that can be passed into {Enumerable#sort} for
    # sorting the labels of subjects according to the given policy.
    # This is equivalent to calling #label_for on all the subjects and
    # then comparing those according to #literal_sort, but caches the
    # labels for speed.
    #
    # @param reverse [false, true] whether to reverse the sort
    # @param datatype [RDF::URI, Array<RDF::URI>] preference for datatype(s)
    # @param language [String, Symbol, Array<String, Symbol>]
    #  preference for language(s)
    # @param nocase [true, false] whether to sort case-sensitive
    # @param longer [false, true] whether to sort longer strings
    #  before shorter strings where the longer string begins with the
    #  shorter string
    # @yieldparam comparand [Object] the comparand to be transformed
    # @yieldreturn cleaned [Object] the transformed comparand
    #
    # @return [Proc] the comparison function
    #
    def cmp_label reverse: false, cache: nil, datatype: nil, language: nil,
        nocase: false, longer: false, desc: false, alt: false, &block

      # obtain label cmp
      cmp = cmp_literal reverse: reverse, datatype: datatype,
        language: language, nocase: nocase, longer: longer

      cache ||= {} # structs
      lcache = {} # separate label cache

      # XXX 2022-12-01 ADD RANKING FOR PREDICATES TOO: maybe do some
      # kinda scoring thing? language tag, predicate, string length,
      # lexical...

      lambda do |a, b|
        # optional preprocess
        a, b = [a, b].map(&block) if block

        # obtain and cache the labels
        [a, b].each do |x|
          lcache[x] ||= (label_for(x, struct: cache[x]) || [nil, x]).last
        end

        # now run the label cmp
        cmp.(lcache[a], lcache[b])
      end
    end

    # Generate a closure that can be passed into {Enumerable#sort} for
    # comparing an arbitrary pair of RDF terms. This effectively wraps
    # and coalesces the behaviour of #cmp_label and #cmp_resource.
    #
    # @param reverse [false, true] whether to reverse the sort
    # @param labels [true, false]
    # @param datatype [RDF::URI, Array<RDF::URI>] preference for datatype(s)
    # @param language [String, Symbol, Array<String, Symbol>]
    #  preference for language(s)
    # @param nocase [true, false] whether to sort case-sensitive
    # @param longer [false, true] whether to sort longer strings
    #  before shorter strings where the longer string begins with the
    #  shorter string
    # @yieldparam comparand [Object] the comparand to be transformed
    # @yieldreturn cleaned [Object] the transformed comparand
    #
    # @return [Proc] the comparison function
    #
    def cmp_term reverse: false, labels: true, cache: nil, datatype: nil,
        language: nil, nocase: false, longer: false, desc: false, alt: false,
        www: false, prioritize: [], order: [:uri, :blank, :literal], &block

      # deal with node order
      order = coerce_node_spec order
      ospec = NTESTS.keys.map do |s|
        [s, order.index(s) || NTESTS.keys.count]
      end.to_h

      labcmp = cmp_label cache: cache, datatype: datatype, language: language,
        nocase: nocase, longer: longer, desc: desc, alt: alt, &block

      uricmp = cmp_resource www: www, prioritize: prioritize,
        blankfirst: ospec[:blank] < ospec[:uri], &block

      lambda do |a, b|
        # note we do the reverse here so the whole comparison is reversed
        a, b = b, a if reverse

        c = labcmp.(a, b)
        # warn "#{a.inspect} <=> #{b.inspect} == #{c}"
        c == 0 && [a, b].all?(&:iri?) ? uricmp.(a, b) : c
      end
    end

    # Invert a given struct so that `{ predicate => Set[object] }`
    # becomes `{ object => Set[predicate] }`. Optionally run a block in
    # the inner loop. If the block returns an `Array`, the first two
    # values will be assigned to the predicate and object in the
    # returned inverted struct. Return an explicit `nil` in the block to
    #
    # @param struct [Hash{RDF::Resource => Set}] a structure containing
    #  the predicates and objects for a given subject.
    # @yieldparam predicate [RDF::Resource] the predicate of the statement
    # @yieldparam object    [RDF::Value] the object of the statement
    # @yieldreturn          [nil, Array] an optional predicate-object pair.
    #
    # @return [Hash] the inverted struct.
    #
    def invert_struct struct, &block
      nodes = {}

      struct.each do |p, v|
        v.each do |o|
          # copy the predicate so we don't overwrite it
          pi = p

          if block
            tmp = block.call pi, o
            # assign block return if it has one
            pi, o = *tmp if tmp.is_a? Array
          end

          # now assign to output
          nodes[o] ||= Set.new
          nodes[o] << pi
        end
      end

      nodes
    end

    # Find a subset of a struct for a given set of predicates,
    # optionally inverting to give the objects as keys and predicates as
    # values.
    #
    # @param struct [Hash]
    # @param properties [RDF::URI, #to_a] the properties to find
    # @param entail [false, true] whether to entail the predicate(s)
    # @param invert [false, true] whether to invert the resulting hash
    #
    # @return [Hash] the selected subset (which could be empty)
    #
    def find_in_struct struct, properties, entail: false, invert: false
      properties = coerce_resources properties
      properties = property_set properties if entail

      struct = struct.select { |p, _| properties.include? p }

      invert ? invert_struct(struct) : struct
    end

    # Given a structure of the form +{ predicate => [objects] }+,
    # rearrange the structure into one more amenable to rendering
    # RDFa. Returns a hash of the form +{ resources: { r1 => Set[p1, pn]
    # }, literals: { l1 => Set[p2, pm] }, types: Set[t1, tn], datatypes:
    # Set[d1, dn] }+. This inverted structure can then be conveniently
    # traversed to generate the RDFa. An optional block lets us examine
    # the predicate-object pairs as they go by.
    #
    # @param struct [Hash] The struct of the designated form
    # @yield [p, o] An optional block is given the predicate-object pair
    # @return [Hash] The inverted structure, as described.
    #
    def prepare_collation struct, &block
      resources = {}
      literals  = {}
      datatypes = Set.new
      types     = Set.new

      struct.each do |p, v|
        v.each do |o|
          block.call p, o if block

          if o.literal?
            literals[o] ||= Set.new
            literals[o].add p
            # collect the datatype
            datatypes.add o.datatype if o.has_datatype?
          else
            if  p == RDF::RDFV.type
              # separate the type
              types.add o
            else
              # collect the resource
              resources[o] ||= Set.new
              resources[o].add p
            end
          end
        end
      end

      { resources: resources, literals: literals,
       datatypes: datatypes, types: types }
    end

    # Instantiate a Resource on the given term. It is up to you to
    # ensure that `term` is actually in the graph.
    #
    # @param term [RDF::Resource] the term in question
    # @param resolver [nil, Intertwingler::Resolver] an optional `Resolver`
    #  instance
    #
    # @return [Intertwingler::Resource] a `Resource` bound to the term
    #
    def resource term, resolver: nil
      term = assert_resource term
      Resource.new self, term, resolver: resolver
    end

    # not sure what these are tbh
    #
    # def classes rdftype
    # end
    #
    # def properties predicate
    # end

    private

    SKOS_HIER = [
      {
        element: :subject,
        pattern: -> c, p { [nil, p, c] },
        preds: [RDF::Vocab::SKOS.broader,  RDF::Vocab::SKOS.broaderTransitive],
      },
      {
        element: :object,
        pattern: -> c, p { [c, p, nil] },
        preds: [RDF::Vocab::SKOS.narrower, RDF::Vocab::SKOS.narrowerTransitive],
      }
    ]
    SKOS_HIER.each do |struct|
      # lol how many times are we gonna cart this thing around
      preds = struct[:preds]
      i = 0
      loop do
        equiv = preds[i].entail(:equivalentProperty) - preds
        preds.insert(i + 1, *equiv) unless equiv.empty?
        i += equiv.length + 1;
        break if i >= preds.length
      end
    end

    public

    # Obtain all the sub-concepts of  a given `skos:Concept`.
    #
    # @param concept [RDF::Resource] the `skos:Concept` in question
    # @param extra [Array<RDF::Resource>] additional types for consideration
    #
    # @return [Array<RDF::Resource>] the sub-concepts.
    #
    # @note This method was imported from {Intertwingler::Context} and may
    #  predate the refactor that yielded this module.
    #
    def sub_concepts concept, extra: []
      concept = assert_resource concept
      extra   = assert_resources extra

      # we need an array for a queue, and a set to accumulate the
      # output as well as a separate 'seen' set
      queue = [concept]
      seen  = Set.new queue.dup
      out   = seen.dup

      # it turns out that the main SKOS hierarchy terms, while not
      # being transitive themselves, are subproperties of transitive
      # relations which means they are as good as being transitive.

      while c = queue.shift
        SKOS_HIER.each do |struct|
          elem, pat, preds = struct.values_at(:element, :pattern, :preds)
          preds.each do |p|
            query(pat.call c, p).each do |stmt|
              # obtain hierarchical element
              hierc = stmt.send elem

              # skip any further processing if we have seen this concept
              next if seen.include? hierc
              seen << hierc

              next if !extra.empty? and !extra.any? do |t|
                has_statement? RDF::Statement.new(hierc, RDF.type, t)
              end

              queue << hierc
              out   << hierc
            end
          end
        end
      end

      out.to_a.sort.uniq
    end

    # Obtain all the audiences (or non-audiences) for a given subject.
    # By default entails sub-audiences
    #
    # @param subject [RDF::Resource] the subject to search
    # @param proximate [false, true] whether to limit to immediate audiences
    # @param invert [false, true] whether to search for "non-audiences"
    #
    # @return [Array<RDF::Resource>] the audiences for the subject.
    #
    def audiences_for subject, proximate: false, invert: false
      p = invert ? Intertwingler::Vocab::CI['non-audience'] : RDF::Vocab::DC.audience

      subject = assert_resource subject

      objs = objects_for(subject, p, only: :resource)
      return objs if proximate

      # return the sub-concepts
      objs.reduce([]) { |a, o| a + sub_concepts(o) }.uniq
    end

    # Get all "reachable" UUID-identified entities (subjects which are
    # also objects)
    #
    # @param published [false, true] whether to limit to published resources
    #
    # @return [Array<RDF::Resource>] the resources in question
    #
    def reachable published: false
      p = published ? -> x { published?(x) } : -> x { true }
      # now get the subjects which are also objects
      subjects.select do |s|
        s.uri? && s =~ /^urn:uuid:/ && has_object?(s) && p.call(s)
      end
    end

  end

  # This class provides a resource-oriented view of the graph. When I
  # get around to implementing it. (Python librdf has one of these and
  # it's handy. This should really belong in the core rdflib.)
  class Resource
    attr_reader :repository, :resolver

    def initialize repository, subject, resolver: nil, cache: 1000
      @me = assert_resource subject

      @repository = repository
      @resolver   = resolver
    end

    # oh hell yes
    def [] predicate
      # resolve curie
    end

    # tempted to do a method_missing thing for predicates in the same
    # namespace as the subject but tbh that would probably be a mess

  end

end

# sneak this bad boy into RDF::Queryable where it belongs
module RDF::Queryable
  include Intertwingler::GraphOps
end

# also put it in RDF::Repository as a downstream module has a `method_missing`
class RDF::Repository
  include Intertwingler::GraphOps
end