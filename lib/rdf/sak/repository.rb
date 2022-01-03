require 'rdf/sak/version'

require 'rdf'

module RDF::SAK
  # This is to attach inferencing operations directly to the
  # repository instead of bolting it on every time like a schmuck

  # this module bolts functionality onto RDF::Repository

  module Repository
    include RDF::SAK::Util::Clean

    private

    # rdf term type tests
    NTESTS = { uri: :"uri?", blank: :"node?", literal: :"literal?" }.freeze
    NMAP   = { iri: :uri, bnode: :blank }.merge(
      (%i[uri blank literal] * 2).transpose.to_h)

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

    def is_language? literal, languages
      return false unless literal.literal? and lang = literal.language
      languages = languages.respond_to?(:to_a) ? languages.to_a : [languages]
      languages.map! { |lang| lang.to_s.strip.tr_s(?_, ?-).downcase }
      lang = lang.to_s.strip.tr_s(?_, ?-).downcase

      languages.include? lang
    end

    def invert_semantic predicates, entail: false
      inverted = Set[]
      # scare up the reverse properties
      predicate.each do |p|
        # inverse properties are available by entailment
        inverted |= p.inverseOf.to_set if p.respond_to? :inverseOf
        # symmetric properties go in as-is
        inverted << p if symmetric? p
      end

      # don't forget to entail
      entail ? property_set(inverted) : inverted
    end

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

    # Retrieve the current structure being used to govern label
    # retrieval.
    #
    # @return [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    # 
    def label_struct
      @labels ||= process_labels LABELS
    end

    def label_struct= struct
      @labels = process_labels struct
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
      predicate = assert_resources predicate, blank: false, empty: false
      datatype  = assert_resources datatype,  blank: false
      graph     = assert_resources graph

      # entail all the predicates
      predicate = property_set predicate if entail

      # do the reverse predicates once now instead of recomputing them
      # with every graph
      revp = invert_semantic predicate, entail: entail unless only == [:literal]

      # add a single nil graph for triple semantics
      graph << nil if graph.empty?

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

      predicate = assert_resources predicate, blank: false, empty: false
      object    = assert_resource  object
      graph     = assert_resources graph

      predicate = property_set predicate if entail

      # note that this is a slightly different regime than object_for
      revp = invert_semantic predicate, entail: entail unless object.literal?

      # add nil graph to the array for triple semantics
      graph << nil if graph.empty?

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

           p = stmt.predicate
           p = (RDF::Vocabulary.find_term(p) rescue p) || p
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
      rsrc.values.each { |v| v.sort!.uniq! }
      rsrc
    end
    
    # Obtain all and only the `rdf:type`s directly asserted on the subject.
    # 
    # @param repo [RDF::Queryable]
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
        return struct[RDF.type].select(&:uri?)
      end

      objects_for subject, RDF.type, graph: graph, entail: entail, only: :uri
    end

    # Obtain the most appropriate label(s) for the subject's type(s).
    # Returns one or more (depending on the `unique` flag)
    # predicate-object pairs in order of preference.
    #
    # @param subject [RDF::Resource]
    # @param unique [true, false] only return the first pair
    # @param type [RDF::Term, Array] supply asserted types if already retrieved
    # @param lang [nil] not currently implemented (will be conneg)
    # @param desc [false, true] retrieve description instead of label
    # @param alt  [false, true] retrieve alternate instead of main
    #
    # @return [Array<(RDF::URI, RDF::Literal)>, Array<Array<(RDF::URI,
    #  RDF::Literal)>>] either a predicate-object pair or an array of
    #  pairs.
    #
    def label_for subject, graph: nil, entail: true, unique: true,
        lang: nil, desc: false, alt: false, struct: nil
      subject = assert_resource  subject
      graph   = assert_resources graph

      asserted = types_for subject, graph: graph, struct: struct

      strata = entail ? type_strata(asserted) : asserted

      struct ||= struct_for subject, graph: graph, only: :literal
      seen = {}
      accum = []

      strata.each do |types|
        types.each do |cls|
          next unless preds = (label_struct.dig(
            cls, desc ? :desc : :label) || [])[alt ? 1 : 0]
          preds.each do |p|
            next unless vals = struct[p]
            vals.each do |v|
              next unless v.literal?
              pair = [p, v]
              accum << pair unless seen[pair]
              seen[pair] = true
            end

            # XXX TODO sort vals
          end
        end
      end

      unique ? accum.first : accum.uniq
    end

    # return all RDF types present in the graph.
    def all_types graph: nil
      graph = assert_resources graph
      graph << nil if graph.empty?
      graph.map { |g| query([nil, RDF.type, nil, g]).objects }.flatten.uniq
    end

    #
    def all_of_type rdftype, graph: nil, &block
      rdftype = assert_resources rdftype
      graph   = assert_resources graph
      # get all the RDF types in the graph(s)
      out = []

      all_types(graph: graph).each do |t|
        next unless type_is? t, rdftype
        out += graph.map { |g| query([nil, RDF.type, t, g]).subjects }.flatten
      end
      
      out.uniq.sort
    end

    # Obtain a stack of RDF types for an asserted initial type or set
    # thereof. Returns an array of arrays, where the first is the
    # asserted types and their inferred equivalents, and subsequent
    # elements are immediate superclasses and their equivalents. A given
    # URI will only appear once in the entire structure. When `descend`
    # is set, the resulting array will be flat.
    #
    # @param rdftype [RDF::Term, :to_a]
    # @param descend [true,false] descend instead of ascend
    #
    # @return [Array<Array<RDF::URI>>]
    #
    def type_strata rdftype, descend: false
      rdftype = assert_resources rdftype, vocab: true

      return [] if rdftype.empty?

      # essentially what we want to do is construct a layer of
      # asserted classes and their inferred equivalents, then probe
      # the classes in the first layer for subClassOf assertions,
      # which will form the second layer, and so on.

      queue  = [rdftype]
      strata = []
      seen   = Set.new
      qmeth  = descend ? :subClass : :subClassOf 

      while qin = queue.shift
        qwork = []

        qin.each do |q|
          qwork << q # entail doesn't include q
          qwork += q.entail(:equivalentClass) if
            q.uri? and q.respond_to? :class?
        end

        # grep and flatten
        qwork = qwork.map do |t|
          next t if t.is_a? RDF::Vocabulary::Term
          RDF::Vocabulary.find_term t rescue nil
        end.compact.uniq - seen.to_a
        seen |= qwork

        # warn "qwork == #{qwork.inspect}"

        # push current layer out
        strata.push qwork.dup unless qwork.empty?
     
        # now deal with subClassOf
        qnext = []
        qwork.each { |q| qnext += q.send(qmeth) if q.respond_to? qmeth }

        # grep and flatten this too
        qnext = qnext.map do |t|
          next t if t.is_a? RDF::Vocabulary::Term
          RDF::Vocabulary.find_term t rescue nil
        end.compact.uniq - seen.to_a
        # do not append qsuper to seen!

        # warn "qsuper == #{qsuper.inspect}"

        # same deal, conditionally push the input queue
        queue.push qnext.dup unless qnext.empty?
      end

      # voila
      descend ? strata.flatten : strata
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
      reftype = reftype.respond_to?(:to_a) ? reftype.to_a : [reftype]
      return if reftype.empty?

      reftype.map! { |t| RDF::Vocabulary.find_term t rescue t }

      # generate types, including optionally base classes if they aren't
      # already present in the strata (this will be automatically 
      types = type_strata(type)
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

    private

    # this is the recursive method that runs after the initial input
    # has been sanitized
    def property_set_internal properties
      # short circuit
      return properties if properties.empty?

      # get all the equivalents, again note we can't trust this output
      # completely so we filter for URIs
      properties |= properties.to_a.map do |p|
        p.entail :equivalentProperty
      end.flatten.select(&:uri?).to_set

      # get all the subproperties
      subp = properties.reduce(Set[]) do |s, p|
        # we can't actually be sure what's in this
        s |= p.subProperty.flatten.to_set if p.respond_to? :subProperty
        s # noop otherwise
      end

      # subtracting the predicates from the subproperties should have
      # the effect of a cycle guard
      return properties if (subp - properties).empty?

      # now we can pass in the union of these two sets
      property_set_internal(properties | subp)
    end

    public

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

      property_set_internal properties
    end

    # instantiate a Resource
    def resource term
    end

    def classes rdftype
    end

    def properties predicate
    end

    def list_head subject
    end

    # lol this is a bastard thing to do but i don't care it's ruby
    extend RDF::Repository
  end

  # This class provides a resource-oriented view of the graph.
  class Resource
    def initialize repo, subject, resolver: nil
    end

    # oh hell yes
    def [] predicate
      # resolve curie
    end

    # tempted to do a method_missing thing for predicates in the same
    # namespace as the subject but tbh that would probably be a mess

  end

end
