require 'rdf/sak/version'

require 'rdf'
require 'rdf/vocab'
require 'rdf/reasoner'
require 'rdf/sak/resolver'
require 'rdf/sak/util/clean'
require 'rdf/sak/mimemagic'

# load up my vocabs before reasoner is applied
require 'rdf/sak/ci'
require 'rdf/sak/tfo'
require 'rdf/sak/ibis'

# also third-party vocabs not found in RDF::Vocab
require 'rdf/sak/pav'
require 'rdf/sak/qb'
require 'rdf/sak/scovo'

module RDF::SAK

  # gotta make sure this gets run
  RDF::Reasoner.apply(:rdfs, :owl)

  # This is to attach inferencing operations directly to the
  # repository instead of bolting it on every time like a schmuck

  # this module bolts functionality onto RDF::Repository

  module Repository
    include RDF::SAK::Util::Clean

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

    def coerce_languages languages
      languages = languages.respond_to?(:to_a) ? languages.to_a : [languages]
      languages.map { |lang| lang.to_s.strip.tr_s(?_, ?-).downcase }.uniq
    end

    def is_language? literal, languages
      return false unless literal.literal? and lang = literal.language
      languages = coerce_languages languages 
      lang = lang.to_s.strip.tr_s(?_, ?-).downcase

      languages.include? lang
    end

    def invert_semantic predicates, entail: false
      inverted = Set[]
      # scare up the reverse properties
      predicates.each do |p|
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

    # Retrieve the current structure being used to govern how labels
    # are resolved against subjects.
    #
    # @return [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  the label structure
    # 
    def label_struct
      @labels ||= process_labels LABELS
    end

    # Set a new structure for determining how labels are
    # resolved. This is a hash where the keys are RDF types, and the
    # values are hashes containing the keys `:label` and `:desc`,
    # whose values are arrays containing one or two values (for main
    # and alternate; if alternate is missing then main will be used
    # instead), which themselves are arrays containing one or more RDF
    # properties, in order of preference for the given class.
    #
    # @param struct [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  what a mouthful
    #
    # @return [Hash{RDF::URI=>Hash{Symbol=>Array<Array<RDF::URI>>}}]
    #  the same label structure, but normalized
    #
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
        lang: nil, desc: false, alt: false, struct: nil

      # a literal is its own label
      if subject.is_a? RDF::Literal
        # do this for return value parity
        return unique ? [nil, subject] : [[nil, subject]]
      end

      subject = assert_resource  subject
      graph   = assert_resources graph

      asserted = types_for subject, graph: graph, struct: struct

      strata = entail ? type_strata(asserted) : asserted

      struct ||= struct_for subject, graph: graph, only: :literal
      seen  = {}
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

    private

    AUTHOR  = [RDF::SAK::PAV.authoredBy, RDF::Vocab::DC.creator,
      RDF::Vocab::DC11.creator, RDF::Vocab::PROV.wasAttributedTo]
    CONTRIB = [RDF::SAK::PAV.contributedBy, RDF::Vocab::DC.contributor,
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
        o = repo.first_object([subject, pred, nil])
        next unless o
        # note this use of RDF::List is not particularly well-documented
        authors += RDF::List.new(subject: o, graph: self).to_a
      end

      # now try various permutations of the author/contributor predicate
      unsorted = (contrib ? CONTRIB : AUTHOR).reduce([]) do |u, pred|
        u + repo.query([subject, pred, nil]).objects
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
    # @param 
    #
    # @return [Array<RDF::Resource>] the replacements, if any
    #
    def replacements_for subject, graph: nil, published: true, noop: false
      subject = assert_resource subject
      graph   = assert_resources graph

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
    def dates_for subject, graph: nil, predicate: RDF::Vocab::DC.date,
        datatype: [RDF::XSD.dateTime, RDF::XSD.date]
      objects_for subject, predicate, graph: graph,
        datatype: datatype, only: :literal
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
    def formats_for subject, predicate: RDF::Vocab::DC.format,
        datatype: [RDF::XSD.token]
      objects_for(subject, predicate,
                  graph: graph, datatype: datatype, only: :literal) do |o|
        t = o.object
        t =~ /\/?/ ? RDF::SAK::MimeMagic.new(t.to_s.downcase) : nil
      end.compact.sort.uniq
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
      # we explicitly add this class because it overrides the fact that 
      RDF::Vocab::BIBO.DocumentPart => {
        RDF::Vocab::DC.hasPart            => true,
        RDF::Vocab::DC.isPartOf           => false,
      }.freeze,
    }.freeze

    # these are all the types unambiguously considered to be "documents"
    DOCUMENTS = [RDF::Vocab::FOAF.Document].freeze

    def expand_documents docs
      docs = assert_resources docs, blank: false, empty: false, vocab: true
      type_strata(docs, descend: true) - fragment_spec.keys
    end

    def expand_fragments spec
      out = {}

      # create an initial queue of rdf types
      typeq = spec.keys
      i = 0
      while i < typeq.length do
        type = typeq[i]
        # we don't want to do this for base types
        next i += 1 if [RDF::RDFS.Resource, RDF::OWL.Thing].include? type
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

    def fragment_spec
      @fragments ||= expand_fragments FRAGMENTS
    end

    def fragment_spec= spec
      @fragments = expand_fragments spec
    end

    # Retrieve a host document for a fragment, if it exists; null
    # otherwise (or itself if `:noop` is true).
    def host_for subject, graph: nil,
        published: true, unique: true, noop: false, seen: Set[]
      subject = assert_resource subject
      graph   = assert_resources graph

      # we begin by looking for an explicit designation
      host = objects_for(subject, RDF::SAK::CI['fragment-of'],
        graph: graph, only: :resource).first

      # get all the classes but the two basic ones that will net everything
      ft = fragment_spec.keys - [RDF::RDFS.Resource, RDF::OWL.Thing]

      types = types_for subject, graph: graph
      isdoc = type_is? types, document_types
      frags = type_is? types, ft

      # this condition is true if 
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
        pab = {}
        hosts = preds.reduce([]) do |a, pair|
          pred, rev = pair
          if rev
            # reverse relations include list heads
            a += subjects_for pred, subject, graph: graph, only: :resource
            a += subjects_for pred, head, graph: graph, only: :resource if head
          else
            a += objects_for subject, pred, only: :resource
          end
          a # <-- the accumulator
        end.uniq.select { |h| rdf_type? h, document_types }.sort do |a, b|
          # sort by publication status
          pa = pab[a] ||= (published?(repo, a) ? -1 : 0)
          pb = pab[b] ||= (published?(repo, b) ? -1 : 0)
          # now sort by published-ness
          c = pa <=> pb
          # XXX TODO maybe sort by date? i unno
          # sort lexically if it's a tie
          c == 0 ? a <=> b : c
        end

        # the first one will be our baby
        if host = hosts.first and not seen.include? host
          parent = host_document repo, host, base: base, frag_map: frag_map,
            documents: documents, seen: seen | Set[host]
          return parent if parent
        end
      end

      # return the noop
      noop ? host || subject : host
    end

    # Return true if the subject is a document fragment.
    def fragment? subject, graph: nil, published: true
      !!host_for(subject, graph: graph, published: published)
    end

    # Retrieve the RDF types considered to be "documents". Thies
    #
    # @return [Array<RDF::URI>] all recognized document types
    #
    def document_types
      @documents ||= expand_documents DOCUMENTS
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

    # Determine whether the subject is considered "published".
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

      if host = host_for(subject, graph: graph, published: false)
        return published? host, graph: graph,
          circulated: circulated, retired: retired, indexed: indexed
      end

      if indexed
        ix = objects_for(subject, RDF::SAK::CI.indexed, graph: graph,
          only: :literal, datatype: RDF::XSD.boolean).first
        return false if ix and ix.object == false
      end

      # obtain the status
      candidates = objects_for(
        subject, RDF::Vocab::BIBO.status, only: :resource).to_set

      return false if !retired and candidates.include? RDF::SAK::CI.retired

      # set up a test set of statuses
      test = Set[RDF::Vocab::BIBO['status/published']]
      test << RDF::SAK::CI.circulated if circulated

      # if this isn't empty then we're "published"
      !(candidates & test).empty?
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
    #
    # @yieldparam subject [RDF::Resource] the subject
    # @yieldparam type [RDF::Resource] the asserted type of the subject
    # @yieldparam graph [nil, RDF::URI] the graph where the statement
    #  was found
    #
    # @return [Array<RDF::Resource>] the subjects of the given type(s)
    #
    def all_of_type rdftype, graph: nil, &block
      rdftype = assert_resources rdftype
      graph   = assert_resources graph
      # get all the RDF types in the graph(s)
      out = []

      all_types(graph: graph).each do |t|
        next unless type_is? t, rdftype
        out += graph.map do |g|
          query([nil, RDF.type, t, g]).subjects.map do |s|
            block ? block.call(s, t, g) : s
          end
        end.flatten(1)
        # only flatten the first layer, the second is concatenated
      end
      
      out.sort.uniq
    end

    # Obtain a stack of RDF types for an asserted initial type or set
    # thereof. Returns an array of arrays, where the first is the
    # asserted types and their inferred equivalents, and subsequent
    # elements are immediate superclasses and their equivalents. A given
    # URI will only appear once in the entire structure. When `descend`
    # is set, the resulting array will be flat.
    #
    # @param rdftype [RDF::Term, :to_a] the type(s) to inspect
    # @param descend [true, false] descend instead of ascend
    #
    # @return [Array<Array<RDF::URI>>] the type stratum
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
      seen   = Set[]
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
      reftype = assert_resource reftype, blank: false, vocab: true
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

    # as i will invariably trip over this
    alias_method :predicate_set, :property_set

    # Generate a closure that can be passed into {Enumerable#sort} for
    # sorting literals according to the given policy.
    #
    # @param reverse [false, true] whether to reverse the sort
    # @param datatype [RDF::URI, Array<RDF::URI>] preference for datatype(s)
    # @param language [String, Symbol, Array<String, Symbol>]
    #  preference for language(s)
    # @param nocase [true, false] whether to sort case-sensitive
    # @param longer [false, true] whether to sort longer strings
    #  before shorter strings where the longer string begins with the
    #  shorter string
    #
    # @return [Proc] the comparison function
    #
    def cmp_literal reverse: false, datatype: nil, language: nil,
        nocase: true, longer: false, resources_first: false
      datatype = assert_resources datatype, blank: false
      language = coerce_languages language

      # map the result of literal? to something we can compare
      cmp_rsrc = { false => 1, true => 0 } 
      cmp_rsrc[false] = -1 if resources_first

      lambda do |a, b|
        # first flip if reverse
        a, b = b, a if reverse
        # then detect if both are literal
        c = cmp_rsrc[a.literal?] <=> cmp_rsrc[b.literal?]

        return c if c != 0

        # these are both literal
        if a.literal?
          # then detect if both are the same type
          unless datatype.empty?
            ad = datatype.index(a.datatype) || Float::INFINITY
            bd = datatype.index(b.datatype) || Float::INFINITY

            c = ad <=> bd

            return c if c != 0
          end

          # then detect if both are the same language
          if a.datatype == RDF.langString and !language.empty?
            al, bl = [a, b].map do |x|
              x = x.language.downcase.tr_s(?_, ?-)
              language.index(x) || Float::INFINITY
            end

            c = al <=> bl

            return c if c != 0
          end

          # then optionally squash to lower case
          a, b = [a, b].map do |x|
            x = x.value
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
    #
    # @return [Proc] the comparison function
    #
    def cmp_label reverse: false, cache: nil, datatype: nil, language: nil,
        nocase: false, longer: false, desc: false, alt: false

      # obtain label cmp
      cmp = cmp_literal reverse: reverse, datatype: datatype,
        language: language, nocase: nocase, longer: longer

      cache ||= {}

      lambda do |a, b|
        # obtain and cache the labels
        cache[a] ||= (label_for(a) || []).last || a
        cache[b] ||= (label_for(b) || []).last || b

        # now run the label cmp
        cmp.(cache[a], cache[b])
      end
    end

    # Instantiate a Resource on the given term. It is up to you to
    # ensure that `term` is actually in the graph.
    #
    # @param term [RDF::Resource] the term in question
    # @param resolver [nil, RDF::SAK::Resolver] an optional `Resolver`
    #  instance
    #
    # @return [RDF::SAK::Resource] a `Resource` bound to the term
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
  end

  # This class provides a resource-oriented view of the graph.
  class Resource
    attr_reader :repository, :resolver

    def initialize repo, subject, resolver: nil
      @me = assert_resource subject

      @repository = repo
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

# no wait THIS is the bastard thing
module RDF::Queryable
  include RDF::SAK::Repository
end
