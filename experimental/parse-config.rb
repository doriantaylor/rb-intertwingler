#!/usr/bin/env ruby
# -*- mode: enh-ruby -*-

# third-party rdf
require 'rdf'
require 'rdf/vocab'  # dat vocab
require 'rdf/turtle' # to parse input
require 'sparql'     # dat algebra, lol

# this should haul in a bunch of stuff
require 'intertwingler/vocab'
require 'intertwingler/graphops'

# save some typing
ITCV = Intertwingler::Vocab::ITCV
TFO  = Intertwingler::Vocab::TFO
CI   = Intertwingler::Vocab::CI
SH   = RDF::Vocab::SH
XSD  = RDF::Vocab::XSD

SAO = SPARQL::Algebra::Operator

PATHS = {
  SH.alternativePath => -> repo, o {
    # this shoouuuuld be a list
    list = RDF::List subject: o, graph: repo
    list.reverse.reduce do |a, x|
      SPARQL::Algebra::Operator::Alt.new x, a
    end
  },
  SH.inversePath     => SPARQL::Algebra::Operator::Reverse,
  SH.oneOrMorePath   => SPARQL::Algebra::Operator::PathPlus,
  SH.zeroOrOnePath   => SPARQL::Algebra::Operator::PathOpt,
  SH.zeroOrMorePath  => SPARQL::Algebra::Operator::PathStar,
}

# entail a URI and return an alt
def entail_term repo, term
  fwd = repo.property_set term
  rev = repo.property_set term, inverse: true

  fwd = fwd.count == 1 ? fwd.first : fwd.reduce do |a, b|
    SPARQL::Algebra::Operator::Alt.new b, a
  end

  unless rev.empty?
    rev = rev.count == 1 ? rev.first : rev.reduce do |a, b|
      SPARQL::Algebra::Operator::Alt.new b, a
    end

    fwd = SPARQL::Algebra::Operator::Alt.new fwd,
      SPARQL::Algebra::Operator::Reverse.new(rev)
  end

  fwd
end

# entail an arbitrary operator
def entail_op repo, expr
  case expr
  when RDF::URI
    entail_term repo, expr
  when SPARQL::Algebra::Operator
    ops = expr.operands.map { |o| entail_op repo, o }
    # XXX there is no accessor for options
    expr.class.new(*ops)
  else
    expr
  end
end

# recursively trace the shacl predicate paths. the shacl module
# appears to be really only about doing shacl shapes and appears to
# turn the rdf into json-ld first before parsing it??? no idea.
def algebra repo, subject, seen = {}
  # it's either a sequence or one of these buggers
  if !repo.objects_for(subject, RDF.first, only: :resource).empty?
    list = RDF::List.new(subject: subject, graph: repo).map do |x|
      algebra repo, x
    end

    return list.reverse.reduce do |a, x|
      SPARQL::Algebra::Operator::Seq.new x, a
    end
  else
    struct = repo.struct_for subject
    # warn struct
    # this should yield exactly one thing
    keys = PATHS.keys & struct.keys
    raise ArgumentError,
      "more than one key: #{keys.sort.join ?,}" if keys.size > 1

    if keys.empty?
      # then this is a term
      return subject
    else
      op   = PATHS[keys.first]
      obj  = struct[keys.first].first # XXX this should only be one

      return op.is_a?(Proc) ? op.call(repo, obj) : op.new(algebra repo, obj)
    end
  end
end

# parse a sparql property
def parse_path path, prefixes
  out = SPARQL::Grammar::Parser.new(path, prefixes: prefixes).parse(:Path)
  out.last
end

# query the graph for the host document using the shacl/sparql-based spec
def host_for_internal repo, subject, spec, seen = nil, dtypes = nil, graph: nil,
    published: false, circulated: false

  # 1. attempt to detect a direct assertion that this is a fragment
  host = repo.objects_for(
    subject, CI['fragment-of'], graph: graph, only: :resource).sort.first

  # XXX disambiguate if there is more than one direct assertion (is
  # document type, is published, newest?, alphabetical)

  ft = spec.keys - [RDF::RDFS.Resource, RDF::OWL.Thing]
  dtypes ||= repo.document_types(fragments: true) & repo.all_types

  types = repo.types_for subject, graph: graph
  isdoc = repo.type_is? types, dtypes
  frags = repo.type_is? types, ft

  unless host or (isdoc and not frags)
    # obtain property paths
    paths = spec.map do |type, path|
      score = repo.type_is?(types, type) or next
      [score, path]
    end.compact.sort do |a, b|
      a.first <=> b.first
    end.map(&:last).flatten(1).uniq

    # accumulate candidates
    tab = {} # type cache
    pab = {} # priority cache
    hosts = paths.reduce([]) do |array, path|
      # okay here is where we construct the query from the algebra

      o = RDF::Query::Variable.new ?o
      c = RDF::Query::Variable.new ?c
      # this says SELECT DISTINCT ?o WHERE {
      #   $subject $path $o . $o a $c FILTER (?c in ($dtypes)) }
      query = SAO::Distinct.new(
        SAO::Project.new([o], SAO::Filter.new(SAO::In.new(c, *dtypes),
          SAO::Sequence.new(SAO::Path.new(subject, path, o),
            RDF::Query.new { pattern [o, RDF.type, c] }))))

      # this just says SELECT DISTINCT ?o WHERE { $subject $path ?o }
      # query = SAO::Distinct.new(SAO::Project.new(
      #   [o], SAO::Path.new(subject, path, o)))

      array + query.execute(repo).map { |sol| sol[:o] }
    end.uniq

    if host = hosts.first and not seen.include? host
      parent = host_for_internal repo, host, spec, seen | Set[host], dtypes,
            graph: graph, published: published, circulated: circulated
          host = parent if parent
    end
  end

  host
end

# load the graph
repo = RDF::Repository.load ARGV.first

# okay let's get the resolver and look at it
resolver = repo.all_of_type(ITCV.Resolver).first

# what are the things we need to get from the resolver?

params = {}

# 1. addressing information, the base URI and its aliases

params[:base] = repo.objects_for(resolver, ITCV.manages, only: :resource).first
params[:aliases] = repo.objects_for(resolver, ITCV.alias, only: :resource)

# 2. prefix and vocab mappings

pfx = params[:prefix] = {}

repo.objects_for(resolver, ITCV.prefix, only: :resource).each do |decl|
  prefix = repo.objects_for(decl, SH.prefix, only: :literal).sort.first
  ns     = repo.objects_for(
    decl, SH.namespace, only: :literal, datatype: XSD.anyURI).sort.first
  # i don't understand why this but
  pfx[prefix.to_s.to_sym] = RDF::URI(ns.to_s)
end

if v = repo.objects_for(resolver, ITCV.vocab).first
  v = RDF::URI(v.to_s) if v.literal?
  params[:vocab] = v
end

# 3. document class and fragment specifiers

params[:documents] = repo.objects_for(resolver, ITCV.document, only: :resource)

params[:fragments] = {}

repo.objects_for(resolver, ITCV.fragment, only: :resource).each do |frag|
  specifier = {}

  specifier[:fragment] = fc = repo.objects_for(frag,
    ITCV[:"fragment-class"], only: :resource).sort.first

  if host =  repo.objects_for(frag,
    ITCV[:"host-class"], only: :resource).sort.first
    specifier[:host] = host
  end

  repo.objects_for(frag, ITCV.via, only: :resource).each do |path|
    # this will be either a term or a path object or a list
    via = specifier[:via] ||= []
    via << entail_op(repo, algebra(repo, path))
  end

  params[:fragments][fc] = specifier
end

# that it?

require 'pry'
binding.pry

#puts "#{params}"
