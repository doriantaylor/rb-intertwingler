#!/usr/bin/env ruby

# this should haul in a bunch of stuff
require 'intertwingler/graphops'

# get us some vocabs
require 'intertwingler/vocab/tfo'
require 'intertwingler/vocab/itcv'

# third-party rdf
require 'rdf/vocab'  # dat vocab
require 'rdf/turtle' # to parse input
require 'sparql'     # dat algebra, lol

# save some typing
ITCV = Intertwingler::Vocab::ITCV
TFO  = Intertwingler::Vocab::TFO
SH   = RDF::Vocab::SH
XSD  = RDF::Vocab::XSD

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
    via << algebra(repo, path)
  end

  params[:fragments][fc] = specifier
end

# that it?

require 'pry'
binding.pry

#puts "#{params}"
