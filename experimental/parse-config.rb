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
  pfx[prefix.to_s] = RDF::URI(ns.to_s)
end

# 3. document class and fragment specifiers

puts params.inspect
