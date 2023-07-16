# Intertwingler — An Engine for Dense Hypermedia

This software is an _engine_ for creating _dense hypermedia_ networks.
Dense hypermedia is what the Web, out of the box, _isn't_. The Web is
_sparse_ hypermedia: big, long documents, with few links aside from
things like navigation and footers. Dense hypermedia is all about
_short_ resources connected by _lots_ of links. One example of dense
hypermedia are the personal knowledge management systems, colloquially
known as _tools for thought_. Another is _knowledge graphs_. The goal
of this product is to support those categories of application, and
others — perhaps even art or literature.

> One important success criterion is to eliminate the mundane aspects
> of "building a website", and otherwise get out of the way.

This is very much a _speaking artifact_: Since the ultimate goal is to
create better conditions for developing dense hypermedia on the Web by
retrofitting it with the capabilities of systems that preceded it
(real and imagined), there are a number of subsidiary problems that
need to be solved, and this system implements concrete ways to solve them.

## Links are first-class citizens

Before we can do anything related to dense hypermedia, we have to
solve for [_link rot_](https://en.wikipedia.org/wiki/Link_rot). The
median URL has a lifespan that can be measured in weeks. If you have
orders of magnitude more addressable resources under management than
the median, this kind of performance is a non-starter. Link rot
doesn't need to happen (at least, for now, up to keeping one's domain
name bill paid), but what _does_ need to happen in order to fix it is
a radical rethinking of how Web-based software is made. This system
shows how to do it.

> [My own site](https://doriantaylor.com/), which admittedly has only
> been on this system since I made the latter in 2018, nevertheless
> still serves every URL it has ever exposed, dating back to the
> summer of 2008. I also use it for my client extranets, and my book
> project, [The Nature of Software](https://the.natureof.software/).

One ongoing criticism of the Web by [Ted
Nelson](https://en.wikipedia.org/wiki/Ted_Nelson), the man who in 1960
coined the term _hypertext_ (not to mention what it means to
[_intertwingle_](https://en.wikipedia.org/wiki/Intertwingularity)), is
that links only go in one direction: without extra apparatus, you
can't see what links _to_ you. Well, turns out the apparatus to
display backlinks is the same apparatus as the one for eliminating
link rot.

## Links (_and_ resources) have different species

The Web has three kinds of links: the conventional arc that when
activating it (typically) completely replaces the representational state
(both `<a>` and forms), what I would characterize as a "naïve embed" —
images, A/V, and `iframe` documents — and non-printing metadata.
Earlier systems had all kinds of other links besides, like
[stretchtext](https://en.wikipedia.org/wiki/StretchText), conditional
display, and proper, seamless
[transclusion](https://en.wikipedia.org/wiki/Transclusion). These of
course can all be done on the Web, but the solutions are suboptimal.
In particular, the embedded metadata that drives these capabilities
tends to be ad-hoc and mutually incompatible, making it single-purpose
for some particular UI framework or other. Many content management
systems, moreover, have a concept of _content_ type, but few systems —
even sophisticated PKM systems — have a concept of _link_ type (as in,
precisely what the link _means_). It's the link types in
conjunction with the content types that make it possible to _derive_
how they ought to be rendered in the user interface.

## Don't copy what you can reference

One perennial problem of informational content, whether on the Web or
even digital at all, is keeping it up to date. A necessary condition
for keeping content up to date is ensuring that there is precisely
_one_ authoritative copy of it.

> The key word here of course is _authoritative_. We will invariably
> need multiple copies for things like cache and backups, but having
> exactly _one_ copy that drives all the others is absolutely
> indispensable.

This principle can be extended to resources which can be modeled as
_functions_ of other resources, for example the HTML that corresponds
to a Markdown document, or a cropped and/or resized image. Explicitly
modeling these as transformations shrinks the footprint of original
content to be managed.

Finally, for content to be reusable it must be finely _addressable_,
with durable addresses at both the document and _sub_-document level.

## Standard interfaces & Data transparency

With this system we're trying to imagine what it means to be a "model
citizen" on the Web: a reliable source of clear, actionable
information. This is not only entails everything already discussed,
but also:

* structured, machine-actionable data is available for every resource,
* interfaces are standard, so as not to require custom API adapters,
* this includes data _semantics_ as well as syntax,
* A user (with sufficient authority) should be able to export 100% of
  the system's instance data, and furthermore that data should _mean
  something_ to other systems.

## Layered system, clear development targets

This system anticipates being situated in a heterogeneous operating
environment, sharing space with other programming languages and
frameworks. Indeed, this engine can be thought of as a "language bus",
that marshals all things Ruby. The design is intended to be copied to
other programming languages, and these systems are expected to
interoperate in a daisy chain-like configuration.

Every component in this system, including the central piece that does
the routing, is implemented as a
[Rack](https://rubydoc.info/gems/rack/) handler, which ultimately
could be run as a stand-alone microservice. The handlers subsequently
subdivide into two subspecies:

* **Content handlers** that either originate information resources or
  proxy them from somewhere else,
* **transforms** that manipulate HTTP requests or responses in transit.

Since every building block in the system is a potentially stand-alone
Rack component, the language spoken between them is nominally HTTP.
This not only makes for _extremely_ well-defined development targets —
you get a request and return a response — but the system anticipates
future segmentation, including, as mentioned, across different
programming languages, machines, and runtimes.

> I should note that HTTP communication within the process space of a
> particular runtime is simulated, so we don't waste resources
> unnecessarily re-parsing and serializing. I also have a rudimentary
> sub-protocol in the works for specific constraints on how these
> components, particularly the transforms, are expected to behave.

# (Original pre-refactor README; leaving it here until I broom it)

This library can be understood as something of a workbench for the
development of a set of patterns and protocols for Web content—a
[content management
_meta-system_](https://doriantaylor.com/content-management-meta-system).
The proximate goals of this implementation are as follows:

* Durable addresses with fine-grained addressability of content,
* Adherence to declarative content models mediated by open standards,
* Easy content re-use, i.e., _transclusion_,
* Rich embedded metadata for search, social media, and other purposes.

`Intertwingler` is also intended to serve as a substrate for:

* The maturation of a [content inventory
  ontology](https://vocab.methodanstructure.com/content-inventory#),
* The use of semantic metadata [for manipulating content
  presentation](https://github.com/doriantaylor/rdfa-xslt),
* Development of a deterministic, standards-compliant, [client-side
  transclusion mechanism](https://github.com/doriantaylor/xslt-transclusion).

Finally, this library is also intended to establish a set of hard
requirements for the kind of RDF and Linked Data infrastructure that
would be necessary to replicate its behaviour in a more comprehensive
content management system.

The only public website currently generated by `Intertwingler` is [my own
personal site](https://doriantaylor.com/), which I use as a test
corpus. However, this library draws together a number of techniques
which have been in use with employers and clients for well over a decade.

## MAJOR REFACTOR/OVERHAUL LOL

* [x] RDF::Repository extension (entailment, etc)
* [x] URI resolver
* [ ] generalized markup document interface
* [ ] data forensics from docs
  * [ ] nlp
  * [ ] metrics
  * [ ] pre-rdfa scan
  * [ ] version control
* [ ] source driver
* [ ] _surface_ driver
  * [ ] web microservicey app thing
    * [ ] caching proxy?
* [ ] command line (batch mode whatev)
  * [ ] actual shell

### TODO

* [ ] empty out `Intertwingler::Context`
  * [ ] move source-fetching stuff to `Intertwingler::Source`
  * [ ] move target-writing stuff to `Intertwingler::Surface::DocumentRoot`
  * [ ] move document stuff to `Intertwingler::Document`
    * [ ] get rid of `write_to_target`
  * [ ] overhaul the configuration/marshalling
* [ ] empty out `Intertwingler::Util::Messy`
  * [ ] move everything in it to its respective more-sensible module
  * [ ] merge `Intertwingler::Util::Clean` back to `Intertwingler::Util`

### AFTER THAT

* [ ] get rid of `Intertwingler::Document` and everything under it
  * [ ] replace it with `Intertwingler::Representation` and friends
    * [ ] generated markup should be a subclass of `Intertwingler::Source`
* [ ] implement content transforms as pure functions
  * [ ] break out the markup-generating and manipulating ones
  * [ ] do some for images (scale/crop/etc)
  * [ ] hook up the content-addressable store and [TFO ontology](https://vocab.methodandstructure.com/transformation#)
    * [ ] use it for rudimentary caching
* [ ] [Loupe](https://vocab.methodandstructure.com/loupe#)

## ARCHITECTURE

maybe its own document? who knows!

* [ ] [notion of "source" and "surface"](https://rdf-sak.ibis.makethingsmakesense.com/95bab4f0-5b0d-42b5-a39d-768d1d725234)
  * [ ] we won't call it "origin" because that has special meaning
    * [ ] a "source" *can* be an "origin" but not necessarily (e.g. reverse proxy)
  * [ ] a source just has to take a URI and `Accept-*` header set (also `Authorization` if applicable) and return a representation
  * [ ] a "sink" is not the best word because a sink will actually *request* resources
    * [ ] instead we call it `Surface`
  * [ ] [so we implement the existing file system source as a `Source`](https://rdf-sak.ibis.makethingsmakesense.com/73409baf-2a94-4c6d-bbc9-54a7825009ab)
  * [ ] we implement the existing file system target as a `Surface`
    * [ ] [we change `write_to_target` to be something that the "sink" *pulls* rather than *pushes*.](https://rdf-sak.ibis.makethingsmakesense.com/3d348205-3878-4c60-9aa4-d8f16cddae91)
  * [ ] [we also implement the `Rack` app as a `Surface`](https://rdf-sak.ibis.makethingsmakesense.com/3cb7ab04-dd1a-443c-a5a3-8f43923ed0d7)
  * [ ] implement generated representations as "sources"
    * [ ] concept schemes (glossary/index/thesaurus)
    * [ ] bibliographies/book lists
    * [ ] rolodexes
    * [ ] document stats
    * [ ] rss/atom feeds
    * [ ] google sitemaps
    * [ ] arbitrary graph patches
* [ ] make any operation that can be made a pure function, a pure function
  * [ ] we want this because it's composable
  * [ ] most if not all ordinary web pages will probably have a sequence of stock transforms
* [ ] we want a command line shell
  * [ ] start daemon from shell?

## Future Directions

Ultimately, this implementation is disposable. What matter are the
patterns laid down in the program output. If these patterns can be
sufficiently normalized, then the behaviour ought to at least be
replicable in other content management systems. Failing that, the
patterns will serve as a set of requirements for future systems. The
ideal is that adherence to existing open standards plus a modest set
of additional constraints could, at least from the perspective of the
audience, make the CMS infrastructure disappear.

As for the texture and topology of the content itself, squaring away
the mundane problems of stable addressing, transclusion, and the
product-agnostic separation of content from presentation, the emphasis
on the _page_ as the basic unit of content can be subjected to further
scrutiny. The long-term goal of this project is a structure with
smaller nodes and many more links between them, something I am calling
_dense hypermedia_.

## How it Works

Because the focus of `Intertwingler` is a corpus of interrelated Web
resources marked up just so, rather than presenting as an on-line Web
application, it instead both consumes and emits a set of static
files. It currently expects a directory of (X)HTML and/or Markdown as
its document source, along with one or more serialized RDF graphs
which house the metadata.

### Inputs

The files in the source tree can be named in any way, however, in the
metadata, they are expected to be primarily associated with UUIDs,
like the following (complete) entry:

```turtle
@prefix owl:  <http://www.w3.org/2002/07/owl#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix xhv:  <http://www.w3.org/1999/xhtml/vocab#> .
@prefix dct:  <http://purl.org/dc/terms/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix bibo: <http://purl.org/ontology/bibo/> .
@prefix bs:   <http://purl.org/ontology/bibo/status/> .
@prefix ci:   <https://privatealpha.com/ontology/content-inventory/1#> .

<urn:uuid:b5c16511-6904-4130-b4e4-dd553f31bdd8>
    dct:abstract "Yet another slapdash about-me page."@en ;
    dct:created "2019-03-11T08:14:10+00:00"^^xsd:dateTime ;
    dct:creator <urn:uuid:ca637d4b-c11b-4152-be98-bde602e7abd4> ;
    dct:hasPart <https://www.youtube.com/embed/eV84dXJUvY8> ;
    dct:modified "2019-05-17T05:08:42+00:00"^^xsd:dateTime,
        "2019-05-17T18:37:37+00:00"^^xsd:dateTime ;
    dct:references <urn:uuid:01a1ad7d-8af0-4ad7-a8ec-ffaaa06bb6f1>,
        <urn:uuid:0d97c820-8929-42a1-8aca-f9e165d8085e>,
        <urn:uuid:1ae2942f-4ba6-4552-818d-0e6b91a4abee>,
        <urn:uuid:97bd312d-267d-433b-afc7-c0e9ab3311ed>,
        <urn:uuid:99cc7d73-4779-4a7c-82de-b9f08af85cf8>,
        <urn:uuid:a03bf9b5-56e6-42c7-bcd9-37ea6ad6a3d1>,
        <urn:uuid:afe5a68a-a224-4be9-9fec-071ab55aa70d>,
        <urn:uuid:d74d7e24-a6d6-4f49-94e8-e905d317c988>,
        <urn:uuid:ef4587cd-d8c6-4b3d-aa01-d07ab49eda4f> ;
    dct:replaces <urn:uuid:1944f86f-cafb-42f7-bca3-ad518f9b6ec7>,
        <urn:uuid:d30a49d7-71ed-4355-bc44-4bbe3d90e000> ;
    dct:title "Hello, Internet"@en ;
    bibo:status bs:published ;
    a bibo:Note ;
    xhv:alternate <urn:uuid:4a13ab5a-67a5-4e1a-970f-3425df7035bb>,
        <urn:uuid:c2f57107-9f79-4f13-a83c-4c73448c1c0b> ;
    xhv:bookmark <urn:uuid:c2f57107-9f79-4f13-a83c-4c73448c1c0b> ;
    xhv:index <urn:uuid:4a13ab5a-67a5-4e1a-970f-3425df7035bb> ;
    xhv:meta <urn:uuid:47d52b80-50df-4ac4-a3f3-941c95c1aa14> ;
    owl:sameAs <https://doriantaylor.com/breaking-my-silence-on-twitter>,
        <https://doriantaylor.com/person/dorian-taylor>,
        <https://doriantaylor.com/what-i-do>,
        <https://doriantaylor.com/what-i-do-for-money> ;
    foaf:depiction <https://doriantaylor.com/file/form-content-masked;desaturate;scale=800,700> ;
    ci:canonical <https://doriantaylor.com/hello-internet> ;
    ci:canonical-slug "hello-internet"^^xsd:token ;
    ci:indexed false .
```

> Another program generates these entries from version-controlled
> source trees. It was written over a decade ago in Python, intended
> to handle the Mercurial version control system. Eventually it will
> be switched to Git, rewritten in Ruby, and made part of `Intertwingler`.

### Outputs

The library has methods to generate the following kinds of file:

* (X)HTML documents, where file inputs are married with metadata
* Special indexes, generated completely from metadata
* Atom feeds, with various partitioning criteria
* Google site maps,
* Apache rewrite maps.

All files are generated in the target directory as their canonical
UUID and a representative extension,
e.g. `b5c16511-6904-4130-b4e4-dd553f31bdd8.xml`. Human-readable URIs
are then overlaid onto these canonical addresses using rewrite
maps. This enables the system to retain a memory of address-to-UUID
assignments over time, with the eventual goal to eliminate 404 errors.

Documents themselves are traversed and embedded with metadata.
Wherever possible, links are given titles and semantic relations, and
a list of backlinks from all other resources is constructed. There are
modules for generating various elements in the `<head>` to accommodate
the various search and social media platforms. The strategy is to hold
the master data in the most appropriate RDF vocabularies, and then
subsequently map those properties to their proprietary counterparts,
e.g., Facebook OGP, Schema.org (Google), and Twitter Cards.

> The latter representations tend to cut corners with various
> datatypes and properties; it's much easier to go from strict RDF to
> a platform-specific schema than it is to go the other way around.

#### Private content

While a fine-grained access control subsystem is decidedly out of
scope, `Intertwingler` has a rudimentary concept of _private_ resources. A
resource is assumed to be "private" unless its `bibo:status` is set to
`bibo:status/published`. A document may therefore have both a public
and private version, as the private version may have links to and from
other documents which themselves are not published. Links to private
documents get pruned from published ones, so as not to leak their
existence.

### Running

I endeavour to create a command-line interface but the _library_
interface is currently not stable enough to warrant it. The best way
to use `Intertwingler` for now is within an interactive `pry` session:

```bash
~$ cd rdf-sak
~/rdf-sak$ pry -Ilib
[1] pry(main)> require 'intertwingler'
=> true
[2] pry(main)> ctx = Intertwingler::Context.new config: '~/my.conf'
=> <Intertwingler::Context ...>
[3] pry(main)> doc = ctx.visit 'some-uri'
=> <Intertwingler::Context::Document ...>
[4] pry(main)> doc.write_to_target
```

A YAML configuration file can be supplied in lieu of individual
parameters to the constructor:

```yaml
base: https://doriantaylor.com/
graph: # RDF graphs
  - ~/projects/active/doriantaylor.com/content-inventory.ttl
  - ~/projects/active/doriantaylor.com/concept-scheme.ttl
source: ~/projects/active/doriantaylor.com/source
target: ~/projects/active/doriantaylor.com/target
private: .private     # private content, relative to target
transform: /transform # XSLT stylesheet attached to all documents
```

### Deployment

Deploying the content can be done with `rsync`, although it requires
certain configuration changes on the server in order to work properly.

In Apache in particular, rewrite maps need to be defined in the main
server configuration:

```apache
# one need not place these in the document root but it's handy for rsync
RewriteEngine On
RewriteMap rewrite  ${DOCUMENT_ROOT}/.rewrite.map
RewriteMap redirect ${DOCUMENT_ROOT}/.redirect.map
RewriteMap gone     ${DOCUMENT_ROOT}/.gone.map

# then we just add something like this to prevent access to them
<Files ".*.map">
Require all denied
</Files>
```

The rest of the configuration can go in an `.htaccess`:

```apache
# this redirects to canonical slugs
RewriteCond "%{ENV:REDIRECT_SCRIPT_URL}" ^$
RewriteCond %{REQUEST_URI} "^/*(.*?)(\.xml)?$"
RewriteCond ${redirect:%1} ^.+$
RewriteRule ^/*(.*?)(\.xml)?$ ${redirect:$1} [L,NS,R=308]

# this takes care of terminated URIs
RewriteCond %{REQUEST_URI} ^/*(.*?)(\..+?)?$
RewriteCond ${gone:%1} ^.+$
RewriteRule .* - [L,NS,G]

# this internally rewrites slugs to UUIDs
RewriteCond "%{ENV:REDIRECT_SCRIPT_URL}" ^$
RewriteCond %{REQUEST_URI} ^/*(.*?)(\..+?)?$
RewriteCond ${rewrite:%1} ^.+$
RewriteRule ^/*(.*?)(\..+?)?$ /${rewrite:$1} [NS,PT]

# we can do something perhaps more elaborate to route private content
SetEnvIf Remote_Addr "31\.3\.3\.7" PRIVATE

# this splices in the private content if it is present
RewriteCond %{ENV:PRIVATE} !^$
RewriteCond %{DOCUMENT_ROOT}/.private%{REQUEST_URI} -F
RewriteRule ^/*(.*) /.private/$1 [NS,PT]
```

Configurations for other platforms can be figured out on request.

## Documentation

API documentation, for what it's worth at the moment, can be found [in
the usual place](https://rubydoc.info/github/doriantaylor/rb-intertwingler/main).

## Installation

For now I recommend just running the library out of its source tree:

```bash
~$ git clone git@github.com/doriantaylor/rb-intertwingler.git intertwingler
~$ cd intertwingler
~/intertwingler$ bundle install
```

## Contributing

Bug reports and pull requests are welcome at
[the GitHub repository](https://github.com/doriantaylor/rb-intertwingler).

## Copyright & License

©2018-2023 [Dorian Taylor](https://doriantaylor.com/)

This software is provided under
the [Apache License, 2.0](https://www.apache.org/licenses/LICENSE-2.0).
