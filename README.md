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

# History

This module began life as a thing called `RDF::SAK`, or the Content
Swiss Army Knife. After positing the notion of [content management
_meta-system_](https://doriantaylor.com/content-management-meta-system),
I made an initial cut in 2018, to support some work I was doing
for a client. It quickly became a breadboard and/or test environment
for developing what I just referred to as "good ideas about Web
content", which I ultimately realized as a static website generator,
in the same vein as [Jekyll](https://jekyllrb.com/) or
[11ty](https://www.11ty.dev/). Since most of my work was around
durable addressing and embedded metadata, a live engine was not a high
priority. Priorities have since changed.

Five years prior to creating `RDF::SAK`, in 2013, I designed a
protocol to aid in the development of Semantic Web applications called
[RDF-KV](https://doriantaylor.com/rdf-kv). It provides an
extraordinarily simple mechanism for getting RDF statement deltas
(i.e., commands to add and/or remove statements) from a Web client to
a graph database on the server, with a minimum of moving parts (i.e.,
no JavaScript). To test the implementation, I needed a complete
vocabulary, so I used [the IBIS
vocabulary](https://vocab.methodandstructure.com/ibis#) I had written
a year earlier, and created a tool called
[`App::IBIS`](https://github.com/doriantaylor/p5-app-ibis). This tool
[turned out to be useful](https://ibis.makethingsmakesense.com/), but
limited in its capacity for expansion, because it was written in Perl,
which does not have an RDF reasoner, a piece of software that is both
highly abstract and difficult to write (a rudimentary yet satisficing
one of which Ruby happens to possess). Without a reasoner, `App::IBIS`
was much too sclerotic to develop very far past the initial prototype.

> `App::IBIS` is unambiguously a dense hypermedia application, and
> developing it meant generating a lot of markup that was thick with
> embedded RDFa metadata. This led me to create a family of terse
> markup generators
> ([Perl](https://metacpan.org/pod/Role::Markup::XML),
> [Ruby](https://github.com/doriantaylor/rb-xml-mixup),
> [JavaScript](https://github.com/doriantaylor/js-markup-mixup)) with
> some nice advantages over their incumbents. Working extensively with
> RDFa helped develop technique for reusing the embedded metadata for
> directing presentation markup, as well as providing the basis for
> CSS selectors in both HTML and SVG.

The plan for `RDF::SAK` was always to turn it into a live engine that
could be accessed and updated online. Nevertheless, due to its
decidedly organic origins, it was (and still very much is) a huge mess
that needed (and still needs) several rounds of intense refactoring. I
began this work in December of 2021 but suspended it a few weeks later
due to an injury, and this refactor had to take a back seat to other
priorities for most of 2022. I decided early this year (2023) I was
going to complete the overhaul no matter what, which, it later turned
out, the [Summer of Protocols](https://summerofprotocols.com/)
organizers have graciously elected to sponsor.

> I have also gotten some interest, beginning last year, in the use of
> IBIS as a planning tool. Part of the impetus for getting `RDF::SAK`
> to a state where it can take over from the torpid `App::IBIS` is
> that I have an [entire project planning
> framework](https://vocab.methodandstructure.com/process-model#)
> based on
> [IBIS](https://en.wikipedia.org/wiki/Issue-based_information_system)
> for which any tooling will need a more flexible substrate. I am also
> grateful for my clients who support this development.

This project also represents a confluence of over two decades of work
on the Web. What is now called `Intertwingler` closely tracks a design
I sketched out back in 2006 for a "Web substrate", with the intent of
decoupling functionality that generates _content_ from that which
merely _manipulates_ it, noting that separating the two would result
in both ending up markedly simpler. This design drew on technique I
had developed at my first tech job back in 1999.

I had had a personal site from 1998 to about 2003, and by 2008 I was
ready to put one up again. It was around this time I had realized that
one could use XSLT (which I had picked up in 2001) to transform
(X)HTML into _itself_, meaning it could be used _in the browser_ as an
extremely lazy Web template engine that does its page composition at
the _network_ level. This means you can mix content sources on the
server side, which can be any mixture of static or dynamic content
written in any programming language or framework you like, since all
communication happens using standard protocols and data formats. This
is a technique I have used and expanded on for the last 15 years.

> Specifically, I have written [an RDFa query
> engine](https://github.com/doriantaylor/xslt-rdfa) (2016) and
> [seamless transclusion
> mechanism](https://github.com/doriantaylor/xslt-transclusion)
> (2018). While XSLT is still actively developed and used in
> publishing _outside_ the Web, I am somewhat concerned about its
> future as a native capability in the browser. XML is irredeemably
> out of fashion in mainstream Web circles (despite ostensibly having
> been reinvented as "custom elements"), but in my opinion XSLT is,
> for reasons too numerous to articualate here, unparalleled in its
> ability to manipulate markup — which is why I continue to use
> it. Indeed, [a compact, easier-to-type XSLT
> syntax](https://doriantaylor.com/file/xslt-mockup) similar to
> [RelaxNG](https://www.oasis-open.org/committees/relax-ng/compact-20021121.html)
> may be enough to renew interest in it. I should note that the use of
> XSLT is not strictly necessary; you could probably acccomplish the
> same effect using (a lot more) JavaScript.

When I went to put the site up in 2008, I was keenly interested in
creating _dense hypermedia_ (though I would coin that term much
later). I wanted to convey information without forcing the audience to
read any more than they had to. The constraints were:

1. that no page should be so long that it scrolls (on an average
   desktop monitor),
2. any digressions, footnotes or parenthetical remarks would be
   hived off to their own page and linked,
3. no `404` errors — URLs do not get exposed to the wild until there
   is something at that location.

These constraints made it very difficult to operate. For one, having
to stop and think up a URL for a page because you happened to digress
a bit in the page you were just writing (which, since URLs tend to
track with titles, ultimately meant coming up with a title) is a
jarring context switch of considerable cognitive overhead. Moreover,
this would an exponential jump in workload, because the digressions
would invariably generate their own digressions, and since nothing
could ship until all of it was complete (or at least roughed in), it
would take forever to do anything. Notwithstanding, I got about 40
pages into what I called a [Resource Handling and Representation
Policy](https://doriantaylor.com/policy/resource-handling-and-representation)
done in this style, before I gave up and decided to just write essays.

> This policy manual actually worked out a number of design decisions
> that are still perfectly valid fifteen years later, and have made
> their way into the `Intertwingler`.

The experience of writing this policy promptly moved me to start
thinking about a mechanism that would enable information resources to
be stored under canonical identifiers (specifically
[UUIDs](https://datatracker.ietf.org/doc/html/rfc4122) and
[cryptographic hashes](https://datatracker.ietf.org/doc/html/rfc6920))
that traded off legibility for being _durable_, and overlay
human-friendly addresses on top. It would likewise track changes to
these addresses, try to fix errors, and ensure _all_ URLs on a domain
that have _ever_ been exposed to the wild route to _something_. I got
this subsystem to finally work in `RDF::SAK` in 2019, and it remains
present in `Intertwingler`.

> The need to solve the same problem for fragment identifiers led me
> ([apparently back in
> 2012](https://metacpan.org/pod/Data::UUID::NCName)) to invent [a
> compact UUID
> representation](https://datatracker.ietf.org/doc/html/draft-taylor-uuid-ncname)
> which I am (slowly) trying to get graduated into an RFC.

The state mechanism for this URL naming history is a [content
inventory
vocabulary](https://vocab.methodandstructure.com/content-inventory#) I
began roughing in around 2010. This was originally conceived as a data
storage and exchange format for website content inventories, but has
since become a catch-all, including a structure for holding
quantitative metrics to help content strategists apprehend the
contours of websites (developed in 2011), and a sophisticated
set-theoretic mechanism for modeling audiences, and pairing (or
_anti_-pairing) them with content (2019).

As I mentioned above, the `Intertwingler` engenders an ultimately
_simpler_ system by decoupling the _generation_ of content from its
subsequent downstream _manipulation_. I had sketched out how this was
going to work as far back as 2008, along with a couple ill-fated
prototypes. It wasn't until a project in 2020 though that I completed
a [Transformation Functions
Ontology](https://vocab.methodandstructure.com/transformation#)
(started in 2014) and concomitant infrastructure that would resolve
transformation functions, apply them to content, and cache their
results. This infrastructure depends on earlier work on
content-addressable stores ([Perl in
2013](https://github.com/doriantaylor/p5-store-digest), [Ruby in
2019](https://github.com/doriantaylor/rb-store-digest)), that use
[RFC6920](https://datatracker.ietf.org/doc/html/rfc6920) `ni:` URIs,
making them compatible with RDF.

> On a similar tack, I also explored [creating a registry for query
> parameters](https://metacpan.org/pod/Params::Registry::Template)
> (2015), with the triple purpose of parsing and validating input,
> generating round-trip-stable query strings, and facilitating the
> creation of organization-wide policy for the names, types, and
> semantics of query parameters. I do not currently have a Ruby port
> of this particular software, but I will probably eventually make
> one, along with an RDF vocabulary as an extension to the
> transformation one for expressing the configuration.

And so, the `Intertwingler` is an odyssey spanning over two decades,
which is fitting, since its ultimate goal is to retrofit the Web with
the capabilities of its hypermedia predecessors.

# Anatomy

As I have hopefully communicated, the `Intertwingler` is in a state of
absolute disarray, still undergoing its metamorphosis from the
less-ambitious and much more organic `RDF::SAK`. I have tried to
outline some of the more important modules; those I have left out are
either not very interesting (such as the generated vocabularies under
[`Intertwingler::Vocab`](lib/intertwingler/vocab.rb) or slated for
removal. Checkmarks on the bullet points indicate the modules are
complete enough to use.

## Essential components

* [X] [`Intertwingler::GraphOps`](lib/intertwingler/graphops.rb) is a
      mix-in that extends `RDF::Queryable` with the all-important
      inferencing operations.
* [X] [`Intertwingler::Resolver`](lib/intertwingler/resolver.rb) is
      the _also_-all-important URI resolver.
* [ ] [`Intertwingler::Representation`](lib/intertwingler/representation.rb)
      is a cheap knockoff of a
      [monad](https://en.wikipedia.org/wiki/Monad_(functional_programming))-like
      structure that enables parsed, in-memory representations of content to
      persist across successive transformations, so they don't get
      unnecessarily serialized and reparsed.
* [ ] [`Intertwingler::Document`](lib/intertwingler/document.rb)
      houses (mostly) context-free markup generation (though may be
      dissipated into other modules).

## Engine components

Everything in the engine, including the engine itself, is an
[`Intertwingler::Handler`](lib/intertwingler/handler.rb) that accepts
a `Rack::Request` and returns a `Rack::Response`, plus an embedded
adapter so it can be used directly as a stand-alone Rack application.

### Handler

* [ ] [`Intertwingler::Handler::Generated`](lib/intertwingler/handler/generated.rb)
* [ ] [`Intertwingler::Handler::FileSystem`](lib/intertwingler/handler/filesystem.rb)
* [ ] [`Intertwingler::Handler::CAS`](lib/intertwingler/handler/cas.rb)
* [ ] [`Intertwingler::Handler::Proxy`](lib/intertwingler/handler/proxy.rb)

### Transforms

An [`Intertwingler::Transform`](lib/intertwingler/transform.rb) is a
specialized [`Intertwingler::Handler`](lib/intertwingler/handler.rb)
that responds to `POST` requests to a single URI. I am still working
out the details of a protocol but the general sense is you `POST` a
payload and it returns the transformed payload back. When a transform
is in the engine, this happens automatically by subrequest. Shortcuts
are in place (via
[`Intertwingler::Representation`](lib/intertwingler/representation.rb))
for transformations that happen in the same process space.

* [ ] [`Intertwingler::Transform`](lib/intertwingler/transform.rb) is
      the base which also includes
      `Intertwingler::Transform::Harness`, which probably needs some
      to bring it up to par with the rest of the system.
* [ ] `Intertwingler::Transform::Tidy` for sanitizing/normalizing HTML
      via [`tidy`](https://www.html-tidy.org/).
* [ ] `Intertwingler::Transform::Nokogiri` for transforming HTML/XML
      via `Nokogiri`;
  * [ ] Transform Markdown into HTML
  * [ ] Turn HTML into XHTML and vice versa
  * [ ] Strip comments
  * [ ] Reindent markup
  * [ ] Repair/"rehydrate" RDFa
    * [ ] Normalize RDFa prefixes
  * [ ] Mangle `mailto:` addresses (by whatever house style) to prevent spam
  * [ ] Insert stylesheet references
  * [ ] Rewrite links
  * [ ] Add backlinks
  * [ ] Add secondary links (e.g. glossary entries)
  * [ ] Add (e.g.) Amazon affiliate codes to `amazon.com` links
  * [ ] Add social media metadata (Google, Facebook, Twitter, whoever…)
* [ ] `Intertwingler::Transform::Vips` for images via `Vips`.
  * [ ] Crop images
  * [ ] Resize (downward only due to potential denial of resources)
  * [ ] Desaturate
  * [ ] Posterize
  * [ ] etc…

## "Offline" components


* [ ] [`Intertwingler::CLI`](lib/intertwingler/cli.rb) is the command
      line harness.
* [ ] `Intertwingler::Static` is (to be) an "end cap" on the engine
      that performs the legacy static site generator function.
* [ ] [`Intertwingler::DocStats`](lib/intertwingler/docstats.rb)
      gathers statistics about a corpus of documents.
* [ ] [`Intertwingler::NLP`](lib/intertwingler/nlp.rb) is a _very_
      rudimentary natural language processor for extracting terminology
      (jargon, acronyms, proper nouns etc.) from a corpus of documents.
* [ ] [`Intertwingler::URLRunner`](lib/intertwingler/urlrunner.rb) is
      planned as a generic crawler (eventually with some kind of
      `Handler` interface) for resolving link previews.

# Documentation

API documentation, for what it's worth at the moment, can be found [in
the usual place](https://rubydoc.info/github/doriantaylor/rb-intertwingler/main).

# Installation

For now I recommend just running the library out of its source tree:

```bash
~$ git clone git@github.com/doriantaylor/rb-intertwingler.git intertwingler
~$ cd intertwingler
~/intertwingler$ bundle install
```

# Contributing

Bug reports and pull requests are welcome at
[the GitHub repository](https://github.com/doriantaylor/rb-intertwingler).

# Copyright & License

©2018-2023 [Dorian Taylor](https://doriantaylor.com/)

This software is provided under
the [Apache License, 2.0](https://www.apache.org/licenses/LICENSE-2.0).
