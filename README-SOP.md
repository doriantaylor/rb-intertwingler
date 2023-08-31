# Intertwingler — An Engine for Dense Hypermedia

`Intertwingler` is an _engine_, very much like
[WordPress](https://www.wordpress.org/) is an engine: you use it to
make websites. You can think of `Intertwingler`, at least _this_
implementation of it, as a _demonstrator_ for the kind of
infrastructure necessary to make the Web do genuine _dense
hypermedia_.

The way to understand _dense_ hypermedia is to contrast it with what
the Web is off the shelf, which is _sparse_ hypermedia: big clunky
_pages_ with not a lot of links, and what links _do_ exist are
sequestered into regions like navigations and other UI. What we want
instead are smaller, more _composable_ units, the mechanism of
composition being — what will end up being a much greater density of —
ordinary links. The effect we are after with `Intertwingler` is to
_pulverize_ Web content, dramatically increasing its _addressability_.
Not only does this afford practical benefits like content reuse, but
new affordances for software tools and creative expression.

# Strategy

The main problem `Intertwingler` has to solve, then, is the fact that
links on the Web are extremely _brittle_. The reason _why_ links on
the Web are brittle is because it's very cheap to change the URL of a
Web resource, and very _expensive_ to change all the places where that
URL is _referenced_. `Intertwingler` solves this problem the following
way:

* It stores _links_ (i.e., referent-reference pairs✱) as first-class objects,
* It assigns every resource a _canonical identifier_ that doesn't budge,
* It _overlays_ human-friendly address components (slugs) on top,
* It _remembers_ prior values for these address components if you change them,
* It uses a custom _resolver_ to do everything in its power to match a
  requested URL to _exactly one_ resource,
* It also has a mechanism for the principled handling _parametrized_
  and _derived resources_, maintaining a registry of parameter names,
  syntaxes, semantics, and other metadata.

`Intertwingler` accomplishes all this by bringing your organization's
entire address space (i.e., every Web URL under every domain you control)
under its management.

> ✱ Actually, `Intertwingler` stores links as _triples_ where the
> third element is the kind of link it is. More on this later.

Also packaged with the `Intertwingler` demonstrator are the means for
creating websites with dense hypermedia characteristics:

* A file system handler, for transition from legacy configurations
* A [content-addressable store]() handler, for bulk storage of _opaque
  resources_
* A markup generation handler, for rendering _transparent resources_
* Mutation handlers (e.g. `PUT` and `POST`) for both opaque and
  transparent resources
* A set of _transforms_ for manipulating _representations_

# Concepts

## (Information) Resource

An information resource is a _relation_ between one or more
_identifiers_ (in this case URIs) and one or more _representations_. A
familiar type of information resource is a _file_, which has exactly
one representation and usually one, but possibly more than one
identifier (file name/path). Web resources have an additional
dimension, which is the _request method_ or _verb_ with which the
resource was requested.

## Representation

A representation (of an information resource on the Web) is a literal
sequence of bytes (octets) that represents the given information
resource. Representations can vary by media type, natural language,
character set, compression, and potentially many other dimensions.

## Opaque Resource

An _opaque_ resource is named such because the enclosing information
system does not need to "see into" it. An opaque resource _may_ have
more than one representation, but one representation will always be
designated as canonical.

## Transparent Resource

A _transparent_ resource is the complement of an opaque resource: the
enclosing information system can, and often _must_, "see into" its
structure and semantics. Since the canonical representation of a
transparent resource resides only in live working memory, all
serializations (that neither discard information nor make it up) are
considered equivalent.

## HTTP(S) Transaction

An HTTP(S) transaction refers to the process of a client issuing a
single request to a server, and that server responding in kind. In
other words, a single request-response pair.

## Handler

An `Intertwingler` handler is a microservice with certain
characteristics. All handlers are designed to be run as stand-alone
units for bench testing and system segmentation. A handler responds to
at least one request method for at least one URI. Handlers have a
_manifest_ that lists the URIs, request methods, parameters, content
types, etc. under their control. This enables the `Intertwingler`
engine to perform preemptive input sanitation, and efficiently route
requests to the correct handler.

## Engine

The `Intertwingler` _engine_ is a special-purpose handler that
marshals all other handlers and transforms, resolves URIs, and routes
requests to handlers. This is the part that faces the external network.

## Transform

A _transform_ is a special-purpose handler that encapsulates one or
more operations (each identified by URI) over a request body. As such,
transforms only respond to `POST` requests. Like all handlers,
transforms have lists of content types for each URI in their manifest
that they will both accept and emit. Transforms are configured to run
in a queue, with the output of one being fed into the input of the
next. Through its interaction with an HTTP message, a transform may
also trigger a _subsequent_ transform to be added to its own, or
another queue.

## Request Transform

A _request_ transform operates over HTTP requests. It can modify the
request's method, URI, headers, body (if present), or any
combination thereof.

## Response Transform

A _response_ transform operates over HTTP responses. Analogous to
request transforms, response transforms can manipulate the response
status, headers, body, or any combination thereof. _Un_like a request
transform, there are multiple queues for response transforms: an
early-run queue and a late-run queue, with an _addressable_ queue
sandwiched between them.

# Architecture

> figure goes here

## Handlers

Everything in `Intertwingler` is a handler, including the engine
itself. At root, a handler is a _microservice_ created in compliance
with the host language's lightweight Web server interface (in our case
with Ruby, [that would be Rack](https://github.com/rack/rack)).

A handler is intended to be only interfaced with using HTTP (or,
again, the Web server interface's approximation of it). That is, a
handler instance is a callable object that accepts a request object
and returns a response object. A handler is expected to contain at
least one URI that will respond to at least one request method.

## The `Intertwingler` Engine

The `Intertwingler` engine imagines itself one day turned into a
high-performance, stand-alone reverse proxy, with hot-pluggable
handlers (and by extension, transforms) that interface internally over
HTTP. That is the lens with which to view the design. It is meant to
be put at the edge of an organization's Web infrastructure and manage
the Web address space for all of the organization's DNS domains.

When an HTTP transaction occurs completely within the engine's process
space (i.e., it does not try to access handlers running in other
processes/engines), the engine has strategies to mitigate the amount
of extraneous parsing and serialization that would otherwise occur.

## `Intertwingler` Handler Manifests (In Progress)

Still in progress at the time of this writing is a finalized design
for handler _manifests_, though some details are certain. A manifest
is intended to advertise the set of URIs that a given handler will
respond to, along with:

* what request methods are recognized,
* what content types are available in the response,
* what URI query parameters are recognized, their data types,
  cardinality, etc.,
* what content types are accepted in requests (at least the ones that
  send body content)
* in the case of `POST`ed HTML forms
  (`application/x-www-form-urlencoded` and `multipart/form-data`
  types), parameter lists analogous to query parameters,
* etc…

The exact format of the manifest payload is still yet to be
determined. What is _known_ is that handler manifests will be
retrieved by the special `OPTIONS *` request, intended to address the
server (in this case microservice) directly rather than any one
particular resource it manages. Since the [HTTP
specification](https://datatracker.ietf.org/doc/html/rfc9110#section-9.3.7)
does not explicitly define semantics for any content in response to
`OPTIONS *`, we future-proof by only sending the manifest if a
[`Prefer:
return=representation`](https://datatracker.ietf.org/doc/html/rfc7240#section-4.2)
header is present in the request, in addition to the ordinary content
negotiation headers, `Accept` and so on.

## `Intertwingler` Transform Protocol

The transform protocol is inspired by [the FastCGI
specification](https://fastcgi-archives.github.io/FastCGI_Specification.html#S6.3),
and its use in server modules like Apache's
[`mod_authnz_fcgi`](https://httpd.apache.org/docs/2.4/mod/mod_authnz_fcgi.html). In
this configuration, the main server issues a subrequest to a FastCGI
daemon, and then uses the response, in this case, to determine if the
outermost request is authorized. The reasoning goes that this
behaviour can be generalized to ordinary HTTP (in our era of reverse
proxies, FastCGI is an extra step), as well as handle other concerns
in addition to authorization. (Indeed, FastCGI itself [also specifies
a _filter_
role](https://fastcgi-archives.github.io/FastCGI_Specification.html#S6.4),
but I have not seen a server module that can take advantage of it.)

A direct request to a transform looks like a `POST` to

### Addressable Transforms

## RDF-KV Protocol

## Content Inventory Ontology

## Transformation Functions Ontology

## Implementation Note

Parts of `Intertwingler`, notably the URI resolver, depend on a
[reasoner](https://en.wikipedia.org/wiki/Reasoner) to make inferences
about assertions in the database. In 2018, when I began working on
`Intertwingler`'s predecessor, `RDF::SAK`, the only workable
implementations of reasoners were in Java and Ruby. I chose Ruby
because it was easier for prototyping. My vision for `Intertwingler`,
though, is that it eventually has implementations in as many languages
as it can.

# Sponsorship

The bulk of the overhaul that transformed `RDF::SAK` into
`Intertwingler` was funded through a generous research fellowship by
the [Ethereum Foundation](https://ethereum.foundation/), through their
inaugural [Summer of Protocols](https://summerofprotocols.com/)
program. This was a unique opportunity for which I am sincerely
grateful. I would also like to thank [Polyneme
LLC](https://polyneme.xyz/) for their financial support and ongoing
interest in the project.

# Contributing

Bug reports and pull requests are welcome at
[the GitHub repository](https://github.com/doriantaylor/rb-intertwingler).

# Copyright & License

©2018-2023 [Dorian Taylor](https://doriantaylor.com/)

This software is provided under
the [Apache License, 2.0](https://www.apache.org/licenses/LICENSE-2.0).
