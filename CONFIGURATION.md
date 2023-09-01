# Getting Started

who knows man i'm just getting started myself

been so busy trying to get the guts to work i haven't really thought about the ux of this

i mean let's say you download and install this thing, then what

(a docker image is forthcoming btw)

anyway it's gonna be an empty thing so not really very interesting to most people for a while

# what _i_ will probably do once the damn thing is working

i already said [in the video](https://youtu.be/d5-lcvKfBM4) what the first things are:

* [my website](https://doriantaylor.com/)
* [the nature of software](https://the.natureof/software/)
* [the ibis tool](https://ibis.makethingsmakesense.com/)

## my website

this thing has a bunch of cruft on the file system which will stay there for the time being

excited to finally replace the fake scaled images that have been there for like 15 years with real ones

also excited to do a content inventory for once, basically bootstrap some content inventory ui

also scan my whole site for terminology, proper nouns etc, finally flesh out the audience modeling stuff

go back and look at the document stats and try to do something useful with it

oh how about do some friggin stretchtext

also noodle with that video stuff

## my client extranets

this is a no-brainer; get everything into content-addressable storage

## the nature of software

this one is a lot newer and can live completely in content-addressable storage

i am very interested in making an annotation system

plus also same indexes as my site: concepts, books, people/orgs

photo index/credits a big one

there is also a bunch of stuff with the original books that i'd like to do

## the ibis tool redux

this thing lives almost completely in the graph

this one will need the proxy handler for sure for external links

here is where most of the work on really dense graph-forward stuff is gonna be

definitely hook in [pm ontology](https://vocab.methodandstructure.com/process-model#)

definitely need a data entry interface for people/orgs too

# anyway so if you were gonna use this to absorb a website

say static site, like jekyll; wordpress or whatever i have no idea

i mean you could probably just load all the files into content-addressable storage, that'd be a good start

> here's the thing i've been thinking though: stuff like git literally *is* a content-addressable store with an index on top, it probably wouldn't be hard to make an adapter that just hoovers up git repositories and sticks their metadata in the graph such that you could perfectly recreate a git repository
>
> only wrinkle is [`Store::Digest`](https://github.com/doriantaylor/rb-store-digest) isn't smart enough to store metadata about functional relationships between objects which is what you would need to represent stuff without it going nuts with redundant data
>
> like if you have a general-purpose content-addressable store, like just a huge gaping maw where you chuck whatever blobs of data, especially if you're using it for cache too, it's gonna get real big real fast
>
> git in particular runs everything through `DEFLATE` no matter what it is and then stores that, but the hashes in the file names correspond to the *un*compressed data
>
> git also diffs the current version of every file against the last version, keeps the current version whole to be fast, and then just stores the diff for all previous versions, which it reconstitutes on the fly (or at least it did, i dunno what it does now)
>
> thing about `Store::Digest` (and really any other content-addressable store) is it knows what it has in it, but it doesn't know _why_; that's what the index layer is for
>
> so if you have two things in the store, a big object and a small object, and can say definitively that the small object is the same as the big object with a particular invertible function applied to it (`gzip(a) = gunzip(b)`), then you can delete the big object and just compute it when you need to
>
> now that gets trickier when a _third_ object is used as input: `diff(a, b) = c <=> patch(b, c) = a` because you will have to make goddamn certain you _never_ throw that part out
>
> easy enough to manage in git because everything in a git repository belongs to _it_. not the case for a general-purpose content-addressable store.
>
> worst thing for a content-addressable store is you have something in there and no idea why
>
> i.e., your graph has no record of the object at all
>
> can't delete it; something else might be using it
>
> anyway, strategies for de-chesterton-fencing a content-addressable store probably a decent idea
>
> inclined to partition it so stuff like cache doesn't get mixed in with non-cache; only thing about that is there *will* be collisions so sometimes you'll be storing the same object twice, although that's probably not nearly as bad as an ocean of runaway blobjects with no provenance
>
> i mean you'll be to scan them or whatever and make determinations on most of them but that is Work™ that somebody has to do
>
> another thing is you could have a definitely-cache flag when you insert an object: if the identical object is ever subsequently inserted with that flag off, then the flag stays off and never gets flipped back on no matter how many other times the object is reinserted with the flag on. hey that's actually kind of a not bad idea
>
> could even have some automatic capacity management and LRU policy or whatever

aaanyway that was my digression on content-addressable stores, back to the absorbing a website business

thing is, `Intertwingler` doesn't really have much to offer you if all you have is a regular website, i mean besides the whole url stuff and i guess the transforms on pages and images etc are nothing to sneer at

that and i guess the potential to make things get real weird

like what do you make when you don't have to think in terms of _pages_ anymore

very PKM-ey but also kinda not

makes me think about investing in some kind of sparse-to-dense onboarding process

that might require me finishing [Loupe](https://vocab.methodandstructure.com/loupe#) though

anyway i'll pick this up later, ta
