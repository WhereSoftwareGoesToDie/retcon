Retcon laws
===========

This document briefly describes the laws, regulations, rules and/or common
courtesies which are required to hold of various bits of retcon. We don't just
call them all laws because, while they are nice properties to have, they may
not necessarily be *required*.

Entity
======

A retcon installation can synchronise multiple types of data between multiple
systems. Each type of data -- customer details, for example -- is a retcon
*entity*.

Each entity has the following:

- A "schema" defining the data structure.

- A merger, which describes the way to merge several values of the entity.

Laws
----

The laws for a merger are those that you'd expect:

- Merging with no changed inputs is identity:

        merge a a a == a

- Merging with a single change in a single field does what you'd expect:

        merge a a b == b

- Merging with multiple changes in multiple, disjoint fields does what you'd
  expect:

        merge a b c == d

   (Which is to say that the set of changed fields in the result is the union
of the set of changed fields in each source, and that the value for each such
field in the result is that of the source in which the field changed).

    Something like the following:

	\forall o, a, b, c \in Entity. changed(o,c) = changed(o,a) \union changed(o,b)

	\forall o, a, b, c \in Entity. changed(o,c) - changed(o,a) = changed(o,b)

	\forall o, a, b, c \in Entity. changed(o,c) - changed(o,b) = changed(o,a)

	\forall o, a, b, c \in Entity, f \in Fields. f \in changed(o,a) -> a.f == c.f

	\forall o, a, b, c \in Entity, f \in Fields. f \in changed(o,b) -> b.f == c.f

Data sources
============

A *data source* describes an external system with which retcon synchronises one
or more types of data. Each data source has the following:

- A local endpoint, to which the remote system can deliver events.

- A remote endpoint, from which retcon can send and/or retrieve data.

- An [Iso][] between an *entity* and the data source's format.

[Iso]: https://hackage.haskell.org/package/lens-1.2/docs/Control-Lens.html#t:Iso

Laws
----

The laws for isomorphisms as documented in the [haddock documentation][Iso]
should apply to a data source.

It should probably be noted that the notion of equality under which the
isomorphism applies is unlikely to be the standard equality from, e.g., `deriving (Eq)`.

System
======


