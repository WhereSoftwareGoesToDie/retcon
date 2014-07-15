% Retcon laws

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

- A function `ident` to derive a unique identifier from an object.

- A merger, which describes the way to merge several values of the entity.

Laws
----

It is an error to merge objects with different identifiers:

$ident(i_1) \neq ident(i_2) \rightarrow merge(i_1, i_2) = \bot$

Empty or missing objects are ignored when merging:

$merge(i_1, ..., i_n) = merge(\{i \| i \leftarrow \{i_1,...,i_n\}, i \neq \emptyset \})$

Merging a single object is identity:

$merge(i_1) = i_1$

Merging identical objects is identity:

$i_1 = i_2 = ... = i_n \rightarrow merge(i_1, i_2, ..., i_n) = i_1$

Merging with a single changed object

    merge a a b == b

Merging with multiple changes in multiple, disjoint fields does what you'd
expect:

    merge a b c == d

(Which is to say that the set of changed fields in the result is the union of
the set of changed fields in each source, and that the value for each such
field in the result is that of the source in which the field changed).

Something like the following:

$\forall o, a, b, c \in Entity. changed(o,c) = changed(o,a) \cup changed(o,b)$

$\forall o, a, b, c \in Entity. changed(o,c) \setminus changed(o,a) = changed(o,b)$

$\forall o, a, b, c \in Entity. changed(o,c) \setminus changed(o,b) = changed(o,a)$

$\forall o, a, b, c \in Entity, f \in Fields. f \in changed(o,a) \rightarrow a.f = c.f$

$\forall o, a, b, c \in Entity, f \in Fields. f \in changed(o,b) \rightarrow b.f = c.f$

Data sources
============

A *data source* describes an external system with which retcon synchronises one
or more types of data. Each data source has the following:

- A local endpoint, to which the remote system can deliver events. This is
a REST-ish API endpoint, an executable command, etc.

- A remote endpoint, from which retcon can send and/or retrieve data. This is
a REST-ish API endpoint, an executable command, etc.

- An [Iso][] between an *entity* and the data source's format.

[Iso]: https://hackage.haskell.org/package/lens-1.2/docs/Control-Lens.html#t:Iso

Laws
----

The laws for isomorphisms as documented in the [haddock documentation][Iso]
should apply to a data source.

It should probably be noted that the notion of equality under which the
isomorphism applies is unlikely to be the standard equality from, e.g.,
`deriving (Eq)`.

System
======

The system must handle at least the following cases:

- An entity is recieved from one source, but is unknown to the other sources.

- An entity is recieved from one source, is unknown to retcon, but is known to
other sources.

- An entity is received from multiple sources.

The system has a set of objects which should be synchronised:

$i_1, i_2, ..., i_{n}$

The output should be a merged object.

$o$

All fields which agree in all inputs are just copied:

$\forall f. sizeof(\{i.f \| i \leftarrow i_1...i_n\}) = 1 \rightarrow o.f = i_1.f$


