% The design of retcon
% Anchor Systems

This document describes the design of retcon at a relatively high level.
This will guide the implementation of retcon version one and will be
revised as we discover the limitations in it.

# Introduction

The retcon system attempts to synchronise data shared between multiple
systems. It does this by attempting to identify data changes which
happen in each external system, composing these changes to form a single
set of changes and then applying them to each of the external systems.

This is implemented in such a way that changes can be merged according
to application specific policies, that changes can be tracked over time,
and that users can perform manual intervention for changes that can't be
handled automatically.

# System structure

The retcon system is divided into a number of components:

- A core library contains types and algorithms to extract, merge, and
  apply changes to collections of documents.

- *Data sources* encapsulate the interaction with specific external
  systems which manage a copy of the data to be synchronised.

- *Merge policies* implement the application-specific policy to be
  applied when merging changes from multiple sources.

- A core "do all the things" function which uses all of the above to implement
the operation of the system.

- One or more handlers which implement a specific user interface allowing
external systems and users to interact with retcon.

# Core library

The core library defines a number of data types and operations on those
types which implement the core functionality to merge changes between
"identical" documents shared between different systems.

The data types defined in the library include:

- The `Document` type represents the content to be synchronised between
systems.

- The `Diff` type describes the changes between one `Document` and another.
Values of this type take an arbitrary label which can be used by a merge policy
to help implement such strategies as, e.g., last writer wins or field-specific
source of truth.

- The `Operation` type describes a single change in a `Diff`. These values take
labels of the same type as the `Diff` that contains them.

- The `MergePolicy` type implements a specific policy which can be
  used to label a `Diff` and to merge labelled `Diff`s.

A number of operations on these types:

- Both `Diff` and `Operation` form functors, simplifying the manipulation of
  their labels.

- Generic `diff`, `mergeDiff`, and `applyDiff` operations act on `Document`,
`Diff`, and `Operation` values maintaining but largely ignoring their labels.

- `generateDiff :: MergePolicy l -> Document -> Document -> Diff l` uses these
to generate a diff between two documents labelled according to the supplied
merge policy.

- `mergeDiff :: MergePolicy l -> [Diff l] -> (Diff (), [Diff l])`
  uses a merge policy to merge a collection of diffs, generating a
  merged `Diff`, and a collection of left-over `Diff`s containing the
  operations which could not be merged.

- `findInitialDocument :: [Document] -> Document` inspects a collection
  of `Document`s and generates a new `Document` can serve as a "common
  ancestor" to each of them.

# Document

The `Document` type is a generic representation of tree-structured key/value
data. They are represented using a Trie-like data structure with sequences of
text values as the key rather than the more traditional sequences of
characters. They are represented in the system using a variant of the Rose tree
data structure with a `Map` of children instead of a list. This boils down to
something very much like:

````{.haskell}
data Trie k v = Node (Maybe v) (Map k (Trie k v))
newtype Document e = Document (Trie Text Text)
  deriving (...)
````

As some point we may want to assess the [list-tries][] package and consider
using it instead of our custom data type.

[list-tries]: http://hackage.haskell.org/package/list-tries

JSON documents can be converted to and from this type (assuming you like
converting the distinct value types in JSON to strings); but arbitrary
`Document`s are *not* necessarily representable as JSON.

This reasonably generic data structure allows us to implement some fairly
generic operations to generate and apply diffs between `Document`s.

## Initial documents

As mentioned, the algorithm described below requires an "initial" document
which serves as the starting point of the possibly-diverging changes to be
merged between the different data sources. The system is able to use the last
known state for documents that have been processed previously but it will have
to conjure a suitable for initial out of the whole cloth for brand new
documents.

There are several straightforward approaches to generating a document suitable
for use as the "initial" for a set of input documents:

- Just use the empty document; or

- Take the intersection of the input documents (i.e. the keys and values on
which they all agree).

It should be obvious that the empty document is prior to all other documents
and, likewise, that the intersection of a set of documents is prior to each of
the individual documents in that set.

While the former choice is simpler the latter can be expected to generate
smaller and less complex diffs, so the initial version of retcon will use it.

````{.haskell}
findInitialDocument :: [Document] -> Document
````

# Merge policy

Each merge policy consists of two parts:

- A function to extract a label value from a `Document` which will then
  be applied to the `Diff`s the policy will be applied to.

- A function to include some or all of the changes in a collection of `Diff`s
  (labelled according to the policy) and return a `Diff` containing all
  included changes and a list of "left-over" `Diff`s containing the excluded
  changes. The left-over `Diff`s will be empty if the policy was able to merge
  all changes.

A first approximation of the merge policy data type might be:

````{.haskell}
data MergePolicy l = MergePolicy
    { extractLabel :: Document -> l
    , mergeDiffs   :: [Diff l] -> (Diff l, [Diff l])
    }
````

The initial implementation will include several sample merge policies but will
not include combinators for composing more complex policies. These initial
policies include:

- Trivially exclude all changes.

- Trivially include all changes.

- Include only non-conflicting changes.

Later versions will include combinators such as:

````{.haskell}
-- | Compose two 'MergePolicy's
andThen :: MergePolicy a -> MergePolicy b -> MergePolicy (a,b)

-- | Apply a 'MergePolicy' on a specific document key only.
onField :: Key -> MergePolicy l -> MergePolicy l

-- | Accept all changes from a specific source.
trustSource :: Source -> MergePolicy Source

-- | Ignore all changes to a specific field.
dropField :: Key -> MergePolicy l

-- | Accept conflicting changes from the document with the highest
-- timestamp.
mostRecentBy :: Key -> MergePolicy String
````

# Entity

An entity encapsulates a collection of data sources and a merge policy which
together specify the system's operation on a particular type of data. An entity
will be represented in the systems as an instance of a type class.

The operations defined in the entity type class are:

- List the data sources which handle this type of data.

- Get the merge policy to apply when processing data of this type.

````{.haskell}

-- | Wrap any data source for the entity 'e' in an existenial type.
-- .
-- Values of 'SomeSource e' represent any data source for the entity 'e'.
data SomeSource e = forall s. DataSource e s => SomeSource (Proxy s)

class Entity e where
    -- | Data sources which handle this type of data.
    entitySources :: Proxy e -> [SomeSource e]

    -- | Merge policy for this type of data.
    entityPolicy :: Proxy e -> MergePolicy l
````

The types for which the class will be instantiated will be symbols (i.e.
type-level strings). This helps to ensure that they cannot be confused with
regular values while allowing us to make use some nice machinery provided by
the GHC libraries.

# Data sources

A data source encapsulates the set of actions required to interact with an
external system which stores data to be synchronised. Data sources are
represented in the system as an instance of a multi-parameter type class with
types representing both the entity (see below) and the data source.

The operations each data source supports are:

- Fetch a document with a specified foreign key.

- Set a document with a specified foreign key.

- Delete a document with a specified foreign key.

Each of these operations will have types like:

````{.haskell}

newtype Retcon a = ...

instance MonadIO Retcon where

class DataSource entity source where
    -- | Write a document to the foreign system, returning the foreign key
    -- for the document.
    setDocument :: Document entity
        -> Maybe (ForeignKey entity source)
        -> Retcon (ForeignKey entity source)

    -- | Retrieve a document from the foreign system.
    getDocument :: ForeignKey entity source
        -> Retcon (Document entity)

    -- | Delete a document from the foreign system.
    deleteDocument :: ForeignKey entity source
        -> Retcon ()
````

The data source types, like the entity types described above, will be symbols
(i.e. type-level strings).

These operations will operate in the `Retcon` monad, which will need to have
access to IO operations (probably through `MonadIO`); logging operations
(possibly via `MonadLogger`); and some specific operations to access the retcon
database lookup and cache tables.

# `runRetcon`

All of the details above are brought together in a single function which is
responsible for evaluating retcon's internal data stores, coordinating access
to external data stores, applying the core algorithm, logging changes, etc.

This will be be implemented using the type literals machinery described above
to match an entity and source from an incoming event description to a pair of
*entity* and *source* types as described above. These will be used to determine
the nature of the event (create, update, delete) by communicating with the
source, and then process it and propagate any changes to other sources.

````{.haskell}
runRetcon :: (String, String, String)
          -> Retcon ()
````

The logic of this function is relatively straightforward:

0. Resolve the event description to specific entity and data source types.

1. Identify the type of change being performed (if the source ID is new, it's
creation; if the source says the document doesn't exist, it's deletion;
otherwise it's an update) and the retcon key of the document to be updated
(allocating a new one, if required).

2. Fetch the matching document from all sources.

3. Find an initial document; either from the cache database table or, if there
isn't one, by generating one from the input documents as described above.

4. Build a diff from the initial document, the input documents, and the merge
policy using the algorithms described above.

5. Update the initial document with this diff and save it in the cache.

6. Update the input documents with this diff, and send them updated documents
back to their corresponding data sources.

This will be implemented in two functions:

- `runRetcon` performs the full process; and itself uses:

- `applyRetconChanges` which, given a set of changes, performs the process from
steps 5 and 6 above.

This division will allow future versions to implement additional features,
e.g., give human users to ability to cherry-pick changes which could not be
applied automatically and push them through.

Each step in this process can record useful information in logs and in
operational database tables. These details include:

- A retcon key and the last known corresponding initial document stored in
`retcon` with a compound primary key: `(entity_name, retcon_key)`.

- A foreign key identifying a document in a data source, stored in
`retcon_keys` with compound primary key `(entity_name, retcon_key, data_source)`.

- A record of the changes made (or not made) to each document (i.e. the merged
`Diff` and any unmerged changes) stored in `retcon_changes` with a compound
primary key `(entity_name, retcon_key)`.

- A record of the document in each data source after each change stored in
`retcon_history` with a compound primary key `(entity_name, retcon_key,
data_source, sequence)`.

# Handlers

The handlers are responsible for implementing the external interface to the
system. Each handler takes an `Entity` as a parameter along with some
implementation specific additional parameters.

Handlers will use a simple database-backed task queue to decouple their
interface from the processing which will take a while. Depending on the rate
limiting for some of the data source systems, it may be necessary to do crazy
things to coordinate access between different data sources which interact with
the same external system.

## Command-line Handler

The first handler to be developed will be for demonstration purposes and serves
as the first "proof of concept" release. This handled will consist of a simple
command line application which accepts arguments:

- An entity name
- A data source name
- A foreign key

This command line executable will make use of demonstration data sources which
store documents in a directory of JSON files.

## Snaplet Handler

Initially the only full-featured handler will be the Snap Framework snaplet
which implements a basic RESTful web API and user interface.

The basic interface will look something like:

- `GET /v1/:entity_name/:id?page=:n` to view history.

- `POST /v1/:entity_name/:source/:fkey` to initiate a new update for the entity
identified by the specified foreign key in the associated data source.

- `DELETE /v1/:entity_name/:source/:fkey` to initiate a delete for the entity
identified by the specified foreign key in the associated data source. This may
not be necessary: we can infer the operation type from identity of the issuing
source and the document we retrieve from it.

- `GET /v1/:entity_name/:id/:n` to inspect a past change.

- `POST /v1/:entity_name/:id/update` to manually apply one or more unmerged
  changes.

Putting the API version number into the path is terrible, but the other
solutions are also terrible while this, at least, is also easy.

