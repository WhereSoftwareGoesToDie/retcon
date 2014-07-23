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

- A generic handler -- parameterised by a collection of data sources and
  a merge policy -- coordinates the interaction between the data
  sources, change algorithms, etc.

# Core library

The core library defines a number of data types and operations on those
types which implement the core functionality to merge changes between
"identical" documents shared between different systems.

The data types defined in the library include:

- The `Document` type in which the content to be synchronised is
  expressed. This will essentially be a nested String/String map a la
  JSON, etc.

- The `Diff` type which describes the changes between one `Document` and
  another. Values of this type take an arbitrary label which can be used
  by a merge policy to help implement such strategies as, e.g., last
  writer wins or field-specific source of truth.

- The `Operation` type describes a single change in a `Diff`. These
  values take the same label as the `Diff` that contains them.

- The `MergePolicy` type implements a specific policy which can be
  applied to label a `Diff` and to merge labelled `Diff`s. This is
  probably just a product type (maybe even just a tuple) of functions.

A number of operations on these types:

- `findInitialDocument :: [Document] -> Document` inspects a collection
  of `Document`s and generates a new `Document` can serve as a "common
  ancestor" to each of them. (NB: this document always exists because
  the empty `Document` is trivially prior to any other documents; so too
  is the intersection of the collection of documents).

- `generateDiff :: MergePolicy l -> Document -> Document -> Diff l`
  generates a `Diff` (labelled as required for the merge policy)
  describing the changes to convert one document into another.

- `mergeDiff :: MergePolicy l -> [Diff l] -> (Diff (), [Diff l])`
  uses a merge policy to merge a collection of diffs, generating a
  merged `Diff`, and a collection of left-over `Diff`s containing the
  operations which could not be merged.

# Merge policy

Each merge policy consists of two parts:

- A function to extract a label value from a `Document` which will then
  be applied to the `Diff`s the policy will be applied to.

- A function to merge two `Diff`s (labelled according to the policy) and
  return a merged `Diff` and a "left-over" `Diff`. The left-over `Diff`
  will be empty if the policy was able to merge all changes.

A first approximation of the merge policy data type might be:

````{.haskell}
data MergePolicy l = MergePolicy
    { extractLabel :: Document -> l
    , acceptDiff   :: Diff l -> Diff l -> (Diff l, Diff l)
    }
````

Or maybe that should be `Diff l -> Operation l -> Bool`?

To make it easy to construct a merge policy which expresses business
rules, the `MergePolicy` type will come equiped with a default policy
and an append operation:

````{.haskell}
defaultPolicy :: MergePolicy ()

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

-- | Trust the "accounts" source about the "paid" field, otherwise apply
-- the most recent changes.
trustAccountsThenMostRecentByTimestamp :: MergePolicy (Source, String)
trustAccountsThenMostRecentByTimestamp = 
    onField "paid" (trustSource "accounts") `andThen`
    mostRecentBy "timestamp"
````

It is likely that some of this machinery could be abstracted to use
existing core or third-party libraries (it would be a `Monoid` if not
for the labels and might something from `lens`).

We need a NOP for both the label extraction and diff merging operations
in the `MergePolicy` type. The label extraction NOP is clearly `const
()` (so long as the first policy has *some* label; `const ()` is a good
choice). The merging NOP is probably means `(,)`.

````{.haskell}
noMergePolicy :: MergePolicy ()
noMergePolicy = MergePolicy (const ()) (,)
````

Then the append operations is something like:

````{.haskell}
andThen :: MergePolicy l -> MergePolicy m -> MergePolicy (l, m)
(MergePolicy e1 m1) `andThen` (MergePolicy e2 m2) = MergePolicy e3 m3
  where
    e3 doc = (e1 doc, e2 doc)
    m3 (Diff l1 o1) (Diff l2 o2) =
        let (acc, rest) = m1 (Diff o1) (Diff o2)
        in m2 acc rest
````

# Data sources

A data source encapsulates the set of actions required to interact with
an external system which stores data to be synchronised. These actions
operate in a special monad, allowing the system to provide some support
functionality automatically.

The operations each data source supports are:

````{.haskell}
-- | Find the foreign key for a document, probably by performing a search
-- of some sort.
findDocument :: (Monad m, RetconSource m) => Document -> m (Maybe ForeignKey)

-- | Retrieve a document from the foreign system.
getDocument :: (Monad m, RetconSource m) => ForeignKey -> m Document

-- | 
setDocument :: (Monad m, RetconSource m) => ForeignKey -> Document -> m ()
````

The `RetconSource` type class is similar in spirit to the typeclasses
which accompany some monad transformers: it allows the underlying monad
to be replaced while still allowing the retcon-specific operations to be
used unchanged (like `MonadIO` with `liftIO`).

These operations will need to have access to IO operations (probably through
`MonadIO`); logging operations (probably via `MonadLogger`); and some specific
operations to access the retcon database lookup and cache tables.

Additional operations in this monad will include things like:

````{.haskell}
datasource :: (Monad m, RetconSource m) => m (Datasource)
````

# Entity

An entity encapsulates a collection of data sources and a merge policy
which together specify the system's operation on a particular type of
data. Once constructed, an `Entity` can be passed to a handler (see
below) which implements the I/O, user interface, etc.

# Handlers

The handlers are responsible for implementing the external interface to the
system. Each handler takes an `Entity` as a parameter along with some
implementation specific additional parameters.

Handlers will use a simple database-backed task queue to decouple their
interface from the processing which will take a while. Depending on the rate
limiting for some of the data source systems, it may be necessary to do crazy
things to coordinate access between different data sources which interact with
the same external system.

## Snaplet Handler

Initially the only handler will be the Snap Framework snaplet which implements
a basic RESTful web API and user interface.

The basic interface will look something like:

- `GET /v1/:entity_name/:id?page=:n` to view history.

- `POST /v1/:entity_name/:id` to initiate a new update.

- `GET /v1/:entity_name/:id/:n` to inspect a past change.

- `POST /v1/:entity_name/:id/update` to manually apply one or more unmerged
  changes.

Putting the API version number into the path is terrible, but the other
solutions are also terrible while this, at least, is also easy.

