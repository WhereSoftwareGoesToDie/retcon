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
-- | Combine two merge policies by applying one and then another.
andThen :: MergePolicy n -> MergePolicy m -> MergePolicy (n,m)
andThen (MergePolicy ext1 pol1) (MergePolicy ext2 pol2) = MergePolicy ext3 pol3
  where
    ext3 doc = (ext1 doc, ext2 doc)
    pol3 diff op = (policyOverLabel fst pol1 diff op) || (policyOverLabel snd pol2 diff op)
````

# Data sources

A data source encapsulates the set of actions required to interact with
an external system which stores data to be synchronised. These actions
operate in a special monad, allowing the system to provide some support
functionality automatically.

The operations each data source supports are:

````{.haskell}
-- | Retrieve a document from the foreign system.
getDocument :: (RetconSource m)
            => ForeignKey
            -> m (Either DataSourceError Document)

-- | Write a document to the foreign system, returning the new foreign
-- key for the document, if any.
setDocument :: (RetconSource m)
            => Maybe ForeignKey
            -> Document
            -> m (Either DataSourceError (Maybe ForeignKey))

-- | Delete a document from the foreign system.
deleteDocument :: (RetconSource m)
               => ForeignKey
               -> m (Either DataSourceError ())
````

The `RetconSource` type class is similar in spirit to the typeclasses which
accompany some monad transformers: it allows the underlying monad to be
replaced while still allowing the retcon-specific operations to be used
unchanged (like `MonadIO` with `liftIO`).

````{.haskell}
class (MonadIO m, MonadLogger m) => RetconSource m where
    entity :: m Entity
    datasource :: m DataSource
````

These operations will need to have access to IO operations (probably through
`MonadIO`); logging operations (possibly via `MonadLogger`); and some specific
operations to access the retcon database lookup and cache tables.

The implementation will likely be something a little bit like this (depending
on the exact functionality we eventually depend on):

````{.haskell}
type RetconSource = ReaderT Entity (ReaderT DataSource IO))

instance RetconSource RetconSource where
    entity = ask
    datasource = lift ask

-- | Run an action to interact with a retcon datasource.
runRetconSource :: MonadIO m => Entity -> DataSource -> RetconSource r -> m r
runRetconSource entity datasource action =
    liftIO $ runReaderT (runReaderT action entity) datasource
````

# Entity

An entity encapsulates a collection of data sources and a merge policy
which together specify the system's operation on a particular type of
data. Once constructed, an `Entity` can be passed to a handler (see
below) which implements the I/O, user interface, etc.

# `runRetcon`

All of the details above are brought together in a single function which is
responsible for evaluating retcon's internal data stores, coordinating access
to external data stores, applying the core algorithm, logging changes, etc.

This core algorithm will have a type a little like:

````{.haskell}
runRetcon :: (MonadIO m)
          => Entity
          -> Event
          -> m ()
````

but is likely to return something slightly more useful than `()`. The `Event`
value will contain information describing the event notification delivered to
the system through the handler (see below) which should include the source
system, the document key from that system and, if required, the event type
(create, update, or delete).

It is entirely possible that we'll be able to implement the requirements
without an explicit event type, infering the event type from information retcon
already has (an event with an unknown document key is creation, an event with
a document which can't be retrieved from that source is deletion, anything else
is an update). If this will work (and it should), we'll do this. Then `Event`
might look something like:

````{.haskell}
type Event = (Source, ForeignKey)
````

The logic of this function is relatively straightforward:

1. Identify the type of change being performed (according to the logic above)
and the retcon key of the document to be updated (allocating a new one, if
required).

2. Fetch the matching document from all sources. If we don't know of a matching
document in a particular data source, the `Nothing` will be replaced in retcon
with the initial document. (This *should* only happen for newly created
documents, in which case the initial document is $\emptyset$ and the diff will
be "copy all the fields").

3. Perform retcon on the documents.

4. Send the updated documents back to their corresponding data sources.

Each step in this process can record useful information in logs and in
operational database tables. These details include:

- A retcon key and the last known corresponding initial document stored in
`retcon_{entity}`.

- A record of the changes made (or not made) to each document (i.e. the merged
`Diff` and any unmerged changes) stored in `retcon_{entity}_changes`.

- A foreign key identifying a document in a data source, stored in
`retcon_{entity}_{source}`.

- A record of the document in each data source after each change stored in `retcon_{entity}_{source}_history`.

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

