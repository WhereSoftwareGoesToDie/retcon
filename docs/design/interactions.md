% Retcon Interactions

This document describes the approach retcon will use in interacting with
external systems. We'll cover interactions with two types of external systems:

1. Systems retcon itself depends on; and

2. Data sources retcon will synchronise.

Internal interactions
=====================

Retcon has fairly modest requirements to run:

- A way to cache inital documents;

- A way to send notifications about unresovled changes;

- A way to log its operation.

The initial version will probably use a [PostgreSQL][] database to store cached
initial documents; [hslogger][]

[PostgreSQL]: http://www.postgresql.org/
[hslogger]: 

Data sources
============

The retcon daemon will synchronise data sources with quite different
understandings of the world: 

- Interacting over HTTP APIs or "run this command and read/write to
stdin/stdout"

- Different document structures

- Slightly (we hope) different field formats.

The data source abstraction must be able to deal with all of these differences
and, hopefully, do so in a principled way. To minimise the initial complexity
let's just assume that data sources look something like this:

````{.haskell}
-- | An @EntityType@ identifies a particular type of data objects to be
--   synchronised between a set of systems.
class EntityType e where

    -- | A type @Key e@ of unique identifiers is associatiated with each
    --   @EntityType@.
    type Key e :: *

    -- | Each @EntityType@ has a name or label to be used in log messages, etc.
    --   This is a pretty terrible way to find this.
    entityLabel :: e -> String

-- | A @DataSource e@ is a pair of functions to load and save a @Document@
--   representing a particular data object.
type DataSource e = (Key e -> IO Document, Document -> IO ())
````
