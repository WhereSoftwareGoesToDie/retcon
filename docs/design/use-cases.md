# Use cases

The following use cases reference two data sources, *upstream* and *local*.

These are arbitrary and are named like so to provide a tangible example.

## Upstream change is propogated locally

The fields called *first-name* and *last-name* are modified in the *upstream*
data source, the library is invoked via an (external) notification system.

The library merges these two fields together into a *name* field and pushes
that change to the *local* data source.

A user now manually triggers the library to synchronise from *local* to
*upstream*, nothing further is changed if we are the only user of the system.

## Upstream delete is handled

A record is deleted in the *upstream* data source, that deletion is either
propogated or reverted, depending on configuration.
