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

## Comparing and resolving conflicting diffs from different sources

A record receives updates to the same fields from *local* and *upstream* at
the same time, and the changes conflict. These will need to be resolved
manually.

### Send out notifications of conflicts

Current conflicts should be assembled in a listable form and then distributed
to system maintainers via a notification interface defined by Retcon's
configuration.

### Display original document and available pending diffs

The administrative interface will need to display the original document, and
offer a selection of diff operations that can be applied to update the
document. To do this, we'll need to be able to display the original document
as JSON, and we'll also a list of diff operations that apply to the document.
The document and list of diffs can then be displayed in a web (or other)
interface for staff to use and update.

### Submit selection of diffs to apply to the document

The administrative interface will submit a series of diff portion IDs to
apply to the document. Retcon will then retrieve the diff portions from the
database, perform the operations that each portion defines against the
original document, and then propagate the updated document to *local* and
*upstream*.
