Retcon API Specifications
=========================

In order to implement a user interface, retcon exposes a small API.

# Requirements

- Clients can notify retcon of a change to be processed.

- Clients can list outstanding conflicts.

- Clients can resolve an outstanding conflict, applying selected conflicting
operations.

# Alternative designs

## Direct database access

Embed user interface and/or HTTP API in the retcon process, allowing direct
access to the data store and calls into the retcon library.

No selected due to tighter coupling and lower resilience to failures.

# Design impacts

The API should supply a diff and the individual operations which could not be
automatically resolved. This will require changing the way conflicting diffs
are stored in the database.

The user interface should display the document, merged diff, and conflicting
operations side by side to give the user sufficient context to resolve
conflicts sensibly. This requires modifying the existing user interface
prototypes.

# Design

## Client

A client monad, abstracted by a typeclass, provides error handling, connection
management, and several primitive operations which directly implement each of
the requirements above. Errors received from the server are automatically
raised in the monad.

- The *notify* operation accepts an entity name, source name, and document
identity and places this in the work queue to be processed. An error is
returned if the names are not known to retcon.

- The *resolve conflict* operation accepts a diff ID and a list of operation
IDs. The diff is marked as resolved (i.e. no longer conflicting) and a new diff
containing the selected operation IDs is prepared and submitted to the work
queue.

- The *list conflicts* operation returns a list containing the details required
for a human user to inspect and resolve a conflict. This includes the diff ID,
merged diff, document (after applying the merged diff), and a list of
conflicting operations.

## Server

A server monad, abstracted by a typeclass, provides error handling and
connection management functionality. Errors in the server monad are
automatically propagated to the client.

A handler function operating in the server monad inspects incoming messages
and, using case analysis, invokes the appropriate operations.

- The *notify* operation checks that the entity name and source name are known
to retcon and, if so, adds the corresponding ForeignKey to the work queue for
processing and returns "OK" to the client. If the names are unknown, an error
is raised and returned to the client.

- The *resolve conflict* operation checks that the diff is not yet resolved (if
it is, an error is raised and returned to the client). The diff is marked as
resolve (i.e. no longer conflicting). A new diff is constructed containing the
selected operations, if any, and added to the work queue for processing. An
"OK" message is returned to the client.

- The *list conflicts* operation retrieves any conflicted diffs from the data
store and returns them to the client. This will include the diff ID, the
document and the merged diff (to provide context to the end user), and a list
of conflicting operations.

The work queue mentioned above serialises processing by retcon to avoid the
need for mutual exclusion controls around data source operations. This queue is
stored in the data store, to increase resilience to failure.

