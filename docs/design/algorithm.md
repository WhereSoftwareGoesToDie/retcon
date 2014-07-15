% Retcon Algorithm

Retcon is a system to maintain the consistency of structured data replicated in
multiple systems. This document is a short overview of the algorithm which
retcon uses to merge updates from multiple sources.

Data Structures
===============

Document
--------

The inputs and outputs to the retcon algorithm are documents. Documents are,
essentially, key/value tree structures (i.e. JSON).

Documents can be empty (this will be important later) or contain an unordered
sequence of key/value pairs.

- Keys are strings.

- Values may comprise strings or some documents.

We might want to just say "documents are the JSON AST from [aeson][]"?

[aeson]: http://hackage.haskell.org/package/aeson

Diff
----

A diff describes the changes between a source document and a destination
document.

A individual change has the following forms:

- "INSERT key value" sets the given key to the specified value. A value is any
value which can be present in a document as described above.

- "DELETE key" deletes the given key.

It may be useful to extend this set of operations with more specific operations
to support the semantics of specific document value types (e.g. "LIST-APPEND
key value", "SET-INSERT key value").

A diff, then, is just a sequence (list) of operations.

Algorithm
=========

The algorithm is relatively simple:

1. Get the last known state for the document from the retcon cache. If no
cached document is found, build one from the inputs (see below).

2. Generate a diff (as described above) from the initial state to each of the
input documents.

3. Merge the diffs to form a single diff according to the configured merge
policy (see below).

4. Apply the diff to the cached document and save it in the cache.

5. Apply the diff to each of the input documents.

6. Send the patched input back to the appropriate upstream.

Here's some pseudo-Haskell code making it a little more explicit:

````{.haskell}
ident <- identifier inputs

cached <- getCache ident
let initial = case cached of
    Nothing  -> generateInitial inputs
    Just doc -> doc

let diffs = map (diff initial) inputs

let diff = mergeDiffs mergePolicy diffs

setCache ident $ applyDiff diff initial

sequence_ $ map (sendUpstream . applyDiff diff) inputs
````

Some theory
-----------

It probably isn't terribly true -- and I certainly haven't proved it -- but we
could think of what we're doing in an order theoretic sense:

- Documents form a partial order (a poset).

- The empty document $\emptyset$ is the least element.

- In general, there are no greatest elements.

- Some applications (i.e. the entities described in the laws document) *might*
have a greatest element but this is of no importance.

- A cached initial document is in the lower bound of the set of input
documents.

- A generated initial document is, by construction, a greatest lower bound of
the set of input documents.

- It's always possible to generate an initial document; if nothing else, we can
use $\emptyset$.

- A diff is a sequence of operations which navigate edges between the documents
in the poset.

Building an initial document
----------------------------

When retcon does not have an initial document to use in merging the input
documents, it will construct one from the input documents. Ideally this
algorithm will find the initial document which results in a near-optimal set of
diffs to the given input documents, but merging the diffs is likely to reduce
or remove any redundancy introduced by a sub-optimal choice in initial
documents (so let's not worry too much).

Any non-empty set of input documents has a trivial initial document: the empty
document. This will result in very large diffs and force all of the difficulty
into the merging algorithm. In all likely hood, this will just result in
changes *not* being merged automatically.

As the initial document is only used within the retcon system, it's safe to
select a document which will be invalid to external systems. With this fact in
mind, here's the first version of this algorithm:

1. Take the intersection of the input documents.

No doubt we'll be able to find a better algorithm or, more likely, better
heuristics for this this when we start experimenting with real data.

Merge policies
--------------

The linchpin of the retcon algorithm describes above is the merging of the
diffs for each of the input documents. This function transforms a collection of
diffs on individual input documents into a single diff which includes the
compatible changes from all of those changes. Different deployments will have
different ideas about which changes are compatible and how to handle
incompatible changes, so the particular strategy to be used is a configurable
parameter.

Merge policies can be composed to express the policy appropriate for a specific
application.

Example:

````{.haskell}
commentField `using` (textfriendlymerge (filter (isSpace) . map toLower)) .
lastWriterWins
````

### Last writer wins

The last writer wins strategy takes a lens to a field and sorts the input
documents (monotonically non-decreasing, lexicographic order) over this field,
then copies the changes into the output diff, replacing earlier changes with
later changes in the case of a key clash.

The generated diff contains at most one change with any particular key.

This will result in conflicting or overlapping changes being overwritten.

### Non-conflicting changes

The non-conflicting changes strategy omits all conflicting operations (i.e. all
operations on a key with multiple non-identical operations).

The generated diff contains at most one change with any particular key.

Keys with conflicting changes will *not* be changed in the generated diff. As
the diff is applied to the original input document from each data source,
conflicting changes will remain untouched in their respective upstream fields.

### Text-friendly changes

The text-friendly changes strategy considers identical and changes which affect
the same key and which have equivalent effects up to some supplied function
`String -> String`. This can be used to implement case-insensitive or
white-space agnostic changes on particular fields.

