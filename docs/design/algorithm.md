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

A diff, then, is just a sequence (list) of changes.

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

````
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

Building an initial document
----------------------------

Merge policies
--------------

