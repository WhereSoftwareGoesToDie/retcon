Retcon Wire Format
==================

In order to implement it's client API, Retcon uses a simple wire format
structured around single request response exchanges.

# Requirements

- Simple request response handling
- Server side error transmission on response

# Alternative designs

* JSON REST API, would have been easier to use in some cases but "heavier" to run
  and seems too high level. We would throw away type safety.
* Some kind of haskell RPC framework, no good due to portability.

# Design impacts

Clients will need to be implemented in native language with binary formats, the
pay off here is that haskell gets type safety and tight coupling with the
server side code.

All encoding is currently using Data.Binary, which will soon use a standardized
format.

# Design

## Message transmission (ZMQ)

Messages will be sent as multipart ZMQ messages. Two parts for requests, two
for responses.

Clients will use a REQ socket to connect to a server with a REP socket.

## Request message parts

Requests look like this:

```
	[ header | payload ]
```

Where header is an encoded Int that enumerates the encoded payload as follows:

Request           Header  Payload                   Expected response
-------           ------  -------                   -----------------
ListConflicts     0       EMPTY                     ConflictListing
ResolveConflict   1       [Int]                     ResolveOK
Notify            2       (String, String, String)  NotifyOK


Note:
* EMPTY represents an empty payload of length 0.
* JSON represents ???

## Response message parts

Responses look like this:

```
	[ is_error | payload ]
```

Where is_error is an encoded Bool that indicates if the payload represents an
error enumeration. If is_error is set to true, payload is an encoded Int that
enumerates the error. Otherwise, the payload is a response uniquely determined
by request.

Here are the possible error payloads:

Error                    Payload
-----                    -------
TimeoutError             0
UnknownServerError       1

Here are the possible response payloads:


Response          Payload
--------          -------
ConflictListing   (JSON, Int, JSON, [(Int, JSON)])
ResolveOK         0
NotifyOK          1
