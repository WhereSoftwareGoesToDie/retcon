Retcon
======

[![Build Status](https://travis-ci.org/anchor/retcon.svg?branch=master)](https://travis-ci.org/anchor/retcon)

A Haskell library and daemon for synchronising data sources. `retcon`
operates by generating, merging, and applying diffs according.

This library is intended to used via the (yet to be built) daemon that allows
clients to query and signal it via the [wire format]{docs/design/wire-format.md}.

The first client to be developed will be a HTTP/HTML frontend to Retcon that
allows users to resolve conflicts intelligently. A link will be placed here
when that is ready.

This package contains:

- A library `retcon` which implements the functionality (code in `lib/`)

- A command-line application `retcon-demo` which provides a simple example for
implementing `retcon` (in `src/`).

- A command-line application `server-demo` which provides a simple example of
the API server (in `src/server-demo.hs`).

- A command-line application `client-demo` which provides a simple example of
the API client (in `src/client-demo.hs`).

- A command-line application `send-notifications` which send email messages
warning of conflicts (in `src/`).

Usage
-----

The `retcon-demo` command is compiled from the `src/CLI.hs` file. It
supports two entities (customers and events) each with several data
sources. You process an update by running the command with the event,
source, and key as command line parameters:

    retcon-demo event icalendar 1234

Implementing your own such program is a matter of following the example in
`CLI.hs` and implementing your own `RetconEntity` and `RetconDataSource`
instances, constructing a `RetconConfig`, and invoking the `dispatch`
function.

Documentation
-------------

Documents, including use cases and design documentation, can be found in the
`docs/` directory.
