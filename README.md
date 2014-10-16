Retcon
======

[![Build Status](https://travis-ci.org/anchor/retcon.svg?branch=master)](https://travis-ci.org/anchor/retcon)

A Haskell library and daemon for synchronising data sources. `retcon`
operates by generating, merging, and applying diffs according.

This package contains:

- A library `retcon` which implements the functionality (code in `lib/`)

- A command-line application `retcon-demo` which provides a simple example for
implementing `retcon` (in `src/`).

- A command-line application `send-notifications` which send email messages
warning of conflicts (in `src/`).

In future it will also contain a Snap-based daemon exposing a REST-ish API.

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
