Retcon
======

[![Build status][badge]][status]

*Retcon* is a small system to [retcon][] structured documents between two or
more data sources. The goal is to de-couple systems which operate on the same
data: each can maintain it's own copy of the data and rely on *retcon* to
propagate any changes.

While *retcon* can propagate non-conflicting changes automatically, most
conflicting changes will need to be resolve by a human being. --*Retcon* can
send email notifications of conflicts based on simple routing rules.--

*Retcon* also exposes a small network API allowing other tools and humans to
post change notifications, resolve conflicts, etc.

[badge]: https://travis-ci.org/anchor/retcon.svg?branch=master
[status]: https://travis-ci.org/anchor/retcon
[retcon]: https://en.wikipedia.org/wiki/Retcon


Binaries
--------

This package produces three executables:

* retcond: long-running server that receives notifications and issues updates
  to entity sources as required.
* retcon-client: send a notification to a retcond.
* retcon-oneshot: useful for testing and debugging, perform a retcon update
  without going through the rigmarole of setting up a retcond and then issuing
  a requests with retcon-client.
