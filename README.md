Synchronise
===========

*Synchronise* is a small system to synchronise structured documents between two
or more data sources. The goal is to de-couple systems which operate on the
same data: each can maintain it's own copy of the data and rely on
*synchronise* to propagate any changes.

While *synchronise* can propagate non-conflicting changes automatically, most
conflicting changes will need to be resolve by a human being. *Synchronise* can
send email notifications of conflicts based on simple routing rules.

*Synchronise* also exposes a small HTTP API and web interface allowing other
tools and humans to post change notifications, resolve conflicts, etc.
