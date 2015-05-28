Overview of Retcon
==================

This document provides a high-level overview of Retcon.

Introduction
------------

Retcon is designed to help propagate changes to data which are duplicated
between multiple systems. It includes a number of assumptions:

- The data are structured documents (i.e. JSON);
- The data are relatively small;
- Changes are to fields which are more or less independent of each other.

Data Model
----------

A dataset which is duplicated between systems and should be managed by Retcon
is called an *entity*. Examples might include "customer record", "user
profile", or "application configuration". A Retcon daemon can manage multiple
entities.

The dataset for an entity will be stored in and used by several systems. Retcon
call each of these a *data source* for that entity. Examples might include
"accounting", "Office 365", or "active directory".

During normal steady-state operation a copy of each data item will be present
in each data source for that entity. The goal of Retcon is to keep each of
these copies in sync.

Each copy will be identified by a *foreign key* assigned by the data source.
Retcon will treat a foreign key value as uninterpreted text.

A single *internal key* is assigned for each data item. Retcon tracks the
associations between an internal key and the corresponding foreign key for each
data source.

System Structure
----------------

There are four key components in a Retcon deployment:

1. Retcon itself runs as a daemon.

1. Retcon stores it's own data in a PostgreSQL database.

1. Data sources are external programs which Retcon invokes each time it wants
to communicate with an external system.

1. External systems live in the outside world and are not directly connected
with Retcon.

````
    +--------+             +-------------+             +-----------------+
    | Retcon |<----------->| Data Source |+<---------->+ External System |+
    +--------+             +-------------+|+           +-----------------+|+
        |                   +-------------+|            +-----------------+|
        |                    +-------------+             +-----------------+
       \|/
    +--------+
    |   DB   |
    +--------+
````
