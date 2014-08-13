% Retcon User API
% Anchor Systems

This document provides an overview of the user API for the retcon system. This
information will allow you to implement support for your own entities and data
sources.

# Overview

The retcon user API is structured around two sets of key data types:

- The types which represent entities, i.e. types of data; and

- The types which represent data sources, i.e. the external systems which hold
those data.

Both of these are represented using type-level strings and type class
instances.

## Entities

An *entity* is a "type" of data which should be processed and synchronised by
the system. Examples might include:

- Customer accounts

- Contacts

- Tickets

## Data sources

A *data source* is an interface to an external system in which data for
a particular entity is replicated. Examples might (hypothetically) include:

- The Customer API and Netsuite data sources for customer accounts.

- The Customer API and RT data sources for contacts.

- The RT and TeamWork data sources for tickets.

# Interface

The interface you must implement is relatively simple, though not trivial.

## Implementing a data source

To implement a new data source, you must do several things:

0. Determine the name for the entity you'll be dealing with. This is the type
in the `RetconEntity` instance (see below).

1. Select a new, unique, name for your data source. It should be short, unique
and fairly lower-case alphanumeric as you can make it.

2. Implement `getDocument`, `setDocument`, and `deleteDocument` functions which
communicate with the back-end system.

3. Implement the `RetconDataSource` type class with this string as the type and
these functions as the methods.

````{.haskell}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

import GHC.TypeLits
import Retcon.Monad
import Retcon.DataSource

instance RetconDataSource "account" "customer-api" where
    getDocument key = error "getDocument is not defined"

    setDocument doc key = error "setDocument is not defined"

    deleteDocument key = error "deleteDocument is not defined"
````

## Implementing an entity

Implementing an entity is a little simpler than implementing a data source.

1. Select a name for the entity.

2. Implement the `RetconEntity` type class with that name as the type. 

````{.haskell}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

import Data.Proxy
import GHC.TypeLits
import Retcon.DataSource

instance RetconEntity "account" where
    entitySources _ = [SomeDataSource (Proxy :: Proxy "customer-api")]
````

