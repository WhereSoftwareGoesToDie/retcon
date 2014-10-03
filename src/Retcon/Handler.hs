--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Dispatch events with a retcon configuration.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Retcon.Handler where

import Control.Applicative
import Control.Exception
import Control.Exception.Enclosed (tryAny)
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality
import Database.PostgreSQL.Simple
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Diff
import Retcon.Document
import Retcon.Error
import Retcon.MergePolicy
import Retcon.Monad
import Retcon.Options
import Retcon.Store

-- | Check that two symbols are the same.
same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)

-- | Operations to be performed in response to data source events.
data RetconOperation entity source =
      RetconCreate (ForeignKey entity source) -- ^ Create a new document.
    | RetconDelete (InternalKey entity)       -- ^ Delete an existing document.
    | RetconUpdate (InternalKey entity)       -- ^ Update an existing document.
    | RetconProblem (ForeignKey entity source) RetconError -- ^ Record an error.
    deriving (Show)

instance Eq (RetconOperation entity source) where
    (RetconCreate fk1) == (RetconCreate fk2) = fk1 == fk2
    (RetconDelete ik1) == (RetconDelete ik2) = ik1 == ik2
    (RetconUpdate ik1) == (RetconUpdate ik2) = ik1 == ik2
    (RetconProblem fk1 _) == (RetconProblem fk2 _) = fk1 == fk2
    _ == _ = False

-- | Interact with the data source which triggered in an event to identify
-- the operation to be performed.
--
-- This function should be able to return a 'RetconProblem' value, but currently
-- doesn't.
determineOperation :: (ReadableToken s, RetconDataSource entity source)
                   => DataSourceState entity source
                   -> ForeignKey entity source
                   -> RetconHandler s (RetconOperation entity source)
determineOperation state fk = do
    opt <- retconOptions <$> getRetconState

    when (optVerbose opt) $ $(logInfo) $ T.concat
        [ "DETERMINE: "
        , T.pack . show . foreignKeyValue $ fk
        ]

    ik' <- lookupInternalKey fk

    when (optVerbose opt) . $(logDebug) $ T.concat
        [ "Looking for key: "
        , T.pack . show $ ik'
        ]

    doc' <- runRetconAction state $ getDocument fk

    when (optVerbose opt) . $(logDebug) $ T.concat
        [ "Looking for document: "
        , T.pack . show $ doc'
        ]

    return $ case (ik', doc') of
        (Nothing, Left  _) -> RetconProblem fk (RetconSourceError "Unknown key, no document")
        (Nothing, Right _) -> RetconCreate fk
        (Just ik, Left  _) -> RetconDelete ik
        (Just ik, Right _) -> RetconUpdate ik

-- | Perform the action/s described by a 'RetconOperation' value.
runOperation :: (ReadableToken store, WritableToken store, RetconDataSource entity source)
             => DataSourceState entity source
             -> RetconOperation entity source
             -> RetconHandler store ()
runOperation state event =
    case event of
        RetconCreate  fk -> create state fk
        RetconDelete  ik -> delete state ik
        RetconUpdate  ik -> update state ik
        RetconProblem fk err -> reportError state fk err

-- | Parse a request string and handle an event.
dispatch :: forall store. (ReadableToken store, WritableToken store)
         => String
         -> RetconHandler store ()
dispatch work = do
    let (entity_str, source_str, key) = read work :: (String, String, String)
    entities <- retconState <$> getRetconState

    case (someSymbolVal entity_str, someSymbolVal source_str) of
        (SomeSymbol entity, SomeSymbol source) ->
            forM_ entities $ \(InitialisedEntity e dss) -> when (same e entity) $
                forM_ dss $ \(InitialisedSource (sp :: Proxy st) dst :: InitialisedSource et) -> do
                            let fk = ForeignKey key :: ForeignKey et st

                            when (same source sp) $ process dst fk

-- | Run the retcon process on an event.
retcon :: (ReadableToken s, WritableToken s)
       => RetconOptions
       -> RetconConfig
       -> s
       -> String
       -> IO (Either RetconError ())
retcon opts config store key =
    runRetconMonad' opts config store () $ dispatch key

-- | Process an event on a specified 'ForeignKey'.
--
-- This function is responsible for determining the type of event which has
-- occured and invoking the correct 'RetconDataSource' actions and retcon
-- algorithms to handle it.
process :: forall store entity source. (ReadableToken store, WritableToken store, RetconDataSource entity source)
        => DataSourceState entity source
        -> ForeignKey entity source
        -> RetconHandler store ()
process state fk = do
    $logDebug . T.pack . concat $ [ "EVENT against ", show $ length sources
                                  , " sources"
                                  ]

    determineOperation state fk >>= runOperation state
  where
    sources = entitySources (Proxy :: Proxy entity)

-- | Process a creation event.
create :: forall store entity source. (ReadableToken store, WritableToken store, RetconDataSource entity source)
       => DataSourceState entity source
       -> ForeignKey entity source
       -> RetconHandler store ()
create state fk = do
    $logDebug "CREATE"

    -- Allocate a new InternalKey to represent this entity.
    ik <- createInternalKey
    recordForeignKey ik fk

    -- Use the new Document as the initial document.
    doc' <- runRetconAction state $ getDocument fk

    case doc' of
        Left _ -> do
            deleteState ik
            throwError (RetconSourceError "Notification of a new document which doesn't exist")
        Right doc -> do
            recordInitialDocument ik doc
            setDocuments ik . map (const doc) $ entitySources (Proxy :: Proxy entity)
    return ()

-- | Process a deletion event.
delete :: (ReadableToken store, WritableToken store, RetconEntity entity)
       => state
       -> InternalKey entity
       -> RetconHandler store ()
delete state ik = do
    $logDebug "DELETE"

    -- Delete from data sources.
    results <- carefully $ deleteDocuments ik

    -- TODO: Log things.

    -- Delete the internal associated with the key.
    deleteState ik

-- | Process an update event.
update :: (ReadableToken store, WritableToken store, RetconEntity entity)
       => state
       -> InternalKey entity
       -> RetconHandler store ()
update state ik = do
    $logDebug "UPDATE"

    -- Fetch documents.
    docs <- carefully $ getDocuments ik
    let valid = rights docs

    -- Find or calculate the initial document.
    --
    -- TODO This is fragile in the case that only one data sources has a document.
    initial <- fromMaybe (calculateInitialDocument valid) <$>
               lookupInitialDocument ik

    -- Build the diff.
    let diffs = map (diff initial) valid
    let (diff, fragments) = mergeDiffs ignoreConflicts diffs

    -- Apply the diff to each source document.
    --
    -- TODO: We replace documents we couldn't get with the initial document. The
    -- initial document may not be "valid".
    let output = map (applyDiff diff . either (const initial) id) docs

    -- TODO: Record changes in database.

    -- Save documents.
    results <- carefully $ setDocuments ik output

    -- TODO: Log all the failures.

    return ()

-- | Report an error in determining the operation, communicating with the data
-- source or similar.
reportError :: (RetconDataSource entity source)
            => state
            -> ForeignKey entity source
            -> RetconError
            -> RetconHandler store ()
reportError state fk err = do
    $logError . T.pack . concat $ [ "Could not process event for "
                                  , show . foreignKeyValue $ fk
                                  , ". "
                                  , show err
                                  ]
    return ()

-- * Data source wrappers
--
-- $ These actions wrap the operations for a single data source and apply them
-- lists of arbitrary data sources.

-- | Get 'Document's corresponding to an 'InternalKey' for all sources for an
-- entity.
getDocuments :: forall store entity. (ReadableToken store, RetconEntity entity)
             => InternalKey entity
             -> RetconHandler store [Either RetconError Document]
getDocuments ik = do
    let entity = Proxy :: Proxy entity
    entities <- retconState <$> getRetconState

    results <- forM entities $ \(InitialisedEntity current sources) ->
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl -> forM sources $ \(InitialisedSource (_ :: Proxy source) state) ->
                -- Flatten any nested errors.
                (do
                    -- Lookup the foreign key for this data source.
                    $(logError) "Attempting to lookup!"
                    mkey :: Maybe (ForeignKey entity source) <- lookupForeignKey ik
                    $(logError) "Done looking! About to inspect"
                    -- If there was a key, use it to fetch the document.
                    case mkey of
                        Nothing -> do
                            $(logError) "Could not find a document id! :-("
                            return . Left $ RetconFailed
                        Just fk -> do
                            $(logError) "Found an ID! Can we get it?"
                            res <- runRetconAction state $ getDocument fk
                            $(logError) $ T.concat ["Done! ", T.pack . show $ res]
                            return res
                    )
    return . concat $ results

-- | Set 'Document's corresponding to an 'InternalKey' for all sources for an
-- entity.
setDocuments :: forall store entity. (ReadableToken store, WritableToken store, RetconEntity entity)
             => InternalKey entity
             -> [Document]
             -> RetconHandler store [Either RetconError ()]
setDocuments ik docs = do
    let entity = Proxy :: Proxy entity
    entities <- retconState <$> getRetconState

    results <- forM entities $ \(InitialisedEntity current sources) ->
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl -> forM (zip docs sources) $
                \(doc, InitialisedSource (_ :: Proxy source) state) ->
                    join . first RetconError <$> tryAny (do
                        (fk :: Maybe (ForeignKey entity source)) <- lookupForeignKey ik
                        fk' <- runRetconAction state $ setDocument doc fk
                        case fk' of
                            Left  _  -> return ()
                            Right fk -> void $ recordForeignKey ik fk
                        return $ Right ()
                    )
    return . concat $ results

-- | Delete the 'Document' corresponding to an 'InternalKey' for all sources.
deleteDocuments :: forall store entity. (ReadableToken store, WritableToken store, RetconEntity entity)
                => InternalKey entity
                -> RetconHandler store [Either RetconError ()]
deleteDocuments ik = do
    let entity = Proxy :: Proxy entity
    entities <- retconState <$> getRetconState

    results <- forM entities $ \(InitialisedEntity current sources) ->
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl -> forM sources $
                \(InitialisedSource (_:: Proxy source) state) ->

                    do
                        (fk' :: Maybe (ForeignKey entity source)) <- lookupForeignKey ik
                        case fk' of
                            Nothing -> return . Right $ ()
                            Just fk -> runRetconAction state $ deleteDocument fk
    return . concat $ results

-- * Storage backend wrappers
--
-- $ These actions wrap the data storage backend operations and lift them into
-- the RetconHandler monad.

-- | Delete the internal state associated with an 'InternalKey'.
deleteState :: forall store entity. (WritableToken store, RetconEntity entity)
            => InternalKey entity
            -> RetconHandler store ()
deleteState ik = do
    let key = T.pack . show . internalKeyValue $ ik
    opt <- retconOptions <$> getRetconState

    $logInfo $ T.concat ["DELETE state for ", key]

    deleteInitialDocument ik
    n_fk <- deleteForeignKeys ik
    n_ik <- deleteInternalKey ik

    when (optVerbose opt) $ do
        $logDebug $ T.concat [ "Deleted foreign key/s for ", key, ": "
                             , T.pack . show $ n_fk
                             ]
        $logDebug $ T.concat [ "Deleted internal key/s for ", key, ": "
                             , T.pack . show $ n_ik
                             ]
    return ()

-- | Attempt to translate a 'ForeignKey' from one source into a 'ForeignKey'
-- for another source.
translateForeignKey
    :: forall entity source1 source2 store.
        (ReadableToken store, RetconDataSource entity source1,
        RetconDataSource entity source2)
    => ForeignKey entity source1
    -> RetconHandler store (Maybe (ForeignKey entity source2))
translateForeignKey from =
    lookupInternalKey from >>= maybe (return Nothing) lookupForeignKey

-- | Get all diffs for a Document
-- Use for displaying diffs
getInitialDocumentDiffs :: forall store entity. (ReadableToken store, WritableToken store, RetconEntity entity)
       => InternalKey entity
       -> RetconHandler store [Diff Int]
getInitialDocumentDiffs ik = do
    -- conn <- asks retconConnection
    let conn = undefined
    (results :: [Only Int]) <- liftIO $ query conn selectQ (internalKeyValue ik)
    let ids = map fromOnly results
    let rawDiffs = map (\d -> Diff d []) ids
    mapM completeDiff rawDiffs
    where
        selectQ = "SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?"

-- | Build a Diff object from a Diff ID
-- Use for displaying diffs
completeDiff :: (ReadableToken store) => Diff Int -> RetconHandler store (Diff Int)
completeDiff (Diff diff_id _) = do
    diffOps <- getDbDiffOps diff_id
    return $ Diff diff_id diffOps

-- | Get DiffOp objects belonging to a Diff ID
-- Use for displaying diffs
getDbDiffOps :: (ReadableToken store, FromJSON l) => Int -> RetconHandler store [DiffOp l]
getDbDiffOps diff_id = do
    -- conn <- asks retconConnection
    let conn = undefined
    (results :: [Only Value]) <- liftIO $ query conn selectQ (Only diff_id)
    return . mapMaybe (constructDiffOpFromDb . fromOnly) $ results
    where
        selectQ = "SELECT portion FROM retcon_diff_portion WHERE diff_id = ?"

constructDiffOpFromDb :: (FromJSON l) => Value -> Maybe (DiffOp l)
constructDiffOpFromDb v =
    case (fromJSON v :: (FromJSON l) => Result (DiffOp l)) of
        Error _   -> Nothing
        Success d -> Just d
