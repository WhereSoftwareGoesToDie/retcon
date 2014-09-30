
            -- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Test.Hspec

import Retcon.Application
import Retcon.Config
import Retcon.DataSource
import Retcon.Diff
import Retcon.Document
import Retcon.Error
import Retcon.Handler
import Retcon.Monad
import Retcon.Options
import Retcon.Store (RWToken, RetconStore, token)
import qualified Retcon.Store.Memory as Memory

import Control.Applicative
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import GHC.TypeLits ()
import System.Process

testDBName :: ByteString
testDBName = "retcon_handler_test"

testConnection :: ByteString
testConnection = BS.concat [BS.pack "dbname='", testDBName, BS.pack "'"]

-- * Dispatching Tests
--
-- $ These tests exercise the logic used in handling events appropriately.

-- | Create a new document in one of these stores.
newTestDocument :: Text
                -> Maybe Value
                -> IORef (Map Text Value)
                -> IO (Text, Value)
newTestDocument n doc' ref = do
    let doc = fromMaybe (Object HM.empty) doc'
    key <- atomicModifyIORef' ref (\store ->
        let key = n `T.append` ( T.pack . show $ M.size store)
        in (M.insert key doc store, key))
    return (key, doc)

-- | In-memory data source delete operation
deleteDocumentIORef :: (RetconDataSource entity source)
                    => IORef (Map Text Value)
                    -> ForeignKey entity source
                    -> DataSourceAction (DataSourceState entity source) ()
deleteDocumentIORef ref (ForeignKey fk') = do
    let k = T.pack fk'
    liftIO $ atomicModifyIORef' ref (\m -> (M.delete k m, ()))

setDocumentIORef :: (RetconDataSource entity source)
                 => String
                 -> IORef (Map Text Value)
                 -> Document
                 -> Maybe (ForeignKey entity source)
                 -> DataSourceAction (DataSourceState entity source) (ForeignKey entity source)
setDocumentIORef name ref doc (Nothing) = do
    k <- liftIO $ atomicModifyIORef' ref
        (\m -> let k = T.pack $ name ++ (show . M.size $ m)
               in (M.insert k (toJSON doc) m, k))
    return $ ForeignKey $ T.unpack k

setDocumentIORef name ref doc (Just (ForeignKey fk')) = do
    let fk = T.pack fk'
    k <- liftIO $ atomicModifyIORef' ref
        (\m -> (M.insert fk (toJSON doc) m, fk))
    return $ ForeignKey $ T.unpack k

getDocumentIORef :: (RetconDataSource entity source)
                 => IORef (Map Text Value)
                 -> ForeignKey entity source
                 -> DataSourceAction (DataSourceState entity source) Document
getDocumentIORef ref (ForeignKey fk') = do
    let key = T.pack fk'
    doc' <- liftIO $ atomicModifyIORef' ref
        (\m -> (m, M.lookup key m))
    case doc' of
        Just doc -> case fromJSON doc of
            Success r -> return r
            _         -> throwError RetconFailed
        Nothing  -> throwError RetconFailed

-- | Configuration to use when testing retcon event dispatch.
dispatchConfig :: RetconConfig
dispatchConfig = RetconConfig [ SomeEntity (Proxy :: Proxy "dispatchtest") ]

instance RetconEntity "dispatchtest" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "dispatch1")
                      , SomeDataSource (Proxy :: Proxy "dispatch2")
                      ]

instance RetconDataSource "dispatchtest" "dispatch1" where

    data DataSourceState "dispatchtest" "dispatch1" =
        Dispatch1 { dispatch1State :: IORef (Map Text Value) }

    initialiseState = do
        ref <- newIORef M.empty
        return $ Dispatch1 ref

    finaliseState (Dispatch1 ref) =
        writeIORef ref M.empty

    getDocument fk = do
        ref <- asks dispatch1State
        getDocumentIORef ref fk

    setDocument doc fk = do
        ref <- asks dispatch1State
        setDocumentIORef "dispatch1-" ref doc fk

    deleteDocument fk = do
        ref <- asks dispatch1State
        deleteDocumentIORef ref fk

instance RetconDataSource "dispatchtest" "dispatch2" where

    data DataSourceState "dispatchtest" "dispatch2" =
        Dispatch2 { dispatch2State :: IORef (Map Text Value) }

    initialiseState = do
        ref <- newIORef M.empty
        return $ Dispatch2 ref

    finaliseState (Dispatch2 ref) =
        writeIORef ref M.empty

    getDocument fk = do
        ref <- asks dispatch2State
        getDocumentIORef ref fk

    setDocument doc fk = do
        ref <- asks dispatch2State
        setDocumentIORef "dispatch2-" ref doc fk

    deleteDocument fk = do
        ref <- asks dispatch2State
        deleteDocumentIORef ref fk

-- | Tests for code determining and applying retcon operations.
operationSuite :: Spec
operationSuite = around (prepareDatabase . prepareDispatchSuite) $ do
    let opt = defaultOptions {
          optDB = testConnection
        , optLogging = LogNone
        }

    describe "Determining operations" $ do
        it "should result in a create when new key is seen, with document." $ do
            -- Create a document in dispatch1; leave dispatch2 and the
            -- database tables empty.
            state <- initialiseEntities (retconEntities dispatchConfig)

            let entity = Proxy :: Proxy "dispatchtest"
            let source = Proxy :: Proxy "dispatch1"
            let Just st@(Dispatch1 ref) = accessState state entity source

            (fk', _) <- newTestDocument "dispatch1-" Nothing ref
            let fk = ForeignKey (T.unpack fk') :: ForeignKey "dispatchtest" "dispatch1"
            op <- run opt state $ determineOperation st fk

            _ <- finaliseEntities state

            op `shouldBe` (Right $ RetconCreate fk)

        it "should result in an error when new key is seen, but no document." $ do
            state <- initialiseEntities (retconEntities dispatchConfig)

            let entity = Proxy :: Proxy "dispatchtest"
            let source = Proxy :: Proxy "dispatch1"
            let Just st@(Dispatch1 ref) = accessState state entity source

            let fk = ForeignKey "this is new" :: ForeignKey "dispatchtest" "dispatch1"
            op <- run opt state $ determineOperation st fk

            _ <- finaliseEntities state
            op `shouldBe` (Right $ RetconProblem fk RetconFailed)

        it "should result in a update when old key is seen, with document." $
            withConfiguration opt $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)
                result <- testHandler store $ do

                    let es = "dispatchtest" :: String
                    let ds = "dispatch1" :: String
                    let entity = Proxy :: Proxy "dispatchtest"
                    let source = Proxy :: Proxy "dispatch1"
                    let Just st@(Dispatch1 ref) = accessState state entity source

                    -- Prepare data in the testing data source.
                    (fk', doc) <- liftIO $ newTestDocument "dispatch1-" Nothing ref

                    -- Set up the keys.
                    (ik :: InternalKey "dispatchtest") <- createInternalKey
                    let fk = ForeignKey (T.unpack fk') :: ForeignKey "dispatchtest" "dispatch1"
                    recordForeignKey ik fk

                    -- Determine the operation to perform.
                    op <- determineOperation st fk
                    return (op, ik)
                _ <- finaliseEntities state

                case result of
                    Right (RetconUpdate _, ik) ->
                        result `shouldBe` Right (RetconUpdate ik, ik)
                    _ -> error "Does not match"

        it "should result in a delete when old key is seen, but no document." $
            withConfiguration opt $ \(store, opts) -> do
                let fk1 = "dispatch1-deleted-docid" :: String

                state <- initialiseEntities (retconEntities dispatchConfig)
                result <- testHandler store $ do

                    -- Insert a key into the database, but no document in the store.
                    let es = "dispatchtest" :: String
                    let ds = "dispatch1" :: String
                    let entity = Proxy :: Proxy "dispatchtest"
                    let source = Proxy :: Proxy "dispatch1"
                    let Just st@(Dispatch1 ref) = accessState state entity source

                    ik <- createInternalKey
                    let fk = ForeignKey fk1 :: ForeignKey "dispatchtest" "dispatch1"
                    recordForeignKey ik fk

                    op <- determineOperation st fk
                    return (op, ik)

                _ <- finaliseEntities state

                case result of
                    Right (op, ik) -> op `shouldBe` (RetconDelete ik)
                    _ -> error "No match"

    describe "Evaluating operations" $ do
        it "should process a create operation." $
            withConfiguration opt $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let es = "dispatchtest" :: String
                let ds = "dispatch1" :: String
                let entity = Proxy :: Proxy "dispatchtest"
                let source = Proxy :: Proxy "dispatch1"
                let Just st@(Dispatch1 ref) = accessState state entity source

                (fk', _) <- newTestDocument "dispatch1-" Nothing ref

                result <- testHandler store $ do
                    let fk = ForeignKey (T.unpack fk')

                    let op = RetconCreate fk :: RetconOperation "dispatchtest" "dispatch1"

                    runOperation st op

                mem <- case store of
                    Memory.MemStorage ref -> readIORef ref
                mem `shouldBe` Memory.emptyState

                let iks = Memory.memItoF mem
                let fks = Memory.memFtoI mem

                _ <- finaliseEntities state
                (result, M.size iks, M.size fks) `shouldBe` (Right (), 1, 2)

        it "should process an error operation." $
            withConfiguration opt $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let es = "dispatchtest" :: String
                let ds = "dispatch1" :: String
                let entity = Proxy :: Proxy "dispatchtest"
                let source = Proxy :: Proxy "dispatch1"
                let Just st@(Dispatch1 ref) = accessState state entity source

                (fk', _) <- newTestDocument "dispatch1-" Nothing ref
                let fk = ForeignKey (T.unpack fk')
                let op = RetconProblem fk (RetconUnknown "Testing error reporting.") :: RetconOperation "dispatchtest" "dispatch1"

                res <- run opt state $ runOperation st op

                _ <- finaliseEntities state
                res `shouldBe` Right ()

        it "should process an update operation." $
            withConfiguration opt $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let source1 = Proxy :: Proxy "dispatch1"
                let source2 = Proxy :: Proxy "dispatch2"
                let Just st1@(Dispatch1 ref1) = accessState state entity source1
                let Just st2@(Dispatch2 ref2) = accessState state entity source2

                let change = Diff () [ InsertOp () ["name"] "INSERT ONE"
                                     , InsertOp () ["address"] " 201 Elizabeth"
                                     ]
                let doc1  = toJSON (mempty :: Document)
                let doc2 = toJSON $ applyDiff change mempty
                (fk1', _) <- newTestDocument "dispatch1-" (Just doc1) ref1
                (fk2', _) <- newTestDocument "dispatch2-" (Just doc2) ref2

                result <- testHandler store $ do
                    (ik :: InternalKey "dispatchtest") <- createInternalKey
                    let fk1 = ForeignKey (T.unpack fk1') :: ForeignKey "dispatchtest" "dispatch1"
                    let fk2 = ForeignKey (T.unpack fk2') :: ForeignKey "dispatchtest" "dispatch2"
                    recordForeignKey ik fk1
                    recordForeignKey ik fk2

                    -- Perform the operation.
                    let op = RetconUpdate ik :: RetconOperation "dispatchtest" "dispatch1"
                    runOperation st1 op

                -- Check the things.
                d1 <- readIORef ref1
                d2 <- readIORef ref2
                (iks, fks) <- case store of
                    Memory.MemStorage ref -> return (M.empty, M.empty)
                {-
                [Only (n_ik::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon"
                [Only (n_fk::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"
                -}
                let n_ik = 0
                let n_fk = 0

                _ <- finaliseEntities state
                (M.size d1, M.size d2, M.size iks, M.size fks) `shouldBe` (1, 1, 1, 2)

        it "should process a delete operation." $
            withConfiguration opt $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let source1 = Proxy :: Proxy "dispatch1"
                let source2 = Proxy :: Proxy "dispatch2"
                let Just st1@(Dispatch1 ref1) = accessState state entity source1
                let Just st2@(Dispatch2 ref2) = accessState state entity source2

                let fk1' = "dispatch1-lolno"
                (fk2', doc) <- newTestDocument "dispatch2-" Nothing ref2

                result <- testHandler store $ do
                    (ik :: InternalKey "dispatchtest") <- createInternalKey
                    let fk1 = ForeignKey fk1' :: ForeignKey "dispatchtest" "dispatch1"
                    let fk2 = ForeignKey (T.unpack fk2') :: ForeignKey "dispatchtest" "dispatch2"
                    recordForeignKey ik fk1
                    recordForeignKey ik fk2


                    -- Perform the operation.
                    let op = RetconDelete ik :: RetconOperation "dispatchtest" "dispatch1"
                    runOperation st1 op

                -- Check the things.
                d1 <- readIORef ref1
                d2 <- readIORef ref2
                {-
                [Only (n_ik::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon"
                [Only (n_fk::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"
                -}
                let n_ik = 0
                let n_fk = 0

                _ <- finaliseEntities state
                (M.size d1, M.size d2, n_ik, n_fk) `shouldBe` (0, 0, 0, 0)
  where
    run opt state action =
        withConfiguration opt $ \(store, opts) ->
            runRetconHandler opts state (token store) action

-- | Test suite for dispatching logic.
dispatchSuite :: Spec
dispatchSuite = around (prepareDatabase . prepareDispatchSuite) $ do
    let opts = defaultOptions {
          optDB = testConnection
        , optLogging = LogNone
        }

    describe "Dispatching changes" $ do
        -- NEW and EXISTS
        it "should create when new id and source has the document" $
            withConfiguration opts $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let ds1 = Proxy :: Proxy "dispatch1"
                let ds2 = Proxy :: Proxy "dispatch2"
                let Just (Dispatch1 ref1) = accessState state entity ds1
                let Just (Dispatch2 ref2) = accessState state entity ds2

                -- Create a document in dispatch1; leave dispatch2 and the
                -- database tables empty.
                (fk, doc) <- newTestDocument "dispatch1-" Nothing ref1

                -- Dispatch the event.
                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", fk] }
                let input = show ("dispatchtest", "dispatch1", fk)

                res <- run opts' state store $ dispatch input

                -- The document should be present in both stores, with an
                -- InternalKey and two ForeignKeys.

                d1 <- readIORef ref1
                d2 <- readIORef ref2

                (iks, fks) <- case store of
                    Memory.MemStorage ref -> do
                        state <- liftIO $ readIORef ref
                        return (Memory.memItoF state, Memory.memFtoI state)

                _ <- finaliseEntities state
                (M.size d1, M.size d2, M.size iks, M.size fks) `shouldBe` (1, 1, 1, 2)

        -- NEW and NO EXIST
        it "should error when new id and source hasn't the document" $
            withConfiguration opts $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let ds1 = Proxy :: Proxy "dispatch1"
                let ds2 = Proxy :: Proxy "dispatch2"
                let Just (Dispatch1 ref1) = accessState state entity ds1
                let Just (Dispatch2 ref2) = accessState state entity ds2

                -- Both dispatch1 and dispatch2, and the database tables are
                -- still empty.

                -- Dispatch the event.
                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", "999"] }
                let input = show ("dispatchtest", "dispatch1", "999")
                res <- run opts' state store $ dispatch input

                -- Check that there are still no InternalKey or ForeignKey
                -- details in the database.
                d1 <- readIORef ref1
                d2 <- readIORef ref2
                {-
                [Only (n_ik :: Int)] <- liftIO $ query_ conn "SELECT COUNT(*) FROM retcon;"
                [Only (n_fk :: Int)] <- liftIO $ query_ conn "SELECT COUNT(*) FROM retcon_fk;"
                -}
                let n_ik = 0
                let n_fk = 0

                _ <- finaliseEntities state
                (M.size d1, M.size d2, n_ik, n_fk) `shouldBe` (0, 0, 0, 0)

        -- OLD and EXISTS
        it "should update when old id and source has the document" $
            withConfiguration opts $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let ds1 = Proxy :: Proxy "dispatch1"
                let ds2 = Proxy :: Proxy "dispatch2"
                let Just (Dispatch1 ref1) = accessState state entity ds1
                let Just (Dispatch2 ref2) = accessState state entity ds2

                -- Create a document, with changes in dispatch1.
                let change = Diff () [ InsertOp () ["name"] "INSERT ONE"
                                     , InsertOp () ["address"] " 201 Elizabeth"
                                     ]
                let doc = toJSON (mempty :: Document)
                let doc' = toJSON $ applyDiff change mempty
                (fk1', _) <- newTestDocument "dispatch1-" (Just doc') ref1
                (fk2', _) <- newTestDocument "dispatch2-" (Just doc) ref2

                result <- testHandler store $ do
                    -- Add a new InternalKey and ForeignKeys for both data sources.
                    (ik :: InternalKey "dispatchtest") <- createInternalKey
                    let fk1 = ForeignKey (T.unpack fk1') :: ForeignKey "dispatchtest" "dispatch1"
                    let fk2 = ForeignKey (T.unpack fk2') :: ForeignKey "dispatchtest" "dispatch2"
                    dispatch . show $ ("dispatchtest", "dispatch1", fk1')

                -- Check that the documents are now the same.
                d1 <- atomicModifyIORef' ref1 (\m->(m,M.lookup fk1' m))
                d2 <- atomicModifyIORef' ref2 (\m->(m,M.lookup fk2' m))
                {-
                [Only (n::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"
                -}
                let n = 0

                _ <- finaliseEntities state
                (n, d1, d2) `shouldBe` (2, Just doc', Just doc')

        -- OLD and NO EXIST
        it "should delete when old id and source hasn't the document" $
            withConfiguration opts $ \(store, opts) -> do
                state <- initialiseEntities (retconEntities dispatchConfig)

                let entity = Proxy :: Proxy "dispatchtest"
                let ds1 = Proxy :: Proxy "dispatch1"
                let ds2 = Proxy :: Proxy "dispatch2"
                let Just (Dispatch1 ref1) = accessState state entity ds1
                let Just (Dispatch2 ref2) = accessState state entity ds2

                -- Foreign keys are presents for dispatch1 and dispatch2, but only
                -- dispatch2 contains a document.
                let fk1' = "dispatch1-lolno"
                (fk2', doc) <- newTestDocument "dispatch2-" Nothing ref2

                result <- testHandler store $ do
                    (ik :: InternalKey "dispatchtest") <- createInternalKey
                    let fk1 = ForeignKey fk1' :: ForeignKey "dispatchtest" "dispatch1"
                    let fk2 = ForeignKey (T.unpack fk2') :: ForeignKey "dispatchtest" "dispatch2"
                    dispatch . show $ ("dispatchtest", "dispatch1", fk1')

                either (error . show) (const . return $ ()) $ first show result

                -- Both stores should be empty, along with both the retcon and
                -- retcon_fk tables.
                d1 <- readIORef ref1
                d2 <- readIORef ref2

                {-
                [Only (n_ik::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon"
                [Only (n_fk::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"
                -}
                let n_ik = 0
                let n_fk = 0

                _ <- finaliseEntities state
                (M.size d1, M.size d2, n_ik, n_fk) `shouldBe` (0,0,0,0)

  where
    run opt state store action = runRetconHandler opt state (token store) action

namesSuite :: Spec
namesSuite = describe "Human readable names" $ do
    it "should be known for entities" $ do
        let entity = SomeEntity (Proxy :: Proxy "dispatchtest")
        someEntityName entity `shouldBe` "dispatchtest"

    it "should be known for data sources of an entity" $ do
        let entity = SomeEntity (Proxy :: Proxy "dispatchtest")
        someEntityNames entity `shouldBe` ("dispatchtest", ["dispatch1", "dispatch2"])

-- | Setup and teardown for the initial document tests.
prepareDatabase :: IO () -> IO ()
prepareDatabase action = bracket setupSuite teardownSuite (const action)
  where
    dbname :: String
    dbname = BS.unpack testDBName

    setupSuite :: IO ()
    setupSuite = return ()
    {-
    do
        _ <- system $ concat [ "dropdb --if-exists ", dbname , " >/dev/null 2>&1"
                             , " && createdb ", dbname
                             , " && psql --quiet --file=retcon.sql ", dbname
                             ]
        return ()
    -}

    teardownSuite :: () -> IO ()
    teardownSuite () = return ()
    {-
    do
        _ <- system $ concat [ "dropdb --if-exists ", dbname, " >/dev/null 2>&1"
                             ]
        return ()
    -}

-- | Setup and teardown for the dispatch test suite.
prepareDispatchSuite :: IO () -> IO ()
prepareDispatchSuite action = bracket setup teardown (const action)
  where
    -- Create and initialise the database and data sources.
    setup :: IO ()
    setup = return ()

    -- Clean up the database and data sources.
    teardown :: () -> IO ()
    teardown = return

-- | Open a connection to the configured database.
withConfiguration :: RetconOptions
                  -> ((Memory.MemStorage, RetconOptions) -> IO r)
                  -> IO r
withConfiguration opt = bracket openConnection closeConnection
  where
    openConnection = do
        ref <- newIORef Memory.emptyState
        return (Memory.MemStorage ref, opt)
    closeConnection (Memory.MemStorage ref, _) = do
        writeIORef ref Memory.emptyState

-- * Initial Document Tests
--
-- $ These tests exercise the storage of initial documents in the retcon
-- database.

instance RetconEntity "alicorn_invoice" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "alicorn_source") ]

instance RetconDataSource "alicorn_invoice" "alicorn_source" where

    data DataSourceState "alicorn_invoice" "alicorn_source" = Alicorn

    initialiseState = return Alicorn

    finaliseState Alicorn = return ()

    getDocument key = error "We're not calling this"
    setDocument doc key = error "We're not calling this"
    deleteDocument key = error "We're not calling this"

-- | Test suite for initial document.
initialDocumentSuite :: Spec
initialDocumentSuite = around prepareDatabase $ do
    let opt = defaultOptions {
          optDB = testConnection
        , optLogging = LogNone
        }

    describe "Initial documents" $ do
        it "reads and writes initial documents" $
            withConfiguration opt $ \(store, _) -> do
                let testDoc = Document [(["key"], "value")]
                result <- testHandler store $ do

                    (ik :: InternalKey "alicorn_invoice") <- createInternalKey
                    put <- recordInitialDocument ik testDoc
                    get <- getInitialDocument ik

                    return (put, get)

                result `shouldBe` Right ((), Just testDoc)

        it "deletes initial documents" $
            withConfiguration opt $ \(store, _) -> do
                let testDoc = Document [(["isThisGoingToGetDeleted"], "yes")]
                result <- testHandler store $ do
                    (ik :: InternalKey "alicorn_invoice") <- createInternalKey
                    maybePut <- recordInitialDocument ik testDoc
                    maybeDel <- deleteInitialDocument ik
                    maybeGet <- getInitialDocument ik
                    return (maybePut, maybeDel, maybeGet)

                result `shouldBe` Right ((), (), Nothing)

-- | Test suite for diff database handling.
diffDatabaseSuite :: Spec
diffDatabaseSuite = around prepareDatabase $ do
    let opt = defaultOptions {
          optDB = testConnection
        , optLogging = LogNone
        }

    describe "Database diffs" $
        it "writes a diff to database and reads it" $
            withConfiguration opt $ \(store,_ ) -> do
                pendingWith "Data storage API changes."
                let testDiff = Diff 1 [InsertOp 1 [T.pack "a",T.pack "b",T.pack "c"] (T.pack "foo")]

                result <- testHandler store $ do
                    -- Create an InternalKey and a ForeignKey
                    ik <- createInternalKey
                    let fk = ForeignKey "1" :: ForeignKey "alicorn_invoice" "alicorn_source"
                    recordForeignKey ik fk

                    mid <- putDiffIntoDb fk testDiff
                    all_diffs <- getInitialDocumentDiffs ik

                    return (mid, diffChanges . head $ all_diffs)

                result `shouldBe` Right (Just 1, diffChanges testDiff)

testHandler :: Memory.MemStorage
            -> RetconHandler RWToken a
            -> IO (Either RetconError a)
testHandler store a = runRetconHandler defaultOptions [] (token store) a

main :: IO ()
main = hspec $ do
    namesSuite
    operationSuite
    dispatchSuite
    initialDocumentSuite
    diffDatabaseSuite
