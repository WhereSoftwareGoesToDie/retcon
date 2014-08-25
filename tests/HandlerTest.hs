--
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

module Main where

import Test.Hspec

import Retcon.Config
import Retcon.DataSource
import Retcon.Document
import Retcon.Error
import Retcon.Handler
import Retcon.Monad
import Retcon.Options
import TestHelpers

import Control.Applicative
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import GHC.TypeLits
import System.IO
import System.IO.Unsafe
import System.Process

testDBName :: ByteString
testDBName = "retcon_handler_test"

testConnection :: ByteString
testConnection = BS.concat [BS.pack "dbname='", testDBName, BS.pack "'"]

-- * Dispatching Tests
--
-- $ These tests exercise the logic used in handling events appropriately.

-- | Data to be used by '"dispatch1"' below.
dispatch1Data :: IORef (Map Text Value)
dispatch1Data = unsafePerformIO $ newIORef M.empty
{-# NOINLINE dispatch1Data #-}

-- | Data to be used by '"dispatch2"' below.
dispatch2Data :: IORef (Map Text Value)
dispatch2Data = unsafePerformIO $ newIORef M.empty
{-# NOINLINE dispatch2Data #-}

-- | Create a new document in one of these stores.
newTestDocument :: Text
                -> Maybe Value
                -> IORef (Map Text Value)
                -> IO (Text, Value)
newTestDocument n doc ref = do
    doc <- return $ maybe (Object HM.empty) id doc
    key <- atomicModifyIORef ref (\store ->
        let key = n `T.append` ( T.pack . show $ M.size store)
        in (M.insert key doc store, key))
    return (key, doc)

-- | In-memory data source delete operation
deleteDocumentIORef :: (RetconDataSource entity source)
                    => IORef (Map Text Value)
                    -> ForeignKey entity source
                    -> DataSourceAction ()
deleteDocumentIORef ref (ForeignKey fk') = do
    let k = T.pack fk'
    liftIO $ atomicModifyIORef ref (\m -> (M.delete k m, ()))

setDocumentIORef :: (RetconDataSource entity source)
                 => String
                 -> IORef (Map Text Value)
                 -> Document
                 -> Maybe (ForeignKey entity source)
                 -> DataSourceAction (ForeignKey entity source)
setDocumentIORef name ref doc (Nothing) = do
    k <- liftIO $ atomicModifyIORef ref
        (\m -> let k = T.pack $ name ++ (show . M.size $ m)
               in (M.insert k (toJSON doc) m, k))
    liftIO . print $ "Setting a new document in " ++ name ++ " " ++ T.unpack k
    return $ ForeignKey $ T.unpack k

setDocumentIORef name ref doc (Just (ForeignKey fk')) = do
    let fk = T.pack fk'
    liftIO . print $ "Setting a document in " ++ name ++ " " ++ fk'
    k <- liftIO $ atomicModifyIORef ref
        (\m -> (M.insert fk (toJSON doc) m, fk))
    return $ ForeignKey $ T.unpack k

getDocumentIORef :: (RetconDataSource entity source)
                 => IORef (Map Text Value)
                 -> ForeignKey entity source
                 -> DataSourceAction Document
getDocumentIORef ref (ForeignKey fk') = do
    let key = T.pack $ fk'
    doc' <- liftIO $ atomicModifyIORef ref
        (\m -> (m, M.lookup key m))
    case doc' of
        Just doc -> case fromJSON doc of
            Success r -> return r
            _         -> throwError RetconFailed
        Nothing  -> throwError RetconFailed

dispatchConfig = RetconConfig [ SomeEntity (Proxy :: Proxy "dispatchtest") ]

instance RetconEntity "dispatchtest" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "dispatch1")
                      , SomeDataSource (Proxy :: Proxy "dispatch2")
                      ]

instance RetconDataSource "dispatchtest" "dispatch1" where
    getDocument = getDocumentIORef dispatch1Data

    setDocument = setDocumentIORef "dispatch1-" dispatch1Data

    deleteDocument = deleteDocumentIORef dispatch1Data

instance RetconDataSource "dispatchtest" "dispatch2" where
    getDocument = getDocumentIORef dispatch2Data

    setDocument = setDocumentIORef "dispatch2-" dispatch2Data

    deleteDocument = deleteDocumentIORef dispatch2Data

-- | Test suite for dispatching logic.
dispatchSuite :: Spec
dispatchSuite = around (prepareDatabase . prepareDispatchSuite) $ do
    let opts = defaultOptions {
          optDB = testConnection
        , optVerbose = True
        , optLogging = LogStderr
        }

    describe "Dispatching changes" $ do
        -- NEW and EXISTS
        it "should create when new id and source has the document" $
            withConfiguration opts $ \(conn, opts) -> do

                readIORef dispatch1Data >>= print
                readIORef dispatch2Data >>= print

                -- Create a document in dispatch1; leave dispatch2 and the
                -- database tables empty.
                (fk, doc) <- newTestDocument "dispatch1-" Nothing dispatch1Data

                -- Dispatch the event.
                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", fk] }
                let input = show ("dispatchtest", "dispatch1", fk)
                res <- retcon opts' dispatchConfig conn input

                -- The document should be present in both stores, with an
                -- InternalKey and two ForeignKeys.

                d1 <- readIORef dispatch1Data
                d2 <- readIORef dispatch2Data

                [Only (n_ik::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon"
                [Only (n_fk::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"

                (M.size d1, M.size d2, n_ik, n_fk) `shouldBe` (1, 1, 1, 2)

        -- NEW and NO EXIST
        it "should error when new id and source hasn't the document" $
            withConfiguration opts $ \(conn, opts) -> do
                -- Both dispatch1 and dispatch2, and the database tables are
                -- still empty.

                -- Dispatch the event.
                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", "999"] }
                let input = show ("dispatchtest", "dispatch1", "999")
                -- TODO catch!
                res <- retcon opts' dispatchConfig conn input

                -- Check that the data sets are still empty.
                d1 <- readIORef dispatch1Data
                d2 <- readIORef dispatch2Data
                (d1, d2) `shouldBe` (M.empty, M.empty)

                -- Check that there are still no InternalKey or ForeignKey details
                -- in the database.
                (n_ik :: [Only Int]) <- liftIO $ query_ conn "SELECT COUNT(*) FROM retcon;"
                (n_fk :: [Only Int]) <- liftIO $ query_ conn "SELECT COUNT(*) FROM retcon_fk;"
                (n_ik, n_fk) `shouldBe` ([Only 0], [Only 0])

                pendingWith "Setup and teardown will be horrible."

        -- OLD and EXISTS
        it "should update when old id and source has the document" $
            withConfiguration opts $ \(conn, opts) -> do
                -- Create a new document in both data sources
                (fk1, doc) <- newTestDocument "dispatch1-" Nothing dispatch1Data
                (fk2, _) <- newTestDocument "dispatch2-" (Just doc) dispatch2Data

                -- Add a new InternalKey and ForeignKeys for both data sources.
                [Only (ik :: Int)] <- query_ conn "INSERT INTO retcon (entity) VALUES ('dispatchtest') RETURNING id"
                executeMany conn
                    "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
                    ([ ("dispatchtest", ik, "dispatch1", fk1)
                     , ("dispatchtest", ik, "dispatch2", fk2)
                     ] :: [(String, Int, String, Text)])

                -- Dispatch the event.
                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", fk1] }
                let input = show ("dispatchtest", "dispatch1", fk1)
                res <- retcon opts' dispatchConfig conn input

                -- Check that update was dispatched?
                -- Check that the document is in dispatch1.
                -- Check that the document is in dispatch2.
                -- Check that the key for dispatch2 is in retcon_fk.
                -- Check that the key for dispatch1 is in retcon_fk.
                pendingWith "Setup and teardown will be horrible."

        -- OLD and NO EXIST
        it "should delete when old id and source hasn't the document" $
            withConfiguration opts $ \(conn, opts) -> do
                -- Foreign keys are presents for dispatch1 and dispatch2, but only
                -- dispatch2 contains a document.
                let fk1 = "dispatch1-lolno"
                (fk2, doc) <- newTestDocument "dispatch2-" Nothing dispatch2Data

                [Only (ik :: Int)] <- query_ conn "INSERT INTO retcon (entity) VALUES ('dispatchtest') RETURNING id"
                executeMany conn
                    "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
                    ([ ("dispatchtest", ik, "dispatch1", fk1)
                     , ("dispatchtest", ik, "dispatch2", fk2)
                     ] :: [(String, Int, String, Text)])

                let opts' = opts { optArgs = ["dispatchtest", "dispatch1", fk1] }
                let input = show ("dispatchtest", "dispatch1", fk1)
                res <- retcon opts' dispatchConfig conn input

                -- Check that delete was dispatched.
                -- dispatch1Data == M.empty
                -- dispatch2Data == M.empty
                -- SELECT COUNT(*) FROM retcon_fk; == 0
                -- SELECT COUNT(*) FROM retcon; == 0

                -- Both stores should be empty, along with both the retcon and
                -- retcon_fk tables.
                d1 <- readIORef dispatch1Data
                d2 <- readIORef dispatch2Data

                [Only (n_ik::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon"
                [Only (n_fk::Int)] <- query_ conn "SELECT COUNT(*) FROM retcon_fk"

                (M.size d1, M.size d2, n_ik, n_fk) `shouldBe` (0,0,0,0)



-- | Setup and teardown for the initial document tests.
prepareDatabase :: IO () -> IO ()
prepareDatabase action = bracket setupSuite teardownSuite (const action)
  where
    dbname :: String
    dbname = BS.unpack testDBName

    setupSuite :: IO ()
    setupSuite = do
        _ <- system $ concat [ "dropdb --if-exists ", dbname , " >/dev/null 2>&1"
                             , " && createdb ", dbname
                             , " && psql --quiet --file=retcon.sql ", dbname
                             ]
        return ()

    teardownSuite :: () -> IO ()
    teardownSuite () = do
        _ <- system $ concat [ "dropdb --if-exists ", dbname, " >/dev/null 2>&1"
                             ]
        return ()

-- | Setup and teardown for the dispatch test suite.
prepareDispatchSuite :: IO () -> IO ()
prepareDispatchSuite action = bracket setup teardown (const action)
  where
    -- Create and initialise the database and data sources.
    setup :: IO ()
    setup = do
        -- Empty the data sources.
        writeIORef dispatch1Data M.empty
        writeIORef dispatch2Data M.empty
        -- Empty the tables.
        -- DELETE FROM retcon_initial;
        -- DELETE FROM retcon_fk;
        -- DELETE FROM rtcon;
        return ()

    -- Clean up the database and data sources.
    teardown :: () -> IO ()
    teardown = return

-- | Open a connection to the configured database.
withConfiguration cfg = bracket openConnection closeConnection
  where
    openConnection = do
        conn <- connectPostgreSQL $ optDB cfg
        return (conn, cfg)
    closeConnection (conn, cfg) = close conn

-- * Initial Document Tests
--
-- $ These tests exercise the storage of initial documents in the retcon
-- database.

instance RetconEntity "alicorn_invoice" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "alicorn_source") ]

instance RetconDataSource "alicorn_invoice" "alicorn_source" where
    getDocument key = error "We're not calling this"
    setDocument doc key = error "We're not calling this"
    deleteDocument key = error "We're not calling this"


-- | Test suite for initial document.
initialDocumentSuite :: Spec
initialDocumentSuite = around prepareDatabase $ do
    describe "Initial documents" $ do
        it "reads and writes initial documents" $ do
            let testDoc = Document [(["key"], "value")]
            result <- testHandler $ do
                conn <- asks retconConnection

                result <- liftIO $ query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only "alicorn_invoice" :: Only String)
                case result of
                    [Only ik_base] -> do
                        let ik = InternalKey ik_base :: InternalKey "alicorn_invoice"

                        maybePut <- putInitialDocument ik testDoc
                        maybeGet <- getInitialDocument ik
                        return $ (maybePut, maybeGet)
                    _ -> error "I have no idea what happened here"
            case result of
                Left e -> error (show e)
                Right (maybe_put, maybe_get) -> do
                    maybe_put `shouldBe` ()
                    maybe_get `shouldBe` (Just testDoc)

        it "deletes initial documents" $ do
            let testDoc = Document [(["isThisGoingToGetDeleted"], "yes")]
            result <- testHandler $ do
                conn <- asks retconConnection

                result <- liftIO $ query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only "alicorn_invoice" :: Only String)
                case result of
                    [Only ik_base] -> do
                        let ik = InternalKey ik_base :: InternalKey "alicorn_invoice"

                        maybePut <- putInitialDocument ik testDoc
                        maybeDel <- deleteInitialDocument ik
                        maybeGet <- getInitialDocument ik
                        return $ (maybePut, maybeDel, maybeGet)
                    _ -> error "I have no idea what happened here"
            case result of
                Left e -> error (show e)
                Right (maybe_put, maybe_del, maybe_get) -> do
                    maybe_put `shouldBe` ()
                    maybe_del `shouldBe` ()
                    maybe_get `shouldBe` Nothing

testHandler :: RetconHandler a -> IO (Either RetconError a)
testHandler a = bracket setupConn closeConn run
    where
        setupConn = connectPostgreSQL testConnection
        closeConn = close
        run conn = do
            let cfg = RetconConfig []
            result <- runRetconHandler defaultOptions cfg conn a
            return result

main :: IO ()
main = hspec $ do
    dispatchSuite
    initialDocumentSuite
