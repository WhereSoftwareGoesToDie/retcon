{-# LANGUAGE OverloadedStrings #-}

module Retcon.Data.Storage where

data RetconDataStore = RetconDataStoreMemory
     				 | RetconDataStorePsql {
 						connStr :: ByteString
     				 } deriving Show

class RetconStorage RetconDataStoreMemory a where
	putData :: a
	getData :: Maybe a
	delData :: ()

class RetconStorage RetconDataStorePsql a where
	putData :: a
	getData :: Maybe a
	delData :: ()

setupPsql :: RetconDataStorePsql -> Connection
setupPsql store = 

instance (RetconDataSource entity source) => RetconStorage RetconDataStorePsql (ForeignKey entity source) where
	putData ds fk = bracket setupPsql shutdownPsql (run fk)
		where 
			run fk conn = do
				case (lookupInternalKey fk) of
					Nothing -> error "No internal key"
					Just ik -> do
						let (entity, source, fid) = foreignKeyValue fk
						let (entity', iid) = internalKeyValue ik
						let values = (entity, iid, source, fid)
		    			let sql = "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
		    			void $ liftIO $ execute conn sql values
		    			return fk
	getData = do
		let conn = (connStr ds)
	delData = do


