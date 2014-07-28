module TestHelpers where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           System.Directory
import           System.FilePath

import Retcon.Diff
import Retcon.Document

-- | Get the path to a test data file.
testDataFile :: FilePath -> IO FilePath
testDataFile file = do
    cwd <- getCurrentDirectory
    return $ joinPath [cwd, "tests", "data", file]

-- | Load a 'Document' from a JSON file.
testLoad :: FilePath -> IO (Maybe Document)
testLoad name = do
    file <- testDataFile name
    input <- BS.readFile file
    return $ decode input

