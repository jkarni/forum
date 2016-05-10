{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module ForumSpec (spec) where

import Prelude hiding ((.))
import Data.Text (Text)
import System.Process
import Test.Hspec

import Forum
import Paths_forum

data Species = Species
  { speciesName  :: Text
  , speciesGenus :: Key Genus "genus_name" Text
  } deriving (Eq, Show, Read)

data Genus = Genus
  { genusName   :: Text
  , genusFamily :: Key Family "family_name" Text
  } deriving (Eq, Show, Read)

data Family = Family
  { familyName   :: Text
  } deriving (Eq, Show, Read)

mkStmts ''Species defaultTHOptions
mkStmts ''Genus defaultTHOptions
mkStmts ''Family defaultTHOptions

spec :: Spec
spec = describe "forum" $ before createTestDb $ do

  it "allows querying" $ \conn -> do
    run conn (speciesName_ . species)
      `shouldReturn` Right ["Tilia europeae", "Tilia tomentosa"]

  it "allows querying over keys" $ \conn -> do
    run conn (familyName_ . genusFamily_ . speciesGenus_ . species)
      `shouldReturn` Right ["Malvacea", "Malvacea"]

  {-it "allows querying for keys" $ \conn -> do-}
    {-run conn $ species . genus . familyKey-}

  it "allows filtering" $ \conn -> do
    run conn $ speciesName_
             . speciesName_ .== "Tilia europeae"
             . species
      `shouldReturn` Right ["Tilia europeae"]

createTestDb :: IO Connection
createTestDb = do
  callCommand dropCmd
  callCommand createCmd
  schemaFile <- getDataFileName "test/schema.sql"
  print schemaFile
  callCommand $ "psql --file '" ++ schemaFile
             ++ "' forum-test >/dev/null 2>/dev/null"
  econn <- acquire info
  case econn of
    Left e -> error $ show e
    Right conn -> return conn
  where
    dropCmd   = "dropdb forum-test >/dev/null 2>/dev/null || true"
    createCmd = "createdb forum-test >/dev/null 2>/dev/null || true"
    info      = "dbname=forum-test"
