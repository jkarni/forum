{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module ForumSpec (spec) where

import Prelude hiding ((.))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Process
import Test.Hspec

import Forum
import Paths_forum

data Species = Species
  { speciesName  :: Text
  , speciesGenus :: Key Genus "genus_name" Text
  } deriving (Eq, Show, Read, Generic, Encodable)

data Genus = Genus
  { genusName   :: Text
  , genusFamily :: Key Family "family_name" Text
  } deriving (Eq, Show, Read, Generic, Encodable)

data Family = Family
  { familyName   :: Text
  } deriving (Eq, Show, Read, Generic, Encodable)

mkStmts ''Species defaultTHOptions
mkStmts ''Genus defaultTHOptions
mkStmts ''Family defaultTHOptions

spec :: Spec
spec = describe "forum" $ before createTestDb $ after release $ do

  context "querying" $ do

    it "allows querying" $ \conn -> do
      run conn (species # speciesName_)
        `shouldReturn` Right ["Tilia europeae", "Tilia tomentosa"]

    it "allows querying over keys" $ \conn -> do
      run conn (species # speciesGenus_ # genusFamily_ # familyName_ )
        `shouldReturn` Right ["Malvacea", "Malvacea"]

    {-it "allows querying for keys" $ \conn -> do-}
      {-run conn $ species . genus . familyKey-}

    {-it "allows filtering" $ \conn -> do-}
      {-run conn $ speciesName_-}
               {-. (speciesName_ .== "Tilia europeae")-}
               {-. species-}
        {-`shouldReturn` Right ["Tilia europeae"]-}

  {-context "setting" $ do-}

    {-
    -- this works but can only properly be tested with 'with'
    it "allows setting" $ \conn -> do
      run conn $ species # speciesName_ # set "Tilia cordata"
        `shouldReturn` Right []
      run conn (species # speciesName_)
        `shouldReturn` Right ["Tilia cordata", "Tilia cordata"]
        -}

  context "inserting" $ do

    it "allows inserting" $ \conn -> do
      run conn (species # insert (Species "new" (Key "Tilia")))
       `shouldReturn` Right []
      run conn (species # speciesName_)
       `shouldReturn` Right ["Tilia europeae", "Tilia tomentosa", "new"]


createTestDb :: IO Connection
createTestDb = do
  callCommand dropCmd
  callCommand createCmd
  schemaFile <- getDataFileName "test/schema.sql"
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
