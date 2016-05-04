module ForumSpec (spec) where

import Prelude hiding ((.))
import Data.Text (Text)
import System.Process
import Test.Hspec

import Forum
import Paths_forum

data Species = Species
  { speciesName  :: Text
  , speciesGenus :: Key Genus Text
  } deriving (Eq, Show, Read)

data Genus = Genus
  { genusName   :: Text
  , genusFamily :: Key Family Text
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
    run conn (speciesName_ . species) `shouldReturn` Right ["Tilia europea"]
    {-run conn $ genus . speciesGenus_-}

  {-it "allows querying over keys" $ \conn -> do-}
    {-run conn $ species . speciesGenus_ . genusFamily_ . familyName_-}

  {-it "allows querying for keys" $ \conn -> do-}
    {-run conn $ species . genus . familyKey-}

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
