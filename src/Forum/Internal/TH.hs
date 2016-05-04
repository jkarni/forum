module Forum.Internal.TH where

import Data.Char (isUpper, toLower)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Data.String (fromString)

import Forum.Internal.Types
import Forum.Internal.Utils

mkStmts :: TH.Name -> THOptions -> TH.Q [TH.Dec]
mkStmts n opts = do
  r <- TH.reify n
  case r of
    TH.TyConI (TH.DataD _ctx typName _ [TH.RecC _conName fields] _)
      -> do
       fs <- mapM (mkField typName (TH.ConT typName) opts) fields
       f <- mkTable typName (TH.ConT typName) opts
       return . concat $ f : fs
    a -> fail $ show a

-- TODO: Add type!
mkField :: TH.Name -> TH.Type -> THOptions -> TH.VarStrictType -> TH.Q [TH.Dec]
mkField typName otyp opts (fieldName, _, typ) = do
  let name = TH.mkName $ fnRenamer opts (TH.nameBase typName) (TH.nameBase fieldName)
      dbname = dbRenamer opts (TH.nameBase typName) (TH.nameBase fieldName)
  t <- [t| forall st. Statement st $(return otyp) $(return typ) |]
  d <- [d|
    $(TH.varP name) = mkFieldStmt $ fromString $(TH.litE $ TH.StringL dbname)
    |]
  return $ TH.SigD name t : d

-- TODO: Add type!
mkTable :: TH.Name -> TH.Type -> THOptions -> TH.Q [TH.Dec]
mkTable typName otyp opts = do
  let name = TH.mkName $ tableFnRenamer opts (TH.nameBase typName)
      dbname = tableDbRenamer opts (TH.nameBase typName)
  t <- [t| forall st. Statement st () $(return otyp) |]
  d <- [d| $(TH.varP name) = mkTableStmt $ fromString $(TH.litE $ TH.StringL dbname)
    |]
  return $ TH.SigD name t : d

defaultTHOptions :: THOptions
defaultTHOptions = THOptions
  { fnRenamer = \_typ field -> field ++ "_"
  , dbRenamer = \typ field -> snakeCaseRenamer $ drop (length typ) field
  , tableDbRenamer = unTitleCase
  , tableFnRenamer = unTitleCase
  }


snakeCaseRenamer :: String -> String
snakeCaseRenamer []     = error "Cannot be empty string"
snakeCaseRenamer (n:ns) = go [toLower n] ns
  where
    go r []                 = reverse r
    go r (x:xs) | isUpper x = go (toLower x : '_' : r) xs
                | otherwise = go (x : r) xs
