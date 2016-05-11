module Forum.Internal.TH where

import Data.Char (isUpper, toLower)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Data.String (fromString)
import Data.Proxy (Proxy(..))

import Forum.Internal.Types
import Forum.Internal.Utils
import Forum.Internal.HasTable

mkStmts :: TH.Name -> THOptions -> TH.Q [TH.Dec]
mkStmts n opts = do
  r <- TH.reify n
  case r of
    TH.TyConI (TH.DataD _ctx typName _ [TH.RecC _conName fields] _)
      -> do
       fs <- mapM (mkField typName (TH.ConT typName) opts) fields
       f <- mkTable typName (TH.ConT typName) opts
       let fieldNames = (\(n, _, _) -> n) <$> fields
       inst <- mkHasTable typName fieldNames opts
       return . concat $ inst : f : fs
    a -> fail $ show a

mkField :: TH.Name -> TH.Type -> THOptions -> TH.VarStrictType -> TH.Q [TH.Dec]
mkField typName otyp opts (fieldName, _, typ) = do
  let name = TH.mkName $ fnRenamer opts (TH.nameBase typName) (TH.nameBase fieldName)
      dbname = dbRenamer opts (TH.nameBase typName) (TH.nameBase fieldName)
      ktyp = ''Key
  case typ of
    TH.AppT (TH.AppT (TH.AppT ktyp otherTbl) tblKey) keyTyp -> do
      t <- [t| forall st. Statement st $(return otyp) $(return otherTbl) |]
      d <- [d|
        $(TH.varP name) = mkFKStmt (Proxy :: Proxy $(return typ)) (fromString $(TH.litE $ TH.StringL dbname))
        |]
      return $ TH.SigD name t : d
    _ -> do
      t <- [t| forall st. Statement st $(return otyp) $(return typ) |]
      d <- [d|
        $(TH.varP name) = mkFieldStmt $ fromString $(TH.litE $ TH.StringL dbname)
        |]
      return $ TH.SigD name t : d

mkTable :: TH.Name -> TH.Type -> THOptions -> TH.Q [TH.Dec]
mkTable typName otyp opts = do
  let name = TH.mkName $ tableFnRenamer opts (TH.nameBase typName)
  t <- [t| forall st. Statement st () $(return otyp) |]
  d <- [d| $(TH.varP name) = mkTableStmt tbl fields
             where
               proxy = Proxy :: Proxy $(TH.conT typName)
               tbl = SimpleName $ tableName proxy
               fields = SimpleName <$> tableFields proxy
    |]
  return $ TH.SigD name t : d

mkHasTable :: TH.Name -> [TH.Name] -> THOptions -> TH.Q [TH.Dec]
mkHasTable typName fields opts = do
  let dbname = tableDbRenamer opts (TH.nameBase typName)
      fields' = dbRenamer opts (TH.nameBase typName) . TH.nameBase <$> fields
      fieldExps = TH.ListE (TH.LitE . TH.StringL <$> fields')
  [d| instance HasTable $(TH.conT typName) where
        tableName = const $ fromString $(TH.litE $ TH.StringL dbname)
        tableFields _ = fromString <$> $(return fieldExps)
    |]


defaultTHOptions :: THOptions
defaultTHOptions = THOptions
  { fnRenamer = \_typ field -> field ++ "_"
  , dbRenamer = \_typ field -> snakeCaseRenamer field
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
