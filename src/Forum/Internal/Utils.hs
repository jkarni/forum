module Forum.Internal.Utils where

import Data.Char (toLower)

import qualified Hasql.Query as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Connection as Hasql

import Forum.Internal.Types
import Forum.Internal.Decodable
import Forum.Internal.ToQuery

nextName :: Name -> Name
nextName (CountName n x) = CountName n $ succ x
nextName _ = error "Can only be used with CountName"

addSelect :: (Name -> Select) -> Statement st a b
addSelect f = Statement $ \(n, ss) -> (nextName n, ss ++ [CreateView n (f n)])

mkTableStmt :: Name -> Statement st a b
mkTableStmt n = addSelect $ \_ -> Select Star (FromCls n) NoWhereCls

mkFieldStmt :: Name -> Statement st a b
mkFieldStmt field = addSelect $ \tbl -> Select (Fields [field]) (FromCls tbl) NoWhereCls

run :: Decodable b => Hasql.Connection -> Statement st () b -> IO (Either Hasql.Error [b])
run conn s = Hasql.run (Hasql.query () $ mkHasqlQuery s) conn

mkHasqlQuery :: Decodable b => Statement st () b -> Hasql.Query () [b]
mkHasqlQuery s =  Hasql.statement (statementToQuery s) Hasql.unit decode True

unTitleCase :: String -> String
unTitleCase (x:xs) = toLower x : xs
unTitleCase [] = error "Empty name"
