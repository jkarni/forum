module Forum.Internal.Utils where

import Data.Char (toLower)
import Data.Proxy (Proxy(..))
import Data.Monoid ((<>))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Hasql.Query as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Connection as Hasql

import Forum.Internal.Types
import Forum.Internal.Decodable
import Forum.Internal.ToQuery
import Forum.Internal.HasTable

nextName :: Name -> Name
nextName (CountName n x) = CountName n $ succ x
nextName _ = error "Can only be used with CountName"

addSelect :: (Name -> Select) -> Statement st a b
addSelect f = Statement $ \(n, ss) -> (nextName n, ss ++ [CreateView (nextName n) (f n)])


mkTableStmt :: Name -> Statement st a b
mkTableStmt n = addSelect $ \_ -> Select Star (FromCls n) NoWhereCls

mkFieldStmt :: Name -> Statement st a b
mkFieldStmt field = addSelect $ \tbl -> Select (Fields [field]) (FromCls tbl) NoWhereCls


mkFKStmt :: forall st a b field v. (KnownSymbol field, HasTable b)
    => Proxy (Key b field v) -> Name -> Statement st a b
mkFKStmt _ n = addSelect $ \tbl ->
  Select (Fields [ntStar]) (InnerJoin tbl nt
                      $ On (addField tbl (nameToQuery n)) (addField nt fName)) NoWhereCls
  where
    nt = SimpleName $ tableName (Proxy :: Proxy b)
    ntStar = SimpleName $ tableName (Proxy :: Proxy b) <> ".*"
    addField (SimpleName n) f = SimpleName (n <> "." <> f)
    addField (CountName n c) f = SimpleName (n <> fromString (show c) <> "." <> f)
    fName = fromString (symbolVal (Proxy :: Proxy field))
    nameToQuery (SimpleName s) = s
    nameToQuery (CountName s n) = s <> fromString (show n)



run :: Decodable b => Hasql.Connection -> Statement st () b -> IO (Either Hasql.Error [b])
run conn s = Hasql.run go conn
  where
    go = do
      let (n, q) = statementToQuery s
      Hasql.sql q
      Hasql.query () $ Hasql.statement
        (selectToQuery $ Select Star (FromCls n) NoWhereCls)
        Hasql.unit decode True

unTitleCase :: String -> String
unTitleCase (x:xs) = toLower x : xs
unTitleCase [] = error "Empty name"
