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
import Forum.Internal.Encodable
import Forum.Internal.ToQuery
import Forum.Internal.HasTable

nextName :: Name -> Name
nextName (CountName n x) = CountName n $ succ x
nextName _ = error "Can only be used with CountName"

addSelect :: (Name -> [Name] -> (Select, [Name])) -> Statement st a b
addSelect f = Statement $ (\(n, ns, ss) ->
  let (sel, ns') = f n ns
  in (nextName n, ns', ss ++ [CreateView (nextName n) sel]))

addUpdate :: Encodable a => a -> Statement UpdateStmt a ()
addUpdate a = Statement $ (\(n, ns, ss) ->
  (nextName n, [], ss ++ [Update n ns a]))

addInsert :: Encodable a => a -> Statement UpdateStmt a ()
addInsert as = Statement $ (\(n, ns, ss) -> (n, [], ss ++ [Insert n as]))

addDelete :: Statement UpdateStmt a ()
addDelete = Statement $ \(n, ns, ss) -> (n, [], ss ++ [Delete n])

mkTableStmt :: Name -> [Name] -> Statement st a b
mkTableStmt n ns = addSelect $ \_ _ -> (Select (Fields ns) (FromCls n) NoWhereCls, ns)

mkFieldStmt :: Name -> Statement st a b
mkFieldStmt field = addSelect $ \tbl _ns -> (Select (Fields [field]) (FromCls tbl) NoWhereCls, [field])




mkFKStmt :: forall st a b field v. (KnownSymbol field, HasTable b)
    => Proxy (Key b field v) -> Name -> Statement st a b
mkFKStmt _ name = addSelect $ \tbl _ ->
  (Select (Fields [ntStar]) (InnerJoin tbl nt
                      $ On (addField tbl (nameToQuery name)) (addField nt fName)) NoWhereCls
  , SimpleName <$> tableFields (Proxy :: Proxy b))
  where
    nt = SimpleName $ tableName (Proxy :: Proxy b)
    ntStar = SimpleName $ tableName (Proxy :: Proxy b) <> ".*"
    addField (SimpleName n) f = SimpleName (n <> "." <> f)
    addField (CountName n c) f = SimpleName (n <> fromString (show c) <> "." <> f)
    fName = fromString (symbolVal (Proxy :: Proxy field))


run :: Decodable b => Hasql.Connection -> Statement st () b -> IO (Either Hasql.Error [b])
run conn s = Hasql.run go conn
  where
    dropView name = Hasql.sql $ "DROP VIEW IF EXISTS " <> nameToQuery name <> " CASCADE;"
    go = do
      let (n, q) = statementToQuery s
      q
      -- TODO - this should only happen if we want a result.
      result <- Hasql.query () $ Hasql.statement
        (selectToQuery $ Select Star (FromCls n) NoWhereCls)
        Hasql.unit decode True
      mapM dropView $ reverse $ namesTill n
      return result

unTitleCase :: String -> String
unTitleCase (x:xs) = toLower x : xs
unTitleCase [] = error "Empty name"
