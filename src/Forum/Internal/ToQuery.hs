module Forum.Internal.ToQuery where

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Control.Monad.Identity (Identity(..))
import qualified Text.Show.ByteString as BS
import Forum.Internal.Types
import Forum.Internal.Encodable
import Forum.Internal.Decodable

import qualified Hasql.Session as Hasql
import qualified Hasql.Query as Hasql
import qualified Hasql.Decoders as Hasql


import Debug.Trace

-- | The randomly generated string @jVc508YFgIwgy7lAmNXG@
-- Don't otherwise use this string in your SQL!
randomName :: Name
randomName = CountName "jVc508YFgIwgy7lAmNXG" 0

namesTill :: Name -> [Name]
namesTill (CountName x n) = [CountName x n' | n' <- [1..n]]

statementToQuery :: Statement st () b -> (Name, Hasql.Session ())
statementToQuery s = (n, sequence_ (stmtToQuery <$> stmts))
  where
   (n, ns, stmts) = getStatement s (randomName, [], [])

stmtToQuery :: Stmt s -> Hasql.Session ()
stmtToQuery (CreateView n s)
  = Hasql.sql $ traceShowId
    $ "CREATE TEMP VIEW "
    <> nameToQuery n <> " AS ( " <> selectToQuery s <> " ); "
stmtToQuery (Update n ns v)
  = Hasql.query v $ Hasql.statement stmt encode Hasql.unit True
  where
    stmt = traceShowId $
     "UPDATE " <> nameToQuery n <>
     " SET " <> BS.intercalate ", " (nameToQuery <$> ns) <> " = $1;"
stmtToQuery (Insert n v)
  = Hasql.query v $ Hasql.statement stmt encode Hasql.unit True
  where
    addOne new old = old <> ", $" <> toStrict (BS.show new) -- <> ", $" <> old
    mkParams = BS.drop 1 $ foldr addOne "" $ reverse [1 .. (fieldCount $ Identity v)]
    stmt = traceShowId $
     "INSERT INTO " <> nameToQuery n <>
     " VALUES ( " <> mkParams <> " );"

nameToQuery :: Name -> ByteString
nameToQuery (SimpleName s) = s
nameToQuery (CountName s n) = s <> toStrict (BS.show n)

selectToQuery :: Select -> ByteString
selectToQuery (Select fields from where_)
  = "SELECT " <> fieldsToQuery fields <> " " <> fromToQuery from <> whereToQuery where_

fieldsToQuery :: Fields -> ByteString
fieldsToQuery Star = "*"
fieldsToQuery (Fields fs) = " " <> BS.intercalate ", " (nameToQuery <$> fs) <> ""

whereToQuery :: WhereCls -> ByteString
whereToQuery NoWhereCls = " "
whereToQuery (Binop lhs op rhs) = " WHERE " <> lhs <> " " <> op <> " " <> rhs
whereToQuery _ = error "TODO"

fromToQuery :: FromCls -> ByteString
fromToQuery (FromCls v) = "FROM " <> nameToQuery v
fromToQuery (InnerJoin t1 t2 oncls)
  = "FROM " <> nameToQuery t1 <> " INNER JOIN " <> nameToQuery t2 <> onToQuery oncls

onToQuery :: OnCls -> ByteString
onToQuery (On a b) = " ON (" <> nameToQuery a <> " = " <> nameToQuery b <> " )"
