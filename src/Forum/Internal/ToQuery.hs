module Forum.Internal.ToQuery where

import Data.Monoid
import Data.ByteString
import Data.ByteString.Lazy (toStrict)
import Text.Show.ByteString as BS
import Forum.Internal.Types


-- | The randomly generated string @jVc508YFgIwgy7lAmNXG@
-- Don't otherwise use this string in your SQL!
randomName :: Name
randomName = CountName "jVc508YFgIwgy7lAmNXG" 0

statementToQuery :: Statement st () b -> ByteString
statementToQuery s = intercalate "\n" $ stmtToQuery <$> stmts
  where
   (_, stmts) = getStatement s (randomName, [])

nameToQuery :: Name -> ByteString
nameToQuery (SimpleName s) = s
nameToQuery (CountName s n) = s <> toStrict (BS.show n)

stmtToQuery :: Stmt s -> ByteString
stmtToQuery (CreateView n s)
  = "CREATE TEMP VIEW " <> nameToQuery n <> " AS ( " <> selectToQuery s <> " <> );"

selectToQuery :: Select -> ByteString
selectToQuery (Select fields from where_)
  = "SELECT " <> fieldsToQuery fields <> fromToQuery from <> whereToQuery where_

fieldsToQuery :: Fields -> ByteString
fieldsToQuery Star = "*"
fieldsToQuery (Fields fs) = "( " <> intercalate "," (nameToQuery <$> fs) <> ")"

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
