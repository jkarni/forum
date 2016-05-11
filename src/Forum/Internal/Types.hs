{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Forum.Internal.Types where

import Prelude hiding ((.), id)
import Data.ByteString (ByteString)
import Control.Category (Category(..))
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Data.String (IsString(..))

import Forum.Internal.Encodable

data StatementType = UpdateStmt | QueryStmt
  deriving (Eq, Show, Read, Generic)

data Name
  = SimpleName ByteString
  | CountName ByteString Int
  deriving (Eq, Show, Read, Generic)

instance IsString Name where
  fromString = SimpleName . fromString

data Statement (st :: StatementType) a b
  = Statement { getStatement :: (Name, [Name], [Stmt st]) -> (Name, [Name], [Stmt st])}
  deriving (Generic)

instance Category (Statement st) where
  id = Statement id
  Statement a . Statement b = Statement (a . b)

instance Monoid (Statement 'UpdateStmt a ()) where
  mempty = Statement id
  --  The requirement here is that a @Statement s () x@ will never make
  --  assumptions about what table it gets. This seems sensible, but it still
  --  deserves to be documented here.
  Statement f `mappend` Statement g = Statement $ \(n, ns, ss) ->
    let (_, _, s1) = f (n, ns, ss)
    in g (n, ns, s1)

-- | A key to some other table. Represents a "REFERENCES" in SQL.
--
-- Thus, @Key User "id" Int@ is a reference to the "id" key in the table for
-- @User@.
newtype Key tbl (ref :: Symbol) a = Key { unKey :: a }
  deriving (Functor, Eq, Show, Read, Generic, Num, Enum, Ord)



-- * SQL AST

data Stmt (st :: StatementType) where
  CreateView :: Name -> Select -> Stmt st
  Update     :: Encodable a => Name -> [Name] -> a -> Stmt UpdateStmt


data Select
  = Select Fields FromCls WhereCls
  deriving (Eq, Show, Read, Generic)


data Fields
  = Star
  | Fields [Name]
  deriving (Eq, Show, Read, Generic)

data FromCls
  = FromCls Name
  | InnerJoin Name Name OnCls
  deriving (Eq, Show, Read, Generic)

data OnCls = On Name Name
  deriving (Eq, Show, Read, Generic)

data WhereCls
  = Unop ByteString ByteString
  | Binop ByteString ByteString ByteString
  | InCls ByteString Select
  | NoWhereCls
  deriving (Eq, Show, Read, Generic)

-- * SqlExpr



-- * TH-related

data THOptions = THOptions
  { fnRenamer :: String -> String -> String
  -- ^ How to name the field functions produced. The first argument is type
  -- name, and the second is the field name.
  , dbRenamer :: String -> String -> String
  -- ^ What the database field's name is. The first argument is type
  -- name, and the second is the field name.
  , tableFnRenamer :: String -> String
  -- ^ How to name the table function produced. The first argument is the type name.
  , tableDbRenamer :: String -> String
  -- ^ What the database table's name is. The first argument is the type name.
  } deriving (Generic)
