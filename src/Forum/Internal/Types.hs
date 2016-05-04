{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Forum.Internal.Types where

import Prelude hiding ((.), id)
import Data.ByteString (ByteString)
import Control.Category (Category(..))
import GHC.Generics (Generic)
import Data.String (IsString(..))

data StatementType = UpdateStmt | QueryStmt
  deriving (Eq, Show, Read, Generic)

data Name
  = SimpleName ByteString
  | CountName ByteString Int
  deriving (Eq, Show, Read, Generic)

instance IsString Name where
  fromString = SimpleName . fromString

data Statement (st :: StatementType) a b
  = Statement { getStatement :: (Name, [Stmt st]) -> (Name, [Stmt st])}
  deriving (Generic)

instance Category (Statement st) where
  id = Statement id
  Statement a . Statement b = Statement (a . b)

instance Monoid (Statement 'UpdateStmt a ()) where
  mempty = Statement id
  --  The requirement here is that a @Statement s () x@ will never make
  --  assumptions about what table it gets. This seems sensible, but it still
  --  deserves to be documented here.
  Statement f `mappend` Statement g = Statement $ \(n, ss) ->
    let (_, s1) = f (n, ss)
    in g (n, s1)

newtype Key tbl a = Key { unKey :: a }
  deriving (Functor, Eq, Show, Read, Generic, Num, Enum, Ord)



-- * SQL AST

data Stmt (st :: StatementType) where
  CreateView :: Name -> Select -> Stmt st


data Select =
  Select Fields FromCls WhereCls
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
