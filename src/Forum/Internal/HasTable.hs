module Forum.Internal.HasTable where

import Data.ByteString (ByteString)

-- | A class for datatypes that have a corresponding table.
class HasTable a where
  -- | Name of corresponding table in DB
  tableName :: proxy a -> ByteString
