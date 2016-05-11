module Forum.Internal.Encodable where

import Data.Default.Class (def)
import qualified Hasql.Encoders as Hasql
import Data.Time (DiffTime, UTCTime, Day, TimeOfDay, LocalTime)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant.Generic
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))

class Encodable a where
  encode :: Hasql.Params a
  default encode :: Deciding Encodable a => Hasql.Params a
  encode = deciding (Proxy :: Proxy Encodable) encode

instance Encodable Char where
  encode = Hasql.value def
instance Encodable Bool where
  encode = Hasql.value def
instance Encodable Int16 where
  encode = Hasql.value def
instance Encodable Int32 where
  encode = Hasql.value def
instance Encodable Int64 where
  encode = Hasql.value def
instance Encodable Double where
  encode = Hasql.value def
instance Encodable Float where
  encode = Hasql.value def
instance Encodable Text where
  encode = Hasql.value def
instance Encodable ByteString where
  encode = Hasql.value def
instance Encodable DiffTime where
  encode = Hasql.value def
instance Encodable UTCTime where
  encode = Hasql.value def
instance Encodable Day where
  encode = Hasql.value def
instance Encodable TimeOfDay where
  encode = Hasql.value def
instance Encodable LocalTime where
  encode = Hasql.value def

