module Forum.Internal.Decodable where

import Data.Default.Class (def)
import qualified Hasql.Decoders as Hasql
import Data.Time
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int

class Decodable a where
  decode :: Hasql.Row a

instance Decodable Char where
  decode = Hasql.value def
instance Decodable Bool where
  decode = Hasql.value def
instance Decodable Int16 where
  decode = Hasql.value def
instance Decodable Int32 where
  decode = Hasql.value def
instance Decodable Int64 where
  decode = Hasql.value def
instance Decodable Double where
  decode = Hasql.value def
instance Decodable Float where
  decode = Hasql.value def
instance Decodable Text where
  decode = Hasql.value def
instance Decodable ByteString where
  decode = Hasql.value def
instance Decodable DiffTime where
  decode = Hasql.value def
instance Decodable UTCTime where
  decode = Hasql.value def
instance Decodable Day where
  decode = Hasql.value def
instance Decodable TimeOfDay where
  decode = Hasql.value def
instance Decodable LocalTime where
  decode = Hasql.value def

