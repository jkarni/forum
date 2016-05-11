module Forum
 ( Decodable(..)
 , Encodable(..)
 , mkStmts
 , Key(..)
 , THOptions(..)
 , defaultTHOptions
 , Statement(..)
 , StatementType(..)
 , run
 , run_
 , Category(..)
 , Connection
 , settings
 , acquire
 , release

 , set
 , insert
 , delete

 , (#)
 )
where

import Prelude (flip)
import Forum.Internal
import Control.Category (Category(..))
import Hasql.Connection (Connection, settings, acquire, release)

(#) :: Category cat => cat a b -> cat b c -> cat a c
(#) = flip (.)
