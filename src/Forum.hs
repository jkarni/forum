module Forum
 ( Decodable(..)
 , mkStmts
 , Key(..)
 , THOptions(..)
 , defaultTHOptions
 , Statement(..)
 , StatementType(..)
 , run
 , Category(..)
 , Connection
 , settings
 , acquire
 , release

 , set

 , (#)
 )
where

import Prelude (flip)
import Forum.Internal
import Control.Category (Category(..))
import Hasql.Connection (Connection, settings, acquire, release)

(#) :: Category cat => cat a b -> cat b c -> cat a c
(#) = flip (.)
