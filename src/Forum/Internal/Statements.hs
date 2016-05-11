module Forum.Internal.Statements where

import Forum.Internal.Encodable
import Forum.Internal.Types
import Forum.Internal.Utils

set :: Encodable a => a -> Statement UpdateStmt a ()
set = addUpdate

insert :: Encodable a => a -> Statement UpdateStmt a ()
insert = addInsert

{-(.==) :: Encodable b => Statement st a b -> b -> Statement st a a-}
{-(.==) = _-}

{-distinct :: Statement st a a-}
{-distinct = _-}

{-limit :: Int -> Statement st a a-}
{-limit = _-}

{-offset :: Int -> Statement st a a-}
{-offset = _-}

{-ascendingBy :: Statement a b -> Statement a a-}
{-ascendingBy = _-}

{-descendingBy :: Statement a b -> Statement a a-}
{-descendingBy = _-}


{-moveSelectToWhere :: Statement a b -> ByteString -> Statement a a-}
{-moveSelectToWhere (Statement stmt) cond = Statement $ \v ->-}

-- groupBy, suniqueum, etc.
