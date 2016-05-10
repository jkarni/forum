module Forum.Internal.Statements where

(.==) :: Statement st a b -> b -> Statement st a a
(.==) = _

moveSelectToWhere :: Statement a b -> ByteString -> Statement a a
moveSelectToWhere (Statement stmt) cond = Statement $ \v ->

-- groupBy, sum, etc.
