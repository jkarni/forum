module Forum.Internal.Statements where

with :: Statement s a Bool -> Statement s a a -> Statement s a a
with s = _

-- groupBy, sum, etc.
