module Utilities where

-- Return the first thing if it's not Nothing, otherwise return the second.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- misleading name (maybe-map)?
-- It just applies f to the Maybe if it's not nothing.
-- should be called mapply or something
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

