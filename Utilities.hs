module Utilities where

--Applies function f1 to first component of the tuple, and f2 to the second component.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Return the first thing if it's not Nothing, otherwise return the second.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- misleading name (maybe-map)?
-- It just applies f to the Maybe if it's not nothing.
-- should be called mapply or something
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap _ Nothing  = Nothing
mmap f (Just x)  = Just (f x)


--Try to "extract" the value of a Maybe type that is returned from a function. If the Maybe-function returns Nothing,  the try-function just returns the argument.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)


--Repeatedly apply the function until its value becomes "stable" (i.e. f(x) = x). Infinite loop if there is no such value?
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)


--Indexes a list by using numbers of the interval (0,1). For example, pick 0.5 will choose the middle index.
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
