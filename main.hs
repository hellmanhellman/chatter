import Utilities



-- pretty self explanatory
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) repl
  | w == x    = repl ++ substitute w xs repl
  | otherwise = (x : substitute w xs repl)


-- Almost word for word implementation from the website
-- very hard.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match w (x:xs) (y:ys)
  | x == y = match w xs ys
  | x == w = singleWildcardMatch (x:xs) (y:ys) `orElse` longerWildcardMatch (x:xs) (y:ys)
  | otherwise = Nothing

singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (w:xs) (y:ys)
  | (match w xs ys) /= Nothing = Just [y]
  | otherwise = Nothing

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (w:xs) (y:ys) = mmap (y :) (match w (w:xs) ys)


-- Main
main = print $ match '*' "* and *" "you and me"

