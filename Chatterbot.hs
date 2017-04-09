module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  i <- randomIO :: IO Float
  let phrasePairs = ppsToLower (map (map2 (id, (pick i))) brain)
  return (\x -> [unwords (rulesApply phrasePairs x)])

--Some help-methods for making Strings/Phrases lower case.
sToLower = map toLower
pToLower = map sToLower

--Converts the input phrase to lower case. The response phrase is unaffected.
ppsToLower = map (map2 (pToLower, id))

--Takes a list of pattern transformations and a phrase as arguments. Returns a transformed phrase.
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply transformations phrase
  | result == Nothing    = []
  | otherwise            = try (transformationsApply "*" reflect transformations) phrase
  where result = transformationsApply "*" reflect transformations phrase

--"Reflects" words of a phrase, e.g. replaces "me" with "you". If a word has no reflection, it remains unchanged.
reflect :: Phrase -> Phrase
reflect = map (flip get reflections)

--Helper method of reflect. Returns the corresponding word (e.g. "was" -> "were") if it is in the table, or else returns the input argument.
get :: String -> [(String,String)] -> String
get k table
  | length list == 0   = k
  | otherwise          = snd (head list)
  where list = [(k',v) | (k',v) <- table, k == k']

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (map2 (words, map words))


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) replacement
  | x == w   = replacement ++ (substitute w xs replacement)
  | otherwise       = x : (substitute w xs replacement)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match w (x:xs) (y:ys)
  | x == y = match w xs ys
  | x == w = singleWildcardMatch (x:xs) (y:ys) `orElse` longerWildcardMatch (x:xs) (y:ys)
  | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch (w:xs) (y:ys)
  | (match w xs ys) /= Nothing = Just [y]
  | otherwise = Nothing

longerWildcardMatch (w:xs) (y:ys) = mmap (y :) (match w (w:xs) ys)



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f str tuple  = mmap (substitute w (snd tuple)) subst
  where subst = mmap f (match w (fst tuple) str)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (x:xs) str
  | transformation == Nothing   = transformationsApply w f xs str
  | otherwise                   = transformation 
  where transformation = transformationApply w f str x
