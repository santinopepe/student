-- Pilar Frutos and Santino Pepe

module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
import Control.Applicative (Alternative(empty))
import System.Console.GetOpt (ArgDescr(NoArg))

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

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

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
makePair (Rule (pat, temps)) = do
  u <- randomIO :: IO Double
  return (pat, pick u temps)

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
rulesApply rules phrase = 
  case transformationsApply reflect rules phrase of
    Nothing -> phrase
    Just p  ->  p

--- >>> reflect []
--- >>> reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"] == ["you", "will", "never", "see", "your",  "reflection", "in", "my", "eyes"]
-- []
-- True
reflect :: Phrase -> Phrase
reflect = concatMap reflectWords

reflectWords :: String -> [String]
reflectWords w = case lookup w reflections of
  Just r -> words r
  Nothing -> [w]


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
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
ruleCompile (pat, answers) =
  Rule (compiledPattern, compiledAnswers)
  where
    compiledPattern = stringToPattern "*" (map toLower pat)
    compiledAnswers = map (stringToPattern "*" . map toLower) answers

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- >>> mkPattern '*' "" == Pattern []
-- True
-- >>> mkPattern '*' "a*b" == Pattern [Item 'a',Wildcard,Item 'b']
-- True
-- >>> mkPattern '*' "Hi *!" == Pattern [Item 'H', Item 'i', Item ' ', Wildcard, Item '!']
-- True
mkPattern :: Eq a => a -> [a] -> Pattern a
mkPattern wc s = Pattern (map toElem s)
  where
    toElem x = 
      if x == wc 
        then Wildcard 
        else Item x
        
          

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
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

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
reductionsApply reductions = fix reduceOnce
  where
    reduceOnce phrase =
      case transformationsApply id reductions phrase of
        Nothing -> phrase
        Just p  -> p
      

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument

-- >>> substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37" == "3*cos(5.37) + 4 - 5.37"
substitute :: Eq a => Template a -> [a] -> [a]
substitute (Pattern p) input = replace p input
  where 
    replace [] input = []
    replace (x:xs) input =
      case x of
        Wildcard -> input ++ replace xs input 
        Item y -> y : replace xs input

   

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
--- >>> match (mkPattern 'x' "hello") ""
-- Nothing
--- >>> match (mkPattern 'x' "") "abc"
-- Nothing
--- >>> match (mkPattern '*' "frodo") "gandalf"
-- Nothing
--- >>> match (mkPattern 'x' "abcd") "abcd"
-- Just ""

--- >>> match (mkPattern 'x' "2*x+3") "2*7+3"
-- Just "7"

--- >>> match (mkPattern '*' "* and *") "you and me"
-- Just "you"

--- >>> match (mkPattern '*' "*do") "bdo"
--- >>> match (mkPattern '*' "*do") "dobedo"
--- >>> match (mkPattern '*' "*do") "bedobe"
-- Just "b"
-- Just "dobe"
-- Nothing

match :: Eq a => Pattern a -> [a] -> Maybe [a]
match (Pattern p) s = find p s
  where 
    find [] [] = Just []
    find [] s = Nothing 
    find p [] = Nothing 
    find (x:xs) (y:ys) = 
      case x of
        Wildcard -> case singleWildcardMatch (Pattern (x:xs)) (y:ys)  of
                    Nothing -> longerWildcardMatch (Pattern (x:xs)) (y:ys) 
                    Just m -> Just m
        Item z -> if z /= y 
                  then Nothing 
                  else find xs ys

-- Helper function to match

-- >>> singleWildcardMatch (mkPattern '*' "*do") "bdo"
-- >>> singleWildcardMatch (mkPattern '*' "*do") "dobedo"
-- >>> singleWildcardMatch (mkPattern '*' "*do") "bedobe"
-- Just "b"
-- Nothing
-- Nothing

--- >>> longerWildcardMatch (mkPattern '*' "*do") "bdo"
--- >>> longerWildcardMatch (mkPattern '*' "*do") "dobedo"
--- >>> longerWildcardMatch (mkPattern '*' "*do") "bedobe"
-- Nothing
-- Just "dobe"
-- Nothing
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]

longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) = 
  case match (Pattern (Wildcard:ps)) xs of
    Nothing -> Nothing
    Just zs -> Just (x:zs)




-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
transformationApply f s (p, t) = 
  case matchAndTransform f p s of
    Nothing -> Nothing
    Just r -> Just (substitute t r)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
transformationsApply f patterns s = foldl (orElse) Nothing [transformationApply f s (p, t) | (p, t) <- patterns]
  where orElse Nothing y = y
        orElse x _ = x
