{-
resources used:

- https://wiki.haskell.org/Getting_the_current_date
  Aided in understanding date date type and use.

- https://stackoverflow.com/questions/31342012/read-and-writing-to-file-in-haskell
  Helped understand reading and writing to files in Haskell.

- https://www.youtube.com/watch?v=X8XHXhSvfrY&t=1373s
  Used for understanding Haskell input/output and file handling.

- https://mail.haskell.org/pipermail/beginners/2010-October/005538.html
  Helped overcome issues of file locking and explaining simply.

- https://livebook.manning.com/book/get-programming-with-haskell/chapter-39#19
  Explaining HTTP requests in Haskell.

- https://github.com/15Dkatz/official_joke_api
  Official Joke API.

- https://stackoverflow.com/questions/47816174/how-to-sleep-or-delay-the-thread-in-haskell
  Explaining how to delay the thread.

- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms
  Explaining how to generate random values.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Control.Monad.State
import System.IO
import qualified Control.Exception as Exc
import qualified Network.HTTP.Simple as HTTP
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)


--------------------------------------------------------------------------------
{-
  reads entire file content, ensures no issus due to haskell being lazy
  preventing locked file when reading

  evaluate (length contents) forces IO to read the full file, causing the handle (han) to close
-}

fileRead :: FilePath -> IO String
fileRead path =
  withFile path ReadMode $ \han -> do
    contents <- hGetContents han
    Exc.evaluate (length contents)
    pure contents

-- Restore remembered facts from text file, skipping invalid lines
reviveMem :: IO (Map String String)
reviveMem = do
  fileContents <- fileRead "memoryFile.txt"
  let lineVals = lines fileContents
  let pairs = mapMaybe splitLine lineVals -- removes invalid entries
  pure (Map.fromList pairs)

-- splits into key value based on the first instance of ","
splitLine :: String -> Maybe (String, String)
splitLine line = 
  case break (== ',') line of
    (k, ',' : v) -> Just (k,v)
    _ -> Nothing
  

{- 
  Program defines memory as a record storing a map of string -> string and a Maybe Int
  Map contains facts to remember, Maybe Int contains last expression

  App is a wrapper for the combined state and IO monad
  Run Repl runs the loop and unwraps the state and Io
-}

data Memory = Memory
  { remember :: Map String String
  , prevExpr :: Maybe Int
  }

-- generates new state, based of previous conversations
newMem :: IO Memory
newMem = do
  mapping <- reviveMem
  pure Memory
    { remember = mapping
    , prevExpr = Nothing
    }

type App a = StateT Memory IO a

-- starts REPL
runREPL :: IO ()
runREPL = do
  runStart
  mem <- newMem
  evalStateT loop mem

-- basic loop that asks for input, process is and returns output forever
loop :: App ()
loop = forever $ do
  liftIO (putStr prompt)
  req <- liftIO getLine
  respondTo (readRequest req)

--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | All data types used

data Request = Hello | Time TimeRequest | Formula Expr | ToMemory String String | Recall String | Joke String | RPSmove Move | Unknown
  deriving (Eq, Ord, Show)

data TimeRequest = Today | Tomorrow | Since Day
  deriving (Eq, Ord, Show)

data Expr = Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Number Int | That
  deriving (Eq, Ord, Show)

data Move = Rock | Paper | Scissors
  deriving (Eq, Ord, Show, Enum)

data Result = Win | Draw | Loss

-- defined as a record containing two parts of the joke
-- Generic allows JSON to be parsed automatically based of JokeForm structure
data JokeForm = JokeForm
  { setup :: String
  , punchline :: String
  } deriving (Show, Generic)

-- indicates JokeForm is decoded from JSON
instance FromJSON JokeForm

{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}

readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  _ -> Unknown
  Right req -> req

-- Attempts each parse, returning the first valid parsing
parseRequest :: Parser Request
parseRequest = Hello <$ string "Hello"
  <|> Time Today <$ string "What day is it?"
  <|> Time Tomorrow <$ string "What day is it tomorrow?"
  <|> Time . Since <$> parseDay
  <|> Formula <$> (string "What is " *> parseExpr <* string "?") -- Consumes "What is " and "?" and returns parseExpr (also captures "what is that")
  <|> parseToMemory
  <|> parseRecall
  <|> parseJoke
  <|> string "I want to play rock paper scissors with " *> parseRPS 
  <|> string "Play RPS with " *> parseRPS

-- Returns RPSmove type and the move value played by the user
parseRPS :: Parser Request
parseRPS = do
  move <- (RPSmove Rock <$ string "rock")
    <|> (RPSmove Paper <$ string "paper")
    <|> (RPSmove Scissors <$ string "scissors")
  string "."
  pure move

-- returns joke data type and the type of joke it is (stored as string)
parseJoke :: Parser Request
parseJoke = do
  string "Tell me a joke about "
  x <- someTill anySingle (string ".")
  pure (Joke x)


-- Parses request to store in memory
parseToMemory :: Parser Request
parseToMemory = do
  string "Remember that "
  x <- someTill anySingle (string " is ") -- parses and stores chars until " is ", which is ignored
  y <- someTill anySingle (string ".")
  pure (ToMemory x y)

-- Parses request to retrieve from memory
parseRecall :: Parser Request
parseRecall = do
    string "Tell me about "
    y <- someTill anySingle (string ".")
    pure (Recall y)

{-
  parseExpr and parseRemainder work together to parse an expression and prevent infinite recurssion by using fold
  parseExpr evaluates the first number then 1 or more operator and number pairs, which are parsed by parseRemainder
  parseRemainder parses an operator and number, producing a tuple
  This is then processed by foldl, which combines the tuples and original numbers
-}

parseExpr :: Parser Expr
parseExpr = do
  num1 <- That <$ string "that" <|> Number <$> parseLonghand
  rhs <- many parseRemainder
  pure (foldl (\current (oper, num) -> oper current num) num1 rhs)

parseRemainder :: Parser (Expr -> Expr -> Expr,Expr)
parseRemainder = do
  oper <- Add <$ string " plus "
    <|> Subtract <$ string " minus "
    <|> Multiply <$ string " times "
  num <- That <$ string "that" <|> Number <$> parseLonghand
  pure (oper,num)

-- parses day format, returns as Day type
parseDay :: Parser Day
parseDay = do
  string "How long ago was "
  year <- read <$> count 4 digitChar
  char '-'
  month <- read <$> count 2 digitChar
  char '-'
  day <- read <$> count 2 digitChar
  pure (fromGregorian year month day)

{-
  eval passed previous parsed expression (maybe int) and current expression
  Uses pattern matches to recurssively break down expression

  Bind is used such that if Maybe is encountered the expression fails
-}

eval :: Maybe Int -> Expr -> Maybe Int
eval _ (Number n) = Just n
eval (Just p) That = Just p
eval Nothing That = Nothing
eval mem (Add expr1 expr2) = do
  x <- eval mem expr1
  y <- eval mem expr2
  pure (x + y)
eval mem (Subtract expr1 expr2) = do
  x <- eval mem expr1
  y <- eval mem expr2
  pure (x - y)
eval mem (Multiply expr1 expr2) = do
  x <- eval mem expr1
  y <- eval mem expr2
  pure (x * y)


respondTo :: Request -> App ()
respondTo Unknown = liftIO $ putStrLn "I don't understand that."
respondTo Hello = liftIO $ putStrLn "Hi there!"

-- Responding to time situations
respondTo (Time Today) = do
  today <- liftIO $ utctDay <$> getCurrentTime 
  liftIO $ putStrLn ("Today is " ++ show (dayOfWeek today) ++ ".")

respondTo (Time Tomorrow) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let tomorrow = addDays 1 today
  liftIO $ putStrLn ("Tomorrow is " ++ show (dayOfWeek tomorrow) ++ ".")

respondTo (Time (Since day)) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let elapsed = diffDays today day -- diffDays function finds difference between days in days
  liftIO $ putStrLn (show day ++ " was " ++ show elapsed ++ " days ago." )



-- gets the current state for previous evaluation, and uses eval function
-- if valid previous expression updated and answer output
respondTo (Formula val) = do
  memory <- get
  case eval (prevExpr memory) val of
    Nothing -> liftIO $ putStrLn "I haven't evaluated anything yet."
    Just x -> do
      modify (\mem -> mem { prevExpr = Just x} )
      liftIO $ putStrLn ("The answer is " ++ printLonghand x ++ ".")
 


-- takes previous memory state (Map) and inserts new knowledge via a Map
-- Also writes to the memoryFile the new key value pair so it can be saved
-- if the key is already present it is overwritten
respondTo (ToMemory key val) = do
  modify (\memory -> memory { remember = Map.insert key val (remember memory) })
  mem <- get
  liftIO $ writeFile "memoryFile.txt" (fileToString (remember mem))
  liftIO $ putStrLn "Okay."

-- Gets memory state, looks for the key in map and returns based on the result of the lookup
respondTo (Recall key) = do
  memory <- get
  case Map.lookup key (remember memory) of
    Nothing -> liftIO $ putStrLn ("Sorry, I don't know anything about " ++ key ++ ".")
    Just x -> liftIO $ putStrLn ("Sure - " ++ key ++ " is " ++ x ++ ".")


{-
  When responding to a joke, the joke must be retrieved based upon the parsed category

  This is retrieved by a HTTP request in fetchJokes and error checked in getJoke

  If a valid call and category the joke is displayed
-}

respondTo (Joke category) = do
  result <- liftIO $ getJoke category
  case result of
    Left errMsg -> liftIO $ putStrLn errMsg
    Right joke -> do
      liftIO $ putStrLn (setup joke)
      liftIO $ threadDelay 5000000 -- delays for 5 seconds
      liftIO $ putStrLn (punchline joke)



{-
  Responds to RPS challenge and move, generates a random move for AI

  delays the thread for dramatic effect before displaying the result
-}

respondTo (RPSmove move) = do
  aiMove <- liftIO randomMove
  let win = determineResult move aiMove
  liftIO $ threadDelay 1000000 
  liftIO $ putStrLn ("AI move: " ++ show aiMove ++ ".")
  case win of
    Win -> liftIO $ putStrLn "Good move, you win."
    Draw -> liftIO $ putStrLn "You drew."
    Loss -> liftIO $ putStrLn "Unlucky you lost."


fileToString :: Map String String -> String
fileToString mem =
  unlines [k ++ "," ++ v | (k, v) <- Map.toList mem]



-- Fetch jokes from API
-- Exception from HTTP or JSON decoding stored as left values (from either)
fetchJokes :: String -> IO (Either Exc.SomeException [JokeForm])
fetchJokes category =
  Exc.try $ do
    request <- HTTP.parseRequest ("https://official-joke-api.appspot.com/jokes/" ++ category ++ "/random")
    response <- HTTP.httpJSON request
    pure (HTTP.getResponseBody response :: [JokeForm])

-- Retrieves a single joke
-- Converts API exception into error messages which are displayed in respondTo
getJoke :: String -> IO (Either String JokeForm)
getJoke category = do
  result <- fetchJokes category
  case result of 
    Left _ -> pure (Left "couldn't retrieve a joke via API")
    Right jokeLine ->
      pure $
        case jokeLine of
          (j:_) -> Right j
          [] -> Left "no jokes in the category"




-- returns a random move for the AI
randomMove :: IO Move
randomMove = toEnum <$> randomRIO (0, 2)

-- pattern matches to determine the outcome based of moves and returns Result type
determineResult :: Move -> Move -> Result
determineResult Rock Scissors = Win
determineResult Paper Rock = Win
determineResult Scissors Paper = Win
determineResult x y
  | x == y = Draw
  | otherwise = Loss



