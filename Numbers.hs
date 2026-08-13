module LGPT.Numbers where

import LGPT.Helpers
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
{- | Print an integer in longhand form. 
 
    You don't need to touch this, but feel free to read it and try to work out what it's doing :)

    You can open `stack repl` and try out `printLonghand` on some numbers to see how it works. Examples are also given in test/Spec.hs.
-}
printLonghand :: Int -> String
printLonghand n
  | n >  1000000  = "an unfathomably large number"
  | n == 1000000  = "one million"
  | n == 0        = "zero"
  | n >= 1000     = let
      thouStr   = printLonghand (n `div` 1000) ++ " thousand" 
      and       = if n `mod` 1000 >= 100 then " " else " and "
      hundStr   = printLonghand (n `mod` 1000)
      in if n `mod` 1000 == 0
         then thouStr
         else thouStr ++ and ++ hundStr
  | n >= 100 = let
      hundStr = printLonghand (n `div` 100) ++ " hundred"
      tensStr = printLonghand (n `mod` 100)
      in if n `mod` 100 == 0
         then hundStr
         else hundStr ++ " and " ++ tensStr
  | n >= 20 = let 
      tensStr = tens !! ((n `div` 10) - 2) 
      hyphen  = if n `mod` 10 /= 0 then "-" else ""
      unitStr = ("" : oneToNine) !! (n `mod` 10)
      in tensStr ++ hyphen ++ unitStr
  | n > 0 = ("" : oneToNine ++ tenToTwenty) !! n
  | otherwise    = "a negative number"

--------------------------------------------------------------------------------
{- | Parse an integer in longhand form. 
 
    You don't need to touch this, but feel free to read it and try to work out what it's doing :)

    This parser is a lot more complicated than the ones you need to write.
-}
parseLonghand :: Parser Int
parseLonghand = choice 
  [ string "one million" >> pure 1000000
  , try parseSubMillion
  , try parseSubThousand
  , try parseSubHundred
  , try parseUnit
  , string "zero" >> pure 0
  ]
  where
    parseSubMillion :: Parser Int
    parseSubMillion = do
      th <- choice [try parseSubThousand, parseSubHundred]
      string " thousand"
      rest <- choice
        [ string " and " >> parseSubHundred
        , string " " >> parseSubThousand
        , pure 0
        ]
      pure $ th * 1000 + rest

    parseSubThousand :: Parser Int
    parseSubThousand = do
      h <- parseUnit
      string " hundred"
      rest <- (string " and " >> parseSubHundred) <|> pure 0
      pure $ h * 100 + rest

    parseSubHundred :: Parser Int
    parseSubHundred = choice
      [ do
        tens <- choice $ zipWith (\v s -> string s >> pure v) [20,30..90] tens
        rest <- (string "-" >> parseUnit) <|> pure 0
        pure $ tens + rest
      , parseTenToTwenty
      , parseUnit
      ]

    parseTenToTwenty :: Parser Int
    parseTenToTwenty = choice $ zipWith (\v s -> string s >> pure v) [10..19] tenToTwenty

    parseUnit :: Parser Int
    parseUnit = choice $ zipWith (\v s -> string s >> pure v) 
      [1..9] 
      oneToNine


--------------------------------------------------------------------------------
-- Helpers used for both parsing and printing

oneToNine :: [String]
oneToNine = 
  [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tenToTwenty :: [String]
tenToTwenty = 
  [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
  , "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = 
  [ "twenty", "thirty", "forty", "fifty"
  , "sixty", "seventy", "eighty", "ninety"]
