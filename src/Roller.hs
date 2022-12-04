{-# LANGUAGE OverloadedStrings #-}

module Roller where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import System.Random.Stateful (UniformRange (uniformRM), globalStdGen)
import Text.Megaparsec (Parsec, choice, optional, parse, try)
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

data Dice = D4 | D6 | D8 | D10 | D12 | D20 | D100 deriving (Show)

diceToInt :: Num p => Dice -> p
diceToInt D4 = 4
diceToInt D6 = 6
diceToInt D8 = 8
diceToInt D10 = 10
diceToInt D12 = 12
diceToInt D20 = 20
diceToInt D100 = 100

data Roll = Roll Int Int Dice deriving (Show)

runRoll :: Roll -> IO Int
runRoll (Roll c n d) = comb <$> rolls
  where
    comb l = c + sum l
    rolls = replicateM n $ uniformRM (1, diceToInt d) globalStdGen

type Parser = Parsec Void Text

prefix :: Parser Text
prefix = string "~roll "

addition :: Parser (Maybe Int)
addition = optional . try $ L.decimal <* char '+'

dice :: Parser Dice
dice =
  choice
    [ D4 <$ string "d4",
      D6 <$ string "d6",
      D8 <$ string "d8",
      D10 <$ string "d10",
      D12 <$ string "d12",
      D20 <$ string "d20",
      D100 <$ string "d100"
    ]

roll :: Parser Roll
roll = prefix *> getRoll
  where
    getRoll = Roll <$> add <*> L.decimal <*> dice
    add = fromMaybe 0 <$> addition

parseRoll = parse roll ""