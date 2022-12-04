module Roller where

import Data.Text (isPrefixOf, pack, split, unpack, append)
import qualified Data.Text (words)
import Discord.Types
import Text.Read (readMaybe)
import qualified System.Random
import Control.Monad (replicateM)

newtype Dice = Dice Int deriving (Read)

instance Show Dice where
  show (Dice i) = 'D' : show i

standardDiceList = [4, 6, 8, 10, 12, 20, 100]

data Roll = Roll Int Dice Int deriving (Show, Read)

getRoll :: Message -> Maybe Roll
getRoll m = standardizeRoll =<< (readMaybe . reCombine $ splitMessage m)

standardizeRoll r = if getHigh r `elem` standardDiceList then Just r else Nothing

getHigh (Roll _ (Dice i) _) = i

delims = ['d', 'D', '+']

isValidRoll m = case Data.Text.words $ messageContent m of
  [c,_] -> pack "~roll" `isPrefixOf` c
  _ -> False

splitMessage m = case Data.Text.words $ messageContent m of
  [_,par] -> split (`elem` delims) par
  _ -> []


reCombine l = case map unpack l of
  [a,d,f] -> concat ["Roll ",a," (Dice ",d,") ",f]
  [a,d]   -> concat ["Roll ",a," (Dice ",d,") 0"]
  _       -> ""

applyRolls (Roll n (Dice d) a) = fmap addDice . replicateM n $ System.Random.randomRIO (1, d)
  where addDice l = a + sum l