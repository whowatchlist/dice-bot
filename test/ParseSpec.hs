{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Roller (Dice (..), Roll (Roll), roll)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)

parseSpec :: Spec
parseSpec = do
  describe "Roller.roll" $ do
    it "should parse a simple dice roll" $
      parse roll "" "~roll 1d6" `shouldParse` Roll 0 1 D6
    it "should parse a constant addition" $
      parse roll "" "~roll 10+1d10" `shouldParse` Roll 10 1 D10
    it "should fail with a wrong dice" $
      parse roll "" `shouldFailOn` "~roll 2d15"