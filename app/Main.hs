{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, void, when)
import Data.Text (isPrefixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Roller (applyRolls, getRoll, isValidRoll)
import UnliftIO.Concurrent

main :: IO ()
main = startClient

-- | Replies "pong" to every message that starts with "ping"
startClient :: IO ()
startClient = do
  token <- TIO.readFile "token.txt"
  userFacingError <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue
  TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (isPing m && not (fromBot m) && isValidRoll m) $ do
    case unpack (messageContent m) !! 5 of
      ' ' -> do
        threadDelay (2 * 10 ^ 6)
        roll1 m
      'd' -> do
        threadDelay (2 * 10 ^ 6)
        roll2 min m
      'a' -> do
        threadDelay (2 * 10 ^ 6)
        roll2 max m
      _ -> do sendMessage m $ pack "invalid roll"
  {- threadDelay (2 * 10^6)
  roll <- newAndRoll m
  void $ restCall (R.CreateMessage (messageChannelId m)  $ makePretty roll)
  if isAdvantageOrDis m
    then do
      roll <- newAndRoll m
      void $ restCall (R.CreateMessage (messageChannelId m)  $ makePretty roll)
    else do return () -}
  _ -> return ()

roll1 m = case getRoll m of
  Just r -> do
    rolled <- applyRolls r
    sendMessage m . pack $ "You rolled: " ++ show rolled
  Nothing -> do sendMessage m "Invalid Roll"

roll2 combiner m = case getRoll m of
  Just r -> do
    rolled1 <- applyRolls r
    rolled2 <- applyRolls r
    sendMessage m . pack $ "You rolled: " ++ show rolled1 ++ " and " ++ show rolled2
    sendMessage m . pack $ "Final Result: " ++ show (combiner rolled1 rolled2)
  Nothing -> do sendMessage m "Invalid Roll"

sendMessage input responce = void $ restCall (R.CreateMessage (messageChannelId input) responce)

fromBot = userIsBot . messageAuthor

isPing = ("~roll" `isPrefixOf`) . toLower . messageContent
