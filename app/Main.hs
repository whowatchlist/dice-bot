{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, void, when)
import Data.Text (isPrefixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Roller (parseRoll, runRoll)
import Text.Megaparsec (errorBundlePretty)
import UnliftIO (liftIO)

main :: IO ()
main = startClient

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
  MessageCreate m -> when (isPing m && not (fromBot m)) $ do
    case parseRoll . messageContent $ m of
      Left peb -> sendMessage m . pack . errorBundlePretty $ peb
      Right ro -> do
        res <- liftIO . runRoll $ ro
        sendMessage m . pack $ "You rolled: " ++ show res
  _ -> return ()

sendMessage input responce = void $ restCall (R.CreateMessage (messageChannelId input) responce)

fromBot = userIsBot . messageAuthor

isPing = ("~roll" `isPrefixOf`) . toLower . messageContent
