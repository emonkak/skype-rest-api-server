{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import App.Database
import App.Fetcher
import App.Router
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans (liftIO)
import Web.Scotty (scotty)
import Network.Skype.Protocol

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Network.Skype.API as Skype
import qualified Network.Skype.Command.Misc as Skype
import qualified Network.Skype.Core as Skype
import qualified Network.Skype.Parser as Skype

main :: IO ()
main = do
  initDatabase

  connection <- Skype.connect "skype-rest-api-server"

  _ <- Skype.runSkype connection $ do
    Skype.protocol 9999

    fork $ Skype.onNotification $ \notification -> do
      liftIO $ BSC.putStrLn notification

      case Skype.parseNotification notification of
        Right (ChatMessage chatMessageID (ChatMessageStatus _)) -> do
          -- Received a message.
          fetchChatMessage chatMessageID >>= liftIO . withDatabase . storeChatMessage
        _ -> return ()

  scotty 3000 $ router connection
