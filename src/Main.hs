{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import App.Database
import App.Router
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import Web.Scotty (scotty)

import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Misc as Skype
import qualified Web.Skype.Core as Skype
import qualified Web.Skype.Parser as Skype

main :: IO ()
main = do
  initDatabase

  connection <- Skype.connect "skype-rest-api-server"

  _ <- Skype.runSkype connection $ do
    Skype.protocol 9999

    fork $ Skype.onNotification $ \notification -> do
      case Skype.parseNotification notification of

      liftIO $ print $ Skype.parseNotification notification

  scotty 3000 $ router connection
