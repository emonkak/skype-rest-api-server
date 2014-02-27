{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import Web.Skype.Server
import Web.Skype.Database

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

    notificationChan <- Skype.dupNotificationChan

    fork $ forever $ do
      notification <- liftIO $ atomically $ readTChan notificationChan

      liftIO $ print $ Skype.parseNotification notification

  server 3000 connection
