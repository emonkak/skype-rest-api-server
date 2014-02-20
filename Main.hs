{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON(..), Value(..), (.=), object)
import Web.Scotty
import Web.Skype.Command.Misc
import Web.Skype.Core
import Web.Skype.Parser (parseNotification)
import Web.Skype.Protocol

import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Chat as Chat

instance ToJSON SkypeError where
  toJSON (SkypeError code command description) = object
    [ "error_code" .= code
    , "error_command" .= command
    , "error_description" .= description
    ]

instance ToJSON ChatStatus where
  toJSON ChatStatusLegacyDialog = String "legacy_dialog"
  toJSON ChatStatusDialog = String "dialog"
  toJSON ChatStatusMultiSubscribed = String "multi_subscribed"
  toJSON ChatStatusUnsubscribed = String "unsubscribed"

main :: IO ()
main = do
  connection <- Skype.connect "skype-server"

  _ <- runSkype connection $ protocol 9999

  _ <- runSkype connection $ do
    notificationChan <- dupNotificationChan

    fork $ forever $ do
      notification <- liftIO $ atomically $ readTChan notificationChan

      liftIO $ maybe (print notification) print $ parseNotification notification

  scotty 3000 $ do
    get  "/chat/" $ allChats connection
    get  (regex "^/chat/(#.*)") $ getChat connection
    post (regex "^/chat/(#.*)") $ postChat connection

allChats :: Skype.SkypeConnection -> ActionM ()
allChats connection = do
  response <- liftIO $ runSkype connection $ mapM helper =<< Chat.searchAllChats
  either json json response
  where
    helper chatID = object <$> sequence
      [ pure ("chat_id" .= chatID)
      , (.=) "timestamp"     <$> Chat.getTimestamp chatID
      , (.=) "adder"         <$> Chat.getAdder chatID
      , (.=) "status"        <$> Chat.getStatus chatID
      , (.=) "topic"         <$> Chat.getTopic chatID
      , (.=) "window_title"  <$> Chat.getWindowTitle chatID
      , (.=) "is_bookmarked" <$> Chat.isBookmarked chatID
      ]

getChat :: Skype.SkypeConnection -> ActionM ()
getChat connection = do
  chatID <- param "1"
  response <- liftIO $ runSkype connection $
    object <$> sequence [ (.=) "timestamp"       <$> Chat.getTimestamp chatID
                        , (.=) "adder"           <$> Chat.getAdder chatID
                        , (.=) "status"          <$> Chat.getStatus chatID
                        , (.=) "all_posters"     <$> Chat.getAllPosters chatID
                        , (.=) "all_members"     <$> Chat.getAllMembers chatID
                        , (.=) "topic"           <$> Chat.getTopic chatID
                        , (.=) "active_members"  <$> Chat.getActiveMembers chatID
                        , (.=) "window_title"    <$> Chat.getWindowTitle chatID
                        , (.=) "all_messages"    <$> Chat.getAllMessages chatID
                        , (.=) "recent_messages" <$> Chat.getRecentMessages chatID
                        , (.=) "is_bookmarked"   <$> Chat.isBookmarked chatID
                        ]
  either json json response

postChat :: Skype.SkypeConnection -> ActionM ()
postChat connection = do
  chatID <- param "1"
  message <- param "message"
  response <- liftIO $ runSkype connection $ Chat.sendMessage chatID message
  either json json response
