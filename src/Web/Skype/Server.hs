{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Web.Skype.Server (server) where

import Control.Monad.Trans (MonadIO, liftIO)
import Web.Scotty
import Web.Skype.Core
import Web.Skype.Database
import Web.Skype.Fetcher

import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Chat as Chat
import qualified Web.Skype.Command.User as User

server :: Int -> Skype.Connection -> IO ()
server port connection = scotty port $ do
  get  "/chat/"                       $ getAllChats connection
  get  "/chat/search"                 $ getSearchChatMessage
  get  "/user/"                       $ getAllUsers connection
  get  "/user/:user_id"               $ getUser connection
  get  (regex "^/chat/(#.*)")         $ getChat connection
  post (regex "^/chat/(#.*)")         $ postChat connection
  get  (regex "^/chat/message/(#.*)") $ getAllChatMessages connection

defaultPerPage :: Int
defaultPerPage = 30

getAllChats :: Skype.Connection -> ActionM ()
getAllChats connection = do
  response <- liftIO $ runSkype connection $
              Chat.searchAllChats >>= mapM pullChat
  either json json response
  where
    pullChat chatID = do
      maybeChat <- liftIO $ withDatabase $ findChat chatID
      case maybeChat of
        Just chat -> return chat
        Nothing -> do
          chat <- fetchChat chatID
          liftIO $ withDatabase $ storeChat chat
          return chat

getChat :: Skype.Connection -> ActionM ()
getChat connection = do
  chatID <- param "1"
  response <- liftIO $ runSkype connection $ fetchChat chatID
  either json json response

getAllChatMessages :: Skype.Connection -> ActionM ()
getAllChatMessages connection = do
  chatID <- param "1"
  perPage <- param "per_page" `orElse` defaultPerPage
  page <- param "page" `orElse` 0
  response <- liftIO $ runSkype connection $ do
    messageIDs <- Chat.getAllMessages chatID
    mapM pullChatMessage $ take perPage $ drop (page * perPage) messageIDs
  either json json response
  where
    orElse q def = rescue q $ \_ -> return def

    pullChatMessage chatMessageID = do
      maybeChatMessage <- liftIO $ withDatabase $ findChatMessge chatMessageID
      case maybeChatMessage of
        Just chatMessage -> return chatMessage
        Nothing -> do
          chatMessage <- fetchChatMessage chatMessageID
          liftIO $ withDatabase $ storeChatMessage chatMessage
          return chatMessage

getSearchChatMessage :: ActionM ()
getSearchChatMessage = do
  query <- param "q"
  response <- liftIO $ withDatabase $ searchChatMessage query
  json response

postChat :: Skype.Connection -> ActionM ()
postChat connection = do
  chatID <- param "1"
  message <- param "message"
  response <- liftIO $ runSkype connection $ Chat.sendMessage chatID message
  either json json response

getUser :: Skype.Connection -> ActionM ()
getUser connection = do
  userID <- param "user_id"
  response <- liftIO $ runSkype connection $ fetchUser userID
  either json json response

getAllUsers :: Skype.Connection -> ActionM ()
getAllUsers connection = do
  response <- liftIO $ runSkype connection $ User.searchAllFriends >>= mapM fetchUser
  either json json response
