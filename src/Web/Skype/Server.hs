{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Web.Skype.Server (server) where

import Control.Applicative
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Web.Scotty
import Web.Skype.Core
import Web.Skype.Database
import Web.Skype.Entity
import Web.Skype.Protocol
import Web.Skype.Tokenizer

import qualified Data.Text as T
import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Chat as Chat
import qualified Web.Skype.Command.ChatMessage as ChatMessage

server :: Int -> Skype.Connection -> IO ()
server port connection = scotty port $ do
  get  "/chat/"                       $ getAllChats connection
  get  "/chat/search"                 $ getSearchChatMessage
  get  (regex "^/chat/(#.*)")         $ getChat connection
  post (regex "^/chat/(#.*)")         $ postChat connection
  get  (regex "^/chat/message/(#.*)") $ getAllChatMessages connection

getAllChats :: Skype.Connection -> ActionM ()
getAllChats connection = do
  response <- liftIO $ runSkype connection $
              Chat.searchAllChats >>= mapM fetchChat
  either json json response

getChat :: Skype.Connection -> ActionM ()
getChat connection = do
  chatID <- param "1"
  response <- liftIO $ runSkype connection $ fetchChat chatID
  either json json response

getAllChatMessages :: Skype.Connection -> ActionM ()
getAllChatMessages connection = do
  chatMessageID <- param "1"
  response <- liftIO $ runSkype connection $
    Chat.getAllMessages chatMessageID >>= mapM getChatMessage
  either json json response
  where
    getChatMessage chatMessageID = do
      maybeChatMessage <- liftIO $ withDatabase $ findChatMessge chatMessageID
      case maybeChatMessage of
        Just chatMessage -> return chatMessage
        Nothing          -> do
          chatMessage <- fetchChatMessage chatMessageID
          liftIO $ withDatabase $ storeChatMessage chatMessage
          return chatMessage

getSearchChatMessage :: ActionM ()
getSearchChatMessage = do
  query <- param "q"
  response <- liftIO $ withDatabase $ searchChatMessage $
              T.intercalate " " $ unigram query
  json response

postChat :: Skype.Connection -> ActionM ()
postChat connection = do
  chatID <- param "1"
  message <- param "message"
  response <- liftIO $ runSkype connection $ Chat.sendMessage chatID message
  either json json response

fetchChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
          => ChatID
          -> SkypeT m ChatEntity
fetchChat chatID = ChatEntity chatID
               <$> Chat.getTimestamp chatID
               <*> Chat.getAdder chatID
               <*> Chat.getStatus chatID
               <*> Chat.getTopic chatID
               <*> Chat.getWindowTitle chatID
               <*> Chat.isBookmarked chatID

fetchChatMessage :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                 => ChatMessageID
                 -> SkypeT m ChatMessageEntity
fetchChatMessage chatMessageID = ChatMessageEntity chatMessageID
                             <$> ChatMessage.getTimestamp chatMessageID
                             <*> ChatMessage.getSender chatMessageID
                             <*> ChatMessage.getSenderDisplayName chatMessageID
                             <*> ChatMessage.getType chatMessageID
                             <*> ChatMessage.getStatus chatMessageID
                             <*> ChatMessage.getLeaveReason chatMessageID
                             <*> ChatMessage.getChat chatMessageID
                             <*> ChatMessage.isEditable chatMessageID
                             <*> ChatMessage.getBody chatMessageID
