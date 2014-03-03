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
import qualified Web.Skype.Command.User as User

server :: Int -> Skype.Connection -> IO ()
server port connection = scotty port $ do
  get  "/chat/"                       $ getAllChats connection
  get  "/chat/search"                 $ getSearchChatMessage
  get  "/user/"                       $ getAllUser connection
  get  "/user/:user_id"              $ getUser connection
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

getUser :: Skype.Connection -> ActionM ()
getUser connection = do
  userID <- param "user_id"
  response <- liftIO $ runSkype connection $ fetchUser userID
  either json json response

getAllUser :: Skype.Connection -> ActionM ()
getAllUser connection = do
  response <- liftIO $ runSkype connection $ User.searchAllFriends >>= mapM fetchUser
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
                             <*> ChatMessage.getBody chatMessageID

fetchUser :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
          => UserID
          -> SkypeT m UserEntity
fetchUser userID = UserEntity userID
               <$> User.getFullName userID
               <*> User.getBirthday userID
               <*> User.getSex userID
               <*> User.getLanguage userID
               <*> User.getCountry userID
               <*> User.getProvince userID
               <*> User.getCity userID
               <*> User.getHomePhone userID
               <*> User.getOfficePhone userID
               <*> User.getMobilePhone userID
               <*> User.getHomepage userID
               <*> User.getAbout userID
               <*> User.isVideoCapable userID
               <*> User.isVoicemailCapable userID
               <*> User.getBuddyStatus userID
               <*> User.isAuthorized userID
               <*> User.isBlocked userID
               <*> User.getOnlineStatus userID
               <*> User.getLastOnlineTimestamp userID
               <*> User.canLeaveVoicemail userID
               <*> User.getSpeedDial userID
               <*> User.getMoodText userID
               <*> User.getTimezone userID
               <*> User.isCallForwardingActive userID
               <*> User.getNumberOfAuthedBuddies userID
               <*> User.getDisplayName userID
