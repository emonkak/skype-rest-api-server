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

import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Chat as Chat
import qualified Web.Skype.Command.ChatMessage as ChatMessage
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
