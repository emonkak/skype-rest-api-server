{-# LANGUAGE FlexibleContexts #-}

module App.Fetcher where

import App.Entity
import Control.Applicative
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Skype.Core
import Network.Skype.Protocol

import qualified Network.Skype.Command.Chat as Chat
import qualified Network.Skype.Command.ChatMessage as ChatMessage
import qualified Network.Skype.Command.User as User

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
