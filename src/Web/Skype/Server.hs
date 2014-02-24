{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Skype.Server (server) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON(..), Value(..), (.=), object)
import Web.Scotty
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Web.Skype.API as Skype
import qualified Web.Skype.Command.Chat as Chat
import qualified Web.Skype.Command.ChatMessage as ChatMessage

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

instance ToJSON ChatMessageType where
  toJSON ChatMessageTypeSetTopic          = String "set_topic"
  toJSON ChatMessageTypeSaid              = String "said"
  toJSON ChatMessageTypeAddMembers        = String "add_members"
  toJSON ChatMessageTypeSawMembers        = String "saw_members"
  toJSON ChatMessageTypeCreatedChatWith   = String "created_chat_with"
  toJSON ChatMessageTypeLeft              = String "left"
  toJSON ChatMessageTypePostedContacts    = String "posted_contacts"
  toJSON ChatMessageTypeGapInChat         = String "gap_in_chat"
  toJSON ChatMessageTypeSetRole           = String "set_role"
  toJSON ChatMessageTypeKicked            = String "kicked"
  toJSON ChatMessageTypeKickBanned        = String "kick_banned"
  toJSON ChatMessageTypeSetOptions        = String "set_options"
  toJSON ChatMessageTypeSetPicture        = String "set_picture"
  toJSON ChatMessageTypeSetGuideLines     = String "set_guide_lines"
  toJSON ChatMessageTypeJoinedAsApplicant = String "joined_as_applicant"
  toJSON ChatMessageTypeUnkown            = String "unknown"

instance ToJSON ChatMessageStatus where
  toJSON ChatMessageStatusSending = String "sending"
  toJSON ChatMessageStatusSent    = String "sent"
  toJSON ChatMessageStatusReceive = String "receive"
  toJSON ChatMessageStatusRead    = String "read"

instance ToJSON ChatMessageLeaveReason where
  toJSON ChatMessageLeaveReasonUserNotFound          = String "user_not_found"
  toJSON ChatMessageLeaveReasonUserIncapable         = String "user_incapable"
  toJSON ChatMessageLeaveReasonAdderMustBeFriend     = String "adder_must_be_friend"
  toJSON ChatMessageLeaveReasonAdderMustBeAuthorized = String "adder_must_be_authorized"
  toJSON ChatMessageLeaveReasonUnsubscribe           = String "unsubscribe"

server :: Int -> Skype.Connection -> IO ()
server port connection = scotty port $ do
  get  "/chat/"                 $ getAllChats connection
  get  (regex "^/chat/(#.*)")   $ param "1"  >>= getChat connection
  post (regex "^/chat/(#.*)")   $ param "1"  >>= postChat connection
  get  "/chat_message/:id"      $ param "id" >>= getChatMessage connection

getAllChats :: Skype.Connection -> ActionM ()
getAllChats connection = do
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

getChat :: Skype.Connection -> ChatID -> ActionM ()
getChat connection chatID = do
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

getChatMessage :: Skype.Connection -> ChatMessageID -> ActionM ()
getChatMessage connection chatMessageID = do
  response <- liftIO $ runSkype connection $
    object <$> sequence [ (.=) "timestamp"           <$> ChatMessage.getTimestamp chatMessageID
                        , (.=) "sender"              <$> ChatMessage.getSender chatMessageID
                        , (.=) "sender_display_name" <$> ChatMessage.getSenderDisplayName chatMessageID
                        , (.=) "type"                <$> ChatMessage.getType chatMessageID
                        , (.=) "status"              <$> ChatMessage.getStatus chatMessageID
                        , (.=) "leave_reason"        <$> ChatMessage.getLeaveReason chatMessageID
                        , (.=) "chat"                <$> ChatMessage.getChat chatMessageID
                        , (.=) "all_users"           <$> ChatMessage.getAllUsers chatMessageID
                        , (.=) "is_editable"         <$> ChatMessage.isEditable chatMessageID
                        , (.=) "body"                <$> ChatMessage.getBody chatMessageID
                        ]
  either json json response

postChat :: Skype.Connection -> ChatID -> ActionM ()
postChat connection chatID = do
  message <- param "message"
  response <- liftIO $ runSkype connection $ Chat.sendMessage chatID message
  either json json response
