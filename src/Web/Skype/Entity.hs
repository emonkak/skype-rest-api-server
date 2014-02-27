{-# LANGUAGE DeriveDataTypeable,
             FlexibleInstances,
             OverlappingInstances,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Skype.Entity where

import Control.Applicative
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow(..), SQLData(..), field)
import Database.SQLite.Simple.FromField (FromField(..), ResultError(..), returnError, fieldData)
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.Text as T

class ToString a where
  toString :: a -> String

instance (ToString a) => ToJSON a where
  toJSON = toJSON . toString

instance (ToString a) => ToField a where
  toField = toField . toString

-- * Chat

data ChatEntity = ChatEntity
  { _chatID :: ChatID
  , _chatTimestamp :: Timestamp
  , _chatAdder :: Maybe UserID
  , _chatStatus :: ChatStatus
  , _chatTopic :: ChatTopic
  , _chatWindowTitle :: ChatWindowTitle
  , _chatIsBookmarked :: Bool
  }
  deriving (Show, Typeable)

instance Eq ChatEntity where
  (==) x y = _chatID x == _chatID y

instance ToJSON ChatEntity where
  toJSON chat = object
    [ "chat_id"       .= _chatID chat
    , "timestamp"     .= _chatTimestamp chat
    , "adder"         .= _chatAdder chat
    , "status"        .= _chatStatus chat
    , "topic"         .= _chatTopic chat
    , "window_title"  .= _chatWindowTitle chat
    , "is_bookmarked" .= _chatIsBookmarked chat
    ]

instance FromRow ChatEntity where
  fromRow = ChatEntity <$> field  -- chat_id
                       <*> field  -- timestamp
                       <*> field  -- adder
                       <*> field  -- status
                       <*> field  -- topic
                       <*> field  -- window_title
                       <*> field  -- is_bookmarked

instance ToString ChatStatus where
  toString ChatStatusLegacyDialog    = "legacy_dialog"
  toString ChatStatusDialog          = "dialog"
  toString ChatStatusMultiSubscribed = "multi_subscribed"
  toString ChatStatusUnsubscribed    = "unsubscribed"

instance FromField ChatStatus where
  fromField f = case fieldData f of
    SQLText "legacy_dialog"    -> Ok ChatStatusLegacyDialog
    SQLText "dialog"           -> Ok ChatStatusDialog
    SQLText "multi_subscribed" -> Ok ChatStatusMultiSubscribed
    SQLText "unsubscribed"     -> Ok ChatStatusUnsubscribed
    SQLText t                  -> returnError ConversionFailed f ("unexpected text: " <> T.unpack t)
    _                          -> returnError ConversionFailed f "need a text"

-- * Chat message

data ChatMessageEntity = ChatMessageEntity
  { _chatMessageID :: ChatMessageID
  , _chatMessageTimestamp :: Timestamp
  , _chatMessageSender :: UserID
  , _chatMessageSenderDisplayName :: UserDisplayName
  , _chatMessageType :: ChatMessageType
  , _chatMessageStatus :: ChatMessageStatus
  , _chatMessageLeaveReason :: Maybe ChatMessageLeaveReason
  , _chatMessageChat :: ChatID
  , _chatMessageIsEditable :: Bool
  , _chatMessageBody :: ChatMessageBody
  }
  deriving (Show, Typeable)

instance Eq ChatMessageEntity where
  (==) x y = _chatMessageID x == _chatMessageID y

instance ToJSON ChatMessageEntity where
  toJSON chatMessage = object
    [ "chat_message_id"     .= _chatMessageID chatMessage
    , "timestamp"           .= _chatMessageTimestamp chatMessage
    , "sender"              .= _chatMessageSender chatMessage
    , "sender_display_name" .= _chatMessageSenderDisplayName chatMessage
    , "type"                .= _chatMessageType chatMessage
    , "status"              .= _chatMessageStatus chatMessage
    , "leave_reason"        .= _chatMessageLeaveReason chatMessage
    , "chat"                .= _chatMessageChat chatMessage
    , "is_editable"         .= _chatMessageIsEditable chatMessage
    , "body"                .= _chatMessageBody chatMessage
    ]

instance FromRow ChatMessageEntity where
  fromRow = ChatMessageEntity <$> field  -- chat_message_id
                              <*> field  -- timestamp
                              <*> field  -- sender
                              <*> field  -- sender_display_name
                              <*> field  -- type
                              <*> field  -- status
                              <*> field  -- leave_reason
                              <*> field  -- chat
                              <*> field  -- is_editable
                              <*> field  -- body

instance ToString ChatMessageType where
  toString ChatMessageTypeSetTopic          = "set_topic"
  toString ChatMessageTypeSaid              = "said"
  toString ChatMessageTypeAddedMembers      = "added_members"
  toString ChatMessageTypeSawMembers        = "saw_members"
  toString ChatMessageTypeCreatedChatWith   = "created_chat_with"
  toString ChatMessageTypeLeft              = "left"
  toString ChatMessageTypePostedContacts    = "posted_contacts"
  toString ChatMessageTypeGapInChat         = "gap_in_chat"
  toString ChatMessageTypeSetRole           = "set_role"
  toString ChatMessageTypeKicked            = "kicked"
  toString ChatMessageTypeKickBanned        = "kick_banned"
  toString ChatMessageTypeSetOptions        = "set_options"
  toString ChatMessageTypeSetPicture        = "set_picture"
  toString ChatMessageTypeSetGuideLines     = "set_guide_lines"
  toString ChatMessageTypeJoinedAsApplicant = "joined_as_applicant"
  toString ChatMessageTypeEmoted            = "emoted"
  toString ChatMessageTypeUnkown            = "unknown"

instance FromField ChatMessageType where
  fromField f = case fieldData f of
    SQLText "set_topic"           -> Ok ChatMessageTypeSetTopic
    SQLText "said"                -> Ok ChatMessageTypeSaid
    SQLText "added_members"       -> Ok ChatMessageTypeAddedMembers
    SQLText "saw_members"         -> Ok ChatMessageTypeSawMembers
    SQLText "created_chat_with"   -> Ok ChatMessageTypeCreatedChatWith
    SQLText "left"                -> Ok ChatMessageTypeLeft
    SQLText "posted_contacts"     -> Ok ChatMessageTypePostedContacts
    SQLText "gap_in_chat"         -> Ok ChatMessageTypeGapInChat
    SQLText "set_role"            -> Ok ChatMessageTypeSetRole
    SQLText "kicked"              -> Ok ChatMessageTypeKicked
    SQLText "kick_banned"         -> Ok ChatMessageTypeKickBanned
    SQLText "set_options"         -> Ok ChatMessageTypeSetOptions
    SQLText "set_picture"         -> Ok ChatMessageTypeSetPicture
    SQLText "set_guide_lines"     -> Ok ChatMessageTypeSetGuideLines
    SQLText "joined_as_applicant" -> Ok ChatMessageTypeJoinedAsApplicant
    SQLText "emoted"              -> Ok ChatMessageTypeEmoted
    SQLText "unknown"             -> Ok ChatMessageTypeUnkown
    SQLText t                     -> returnError ConversionFailed f ("unexpected text: " <> T.unpack t)
    _                             -> returnError ConversionFailed f "need a text"

instance ToString ChatMessageStatus where
  toString ChatMessageStatusSending = "sending"
  toString ChatMessageStatusSent    = "sent"
  toString ChatMessageStatusReceive = "receive"
  toString ChatMessageStatusRead    = "read"

instance FromField ChatMessageStatus where
  fromField f = case fieldData f of
    SQLText "sending" -> Ok ChatMessageStatusSending
    SQLText "sent"    -> Ok ChatMessageStatusSent
    SQLText "receive" -> Ok ChatMessageStatusReceive
    SQLText "read"    -> Ok ChatMessageStatusRead
    SQLText _         -> returnError ConversionFailed f "unexpected text"
    _                 -> returnError ConversionFailed f "need a text"

instance ToString ChatMessageLeaveReason where
  toString ChatMessageLeaveReasonUserNotFound          = "user_not_found"
  toString ChatMessageLeaveReasonUserIncapable         = "user_incapable"
  toString ChatMessageLeaveReasonAdderMustBeFriend     = "adder_must_be_friend"
  toString ChatMessageLeaveReasonAdderMustBeAuthorized = "adder_must_be_authorized"
  toString ChatMessageLeaveReasonUnsubscribe           = "unsubscribe"

instance FromField ChatMessageLeaveReason where
  fromField f = case fieldData f of
    SQLText "user_not_found"           -> Ok ChatMessageLeaveReasonUserNotFound
    SQLText "user_incapable"           -> Ok ChatMessageLeaveReasonUserIncapable
    SQLText "adder_must_be_friend"     -> Ok ChatMessageLeaveReasonAdderMustBeFriend
    SQLText "adder_must_be_authorized" -> Ok ChatMessageLeaveReasonAdderMustBeAuthorized
    SQLText "unsubscribe"              -> Ok ChatMessageLeaveReasonUnsubscribe
    SQLText t                          -> returnError ConversionFailed f ("unexpected text: " <> T.unpack t)
    _                                  -> returnError ConversionFailed f "need a text"

-- * Error

instance ToJSON SkypeError where
  toJSON (SkypeError code command description) = object
    [ "error_code"        .= code
    , "error_command"     .= command
    , "error_description" .= description
    ]
