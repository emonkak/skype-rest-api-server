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
import qualified Data.Text.Encoding as T

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
  }
  deriving (Show, Typeable)

instance Eq ChatEntity where
  (==) x y = _chatID x == _chatID y

instance ToJSON ChatEntity where
  toJSON chat = object
    [ "chat_id"       .= T.decodeLatin1 (_chatID chat)
    , "timestamp"     .= _chatTimestamp chat
    , "adder"         .= (T.decodeLatin1 <$> _chatAdder chat)
    , "status"        .= _chatStatus chat
    , "topic"         .= _chatTopic chat
    , "window_title"  .= _chatWindowTitle chat
    ]

instance FromRow ChatEntity where
  fromRow = ChatEntity <$> field  -- chat_id
                       <*> field  -- timestamp
                       <*> field  -- adder
                       <*> field  -- status
                       <*> field  -- topic
                       <*> field  -- window_title

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
  , _chatMessageBody :: ChatMessageBody
  }
  deriving (Show, Typeable)

instance Eq ChatMessageEntity where
  (==) x y = _chatMessageID x == _chatMessageID y

instance ToJSON ChatMessageEntity where
  toJSON chatMessage = object
    [ "chat_message_id"     .= _chatMessageID chatMessage
    , "timestamp"           .= _chatMessageTimestamp chatMessage
    , "sender"              .= T.decodeLatin1 (_chatMessageSender chatMessage)
    , "sender_display_name" .= _chatMessageSenderDisplayName chatMessage
    , "type"                .= _chatMessageType chatMessage
    , "status"              .= _chatMessageStatus chatMessage
    , "leave_reason"        .= _chatMessageLeaveReason chatMessage
    , "chat"                .= T.decodeLatin1 (_chatMessageChat chatMessage)
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
    , "error_command"     .= T.decodeLatin1 command
    , "error_description" .= description
    ]

-- * User

data UserEntity = UserEntity
  { _userID :: UserID
  , _userFullName :: UserFullName
  , _userBirthday :: Maybe UserBirthday
  , _userSex :: UserSex
  , _userLanguage :: Maybe (UserLanguageISOCode, UserLanguage)
  , _userCountry :: Maybe (UserCountryISOCode, UserCountry)
  , _userProvince :: UserProvince
  , _userCity :: UserCity
  , _userHomePhone :: UserPhone
  , _userOfficePhone :: UserPhone
  , _userMobilePhone :: UserPhone
  , _userHomepage :: UserHomepage
  , _userAbout :: UserAbout
  , _userIsVideoCapable :: Bool
  , _userIsVoicemailCapable :: Bool
  , _userBuddyStatus :: UserBuddyStatus
  , _userIsAuthorized :: Bool
  , _userIsBlocked :: Bool
  , _userOnlineStatus :: UserOnlineStatus
  , _userLastOnlineTimestamp :: Timestamp
  , _userCanLeaveVoicemail :: Bool
  , _userSpeedDial :: UserSpeedDial
  , _userMoodText :: UserMoodText
  , _userTimezone :: UserTimezoneOffset
  , _userIsCallForwardingActive :: Bool
  , _userNumberOfAuthedBuddies :: Int
  , _userDisplayName :: UserDisplayName
  }
  deriving (Show, Typeable)

instance Eq UserEntity where
  (==) x y = _userID x == _userID y

instance ToJSON UserEntity where
  toJSON user = object
    [ "user_id"                   .= T.decodeLatin1 (_userID user)
    , "full_name"                 .= _userFullName user
    , "birthday"                  .= (show <$> _userBirthday user)
    , "sex"                       .= _userSex user
    , "language"                  .= (snd <$> _userLanguage user)
    , "language_iso_code"         .= (fst <$> _userLanguage user)
    , "country"                   .= (snd <$> _userCountry user)
    , "country_iso_code"          .= (fst <$> _userCountry user)
    , "province"                  .= _userProvince user
    , "city"                      .= _userCity user
    , "home_phone"                .= _userHomePhone user
    , "office_phone"              .= _userOfficePhone user
    , "mobile_phone"              .= _userMobilePhone user
    , "homepage"                  .= _userHomepage user
    , "about"                     .= _userAbout user
    , "is_video_capable"          .= _userIsVideoCapable user
    , "is_voice_mail_capable"     .= _userIsVoicemailCapable user
    , "buddy_status"              .= _userBuddyStatus user
    , "is_authorized"             .= _userIsAuthorized user
    , "is_blocked"                .= _userIsBlocked user
    , "online_status"             .= _userOnlineStatus user
    , "last_online_timestamp"     .= _userLastOnlineTimestamp user
    , "can_leave_voicemail"       .= _userCanLeaveVoicemail user
    , "speed_dial"                .= _userSpeedDial user
    , "mood_text"                 .= _userMoodText user
    , "timezone"                  .= _userTimezone user
    , "is_call_forwarding_active" .= _userIsCallForwardingActive user
    , "number_of_authed_buddies"  .= _userNumberOfAuthedBuddies user
    , "display_name"              .= _userDisplayName user
    ]

instance ToString UserSex where
  toString UserSexUnknown = "unknown"
  toString UserSexMale    = "male"
  toString UserSexFemale  = "female"

instance ToString UserStatus where
  toString UserStatusUnknown      = "unknown"
  toString UserStatusOnline       = "online"
  toString UserStatusOffline      = "offline"
  toString UserStatusSkypeMe      = "skype_me"
  toString UserStatusAway         = "away"
  toString UserStatusNotAvailable = "not_available"
  toString UserStatusDoNotDisturb = "do_not_disturb"
  toString UserStatusInvisible    = "invisible"
  toString UserStatusLoggedOut    = "logged_out"

instance ToString UserBuddyStatus where
  toString UserBuddyStatusNeverBeen = "never_been"
  toString UserBuddyStatusDeleted   = "deleted"
  toString UserBuddyStatusPending   = "pending"
  toString UserBuddyStatusAdded     = "added"

instance ToString UserOnlineStatus where
  toString UserOnlineStatusUnknown      = "unknown"
  toString UserOnlineStatusOffline      = "offline"
  toString UserOnlineStatusOnline       = "online"
  toString UserOnlineStatusAway         = "away"
  toString UserOnlineStatusNotAvailable = "not_available"
  toString UserOnlineStatusDoNotDisturb = "do_not_disturb"
