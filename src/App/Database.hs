{-# LANGUAGE OverloadedStrings #-}

module App.Database (
  initDatabase,
  withDatabase,
  findChat,
  findChatMessge,
  searchChatMessage,
  storeChat,
  storeChatMessage
) where

import App.Entity
import App.Tokenizer
import Control.Applicative
import Control.Monad.Reader
import Data.Maybe (listToMaybe)
import Data.Monoid
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Web.Skype.Protocol

import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite

type ConnectionT m a = ReaderT SQLite.Connection m a

initDatabase :: IO ()
initDatabase = withDatabase $ mapM_ execute_
  [ sql_createChatTable
  , sql_createChatMessagesTable
  , sql_createChatMessagesFTSTable
  ]
  where
    sql_createChatTable = mconcat
      [ "CREATE TABLE IF NOT EXISTS chats"
      , "( chat_id TEXT PRIMARY KEY"
      , ", timestamp INTEGER NOT NULL"
      , ", adder TEXT DEFAULT NULL"
      , ", status TEXT NOT NULL"
      , ", topic TEXT NOT NULL"
      , ", window_title TEXT NOT NULL"
      , ")"
      ]

    sql_createChatMessagesTable = mconcat
      [ "CREATE TABLE IF NOT EXISTS chat_messages"
      , "( chat_message_id INTEGER PRIMARY KEY"
      , ", timestamp INTEGER NOT NULL"
      , ", sender TEXT NOT NULL"
      , ", sender_display_name TEXT NOT NULL"
      , ", type TEXT NOT NULL"
      , ", status TEXT NOT NULL"
      , ", leave_reason TEXT"
      , ", chat TEXT NOT NULL"
      , ", body TEXT NOT NULL"
      , ")"
      ]

    sql_createChatMessagesFTSTable = mconcat
      [ "CREATE VIRTUAL TABLE IF NOT EXISTS chat_messages_fts USING fts4"
      , "( chat_message_id INTEGER PRIMARY KEY"
      , ", tokens TEXT NOT NULL"
      , ")"
      ]

findChat :: ChatID -> ConnectionT IO (Maybe ChatEntity)
findChat = ((<$>) listToMaybe) . query sql . SQLite.Only
  where
    sql = "SELECT * FROM chats WHERE chat_id = ?"

findChatMessge :: ChatMessageID -> ConnectionT IO (Maybe ChatMessageEntity)
findChatMessge = ((<$>) listToMaybe) . query sql . SQLite.Only
  where
    sql = "SELECT * FROM chat_messages WHERE chat_message_id = ?"

searchChatMessage :: T.Text -> ConnectionT IO [ChatMessageEntity]
searchChatMessage = query sql . SQLite.Only . T.intercalate " " . bigram
  where
    sql = "SELECT chat_messages.* FROM chat_messages_fts JOIN chat_messages USING (chat_message_id) WHERE tokens MATCH ?"

storeChat :: ChatEntity -> ConnectionT IO ()
storeChat chat = do
  execute sql_insertIntoChat
    ( _chatID chat
    , _chatTimestamp chat
    , _chatAdder chat
    , _chatStatus chat
    , _chatTopic chat
    , _chatWindowTitle chat
    )
  where
    sql_insertIntoChat = mconcat
      [ "INSERT OR REPLACE INTO chats"
      , "( chat_id"
      , ", timestamp"
      , ", adder"
      , ", status"
      , ", topic"
      , ", window_title"
      , ") VALUES (?, ?, ?, ?, ?, ?)"
      ]

storeChatMessage :: ChatMessageEntity -> ConnectionT IO ()
storeChatMessage chatMessage = do
  execute sql_insertIntoChatMessages
    ( _chatMessageID chatMessage
    , _chatMessageTimestamp chatMessage
    , _chatMessageSender chatMessage
    , _chatMessageSenderDisplayName chatMessage
    , _chatMessageType chatMessage
    , _chatMessageStatus chatMessage
    , _chatMessageLeaveReason chatMessage
    , _chatMessageChat chatMessage
    , _chatMessageBody chatMessage
    )
  execute sql_insertIntoChatMessagesFTS
    ( _chatMessageID chatMessage
    , T.intercalate " " $ bigram $ _chatMessageBody chatMessage
    )
  where
    sql_insertIntoChatMessages = mconcat
      [ "INSERT OR REPLACE INTO chat_messages"
      , "( chat_message_id"
      , ", timestamp"
      , ", sender"
      , ", sender_display_name"
      , ", type"
      , ", status"
      , ", leave_reason"
      , ", chat"
      , ", body"
      , ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ]

    sql_insertIntoChatMessagesFTS = mconcat
      [ "INSERT OR REPLACE INTO chat_messages_fts"
      , "( chat_message_id"
      , ", tokens"
      , ") VALUES (?, ?)"
      ]

-- * Utilities

withDatabase :: ConnectionT IO a -> IO a
withDatabase action = do
  homeDirectory <- getHomeDirectory
  SQLite.withConnection (homeDirectory </> ".skype.db") (runReaderT action)

execute :: (SQLite.ToRow q) => SQLite.Query -> q -> ConnectionT IO ()
execute template qs = do
  conn <- ask
  liftIO $ SQLite.execute conn template qs

execute_ :: SQLite.Query -> ConnectionT IO ()
execute_ template = do
  conn <- ask
  liftIO $ SQLite.execute_ conn template

query :: (SQLite.ToRow q, SQLite.FromRow r) => SQLite.Query -> q -> ConnectionT IO [r]
query template qs = do
  conn <- ask
  liftIO $ SQLite.query conn template qs

query_ :: (SQLite.FromRow r) => SQLite.Query -> ConnectionT IO [r]
query_ template = do
  conn <- ask
  liftIO $ SQLite.query_ conn template
