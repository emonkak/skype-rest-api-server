{-# LANGUAGE OverloadedStrings #-}

module Web.Skype.Server.Database where

-- import qualified Database.SQLite.Simple as SQLite

initDatabase :: IO ()
initDatabase = return ()

sql_createChatMessagesTable :: String
sql_createChatMessagesTable =
  "CREATE TABLE chats (chat_id TEXT PRIMARY KEY\
                      \timestamp INTEGER)\
                      \adder TEXT\
                      \status TEXT NOT NULL\
                      \topic TEXT NOT NULL\
                      \window_title TEXT NOT NULL\
                      \is_bookmarked INTEGER NOT NULL)"
