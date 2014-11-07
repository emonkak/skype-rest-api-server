module App.Router where

import App.Controller
import Web.Scotty

import qualified Network.Skype.API as Skype

router :: Skype.Connection -> ScottyM ()
router connection = do
  get  "/chats/"                        $ getAllChats connection
  get  "/chats/search"                  $ getSearchChatMessage
  get  "/users/"                        $ getAllUsers connection
  get  "/users/:user_id"                $ getUser connection
  get  (regex "^/chats/messages/(.*)")  $ getAllChatMessages connection
  get  (regex "^/chats/(.*)")           $ getChat connection
  post (regex "^/chats/(.*)")           $ postChat connection
