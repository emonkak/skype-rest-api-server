module App.Router where

import App.Controller
import Web.Scotty

import qualified Web.Skype.API as Skype

router :: Skype.Connection -> ScottyM ()
router connection = do
  get  "/chats/"                        $ getAllChats connection
  get  "/chats/search"                  $ getSearchChatMessage
  get  "/users/"                        $ getAllUsers connection
  get  "/users/:user_id"                $ getUser connection
  get  (regex "^/chats/(#.*)")          $ getChat connection
  post (regex "^/chats/(#.*)")          $ postChat connection
  get  (regex "^/chats/messages/(#.*)") $ getAllChatMessages connection
