{-# LANGUAGE FlexibleContexts,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import App.Database
import App.Entity
import App.Fetcher
import App.Router
import Control.Concurrent.Lifted (fork)
import Control.Exception (IOException, handle)
import Control.Monad (unless, void, when)
import Control.Monad.Error.Class (Error, MonadError, strMsg, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Skype.Protocol
import System.Console.GetOpt
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.Process (readProcess)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Network.Skype.API as Skype
import qualified Network.Skype.Command.Chat as Chat
import qualified Network.Skype.Command.Misc as Skype
import qualified Network.Skype.Core as Skype
import qualified Network.Skype.Parser as Skype
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.Scotty as Scotty

data AppOptions = AppOptions
  { appOptionCallback :: Maybe FilePath
  , appOptionServer :: Warp.Settings
  }

defaultOptions :: AppOptions
defaultOptions = AppOptions
  { appOptionCallback = Nothing
  , appOptionServer = Warp.defaultSettings
  }

optionSpec :: [OptDescr (AppOptions -> AppOptions)]
optionSpec =
  [ Option ['c'] ["callback"]
           (ReqArg (\param opts -> opts { appOptionCallback = Just param }) "FILE")
           "Notification callback script path"
  , Option ['p'] ["port"]
           (ReqArg (\param opts -> opts { appOptionServer = Warp.setPort (read param) (appOptionServer opts) }) "PORT")
           "Port to listen on"
  ]

parseOptions :: (MonadError e m, Error e) => [String] -> m AppOptions
parseOptions args = case getOpt Permute optionSpec args of
  (opts, _, [])  -> return $ foldl (flip id) defaultOptions opts
  (_, _, errors) -> throwError $ strMsg (concat errors ++ usage)
  where
    usage = usageInfo "Usage: skype-rest-api-server [OPTION...]" optionSpec

invokeCallbackScript :: (MonadIO m, MonadBaseControl IO m, Skype.MonadSkype m)
                     => ChatMessageEntity
                     -> FilePath
                     -> Skype.SkypeT m ()
invokeCallbackScript chatMessage script = do
  let chatID = _chatMessageChat chatMessage
  chat <- fetchChat $ chatID

  let json = case Aeson.toJSON chatMessage of
               Aeson.Object values -> Aeson.Object $ HM.insert "chat" (Aeson.toJSON chat) values
               x                   -> x

  path <- liftIO $ canonicalizePath script
  output <- liftIO $ handle (\(_ :: IOException) -> return "") $
                     readProcess path [] $ BSLC.unpack $ Aeson.encode json

  unless (null output) $ void $ Chat.sendMessage chatID $ T.pack output

main :: IO ()
main = do
  options <- getArgs >>= parseOptions

  connection <- Skype.connect "skype-rest-api-server"

  initDatabase

  _ <- Skype.runSkype connection $ do
    Skype.protocol 9999

    fork $ Skype.onNotification $ \notification -> do
      liftIO $ BSLC.putStrLn notification

      case Skype.parseNotification notification of
        Right (ChatMessage chatMessageID (ChatMessageStatus status)) -> do
          chatMessage <- fetchChatMessage chatMessageID

          liftIO $ withDatabase $ storeChatMessage chatMessage

          when (status == ChatMessageStatusReceive) $
            maybe (return ()) (invokeCallbackScript chatMessage) $ appOptionCallback options
        _ -> return ()

  Scotty.scottyOpts (Scotty.Options 1 $ appOptionServer options) $
                    router connection
