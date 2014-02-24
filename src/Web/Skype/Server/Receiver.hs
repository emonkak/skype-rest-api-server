{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Skype.Server.Receiver where

import Web.Skype.Protocol

receiver :: NotificationObject -> IO ()
receiver _ = return ()
