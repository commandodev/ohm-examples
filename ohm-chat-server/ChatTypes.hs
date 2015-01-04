{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module ChatTypes where

import GHC.Generics
import Data.Set (Set)
import Data.Aeson as Aeson
import qualified Data.Text as Text

type Uname = Text.Text

data AddUser = AddUser Uname deriving (Show, Generic)

instance Aeson.ToJSON AddUser
instance Aeson.FromJSON AddUser

data NewMessage = NewMessage Uname deriving (Show, Generic)

instance Aeson.ToJSON NewMessage
instance Aeson.FromJSON NewMessage

data Said = Said Uname Text.Text deriving (Show, Generic)

instance Aeson.ToJSON Said
instance Aeson.FromJSON Said

data UserName = UserName Uname deriving (Show, Generic)

instance Aeson.ToJSON UserName
instance Aeson.FromJSON UserName

data UserJoined = UserJoined Uname deriving (Show, Generic)

instance Aeson.ToJSON UserJoined
instance Aeson.FromJSON UserJoined


data Loggedin = Loggedin Uname deriving (Show, Generic)

instance Aeson.ToJSON Loggedin
instance Aeson.FromJSON Loggedin

data InitialState = InitialState {
    sConnected :: Set Uname
  , sTyping :: Set Uname
  , sMessages :: [Said]
  } deriving (Show, Generic)

instance Aeson.ToJSON InitialState
instance Aeson.FromJSON InitialState

