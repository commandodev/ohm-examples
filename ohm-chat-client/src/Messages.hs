{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Messages where

import GHC.Generics
import Data.Aeson hiding ((.=))
import Data.Set
import qualified Data.Set as S
import Data.Text (Text)
import Control.Lens

import ChatTypes

data View =
    LoginView
  | ChatView
  deriving (Show, Generic)
  
instance ToJSON View
instance FromJSON View

              
data ChatModel = ChatModel {
    _messages :: [Said]
  , _peopleChatting :: Set Text
  , _peopleTyping :: Set Text
  , _userName :: Maybe Text
  , _msgBox :: Text
  } deriving (Show)

data LoginModel = LoginModel {
   _loginBox :: Text
   } deriving (Show)

data AppModel = AppModel {
    _currentView :: View
  , _chat :: ChatModel
  , _login :: LoginModel
  } deriving Show
  
makeLenses ''ChatModel
makeLenses ''LoginModel
makeLenses ''AppModel


data ChatMessage = 
   SomeoneTyping UserName
 | StopTyping UserName
 | SetMessageBoxText Text
 | SetName UserName
 | NewUser UserJoined
 | UserLeft UserJoined
 | NewChatMessage Said
 | LoadState InitialState
 | CurrentlyConnected (Set Uname)
 | CurrentlyTyping (Set Uname)
 deriving (Show, Generic)

makePrisms ''ChatMessage

instance ToJSON ChatMessage
instance FromJSON ChatMessage


data ChatUIEvent = 
   EnteringText Text
 | EnterMessage Said
 deriving (Show, Generic)

makePrisms ''ChatUIEvent

instance ToJSON ChatUIEvent
instance FromJSON ChatUIEvent

--------------------------------------------------------------------------------

data LoginMessage = 
   EnteringName Text
 | UserLogin Text
 deriving (Show, Generic)

makePrisms ''LoginMessage

instance ToJSON LoginMessage
instance FromJSON LoginMessage


data Message chat = 
    SwitchView View
  | Login LoginMessage
  | Chat chat
  deriving (Show, Generic)

makePrisms ''Message
  
instance ToJSON a => ToJSON (Message a)
instance FromJSON a => FromJSON (Message a)


process :: Message ChatMessage -> AppModel -> AppModel
process (SwitchView v) model = model & currentView .~ v

process (Login (EnteringName uName)) model = model & login.loginBox .~ uName
process (Login (UserLogin uName)) model = model &~ do
    login.loginBox .= ""
    chat.userName .= Just uName
    
process (Chat msg) model = model & chat %~ processChat msg

processChat :: ChatMessage -> ChatModel -> ChatModel
processChat (SetMessageBoxText s) model = model & msgBox .~ s
processChat (SomeoneTyping (UserName name)) model = model & peopleTyping %~ (S.insert name)
processChat (StopTyping (UserName name)) model = model & peopleTyping %~ (S.delete name)
processChat (SetName (UserName name)) model = model & userName .~ Just name
processChat (NewUser (UserJoined name)) model = model &~ do
  peopleChatting %= (S.insert name)
processChat (UserLeft (UserJoined name)) model = model &~ do
  peopleChatting %= (S.delete name) 
processChat (NewChatMessage message) model = model &~ do
   messages %= (message:)
processChat (LoadState (InitialState c t m)) model = model &~ do
   peopleChatting .= c
   peopleTyping .= t
   messages .= m
processChat (CurrentlyConnected c) model = model & peopleChatting .~ c
processChat (CurrentlyTyping t) model = model & peopleTyping .~ t

