{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Data.Aeson (ToJSON)
import qualified Data.Set as S
import Pipes
import qualified Pipes.Concurrent as PC
--import Prelude hiding ((.))
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Messages
import Render
import Ohm.Component
import ChatTypes
import Ohm.SocketIO ( SocketIO, socketIONew, socketIOWaitForConnection, socketIOOpen
                , sioSend, sioSend_, sioSub
                )

initialModel :: AppModel
initialModel = AppModel {
    _currentView = LoginView
  , _chat = ChatModel {
        _messages = []
      , _peopleChatting = S.empty
      , _peopleTyping = S.empty
      , _userName = Nothing
      , _msgBox    = ""
      }
  , _login = LoginModel {
        _loginBox = ""
      }
  }
  
data Env = Env {
    ws :: SocketIO
  }
  
type ProcessorMonad = ReaderT Env IO

logMessage :: (Show a, MonadIO m) => String -> a -> m () 
logMessage msg a = liftIO . putStrLn $ msg ++ ": " ++ (show a)

wsEmit :: (ToJSON a) => String -> a -> ProcessorMonad ()
wsEmit chan msg = do
  sio <- ws <$> ask
  liftIO $ sioSend sio chan msg

wsEmit_ :: String -> ProcessorMonad ()
wsEmit_ chan = do
  sio <- ws <$> ask
  liftIO $ sioSend_ sio chan


chatMessageProcessor :: Processor ProcessorMonad ChatUIEvent ChatMessage
chatMessageProcessor = Processor $ \msg -> do
  case msg of
    EnteringText t -> do
      lift $ wsEmit_ "typing"
      yield $ SetMessageBoxText t
    EnterMessage (Said _ m) -> do
      logMessage "SAID" msg
      lift $ wsEmit "new message" $ NewMessage m 
      lift $ wsEmit_ "stop typing"
      yield (SetMessageBoxText "")

appMessageProcessor :: Processor ProcessorMonad (Message ChatUIEvent) (Message ChatMessage)
appMessageProcessor = Processor $ \msg -> do
  case msg of
    Login (UserLogin uName) -> do
      logMessage "LOGIN" msg
      lift $ wsEmit "add user" (AddUser uName)
      yield (SwitchView ChatView)
      yield (Login (UserLogin uName))
    m@(Login (EnteringName n)) -> do
      logMessage "ENTERINGNAME" m
      yield (Login (EnteringName n))
    _ -> return ()


modelComp :: Component Env (Message ChatMessage) AppModel (Message ChatUIEvent)
modelComp = Component process rootView combined
  where
    combined = (Chat <$> handles _Chat chatMessageProcessor)
            <> appMessageProcessor

main :: IO ()
main = do
  s <- socketIONew "http://localhost:8000"
  putStrLn "socket"
  socketIOOpen s
  putStrLn "open"
  socketIOWaitForConnection s
  putStrLn "connected"
  modelEvents <- runComponent (initialModel) (Env s) modelComp
  sioSub s "login"               $ \(Loggedin _) -> 
    sioSend_ s "load state"                                                             
  sioSub s "new message"         $ sendToModel modelEvents (Chat . NewChatMessage)               
  sioSub s "user joined"         $ sendToModel modelEvents (Chat . NewUser)                    
  sioSub s "user left"           $ sendToModel modelEvents (Chat . UserLeft)                   
  --sioSub s "login"             $ sendToModel modelEvents (Chat . NewUser)                  
  -- sioSub s "typing"              $ sendToModel modelEvents (Chat . SomeoneTyping)              
  -- sioSub s "stop typing"         $ sendToModel modelEvents (Chat . StopTyping)                 
  sioSub s "currently connected" $ sendToModel modelEvents (Chat . CurrentlyConnected) 
  sioSub s "currently typing"    $ sendToModel modelEvents (Chat . CurrentlyTyping) 
  sioSub s "state"               $ sendToModel modelEvents (Chat . LoadState)
  
  where
    sendToModel evts f = void . atomically . PC.send evts . f
