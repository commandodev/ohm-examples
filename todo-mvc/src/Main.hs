{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens hiding (Index, Action)
import Pipes
--import Prelude hiding ((.))
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Foldable (traverse_)
import Ohm.Component
import Ohm.HTML
import VirtualDom
import VirtualDom.Prim (HTMLElement, _HTMLElement, HTML, text, properties, attributes)
import VirtualDom.HTML.Attributes hiding (form_, span_)
                
import GHCJS.Foreign

--------------------------------------------------------------------------------

type Index = Int

data Filter = All | Active | Completed deriving (Eq, Show)

data Action
  = NewItem String
  | RemoveItem Index
  | SetEditText String
  | SetCompleted Index Bool
  | SetFilter Filter

data Item = Item 
 { _title :: String
 , _completed :: Bool
 } deriving Show
 
--makeLenses ''Item

data ToDo = ToDo
  { _items :: [Item]
  , _editText :: String
  , _filter :: Filter
  } deriving Show

-- makeLenses ''ToDo 
 
initialToDo :: ToDo
initialToDo = ToDo [] "" All

--------------------------------------------------------------------------------

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = let (h, t) = splitAt idx xs
                  in h ++ tail t

logMessage :: (Show a, MonadIO m) => String -> a -> m () 
logMessage msg obj = liftIO . putStrLn $ msg ++ ": " ++ (show obj)

process :: Action -> ToDo -> ToDo
process = undefined
--process (NewItem str) todo = todo &~ do
--   (to _items) %= (Item str False:)
--   to _editText .= ""
-- process (RemoveItem idx) todo = todo & to _items %~ deleteAt idx
-- process (SetEditText str) todo = todo & to _editText .~ str
-- process (SetCompleted idx c) todo = todo & to _items.element idx.to _completed .~ c
-- process (SetFilter f) todo = todo & to _filter .~ f

  
--------------------------------------------------------------------------------


showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"

filterItems :: Filter -> [Item] -> [Item]
filterItems All = id
filterItems Active = filter (not . _completed)
filterItems Completed = filter _completed

todoView :: DOMEvent Action -> ToDo -> HTML
todoView chan todo@(ToDo itemList _txtEntry currentFilter) =
  with div_
    (classes .= ["body"])
    [ titleRender, itemsRender, renderFilters chan todo]
  where
  titleRender = with h1_ (classes .= ["title"]) ["todos"]
  itemsRender = with ul_ (classes .= ["items"])
    (newItem chan todo : (map (renderItem chan) $ zip [0..] filteredItems))
  filteredItems = filterItems currentFilter itemList

newItem :: DOMEvent Action -> ToDo -> HTML  
newItem chan todo =
  with li_ (classes .= ["newItem"])
    [ into form_
      [ with input_ (do
             attributes . at "placeholder" ?= "Create a new task"
             properties . at "value" ?= value
             onInput $ contramap SetEditText chan)
             []
        , with (btn click "Create") (attributes . at "hidden" ?= "true") ["Create"]
      ]
    ]
  where
  value = (todo ^. to _editText.to toJSString)
  click = (const $ (channel chan) $ NewItem (todo ^. to _editText))

renderItem :: DOMEvent Action -> (Int, Item) -> HTML
renderItem chan (idx, (Item itemTitle complete)) =
  into li_
    [ into form_
      [ with input_ (do
          properties . at "type" ?= "checkbox"
          attributes . at "title" ?= "Mark as Completed"
          properties . at "checked" ?= (if complete then "checked" else "")
          onChange $ contramap (const $ SetCompleted idx (if complete then False else True)) chan
          classes .= ["completed"])
          []
      , with span_ (classes .= ["description"])
          [text itemTitle]
      , cancelBtn
      ]
    ]
  where
  clickCancel = const $ channel chan $ RemoveItem idx
  cancelBtn = (btn clickCancel "✖") & _HTMLElement %~
    (execState $ do
       classes .= ["complete"]
       attributes . at "title" ?= "Remove Item")

renderFilters :: DOMEvent Action -> ToDo -> HTML
renderFilters chan todo =
  with ul_ (classes .= ["filters"])
    (renderFilter <$> [All, Active, Completed])
  where
  currentFilter = (todo ^. to _filter)
  renderFilter f =
    into li_
      [ with a_ (do
          attributes . at "href" ?= "#"
          classes .= (if f == currentFilter then ["selected"] else [])
          onClick $ filterClick f)
          [text $ showFilter f]
      ]
  filterClick f = DOMEvent $ const $ (channel chan) $ SetFilter f      

btn :: (() -> IO ()) -> String -> HTML
btn click txt = with button_ (onClick $ DOMEvent click) [text txt]         

--------------------------------------------------------------------------------


modelComp :: Component () Action ToDo Action
modelComp = Component process todoView idProcessor

main :: IO ()
main = 
  void $ initDomDelegator >> runComponent initialToDo () modelComp
