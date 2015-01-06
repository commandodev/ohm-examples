{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens hiding (Index, Action)
import Pipes
--import Prelude hiding ((.))
import Control.Applicative
import Data.Foldable (traverse_)
import Ohm.Component
import Ohm.HTML
import Prelude hiding (div,id,span,map, filter)
import qualified Prelude as P
                
--------------------------------------------------------------------------------

type Index = Int

data Filter = All | Active | Completed deriving (Eq, Show)

data Action
  = NewItem String
  | RemoveItem Index
  | SetEditText String
  | SetCompleted Index Bool
  | SetFilter Filter
  | DoNothing

data Item = Item 
 { _title :: String
 , _completed :: Bool
 } deriving Show
 
makeLenses ''Item

data ToDo = ToDo
  { _items :: [Item]
  , _editText :: String
  , _filter :: Filter
  } deriving Show

makeLenses ''ToDo 
 
initialToDo :: ToDo
initialToDo = ToDo [] "" All

--------------------------------------------------------------------------------

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = let (h, t) = splitAt idx xs
                  in h ++ tail t

logMessage :: (Show a, MonadIO m) => String -> a -> m () 
logMessage msg obj = liftIO . putStrLn $ msg ++ ": " ++ (show obj)

process :: Action -> ToDo -> ToDo
process (NewItem str) todo = todo &~ do
   items %= (Item str False:)
   editText .= ""
process (RemoveItem idx) todo = todo & items %~ deleteAt idx
process (SetEditText str) todo = todo & editText .~ str
process (SetCompleted idx c) todo = todo & items.element idx.completed .~ c
process (SetFilter f) todo = todo & filter .~ f
process _ todo = todo

  
--------------------------------------------------------------------------------


showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"

filterItems :: Filter -> [Item] -> [Item]
filterItems All = P.id
filterItems Active = P.filter (not . _completed)
filterItems Completed = P.filter _completed

todoView :: DOMEvent Action -> ToDo -> HTML
todoView chan todo@(ToDo itemList _txtEntry currentFilter) =
  with div
    (classes .= ["body"])
    [ titleRender, itemsRender, renderFilters chan todo]
  where
  titleRender = with h1 (classes .= ["title"]) ["todos"]
  itemsRender = with ul (classes .= ["items"])
    (newItem chan todo : (P.map (renderItem chan) $ zip [0..] filteredItems))
  filteredItems = filterItems currentFilter itemList

newItem :: DOMEvent Action -> ToDo -> HTML  
newItem chan todo =
  with li (classes .= ["newItem"])
    [ into form
      [ with input (do
             attrs . at "placeholder" ?= "Create a new task"
             attrs . at "value" ?= value
             onInput $ contramap SetEditText chan)
             []
        , with (btn click "Create") (attrs . at "hidden" ?= "true") ["Create"]
      ]
    ]
  where
  value = (todo ^. editText.to toJSString)
  click = (const $ (channel chan) $ NewItem (todo ^. editText))

renderItem :: DOMEvent Action -> (Int, Item) -> HTML
renderItem chan (idx, (Item itemTitle complete)) =
  into li
    [ into form
      [ with input (do
          attrs . at "type" ?= "checkbox"
          attrs . at "title" ?= "Mark as Completed"
          attrs . at "checked" ?= (if complete then "checked" else "")
          onChange $ contramap (const $ SetCompleted idx (if complete then False else True)) chan
          classes .= ["completed"])
          []
      , with span (classes .= ["description"])
          [text itemTitle]
      , (btn clickCancel "✖") &~ do
          classes .= ["complete"]
          attrs . at "title" ?= "Remove Item"
      ]
    ]
  where clickCancel = const $ channel chan $ RemoveItem idx

renderFilters :: DOMEvent Action -> ToDo -> HTML
renderFilters chan todo =
  with ul (classes .= ["filters"])
    (renderFilter <$> [All, Active, Completed])
  where
  currentFilter = (todo ^. filter)
  renderFilter f =
    into li
      [ with a (do
          attrs . at "href" ?= "#"
          classes .= (if f == currentFilter then ["selected"] else [])
          onClick $ filterClick f)
          [text $ showFilter f]
      ]
  filterClick f = DOMEvent $ const $ (channel chan) $ SetFilter f      

btn :: (() -> IO ()) -> String -> HTML
btn click txt = with button (onClick $ DOMEvent click) [text txt]         

--------------------------------------------------------------------------------


modelComp :: Component () Action ToDo Action
modelComp = Component process todoView idProcessor -- (Pipes.map P.id)

main :: IO ()
main = void $ runComponent (initialToDo) () modelComp
