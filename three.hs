{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding ((++))

import ClassyPrelude ((++))
import Control.Monad
import Control.Monad.State (StateT, get, lift, runStateT, put)
import Control.Monad.Trans.Control (control)
import Data.Default
import Data.IORef
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHCJS.Foreign
import JavaScript.JQuery
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

main = do
  -- myClick <- select "<div>click here</div>"
  let ts = initialTodos
  todoRef <- newIORef ts
  l <- todoList todoRef
  select "#todo-list-div" >>= appendJQuery l
  myClick <- select "#destroy-3"
  myCount <- select "<div>0</div>"
  let getCount = atomicModifyIORef todoRef
			(\c -> let c' = tail c
                                in (c', T.pack $ show (length c')))
      showCount _ = do
        x <- getCount
        setText x myCount
        return ()
  let destroy e = do
	x <- target e >>= selectElement >>= getAttr "n"
        -- y <- parentsUntil "li" (Just "li") x
        y <- select $ "#todo-list-" ++ x
	detach y
	return ()
  -- click showCount def myClick
  -- buts <- find "button.destroy" l
  -- buts <- select "button#destroy-16"
  buts <- select "button.destroy"
  click destroy def buts
  -- lift $ click showCount def myClick
  -- select "body" >>= appendJQuery myClick >>= appendJQuery myCount
  select "body" >>= appendJQuery myCount
  
  --myThing <- fetchTodoList
  -- lift $ select "#todo-list-div" >>= appendJQuery myThing
  return ()

thing :: Int -> IO ()
thing i = do
  myThing <- select "<div>this is my new thing!</div>"
  select "body" >>= appendJQuery myThing
  select (T.append "#todo-list-" (T.pack (show i))) >>= detach
  -- select "#todo-list-3" >>= detach
  return ()

thing2 :: Int -> StateT Todos IO ()
thing2 i = do
  ts <- get
  let mt = L.find (\(x,_,_) -> x == i) ts
  case mt of
    Nothing -> do
      myThing <- lift $ select "<div>not found</div>"
      lift $ select "body" >>= appendJQuery myThing
      return ()
    Just t -> do
      put $ L.delete t ts
      lift $ select (T.append "#todo-list-" (T.pack (show i))) >>= detach
      updateBindings
      return ()

updateBindings = do
  myThing <- lift $ select "<div>this is my new thing!</div>"
  lift $ select "body" >>= appendJQuery myThing
  ts <- get
  let left = L.length $ L.filter (\(_, _, c) -> c) ts
  e <- lift $ select "#todo-count strong"
  -- e <- lift $ select "#todo-count"
  lift $ setText (T.pack $ show left) e
  
todoList r = do
  ts <- readIORef r
  select $ T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (i, t, c) <- ts
        <li :c:.completed #todo-list-#{i}>
          <input .toggle type=checkbox>
          <label>
            #{t}
          <button #destroy-#{i} n=#{i} class=destroy>
  |]

type Todo = (Int, Text, Bool)
type Todos = [Todo]

initialTodos :: Todos
initialTodos = 
  [ (19, "Steal underpants", True)
  , (3, "???", False)
  , (16, "Profit!", False)
  ]
