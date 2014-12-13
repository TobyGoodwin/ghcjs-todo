{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding ((++))

import ClassyPrelude ((++), readMay)
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
	xs <- target e >>= selectElement >>= getAttr "n"
	let mx = readMay xs
	case mx of
          Nothing -> return ()
          Just n -> do
            atomicModifyIORef todoRef $ todoDestroy n
            select ("#todo-list-" ++ xs) >>= detach
            updateBindings todoRef
            return ()
  buts <- select "button.destroy"
  click destroy def buts
  -- select "body" >>= appendJQuery myCount
  --myThing <- fetchTodoList
  -- lift $ select "#todo-list-div" >>= appendJQuery myThing
  return ()

todoDestroy n ts =
  let mt = L.find (\(x,_,_) -> x == n) ts
  in case mt of
    Nothing -> (ts, ())
    Just t -> (L.delete t ts, ())

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
      -- updateBindings
      return ()

updateBindings r = do
  ts <- readIORef r
  myThing <- select $ "<div>" ++ (T.pack $ show ts) ++ "</div>"
  select "body" >>= appendJQuery myThing
  let nDone = L.length $ L.filter (\(_, _, c) -> c) ts
      nLeft = L.length ts - nDone
  select "#bind-n-left" >>= setText (T.pack $ show nLeft)
  select "#bind-n-done" >>= setText (T.pack $ show nDone)
  -- e <- lift $ select "#todo-count"
  -- setText (T.pack $ show nLeft) e
  
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
