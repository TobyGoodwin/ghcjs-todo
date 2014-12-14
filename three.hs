{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude

import Control.Monad
import Control.Monad.State (StateT, get, lift, runStateT, put)
import Data.Default
-- import Data.IORef
import Data.Char (chr)
import qualified Data.List as L hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHCJS.Foreign
import JavaScript.JQuery hiding (not)
import qualified JavaScript.JQuery as J
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

main = do
  let ts = initialTodos
  todoRef <- newIORef ts
  updateTodos todoRef
  select "input#new-todo" >>=
    J.on (create todoRef) "keyup" def
  updateBindings todoRef
  return ()

updateTodos todoRef = do
  l <- todoList todoRef
  s <- select "#todo-list"
  replaceWith l s
  buts <- select "button.destroy"
  click (destroy todoRef) def buts
  togs <- select "input.toggle"
  click (toggle todoRef) def togs

destroy todoRef e = do
  xs <- target e >>= selectElement >>= getAttr "n"
  let mx = readMay xs
  case mx of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef todoRef $ todoDestroy n
      select ("#todo-list-" ++ xs) >>= detach
      updateBindings todoRef
      return ()

toggle todoRef e = do
  xs <- target e >>= selectElement >>= getAttr "n"
  let mx = readMay xs
  case mx of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef todoRef $ todoToggle n
      updateTodos todoRef
      updateBindings todoRef
      return ()

create todoRef e = do
  k <- which e
  when (chr k == '\r') $ do
    i <- select "input#new-todo"
    v <- getVal i
    setVal "" i
    myThing <- select $ "<div>create called: " ++ tshow v ++ "</div>"
    select "body" >>= appendJQuery myThing
    atomicModifyIORef todoRef $ todoCreate v
    updateTodos todoRef
    updateBindings todoRef

todoCreate :: Text -> [Todo] -> ([Todo], ())
todoCreate t ts =
  let n = fromMaybe 0 $ maximumMay $ map (\(x, _, _) -> x) ts
  in ((n+1, t, False) : ts, ())

todoDestroy n ts =
  let mt = L.find (\(x,_,_) -> x == n) ts
  in case mt of
    Nothing -> (ts, ())
    Just t -> (L.delete t ts, ())

todoToggle n ts =
  let mt = L.find (\(x,_,_) -> x == n) ts
  in case mt of
    Nothing -> (ts, ())
    Just todo@(i, t, c) -> ((i, t, not c) : L.delete todo ts, ())

updateBindings r = do
  ts <- readIORef r
  myThing <- select $ "<div>" ++ tshow ts ++ "</div>"
  select "body" >>= appendJQuery myThing
  let nDone = L.length $ L.filter (\(_, _, c) -> c) ts
      nLeft = L.length ts - nDone
  select "#bind-n-left" >>= setText (tshow nLeft)
  select "#bind-n-done" >>= setText (tshow nDone)
  return ()
  
todoList r = do
  ts <- sort <$> readIORef r
  -- let ts1 = sort ts0
  return $ T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (i, t, c) <- ts
        <li :c:.completed #todo-list-#{i}>
          <input .toggle n=#{i} type=checkbox :c:checked>
          <label>
            #{t}
          <button .destroy n=#{i}>
  |]

type Todo = (Int, Text, Bool)
type Todos = [Todo]

initialTodos :: Todos
initialTodos = 
  [ (3, "Steal underpants", True)
  , (14, "???", False)
  , (16, "Profit!", False)
  ]
