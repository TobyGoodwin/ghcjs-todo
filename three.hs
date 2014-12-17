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
import GHCJS.Prim (fromJSString, toJSString)
import GHCJS.Types (JSString)
import JavaScript.JQuery hiding (filter, find, not)
import qualified JavaScript.JQuery as J
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

foreign import javascript unsafe "$r = window.location.hash;"
  windowLocationHash :: IO JSString
foreign import javascript safe   "window.location.hash = $1;"
  setWindowLocationHash :: JSString -> IO ()

main = do
  h <- T.pack . fromJSString <$> windowLocationHash
  let f = if T.null h then "all" else T.drop 1 h
  select ("a#filter-" ++ f) >>= addClass "selected"
  -- h <- windowLocationHRef
  -- myThing <- select $ "<div>url: " ++ T.pack (fromJSString h) ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  myThing <- select $ "<div>filter: " ++ f ++ "</div>"
  select "body" >>= appendJQuery myThing
  let ts = initialTodos
      s = case f of
            "active" -> State (\(_, _, c) -> not c) ts
            "completed" -> State (\(_, _, c) -> c) ts
            _ -> State (const True) ts
  stateRef <- newIORef s
  updateTodos stateRef
  select "input#new-todo" >>= J.on (create stateRef) "keyup" def
  select "button#clear-completed" >>= click (clearCompleted stateRef) def
  select "input#toggle-all" >>= click (toggleAll stateRef) def
  updateBindings stateRef
  -- setWindowLocationHash $ toJSString "hash"

updateTodos :: IORef State -> IO ()
updateTodos stateRef = do
  l <- todoList stateRef
  s <- select "#todo-list"
  replaceWith l s
  buts <- select "button.destroy"
  click (destroy stateRef) def buts
  togs <- select "input.toggle"
  click (toggle stateRef) def togs
  select "#todo-list label" >>= click (beginEdit stateRef) def
  return ()

destroy stateRef e = do
  x <- target e >>= selectElement
  a <- getAttr "n" x
  let mn = readMay a
  case mn of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef stateRef $ app1Ref todoDestroy n
      parent x >>= detach
      updateBindings stateRef

toggle stateRef e = do
  xs <- target e >>= selectElement >>= getAttr "n"
  let mx = readMay xs
  case mx of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef stateRef $ app1Ref todoToggle n
      updateTodos stateRef -- XXX shouldn't replace complete list
      updateBindings stateRef

toggleAll stateRef e = do
  x <- target e >>= selectElement >>= is ":checked"
  atomicModifyIORef stateRef $ app0Ref (if x then todoAllSet else todoAllReset)
  updateTodos stateRef -- XXX shouldn't replace complete list
  updateBindings stateRef

create stateRef e = do
  k <- which e
  when (chr k == '\r') $ do
    i <- select "input#new-todo"
    v <- getVal i
    setVal "" i
    myThing <- select $ "<div>create called: " ++ tshow v ++ "</div>"
    select "body" >>= appendJQuery myThing
    atomicModifyIORef stateRef $ app1Ref todoCreate v
    updateTodos stateRef
    updateBindings stateRef

beginEdit stateRef e = do
  x <- target e >>= selectElement
  p <- parent x
  nt <- getAttr "n" p
  let n = fromMaybe 0 $ readMay nt
  t <- getText x
  let i = T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .editing>
      <input .edit value=#{t}>
  |]
  replaceWith i p
  s <- select "input.edit"
  J.on (acceptEdit stateRef n) "focusout"  def s
  J.on (keyEdit stateRef n) "keyup"  def s
  focus s
  return ()

keyEdit stateRef n e = do
  k <- which e
  when (chr k == '\r') $ acceptEdit stateRef n e
  when (chr k == '\ESC') $ updateTodos stateRef -- abort
  
acceptEdit stateRef n e = do
  x <- target e >>= selectElement
  v <- getVal x
  atomicModifyIORef stateRef $ app2Ref todoUpdate n v
  updateTodos stateRef

clearCompleted stateRef e = do
  atomicModifyIORef stateRef $ app0Ref todoClear
  updateTodos stateRef
  updateBindings stateRef

app0Ref f (State a ts) = (State a (f ts), ())
app1Ref f p (State a ts) = (State a (f p ts), ())
app2Ref f p q (State a ts) = (State a (f p q ts), ())

todoAllSet :: [Todo] -> [Todo]
todoAllSet = map (\(i, t, _) -> (i, t, True))

todoAllReset :: [Todo] -> [Todo]
todoAllReset = map (\(i, t, _) -> (i, t, False))

todoClear :: [Todo] -> [Todo]
todoClear = filter (\(_, _, c) -> not c)

todoCreate :: Text -> [Todo] -> [Todo]
todoCreate t ts =
  let n = fromMaybe 0 $ maximumMay $ map (\(x, _, _) -> x) ts
  in (n+1, t, False) : ts

todoDestroy n ts =
  let mt = L.find (\(x,_,_) -> x == n) ts
  in case mt of
    Nothing -> ts
    Just t -> L.delete t ts

todoToggle n ts =
  let mt = L.find (\(x,_,_) -> x == n) ts
  in case mt of
    Nothing -> ts
    Just todo@(i, t, c) -> (i, t, not c) : L.delete todo ts

todoUpdate :: Int -> Text -> [Todo] -> [Todo]
todoUpdate n t ts =
  case find (\(x,_,_) -> x == n) ts of
    Nothing -> todoCreate t ts
    Just todo@(i, _, c) -> (i, t, c) : L.delete todo ts

updateBindings stateRef = do
  State f ts <- readIORef stateRef
  myThing <- select $ "<div>" ++ tshow ts ++ "</div>"
  select "body" >>= appendJQuery myThing
  let nDone = L.length $ L.filter (\(_, _, c) -> c) ts
      nLeft = L.length ts - nDone
      pLeft = (if nLeft == 1 then "item" else "items") ++ " left"
  select "#bind-n-left" >>= setText (tshow nLeft)
  select "#bind-phrase-left" >>= setText pLeft
  select "#bind-n-done" >>= setText (tshow nDone)
  select "button#clear-completed" >>=
    setAttr "style" (if nDone == 0 then "display:none" else "display:block")
  select "input#toggle-all"
    >>= if nLeft == 0 then setProp "checked" "true" else removeProp "checked"
  return ()
  
todoList stateRef = do
  State f ts0 <- readIORef stateRef
  let ts = sort $ filter f ts0
  return $ T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (i, t, c) <- ts
        <li :c:.completed n=#{i}>
          <input .toggle n=#{i} type=checkbox :c:checked>
          <label>
            #{t}
          <button .destroy n=#{i}>
  |]

type Todo = (Int, Text, Bool)
type Filter = Todo -> Bool
data State = State Filter [Todo]

initialTodos :: [Todo]
initialTodos =
  [ (3, "Steal underpants", True)
  , (14, "???", False)
  , (16, "Profit!", False)
  ]
