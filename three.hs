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
  todoRef <- newIORef ts
  updateTodos todoRef
  select "input#new-todo" >>= J.on (create todoRef) "keyup" def
  select "button#clear-completed" >>= click (clearCompleted todoRef) def
  select "input#toggle-all" >>= click (toggleAll todoRef) def
  updateBindings todoRef
  -- setWindowLocationHash $ toJSString "hash"

updateTodos todoRef = do
  l <- todoList todoRef
  s <- select "#todo-list"
  replaceWith l s
  buts <- select "button.destroy"
  click (destroy todoRef) def buts
  togs <- select "input.toggle"
  click (toggle todoRef) def togs
  select "#todo-list label" >>= click (beginEdit todoRef) def
  return ()

destroy todoRef e = do
  x <- target e >>= selectElement
  a <- getAttr "n" x
  let mn = readMay a
  case mn of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef todoRef $ app1Ref todoDestroy n
      parent x >>= detach
      updateBindings todoRef

toggle todoRef e = do
  xs <- target e >>= selectElement >>= getAttr "n"
  let mx = readMay xs
  case mx of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef todoRef $ app1Ref todoToggle n
      updateTodos todoRef -- XXX shouldn't replace complete list
      updateBindings todoRef

toggleAll todoRef e = do
  x <- target e >>= selectElement >>= is ":checked"
  atomicModifyIORef todoRef $ app0Ref (if x then todoAllSet else todoAllReset)
  updateTodos todoRef -- XXX shouldn't replace complete list
  updateBindings todoRef

create todoRef e = do
  k <- which e
  when (chr k == '\r') $ do
    i <- select "input#new-todo"
    v <- getVal i
    setVal "" i
    myThing <- select $ "<div>create called: " ++ tshow v ++ "</div>"
    select "body" >>= appendJQuery myThing
    atomicModifyIORef todoRef $ app1Ref todoCreate v
    updateTodos todoRef
    updateBindings todoRef

beginEdit todoRef e = do
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
  J.on (acceptEdit todoRef n) "focusout"  def s
  J.on (keyEdit todoRef n) "keyup"  def s
  focus s
  return ()

keyEdit todoRef n e = do
  k <- which e
  when (chr k == '\r') $ acceptEdit todoRef n e
  when (chr k == '\ESC') $ updateTodos todoRef -- abort
  
acceptEdit todoRef n e = do
  x <- target e >>= selectElement
  v <- getVal x
  atomicModifyIORef todoRef $ app2Ref todoUpdate n v
  updateTodos todoRef

clearCompleted todoRef e = do
  atomicModifyIORef todoRef $ app0Ref todoClear
  updateTodos todoRef
  updateBindings todoRef

app0Ref f x = (f x, ())
app1Ref f x y = (f x y, ())
app2Ref f x y z = (f x y z, ())

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

updateBindings r = do
  ts <- readIORef r
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
  
todoList r = do
  ts <- sort <$> readIORef r
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
type Todos = [Todo]

initialTodos :: Todos
initialTodos = 
  [ (3, "Steal underpants", True)
  , (14, "???", False)
  , (16, "Profit!", False)
  ]

-- lookupHash :: IO Text
-- lookupHash = do
--   uj <- windowLocationHash
--   let u = T.pack $ fromJSString uj
--       (_, h) = T.breakOn "#" u
--   return $ T.drop 1 h
  
-- doesn't decode
-- lookupQueryString :: Text -> IO (Maybe Text)
-- lookupQueryString k = do
--   uj <- windowLocationHRef
--   let u = T.pack $ fromJSString uj
--       (_, q) = T.breakOnEnd "?" u
--       qs0 = T.splitOn "&" q -- XXX ";" is permitted, although rare
--       qs1 = map (T.breakOnEnd "=") qs0
--   return $ lookup (k ++ "=") qs1
