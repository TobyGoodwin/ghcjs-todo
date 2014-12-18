{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude

import Control.Monad.State (StateT, get, lift, runStateT, put)
import Data.Default
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
  stateRef <- newIORef (State f initialTodos)
  updateTodos stateRef
  updateBindings stateRef
  initClicks stateRef

initClicks stateRef = do
  select "input#new-todo" >>= J.on (create stateRef) "keyup" def
  select "button#clear-completed" >>= click (clearCompleted stateRef) def
  select "input#toggle-all" >>= click (toggleAll stateRef) def
  mapM filterClick ["all", "active", "completed"]
  where
    filterClick f = select ("a#filter-" ++ f) >>= click (moveTo stateRef) def

updateTodos :: IORef State -> IO ()
updateTodos stateRef = do
  l <- todoList stateRef
  s <- select "#todo-list"
  replaceWith l s
  select "button.destroy" >>= click (destroy stateRef) def
  select "input.toggle" >>= click (toggle stateRef) def
  select "#todo-list label" >>= click (beginEdit stateRef) def
  return ()

destroy stateRef e = do
  x <- target e >>= selectElement
  a <- getAttr "n" x
  case readMay a of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef stateRef $ app1Ref todoDestroy n
      parent x >>= detach
      updateBindings stateRef

toggle stateRef e = do
  x <- target e >>= selectElement
  a <- getAttr "n" x
  case readMay a of
    Nothing -> return ()
    Just n -> do
      atomicModifyIORef stateRef $ app1Ref todoToggle n
      p <- parent x
      h <- hasClass "completed" p
      -- wot no toggleClass?
      if h then removeClass "completed" p
           else addClass "completed" p
      updateBindings stateRef

toggleAll stateRef e = do
  x <- target e >>= selectElement >>= is ":checked"
  atomicModifyIORef stateRef $ app0Ref (if x then todoAllSet else todoAllReset)
  updateTodos stateRef -- XXX shouldn't replace complete list
  updateBindings stateRef

moveTo stateRef e = do
  x <- target e >>= selectElement >>= getAttr "id"
  case stripPrefix "filter-" x of
    Nothing -> return ()
    Just f -> do
      State o _ <- readIORef stateRef
      select ("a#filter-" ++ o) >>= removeClass "selected"
      atomicModifyIORef stateRef $ \(State _ ts) -> (State f ts,())
      updateTodos stateRef
      updateBindings stateRef
  
create stateRef e = do
  k <- which e
  when (chr k == '\r') $ do
    i <- select "input#new-todo"
    v <- getVal i
    setVal "" i
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
  -- handy for debugging
  -- myThing <- select $ "<div>" ++ tshow f ++ "</div>"
  -- select "body" >>= appendJQuery myThing
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
  select ("a#filter-" ++ f) >>= addClass "selected"
  return ()
  
todoList stateRef = do
  State f ts0 <- readIORef stateRef
  let ts = sort $ filter (todoFilter f) ts0
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
type FilterName = Text
type Filter = Todo -> Bool
data State = State FilterName [Todo]

initialTodos :: [Todo]
initialTodos =
  [ (3, "Steal underpants", True)
  , (14, "???", False)
  , (16, "Profit!", False)
  ]

status :: Todo -> Bool
status (_, _, c) = c

todoFilter :: FilterName -> Filter
todoFilter "active" = not . status
todoFilter "completed" = status
todoFilter _ = const True
