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

eventHandle stateRef stateFn otherFn e = do
  stateChange stateRef stateFn
  let x = otherFn ()
  return ()

stateChange :: IORef State -> (State -> State) -> IO ()
stateChange stateRef f = do
  (old, new) <- atomicModifyIORef stateRef f'
  myThing <- select $ "<div>old is: " ++ tshow old ++ "</div>"
  select "body" >>= appendJQuery myThing
  myThing <- select $ "<div>new is: " ++ tshow new ++ "</div>"
  select "body" >>= appendJQuery myThing
  pageChange old new
  return ()
  where
    f' o = let n = f o in (n, (o, n))

pageChange :: State -> State -> IO ()
pageChange (State oldf oldts) (State newf newts) = do
  listChange (prep oldf oldts) (prep newf newts)
  where
    prep f = sort . filter (todoFilter f)

listChange :: [Todo] -> [Todo] -> IO ()
listChange [] [] = return ()
listChange [] (n:ns) = do
  listAppend n
  listChange [] ns
listChange (o:os) [] = do
  listDelete o
  listChange os []
listChange old@(o:os) new@(n:ns) =
  case o `compare` n of
    EQ -> listChange os ns
    LT -> do
      listDelete o
      listChange os new
    GT -> do
      listInsert n o
      listChange old ns

listDelete (i, t, c) = do
  myThing <- select $ "<div>will delete " ++ tshow i ++ "</div>"
  select "body" >>= appendJQuery myThing
  select ("#todo-list li[n='" ++ tshow i ++ "']") >>= detach

listAppend item = do
  myThing <- select $ "<div>will append " ++ tshow item ++ "</div>"
  select "body" >>= appendJQuery myThing
  x <- select $ listItem item
  select "#todo-list" >>= appendJQuery x

listInsert item (b, _, _)= do
  myThing <- select $ "<div>will insert " ++ tshow item ++ " before " ++ tshow b ++ "</div>"
  select "body" >>= appendJQuery myThing
  let x = listItem item
  select ("#todo-list li[n='" ++ tshow b ++ "']") >>= before x

listItem (i, t, c) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li :c:.completed n=#{i}>
      <input .toggle n=#{i} type=checkbox :c:checked>
      <label>
        #{t}
      <button .destroy n=#{i}>
  |]
  
initClicks stateRef = do
  select "input#new-todo" >>= J.on (create stateRef) "keyup" def
  -- select "button#clear-completed" >>= click (clearCompleted stateRef) def
  select "button#clear-completed" >>=
    click (eventHandle stateRef todoClear' id) def
  select "input#toggle-all" >>= click (toggleAll stateRef) def
  mapM filterClick ["all", "active", "completed"]
  where
    --filterClick f = select ("a#filter-" ++ f) >>= click (moveTo stateRef) def
    filterClick f = select ("a#filter-" ++ f) >>=
                      click (eventHandle stateRef (moveTo' f) id) def

clearCompleted stateRef e = do
  atomicModifyIORef stateRef $ app0Ref todoClear
  updateTodos stateRef
  updateBindings stateRef

todoClear :: [Todo] -> [Todo]
todoClear = filter (\(_, _, c) -> not c)

todoClear' :: State -> State
todoClear' (State f ts) = State f $ filter (not . status) ts

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

moveTo' :: Text -> State -> State
moveTo' f (State _ ts) = State f ts

create stateRef e = do
  k <- which e
  when (chr k == '\r') $ do
    i <- select "input#new-todo"
    v <- getVal i
    setVal "" i
    atomicModifyIORef stateRef $ app1Ref todoCreate v
    updateTodos stateRef
    updateBindings stateRef

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

app0Ref f (State a ts) = (State a (f ts), ())
app1Ref f p (State a ts) = (State a (f p ts), ())
app2Ref f p q (State a ts) = (State a (f p q ts), ())

todoAllSet :: [Todo] -> [Todo]
todoAllSet = map (\(i, t, _) -> (i, t, True))

todoAllReset :: [Todo] -> [Todo]
todoAllReset = map (\(i, t, _) -> (i, t, False))

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
  deriving Show

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
