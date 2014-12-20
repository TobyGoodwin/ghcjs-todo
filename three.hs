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
import JavaScript.JQuery hiding (filter, find, not, on)
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
  stateRef <- newIORef stateNull
  stateChange stateRef (const (stateInit f))
  updateBindings stateRef
  initClicks stateRef
  where
    stateNull = State "" [] False
    stateInit f = State f initialTodos False

-- wildly generic event handler. call eventFn to munge the event in some way,
-- making the result of that available to stateFn, a pure State changer
eventHandle :: IORef State -> (Event -> IO a) -> (a -> State -> State) ->
                Event -> IO ()
eventHandle stateRef eventFn stateFn e = do
  myThing <- select $ "<div>eventhandle</div>"
  select "body" >>= appendJQuery myThing
  x <- eventFn e
  stateChange stateRef (stateFn x)
  return ()

stateChange :: IORef State -> (State -> State) -> IO ()
stateChange stateRef f = do
  (old, new) <- atomicModifyIORef stateRef f'
  myThing <- select $ "<div>old is: " ++ tshow old ++ "</div>"
  select "body" >>= appendJQuery myThing
  myThing <- select $ "<div>new is: " ++ tshow new ++ "</div>"
  select "body" >>= appendJQuery myThing
  pageChange old new
  -- select "#todo-list" >>= J.on checkedit "click" def { hsDescendantFilter = Just "label" }
  return ()
  where
    f' o = let n = f o in (n, (o, n))

checkedit _ = do
  myThing <- select $ "<div>checkedit</div>"
  select "body" >>= appendJQuery myThing
  return ()

checkedit2 () s = s

pageChange :: State -> State -> IO ()
pageChange (State oldf oldts olded) (State newf newts newed) = do
  listChange (prep oldf oldts) (prep newf newts)
  when (olded && not newed) $
    select "input#new-todo" >>= setVal "" >> return ()
  return ()
  where
    prep f = sortBy (compare `on` todoId) . filter (todoFilter f)

-- if we abstracted listChange (perhaps have a listChanger record type that
-- holds the functions for delete, append, etc) we could call it twice, once to
-- change the actual list, and again to manipulate "hidden" class attributes.
-- that would mean even less DOM perturbation
listChange :: [Todo] -> [Todo] -> IO ()
listChange [] [] = return ()
listChange [] (n:ns) = do
  listAppend n
  listChange [] ns
listChange (o:os) [] = do
  listDelete o
  listChange os []
listChange old@(o:os) new@(n:ns) =
  case (compare `on` todoId) o n of
    EQ -> do
      when (o /= n) $ todoChange o n
      listChange os ns
    LT -> do
      listDelete o
      listChange os new
    GT -> do
      listInsert n o
      listChange old ns

listDelete t = do
  -- myThing <- select $ "<div>will delete " ++ tshow (todoId t) ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  select ("#todo-list li[n='" ++ tshow (todoId t) ++ "']") >>= detach

listAppend item = do
  -- myThing <- select $ "<div>will append " ++ tshow item ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  x <- select $ listItem item
  select "#todo-list" >>= appendJQuery x

listInsert item b = do
  -- myThing <- select $ "<div>will insert " ++ tshow item ++ " before " ++ tshow b ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  let x = listItem item
  select ("#todo-list li[n='" ++ tshow (todoId b) ++ "']") >>= before x

listItem (Todo i t c e) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    $if e 
      <li .editing>
        <input .edit value=#{t}>
    $else 
      <li :c:.completed n=#{i}>
        <input .toggle n=#{i} type=checkbox :c:checked>
        <label>
          #{t}
        <button .destroy n=#{i}>
  |]

todoChange (Todo i ot oc oe) n@(Todo _ nt nc ne) = do
  x <- select ("#todo-list li[n='" ++ tshow i ++ "']")
  when (ot /= nt) $ setText nt x >> return ()
  when (oc /= nc) $ do
    -- wot no toggleClass?
    h <- hasClass "completed" x
    if h then do
           removeClass "completed" x >>= 
             J.find "input.toggle" >>= removeProp "checked"
           return ()
         else do
           addClass "completed" x >>=
             J.find "input.toggle" >>= setProp "checked" "true"
           return ()
  when (oe /= ne) $ do
    myThing <- select $ "<div>doing editing change</div>"
    select "body" >>= appendJQuery myThing
    let i = listItem n
    replaceWith i x
    when ne $ select "#todo-list li.editing input" >>= focus >> return ()
  return ()
  
initClicks :: IORef State -> IO ()
initClicks stateRef = do
  select "input#new-todo" >>=
    J.on (eventHandle stateRef eventKey create) "keyup" def
  select "button#clear-completed" >>= doClick eventNull todoClear
  select "input#toggle-all" >>= doClick eventChecked toggleAll
  mapM filterClick ["all", "active", "completed"]
  s <- select "ul#todo-list"
  doOn "click" "button.destroy" eventTodoIndex destroy s
  -- doOn "click" "input.toggle" eventTodoIndex toggle s
  -- doOn "click" "label" eventTodoIndex editing s
  -- J.on (eventHandle stateRef eventTodoIndex editing) "click" def { hsDescendantFilter = Just "label" } s
  -- works: select "ul#todo-list" >>= J.on checkedit "click" def { hsDescendantFilter = Just "label" }
  -- works: J.on checkedit "click" def { hsDescendantFilter = Just "label" } s
  -- works J.on checkedit "click" def { hsDescendantFilter = Just "label" } s
  -- works J.on (eventHandle stateRef eventNull checkedit2) "click" def { hsDescendantFilter = Just "label" } s
  -- works doOn "click" "label" eventNull checkedit2 s
  doOn "click" "label" eventTodoIndex' editing s
  -- select "ul#todo-list button.destroy" >>= doClick eventTodoIndex destroy
  -- select "ul#todo-list input.toggle" >>= doClick eventTodoIndex toggle
  -- select "ul#todo-list label" >>=
  --   dblclick (eventHandle stateRef eventTodoIndex editing) def
  -- s <- select "ul#todo-list"
  doOn "focusout" "li.editing" eventIndexText acceptEdit s
  doOn "keyup" "li.editing" eventKey keyEdit s
  -- J.on (eventHandle stateRef eventIndexText acceptEdit) "focusout" def s
  -- J.on (eventHandle stateRef eventKey keyEdit) "keyup" def s
  return ()
  where
    -- currently handing f straight to the State changer - would it make more
    -- sense to provide an eventFn that can extract it?
    filterClick f = select ("a#filter-" ++ f) >>=
                      doClick eventNull (moveTo f)
    doClick ef sf = click (eventHandle stateRef ef sf) def
    doOn evt desc ef sf =
      J.on (eventHandle stateRef ef sf) evt def { hsDescendantFilter = Just desc }

eventNull _ = return ()
eventTodoIndex e = do
  a <- target e >>= selectElement >>= getAttr "n"
  return $ readMay a
eventTodoIndex' e = do
  a <- target e >>= selectElement >>= parent >>= getAttr "n"
  return $ readMay a
eventChecked e = target e >>= selectElement >>= is ":checked"
eventKey e = do
  k <- which e
  v <- target e >>= selectElement >>= getVal
  return (k, v)
eventIndexText e = do
  x <- target e >>= selectElement
  p <- parent x
  n <- getAttr "n" p
  t <- getText x
  return (readMay n, t)

todoClear :: () -> State -> State
todoClear _ s = s { stateTodos = filter (not . todoStatus) (stateTodos s) }

moveTo :: Text -> () -> State -> State
moveTo f _ s = s { stateFilter = f }

destroy :: Maybe Int -> State -> State
destroy Nothing s = s
destroy (Just n) s =
  case find ((n ==) . todoId) ts of
    Nothing -> s
    Just t -> s { stateTodos = L.delete t ts }
  where ts = stateTodos s

stateTodosChange :: Maybe Int -> (Todo -> Todo) -> State -> State
stateTodosChange Nothing _ s = s
stateTodosChange (Just n) f s =
  case find ((n ==). todoId) ts of
    Nothing -> s
    Just t -> s { stateTodos = f t : L.delete t ts }
  where ts = stateTodos s

toggle :: Maybe Int -> State -> State
toggle Nothing s = s
toggle (Just n) s =
  case find ((n ==). todoId) ts of
    Nothing -> s
    Just t -> s { stateTodos =
                    t { todoStatus = not (todoStatus t) } : L.delete t ts
                }
  where ts = stateTodos s

editing :: Maybe Int -> State -> State
-- editing mi s = s { stateEditing = mi }
editing mi = stateTodosChange mi (\x -> x { todoEditing = True })
-- editing mi s = s { stateTodos = [] }

toggleAll :: Bool -> State -> State
toggleAll x s = s { stateTodos = map setStatus (stateTodos s) }
  where
    setStatus t = t { todoStatus = x }

keyEnter = 13 :: Int
keyEscape = 27 :: Int

create :: (Int, Text) -> State -> State
create (k, todo) s
  | k == keyEnter = s { stateTodos = newt : ts, stateEditing = False }
  | k == keyEscape = s { stateEditing = False }
  | otherwise = s { stateEditing = True }
  where
    ts = stateTodos s
    m = fromMaybe 0 (maximumMay $ map todoId ts)
    newt = Todo (m+1) todo False False

keyEdit :: (Int, Text) -> State -> State
keyEdit (k, todo) s
  | k == keyEnter = s { stateTodos = newt : ts }
  | otherwise = s
  where
    ts = stateTodos s
    m = fromMaybe 0 (maximumMay $ map todoId ts)
    newt = Todo (m+1) todo False False

{-
acceptEdit stateRef n e = do
  x <- target e >>= selectElement
  v <- getVal x
  atomicModifyIORef stateRef $ app2Ref todoUpdate n v
  updateTodos stateRef
-}

{-
acceptEdit :: (Int, Text) -> State -> State
acceptEdit (i, t) s =
  case find ((i ==) . todoId) ts of
    Nothing -> create (keyEnter, t) -- cannot happen?
    Just todo -> s { stateTodos = todo { todoText = t } : L.delete t (stateTodos s) }
-}
acceptEdit :: (Maybe Int, Text) -> State -> State
acceptEdit (Nothing, _) s = s
acceptEdit (Just i, t) s =
  case find ((i ==) . todoId) ts of
    Nothing -> create (keyEnter, t) s -- cannot happen?
    Just todo -> s { stateTodos = todo { todoText = t } : L.delete todo ts }
  where ts = stateTodos s

updateTodos :: IORef State -> IO ()
updateTodos stateRef = do
  myThing <- select $ "<div>updateTodos called!</div>"
  select "body" >>= appendJQuery myThing
  return ()
  {-
  l <- todoList stateRef
  s <- select "#todo-list"
  replaceWith l s
  select "button.destroy" >>= click (destroy stateRef) def
  select "input.toggle" >>= click (toggle stateRef) def
  select "#todo-list label" >>= click (beginEdit stateRef) def
  return () -}

{-
beginEdit stateRef e = do
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

eventIndexText e = do
  x <- target e >>= selectElement
  p <- parent x
  n <- getAttr "n" p
  t <- getText x
  return (readMay n, t)

acceptEdit stateRef n e = do
  x <- target e >>= selectElement
  v <- getVal x
  atomicModifyIORef stateRef $ app2Ref todoUpdate n v
  updateTodos stateRef

app0Ref f (State a ts) = (State a (f ts), ())
app1Ref f p (State a ts) = (State a (f p ts), ())
app2Ref f p q (State a ts) = (State a (f p q ts), ())
-}
{-
todoAllSet :: [Todo] -> [Todo]
todoAllSet = map (\(i, t, _) -> (i, t, True))

todoAllReset :: [Todo] -> [Todo]
todoAllReset = map (\(i, t, _) -> (i, t, False))
-}
{-
todoCreate :: Text -> [Todo] -> [Todo]
todoCreate t ts =
  let n = fromMaybe 0 $ maximumMay $ map todoId ts
  in Todo (n+1) t False : ts

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
  case find (\x -> todoId x == n) ts of
    Nothing -> todoCreate t ts
    Just todo -> todo { todoText = t } : L.delete todo ts
-}

updateBindings stateRef = do
  State f ts _ <- readIORef stateRef
  -- handy for debugging
  -- myThing <- select $ "<div>" ++ tshow f ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  let nDone = L.length $ L.filter todoStatus ts
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

type TodoId = Int
data Todo = Todo { todoId :: TodoId
                 , todoText :: Text
                 , todoStatus :: Bool
                 , todoEditing :: Bool
                 } deriving (Eq, Show)

type FilterName = Text
type Filter = Todo -> Bool
data State = State { stateFilter :: FilterName
                   , stateTodos :: [Todo]
                   , stateEditing :: Bool
                   }
  deriving Show

initialTodos :: [Todo]
initialTodos =
  [ Todo 3 "Steal underpants" True False
  , Todo 14 "???" False False
  , Todo 16 "Profit!" False False
  ]

todoFilter :: FilterName -> Filter
todoFilter "active" = not . todoStatus
todoFilter "completed" = todoStatus
todoFilter _ = const True
