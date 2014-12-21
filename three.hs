{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude

import Data.Default
import qualified Data.List as L hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHCJS.Prim (fromJSString)
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
  initClicks stateRef
  where
    stateNull = State "" [] False
    stateInit f = State f initialTodos False

-- wildly generic event handler. call eventFn to munge the event in some way,
-- making the result of that available to stateFn, a pure State changer
eventHandle :: IORef State -> (Event -> IO a) -> (a -> State -> State) ->
                Event -> IO ()
eventHandle stateRef eventFn stateFn e = do
  x <- eventFn e
  -- myThing <- select $ "<div>eventhandle: x is " ++ tshow x ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  stateChange stateRef (stateFn x)
  return ()

stateChange :: IORef State -> (State -> State) -> IO ()
stateChange stateRef f = do
  (old, new) <- atomicModifyIORef stateRef f'
  -- handy for debugging
  -- myThing <- select $ "<div>old is: " ++ tshow old ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  -- myThing <- select $ "<div>new is: " ++ tshow new ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  pageChange old new
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
  when (oldf /= newf) $ do
    select ("a#filter-" ++ oldf) >>= removeClass "selected"
    select ("a#filter-" ++ newf) >>= addClass "selected"
    return ()
  updateBindings oldts newts
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

todoSelector :: Todo -> Text
todoSelector t = "#todo-list li[n='" ++ tshow (todoId t) ++ "']"

listDelete t = select (todoSelector t) >>= detach

listAppend item = do
  x <- select $ listItem item
  select "#todo-list" >>= appendJQuery x

listInsert item b = select (todoSelector b) >>= before (listItem item)

listItem (Todo i t c e) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    $if e 
      <li .editing n=#{i}>
        <input .edit value=#{t}>
    $else 
      <li :c:.completed n=#{i}>
        <input .toggle type=checkbox :c:checked>
        <label>
          #{t}
        <button .destroy>
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
    let it = listItem n
    replaceWith it x
    when ne $ select "#todo-list li.editing input" >>= focus >> return ()
  return ()
  
initClicks :: IORef State -> IO ()
initClicks stateRef = do
  n <- select "input#new-todo"
  doOn "keyup" "" eventValKey create n
  doOn "focusout" "" eventValEnter create n
  select "button#clear-completed" >>= doClick eventNull todoClear
  select "input#toggle-all" >>= doClick eventChecked toggleAll
  mapM filterClick ["all", "active", "completed"]
  s <- select "ul#todo-list"
  doOn "change" "input.toggle" eventIndex toggle s
  doOn "click" "label" eventIndex editing s
  doOn "click" "button.destroy" eventIndex destroy s
  doOn "focusout" "li.editing" eventIndexTextEnter keyEdit s
  doOn "keyup" "li.editing" eventIndexTextKey keyEdit s
  return ()
  where
    -- currently handing f straight to the State changer - would it make more
    -- sense to provide an eventFn that can extract it?
    filterClick f = select ("a#filter-" ++ f) >>=
                      doClick eventNull (moveTo f)
    doClick ef sf = click (eventHandle stateRef ef sf) def
    doOn evt desc ef sf =
      J.on (eventHandle stateRef ef sf) evt
        def { hsDescendantFilter = Just desc }

eventNull _ = return ()
eventIndex e = do
  a <- target e >>= selectElement >>= parent >>= getAttr "n"
  return $ readMay a
eventChecked e = target e >>= selectElement >>= is ":checked"
eventVal e = target e >>= selectElement >>= getVal
eventValKey e = do
  v <- eventVal e
  k <- which e
  return (v, k)
eventValEnter e = do
  v <- eventVal e
  return (v, keyEnter)
eventIndexTextKey e = do
  i <- eventIndex e
  (v, k) <- eventValKey e
  return (i, v, k)
-- moving focus out of editing box is equivalent to hitting enter
eventIndexTextEnter e = do
  i <- eventIndex e
  v <- target e >>= selectElement >>= getVal
  return (i, v, keyEnter)

todoClear :: () -> State -> State
todoClear _ s = s { stateTodos = filter (not . todoStatus) (stateTodos s) }

moveTo :: Text -> () -> State -> State
moveTo f _ s = s { stateFilter = f }

toggle :: Maybe Int -> State -> State
toggle = stateTodosChange tog
  where tog t = t { todoStatus = not (todoStatus t) }

destroy :: Maybe Int -> State -> State
destroy Nothing s = s
destroy (Just n) s =
  case find ((n ==) . todoId) ts of
    Nothing -> s
    Just t -> s { stateTodos = L.delete t ts }
  where ts = stateTodos s

stateTodosChange :: (Todo -> Todo) -> Maybe Int -> State -> State
stateTodosChange _ Nothing s = s
stateTodosChange f (Just n) s =
  case find ((n ==). todoId) ts of
    Nothing -> s
    Just t -> s { stateTodos = f t : L.delete t ts }
  where ts = stateTodos s

editing :: Maybe Int -> State -> State
editing = stateTodosChange (\x -> x { todoEditing = True })

toggleAll :: Bool -> State -> State
toggleAll x s = s { stateTodos = map setStatus (stateTodos s) }
  where
    setStatus t = t { todoStatus = x }

keyEnter = 13 :: Int
keyEscape = 27 :: Int

create :: (Text, Int) -> State -> State
create (todo, k) s
  | k == keyEnter = if T.null todo then abandon
                      else s { stateTodos = newt : ts, stateEditing = False }
  | k == keyEscape = abandon
  | otherwise = s { stateEditing = True }
  where
    abandon = s { stateEditing = False }
    ts = stateTodos s
    m = fromMaybe 0 (maximumMay $ map todoId ts)
    newt = Todo (m+1) todo False False

keyEdit :: (Maybe TodoId, Text, Int) -> State -> State
keyEdit (mi, todo, k) s
  | k == keyEnter = stateTodosChange fin mi s
  | k == keyEscape = stateTodosChange abndn mi s
  | otherwise = s
  where
    fin t = t { todoText = todo, todoEditing = False }
    abndn t = t { todoEditing = False }

updateBindings :: [Todo] -> [Todo] -> IO ()
updateBindings old new = do
  when (oldLeft /= newLeft) $
    select "#bind-n-left" >>= setText (tshow newLeft) >> return ()
  when (oldWord /= newWord) $
    select "#bind-phrase-left" >>= setText newWord >> return ()
  when (oldDone /= newDone) $
    select "#bind-n-done" >>= setText (tshow newDone) >> return ()
  when ((oldDone == 0) /= (newDone == 0)) $ 
    select "button#clear-completed" >>=
      setAttr "style" ("display:" ++
        if newDone == 0 then "none" else "block") >> return ()
  when ((oldDone /= 0 && oldLeft == 0) /= (newDone /= 0 && newLeft == 0)) $ do
    select "input#toggle-all" >>=
      if newDone /= 0 && newLeft == 0
        then setProp "checked" "true"
        else removeProp "checked"
    return ()
  where
    newDone = L.length $ L.filter todoStatus new
    oldDone = L.length $ L.filter todoStatus old
    newLeft = L.length new - newDone
    oldLeft = L.length old - oldDone
    newWord = (if newLeft == 1 then "item" else "items") ++ " left"
    oldWord = (if oldLeft == 1 then "item" else "items") ++ " left"

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
                   } deriving Show

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
