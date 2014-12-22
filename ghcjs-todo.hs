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
  -- handy for debugging
  -- myThing <- select $ "<div>old is: " ++ tshow old ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  -- myThing <- select $ "<div>new is: " ++ tshow new ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  (old, new) <- atomicModifyIORef stateRef f'
  pageChange old new
  return ()
  where
    f' o = let n = f o in (n, (o, n))

pageChange :: State -> State -> IO ()
pageChange (State oldf oldts olded) (State newf newts newed) = do
  listChange olds news
  mapM_ reveal $ nshows L.\\ oshows
  mapM_ hide $ oshows L.\\ nshows
  when (olded && not newed) $
    select "input#new-todo" >>= setVal "" >> return ()
  when (oldf /= newf) $ do
    select (filterSelector oldf) >>= removeClass "selected"
    select (filterSelector newf) >>= addClass "selected"
    return ()
  updateBindings oldts newts
  where
    prep = sortBy (compare `on` todoId)
    olds = prep oldts; news = prep newts
    oshows = map todoId $ filter (todoFilter oldf) olds
    nshows = map todoId $ filter (todoFilter newf) news

listChange :: [Todo] -> [Todo] -> IO ()
listChange [] [] = return ()
listChange [] (n:ns) = todoAppend n >> listChange [] ns
listChange (o:os) [] = todoDelete o >> listChange os []
listChange old@(o:os) new@(n:ns) =
  case (compare `on` todoId) o n of
    EQ -> (when (o /= n) $ todoChange o n) >> listChange os ns
    LT -> todoDelete o >> listChange os new
    GT -> todoInsert n o >> listChange old ns

reveal :: TodoId -> IO ()
reveal n = select (todoIdSelector n) >>= removeClass "hidden" >> return ()

hide :: TodoId -> IO ()
hide n = select (todoIdSelector n) >>= addClass "hidden" >> return ()

filterSelector :: Text -> Text
filterSelector = ("a#filter-" ++)

todoSelector :: Todo -> Text
todoSelector = todoIdSelector . todoId

todoIdSelector :: TodoId -> Text
todoIdSelector i = "#todo-list li[n='" ++ tshow i ++ "']"

todoDelete t = select (todoSelector t) >>= detach

todoAppend item = do
  myThing <- select $ "<div>append: " ++ tshow item ++ "</div>"
  select "body" >>= appendJQuery myThing
  x <- select $ todoItem item
  select "#todo-list" >>= appendJQuery x

todoInsert item b = select (todoSelector b) >>= before (todoItem item)

todoItem (Todo i t c e) =
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

todoChange o@(Todo i ot oc oe) n@(Todo _ nt nc ne) = do
  x <- select (todoSelector o)
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
    replaceWith (todoItem n) x
    when ne $ select "#todo-list li.editing input" >>= focus >> return ()
  
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
    filterClick f = select (filterSelector f) >>= doClick eventNull (moveTo f)
    doClick ef sf = click (eventHandle stateRef ef sf) def
    doOn evt desc ef sf =
      J.on (eventHandle stateRef ef sf) evt
        def { hsDescendantFilter = Just desc }

eventNull _ = return ()
eventIndex e = readMay <$>
  (target e >>= selectElement >>= parent >>= getAttr "n")
eventChecked e = target e >>= selectElement >>= is ":checked"
eventVal e = target e >>= selectElement >>= getVal
eventValKey e = (,) <$> eventVal e <*> which e
-- moving focus out of editing box is equivalent to hitting enter
eventValEnter e = (,) <$> eventVal e <*> pure keyEnter
eventIndexTextKey e = (,,) <$> eventIndex e <*> eventVal e <*> which e
eventIndexTextEnter e = (,,) <$> eventIndex e <*> eventVal e <*> pure keyEnter

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

-- this seems repetitive
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
