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
import JavaScript.JQuery hiding (filter, find, last, not, on)
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
    stateInit f = State f (sort initialTodos) False

-- wildly generic event handler. call eventFn to munge the event in some way,
-- making the result of that available to stateFn, a pure State changer
eventHandle :: IORef State -> (Event -> IO a) -> (a -> State -> State) ->
                Event -> IO ()
eventHandle stateRef eventFn stateFn e = void $ do
  x <- eventFn e
  -- myThing <- select $ "<div>eventhandle: x is " ++ tshow x ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  stateChange stateRef (stateFn x)

stateChange :: IORef State -> (State -> State) -> IO ()
stateChange stateRef f = void $ do
  -- handy for debugging
  -- myThing <- select $ "<div>old is: " ++ tshow old ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  -- myThing <- select $ "<div>new is: " ++ tshow new ++ "</div>"
  -- select "body" >>= appendJQuery myThing
  (old, new) <- atomicModifyIORef stateRef f'
  pageChange old new
  where
    f' o = let n = f o in (n, (o, n))

-- listChange will add or delete <li> elements to reflect the state change;
-- updateBindings 
pageChange :: State -> State -> IO ()
pageChange (State oldf oldts olded) (State newf newts newed) = do
  listChange oldts newts
  updateBindings oldts newts
  when (olded && not newed) $
    void $ select "input#new-todo" >>= setVal ""
  if (oldf /= newf)
    then do
      select (filterSelector oldf) >>= removeClass "selected"
      select (filterSelector newf) >>= addClass "selected"
      let oldshows = map todoId $ filter (todoFilter oldf) newts
      mapM_ reveal $ newshows L.\\ oldshows
      mapM_ hide $ oldshows L.\\ newshows
    else do
      let oldshows = map todoId $ filter (todoFilter oldf) oldts
      mapM_ reveal $ newshows L.\\ oldshows
      mapM_ hide $ oldshows L.\\ newshows
      when (newf == "completed" && length oldts < length newts) $
        hide $ todoId (L.last newts)
  where
    newshows = map todoId $ filter (todoFilter newf) newts

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
reveal n = void $ select (todoIdSelector n) >>= removeClass "hidden"

hide :: TodoId -> IO ()
hide n = void $ select (todoIdSelector n) >>= addClass "hidden"

filterSelector :: Text -> Text
filterSelector = ("a#filter-" ++)

todoSelector :: Todo -> Text
todoSelector = todoIdSelector . todoId

todoIdSelector :: TodoId -> Text
todoIdSelector i = "#todo-list li[n='" ++ tshow i ++ "']"

todoDelete :: Todo -> IO ()
todoDelete t = void $ select (todoSelector t) >>= detach

todoAppend :: Todo -> IO ()
todoAppend item = do
  x <- select $ todoItem item
  void $ select "#todo-list" >>= appendJQuery x

todoInsert :: Todo -> Todo -> IO ()
todoInsert item b = void $ select (todoSelector b) >>= before (todoItem item)

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
  when (ot /= nt) $ void $ setText nt x
  when (oc /= nc) $ do
    -- wot no toggleClass?
    h <- hasClass "completed" x
    if h then do
           void $ removeClass "completed" x >>= 
             J.find "input.toggle" >>= removeProp "checked"
         else do
           void $ addClass "completed" x >>=
             J.find "input.toggle" >>= setProp "checked" "true"
  when (oe /= ne) $ do
    replaceWith (todoItem n) x
    when ne $ void $ select "#todo-list li.editing input" >>= focus
  
initClicks :: IORef State -> IO ()
initClicks stateRef = void $ do
  n <- select "input#new-todo"
  doOn "keyup" "" eventValKey create n
  doOn "focusout" "" eventValEnter create n
  select "button#clear-completed" >>= doClick eventNull todoClear
  select "input#toggle-all" >>= doClick eventChecked toggleAll
  mapM filterClick ["all", "active", "completed"]
  s <- select "ul#todo-list"
  doOn "change" "input.toggle" eventIndex toggle s
  doOn "dblclick" "label" eventIndex editing s
  doOn "click" "button.destroy" eventIndex destroy s
  doOn "focusout" "li.editing" eventIndexTextEnter keyEdit s
  doOn "keyup" "li.editing" eventIndexTextKey keyEdit s
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
    Just t -> s { stateTodos = sort $ f t : L.delete t ts }
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
                      else s { stateTodos = sort $ newt : ts
                             , stateEditing = False }
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
  when (oldAny /= newAny) $
    if newAny then void $ select "footer#footer" >>= removeClass "hidden"
              else void $ select "footer#footer" >>= addClass "hidden"
  when (oldLeft /= newLeft) $
    void $ select "#bind-n-left" >>= setText (tshow newLeft)
  when (oldWord /= newWord) $
    void $ select "#bind-phrase-left" >>= setText newWord
  when (oldDone /= newDone) $
    void $ select "#bind-n-done" >>= setText (tshow newDone)
  when ((oldDone == 0) /= (newDone == 0)) $ 
    void $ select "button#clear-completed" >>=
      setAttr "style" ("display:" ++ if newDone == 0 then "none" else "block")
  when ((oldDone /= 0 && oldLeft == 0) /=
        (newDone /= 0 && newLeft == 0)) $ void $ do
    select "input#toggle-all" >>=
      if newDone /= 0 && newLeft == 0
        then setProp "checked" "true"
        else removeProp "checked"
  where
    newAny = L.length new /= 0
    oldAny = L.length old /= 0
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

instance Ord Todo
  where compare = compare `on` todoId

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
