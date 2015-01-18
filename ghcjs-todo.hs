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
import GHCJS.Prim (fromJSString, toJSString)
import GHCJS.Types (JSString)
import JavaScript.JQuery hiding (Event, filter, find, last, not, on)
import qualified JavaScript.JQuery as J
import Reactive.Banana as RB
import Reactive.Banana.Frameworks
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

foreign import javascript unsafe "$r = window.location.hash;"
  windowLocationHash :: IO JSString
foreign import javascript safe   "window.location.hash = $1;"
  setWindowLocationHash :: JSString -> IO ()

main :: IO ()
main = do
  todoRef <- extInit
  todos <- extFetch todoRef
  domListUpdate todos
  mapM_ domAppend todos
  f <- filterFromLocation
  newFilter (Filter f)
  (addHandler, fire) <- newAddHandler
  compile (makeNetworkDescription todos addHandler) >>= actuate
  initJEvents fire todoRef

filters :: [Text]
filters = ["all", "active", "completed"]

filterFromLocation :: IO Text
filterFromLocation = do
  h <- T.pack . fromJSString <$> windowLocationHash
  return $ if T.null h then "all" else T.drop 1 h

-- set up DOM / JavaScript events
initJEvents fire todoRef = do
  s <- select todoListSelector
  descOn deleteClick "click" s "button.destroy"
  descOn todoDoubleClick "dblclick" s "label"
  descOn todoKeyUp "keyup" s "li.editing"
  descOn todoCheckbox "change" s "input.toggle"
  plainOn allCheckbox "click" "input#toggle-all"
  plainOn (newKeyUp todoRef) "keyup" "input#new-todo"
  plainOn (clearDone) "click" "button#clear-completed"
  mapM filterOn filters
  newClear
  where
    plainOn fn ev x = void $ select x >>= J.on (fn fire) ev def
    descOn fn ev x desc =
      J.on (fn fire) ev def { hsDescendantFilter = Just desc } x
    filterOn f = plainOn (filterClick f) "click" (filterSelector f)

-- the banana network
makeNetworkDescription init h = do
  e <- fromAddHandler h
  -- reactimate $ fmap domUpdate e

  let todoFns = filterJust $ fmap justTodoFns e
      todosB = accumB init todoFns
  todoC <- changes todosB
  reactimate' $ fmap domListUpdate <$> todoC

  -- build a behaviour that is a function pairing the todos list with its
  -- argument, then apply that to the stream of events: result is a stream of
  -- events with type ([Todo], REvent). 
  let k = fmap (,) todosB
      l = apply k e
  reactimate $ fmap checkIt l
  reactimate $ fmap update2 l
  return ()
  where
    checkIt x = putStrLn $ "checkit: " ++ tshow x

update2 (ts, AllToggle b) =
  void $ select (todoItemsSelector "") >>= setDoneSelection b
update2 (ts, NewEnter t) = do
  f <- filterFromLocation
  domAppendHide t (f == "completed")
  newClear
update2 (_, NewAbandon) = newClear
update2 (ts, Toggle b n) = do
  x <- select $ todoIdSelector n
  setDoneSelection b x
  h <- T.pack . fromJSString <$> windowLocationHash
  let f = if T.null h then "all" else T.drop 1 h
  when (b && f == "active") $ void $ addClass "hidden" x
  when (not b && f == "completed") $ void $ addClass "hidden" x
update2 (ts, Edit n) = do
  x <- select (todoIdSelector n)
  t <- J.find "label" x >>= getText
  void $ replaceWith (editItem n t) x
update2 (ts, Enter t n) = do
  x <- select (todoIdSelector n)
  case find ((n ==) . todoId) ts of
    Just o -> void $ replaceWith (todoItem $ o { todoText = t }) x
    Nothing -> return ()
update2 (ts, Delete n) = domIndexDelete n
update2 (ts, Filter f) = do
  setFilter f
  case f of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""
update2 (ts, DoneClear) = void $ select (todoItemsSelector ".completed") >>= detach


-- reactive events
data REvent = AllToggle Bool | NewEnter Todo | NewAbandon |
                Toggle Bool Int | Edit Int | Enter Text Int | Delete Int |
                Filter Text | DoneClear deriving Show

-- the javascript event handlers: these fire REvents

deleteClick = todoIndexFire Delete
todoDoubleClick = todoIndexFire Edit

todoIndexFire con fire e = do
  i <- readMay <$> (target e >>= selectElement >>= parent >>= getAttr "n")
  case i of
    Just j -> fire $ con j
    Nothing -> return ()

keyEnter = 13 :: Int
keyEscape = 27 :: Int

todoKeyUp fire e = do
  k <- which e
  when (k == keyEnter) $ do
    v <- target e >>= selectElement >>= getVal
    todoIndexFire (Enter v) fire e

newKeyUp todoRef fire e = do
  k <- which e
  when (k == keyEnter) $ do
    v <- target e >>= selectElement >>= getVal
    when (not $ T.null v) $ do
      t <- extCreate todoRef v
      fire $ NewEnter t
  when (k == keyEscape) $ fire NewAbandon

clearDone fire _ = fire DoneClear

filterClick f fire _ = fire $ Filter f

todoCheckbox fire e = do
  x <- target e >>= selectElement
  b <- is ":checked" x
  todoIndexFire (Toggle b) fire e

allCheckbox fire e =
  target e >>= selectElement >>= is ":checked" >>= fire . AllToggle

-- the reactive output functions

newClear = void $ select newTodoSelector >>= setVal ""

domUpdate (AllToggle b) =
  void $ select (todoItemsSelector "") >>= setDoneSelection b
domUpdate (NewEnter t) = do
  f <- filterFromLocation
  domAppendHide t (f == "completed")
  newClear
domUpdate NewAbandon = newClear
domUpdate (Toggle b n) = do
  x <- select $ todoIdSelector n
  setDoneSelection b x
  h <- T.pack . fromJSString <$> windowLocationHash
  let f = if T.null h then "all" else T.drop 1 h
  when (b && f == "active") $ void $ addClass "hidden" x
  when (not b && f == "completed") $ void $ addClass "hidden" x
domUpdate (Edit n) = do
  x <- select (todoIdSelector n)
  t <- J.find "label" x >>= getText
  void $ replaceWith (editItem n t) x
domUpdate (Enter t n) = do
  x <- select (todoIdSelector n)
  -- XXX grab the Done status from existing
  void $ replaceWith (todoItem $ Todo n t False) x
domUpdate (Delete n) = domIndexDelete n
domUpdate (Filter f) = do
  setFilter f
  case f of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""
domUpdate DoneClear = void $ select (todoItemsSelector ".completed") >>= detach

domListUpdate ts = do
  setSpan "bind-n-left" $ tshow todosLeft
  setSpan "bind-phrase-left" todosLeftPhrase
  setSpan "bind-n-done" $ tshow todosDone
  setToggleAll ts
  footer ts
  clearDoneButton ts
  -- updateTodo ts
  where
    setSpan x y = void $ select ("#" ++ x) >>= setText y
    todosLeft = L.length $ filter (not . todoDone) ts
    todosDone = L.length $ filter todoDone ts
    todosLeftPhrase =
      (if todosLeft == 1 then "item" else "items") ++ " left"

newFilter (Filter f) = do
  setFilter f
  case f of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""

setFilter f = do
  select (filterSelector f) >>= addClass "selected"
  mapM_ deselect (L.delete f filters)
  where
    deselect g = void $ select (filterSelector g) >>= removeClass "selected"

todoItem (Todo i t c) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .new :c:.completed n=#{i}>
      <input .toggle type=checkbox :c:checked>
      <label>
        #{t}
      <button .destroy>
  |]

editItem i t =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .editing n=#{i}>
      <input .edit value=#{t}>
  |]

setDoneSelection b x = do
  if b then void $ addClass "completed" x >>=
                    J.find "input.toggle" >>= setProp "checked" "true"
       else void $ removeClass "completed" x >>= 
               J.find "input.toggle" >>= removeProp "checked"

justTodoFns :: REvent -> Maybe ([Todo] -> [Todo])
justTodoFns (Toggle b n) = Just $ todoIndexHelper set n
  where 
    set x ts = x { todoDone = b } : L.delete x ts
justTodoFns (AllToggle b) = Just $ map set
  where set t = t { todoDone = b }
justTodoFns (Delete n) = Just $ todoIndexHelper del n
  where
    del x ts = L.delete x ts
justTodoFns (Enter t n) = Just $ todoIndexHelper ins n
  where
    ins x ts = x { todoText = t } : L.delete x ts
justTodoFns (NewEnter t) = Just (t :)
justTodoFns DoneClear = Just $ filter (not . todoDone)
justTodoFns _ = Nothing

todoIndexHelper f n ts = 
  case find ((n ==) . todoId) ts of
    Nothing -> ts
    Just x -> f x ts

updateTodo :: [Todo] -> IO ()
updateTodo ts = do
  putStrLn $ "in updateTodo: " ++ tshow ts
  return ()

setToggleAll :: [Todo] -> IO ()
setToggleAll ts = void $ select "input#toggle-all" >>=
                      if allDone then setProp "checked" "true"
                        else removeProp "checked"
  where allDone = not (L.null ts) && L.null (L.filter (not . todoDone) ts)
  
footer :: [Todo] -> IO ()
footer ts = hideIf noTodos footerSelector
  where noTodos = L.null ts

clearDoneButton :: [Todo] -> IO ()
clearDoneButton ts = hideIf noneDone buttonClearSelector
  where noneDone = L.null $ L.filter todoDone ts

reveal :: Text -> IO ()
reveal j = void $ select j >>= removeClass "hidden"

hide :: Text -> IO ()
hide j = void $ select j >>= addClass "hidden"

hideIf c j = (if c then hide else reveal) j

filterSelector :: Text -> Text
filterSelector = ("a#filter-" ++)

todoIdSelector :: TodoId -> Text
todoIdSelector i = todoItemsSelector $ "[n='" ++ tshow i ++ "']"

todoListSelector = "ul#todo-list" :: Text
todoItemsSelector x = todoListSelector ++ " li" ++ x
buttonClearSelector = "button#clear-completed" :: Text
footerSelector = "footer#footer" :: Text
newTodoSelector = "input#new-todo" :: Text

domIndexDelete :: Int -> IO ()
domIndexDelete n = void $ select (todoIdSelector n) >>= detach

domAppend :: Todo -> IO ()
domAppend item = do
  x <- select $ todoItem item
  void $ select todoListSelector >>= appendJQuery x

domAppendHide :: Todo -> Bool -> IO ()
domAppendHide item hide = do
  x <- select $ todoItem item
  t <- select todoListSelector >>= appendJQuery x
  when hide $ void $ J.find "li:last" t >>= addClass "hidden"

type TodoId = Int
data Todo = Todo { todoId :: TodoId
                 , todoText :: Text
                 , todoDone :: Bool
                 } deriving (Eq, Show)

instance Ord Todo
  where compare = compare `on` todoId

extInit = newIORef $ sort initialTodos
  where
    initialTodos =
      [ Todo 3 "Steal underpants" True
      , Todo 14 "???" False
      , Todo 16 "Profit!" False
      ]

extFetch todoRef = readIORef todoRef

extCreate ref v = atomicModifyIORef ref create
  where
    create ts =
      let m = fromMaybe 0 (maximumMay $ map todoId ts)
          newt = Todo (m+1) v False
      in (newt : ts, newt)
