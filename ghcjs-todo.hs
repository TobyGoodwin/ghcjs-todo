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
  bindings todos
  mapM_ domAppend todos
  f <- filterFromLocation
  newFilter (Filter f)
  (addHandler, fire) <- newAddHandler
  compile (makeNetworkDescription todos f addHandler) >>= actuate
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
makeNetworkDescription initTodos initFilter h = do
  e <- fromAddHandler h

  -- behaviour for the Todo list
  let todoFns = filterJust $ fmap justTodoFns e
      todosB = accumB initTodos todoFns

  -- behaviour for the filter
  let filterB = stepper initFilter $ filterJust $ fmap justFilterEs e

  -- combine the Todo list and filter behaviours with the event stream
  let tevents = TEvent <$> todosB <*> filterB <@> e
  reactimate $ fmap update tevents

  -- monitor changes to the Todo list behaviour; update bindings
  todosC <- changes todosB
  reactimate' $ fmap bindings <$> todosC

update (TEvent ts f (AllToggle b)) = do
  let us = filter ((b /= ) . todoDone) ts
  mapM_ toggle us
  where
    toggle t = do
      x <- select $ todoSelector t
      setDoneSelection b x
      hideOrReveal f (not b) b x

update (TEvent _ f (NewEnter t)) = do
  domAppendHide t (f == "completed")
  newClear

update (TEvent _ _ NewAbandon) = newClear

update (TEvent ts f (Toggle b n)) = do
  x <- select $ todoIdSelector n
  setDoneSelection b x
  hideOrReveal f (not b) b x

update (TEvent ts _ (Edit i)) =
  case find ((i ==) . todoId) ts of
    Just j -> do
      x <- select (todoIdSelector i)
      void $ replaceWith (editItem i (todoText j)) x
    Nothing -> return ()

update (TEvent ts _ (Enter t i)) = do
  x <- select (todoIdSelector i)
  case find ((i ==) . todoId) ts of
    Just j -> void $ replaceWith (todoItem $ j { todoText = t }) x
    Nothing -> return ()

update (TEvent ts _ (Delete n)) = domIndexDelete n

update (TEvent ts oldf (Filter newf)) = do
  setFilter newf
  case newf of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""

update (TEvent ts _ DoneClear) =
  void $ select (todoItemsSelector ".completed") >>= detach


-- reactive events
data REvent = AllToggle Bool | NewEnter Todo | NewAbandon |
                Toggle Bool Int | Edit Int | Enter Text Int | Delete Int |
                Filter Text | DoneClear deriving Show

-- triple consisting of a Todo list, the current filter, and an REvent
data TEvent = TEvent [Todo] Text REvent deriving Show

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

bindings ts = do
  setSpan "bind-n-left" $ tshow todosLeft
  setSpan "bind-phrase-left" todosLeftPhrase
  setSpan "bind-n-done" $ tshow todosDone
  setToggleAll ts
  hideIf (todos == 0) footerSelector
  hideIf (todosDone == 0) buttonClearSelector
  hideIf (todos == 0) toggleAllSelector
  clearDoneButton ts
  where
    setSpan x y = void $ select ("#" ++ x) >>= setText y
    todos = L.length ts
    todosDone = L.length $ filter todoDone ts
    todosLeft = todos - todosDone
    todosLeftPhrase = (if todosLeft == 1 then "item" else "items") ++ " left"
    setToggleAll ts = void $ select "input#toggle-all" >>=
                        if allDone then setProp "checked" "true"
                          else removeProp "checked"
    allDone = todos /= 0 && todosLeft == 0

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

justFilterEs (Filter x) = Just x
justFilterEs _ = Nothing

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
reveal t = select t >>= jReveal

jReveal :: JQuery -> IO ()
jReveal = void . removeClass "hidden"

hide :: Text -> IO ()
hide t = select t >>= jHide

jHide :: JQuery -> IO ()
jHide = void . addClass "hidden"

hideIf c j = (if c then hide else reveal) j

filterSelector :: Text -> Text
filterSelector = ("a#filter-" ++)

todoSelector :: Todo -> Text
todoSelector = todoIdSelector . todoId

todoIdSelector :: TodoId -> Text
todoIdSelector i = todoItemsSelector $ "[n='" ++ tshow i ++ "']"

todoListSelector = "ul#todo-list" :: Text
todoItemsSelector x = todoListSelector ++ " li" ++ x

toggleAllSelector = "input#toggle-all" :: Text
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

-- given a filter, a previous state of the "done" flag, the current state
-- of the flag, and a selection, hide or reveal the selection as necessary
hideOrReveal f old new j = do
  when (filterHelper f old /= filterHelper f new) $
      if filterHelper f new then jReveal j else jHide j
  where
    filterHelper "all" = const True
    filterHelper "active" = not
    filterHelper "completed" = id
