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
import Reactive.Banana
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
  mapM_ domAppend todos
  f <- filterFromLocation
  newFilter (NewFilter f)
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
  let deleteEs = filterJustMap justDeleteEs e
  reactimate $ fmap todoDelete deleteEs
  let startEditEs = filterJustMap justStartEditEs e
  reactimate $ fmap todoStartEdit startEditEs
  let todoEnterEs = filterJustMap justTodoEnterEs e
  reactimate $ fmap todoEnter todoEnterEs
  reactimate $ fmap todoToggle $ filterJustMap justToggleEs e
  reactimate $ fmap allToggle $ filterJustMap justAllToggleEs e
  reactimate $ fmap newEnter $ filterJustMap justNewEnterEs e
  reactimate $ fmap newAbandon $ filterJustMap justNewAbandonEs e
  reactimate $ fmap newFilter $ filterJustMap justNewFilterEs e

  let todoFns = filterJustMap justTodoFns e
      todosB = accumB init todoFns
  todoC <- changes todosB
  -- can't we use a single function to do all this?
  reactimate' $ fmap (setSpan "bind-n-left" . tshow . todosLeft) <$> todoC
  reactimate' $ fmap (setSpan "bind-phrase-left" . todosLeftPhrase) <$> todoC
  reactimate' $ fmap (setSpan "bind-n-done" . tshow . todosDone) <$> todoC
  reactimate' $ fmap setToggleAll <$> todoC
  reactimate' $ fmap footer <$> todoC
  reactimate' $ fmap clearDoneButton <$> todoC
  reactimate' $ fmap updateTodo <$> todoC
  where
    filterJustMap f = filterJust . fmap f
    setSpan x y = void $ select ("#" ++ x) >>= setText y

todosLeft = L.length . filter (not . todoDone)  -- XXX use a behaviour
todosDone = L.length . filter todoDone
todosLeftPhrase ts = (if todosLeft ts == 1 then "item" else "items") ++ " left"

-- reactive events
data REvent = Delete Int | NewEnter Todo | NewAbandon |
                StartEdit Int | TodoEnter Text Int |
                Toggle Bool Int | ToggleAll Bool | ClearDone |
                NewFilter Text

-- the javascript event handlers: these fire REvents

deleteClick = todoIndexFire Delete
todoDoubleClick = todoIndexFire StartEdit

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
    todoIndexFire (TodoEnter v) fire e

newKeyUp todoRef fire e = do
  k <- which e
  when (k == keyEnter) $ do
    v <- target e >>= selectElement >>= getVal
    when (not $ T.null v) $ do
      t <- extCreate todoRef v
      fire $ NewEnter t
  when (k == keyEscape) $ fire NewAbandon

clearDone fire _ = do
  select (todoListSelector ++ " li.completed") >>= detach
  fire ClearDone

filterClick f fire _ = fire $ NewFilter f

todoCheckbox fire e = do
  x <- target e >>= selectElement
  b <- is ":checked" x
  todoIndexFire (Toggle b) fire e

allCheckbox fire e = do
  r <- target e >>= selectElement >>= is ":checked"
  fire $ ToggleAll r

todoDelete (Delete n) = domIndexDelete n

newEnter (NewEnter t) = do
  f <- filterFromLocation
  domAppendHide t (f == "completed")
  newClear

newAbandon _ = newClear

newClear = void $ select newTodoSelector >>= setVal ""

newFilter (NewFilter f) = do
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

todoStartEdit (StartEdit n) = do
  x <- select (todoIdSelector n)
  t <- J.find "label" x >>= getText
  void $ replaceWith (editItem n t) x

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

todoToggle (Toggle b n) = do
  x <- select $ todoIdSelector n
  setDoneSelection b x
  h <- T.pack . fromJSString <$> windowLocationHash
  let f = if T.null h then "all" else T.drop 1 h
  when (b && f == "active") $ void $ addClass "hidden" x
  when (not b && f == "completed") $ void $ addClass "hidden" x

allToggle (ToggleAll b) = setDoneSelection b =<< select (todoItemsSelector "")
  
todoEnter (TodoEnter t n) = do
  x <- select (todoIdSelector n)
  void $ replaceWith (todoItem $ Todo n t False) x

todoItem (Todo i t c) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .new :c:.completed n=#{i}>
      <input .toggle type=checkbox :c:checked>
      <label>
        #{t}
      <button .destroy>
  |]

-- I'd like to generalize the following event selection functions, but don't
-- know how: a constructor is just a function, but we for a pattern match we
-- need a constant. (Probably there's a GHC extension that can do it.)
justNewEnterEs x@(NewEnter _) = Just x
justNewEnterEs _ = Nothing

justNewAbandonEs NewAbandon = Just NewAbandon
justNewAbandonEs _ = Nothing

justNewFilterEs x@(NewFilter _) = Just x
justNewFilterEs _ = Nothing

justDeleteEs x@(Delete _) = Just x
justDeleteEs _ = Nothing

justStartEditEs x@(StartEdit n) = Just x
justStartEditEs _ = Nothing

justTodoEnterEs x@(TodoEnter _ _) = Just x
justTodoEnterEs _ = Nothing

justToggleEs x@(Toggle _ _) = Just x
justToggleEs _ = Nothing

justAllToggleEs x@(ToggleAll b) = Just x
justAllToggleEs _ = Nothing

justTodoFns :: REvent -> Maybe ([Todo] -> [Todo])
justTodoFns (Toggle b n) = Just set
  where 
    set ts = case find ((n ==) . todoId) ts of
      Nothing -> ts
      Just x -> x { todoDone = b } : L.delete x ts
justTodoFns (ToggleAll b) = Just $ map set
  where set t = t { todoDone = b }
justTodoFns (Delete n) = Just $ del n
  where
    del i ts = case find ((i ==) . todoId) ts of
                Nothing -> ts
                Just t -> L.delete t ts
justTodoFns (TodoEnter t n) = Just ins
  where
    ins ts = case find ((n ==) . todoId) ts of
      Nothing -> ts
      Just x -> x { todoText = t } : L.delete x ts
justTodoFns (NewEnter t) = Just (t :)
justTodoFns ClearDone = Just $ filter (not . todoDone)
justTodoFns _ = Nothing

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
