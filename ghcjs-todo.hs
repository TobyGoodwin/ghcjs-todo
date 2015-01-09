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
  todos <- sort <$> initialTodos
  let stateInit = State f todos False
  stateRef <- newIORef stateInit
  updateBindings stateNull stateInit bindings
  updateTweaks stateNull stateInit tweaksBool
  mapM_ domAppend todos
  initClicks stateRef
  where
    stateNull = State "" [] False

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
-- updateBindings deals with the counts in the footer;
-- we then need to add and remove "hidden" classes according to the current
-- filter - this leaves an awkward corner case, since if we add a new todo when
-- the filter is "completed", we need it to vanish immediately. 
-- (*) personally, i think a better interface is instead to mark it as done,
-- and leave it displayed (which also avoids the special code here), but that's
-- not what the other implementations of the todo app do
pageChange :: State -> State -> IO ()
pageChange os@(State oldf oldts olded) ns@(State newf newts newed) = do
  when (olded && not newed) $
    void $ select newTodoSelector >>= setVal ""
  updateBindings os ns bindings
  updateTweaks os ns tweaksBool
  listChange oldts newts
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
      -- (*) this is the corner case: remove the next statement and instate the
      -- one in create if you prefer my "already done" solution
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

buttonClearSelector = "button#clear-completed" :: Text
footerSelector = "footer#footer" :: Text
newTodoSelector = "input#new-todo" :: Text

todoDelete :: Todo -> IO ()
todoDelete t = do
  void $ select (todoSelector t) >>= detach

todoAppend :: Todo -> IO ()
todoAppend item = do
  domAppend item

domAppend :: Todo -> IO ()
domAppend item = do
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
  n <- select newTodoSelector
  setVal "" n
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
todoClear _ s = s { stateTodos = filter (not . todoDone) (stateTodos s) }

moveTo :: Text -> () -> State -> State
moveTo f _ s = s { stateFilter = f }

toggle :: Maybe Int -> State -> State
toggle = stateTodosChange tog
  where tog t = t { todoDone = not (todoDone t) }

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
    setStatus t = t { todoDone = x }

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
    -- (*) this is the line you need if you think that new todos should be
    -- marked done if entered on the "completed" screen
    -- newt = Todo (m+1) todo (stateFilter s == "completed") False
    newt = Todo (m+1) todo False False

keyEdit :: (Maybe TodoId, Text, Int) -> State -> State
keyEdit (mi, todo, k) s
  | k == keyEnter = stateTodosChange fin mi s
  | k == keyEscape = stateTodosChange abndn mi s
  | otherwise = s
  where
    fin t = t { todoText = todo, todoEditing = False }
    abndn t = t { todoEditing = False }

-- |A 'Binding' represents some text in the application which depends on the
-- 'State'. In the DOM, there needs to be a '<span>' element which will be
-- updated when the 'State' changes. (For a more complicated app, it might be
-- worth parameterizing 'Binding' on a type, much as 'Tweak' is.)
data Binding =
  Binding { bindingId :: Text -- ^ The 'id' of the '<span>' element
          , bindingFn :: State -> Text -- ^ Function to produce the bound value
          }

-- |'updateBindings' takes an old and a new 'State' and a list of 'Binding's.
-- For each 'Binding', if the result of applying the 'bindingFn' to old and new 
-- differs, the DOM is updated with the new value.
updateBindings :: State -> State -> [Binding] -> IO ()
updateBindings old new = mapM_ binder
  where
    binder (Binding i f) =
      let ov = f old; nv = f new
      in when (ov /= nv) $ void $ select ("#" ++ i) >>= setText nv

bindings =
  [ Binding "bind-n-done" showDone
  , Binding "bind-n-left" showLeft
  , Binding "bind-phrase-left" phraseLeft
  ]
  where
    showDone = tshow . done
    showLeft = tshow . left
    phraseLeft s = (if left s == 1 then "item" else "items") ++ " left"
    left s = total s - done s
    total = L.length . stateTodos
    done = L.length . L.filter todoDone . stateTodos

-- |A 'Tweak' is more general than a 'Binding', with an arbitrary 'tweakGuard'
-- function, and an arbitrary action.
data Tweak a =
  Tweak { tweakGuard :: State -> a -- ^Function of 'State' to see change
        , tweakAction :: State -> IO () -- ^Action to apply
        }

tweaksBool = 
  [ Tweak noTodos footer
  , Tweak noneDone buttonDone
  , Tweak allDone checkboxDone
  ]
  where
    noTodos = L.null . stateTodos
    footer s = void $ select footerSelector >>= hideIf (noTodos s)
    noneDone = L.null  . L.filter todoDone . stateTodos
    buttonDone s = void $ select buttonClearSelector >>=
                          hideIf (noneDone s)
    allDone s = not (noTodos s) &&
                  L.null (L.filter (not . todoDone) (stateTodos s))
    checkboxDone s = void $ select "input#toggle-all" >>=
                      if allDone s then setProp "checked" "true"
                        else removeProp "checked"
    hideIf c = (if c then addClass else removeClass) "hidden"

-- |'updateTweaks' takes an old and a new 'State' and a list of 'Tweak's.
-- For each 'Tweak', if the result of applying the 'tweakGuard' function to old
-- and new differs, the 'tweakAction' is performed.
updateTweaks :: Eq a => State -> State -> [Tweak a] -> IO ()
updateTweaks old new = mapM_ tweaker
  where
    tweaker (Tweak c a) = when (c old /= c new) $ a new

type TodoId = Int
data Todo = Todo { todoId :: TodoId
                 , todoText :: Text
                 , todoDone :: Bool
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

initialTodos :: IO [Todo]
initialTodos =
  return 
    [ Todo 3 "Steal underpants" True False
    , Todo 14 "???" False False
    , Todo 16 "Profit!" False False
    ]

todoFilter :: FilterName -> Filter
todoFilter "active" = not . todoDone
todoFilter "completed" = todoDone
todoFilter _ = const True
