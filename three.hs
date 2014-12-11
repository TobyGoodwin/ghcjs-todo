{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.State (StateT, get, lift, runStateT)
import Data.Default
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import JavaScript.JQuery
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

main = do
  myClick <- select "<div>click here</div>"
  myCount <- select "<div>0</div>"
  counter <- newIORef (0::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', T.pack $ show c'))
  click (\_ -> void $ getCount >>= flip setText myCount) def myClick
  select "body" >>= appendJQuery myClick >>= appendJQuery myCount
  runStateT stMain initialTodos
  return ()

stMain :: StateT Todos IO ()
stMain = do
  myThing <- fetchTodoList
  lift $ select "#todo-list-div" >>= appendJQuery myThing
  return ()

thing :: Int -> IO ()
thing i = do
  myThing <- select "<div>this is my new thing!</div>"
  select "body" >>= appendJQuery myThing
  select (T.append "#todo-list-" (T.pack (show i))) >>= detach
  -- select "#todo-list-3" >>= detach
  return ()

--           <button class="destroy" onclick="h$run(h$mainZCMainzithing)">
fetchTodoList = do
  ts <- get
  lift $ select $ T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (i, t, c) <- ts
        <li :c:.completed #todo-list-#{i}>
          <input .toggle type=checkbox>
          <label>
            #{t}
          <button class=destroy onclick=delete_helper(#{i})>
  |]

type Todo = (Int, Text, Bool)
type Todos = [Todo]

initialTodos :: Todos
initialTodos = 
  [ (19, "Steal underpants", True)
  , (3, "???", False)
  , (16, "Profit!", False)
  ]
