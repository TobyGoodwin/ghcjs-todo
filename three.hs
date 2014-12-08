{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
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
  myThing <- fetchTodoList
  select "#todo-list-div" >>= appendJQuery myThing

fetchTodoList = do
  select $ T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (t, c) <- ts
        <li :c:.completed>
          <input .toggle type=checkbox>
          <label>
            #{t}
          <button class="destroy">
  |]
  where
    ts :: [(Text, Bool)]
    ts = [("Steal underpants", True), ("???", False), ("Profit!", False)]
