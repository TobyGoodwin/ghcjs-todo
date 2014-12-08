{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.Default
import Data.IORef
import JavaScript.JQuery
import qualified Data.Text as T

main = do
  myClick <- select "<div>click here</div>"
  myCount <- select "<div>0</div>"
  counter <- newIORef (0::Int)
  let getCount = atomicModifyIORef counter (\c -> let c' = c+1 in (c', T.pack $ show c'))
  click (\_ -> void $ getCount >>= flip setText myCount) def myClick
  select "body" >>= appendJQuery myClick >>= appendJQuery myCount
