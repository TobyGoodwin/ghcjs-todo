{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative ((<$>))
import Data.Text.Lazy (Text, unpack)
import GHCJS.DOM (runWebGUI, webViewGetDomDocument)
import GHCJS.DOM.Document (documentCreateElement, documentGetElementById, documentGetBody)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerText, htmlElementSetInnerHTML)
import GHCJS.DOM.Node (nodeInsertBefore, nodeAppendChild)
import GHCJS.DOM.Types (castToHTMLDivElement)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

main = do
  -- Running a GUI creates a WebKitGtk window in native code,
  -- but just returns the browser window when compiled to JavaScript
  runWebGUI $ \ webView -> do
    -- WebKitGtk provides the normal W3C DOM functions
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    -- Lets use some Hamlet to replace HTerm with some HTML
    Just div <- fmap castToHTMLDivElement <$> documentCreateElement doc "div"
    htmlElementSetInnerHTML div . unpack $ renderHtml [shamlet|$newline always
        <h1 #heading>
          Hello and Welcome GHCJS
        <p>
          Know any poor prime numbers?
    |]
    Just todoListElem <- documentGetElementById doc "todo-list-div"
    todoList <- fetchTodoList doc
    nodeAppendChild todoListElem (Just todoList)
    return ()

fetchTodoList doc = do
  Just ul <- fmap castToHTMLDivElement <$> documentCreateElement doc "div"
  htmlElementSetInnerHTML ul . unpack $ renderHtml [shamlet|$newline always
    <ul #todo-list>
      $forall (t, c) <- ts
        <li :c:.completed>
          <input .toggle type=checkbox>
          <label>
            #{t}
          <button class="destroy">
  |]
  return ul
  where
    ts = [("Steal underpants", True), ("???", False), ("Profit!", False)]
