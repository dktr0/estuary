module Estuary.Test.Dom where

import GHCJS.DOM(currentDocument)
import GHCJS.DOM.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Nullable
import GHCJS.Types

import GHCJS.Prim(toJSArray, fromJSArray)

import qualified GHCJS.DOM.HTMLInputElement as HTMLInputElement
import qualified GHCJS.DOM.HTMLElement as HTMLElement

-- querySelector :: (MonadIO m, IsDocument self, ToJSString selectors) =>
--   self -> selectors -> m (Maybe Element)
-- querySelector :: (MonadIO m, IsElement self, ToJSString selectors) =>
--   self -> selectors -> m (Maybe Element)

maybePFromNullableJSVal :: (PFromJSVal e) => Nullable JSVal -> Maybe e
maybePFromNullableJSVal = (fmap pFromJSVal) . nullableToMaybe

findMatchingSelectorInDocument :: (ToJSString s, PFromJSVal e) => s -> IO (Maybe e)
findMatchingSelectorInDocument query =
  fmap maybePFromNullableJSVal $ 
    js_docQuerySelector (toJSString query)

findMatchingSelector :: (IsElement c, ToJSString s, PFromJSVal e) => c -> s -> IO (Maybe e)
findMatchingSelector container query = 
  fmap maybePFromNullableJSVal $ 
    js_querySelector (pToJSVal $ toElement container) (toJSString query)

findAllMatchingSelectorInDocument :: (ToJSString s, FromJSVal e) => s -> IO ([e])
findAllMatchingSelectorInDocument query =
  js_docQuerySelectorAll (toJSString query) >>= fromJSValUncheckedListOf

findAllMatchingSelector :: (IsElement c, ToJSString s, FromJSVal e) => c -> s -> IO ([e])
findAllMatchingSelector container query = 
  js_querySelectorAll (pToJSVal $ toElement container) (toJSString query)  >>= fromJSValUncheckedListOf


click :: (IsHTMLElement e) => e -> IO ()
click = HTMLElement.click

innerText :: (IsHTMLElement e, FromJSString s) => e -> IO (s)
innerText = HTMLElement.getInnerText

-- Like textContent but only for immediate children of the input element
immediateText :: (IsElement e, FromJSString s) => e -> IO (s)
immediateText el = fromJSString <$> js_immediateText (pToJSVal $ toElement el)

changeValue :: (IsHTMLElement e, ToJSString val) => e -> val -> IO ()
changeValue e value = do
  js_setValue (pToJSVal $ toElement e) (toJSString value)
  notifyChanged e


notifyChanged :: (IsEventTarget e) => e -> IO ()
notifyChanged e = js_dispatchInputEvent (pToJSVal $ toEventTarget e)

------------------------------------------
-- JS FFI
------------------------------------------

foreign import javascript safe
  "$1.value = $2;"
  js_setValue :: JSVal -> JSString -> IO ()

foreign import javascript safe
  "document.querySelector($1)"
  js_docQuerySelector :: JSString -> IO (Nullable JSVal)

foreign import javascript safe
  "$1.querySelector($2)"
  js_querySelector :: JSVal -> JSString -> IO (Nullable JSVal)

foreign import javascript safe
  "Array.from(document.querySelector($1))"
  js_docQuerySelectorAll :: JSString -> IO (JSVal)

foreign import javascript safe
  "Array.from($1.querySelector($2))"
  js_querySelectorAll :: JSVal -> JSString -> IO (JSVal)

foreign import javascript safe
  "$1.dispatchEvent(new Event('input', {bubbles: true, cancelable: true}))"
  js_dispatchInputEvent :: JSVal -> IO ()

foreign import javascript safe
  "$r = Array.from($1.childNodes)\
  \.filter(function (n) { return n.nodeType === 3; })\
  \.map(function (n) { return n.nodeValue; })\
  \.join('');"
  js_immediateText :: JSVal -> IO (JSString)