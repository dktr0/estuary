{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE TypeFamilies #-} -- @I don't remember what this was for, might not need....
{-# LANGUAGE OverloadedStrings  #-}

module Estuary.WebDirt.Foreign where
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import GHCJS.Foreign.Internal

foreign import javascript unsafe "$r = new WebDirt(null,null,null, function() {console.log('callback from WebDirt constructor completed');});" newWebDirt :: IO (T.JSVal)
foreign import javascript unsafe "$1.queue({sample_name: 'cp', sample_n:0});" webDirtTestMessage ::T.JSVal->IO (T.JSVal)
foreign import javascript unsafe "$1.queue({sample_name: $2, sample_n:0});" playSample::T.JSVal ->T.JSVal -> IO()
