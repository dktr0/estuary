{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Chat where

import Data.Time
import Data.Text (Text, unpack)
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import System.IO.Unsafe

import Control.Monad.Trans (liftIO)
import Data.Map.Strict (fromList)
import Control.Monad
import Estuary.Types.Ensemble
import Estuary.Types.Participant
import Estuary.Types.Chat
import Estuary.Types.Definition
import qualified Data.Text as T



import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Types.Definition
import Estuary.Widgets.Text
import Control.Monad
import Reflex.Dom
import Reflex.Dynamic


chatWidget :: MonadWidget t m => Dynamic t SpecChat -> W t m (Variable t SpecChat)
chatWidget delta  =  divClass "ensembleStatusWidget code-font" $ mdo
  input <-  divClass "terminalHeader code-font primary-color" $ mdo -- :: Event t Text
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let chatInput = tag (current (value inputWidget)) enterPressed -- :: Event t b
    let resetText = fmap (\_ -> "") enterPressed
    let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
    inputWidget <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
    pure $ chatInput

  divClass "fullWidthDiv" $ mdo
    let msgList = fmap makeList (currentValue state) --  currentValue :: Dynamic t a,
    divClass "chatMessageContainer" $ simpleList msgList $ \v -> do
      let newMsg = fmap formatMsg v
      divClass "chatMessage code-font primary-color, " $ dynText newMsg

  userString <- userHandle >>= (sample . current)

  makeMsgThread <- performEvent $ fmap liftIO $ attachWith (sendMessage userString) (current $ currentValue state) input -- Event (Maybe SpechChat)
  let z =  fmapMaybe id makeMsgThread
  state <- variable delta z -- ::  m (Variable t a)
  pure $ state

formatMsg :: Chat -> Text
formatMsg v = chatSender v <> ": " <> chatText v <> "\n"

makeList :: SpecChat -> [Chat]
makeList sc =  sc

sendMessage :: Text -> SpecChat -> Text {- Message -} -> IO (Maybe SpecChat)
sendMessage userString sc msg = do
  -- had to cut down on arguments, how should I get uHandle? can i retrieve context outside of monad?
  --let userString = "dog" --unpack user
  case userString of
    "anonymous" -> return Nothing
    _ -> do
      y <- makeAMessageNow userString msg
      return $ Just $ y : sc

makeAMessageNow :: Text -> Text ->IO Chat
makeAMessageNow s t = do
  now <- getCurrentTime
  return $ Chat { chatTime = now, chatSender = s, chatText = t }
