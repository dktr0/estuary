import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))

main = mainWidget $ el "div" $ do
    nx <- numberInput
    d <- dropdown "*" (constDyn ops) def
    ny <- numberInput
    values <- combineDyn (,) nx ny
    result <- combineDyn (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
    resultString <- mapDyn show result
    text " = "
    dynText resultString
    
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
    n <- textInput $ def & textInputConfig_inputType .~ "number"
                         & textInputConfig_initialValue .~ "0"
    mapDyn readMay $ _textInput_value n

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

stringToOp s = case s of
                    "-" -> (-)
                    "*" -> (*)
                    "/" -> (/)
                    _ -> (+)

