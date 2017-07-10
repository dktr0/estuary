module Estuary.Protocol.JSON where

import Estuary.Tidal.Types
import Estuary.Types.Definition
import Estuary.Types.Action

-- there is probably a better place for all the definitions below

isEstuaryEdit :: Action Definition -> Bool
isEstuaryEdit (Edit _ (Structure _)) = True
isEstuaryEdit _ = False

isTextEdit :: Action Definition -> Bool
isTextEdit (Edit _ (EvaluableText _)) = True
isTextEdit _ = False

isLabelEdit :: Action Definition -> Bool
isLabelEdit (Edit _ (LabelText _)) = True
isLabelEdit _ = False

justEstuaryCode :: Action Definition -> TransformedPattern
justEstuaryCode (Edit _ (Structure x)) = x
justEstuaryCode _ = error "can't get estuary code from non EstuaryEdit"

justTextCode :: Action Definition -> String
justTextCode (Edit _ (EvaluableText x)) = x
justTextCode (Eval _ (EvaluableText x)) = x
justTextCode _ = error "can't get text code from non TextEdit or TextEval"

justText :: Action Definition -> String
justText (Edit _ (EvaluableText x)) = x
justText (Eval _ (EvaluableText x)) = x
justText (Edit _ (LabelText x)) = x
justText (Chat _ x) = x
justText _ = error "can't get text from non TextEdit/TextEval/LabelEdit/Chat"
