module Estuary.Types.View where

import Text.ParserCombinators.Parsec
import Text.JSON
import Estuary.Utility (firstKey)

data View =
  Views [View] |
  ViewDiv String View |
  LabelView Int |
  StructureView Int |
  TidalTextView Int |
  EvaluableTextView Int |
  CQenzeView Int |
  MoreliaView Int |
  Test1View Int |
  SabortsView Int |
  SaludosView Int |
  ColombiaView Int |
  SiView Int |
  SentidosView Int|
  RGGTRN Int |
  PuntoyyaView Int |
  SucixxxView Int |
  VocesotrevezView Int |
  ImaginaView Int |
  AlobestiaView Int
  deriving (Show,Eq)

instance JSON View where
  showJSON (Views xs) = encJSDict [("Views",xs)]
  showJSON (ViewDiv c v) = encJSDict [("ViewDiv",showJSON c),("v",showJSON v)]
  showJSON (LabelView n) = encJSDict [("LabelView",n)]
  showJSON (StructureView n) = encJSDict [("StructureView",n)]
  showJSON (TidalTextView n) = encJSDict [("TidalTextView",n)]
  showJSON (EvaluableTextView n) = encJSDict [("EvaluableTextView",n)]
  showJSON (CQenzeView n) = encJSDict [("CQenzeView",n)]
  showJSON (MoreliaView n) = encJSDict [("MoreliaView",n)]
  showJSON (Test1View n) = encJSDict [("Test1View",n)]
  showJSON (SabortsView n) = encJSDict [("SabortsView",n)]
  showJSON (SaludosView n) = encJSDict [("SaludosView",n)]
  showJSON (ColombiaView n) = encJSDict [("ColombiaView",n)]
  showJSON (SiView n) = encJSDict [("SiView",n)]
  showJSON (SentidosView n) = encJSDict [("SentidosView",n)]
  showJSON (PuntoyyaView n) = encJSDict [("PuntoyyaView",n)]
  showJSON (SucixxxView n) = encJSDict [("SucixxxView",n)]
  showJSON (VocesotrevezView n) = encJSDict [("VocesotrevezView",n)]
  showJSON (ImaginaView n) = encJSDict [("ImaginaView",n)]
  showJSON (AlobestiaView n) = encJSDict [("AlobestiaView",n)]
  readJSON (JSObject x) | firstKey x == "Views" = Views <$> valFromObj "Views" x
  readJSON (JSObject x) | firstKey x == "ViewDiv" = ViewDiv <$> valFromObj "ViewDiv" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "LabelView" = LabelView <$> valFromObj "LabelView" x
  readJSON (JSObject x) | firstKey x == "StructureView" = StructureView <$> valFromObj "StructureView" x
  readJSON (JSObject x) | firstKey x == "TidalTextView" = TidalTextView <$> valFromObj "TidalTextView" x
  readJSON (JSObject x) | firstKey x == "EvaluableTextView" = EvaluableTextView <$> valFromObj "EvaluableTextView" x
  readJSON (JSObject x) | firstKey x == "CQenzeView" = CQenzeView <$> valFromObj "CQenzeView" x
  readJSON (JSObject x) | firstKey x == "MoreliaView" = MoreliaView <$> valFromObj "MoreliaView" x
  readJSON (JSObject x) | firstKey x == "Test1View" = Test1View <$> valFromObj "Test1View" x
  readJSON (JSObject x) | firstKey x == "SabortsView" = SabortsView <$> valFromObj "SabortsView" x
  readJSON (JSObject x) | firstKey x == "SaludosView" = SaludosView <$> valFromObj "SaludosView" x
  readJSON (JSObject x) | firstKey x == "ColombiaView" = ColombiaView <$> valFromObj "ColombiaView" x
  readJSON (JSObject x) | firstKey x == "SiView" = SiView <$> valFromObj "SiView" x
  readJSON (JSObject x) | firstKey x == "SentidosView" = SentidosView <$> valFromObj "SentidosView" x
  readJSON (JSObject x) | firstKey x == "PuntoyyaView" = PuntoyyaView <$> valFromObj "PuntoyyaView" x
  readJSON (JSObject x) | firstKey x == "Sucixxx" = SucixxxView <$> valFromObj "Sucixxx" x
  readJSON (JSObject x) | firstKey x == "VocesotrevezView" = VocesotrevezView <$> valFromObj "VocesotrevezView" x
  readJSON (JSObject x) | firstKey x == "ImaginaView" = ImaginaView <$> valFromObj "ImaginaView" x
  readJSON (JSObject x) | firstKey x == "AlobestiaView" = AlobestiaView <$> valFromObj "AlobestiaView" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Protocol.View: " ++ (show x)
  readJSON _ = Error $ "Unable to parse non-JSObject as Estuary.Protocol.View"


standardView :: View
standardView = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,PuntoyyaView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,PuntoyyaView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,PuntoyyaView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,PuntoyyaView 7]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,SucixxxView 9]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,SucixxxView 11]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,SucixxxView 13]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,SucixxxView 15]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,VocesotrevezView 17]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,VocesotrevezView 19]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,VocesotrevezView 21]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,VocesotrevezView 23]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,ImaginaView 25]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,ImaginaView 27]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,ImaginaView 29]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,AlobestiaView 31]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,AlobestiaView 33]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,AlobestiaView 35])
  ]

emptyView :: View
emptyView = Views []

viewsParser :: GenParser Char a View
viewsParser = do
  spaces
  many1 viewParser >>= return . Views

viewParser :: GenParser Char a View
viewParser = do
  v <- choice [viewDiv,labelView,structureView,tidalTextView,evaluableTextView,cqenzeView,moreliaView, testView, saludosView, sentidosView, siView, colombiaView, sabortsView, puntoyyaView, sucixxxView, vocesotrevezView, imaginaView, alobestiaView]
  spaces
  return v

viewDiv = between (char '{') (char '}') $ do
  spaces
  cssClass <- many1 alphaNum
  skipMany1 space
  vs <- viewsParser
  spaces
  return $ ViewDiv cssClass vs

labelView = string "label:" >> (read <$> many1 digit) >>= return . LabelView
structureView = string "structure:" >> (read <$> many1 digit) >>= return . StructureView
evaluableTextView = string "evaluable:" >> (read <$> many1 digit) >>= return . EvaluableTextView
testView = string "test1:" >> (read <$> many1 digit) >>= return . Test1View
sabortsView = string "saborts:" >> (read <$> many1 digit) >>= return . SabortsView
tidalTextView = string "tidal:" >> (read <$> many1 digit) >>= return . TidalTextView
cqenzeView = string "cquenze:" >> (read <$> many1 digit) >>= return . CQenzeView
moreliaView = string "morelia:" >> (read <$> many1 digit) >>= return . MoreliaView
saludosView = string "saludos:" >> (read <$> many1 digit) >>= return . SaludosView
colombiaView = string "colombia:" >> (read <$> many1 digit) >>= return . ColombiaView
sentidosView = string "sentidos:" >> (read <$> many1 digit) >>= return . SentidosView
siView = string "si:" >> (read <$> many1 digit) >>= return . SiView
puntoyyaView = string "puntoyyaView:" >> (read <$> many1 digit) >>= return . PuntoyyaView
sucixxxView = string "sucixxxView:" >> (read <$> many1 digit) >>= return . SucixxxView
imaginaView = string "imaginaView:" >> (read <$> many1 digit) >>= return . ImaginaView
vocesotrevezView = string "vocesotrevezView:" >> (read <$> many1 digit) >>= return . VocesotrevezView
alobestiaView = string "alobestiaView:" >> (read <$> many1 digit) >>= return . AlobestiaView


presetView :: String -> View
presetView "UIO" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,PuntoyyaView 1]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,PuntoyyaView 3]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,PuntoyyaView 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,PuntoyyaView 7]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,SucixxxView 9]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,SucixxxView 11]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,SucixxxView 13]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,SucixxxView 15]),
    ViewDiv "eightMiddleL" (Views [LabelView 16,VocesotrevezView 17]),
    ViewDiv "eightMiddleR" (Views [LabelView 18,VocesotrevezView 19]),
    ViewDiv "eightMiddleL" (Views [LabelView 20,VocesotrevezView 21]),
    ViewDiv "eightMiddleR" (Views [LabelView 22,VocesotrevezView 23]),
    ViewDiv "eightMiddleL" (Views [LabelView 24,ImaginaView 25]),
    ViewDiv "eightMiddleR" (Views [LabelView 26,ImaginaView 27]),
    ViewDiv "eightMiddleL" (Views [LabelView 28,ImaginaView 29]),
    ViewDiv "eightMiddleR" (Views [LabelView 30,AlobestiaView 31]),
    ViewDiv "eightMiddleL" (Views [LabelView 32,AlobestiaView 33]),
    ViewDiv "eightMiddleR" (Views [LabelView 34,AlobestiaView 35])
    ]


{--presetView "Bogota" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,Test1View 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,Test1View 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,SaludosView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,SaludosView 7]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,ColombiaView 9]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,ColombiaView 11]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,ColombiaView 13]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,ColombiaView 15]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,SentidosView 17]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,SentidosView 19]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,SentidosView 21]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,SentidosView 23]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,SiView 25]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,SiView 27]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,SiView 29]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,SiView 31]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35])
  ]
--}

presetView "Manizales" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,PuntoyyaView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,PuntoyyaView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,PuntoyyaView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,PuntoyyaView 7]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,SucixxxView 9]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,SucixxxView 11]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,SucixxxView 13]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,SucixxxView 15]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,VocesotrevezView 17]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,VocesotrevezView 19]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,VocesotrevezView 21]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,VocesotrevezView 23]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,ImaginaView 25]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,ImaginaView 27]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,ImaginaView 29]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,AlobestiaView 31]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,AlobestiaView 33]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,AlobestiaView 35])
  ]

presetView "Medellin" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,Test1View 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,Test1View 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,TidalTextView 17]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,TidalTextView 19]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,TidalTextView 21]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,TidalTextView 23]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,TidalTextView 25]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,TidalTextView 27]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,TidalTextView 29]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,TidalTextView 31]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35])
  ]

presetView "Lima" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,Test1View 1]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,Test1View 3]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15]),
    ViewDiv "eightMiddleL" (Views [LabelView 16,TidalTextView 17]),
    ViewDiv "eightMiddleR" (Views [LabelView 18,TidalTextView 19]),
    ViewDiv "eightMiddleL" (Views [LabelView 20,TidalTextView 21]),
    ViewDiv "eightMiddleR" (Views [LabelView 22,TidalTextView 23]),
    ViewDiv "eightMiddleL" (Views [LabelView 24,TidalTextView 25]),
    ViewDiv "eightMiddleR" (Views [LabelView 26,TidalTextView 27]),
    ViewDiv "eightMiddleL" (Views [LabelView 28,TidalTextView 29]),
    ViewDiv "eightMiddleR" (Views [LabelView 30,TidalTextView 31]),
    ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33]),
    ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35])
    ]


presetView "Uio" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,PuntoyyaView 1]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,PuntoyyaView 3]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,PuntoyyaView 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,PuntoyyaView 7]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,SucixxxView 9]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,SucixxxView 11]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,SucixxxView 13]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,SucixxxView 15]),
    ViewDiv "eightMiddleL" (Views [LabelView 16,VocesotrevezView 17]),
    ViewDiv "eightMiddleR" (Views [LabelView 18,VocesotrevezView 19]),
    ViewDiv "eightMiddleL" (Views [LabelView 20,VocesotrevezView 21]),
    ViewDiv "eightMiddleR" (Views [LabelView 22,VocesotrevezView 23]),
    ViewDiv "eightMiddleL" (Views [LabelView 24,ImaginaView 25]),
    ViewDiv "eightMiddleR" (Views [LabelView 26,ImaginaView 27]),
    ViewDiv "eightMiddleL" (Views [LabelView 28,ImaginaView 29]),
    ViewDiv "eightMiddleR" (Views [LabelView 30,AlobestiaView 31]),
    ViewDiv "eightMiddleL" (Views [LabelView 32,AlobestiaView 33]),
    ViewDiv "eightMiddleR" (Views [LabelView 34,AlobestiaView 35])
    ]


presetView "RGGTRN" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,VocesotrevezView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,SabortsView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,SabortsView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,SabortsView 7])
  ]

presetView _ = standardView
