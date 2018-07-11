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
  SabortsView Int |
  SaludosView Int |
  ColombiaView Int |
  SiView Int |
  SentidosView Int|
  NaturalView Int |
  MedellinView Int |
  LaCalleView Int |
  MariaView Int |
  CrudoView Int |
  PuntoyyaView Int |
  SucixxxView Int |
  VocesotrevezView Int |
  ImaginaView Int |
  AlobestiaView Int |
  Test1View Int
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
  showJSON (SabortsView n) = encJSDict [("SabortsView",n)]
  showJSON (SaludosView n) = encJSDict [("SaludosView",n)]
  showJSON (ColombiaView n) = encJSDict [("ColombiaView",n)]
  showJSON (NaturalView n) = encJSDict [("NaturalView",n)]
  showJSON (MedellinView n) = encJSDict [("MedellinView",n)]
  showJSON (LaCalleView n) = encJSDict [("LaCalleView",n)]
  showJSON (MariaView n) = encJSDict [("MariaView",n)]
  showJSON (CrudoView n) = encJSDict [("CrudoView",n)]
  showJSON (SentidosView n) = encJSDict [("SentidosView",n)]
  showJSON (PuntoyyaView n) = encJSDict [("PuntoyyaView",n)]
  showJSON (SucixxxView n) = encJSDict [("SucixxxView",n)]
  showJSON (VocesotrevezView n) = encJSDict [("VocesotrevezView",n)]
  showJSON (ImaginaView n) = encJSDict [("ImaginaView",n)]
  showJSON (AlobestiaView n) = encJSDict [("AlobestiaView",n)]
  showJSON (Test1View n) = encJSDict [("Test1View",n)]
  readJSON (JSObject x) | firstKey x == "Views" = Views <$> valFromObj "Views" x
  readJSON (JSObject x) | firstKey x == "ViewDiv" = ViewDiv <$> valFromObj "ViewDiv" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "LabelView" = LabelView <$> valFromObj "LabelView" x
  readJSON (JSObject x) | firstKey x == "StructureView" = StructureView <$> valFromObj "StructureView" x
  readJSON (JSObject x) | firstKey x == "TidalTextView" = TidalTextView <$> valFromObj "TidalTextView" x
  readJSON (JSObject x) | firstKey x == "EvaluableTextView" = EvaluableTextView <$> valFromObj "EvaluableTextView" x
  readJSON (JSObject x) | firstKey x == "CQenzeView" = CQenzeView <$> valFromObj "CQenzeView" x
  readJSON (JSObject x) | firstKey x == "MoreliaView" = MoreliaView <$> valFromObj "MoreliaView" x
  readJSON (JSObject x) | firstKey x == "SabortsView" = SabortsView <$> valFromObj "SabortsView" x
  readJSON (JSObject x) | firstKey x == "SaludosView" = SaludosView <$> valFromObj "SaludosView" x
  readJSON (JSObject x) | firstKey x == "ColombiaView" = ColombiaView <$> valFromObj "ColombiaView" x
  readJSON (JSObject x) | firstKey x == "SiView" = SiView <$> valFromObj "SiView" x
  readJSON (JSObject x) | firstKey x == "NaturalView" = NaturalView <$> valFromObj "NaturalView" x
  readJSON (JSObject x) | firstKey x == "MedellinView" = MedellinView <$> valFromObj "MedellinView" x
  readJSON (JSObject x) | firstKey x == "LaCalleView" = LaCalleView <$> valFromObj "LaCalleView" x
  readJSON (JSObject x) | firstKey x == "MariaView" = MariaView <$> valFromObj "MariaView" x
  readJSON (JSObject x) | firstKey x == "CrudoView" = CrudoView <$> valFromObj "CrudoView" x
  readJSON (JSObject x) | firstKey x == "PuntoyyaView" = PuntoyyaView <$> valFromObj "PuntoyyaView" x
  readJSON (JSObject x) | firstKey x == "SucixxxView" = SucixxxView <$> valFromObj "SucixxxView" x
  readJSON (JSObject x) | firstKey x == "VocesotrevezView" = VocesotrevezView <$> valFromObj "VocesotrevezView" x
  readJSON (JSObject x) | firstKey x == "ImaginaView" = ImaginaView <$> valFromObj "ImaginaView" x
  readJSON (JSObject x) | firstKey x == "AlobestiaView" = AlobestiaView <$> valFromObj "AlobestiaView" x
  readJSON (JSObject x) | firstKey x == "Test1View" = Test1View <$> valFromObj "Test1View" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Protocol.View: " ++ (show x)
  readJSON _ = Error $ "Unable to parse non-JSObject as Estuary.Protocol.View"

standardView :: View
standardView = Views [
  ViewDiv "eightTopL" (Views [LabelView 1, StructureView 2]),
  ViewDiv "eightTopR" (Views [LabelView 3, StructureView 4]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TidalTextView 6]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TidalTextView 8]),
  ViewDiv "eightBottomL" (Views [LabelView 9, CQenzeView 10]),
  ViewDiv "eightBottomR" (Views [LabelView 11, EvaluableTextView 12])
  ]

emptyView :: View
emptyView = Views []

viewsParser :: GenParser Char a View
viewsParser = do
  spaces
  many1 viewParser >>= return . Views

viewParser :: GenParser Char a View
viewParser = do
  v <- choice [viewDiv,labelView,structureView,tidalTextView,evaluableTextView,cqenzeView,moreliaView, sabortsView, saludosView, sentidosView, siView, colombiaView, naturalView, medellinView, laCalleView, mariaView, crudoView, puntoyyaView, sucixxxView, vocesotrevezView, imaginaView, alobestiaView, test1View]
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
tidalTextView = string "tidal:" >> (read <$> many1 digit) >>= return . TidalTextView
cqenzeView = string "cquenze:" >> (read <$> many1 digit) >>= return . CQenzeView
moreliaView = string "morelia:" >> (read <$> many1 digit) >>= return . MoreliaView
sabortsView = string "saborts:" >> (read <$> many1 digit) >>= return . SabortsView
saludosView = string "saludos:" >> (read <$> many1 digit) >>= return . SaludosView
colombiaView = string "colombia:" >> (read <$> many1 digit) >>= return . ColombiaView
sentidosView = string "sentidos:" >> (read <$> many1 digit) >>= return . SentidosView
siView = string "si:" >> (read <$> many1 digit) >>= return . SiView
naturalView = string "naturalView:" >> (read <$> many1 digit) >>= return . NaturalView
medellinView = string "medellinView:" >> (read <$> many1 digit) >>= return . MedellinView
laCalleView = string "laCalleView:" >> (read <$> many1 digit) >>= return . LaCalleView
mariaView = string "mariaView:" >> (read <$> many1 digit) >>= return . MariaView
crudoView = string "crudoView:" >> (read <$> many1 digit) >>= return . CrudoView
puntoyyaView = string "puntoyyaView:" >> (read <$> many1 digit) >>= return . PuntoyyaView
sucixxxView = string "sucixxxView:" >> (read <$> many1 digit) >>= return . SucixxxView
imaginaView = string "imaginaView:" >> (read <$> many1 digit) >>= return . ImaginaView
vocesotrevezView = string "vocesotrevezView:" >> (read <$> many1 digit) >>= return . VocesotrevezView
alobestiaView = string "alobestiaView:" >> (read <$> many1 digit) >>= return . AlobestiaView
test1View = string "test1View:" >> (read <$> many1 digit) >>= return . Test1View

presetView :: String -> View

presetView "iclc2017" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,MoreliaView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,CQenzeView 3]),
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

presetView "Bogota" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,SaludosView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,SaludosView 3]),
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

presetView "Manizales" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,NaturalView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,NaturalView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7])
  ]

presetView "Medellin" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,MedellinView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,MedellinView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7])
  ]

presetView "Lima" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,LaCalleView 1]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,LaCalleView 3]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,MariaView 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,MariaView 7]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,CrudoView 9]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,CrudoView 11]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15])
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
  ViewDiv "eightMiddleL" (Views [LabelView 0,SabortsView 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,SabortsView 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,SabortsView 9]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11])
  ]

presetView "test" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,Test1View 1]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,Test1View 3]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,Test1View 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7])
  ]


presetView _ = standardView
