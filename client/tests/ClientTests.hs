{-# LANGUAGE OverloadedStrings #-}


import Test.Microspec
import Data.Text (Text)
import Data.Either
import Data.Time
import Data.IntMap as IntMap
import Estuary.Types.Tempo
import Estuary.Languages.CineCer0.Signal
import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Parser
import Estuary.Languages.CineCer0.Spec

import Estuary.Types.Terminal as Terminal
import Estuary.Types.View



main :: IO ()
main = microspec $ do
  describe "the terminal command parser" $ do
    it "parses an empty string succesfully" $ Terminal.parseCommand "" `shouldBe` Right (Terminal.Chat "") -- isLeft (Terminal.parseCommand "") `shouldBe` True
    it "parses an empty string after a '!' producing an error msg" $ isLeft (Terminal.parseCommand "! ") `shouldBe` True
    it "parses a chat that begins with a non-! character" $ Terminal.parseCommand "hello" `shouldBe` Right (Terminal.Chat "hello")
    it "parses a resetzones command" $ Terminal.parseCommand "!resetzones" `shouldBe`  Right Terminal.ResetZones
    it "parses a resetviews command" $ Terminal.parseCommand "!resetviews" `shouldBe`  Right Terminal.ResetViews
    it "parses a resettempo command" $ Terminal.parseCommand "!resettempo" `shouldBe`  Right Terminal.ResetTempo
    it "parses a reset command" $ Terminal.parseCommand "!reset" `shouldBe`  Right Terminal.Reset
    it "parses a presetview command with an identifier as argument" $ Terminal.parseCommand "!presetview twocolumns" `shouldBe`  Right (Terminal.PresetView "twocolumns")
    it "parses a presetview command with a text as argument" $ Terminal.parseCommand "!presetview \"twocolumns\"" `shouldBe`  Right (Terminal.PresetView "twocolumns")
    it "parses a presetview command with an identifier as argument" $ Terminal.parseCommand "!publishview def" `shouldBe`  Right (Terminal.PublishView "def")
    it "parses a publishview command with a text as argument" $ Terminal.parseCommand "!publishview \"def\"" `shouldBe`  Right (Terminal.PublishView "def")
    it "parses a publishdefaultview command" $ Terminal.parseCommand "!publishdefaultview" `shouldBe`  Right (Terminal.PublishView "def")
    it "parses an activeview command with a text as argument" $ Terminal.parseCommand "!activeview" `shouldBe`  Right (Terminal.ActiveView)
    it "parses a localview command" $ Terminal.parseCommand "!localview (grid 2 3 [[label 1,code 2 0],[label 3,code 4 0],[label 5,code 6 0],[label 7,code 8 0],[label 9,code 10 0],[label 11,code 12 0]])" `shouldBe`  Right (Terminal.LocalView $ GridView 2 3 [(Views [LabelView 1, CodeView 2 0]), (Views [LabelView 3, CodeView 4 0]), (Views [LabelView 5, CodeView 6 0]), (Views [LabelView 7, CodeView 8 0]), (Views [LabelView 9, CodeView 10 0]), (Views [LabelView 11, CodeView 12 0])])
    it "parses a listviews command" $ Terminal.parseCommand "!listviews" `shouldBe`  Right Terminal.ListViews
    it "parses a dumpview command" $ Terminal.parseCommand "!dumpview" `shouldBe`  Right Terminal.DumpView
    it "parses a startstreaming command" $ Terminal.parseCommand "!startstreaming" `shouldBe` Right Terminal.StartStreaming
    it "parses a streamid command" $ Terminal.parseCommand "!streamid" `shouldBe`  Right Terminal.StreamId
    it "parses a delay command with a double as argument" $ Terminal.parseCommand "!delay 0.5" `shouldBe` Right (Terminal.Delay 0.5)
    it "parses a delay command with an int as argument" $ Terminal.parseCommand "!delay 1" `shouldBe` Right (Terminal.Delay 1)
    it "parses a deletethisensemble command" $ Terminal.parseCommand "!deletethisensemble \"mypassword123\"" `shouldBe` Right (Terminal.DeleteThisEnsemble "mypassword123")
    it "parses a deleteensemble command" $ Terminal.parseCommand "!deleteensemble \"testEnsemble\" \"mypassword123\"" `shouldBe` Right (Terminal.DeleteEnsemble "testEnsemble" "mypassword123") -- no access to moderator password though EStuary's interface!
    it "parses a ancienttempo command" $ Terminal.parseCommand "!ancienttempo" `shouldBe` Right Terminal.AncientTempo
    it "parses a showtempo command" $ Terminal.parseCommand "!showtempo" `shouldBe` Right Terminal.ShowTempo
    it "parses a setcps command with a double as argument" $ Terminal.parseCommand "!setcps 0.5" `shouldBe` Right (Terminal.SetCPS 0.5)
    it "parses a setcps command with an int as argument" $ Terminal.parseCommand "!setcps 1" `shouldBe` Right (Terminal.SetCPS 1)
    it "parses a setbpm command with a double as argument" $ Terminal.parseCommand "!setbpm 60.45" `shouldBe` Right (Terminal.SetBPM 60.45)
    it "parses a setbpm command with an int as argument" $ Terminal.parseCommand "!setbpm 60" `shouldBe` Right (Terminal.SetBPM 60)
    it "parses a insertaudioresource command" $ Terminal.parseCommand "!insertaudioresource \"https://github.com/luisnavarrodelangel/estuary-samples/tree/main/teclado\" miteclado 0" `shouldBe` Right (Terminal.InsertAudioResource "https://github.com/luisnavarrodelangel/estuary-samples/tree/main/teclado" "miteclado" 0 )
    it "parses a deleteaudioresource command" $ Terminal.parseCommand "!deleteaudioresource miteclado 0" `shouldBe` Right (Terminal.DeleteAudioResource "miteclado" 0 )
    it "parses a appendaudioresource command" $ Terminal.parseCommand "!appendaudioresource \"https://github.com/luisnavarrodelangel/estuary-samples/tree/main/teclado\" miteclado" `shouldBe` Right (Terminal.AppendAudioResource "https://github.com/luisnavarrodelangel/estuary-samples/tree/main/teclado" "miteclado" )


  describe "the CineCer0 parser" $ do
    it "parses an empty string" $ fmap (IntMap.null . layerSpecMap) (cineCer0 eTime "") `shouldBe` Right True
    it "parses a comment" $ fmap (IntMap.null . layerSpecMap) (cineCer0 eTime "-- my comment") `shouldBe` Right True
    it "parses empty statements" $ fmap (IntMap.null . layerSpecMap) (cineCer0 eTime ";;") `shouldBe` Right True
    it "parses the name of a video" $ fmap (fmap layer . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "\"myMovie.mov\"") `shouldBe` Right (Just $ Right "myMovie.mov")
    it "parses the name of a video using the function 'video'" $ fmap (fmap layer . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "video \"myMovie.mov\"") `shouldBe` Right (Just $ Right "myMovie.mov")
    it "parses the volume command" $ fmap (fmap (\vs -> fieldFromLayerSpec (volume vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "vol 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    -- it "parses a mute command" $ fmap (fmap (\vs -> fieldFromLayerSpec (mute vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "mute $ \"myMovie.mov\"") `shouldBe` Right (Just True)
    it "parses the ramp command" $ fmap (fmap (\vs -> fieldFromLayerSpec (volume vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "vol (ramp 10 0 1) $ \"myMovie.mov\"") `shouldBe` Right (Just 0)
    it "parses the fadeIn command" $ fmap (fmap (\vs -> fieldFromLayerSpec (volume vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "vol (fadeIn 8) $ \"myMovie.mov\"") `shouldBe` Right (Just 0)
    it "parses the fadeOut command" $ fmap (fmap (\vs -> fieldFromLayerSpec (volume vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "vol (fadeOut 8) $ \"myMovie.mov\"") `shouldBe` Right (Just 1)
    it "parses the posX command" $ fmap (fmap (\vs -> fieldFromLayerSpec (posX vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setPosX 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "parses the posY command" $ fmap (fmap (\vs -> fieldFromLayerSpec (posY vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setPosY 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "parses the sin command" $ fmap (fmap (\vs -> fieldFromLayerSpec (posY vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setPosY (sin 0.1) $ \"myMovie.mov\"") `shouldBe` Right (Just 0)
    it "parses the width command" $ fmap (fmap (\vs -> fieldFromLayerSpec (width vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "width 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "parses the height command" $ fmap (fmap (\vs -> fieldFromLayerSpec (height vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "height 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "parses the opacity command" $ fmap (fmap (\vs -> fieldFromLayerSpec (opacity vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "opacity 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setOpacity command" $ fmap (fmap (\vs -> fieldFromLayerSpec (opacity vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setOpacity 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftOpacity command" $ fmap (fmap (\vs -> fieldFromLayerSpec (opacity vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftOpacity 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the blur command" $ fmap (fmap (\vs -> fieldFromLayerSpec (blur vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "blur 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setBlur command" $ fmap (fmap (\vs -> fieldFromLayerSpec (blur vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setBlur 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftBlur command" $ fmap (fmap (\vs -> fieldFromLayerSpec (blur vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftBlur 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the brightness command" $ fmap (fmap (\vs -> fieldFromLayerSpec (brightness vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "brightness 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setBrightness command" $ fmap (fmap (\vs -> fieldFromLayerSpec (brightness vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setBrightness 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftBrightness command" $ fmap (fmap (\vs -> fieldFromLayerSpec (brightness vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftBrightness 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the contrast command" $ fmap (fmap (\vs -> fieldFromLayerSpec (contrast vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "contrast 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setContrast command" $ fmap (fmap (\vs -> fieldFromLayerSpec (contrast vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setContrast 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftContrast command" $ fmap (fmap (\vs -> fieldFromLayerSpec (contrast vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftContrast 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the grayscale command" $ fmap (fmap (\vs -> fieldFromLayerSpec (grayscale vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "grayscale 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setGrayscale command" $ fmap (fmap (\vs -> fieldFromLayerSpec (grayscale vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setGrayscale 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftGrayscale command" $ fmap (fmap (\vs -> fieldFromLayerSpec (grayscale vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftGrayscale 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the saturate command" $ fmap (fmap (\vs -> fieldFromLayerSpec (saturate vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "saturate 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "parses the setSaturate command" $ fmap (fmap (\vs -> fieldFromLayerSpec (saturate vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "setSaturate 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    -- it "parses the shiftSaturate command" $ fmap (fmap (\vs -> fieldFromLayerSpec (saturate vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "shiftSaturate' 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))

    it "parses the every command" $ fmap (fmap (\vs -> (fieldFromLayerSpec (playbackPosition vs), fieldFromLayerSpec (playbackRate vs))) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "every 2 10 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0, Just 3.125))

    -- masks receive values from 0 - 1
    it "parses the 'cirlceMask' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (mask vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "circleMask 0.5 $ \"myMovie.mov\"") `shouldBe`  Right (Just "clip-path:circle(35.5% at 50% 50%);")
    it "parses the 'sqrMask' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (mask vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "sqrMask 0.5 $ \"myMovie.mov\"") `shouldBe`  Right (Just "clip-path: inset(25.0%);")
    it "parses the 'rectMask' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (mask vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "rectMask 0.5 0.5 1 1 $ \"myMovie.mov\"") `shouldBe`  Right (Just "clip-path: inset(50.0% 50.0% 100.0% 100.0%);")
 -- text funcs
    it "parses the 'text' command" $ fmap (fmap layer . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "text \"my text\"") `shouldBe` Right (Just $ Left "my text")
    it "parses the 'font' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (fontFamily vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "font \"Arial\" $ text \"my text\"") `shouldBe` Right (Just "Arial")
    it "parses the 'fontSize' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (fontSize vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "fontSize 4 $ text \"my text\"") `shouldBe` Right (Just 4.0)
    it "parses the 'strike' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (strike vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "strike $ text \"my text\"") `shouldBe` Right (Just True)
    it "parses the 'bold' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (bold vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "bold $ text \"my text\"") `shouldBe` Right (Just True)
    it "parses the 'italic' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (italic vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "italic $ text \"my text\"") `shouldBe` Right (Just True)
    it "parses the 'border' command" $ fmap (fmap (\vs -> fieldFromLayerSpec (border vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "border $ text \"my text\"") `shouldBe` Right (Just True)
    -- it "parses the 'colour' command" $  (fmap (fmap (\vs ->  (colour . fieldFromLayerSpec vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "colour \"magenta\" $ text \"my text\"")) `shouldBe` Right (Just "magenta")
    -- it "parses the 'rgb' command" $ fmap (fmap (\vs ->  (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "rgb 0.7 0 0.9 $ text \"my text\"") `shouldBe` Right (Just 0.7 0 0.9)
    -- it "parses the 'hsl' command" $ fmap (fmap (\vs ->  (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "hsl 1 1 0.7 $ text \"my text\"") `shouldBe` Right (Just 1 1 0.7)
    -- it "parses the 'hsv' command" $ fmap (fmap (\vs ->  (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "hsv 1 1 0.7 $ text \"my text\"") `shouldBe` Right (Just 1 1 0.7)
    -- it "parses the 'rgba' command" $ fmap (fmap (\vs -> (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "rgba 0.7 0 0.9 0.5 $ text \"my text\"") `shouldBe` Right (Just 0.7 0 0.9 0.5)
    -- it "parses the 'hsla' command" $ fmap (fmap (\vs -> (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "hsla 1 1 0.7 $ text \"my text\"") `shouldBe` Right (Just 1 1 0.7 0.5)
    -- it "parses the 'hsva' command" $ fmap (fmap (\vs -> (colour vs)) . IntMap.lookup 0 . layerSpecMap) (cineCer0 eTime "hsva 1 1 0.7 $ text \"my text\"") `shouldBe` Right (Just 1 1 0.7 0.5)

-- f :: Either String (Maybe Colour) -> Either String (Maybe (Signal String)) -- Either String (Maybe (Signal String))
-- f (Right (Just (Colour x))) = Right $ Just x
-- f _ = Right $ Nothing


fieldFromLayerSpec f = f tempoTest vidlen rTime eTime aTime

hoy = fromGregorian 2019 05 04

unMes = fromGregorian 2019 06 04

timeTempo = UTCTime hoy 0 -- time in which the tempo mark starts counting

rTime = UTCTime hoy 60 -- one month later the render time is introduced

eTime = UTCTime hoy 58 -- one month later the evaltime is introduced

aTime = defaultAnchor myTempo eTime -- calculated from the tempo and the evaltime

tempoTest = Tempo { freq= 0.5, time= timeTempo, count= 100}

vidlen = realToFrac 12.5 :: NominalDiffTime

mark ndt utc = addUTCTime ndt utc
