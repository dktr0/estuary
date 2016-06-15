module Types.RequestMap where

import Widgets.SoundWidget
import Data.Map as Map

data SoundReqMap = SoundReqMap (Map Int (Maybe SoundWidgetRequest)) deriving (Eq)

initialReqMap :: SoundReqMap
initialReqMap = simple (Just (Default simpleSound "sn"))

insert :: (Int,Int,SoundWidgetRequest) -> SoundReqMap -> SoundReqMap
insert (dkey,ikey,req) (SoundReqMap reqmap) = if Map.member (ikey+1) reqmap
                                                  then do
                                                       let ureqmap = if (dkey == ikey) then pattern else Map.delete dkey reqmap
                                                       let (lp,gp) = Map.partitionWithKey (\ k _ -> k > (ikey+1)) ureqmap
                                                       let gpp = Map.mapKeys (+1) gp
                                                       let lpp = Map.insert ((+1) $ fst $ Map.findMax lp) (Just (req)) reqmap
                                                       SoundReqMap (Map.union lpp gpp)
                                                  else SoundReqMap (Map.insert (ikey+1) (Just (req)) reqmap)

adjustReq :: (SoundWidgetRequest,Int) -> SoundReqMap -> SoundReqMap
adjustReq (req,key) (SoundReqMap reqmap) = SoundReqMap (Map.adjust key (Just (req)) reqmap)

-- set value at key to nothing
delete :: Int -> SoundReqMap -> SoundReqMap
delete key (SoundReqMap reqmap) = SoundReqMap (Map.delete key reqmap)

-- convenience functions
empty :: SoundReqMap
empty = SoundReqMap Map.empty

simple :: SoundWidgetRequest -> SoundReqMap
simple x = SoundReqMap (Map.singleton 1 Just (x))
