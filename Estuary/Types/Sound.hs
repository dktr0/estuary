module Types.Sound where

data SoundEvent = ClickE | DragE | DropE | DragoverE | DragendE | HoveroverE | Empty deriving (Eq, Show)

data Sound = Sound (Maybe (String,Int,Int,Bool)) deriving (Eq)

silentSound = Sound (Nothing)

initialSound :: Sound
initialSound = simpleSound "sn"

simpleSound :: String -> Sound
simpleSound x = Sound (Just (x,0,1,False))

setSound :: Sound -> Sound
setSound (Sound(sound)) = Sound(sound)

name :: Sound -> String
name (Sound (Just (n,_,_,_))) = n
-- deliberately not implementing Nothing to throw exception

n :: Sound -> Int
n (Sound (Just (_,x,_,_))) = x
-- deliberately not implementing Nothing to throw exception

repeats :: Sound -> Int
repeats (Sound (Just (_,_,x,_))) = x
-- deliberately not implementing Nothing to throw exception

degraded :: Sound -> Bool
degraded (Sound (Just (_,_,_,x))) = x
-- deliberately not implementing Nothing to throw exception

isSilent :: Sound -> Bool
isSilent (Sound Nothing) = True
isSilent _ = False

instance Show Sound where
  show (Sound (Just (x,y,1,False))) = x++":"++(show y)
  show (Sound (Just (x,y,r,False))) = x++":"++(show y)++"*"++(show r)
  show (Sound (Just (x,y,1,True)))  = x++":"++(show y)++"?"
  show (Sound (Just (x,y,r,True)))  = x++":"++(show y)++"*"++(show r)++"?"
  show (Sound Nothing) = "~"

incrementNum :: Int -> Int
incrementNum n = n+1
-- deliberately not implementing Nothing to throw exception

decrementNum :: Int -> Int
decrementNum n = n-1

degrade :: Sound -> Sound
degrade (Sound (Just (a,b,c,_))) = Sound (Just (a,b,c,True))

unDegrade :: Sound -> Sound
unDegrade (Sound (Just (a,b,c,_))) = Sound (Just (a,b,c,False))

setDegrade :: Bool -> Sound -> Sound
setDegrade bool (Sound (Just (a,b,c,d))) = Sound (Just (a,b,c,bool))

setSamp :: String -> Sound -> Sound
setSamp newName (Sound (Just (a,b,c,d))) = Sound (Just (newName,b,c,d))

setN :: Int -> Sound -> Sound
setN n (Sound (Just (a,b,c,d))) = Sound (Just (a,n,c,d))

setReps :: Int -> Sound -> Sound
setReps n (Sound (Just (a,b,c,d))) = Sound (Just (a,b,n,d))

refresh :: Sound -> Sound -> Sound
refresh (Sound(sound)) silentSound = Sound(sound)
