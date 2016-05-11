module Sound where

data Sound = Sound (Maybe (String,Int,Int,Bool)) deriving (Eq)

silentSound = Sound (Nothing)

simpleSound :: String -> Sound
simpleSound x = Sound (Just (x,0,1,False))

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

degrade :: Sound -> Sound
degrade (Sound (Just (a,b,c,_))) = Sound (Just (a,b,c,True))

unDegrade :: Sound -> Sound
unDegrade (Sound (Just (a,b,c,_))) = Sound (Just (a,b,c,False))

instance Show Sound where
  show (Sound (Just (x,y,1,False))) = x++":"++(show y)
  show (Sound (Just (x,y,r,False))) = x++":"++(show y)++"*"++(show r)
  show (Sound (Just (x,y,1,True)))  = x++":"++(show y)++"?"
  show (Sound (Just (x,y,r,True)))  = x++":"++(show y)++"*"++(show r)++"?"
  show (Sound Nothing) = "~"

incrementN :: Sound -> Sound
incrementN (Sound (Just (a,b,c,d))) = Sound (Just (a,b+1,c,d))
-- deliberately not implementing Nothing to throw exception

decrementN :: Sound -> Sound
decrementN (Sound (Just (a,b,c,d))) = Sound (Just (a,b-1,c,d))

incrementRepeats :: Sound -> Sound 
incrementRepeats (Sound (Just (a,b,c,d))) = Sound (Just (a,b,c+1,d))

decrementRepeats :: Sound -> Sound
decrementRepeats (Sound (Just (a,b,1,d))) = Sound (Just (a,b,1,d))
decrementRepeats (Sound (Just (a,b,c,d))) = Sound (Just (a,b,c-1,d))

isSilent :: Sound -> Bool
isSilent (Sound Nothing) = True
isSilent _ = False
