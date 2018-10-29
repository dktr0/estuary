StransformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern,Event t (EditSignal a)))
transformedPatternWidget iTransPat _ = el "div" $ do
  deleteButton <- button "-"
  deleted <- toggle False deleteButton
  let regBuilder (i,iTrans) e = do
  	transf <- parameteredPatternTransformer iTransf never
	sPat <- dropdownPatternWidget (toSpecPat i) never
  	combinDyn (\t (s,e) -> (TransformedPattern t s,e))
  let nextBuilder = do
  	c <- patternCombinatorDropDown Merge never >>= mapDyn fst 
  	--mapDyn show c >>= dynText
  	(v,e) <- transformedPatternWidget (UntransformedPattern iSpecPat) never >>= splitDyn
  	combineDyn (,) v e >>= combineDyn (\comb (x,y)-> (x,y,comb)) 



  val <- resettableWidget regBuilder iTransPat never rebuildEvent



  val' <- combineDyn (\(v,e,c) (vr,er) -> TransformedPattern)

  where 
  	toSpecPat (TransformedPattern (Combine s _) _) = s
  	toSpecPat (TransformedPattern t x) = toSpecPat x 
  	toSpecPat (UntransformedPattern s) = s
  	iTransf
  	iSpecPat


    resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)


widget iTransPat _ = mdo
	(v,e) <- resettableWidget transformedPatternWidget iTransPat (ffilter (e)) >>= splitDyn
	return v

