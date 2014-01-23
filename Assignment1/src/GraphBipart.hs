graphFitness g bs = snd $ V.foldl countAdj (0,0) g
  where
    sumAdj i score j
      | bs V.! i == bs V.! j = score - 1
      | otherwise = score
    countAdj (i,score) vx = (i+1, V.foldl (sumAdj i) score vx)

localSearchIteration f bs = V.ifoldl iLocalSearch (f bs,bs) bs
  where
    lim = V.length bs - 1
    allIx = V.fromList [0 .. lim]
    swapBits (bestFt,best) i = do
      j <- get
      put (j+1)
      let
        bs' = V.modify (\v -> MV.swap v i j) bs
      case j of
        _ | j > lim -> return (bestFt,best)
        _ | bs V.! j /= bs V.! i && f bs' > bestFt -> swapBits (f bs',bs') i
        _ -> swapBits (bestFt,best) i
          
    iLocalSearch best i _ = fst $ runState (swapBits best i) (i+1)

localSearch f stop bs = fst $ runState (go bs) 1
  where
    go bs = do
      i <- get
      put (i+1)
      let
        (bsFt,bs') = localSearchIteration f bs
      case stop i of
        False | bsFt > f bs -> go bs'
        _ -> return bs
