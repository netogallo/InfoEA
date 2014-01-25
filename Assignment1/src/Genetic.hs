{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (maximumBy,findIndex)
import Control.Monad
import Control.Monad.Random
import Data.Random.Extras
import Data.Maybe (fromJust)
import Data.RVar (sampleRVar)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Random.Source.IO
import qualified Data.Random.Internal.Source as I
import Data.Random.Distribution.Normal
import Data.Random.Distribution (rvar)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ratio
import GHC.Real (Ratio(..))
import qualified Data.Sequence as S
import qualified Data.Vector.Mutable as MV
import Debug.Trace
import Control.Monad.Trans.State
import Control.Monad.Trans (lift)
import Prelude hiding (iterate)
import Graphics.Gnuplot.Simple
import System.Environment (getArgs,getProgName)
import Control.Applicative
import System.FilePath
import Control.Concurrent.Async (wait,async)
import GraphParser
import qualified Data.HashTable.ST.Basic as HT
-- import Data.Hashable
import Data.Hashable
import Control.Monad.ST
import Data.Time.Clock
import Control.DeepSeq
import Control.Parallel.Strategies
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import System.IO
import qualified Data.IORef as R

data Result a =
  Result {
  iterations :: Int,
  maxFitness :: Double,
  maxValue :: a,
  lastGeneration :: Vector (Double, a),
  runTime :: Double
  } deriving (Show, Read)

data Results a =
  Results {
    experimentName :: String,
    results :: [Result a]
    } deriving (Show, Read)

instance (A.ToJSON a) => A.ToJSON (Results a) where
  toJSON Results{..} = A.object [
    "experimentName" A..= experimentName,
    "results" A..= results
    ]

instance (A.ToJSON a) => A.ToJSON (Result a) where
  toJSON Result{..} = A.object [
    "iterations" A..= iterations,
    "maxFitness" A..= maxFitness,
    "maxValue" A..= maxValue,
    "lastGeneration" A..= lastGeneration,
    "runTime" A..= runTime
    ]

instance NFData a => NFData (Result a) where
  rnf Result{..} = iterations `deepseq`
                   maxFitness `deepseq`
                   maxValue `deepseq`
                   lastGeneration `deepseq`
                   runTime `deepseq` ()

instance (Hashable a) => Hashable (Vector a) where
  hash v 
    | V.length v > 0 = V.foldl (\a b -> hashWithSalt a b) 0 v
    | otherwise = 0 
  hashWithSalt x v = hashWithSalt x (hash v)

data Operation = Mutate | Crossover deriving (Enum)

data Generation m a = Generation{
  gFitness :: a -> Double,
  genMembers :: HT.HashTable m a Double,
  weakest :: (a,Double),
  strongest :: (a,Double)
  }

data Graph = Vector (Vector Int)

True  `xor` False = True
False `xor` True  = True
_     `xor` _     = False

infinity = 10 ** (1024)

findMaximals :: HT.HashTable s a Double -> ST s ((a,Double),(a,Double))
findMaximals ht = HT.foldM maximalFold (empty,empty') ht
  where
    empty = (undefined,infinity)
    empty' = (undefined,-infinity)
    cmp op (k1,v1) (k2,v2)
      | op v1 v2 = (k2,v2)
      | otherwise = (k1,v1)
    
    maximalFold (minV,maxV) e = return $ (cmp (>) minV e,cmp (<) maxV e)

genInsert g v = do
  e <- HT.lookup members v
  case e of
    Just _ -> return (False,g)
    Nothing -> do
      HT.delete members $ fst $ weakest g
      HT.insert members v ft
      (minV,maxV) <- findMaximals members
      return (True,g{weakest=minV,strongest=maxV})

  where
    ft = gFitness g v
    members = genMembers g

genInsertStrong g v
  | stronger = genInsert g v
  | otherwise = return (False,g)
  where
    stronger = snd (weakest g) <= gFitness g v

getMembers ht = do
  xs <- HT.foldM (\l (k,v) -> return ((v,k) : l)) [] ht
  return $ V.fromList xs

randBS len = V.mapM (const getRandom) $ V.fromList [1 .. len]

initGen initM ft size len = do
  ht <- lift $ HT.newSized size
  populate size ht
  (minV,maxV) <- lift $ findMaximals ht
  return $ Generation{
    weakest = minV,
    strongest = maxV,
    genMembers = ht,
    gFitness = ft
    }

  where
    populate size ht
      | size <= 0 = return ht
      | otherwise = do
        bs <- initM
        e <- lift $ HT.lookup ht bs
        case e of
          Just _ -> populate size ht
          Nothing -> do
            lift $ HT.insert ht bs (ft bs)
            populate (size-1) ht 
  

compareGen :: Ord a => (a, Vector b) -> (a, Vector b) -> Ordering
compareGen a b = compare (fst a) (fst b)

data StopCond a = StopCond{
  currentFittest :: a,
  currentFittness :: Double,
  currentGeneration :: Vector (Double, a),
  numIterations :: Int,
  itersNoImprovement :: Int
  }

data Env m g a where
  Env :: NFData a => {
    crossover :: [Vector a] -> RandT g m [Vector a],
    mutate :: Vector a -> RandT g m (Vector a),
    ls :: Vector a -> RandT g m (Vector a),
    tournament :: forall x . Vector x -> RandT g m [Vector x],
    fitness :: (Vector a) -> Double,
    selOperation :: RandT g m Operation,
    target :: Double,
    stopCond :: StopCond (Vector a) -> Bool,
    initializer :: RandT g m (Vector a)
  } -> Env m g a


graphFitness vx prev bs = V.foldl countAdj (initScore) is
  where
    sumAdj bits i score j
      | bits V.! i == bits V.! j = score - 1
      | otherwise = score
    countAdjDual prevBs score i =
      let
        vs = (vx V.! i)
        scoreF bs' i' = V.foldl (sumAdj bs' i') 0 $ vx V.! i'
        oldScore = scoreF prevBs i * 2
        newScore = scoreF bs i * 2
        
      in (score - oldScore + newScore)

    countAdjSingle score i = (V.foldl (sumAdj bs i) score $ vx V.! i)

    (initScore,countAdj,is) = case prev of
      Just (ftPrevBs, prevBs) -> let
        reComp = V.map fst $ V.filter snd $ V.izipWith (\i x y -> (i,x `xor` y)) prevBs bs
        in
         (ftPrevBs, countAdjDual prevBs,reComp)
      Nothing -> (0, countAdjSingle, V.fromList [0..(V.length vx - 1)])

localSearchIteration ::
  (NFData b, Eq b) =>
  (Maybe (Double,Vector b) -> Vector b -> Double) ->
  Vector b ->
  (Double, Vector b)
localSearchIteration f bs = V.ifoldl iLocalSearch (f Nothing bs,bs) bs
  where
    lim = V.length bs - 1
    allIx = V.fromList [0 .. lim]
    validSwaps sol@(bestFt,_) j i =
      let
        calcFt x = (f (Just sol) x,x)
      in 
       V.fromList [
         calcFt $ V.update bs (V.fromList [(j', bs V.! i), (i, bs V.! j')])
         | j' <- [j  .. lim]
         , bs V.! j' /= bs V.! i
         ]

    iLocalSearch best i _ =
      let
        neighborhood = withStrategy (parTraversable rdeepseq) $ validSwaps best (i+1) i
        fittest = V.maximumBy (\a b -> compare (fst a) (fst b)) neighborhood
        nSize = V.length neighborhood
      in
       if | nSize < 1 -> best
          | (fst fittest) > fst best -> fittest
          | otherwise -> best

localSearch f stop bs = fst $ runState (go bs) 1
  where
    go bs = do
      i <- get
      put (i+1)
      let
        sol@(bsFt,bs') = localSearchIteration f bs
      case stop i of
        False | bsFt > f (Just sol) bs -> go bs'
        _ -> return bs

-- | Swap two different randomly selected bits in a vector
-- Warning: function does not check wether the vector has
-- different values at all. Such vector will result in an
-- infinite loop
balancedOnePointMutation v = do
  (r1, r2) <- (,) <$> getRandomR (0,l) <*> getRandomR (0,l)
  return $ swapped r1 r2
  where
    l = V.length v - 1
    swapped r1 r2
      | v V.! r1 == v V.! r2 = swapped r1 ((r2 + 1) `mod` (l + 1))
      | otherwise = V.update v $ V.fromList [(r1,v V.! r2),(r2, v V.! r1)]

multiBalancedOnePointMutation c v = foldM (\v' _ -> balancedOnePointMutation v') v [1 .. c]

balancedBsInitializer :: (Functor m, Monad m, RandomGen g) => Int -> RandT g m (Vector Bool)
balancedBsInitializer n =
  foldM (\s _ -> balancedOnePointMutation s) v [1 .. n]
  where
    v = V.fromList [i `mod` 2 == 0 | i <- [1 .. n]]

balancedUniformCrossoverAll (va:vb:vs) = do
  vx <- balancedUniformCrossover va vb
  liftM (vx :) $ balancedUniformCrossoverAll vs
balancedUniformCrossoverAll x = return []

balancedUniformCrossover va vb
  | la > lb = balancedUniformCrossover vb va
  | otherwise = do
    seed <- getRandom 
    let gen = mkStdGen seed
    mBalanced gen va vb
  where
    la = V.length va
    lb = V.length vb

test = do
  let ls = (replicate 10 True) ++ (replicate 10 False)
  va <- sampleRVar $ liftM V.fromList $ shuffle ls
  vb <- sampleRVar $ liftM V.fromList $ shuffle ls
  g <- trace "-------" getStdGen
  vx <- mBalanced g (trace (show va) va) (trace (show vb) vb)
  if V.length (V.filter id (trace (show vx) vx)) /= 10 then error ("Bad: " ++ (show va) ++ (show vb)) else return ()

data SelState = AnyBit | TrueBit | FalseBit deriving (Eq,Enum)

-- | Efficient uniform crossover which assures the resulting vector
-- is balanced as long as both parents are balanced and equal lenght
-- going through the vector once and doing the modification in place 
mBalanced gen va vb =
  return $ V.modify (\v -> runRandT (runStateT (V.foldM (mBalancedFold va vb v) () ixs) AnyBit) gen >> return ()) vb
  where
    ixs = V.imap (\i _ -> i) vb
                                         

mBalancedFold va vb v _ i = do
      c <- get
      p <- lift getRandom
      case (va V.! i, vb V.! i) of
        (a,b) | a == b -> write v i a
        _ | c == TrueBit -> do
          put AnyBit
          write v i True
        _ | c == FalseBit -> do
          put AnyBit
          write v i False
        _ | p -> (put FalseBit) >> write v i True
        _ -> (put TrueBit) >> write v i False

  where
    write v i x = lift $ lift $ MV.write v i x

numBits :: MonadRandom m => m Integer 
numBits = numBits' 1
  where
    numBits' v = do      
      r <- getRandomR (0,1)
      if r > (1/2 :: Double)
        then numBits' (v+1)
        else return v

vNormal v = rvar $ Normal mean sigma
  where
    vLen = (fromIntegral $ V.length v)
    mean = vLen / 2
    sigma = vLen / 6

vSample :: forall m a . I.MonadRandom m => Vector a -> m a
vSample v = do
  i' <- liftM round vDist
  let i
        | i' >= vLen = vLen - 1
        | i' < 0 = 0
        | otherwise = i'
  return $ v V.! i
  where
    vDist :: I.MonadRandom m => m Double
    vDist = sampleRVar $ vNormal v
    vLen = V.length v

choiceExtractVec v = do
  i <- getRandomR (0,vMax)
  let
    v' = V.modify (\v -> MV.swap v i vMax) v 
  return $ (v V.! i,V.take vMax v')
  where
    vMax = V.length v - 1

chooseBitsGen n vs = do
  (res,vs') <- chooseBitsGen' n vs
  return (V.fromList res,vs')

chooseBitsGen' n vs
  | V.length vs == 0 = return ([],V.empty)
  | n < 1 = return ([],vs)
  | otherwise = foldM cata ([],vs) [1..n] 
  where
    cata (sel,rem) _ = do
      (x,xs) <- choiceExtractVec vs
      return (x:sel,xs)
    
chooseBits n vs = liftM fst $ chooseBitsGen n vs

flipInt 0 = 1
flipInt _ = 0

showRat :: Ratio Integer -> Double
showRat (a :% b) = fromIntegral a / fromIntegral b 

flipBits bits 0 = return bits
flipBits bits n = do
  ixs' <- chooseBits n $ V.fromList [0 .. V.length bits - 1]
  let ixs = V.map (\i -> (i,flipInt $ bits V.! i))  ixs'
  return $ V.update bits ixs

mutateDefault bits = numBits >>= flipBits bits 

mutateNoShame f bits = liftM (*f) numBits >>= flipBits bits

selFun :: (I.MonadRandom m) => Ratio Integer ->  m Operation
selFun r = sampleRVar $ choiceSeq $ mutateR S.>< mutateC
  where
    (cross :% total) = r
    mutateC = S.replicate (abs $ fromIntegral cross) Crossover
    mutateR
      | total == 0 && cross == 0 = S.fromList [Mutate]
      | otherwise = S.replicate (abs $ fromIntegral $ total - cross) Mutate

defSel :: I.MonadRandom m => m Operation
defSel = selFun $ 1/2
          
tournamentN' num s vs = liftM fst $ foldM chooseFun ([],vs) [1..num]
  where
    chooseFun (sel,opts) _ = do
      (newSel,remOpts) <- chooseBitsGen s opts
      return (newSel:sel,remOpts)

tournamentN num s vs = tournamentN' num s vs

uniformCrossoverVector p (v1,v2) = V.mapM selector $ V.zip v1 v2 
  where
    selector (a,b) = do
      mustFlip <- liftM (p>) $ getRandomR (0,1)
      return $ if mustFlip then a else b


pairs (x:y:xs) = (x,y) : pairs xs
pairs (x:_) = [(x,x)]
pairs _ = []

uniformCrossover :: (MonadRandom m,I.MonadRandom m, Num a, Eq a) => Double -> [Vector a] -> m [Vector a]
uniformCrossover p vs = mapM (uniformCrossoverVector p) $ pairs vs 

unGroupTrap mapping v = V.map (\i -> v V.! i) mapping

groupTrap mapping v = V.update v $ V.fromList reLoc
  where
    vLen = V.length v - 1
    reLoc = [(mapping V.! i,v V.! i) | i <- [0 .. vLen]]

bFun d v
  | V.length v == V.sum v = fromIntegral $ V.sum v
  | otherwise = k - d - (k - d)/(k - 1) * fromIntegral (V.sum v)
  where
    k = fromIntegral $ V.length v

ungroupedTrapFunction m k d =
  (trapFunction k d) . (groupTrap m)

trapMapping = V.fromList $ unsafePerformIO $ sampleRVar $ shuffle [0..99]

trapFunction k d v = sum $ map bMap js

  where
    vMax = V.length v `div` k - 1
    js = [i | i <- [0 .. vMax]]
    bMap j = bFun d $ V.slice (k*j) k v

fit minV maxV v
  | maxV < v = maxV
  | minV > v = minV
  | otherwise = v

zipper (x1:x2:xs) = (x1,x2) : zipper xs
zipper _ = []

delta = 0.0001 :: Double

twoPointCrossoverAll :: (MonadRandom m,I.MonadRandom m) => Double -> [Vector a] -> m [Vector a]
twoPointCrossoverAll sigFact =
  return . zipper
  >=> mapM (uncurry $ twoPointCrossover sigFact)

twoPointCrossover :: forall m a . (MonadRandom m,I.MonadRandom m)
  => Double -> Vector a -> Vector a -> m (Vector a) 
twoPointCrossover sigFact v1 v2'
  | v1Len < v2Len = twoPointCrossover sigFact v2' v1
  | otherwise = do
    i0 <- getRandomR (0,v1Len) :: m Int
    iN' <- sampleRVar $ rvar $ Normal vMean vSig :: m Double
    let
      iN = i0 + fit 0 v1Len (round iN') :: Int
      rep = V.fromList [(i `mod` v1Len, v2 V.! (i `mod` v2Len)) | i <- [i0 .. iN]]
    return $ V.update v1 rep
    
  where
    v1Len = V.length v1 :: Int
    v2Len = V.length v2'
    v2 = V.generate v1Len (\i -> v2' V.! (i `mod` v2Len)) 
    vMid = v1Len `div` 4
    vMean = fromIntegral vMid :: Double
    vSig = fromIntegral vMid / sigFact :: Double

-- | Function that applies the selected operator on
-- a random selection of memebers from the current
-- population and evaluates wether an improvement
-- was achieved. In the positive case the function
-- adds the member to the new generation
performOperation Env{..} op gen = do
  members <- lift $ getMembers $ genMembers gen
  elems <- liftM (map maxGen) $ tournament $ members
  es <- operate elems
  newGen <- liftM (maximumBy cmpGen) $
            operate elems >>= mapM ls
  lift $ genInsertStrong gen newGen
    
  where
        
    cmpGen x y =
      let
        fa = fitness x
        fb = fitness y
      in
       compare fa fb
    maxGen = V.maximumBy compareGen
    operate elems' =
      let
        elems = map snd elems'
      in
       case op of
         Crossover -> crossover elems
         Mutate -> mapM mutate elems
         

graphBuilder adj = V.fromList $ map (\(i,is) -> V.fromList $ map pred is) adj

loadGraph f = liftM graphBuilder $ readGraph f

maximumByWithIx cmp = minimumByWithIx (\x y -> ord' $ cmp x y)
  where
    ord' LT = GT
    ord' GT = LT
    ord' a = a
    

minimumByWithIx cmp v =
  V.ifoldl minCmp (0, v V.! 0) v
  where
    minCmp (ix,x) ix' x'
      | cmp x x' == GT = (ix',x')
      | otherwise = (ix,x)

                     
nextGeneration env gen = do
  op <- selOperation env
  performOperation env op gen

-- | Function that evolves the generations and counts the number
-- of generations that have evolved until 
-- a solution is found or the stop criteria is met 
nextGenerations count env gen = do
  len <- liftM (V.length) $ lift $ getMembers $ genMembers gen
  (final,total,_) <- liftM snd $ flip runStateT (gen,0,0) $ nextGenerationsS env (target env) len
  return (final,total)

-- | Helper function that keeps the count and current generation
-- in memory used by the nextGenerations
-- function. This function is implemented to use
-- tail recursion and the state monad for efficiency
nextGenerationsS e@Env{..} goal lim = do
  (gen,total,c) <- get
  (improved,newGen) <- lift $ nextGeneration e gen
  members <- lift $ lift $ getMembers $ genMembers newGen
  let
    c'| improved = 0
      | otherwise = c+1
    stop = stopCond $ StopCond{
      currentFittest = fst $ strongest newGen ,
      currentFittness = snd $ strongest newGen,
      currentGeneration = members,
      numIterations = total,
      itersNoImprovement = c'
      }                    
  put (newGen,total+1,c')
  unless stop $ nextGenerationsS e goal lim 
        
-- cmpFitness env a b = compare (fitness env a) (fitness env b)

-- | Run an experiment that uses an Evolutionary algorithm to
-- optimize a function by creating multiple generations and
-- and selecting the best members of each generation to find
-- the best solution
runExperimentM env@Env{..} size bsSize = do
  init <- initGen initializer fitness size bsSize
  gs' <- nextGenerations 0 env init
  let
    (gs,iters :: Int) = gs'
    fittest =  fst $ strongest gs

  members <- lift $ getMembers $ genMembers gs
  return $ Result{
    iterations = iters,
    maxFitness = fitness fittest,
    maxValue = fittest,
    lastGeneration = members,
    runTime = 0
    }

runExperiment :: (Show a, Eq a, Hashable a, RandomGen t,Random a) =>
                 Int -> Int -> (forall s . Env (ST s) t a)
                 -> t -> Result (Vector a)
runExperiment size bsSize env g  =
  runST $ liftM fst $ (runRandT (runExperimentM env size bsSize) g) 

benchmarkExperiment exp = do
  gen <- getStdGen
  t0 <- getCurrentTime
  let
    res = exp gen
  res `deepseq` (return ())
  tn <- getCurrentTime
  return $ res{runTime = fromRational
                          (toRational $ diffUTCTime tn t0)
              }

mslsEnv :: (RandomGen g, Monad m, Functor m) => Vector (Vector Int) -> Env m g Bool
mslsEnv g = Env {
  crossover = undefined,
  mutate = return,
  ls = return . ls,
  tournament = return . (:[]),
  fitness = ft,
  selOperation = return Mutate,
  target = 0,
  stopCond = \_ -> True,
  initializer = balancedBsInitializer (V.length g)
                   
  }
  where
    ft = graphFitness g Nothing
    ls :: Vector Bool -> Vector Bool
    ls = localSearch (graphFitness g) (const False)

mutateLSEnv num c g = (mslsEnv g){

  mutate = multiBalancedOnePointMutation c,
  -- Stop once there is no LS improvement
  stopCond = \st -> numIterations st > num
  }

genLSEnv num g = (mslsEnv g){
  crossover = balancedUniformCrossoverAll,
  selOperation = return Crossover,
  tournament = tournamentN 2 1,
  stopCond = \st -> itersNoImprovement st > num
  }

-- | Wrapper for the multi-start local search
msls :: RandomGen g =>
  Int -- | Number of starting points
  -> Vector (Vector Int) -- | Graph as Adjacency list
  -> g -- | Random number generator
  -> Result (Vector Bool) -- | The result of the search 
msls sp gr gen = runExperiment sp (V.length gr) (mslsEnv gr) gen 

mutateLocalSearch ::
  RandomGen g =>
  Int -- | Number of local search/mutations performed
  -> Int -- | Number of vertexes to be mutated
  -> Vector (Vector Int) -- | Graph
  -> g -- | Random number generator
  -> Result (Vector Bool)
mutateLocalSearch num dist gr gen =
  runExperiment 1 (V.length gr) (mutateLSEnv num dist gr) gen


geneticLocalSearch size num gr gen =
  runExperiment size (V.length gr) (genLSEnv num gr) gen

collectExperiments num name expr =
  liftM (Results name) $ mapM (const $ benchmarkExperiment expr) [1..num]

numRuns = 30
genSize = 1000

experiments :: Vector (Vector Int) -> IO [Results (Vector Bool)]
experiments gr = sequence [
  collectExperiments numRuns "Mulit Start LS" $ msls genSize gr,
  mutateLS 2,
  mutateLS 3,
  mutateLS 4,
  genLS 50,
  genLS 100
  ]


  where
    mutateLS mSize =
      collectExperiments numRuns ("Mutate LS " ++ (show mSize) ++ "pts") $ mutateLocalSearch genSize mSize gr
    genLS genSize = collectExperiments numRuns
                    ("Genetic LS (" ++ (show (genSize :: Int)) ++ ")") $
                    geneticLocalSearch genSize 5 gr

mainRunner graph out = do
  
  gen <- getStdGen
  gr <- loadGraph graph 
  res <- experiments gr
  withFile out WriteMode $ \h -> BS.hPutStr h (A.encode res)


main = do
  args <- getArgs
  name <- getProgName
  case args of
    [graph,out] -> mainRunner graph out
    _ -> print $ "Usage: " ++ name ++ " {Graph File} {Out File}"

-- -- | Run an iteration of an experiment and collect the results
-- iteration env@Env{..} size = do
--   (succ,sFt,sIters) <- get
--   (iters,ft,strong,_) <- lift $ runExperiment env size
--   let
--     succ'= if strong == target then succ + 1 else succ
--   put (succ',ft+sFt,iters+sIters)

-- -- | Function that runs a particular experiment multiple times with the
-- -- same parameters and reports statistics about the outcome
-- iterate _ _ [] = return []
-- iterate next rep (i:is) = do
--   let
--     (env,size) = next i
--   runs <- mapM (const $ async $ flip runStateT (0,0,0) $ iteration env size) [1..rep]
--   (succC,sFt,sIters) <- foldM cata (0,0,0) runs
--   let
--     repR = fromIntegral rep
--     mSucc = fromIntegral succC / repR
--     mFt = sFt / repR
--     mIters = fromIntegral sIters / repR
--   v <- liftM ((i,(mSucc,mFt,mIters)) :) $ iterate next rep is
--   -- performGC
--   return v
--   where
--     cata (succC,sFt,sIters) as = do
--       (_,(succ,ft,iters)) <- wait as
--       return (succC + succ,ft + sFt, iters + sIters)

-- -- | Fucntion used to group the results from running
-- -- an experiment multiple ti
-- collector elems = foldr cata ([],[],[],[],[]) elems
--   where
--     cata (i,(x,y,z)) (xs,ys,zs,its,succIts) = ((i,x):xs,(i,y):ys,(i,z):zs,(z,y):its,(z,x):succIts)

-- plotDeceptiveTrapFunctionRandomized  = plotTrapFunction title env 
--   where
--     title tSize pc' = "Randomized Deceptive Trap Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = trapRandomized{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC
--     }

-- plotDeceptiveTrapFunctionOrd  = plotTrapFunction title env 
--   where
--     title tSize pc' = "Deceptive Trap Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = trapRandomized{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC,
--       fitness = trapFunction 4 1
--     }

-- plotTrapFunctionOrd  = plotTrapFunction title env 
--   where
--     title tSize pc' = "Trap Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = trapRandomized{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC,
--       fitness = trapFunction 4 2.5
--     }

-- plotTrapFunctionRand  = plotTrapFunction title env 
--   where
--     title tSize pc' = "Randomized Trap Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = trapRandomized{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC,
--       fitness = (ungroupedTrapFunction trapMapping 4 2.5)
--     }


-- plotSumFunction = plotTrapFunction title env
--   where
--     title tSize pc' = "Sum Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = sumEnv{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC
--     }

-- plotScaledSumFunction = plotTrapFunction title env
--   where
--     title tSize pc' = "Scaled Sum Function: "
--                       ++ " (Tournament: " ++ show tSize
--                       ++ ", P-Crossover: " ++ (show pc') ++ ")"
--     env tSize pC = scaledSumEnv{
--       tournament = tournamentN 2 tSize,
--       selOperation = selFun pC
--     }
    

-- -- | Utility function that generates plots from running
-- -- the given experiment with different parameters several
-- -- times with multiple generation sizes
-- plotTrapFunction titleF env iters fact dir fName' = 
--   mapM_ makePlot params
--   where
--     params = [(pC,tSize) | pC <- [0,1%2,1], tSize <- [1,2]]
--     makePlot :: (Ratio Integer,Int) -> IO ()
--     makePlot (pC,tSize) = do
--       let
--         fName = fName' ++ "_"++(show pC)++"_"++(show tSize)
--         conds = [
--           (True,(env,"Two Point Crossover"))
--           ,(pC>0,(\x y -> (env x y){crossover = uniformCrossover 0.5},"Uniform Crossover (P-Flip: 0.5)"))
--            ,(pC>0,(\x y -> (env x y){crossover = mapM mutateDefault >=> twoPointCrossoverAll 2.0},"Randomized Two Point Crossover")) 
--           ]
--         gens = [i*fact | i <- [1..iters]] :: [Integer]
--         pc' = showRat pC :: Double
--         title = titleF tSize pc'
--         cata (p1,p2,p3,p4,p5) (exec,(envI,name))
--           | exec = do
--             (p1',p2',p3',p4',p5') <- liftM collector $ iterate (\i -> (envI tSize pC,i)) 30 gens
--             return $ ((p1Line name,p1') : p1,(p2Line name,p2') : p2,(p3Line name,p3') : p3,(p2Line (name ++ " (Its)"),p4') : p4,(p1Line (name ++ " (Its)"),p5') : p5)
--           | otherwise = return (p1,p2,p3,p4,p5)
          
--         ps = PlotStyle{plotType=Lines,lineSpec=DefaultStyle 1} 
--         p1Line n = ps{
--           lineSpec = CustomStyle [LineTitle $ "Successful Runs (" ++ n ++ ")"]
--         }
--         p2Line n = ps{
--           lineSpec=CustomStyle [LineTitle $ "Mean Fitness (" ++ n ++ ")"]
--         }
--         p3Line n = ps{
--           lineSpec=CustomStyle [LineTitle $ "Mean Number Iterations (" ++ n ++ ")"]
--         }
--         plotFGen xLabel vals label adj = 
--           plotListsStyle [Title title,YLabel label,XLabel xLabel,PNG $ dir </> (fName ++ adj ++ ".png")] vals
--         plotF = plotFGen "Generation Size"
--       (p1,p2,p3,p4,p5) <- foldM cata ([],[],[],[],[]) conds
--       plotF p1 "Success Percentage" "_1"
--       plotF p2 "Fitness" "_2"
--       plotF p3 "Iterations" "_3"
--       plotListsStyle [Title title,YLabel "Fitness",XLabel "Iterations", PNG $ dir </> (fName ++ "_4.png")] p4
--       plotListsStyle [Title title,YLabel "Success Percentage",XLabel "Iterations", PNG $ dir </> (fName ++ "_5.png")] p5
     
-- plots = zip [1..] [
--   (plotDeceptiveTrapFunctionRandomized,"Randomized Deceptive Trap Function")
--   ,(plotDeceptiveTrapFunctionOrd,"Deceptive Trap Function")
--   ,(plotTrapFunctionOrd,"Trap Function")
--   ,(plotTrapFunctionRand,"Randomized Trap Function")
--   ,(plotSumFunction,"Sum Function")
--   ,(plotScaledSumFunction,"Scaled Sum Function")
--   ]


-- executePlots numsStr out iters fact =
--   mapM_ executePlot nums
--   where
--     nums = map (\x -> fromEnum x - fromEnum '0') numsStr
--     executePlot i =
--       case lookup i plots of
--         Just (f',n) -> f' iters fact out n 
--         Nothing -> putStrLn $ "The index " ++ show i ++ " is not a valid plot."

-- help = do
--   name <- getProgName
--   let
--     txt = concat [
--       "Genetic Programming Assignment:",
--       "\n\tUsage: ",name," [args]\n",
--       "\nValid arguments [args]:\n",
--       "\t --help \tPrint this screen\n",
--       "\t -i {Num} \tNumber of experiments\n",
--       "\t -f {Num} \tGeneration Size Factor\n",
--       "\t -d {Dir} \tOutput Directory (Must Exist)",
--       "\t\t\tThe generation size at iteration i is i*f\n",
--       "\t -p {Nums} \tThe desired plots as a list of integers\n",
--       "\t\t\tThe valid plots are:\n"
--       ]
--           ++ concatMap (\(i,(_,n)) -> "\t\t\t* "++ (show i) ++ " " ++ n ++ "\n") plots
--           ++ concat ["\n\nExample: ",name," -i 5 -m 10 -p 134"]
--   putStrLn txt

-- getArg a = do
--   args <- getArgs
--   let
--     v = do
--       i <- findIndex (== a) args
--       case length args of
--         l | l > i+1 -> Just $ args !! (i+1)
--         _ -> Nothing
--   case v of
--     Just v -> return $ Just v
--     Nothing -> return Nothing

-- getArgOrFail arg = do
--   a <- getArg arg
--   case a of
--     Just a' | length a' > 0 -> return a'
--     _ -> do
--       help
--       error $ "Could not find argument: " ++ arg

-- operation = do
--   it <- liftM read $ getArgOrFail "-i"
--   f <- liftM read $ getArgOrFail "-f"
--   p <- getArg "-p"
--   dir <- getArg "-d"
--   case (Just $ executePlots) <*> (p <|> Just ['1'..'6']) <*> (dir <|> Just "./") of
--     Just plotter | it > 0 && f >0 -> plotter it f
--     _ -> do
--       help
--       error $ "The given parameters are not valid."

-- main = do
--   h <- liftM (findIndex (== "--help")) getArgs
--   case h of
--     Nothing -> operation
--     Just _ -> help
