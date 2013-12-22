{-# Language Rank2Types,MultiParamTypeClasses,ScopedTypeVariables,GADTs,RecordWildCards, KindSignatures #-}
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
import System.Mem

data Operation = Mutate | Crossover deriving (Enum)

data Generation a = Generation{
  genMembers :: Vector (Double,a),
  weakestIx :: (Double,Int),
  strongestIx :: (Double,Int)
  }

weakest gen = genMembers gen V.! (snd $ weakestIx gen)
strongest gen = genMembers gen V.! (snd $ strongestIx gen)

compareGen :: Ord a => (a,Vector b) -> (a, Vector b) -> Ordering
compareGen a b = compare (fst a) (fst b)

findStrongest gen = maximumByWithIx compareGen $ genMembers gen

findWeakest gen = minimumByWithIx compareGen $ genMembers gen 

data Env (m :: * -> *) a where
  Env :: (MonadRandom m, I.MonadRandom m) => {
    crossover :: [Vector a] -> m [Vector a],
    mutate :: Vector a -> m (Vector a),
    tournament :: forall x . Vector x -> m [Vector x],
    fitness :: (Vector a) -> Double,
    selOperation :: m Operation,
    target :: Vector a
                    
  } -> Env m a

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
    vDist :: m Double
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

performOperation Env{..} op gen = do
  elems <- liftM (map maxGen) $ tournament $ genMembers gen
  let
    elems' = elems
  newGen <- liftM (maximumBy cmpGen) $ operate elems
  let
    fNew = fitness newGen
  case compare (fNew - delta) (fst $ weakestIx gen) of
    LT -> return (False,gen)
    EQ -> return (False,repWeak False (fNew,newGen))
    _ ->  return (True,repWeak True (fNew,newGen))
    
  where
    repWeak stronger (f,newGen) =
      let
        rep = V.fromList [(snd $ weakestIx gen,(f,newGen))]
        gen' = V.update (genMembers gen) rep
        (strongIx,strong) = maximumByWithIx compareGen gen'
        (weakIx,weak) = minimumByWithIx compareGen gen'
        weakestIx'
          | stronger = (fst weak,weakIx)
          | otherwise = weakestIx gen
      in
       gen{
         genMembers = gen',
         weakestIx = weakestIx',
         strongestIx = (fst strong,strongIx)
         }
        
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

sumEnv :: (MonadRandom m,I.MonadRandom m) => Env m Int
sumEnv = Env{
  crossover = twoPointCrossoverAll 2.0,
  tournament = tournamentN 2 2,
  mutate = mutateDefault,
  fitness = fromIntegral . V.sum,
  selOperation = defSel,
  target = V.replicate 100 1
  }

scaledSumEnv :: (MonadRandom m,I.MonadRandom m) => Env m Int
scaledSumEnv = sumEnv{
  fitness = V.ifoldl (\s i x -> fromIntegral (i * x) + s) 0
  }
                   
trapRandomized = Env{
  mutate = mutateDefault,
  crossover = twoPointCrossoverAll 2.0,
  tournament = tournamentN 2 2,
  fitness = (ungroupedTrapFunction trapMapping 4 1),
  selOperation = defSel,
  target = V.replicate 100 1
}

trapRandomizedMutateBiased = 
  trapRandomized{
    selOperation = selFun (1/3)
    }
                     
nextGeneration env gen = do
  op <- selOperation env
  performOperation env op gen

nextGenerations count goal env gen = do
  (final,total,_) <- liftM snd $ flip runStateT (gen,0,0) $ nextGenerationsS env (fitness env goal) lim
  return (final,total)
  where
    lim = 10 * V.length (genMembers gen)

nextGenerationsS e@Env{..} goal lim = do
  (gen,total,c) <- get
  (improved,newGen) <- lift $ nextGeneration e gen
  let
    (c',stop)
      | goal == (fst $ strongestIx gen) = (0,True)
      | not improved && c >= lim = (0,True)
      | improved = (0,False)
      | otherwise = (c+1,False)
  put (newGen,total+1,c')
  unless stop $ nextGenerationsS e goal lim 
        
cmpFitness env a b = compare (fitness env a) (fitness env b)

-- | Run an experiment that uses an Evolutionary algorithm to
-- optimize a function by creating multiple generations and
-- and selecting the best members of each generation to find
-- the best solution
runExperiment env@Env{..} size = do
  init <- initPop
  gs' <- nextGenerations 0 target env init
  let
    (gs,iters) = gs'
    fittest =  snd $ strongest gs
  return (iters,fitness fittest,fittest,gs)

  where
    newV = V.fromList $ replicate (V.length target) (0 :: Int)
    initPop = do
      init <- V.mapM (const $ V.mapM (const $ getRandomR (0,1)) newV) $ V.fromList [1..size]
      let
        (weakIx,weak) = minimumByWithIx (cmpFitness env) init
        (strongIx,strong) = maximumByWithIx (cmpFitness env) init
      return $ Generation{
        genMembers = V.map (\x -> (realToFrac $ fitness x,x)) init,
        weakestIx = (fitness weak, weakIx),
        strongestIx = (fitness strong, strongIx)
        }

-- | Run an iteration of an experiment and collect the results
iteration env@Env{..} size = do
  (succ,sFt,sIters) <- get
  (iters,ft,strong,_) <- lift $ runExperiment env size
  let
    succ'= if strong == target then succ + 1 else succ
  put (succ',ft+sFt,iters+sIters)

-- | Function that runs a particular experiment multiple times with the
-- same parameters and reports statistics about the outcome
iterate _ _ [] = return []
iterate next rep (i:is) = do
  let
    (env,size) = next i
  runs <- mapM (const $ async $ flip runStateT (0,0,0) $ iteration env size) [1..rep]
  (succC,sFt,sIters) <- foldM cata (0,0,0) runs
  let
    repR = fromIntegral rep
    mSucc = fromIntegral succC / repR
    mFt = sFt / repR
    mIters = fromIntegral sIters / repR
  v <- liftM ((i,(mSucc,mFt,mIters)) :) $ iterate next rep is
  performGC
  return v
  where
    cata (succC,sFt,sIters) as = do
      (_,(succ,ft,iters)) <- wait as
      return (succC + succ,ft + sFt, iters + sIters)

-- | Fucntion used to group the results from running
-- an experiment multiple ti
collector elems = foldr cata ([],[],[],[],[]) elems
  where
    cata (i,(x,y,z)) (xs,ys,zs,its,succIts) = ((i,x):xs,(i,y):ys,(i,z):zs,(z,y):its,(z,x):succIts)

plotDeceptiveTrapFunctionRandomized  = plotTrapFunction title env 
  where
    title tSize pc' = "Randomized Deceptive Trap Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = trapRandomized{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC
    }

plotDeceptiveTrapFunctionOrd  = plotTrapFunction title env 
  where
    title tSize pc' = "Deceptive Trap Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = trapRandomized{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC,
      fitness = trapFunction 4 1
    }

plotTrapFunctionOrd  = plotTrapFunction title env 
  where
    title tSize pc' = "Trap Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = trapRandomized{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC,
      fitness = trapFunction 4 2.5
    }

plotTrapFunctionRand  = plotTrapFunction title env 
  where
    title tSize pc' = "Randomized Trap Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = trapRandomized{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC,
      fitness = (ungroupedTrapFunction trapMapping 4 2.5)
    }


plotSumFunction = plotTrapFunction title env
  where
    title tSize pc' = "Sum Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = sumEnv{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC
    }

plotScaledSumFunction = plotTrapFunction title env
  where
    title tSize pc' = "Scaled Sum Function: "
                      ++ " (Tournament: " ++ show tSize
                      ++ ", P-Crossover: " ++ (show pc') ++ ")"
    env tSize pC = scaledSumEnv{
      tournament = tournamentN 2 tSize,
      selOperation = selFun pC
    }
    

-- | Utility function that generates plots from running
-- the given experiment with different parameters several
-- times with multiple generation sizes
plotTrapFunction titleF env iters fact dir fName' = 
  mapM_ makePlot params
  where
    params = [(pC,tSize) | pC <- [0,1%2,1], tSize <- [1,2]]
    makePlot :: (Ratio Integer,Int) -> IO ()
    makePlot (pC,tSize) = do
      let
        fName = fName' ++ "_"++(show pC)++"_"++(show tSize)
        conds = [
          (True,(env,"Two Point Crossover"))
          ,(pC>0,(\x y -> (env x y){crossover = uniformCrossover 0.5},"Uniform Crossover (P-Flip: 0.5)"))
           ,(pC>0,(\x y -> (env x y){crossover = mapM mutateDefault >=> twoPointCrossoverAll 2.0},"Randomized Two Point Crossover")) 
          ]
        gens = [i*fact | i <- [1..iters]] :: [Integer]
        pc' = showRat pC :: Double
        title = titleF tSize pc'
        cata (p1,p2,p3,p4,p5) (exec,(envI,name))
          | exec = do
            (p1',p2',p3',p4',p5') <- liftM collector $ iterate (\i -> (envI tSize pC,i)) 30 gens
            return $ ((p1Line name,p1') : p1,(p2Line name,p2') : p2,(p3Line name,p3') : p3,(p2Line (name ++ " (Its)"),p4') : p4,(p1Line (name ++ " (Its)"),p5') : p5)
          | otherwise = return (p1,p2,p3,p4,p5)
          
        ps = PlotStyle{plotType=Lines,lineSpec=DefaultStyle 1} 
        p1Line n = ps{
          lineSpec = CustomStyle [LineTitle $ "Successful Runs (" ++ n ++ ")"]
        }
        p2Line n = ps{
          lineSpec=CustomStyle [LineTitle $ "Mean Fitness (" ++ n ++ ")"]
        }
        p3Line n = ps{
          lineSpec=CustomStyle [LineTitle $ "Mean Number Iterations (" ++ n ++ ")"]
        }
        plotFGen xLabel vals label adj = 
          plotListsStyle [Title title,YLabel label,XLabel xLabel,PNG $ dir </> (fName ++ adj ++ ".png")] vals
        plotF = plotFGen "Generation Size"
      (p1,p2,p3,p4,p5) <- foldM cata ([],[],[],[],[]) conds
      plotF p1 "Success Percentage" "_1"
      plotF p2 "Fitness" "_2"
      plotF p3 "Iterations" "_3"
      plotListsStyle [Title title,YLabel "Fitness",XLabel "Iterations", PNG $ dir </> (fName ++ "_4.png")] p4
      plotListsStyle [Title title,YLabel "Success Percentage",XLabel "Iterations", PNG $ dir </> (fName ++ "_5.png")] p5
     
plots = zip [1..] [
  (plotDeceptiveTrapFunctionRandomized,"Randomized Deceptive Trap Function")
  ,(plotDeceptiveTrapFunctionOrd,"Deceptive Trap Function")
  ,(plotTrapFunctionOrd,"Trap Function")
  ,(plotTrapFunctionRand,"Randomized Trap Function")
  ,(plotSumFunction,"Sum Function")
  ,(plotScaledSumFunction,"Scaled Sum Function")
  ]


executePlots numsStr out iters fact =
  mapM_ executePlot nums
  where
    nums = map (\x -> fromEnum x - fromEnum '0') numsStr
    executePlot i =
      case lookup i plots of
        Just (f',n) -> f' iters fact out n 
        Nothing -> putStrLn $ "The index " ++ show i ++ " is not a valid plot."

help = do
  name <- getProgName
  let
    txt = concat [
      "Genetic Programming Assignment:",
      "\n\tUsage: ",name," [args]\n",
      "\nValid arguments [args]:\n",
      "\t --help \tPrint this screen\n",
      "\t -i {Num} \tNumber of experiments\n",
      "\t -f {Num} \tGeneration Size Factor\n",
      "\t -d {Dir} \tOutput Directory (Must Exist)",
      "\t\t\tThe generation size at iteration i is i*f\n",
      "\t -p {Nums} \tThe desired plots as a list of integers\n",
      "\t\t\tThe valid plots are:\n"
      ]
          ++ concatMap (\(i,(_,n)) -> "\t\t\t* "++ (show i) ++ " " ++ n ++ "\n") plots
          ++ concat ["\n\nExample: ",name," -i 5 -m 10 -p 134"]
  putStrLn txt

getArg a = do
  args <- getArgs
  let
    v = do
      i <- findIndex (== a) args
      case length args of
        l | l > i+1 -> Just $ args !! (i+1)
        _ -> Nothing
  case v of
    Just v -> return $ Just v
    Nothing -> return Nothing

getArgOrFail arg = do
  a <- getArg arg
  case a of
    Just a' | length a' > 0 -> return a'
    _ -> do
      help
      error $ "Could not find argument: " ++ arg

operation = do
  it <- liftM read $ getArgOrFail "-i"
  f <- liftM read $ getArgOrFail "-f"
  p <- getArg "-p"
  dir <- getArg "-d"
  case (Just $ executePlots) <*> (p <|> Just ['1'..'6']) <*> (dir <|> Just "./") of
    Just plotter | it > 0 && f >0 -> plotter it f
    _ -> do
      help
      error $ "The given parameters are not valid."

main = do
  h <- liftM (findIndex (== "--help")) getArgs
  case h of
    Nothing -> operation
    Just _ -> help
