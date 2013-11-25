{-# Language Rank2Types,MultiParamTypeClasses #-}
module Main where

import Data.List (maximumBy)
import Control.Monad
import Control.Monad.Random
import Data.Random.Extras
import Data.Maybe (fromJust)
import Data.RVar (sampleRVar)
import Data.Random.Distribution.Uniform
import Data.Random (Distribution)
import qualified Data.Vector as V
import Data.Vector (Vector)

data Operation = Mutate | Crossover deriving (Enum)

instance Distribution StdUniform Operation where

data Env m = Env{
  crossover :: Vector Int -> m (Vector Int),
  mutate :: Vector Int -> m (Vector Int),
  tournament :: [Vector Int] -> m [[Vector Int]],
  fitness :: (Vector Int) -> Double,
  selOperation :: m Operation
  }

numBits :: MonadRandom m => m Integer 
numBits = numBits' 1
  where
    numBits' v = do      
      r <- getRandomR (0,1)
      if r > (1/2 :: Double)
        then numBits' (v+1)
        else return v

chooseBitsGen _ [] = return ([],[])
chooseBitsGen n vs 
  | n < 1 = return ([],vs)
  | otherwise = do
    (rs,r) <- fromJust $ choiceExtract vs
    (res',rs') <- chooseBitsGen (n-1) rs
    return (r:res',rs')

chooseBits n vs = liftM fst $ chooseBitsGen n vs

flipInt 0 = 1
flipInt _ = 0

flipBits bits 0 = return bits
flipBits bits n = do
  ixs' <- sampleRVar $ chooseBits n [0 .. V.length bits - 1]
  let ixs = V.fromList $ [(i,flipInt $ bits V.! i) | i <- ixs']
  return $ V.update bits ixs

mutateDefault bits = numBits >>= flipBits bits 

defSel :: MonadRandom m => m Operation
defSel = liftM toEnum $ getRandomR (0,1 :: Int)
  
tournamentN' num s vs = liftM fst $ foldM chooseFun ([],vs) [1..num]
  where
    chooseFun (sel,opts) _ = do
      (newSel,remOpts) <- chooseBitsGen s opts
      return (newSel:sel,remOpts)

tournamentN num s vs = sampleRVar $ tournamentN' num s vs

uniformCrossover p = V.mapM $ \e -> do
  mustFlip <- liftM (p>) $ getRandomR (0,1)
  return $ if mustFlip
           then flipInt e
           else e

-- performOperation :: Monad m => Env m -> Operation -> [[Int]] -> m [[Int]]
performOperation env op gen = do
  elems <- liftM (map maxGen) $ tournament env gen
  newGen <- liftM (maximumBy cmpGen) $ mapM operate elems
  case cmpGen newGen worse of
    LT -> return $ (False,worse : rest)
    EQ -> return $ (True,newGen : rest)
    GT -> return $ (True,newGen : rest)
    
  where
    (g:gs) = gen
    (worse,rest) = foldl (\(e,es) x ->
                           case cmpGen x e of
                             LT -> (x,e:es)
                             EQ -> (x,e:es)
                             GT -> (e,x:es)) (g,[]) gs
    cmpGen x y = compare (fitness env x) (fitness env y)
    maxGen = maximumBy cmpGen
    operate e =
      case op of
        Mutate -> mutate env e
        Crossover -> crossover env e

sumEnvironment c t = Env{
  crossover = c,
  tournament = t,
  mutate = mutateDefault,
  fitness = fromIntegral . V.sum,
  selOperation = defSel
  }
    
nextGeneration env gen = do
  op <- selOperation env
  performOperation env op gen

nextGenerations' count goal env gen = do
  (improved,newGen) <- nextGeneration env gen
  go improved newGen
  where
    go improved newGen
      | elem goal newGen = return newGen
      | not improved && count >= 10 * length gen = return gen
      | improved = nextGenerations' 0 goal env newGen
      | otherwise = nextGenerations' (count + 1) goal env newGen

runExperiment goal env size = do
  init <- mapM (const $ V.mapM (const $ getRandomR (0,1)) newV) [1..size]
  nextGenerations' 0 goal env init

  where
    newV = V.fromList $ replicate 100 (0 :: Int)

main = return ()
