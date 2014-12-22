{-#LANGUAGE TypeSynonymInstances,FlexibleInstances,GeneralizedNewtypeDeriving,RecordWildCards#-}
module Genetics where
import Ant
import Game

import Data.Foldable hiding (concat)
import Data.Array.Repa hiding ((++),concat)
import Data.Vector as Vec hiding ((++))
import Control.Monad as Monad
import Data.Functor
import Control.Monad.Random as Rand
import Data.List(sort,sortBy)
import Data.Function

data EvOptions = EvOptions{
                mutationChance::Float,
                elitePart::Float,
                maxGeneration::Int,
                indCount::Int,
                indLength::Int
                }

fitness::Field->Chromosome->Int
fitness f = score . Data.Foldable.foldl Game.update gs
        where gs = GameState{score = 0, dir = East, pos = Z:.0:.0, field = f}

newtype Chromosome' a = Chromosome' (Vec.Vector a) deriving (Functor,Foldable)
type Chromosome = Chromosome' (Expr Action)

instance Show Chromosome where
  show (Chromosome' chr) = Prelude.concat . Vec.toList $ (\e-> show e ++ " ") <$> chr
  
type Population = [Chromosome]

initGene::Int->GenRand Chromosome
initGene n = Chromosome' <$> Vec.replicateM n randAction
        where randAction::GenRand (Expr Action)
              randAction = randTree 1


initPopulation :: Int -> Int -> GenRand Population
initPopulation m n = Monad.replicateM m $ initGene n

crossover :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover ca@(Chromosome' a) cb@(Chromosome' b) 
  | Vec.length a <= 1 = return (ca, cb)
  | Vec.length a == 2 = return (Chromosome' $ a Vec.// [(1, b Vec.! 1)], Chromosome' $ b Vec.// [(0, a Vec.! 0)])
  | otherwise         = crossover3 ca cb

crossover3 :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover3 (Chromosome' a) (Chromosome' b)  = do
  [p1, p2, p3] <- sort <$> Monad.replicateM 3 (getRandomR (1, n - 2))
  let a' = Vec.concat [firsts p1 a, middles p1 p2 b, middles p2 p3 a, lasts p3 b]
  let b' = Vec.concat [firsts p1 b, middles p1 p2 a, middles p2 p3 b, lasts p3 a]
  return (Chromosome' a', Chromosome' b')
  where
    n = Vec.length a
    firsts = Vec.slice 0
    lasts p = Vec.slice p (n - p)
    middles p1 p2 = Vec.slice p1 (p2 - p1)

mutation :: Chromosome -> GenRand Chromosome
mutation (Chromosome' a) = do
  i <- getRandomR (0, Vec.length a - 1)
  q <- randTree 1 :: GenRand (Expr Action)
  return $ Chromosome' $ a Vec.// [(i, q)]
  
findBest::Field->Population->(Int,Chromosome)
findBest field pop = Data.Foldable.maximumBy (compare `on` fst) $ Prelude.zip (fitness field <$> pop) pop

randChoice :: Rational -> GenRand a -> GenRand a -> GenRand a
randChoice chance th els = join (Rand.fromList [(th, chance), (els, 1 - chance)])

nextPopulation::Field->EvOptions->Population->GenRand Population
nextPopulation field evo@(EvOptions {..}) pop = do
        newPop' <-liftM Prelude.concat $ Monad.replicateM (nonElite `div` 2) $ do
            a1 <- takeChr
            b1 <- takeChr
            (a2, b2) <- crossover a1 b1
            a3 <- applyMutation a2
            b3 <- applyMutation b2
            return [a3, b3]
        let newPop = elite ++ newPop'
        return $ if Prelude.length newPop <= Prelude.length pop then newPop else Prelude.tail newPop
        where   fits = toRational <$> fitness field <$> pop
                maxfit = Prelude.maximum fits
                chances = Prelude.zip pop ((/maxfit) <$> fits)
                takeChr = Rand.fromList chances
                applyMutation c = randChoice (toRational mutationChance) (mutation c) (return c)
                sortedPop = snd $ Prelude.unzip $ sortBy (compare `on` fst) $ Prelude.zip (fitness field <$> pop) pop 
                elite = Prelude.take (ceiling $ fromIntegral (Prelude.length pop) * elitePart) sortedPop
                nonElite = Prelude.length pop - Prelude.length elite
                
solveEv::StdGen->Field->EvOptions->IO Chromosome
solveEv gen field evo@(EvOptions{..}) = evalRandT solve' gen
    where solve' = do
              pop <- initPopulation indCount indLength 
              lastPop <- Monad.foldM (\a _->nextPopulation field evo a) pop [1 .. maxGeneration]
              return (snd $ findBest field lastPop)