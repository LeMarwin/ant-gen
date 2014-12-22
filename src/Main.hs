module Main where

import Ant
import Game
import Genetics
import Control.Monad.Random
import Control.Monad (join,mapM_)
import Data.Array.Repa as Repa hiding ((++)) 
import Data.Array.Repa.Eval as RE
import Control.Applicative  
import Data.Foldable hiding (mapM_)
import System.IO
import Data.Vector as Vec hiding ((++))


chk::(Eq a)=>[a]->[a]->Bool
chk a b = Prelude.and $ Prelude.zipWith (==) a b


stdOpts::EvOptions
stdOpts = EvOptions{
                mutationChance = 0.3,
                elitePart = 0.2,
                maxGeneration = 10,
                indCount = 100,
                indLength = 50}

main::IO()
main = do
   fh <- openFile "./src/data.txt" ReadMode
   raw <- hGetContents fh
 --let e = Data.Foldable.foldl (turnAnt) North $ (eval HasCherry) `fmap` a
   let w = (\e -> if(e=='1') then 1 else 0) `fmap` (Prelude.concat . lines $ raw)
   let findata = (RE.fromList (Z:.33:.33) w) ::Field
   
   rng <- newStdGen
   a <- solveEv rng findata stdOpts
   print a
   print $ fitness findata a
   hClose fh