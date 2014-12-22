{-# LANGUAGE RecordWildCards,GADTs#-}
module Game where
import Data.Array.Repa as Repa
import Data.Functor
import Data.Foldable (foldl)
import Ant

type Field = (Array U DIM2 Int)

showArr'(f) = unlines . toList $ traverse f (\(Z:.h:._) -> Z:.h) $ 
    \getter (Z:.irow) -> concat $ show . getter <$> ix2 irow <$> [0 .. fw-1]
    where (Z:._:.fw) = extent f

type FieldSize = DIM2

-- | Creating of empty field
cleanField :: FieldSize -> Field
cleanField s = computeUnboxedS $ fromFunction s (const 0)

data GameState = GameState{
                    score::Int,
                    pos::DIM2,
                    dir::AntState,
                    field::Field} deriving (Show)

safeInc (Z:.fh:.fw) (Z:.a:.b) c = case c of
        North -> if(a-1>=0) then (ix2 (a-1) b) else (ix2 (fh-1) b)
        South -> if(a+1<fh) then (ix2 (a+1) b) else (ix2 0 b)
        East  -> if(b+1<fw) then (ix2 a (b+1)) else (ix2 a 0)
        West  -> if(b-1>=0) then (ix2 a (b-1)) else (ix2 a (fw-1))

check::GameState->Expr Bool
check GameState{..} =
        if(field!nexPos==1) then HasCherry else NoCherry
        where nexPos = safeInc (extent field) pos dir

setVal a (Z:.ai:.aj) (val) = computeS $ traverse a id (\f (Z:.i:.j)->if i == ai && j == aj then val else f (Z :. i :. j) )
        
update::GameState->Expr Action->GameState
update st@(GameState{..}) (Act a) = case a of
        Ant.Left  -> GameState{score=score,pos=pos,dir = (turnAnt dir Ant.Left),field=field}
        Ant.Right -> GameState{score=score,pos=pos,dir = (turnAnt dir Ant.Right),field=field}
        Forward   -> GameState{score=score+addScr,pos=newP,dir = dir,field=newField}
        where newP = safeInc (extent field) pos dir
              addScr = if((check st) == HasCherry) then 1 else 0 
              newField = setVal field newP 0
      
update st iff = update st $ Act $ eval (check st) iff 