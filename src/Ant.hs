{-# LANGUAGE GADTs,FlexibleInstances,DeriveFunctor,GeneralizedNewtypeDeriving#-}
module Ant where
import Control.Monad.Random as MR
import Data.Functor
import Control.Monad (join, liftM,liftM2, replicateM)
import System.IO
import Data.Vector as Vec hiding ((++))
import Data.List as List
import Data.Foldable

data Action = Forward|Left|Right deriving (Enum, Bounded)
data AntState = East|West|North|South deriving (Enum, Bounded)

instance Show Action where
        show Forward = "Forward"
        show Ant.Left = "Left"
        show Ant.Right = "Right"

instance Show AntState where
        show East = "E"
        show West = "W"
        show North = "N"
        show South = "S"

data Expr a where
        Act        :: Action->Expr Action
        If         :: Expr a->Expr a->Expr a
        HasCherry  :: Expr Bool
        NoCherry   :: Expr Bool
        Null       :: Expr Bool

instance Eq (Expr Bool) where
        HasCherry == HasCherry = True
        NoCherry == NoCherry = True
        _ == _ = False

eval:: Expr Bool->Expr a -> a
eval HasCherry (If t _) = eval HasCherry t
eval NoCherry (If _ e) =  eval NoCherry e
eval _ (Act a) = a

instance Show a=>Show (Expr a) where
        show (Act a) = show a
        show HasCherry = "+"
        show NoCherry = "-"
        show Null = "Null"
        show (If a b) = "if(<0>) then {" ++ show a ++ "} else {" ++ show b ++ "}"


class RandTree a where
  randTree :: Int->GenRand (Expr a)

randIf::Int->GenRand(Expr Action)
randIf n = liftM2 If randTree' randTree''
  where randTree' = randTree $ n-1
        randTree'' = randTree $ n-1

instance RandTree Action where
  randTree n
       |n==0 = join $ uniform $ List.tail vars
       |otherwise =  join $ uniform vars
       where vars = [randIf n,
                     return $ Act Forward,
                     return $ Act Ant.Left,
                     return $ Act Ant.Right]

turnAnt::AntState->Action->AntState
turnAnt North b = case b of
                        Ant.Left -> West
                        Ant.Right -> East
                        Forward -> North
turnAnt South b = case b of
                        Ant.Left -> East
                        Ant.Right -> West
                        Forward -> South
turnAnt East b = case b of
                        Ant.Left -> North
                        Ant.Right -> South
                        Forward -> East
turnAnt West b = case b of
                        Ant.Left -> South
                        Ant.Right -> North
                        Forward -> West
turnAnt d _ = d

type GenRand = RandT StdGen IO 

