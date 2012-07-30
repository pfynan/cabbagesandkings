{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeOperators #-}
module World where

import Data.Label
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map

type Coord = (Int, Int)

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


data Tile = Wall
          | Tree
          | Floor
          | Empty
          deriving (Eq,Ord,Show)

data Moveable = Moveable { _key      :: Int
                         , _position :: Coord
                         , _monster  :: Monster
                         } deriving (Eq,Ord,Show)

data Monster = StrayDog { _health :: Int} deriving (Eq,Ord,Show)

type Level = Array Coord Tile

data World = World
           { _hero     :: Coord
           , _monsters :: Map Int Moveable
           , _level    :: Level
           , _messages :: [String]
           } deriving Show


$(mkLabels [''World,''Moveable,''Monster])

initlevel ::  Level
initlevel = array ((0,0),(79,24)) [((x,y), 
                if x `rem` 5  == 0
                 && y `rem` 7 == 0
                    then Tree
                    else Floor)
                | x <- [0..79]
                , y <- [0..24]]


initmonsters :: Map Int Moveable
initmonsters = Map.fromList $ zipWith (\n c -> (n,(Moveable n c (StrayDog 10)))) [1..10]
                                [(x,y)
                                   | x <- [0..79]
                                   , x `rem` 6 == 0
                                   , y <- [0..24]
                                   , y `mod` 6 == 0]

inithero :: Coord
inithero = (2,2)

