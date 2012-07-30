--file: Main.hs
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeOperators #-}
module Main where

import Prelude hiding (id,(.))
import System.Exit (exitSuccess)

import Reactive.Banana
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Category
import Data.Monoid
import Data.Label
import Data.Array
import UI.HSCurses.Curses
import Control.Monad(forever)
import Control.Exception(bracket_)

import World
import Draw
import Data.List(delete)

type EventSource a = (AddHandler a, a -> IO ())


data Input = MoveUp
           | MoveDown
           | MoveLeft
           | MoveRight
           | Exit
           | Redraw
           deriving (Eq,Ord)

type KeyMap = Map Char Input

main ::  IO c
main = bracket_ initCurses (endWin >> putStrLn "Be seeing you...")  $ do
    
    echo False

    eventsource <- newAddHandler
    compile (network eventsource) >>= actuate


    let kp (KeyChar k) = snd eventsource k
        kp _ = return ()

    kp (KeyChar 'r')

    forever $ getCh >>= kp

keyMap :: KeyMap
keyMap = 'k' ~> MoveUp
       <> 'j' ~> MoveDown
       <> 'h' ~> MoveLeft
       <> 'l' ~> MoveRight
       <> 'r' ~> Redraw
       <> 'q' ~> Exit

(~>) :: a -> b -> Map a b
(~>) = Map.singleton

registerKeyPress :: (AddHandler a, b)-> NetworkDescription t (Reactive.Banana.Event t a)
registerKeyPress es = fromAddHandler . fst $ es

matchE :: Eq a => forall t. a -> Event t a -> Event t a
matchE t = filterE (== t)

applyE ::  Apply f g => g (a -> b) -> f a -> g b
applyE e b = (\bv ef -> ef bv) <$> b <@> e

network :: forall t. EventSource Char -> NetworkDescription t ()
network ev = do
    keypress <- registerKeyPress ev


    let input  = filterJust $ (`Map.lookup` keyMap) <$> keypress
        exit   = matchE Exit   input
        redraw = matchE Redraw input
        emove  = eMove input -- Action

    let -- Movin'
        enextpos = emove `applyE` bpos
        
        -- Reaction
        enewpos = enohitmonst

        -- Collision with tiles
        -- Action
        ecollidefix = splitfixed <$> enextpos

        splitfixed p = let t = fixedAtPos initlevel p in case t of
                            Floor -> Left p
                            _     -> Right t

        (enohitfixed,ehitfixed) = split ecollidefix

        
        --Reaction
        etreed = matchE Tree ehitfixed
        
        -- Collision with monsters
        -- Action
        ecollidemonst = splitmonst <$> bmonsters <@> enohitfixed

        splitmonst ms p = case moveableAtPos ms p of
                              m:_ -> Right m
                              []  -> Left p

        (enohitmonst,ehitmonst) = split ecollidemonst

        -- Reaction
        edmgmonst = modify (health . monster) (subtract 1) <$> ehitmonst

        eumonst = updatemonster <$> edmgmonst

        updatemonster m = Map.insert (get key m) m 

        ekillmonst = filterE (\m -> 0 >= get (health . monster) m) edmgmonst

        edmonst = (\m -> Map.delete (get key m)) <$> ekillmonst
        
        -- Move monsters
        -- Action
        eturn = unions [ () <$ ehitmonst, () <$ enewpos]

        -- Reaction
        emovems = fmap movem <$ eturn

        movem m = m'
            where m' = modify position (|+| (1,0)) m
        
        -- State or outputs
        bpos :: Behavior t Coord
        bpos = stepper inithero enewpos -- accumB inithero emovechecked
        emsg = unions ["You eat a banana" <$ etreed
                      ,(\m -> "You hit the stray dog " 
                           ++ (show . get (health . monster) $ m)) <$> edmgmonst
                      ,"You kill the stray dog" <$ ekillmonst
                      ]
        bmsg = stepper [""] ((:[]) <$> emsg)
        bmonsters = accumB initmonsters (eumonst `union` edmonst `union` emovems)
        blevel = stepper initlevel never

        bscreen =  World <$> bpos <*> bmonsters <*> blevel <*> bmsg
        
        eredraw = bscreen <@ redraw
    

    edraw <- changes bscreen

    reactimate $ draw <$>  edraw `union` eredraw
    reactimate $ handleExit <$ exit

eMove input = emove
        where up      = matchE MoveUp     input
              down    = matchE MoveDown   input
              left    = matchE MoveLeft   input
              right   = matchE MoveRight  input
              --redraw  = matchE Redraw input
              emup    = (|+| ( 0,-1)) <$ up
              emdown  = (|+| ( 0, 1)) <$ down
              emleft  = (|+| (-1, 0)) <$ left
              emright = (|+| ( 1, 0)) <$ right
              --emrd    = id            <$ redraw
              emove   = emup
                `union` emdown
                `union` emleft 
                `union` emright
              --  `union` emrd

        

isInBounds (x,y) ((x0,y0),(x1,y1)) = x >= x0 && y >= y0 && x <= x1 && y <= y1

fixedAtPos lvl pos
        | isInBounds pos $ bounds $ lvl = lvl ! pos
        | otherwise = Empty

moveableAtPos ms pos = Map.foldl' (\xs y -> if (get position y == pos) then y:xs else xs) [] ms 



-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit ::  IO a
handleExit = exitSuccess

