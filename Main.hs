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
import Control.Monad(forM_)

import World

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

    snd eventsource  'r' 

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

matchE t = filterE (== t)

network :: forall t. EventSource Char -> NetworkDescription t ()
network ev = do
    keypress <- registerKeyPress ev

    let applyE e b = (\bv ef -> ef bv) <$> b <@> e

    let input  = filterJust $ (`Map.lookup` keyMap) <$> keypress
        exit   = matchE Exit   input
        emove  = eMove input

    let 
        collides lvl mons p = ((`notElem` [Empty,Tree]) . fixedAtPos lvl) p
                                 && Nothing == moveableAtPos mons p
        

        enewpos = emove `applyE` bpos
        enewposchecked = filterApply (collides <$> blevel <*> bmonsters) enewpos

        ecollidefix =  fixedAtPos initlevel <$> enewpos

        ehitmonst :: Event t Moveable
        ehitmonst = filterJust $ moveableAtPos <$> bmonsters <@> enewpos

        etreed = matchE Tree ecollidefix

        edmgmonst = modify (health . monster) (subtract 1) <$> ehitmonst

        eumonst = updatemonster <$> edmgmonst

        updatemonster m = Map.insert (get position m) m

        ekillmonst = filterE (\m -> 0 >= get (health . monster) m) edmgmonst

        edmonst = (\(Moveable a _) -> Map.delete a) <$> ekillmonst


        bpos      = stepper inithero enewposchecked -- accumB inithero emovechecked
        emsg = unions ["You eat a banana" <$ etreed
                      ,(\m -> "You hit the stray dog" 
                           ++ (show . get (health . monster) $ m)) <$> edmgmonst
                      ,"You kill the stray dog" <$ ekillmonst
                      ]
        bmsg = stepper [""] ((:[]) <$> emsg)
        bmonsters = accumB initmonsters (eumonst `union` edmonst)
        blevel = stepper initlevel never

        bscreen =  World <$> bpos <*> bmonsters <*> blevel <*> bmsg
    

    edraw <- changes bscreen

    reactimate $ draw <$>  edraw
    reactimate $ handleExit <$ exit

eMove input = emove
        where up      = matchE MoveUp     input
              down    = matchE MoveDown   input
              left    = matchE MoveLeft   input
              right   = matchE MoveRight  input
              redraw  = matchE Redraw input
              emup    = (|+| ( 0,-1)) <$ up
              emdown  = (|+| ( 0, 1)) <$ down
              emleft  = (|+| (-1, 0)) <$ left
              emright = (|+| ( 1, 0)) <$ right
              emrd    = id            <$ redraw
              emove   = emup
                `union` emdown
                `union` emleft 
                `union` emright
                `union` emrd

-- dot = ((.).(.))

isInBounds (x,y) ((x0,y0),(x1,y1)) = x >= x0 && y >= y0 && x <= x1 && y <= y1

fixedAtPos lvl pos
        | isInBounds pos $ bounds $ lvl = lvl ! pos
        | otherwise = Empty

moveableAtPos monsts pos = Map.lookup pos monsts 


renderObj Wall  = '#'
renderObj Floor = '.'
renderObj Tree  = '+'

renderMonster (Moveable _ (StrayDog _)) = 'd'

draw w = draw' >> refresh
        where renderedlevel = renderObj <$> get level w
              updates = [(get hero w,'@')]
                      <> Map.toList (renderMonster <$> (get monsters w))
              flatlevel = renderedlevel // updates
              draw' = do 
                forM_ [(y,x) | y <- [0..24], x <- [0..79]] $
                            \(y,x) -> mvAddCh y x $ toCh (flatlevel ! (x,y))
                mvWAddStr stdScr 25 0 $ head $ get messages w
              toCh = fromIntegral . fromEnum



-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit ::  IO a
handleExit = exitSuccess

