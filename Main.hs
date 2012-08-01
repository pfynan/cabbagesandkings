--file: Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import System.Exit (exitSuccess)

import Reactive.Banana
import Control.Monad (forever)
import Data.Maybe (fromJust)

-----------------------
-- Types
-----------------------

type Coord = (Int, Int)

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- Input represents all the valid inputs 
data Input = Up
           | Down
           | Left
           | Right
           | Exit
           | Redraw
           deriving Eq

-- EventSource is the connection between the network and the IO layer
-- Cribbed shamelessly from:
--      https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/examples/SlotMachine.hs
type EventSource a = (AddHandler a, a -> IO ())

-- Again, cribbed from example
fire :: EventSource a -> a -> IO ()
fire = snd

registerEvent :: EventSource a -> NetworkDescription t (Event t a)
registerEvent = fromAddHandler . fst

main = do
    initScreen

    keypress <- newAddHandler
    compile (network keypress) >>= actuate

    -- Refresh the screen so it draws the first time
    fire keypress 'r'

    forever $ getChar >>= fire keypress

-- Some combinators to help make event network cleaner

-- matchE only passes events that match parameter
matchE ::  Eq a => a -> Event t a -> Event t a
matchE t = filterE (== t)

-- applyE applies a function in an event to a behavior
applyE :: Event t (a -> b) -> Behavior t a -> Event t b
applyE e b = (\b' e' -> e' b') <$> b <@> e

network :: forall t. EventSource Char -> NetworkDescription t ()
network kp = do
    keypress <- registerEvent kp

    let -- Convert key to valid input
        input  = filterJust $ keyMap <$> keypress

        -- Separate out each input into its own event
        up     = matchE Up     input
        down   = matchE Down   input
        left   = matchE Left   input
        right  = matchE Right  input
        redraw = matchE Redraw input
        exit   = matchE Exit   input

    let -- Replace input with function that performs action 
        emup             = (|+| ( 0,-1)) <$ up
        emdown           = (|+| ( 0, 1)) <$ down
        emleft           = (|+| (-1, 0)) <$ left
        emright          = (|+| ( 1, 0)) <$ right
        emove            = emup 
                   `union` emdown
                   `union` emleft 
                   `union` emright

        -- Perform that action
        enewpos  = emove `applyE` bpos

        -- ...but only if the result is on the screen
        enextpos = filterE isOnscreen enewpos

        -- and keep track of the current position
        bpos     = stepper (0,0) enextpos

        -- To redraw screen, just draw the behavior
        eredraw  = bpos <@ redraw
        
    reactimate $ drawHero <$> enextpos `union` eredraw
    reactimate $ handleExit <$ exit

keyMap :: Char -> Maybe Input
keyMap 'k' = Just Up
keyMap 'j' = Just Down
keyMap 'h' = Just Left
keyMap 'l' = Just Right
keyMap 'r' = Just Redraw
keyMap 'q' = Just Exit
keyMap _   = Nothing

initScreen ::  IO ()
initScreen = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Cabbages and Kings"

-- Hardcoded screen size, but 640k should be enough for anybody
isOnscreen :: Coord -> Bool
isOnscreen (x,y) = x >= 0 && y >= 0 && x <= 79 && y <= 24

drawHero :: Coord -> IO ()
drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"


-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit ::  IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull White ]
  putStrLn "Thank you for playing!"
  exitSuccess

