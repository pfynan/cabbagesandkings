--file: Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import System.Exit (exitSuccess)

import Reactive.Banana
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when,forM,forever)
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Monoid


type Coord = (Int, Int)

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


data Input = Up
           | Down
           | Left
           | Right
           | Exit
           | Redraw
           deriving (Eq,Enum,Bounded,Ord)

type EventSource a = (AddHandler a, a -> IO ())
type KeyMap = Map Char Input

main = do
    initScreen

    keypress <- newAddHandler
    compile (network keypress) >>= actuate

    let kp = snd keypress

    kp 'r'

    forever $ getChar >>= kp

keyMap :: KeyMap
keyMap =
           bindKey 'k' Up
        <>  bindKey 'j' Down
        <>  bindKey 'h' Left
        <>  bindKey 'l' Right
        <>  bindKey 'r' Redraw
        <>  bindKey 'q' Exit

bindKey :: Char -> Input -> KeyMap
bindKey = Map.singleton

registerKeyPress :: forall t. ReaderT (EventSource Char)
                                (NetworkDescription t) 
                                (Event t Char)
registerKeyPress = do
        x <- ask
        lift . fromAddHandler . fst $ x


network :: forall t. (EventSource Char) -> NetworkDescription t ()
network = runReaderT $ do
    keypress <- registerKeyPress

    let matchE t = filterE (== t)
        dup f a = (f a,f a)

    let input  = filterJust $ (\x -> Map.lookup x keyMap) <$> keypress
        up     = matchE Up     input
        down   = matchE Down   input
        left   = matchE Left   input
        right  = matchE Right  input
        redraw = matchE Redraw input
        exit   = matchE Exit   input

    let emup             = (|+| ( 0,-1)) <$ up
        emdown           = (|+| ( 0, 1)) <$ down
        emleft           = (|+| (-1, 0)) <$ left
        emright          = (|+| ( 1, 0)) <$ right
        emrd             = id            <$ redraw
        emove            = emup 
                   `union` emdown
                   `union` emleft 
                   `union` emright
                   `union` emrd
        emovechecked     = filterApply ((\pos move -> isOnscreen (move $ pos)) <$> bpos) emove
        (epos,bpos)      = mapAccum (0,0) $ dup <$> emovechecked
        isOnscreen (x,y) = x >= 0 && y >= 0 && x <= 79 && y <= 24
        
    lift . reactimate $ drawHero <$> epos
    lift . reactimate $ handleExit <$ exit

initScreen = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Cabbages and Kings"

drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"


-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull White ]
  putStrLn "Thank you for playing!"
  exitSuccess

