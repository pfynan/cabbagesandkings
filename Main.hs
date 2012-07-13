--file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import Reactive.Banana
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Monoid

type Coord = (Int, Int)

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-
data World = World { wHero :: Coord }
-}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq,Enum,Bounded)

type EventSource a = (AddHandler a, a -> IO ())
type KeyMap = Data.Map Char Input
type EventMap = Data.Map Input (AddHandler ())

main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Cabbages and Kings"
    let keymap = setupEvents
    esup    <- newAddHandler
    esdown  <- newAddHandler
    esleft  <- newAddHandler
    esright <- newAddHandler

    network <- compile $ do
        eup    <- fromES esup
        edown  <- fromES esdown
        eleft  <- fromES esleft
        eright <- fromES esright

        let emup    = (|+| ( 0,-1)) <$ eup
            emdown  = (|+| ( 0, 1)) <$ edown
            emleft  = (|+| (-1, 0)) <$ eleft
            emright = (|+| ( 1, 0)) <$ eright
            emove   = emup `union` emdown `union` emleft `union` emright
            epos    = accumE (0,0) emove

        reactimate $ drawHero <$> epos
    
    actuate network

    let loop = do
            char <- getChar
            case char of
                'k' -> fire esup ()
                'j' -> fire esdown ()
                'h' -> fire esleft ()
                'l' -> fire esright ()
                _   -> return ()
            when (char /= 'q') loop
    loop
    handleExit

keyMap :: Data.Map Char Input
keyMap =
           bindKey 'k' Up
        <>  bindKey 'j' Down
        <>  bindKey 'h' Left
        <>  bindKey 'l' Right

bindKey :: Data.Map Char Input
bindKey = Map.singleton

registerEvents :: Data.Map Input EventMap
registerEvents = fromList $ map (\x -> (x,newAddHandler)) [minBound..maxBound]


fromEM :: Input -> EventMap -> NetworkDescription t (Event t a)
fromEM e evmap = fromAddHandler . fst . fromJust . Map.lookup $ e evmap

fire :: Input -> EventMap -> IO ()
fire e evmap = snd $ fromJust $ Map.lookup e evmap $ ()

drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"
{-
-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _  -> getInput
-}

{-
dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-1, 0)
  | d == Right = (1,  0)
  | otherwise  = (0,  0)
-}           
{-
-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDir w@(World hero) input = gameLoop (w { wHero = newCoord })
  where newCoord       = (newX, newY)
        (heroX, heroY) = hero |+| (dirToCoord input)
        hConst i       = max 0 (min i 80)
        newX           = hConst heroX
        newY           = hConst heroY
-}        
{-
-- update the game loop to add in the goodbye message
gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir world input
-}

-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"
