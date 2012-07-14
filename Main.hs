--file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import Reactive.Banana
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when,forM)
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
type EventMap = Map Input (EventSource ())

main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Cabbages and Kings"
    let keymap = keyMap
    eventmap <- registerEvents

    network <- compile $ do
        eup    <- fromEM Up eventmap
        edown  <- fromEM Down eventmap
        eleft  <- fromEM Left eventmap
        eright <- fromEM Right eventmap
        erd    <- fromEM Redraw eventmap

        let emup    = (|+| ( 0,-1)) <$ eup
            emdown  = (|+| ( 0, 1)) <$ edown
            emleft  = (|+| (-1, 0)) <$ eleft
            emright = (|+| ( 1, 0)) <$ eright
            emrd    = id <$ erd
            emove   = emup `union` emdown `union` emleft `union` emright `union` emrd
            epos    = accumE (0,0) emove

        reactimate $ drawHero <$> epos
    
    actuate network

    fire Redraw eventmap
    let loop = do
            char <- getChar
            case char of
                'k' -> fire Up eventmap
                'j' -> fire Down  eventmap
                'h' -> fire Left  eventmap
                'l' -> fire Right  eventmap
                _   -> return ()
            when (char /= 'q') loop
    loop
    handleExit

keyMap :: KeyMap
keyMap =
           bindKey 'k' Up
        <>  bindKey 'j' Down
        <>  bindKey 'h' Left
        <>  bindKey 'l' Right

bindKey :: Char -> Input -> KeyMap
bindKey = Map.singleton

registerEvents :: IO EventMap
registerEvents = do
        l <- forM [minBound..maxBound] $ \i -> do
            x <- newAddHandler
            return (i,x)
        return . Map.fromList $ l


fromEM :: Input -> EventMap -> NetworkDescription t (Event t ())
fromEM e evmap = fromAddHandler . fst . fromJust $ Map.lookup e evmap

fire :: Input -> EventMap -> IO ()
fire e evmap = (snd $ fromJust $ Map.lookup e evmap) $ ()

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

