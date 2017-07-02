module Main where

import Control.Applicative
import Control.Monad
import SDL
import Transient.Base
import Transient.Internals

newtype NeedRedraw = NeedRedraw Bool

redraw :: TransIO ()
redraw = do
  NeedRedraw needRedraw <- getState
  when needRedraw $ do
    renderer <- getState
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    present renderer
    setState $ NeedRedraw False

main :: IO ()
main = void . runTransient . threads 0 $ do              -- run TransIO in initial (bounded) thread
  initSDL
  setState $ NeedRedraw True
  redraw
  e <- waitEvents SDL.waitEvent
  exitSDL e <|> redraw

initSDL :: TransIO ()
initSDL = do
  SDL.initialize [InitVideo]
  window <- createWindow "Composable" defaultWindow
  setState window
  createRenderer window (-1) defaultRenderer >>= setState

exitSDL :: Event -> TransIO ()
exitSDL (Event _ QuitEvent) = do
  getState >>= destroyRenderer
  getState >>= destroyWindow
  SDL.quit
  killBranch
exitSDL _ = stop
