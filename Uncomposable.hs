module Main where

import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types (CInt)
import Data.Word
import SDL

dynBClick :: CInt -> M ()
dynBClick i = do
  l <- gets $ length . getButtons
  liftIO . putStrLn $ "I'm " ++ show (i + 1) ++ " of " ++ show (l - 2)

bMinusClick :: M ()
bMinusClick = modify' h where
  h s@St {getButtons = bs} = if length bs > 2
                             then s {getButtons = tail bs, needRedraw = True}
                             else s

bPlusClick :: M ()
bPlusClick = modify' h where
  h s@St {getButtons = bs} = s {getButtons = b : bs, needRedraw = True} where
    l = fromIntegral $ length bs - 2
    b = Button  { getButtonRect = Rectangle (P (V2 120 (20 + l * 50))) (V2 40 40),
                  getButtonColor = V4 128 128 128 255,
                  getButtonOnClick = dynBClick l }

bPlus :: Button
bPlus = Button { getButtonRect = Rectangle (P (V2 20 20)) (V2 80 80),
                 getButtonColor = V4 0 128 0 255,
                 getButtonOnClick = bPlusClick }

bMinus :: Button
bMinus = Button { getButtonRect = Rectangle (P (V2 20 120)) (V2 80 80),
                  getButtonColor = V4 128 0 0 255,
                  getButtonOnClick = bMinusClick }

initialButtons :: [Button]
initialButtons = [bPlus, bMinus]

type M a = StateT St IO a

data St = St { getWindow :: Window, getRenderer :: Renderer,
               getButtons :: [Button], needRedraw :: Bool }

data Button = Button {getButtonRect :: Rectangle CInt,
                      getButtonColor :: V4 Word8,
                      getButtonOnClick :: M ()}

drawButton :: Button -> M ()
drawButton b = do
  renderer <- gets getRenderer
  rendererDrawColor renderer $= getButtonColor b
  fillRect renderer (Just $ getButtonRect b)

buttonTryClick :: Point V2 CInt -> Button -> M ()
buttonTryClick (P (V2 px py)) b = do
  let Rectangle (P (V2 x y)) (V2 w h) = getButtonRect b
  when (px >= x && px <= x + w - 1 &&
        py >= y && py <= y + h - 1) $ getButtonOnClick b

redraw :: M ()
redraw = do
  renderer <- gets getRenderer
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  gets getButtons >>= mapM_ drawButton
  present renderer
  modify' $ \s -> s {needRedraw = False}

main :: IO ()
main = do
  initialize [InitVideo]
  window <- createWindow "Uncomposable" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let loop = do
        r <- gets needRedraw
        when r redraw
        waitEvent >>= \case
          Event _ QuitEvent -> return ()
          Event _ (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))))
                            -> do gets getButtons >>= mapM_ (buttonTryClick (P (V2 (fromIntegral x) (fromIntegral y))))
                                  loop
          _                 -> loop
  evalStateT loop St {getWindow = window, getRenderer = renderer,
                      getButtons = initialButtons, needRedraw = True}
  destroyRenderer renderer
  destroyWindow window
  quit
