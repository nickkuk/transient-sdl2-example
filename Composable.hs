module Main where

import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types (CInt)
import Data.Word
import SDL
import Transient.Base
import Transient.Indeterminism
import Transient.Internals

main :: IO ()
main = putStrLn "Hello, World!"
