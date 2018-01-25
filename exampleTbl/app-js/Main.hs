module Main where

import MainW2 (mainW)
import Language.Javascript.JSaddle (liftJSM)

main :: IO ()
main = liftJSM mainW
