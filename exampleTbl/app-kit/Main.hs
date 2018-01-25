
module Main where

import MainW2 (mainW)

import Language.Javascript.JSaddle.WebKitGTK (run)

main :: IO ()
main = run mainW

