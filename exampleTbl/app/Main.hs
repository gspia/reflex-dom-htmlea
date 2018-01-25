
module Main where

import MainW2 (mainW)

import Reflex.Dom      (run)
import Reflex.Dom.Core (mainWidget)

main :: IO ()
main = run $ mainWidget mainW

