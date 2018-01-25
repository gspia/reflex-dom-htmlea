
module Main where

import MainW (mainW)

import Reflex.Dom      (run)
import Reflex.Dom.Core (mainWidget)


main :: IO ()
main = run $ mainWidget mainW

