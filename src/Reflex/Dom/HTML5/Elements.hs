{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Elements
Description : HTML5 elements with some helper functions.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Elements

HTML5 elements with some helper functions. This module
brings all submodules in one go.

This module defines html5 elements and with corresponding
attributes, the compiler can help to avoid some of the writing
typos that were otherwise to be found on runtime.

To make an element with attributes, write e.g.
@
   eDiv (className "myClass" $ id_ "myId" def) $ do
     text "my div etc"
     ...
@
To make an element without attributes:
@
   eDivN $ do
     text "my div etc"
     ...
@

An example can be found in example-directory.

Naming convention:
* eAbbr'  - takes attributes and returns 'm (El, a)'
* eAbbr   - takes attributes and returns 'm a'
* eAbbrN' - no attributes and returns 'm (El, a)'
* eAbbrN  - no attributes and returns 'm a'
* eAbbrD' - dynamic attributes and returns 'm (El, a)'
* eAbbrD  - dynamic attributes and returns 'm a'

The interactive elements add C and CD -ending tags:
* eAC - takes attributes and 'm ()', give 'm (Event t ())'
* eACD - takes dynamic attributes and 'm ()', give 'm (Event t ())'

== Notes

The element constructing functions don't allow non-conforming
HTML to be constructed directly with some exceptions. E.g.
this lib still has br-element. There are few ways to by pass
the constraints, in case there is some special needs:
* el "element name" attrs ... can be used and combined with these
* see AnyAttr (it is, however, incomplete)
* addCustom can be used to add anything

-}

module Reflex.Dom.HTML5.Elements (
  module X
  ) where

import Reflex.Dom.HTML5.Elements.Elements    as X
import Reflex.Dom.HTML5.Elements.Embedded    as X
import Reflex.Dom.HTML5.Elements.Interactive as X
import Reflex.Dom.HTML5.Elements.Metadata    as X
import Reflex.Dom.HTML5.Elements.SectionHead as X
import Reflex.Dom.HTML5.Elements.Tabular     as X

