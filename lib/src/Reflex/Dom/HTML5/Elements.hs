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
   div (className "myClass" $ id_ "myId" defGlobals) $ do
     text "my div etc"
     ...
@
To make an element without attributes:
@
   divN $ do
     text "my div etc"
     ...
@

An example can be found in example-directory.

Naming convention:

    * abbr'  - takes attributes and returns 'm (El, a)'
    * abbr   - takes attributes and returns 'm a'
    * abbrN' - no attributes and returns 'm (El, a)'
    * abbrN  - no attributes and returns 'm a'
    * abbrD' - dynamic attributes and returns 'm (El, a)'
    * abbrD  - dynamic attributes and returns 'm a'

The interactive elements add C and CD -ending tags:

    * aC - takes attributes and 'm ()', give 'm (Event t ())'
    * aCD - takes dynamic attributes and 'm ()', give 'm (Event t ())'


== Notes

The element constructing functions don't allow non-conforming
HTML to be constructed directly with some exceptions. E.g.
this lib still has br-element. There are few ways to by pass
the constraints, in case there is some special needs:

    * el "element name" attrs ... can be used and combined with these
    * see AnyAttr (it is, however, incomplete)
    * addCustom can be used to add anything

-}

module Reflex.Dom.HTML5.Elements
    ( module Reflex.Dom.HTML5.Elements.Elements
    , module Reflex.Dom.HTML5.Elements.Embedded
    , module Reflex.Dom.HTML5.Elements.Interactive
    , module Reflex.Dom.HTML5.Elements.Metadata
    , module Reflex.Dom.HTML5.Elements.SectionHead
    , module Reflex.Dom.HTML5.Elements.Tabular
    ) where

import Reflex.Dom.HTML5.Elements.Elements
import Reflex.Dom.HTML5.Elements.Embedded
import Reflex.Dom.HTML5.Elements.Interactive
import Reflex.Dom.HTML5.Elements.Metadata
import Reflex.Dom.HTML5.Elements.SectionHead
import Reflex.Dom.HTML5.Elements.Tabular

