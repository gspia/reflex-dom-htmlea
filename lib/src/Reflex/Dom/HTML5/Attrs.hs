{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs
Description : HTML5 Attributes with some helper functions.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Attrs

HTML5 Attributes with some helper functions. This module
brings all submodules in one go.

This module defines html5 attributes and with corresponding
elements, the compiler can help to avoid some of the writing
typos that were otherwise to be found on runtime.

To make an attribute map, write e.g.
@
  let a = href (URL "localhost:8000") $ className "myClass" $ id_ "myId" defHref
@

An example can be found in example-directory.

The element constructing functions don't allow non-conforming
HTML to be constructed directly. There are a few ways to by pass,
in case there is some need:

    * el "element name" attrs ... can be used and combined with these
    * see AnyAttr (it is, however, incomplete)
    * addCustom can be used to add anything

-}

module Reflex.Dom.HTML5.Attrs
    ( module Reflex.Dom.HTML5.Attrs.Aria
    , module Reflex.Dom.HTML5.Attrs.Attrs
    , module Reflex.Dom.HTML5.Attrs.Common
    , module Reflex.Dom.HTML5.Attrs.DataAttrs
    , module Reflex.Dom.HTML5.Attrs.Globals
    , module Reflex.Dom.HTML5.Attrs.AnyAttr
    ) where

import Reflex.Dom.HTML5.Attrs.Aria
import Reflex.Dom.HTML5.Attrs.Attrs
import Reflex.Dom.HTML5.Attrs.Common
import Reflex.Dom.HTML5.Attrs.DataAttrs
import Reflex.Dom.HTML5.Attrs.Globals
import Reflex.Dom.HTML5.Attrs.AnyAttr

