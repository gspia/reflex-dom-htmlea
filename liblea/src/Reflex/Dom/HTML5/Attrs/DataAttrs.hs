{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs.DataAttrs
Description : Support for various data-* attributes.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Data attributes

This module defines some common data-* attributes
(common in some css-libs).

-}

module Reflex.Dom.HTML5.Attrs.DataAttrs where

import Data.Text (Text)
import Reflex.Dom.HTML5.Attrs.Globals (AttrHasDnmval, dNmVal)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | The 'dToggle' and 'dTarget' define attributes
-- that construct data-toggle = "txt" or data-target = "txt"
-- attributes.
-- See the reflex-dom-themes lib for an example of usage (bootstrap-ex).
dToggle, dTarget :: AttrHasDnmval a => Text -> a -> a
dToggle = dNmVal "toggle"
dTarget = dNmVal "target"


