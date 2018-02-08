{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table
Description : Table construction functions.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Table

The 'mkTableV' can be used to construct a table. Its inputs specify,
what happens, when a cell is clicked on table inside tbody or on its headers,
or when several cells are selected with drag-like mouse move.


-}

module Reflex.Dom.HTML5.Component.Table
    ( module Reflex.Dom.HTML5.Component.Table.TableV
    , module Reflex.Dom.HTML5.Component.Table.StateInfo
    , module Reflex.Dom.HTML5.Component.Table.Common
    , module Reflex.Dom.HTML5.Component.Table.TdComb
    , module Reflex.Dom.HTML5.Component.Table.ThComb
    , module Reflex.Dom.HTML5.Component.Table.TfootComb
    ) where

import Reflex.Dom.HTML5.Component.Table.Common
import Reflex.Dom.HTML5.Component.Table.StateInfo
import Reflex.Dom.HTML5.Component.Table.TdComb
import Reflex.Dom.HTML5.Component.Table.ThComb
import Reflex.Dom.HTML5.Component.Table.TfootComb
import Reflex.Dom.HTML5.Component.Table.TableV

