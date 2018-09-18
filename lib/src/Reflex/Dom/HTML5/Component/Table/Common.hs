{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.Common
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Table.Common

This module defines most of the functionality used with the tables.

Idea shortly, if the defaults are not providing what is needed:

    * Try different cell drawing function, like 'drawDivContent',
      write your own (take one as a starting point).
      Note that the drawing-function is most likely closely tied
      to the input the 'mkTable' is given.
    * Try different listen-functions, like 'listenMe',
      write your own (take one as a starting point).
    * Try different act-functions to handle state management,
      like 'actMU', write you own (take one as a starting point).

You can also override the functions that combine state management,
drawing and event listening. The function that takes the input vectors
and table definitions, is reasonable short as well (so that it is easy to
write a new one if the above ones don't give enough flexibility).

The combining functions are defined in the following modules:

    * "TdComb"
    * "ThComb"
    * "TfootComb"

-}

module Reflex.Dom.HTML5.Component.Table.Common
    (
    -- * Data structures
    --
    -- ** Captions and table behavior
    CaptionConf (..)
    , captionConfText
    , captionConfAttrs
    , mkCaption

    -- * Helpers.
    , updateCompState

    , module Reflex.Dom.HTML5.Component.Common.CompEvent
    ) where

import           Control.Arrow                  ((***))
import           Control.Lens
-- import           Control.Monad.Fix
-- import           Data.Semigroup                 ((<>))
-- import qualified Data.Set                       as Set
import           Data.Text                      (Text)
-- import qualified Data.Text                      as T
-- import qualified GHCJS.DOM.EventM               as DOM
-- import qualified GHCJS.DOM.GlobalEventHandlers  as DOM
-- import qualified GHCJS.DOM.HTMLElement          as DOM
import           Reflex
import           Reflex.Dom.Core
-- import qualified GHCJS.DOM.MouseEvent      as DOM
-- import qualified GHCJS.DOM.Types                as DOM
-- import           Language.Javascript.JSaddle


import           Reflex.Dom.HTML5.Component.Table.ActElem
import           Reflex.Dom.HTML5.Component.Common.CompEvent
-- import           Reflex.Dom.HTML5.Attrs.Globals
import qualified Reflex.Dom.HTML5.Elements as E


-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Caption-element definitions (attributes and text).
data CaptionConf t = CaptionConf
    { _captionConfText  ∷ Text  -- ^ caption text
    , _captionConfAttrs ∷ Dynamic t E.Caption -- ^ caption attributes
    }

-- | A lens.
captionConfText ∷ Lens' (CaptionConf t) Text
captionConfText f (CaptionConf txt attrs) = fmap (`CaptionConf` attrs) (f txt)

-- | A lens.
captionConfAttrs ∷ Lens' (CaptionConf t) (Dynamic t E.Caption)
captionConfAttrs f (CaptionConf txt attrs) = fmap (CaptionConf txt) (f attrs)

-- | Construct the html for the caption, if present.
mkCaption ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
          ⇒ Maybe (CaptionConf t) -- ^ Caption definitions, see 'CaptionConf'.
          → m ()
mkCaption mcapdef =
    case mcapdef of
        Just capdef → E.captionD (_captionConfAttrs capdef) $
            text $ _captionConfText capdef
        Nothing     → blank


--------------------------------------------------------------------------------


-- | 'Table' uses this one. See primitive examples, too.
-- We make a separate update mechanism for tables because we want to
-- use 'duStd'-helper method.
updateCompState ∷ forall t m r. (Reflex t, MonadHold t m, ActSretval r)
             ⇒ CompOtherEvs t → Event t (CompEvent t ActElem r)
             → m (CompState t ActElem r)
updateCompState tblOevs eCompEvent =
    updateComponentState duStd tblOevs eCompEvent


--------------------------------------------------------------------------------

-- | Helper: given a pair of 'ActElem's, return two 'ActElem's that bound the
-- same area as original but in which the first elem in pair is lower-left
-- corner and the second a upper-right one.
duStd ∷ forall t r. (Reflex t, ActSretval r)
      ⇒ Dynamic t (ActiveState t ActElem r, ActiveState t ActElem r)
      → Dynamic t (ActiveState t ActElem r, ActiveState t ActElem r)
duStd dDu = selminmax <$> dDu
  where
    selminmax ∷ (ActiveState t ActElem r, ActiveState t ActElem r)
              → (ActiveState t ActElem r, ActiveState t ActElem r)
    -- selminmax (ActERC (i1,j1), ActERC (i2,j2))
    selminmax (ast1,ast2) = (dsa *** dsa) aep
        where
            dsa ∷ ActElem → ActiveState t ActElem r
            dsa a = defActiveStateTbl & \d → d { _activeStateElemId = a }
            -- dsa a = def & set activeStateElemId a
            aep ::(ActElem, ActElem)
            aep = smm (_activeStateElemId ast1, _activeStateElemId ast2)
            -- aep = smm (view activeStateElemId ast1, view activeStateElemId ast2)
            --
            smm ∷ (ActElem, ActElem) → (ActElem, ActElem)
            smm (ActERC (i1,j1), ActERC (i2,j2))
              = (ActERC (min i1 i2, min j1 j2), ActERC (max i1 i2, max j1 j2))
            smm a = a

--------------------------------------------------------------------------------


