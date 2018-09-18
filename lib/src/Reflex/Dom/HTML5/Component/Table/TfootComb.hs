{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.TfootComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017 -
License     : BSD
Maintainer  : gspia

= TfootComb

Function 'adeCombFun' makes a cell on a table with the associated behavior, given
with 'TfootFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.TfootComb
    ( TfootFuns (..)
    , tfootADEs
    , tfootCombFun
    , tfootTdAttr
    , tfootTrAttr
    , defTfootAttrF
    , defTfootFuns
    ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

-- import qualified Reflex.Dom.HTML5.Attrs as A
import           Reflex.Dom.HTML5.Component.Table.ActElem
import           Reflex.Dom.HTML5.Component.Table.Common
import qualified Reflex.Dom.HTML5.Elements as E

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table foot section.
-- Function 'mkTable' can use TfootFuns when building a table.
data TfootFuns t m r = TfootFuns
    { _tfootADEs ∷ CommonADEfuns t m Text ActElem r -- ^ See 'CommonADEfuns'
    , _tfootCombFun ∷ CommonADEfuns t m Text ActElem r
              → Dynamic t (Maybe [ActiveState t ActElem r])
              → ActiveState t ActElem r
              → Dynamic t Text
              → Event t (CompEvent t ActElem r)
              → CompState t ActElem r
              → m (CompEvent t ActElem r)
    -- ^ A function that combines the fours functions in _tfootADEs.
    -- I.e. 'adeCombFun'.
    , _tfootTdAttr ∷ ActiveState t ActElem r → Dynamic t E.Td
    -- ^ A function that sets the td-attributes.
    , _tfootTrAttr ∷ ActElem → Dynamic t E.Tr
    -- ^ A function that sets the tr-attributes.
    }

-- | A lens.
tfootADEs ∷ Lens' (TfootFuns t m r) (CommonADEfuns t m Text ActElem r)
tfootADEs f (TfootFuns f1 f2 f3 f4) = fmap (\g → TfootFuns g f2 f3 f4) (f f1)

-- | A lens.
tfootCombFun ∷ Lens' (TfootFuns t m r)
  (CommonADEfuns t m Text ActElem r
  → Dynamic t (Maybe [ActiveState t ActElem r])
  → ActiveState t ActElem r
  → Dynamic t Text → Event t (CompEvent t ActElem r)
  → CompState t ActElem r → m (CompEvent t ActElem r))
tfootCombFun f (TfootFuns f1 f2 f3 f4) = fmap (\g → TfootFuns f1 g f3 f4) (f f2)

-- | A lens.
tfootTdAttr ∷ Lens' (TfootFuns t m r) (ActiveState t ActElem r → Dynamic t E.Td)
tfootTdAttr f (TfootFuns f1 f2 f3 f4) = fmap (\g → TfootFuns f1 f2 g f4) (f f3)

-- | A lens.
tfootTrAttr ∷ Lens' (TfootFuns t m r) (ActElem → Dynamic t E.Tr)
tfootTrAttr f (TfootFuns f1 f2 f3 f4) = fmap (TfootFuns f1 f2 f3) (f f4)

--------------------------------------------------------------------------------

-- | Default value for 'TfootFuns'. This uses default 'CommonADEfuns', that is
-- 'defCommonADEfuns', general purpose 'adeCombFun',
-- a const function that gives default dynamic td-attributes and
-- a const function that gives default dynamic tr-attributes.
defTfootFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
               , DomBuilder t m, PostBuild t m, MonadJSM m
               , DomBuilderSpace m ~ GhcjsDomSpace
               , ActSretval r)
             ⇒ TfootFuns t m r
defTfootFuns = TfootFuns defCommonADEfuns
                    adeCombFun (const $ pure E.defTd) (const $ pure E.defTr)

--------------------------------------------------------------------------------

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defTfootAttrF ∷ forall t r. Reflex t ⇒ ActiveState t ActElem r → Dynamic t E.Tfoot
defTfootAttrF = defActivationAttrF E.defTfoot

--------------------------------------------------------------------------------
