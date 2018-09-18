{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.ThComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017 -
License     : BSD
Maintainer  : gspia

= ThComb

Function 'adeCombFun' makes a cell on a table with the associated behavior, given
with 'ThFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.ThComb
    ( ThFuns (..)
    , thfADEs
    , thfCombFun
    , thfThAttr
    , thfTrAttr
    , defThAttrF
    , defThFuns
    ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Text                   (Text)
import           Reflex
import           Reflex.Dom.Core
import           Language.Javascript.JSaddle

-- import qualified Reflex.Dom.HTML5.Attrs as A
import           Reflex.Dom.HTML5.Component.Table.ActElem
import qualified Reflex.Dom.HTML5.Elements as E
import           Reflex.Dom.HTML5.Component.Table.Common

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'Reflex.Dom.HTML5.Component.Table.Table.mkTable'
-- can use ThFuns when building a table.
data ThFuns t m r = ThFuns
    { _thfADEs ∷ CommonADEfuns t m Text ActElem r -- ^ See 'CommonADEfuns'
    , _thfCombFun ∷ CommonADEfuns t m Text ActElem r
              → Dynamic t (Maybe [ActiveState t ActElem r])
              → ActiveState t ActElem r
              → Dynamic t Text
              → Event t (CompEvent t ActElem r)
              → CompState t ActElem r
              → m (CompEvent t ActElem r)
    -- ^ A function that combines the fours functions in _thfADEs,
    -- i.e. 'adeCombFun'.
    , _thfThAttr ∷ ActiveState t ActElem r → Dynamic t E.Th
    -- ^ A function that sets the td-attributes.
    , _thfTrAttr ∷ ActElem → Dynamic t E.Tr
    -- ^ A function that sets the tr-attributes.
    }

-- | Lens.
thfADEs ∷ Lens' (ThFuns t m r) (CommonADEfuns t m Text ActElem r)
thfADEs f (ThFuns f1 f2 f3 f4) = fmap (\g → ThFuns g f2 f3 f4) (f f1)

-- | Lens.
thfCombFun ∷ Lens' (ThFuns t m r)
    (CommonADEfuns t m Text ActElem r
    → Dynamic t (Maybe [ActiveState t ActElem r])
    → ActiveState t ActElem r
    → Dynamic t Text → Event t (CompEvent t ActElem r)
    → CompState t ActElem r → m (CompEvent t ActElem r))
thfCombFun f (ThFuns f1 f2 f3 f4) = fmap (\g → ThFuns f1 g f3 f4) (f f2)

-- | Lens.
thfThAttr ∷ Lens' (ThFuns t m r) (ActiveState t ActElem r → Dynamic t E.Th)
thfThAttr f (ThFuns f1 f2 f3 f4) = fmap (\g → ThFuns f1 f2 g f4) (f f3)

-- | Lens.
thfTrAttr ∷ Lens' (ThFuns t m r) (ActElem → Dynamic t E.Tr)
thfTrAttr f (ThFuns f1 f2 f3 f4) = fmap (ThFuns f1 f2 f3) (f f4)

--------------------------------------------------------------------------------
-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defThAttrF ∷ forall t r. Reflex t ⇒ ActiveState t ActElem r → Dynamic t E.Th
defThAttrF = defActivationAttrF E.defTh

--------------------------------------------------------------------------------

-- | Default value for 'ThFuns'. This used default 'CommonADEfuns', that is
-- 'defCommonADEfuns', general purpose 'adeCombFun',
-- 'defThAttrF' as default th-attributes and default dynamic tr-attributes
defThFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
            , DomBuilder t m, PostBuild t m, MonadJSM m
            , DomBuilderSpace m ~ GhcjsDomSpace
            , ActSretval r)
          ⇒ ThFuns t m r
defThFuns = ThFuns defCommonADEfuns adeCombFun defThAttrF (const $ pure E.defTr)

