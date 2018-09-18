{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.TdComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017 -
License     : BSD
Maintainer  : gspia

= TdComb

TdComb-module contains utilities to handle cells inside tr (a row) inside
tbody-element. A row is build with td-elements, thus the name.

Function 'adeCombFun' makes a cell on a table with the associated behavior, given
with 'TdFuns'.

Note: we may move some of the row-related functions to a separate module, as
header and footer may use the same methods but in different context.

-}

module Reflex.Dom.HTML5.Component.Table.TdComb
    ( TdFuns (..)
    , tdfADEs
    , tdfCombFun
    , tdfTdAttr
    , tdfTrAttr
    , tdfTbodyAttr
    , defTdAttrF
    , defTdFuns
    , silentPlainTdFuns
    , tdFunPre
    ) where

import           Control.Lens
import           Control.Monad.Fix
-- import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.HTML5.Component.Common.DrawFuns
import           Reflex.Dom.HTML5.Component.Table.ActElem
import           Reflex.Dom.HTML5.Component.Table.Common
-- import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'mkTable' uses TdFuns to build a table.
data TdFuns t m a r = TdFuns
    { _tdfADEs ∷ CommonADEfuns t m a ActElem r
    -- ^ See 'CommonADEfuns'. This contains four functions that
    -- change the 'ActiveState', decide on how to activate cells,
    -- draw cells and how the cells produce events.
    , _tdfCombFun ∷ CommonADEfuns t m a ActElem r
              → Dynamic t (Maybe [ActiveState t ActElem r])
              → ActiveState t ActElem r
              → Dynamic t a
              → Event t (CompEvent t ActElem r)
              → CompState t ActElem r
              → m (CompEvent t ActElem r)
    -- ^ A function that combines the fours functions in _tdfADEs.
    -- I.e. 'adeCombFun'.
    , _tdfTdAttr ∷ ActiveState t ActElem r → Dynamic t E.Td
    -- ^ A function that sets the td-attributes. 'mkRowV' uses this one.
    , _tdfTrAttr ∷ Dynamic t (ActiveState t ActElem r) → ActElem → Dynamic t E.Tr
    -- ^ A function that sets the tr-attributes. First param is the last row
    -- where an event occurred (with state info) and the second parameter tells,
    -- which row we are making (drawing, building).
    -- 'mkRowV' uses this one.
    , _tdfTbodyAttr ∷ Dynamic t E.Tbody
    -- ^ Tbody-attributes, i.e., 'defTbody'.
    }

-- | A lens.
tdfADEs ∷ Lens' (TdFuns t m a r) (CommonADEfuns t m a ActElem r)
tdfADEs f (TdFuns f1 f2 f3 f4 f5) = fmap (\g → TdFuns g f2 f3 f4 f5) (f f1)

-- | A lens.
tdfCombFun ∷ Lens' (TdFuns t m a r)
  (CommonADEfuns t m a ActElem r
  → Dynamic t (Maybe [ActiveState t ActElem r])
  → ActiveState t ActElem r
  → Dynamic t a → Event t (CompEvent t ActElem r)
  → CompState t ActElem r → m (CompEvent t ActElem r))
tdfCombFun f (TdFuns f1 f2 f3 f4 f5) = fmap (\g → TdFuns f1 g f3 f4 f5) (f f2)

-- | A lens.
tdfTdAttr ∷ Lens' (TdFuns t m a r) (ActiveState t ActElem r → Dynamic t E.Td)
tdfTdAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (\g → TdFuns f1 f2 g f4 f5) (f f3)

-- | A lens.
tdfTrAttr ∷ Lens' (TdFuns t m a r) (Dynamic t (ActiveState t ActElem r) → ActElem
          → Dynamic t E.Tr)
tdfTrAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (\g → TdFuns f1 f2 f3 g f5) (f f4)

-- | A lens.
tdfTbodyAttr ∷ Lens' (TdFuns t m a r) (Dynamic t E.Tbody)
tdfTbodyAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (TdFuns f1 f2 f3 f4) (f f5)


--------------------------------------------------------------------------------

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defTdAttrF ∷ forall t r. Reflex t ⇒ ActiveState t ActElem r → Dynamic t E.Td
defTdAttrF = defActivationAttrF E.defTd


--------------------------------------------------------------------------------


-- | Default value for 'TdFuns'. This used default 'CommonADEfuns', that is
-- 'defCommonADEfuns', general purpose 'adeCombFun',
-- 'defTdAttrF' as default td-attributes, a const function that gives
-- default dynamic tr-attributes, and default dynamic tbody-attributes.
defTdFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
          , PostBuild t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace
          , ActSretval r)
          -- , PostBuild t m, MonadJSM m, Show a, DomBuilderSpace m ~ GhcjsDomSpace)
          ⇒ TdFuns t m a r
defTdFuns = TdFuns defCommonADEfuns adeCombFun defTdAttrF
        (const $ const $ pure E.defTr) (pure E.defTbody)

--------------------------------------------------------------------------------

-- | No cell is listening for events and no cell is having activity management.
-- Thus, this uses 'silentPlainADE' and 'adeCombSilentPlain' as combining 
-- function.
silentPlainTdFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                     , DomBuilder t m, PostBuild t m, MonadJSM m
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ TdFuns t m a r
silentPlainTdFuns =
    TdFuns silentPlainADE adeCombSilentPlain defTdAttrF
        (const $ const $ pure E.defTr) (pure E.defTbody)


--------------------------------------------------------------------------------


-- | A pre-version of 'tdComb'.
-- This makes the visible contents inside a td-element, that is,
-- an extra div.
-- And in addition to that, this returns events from that div.
tdFunPre ∷ forall t m r. (Reflex t, MonadHold t m, TriggerEvent t m
                      , PostBuild t m, DomBuilder t m,  MonadJSM m
                      , DomBuilderSpace m ~ GhcjsDomSpace
                      , ActSretval r)
      ⇒ (Dynamic t (Maybe [ActiveState t ActElem r])
            → Event t (CompEvent t ActElem r) → CompState t ActElem r
            → ActiveState t ActElem r → m (ActiveState t ActElem r))
      → Dynamic t (Maybe [ActiveState t ActElem r])
      → ActiveState t ActElem r
      → Event t (CompEvent t ActElem r)
      → CompState t ActElem r
      → m (CompEvent t ActElem r)
tdFunPre actF mae me eTB tblSt = do
    let ac = listenMe me
    actS ← actF mae eTB tblSt ac
    (e,_) ← drawDivActElemEx (constDyn ()) actS
    elemEvF actS e


    -- let actstate txt = def & set activeStateElemId txt
    -- let ag = ActiveGroup $ Set.singleton me
    --     ac = actstate me & set activeStateListen (constDyn ag)
    -- actS ← actMD eTB dDU ac
    -- (e,_) ← eDivN' $ do
    --   text $ T.pack . show $ me
    --   dynText $ (T.pack . show) <$> view activeStateActive actS
    -- The next three lines are used to prevent the highlighting of the
    -- selected cells (typically the browser select highlights to end of
    -- row for each row contained between the start and end cells).
    -- let htElraw ∷ RawElement GhcjsDomSpace = _element_raw e
    -- let htT = DOM.uncheckedCastTo DOM.HTMLElement htElraw
    -- let htT = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw e
    -- eMoDo ← wrapDomEvent htT (`DOM.on` DOM.mouseDown) DOM.preventDefault
    -- eMoUp ← wrapDomEvent htT (`DOM.on` DOM.mouseUp) DOM.preventDefault
    -- let adeEvent =
    --       CompEvent me
    --         (me <$ eMoDo)
    --         (me <$ eMoUp)
    --         -- (me <$ domEvent Mousedown e)
    --         -- (me <$ domEvent Mouseup e)
    --         (me <$ domEvent Mouseenter e)
    -- pure adeEvent


--------------------------------------------------------------------------------

-- example3 ∷ forall t m. (MonadWidget t m) ⇒ m ()
-- example3 = do
--   let actstate txt = def & set activeStateElemId txt
--   let tblR = def ∷ CompState t ActElem
--       r1 = ActERC <$> [(1,1), (1,2), (1,3)] -- row 1
--       r2 = ActERC <$> [(2,1), (2,2), (2,3)] -- row 2
--       tdFs = TdFuns listenMe actMU drawDiv elemEv
--              (const def) (const def) def def
--       i = (Prelude.!!)
--   eH2N $ text "Third table example"
--   eDivN $ do
--     eRes ← eTableD (_tableAttr tdFs) $ mdo
--       (htTb,eB) ← eTbodyD' (_tbodyAttr tdFs) $ do
--         let dDU = _dUpLDownR tblR
--         et1 ← eTrD (_trAttr tdFs $ ActErow 1) $ do
--           ed1 ← eTdD (_tdAttr tdFs $ r1 `i` 0) $ tdElm tdFs eB dDU $ r1 `i` 0
--           ed2 ← eTdD (_tdAttr tdFs $ r1 `i` 1) $ tdElm tdFs eB dDU $ r1 `i` 1
--           ed3 ← eTdD (_tdAttr tdFs $ r1 `i` 2) $ tdElm tdFs eB dDU $ r1 `i` 2
--           eTdN $ do
--             let me = ActERC (1,4)
--                 ac = listenListNotMe r1 me
--             actS ← _adeActivate tdFs eB dDU ac
--             _adeDraw tdFs me actS
--           pure $ tblEvFiring [ed1,ed2,ed3]
--         et2 ← eTrD (_trAttr tdFs $ ActErow 2) $ do
--           ed1 ← eTdD (_tdAttr tdFs $ r2 `i` 0) $ tdElm tdFs eB dDU $ r2 `i` 0
--           ed2 ← eTdD (_tdAttr tdFs $ r2 `i` 1) $ tdElm tdFs eB dDU $ r2 `i` 1
--           ed3 ← eTdD (_tdAttr tdFs $ r2 `i` 2) $ tdElm tdFs eB dDU $ r2 `i` 2
--           eTdN $ do
--             let me = ActERC (2,4)
--                 ac = listenListNotMe r2 me
--             actS ← _adeActivate tdFs eB dDU ac
--             _adeDraw tdFs me actS
--           pure $ tblEvFiring [ed1,ed2,ed3]
--         pure $ leftmost [et1,et2]
--       tblR ← updateCompState t blR eB
--       pure tblR
--     let eResTxt = _me <$> _eCompEvent eRes
--         eU = coincidence $ _mUp <$> _eCompEvent eRes ∷ Event t ActElem
--     blank
--     dElm ← holdDyn ActEnone eResTxt
--     dUpElm ← holdDyn ActEnone eU
--     ePN $ do
--         text "last (any) activity is on cell "
--         dynText $ (T.pack . show) <$> dElm
--     ePN $ do
--         text "last clicked cell is on "
--         dynText $ (T.pack . show) <$> dUpElm
--------------------------------------------------------------------------------

