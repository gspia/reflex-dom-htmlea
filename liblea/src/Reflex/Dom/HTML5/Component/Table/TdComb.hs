{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.TdComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TdComb

-}

module Reflex.Dom.HTML5.Component.Table.TdComb where

import           Control.Monad.Fix
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Elements

-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'mkTableV' uses TdFuns to build a table.
data TdFuns t m a = TdFuns
    { _tdfADEs :: CommonADEfuns t m a -- ^ See 'CommonADEfuns'
    , _tdfCombFun :: TdFuns t m a
              → ActElem
              → a
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ _tdfCombFun, a function that combines the above fours functions.
    -- I.e. 'tdComb
    -- or 'tdFun'
    , _tdfTdAttr :: ActElem → Dynamic t ETd
    -- ^ _tdfTdAttr, i.e. a function that sets the td-attributes.
    , _tdfTrAttr :: ActElem → Dynamic t ETr
    -- ^ _tdfTrAttr, i.e. a function that sets the tr-attributes.
    , _tdfTbodyAttr :: Dynamic t ETbody
    -- ^ _tdfTbodyAttr, tbody-attributes, i.e., def.
    , _tdfTableAttr :: Dynamic t ETable
    -- ^ _tdfTableAttr, table-attributes, i.e., def.
    }

-- makeLenses ''TdFuns

----------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table.
-- This function takes a data type 'TdFuns' having functions that can
--   - define behaviour on mouse (and other) events
--   - define, how to draw the contents of a cell of a table
--   - define, what and how to return
tdComb ∷ forall t m a. (Reflex t, MonadHold t m, TriggerEvent t m
                       , PostBuild t m, DomBuilder t m,  MonadJSM m)
       ⇒ TdFuns t m a
       → ActElem                 -- ^ "me" (to be able to diff to eTB src)
       → a                       -- ^ input content (dwawEl-fun uses)
       → Event t (TableEvent t)  -- ^ an event somewhere on the table (eTB)
       → TableState t
       → m (TableEvent t)        -- ^ events we want this cell to return
tdComb tdFuns me a eTB tblSt = do
    -- _actSt, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that "listening" and "activation" should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu ._tdfADEs) tdFuns eTB tblSt $ (_actSt . _tdfADEs ) tdFuns me
    e <- (_drawEl. _tdfADEs) tdFuns me a actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- td-element (that is, a "cell") inside tbody rows (tr-elements).
    (_cellEv . _tdfADEs) tdFuns actS e
    -- _cellEv, i.e. 'cellEvF' - what events we are listening inside
    -- td-elements. Note that using other events here means that many
    -- other functions in this module should be changed or given alternative
    -- implementations.

----------------------------------------------------------------------------------

-- | Like 'tdComb' but doesn't return any events (just default that is
-- full of never's).
tdCombLstner ∷ forall t m a. (Reflex t, MonadHold t m, TriggerEvent t m
                              , PostBuild t m, DomBuilder t m,  MonadJSM m)
      ⇒ TdFuns t m a
      → ActElem                 -- ^ "me" (to be able to diff to eTB src)
      → a                       -- ^ input content (dwawEl-fun uses)
      → Event t (TableEvent t)  -- ^ an event somewhere on the table
      → TableState t
      → m (TableEvent t)        -- ^ This returns default
tdCombLstner tdFuns me a eTB tblSt = do
    actS <- (_actFu . _tdfADEs) tdFuns eTB tblSt $ (_actSt . _tdfADEs) tdFuns me
    _ <- (_drawEl . _tdfADEs) tdFuns me a actS
    pure def


--------------------------------------------------------------------------------

-- Hmm using this isn't that obvious. Some class constraints missing?
-- (Try changing the tdElm. Even changing it to tdElm again doesn't seem to work.)
defaultTdFuns ∷ forall t m a. (Reflex t, MonadHold t m, TriggerEvent t m
                              , MonadFix m, DomBuilder t m, PostBuild t m
                              , MonadJSM m, Show a
                              , DomBuilderSpace m ~ GhcjsDomSpace)
              ⇒ TdFuns t m a
defaultTdFuns = TdFuns defaultCommonADE tdComb (const def) (const def) def def

--------------------------------------------------------------------------------

-- | A pre-version of 'tdComb'.
-- This makes the visible contents inside a td-element, that is,
-- an extra div.
-- And in addition to that, this returns events from that div.
tdFun ∷ forall t m. (Reflex t, MonadHold t m, TriggerEvent t m,
                      PostBuild t m, DomBuilder t m,  MonadJSM m,
                      DomBuilderSpace m ~ GhcjsDomSpace)
      ⇒ (Event t (TableEvent t) → TableState t
      → ActiveState t → m (ActiveState t))
      → ActElem
      → Event t (TableEvent t)
      → TableState t
      → m (TableEvent t)
tdFun actF me eTB tblSt = do
    let ac = listenMe me
    actS <- actF eTB tblSt ac
    e <- drawDivActElemEx me () actS
    cellEvF actS e
    -- let actstate txt = def & set activeStateMe txt
    -- let ag = ActiveGroup $ Set.singleton me
    --     ac = actstate me & set activeStateListen (constDyn ag)
    -- actS <- actMD eTB dDU ac
    -- (e,_) <- eDivN' $ do
    --   text $ T.pack . show $ me
    --   dynText $ (T.pack . show) <$> view activeStateActive actS
    -- The next three lines are used to prevent the highlighting of the
    -- selected cells (typically the browser select highlights to end of
    -- row for each row contained between the start and end cells).
    -- let htElraw :: RawElement GhcjsDomSpace = _element_raw e
    -- let htT = DOM.uncheckedCastTo DOM.HTMLElement htElraw
    -- let htT = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw e
    -- eMoDo <- wrapDomEvent htT (`DOM.on` DOM.mouseDown) DOM.preventDefault
    -- eMoUp <- wrapDomEvent htT (`DOM.on` DOM.mouseUp) DOM.preventDefault
    -- let cellEv =
    --       TableEvent me
    --         (me <$ eMoDo)
    --         (me <$ eMoUp)
    --         -- (me <$ domEvent Mousedown e)
    --         -- (me <$ domEvent Mouseup e)
    --         (me <$ domEvent Mouseenter e)
    -- pure cellEv

--------------------------------------------------------------------------------

-- example3 :: forall t m. (MonadWidget t m) => m ()
-- example3 = do
--   let actstate txt = def & set activeStateMe txt
--   let tblR = def :: TableState t
--       r1 = ActERC <$> [(1,1), (1,2), (1,3)] -- row 1
--       r2 = ActERC <$> [(2,1), (2,2), (2,3)] -- row 2
--       tdFs = TdFuns listenMe actMU drawDiv cellEv
--              (const def) (const def) def def
--       i = (Prelude.!!)
--   eH2N $ text "Third table example"
--   eDivN $ do
--     eRes <- eTableD (_tableAttr tdFs) $ mdo
--       (htTb,eB) <- eTbodyD' (_tbodyAttr tdFs) $ do
--         let dDU = _dUpLDownR tblR
--         et1 <- eTrD (_trAttr tdFs $ ActErow 1) $ do
--           ed1 <- eTdD (_tdAttr tdFs $ r1 `i` 0) $ tdElm tdFs eB dDU $ r1 `i` 0
--           ed2 <- eTdD (_tdAttr tdFs $ r1 `i` 1) $ tdElm tdFs eB dDU $ r1 `i` 1
--           ed3 <- eTdD (_tdAttr tdFs $ r1 `i` 2) $ tdElm tdFs eB dDU $ r1 `i` 2
--           eTdN $ do
--             let me = ActERC (1,4)
--                 ac = listenListNotMe r1 me
--             actS <- _actFu tdFs eB dDU ac
--             _drawEl tdFs me actS
--           pure $ tblEvFiring [ed1,ed2,ed3]
--         et2 <- eTrD (_trAttr tdFs $ ActErow 2) $ do
--           ed1 <- eTdD (_tdAttr tdFs $ r2 `i` 0) $ tdElm tdFs eB dDU $ r2 `i` 0
--           ed2 <- eTdD (_tdAttr tdFs $ r2 `i` 1) $ tdElm tdFs eB dDU $ r2 `i` 1
--           ed3 <- eTdD (_tdAttr tdFs $ r2 `i` 2) $ tdElm tdFs eB dDU $ r2 `i` 2
--           eTdN $ do
--             let me = ActERC (2,4)
--                 ac = listenListNotMe r2 me
--             actS <- _actFu tdFs eB dDU ac
--             _drawEl tdFs me actS
--           pure $ tblEvFiring [ed1,ed2,ed3]
--         pure $ leftmost [et1,et2]
--       tblR <- updateTableState tblR eB
--       pure tblR
--     let eResTxt = _me <$> _eTableEvent eRes
--         eU = coincidence $ _mUp <$> _eTableEvent eRes :: Event t ActElem
--     blank
--     dElm <- holdDyn ActEnone eResTxt
--     dUpElm <- holdDyn ActEnone eU
--     ePN $ do
--         text "last (any) activity is on cell "
--         dynText $ (T.pack . show) <$> dElm
--     ePN $ do
--         text "last clicked cell is on "
--         dynText $ (T.pack . show) <$> dUpElm
--------------------------------------------------------------------------------

