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

Function 'tdComb' makes a cell on a table with the associated behavior, given
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
    , tdComb
    , tdCombLstner
    , tdCombSilentPlain
    , defTdAttrF
    , silentPlainTdFuns
    , tdFun
    ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Default
-- import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.HTML5.Component.Table.StateInfo
import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Attrs
import           Reflex.Dom.HTML5.Elements

-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'mkTableV' uses TdFuns to build a table.
data TdFuns t m a = TdFuns
    { _tdfADEs :: CommonADEfuns t m a
    -- ^ See 'CommonADEfuns'.
    , _tdfCombFun :: TdFuns t m a
              → Dynamic t (Maybe [ActiveState t])
              → ActiveState t
              → a
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ A function that combines the above fours functions.
    -- I.e. 'tdComb' or 'tdFun'.
    , _tdfTdAttr :: ActiveState t → Dynamic t ETd
    -- ^ A function that sets the td-attributes.
    , _tdfTrAttr :: Dynamic t (ActiveState t) -> ActElem → Dynamic t ETr
    -- ^ A function that sets the tr-attributes. First param is the last row
    -- where an event occurred (with state info) and the second parameter tells,
    -- which row we are making (drawing, building).
    , _tdfTbodyAttr :: Dynamic t ETbody
    -- ^ Tbody-attributes, i.e., def.
    }

-- | A lens.
tdfADEs :: Lens' (TdFuns t m a) (CommonADEfuns t m a)
tdfADEs f (TdFuns f1 f2 f3 f4 f5) = fmap (\g -> TdFuns g f2 f3 f4 f5) (f f1)

-- | A lens.
tdfCombFun :: Lens' (TdFuns t m a)
  (TdFuns t m a → Dynamic t (Maybe [ActiveState t])
  → ActiveState t → a → Event t (TableEvent t) → TableState t → m (TableEvent t))
tdfCombFun f (TdFuns f1 f2 f3 f4 f5) = fmap (\g -> TdFuns f1 g f3 f4 f5) (f f2)

-- | A lens.
tdfTdAttr :: Lens' (TdFuns t m a) (ActiveState t -> Dynamic t ETd)
tdfTdAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (\g -> TdFuns f1 f2 g f4 f5) (f f3)

-- | A lens.
tdfTrAttr :: Lens' (TdFuns t m a) (Dynamic t (ActiveState t) -> ActElem
          -> Dynamic t ETr)
tdfTrAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (\g -> TdFuns f1 f2 f3 g f5) (f f4)

-- | A lens.
tdfTbodyAttr :: Lens' (TdFuns t m a) (Dynamic t ETbody)
tdfTbodyAttr f (TdFuns f1 f2 f3 f4 f5) = fmap (TdFuns f1 f2 f3 f4) (f f5)


-- makeLenses ''TdFuns

----------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table.
-- This function takes a data type 'TdFuns' having functions that can
--
--     * define behaviour on mouse (and other) events
--     * define, how to draw the contents of a cell of a table
--     * define, what and how to return
--
-- To see, how this is used, take a look of 'mkRow'. It makes a row, that is,
-- constructs a tr-element that contains td-elements. Each td-element is made
-- with this function or similar.
tdComb ∷ forall t m a. (Reflex t, MonadHold t m, TriggerEvent t m
                       , PostBuild t m, DomBuilder t m,  MonadJSM m)
       ⇒ TdFuns t m a
       -- Functions: listening, activity management, drawing and event capturing.
       → Dynamic t (Maybe [ActiveState t])
       -- ^ Settings from the table conf.
       → ActiveState t
       -- ^ This td-element (to be able to differentiate against eTB event sources)
       -- we are making.
       → a                       -- ^ Input content (dwawEl-fun uses).
       → Event t (TableEvent t)  -- ^ An event somewhere on the table (eTB).
       → TableState t
       -- ^ This give information about the events and current state of
       -- the table (are we in or out, is mouse button pressed or not etc).
       → m (TableEvent t)        -- ^ Events we want this cell to return.
tdComb tdFuns mae me a eTB tblSt = do
    -- _actListen, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that listening and activation should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu ._tdfADEs) tdFuns mae eTB tblSt
        $ (_actListen . _tdfADEs ) tdFuns me
    -- outputActiveState me "in tdco, me"
    -- outputActiveState actS "in tdco, actS"
    e <- (_drawEl. _tdfADEs) tdFuns a actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- td-element (that is, a cell) inside tbody rows (tr-elements).
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
       -- Functions: listening, activity management, drawing and event capturing.
      → Dynamic t (Maybe [ActiveState t])
      -- ^ Settings from the table conf.
      → ActiveState t
      -- ^ This td-element (to be able to differentiate against eTB event sources)
      -- we are making.
      → a                       -- ^ Input content (dwawEl-fun uses).
      → Event t (TableEvent t)  -- ^ An event somewhere on the table.
      → TableState t
      → m (TableEvent t)        -- ^ This returns default.
tdCombLstner tdFuns mae me a eTB tblSt = do
    actS <- (_actFu . _tdfADEs) tdFuns mae eTB tblSt
                $ (_actListen . _tdfADEs) tdFuns me
    _ <- (_drawEl . _tdfADEs) tdFuns a actS
    pure def

----------------------------------------------------------------------------------

-- | Like 'tdComb' but doesn't return any events (just default that is
-- full of never's) nor doesn't handle states. This just calls the
-- drawing function.
tdCombSilentPlain ∷ forall t m a. (Reflex t, MonadHold t m, TriggerEvent t m
                              , PostBuild t m, DomBuilder t m,  MonadJSM m)
      ⇒ TdFuns t m a
      -- Functions: listening, activity management, drawing and event capturing.
      → Dynamic t (Maybe [ActiveState t])
      -- ^ Settings from the table conf.
      → ActiveState t
      -- ^ This td-element (to be able to differentiate against eTB event sources)
      -- we are making.
      → a                       -- ^ Input content (dwawEl-fun uses).
      → Event t (TableEvent t)  -- ^ An event somewhere on the table.
      → TableState t
      → m (TableEvent t)        -- ^ This returns default.
tdCombSilentPlain tdFuns _mae _me a _eTB _tblSt = do
    _ <- (_drawEl . _tdfADEs) tdFuns a def
    pure def

--------------------------------------------------------------------------------

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defTdAttrF :: forall t. Reflex t => ActiveState t → Dynamic t ETd
defTdAttrF ast =
    let dA = view activeStateActive ast
        dAv = view activeStateActivable ast
        dAGl = view activeStateActiveGl ast
        dNAGl = view activeStateNotActiveGl ast
        dNAvGl = view activeStateNotActivableGl ast
     in (\bA bAv gA gNa gNav -> attrSetGlobals
         (if bAv
            then if bA
                then gA
                else gNa
            else gNav
         ) def
        ) <$> dA <*> dAv <*> dAGl <*> dNAGl <*> dNAvGl


--------------------------------------------------------------------------------

instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
         , PostBuild t m, MonadJSM m, Show a, DomBuilderSpace m ~ GhcjsDomSpace)
          => Default (TdFuns t m a)
    where
      def = TdFuns def tdComb defTdAttrF (const def) def

--------------------------------------------------------------------------------

-- | No cell is listening for events and no cell is having activity management.
silentPlainTdFuns :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                     , DomBuilderSpace m ~ GhcjsDomSpace)
                  => TdFuns t m a
silentPlainTdFuns =
    TdFuns silentPlainADE tdCombSilentPlain defTdAttrF (const def) def



--------------------------------------------------------------------------------

-- | A pre-version of 'tdComb'.
-- This makes the visible contents inside a td-element, that is,
-- an extra div.
-- And in addition to that, this returns events from that div.
tdFun ∷ forall t m. (Reflex t, MonadHold t m, TriggerEvent t m,
                      PostBuild t m, DomBuilder t m,  MonadJSM m,
                      DomBuilderSpace m ~ GhcjsDomSpace)
      ⇒ (Dynamic t (Maybe [ActiveState t])
            → Event t (TableEvent t) → TableState t
            → ActiveState t → m (ActiveState t))
      → Dynamic t (Maybe [ActiveState t])
      → ActiveState t
      → Event t (TableEvent t)
      → TableState t
      → m (TableEvent t)
tdFun actF mae me eTB tblSt = do
    let ac = listenMe me
    actS <- actF mae eTB tblSt ac
    e <- drawDivActElemEx () actS
    cellEvF actS e
    -- let actstate txt = def & set activeStateElem txt
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
--   let actstate txt = def & set activeStateElem txt
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

