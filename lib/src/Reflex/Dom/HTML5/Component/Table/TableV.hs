{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
-- {-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.TableV
Description : Helpers to build tables.
Copyright   : (c) gspia 2017 -
License     : BSD
Maintainer  : gspia

= TableV

A table interface for vector inputs.

Function 'mkTableV' needs

    * A matrix (vector of vectors) input.
    * A set of functions that defines, how things are shown up and what happens on
      events (e.g. on mouse button click). See 'TdFuns'.
    * Optional: a definition about caption. See 'CaptionConf'.
    * Optional: a definition about table headers. See 'HeaderConfV' and 'ThFuns'.
    * Optional: a definition about table (column) footers.
      See 'FootConfV' and 'TfootFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.TableV
    (
    -- * Column headers
    --
    -- | We give styling through ECol, EColGroup, EThead, ETr and ETh.
    --
    -- We give drawing function (that can use state information) in 'ThFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    HeaderConfV (..)
    , colThFunsV
    , colConfsV
    , colTextV
    , colTheadAttrsV
    , mkTheadV

    -- * Column footers
    --
    -- | We give styling through ETfoot, ETr and  ETd.
    --
    -- We give drawing function (that can use state information) in 'TfootFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , FootConfV (..)
    , footFunsV
    , footTextV
    , footAttrsV
    , mkTfootV

    -- * Table construction
    --
    -- | We give styling through ETable and ETbody.
    --
    -- We give drawing function (that can use state information) in 'TdFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , TableConfV (..)
    , tableTdFunsV
    , tableCaptionV
    , tableHeaderV
    , tableFooterV
    , tableTableAttrV
    , tableEventsV
    , tableRowFunV
    , tableRowEvFilterV
    , tableActivityConfV
    , cellListenerBodyV
    , cellDrawBodyV
    , cellActivityBodyV
    , silentPlainTableConfV
    , mkTableV
    , mkRowV
    ) where

import           Control.Lens
-- import           Control.Monad
import           Control.Monad.Fix
import           Data.Default
-- import           Data.Maybe                              (fromMaybe)
import           Data.Text                               (Text)
-- import qualified Data.Text                              as T
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as V

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

-- import Reflex.Dom.HTML5.Attrs.Globals
-- import Reflex.Dom.HTML5.Elements.Tabular
import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.StateInfo
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Elements
-- import           Reflex.Dom.HTML5.Attrs

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | The col and colgroup -element definitions with column header texts
-- to be used at the thead-element and before it. Text are put inside
-- thead. Thead contains a tr containing th's, that in turn, contain the texts.
-- The way each head (th) and tr are drawn (output), is specified with
-- 'ThFuns', see 'mkTheadV'.
data HeaderConfV t m = HeaderConfV
    { _colThFunsV     :: ThFuns t m
    -- ^ See 'ThFuns'.
    , _colConfsV       :: Vector (Dynamic t ECol)
    -- ^ Colgroup -definitions.
    , _colTextV       :: Vector Text
    -- ^ Column header texts inside th-elements.
    , _colTheadAttrsV :: Dynamic t EThead
    -- ^ Thead attributes.
    }

-- | A lens.
colThFunsV :: Lens' (HeaderConfV t m) (ThFuns t m)
colThFunsV f (HeaderConfV f1 f2 f3 f4) = fmap (\g -> HeaderConfV g f2 f3 f4) (f f1)

-- | A lens.
colConfsV :: Lens' (HeaderConfV t m) (Vector (Dynamic t ECol))
colConfsV f (HeaderConfV f1 f2 f3 f4) = fmap (\g -> HeaderConfV f1 g f3 f4) (f f2)

-- | A lens.
colTextV :: Lens' (HeaderConfV t m) (Vector Text)
colTextV f (HeaderConfV f1 f2 f3 f4) = fmap (\g -> HeaderConfV f1 f2 g f4) (f f3)

-- | A lens.
colTheadAttrsV :: Lens' (HeaderConfV t m) (Dynamic t EThead)
colTheadAttrsV f (HeaderConfV f1 f2 f3 f4) = fmap (HeaderConfV f1 f2 f3) (f f4)

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part. This also constructs the
-- colgroup-elements.
mkTheadV ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (HeaderConfV t m)
         -- ^ From table configurations. See 'ThFuns' and 'HeaderConfV'.
         →  Dynamic t (Maybe [ActiveState t])
         -- ^ User given activity information obtained from table configurations.
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from this header of from cells).
         → TableState t
         -- ^ A function combined th's can use the current 'TableState'.
         → m (Event t (TableEvent t))
         -- ^ Header returns the same events as cells (they get combined in
         -- the 'mkTableV').
mkTheadV mColdefs mae evB tblSt =
    case mColdefs of
        Just (HeaderConfV thFs cdefs ctxts dTheadA) -> mdo
            eColGroupN $ V.forM_ cdefs (`eColD` blank)
            eTheadD dTheadA $ do
                let hst = def :: ActiveState t
                    hElms = V.imap (\i c ->
                        (hst & set activeStateElem (ActEcolh i)
                        , c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM hElms
                        (\(ve,txt) -> mdo
                            -- Use the conf info given by the user
                            let meAst2 = confAst2Ast mae meAst
                            te <- eThD (_thfThAttr thFs meAst2) $
                                        _thfCombFun thFs thFs
                                        mae ve txt evB tblSt
                            let meAst :: ActiveState t = _teMe te
                            pure te
                        )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never


--------------------------------------------------------------------------------

-- | Foot definitions specify a text for each column and attributes for
-- the tfoot-element. Tfoot contains a tr-element that contains td-elements.
-- The way each column foot (td) and tr are drawn (output), is specified with
-- 'TfootFuns', see 'mkTfootV'.
data FootConfV t m = FootConfV
    { _footFunsV  :: TfootFuns t m
    -- ^ Functions specifying behavior and look. See 'TfootFuns'.
    , _footTextV  :: Vector Text
    -- ^ Texts inside td's.
    , _footAttrsV :: Dynamic t ETfoot
    -- ^ Tfoot attributes.
    }

-- | A lens.
footFunsV :: Lens' (FootConfV t m) (TfootFuns t m)
footFunsV f (FootConfV f1 f2 f3) = fmap (\g -> FootConfV g f2 f3) (f f1)

-- | A lens.
footTextV :: Lens' (FootConfV t m) (Vector Text)
footTextV f (FootConfV f1 f2 f3) = fmap (\g -> FootConfV f1 g f3) (f f2)

-- | A lens.
footAttrsV :: Lens' (FootConfV t m) (Dynamic t ETfoot)
footAttrsV f (FootConfV f1 f2 f3) = fmap (FootConfV f1 f2) (f f3)

--------------------------------------------------------------------------------

-- | Construct the html for the tfoot-part.
mkTfootV ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (FootConfV t m)
         -- ^ From table configurations. See 'TdFuns' and 'FootConfV'.
         →  Dynamic t (Maybe [ActiveState t])
         -- ^ User given activity information obtained from table configurations.
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from, head, cells or from foot).
         → TableState t
         -- ^ A function combining td's can use the current 'TableState'.
         → m (Event t (TableEvent t))
         -- ^ Footer returns the same events as cells (they get combined in
         -- the 'mkTableV').
mkTfootV mFootdefs mae evB tblSt =
    case mFootdefs of
        Just (FootConfV tdFs ctxts dTfootA) -> mdo
            eTfootD dTfootA $ do
                let tdst = def :: ActiveState t
                    tdElms = V.imap (\i c ->
                        (tdst & set activeStateElem (ActEcols i)
                        , c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM tdElms
                        (\(ve,txt) -> mdo
                            -- Use the conf info given by the user
                            let meAst2 = confAst2Ast mae meAst
                            te <- eTdD (_tfootTdAttr tdFs meAst2) $
                                         _tfootCombFun tdFs tdFs
                                         mae ve txt evB tblSt
                            let meAst :: ActiveState t = _teMe te
                            pure te
                        )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Table configuration tells, how the table behaves on different
-- table and cell events, what kind of decorations we put and how they
-- change based on user actions.
data TableConfV t m a = TableConfV
    { _tableTdFunsV ∷ TdFuns t m a
    -- ^ 'TdFuns' contains attributes and function definitions.
    , _tableCaptionV ∷ Maybe (CaptionConf t)
    -- ^ 'CaptionConf' contains attributes and text.
    , _tableHeaderV ∷ Maybe (HeaderConfV t m)
    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
    -- header texts and behavior.
    , _tableFooterV ∷ Maybe (FootConfV t m)
    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
    , _tableTableAttrV ∷ Dynamic t ETable
    -- ^ Attributes for the table element.
    , _tableEventsV ∷ Element EventResult (DomBuilderSpace m) t
                   ->  m (TableOtherEvs t)
    -- ^ Events originating from table element (and not from cells).
    , _tableRowFunV ∷ TableConfV t m a → Event t (TableEvent t) → TableState t
        → Int → Vector (ActiveState t, a) → m (Event t (TableEvent t))
    -- ^ Function that makes a row.
    , _tableRowEvFilterV ∷ Event t (TableEvent t) → Event t ActElem
    -- ^ A filtering function that row making function uses. This affects
    -- to the dynamic ActElem that is given to the function that makes
    -- tr-attributes.
    , _tableActivityConfV :: Dynamic t (Maybe [ActiveState t])
    -- ^ Empty list means no cell is activated initially. This can also activate
    -- a row or column with a single 'ActElem' inside 'ActiveState'.
    }

-- | A lens.
tableTdFunsV :: Lens' (TableConfV t m a) (TdFuns t m a)
tableTdFunsV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV g f2 f3 f4 f5 f6 f7 f8 f9) (f f1)

-- | A lens.
tableCaptionV :: Lens' (TableConfV t m a) (Maybe (CaptionConf t))
tableCaptionV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 g f3 f4 f5 f6 f7 f8 f9) (f f2)

-- | A lens.
tableHeaderV :: Lens' (TableConfV t m a) (Maybe (HeaderConfV t m))
tableHeaderV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 g f4 f5 f6 f7 f8 f9) (f f3)

-- | A lens.
tableFooterV :: Lens' (TableConfV t m a) (Maybe (FootConfV t m))
tableFooterV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 f3 g f5 f6 f7 f8 f9) (f f4)

-- | A lens.
tableTableAttrV :: Lens' (TableConfV t m a) (Dynamic t ETable)
tableTableAttrV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 f3 f4 g f6 f7 f8 f9) (f f5)

-- | A lens.
tableEventsV :: Lens' (TableConfV t m a)
    (Element EventResult (DomBuilderSpace m) t ->  m (TableOtherEvs t))
tableEventsV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 f3 f4 f5 g f7 f8 f9) (f f6)

-- | A lens.
tableRowFunV :: Lens' (TableConfV t m a)
    (TableConfV t m a → Event t (TableEvent t) → TableState t
        → Int → Vector (ActiveState t, a) → m (Event t (TableEvent t)))
tableRowFunV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 f3 f4 f5 f6 g f8 f9) (f f7)
-- | A lens.
tableRowEvFilterV :: Lens' (TableConfV t m a)
    (Event t (TableEvent t) → Event t ActElem)
tableRowEvFilterV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfV f1 f2 f3 f4 f5 f6 f7 g f9) (f f8)

-- | A lens.
tableActivityConfV :: Lens' (TableConfV t m a)
    (Dynamic t (Maybe [ActiveState t]))
tableActivityConfV f (TableConfV f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (TableConfV f1 f2 f3 f4 f5 f6 f7 f8) (f f9)



-- | Cell listener -lens for tbody cells.
cellListenerBodyV :: Lens' (TableConfV t m a) (ActiveState t -> ActiveState t)
cellListenerBodyV = tableTdFunsV . tdfADEs . actListen

-- | Cell drawing -lens for tbody-cells
cellDrawBodyV :: Lens' (TableConfV t m a)
    (a -> ActiveState t
    -> m (Element EventResult (DomBuilderSpace m) t))
cellDrawBodyV = tableTdFunsV . tdfADEs . drawEl

-- | Cell activity function -lens for tbody-cells
cellActivityBodyV :: Lens' (TableConfV t m a)
    (Dynamic t (Maybe [ActiveState t])
             → Event t (TableEvent t)
             → TableState t
             → ActiveState t → m (ActiveState t))
cellActivityBodyV = tableTdFunsV . tdfADEs . actFu

--------------------------------------------------------------------------------


instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
         , PostBuild t m, MonadJSM m, Show a, DomBuilderSpace m ~ GhcjsDomSpace)
      => Default (TableConfV t m a)
    where
      def = TableConfV def def def def (constDyn def)
            tableEventsEnLe mkRowV teFilterMU (constDyn def)

--------------------------------------------------------------------------------

-- | This is easier to start with if just needing a decorated table.
-- This allows normal selection and copying from the table.
silentPlainTableConfV :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                        , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                        , DomBuilderSpace m ~ GhcjsDomSpace)
                     => TableConfV t m a
silentPlainTableConfV =
    TableConfV silentPlainTdFuns def def def (constDyn def)
    tableEventsNone mkRowV teFilterNone
    (constDyn def)



--------------------------------------------------------------------------------

-- | Basic row building function that keeps up the information about the
-- last row having an event. We can select between mouse button press
-- ('teFilterMD'), mouse button release ('teFilterMU'), mouse enter
-- ('teFilterMEnter') and having no events at all ('teFilterNone').
--
-- The function ('_tdfTrAttr') that makes tr-attributes can use the
-- last event row and current building row information.
--
-- Note: this function and the filters are probably going to move to other
-- place, when the corresponding builders are made for headers and footers.
mkRowV ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
     ⇒ TableConfV t m a
     -- ^ Table configurations.
     → Event t (TableEvent t)
     -- ^ An event somewhere on the table.
     → TableState t
     -- ^ 'TableState', i.e., the cell where last button release happened.
     → Int
     -- ^ Row number.
     → Vector (ActiveState t, a)
     -- ^ A row, that is, cell-coord info and the content.
     → m (Event t (TableEvent t))
mkRowV tc evB tblSt i v = do
    let tdFs = view tableTdFunsV tc
        dmlst = view tableActivityConfV tc :: Dynamic t (Maybe [ActiveState t])
        astD = def & set activeStateElem (ActErow i) :: ActiveState t
    -- Keep state for the row and use the user supplied listeners etc.
    -- We keep row-state similarly to cell states. We may have to add
    -- separate "rowActMU/MD/Switch" functions etc.
    actS :: ActiveState t
          <- (_actFu ._tdfADEs . _tableTdFunsV) tc
                 (_tableActivityConfV tc) evB tblSt
             $ (_actListen . _tdfADEs . _tableTdFunsV) tc astD
    evTE <- eTrD (_tdfTrAttr tdFs (constDyn actS) $ ActErow i) $ do
        eds <- V.forM v
            ( \(ve,ast2) -> mdo
                -- Use the conf info given by the user, this is for the
                -- td-attributes.
                let meAst = confAst2Ast dmlst $ _teMe te
                te <- eTdD (_tdfTdAttr tdFs meAst) $
                    _tdfCombFun tdFs tdFs dmlst ve ast2 evB tblSt
                    -- meAst in place of ve would cause a loop, note that
                    -- its definition requires recursive do.
                pure te
            )
        pure $ tblEvFiring $ V.toList eds
    pure evTE

-- TODO the following can probably be removed...
    -- evPB <- getPostBuild
        -- evAEout :: Event t ActElem = view tableRowEvFilter tc evB
        -- evSta = updated (view tsDynURelease tblSt)
        -- dlst = (fromMaybe []) <$> dmlst
        -- daes = map (view activeStateElem) <$> dlst
        -- dmast = (\mast -> case mast of
        --                      Just astlst -> fstInsideASt astD astlst
        --                      Nothing     -> Just astD
        --         ) <$> dmlst
        -- evAst2 = tag (current dmast) evPB
    -- dBoInit <- holdDyn False $ leftmost [False <$ evAEout, True <$ evAst2]
    -- The following state is kept for each row separately. So, unless given
    -- outside mkRow, it only keeps information of the last cell pressed on the
    -- row. And no information from the other rows.
    -- dAEout :: Dynamic t ActElem <- holdDyn ActEnone evAEout
    -- dAstout <- holdDyn def evSta
    -- If initializing, use the provided inputs, else use the input given
    -- by the event. This is for the tr-attributes.
    -- let dAst :: Dynamic t (ActiveState t)
    --       = (\b mast ae ->
    --           if b
    --              then fromMaybe def mast
    --              else def & set activeStateElem ae
    --         ) <$> dBoInit <*> dmast <*> dAEout
    -- let dAst2 :: Dynamic t (ActiveState t)
    --       = (\b mast ast ->
    --           if b
    --              then fromMaybe def mast
    --              else ast
    --         ) <$> dBoInit <*> dmast <*> dAstout

                    -- ePN $ do
                    --     text "b init = "
                    --     dynText $ (T.pack . show) <$>  dBoInit
                    --     text ", drawing row i = "
                    --     text $ (T.pack . show) i
                    --     text ", daes:"
                    --     dynText $ (T.pack . show) <$> daes
                    --     text ";"
                    -- outputActiveStateDyn dAst "dAst" -- works, no loop
                    --
                    -- outputActiveState meAst "meast" -- this causes a loop
                    -- confAst2Ast modifies replaces most of the fields of
                    -- _teMe te -input
                    -- and returns meAst. Only the ActElem is not replaced.
                    -- And the definition of meAst requires mfix.
                    --





--------------------------------------------------------------------------------

-- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim).
-- 'TdFuns' gives the look and behavior of the table by specifying functions
-- to drawing the contents of td-elements ("the cells"), to acting on
-- different events etc. See examples on their usage.
-- 'TdFuns' and some pre-defined functions can be found from 'TableCommon' module.
mkTableV ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ TableConfV t m a
         -- ^ 'TableConf' contains definitions, that is, functions and dynamic
         -- attributes that are used to construct a table element.
         → Vector (Vector a)
         -- ^ Contents of the table.
         → m (TableState t)
         -- ^ 'TableState' has event and state variables.
mkTableV tc acElmsVV = mdo
    let elms = V.imap mkA4r acElmsVV
    (htTable, tblState) <- eTableD' (view tableTableAttrV tc) $ mdo
        mkCaption (view tableCaptionV tc)
        evB2 <- mkTheadV (view tableHeaderV tc)
                         (view tableActivityConfV tc)
                         evB tblSt
        (_,evB1) <- eTbodyD' (view (tableTdFunsV . tdfTbodyAttr) tc) $ do
            ets <- V.imapM (mkRowV tc evB tblSt) elms
            pure $ leftmost $ V.toList ets
        evB3 <- mkTfootV (view tableFooterV tc)
                         (view tableActivityConfV tc)
                         evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt <- updateTableState tblOE evB
        pure tblSt
    tblOE <- view tableEventsV tc htTable
    pure tblState
    where
      -- TODO TODO TODO use the provided activity information in tc
      -- (or is the current way of using it in the functions making rows enough)
      ast :: ActiveState t
      ast = def
      mkA4r ∷ Int → Vector a → Vector (ActiveState t, a)
      mkA4r i = V.imap (\j e -> (ast & set activeStateElem (ActERC (i,j)) , e))


