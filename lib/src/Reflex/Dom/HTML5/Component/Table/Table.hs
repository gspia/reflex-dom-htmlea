{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.Table
Description : Helpers to build tables.
Copyright   : (c) gspia 2017 -
License     : BSD
Maintainer  : gspia

= Table

A table interface for vector inputs.

Function 'mkTable' needs

    * A matrix (vector of vectors) input.
    * A set of functions that defines, how things are shown up and what happens on
      events (e.g. on mouse button click). See 'TdFuns'.
    * Optional: a definition about caption. See 'CaptionConf'.
    * Optional: a definition about table headers. See 'HeaderConfV' and 'ThFuns'.
    * Optional: a definition about table (column) footers.
      See 'FootConf' and 'TfootFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.Table
    (
    -- * Column headers
    --
    -- | We give styling through 'Col', 'ColGroup', 'Thead', 'Tr' and 'Th'
    -- elements.
    --
    -- We give drawing function (that can use state information) in 'ThFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    HeaderConf (..)
    , colThFuns
    , colConfs
    , colText
    , colTheadAttrs
    , mkThead

    -- * Column footers
    --
    -- | We give styling through 'Tfoot', 'Tr' and  'Td' elements.
    --
    -- We give drawing function (that can use state information) in 'TfootFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , FootConf (..)
    , footFuns
    , footText
    , footAttrs
    , mkTfoot

    -- * Table construction
    --
    -- | We give styling through 'Table' and 'Tbody' elements.
    --
    -- We give drawing function (that can use state information) in 'TdFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , TableConf (..)
    , tableTdFuns
    , tableCaption
    , tableHeader
    , tableFooter
    , tableTableAttr
    , tableEvents
    , tableRowFun
    , tableActivityConf
    , cellListenerBody
    , cellDrawBody
    , cellActivityBody
    , defTableConf
    , silentPlainTableConf
    , mkTable
    , mkRow

    -- * Drawing functions
    , module Reflex.Dom.HTML5.Component.Common.DrawFuns

    ) where

import           Control.Lens
import           Control.Monad.Fix
-- import           Data.Semigroup ((<>))
import           Data.Text                               (Text)
-- import qualified Data.Text                              as T
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as V

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

-- import Reflex.Dom.HTML5.Attrs.Globals
-- import Reflex.Dom.HTML5.Elements.Tabular
import           Reflex.Dom.HTML5.Component.Common.DrawFuns
import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.ActElem
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import qualified Reflex.Dom.HTML5.Elements as E
-- import           Reflex.Dom.HTML5.Attrs

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | The col and colgroup -element definitions with column header texts
-- to be used at the thead-element and before it. Text are put inside
-- thead. Thead contains a tr containing th's, that in turn, contain the texts.
-- The way each head (th) and tr are drawn (output), is specified with
-- 'ThFuns', see 'mkTheadV'.
data HeaderConf t m r = HeaderConf
    { _colThFuns     ∷ ThFuns t m r
    -- ^ See 'ThFuns'.
    , _colConfs       ∷ Vector (Dynamic t E.Col)
    -- ^ Colgroup -definitions.
    , _colText       ∷ Vector Text
    -- ^ Column header texts inside th-elements.
    , _colTheadAttrs ∷ Dynamic t E.Thead
    -- ^ Thead attributes.
    }

-- | A lens.
colThFuns ∷ Lens' (HeaderConf t m r) (ThFuns t m r)
colThFuns f (HeaderConf f1 f2 f3 f4) = fmap (\g → HeaderConf g f2 f3 f4) (f f1)

-- | A lens.
colConfs ∷ Lens' (HeaderConf t m r) (Vector (Dynamic t E.Col))
colConfs f (HeaderConf f1 f2 f3 f4) = fmap (\g → HeaderConf f1 g f3 f4) (f f2)

-- | A lens.
colText ∷ Lens' (HeaderConf t m r) (Vector Text)
colText f (HeaderConf f1 f2 f3 f4) = fmap (\g → HeaderConf f1 f2 g f4) (f f3)

-- | A lens.
colTheadAttrs ∷ Lens' (HeaderConf t m r) (Dynamic t E.Thead)
colTheadAttrs f (HeaderConf f1 f2 f3 f4) = fmap (HeaderConf f1 f2 f3) (f f4)

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part. This also constructs the
-- colgroup-elements.
mkThead ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                      , TriggerEvent t m, MonadJSM m, MonadFix m
                      , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                      , ActSretval r)
         ⇒ Maybe (HeaderConf t m r)
         -- ^ From table configurations. See 'ThFuns' and 'HeaderConfV'.
         →  Dynamic t (Maybe [ActiveState t ActElem r])
         -- ^ User given activity information obtained from table configurations.
         → Event t (CompEvent t ActElem r)
         -- ^ Event coming from table (can be from this header of from cells).
         → CompState t ActElem r
         -- ^ A function combined th's can use the current 'CompState'.
         → m (Event t (CompEvent t ActElem r))
         -- ^ Header returns the same events as cells (they get combined in
         -- the 'mkTable').
mkThead mColdefs mae evB tblSt =
    case mColdefs of
        Just (HeaderConf thFs cdefs ctxts dTheadA) → mdo
            let combF = _thfCombFun thFs (_thfADEs thFs)
            E.colGroupN $ V.forM_ cdefs (`E.colD` blank)
            E.theadD dTheadA $ do
                let hst = defActiveStateTbl
                    hElms = V.imap (\i c →
                        (hst & set activeStateElemId (ActEcolh i)
                        , c)) ctxts
                (_,evB') ← E.trN' $ do
                    eds ← V.forM hElms
                        (\(ve,txt) → mdo
                            -- Use the conf info given by the user
                            -- TODO see the mkRow for explanation
                            -- let meAst2 = confAst2Ast mae meAst
                            -- te ← E.thD (_thfThAttr thFs meAst2) $
                            te ← E.thD (_thfThAttr thFs $ _ceMe te) $
                                        -- _thfCombFun thFs thFs
                                        combF mae ve (constDyn txt) evB tblSt
                            -- let meAst ∷ ActiveState t ActElem = _ceMe te
                            pure te
                        )
                    pure $ compEvFiring $ V.toList eds
                pure evB'
        Nothing → pure never


--------------------------------------------------------------------------------

-- | Foot definitions specify a text for each column and attributes for
-- the tfoot-element. Tfoot contains a tr-element that contains td-elements.
-- The way each column foot (td) and tr are drawn (output), is specified with
-- 'TfootFuns', see 'mkTfoot'.
data FootConf t m r = FootConf
    { _footFuns  ∷ TfootFuns t m r
    -- ^ Functions specifying behavior and look. See 'TfootFuns'.
    , _footText  ∷ Vector Text
    -- ^ Texts inside td's.
    , _footAttrs ∷ Dynamic t E.Tfoot
    -- ^ Tfoot attributes.
    }

-- | A lens.
footFuns ∷ Lens' (FootConf t m r) (TfootFuns t m r)
footFuns f (FootConf f1 f2 f3) = fmap (\g → FootConf g f2 f3) (f f1)

-- | A lens.
footText ∷ Lens' (FootConf t m r) (Vector Text)
footText f (FootConf f1 f2 f3) = fmap (\g → FootConf f1 g f3) (f f2)

-- | A lens.
footAttrs ∷ Lens' (FootConf t m r) (Dynamic t E.Tfoot)
footAttrs f (FootConf f1 f2 f3) = fmap (FootConf f1 f2) (f f3)

--------------------------------------------------------------------------------

-- | Construct the html for the tfoot-part.
mkTfoot ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                      , TriggerEvent t m, MonadJSM m, MonadFix m
                      , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                      , ActSretval r)
         ⇒ Maybe (FootConf t m r)
         -- ^ From table configurations. See 'TdFuns' and 'FootConfV'.
         →  Dynamic t (Maybe [ActiveState t ActElem r])
         -- ^ User given activity information obtained from table configurations.
         → Event t (CompEvent t ActElem r)
         -- ^ Event coming from table (can be from, head, cells or from foot).
         → CompState t ActElem r
         -- ^ A function combining td's can use the current 'CompState'.
         → m (Event t (CompEvent t ActElem r))
         -- ^ Footer returns the same events as cells (they get combined in
         -- the 'mkTable').
mkTfoot mFootdefs mae evB tblSt =
    case mFootdefs of
        Just (FootConf tdFs ctxts dTfootA) → mdo
            let combF = _tfootCombFun tdFs (_tfootADEs tdFs)
            E.tfootD dTfootA $ do
                let tdst = defActiveStateTbl
                    tdElms = V.imap (\i c →
                        (tdst & set activeStateElemId (ActEcols i), c)) ctxts
                (_,evB') ← E.trN' $ do
                    eds ← V.forM tdElms
                        (\(ve,txt) → mdo
                            -- Use the conf info given by the user
                            -- TODO see the mkRow for explanation
                            -- let meAst2 = confAst2Ast mae meAst
                            -- te ← E.tdD (_tfootTdAttr tdFs meAst2) $
                            te ← E.tdD (_tfootTdAttr tdFs $ _ceMe te) $
                                         combF mae ve (constDyn txt) evB tblSt
                            -- let meAst ∷ ActiveState t ActElem = _ceMe te
                            pure te
                        )
                    pure $ compEvFiring $ V.toList eds
                pure evB'
        Nothing → pure never

--------------------------------------------------------------------------------

-- | Table configuration tells, how the table behaves on different
-- table and cell events, what kind of decorations we put and how they
-- change based on user actions.
data TableConf t m a r = TableConf
    { _tableTdFuns ∷ TdFuns t m a r
    -- ^ 'TdFuns' contains attributes and function definitions.
    , _tableCaption ∷ Maybe (CaptionConf t)
    -- ^ 'CaptionConf' contains attributes and text.
    , _tableHeader ∷ Maybe (HeaderConf t m r)
    -- ^ 'ThFuns' contains attrs and functions for Th-els.  Colgroup data with
    -- header texts and behavior.
    , _tableFooter ∷ Maybe (FootConf t m r)
    -- ^ 'TfootFuns' contains attrs and functions for Td-els with texts.
    , _tableTableAttr ∷ Dynamic t E.Table
    -- ^ Attributes for the table element (html-element).
    , _tableEvents ∷ Element EventResult (DomBuilderSpace m) t
                   →  m (CompOtherEvs t)
    -- ^ Events originating from table element (and not from cells).
    , _tableRowFun ∷ TableConf t m a r → Event t (CompEvent t ActElem r)
        → CompState t ActElem r
        → Int → Vector (ActiveState t ActElem r, Dynamic t a)
        → m (Event t (CompEvent t ActElem r))
    -- ^ Function that makes a row.
    , _tableActivityConf ∷ Dynamic t (Maybe [ActiveState t ActElem r])
    -- ^ Empty list means no cell is activated initially. This can also activate
    -- a row or column with a single 'ActElem' inside 'ActiveState'.
    }

    -- , _tableRowEvFilter ∷ Event t (CompEvent t ActElem) → Event t ActElem
    -- A filtering function that row making function uses. This affects
    -- to the dynamic ActElem that is given to the function that makes
    -- tr-attributes.

-- | A lens.
tableTdFuns ∷ Lens' (TableConf t m a r) (TdFuns t m a r)
tableTdFuns f (TableConf f1 f2 f3 f4 f5 f6 f7 f8 ) =
    fmap (\g → TableConf g f2 f3 f4 f5 f6 f7 f8 ) (f f1)

-- | A lens.
tableCaption ∷ Lens' (TableConf t m a r) (Maybe (CaptionConf t))
tableCaption f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 g f3 f4 f5 f6 f7 f8) (f f2)

-- | A lens.
tableHeader ∷ Lens' (TableConf t m a r) (Maybe (HeaderConf t m r))
tableHeader f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 f2 g f4 f5 f6 f7 f8) (f f3)

-- | A lens.
tableFooter ∷ Lens' (TableConf t m a r) (Maybe (FootConf t m r))
tableFooter f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 f2 f3 g f5 f6 f7 f8) (f f4)

-- | A lens.
tableTableAttr ∷ Lens' (TableConf t m a r) (Dynamic t E.Table)
tableTableAttr f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 f2 f3 f4 g f6 f7 f8) (f f5)

-- | A lens.
tableEvents ∷ Lens' (TableConf t m a r)
    (Element EventResult (DomBuilderSpace m) t →  m (CompOtherEvs t))
tableEvents f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 f2 f3 f4 f5 g f7 f8) (f f6)

-- | A lens.
tableRowFun ∷ Lens' (TableConf t m a r)
    (TableConf t m a r → Event t (CompEvent t ActElem r) → CompState t ActElem r
        → Int → Vector (ActiveState t ActElem r, Dynamic t a)
        → m (Event t (CompEvent t ActElem r)))
tableRowFun f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (\g → TableConf f1 f2 f3 f4 f5 f6 g f8) (f f7)

-- A lens.
-- tableRowEvFilter ∷ Lens' (TableConf t m a r)
    -- (Event t (CompEvent t ActElem r) → Event t ActElem)
-- tableRowEvFilter f (TableConf f1 f2 f3 f4 f5 f6 f7 f8 ) =
    -- fmap (\g → TableConf f1 f2 f3 f4 f5 f6 f7 g ) (f f8)

-- | A lens.
tableActivityConf ∷ Lens' (TableConf t m a r)
    (Dynamic t (Maybe [ActiveState t ActElem r]))
tableActivityConf f (TableConf f1 f2 f3 f4 f5 f6 f7 f8) =
    fmap (TableConf f1 f2 f3 f4 f5 f6 f7) (f f8)



-- | Cell listener -lens for tbody cells.
cellListenerBody ∷ Lens' (TableConf t m a r)
                          (ActiveState t ActElem r → ActiveState t ActElem r)
cellListenerBody = tableTdFuns . tdfADEs . adeListen

-- | Cell drawing -lens for tbody-cells
cellDrawBody ∷ Lens' (TableConf t m a r)
    (Dynamic t a → ActiveState t ActElem r
    → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
cellDrawBody = tableTdFuns . tdfADEs . adeDraw

-- | Cell activity function -lens for tbody-cells
cellActivityBody ∷ Lens' (TableConf t m a r)
    (Dynamic t (Maybe [ActiveState t ActElem r])
             → Event t (CompEvent t ActElem r)
             → CompState t ActElem r
             → ActiveState t ActElem r → m (ActiveState t ActElem r))
cellActivityBody = tableTdFuns . tdfADEs . adeActivate

--------------------------------------------------------------------------------


-- | A default value for a table. Take this and override needed parts:
-- most likely there is a need to tell, which drawing function to use.
defTableConf ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
               -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
               , DomBuilder t m, PostBuild t m, MonadJSM m
               , DomBuilderSpace m ~ GhcjsDomSpace
               , ActSretval r)
             ⇒ TableConf t m a r
defTableConf =
    TableConf defTdFuns Nothing Nothing Nothing (constDyn E.defTable)
        compEventsEnLe mkRow (constDyn Nothing)
        -- compEventsEnLe mkRow ceFilterMU (constDyn Nothing)

--------------------------------------------------------------------------------

-- | This is easier to start with if just needing a decorated table.
-- This allows normal selection and copying from the table.
silentPlainTableConf ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                       -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                       , DomBuilder t m, PostBuild t m, MonadJSM m
                       , DomBuilderSpace m ~ GhcjsDomSpace
                       , ActSretval r)
                     ⇒ TableConf t m a r
silentPlainTableConf =
    TableConf silentPlainTdFuns Nothing Nothing Nothing (constDyn E.defTable)
        compEventsNone mkRow (constDyn Nothing)
        -- compEventsNone mkRow ceFilterNone (constDyn Nothing)



--------------------------------------------------------------------------------

-- | Basic row building function that keeps up the information about the
-- last row having an event. 
--
-- The function ('_tdfTrAttr') that makes tr-attributes can use the
-- last event row and current building row information.
--
-- Note: this function and the filters are probably going to move to other
-- place, when the corresponding builders are made for headers and footers.
mkRow ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
     ⇒ TableConf t m a r
     -- ^ Table configurations.
     → Event t (CompEvent t ActElem r)
     -- ^ An event somewhere on the table.
     → CompState t ActElem r
     -- ^ 'CompState', i.e., the cell where last button release happened.
     → Int
     -- ^ Row number.
     → Vector (ActiveState t ActElem r, Dynamic t a)
     -- ^ A row, that is, cell-coord info and the content.
     → m (Event t (CompEvent t ActElem r))
mkRow tc evB tblSt i v = do
-- TODO add the following functionality, or is it needed at all?
-- (There are already listeners. And we may want to be able to track
-- many sources of events.)
-- We can select between mouse button press
-- ('ceFilterMD'), mouse button release ('ceFilterMU'), mouse enter
-- ('ceFilterMEnter') and having no events at all ('ceFilterNone').
    let tdFs = view tableTdFuns tc
        combF = _tdfCombFun tdFs (_tdfADEs tdFs)
        dmlst = view tableActivityConf tc
            ∷ Dynamic t (Maybe [ActiveState t ActElem r])
        astD = defActiveStateTbl & set activeStateElemId (ActErow i)
            ∷ ActiveState t ActElem r
    -- Keep state for the row and use the user supplied listeners etc.
    -- We keep row-state similarly to cell states. We may have to add
    -- separate "rowActMU/MD/Switch" functions etc.
    actS ∷ ActiveState t ActElem r
          ← (_adeActivate ._tdfADEs . _tableTdFuns) tc
                 (_tableActivityConf tc) evB tblSt
             $ (_adeListen . _tdfADEs . _tableTdFuns) tc astD
    evTE ← E.trD (_tdfTrAttr tdFs (constDyn actS) $ ActErow i) $ do
        eds ← V.forM v
            ( \(ve,ast2) → mdo
                -- Use the conf info given by the user, this is for the
                -- td-attributes.
                -- Hmm, it is probably enough that the combF handles the
                -- user given info/initial values and thus no need for confAst2Ast.
                -- At the same time, not using confAst2Ast makes e.g.
                -- table-exampleV15 much simpler and easier to handle.
                -- TODO is it safe to remove the meAst?
                -- let meAst = confAst2Ast dmlst $ _ceMe te
                -- te ← E.tdD (_tdfTdAttr tdFs meAst) $
                te ← E.tdD (_tdfTdAttr tdFs $ _ceMe te) $
                    combF dmlst ve ast2 evB tblSt
                    -- meAst in place of ve would cause a loop, note that
                    -- its definition requires recursive do.
                pure te
            )
        pure $ compEvFiring $ V.toList eds
    pure evTE



--------------------------------------------------------------------------------

-- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim).
-- 'TdFuns' gives the look and behavior of the table by specifying functions
-- to drawing the contents of td-elements ("the cells"), to acting on
-- different events etc. See examples on their usage.
-- 'TdFuns' and some pre-defined functions can be found from 'TableCommon' module.
mkTable ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
         ⇒ TableConf t m a r
         -- ^ 'TableConf' contains definitions, that is, functions and dynamic
         -- attributes that are used to construct a table element.
         → Vector (Vector a)
         -- ^ Contents of the table.
         → m (CompState t ActElem r)
         -- ^ 'CompState' has event and state variables.
mkTable tc acElmsVV = mdo
    let elms = V.imap mkA4r acElmsVV
    (htTable, tblState) ← E.tableD' (view tableTableAttr tc) $ mdo
        mkCaption (view tableCaption tc)
        evB2 ← mkThead (view tableHeader tc)
                       (view tableActivityConf tc)
                       evB tblSt
        (_,evB1) ← E.tbodyD' (view (tableTdFuns . tdfTbodyAttr) tc) $ do
            ets ← V.imapM (mkRow tc evB tblSt) elms
            pure $ leftmost $ V.toList ets
        evB3 ← mkTfoot (view tableFooter tc)
                       (view tableActivityConf tc)
                       evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt ← updateCompState tblOE evB
        pure tblSt
    tblOE ← view tableEvents tc htTable
    pure tblState
    where
      -- TODO TODO TODO use the provided activity information in tc
      -- (or is the current way of using it in the functions making rows enough)
      ast ∷ ActiveState t ActElem r
      ast = defActiveStateTbl
      mkA4r ∷ Int → Vector a → Vector (ActiveState t ActElem r, Dynamic t a)
      mkA4r i = V.imap (\j e → (ast & set activeStateElemId (ActERC (i,j))
                               , constDyn e))


