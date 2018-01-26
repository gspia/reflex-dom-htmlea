{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.TableV
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TableV

-}

module Reflex.Dom.HTML5.Component.TableV where

import           Control.Monad.Fix
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
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Elements

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Column header definitions for col and colgroup definitions with texts
-- to be used at the thead-element and before it.
data ColHeaderV t = ColHeaderV
    { _colDefsV       :: Vector (Dynamic t ECol) -- ^ colgroup -definitions
    , _colTextV       :: Vector Text             -- ^ column header texts
    , _colTheadAttrsV :: Dynamic t EThead  -- ^ thead attributes
    }

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part, if present.
mkTheadV ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (ThFuns t m, ColHeaderV t)
         -- ^ See 'ThFuns' and 'ColHeaderV'
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from this header of from cells)
         → TableState t
         -- ^ th-combining function can use the current tablestate
         → m (Event t (TableEvent t))
         -- ^ Header returns the same events as cells (they get combined).
mkTheadV mColdefs evB tblSt =
    case mColdefs of
        Just (thFs, ColHeaderV cdefs ctxts dTheadA) -> mdo
            eColGroupN $ V.forM_ cdefs (`eColD` blank)
            eTheadD dTheadA $ do
                let hElms = V.imap (\i c -> (ActEcolh i, c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM hElms
                        (\(ve,txt) -> eThD (_thfThAttr thFs ve)
                            $ _thfCombFun thFs thFs ve txt evB tblSt )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Column header definitions for col and colgroup definitions with texts
-- to be used at the thead-element and before it.
data FootDefsV t = FootDefsV
    { _footTextV  :: Vector Text       -- ^ texts inside td's
    , _footAttrsV :: Dynamic t ETfoot  -- ^ tfoot attributes
    }

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part, if present.
mkTfootV ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (TfootFuns t m, FootDefsV t)
         -- ^ See 'TdFuns' and 'FootDefsV'
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from, head, cells or from foot)
         → TableState t
         -- ^ td-combining function can use the current tablestate
         → m (Event t (TableEvent t))
         -- ^ Footer returns the same events as cells (they get combined).
mkTfootV mFootdefs evB tblSt =
    case mFootdefs of
        Just (tdFs, FootDefsV ctxts dTfootA) -> mdo
            eTfootD dTfootA $ do
                let tdElms = V.imap (\i c -> (ActEcols i, c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM tdElms
                        (\(ve,txt) -> eTdD (_tfootTdAttr tdFs ve)
                            $ _tfootCombFun tdFs tdFs ve txt evB tblSt )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim).
-- TdFuns gives the look and behavior of the table by specifying functions
-- to drawing the contents of td-elements ("the cells"), to acting on
-- different events etc. See examples on their usage.
-- TdFuns and some pre-defined functions can be found from 'TableCommon' module.
mkTableV ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ TdFuns t m a
         -- ^ 'TdFuns' contains attributes and function definitions
         → Maybe (CaptionDef t)
         -- ^ 'CaptionDef' contains attributes and text
         → Maybe (ThFuns t m, ColHeaderV t)
         -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
         -- header texts and behavior.
         → Maybe (TfootFuns t m, FootDefsV t)
         -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
         → Vector (Vector a) -- ^ Contents of the table
         → m (TableState t)  -- ^ 'TableState' has event and state variables.
mkTableV tdFs mcapdefs mColdefs mFootdefs acElmsVV = mdo
    let elms = V.imap mkA4r acElmsVV
    (htTable, tblState) <- eTableD' (_tdfTableAttr tdFs) $ mdo
        mkCaption mcapdefs
        evB2 <- mkTheadV mColdefs evB tblSt
        (_,evB1) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
            ets <- V.imapM (mkRow evB tblSt) elms
            pure $ leftmost $ V.toList ets
        evB3 <- mkTfootV mFootdefs evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt <- updateTableState tblOE evB
        pure tblSt
    -- let eMoEn = domEvent Mouseenter htTable
    --     eMoLe = domEvent Mouseleave htTable
    --     tblOE = TableOtherEvs eMoEn eMoLe
    tblOE <- tableEvents htTable
    pure tblState
    where
      mkA4r ∷ Int → Vector a → Vector (ActElem, a)
      mkA4r i = V.imap (\j e -> (ActERC (i,j),e))
      --
      mkRow ∷ Event t (TableEvent t)
            → TableState t
            → Int
            → Vector (ActElem, a)
            → m (Event t (TableEvent t))
      mkRow evB tblSt2 i v =
        eTrD (_tdfTrAttr tdFs $ ActErow i) $ do
            eds <- V.forM v
                (\(ve,ae) -> eTdD (_tdfTdAttr tdFs ve)
                    $ _tdfCombFun tdFs tdFs ve ae evB tblSt2 )
                -- Note that in the _tdCombFun tdFs tdFs the first is used
                -- to get the combining function and call it. The second is
                -- a parameter to that function.
            pure $ tblEvFiring $ V.toList eds

