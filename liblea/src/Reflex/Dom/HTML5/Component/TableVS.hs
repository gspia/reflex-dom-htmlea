{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.HTML5.Component.TableVS
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TableVS

-}

module Reflex.Dom.HTML5.Component.TableVS where

import           Control.Monad.Fix
import           Data.Singletons.TypeLits
-- import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text)
-- import qualified Data.Text                               as T
import qualified Data.Vector.Sized                       as W

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Elements

-- import           Data.Finite
import           Data.Singletons
-- import           Data.Singletons.Decide

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

data ColHeaderVS c t = ColHeaderVS
    { _colDefsVS       :: W.Vector c (Dynamic t ECol)
    , _colTextVS       :: W.Vector c Text
    , _colTheadAttrsVS :: Dynamic t EThead
    }

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part, if present.
mkTheadVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
  , TriggerEvent t m, MonadJSM m, MonadFix m
  , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
  ⇒ Maybe (ThFuns t m, ColHeaderVS cols t)
  → Event t (TableEvent t)
  → TableState t
  → m (Event t (TableEvent t))
mkTheadVS mColdefs evB tblSt =
    case mColdefs of
        Just (thFs, ColHeaderVS cdefs ctxts dTheadA) -> mdo
            eColGroupN $ W.forM_ cdefs (`eColD` blank)
            eTheadD dTheadA $ do
                let hElms = W.imap (\i c -> (ActEcolh i, c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- W.forM hElms
                        (\(ve,txt) -> eThD (_thfThAttr thFs ve)
                            $ _thfCombFun thFs thFs ve txt evB tblSt )
                    pure $ tblEvFiring $ W.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Column header definitions for col and colgroup definitions with texts
-- to be used at the thead-element and before it.
data FootDefsVS c t = FootDefsVS
    { _footTextVS  :: W.Vector c Text   -- ^ texts inside td's
    , _footAttrsVS :: Dynamic t ETfoot  -- ^ tfoot attributes
    }

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part, if present.
mkTfootVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (TfootFuns t m, FootDefsVS cols t)
         -- ^ See 'TdFuns' and 'FootDefsV'
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from, head, cells or from foot)
         → TableState t
         -- ^ td-combining function can use the current tablestate
         → m (Event t (TableEvent t))
         -- ^ Footer returns the same events as cells (they get combined).
mkTfootVS mFootdefs evB tblSt =
    case mFootdefs of
        Just (tdFs, FootDefsVS ctxts dTfootA) -> mdo
            eTfootD dTfootA $ do
                let tdElms = W.imap (\i c -> (ActEcols i, c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- W.forM tdElms
                        (\(ve,txt) -> eTdD (_tfootTdAttr tdFs ve)
                            $ _tfootCombFun tdFs tdFs ve txt evB tblSt )
                    pure $ tblEvFiring $ W.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim). This module
-- use Vector-definition given in the Vector-Sized -package.
-- TdFuns gives the look and behavior of the table by specifying functions
-- to drawing the contents of td-elements ("the cells"), to acting on
-- different events etc. See examples on their use.
-- TdFuns and some pre-defined functions can be found from 'TableCommon' module.
mkTableVS ∷ forall rows cols t m a.
    (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m,
    MonadJSM m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace,
    KnownNat rows, KnownNat cols)
    ⇒ TdFuns t m a
    -- ^ 'TdFuns' contains attributes and function definitions
    → Maybe (CaptionDef t)
    -- ^ 'CaptionDef' contains attributes and text
    → Maybe (ThFuns t m, ColHeaderVS cols t)
    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
    -- header texts and behavior.
    → Maybe (TfootFuns t m, FootDefsVS cols t)
    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
    → W.Vector rows  (W.Vector cols a) -- ^ Contents of the table
    → m (TableState t) -- ^ 'TableState' gives event and state variables.
mkTableVS = mkTableVS_ sing sing


-- | See 'mkTableVS'. Sometimes it nice to be able to give size also as
-- parameters.
mkTableVS_ ∷ forall rows cols t m a.
    (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m,
    MonadJSM m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace,
    KnownNat rows, KnownNat cols)
    ⇒  Sing rows
    → Sing cols
    → TdFuns t m a
    -- ^ 'TdFuns' contains attributes and function definitions
    → Maybe (CaptionDef t)
    -- ^ 'CaptionDef' contains attributes and text
    → Maybe (ThFuns t m, ColHeaderVS cols t)
    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
    -- header texts and behavior.
    → Maybe (TfootFuns t m, FootDefsVS cols t)
    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
    → W.Vector rows  (W.Vector cols a) -- ^ Contents of the table
    → m (TableState t) -- ^ 'TableState' gives event and state variables.
mkTableVS_ _s1 _s2 tdFs mcapdefs mColdefs mFootdefs acElmsWW = mdo
    let elms = W.imap mkA4r acElmsWW
    (htTable, tblState) <- eTableD' (_tdfTableAttr tdFs) $ mdo
        mkCaption mcapdefs
        evB2 <- mkTheadVS mColdefs evB tblSt
        (_htTb,evB1) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
            ets <- W.imapM (mkRow evB tblSt) elms
            pure $ leftmost $ W.toList ets
        evB3 <- mkTfootVS mFootdefs evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt <- updateTableState tblOE evB
        pure tblSt
    tblOE <- tableEvents htTable
    pure tblState
    where
      mkA4r ∷ Int → W.Vector cols a → W.Vector cols (ActElem, a)
      mkA4r i = W.imap (\j e -> (ActERC (i,j),e))
      mkRow ∷ Event t (TableEvent t)
            → TableState t
            → Int
            → W.Vector cols (ActElem,a)
            → m (Event t (TableEvent t))
      mkRow evB tblSt2 i v =
        eTrD (_tdfTrAttr tdFs $ ActErow i) $ do
            eds <- W.forM v
                (\(ve,ae) -> eTdD (_tdfTdAttr tdFs ve)
                    $ _tdfCombFun tdFs tdFs ve ae evB tblSt2)
                    --  _tdCombFun tdFs tdFs ve ae evB dDU eMoLe)
                -- Note that in the _tdCombFun tdFs tdFs the first is used
                -- to get the combining function and call it. The second is
                -- a parameter to that function.
            pure $ tblEvFiring $ W.toList eds
