{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


{-|
Module      : Reflex.Dom.HTML5.Elements.Tabular
Description : HTML5 tabular elements
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Tabular

This module contains table and its children elements:
caption, colgroup, thead, tbody, tfoot, tr
Moreover: td, th, col, row

The naming convention follows that of Elements.

-}

module Reflex.Dom.HTML5.Elements.Tabular where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
-- import Foreign.JavaScript.TH
-- import Reflex.Dom.Main
-- import Reflex.Dom.Builder.Immediate
-- import Reflex.Dom.Core
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       PostBuild, DomBuilderSpace, elAttr',
                       elDynAttr', el')

import Reflex.Dom.HTML5.Attrs (AttrMap, attrMap,
  attrSetGlobals, attrSetAccessKey, attrSetContentEditable,
  attrSetContextMenu, attrSetClassName, attrSetDir, attrSetDraggable,
  attrSetHidden, attrSetId, attrSetLang, attrSetSpellCheck,
  attrSetStyle, attrSetTabIndex, attrSetTitle, attrSetTranslate,
  attrSetSpan, attrSetAbbr, attrSetColSpan, attrSetHeaders,
  attrSetRowSpan, attrSetScope, attrSetAnmval, attrSetDnmval,
  attrSetRole, attrSetSlot,
  AttrHasGlobals, AttrHasAccessKey, AttrHasContentEditable,
  AttrHasContextMenu, AttrHasClass, AttrHasDir, AttrHasDraggable,
  AttrHasGlobals, AttrHasAnmval, AttrHasContentEditable,
  AttrHasContextMenu, AttrHasDnmval, AttrHasDir, AttrHasDraggable,
  AttrHasGlobals, AttrHasAnmval, AttrHasContentEditable,
  AttrHasContextMenu, AttrHasClass, AttrHasDir, AttrHasDraggable,
  AttrHasHidden, AttrHasId, AttrHasLang, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasHidden, AttrHasId, AttrHasRole, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasHidden, AttrHasId, AttrHasSlot, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasSpan, AttrHasAbbr, AttrHasColSpan, AttrHasHeaders,
  AttrHasRowSpan, AttrHasScope, AttrHasAnmval, AttrHasDnmval,
  AttrHasRole, AttrHasSlot,
  Abbr, Span, ColSpan, Headers, RowSpan, Scope,
  Globals, AttrGetClassName, attrGetClassName, gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )

------------------------------------------------------------------------------

-- | Caption-element has only Global attributes.
data ECaption = ECaption
  { _eCaptionGlobals :: Maybe Globals
  , _eCaptionCustom  :: Maybe Attr
  }


instance AttrMap ECaption where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eCaptionGlobals b
    ] <> maybeToList (_eCaptionCustom b)

instance Default ECaption where
  def = ECaption def def

instance Monoid ECaption where
  mempty = def
  mappend (ECaption a1 a2) (ECaption b1 b2) = ECaption (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ECaption where
   attrSetGlobals p b = b { _eCaptionGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ECaption
  where attrSetAccessKey p g = g { _eCaptionGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasAnmval ECaption
  where attrSetAnmval p g = g { _eCaptionGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasContentEditable ECaption
  where attrSetContentEditable p g = g  { _eCaptionGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasContextMenu ECaption
  where attrSetContextMenu p g     = g { _eCaptionGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasClass ECaption
  where attrSetClassName p g           = g { _eCaptionGlobals = Just (attrSetClassName p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasDnmval ECaption
  where attrSetDnmval p g           = g { _eCaptionGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasDir ECaption
  where attrSetDir p g             = g { _eCaptionGlobals = Just (attrSetDir p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasDraggable ECaption
  where attrSetDraggable p g       = g { _eCaptionGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasHidden ECaption
  where attrSetHidden p g          = g { _eCaptionGlobals = Just (attrSetHidden p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasId ECaption
  where attrSetId p g              = g { _eCaptionGlobals = Just (attrSetId p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasLang ECaption
  where attrSetLang p g            = g { _eCaptionGlobals = Just (attrSetLang p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasRole ECaption
  where attrSetRole p g            = g { _eCaptionGlobals = Just (attrSetRole p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasSlot ECaption
  where attrSetSlot p g            = g { _eCaptionGlobals = Just (attrSetSlot p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasSpellCheck ECaption
  where attrSetSpellCheck p g      = g { _eCaptionGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasStyle ECaption
  where attrSetStyle p g           = g { _eCaptionGlobals = Just (attrSetStyle p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasTabIndex ECaption
  where attrSetTabIndex p g        = g { _eCaptionGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasTitle ECaption
  where attrSetTitle p g           = g { _eCaptionGlobals = Just (attrSetTitle p (fromMaybe gDef (_eCaptionGlobals g))) }
instance AttrHasTranslate ECaption
  where attrSetTranslate p g       = g { _eCaptionGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eCaptionGlobals g))) }

instance AttrGetClassName ECaption where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eCaptionGlobals g)

instance AttrHasCustom ECaption where attrSetCustom p g       = g { _eCaptionCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"caption\" ... @
eCaption' :: forall t m a. (DomBuilder t m) => ECaption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCaption' b = elAttr' "caption" (attrMap b)

-- | A short-hand notion for @ elAttr \"caption\" ... @
eCaption :: forall t m a. (DomBuilder t m) => ECaption -> m a -> m a
eCaption b children = snd <$> eCaption' b children

-- | A short-hand notion for @ el\' \"caption\" ... @
eCaptionN' :: forall t m a. (DomBuilder t m) => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCaptionN' = el' "caption"

-- | A short-hand notion for @ el \"caption\" ... @
eCaptionN :: forall t m a. (DomBuilder t m) => m a -> m a
eCaptionN children = snd <$> eCaptionN' children

-- | A short-hand notion for @ elDynAttr\' \"caption\" ... @
eCaptionD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECaption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCaptionD' b = elDynAttr' "caption" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"caption\" ... @
eCaptionD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECaption -> m a -> m a
eCaptionD b children = snd <$> eCaptionD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Col-element
data ECol = ECol
  { _eColGlobals :: Maybe Globals
  , _eColSpan    :: Maybe Span
  , _eColCustom  :: Maybe Attr
  }


instance AttrMap ECol where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eColGlobals b
    , attrMap <$> _eColSpan b
    ] <> maybeToList (_eColCustom b)

instance Default ECol where
  def = ECol def def def

instance Monoid ECol where
  mempty = def
  mappend (ECol a1 a2 a3) (ECol b1 b2 b3) = ECol (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals ECol where
   attrSetGlobals p b = b { _eColGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ECol
  where attrSetAccessKey p g = g { _eColGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasAnmval ECol
  where attrSetAnmval p g = g { _eColGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasContentEditable ECol
  where attrSetContentEditable p g = g  { _eColGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasContextMenu ECol
  where attrSetContextMenu p g     = g { _eColGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasClass ECol
  where attrSetClassName p g           = g { _eColGlobals = Just (attrSetClassName p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasDnmval ECol
  where attrSetDnmval p g           = g { _eColGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasDir ECol
  where attrSetDir p g             = g { _eColGlobals = Just (attrSetDir p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasDraggable ECol
  where attrSetDraggable p g       = g { _eColGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasHidden ECol
  where attrSetHidden p g          = g { _eColGlobals = Just (attrSetHidden p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasId ECol
  where attrSetId p g              = g { _eColGlobals = Just (attrSetId p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasLang ECol
  where attrSetLang p g            = g { _eColGlobals = Just (attrSetLang p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasRole ECol
  where attrSetRole p g            = g { _eColGlobals = Just (attrSetRole p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasSlot ECol
  where attrSetSlot p g            = g { _eColGlobals = Just (attrSetSlot p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasSpellCheck ECol
  where attrSetSpellCheck p g      = g { _eColGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasStyle ECol
  where attrSetStyle p g           = g { _eColGlobals = Just (attrSetStyle p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasTabIndex ECol
  where attrSetTabIndex p g        = g { _eColGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasTitle ECol
  where attrSetTitle p g           = g { _eColGlobals = Just (attrSetTitle p (fromMaybe gDef (_eColGlobals g))) }
instance AttrHasTranslate ECol
  where attrSetTranslate p g       = g { _eColGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eColGlobals g))) }

instance AttrGetClassName ECol where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eColGlobals g)

instance AttrHasSpan  ECol where attrSetSpan p g  = g { _eColSpan  = Just p }

instance AttrHasCustom ECol where attrSetCustom p g       = g { _eColCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"col\" ... @
eCol' :: forall t m a. DomBuilder t m => ECol -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCol' b = elAttr' "col" (attrMap b)

-- | A short-hand notion for @ elAttr \"col\" ... @
eCol :: forall t m a. DomBuilder t m => ECol -> m a -> m a
eCol b children = snd <$> eCol' b children

-- | A short-hand notion for @ el\' \"col\" ... @
eColN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eColN' = el' "col"

-- | A short-hand notion for @ el \"col\" ... @
eColN :: forall t m a. DomBuilder t m => m a -> m a
eColN children = snd <$> eColN' children

-- | A short-hand notion for @ elDynAttr\' \"col\" ... @
eColD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECol -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eColD' b = elDynAttr' "col" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"col\" ... @
eColD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECol -> m a -> m a
eColD b children = snd <$> eColD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | ColGroup-element
data EColGroup = EColGroup
  { _eColGroupGlobals :: Maybe Globals
  , _eColGroupSpan    :: Maybe Span
  , _eColGroupCustom  :: Maybe Attr
  }


instance AttrMap EColGroup where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eColGroupGlobals b
    , attrMap <$> _eColGroupSpan b
    ] <> maybeToList (_eColGroupCustom b)

instance Default EColGroup where
  def = EColGroup def def def

instance Monoid EColGroup where
  mempty = def
  mappend (EColGroup a1 a2 a3) (EColGroup b1 b2 b3)
    = EColGroup (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EColGroup where
   attrSetGlobals p b = b { _eColGroupGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EColGroup
  where attrSetAccessKey p g = g { _eColGroupGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasAnmval EColGroup
  where attrSetAnmval p g = g { _eColGroupGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasContentEditable EColGroup
  where attrSetContentEditable p g = g  { _eColGroupGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasContextMenu EColGroup
  where attrSetContextMenu p g     = g { _eColGroupGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasClass EColGroup
  where attrSetClassName p g           = g { _eColGroupGlobals = Just (attrSetClassName p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasDnmval EColGroup
  where attrSetDnmval p g           = g { _eColGroupGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasDir EColGroup
  where attrSetDir p g             = g { _eColGroupGlobals = Just (attrSetDir p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasDraggable EColGroup
  where attrSetDraggable p g       = g { _eColGroupGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasHidden EColGroup
  where attrSetHidden p g          = g { _eColGroupGlobals = Just (attrSetHidden p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasId EColGroup
  where attrSetId p g              = g { _eColGroupGlobals = Just (attrSetId p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasLang EColGroup
  where attrSetLang p g            = g { _eColGroupGlobals = Just (attrSetLang p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasRole EColGroup
  where attrSetRole p g            = g { _eColGroupGlobals = Just (attrSetRole p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasSlot EColGroup
  where attrSetSlot p g            = g { _eColGroupGlobals = Just (attrSetSlot p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasSpellCheck EColGroup
  where attrSetSpellCheck p g      = g { _eColGroupGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasStyle EColGroup
  where attrSetStyle p g           = g { _eColGroupGlobals = Just (attrSetStyle p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasTabIndex EColGroup
  where attrSetTabIndex p g        = g { _eColGroupGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasTitle EColGroup
  where attrSetTitle p g           = g { _eColGroupGlobals = Just (attrSetTitle p (fromMaybe gDef (_eColGroupGlobals g))) }
instance AttrHasTranslate EColGroup
  where attrSetTranslate p g       = g { _eColGroupGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eColGroupGlobals g))) }

instance AttrGetClassName EColGroup where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eColGroupGlobals g)

instance AttrHasSpan  EColGroup where attrSetSpan p g  = g { _eColGroupSpan  = Just p }

instance AttrHasCustom EColGroup where attrSetCustom p g       = g { _eColGroupCustom = Just p }

eColGroup' :: forall t m a. DomBuilder t m => EColGroup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eColGroup' b = elAttr' "colgroup" (attrMap b)

eColGroup :: forall t m a. DomBuilder t m => EColGroup -> m a -> m a
eColGroup b children = snd <$> eColGroup' b children

eColGroupN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eColGroupN' = el' "colgroup"

eColGroupN :: forall t m a. DomBuilder t m => m a -> m a
eColGroupN children = snd <$> eColGroupN' children

eColGroupD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EColGroup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eColGroupD' b = elDynAttr' "colgroup" (attrMap <$> b)

eColGroupD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EColGroup -> m a -> m a
eColGroupD b children = snd <$> eColGroupD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Table-element
-- Attribute border has been marked as obsolete in HTML 5.3.
data ETable = ETable
  { _eTableGlobals   :: Maybe Globals
  -- , _eTableBorder :: Maybe Border
  , _eTableCustom    :: Maybe Attr
  }


instance AttrMap ETable where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTableGlobals b
    -- , attrMap <$> _eTableBorder b
    ] <> maybeToList (_eTableCustom b)

instance Default ETable where
  def = ETable def def

instance Monoid ETable where
  mempty = def
  mappend (ETable a1 a2) (ETable b1 b2) = ETable (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ETable where
   attrSetGlobals p b = b { _eTableGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETable
  where attrSetAccessKey p g = g { _eTableGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasAnmval ETable
  where attrSetAnmval p g = g { _eTableGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasContentEditable ETable
  where attrSetContentEditable p g = g  { _eTableGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasContextMenu ETable
  where attrSetContextMenu p g     = g { _eTableGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasClass ETable
  where attrSetClassName p g           = g { _eTableGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasDnmval ETable
  where attrSetDnmval p g           = g { _eTableGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasDir ETable
  where attrSetDir p g             = g { _eTableGlobals = Just (attrSetDir p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasDraggable ETable
  where attrSetDraggable p g       = g { _eTableGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasHidden ETable
  where attrSetHidden p g          = g { _eTableGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasId ETable
  where attrSetId p g              = g { _eTableGlobals = Just (attrSetId p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasLang ETable
  where attrSetLang p g            = g { _eTableGlobals = Just (attrSetLang p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasRole ETable
  where attrSetRole p g            = g { _eTableGlobals = Just (attrSetRole p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasSlot ETable
  where attrSetSlot p g            = g { _eTableGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasSpellCheck ETable
  where attrSetSpellCheck p g      = g { _eTableGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasStyle ETable
  where attrSetStyle p g           = g { _eTableGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasTabIndex ETable
  where attrSetTabIndex p g        = g { _eTableGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasTitle ETable
  where attrSetTitle p g           = g { _eTableGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTableGlobals g))) }
instance AttrHasTranslate ETable
  where attrSetTranslate p g       = g { _eTableGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTableGlobals g))) }

instance AttrGetClassName ETable where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTableGlobals g)

-- instance AttrHasBorder  ETable where attrSetBorder p g  = g { _eTableBorder  = Just p }

instance AttrHasCustom ETable where attrSetCustom p g       = g { _eTableCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"table\" ... @
eTable' :: forall t m a. DomBuilder t m => ETable -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTable' b = elAttr' "table" (attrMap b)

-- | A short-hand notion for @ elAttr \"table\" ... @
eTable :: forall t m a. DomBuilder t m => ETable -> m a -> m a
eTable b children = snd <$> eTable' b children

-- | A short-hand notion for @ el\' \"table\" ... @
eTableN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTableN' = el' "table"

-- | A short-hand notion for @ el \"table\" ... @
eTableN :: forall t m a. DomBuilder t m => m a -> m a
eTableN children = snd <$> eTableN' children

-- | A short-hand notion for @ elDynAttr\' \"table\" ... @
eTableD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETable -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTableD' b = elDynAttr' "table" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"table\" ... @
eTableD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETable -> m a -> m a
eTableD b children = snd <$> eTableD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Thead-element has only Global attributes.
data EThead = EThead
  { _eTheadGlobals :: Maybe Globals
  , _eTheadCustom  :: Maybe Attr
  }


instance AttrMap EThead where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTheadGlobals b
    ] <> maybeToList (_eTheadCustom b)

instance Default EThead where
  def = EThead def def

instance Monoid EThead where
  mempty = def
  mappend (EThead a1 a2) (EThead b1 b2) = EThead (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EThead where
   attrSetGlobals p b = b { _eTheadGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EThead
  where attrSetAccessKey p g = g { _eTheadGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasAnmval EThead
  where attrSetAnmval p g = g { _eTheadGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasContentEditable EThead
  where attrSetContentEditable p g = g  { _eTheadGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasContextMenu EThead
  where attrSetContextMenu p g     = g { _eTheadGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasClass EThead
  where attrSetClassName p g           = g { _eTheadGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasDnmval EThead
  where attrSetDnmval p g           = g { _eTheadGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasDir EThead
  where attrSetDir p g             = g { _eTheadGlobals = Just (attrSetDir p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasDraggable EThead
  where attrSetDraggable p g       = g { _eTheadGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasHidden EThead
  where attrSetHidden p g          = g { _eTheadGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasId EThead
  where attrSetId p g              = g { _eTheadGlobals = Just (attrSetId p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasLang EThead
  where attrSetLang p g            = g { _eTheadGlobals = Just (attrSetLang p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasRole EThead
  where attrSetRole p g            = g { _eTheadGlobals = Just (attrSetRole p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasSlot EThead
  where attrSetSlot p g            = g { _eTheadGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasSpellCheck EThead
  where attrSetSpellCheck p g      = g { _eTheadGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasStyle EThead
  where attrSetStyle p g           = g { _eTheadGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasTabIndex EThead
  where attrSetTabIndex p g        = g { _eTheadGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasTitle EThead
  where attrSetTitle p g           = g { _eTheadGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTheadGlobals g))) }
instance AttrHasTranslate EThead
  where attrSetTranslate p g       = g { _eTheadGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTheadGlobals g))) }

instance AttrGetClassName EThead where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTheadGlobals g)

instance AttrHasCustom EThead where attrSetCustom p g       = g { _eTheadCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"thead\" ... @
eThead' :: forall t m a. DomBuilder t m => EThead -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eThead' b = elAttr' "thead" (attrMap b)

-- | A short-hand notion for @ elAttr \"thead\" ... @
eThead :: forall t m a. DomBuilder t m => EThead -> m a -> m a
eThead b children = snd <$> eThead' b children

-- | A short-hand notion for @ el\' \"thead\" ... @
eTheadN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTheadN' = el' "thead"

-- | A short-hand notion for @ el \"thead\" ... @
eTheadN :: forall t m a. DomBuilder t m => m a -> m a
eTheadN children = snd <$> eTheadN' children

-- | A short-hand notion for @ elDynAttr\' \"thead\" ... @
eTheadD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EThead -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTheadD' b = elDynAttr' "thead" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"thead\" ... @
eTheadD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EThead -> m a -> m a
eTheadD b children = snd <$> eTheadD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tbody-element has only Global attributes.
data ETbody = ETbody
  { _eTbodyGlobals :: Maybe Globals
  , _eTbodyCustom  :: Maybe Attr
  }


instance AttrMap ETbody where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTbodyGlobals b
    ] <> maybeToList (_eTbodyCustom b)

instance Default ETbody where
  def = ETbody def def

instance Monoid ETbody where
  mempty = def
  mappend (ETbody a1 a2) (ETbody b1 b2) = ETbody (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ETbody where
   attrSetGlobals p b = b { _eTbodyGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETbody
  where attrSetAccessKey p g = g { _eTbodyGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasAnmval ETbody
  where attrSetAnmval p g = g { _eTbodyGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasContentEditable ETbody
  where attrSetContentEditable p g = g  { _eTbodyGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasContextMenu ETbody
  where attrSetContextMenu p g     = g { _eTbodyGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasClass ETbody
  where attrSetClassName p g           = g { _eTbodyGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasDnmval ETbody
  where attrSetDnmval p g           = g { _eTbodyGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasDir ETbody
  where attrSetDir p g             = g { _eTbodyGlobals = Just (attrSetDir p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasDraggable ETbody
  where attrSetDraggable p g       = g { _eTbodyGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasHidden ETbody
  where attrSetHidden p g          = g { _eTbodyGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasId ETbody
  where attrSetId p g              = g { _eTbodyGlobals = Just (attrSetId p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasLang ETbody
  where attrSetLang p g            = g { _eTbodyGlobals = Just (attrSetLang p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasRole ETbody
  where attrSetRole p g            = g { _eTbodyGlobals = Just (attrSetRole p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasSlot ETbody
  where attrSetSlot p g            = g { _eTbodyGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasSpellCheck ETbody
  where attrSetSpellCheck p g      = g { _eTbodyGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasStyle ETbody
  where attrSetStyle p g           = g { _eTbodyGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasTabIndex ETbody
  where attrSetTabIndex p g        = g { _eTbodyGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasTitle ETbody
  where attrSetTitle p g           = g { _eTbodyGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTbodyGlobals g))) }
instance AttrHasTranslate ETbody
  where attrSetTranslate p g       = g { _eTbodyGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTbodyGlobals g))) }

instance AttrGetClassName ETbody where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTbodyGlobals g)

instance AttrHasCustom ETbody where attrSetCustom p g       = g { _eTbodyCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"tbody\" ... @
eTbody' :: forall t m a. DomBuilder t m => ETbody -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTbody' b = elAttr' "tbody" (attrMap b)

-- | A short-hand notion for @ elAttr \"tbody\" ... @
eTbody :: forall t m a. DomBuilder t m => ETbody -> m a -> m a
eTbody b children = snd <$> eTbody' b children

-- | A short-hand notion for @ el\' \"tbody\" ... @
eTbodyN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTbodyN' = el' "tbody"

-- | A short-hand notion for @ el \"tbody\" ... @
eTbodyN :: forall t m a. DomBuilder t m => m a -> m a
eTbodyN children = snd <$> eTbodyN' children

-- | A short-hand notion for @ elDynAttr\' \"tbody\" ... @
eTbodyD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETbody -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTbodyD' b = elDynAttr' "tbody" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"tbody\" ... @
eTbodyD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETbody -> m a -> m a
eTbodyD b children = snd <$> eTbodyD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tfoot-element has only Global attributes.
data ETfoot = ETfoot
  { _eTfootGlobals :: Maybe Globals
  , _eTfootCustom  :: Maybe Attr
  }


instance AttrMap ETfoot where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTfootGlobals b
    ] <> maybeToList (_eTfootCustom b)

instance Default ETfoot where
  def = ETfoot def def

instance Monoid ETfoot where
  mempty = def
  mappend (ETfoot a1 a2) (ETfoot b1 b2) = ETfoot (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ETfoot where
   attrSetGlobals p b = b { _eTfootGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETfoot
  where attrSetAccessKey p g = g { _eTfootGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasAnmval ETfoot
  where attrSetAnmval p g = g { _eTfootGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasContentEditable ETfoot
  where attrSetContentEditable p g = g  { _eTfootGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasContextMenu ETfoot
  where attrSetContextMenu p g     = g { _eTfootGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasClass ETfoot
  where attrSetClassName p g           = g { _eTfootGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasDnmval ETfoot
  where attrSetDnmval p g           = g { _eTfootGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasDir ETfoot
  where attrSetDir p g             = g { _eTfootGlobals = Just (attrSetDir p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasDraggable ETfoot
  where attrSetDraggable p g       = g { _eTfootGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasHidden ETfoot
  where attrSetHidden p g          = g { _eTfootGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasId ETfoot
  where attrSetId p g              = g { _eTfootGlobals = Just (attrSetId p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasLang ETfoot
  where attrSetLang p g            = g { _eTfootGlobals = Just (attrSetLang p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasRole ETfoot
  where attrSetRole p g            = g { _eTfootGlobals = Just (attrSetRole p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasSlot ETfoot
  where attrSetSlot p g            = g { _eTfootGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasSpellCheck ETfoot
  where attrSetSpellCheck p g      = g { _eTfootGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasStyle ETfoot
  where attrSetStyle p g           = g { _eTfootGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasTabIndex ETfoot
  where attrSetTabIndex p g        = g { _eTfootGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasTitle ETfoot
  where attrSetTitle p g           = g { _eTfootGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTfootGlobals g))) }
instance AttrHasTranslate ETfoot
  where attrSetTranslate p g       = g { _eTfootGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTfootGlobals g))) }

instance AttrGetClassName ETfoot where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTfootGlobals g)

instance AttrHasCustom ETfoot where attrSetCustom p g       = g { _eTfootCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"tfoot\" ... @
eTfoot' :: forall t m a. DomBuilder t m => ETfoot -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTfoot' b = elAttr' "tfoot" (attrMap b)

-- | A short-hand notion for @ elAttr \"tfoot\" ... @
eTfoot :: forall t m a. DomBuilder t m => ETfoot -> m a -> m a
eTfoot b children = snd <$> eTfoot' b children

-- | A short-hand notion for @ el\' \"tfoot\" ... @
eTfootN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTfootN' = el' "tfoot"

-- | A short-hand notion for @ el \"tfoot\" ... @
eTfootN :: forall t m a. DomBuilder t m => m a -> m a
eTfootN children = snd <$> eTfootN' children

-- | A short-hand notion for @ elDynAttr\' \"tfoot\" ... @
eTfootD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETfoot -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTfootD' b = elDynAttr' "tfoot" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"tfoot\" ... @
eTfootD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETfoot -> m a -> m a
eTfootD b children = snd <$> eTfootD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Th-element
data ETh = ETh
  { _eThGlobals :: Maybe Globals
  , _eThAbbr    :: Maybe Abbr
  , _eThColSpan :: Maybe ColSpan
  , _eThHeaders :: Maybe Headers
  , _eThRowSpan :: Maybe RowSpan
  , _eThScope   :: Maybe Scope
  , _eThCustom  :: Maybe Attr
  }


instance AttrMap ETh where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eThGlobals b
    , attrMap <$> _eThAbbr b
    , attrMap <$> _eThColSpan b
    , attrMap <$> _eThHeaders b
    , attrMap <$> _eThRowSpan b
    , attrMap <$> _eThScope b
    ] <> maybeToList (_eThCustom b)

instance Default ETh where
  def = ETh def def def def def def def

instance Monoid ETh where
  mempty = def
  mappend (ETh a1 a2 a3 a4 a5 a6 a7) (ETh b1 b2 b3 b4 b5 b6 b7)
    = ETh (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

instance AttrHasGlobals ETh where
   attrSetGlobals p b = b { _eThGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETh
  where attrSetAccessKey p g = g { _eThGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasAnmval ETh
  where attrSetAnmval p g = g { _eThGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasContentEditable ETh
  where attrSetContentEditable p g = g  { _eThGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasContextMenu ETh
  where attrSetContextMenu p g     = g { _eThGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasClass ETh
  where attrSetClassName p g           = g { _eThGlobals = Just (attrSetClassName p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasDnmval ETh
  where attrSetDnmval p g           = g { _eThGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasDir ETh
  where attrSetDir p g             = g { _eThGlobals = Just (attrSetDir p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasDraggable ETh
  where attrSetDraggable p g       = g { _eThGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasHidden ETh
  where attrSetHidden p g          = g { _eThGlobals = Just (attrSetHidden p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasId ETh
  where attrSetId p g              = g { _eThGlobals = Just (attrSetId p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasLang ETh
  where attrSetLang p g            = g { _eThGlobals = Just (attrSetLang p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasRole ETh
  where attrSetRole p g            = g { _eThGlobals = Just (attrSetRole p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasSlot ETh
  where attrSetSlot p g            = g { _eThGlobals = Just (attrSetSlot p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasSpellCheck ETh
  where attrSetSpellCheck p g      = g { _eThGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasStyle ETh
  where attrSetStyle p g           = g { _eThGlobals = Just (attrSetStyle p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasTabIndex ETh
  where attrSetTabIndex p g        = g { _eThGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasTitle ETh
  where attrSetTitle p g           = g { _eThGlobals = Just (attrSetTitle p (fromMaybe gDef (_eThGlobals g))) }
instance AttrHasTranslate ETh
  where attrSetTranslate p g       = g { _eThGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eThGlobals g))) }

instance AttrGetClassName ETh where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eThGlobals g)

instance AttrHasAbbr    ETh where attrSetAbbr p g  = g { _eThAbbr  = Just p }
instance AttrHasColSpan ETh where attrSetColSpan p g  = g { _eThColSpan  = Just p }
instance AttrHasHeaders ETh where attrSetHeaders p g  = g { _eThHeaders  = Just p }
instance AttrHasRowSpan ETh where attrSetRowSpan p g  = g { _eThRowSpan  = Just p }
instance AttrHasScope   ETh where attrSetScope p g  = g { _eThScope  = Just p }

instance AttrHasCustom ETh where attrSetCustom p g       = g { _eThCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"th\" ... @
eTh' :: forall t m a. DomBuilder t m => ETh -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTh' b = elAttr' "th" (attrMap b)

-- | A short-hand notion for @ elAttr \"th\" ... @
eTh :: forall t m a. DomBuilder t m => ETh -> m a -> m a
eTh b children = snd <$> eTh' b children

-- | A short-hand notion for @ el\' \"th\" ... @
eThN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eThN' = el' "th"

-- | A short-hand notion for @ el \"th\" ... @
eThN :: forall t m a. DomBuilder t m => m a -> m a
eThN children = snd <$> eThN' children

-- | A short-hand notion for @ elDynAttr\' \"th\" ... @
eThD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETh -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eThD' b = elDynAttr' "th" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"th\" ... @
eThD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETh -> m a -> m a
eThD b children = snd <$> eThD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Td-element
data ETd = ETd
  { _eTdGlobals :: Maybe Globals
  , _eTdColSpan :: Maybe ColSpan
  , _eTdHeaders :: Maybe Headers
  , _eTdRowSpan :: Maybe RowSpan
  , _eTdCustom  :: Maybe Attr
  }


instance AttrMap ETd where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTdGlobals b
    , attrMap <$> _eTdColSpan b
    , attrMap <$> _eTdHeaders b
    , attrMap <$> _eTdRowSpan b
    ] <> maybeToList (_eTdCustom b)

instance Default ETd where
  def = ETd def def def def def

instance Monoid ETd where
  mempty = def
  mappend (ETd a1 a2 a3 a4 a5) (ETd b1 b2 b3 b4 b5)
    = ETd (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance AttrHasGlobals ETd where
   attrSetGlobals p b = b { _eTdGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETd
  where attrSetAccessKey p g = g { _eTdGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasAnmval ETd
  where attrSetAnmval p g = g { _eTdGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasContentEditable ETd
  where attrSetContentEditable p g = g  { _eTdGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasContextMenu ETd
  where attrSetContextMenu p g     = g { _eTdGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasClass ETd
  where attrSetClassName p g           = g { _eTdGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasDnmval ETd
  where attrSetDnmval p g           = g { _eTdGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasDir ETd
  where attrSetDir p g             = g { _eTdGlobals = Just (attrSetDir p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasDraggable ETd
  where attrSetDraggable p g       = g { _eTdGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasHidden ETd
  where attrSetHidden p g          = g { _eTdGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasId ETd
  where attrSetId p g              = g { _eTdGlobals = Just (attrSetId p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasLang ETd
  where attrSetLang p g            = g { _eTdGlobals = Just (attrSetLang p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasRole ETd
  where attrSetRole p g            = g { _eTdGlobals = Just (attrSetRole p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasSlot ETd
  where attrSetSlot p g            = g { _eTdGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasSpellCheck ETd
  where attrSetSpellCheck p g      = g { _eTdGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasStyle ETd
  where attrSetStyle p g           = g { _eTdGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasTabIndex ETd
  where attrSetTabIndex p g        = g { _eTdGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasTitle ETd
  where attrSetTitle p g           = g { _eTdGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTdGlobals g))) }
instance AttrHasTranslate ETd
  where attrSetTranslate p g       = g { _eTdGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTdGlobals g))) }

instance AttrGetClassName ETd where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTdGlobals g)

instance AttrHasColSpan ETd where attrSetColSpan p g  = g { _eTdColSpan  = Just p }
instance AttrHasHeaders ETd where attrSetHeaders p g  = g { _eTdHeaders  = Just p }
instance AttrHasRowSpan ETd where attrSetRowSpan p g  = g { _eTdRowSpan  = Just p }

instance AttrHasCustom ETd where attrSetCustom p g       = g { _eTdCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"td\" ... @
eTd' :: forall t m a. DomBuilder t m => ETd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTd' b = elAttr' "td" (attrMap b)

-- | A short-hand notion for @ elAttr \"td\" ... @
eTd :: forall t m a. DomBuilder t m => ETd -> m a -> m a
eTd b children = snd <$> eTd' b children

-- | A short-hand notion for @ el\' \"td\" ... @
eTdN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTdN' = el' "td"

-- | A short-hand notion for @ el \"td\" ... @
eTdN :: forall t m a. DomBuilder t m => m a -> m a
eTdN children = snd <$> eTdN' children

-- | A short-hand notion for @ elDynAttr\' \"td\" ... @
eTdD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTdD' b = elDynAttr' "td" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"td\" ... @
eTdD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETd -> m a -> m a
eTdD b children = snd  <$> eTdD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tr-element has only Global attributes.
data ETr = ETr
  { _eTrGlobals :: Maybe Globals
  , _eTrCustom  :: Maybe Attr
  }


instance AttrMap ETr where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTrGlobals b
    ] <> maybeToList (_eTrCustom b)

instance Default ETr where
  def = ETr def def

instance Monoid ETr where
  mempty = def
  mappend (ETr a1 a2) (ETr b1 b2) = ETr (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ETr where
   attrSetGlobals p b = b { _eTrGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETr
  where attrSetAccessKey p g = g { _eTrGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasAnmval ETr
  where attrSetAnmval p g = g { _eTrGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasContentEditable ETr
  where attrSetContentEditable p g = g  { _eTrGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasContextMenu ETr
  where attrSetContextMenu p g     = g { _eTrGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasClass ETr
  where attrSetClassName p g           = g { _eTrGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasDnmval ETr
  where attrSetDnmval p g           = g { _eTrGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasDir ETr
  where attrSetDir p g             = g { _eTrGlobals = Just (attrSetDir p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasDraggable ETr
  where attrSetDraggable p g       = g { _eTrGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasHidden ETr
  where attrSetHidden p g          = g { _eTrGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasId ETr
  where attrSetId p g              = g { _eTrGlobals = Just (attrSetId p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasLang ETr
  where attrSetLang p g            = g { _eTrGlobals = Just (attrSetLang p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasRole ETr
  where attrSetRole p g            = g { _eTrGlobals = Just (attrSetRole p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasSlot ETr
  where attrSetSlot p g            = g { _eTrGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasSpellCheck ETr
  where attrSetSpellCheck p g      = g { _eTrGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasStyle ETr
  where attrSetStyle p g           = g { _eTrGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasTabIndex ETr
  where attrSetTabIndex p g        = g { _eTrGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasTitle ETr
  where attrSetTitle p g           = g { _eTrGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTrGlobals g))) }
instance AttrHasTranslate ETr
  where attrSetTranslate p g       = g { _eTrGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTrGlobals g))) }

instance AttrGetClassName ETr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTrGlobals g)

instance AttrHasCustom ETr where attrSetCustom p g       = g { _eTrCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"tr\" ... @
eTr' :: forall t m a. DomBuilder t m => ETr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTr' b = elAttr' "tr" (attrMap b)

-- | A short-hand notion for @ elAttr \"tr\" ... @
eTr :: forall t m a. DomBuilder t m => ETr -> m a -> m a
eTr b children = snd <$> eTr' b children

-- | A short-hand notion for @ el\' \"tr\" ... @
eTrN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTrN' = el' "tr"

-- | A short-hand notion for @ el \"tr\" ... @
eTrN :: forall t m a. DomBuilder t m => m a -> m a
eTrN children = snd <$> eTrN' children

-- | A short-hand notion for @ elDynAttr\' \"tr\" ... @
eTrD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTrD' b = elDynAttr' "tr" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"tr\" ... @
eTrD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETr -> m a -> m a
eTrD b children = snd <$> eTrD' b children


