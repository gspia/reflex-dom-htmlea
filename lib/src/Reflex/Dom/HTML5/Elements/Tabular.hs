{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
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

import qualified Reflex.Dom.HTML5.Attrs as A

------------------------------------------------------------------------------

-- | Caption-element has only Global attributes.
data Caption = Caption
  { _captionGlobals ∷ Maybe A.Globals
  , _captionCustom  ∷ Maybe A.Attr
  }


-- | Caption has 'A.AttrMap' instance.
instance A.AttrMap Caption where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _captionGlobals bm
    ] <> maybeToList (_captionCustom bm)

-- | Default value for Caption has no attributes set (no globals nor custom).
defCaption ∷ Caption
defCaption = Caption Nothing Nothing

-- | Caption is a monoid (attributes can be appended).
instance Monoid Caption where
  mempty = defCaption
  mappend (Caption a1 a2) (Caption b1 b2) = Caption (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Caption where
   attrSetGlobals pp bm = bm { _captionGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Caption
  where attrSetAccessKey pp g = g { _captionGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Caption
  where attrSetAnmval pp g = g { _captionGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Caption
  where attrSetContentEditable pp g = g  { _captionGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Caption
  where attrSetContextMenu pp g     = g { _captionGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasClass Caption
  where attrSetClassName pp g           = g { _captionGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Caption
  where attrSetDnmval pp g           = g { _captionGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasDir Caption
  where attrSetDir pp g             = g { _captionGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Caption
  where attrSetDraggable pp g       = g { _captionGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Caption
  where attrSetHidden pp g          = g { _captionGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasId Caption
  where attrSetId pp g              = g { _captionGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasLang Caption
  where attrSetLang pp g            = g { _captionGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasRole Caption
  where attrSetRole pp g            = g { _captionGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Caption
  where attrSetSlot pp g            = g { _captionGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Caption
  where attrSetSpellCheck pp g      = g { _captionGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Caption
  where attrSetStyle pp g           = g { _captionGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Caption
  where attrSetTabIndex pp g        = g { _captionGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Caption
  where attrSetTitle pp g           = g { _captionGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_captionGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Caption
  where attrSetTranslate pp g       = g { _captionGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_captionGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Caption where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_captionGlobals g)

-- | An instance.
instance A.AttrHasCustom Caption where attrSetCustom pp g       = g { _captionCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"caption\" ... @
caption' ∷ forall t m a. (DomBuilder t m) ⇒ Caption → m a → m (Element EventResult (DomBuilderSpace m) t, a)
caption' bm = elAttr' "caption" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"caption\" ... @
caption ∷ forall t m a. (DomBuilder t m) ⇒ Caption → m a → m a
caption bm children = snd <$> caption' bm children

-- | A short-hand notion for @ el\' \"caption\" ... @
captionN' ∷ forall t m a. (DomBuilder t m) ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
captionN' = el' "caption"

-- | A short-hand notion for @ el \"caption\" ... @
captionN ∷ forall t m a. (DomBuilder t m) ⇒ m a → m a
captionN children = snd <$> captionN' children

-- | A short-hand notion for @ elDynAttr\' \"caption\" ... @
captionD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Caption → m a → m (Element EventResult (DomBuilderSpace m) t, a)
captionD' bm = elDynAttr' "caption" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"caption\" ... @
captionD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Caption → m a → m a
captionD bm children = snd <$> captionD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Col-element
data Col = Col
  { _colGlobals ∷ Maybe A.Globals
  , _colSpan    ∷ Maybe A.Span
  , _colCustom  ∷ Maybe A.Attr
  }


-- | Col has 'A.AttrMap' instance.
instance A.AttrMap Col where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _colGlobals bm
    , A.attrMap <$> _colSpan bm
    ] <> maybeToList (_colCustom bm)


-- | Default value for Col has no attributes set (no globals nor custom)
-- nor a 'Span'.
defCol ∷ Col
defCol = Col Nothing Nothing Nothing

-- | An instance.
instance Monoid Col where
  mempty = defCol
  mappend (Col a1 a2 a3) (Col b1 b2 b3) = Col (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance A.AttrHasGlobals Col where
   attrSetGlobals pp bm = bm { _colGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Col
  where attrSetAccessKey pp g = g { _colGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_colGlobals g))) }

-- | An instance.
instance A.AttrHasAnmval Col
  where attrSetAnmval pp g = g { _colGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Col
  where attrSetContentEditable pp g = g  { _colGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Col
  where attrSetContextMenu pp g     = g { _colGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasClass Col
  where attrSetClassName pp g           = g { _colGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Col
  where attrSetDnmval pp g           = g { _colGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasDir Col
  where attrSetDir pp g             = g { _colGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Col
  where attrSetDraggable pp g       = g { _colGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Col
  where attrSetHidden pp g          = g { _colGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasId Col
  where attrSetId pp g              = g { _colGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasLang Col
  where attrSetLang pp g            = g { _colGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasRole Col
  where attrSetRole pp g            = g { _colGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Col
  where attrSetSlot pp g            = g { _colGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Col
  where attrSetSpellCheck pp g      = g { _colGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Col
  where attrSetStyle pp g           = g { _colGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Col
  where attrSetTabIndex pp g        = g { _colGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Col
  where attrSetTitle pp g           = g { _colGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_colGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Col
  where attrSetTranslate pp g       = g { _colGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_colGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Col where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_colGlobals g)

-- | An instance.
instance A.AttrHasSpan  Col where attrSetSpan pp g  = g { _colSpan  = Just pp }

-- | An instance.
instance A.AttrHasCustom Col where attrSetCustom pp g       = g { _colCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"col\" ... @
col' ∷ forall t m a. DomBuilder t m ⇒ Col → m a → m (Element EventResult (DomBuilderSpace m) t, a)
col' bm = elAttr' "col" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"col\" ... @
col ∷ forall t m a. DomBuilder t m ⇒ Col → m a → m a
col bm children = snd <$> col' bm children

-- | A short-hand notion for @ el\' \"col\" ... @
colN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
colN' = el' "col"

-- | A short-hand notion for @ el \"col\" ... @
colN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
colN children = snd <$> colN' children

-- | A short-hand notion for @ elDynAttr\' \"col\" ... @
colD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Col → m a → m (Element EventResult (DomBuilderSpace m) t, a)
colD' bm = elDynAttr' "col" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"col\" ... @
colD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Col → m a → m a
colD bm children = snd <$> colD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | ColGroup-element
data ColGroup = ColGroup
  { _colGroupGlobals ∷ Maybe A.Globals
  , _colGroupSpan    ∷ Maybe A.Span
  , _colGroupCustom  ∷ Maybe A.Attr
  }


-- | ColGroup has 'A.AttrMap' instance.
instance A.AttrMap ColGroup where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _colGroupGlobals bm
    , A.attrMap <$> _colGroupSpan bm
    ] <> maybeToList (_colGroupCustom bm)

-- | Default value for ColGroup has no attributes set (no globals nor custom)
-- nor a 'Span'.
defColGroup ∷ ColGroup
defColGroup = ColGroup Nothing Nothing Nothing

-- | An instance.
instance Monoid ColGroup where
  mempty = defColGroup
  mappend (ColGroup a1 a2 a3) (ColGroup b1 b2 b3)
    = ColGroup (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance A.AttrHasGlobals ColGroup where
   attrSetGlobals pp bm = bm { _colGroupGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey ColGroup
  where attrSetAccessKey pp g = g { _colGroupGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval ColGroup
  where attrSetAnmval pp g = g { _colGroupGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable ColGroup
  where attrSetContentEditable pp g = g  { _colGroupGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu ColGroup
  where attrSetContextMenu pp g     = g { _colGroupGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasClass ColGroup
  where attrSetClassName pp g           = g { _colGroupGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval ColGroup
  where attrSetDnmval pp g           = g { _colGroupGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDir ColGroup
  where attrSetDir pp g             = g { _colGroupGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable ColGroup
  where attrSetDraggable pp g       = g { _colGroupGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasHidden ColGroup
  where attrSetHidden pp g          = g { _colGroupGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasId ColGroup
  where attrSetId pp g              = g { _colGroupGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasLang ColGroup
  where attrSetLang pp g            = g { _colGroupGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasRole ColGroup
  where attrSetRole pp g            = g { _colGroupGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasSlot ColGroup
  where attrSetSlot pp g            = g { _colGroupGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck ColGroup
  where attrSetSpellCheck pp g      = g { _colGroupGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasStyle ColGroup
  where attrSetStyle pp g           = g { _colGroupGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex ColGroup
  where attrSetTabIndex pp g        = g { _colGroupGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTitle ColGroup
  where attrSetTitle pp g           = g { _colGroupGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate ColGroup
  where attrSetTranslate pp g       = g { _colGroupGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_colGroupGlobals g))) }

-- | An instance.
instance A.AttrGetClassName ColGroup where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_colGroupGlobals g)

-- | An instance.
instance A.AttrHasSpan  ColGroup where attrSetSpan pp g  = g { _colGroupSpan  = Just pp }

-- | An instance.
instance A.AttrHasCustom ColGroup where attrSetCustom pp g       = g { _colGroupCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"colgroup\" ... @
colGroup' ∷ forall t m a. DomBuilder t m ⇒ ColGroup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
colGroup' bm = elAttr' "colgroup" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"colgroup\" ... @
colGroup ∷ forall t m a. DomBuilder t m ⇒ ColGroup → m a → m a
colGroup bm children = snd <$> colGroup' bm children

-- | A short-hand notion for @ el\' \"colgroup\" ... @
colGroupN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
colGroupN' = el' "colgroup"

-- | A short-hand notion for @ el \"colgroup\" ... @
colGroupN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
colGroupN children = snd <$> colGroupN' children

-- | A short-hand notion for @ elDynAttr\' \"colgroup\" ... @
colGroupD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t ColGroup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
colGroupD' bm = elDynAttr' "colgroup" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"colgroup\" ... @
colGroupD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t ColGroup → m a → m a
colGroupD bm children = snd <$> colGroupD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Table-element
-- A.Attribute border has been marked as obsolete in HTML 5.3.
data Table = Table
  { _tableGlobals   ∷ Maybe A.Globals
  -- , _tableBorder ∷ Maybe A.Border
  , _tableCustom    ∷ Maybe A.Attr
  }


-- | Table has 'A.AttrMap' instance.
instance A.AttrMap Table where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _tableGlobals bm
    -- , A.attrMap <$> _tableBorder bm
    ] <> maybeToList (_tableCustom bm)

-- | Default value for Table has no attributes set (no globals nor custom).
defTable ∷ Table
defTable = Table Nothing Nothing

-- | An instance.
instance Monoid Table where
  mempty = defTable
  mappend (Table a1 a2) (Table b1 b2) = Table (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Table where
   attrSetGlobals pp bm = bm { _tableGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey Table
  where attrSetAccessKey pp g = g { _tableGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Table
  where attrSetAnmval pp g = g { _tableGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Table
  where attrSetContentEditable pp g = g  { _tableGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Table
  where attrSetContextMenu pp g     = g { _tableGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasClass Table
  where attrSetClassName pp g           = g { _tableGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Table
  where attrSetDnmval pp g           = g { _tableGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasDir Table
  where attrSetDir pp g             = g { _tableGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Table
  where attrSetDraggable pp g       = g { _tableGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Table
  where attrSetHidden pp g          = g { _tableGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasId Table
  where attrSetId pp g              = g { _tableGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasLang Table
  where attrSetLang pp g            = g { _tableGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasRole Table
  where attrSetRole pp g            = g { _tableGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Table
  where attrSetSlot pp g            = g { _tableGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Table
  where attrSetSpellCheck pp g      = g { _tableGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Table
  where attrSetStyle pp g           = g { _tableGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Table
  where attrSetTabIndex pp g        = g { _tableGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Table
  where attrSetTitle pp g           = g { _tableGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_tableGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Table
  where attrSetTranslate pp g       = g { _tableGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_tableGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Table where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_tableGlobals g)

-- instance A.AttrHasBorder  Table where attrSetBorder pp g  = g { _tableBorder  = Just pp }

-- | An instance.
instance A.AttrHasCustom Table where attrSetCustom pp g       = g { _tableCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"table\" ... @
table' ∷ forall t m a. DomBuilder t m ⇒ Table → m a → m (Element EventResult (DomBuilderSpace m) t, a)
table' bm = elAttr' "table" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"table\" ... @
table ∷ forall t m a. DomBuilder t m ⇒ Table → m a → m a
table bm children = snd <$> table' bm children

-- | A short-hand notion for @ el\' \"table\" ... @
tableN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
tableN' = el' "table"

-- | A short-hand notion for @ el \"table\" ... @
tableN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
tableN children = snd <$> tableN' children

-- | A short-hand notion for @ elDynAttr\' \"table\" ... @
tableD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Table → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tableD' bm = elDynAttr' "table" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"table\" ... @
tableD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Table → m a → m a
tableD bm children = snd <$> tableD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Thead-element has only Global attributes.
data Thead = Thead
  { _theadGlobals ∷ Maybe A.Globals
  , _theadCustom  ∷ Maybe A.Attr
  }


-- | Thead has 'A.AttrMap' instance.
instance A.AttrMap Thead where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _theadGlobals bm
    ] <> maybeToList (_theadCustom bm)

-- | Default value for Thead has no attributes set (no globals nor custom).
defThead ∷ Thead
defThead = Thead Nothing Nothing

-- | An instance.
instance Monoid Thead where
  mempty = defThead
  mappend (Thead a1 a2) (Thead b1 b2) = Thead (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Thead where
   attrSetGlobals pp bm = bm { _theadGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey Thead
  where attrSetAccessKey pp g = g { _theadGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Thead
  where attrSetAnmval pp g = g { _theadGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Thead
  where attrSetContentEditable pp g = g  { _theadGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Thead
  where attrSetContextMenu pp g     = g { _theadGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasClass Thead
  where attrSetClassName pp g           = g { _theadGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Thead
  where attrSetDnmval pp g           = g { _theadGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasDir Thead
  where attrSetDir pp g             = g { _theadGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Thead
  where attrSetDraggable pp g       = g { _theadGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Thead
  where attrSetHidden pp g          = g { _theadGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasId Thead
  where attrSetId pp g              = g { _theadGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasLang Thead
  where attrSetLang pp g            = g { _theadGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasRole Thead
  where attrSetRole pp g            = g { _theadGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Thead
  where attrSetSlot pp g            = g { _theadGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Thead
  where attrSetSpellCheck pp g      = g { _theadGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Thead
  where attrSetStyle pp g           = g { _theadGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Thead
  where attrSetTabIndex pp g        = g { _theadGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Thead
  where attrSetTitle pp g           = g { _theadGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_theadGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Thead
  where attrSetTranslate pp g       = g { _theadGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_theadGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Thead where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_theadGlobals g)

-- | An instance.
instance A.AttrHasCustom Thead where attrSetCustom pp g       = g { _theadCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"thead\" ... @
thead' ∷ forall t m a. DomBuilder t m ⇒ Thead → m a → m (Element EventResult (DomBuilderSpace m) t, a)
thead' bm = elAttr' "thead" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"thead\" ... @
thead ∷ forall t m a. DomBuilder t m ⇒ Thead → m a → m a
thead bm children = snd <$> thead' bm children

-- | A short-hand notion for @ el\' \"thead\" ... @
theadN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
theadN' = el' "thead"

-- | A short-hand notion for @ el \"thead\" ... @
theadN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
theadN children = snd <$> theadN' children

-- | A short-hand notion for @ elDynAttr\' \"thead\" ... @
theadD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Thead → m a → m (Element EventResult (DomBuilderSpace m) t, a)
theadD' bm = elDynAttr' "thead" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"thead\" ... @
theadD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Thead → m a → m a
theadD bm children = snd <$> theadD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tbody-element has only Global attributes.
data Tbody = Tbody
  { _tbodyGlobals ∷ Maybe A.Globals
  , _tbodyCustom  ∷ Maybe A.Attr
  }


-- | Tbody has 'A.AttrMap' instance.
instance A.AttrMap Tbody where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _tbodyGlobals bm
    ] <> maybeToList (_tbodyCustom bm)

-- | Default value for Tbody has no attributes set (no globals nor custom).
defTbody ∷ Tbody
defTbody = Tbody Nothing Nothing

-- | An instance.
instance Monoid Tbody where
  mempty = defTbody
  mappend (Tbody a1 a2) (Tbody b1 b2) = Tbody (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Tbody where
   attrSetGlobals pp bm = bm { _tbodyGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey Tbody
  where attrSetAccessKey pp g = g { _tbodyGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Tbody
  where attrSetAnmval pp g = g { _tbodyGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Tbody
  where attrSetContentEditable pp g = g  { _tbodyGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Tbody
  where attrSetContextMenu pp g     = g { _tbodyGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasClass Tbody
  where attrSetClassName pp g           = g { _tbodyGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Tbody
  where attrSetDnmval pp g           = g { _tbodyGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasDir Tbody
  where attrSetDir pp g             = g { _tbodyGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Tbody
  where attrSetDraggable pp g       = g { _tbodyGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Tbody
  where attrSetHidden pp g          = g { _tbodyGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasId Tbody
  where attrSetId pp g              = g { _tbodyGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasLang Tbody
  where attrSetLang pp g            = g { _tbodyGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasRole Tbody
  where attrSetRole pp g            = g { _tbodyGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Tbody
  where attrSetSlot pp g            = g { _tbodyGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Tbody
  where attrSetSpellCheck pp g      = g { _tbodyGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Tbody
  where attrSetStyle pp g           = g { _tbodyGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Tbody
  where attrSetTabIndex pp g        = g { _tbodyGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Tbody
  where attrSetTitle pp g           = g { _tbodyGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Tbody
  where attrSetTranslate pp g       = g { _tbodyGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_tbodyGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Tbody where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_tbodyGlobals g)

-- | An instance.
instance A.AttrHasCustom Tbody where attrSetCustom pp g       = g { _tbodyCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"tbody\" ... @
tbody' ∷ forall t m a. DomBuilder t m ⇒ Tbody → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tbody' bm = elAttr' "tbody" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"tbody\" ... @
tbody ∷ forall t m a. DomBuilder t m ⇒ Tbody → m a → m a
tbody bm children = snd <$> tbody' bm children

-- | A short-hand notion for @ el\' \"tbody\" ... @
tbodyN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
tbodyN' = el' "tbody"

-- | A short-hand notion for @ el \"tbody\" ... @
tbodyN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
tbodyN children = snd <$> tbodyN' children

-- | A short-hand notion for @ elDynAttr\' \"tbody\" ... @
tbodyD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tbody → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tbodyD' bm = elDynAttr' "tbody" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"tbody\" ... @
tbodyD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tbody → m a → m a
tbodyD bm children = snd <$> tbodyD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tfoot-element has only Global attributes.
data Tfoot = Tfoot
  { _tfootGlobals ∷ Maybe A.Globals
  , _tfootCustom  ∷ Maybe A.Attr
  }


-- | Tfoot has 'A.AttrMap' instance.
instance A.AttrMap Tfoot where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _tfootGlobals bm
    ] <> maybeToList (_tfootCustom bm)

-- | Default value for Tfoot has no attributes set (no globals nor custom).
defTfoot ∷ Tfoot
defTfoot = Tfoot Nothing Nothing

-- | An instance.
instance Monoid Tfoot where
  mempty = defTfoot
  mappend (Tfoot a1 a2) (Tfoot b1 b2) = Tfoot (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Tfoot where
   attrSetGlobals pp bm = bm { _tfootGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey Tfoot
  where attrSetAccessKey pp g = g { _tfootGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Tfoot
  where attrSetAnmval pp g = g { _tfootGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Tfoot
  where attrSetContentEditable pp g = g  { _tfootGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Tfoot
  where attrSetContextMenu pp g     = g { _tfootGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasClass Tfoot
  where attrSetClassName pp g           = g { _tfootGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Tfoot
  where attrSetDnmval pp g           = g { _tfootGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasDir Tfoot
  where attrSetDir pp g             = g { _tfootGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Tfoot
  where attrSetDraggable pp g       = g { _tfootGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Tfoot
  where attrSetHidden pp g          = g { _tfootGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasId Tfoot
  where attrSetId pp g              = g { _tfootGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasLang Tfoot
  where attrSetLang pp g            = g { _tfootGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasRole Tfoot
  where attrSetRole pp g            = g { _tfootGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Tfoot
  where attrSetSlot pp g            = g { _tfootGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Tfoot
  where attrSetSpellCheck pp g      = g { _tfootGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Tfoot
  where attrSetStyle pp g           = g { _tfootGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Tfoot
  where attrSetTabIndex pp g        = g { _tfootGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Tfoot
  where attrSetTitle pp g           = g { _tfootGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_tfootGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Tfoot
  where attrSetTranslate pp g       = g { _tfootGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_tfootGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Tfoot where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_tfootGlobals g)

-- | An instance.
instance A.AttrHasCustom Tfoot where attrSetCustom pp g       = g { _tfootCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"tfoot\" ... @
tfoot' ∷ forall t m a. DomBuilder t m ⇒ Tfoot → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tfoot' bm = elAttr' "tfoot" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"tfoot\" ... @
tfoot ∷ forall t m a. DomBuilder t m ⇒ Tfoot → m a → m a
tfoot bm children = snd <$> tfoot' bm children

-- | A short-hand notion for @ el\' \"tfoot\" ... @
tfootN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
tfootN' = el' "tfoot"

-- | A short-hand notion for @ el \"tfoot\" ... @
tfootN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
tfootN children = snd <$> tfootN' children

-- | A short-hand notion for @ elDynAttr\' \"tfoot\" ... @
tfootD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tfoot → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tfootD' bm = elDynAttr' "tfoot" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"tfoot\" ... @
tfootD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tfoot → m a → m a
tfootD bm children = snd <$> tfootD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Th-element
data Th = Th
  { _thGlobals ∷ Maybe A.Globals
  , _thAbbr    ∷ Maybe A.Abbr
  , _thColSpan ∷ Maybe A.ColSpan
  , _thHeaders ∷ Maybe A.Headers
  , _thRowSpan ∷ Maybe A.RowSpan
  , _thScope   ∷ Maybe A.Scope
  , _thCustom  ∷ Maybe A.Attr
  }


-- | Th has 'A.AttrMap' instance.
instance A.AttrMap Th where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _thGlobals bm
    , A.attrMap <$> _thAbbr bm
    , A.attrMap <$> _thColSpan bm
    , A.attrMap <$> _thHeaders bm
    , A.attrMap <$> _thRowSpan bm
    , A.attrMap <$> _thScope bm
    ] <> maybeToList (_thCustom bm)

-- | Default value for Th has no attributes set (no globals nor other).
defTh ∷ Th
defTh = Th Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Monoid Th where
  mempty = defTh
  mappend (Th a1 a2 a3 a4 a5 a6 a7) (Th b1 b2 b3 b4 b5 b6 b7)
    = Th (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

-- | An instance.
instance A.AttrHasGlobals Th where
   attrSetGlobals pp bm = bm { _thGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Th
  where attrSetAccessKey pp g = g { _thGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Th
  where attrSetAnmval pp g = g { _thGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Th
  where attrSetContentEditable pp g = g  { _thGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Th
  where attrSetContextMenu pp g     = g { _thGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasClass Th
  where attrSetClassName pp g           = g { _thGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Th
  where attrSetDnmval pp g           = g { _thGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasDir Th
  where attrSetDir pp g             = g { _thGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Th
  where attrSetDraggable pp g       = g { _thGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Th
  where attrSetHidden pp g          = g { _thGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasId Th
  where attrSetId pp g              = g { _thGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasLang Th
  where attrSetLang pp g            = g { _thGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasRole Th
  where attrSetRole pp g            = g { _thGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Th
  where attrSetSlot pp g            = g { _thGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Th
  where attrSetSpellCheck pp g      = g { _thGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Th
  where attrSetStyle pp g           = g { _thGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Th
  where attrSetTabIndex pp g        = g { _thGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Th
  where attrSetTitle pp g           = g { _thGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_thGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Th
  where attrSetTranslate pp g       = g { _thGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_thGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Th where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_thGlobals g)

-- | An instance.
instance A.AttrHasAbbr    Th where attrSetAbbr pp g  = g { _thAbbr  = Just pp }
-- | An instance.
instance A.AttrHasColSpan Th where attrSetColSpan pp g  = g { _thColSpan  = Just pp }
-- | An instance.
instance A.AttrHasHeaders Th where attrSetHeaders pp g  = g { _thHeaders  = Just pp }
-- | An instance.
instance A.AttrHasRowSpan Th where attrSetRowSpan pp g  = g { _thRowSpan  = Just pp }
-- | An instance.
instance A.AttrHasScope   Th where attrSetScope pp g  = g { _thScope  = Just pp }

-- | An instance.
instance A.AttrHasCustom Th where attrSetCustom pp g       = g { _thCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"th\" ... @
th' ∷ forall t m a. DomBuilder t m ⇒ Th → m a → m (Element EventResult (DomBuilderSpace m) t, a)
th' bm = elAttr' "th" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"th\" ... @
th ∷ forall t m a. DomBuilder t m ⇒ Th → m a → m a
th bm children = snd <$> th' bm children

-- | A short-hand notion for @ el\' \"th\" ... @
thN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
thN' = el' "th"

-- | A short-hand notion for @ el \"th\" ... @
thN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
thN children = snd <$> thN' children

-- | A short-hand notion for @ elDynAttr\' \"th\" ... @
thD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Th → m a → m (Element EventResult (DomBuilderSpace m) t, a)
thD' bm = elDynAttr' "th" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"th\" ... @
thD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Th → m a → m a
thD bm children = snd <$> thD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Td-element
data Td = Td
  { _tdGlobals ∷ Maybe A.Globals
  , _tdColSpan ∷ Maybe A.ColSpan
  , _tdHeaders ∷ Maybe A.Headers
  , _tdRowSpan ∷ Maybe A.RowSpan
  , _tdCustom  ∷ Maybe A.Attr
  }


-- | Td has 'A.AttrMap' instance.
instance A.AttrMap Td where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _tdGlobals bm
    , A.attrMap <$> _tdColSpan bm
    , A.attrMap <$> _tdHeaders bm
    , A.attrMap <$> _tdRowSpan bm
    ] <> maybeToList (_tdCustom bm)

-- | Default value for Td has no attributes set (no globals nor other).
defTd ∷ Td
defTd = Td Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Monoid Td where
  mempty = defTd
  mappend (Td a1 a2 a3 a4 a5) (Td b1 b2 b3 b4 b5)
    = Td (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | An instance.
instance A.AttrHasGlobals Td where
   attrSetGlobals pp bm = bm { _tdGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Td
  where attrSetAccessKey pp g = g { _tdGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Td
  where attrSetAnmval pp g = g { _tdGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Td
  where attrSetContentEditable pp g = g  { _tdGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Td
  where attrSetContextMenu pp g     = g { _tdGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasClass Td
  where attrSetClassName pp g           = g { _tdGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Td
  where attrSetDnmval pp g           = g { _tdGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasDir Td
  where attrSetDir pp g             = g { _tdGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Td
  where attrSetDraggable pp g       = g { _tdGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Td
  where attrSetHidden pp g          = g { _tdGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasId Td
  where attrSetId pp g              = g { _tdGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasLang Td
  where attrSetLang pp g            = g { _tdGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasRole Td
  where attrSetRole pp g            = g { _tdGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Td
  where attrSetSlot pp g            = g { _tdGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Td
  where attrSetSpellCheck pp g      = g { _tdGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Td
  where attrSetStyle pp g           = g { _tdGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Td
  where attrSetTabIndex pp g        = g { _tdGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Td
  where attrSetTitle pp g           = g { _tdGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_tdGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Td
  where attrSetTranslate pp g       = g { _tdGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_tdGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Td where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_tdGlobals g)

-- | An instance.
instance A.AttrHasColSpan Td where attrSetColSpan pp g  = g { _tdColSpan  = Just pp }
-- | An instance.
instance A.AttrHasHeaders Td where attrSetHeaders pp g  = g { _tdHeaders  = Just pp }
-- | An instance.
instance A.AttrHasRowSpan Td where attrSetRowSpan pp g  = g { _tdRowSpan  = Just pp }

-- | An instance.
instance A.AttrHasCustom Td where attrSetCustom pp g       = g { _tdCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"td\" ... @
td' ∷ forall t m a. DomBuilder t m ⇒ Td → m a → m (Element EventResult (DomBuilderSpace m) t, a)
td' bm = elAttr' "td" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"td\" ... @
td ∷ forall t m a. DomBuilder t m ⇒ Td → m a → m a
td bm children = snd <$> td' bm children

-- | A short-hand notion for @ el\' \"td\" ... @
tdN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
tdN' = el' "td"

-- | A short-hand notion for @ el \"td\" ... @
tdN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
tdN children = snd <$> tdN' children

-- | A short-hand notion for @ elDynAttr\' \"td\" ... @
tdD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Td → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tdD' bm = elDynAttr' "td" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"td\" ... @
tdD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Td → m a → m a
tdD bm children = snd  <$> tdD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Tr-element has only Global attributes.
data Tr = Tr
  { _trGlobals ∷ Maybe A.Globals
  , _trCustom  ∷ Maybe A.Attr
  }


-- | Tr has 'A.AttrMap' instance.
instance A.AttrMap Tr where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _trGlobals bm
    ] <> maybeToList (_trCustom bm)

-- | Default value for Tr has no attributes set (no globals nor other).
defTr ∷ Tr
defTr = Tr Nothing Nothing

-- | An instance.
instance Monoid Tr where
  mempty = defTr
  mappend (Tr a1 a2) (Tr b1 b2) = Tr (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Tr where
   attrSetGlobals pp bm = bm { _trGlobals = Just pp }

-- Global A.attributes require the following instances.


-- | An instance.
instance A.AttrHasAccessKey Tr
  where attrSetAccessKey pp g = g { _trGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Tr
  where attrSetAnmval pp g = g { _trGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Tr
  where attrSetContentEditable pp g = g  { _trGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Tr
  where attrSetContextMenu pp g     = g { _trGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasClass Tr
  where attrSetClassName pp g           = g { _trGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Tr
  where attrSetDnmval pp g           = g { _trGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasDir Tr
  where attrSetDir pp g             = g { _trGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Tr
  where attrSetDraggable pp g       = g { _trGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Tr
  where attrSetHidden pp g          = g { _trGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasId Tr
  where attrSetId pp g              = g { _trGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasLang Tr
  where attrSetLang pp g            = g { _trGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasRole Tr
  where attrSetRole pp g            = g { _trGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Tr
  where attrSetSlot pp g            = g { _trGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Tr
  where attrSetSpellCheck pp g      = g { _trGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Tr
  where attrSetStyle pp g           = g { _trGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Tr
  where attrSetTabIndex pp g        = g { _trGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Tr
  where attrSetTitle pp g           = g { _trGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_trGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Tr
  where attrSetTranslate pp g       = g { _trGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_trGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Tr where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_trGlobals g)

-- | An instance.
instance A.AttrHasCustom Tr where attrSetCustom pp g       = g { _trCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"tr\" ... @
tr' ∷ forall t m a. DomBuilder t m ⇒ Tr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
tr' bm = elAttr' "tr" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"tr\" ... @
tr ∷ forall t m a. DomBuilder t m ⇒ Tr → m a → m a
tr bm children = snd <$> tr' bm children

-- | A short-hand notion for @ el\' \"tr\" ... @
trN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
trN' = el' "tr"

-- | A short-hand notion for @ el \"tr\" ... @
trN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
trN children = snd <$> trN' children

-- | A short-hand notion for @ elDynAttr\' \"tr\" ... @
trD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
trD' bm = elDynAttr' "tr" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"tr\" ... @
trD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Tr → m a → m a
trD bm children = snd <$> trD' bm children


