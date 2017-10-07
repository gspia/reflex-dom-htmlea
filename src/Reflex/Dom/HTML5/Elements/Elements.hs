{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Elements.Elements
Description : Most of the HTML5 Elements with some helper functions.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Elements

This module defines majority of the elements. For each element,
there is a data-structure that contains the attributes the
element can have, and some functions to produce the element.

An example is easiest way to show how to use:
@
eDiv (setClasses [c1,c2,c3] $ id_ "myId" $ def) $ do
  eCiteN $ do
    ...
  eDialogD dAttrs $ do ...
  where
    dAttrs = title "My Title" $ setClasses [cd1, cd2,...] def
@

Naming convention:
* eAbbr'  - takes attributes and returns 'm (El, a)'
* eAbbr   - takes attributes and returns 'm a'
* eAbbrN' - no attributes and returns 'm (El, a)'
* eAbbrN  - no attributes and returns 'm a'
* eAbbrD' - dynamic attributes and returns 'm (El, a)'
* eAbbrD  - dynamic attributes and returns 'm a'

Element br has 'eBr_' function that uses default attributes
(that is, no attributes) and blank widget (that is, no children
elements). Other elements don't have '_'-ending functions.
Is there need for those?

Some of the elements can be found from the other element modules,
namely interactive, embedded, sectioning, tabular and meta-elements.

For a list of elements, see
[w3c element index](https://w3c.github.io/html/fullindex.html#index-elements).

-}

module Reflex.Dom.HTML5.Elements.Elements where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, MonadWidget, elAttr',
                       elDynAttr', blank, el')
import Reflex.Dom.HTML5.Attrs (AttrMap, attrMap,
  attrSetGlobals, attrSetAccessKey, attrSetContentEditable,
  attrSetContextMenu, attrSetClassName, attrSetDir, attrSetDraggable,
  attrSetHidden, attrSetId, attrSetLang, attrSetSpellCheck,
  attrSetStyle, attrSetTabIndex, attrSetTitle, attrSetTranslate,
  attrSetValueOlLi, attrSetMenuType, attrSetLabel, attrSetReversed,
  attrSetStart, attrSetOlType, attrSetForm, attrSetDateTime,
  attrSetCite, attrSetValueText, attrSetMax, attrSetName, attrSetForId,
  attrSetSelected, attrSetDisabled, attrSetOptimum, attrSetLow,
  attrSetMin, attrSetHigh, attrSetDefault, attrSetIcon, attrSetMenuItemType,
  attrSetTarget, attrSetMethod, attrSetNoValidate,
  attrSetEncType, attrSetAutoComplete, attrSetAction, attrSetOpen,
  attrSetAcceptCharSet, attrSetValueNumber,
  attrSetAnmval, attrSetDnmval, attrSetRole, attrSetSlot,
  AttrHasGlobals, AttrHasAccessKey, AttrHasContentEditable,
  AttrHasContextMenu, AttrHasClass, AttrHasDir, AttrHasDraggable,
  AttrHasGlobals, AttrHasAnmval, AttrHasContentEditable,
  AttrHasHidden, AttrHasId, AttrHasLang, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasHidden, AttrHasId, AttrHasRole, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasHidden, AttrHasId, AttrHasSlot, AttrHasSpellCheck,
  AttrHasStyle, AttrHasTabIndex, AttrHasTitle, AttrHasTranslate,
  AttrHasValueOlLi, AttrHasMenuType, AttrHasLabel, AttrHasReversed,
  AttrHasStart, AttrHasOlType, AttrHasForm, AttrHasDateTime,
  AttrHasCite, AttrHasValueText, AttrHasMax, AttrHasName, AttrHasForId,
  AttrHasSelected, AttrHasDisabled, AttrHasOptimum, AttrHasLow,
  AttrHasMin, AttrHasHigh, AttrHasDefault, AttrHasIcon, AttrHasMenuItemType,
  AttrHasTarget, AttrHasMethod, AttrHasNoValidate,
  AttrHasEncType, AttrHasAcceptCharSet, AttrHasValueNumber, ValueText,
  AttrHasAutoComplete, AttrHasAction, AttrHasOpen,
  AttrHasAnmval, AttrHasDnmval, AttrHasRole, AttrHasSlot,
  ValueOlLi, MenuType, Reversed, Start, OlType, Label, ValueNumber,
  DateTime, Cite, Max, Name, Form, ForId, Selected, Disabled,
  Optimum, High, Low, Min, MenuItemType, Icon, Target, NoValidate,
  Method, EncType, AutoComplete, Action, AcceptCharSet, Open, Default_,
  Globals, AttrGetClassName, attrGetClassName, gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )

------------------------------------------------------------------------------

-- | Abbr-element has only the global attributes.
data EAbbr = EAbbr
  { _eAbbrGlobals :: Maybe Globals
  , _eAbbrCustom  :: Maybe Attr
  }


instance AttrMap EAbbr where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAbbrGlobals b
    ] <> maybe [] (\a -> [a]) (_eAbbrCustom b)

instance Default EAbbr where
  def = EAbbr def def

instance Monoid EAbbr where
  mempty = def
  mappend (EAbbr a1 a2) (EAbbr b1 b2) = EAbbr (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EAbbr where
   attrSetGlobals p b = b { _eAbbrGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EAbbr
  where attrSetAccessKey p g = g { _eAbbrGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasAnmval EAbbr
  where attrSetAnmval p g = g { _eAbbrGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasContentEditable EAbbr
  where attrSetContentEditable p g = g  { _eAbbrGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasContextMenu EAbbr
  where attrSetContextMenu p g     = g { _eAbbrGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasClass EAbbr
  where attrSetClassName p g           = g { _eAbbrGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasDnmval EAbbr
  where attrSetDnmval p g           = g { _eAbbrGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasDir EAbbr
  where attrSetDir p g             = g { _eAbbrGlobals = Just (attrSetDir p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasDraggable EAbbr
  where attrSetDraggable p g       = g { _eAbbrGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasHidden EAbbr
  where attrSetHidden p g          = g { _eAbbrGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasId EAbbr
  where attrSetId p g              = g { _eAbbrGlobals = Just (attrSetId p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasLang EAbbr
  where attrSetLang p g            = g { _eAbbrGlobals = Just (attrSetLang p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasRole EAbbr
  where attrSetRole p g            = g { _eAbbrGlobals = Just (attrSetRole p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasSlot EAbbr
  where attrSetSlot p g            = g { _eAbbrGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasSpellCheck EAbbr
  where attrSetSpellCheck p g      = g { _eAbbrGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasStyle EAbbr
  where attrSetStyle p g           = g { _eAbbrGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasTabIndex EAbbr
  where attrSetTabIndex p g        = g { _eAbbrGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasTitle EAbbr
  where attrSetTitle p g           = g { _eAbbrGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAbbrGlobals g))) }
instance AttrHasTranslate EAbbr
  where attrSetTranslate p g       = g { _eAbbrGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAbbrGlobals g))) }

instance AttrGetClassName EAbbr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAbbrGlobals g)


instance AttrHasCustom EAbbr where attrSetCustom p g       = g { _eAbbrCustom = Just p }


eAbbr' :: forall t m a. DomBuilder t m => EAbbr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAbbr' b = elAttr' "abbr" (attrMap b)

eAbbr :: forall t m a. DomBuilder t m => EAbbr -> m a -> m a
eAbbr b children = snd <$> eAbbr' b children

eAbbrN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAbbrN' = el' "abbr"

eAbbrN :: forall t m a. DomBuilder t m => m a -> m a
eAbbrN children = snd <$> eAbbrN' children

eAbbrD' :: forall t m a. MonadWidget t m => Dynamic t EAbbr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAbbrD' b = elDynAttr' "abbr" (attrMap <$> b)

eAbbrD :: forall t m a. MonadWidget t m => Dynamic t EAbbr -> m a -> m a
eAbbrD b children = snd <$> eAbbrD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Address-element has only the global attributes.
data EAddress = EAddress
  { _eAddressGlobals :: Maybe Globals
  , _eAddressCustom  :: Maybe Attr
  }


instance AttrMap EAddress where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAddressGlobals b
    ] <> maybe [] (\a -> [a]) (_eAddressCustom b)


instance Default EAddress where
  def = EAddress def def

instance Monoid EAddress where
  mempty = def
  mappend (EAddress a1 a2) (EAddress b1 b2) = EAddress (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EAddress where
   attrSetGlobals p b = b { _eAddressGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EAddress
  where attrSetAccessKey p g = g { _eAddressGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasAnmval EAddress
  where attrSetAnmval p g = g { _eAddressGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasContentEditable EAddress
  where attrSetContentEditable p g = g  { _eAddressGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasContextMenu EAddress
  where attrSetContextMenu p g     = g { _eAddressGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasClass EAddress
  where attrSetClassName p g           = g { _eAddressGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasDnmval EAddress
  where attrSetDnmval p g           = g { _eAddressGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasDir EAddress
  where attrSetDir p g             = g { _eAddressGlobals = Just (attrSetDir p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasDraggable EAddress
  where attrSetDraggable p g       = g { _eAddressGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasHidden EAddress
  where attrSetHidden p g          = g { _eAddressGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasId EAddress
  where attrSetId p g              = g { _eAddressGlobals = Just (attrSetId p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasLang EAddress
  where attrSetLang p g            = g { _eAddressGlobals = Just (attrSetLang p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasRole EAddress
  where attrSetRole p g            = g { _eAddressGlobals = Just (attrSetRole p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasSlot EAddress
  where attrSetSlot p g            = g { _eAddressGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasSpellCheck EAddress
  where attrSetSpellCheck p g      = g { _eAddressGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasStyle EAddress
  where attrSetStyle p g           = g { _eAddressGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasTabIndex EAddress
  where attrSetTabIndex p g        = g { _eAddressGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasTitle EAddress
  where attrSetTitle p g           = g { _eAddressGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAddressGlobals g))) }
instance AttrHasTranslate EAddress
  where attrSetTranslate p g       = g { _eAddressGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAddressGlobals g))) }

instance AttrGetClassName EAddress where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAddressGlobals g)

instance AttrHasCustom EAddress where attrSetCustom p g       = g { _eAddressCustom = Just p }

eAddress' :: forall t m a. DomBuilder t m => EAddress -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAddress' b = elAttr' "address" (attrMap b)

eAddress :: forall t m a. DomBuilder t m => EAddress -> m a -> m a
eAddress b children = snd <$> eAddress' b children

eAddressN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAddressN' = el' "address"

eAddressN :: forall t m a. DomBuilder t m => m a -> m a
eAddressN children = snd <$> eAddressN' children

eAddressD' :: forall t m a. MonadWidget t m => Dynamic t EAddress -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAddressD' b = elDynAttr' "address" (attrMap <$> b)

eAddressD :: forall t m a. MonadWidget t m => Dynamic t EAddress -> m a -> m a
eAddressD b children = snd <$> eAddressD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | B-element has only the global attributes.
data EB = EB
  { _eBGlobals :: Maybe Globals
  , _eBCustom  :: Maybe Attr
  }


instance AttrMap EB where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBGlobals b
    ] <> maybe [] (\a -> [a]) (_eBCustom b)


instance Default EB where
  def = EB def def

instance Monoid EB where
  mempty = def
  mappend (EB a1 a2) (EB b1 b2) = EB (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EB where
   attrSetGlobals p b = b { _eBGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EB
  where attrSetAccessKey p g = g { _eBGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasAnmval EB
  where attrSetAnmval p g = g { _eBGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasContentEditable EB
  where attrSetContentEditable p g = g  { _eBGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasContextMenu EB
  where attrSetContextMenu p g     = g { _eBGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasClass EB
  where attrSetClassName p g           = g { _eBGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasDnmval EB
  where attrSetDnmval p g           = g { _eBGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasDir EB
  where attrSetDir p g             = g { _eBGlobals = Just (attrSetDir p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasDraggable EB
  where attrSetDraggable p g       = g { _eBGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasHidden EB
  where attrSetHidden p g          = g { _eBGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasId EB
  where attrSetId p g              = g { _eBGlobals = Just (attrSetId p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasLang EB
  where attrSetLang p g            = g { _eBGlobals = Just (attrSetLang p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasRole EB
  where attrSetRole p g            = g { _eBGlobals = Just (attrSetRole p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasSlot EB
  where attrSetSlot p g            = g { _eBGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasSpellCheck EB
  where attrSetSpellCheck p g      = g { _eBGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasStyle EB
  where attrSetStyle p g           = g { _eBGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasTabIndex EB
  where attrSetTabIndex p g        = g { _eBGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasTitle EB
  where attrSetTitle p g           = g { _eBGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBGlobals g))) }
instance AttrHasTranslate EB
  where attrSetTranslate p g       = g { _eBGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBGlobals g))) }

instance AttrGetClassName EB where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBGlobals g)

instance AttrHasCustom EB where attrSetCustom p g       = g { _eBCustom = Just p }

eB' :: forall t m a. DomBuilder t m => EB -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eB' b = elAttr' "b" (attrMap b)

eB :: forall t m a. DomBuilder t m => EB -> m a -> m a
eB b children = snd <$> eB' b children

eBN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBN' = el' "b"

eBN :: forall t m a. DomBuilder t m => m a -> m a
eBN children = snd <$> eBN' children

eBD' :: forall t m a. MonadWidget t m => Dynamic t EB -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBD' b = elDynAttr' "b" (attrMap <$> b)

eBD :: forall t m a. MonadWidget t m => Dynamic t EB -> m a -> m a
eBD b children = snd <$> eBD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Bdi-element has only the global attributes.
data EBdi = EBdi
  { _eBdiGlobals :: Maybe Globals
  , _eBdiCustom  :: Maybe Attr
  }


instance AttrMap EBdi where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBdiGlobals b
    ] <> maybe [] (\a -> [a]) (_eBdiCustom b)


instance Default EBdi where
  def = EBdi def def

instance Monoid EBdi where
  mempty = def
  mappend (EBdi a1 a2) (EBdi b1 b2) = EBdi (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EBdi where
   attrSetGlobals p b = b { _eBdiGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBdi
  where attrSetAccessKey p g = g { _eBdiGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasAnmval EBdi
  where attrSetAnmval p g = g { _eBdiGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasContentEditable EBdi
  where attrSetContentEditable p g = g  { _eBdiGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasContextMenu EBdi
  where attrSetContextMenu p g     = g { _eBdiGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasClass EBdi
  where attrSetClassName p g           = g { _eBdiGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasDnmval EBdi
  where attrSetDnmval p g           = g { _eBdiGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasDir EBdi
  where attrSetDir p g             = g { _eBdiGlobals = Just (attrSetDir p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasDraggable EBdi
  where attrSetDraggable p g       = g { _eBdiGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasHidden EBdi
  where attrSetHidden p g          = g { _eBdiGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasId EBdi
  where attrSetId p g              = g { _eBdiGlobals = Just (attrSetId p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasLang EBdi
  where attrSetLang p g            = g { _eBdiGlobals = Just (attrSetLang p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasRole EBdi
  where attrSetRole p g            = g { _eBdiGlobals = Just (attrSetRole p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasSlot EBdi
  where attrSetSlot p g            = g { _eBdiGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasSpellCheck EBdi
  where attrSetSpellCheck p g      = g { _eBdiGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasStyle EBdi
  where attrSetStyle p g           = g { _eBdiGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasTabIndex EBdi
  where attrSetTabIndex p g        = g { _eBdiGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasTitle EBdi
  where attrSetTitle p g           = g { _eBdiGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBdiGlobals g))) }
instance AttrHasTranslate EBdi
  where attrSetTranslate p g       = g { _eBdiGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBdiGlobals g))) }

instance AttrGetClassName EBdi where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBdiGlobals g)

instance AttrHasCustom EBdi where attrSetCustom p g       = g { _eBdiCustom = Just p }

eBdi' :: forall t m a. DomBuilder t m => EBdi -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdi' b = elAttr' "bdi" (attrMap b)

eBdi :: forall t m a. DomBuilder t m => EBdi -> m a -> m a
eBdi b children = snd <$> eBdi' b children

eBdiN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdiN' = el' "bdi"

eBdiN :: forall t m a. DomBuilder t m => m a -> m a
eBdiN children = snd <$> eBdiN' children

eBdiD' :: forall t m a. MonadWidget t m => Dynamic t EBdi -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdiD' b = elDynAttr' "bdi" (attrMap <$> b)

eBdiD :: forall t m a. MonadWidget t m => Dynamic t EBdi -> m a -> m a
eBdiD b children = snd <$> eBdiD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Bdo-element has only the global attributes.
data EBdo = EBdo
  { _eBdoGlobals :: Maybe Globals
  , _eBdoCustom  :: Maybe Attr
  }


instance AttrMap EBdo where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBdoGlobals b
    ] <> maybe [] (\a -> [a]) (_eBdoCustom b)


instance Default EBdo where
  def = EBdo def def

instance Monoid EBdo where
  mempty = def
  mappend (EBdo a1 a2) (EBdo b1 b2) = EBdo (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EBdo where
   attrSetGlobals p b = b { _eBdoGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBdo
  where attrSetAccessKey p g = g { _eBdoGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasAnmval EBdo
  where attrSetAnmval p g = g { _eBdoGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasContentEditable EBdo
  where attrSetContentEditable p g = g  { _eBdoGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasContextMenu EBdo
  where attrSetContextMenu p g     = g { _eBdoGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasClass EBdo
  where attrSetClassName p g           = g { _eBdoGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasDnmval EBdo
  where attrSetDnmval p g           = g { _eBdoGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasDir EBdo
  where attrSetDir p g             = g { _eBdoGlobals = Just (attrSetDir p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasDraggable EBdo
  where attrSetDraggable p g       = g { _eBdoGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasHidden EBdo
  where attrSetHidden p g          = g { _eBdoGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasId EBdo
  where attrSetId p g              = g { _eBdoGlobals = Just (attrSetId p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasLang EBdo
  where attrSetLang p g            = g { _eBdoGlobals = Just (attrSetLang p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasRole EBdo
  where attrSetRole p g            = g { _eBdoGlobals = Just (attrSetRole p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasSlot EBdo
  where attrSetSlot p g            = g { _eBdoGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasSpellCheck EBdo
  where attrSetSpellCheck p g      = g { _eBdoGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasStyle EBdo
  where attrSetStyle p g           = g { _eBdoGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasTabIndex EBdo
  where attrSetTabIndex p g        = g { _eBdoGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasTitle EBdo
  where attrSetTitle p g           = g { _eBdoGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBdoGlobals g))) }
instance AttrHasTranslate EBdo
  where attrSetTranslate p g       = g { _eBdoGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBdoGlobals g))) }

instance AttrGetClassName EBdo where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBdoGlobals g)

instance AttrHasCustom EBdo where attrSetCustom p g       = g { _eBdoCustom = Just p }

eBdo' :: forall t m a. DomBuilder t m => EBdo -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdo' b = elAttr' "bdo" (attrMap b)

eBdo :: forall t m a. DomBuilder t m => EBdo -> m a -> m a
eBdo b children = snd <$> eBdo' b children

eBdoN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdoN' = el' "bdo"

eBdoN :: forall t m a. DomBuilder t m => m a -> m a
eBdoN children = snd <$> eBdoN' children

eBdoD' :: forall t m a. MonadWidget t m => Dynamic t EBdo -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBdoD' b = elDynAttr' "bdo" (attrMap <$> b)

eBdoD :: forall t m a. MonadWidget t m => Dynamic t EBdo -> m a -> m a
eBdoD b children = snd <$> eBdoD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | BlockQuote-element
data EBlockQuote = EBlockQuote
  { _eBlockQuoteGlobals :: Maybe Globals
  , _eBlockQuoteCite    :: Maybe Cite
  , _eBlockQuoteCustom  :: Maybe Attr
  }


instance AttrMap EBlockQuote where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBlockQuoteGlobals b
    , attrMap <$> _eBlockQuoteCite b
    ] <> maybe [] (\a -> [a]) (_eBlockQuoteCustom b)


instance Default EBlockQuote where
  def = EBlockQuote def def def

instance Monoid EBlockQuote where
  mempty = def
  mappend (EBlockQuote a1 a2 a3) (EBlockQuote b1 b2 b3)
    = EBlockQuote (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EBlockQuote where
   attrSetGlobals p b = b { _eBlockQuoteGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBlockQuote
  where attrSetAccessKey p g = g { _eBlockQuoteGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasAnmval EBlockQuote
  where attrSetAnmval p g = g { _eBlockQuoteGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasContentEditable EBlockQuote
  where attrSetContentEditable p g = g  { _eBlockQuoteGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasContextMenu EBlockQuote
  where attrSetContextMenu p g     = g { _eBlockQuoteGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasClass EBlockQuote
  where attrSetClassName p g           = g { _eBlockQuoteGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasDnmval EBlockQuote
  where attrSetDnmval p g           = g { _eBlockQuoteGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasDir EBlockQuote
  where attrSetDir p g             = g { _eBlockQuoteGlobals = Just (attrSetDir p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasDraggable EBlockQuote
  where attrSetDraggable p g       = g { _eBlockQuoteGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasHidden EBlockQuote
  where attrSetHidden p g          = g { _eBlockQuoteGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasId EBlockQuote
  where attrSetId p g              = g { _eBlockQuoteGlobals = Just (attrSetId p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasLang EBlockQuote
  where attrSetLang p g            = g { _eBlockQuoteGlobals = Just (attrSetLang p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasRole EBlockQuote
  where attrSetRole p g            = g { _eBlockQuoteGlobals = Just (attrSetRole p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasSlot EBlockQuote
  where attrSetSlot p g            = g { _eBlockQuoteGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasSpellCheck EBlockQuote
  where attrSetSpellCheck p g      = g { _eBlockQuoteGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasStyle EBlockQuote
  where attrSetStyle p g           = g { _eBlockQuoteGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasTabIndex EBlockQuote
  where attrSetTabIndex p g        = g { _eBlockQuoteGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasTitle EBlockQuote
  where attrSetTitle p g           = g { _eBlockQuoteGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBlockQuoteGlobals g))) }
instance AttrHasTranslate EBlockQuote
  where attrSetTranslate p g       = g { _eBlockQuoteGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBlockQuoteGlobals g))) }

instance AttrGetClassName EBlockQuote where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBlockQuoteGlobals g)

instance AttrHasCite EBlockQuote where attrSetCite p g = g {_eBlockQuoteCite = Just p }

instance AttrHasCustom EBlockQuote where attrSetCustom p g       = g { _eBlockQuoteCustom = Just p }

eBlockQuote' :: forall t m a. DomBuilder t m => EBlockQuote -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBlockQuote' b  = elAttr' "blockquote" (attrMap b)

eBlockQuote :: forall t m a. DomBuilder t m => EBlockQuote -> m a -> m a
eBlockQuote b children = snd <$> eBlockQuote' b children

eBlockQuoteN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBlockQuoteN' = el' "blockquote"

eBlockQuoteN :: forall t m a. DomBuilder t m => m a -> m a
eBlockQuoteN children = snd <$> eBlockQuoteN' children

eBlockQuoteD' :: forall t m a. MonadWidget t m => Dynamic t EBlockQuote -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBlockQuoteD' b = elDynAttr' "blockquote" (attrMap <$> b)

eBlockQuoteD :: forall t m a. MonadWidget t m => Dynamic t EBlockQuote -> m a -> m a
eBlockQuoteD b children = snd <$> eBlockQuoteD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Br is non-conforming element in HTML 5.2 and according to it, should be not
-- used (use css instead). See obsolete features -section at W3.

-- | Br-element has only the global attributes.
data EBr = EBr
  { _eBrGlobals :: Maybe Globals
  , _eBrCustom  :: Maybe Attr
  }


instance AttrMap EBr where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBrGlobals b
    ] <> maybe [] (\a -> [a]) (_eBrCustom b)


instance Default EBr where
  def = EBr def def

instance Monoid EBr where
  mempty = def
  mappend (EBr a1 a2) (EBr b1 b2) = EBr (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EBr where
   attrSetGlobals p b = b { _eBrGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBr
  where attrSetAccessKey p g = g { _eBrGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasAnmval EBr
  where attrSetAnmval p g = g { _eBrGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasContentEditable EBr
  where attrSetContentEditable p g = g  { _eBrGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasContextMenu EBr
  where attrSetContextMenu p g     = g { _eBrGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasClass EBr
  where attrSetClassName p g           = g { _eBrGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasDnmval EBr
  where attrSetDnmval p g           = g { _eBrGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasDir EBr
  where attrSetDir p g             = g { _eBrGlobals = Just (attrSetDir p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasDraggable EBr
  where attrSetDraggable p g       = g { _eBrGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasHidden EBr
  where attrSetHidden p g          = g { _eBrGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasId EBr
  where attrSetId p g              = g { _eBrGlobals = Just (attrSetId p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasLang EBr
  where attrSetLang p g            = g { _eBrGlobals = Just (attrSetLang p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasRole EBr
  where attrSetRole p g            = g { _eBrGlobals = Just (attrSetRole p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasSlot EBr
  where attrSetSlot p g            = g { _eBrGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasSpellCheck EBr
  where attrSetSpellCheck p g      = g { _eBrGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasStyle EBr
  where attrSetStyle p g           = g { _eBrGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasTabIndex EBr
  where attrSetTabIndex p g        = g { _eBrGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasTitle EBr
  where attrSetTitle p g           = g { _eBrGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBrGlobals g))) }
instance AttrHasTranslate EBr
  where attrSetTranslate p g       = g { _eBrGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBrGlobals g))) }

instance AttrGetClassName EBr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBrGlobals g)

instance AttrHasCustom EBr where attrSetCustom p g       = g { _eBrCustom = Just p }

eBr' :: forall t m a. DomBuilder t m => EBr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBr' b = elAttr' "br" (attrMap b)

eBr :: forall t m a. DomBuilder t m => EBr -> m a -> m a
eBr b children = snd <$> eBr' b children

eBrN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBrN' = el' "br"

eBrN :: forall t m a. DomBuilder t m => m a -> m a
eBrN children = snd <$> eBrN' children

eBrD' :: forall t m a. MonadWidget t m => Dynamic t EBr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBrD' b = elDynAttr' "br" (attrMap <$> b)

eBrD :: forall t m a. MonadWidget t m => Dynamic t EBr -> m a -> m a
eBrD b children = snd <$> eBrD' b children

eBr_ :: forall t m. DomBuilder t m => m ()
eBr_ = eBr def blank



------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Cite-element has only the global attributes.
data ECite = ECite
  { _eCiteGlobals :: Maybe Globals
  , _eCiteCustom  :: Maybe Attr
  }


instance AttrMap ECite where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eCiteGlobals b
    ] <> maybe [] (\a -> [a]) (_eCiteCustom b)


instance Default ECite where
  def = ECite def def

instance Monoid ECite where
  mempty = def
  mappend (ECite a1 a2) (ECite b1 b2) = ECite (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ECite where
   attrSetGlobals p b = b { _eCiteGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ECite
  where attrSetAccessKey p g = g { _eCiteGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasAnmval ECite
  where attrSetAnmval p g = g { _eCiteGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasContentEditable ECite
  where attrSetContentEditable p g = g  { _eCiteGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasContextMenu ECite
  where attrSetContextMenu p g     = g { _eCiteGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasClass ECite
  where attrSetClassName p g           = g { _eCiteGlobals = Just (attrSetClassName p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasDnmval ECite
  where attrSetDnmval p g           = g { _eCiteGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasDir ECite
  where attrSetDir p g             = g { _eCiteGlobals = Just (attrSetDir p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasDraggable ECite
  where attrSetDraggable p g       = g { _eCiteGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasHidden ECite
  where attrSetHidden p g          = g { _eCiteGlobals = Just (attrSetHidden p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasId ECite
  where attrSetId p g              = g { _eCiteGlobals = Just (attrSetId p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasLang ECite
  where attrSetLang p g            = g { _eCiteGlobals = Just (attrSetLang p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasRole ECite
  where attrSetRole p g            = g { _eCiteGlobals = Just (attrSetRole p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasSlot ECite
  where attrSetSlot p g            = g { _eCiteGlobals = Just (attrSetSlot p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasSpellCheck ECite
  where attrSetSpellCheck p g      = g { _eCiteGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasStyle ECite
  where attrSetStyle p g           = g { _eCiteGlobals = Just (attrSetStyle p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasTabIndex ECite
  where attrSetTabIndex p g        = g { _eCiteGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasTitle ECite
  where attrSetTitle p g           = g { _eCiteGlobals = Just (attrSetTitle p (fromMaybe gDef (_eCiteGlobals g))) }
instance AttrHasTranslate ECite
  where attrSetTranslate p g       = g { _eCiteGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eCiteGlobals g))) }

instance AttrGetClassName ECite where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eCiteGlobals g)

instance AttrHasCustom ECite where attrSetCustom p g       = g { _eCiteCustom = Just p }

eCite' :: forall t m a. DomBuilder t m => ECite -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCite' b  = elAttr' "cite" (attrMap b)

eCite :: forall t m a. DomBuilder t m => ECite -> m a -> m a
eCite b children = snd <$> eCite' b children

eCiteN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCiteN' = el' "cite"

eCiteN :: forall t m a. DomBuilder t m => m a -> m a
eCiteN children = snd <$> eCiteN' children

eCiteD' :: forall t m a. MonadWidget t m => Dynamic t ECite -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCiteD' b  = elDynAttr' "cite" (attrMap <$> b)

eCiteD :: forall t m a. MonadWidget t m => Dynamic t ECite -> m a -> m a
eCiteD b children = snd <$> eCiteD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Code-element has only the global attributes.
data ECode = ECode
  { _eCodeGlobals :: Maybe Globals
  , _eCodeCustom  :: Maybe Attr
  }


instance AttrMap ECode where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eCodeGlobals b
    ] <> maybe [] (\a -> [a]) (_eCodeCustom b)


instance Default ECode where
  def = ECode def def

instance Monoid ECode where
  mempty = def
  mappend (ECode a1 a2) (ECode b1 b2) = ECode (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ECode where
   attrSetGlobals p b = b { _eCodeGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ECode
  where attrSetAccessKey p g = g { _eCodeGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasAnmval ECode
  where attrSetAnmval p g = g { _eCodeGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasContentEditable ECode
  where attrSetContentEditable p g = g  { _eCodeGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasContextMenu ECode
  where attrSetContextMenu p g     = g { _eCodeGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasClass ECode
  where attrSetClassName p g           = g { _eCodeGlobals = Just (attrSetClassName p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasDnmval ECode
  where attrSetDnmval p g           = g { _eCodeGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasDir ECode
  where attrSetDir p g             = g { _eCodeGlobals = Just (attrSetDir p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasDraggable ECode
  where attrSetDraggable p g       = g { _eCodeGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasHidden ECode
  where attrSetHidden p g          = g { _eCodeGlobals = Just (attrSetHidden p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasId ECode
  where attrSetId p g              = g { _eCodeGlobals = Just (attrSetId p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasLang ECode
  where attrSetLang p g            = g { _eCodeGlobals = Just (attrSetLang p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasRole ECode
  where attrSetRole p g            = g { _eCodeGlobals = Just (attrSetRole p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasSlot ECode
  where attrSetSlot p g            = g { _eCodeGlobals = Just (attrSetSlot p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasSpellCheck ECode
  where attrSetSpellCheck p g      = g { _eCodeGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasStyle ECode
  where attrSetStyle p g           = g { _eCodeGlobals = Just (attrSetStyle p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasTabIndex ECode
  where attrSetTabIndex p g        = g { _eCodeGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasTitle ECode
  where attrSetTitle p g           = g { _eCodeGlobals = Just (attrSetTitle p (fromMaybe gDef (_eCodeGlobals g))) }
instance AttrHasTranslate ECode
  where attrSetTranslate p g       = g { _eCodeGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eCodeGlobals g))) }

instance AttrGetClassName ECode where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eCodeGlobals g)

instance AttrHasCustom ECode where attrSetCustom p g       = g { _eCodeCustom = Just p }

eCode' :: forall t m a. DomBuilder t m => ECode -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCode' b  = elAttr' "code" (attrMap b)

eCode :: forall t m a. DomBuilder t m => ECode -> m a -> m a
eCode b children = snd <$> eCode' b children

eCodeN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCodeN' = el' "code"

eCodeN :: forall t m a. DomBuilder t m => m a -> m a
eCodeN children = snd <$> eCodeN' children

eCodeD' :: forall t m a. MonadWidget t m => Dynamic t ECode -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCodeD' b  = elDynAttr' "code" (attrMap <$> b)

eCodeD :: forall t m a. MonadWidget t m => Dynamic t ECode -> m a -> m a
eCodeD b children = snd <$> eCodeD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Data-element
-- Is value-attribute ok here?
data EData = EData
  { _eDataGlobals   :: Maybe Globals
  , _eDataValueText :: Maybe ValueText
  , _eDataCustom    :: Maybe Attr
  }


instance AttrMap EData where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDataGlobals b
    , attrMap <$> _eDataValueText b
    ] <> maybe [] (\a -> [a]) (_eDataCustom b)


instance Default EData where
  def = EData def def def

instance Monoid EData where
  mempty = def
  mappend (EData a1 a2 a3) (EData b1 b2 b3)
    = EData (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EData where
   attrSetGlobals p b = b { _eDataGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EData
  where attrSetAccessKey p g = g { _eDataGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasAnmval EData
  where attrSetAnmval p g = g { _eDataGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasContentEditable EData
  where attrSetContentEditable p g = g  { _eDataGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasContextMenu EData
  where attrSetContextMenu p g     = g { _eDataGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasClass EData
  where attrSetClassName p g           = g { _eDataGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasDnmval EData
  where attrSetDnmval p g           = g { _eDataGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasDir EData
  where attrSetDir p g             = g { _eDataGlobals = Just (attrSetDir p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasDraggable EData
  where attrSetDraggable p g       = g { _eDataGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasHidden EData
  where attrSetHidden p g          = g { _eDataGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasId EData
  where attrSetId p g              = g { _eDataGlobals = Just (attrSetId p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasLang EData
  where attrSetLang p g            = g { _eDataGlobals = Just (attrSetLang p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasRole EData
  where attrSetRole p g            = g { _eDataGlobals = Just (attrSetRole p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasSlot EData
  where attrSetSlot p g            = g { _eDataGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasSpellCheck EData
  where attrSetSpellCheck p g      = g { _eDataGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasStyle EData
  where attrSetStyle p g           = g { _eDataGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasTabIndex EData
  where attrSetTabIndex p g        = g { _eDataGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasTitle EData
  where attrSetTitle p g           = g { _eDataGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDataGlobals g))) }
instance AttrHasTranslate EData
  where attrSetTranslate p g       = g { _eDataGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDataGlobals g))) }

instance AttrGetClassName EData where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDataGlobals g)

instance AttrHasValueText EData where attrSetValueText p g = g {_eDataValueText = Just p }

instance AttrHasCustom EData where attrSetCustom p g       = g { _eDataCustom = Just p }

eData' :: forall t m a. DomBuilder t m => EData -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eData' b  = elAttr' "data" (attrMap b)

eData :: forall t m a. DomBuilder t m => EData -> m a -> m a
eData b children = snd <$> eData' b children

eDataN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDataN' = el' "data"

eDataN :: forall t m a. DomBuilder t m => m a -> m a
eDataN children = snd <$> eDataN' children

eDataD' :: forall t m a. MonadWidget t m => Dynamic t EData -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDataD' b = elDynAttr' "data" (attrMap <$> b)

eDataD :: forall t m a. MonadWidget t m => Dynamic t EData -> m a -> m a
eDataD b children = snd <$> eDataD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | DataList-element has only the global attributes.
data EDataList = EDataList
  { _eDataListGlobals :: Maybe Globals
  , _eDataListCustom  :: Maybe Attr
  }


instance AttrMap EDataList where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDataListGlobals b
    ] <> maybe [] (\a -> [a]) (_eDataListCustom b)


instance Default EDataList where
  def = EDataList def def

instance Monoid EDataList where
  mempty = def
  mappend (EDataList a1 a2) (EDataList b1 b2) = EDataList (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDataList where
   attrSetGlobals p b = b { _eDataListGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDataList
  where attrSetAccessKey p g = g { _eDataListGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasAnmval EDataList
  where attrSetAnmval p g = g { _eDataListGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasContentEditable EDataList
  where attrSetContentEditable p g = g  { _eDataListGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasContextMenu EDataList
  where attrSetContextMenu p g     = g { _eDataListGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasClass EDataList
  where attrSetClassName p g           = g { _eDataListGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasDnmval EDataList
  where attrSetDnmval p g           = g { _eDataListGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasDir EDataList
  where attrSetDir p g             = g { _eDataListGlobals = Just (attrSetDir p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasDraggable EDataList
  where attrSetDraggable p g       = g { _eDataListGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasHidden EDataList
  where attrSetHidden p g          = g { _eDataListGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasId EDataList
  where attrSetId p g              = g { _eDataListGlobals = Just (attrSetId p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasLang EDataList
  where attrSetLang p g            = g { _eDataListGlobals = Just (attrSetLang p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasRole EDataList
  where attrSetRole p g            = g { _eDataListGlobals = Just (attrSetRole p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasSlot EDataList
  where attrSetSlot p g            = g { _eDataListGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasSpellCheck EDataList
  where attrSetSpellCheck p g      = g { _eDataListGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasStyle EDataList
  where attrSetStyle p g           = g { _eDataListGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasTabIndex EDataList
  where attrSetTabIndex p g        = g { _eDataListGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasTitle EDataList
  where attrSetTitle p g           = g { _eDataListGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDataListGlobals g))) }
instance AttrHasTranslate EDataList
  where attrSetTranslate p g       = g { _eDataListGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDataListGlobals g))) }

instance AttrGetClassName EDataList where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDataListGlobals g)

instance AttrHasCustom EDataList where attrSetCustom p g       = g { _eDataListCustom = Just p }

eDataList' :: forall t m a. DomBuilder t m => EDataList -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDataList' b  = elAttr' "datalist" (attrMap b)

eDataList :: forall t m a. DomBuilder t m => EDataList -> m a -> m a
eDataList b children = snd <$> eDataList' b children

eDataListN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDataListN' = el' "datalist"

eDataListN :: forall t m a. DomBuilder t m => m a -> m a
eDataListN children = snd <$> eDataListN' children

eDataListD' :: forall t m a. MonadWidget t m => Dynamic t EDataList -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDataListD' b  = elDynAttr' "datalist" (attrMap <$> b)

eDataListD :: forall t m a. MonadWidget t m => Dynamic t EDataList -> m a -> m a
eDataListD b children = snd <$> eDataListD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Del-element
data EDel = EDel
  { _eDelGlobals  :: Maybe Globals
  , _eDelCite     :: Maybe Cite
  , _eDelDateTime :: Maybe DateTime
  , _eDelCustom   :: Maybe Attr
  }


instance AttrMap EDel where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDelGlobals b
    , attrMap <$> _eDelCite b
    , attrMap <$> _eDelDateTime b
    ] <> maybe [] (\a -> [a]) (_eDelCustom b)


instance Default EDel where
  def = EDel def def def def

instance Monoid EDel where
  mempty = def
  mappend (EDel a1 a2 a3 a4) (EDel b1 b2 b3 b4)
    = EDel (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals EDel where
   attrSetGlobals p b = b { _eDelGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDel
  where attrSetAccessKey p g = g { _eDelGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasAnmval EDel
  where attrSetAnmval p g = g { _eDelGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasContentEditable EDel
  where attrSetContentEditable p g = g  { _eDelGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasContextMenu EDel
  where attrSetContextMenu p g     = g { _eDelGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasClass EDel
  where attrSetClassName p g           = g { _eDelGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasDnmval EDel
  where attrSetDnmval p g           = g { _eDelGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasDir EDel
  where attrSetDir p g             = g { _eDelGlobals = Just (attrSetDir p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasDraggable EDel
  where attrSetDraggable p g       = g { _eDelGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasHidden EDel
  where attrSetHidden p g          = g { _eDelGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasId EDel
  where attrSetId p g              = g { _eDelGlobals = Just (attrSetId p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasLang EDel
  where attrSetLang p g            = g { _eDelGlobals = Just (attrSetLang p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasRole EDel
  where attrSetRole p g            = g { _eDelGlobals = Just (attrSetRole p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasSlot EDel
  where attrSetSlot p g            = g { _eDelGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasSpellCheck EDel
  where attrSetSpellCheck p g      = g { _eDelGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasStyle EDel
  where attrSetStyle p g           = g { _eDelGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasTabIndex EDel
  where attrSetTabIndex p g        = g { _eDelGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasTitle EDel
  where attrSetTitle p g           = g { _eDelGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDelGlobals g))) }
instance AttrHasTranslate EDel
  where attrSetTranslate p g       = g { _eDelGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDelGlobals g))) }

instance AttrGetClassName EDel where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDelGlobals g)

instance AttrHasCite EDel where attrSetCite p g = g {_eDelCite = Just p }
instance AttrHasDateTime EDel where attrSetDateTime p g = g {_eDelDateTime = Just p }

instance AttrHasCustom EDel where attrSetCustom p g       = g { _eDelCustom = Just p }

eDel' :: forall t m a. DomBuilder t m => EDel -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDel' b  = elAttr' "del" (attrMap b)

eDel :: forall t m a. DomBuilder t m => EDel -> m a -> m a
eDel b children = snd <$> eDel' b children

eDelN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDelN' = el' "del"

eDelN :: forall t m a. DomBuilder t m => m a -> m a
eDelN children = snd <$> eDelN' children

eDelD' :: forall t m a. MonadWidget t m => Dynamic t EDel -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDelD' b = elDynAttr' "del" (attrMap <$> b)

eDelD :: forall t m a. MonadWidget t m => Dynamic t EDel -> m a -> m a
eDelD b children = snd <$> eDelD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | DD-element has only the global attributes.
data EDd = EDd
  { _eDdGlobals :: Maybe Globals
  , _eDdCustom  :: Maybe Attr
  }


instance AttrMap EDd where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDdGlobals b
    ] <> maybe [] (\a -> [a]) (_eDdCustom b)

instance Default EDd where
  def = EDd def def

instance Monoid EDd where
  mempty = def
  mappend (EDd a1 a2) (EDd b1 b2) = EDd (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDd where
   attrSetGlobals p b = b { _eDdGlobals = Just p }

-- Global attributes reqere the following instances.
instance AttrHasAccessKey EDd
  where attrSetAccessKey p g = g { _eDdGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasAnmval EDd
  where attrSetAnmval p g = g { _eDdGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasContentEditable EDd
  where attrSetContentEditable p g = g  { _eDdGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasContextMenu EDd
  where attrSetContextMenu p g     = g { _eDdGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasClass EDd
  where attrSetClassName p g           = g { _eDdGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasDnmval EDd
  where attrSetDnmval p g           = g { _eDdGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasDir EDd
  where attrSetDir p g             = g { _eDdGlobals = Just (attrSetDir p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasDraggable EDd
  where attrSetDraggable p g       = g { _eDdGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasHidden EDd
  where attrSetHidden p g          = g { _eDdGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasId EDd
  where attrSetId p g              = g { _eDdGlobals = Just (attrSetId p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasLang EDd
  where attrSetLang p g            = g { _eDdGlobals = Just (attrSetLang p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasRole EDd
  where attrSetRole p g            = g { _eDdGlobals = Just (attrSetRole p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasSlot EDd
  where attrSetSlot p g            = g { _eDdGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasSpellCheck EDd
  where attrSetSpellCheck p g      = g { _eDdGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasStyle EDd
  where attrSetStyle p g           = g { _eDdGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasTabIndex EDd
  where attrSetTabIndex p g        = g { _eDdGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasTitle EDd
  where attrSetTitle p g           = g { _eDdGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDdGlobals g))) }
instance AttrHasTranslate EDd
  where attrSetTranslate p g       = g { _eDdGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDdGlobals g))) }

instance AttrGetClassName EDd where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDdGlobals g)

instance AttrHasCustom EDd where attrSetCustom p g       = g { _eDdCustom = Just p }

eDd' :: forall t m a. DomBuilder t m => EDd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDd' b  = elAttr' "dd" (attrMap b)

eDd :: forall t m a. DomBuilder t m => EDd -> m a -> m a
eDd b children = snd <$> eDd' b children

eDdN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDdN' = el' "dd"

eDdN :: forall t m a. DomBuilder t m => m a -> m a
eDdN children = snd <$> eDdN' children

eDdD' :: forall t m a. MonadWidget t m => Dynamic t EDd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDdD' b  = elDynAttr' "dd" (attrMap <$> b)

eDdD :: forall t m a. MonadWidget t m => Dynamic t EDd -> m a -> m a
eDdD b children = snd <$> eDdD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dfn-element has only the global attributes.
data EDfn = EDfn
  { _eDfnGlobals :: Maybe Globals
  , _eDfnCustom  :: Maybe Attr
  }


instance AttrMap EDfn where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDfnGlobals b
    ] <> maybe [] (\a -> [a]) (_eDfnCustom b)


instance Default EDfn where
  def = EDfn def def

instance Monoid EDfn where
  mempty = def
  mappend (EDfn a1 a2) (EDfn b1 b2) = EDfn (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDfn where
   attrSetGlobals p b = b { _eDfnGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDfn
  where attrSetAccessKey p g = g { _eDfnGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasAnmval EDfn
  where attrSetAnmval p g = g { _eDfnGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasContentEditable EDfn
  where attrSetContentEditable p g = g  { _eDfnGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasContextMenu EDfn
  where attrSetContextMenu p g     = g { _eDfnGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasClass EDfn
  where attrSetClassName p g           = g { _eDfnGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasDnmval EDfn
  where attrSetDnmval p g           = g { _eDfnGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasDir EDfn
  where attrSetDir p g             = g { _eDfnGlobals = Just (attrSetDir p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasDraggable EDfn
  where attrSetDraggable p g       = g { _eDfnGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasHidden EDfn
  where attrSetHidden p g          = g { _eDfnGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasId EDfn
  where attrSetId p g              = g { _eDfnGlobals = Just (attrSetId p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasLang EDfn
  where attrSetLang p g            = g { _eDfnGlobals = Just (attrSetLang p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasRole EDfn
  where attrSetRole p g            = g { _eDfnGlobals = Just (attrSetRole p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasSlot EDfn
  where attrSetSlot p g            = g { _eDfnGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasSpellCheck EDfn
  where attrSetSpellCheck p g      = g { _eDfnGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasStyle EDfn
  where attrSetStyle p g           = g { _eDfnGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasTabIndex EDfn
  where attrSetTabIndex p g        = g { _eDfnGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasTitle EDfn
  where attrSetTitle p g           = g { _eDfnGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDfnGlobals g))) }
instance AttrHasTranslate EDfn
  where attrSetTranslate p g       = g { _eDfnGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDfnGlobals g))) }

instance AttrGetClassName EDfn where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDfnGlobals g)

instance AttrHasCustom EDfn where attrSetCustom p g       = g { _eDfnCustom = Just p }

eDfn' :: forall t m a. DomBuilder t m => EDfn -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDfn' b  = elAttr' "dfn" (attrMap b)

eDfn :: forall t m a. DomBuilder t m => EDfn -> m a -> m a
eDfn b children = snd <$> eDfn' b children

eDfnN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDfnN' = el' "dfn"

eDfnN :: forall t m a. DomBuilder t m => m a -> m a
eDfnN children = snd <$> eDfnN' children

eDfnD' :: forall t m a. MonadWidget t m => Dynamic t EDfn -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDfnD' b  = elDynAttr' "dfn" (attrMap <$> b)

eDfnD :: forall t m a. MonadWidget t m => Dynamic t EDfn -> m a -> m a
eDfnD b children = snd <$> eDfnD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dialog-element
data EDialog = EDialog
  { _eDialogGlobals :: Maybe Globals
  , _eDialogOpen    :: Maybe Open
  , _eDialogCustom  :: Maybe Attr
  }


instance AttrMap EDialog where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDialogGlobals b
    , attrMap <$> _eDialogOpen b
    ] <> maybe [] (\a -> [a]) (_eDialogCustom b)


instance Default EDialog where
  def = EDialog def def def

instance Monoid EDialog where
  mempty = def
  mappend (EDialog a1 a2 a3) (EDialog b1 b2 b3)
    = EDialog (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EDialog where
   attrSetGlobals p b = b { _eDialogGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDialog
  where attrSetAccessKey p g = g { _eDialogGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasAnmval EDialog
  where attrSetAnmval p g = g { _eDialogGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasContentEditable EDialog
  where attrSetContentEditable p g = g  { _eDialogGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasContextMenu EDialog
  where attrSetContextMenu p g     = g { _eDialogGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasClass EDialog
  where attrSetClassName p g           = g { _eDialogGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasDnmval EDialog
  where attrSetDnmval p g           = g { _eDialogGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasDir EDialog
  where attrSetDir p g             = g { _eDialogGlobals = Just (attrSetDir p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasDraggable EDialog
  where attrSetDraggable p g       = g { _eDialogGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasHidden EDialog
  where attrSetHidden p g          = g { _eDialogGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasId EDialog
  where attrSetId p g              = g { _eDialogGlobals = Just (attrSetId p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasLang EDialog
  where attrSetLang p g            = g { _eDialogGlobals = Just (attrSetLang p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasRole EDialog
  where attrSetRole p g            = g { _eDialogGlobals = Just (attrSetRole p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasSlot EDialog
  where attrSetSlot p g            = g { _eDialogGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasSpellCheck EDialog
  where attrSetSpellCheck p g      = g { _eDialogGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasStyle EDialog
  where attrSetStyle p g           = g { _eDialogGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasTabIndex EDialog
  where attrSetTabIndex p g        = g { _eDialogGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasTitle EDialog
  where attrSetTitle p g           = g { _eDialogGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDialogGlobals g))) }
instance AttrHasTranslate EDialog
  where attrSetTranslate p g       = g { _eDialogGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDialogGlobals g))) }

instance AttrGetClassName EDialog where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDialogGlobals g)

instance AttrHasOpen EDialog where attrSetOpen p g = g {_eDialogOpen = Just p }

instance AttrHasCustom EDialog where attrSetCustom p g       = g { _eDialogCustom = Just p }

eDialog' :: forall t m a. DomBuilder t m => EDialog -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDialog' b  = elAttr' "dialog" (attrMap b)

eDialog :: forall t m a. DomBuilder t m => EDialog -> m a -> m a
eDialog b children = snd <$> eDialog' b children

eDialogN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDialogN' = el' "dialog"

eDialogN :: forall t m a. DomBuilder t m => m a -> m a
eDialogN children = snd <$> eDialogN' children

eDialogD' :: forall t m a. MonadWidget t m => Dynamic t EDialog -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDialogD' b = elDynAttr' "dialog" (attrMap <$> b)

eDialogD :: forall t m a. MonadWidget t m => Dynamic t EDialog -> m a -> m a
eDialogD b children = snd <$> eDialogD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Div-element has only the global attributes.
data EDiv = EDiv
  { _eDivGlobals :: Maybe Globals
  , _eDivCustom  :: Maybe Attr
  }


instance AttrMap EDiv where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDivGlobals b
    ] <> maybe [] (\a -> [a]) (_eDivCustom b)

instance Default EDiv where
  def = EDiv def def

instance Monoid EDiv where
  mempty = def
  mappend (EDiv a1 a2) (EDiv b1 b2) = EDiv (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDiv where
   attrSetGlobals p b = b { _eDivGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDiv
  where attrSetAccessKey p g = g { _eDivGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasAnmval EDiv
  where attrSetAnmval p g = g { _eDivGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasContentEditable EDiv
  where attrSetContentEditable p g = g  { _eDivGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasContextMenu EDiv
  where attrSetContextMenu p g     = g { _eDivGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasClass EDiv
  where attrSetClassName p g           = g { _eDivGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasDnmval EDiv
  where attrSetDnmval p g           = g { _eDivGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasDir EDiv
  where attrSetDir p g             = g { _eDivGlobals = Just (attrSetDir p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasDraggable EDiv
  where attrSetDraggable p g       = g { _eDivGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasHidden EDiv
  where attrSetHidden p g          = g { _eDivGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasId EDiv
  where attrSetId p g              = g { _eDivGlobals = Just (attrSetId p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasLang EDiv
  where attrSetLang p g            = g { _eDivGlobals = Just (attrSetLang p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasRole EDiv
  where attrSetRole p g            = g { _eDivGlobals = Just (attrSetRole p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasSlot EDiv
  where attrSetSlot p g            = g { _eDivGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasSpellCheck EDiv
  where attrSetSpellCheck p g      = g { _eDivGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasStyle EDiv
  where attrSetStyle p g           = g { _eDivGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasTabIndex EDiv
  where attrSetTabIndex p g        = g { _eDivGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasTitle EDiv
  where attrSetTitle p g           = g { _eDivGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDivGlobals g))) }
instance AttrHasTranslate EDiv
  where attrSetTranslate p g       = g { _eDivGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDivGlobals g))) }

instance AttrGetClassName EDiv where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDivGlobals g)

instance AttrHasCustom EDiv where attrSetCustom p g       = g { _eDivCustom = Just p }

eDiv' :: forall t m a. DomBuilder t m => EDiv -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDiv' b  = elAttr' "div" (attrMap b)

eDiv :: forall t m a. DomBuilder t m => EDiv -> m a -> m a
eDiv b children = snd <$> eDiv' b children

eDivN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDivN' = el' "div"

eDivN :: forall t m a. DomBuilder t m => m a -> m a
eDivN children = snd <$> eDivN' children

eDivD' :: forall t m a. MonadWidget t m => Dynamic t EDiv -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDivD' b  = elDynAttr' "div" (attrMap <$> b)

eDivD :: forall t m a. MonadWidget t m => Dynamic t EDiv -> m a -> m a
eDivD b children = snd <$> eDivD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dl-element has only the global attributes.
data EDl = EDl
  { _eDlGlobals :: Maybe Globals
  , _eDlCustom  :: Maybe Attr
  }


instance AttrMap EDl where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDlGlobals b
    ] <> maybe [] (\a -> [a]) (_eDlCustom b)


instance Default EDl where
  def = EDl def def

instance Monoid EDl where
  mempty = def
  mappend (EDl a1 a2) (EDl b1 b2) = EDl (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDl where
   attrSetGlobals p b = b { _eDlGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDl
  where attrSetAccessKey p g = g { _eDlGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasAnmval EDl
  where attrSetAnmval p g = g { _eDlGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasContentEditable EDl
  where attrSetContentEditable p g = g  { _eDlGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasContextMenu EDl
  where attrSetContextMenu p g     = g { _eDlGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasClass EDl
  where attrSetClassName p g           = g { _eDlGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasDnmval EDl
  where attrSetDnmval p g           = g { _eDlGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasDir EDl
  where attrSetDir p g             = g { _eDlGlobals = Just (attrSetDir p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasDraggable EDl
  where attrSetDraggable p g       = g { _eDlGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasHidden EDl
  where attrSetHidden p g          = g { _eDlGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasId EDl
  where attrSetId p g              = g { _eDlGlobals = Just (attrSetId p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasLang EDl
  where attrSetLang p g            = g { _eDlGlobals = Just (attrSetLang p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasRole EDl
  where attrSetRole p g            = g { _eDlGlobals = Just (attrSetRole p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasSlot EDl
  where attrSetSlot p g            = g { _eDlGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasSpellCheck EDl
  where attrSetSpellCheck p g      = g { _eDlGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasStyle EDl
  where attrSetStyle p g           = g { _eDlGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasTabIndex EDl
  where attrSetTabIndex p g        = g { _eDlGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasTitle EDl
  where attrSetTitle p g           = g { _eDlGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDlGlobals g))) }
instance AttrHasTranslate EDl
  where attrSetTranslate p g       = g { _eDlGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDlGlobals g))) }

instance AttrGetClassName EDl where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDlGlobals g)

instance AttrHasCustom EDl where attrSetCustom p g       = g { _eDlCustom = Just p }

eDl' :: forall t m a. DomBuilder t m => EDl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDl' b  = elAttr' "dl" (attrMap b)

eDl :: forall t m a. DomBuilder t m => EDl -> m a -> m a
eDl b children = snd <$> eDl' b children

eDlN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDlN' = el' "dl"

eDlN :: forall t m a. DomBuilder t m => m a -> m a
eDlN children = snd <$> eDlN' children

eDlD' :: forall t m a. MonadWidget t m => Dynamic t EDl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDlD' b  = elDynAttr' "dl" (attrMap <$> b)

eDlD :: forall t m a. MonadWidget t m => Dynamic t EDl -> m a -> m a
eDlD b children = snd <$> eDlD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dt-element has only the global attributes.
data EDt = EDt
  { _eDtGlobals :: Maybe Globals
  , _eDtCustom  :: Maybe Attr
  }


instance AttrMap EDt where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDtGlobals b
    ] <> maybe [] (\a -> [a]) (_eDtCustom b)


instance Default EDt where
  def = EDt def def

instance Monoid EDt where
  mempty = def
  mappend (EDt a1 a2) (EDt b1 b2) = EDt (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EDt where
   attrSetGlobals p b = b { _eDtGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDt
  where attrSetAccessKey p g = g { _eDtGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasAnmval EDt
  where attrSetAnmval p g = g { _eDtGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasContentEditable EDt
  where attrSetContentEditable p g = g  { _eDtGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasContextMenu EDt
  where attrSetContextMenu p g     = g { _eDtGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasClass EDt
  where attrSetClassName p g           = g { _eDtGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasDnmval EDt
  where attrSetDnmval p g           = g { _eDtGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasDir EDt
  where attrSetDir p g             = g { _eDtGlobals = Just (attrSetDir p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasDraggable EDt
  where attrSetDraggable p g       = g { _eDtGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasHidden EDt
  where attrSetHidden p g          = g { _eDtGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasId EDt
  where attrSetId p g              = g { _eDtGlobals = Just (attrSetId p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasLang EDt
  where attrSetLang p g            = g { _eDtGlobals = Just (attrSetLang p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasRole EDt
  where attrSetRole p g            = g { _eDtGlobals = Just (attrSetRole p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasSlot EDt
  where attrSetSlot p g            = g { _eDtGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasSpellCheck EDt
  where attrSetSpellCheck p g      = g { _eDtGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasStyle EDt
  where attrSetStyle p g           = g { _eDtGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasTabIndex EDt
  where attrSetTabIndex p g        = g { _eDtGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasTitle EDt
  where attrSetTitle p g           = g { _eDtGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDtGlobals g))) }
instance AttrHasTranslate EDt
  where attrSetTranslate p g       = g { _eDtGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDtGlobals g))) }

instance AttrGetClassName EDt where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDtGlobals g)

instance AttrHasCustom EDt where attrSetCustom p g       = g { _eDtCustom = Just p }

eDt' :: forall t m a. DomBuilder t m => EDt -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDt' b  = elAttr' "dt" (attrMap b)

eDt :: forall t m a. DomBuilder t m => EDt -> m a -> m a
eDt b children = snd <$> eDt' b children

eDtN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDtN' = el' "dt"

eDtN :: forall t m a. DomBuilder t m => m a -> m a
eDtN children = snd <$> eDtN' children

eDtD' :: forall t m a. MonadWidget t m => Dynamic t EDt -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDtD' b  = elDynAttr' "dt" (attrMap <$> b)

eDtD :: forall t m a. MonadWidget t m => Dynamic t EDt -> m a -> m a
eDtD b children = snd <$> eDtD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Em-element has only the global attributes.
data EEm = EEm
  { _eEmGlobals :: Maybe Globals
  , _eEmCustom  :: Maybe Attr
  }


instance AttrMap EEm where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eEmGlobals b
    ] <> maybe [] (\a -> [a]) (_eEmCustom b)


instance Default EEm where
  def = EEm def def

instance Monoid EEm where
  mempty = def
  mappend (EEm a1 a2) (EEm b1 b2) = EEm (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EEm where
   attrSetGlobals p b = b { _eEmGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EEm
  where attrSetAccessKey p g = g { _eEmGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasAnmval EEm
  where attrSetAnmval p g = g { _eEmGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasContentEditable EEm
  where attrSetContentEditable p g = g  { _eEmGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasContextMenu EEm
  where attrSetContextMenu p g     = g { _eEmGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasClass EEm
  where attrSetClassName p g           = g { _eEmGlobals = Just (attrSetClassName p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasDnmval EEm
  where attrSetDnmval p g           = g { _eEmGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasDir EEm
  where attrSetDir p g             = g { _eEmGlobals = Just (attrSetDir p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasDraggable EEm
  where attrSetDraggable p g       = g { _eEmGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasHidden EEm
  where attrSetHidden p g          = g { _eEmGlobals = Just (attrSetHidden p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasId EEm
  where attrSetId p g              = g { _eEmGlobals = Just (attrSetId p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasLang EEm
  where attrSetLang p g            = g { _eEmGlobals = Just (attrSetLang p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasRole EEm
  where attrSetRole p g            = g { _eEmGlobals = Just (attrSetRole p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasSlot EEm
  where attrSetSlot p g            = g { _eEmGlobals = Just (attrSetSlot p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasSpellCheck EEm
  where attrSetSpellCheck p g      = g { _eEmGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasStyle EEm
  where attrSetStyle p g           = g { _eEmGlobals = Just (attrSetStyle p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasTabIndex EEm
  where attrSetTabIndex p g        = g { _eEmGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasTitle EEm
  where attrSetTitle p g           = g { _eEmGlobals = Just (attrSetTitle p (fromMaybe gDef (_eEmGlobals g))) }
instance AttrHasTranslate EEm
  where attrSetTranslate p g       = g { _eEmGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eEmGlobals g))) }

instance AttrGetClassName EEm where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eEmGlobals g)

instance AttrHasCustom EEm where attrSetCustom p g       = g { _eEmCustom = Just p }

eEm' :: forall t m a. DomBuilder t m => EEm -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEm' b  = elAttr' "em" (attrMap b)

eEm :: forall t m a. DomBuilder t m => EEm -> m a -> m a
eEm b children = snd <$> eEm' b children

eEmN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEmN' = el' "em"

eEmN :: forall t m a. DomBuilder t m => m a -> m a
eEmN children = snd <$> eEmN' children

eEmD' :: forall t m a. MonadWidget t m => Dynamic t EEm -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEmD' b  = elDynAttr' "em" (attrMap <$> b)

eEmD :: forall t m a. MonadWidget t m => Dynamic t EEm -> m a -> m a
eEmD b children = snd <$> eEmD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | FieldSet-element
data EFieldSet = EFieldSet
  { _eFieldSetGlobals  :: Maybe Globals
  , _eFieldSetDisabled :: Maybe Disabled
  , _eFieldSetForm     :: Maybe Form
  , _eFieldSetName     :: Maybe Name
  , _eFieldSetCustom   :: Maybe Attr
  }


instance AttrMap EFieldSet where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eFieldSetGlobals b
    , attrMap <$> _eFieldSetDisabled b
    , attrMap <$> _eFieldSetForm b
    , attrMap <$> _eFieldSetName b
    ] <> maybe [] (\a -> [a]) (_eFieldSetCustom b)


instance Default EFieldSet where
  def = EFieldSet def def def def def

instance Monoid EFieldSet where
  mempty = def
  mappend (EFieldSet a1 a2 a3 a4 a5) (EFieldSet b1 b2 b3 b4 b5)
    = EFieldSet (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance AttrHasGlobals EFieldSet where
   attrSetGlobals p b = b { _eFieldSetGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EFieldSet
  where attrSetAccessKey p g = g { _eFieldSetGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasAnmval EFieldSet
  where attrSetAnmval p g = g { _eFieldSetGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasContentEditable EFieldSet
  where attrSetContentEditable p g = g  { _eFieldSetGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasContextMenu EFieldSet
  where attrSetContextMenu p g     = g { _eFieldSetGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasClass EFieldSet
  where attrSetClassName p g           = g { _eFieldSetGlobals = Just (attrSetClassName p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasDnmval EFieldSet
  where attrSetDnmval p g           = g { _eFieldSetGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasDir EFieldSet
  where attrSetDir p g             = g { _eFieldSetGlobals = Just (attrSetDir p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasDraggable EFieldSet
  where attrSetDraggable p g       = g { _eFieldSetGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasHidden EFieldSet
  where attrSetHidden p g          = g { _eFieldSetGlobals = Just (attrSetHidden p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasId EFieldSet
  where attrSetId p g              = g { _eFieldSetGlobals = Just (attrSetId p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasLang EFieldSet
  where attrSetLang p g            = g { _eFieldSetGlobals = Just (attrSetLang p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasRole EFieldSet
  where attrSetRole p g            = g { _eFieldSetGlobals = Just (attrSetRole p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasSlot EFieldSet
  where attrSetSlot p g            = g { _eFieldSetGlobals = Just (attrSetSlot p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasSpellCheck EFieldSet
  where attrSetSpellCheck p g      = g { _eFieldSetGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasStyle EFieldSet
  where attrSetStyle p g           = g { _eFieldSetGlobals = Just (attrSetStyle p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasTabIndex EFieldSet
  where attrSetTabIndex p g        = g { _eFieldSetGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasTitle EFieldSet
  where attrSetTitle p g           = g { _eFieldSetGlobals = Just (attrSetTitle p (fromMaybe gDef (_eFieldSetGlobals g))) }
instance AttrHasTranslate EFieldSet
  where attrSetTranslate p g       = g { _eFieldSetGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eFieldSetGlobals g))) }

instance AttrGetClassName EFieldSet where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eFieldSetGlobals g)

instance AttrHasDisabled EFieldSet where attrSetDisabled p g = g {_eFieldSetDisabled = Just p }
instance AttrHasForm EFieldSet where attrSetForm p g = g {_eFieldSetForm = Just p }
instance AttrHasName EFieldSet where attrSetName p g = g {_eFieldSetName = Just p }

instance AttrHasCustom EFieldSet where attrSetCustom p g       = g { _eFieldSetCustom = Just p }

eFieldSet' :: forall t m a. DomBuilder t m => EFieldSet -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFieldSet' b  = elAttr' "fieldset" (attrMap b)

eFieldSet :: forall t m a. DomBuilder t m => EFieldSet -> m a -> m a
eFieldSet b children = snd <$> eFieldSet' b children

eFieldSetN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFieldSetN' = el' "fieldset"

eFieldSetN :: forall t m a. DomBuilder t m => m a -> m a
eFieldSetN children = snd <$> eFieldSetN' children

eFieldSetD' :: forall t m a. MonadWidget t m => Dynamic t EFieldSet -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFieldSetD' b = elDynAttr' "fieldset" (attrMap <$> b)

eFieldSetD :: forall t m a. MonadWidget t m => Dynamic t EFieldSet -> m a -> m a
eFieldSetD b children = snd <$> eFieldSetD' b children




------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | FigCaption-element has only the global attributes.
data EFigCaption = EFigCaption
  { _eFigCaptionGlobals :: Maybe Globals
  , _eFigCaptionCustom  :: Maybe Attr
  }


instance AttrMap EFigCaption where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eFigCaptionGlobals b
    ] <> maybe [] (\a -> [a]) (_eFigCaptionCustom b)


instance Default EFigCaption where
  def = EFigCaption def def

instance Monoid EFigCaption where
  mempty = def
  mappend (EFigCaption a1 a2) (EFigCaption b1 b2)
    = EFigCaption (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EFigCaption where
   attrSetGlobals p b = b { _eFigCaptionGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EFigCaption
  where attrSetAccessKey p g = g { _eFigCaptionGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasAnmval EFigCaption
  where attrSetAnmval p g = g { _eFigCaptionGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasContentEditable EFigCaption
  where attrSetContentEditable p g = g  { _eFigCaptionGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasContextMenu EFigCaption
  where attrSetContextMenu p g     = g { _eFigCaptionGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasClass EFigCaption
  where attrSetClassName p g           = g { _eFigCaptionGlobals = Just (attrSetClassName p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasDnmval EFigCaption
  where attrSetDnmval p g           = g { _eFigCaptionGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasDir EFigCaption
  where attrSetDir p g             = g { _eFigCaptionGlobals = Just (attrSetDir p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasDraggable EFigCaption
  where attrSetDraggable p g       = g { _eFigCaptionGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasHidden EFigCaption
  where attrSetHidden p g          = g { _eFigCaptionGlobals = Just (attrSetHidden p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasId EFigCaption
  where attrSetId p g              = g { _eFigCaptionGlobals = Just (attrSetId p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasLang EFigCaption
  where attrSetLang p g            = g { _eFigCaptionGlobals = Just (attrSetLang p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasRole EFigCaption
  where attrSetRole p g            = g { _eFigCaptionGlobals = Just (attrSetRole p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasSlot EFigCaption
  where attrSetSlot p g            = g { _eFigCaptionGlobals = Just (attrSetSlot p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasSpellCheck EFigCaption
  where attrSetSpellCheck p g      = g { _eFigCaptionGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasStyle EFigCaption
  where attrSetStyle p g           = g { _eFigCaptionGlobals = Just (attrSetStyle p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasTabIndex EFigCaption
  where attrSetTabIndex p g        = g { _eFigCaptionGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasTitle EFigCaption
  where attrSetTitle p g           = g { _eFigCaptionGlobals = Just (attrSetTitle p (fromMaybe gDef (_eFigCaptionGlobals g))) }
instance AttrHasTranslate EFigCaption
  where attrSetTranslate p g       = g { _eFigCaptionGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eFigCaptionGlobals g))) }

instance AttrGetClassName EFigCaption where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eFigCaptionGlobals g)

instance AttrHasCustom EFigCaption where attrSetCustom p g       = g { _eFigCaptionCustom = Just p }

eFigCaption' :: forall t m a. DomBuilder t m => EFigCaption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigCaption' b  = elAttr' "figcaption" (attrMap b)

eFigCaption :: forall t m a. DomBuilder t m => EFigCaption -> m a -> m a
eFigCaption b children = snd <$> eFigCaption' b children

eFigCaptionN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigCaptionN' = el' "figcaption"

eFigCaptionN :: forall t m a. DomBuilder t m => m a -> m a
eFigCaptionN children = snd <$> eFigCaptionN' children

eFigCaptionD' :: forall t m a. MonadWidget t m => Dynamic t EFigCaption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigCaptionD' b  = elDynAttr' "figcaption" (attrMap <$> b)

eFigCaptionD :: forall t m a. MonadWidget t m => Dynamic t EFigCaption -> m a -> m a
eFigCaptionD b children = snd <$> eFigCaptionD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Figure-element has only the global attributes.
data EFigure = EFigure
  { _eFigureGlobals :: Maybe Globals
  , _eFigureCustom  :: Maybe Attr
  }


instance AttrMap EFigure where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eFigureGlobals b
    ] <> maybe [] (\a -> [a]) (_eFigureCustom b)


instance Default EFigure where
  def = EFigure def def

instance Monoid EFigure where
  mempty = def
  mappend (EFigure a1 a2) (EFigure b1 b2) = EFigure (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EFigure where
   attrSetGlobals p b = b { _eFigureGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EFigure
  where attrSetAccessKey p g = g { _eFigureGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasAnmval EFigure
  where attrSetAnmval p g = g { _eFigureGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasContentEditable EFigure
  where attrSetContentEditable p g = g  { _eFigureGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasContextMenu EFigure
  where attrSetContextMenu p g     = g { _eFigureGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasClass EFigure
  where attrSetClassName p g           = g { _eFigureGlobals = Just (attrSetClassName p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasDnmval EFigure
  where attrSetDnmval p g           = g { _eFigureGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasDir EFigure
  where attrSetDir p g             = g { _eFigureGlobals = Just (attrSetDir p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasDraggable EFigure
  where attrSetDraggable p g       = g { _eFigureGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasHidden EFigure
  where attrSetHidden p g          = g { _eFigureGlobals = Just (attrSetHidden p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasId EFigure
  where attrSetId p g              = g { _eFigureGlobals = Just (attrSetId p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasLang EFigure
  where attrSetLang p g            = g { _eFigureGlobals = Just (attrSetLang p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasRole EFigure
  where attrSetRole p g            = g { _eFigureGlobals = Just (attrSetRole p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasSlot EFigure
  where attrSetSlot p g            = g { _eFigureGlobals = Just (attrSetSlot p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasSpellCheck EFigure
  where attrSetSpellCheck p g      = g { _eFigureGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasStyle EFigure
  where attrSetStyle p g           = g { _eFigureGlobals = Just (attrSetStyle p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasTabIndex EFigure
  where attrSetTabIndex p g        = g { _eFigureGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasTitle EFigure
  where attrSetTitle p g           = g { _eFigureGlobals = Just (attrSetTitle p (fromMaybe gDef (_eFigureGlobals g))) }
instance AttrHasTranslate EFigure
  where attrSetTranslate p g       = g { _eFigureGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eFigureGlobals g))) }

instance AttrGetClassName EFigure where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eFigureGlobals g)

instance AttrHasCustom EFigure where attrSetCustom p g       = g { _eFigureCustom = Just p }

eFigure' :: forall t m a. DomBuilder t m => EFigure -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigure' b  = elAttr' "figure" (attrMap b)

eFigure :: forall t m a. DomBuilder t m => EFigure -> m a -> m a
eFigure b children = snd <$> eFigure' b children

eFigureN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigureN' = el' "figure"

eFigureN :: forall t m a. DomBuilder t m => m a -> m a
eFigureN children = snd <$> eFigureN' children

eFigureD' :: forall t m a. MonadWidget t m => Dynamic t EFigure -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFigureD' b  = elDynAttr' "figure" (attrMap <$> b)

eFigureD :: forall t m a. MonadWidget t m => Dynamic t EFigure -> m a -> m a
eFigureD b children = snd <$> eFigureD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Footer-element has only the global attributes.
data EFooter = EFooter
  { _eFooterGlobals :: Maybe Globals
  , _eFooterCustom  :: Maybe Attr
  }


instance AttrMap EFooter where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eFooterGlobals b
    ] <> maybe [] (\a -> [a]) (_eFooterCustom b)


instance Default EFooter where
  def = EFooter def def

instance Monoid EFooter where
  mempty = def
  mappend (EFooter a1 a2) (EFooter b1 b2) = EFooter (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EFooter where
   attrSetGlobals p b = b { _eFooterGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EFooter
  where attrSetAccessKey p g = g { _eFooterGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasAnmval EFooter
  where attrSetAnmval p g = g { _eFooterGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasContentEditable EFooter
  where attrSetContentEditable p g = g  { _eFooterGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasContextMenu EFooter
  where attrSetContextMenu p g     = g { _eFooterGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasClass EFooter
  where attrSetClassName p g           = g { _eFooterGlobals = Just (attrSetClassName p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasDnmval EFooter
  where attrSetDnmval p g           = g { _eFooterGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasDir EFooter
  where attrSetDir p g             = g { _eFooterGlobals = Just (attrSetDir p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasDraggable EFooter
  where attrSetDraggable p g       = g { _eFooterGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasHidden EFooter
  where attrSetHidden p g          = g { _eFooterGlobals = Just (attrSetHidden p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasId EFooter
  where attrSetId p g              = g { _eFooterGlobals = Just (attrSetId p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasLang EFooter
  where attrSetLang p g            = g { _eFooterGlobals = Just (attrSetLang p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasRole EFooter
  where attrSetRole p g            = g { _eFooterGlobals = Just (attrSetRole p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasSlot EFooter
  where attrSetSlot p g            = g { _eFooterGlobals = Just (attrSetSlot p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasSpellCheck EFooter
  where attrSetSpellCheck p g      = g { _eFooterGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasStyle EFooter
  where attrSetStyle p g           = g { _eFooterGlobals = Just (attrSetStyle p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasTabIndex EFooter
  where attrSetTabIndex p g        = g { _eFooterGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasTitle EFooter
  where attrSetTitle p g           = g { _eFooterGlobals = Just (attrSetTitle p (fromMaybe gDef (_eFooterGlobals g))) }
instance AttrHasTranslate EFooter
  where attrSetTranslate p g       = g { _eFooterGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eFooterGlobals g))) }

instance AttrGetClassName EFooter where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eFooterGlobals g)

instance AttrHasCustom EFooter where attrSetCustom p g       = g { _eFooterCustom = Just p }

eFooter' :: forall t m a. DomBuilder t m => EFooter -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFooter' b  = elAttr' "footer" (attrMap b)

eFooter :: forall t m a. DomBuilder t m => EFooter -> m a -> m a
eFooter b children = snd <$> eFooter' b children

eFooterN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFooterN' = el' "footer"

eFooterN :: forall t m a. DomBuilder t m => m a -> m a
eFooterN children = snd <$> eFooterN' children

eFooterD' :: forall t m a. MonadWidget t m => Dynamic t EFooter -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFooterD' b  = elDynAttr' "footer" (attrMap <$> b)

eFooterD :: forall t m a. MonadWidget t m => Dynamic t EFooter -> m a -> m a
eFooterD b children = snd <$> eFooterD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Form-element
data EForm = EForm
  { _eFormGlobals       :: Maybe Globals
  , _eFormAcceptCharSet :: Maybe AcceptCharSet
  , _eFormAction        :: Maybe Action
  , _eFormAutoComplete  :: Maybe AutoComplete
  , _eFormEncType       :: Maybe EncType
  , _eFormMethod        :: Maybe Method
  , _eFormName          :: Maybe Name
  , _eFormNoValidate    :: Maybe NoValidate
  , _eFormTarget        :: Maybe Target
  , _eFormCustom        :: Maybe Attr
  }


instance AttrMap EForm where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eFormGlobals b
    , attrMap <$> _eFormAcceptCharSet b
    , attrMap <$> _eFormAction b
    , attrMap <$> _eFormAutoComplete b
    , attrMap <$> _eFormEncType b
    , attrMap <$> _eFormMethod b
    , attrMap <$> _eFormName b
    , attrMap <$> _eFormNoValidate b
    , attrMap <$> _eFormTarget b
    ] <> maybe [] (\a -> [a]) (_eFormCustom b)


instance Default EForm where
  def = EForm def def def def def def def def def def

instance Monoid EForm where
  mempty = def
  mappend (EForm a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    (EForm b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
      = EForm (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
              (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)

instance AttrHasGlobals EForm where
   attrSetGlobals p b = b { _eFormGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EForm
  where attrSetAccessKey p g = g { _eFormGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasAnmval EForm
  where attrSetAnmval p g = g { _eFormGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasContentEditable EForm
  where attrSetContentEditable p g = g  { _eFormGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasContextMenu EForm
  where attrSetContextMenu p g     = g { _eFormGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasClass EForm
  where attrSetClassName p g           = g { _eFormGlobals = Just (attrSetClassName p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasDnmval EForm
  where attrSetDnmval p g           = g { _eFormGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasDir EForm
  where attrSetDir p g             = g { _eFormGlobals = Just (attrSetDir p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasDraggable EForm
  where attrSetDraggable p g       = g { _eFormGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasHidden EForm
  where attrSetHidden p g          = g { _eFormGlobals = Just (attrSetHidden p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasId EForm
  where attrSetId p g              = g { _eFormGlobals = Just (attrSetId p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasLang EForm
  where attrSetLang p g            = g { _eFormGlobals = Just (attrSetLang p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasRole EForm
  where attrSetRole p g            = g { _eFormGlobals = Just (attrSetRole p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasSlot EForm
  where attrSetSlot p g            = g { _eFormGlobals = Just (attrSetSlot p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasSpellCheck EForm
  where attrSetSpellCheck p g      = g { _eFormGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasStyle EForm
  where attrSetStyle p g           = g { _eFormGlobals = Just (attrSetStyle p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasTabIndex EForm
  where attrSetTabIndex p g        = g { _eFormGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasTitle EForm
  where attrSetTitle p g           = g { _eFormGlobals = Just (attrSetTitle p (fromMaybe gDef (_eFormGlobals g))) }
instance AttrHasTranslate EForm
  where attrSetTranslate p g       = g { _eFormGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eFormGlobals g))) }

instance AttrGetClassName EForm where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eFormGlobals g)

instance AttrHasAcceptCharSet EForm where attrSetAcceptCharSet p g = g {_eFormAcceptCharSet = Just p }
instance AttrHasAction EForm where attrSetAction p g = g {_eFormAction = Just p }
instance AttrHasAutoComplete EForm where attrSetAutoComplete p g = g {_eFormAutoComplete = Just p }
instance AttrHasEncType EForm where attrSetEncType p g = g {_eFormEncType = Just p }
instance AttrHasMethod EForm where attrSetMethod p g = g {_eFormMethod = Just p }
instance AttrHasName EForm where attrSetName p g = g {_eFormName = Just p }
instance AttrHasNoValidate EForm where attrSetNoValidate p g = g {_eFormNoValidate = Just p }
instance AttrHasTarget EForm where attrSetTarget p g = g {_eFormTarget = Just p }

instance AttrHasCustom EForm where attrSetCustom p g       = g { _eFormCustom = Just p }

eForm' :: forall t m a. DomBuilder t m => EForm -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eForm' b  = elAttr' "form" (attrMap b)

eForm :: forall t m a. DomBuilder t m => EForm -> m a -> m a
eForm b children = snd <$> eForm' b children

eFormN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFormN' = el' "form"

eFormN :: forall t m a. DomBuilder t m => m a -> m a
eFormN children = snd <$> eFormN' children

eFormD' :: forall t m a. MonadWidget t m => Dynamic t EForm -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eFormD' b = elDynAttr' "form" (attrMap <$> b)

eFormD :: forall t m a. MonadWidget t m => Dynamic t EForm -> m a -> m a
eFormD b children = snd <$> eFormD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Header-element has only the global attributes.
data EHeader = EHeader
  { _eHeaderGlobals :: Maybe Globals
  , _eHeaderCustom  :: Maybe Attr
  }


instance AttrMap EHeader where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eHeaderGlobals b
    ] <> maybe [] (\a -> [a]) (_eHeaderCustom b)


instance Default EHeader where
  def = EHeader def def

instance Monoid EHeader where
  mempty = def
  mappend (EHeader a1 a2) (EHeader b1 b2) = EHeader (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EHeader where
   attrSetGlobals p b = b { _eHeaderGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EHeader
  where attrSetAccessKey p g = g { _eHeaderGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasAnmval EHeader
  where attrSetAnmval p g = g { _eHeaderGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasContentEditable EHeader
  where attrSetContentEditable p g = g  { _eHeaderGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasContextMenu EHeader
  where attrSetContextMenu p g     = g { _eHeaderGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasClass EHeader
  where attrSetClassName p g           = g { _eHeaderGlobals = Just (attrSetClassName p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasDnmval EHeader
  where attrSetDnmval p g           = g { _eHeaderGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasDir EHeader
  where attrSetDir p g             = g { _eHeaderGlobals = Just (attrSetDir p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasDraggable EHeader
  where attrSetDraggable p g       = g { _eHeaderGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasHidden EHeader
  where attrSetHidden p g          = g { _eHeaderGlobals = Just (attrSetHidden p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasId EHeader
  where attrSetId p g              = g { _eHeaderGlobals = Just (attrSetId p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasLang EHeader
  where attrSetLang p g            = g { _eHeaderGlobals = Just (attrSetLang p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasRole EHeader
  where attrSetRole p g            = g { _eHeaderGlobals = Just (attrSetRole p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasSlot EHeader
  where attrSetSlot p g            = g { _eHeaderGlobals = Just (attrSetSlot p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasSpellCheck EHeader
  where attrSetSpellCheck p g      = g { _eHeaderGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasStyle EHeader
  where attrSetStyle p g           = g { _eHeaderGlobals = Just (attrSetStyle p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasTabIndex EHeader
  where attrSetTabIndex p g        = g { _eHeaderGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasTitle EHeader
  where attrSetTitle p g           = g { _eHeaderGlobals = Just (attrSetTitle p (fromMaybe gDef (_eHeaderGlobals g))) }
instance AttrHasTranslate EHeader
  where attrSetTranslate p g       = g { _eHeaderGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eHeaderGlobals g))) }

instance AttrGetClassName EHeader where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eHeaderGlobals g)

instance AttrHasCustom EHeader where attrSetCustom p g       = g { _eHeaderCustom = Just p }

eHeader' :: forall t m a. DomBuilder t m => EHeader -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHeader' b  = elAttr' "header" (attrMap b)

eHeader :: forall t m a. DomBuilder t m => EHeader -> m a -> m a
eHeader b children = snd <$> eHeader' b children

eHeaderN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHeaderN' = el' "header"

eHeaderN :: forall t m a. DomBuilder t m => m a -> m a
eHeaderN children = snd <$> eHeaderN' children

eHeaderD' :: forall t m a. MonadWidget t m => Dynamic t EHeader -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHeaderD' b  = elDynAttr' "header" (attrMap <$> b)

eHeaderD :: forall t m a. MonadWidget t m => Dynamic t EHeader -> m a -> m a
eHeaderD b children = snd <$> eHeaderD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Hr-element has only the global attributes.
data EHr = EHr
  { _eHrGlobals :: Maybe Globals
  , _eHrCustom  :: Maybe Attr
  }


instance AttrMap EHr where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eHrGlobals b
    ] <> maybe [] (\a -> [a]) (_eHrCustom b)


instance Default EHr where
  def = EHr def def

instance Monoid EHr where
  mempty = def
  mappend (EHr a1 a2) (EHr b1 b2) = EHr (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EHr where
   attrSetGlobals p b = b { _eHrGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EHr
  where attrSetAccessKey p g = g { _eHrGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasAnmval EHr
  where attrSetAnmval p g = g { _eHrGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasContentEditable EHr
  where attrSetContentEditable p g = g  { _eHrGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasContextMenu EHr
  where attrSetContextMenu p g     = g { _eHrGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasClass EHr
  where attrSetClassName p g           = g { _eHrGlobals = Just (attrSetClassName p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasDnmval EHr
  where attrSetDnmval p g           = g { _eHrGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasDir EHr
  where attrSetDir p g             = g { _eHrGlobals = Just (attrSetDir p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasDraggable EHr
  where attrSetDraggable p g       = g { _eHrGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasHidden EHr
  where attrSetHidden p g          = g { _eHrGlobals = Just (attrSetHidden p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasId EHr
  where attrSetId p g              = g { _eHrGlobals = Just (attrSetId p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasLang EHr
  where attrSetLang p g            = g { _eHrGlobals = Just (attrSetLang p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasRole EHr
  where attrSetRole p g            = g { _eHrGlobals = Just (attrSetRole p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasSlot EHr
  where attrSetSlot p g            = g { _eHrGlobals = Just (attrSetSlot p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasSpellCheck EHr
  where attrSetSpellCheck p g      = g { _eHrGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasStyle EHr
  where attrSetStyle p g           = g { _eHrGlobals = Just (attrSetStyle p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasTabIndex EHr
  where attrSetTabIndex p g        = g { _eHrGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasTitle EHr
  where attrSetTitle p g           = g { _eHrGlobals = Just (attrSetTitle p (fromMaybe gDef (_eHrGlobals g))) }
instance AttrHasTranslate EHr
  where attrSetTranslate p g       = g { _eHrGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eHrGlobals g))) }

instance AttrGetClassName EHr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eHrGlobals g)

instance AttrHasCustom EHr where attrSetCustom p g       = g { _eHrCustom = Just p }

eHr' :: forall t m a. DomBuilder t m => EHr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHr' b  = elAttr' "hr" (attrMap b)

eHr :: forall t m a. DomBuilder t m => EHr -> m a -> m a
eHr b children = snd <$> eHr' b children

eHrN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHrN' = el' "hr"

eHrN :: forall t m a. DomBuilder t m => m a -> m a
eHrN children = snd <$> eHrN' children

eHrD' :: forall t m a. MonadWidget t m => Dynamic t EHr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHrD' b  = elDynAttr' "hr" (attrMap <$> b)

eHrD :: forall t m a. MonadWidget t m => Dynamic t EHr -> m a -> m a
eHrD b children = snd <$> eHrD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | I-element has only the global attributes.
data EI = EI
  { _eIGlobals :: Maybe Globals
  , _eICustom  :: Maybe Attr
  }


instance AttrMap EI where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eIGlobals b
    ] <> maybe [] (\a -> [a]) (_eICustom b)


instance Default EI where
  def = EI def def

instance Monoid EI where
  mempty = def
  mappend (EI a1 a2) (EI b1 b2) = EI (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EI where
   attrSetGlobals p b = b { _eIGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EI
  where attrSetAccessKey p g = g { _eIGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasAnmval EI
  where attrSetAnmval p g = g { _eIGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasContentEditable EI
  where attrSetContentEditable p g = g  { _eIGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasContextMenu EI
  where attrSetContextMenu p g     = g { _eIGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasClass EI
  where attrSetClassName p g           = g { _eIGlobals = Just (attrSetClassName p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasDnmval EI
  where attrSetDnmval p g           = g { _eIGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasDir EI
  where attrSetDir p g             = g { _eIGlobals = Just (attrSetDir p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasDraggable EI
  where attrSetDraggable p g       = g { _eIGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasHidden EI
  where attrSetHidden p g          = g { _eIGlobals = Just (attrSetHidden p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasId EI
  where attrSetId p g              = g { _eIGlobals = Just (attrSetId p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasLang EI
  where attrSetLang p g            = g { _eIGlobals = Just (attrSetLang p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasRole EI
  where attrSetRole p g            = g { _eIGlobals = Just (attrSetRole p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasSlot EI
  where attrSetSlot p g            = g { _eIGlobals = Just (attrSetSlot p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasSpellCheck EI
  where attrSetSpellCheck p g      = g { _eIGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasStyle EI
  where attrSetStyle p g           = g { _eIGlobals = Just (attrSetStyle p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasTabIndex EI
  where attrSetTabIndex p g        = g { _eIGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasTitle EI
  where attrSetTitle p g           = g { _eIGlobals = Just (attrSetTitle p (fromMaybe gDef (_eIGlobals g))) }
instance AttrHasTranslate EI
  where attrSetTranslate p g       = g { _eIGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eIGlobals g))) }

instance AttrGetClassName EI where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eIGlobals g)

instance AttrHasCustom EI where attrSetCustom p g       = g { _eICustom = Just p }

eI' :: forall t m a. DomBuilder t m => EI -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eI' b  = elAttr' "i" (attrMap b)

eI :: forall t m a. DomBuilder t m => EI -> m a -> m a
eI b children = snd <$> eI' b children

eIN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eIN' = el' "i"

eIN :: forall t m a. DomBuilder t m => m a -> m a
eIN children = snd <$> eIN' children

eID' :: forall t m a. MonadWidget t m => Dynamic t EI -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eID' b  = elDynAttr' "i" (attrMap <$> b)

eID :: forall t m a. MonadWidget t m => Dynamic t EI -> m a -> m a
eID b children = snd <$> eID' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | details-element
data EIns = EIns
  { _eInsGlobals  :: Maybe Globals
  , _eInsCite     :: Maybe Cite
  , _eInsDateTime :: Maybe DateTime
  , _eInsCustom   :: Maybe Attr
  }


instance AttrMap EIns where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eInsGlobals b
    , attrMap <$> _eInsCite b
    , attrMap <$> _eInsDateTime b
    ] <> maybe [] (\a -> [a]) (_eInsCustom b)


instance Default EIns where
  def = EIns def def def def

instance Monoid EIns where
  mempty = def
  mappend (EIns a1 a2 a3 a4) (EIns b1 b2 b3 b4)
    = EIns (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals EIns where
   attrSetGlobals p b = b { _eInsGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EIns
  where attrSetAccessKey p g = g { _eInsGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasAnmval EIns
  where attrSetAnmval p g = g { _eInsGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasContentEditable EIns
  where attrSetContentEditable p g = g  { _eInsGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasContextMenu EIns
  where attrSetContextMenu p g     = g { _eInsGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasClass EIns
  where attrSetClassName p g           = g { _eInsGlobals = Just (attrSetClassName p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasDnmval EIns
  where attrSetDnmval p g           = g { _eInsGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasDir EIns
  where attrSetDir p g             = g { _eInsGlobals = Just (attrSetDir p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasDraggable EIns
  where attrSetDraggable p g       = g { _eInsGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasHidden EIns
  where attrSetHidden p g          = g { _eInsGlobals = Just (attrSetHidden p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasId EIns
  where attrSetId p g              = g { _eInsGlobals = Just (attrSetId p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasLang EIns
  where attrSetLang p g            = g { _eInsGlobals = Just (attrSetLang p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasRole EIns
  where attrSetRole p g            = g { _eInsGlobals = Just (attrSetRole p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasSlot EIns
  where attrSetSlot p g            = g { _eInsGlobals = Just (attrSetSlot p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasSpellCheck EIns
  where attrSetSpellCheck p g      = g { _eInsGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasStyle EIns
  where attrSetStyle p g           = g { _eInsGlobals = Just (attrSetStyle p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasTabIndex EIns
  where attrSetTabIndex p g        = g { _eInsGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasTitle EIns
  where attrSetTitle p g           = g { _eInsGlobals = Just (attrSetTitle p (fromMaybe gDef (_eInsGlobals g))) }
instance AttrHasTranslate EIns
  where attrSetTranslate p g       = g { _eInsGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eInsGlobals g))) }

instance AttrGetClassName EIns where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eInsGlobals g)

instance AttrHasCite EIns where attrSetCite p g = g {_eInsCite = Just p }
instance AttrHasDateTime EIns where attrSetDateTime p g = g {_eInsDateTime = Just p }

instance AttrHasCustom EIns where attrSetCustom p g       = g { _eInsCustom = Just p }

eIns' :: forall t m a. DomBuilder t m => EIns -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eIns' b  = elAttr' "ins" (attrMap b)

eIns :: forall t m a. DomBuilder t m => EIns -> m a -> m a
eIns b children = snd <$> eIns' b children

eInsN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eInsN' = el' "ins"

eInsN :: forall t m a. DomBuilder t m => m a -> m a
eInsN children = snd <$> eInsN' children

eInsD' :: forall t m a. MonadWidget t m => Dynamic t EIns -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eInsD' b = elDynAttr' "ins" (attrMap <$> b)

eInsD :: forall t m a. MonadWidget t m => Dynamic t EIns -> m a -> m a
eInsD b children = snd <$> eInsD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Kbd-element has only the global attributes.
data EKbd = EKbd
  { _eKbdGlobals :: Maybe Globals
  , _eKbdCustom  :: Maybe Attr
  }

instance AttrMap EKbd where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eKbdGlobals b
    ] <> maybe [] (\a -> [a]) (_eKbdCustom b)


instance Default EKbd where
  def = EKbd def def

instance Monoid EKbd where
  mempty = def
  mappend (EKbd a1 a2) (EKbd b1 b2) = EKbd (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EKbd where
   attrSetGlobals p b = b { _eKbdGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EKbd
  where attrSetAccessKey p g = g { _eKbdGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasAnmval EKbd
  where attrSetAnmval p g = g { _eKbdGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasContentEditable EKbd
  where attrSetContentEditable p g = g  { _eKbdGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasContextMenu EKbd
  where attrSetContextMenu p g     = g { _eKbdGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasClass EKbd
  where attrSetClassName p g           = g { _eKbdGlobals = Just (attrSetClassName p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasDnmval EKbd
  where attrSetDnmval p g           = g { _eKbdGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasDir EKbd
  where attrSetDir p g             = g { _eKbdGlobals = Just (attrSetDir p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasDraggable EKbd
  where attrSetDraggable p g       = g { _eKbdGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasHidden EKbd
  where attrSetHidden p g          = g { _eKbdGlobals = Just (attrSetHidden p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasId EKbd
  where attrSetId p g              = g { _eKbdGlobals = Just (attrSetId p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasLang EKbd
  where attrSetLang p g            = g { _eKbdGlobals = Just (attrSetLang p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasRole EKbd
  where attrSetRole p g            = g { _eKbdGlobals = Just (attrSetRole p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasSlot EKbd
  where attrSetSlot p g            = g { _eKbdGlobals = Just (attrSetSlot p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasSpellCheck EKbd
  where attrSetSpellCheck p g      = g { _eKbdGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasStyle EKbd
  where attrSetStyle p g           = g { _eKbdGlobals = Just (attrSetStyle p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasTabIndex EKbd
  where attrSetTabIndex p g        = g { _eKbdGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasTitle EKbd
  where attrSetTitle p g           = g { _eKbdGlobals = Just (attrSetTitle p (fromMaybe gDef (_eKbdGlobals g))) }
instance AttrHasTranslate EKbd
  where attrSetTranslate p g       = g { _eKbdGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eKbdGlobals g))) }

instance AttrGetClassName EKbd where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eKbdGlobals g)

instance AttrHasCustom EKbd where attrSetCustom p g       = g { _eKbdCustom = Just p }

eKbd' :: forall t m a. DomBuilder t m => EKbd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eKbd' b  = elAttr' "kbd" (attrMap b)

eKbd :: forall t m a. DomBuilder t m => EKbd -> m a -> m a
eKbd b children = snd <$> eKbd' b children

eKbdN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eKbdN' = el' "kbd"

eKbdN :: forall t m a. DomBuilder t m => m a -> m a
eKbdN children = snd <$> eKbdN' children

eKbdD' :: forall t m a. MonadWidget t m => Dynamic t EKbd -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eKbdD' b  = elDynAttr' "kbd" (attrMap <$> b)

eKbdD :: forall t m a. MonadWidget t m => Dynamic t EKbd -> m a -> m a
eKbdD b children = snd <$> eKbdD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Li-element
data ELi = ELi
  { _eLiGlobals   :: Maybe Globals
  , _eLiValueOlLi :: Maybe ValueOlLi
  , _eLiCustom    :: Maybe Attr
  }


instance AttrMap ELi where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eLiGlobals b
    , attrMap <$> _eLiValueOlLi b
    ] <> maybe [] (\a -> [a]) (_eLiCustom b)


instance Default ELi where
  def = ELi def def def

instance Monoid ELi where
  mempty = def
  mappend (ELi a1 a2 a3) (ELi b1 b2 b3) = ELi (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals ELi where
   attrSetGlobals p b = b { _eLiGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ELi
  where attrSetAccessKey p g = g { _eLiGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasAnmval ELi
  where attrSetAnmval p g = g { _eLiGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasContentEditable ELi
  where attrSetContentEditable p g = g  { _eLiGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasContextMenu ELi
  where attrSetContextMenu p g     = g { _eLiGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasClass ELi
  where attrSetClassName p g           = g { _eLiGlobals = Just (attrSetClassName p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasDnmval ELi
  where attrSetDnmval p g           = g { _eLiGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasDir ELi
  where attrSetDir p g             = g { _eLiGlobals = Just (attrSetDir p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasDraggable ELi
  where attrSetDraggable p g       = g { _eLiGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasHidden ELi
  where attrSetHidden p g          = g { _eLiGlobals = Just (attrSetHidden p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasId ELi
  where attrSetId p g              = g { _eLiGlobals = Just (attrSetId p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasLang ELi
  where attrSetLang p g            = g { _eLiGlobals = Just (attrSetLang p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasRole ELi
  where attrSetRole p g            = g { _eLiGlobals = Just (attrSetRole p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasSlot ELi
  where attrSetSlot p g            = g { _eLiGlobals = Just (attrSetSlot p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasSpellCheck ELi
  where attrSetSpellCheck p g      = g { _eLiGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasStyle ELi
  where attrSetStyle p g           = g { _eLiGlobals = Just (attrSetStyle p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasTabIndex ELi
  where attrSetTabIndex p g        = g { _eLiGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasTitle ELi
  where attrSetTitle p g           = g { _eLiGlobals = Just (attrSetTitle p (fromMaybe gDef (_eLiGlobals g))) }
instance AttrHasTranslate ELi
  where attrSetTranslate p g       = g { _eLiGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eLiGlobals g))) }

instance AttrGetClassName ELi where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eLiGlobals g)

instance AttrHasValueOlLi  ELi where attrSetValueOlLi p g  = g { _eLiValueOlLi  = Just p }

instance AttrHasCustom ELi where attrSetCustom p g       = g { _eLiCustom = Just p }

eLi' :: forall t m a. DomBuilder t m => ELi -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLi' b  = elAttr' "li" (attrMap b)

eLi :: forall t m a. DomBuilder t m => ELi -> m a -> m a
eLi b children = snd <$> eLi' b children

eLiN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLiN' = el' "li"

eLiN :: forall t m a. DomBuilder t m => m a -> m a
eLiN children = snd <$> eLiN' children

eLiD' :: forall t m a. MonadWidget t m => Dynamic t ELi -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLiD' b  = elDynAttr' "li" (attrMap <$> b)

eLiD :: forall t m a. MonadWidget t m => Dynamic t ELi -> m a -> m a
eLiD b children = snd <$> eLiD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Legend-element has only the global attributes.
data ELegend = ELegend
  { _eLegendGlobals :: Maybe Globals
  , _eLegendCustom  :: Maybe Attr
  }


instance AttrMap ELegend where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eLegendGlobals b
    ] <> maybe [] (\a -> [a]) (_eLegendCustom b)


instance Default ELegend where
  def = ELegend def def

instance Monoid ELegend where
  mempty = def
  mappend (ELegend a1 a2) (ELegend b1 b2) = ELegend (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ELegend where
   attrSetGlobals p b = b { _eLegendGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ELegend
  where attrSetAccessKey p g = g { _eLegendGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasAnmval ELegend
  where attrSetAnmval p g = g { _eLegendGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasContentEditable ELegend
  where attrSetContentEditable p g = g  { _eLegendGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasContextMenu ELegend
  where attrSetContextMenu p g     = g { _eLegendGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasClass ELegend
  where attrSetClassName p g           = g { _eLegendGlobals = Just (attrSetClassName p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasDnmval ELegend
  where attrSetDnmval p g           = g { _eLegendGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasDir ELegend
  where attrSetDir p g             = g { _eLegendGlobals = Just (attrSetDir p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasDraggable ELegend
  where attrSetDraggable p g       = g { _eLegendGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasHidden ELegend
  where attrSetHidden p g          = g { _eLegendGlobals = Just (attrSetHidden p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasId ELegend
  where attrSetId p g              = g { _eLegendGlobals = Just (attrSetId p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasLang ELegend
  where attrSetLang p g            = g { _eLegendGlobals = Just (attrSetLang p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasRole ELegend
  where attrSetRole p g            = g { _eLegendGlobals = Just (attrSetRole p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasSlot ELegend
  where attrSetSlot p g            = g { _eLegendGlobals = Just (attrSetSlot p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasSpellCheck ELegend
  where attrSetSpellCheck p g      = g { _eLegendGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasStyle ELegend
  where attrSetStyle p g           = g { _eLegendGlobals = Just (attrSetStyle p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasTabIndex ELegend
  where attrSetTabIndex p g        = g { _eLegendGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasTitle ELegend
  where attrSetTitle p g           = g { _eLegendGlobals = Just (attrSetTitle p (fromMaybe gDef (_eLegendGlobals g))) }
instance AttrHasTranslate ELegend
  where attrSetTranslate p g       = g { _eLegendGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eLegendGlobals g))) }

instance AttrGetClassName ELegend where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eLegendGlobals g)

instance AttrHasCustom ELegend where attrSetCustom p g       = g { _eLegendCustom = Just p }

eLegend' :: forall t m a. DomBuilder t m => ELegend -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLegend' b  = elAttr' "legend" (attrMap b)

eLegend :: forall t m a. DomBuilder t m => ELegend -> m a -> m a
eLegend b children = snd <$> eLegend' b children

eLegendN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLegendN' = el' "legend"

eLegendN :: forall t m a. DomBuilder t m => m a -> m a
eLegendN children = snd <$> eLegendN' children

eLegendD' :: forall t m a. MonadWidget t m => Dynamic t ELegend -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLegendD' b  = elDynAttr' "legend" (attrMap <$> b)

eLegendD :: forall t m a. MonadWidget t m => Dynamic t ELegend -> m a -> m a
eLegendD b children = snd <$> eLegendD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Mark-element has only the global attributes.
data EMark = EMark
  { _eMarkGlobals :: Maybe Globals
  , _eMarkCustom  :: Maybe Attr
  }


instance AttrMap EMark where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMarkGlobals b
    ] <> maybe [] (\a -> [a]) (_eMarkCustom b)


instance Default EMark where
  def = EMark def def

instance Monoid EMark where
  mempty = def
  mappend (EMark a1 a2) (EMark b1 b2) = EMark (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EMark where
   attrSetGlobals p b = b { _eMarkGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMark
  where attrSetAccessKey p g = g { _eMarkGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasAnmval EMark
  where attrSetAnmval p g = g { _eMarkGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasContentEditable EMark
  where attrSetContentEditable p g = g  { _eMarkGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasContextMenu EMark
  where attrSetContextMenu p g     = g { _eMarkGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasClass EMark
  where attrSetClassName p g           = g { _eMarkGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasDnmval EMark
  where attrSetDnmval p g           = g { _eMarkGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasDir EMark
  where attrSetDir p g             = g { _eMarkGlobals = Just (attrSetDir p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasDraggable EMark
  where attrSetDraggable p g       = g { _eMarkGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasHidden EMark
  where attrSetHidden p g          = g { _eMarkGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasId EMark
  where attrSetId p g              = g { _eMarkGlobals = Just (attrSetId p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasLang EMark
  where attrSetLang p g            = g { _eMarkGlobals = Just (attrSetLang p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasRole EMark
  where attrSetRole p g            = g { _eMarkGlobals = Just (attrSetRole p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasSlot EMark
  where attrSetSlot p g            = g { _eMarkGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasSpellCheck EMark
  where attrSetSpellCheck p g      = g { _eMarkGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasStyle EMark
  where attrSetStyle p g           = g { _eMarkGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasTabIndex EMark
  where attrSetTabIndex p g        = g { _eMarkGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasTitle EMark
  where attrSetTitle p g           = g { _eMarkGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMarkGlobals g))) }
instance AttrHasTranslate EMark
  where attrSetTranslate p g       = g { _eMarkGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMarkGlobals g))) }

instance AttrGetClassName EMark where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMarkGlobals g)

instance AttrHasCustom EMark where attrSetCustom p g       = g { _eMarkCustom = Just p }

eMark' :: forall t m a. DomBuilder t m => EMark -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMark' b  = elAttr' "mark" (attrMap b)

eMark :: forall t m a. DomBuilder t m => EMark -> m a -> m a
eMark b children = snd <$> eMark' b children

eMarkN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMarkN' = el' "mark"

eMarkN :: forall t m a. DomBuilder t m => m a -> m a
eMarkN children = snd <$> eMarkN' children

eMarkD' :: forall t m a. MonadWidget t m => Dynamic t EMark -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMarkD' b  = elDynAttr' "mark" (attrMap <$> b)

eMarkD :: forall t m a. MonadWidget t m => Dynamic t EMark -> m a -> m a
eMarkD b children = snd <$> eMarkD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Menu-element
data EMenu = EMenu
  { _eMenuGlobals :: Maybe Globals
  , _eMenuLabel   :: Maybe Label
  , _eMenuType    :: Maybe MenuType
  , _eMenuCustom  :: Maybe Attr
  }


instance AttrMap EMenu where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMenuGlobals b
    , attrMap <$> _eMenuLabel b
    , attrMap <$> _eMenuType b
    ] <> maybe [] (\a -> [a]) (_eMenuCustom b)


instance Default EMenu where
  def = EMenu def def def def

instance Monoid EMenu where
  mempty = def
  mappend (EMenu a1 a2 a3 a4) (EMenu b1 b2 b3 b4)
    = EMenu (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals EMenu where
   attrSetGlobals p b = b { _eMenuGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMenu
  where attrSetAccessKey p g = g { _eMenuGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasAnmval EMenu
  where attrSetAnmval p g = g { _eMenuGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasContentEditable EMenu
  where attrSetContentEditable p g = g  { _eMenuGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasContextMenu EMenu
  where attrSetContextMenu p g     = g { _eMenuGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasClass EMenu
  where attrSetClassName p g           = g { _eMenuGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasDnmval EMenu
  where attrSetDnmval p g           = g { _eMenuGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasDir EMenu
  where attrSetDir p g             = g { _eMenuGlobals = Just (attrSetDir p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasDraggable EMenu
  where attrSetDraggable p g       = g { _eMenuGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasHidden EMenu
  where attrSetHidden p g          = g { _eMenuGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasId EMenu
  where attrSetId p g              = g { _eMenuGlobals = Just (attrSetId p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasLang EMenu
  where attrSetLang p g            = g { _eMenuGlobals = Just (attrSetLang p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasRole EMenu
  where attrSetRole p g            = g { _eMenuGlobals = Just (attrSetRole p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasSlot EMenu
  where attrSetSlot p g            = g { _eMenuGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasSpellCheck EMenu
  where attrSetSpellCheck p g      = g { _eMenuGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasStyle EMenu
  where attrSetStyle p g           = g { _eMenuGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasTabIndex EMenu
  where attrSetTabIndex p g        = g { _eMenuGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasTitle EMenu
  where attrSetTitle p g           = g { _eMenuGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMenuGlobals g))) }
instance AttrHasTranslate EMenu
  where attrSetTranslate p g       = g { _eMenuGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMenuGlobals g))) }

instance AttrGetClassName EMenu where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMenuGlobals g)

instance AttrHasMenuType EMenu where attrSetMenuType p g = g { _eMenuType = Just p }
instance AttrHasLabel  EMenu where attrSetLabel p g  = g { _eMenuLabel  = Just p }

instance AttrHasCustom EMenu where attrSetCustom p g       = g { _eMenuCustom = Just p }

eMenu' :: forall t m a. DomBuilder t m => EMenu -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenu' b  = elAttr' "menu" (attrMap b)

eMenu :: forall t m a. DomBuilder t m => EMenu -> m a -> m a
eMenu b children = snd <$> eMenu' b children

eMenuN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenuN' = el' "menu"

eMenuN :: forall t m a. DomBuilder t m => m a -> m a
eMenuN children = snd <$> eMenuN' children

eMenuD' :: forall t m a. MonadWidget t m => Dynamic t EMenu -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenuD' b  = elDynAttr' "menu" (attrMap <$> b)

eMenuD :: forall t m a. MonadWidget t m => Dynamic t EMenu -> m a -> m a
eMenuD b children = snd <$> eMenuD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | MenuItem-element
data EMenuItem = EMenuItem
  { _eMenuItemGlobals  :: Maybe Globals
  , _eMenuItemIcon     :: Maybe Icon
  , _eMenuItemDisabled :: Maybe Disabled
  , _eMenuItemDefault  :: Maybe Default_
  , _eMenuItemLabel    :: Maybe Label
  , _eMenuItemType     :: Maybe MenuItemType
  , _eMenuItemCustom   :: Maybe Attr
  }


instance AttrMap EMenuItem where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMenuItemGlobals b
    , attrMap <$> _eMenuItemIcon b
    , attrMap <$> _eMenuItemDisabled b
    , attrMap <$> _eMenuItemDefault b
    , attrMap <$> _eMenuItemLabel b
    , attrMap <$> _eMenuItemType b
    ] <> maybe [] (\a -> [a]) (_eMenuItemCustom b)


instance Default EMenuItem where
  def = EMenuItem def def def def def def def

instance Monoid EMenuItem where
  mempty = def
  mappend (EMenuItem a1 a2 a3 a4 a5 a6 a7) (EMenuItem b1 b2 b3 b4 b5 b6 b7)
    = EMenuItem (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

instance AttrHasGlobals EMenuItem where
   attrSetGlobals p b = b { _eMenuItemGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMenuItem
  where attrSetAccessKey p g = g { _eMenuItemGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasAnmval EMenuItem
  where attrSetAnmval p g = g { _eMenuItemGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasContentEditable EMenuItem
  where attrSetContentEditable p g = g  { _eMenuItemGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasContextMenu EMenuItem
  where attrSetContextMenu p g     = g { _eMenuItemGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasClass EMenuItem
  where attrSetClassName p g           = g { _eMenuItemGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasDnmval EMenuItem
  where attrSetDnmval p g           = g { _eMenuItemGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasDir EMenuItem
  where attrSetDir p g             = g { _eMenuItemGlobals = Just (attrSetDir p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasDraggable EMenuItem
  where attrSetDraggable p g       = g { _eMenuItemGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasHidden EMenuItem
  where attrSetHidden p g          = g { _eMenuItemGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasId EMenuItem
  where attrSetId p g              = g { _eMenuItemGlobals = Just (attrSetId p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasLang EMenuItem
  where attrSetLang p g            = g { _eMenuItemGlobals = Just (attrSetLang p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasRole EMenuItem
  where attrSetRole p g            = g { _eMenuItemGlobals = Just (attrSetRole p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasSlot EMenuItem
  where attrSetSlot p g            = g { _eMenuItemGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasSpellCheck EMenuItem
  where attrSetSpellCheck p g      = g { _eMenuItemGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasStyle EMenuItem
  where attrSetStyle p g           = g { _eMenuItemGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasTabIndex EMenuItem
  where attrSetTabIndex p g        = g { _eMenuItemGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasTitle EMenuItem
  where attrSetTitle p g           = g { _eMenuItemGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMenuItemGlobals g))) }
instance AttrHasTranslate EMenuItem
  where attrSetTranslate p g       = g { _eMenuItemGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMenuItemGlobals g))) }

instance AttrGetClassName EMenuItem where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMenuItemGlobals g)

instance AttrHasIcon  EMenuItem where attrSetIcon p g  = g { _eMenuItemIcon  = Just p }
instance AttrHasDisabled  EMenuItem where attrSetDisabled p g  = g { _eMenuItemDisabled  = Just p }
instance AttrHasDefault  EMenuItem where attrSetDefault p g  = g { _eMenuItemDefault  = Just p }
instance AttrHasLabel  EMenuItem where attrSetLabel p g  = g { _eMenuItemLabel  = Just p }
instance AttrHasMenuItemType EMenuItem where attrSetMenuItemType p g = g { _eMenuItemType = Just p }

instance AttrHasCustom EMenuItem where attrSetCustom p g       = g { _eMenuItemCustom = Just p }

eMenuItem' :: forall t m a. DomBuilder t m => EMenuItem -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenuItem' b  = elAttr' "menuitem" (attrMap b)

eMenuItem :: forall t m a. DomBuilder t m => EMenuItem -> m a -> m a
eMenuItem b children = snd <$> eMenuItem' b children

eMenuItemN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenuItemN' = el' "menuitem"

eMenuItemN :: forall t m a. DomBuilder t m => m a -> m a
eMenuItemN children = snd <$> eMenuItemN' children

eMenuItemD' :: forall t m a. MonadWidget t m => Dynamic t EMenuItem -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMenuItemD' b  = elDynAttr' "menuitem" (attrMap <$> b)

eMenuItemD :: forall t m a. MonadWidget t m => Dynamic t EMenuItem -> m a -> m a
eMenuItemD b children = snd <$> eMenuItemD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Meter-element
data EMeter = EMeter
  { _eMeterGlobals     :: Maybe Globals
  , _eMeterHigh        :: Maybe High
  , _eMeterMin         :: Maybe Min
  , _eMeterMax         :: Maybe Max
  , _eMeterLow         :: Maybe Low
  , _eMeterOptimum     :: Maybe Optimum
  , _eMeterValueNumber :: Maybe ValueNumber
  , _eMeterCustom      :: Maybe Attr
  }


instance AttrMap EMeter where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMeterGlobals b
    , attrMap <$> _eMeterHigh b
    , attrMap <$> _eMeterMin b
    , attrMap <$> _eMeterMax b
    , attrMap <$> _eMeterLow b
    , attrMap <$> _eMeterOptimum b
    , attrMap <$> _eMeterValueNumber b
    ] <> maybe [] (\a -> [a]) (_eMeterCustom b)

instance Default EMeter where
  def = EMeter def def def def def def def def

instance Monoid EMeter where
  mempty = def
  mappend (EMeter a1 a2 a3 a4 a5 a6 a7 a8) (EMeter b1 b2 b3 b4 b5 b6 b7 b8)
    = EMeter (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
             (a6 <> b6) (a7 <> b7) (a8 <> b8)

instance AttrHasGlobals EMeter where
   attrSetGlobals p b = b { _eMeterGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMeter
  where attrSetAccessKey p g = g { _eMeterGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasAnmval EMeter
  where attrSetAnmval p g = g { _eMeterGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasContentEditable EMeter
  where attrSetContentEditable p g = g  { _eMeterGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasContextMenu EMeter
  where attrSetContextMenu p g     = g { _eMeterGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasClass EMeter
  where attrSetClassName p g           = g { _eMeterGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasDnmval EMeter
  where attrSetDnmval p g           = g { _eMeterGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasDir EMeter
  where attrSetDir p g             = g { _eMeterGlobals = Just (attrSetDir p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasDraggable EMeter
  where attrSetDraggable p g       = g { _eMeterGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasHidden EMeter
  where attrSetHidden p g          = g { _eMeterGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasId EMeter
  where attrSetId p g              = g { _eMeterGlobals = Just (attrSetId p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasLang EMeter
  where attrSetLang p g            = g { _eMeterGlobals = Just (attrSetLang p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasRole EMeter
  where attrSetRole p g            = g { _eMeterGlobals = Just (attrSetRole p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasSlot EMeter
  where attrSetSlot p g            = g { _eMeterGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasSpellCheck EMeter
  where attrSetSpellCheck p g      = g { _eMeterGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasStyle EMeter
  where attrSetStyle p g           = g { _eMeterGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasTabIndex EMeter
  where attrSetTabIndex p g        = g { _eMeterGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasTitle EMeter
  where attrSetTitle p g           = g { _eMeterGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMeterGlobals g))) }
instance AttrHasTranslate EMeter
  where attrSetTranslate p g       = g { _eMeterGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMeterGlobals g))) }

instance AttrGetClassName EMeter where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMeterGlobals g)

instance AttrHasHigh EMeter where attrSetHigh p g = g {_eMeterHigh = Just p }
instance AttrHasMin EMeter where attrSetMin p g = g {_eMeterMin = Just p }
instance AttrHasMax EMeter where attrSetMax p g = g {_eMeterMax = Just p }
instance AttrHasLow EMeter where attrSetLow p g = g {_eMeterLow = Just p }
instance AttrHasOptimum EMeter where attrSetOptimum p g = g {_eMeterOptimum = Just p }
instance AttrHasValueNumber EMeter where attrSetValueNumber p g = g {_eMeterValueNumber = Just p }

instance AttrHasCustom EMeter where attrSetCustom p g       = g { _eMeterCustom = Just p }

eMeter' :: forall t m a. DomBuilder t m => EMeter -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMeter' b  = elAttr' "meter" (attrMap b)

eMeter :: forall t m a. DomBuilder t m => EMeter -> m a -> m a
eMeter b children = snd <$> eMeter' b children

eMeterN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMeterN' = el' "meter"

eMeterN :: forall t m a. DomBuilder t m => m a -> m a
eMeterN children = snd <$> eMeterN' children

eMeterD' :: forall t m a. MonadWidget t m => Dynamic t EMeter -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMeterD' b  = elDynAttr' "meter" (attrMap <$> b)

eMeterD :: forall t m a. MonadWidget t m => Dynamic t EMeter -> m a -> m a
eMeterD b children = snd <$> eMeterD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ol-element
data EOl = EOl
  { _eOlGlobals  :: Maybe Globals
  , _eOlReversed :: Maybe Reversed
  , _eOlStart    :: Maybe Start
  , _eOlType     :: Maybe OlType
  , _eOlCustom   :: Maybe Attr
  }


instance AttrMap EOl where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eOlGlobals b
    , attrMap <$> _eOlReversed b
    , attrMap <$> _eOlStart b
    , attrMap <$> _eOlType b
    ] <> maybe [] (\a -> [a]) (_eOlCustom b)


instance Default EOl where
  def = EOl def def def def def

instance Monoid EOl where
  mempty = def
  mappend (EOl a1 a2 a3 a4 a5) (EOl b1 b2 b3 b4 b5)
    = EOl (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance AttrHasGlobals EOl where
   attrSetGlobals p b = b { _eOlGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EOl
  where attrSetAccessKey p g = g { _eOlGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasAnmval EOl
  where attrSetAnmval p g = g { _eOlGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasContentEditable EOl
  where attrSetContentEditable p g = g  { _eOlGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasContextMenu EOl
  where attrSetContextMenu p g     = g { _eOlGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasClass EOl
  where attrSetClassName p g           = g { _eOlGlobals = Just (attrSetClassName p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasDnmval EOl
  where attrSetDnmval p g           = g { _eOlGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasDir EOl
  where attrSetDir p g             = g { _eOlGlobals = Just (attrSetDir p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasDraggable EOl
  where attrSetDraggable p g       = g { _eOlGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasHidden EOl
  where attrSetHidden p g          = g { _eOlGlobals = Just (attrSetHidden p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasId EOl
  where attrSetId p g              = g { _eOlGlobals = Just (attrSetId p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasLang EOl
  where attrSetLang p g            = g { _eOlGlobals = Just (attrSetLang p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasRole EOl
  where attrSetRole p g            = g { _eOlGlobals = Just (attrSetRole p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasSlot EOl
  where attrSetSlot p g            = g { _eOlGlobals = Just (attrSetSlot p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasSpellCheck EOl
  where attrSetSpellCheck p g      = g { _eOlGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasStyle EOl
  where attrSetStyle p g           = g { _eOlGlobals = Just (attrSetStyle p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasTabIndex EOl
  where attrSetTabIndex p g        = g { _eOlGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasTitle EOl
  where attrSetTitle p g           = g { _eOlGlobals = Just (attrSetTitle p (fromMaybe gDef (_eOlGlobals g))) }
instance AttrHasTranslate EOl
  where attrSetTranslate p g       = g { _eOlGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eOlGlobals g))) }

instance AttrGetClassName EOl where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eOlGlobals g)

instance AttrHasReversed EOl where attrSetReversed p g  = g { _eOlReversed  = Just p }

instance AttrHasStart    EOl where attrSetStart p g  = g { _eOlStart  = Just p }

instance AttrHasOlType   EOl where attrSetOlType p g  = g { _eOlType  = Just p }

instance AttrHasCustom EOl where attrSetCustom p g       = g { _eOlCustom = Just p }

eOl' :: forall t m a. DomBuilder t m => EOl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOl' b  = elAttr' "ol" (attrMap b)

eOl :: forall t m a. DomBuilder t m => EOl -> m a -> m a
eOl b children = snd <$> eOl' b children

eOlN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOlN' = el' "ol"

eOlN :: forall t m a. DomBuilder t m => m a -> m a
eOlN children = snd <$> eOlN' children

eOlD' :: forall t m a. MonadWidget t m => Dynamic t EOl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOlD' b  = elDynAttr' "ol" (attrMap <$> b)

eOlD :: forall t m a. MonadWidget t m => Dynamic t EOl -> m a -> m a
eOlD b children = snd <$> eOlD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | OptGroup-element
data EOptGroup = EOptGroup
  { _eOptGroupGlobals  :: Maybe Globals
  , _eOptGroupDisabled :: Maybe Disabled
  , _eOptGroupLabel    :: Maybe Label
  , _eOptGroupCustom   :: Maybe Attr
  }


instance AttrMap EOptGroup where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eOptGroupGlobals b
    , attrMap <$> _eOptGroupDisabled b
    , attrMap <$> _eOptGroupLabel b
    ] <> maybe [] (\a -> [a]) (_eOptGroupCustom b)


instance Default EOptGroup where
  def = EOptGroup def def def def

instance AttrHasGlobals EOptGroup where
   attrSetGlobals p b = b { _eOptGroupGlobals = Just p }

instance Monoid EOptGroup where
  mempty = def
  mappend (EOptGroup a1 a2 a3 a4) (EOptGroup b1 b2 b3 b4)
    = EOptGroup (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- Global attributes require the following instances.
instance AttrHasAccessKey EOptGroup
  where attrSetAccessKey p g = g { _eOptGroupGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasAnmval EOptGroup
  where attrSetAnmval p g = g { _eOptGroupGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasContentEditable EOptGroup
  where attrSetContentEditable p g = g  { _eOptGroupGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasContextMenu EOptGroup
  where attrSetContextMenu p g     = g { _eOptGroupGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasClass EOptGroup
  where attrSetClassName p g           = g { _eOptGroupGlobals = Just (attrSetClassName p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasDnmval EOptGroup
  where attrSetDnmval p g           = g { _eOptGroupGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasDir EOptGroup
  where attrSetDir p g             = g { _eOptGroupGlobals = Just (attrSetDir p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasDraggable EOptGroup
  where attrSetDraggable p g       = g { _eOptGroupGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasHidden EOptGroup
  where attrSetHidden p g          = g { _eOptGroupGlobals = Just (attrSetHidden p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasId EOptGroup
  where attrSetId p g              = g { _eOptGroupGlobals = Just (attrSetId p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasLang EOptGroup
  where attrSetLang p g            = g { _eOptGroupGlobals = Just (attrSetLang p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasRole EOptGroup
  where attrSetRole p g            = g { _eOptGroupGlobals = Just (attrSetRole p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasSlot EOptGroup
  where attrSetSlot p g            = g { _eOptGroupGlobals = Just (attrSetSlot p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasSpellCheck EOptGroup
  where attrSetSpellCheck p g      = g { _eOptGroupGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasStyle EOptGroup
  where attrSetStyle p g           = g { _eOptGroupGlobals = Just (attrSetStyle p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasTabIndex EOptGroup
  where attrSetTabIndex p g        = g { _eOptGroupGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasTitle EOptGroup
  where attrSetTitle p g           = g { _eOptGroupGlobals = Just (attrSetTitle p (fromMaybe gDef (_eOptGroupGlobals g))) }
instance AttrHasTranslate EOptGroup
  where attrSetTranslate p g       = g { _eOptGroupGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eOptGroupGlobals g))) }

instance AttrGetClassName EOptGroup where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eOptGroupGlobals g)

instance AttrHasDisabled EOptGroup where attrSetDisabled p g  = g { _eOptGroupDisabled  = Just p }

instance AttrHasLabel    EOptGroup where attrSetLabel p g  = g { _eOptGroupLabel  = Just p }

instance AttrHasCustom EOptGroup where attrSetCustom p g       = g { _eOptGroupCustom = Just p }

eOptGroup' :: forall t m a. DomBuilder t m => EOptGroup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOptGroup' b  = elAttr' "optgroup" (attrMap b)

eOptGroup :: forall t m a. DomBuilder t m => EOptGroup -> m a -> m a
eOptGroup b children = snd <$> eOptGroup' b children

eOptGroupN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOptGroupN' = el' "optgroup"

eOptGroupN :: forall t m a. DomBuilder t m => m a -> m a
eOptGroupN children = snd <$> eOptGroupN' children

eOptGroupD' :: forall t m a. MonadWidget t m => Dynamic t EOptGroup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOptGroupD' b  = elDynAttr' "optgroup" (attrMap <$> b)

eOptGroupD :: forall t m a. MonadWidget t m => Dynamic t EOptGroup -> m a -> m a
eOptGroupD b children = snd <$> eOptGroupD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Option-element
data EOption = EOption
  { _eOptionGlobals   :: Maybe Globals
  , _eOptionDisabled  :: Maybe Disabled
  , _eOptionLabel     :: Maybe Label
  , _eOptionSelected  :: Maybe Selected
  , _eOptionValueText :: Maybe ValueText
  , _eOptionCustom    :: Maybe Attr
  }


instance AttrMap EOption where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eOptionGlobals b
    , attrMap <$> _eOptionDisabled b
    , attrMap <$> _eOptionLabel b
    , attrMap <$> _eOptionSelected b
    , attrMap <$> _eOptionValueText b
    ] <> maybe [] (\a -> [a]) (_eOptionCustom b)


instance Default EOption where
  def = EOption def def def def def def

instance Monoid EOption where
  mempty = def
  mappend (EOption a1 a2 a3 a4 a5 a6) (EOption b1 b2 b3 b4 b5 b6)
    = EOption (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance AttrHasGlobals EOption where
   attrSetGlobals p b = b { _eOptionGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EOption
  where attrSetAccessKey p g = g { _eOptionGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasAnmval EOption
  where attrSetAnmval p g = g { _eOptionGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasContentEditable EOption
  where attrSetContentEditable p g = g  { _eOptionGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasContextMenu EOption
  where attrSetContextMenu p g     = g { _eOptionGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasClass EOption
  where attrSetClassName p g           = g { _eOptionGlobals = Just (attrSetClassName p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasDnmval EOption
  where attrSetDnmval p g           = g { _eOptionGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasDir EOption
  where attrSetDir p g             = g { _eOptionGlobals = Just (attrSetDir p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasDraggable EOption
  where attrSetDraggable p g       = g { _eOptionGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasHidden EOption
  where attrSetHidden p g          = g { _eOptionGlobals = Just (attrSetHidden p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasId EOption
  where attrSetId p g              = g { _eOptionGlobals = Just (attrSetId p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasLang EOption
  where attrSetLang p g            = g { _eOptionGlobals = Just (attrSetLang p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasRole EOption
  where attrSetRole p g            = g { _eOptionGlobals = Just (attrSetRole p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasSlot EOption
  where attrSetSlot p g            = g { _eOptionGlobals = Just (attrSetSlot p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasSpellCheck EOption
  where attrSetSpellCheck p g      = g { _eOptionGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasStyle EOption
  where attrSetStyle p g           = g { _eOptionGlobals = Just (attrSetStyle p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasTabIndex EOption
  where attrSetTabIndex p g        = g { _eOptionGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasTitle EOption
  where attrSetTitle p g           = g { _eOptionGlobals = Just (attrSetTitle p (fromMaybe gDef (_eOptionGlobals g))) }
instance AttrHasTranslate EOption
  where attrSetTranslate p g       = g { _eOptionGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eOptionGlobals g))) }

instance AttrGetClassName EOption where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eOptionGlobals g)

instance AttrHasDisabled EOption where attrSetDisabled p g  = g { _eOptionDisabled  = Just p }

instance AttrHasLabel    EOption where attrSetLabel p g  = g { _eOptionLabel  = Just p }
instance AttrHasSelected    EOption where attrSetSelected p g  = g { _eOptionSelected  = Just p }
instance AttrHasValueText    EOption where attrSetValueText p g  = g { _eOptionValueText  = Just p }

instance AttrHasCustom EOption where attrSetCustom p g       = g { _eOptionCustom = Just p }

eOption' :: forall t m a. DomBuilder t m => EOption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOption' b  = elAttr' "option" (attrMap b)

eOption :: forall t m a. DomBuilder t m => EOption -> m a -> m a
eOption b children = snd <$> eOption' b children

eOptionN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOptionN' = el' "option"

eOptionN :: forall t m a. DomBuilder t m => m a -> m a
eOptionN children = snd <$> eOptionN' children

eOptionD' :: forall t m a. MonadWidget t m => Dynamic t EOption -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOptionD' b  = elDynAttr' "option" (attrMap <$> b)

eOptionD :: forall t m a. MonadWidget t m => Dynamic t EOption -> m a -> m a
eOptionD b children = snd <$> eOptionD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Output-element
data EOutput = EOutput
  { _eOutputGlobals :: Maybe Globals
  , _eOutputForId   :: Maybe ForId
  , _eOutputForm    :: Maybe Form
  , _eOutputName    :: Maybe Name
  , _eOutputCustom  :: Maybe Attr
  }


instance AttrMap EOutput where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eOutputGlobals b
    , attrMap <$> _eOutputForId b
    , attrMap <$> _eOutputForm b
    , attrMap <$> _eOutputName b
    ] <> maybe [] (\a -> [a]) (_eOutputCustom b)


instance Default EOutput where
  def = EOutput def def def def def

instance Monoid EOutput where
  mempty = def
  mappend (EOutput a1 a2 a3 a4 a5) (EOutput b1 b2 b3 b4 b5)
    = EOutput (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance AttrHasGlobals EOutput where
   attrSetGlobals p b = b { _eOutputGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EOutput
  where attrSetAccessKey p g = g { _eOutputGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasAnmval EOutput
  where attrSetAnmval p g = g { _eOutputGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasContentEditable EOutput
  where attrSetContentEditable p g = g  { _eOutputGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasContextMenu EOutput
  where attrSetContextMenu p g     = g { _eOutputGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasClass EOutput
  where attrSetClassName p g           = g { _eOutputGlobals = Just (attrSetClassName p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasDnmval EOutput
  where attrSetDnmval p g           = g { _eOutputGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasDir EOutput
  where attrSetDir p g             = g { _eOutputGlobals = Just (attrSetDir p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasDraggable EOutput
  where attrSetDraggable p g       = g { _eOutputGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasHidden EOutput
  where attrSetHidden p g          = g { _eOutputGlobals = Just (attrSetHidden p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasId EOutput
  where attrSetId p g              = g { _eOutputGlobals = Just (attrSetId p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasLang EOutput
  where attrSetLang p g            = g { _eOutputGlobals = Just (attrSetLang p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasRole EOutput
  where attrSetRole p g            = g { _eOutputGlobals = Just (attrSetRole p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasSlot EOutput
  where attrSetSlot p g            = g { _eOutputGlobals = Just (attrSetSlot p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasSpellCheck EOutput
  where attrSetSpellCheck p g      = g { _eOutputGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasStyle EOutput
  where attrSetStyle p g           = g { _eOutputGlobals = Just (attrSetStyle p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasTabIndex EOutput
  where attrSetTabIndex p g        = g { _eOutputGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasTitle EOutput
  where attrSetTitle p g           = g { _eOutputGlobals = Just (attrSetTitle p (fromMaybe gDef (_eOutputGlobals g))) }
instance AttrHasTranslate EOutput
  where attrSetTranslate p g       = g { _eOutputGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eOutputGlobals g))) }

instance AttrGetClassName EOutput where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eOutputGlobals g)

instance AttrHasForId EOutput where attrSetForId p g  = g { _eOutputForId  = Just p }
instance AttrHasForm EOutput where attrSetForm p g  = g { _eOutputForm  = Just p }

instance AttrHasName    EOutput where attrSetName p g  = g { _eOutputName  = Just p }

instance AttrHasCustom EOutput where attrSetCustom p g       = g { _eOutputCustom = Just p }

eOutput' :: forall t m a. DomBuilder t m => EOutput -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOutput' b  = elAttr' "output" (attrMap b)

eOutput :: forall t m a. DomBuilder t m => EOutput -> m a -> m a
eOutput b children = snd <$> eOutput' b children

eOutputN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOutputN' = el' "output"

eOutputN :: forall t m a. DomBuilder t m => m a -> m a
eOutputN children = snd <$> eOutputN' children

eOutputD' :: forall t m a. MonadWidget t m => Dynamic t EOutput -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eOutputD' b  = elDynAttr' "output" (attrMap <$> b)

eOutputD :: forall t m a. MonadWidget t m => Dynamic t EOutput -> m a -> m a
eOutputD b children = snd <$> eOutputD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | P-element has only the global attributes.
data EP = EP
  { _ePGlobals :: Maybe Globals
  , _ePCustom  :: Maybe Attr
  }


instance AttrMap EP where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _ePGlobals b
    ] <> maybe [] (\a -> [a]) (_ePCustom b)


instance Default EP where
  def = EP def def

instance Monoid EP where
  mempty = def
  mappend (EP a1 a2) (EP b1 b2) = EP (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EP where
   attrSetGlobals p b = b { _ePGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EP
  where attrSetAccessKey p g = g { _ePGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasAnmval EP
  where attrSetAnmval p g = g { _ePGlobals = Just (attrSetAnmval p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasContentEditable EP
  where attrSetContentEditable p g = g  { _ePGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasContextMenu EP
  where attrSetContextMenu p g     = g { _ePGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasClass EP
  where attrSetClassName p g           = g { _ePGlobals = Just (attrSetClassName p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasDnmval EP
  where attrSetDnmval p g           = g { _ePGlobals = Just (attrSetDnmval p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasDir EP
  where attrSetDir p g             = g { _ePGlobals = Just (attrSetDir p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasDraggable EP
  where attrSetDraggable p g       = g { _ePGlobals = Just (attrSetDraggable p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasHidden EP
  where attrSetHidden p g          = g { _ePGlobals = Just (attrSetHidden p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasId EP
  where attrSetId p g              = g { _ePGlobals = Just (attrSetId p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasLang EP
  where attrSetLang p g            = g { _ePGlobals = Just (attrSetLang p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasRole EP
  where attrSetRole p g            = g { _ePGlobals = Just (attrSetRole p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasSlot EP
  where attrSetSlot p g            = g { _ePGlobals = Just (attrSetSlot p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasSpellCheck EP
  where attrSetSpellCheck p g      = g { _ePGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasStyle EP
  where attrSetStyle p g           = g { _ePGlobals = Just (attrSetStyle p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasTabIndex EP
  where attrSetTabIndex p g        = g { _ePGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasTitle EP
  where attrSetTitle p g           = g { _ePGlobals = Just (attrSetTitle p (fromMaybe gDef (_ePGlobals g))) }
instance AttrHasTranslate EP
  where attrSetTranslate p g       = g { _ePGlobals = Just (attrSetTranslate p (fromMaybe gDef (_ePGlobals g))) }

instance AttrGetClassName EP where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_ePGlobals g)

instance AttrHasCustom EP where attrSetCustom p g       = g { _ePCustom = Just p }

eP' :: forall t m a. DomBuilder t m => EP -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eP' b  = elAttr' "p" (attrMap b)

eP :: forall t m a. DomBuilder t m => EP -> m a -> m a
eP b children = snd <$> eP' b children

ePN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePN' = el' "p"

ePN :: forall t m a. DomBuilder t m => m a -> m a
ePN children = snd <$> ePN' children

ePD' :: forall t m a. MonadWidget t m => Dynamic t EP -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePD' b  = elDynAttr' "p" (attrMap <$> b)

ePD :: forall t m a. MonadWidget t m => Dynamic t EP -> m a -> m a
ePD b children = snd <$> ePD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Pre-element has only the global attributes.
data EPre = EPre
  { _ePreGlobals :: Maybe Globals
  , _ePreCustom  :: Maybe Attr
  }


instance AttrMap EPre where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _ePreGlobals b
    ] <> maybe [] (\a -> [a]) (_ePreCustom b)


instance Default EPre where
  def = EPre def def

instance Monoid EPre where
  mempty = def
  mappend (EPre a1 a2) (EPre b1 b2) = EPre (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EPre where
   attrSetGlobals p b = b { _ePreGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EPre
  where attrSetAccessKey p g = g { _ePreGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasAnmval EPre
  where attrSetAnmval p g = g { _ePreGlobals = Just (attrSetAnmval p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasContentEditable EPre
  where attrSetContentEditable p g = g  { _ePreGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasContextMenu EPre
  where attrSetContextMenu p g     = g { _ePreGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasClass EPre
  where attrSetClassName p g           = g { _ePreGlobals = Just (attrSetClassName p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasDnmval EPre
  where attrSetDnmval p g           = g { _ePreGlobals = Just (attrSetDnmval p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasDir EPre
  where attrSetDir p g             = g { _ePreGlobals = Just (attrSetDir p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasDraggable EPre
  where attrSetDraggable p g       = g { _ePreGlobals = Just (attrSetDraggable p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasHidden EPre
  where attrSetHidden p g          = g { _ePreGlobals = Just (attrSetHidden p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasId EPre
  where attrSetId p g              = g { _ePreGlobals = Just (attrSetId p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasLang EPre
  where attrSetLang p g            = g { _ePreGlobals = Just (attrSetLang p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasRole EPre
  where attrSetRole p g            = g { _ePreGlobals = Just (attrSetRole p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasSlot EPre
  where attrSetSlot p g            = g { _ePreGlobals = Just (attrSetSlot p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasSpellCheck EPre
  where attrSetSpellCheck p g      = g { _ePreGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasStyle EPre
  where attrSetStyle p g           = g { _ePreGlobals = Just (attrSetStyle p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasTabIndex EPre
  where attrSetTabIndex p g        = g { _ePreGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasTitle EPre
  where attrSetTitle p g           = g { _ePreGlobals = Just (attrSetTitle p (fromMaybe gDef (_ePreGlobals g))) }
instance AttrHasTranslate EPre
  where attrSetTranslate p g       = g { _ePreGlobals = Just (attrSetTranslate p (fromMaybe gDef (_ePreGlobals g))) }

instance AttrGetClassName EPre where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_ePreGlobals g)

instance AttrHasCustom EPre where attrSetCustom p g       = g { _ePreCustom = Just p }

ePre' :: forall t m a. DomBuilder t m => EPre -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePre' b  = elAttr' "pre" (attrMap b)

ePre :: forall t m a. DomBuilder t m => EPre -> m a -> m a
ePre b children = snd <$> ePre' b children

ePreN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePreN' = el' "pre"

ePreN :: forall t m a. DomBuilder t m => m a -> m a
ePreN children = snd <$> ePreN' children

ePreD' :: forall t m a. MonadWidget t m => Dynamic t EPre -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePreD' b  = elDynAttr' "pre" (attrMap <$> b)

ePreD :: forall t m a. MonadWidget t m => Dynamic t EPre -> m a -> m a
ePreD b children = snd <$> ePreD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Progress-element
data EProgress = EProgress
  { _eProgressGlobals     :: Maybe Globals
  , _eProgressMax         :: Maybe Max
  , _eProgressValueNumber :: Maybe ValueNumber
  , _eProgressCustom      :: Maybe Attr
  }


instance AttrMap EProgress where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eProgressGlobals b
    , attrMap <$> _eProgressMax b
    , attrMap <$> _eProgressValueNumber b
    ] <> maybe [] (\a -> [a]) (_eProgressCustom b)


instance Default EProgress where
  def = EProgress def def def def

instance Monoid EProgress where
  mempty = def
  mappend (EProgress a1 a2 a3 a4) (EProgress b1 b2 b3 b4)
    = EProgress (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals EProgress where
   attrSetGlobals p b = b { _eProgressGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EProgress
  where attrSetAccessKey p g = g { _eProgressGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasAnmval EProgress
  where attrSetAnmval p g = g { _eProgressGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasContentEditable EProgress
  where attrSetContentEditable p g = g  { _eProgressGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasContextMenu EProgress
  where attrSetContextMenu p g     = g { _eProgressGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasClass EProgress
  where attrSetClassName p g           = g { _eProgressGlobals = Just (attrSetClassName p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasDnmval EProgress
  where attrSetDnmval p g           = g { _eProgressGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasDir EProgress
  where attrSetDir p g             = g { _eProgressGlobals = Just (attrSetDir p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasDraggable EProgress
  where attrSetDraggable p g       = g { _eProgressGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasHidden EProgress
  where attrSetHidden p g          = g { _eProgressGlobals = Just (attrSetHidden p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasId EProgress
  where attrSetId p g              = g { _eProgressGlobals = Just (attrSetId p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasLang EProgress
  where attrSetLang p g            = g { _eProgressGlobals = Just (attrSetLang p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasRole EProgress
  where attrSetRole p g            = g { _eProgressGlobals = Just (attrSetRole p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasSlot EProgress
  where attrSetSlot p g            = g { _eProgressGlobals = Just (attrSetSlot p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasSpellCheck EProgress
  where attrSetSpellCheck p g      = g { _eProgressGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasStyle EProgress
  where attrSetStyle p g           = g { _eProgressGlobals = Just (attrSetStyle p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasTabIndex EProgress
  where attrSetTabIndex p g        = g { _eProgressGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasTitle EProgress
  where attrSetTitle p g           = g { _eProgressGlobals = Just (attrSetTitle p (fromMaybe gDef (_eProgressGlobals g))) }
instance AttrHasTranslate EProgress
  where attrSetTranslate p g       = g { _eProgressGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eProgressGlobals g))) }

instance AttrGetClassName EProgress where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eProgressGlobals g)

instance AttrHasMax EProgress where attrSetMax p g  = g { _eProgressMax  = Just p }

instance AttrHasValueNumber    EProgress where attrSetValueNumber p g  = g { _eProgressValueNumber  = Just p }

instance AttrHasCustom EProgress where attrSetCustom p g       = g { _eProgressCustom = Just p }

eProgress' :: forall t m a. DomBuilder t m => EProgress -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eProgress' b  = elAttr' "progress" (attrMap b)

eProgress :: forall t m a. DomBuilder t m => EProgress -> m a -> m a
eProgress b children = snd <$> eProgress' b children

eProgressN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eProgressN' = el' "progress"

eProgressN :: forall t m a. DomBuilder t m => m a -> m a
eProgressN children = snd <$> eProgressN' children

eProgressD' :: forall t m a. MonadWidget t m => Dynamic t EProgress -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eProgressD' b  = elDynAttr' "progress" (attrMap <$> b)

eProgressD :: forall t m a. MonadWidget t m => Dynamic t EProgress -> m a -> m a
eProgressD b children = snd <$> eProgressD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Q-element
data EQ_ = EQ_
  { _eQGlobals :: Maybe Globals
  , _eQCite    :: Maybe Cite
  , _eQCustom  :: Maybe Attr
  }


instance AttrMap EQ_ where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eQGlobals b
    , attrMap <$> _eQCite b
    ] <> maybe [] (\a -> [a]) (_eQCustom b)


instance Default EQ_ where
  def = EQ_ def def def

instance Monoid EQ_ where
  mempty = def
  mappend (EQ_ a1 a2 a3) (EQ_ b1 b2 b3) = EQ_ (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EQ_ where
   attrSetGlobals p b = b { _eQGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EQ_
  where attrSetAccessKey p g = g { _eQGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasAnmval EQ_
  where attrSetAnmval p g = g { _eQGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasContentEditable EQ_
  where attrSetContentEditable p g = g  { _eQGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasContextMenu EQ_
  where attrSetContextMenu p g     = g { _eQGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasClass EQ_
  where attrSetClassName p g           = g { _eQGlobals = Just (attrSetClassName p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasDnmval EQ_
  where attrSetDnmval p g           = g { _eQGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasDir EQ_
  where attrSetDir p g             = g { _eQGlobals = Just (attrSetDir p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasDraggable EQ_
  where attrSetDraggable p g       = g { _eQGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasHidden EQ_
  where attrSetHidden p g          = g { _eQGlobals = Just (attrSetHidden p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasId EQ_
  where attrSetId p g              = g { _eQGlobals = Just (attrSetId p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasLang EQ_
  where attrSetLang p g            = g { _eQGlobals = Just (attrSetLang p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasRole EQ_
  where attrSetRole p g            = g { _eQGlobals = Just (attrSetRole p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasSlot EQ_
  where attrSetSlot p g            = g { _eQGlobals = Just (attrSetSlot p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasSpellCheck EQ_
  where attrSetSpellCheck p g      = g { _eQGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasStyle EQ_
  where attrSetStyle p g           = g { _eQGlobals = Just (attrSetStyle p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasTabIndex EQ_
  where attrSetTabIndex p g        = g { _eQGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasTitle EQ_
  where attrSetTitle p g           = g { _eQGlobals = Just (attrSetTitle p (fromMaybe gDef (_eQGlobals g))) }
instance AttrHasTranslate EQ_
  where attrSetTranslate p g       = g { _eQGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eQGlobals g))) }

instance AttrGetClassName EQ_ where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eQGlobals g)

instance AttrHasCite EQ_ where attrSetCite p g  = g { _eQCite  = Just p }

instance AttrHasCustom EQ_ where attrSetCustom p g       = g { _eQCustom = Just p }

eQ' :: forall t m a. DomBuilder t m => EQ_ -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eQ' b  = elAttr' "q" (attrMap b)

eQ :: forall t m a. DomBuilder t m => EQ_ -> m a -> m a
eQ b children = snd <$> eQ' b children

eQN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eQN' = el' "q"

eQN :: forall t m a. DomBuilder t m => m a -> m a
eQN children = snd <$> eQN' children

eQD' :: forall t m a. MonadWidget t m => Dynamic t EQ_ -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eQD' b  = elDynAttr' "q" (attrMap <$> b)

eQD :: forall t m a. MonadWidget t m => Dynamic t EQ_ -> m a -> m a
eQD b children = snd <$> eQD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rb-element
data ERb = ERb
  { _eRbGlobals :: Maybe Globals
  , _eRbCustom  :: Maybe Attr
  }


instance AttrMap ERb where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eRbGlobals b
    ] <> maybe [] (\a -> [a]) (_eRbCustom b)


instance Default ERb where
  def = ERb def def

instance Monoid ERb where
  mempty = def
  mappend (ERb a1 a2) (ERb b1 b2) = ERb (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ERb where
   attrSetGlobals p b = b { _eRbGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ERb
  where attrSetAccessKey p g = g { _eRbGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasAnmval ERb
  where attrSetAnmval p g = g { _eRbGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasContentEditable ERb
  where attrSetContentEditable p g = g  { _eRbGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasContextMenu ERb
  where attrSetContextMenu p g     = g { _eRbGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasClass ERb
  where attrSetClassName p g           = g { _eRbGlobals = Just (attrSetClassName p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasDnmval ERb
  where attrSetDnmval p g           = g { _eRbGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasDir ERb
  where attrSetDir p g             = g { _eRbGlobals = Just (attrSetDir p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasDraggable ERb
  where attrSetDraggable p g       = g { _eRbGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasHidden ERb
  where attrSetHidden p g          = g { _eRbGlobals = Just (attrSetHidden p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasId ERb
  where attrSetId p g              = g { _eRbGlobals = Just (attrSetId p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasLang ERb
  where attrSetLang p g            = g { _eRbGlobals = Just (attrSetLang p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasRole ERb
  where attrSetRole p g            = g { _eRbGlobals = Just (attrSetRole p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasSlot ERb
  where attrSetSlot p g            = g { _eRbGlobals = Just (attrSetSlot p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasSpellCheck ERb
  where attrSetSpellCheck p g      = g { _eRbGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasStyle ERb
  where attrSetStyle p g           = g { _eRbGlobals = Just (attrSetStyle p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasTabIndex ERb
  where attrSetTabIndex p g        = g { _eRbGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasTitle ERb
  where attrSetTitle p g           = g { _eRbGlobals = Just (attrSetTitle p (fromMaybe gDef (_eRbGlobals g))) }
instance AttrHasTranslate ERb
  where attrSetTranslate p g       = g { _eRbGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eRbGlobals g))) }

instance AttrGetClassName ERb where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eRbGlobals g)

instance AttrHasCustom ERb where attrSetCustom p g       = g { _eRbCustom = Just p }

eRb' :: forall t m a. DomBuilder t m => ERb -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRb' b  = elAttr' "rb" (attrMap b)

eRb :: forall t m a. DomBuilder t m => ERb -> m a -> m a
eRb b children = snd <$> eRb' b children

eRbN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRbN' = el' "rb"

eRbN :: forall t m a. DomBuilder t m => m a -> m a
eRbN children = snd <$> eRbN' children

eRbD' :: forall t m a. MonadWidget t m => Dynamic t ERb -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRbD' b  = elDynAttr' "rb" (attrMap <$> b)

eRbD :: forall t m a. MonadWidget t m => Dynamic t ERb -> m a -> m a
eRbD b children = snd <$> eRbD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rp-element has only the global attributes.
data ERp = ERp
  { _eRpGlobals :: Maybe Globals
  , _eRpCustom  :: Maybe Attr
  }


instance AttrMap ERp where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eRpGlobals b
    ] <> maybe [] (\a -> [a]) (_eRpCustom b)


instance Default ERp where
  def = ERp def def

instance Monoid ERp where
  mempty = def
  mappend (ERp a1 a2) (ERp b1 b2) = ERp (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ERp where
   attrSetGlobals p b = b { _eRpGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ERp
  where attrSetAccessKey p g = g { _eRpGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasAnmval ERp
  where attrSetAnmval p g = g { _eRpGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasContentEditable ERp
  where attrSetContentEditable p g = g  { _eRpGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasContextMenu ERp
  where attrSetContextMenu p g     = g { _eRpGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasClass ERp
  where attrSetClassName p g           = g { _eRpGlobals = Just (attrSetClassName p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasDnmval ERp
  where attrSetDnmval p g           = g { _eRpGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasDir ERp
  where attrSetDir p g             = g { _eRpGlobals = Just (attrSetDir p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasDraggable ERp
  where attrSetDraggable p g       = g { _eRpGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasHidden ERp
  where attrSetHidden p g          = g { _eRpGlobals = Just (attrSetHidden p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasId ERp
  where attrSetId p g              = g { _eRpGlobals = Just (attrSetId p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasLang ERp
  where attrSetLang p g            = g { _eRpGlobals = Just (attrSetLang p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasRole ERp
  where attrSetRole p g            = g { _eRpGlobals = Just (attrSetRole p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasSlot ERp
  where attrSetSlot p g            = g { _eRpGlobals = Just (attrSetSlot p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasSpellCheck ERp
  where attrSetSpellCheck p g      = g { _eRpGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasStyle ERp
  where attrSetStyle p g           = g { _eRpGlobals = Just (attrSetStyle p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasTabIndex ERp
  where attrSetTabIndex p g        = g { _eRpGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasTitle ERp
  where attrSetTitle p g           = g { _eRpGlobals = Just (attrSetTitle p (fromMaybe gDef (_eRpGlobals g))) }
instance AttrHasTranslate ERp
  where attrSetTranslate p g       = g { _eRpGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eRpGlobals g))) }

instance AttrGetClassName ERp where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eRpGlobals g)

instance AttrHasCustom ERp where attrSetCustom p g       = g { _eRpCustom = Just p }

eRp' :: forall t m a. DomBuilder t m => ERp -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRp' b  = elAttr' "rp" (attrMap b)

eRp :: forall t m a. DomBuilder t m => ERp -> m a -> m a
eRp b children = snd <$> eRp' b children

eRpN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRpN' = el' "rp"

eRpN :: forall t m a. DomBuilder t m => m a -> m a
eRpN children = snd <$> eRpN' children

eRpD' :: forall t m a. MonadWidget t m => Dynamic t ERp -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRpD' b  = elDynAttr' "rp" (attrMap <$> b)

eRpD :: forall t m a. MonadWidget t m => Dynamic t ERp -> m a -> m a
eRpD b children = snd <$> eRpD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rt-element has only the global attributes.
data ERt = ERt
  { _eRtGlobals :: Maybe Globals
  , _eRtCustom  :: Maybe Attr
  }


instance AttrMap ERt where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eRtGlobals b
    ] <> maybe [] (\a -> [a]) (_eRtCustom b)


instance Default ERt where
  def = ERt def def

instance Monoid ERt where
  mempty = def
  mappend (ERt a1 a2) (ERt b1 b2) = ERt (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ERt where
   attrSetGlobals p b = b { _eRtGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ERt
  where attrSetAccessKey p g = g { _eRtGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasAnmval ERt
  where attrSetAnmval p g = g { _eRtGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasContentEditable ERt
  where attrSetContentEditable p g = g  { _eRtGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasContextMenu ERt
  where attrSetContextMenu p g     = g { _eRtGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasClass ERt
  where attrSetClassName p g           = g { _eRtGlobals = Just (attrSetClassName p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasDnmval ERt
  where attrSetDnmval p g           = g { _eRtGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasDir ERt
  where attrSetDir p g             = g { _eRtGlobals = Just (attrSetDir p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasDraggable ERt
  where attrSetDraggable p g       = g { _eRtGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasHidden ERt
  where attrSetHidden p g          = g { _eRtGlobals = Just (attrSetHidden p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasId ERt
  where attrSetId p g              = g { _eRtGlobals = Just (attrSetId p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasLang ERt
  where attrSetLang p g            = g { _eRtGlobals = Just (attrSetLang p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasRole ERt
  where attrSetRole p g            = g { _eRtGlobals = Just (attrSetRole p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasSlot ERt
  where attrSetSlot p g            = g { _eRtGlobals = Just (attrSetSlot p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasSpellCheck ERt
  where attrSetSpellCheck p g      = g { _eRtGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasStyle ERt
  where attrSetStyle p g           = g { _eRtGlobals = Just (attrSetStyle p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasTabIndex ERt
  where attrSetTabIndex p g        = g { _eRtGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasTitle ERt
  where attrSetTitle p g           = g { _eRtGlobals = Just (attrSetTitle p (fromMaybe gDef (_eRtGlobals g))) }
instance AttrHasTranslate ERt
  where attrSetTranslate p g       = g { _eRtGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eRtGlobals g))) }

instance AttrGetClassName ERt where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eRtGlobals g)

instance AttrHasCustom ERt where attrSetCustom p g       = g { _eRtCustom = Just p }

eRt' :: forall t m a. DomBuilder t m => ERt -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRt' b  = elAttr' "rt" (attrMap b)

eRt :: forall t m a. DomBuilder t m => ERt -> m a -> m a
eRt b children = snd <$> eRt' b children

eRtN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRtN' = el' "rt"

eRtN :: forall t m a. DomBuilder t m => m a -> m a
eRtN children = snd <$> eRtN' children

eRtD' :: forall t m a. MonadWidget t m => Dynamic t ERt -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRtD' b  = elDynAttr' "rt" (attrMap <$> b)

eRtD :: forall t m a. MonadWidget t m => Dynamic t ERt -> m a -> m a
eRtD b children = snd <$> eRtD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rtc-element has only the global attributes.
data ERtc = ERtc
  { _eRtcGlobals :: Maybe Globals
  , _eRtcCustom  :: Maybe Attr
  }


instance AttrMap ERtc where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eRtcGlobals b
    ] <> maybe [] (\a -> [a]) (_eRtcCustom b)


instance Default ERtc where
  def = ERtc def def

instance Monoid ERtc where
  mempty = def
  mappend (ERtc a1 a2) (ERtc b1 b2) = ERtc (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ERtc where
   attrSetGlobals p b = b { _eRtcGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ERtc
  where attrSetAccessKey p g = g { _eRtcGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasAnmval ERtc
  where attrSetAnmval p g = g { _eRtcGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasContentEditable ERtc
  where attrSetContentEditable p g = g  { _eRtcGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasContextMenu ERtc
  where attrSetContextMenu p g     = g { _eRtcGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasClass ERtc
  where attrSetClassName p g           = g { _eRtcGlobals = Just (attrSetClassName p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasDnmval ERtc
  where attrSetDnmval p g           = g { _eRtcGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasDir ERtc
  where attrSetDir p g             = g { _eRtcGlobals = Just (attrSetDir p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasDraggable ERtc
  where attrSetDraggable p g       = g { _eRtcGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasHidden ERtc
  where attrSetHidden p g          = g { _eRtcGlobals = Just (attrSetHidden p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasId ERtc
  where attrSetId p g              = g { _eRtcGlobals = Just (attrSetId p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasLang ERtc
  where attrSetLang p g            = g { _eRtcGlobals = Just (attrSetLang p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasRole ERtc
  where attrSetRole p g            = g { _eRtcGlobals = Just (attrSetRole p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasSlot ERtc
  where attrSetSlot p g            = g { _eRtcGlobals = Just (attrSetSlot p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasSpellCheck ERtc
  where attrSetSpellCheck p g      = g { _eRtcGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasStyle ERtc
  where attrSetStyle p g           = g { _eRtcGlobals = Just (attrSetStyle p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasTabIndex ERtc
  where attrSetTabIndex p g        = g { _eRtcGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasTitle ERtc
  where attrSetTitle p g           = g { _eRtcGlobals = Just (attrSetTitle p (fromMaybe gDef (_eRtcGlobals g))) }
instance AttrHasTranslate ERtc
  where attrSetTranslate p g       = g { _eRtcGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eRtcGlobals g))) }

instance AttrGetClassName ERtc where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eRtcGlobals g)

instance AttrHasCustom ERtc where attrSetCustom p g       = g { _eRtcCustom = Just p }

eRtc' :: forall t m a. DomBuilder t m => ERtc -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRtc' b  = elAttr' "rtc" (attrMap b)

eRtc :: forall t m a. DomBuilder t m => ERtc -> m a -> m a
eRtc b children = snd <$> eRtc' b children

eRtcN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRtcN' = el' "rtc"

eRtcN :: forall t m a. DomBuilder t m => m a -> m a
eRtcN children = snd <$> eRtcN' children

eRtcD' :: forall t m a. MonadWidget t m => Dynamic t ERtc -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRtcD' b  = elDynAttr' "rtc" (attrMap <$> b)

eRtcD :: forall t m a. MonadWidget t m => Dynamic t ERtc -> m a -> m a
eRtcD b children = snd <$> eRtcD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ruby-element has only the global attributes.
data ERuby = ERuby
  { _eRubyGlobals :: Maybe Globals
  , _eRubyCustom  :: Maybe Attr
  }


instance AttrMap ERuby where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eRubyGlobals b
    ] <> maybe [] (\a -> [a]) (_eRubyCustom b)


instance Default ERuby where
  def = ERuby def def

instance Monoid ERuby where
  mempty = def
  mappend (ERuby a1 a2) (ERuby b1 b2) = ERuby (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ERuby where
   attrSetGlobals p b = b { _eRubyGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ERuby
  where attrSetAccessKey p g = g { _eRubyGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasAnmval ERuby
  where attrSetAnmval p g = g { _eRubyGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasContentEditable ERuby
  where attrSetContentEditable p g = g  { _eRubyGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasContextMenu ERuby
  where attrSetContextMenu p g     = g { _eRubyGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasClass ERuby
  where attrSetClassName p g           = g { _eRubyGlobals = Just (attrSetClassName p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasDnmval ERuby
  where attrSetDnmval p g           = g { _eRubyGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasDir ERuby
  where attrSetDir p g             = g { _eRubyGlobals = Just (attrSetDir p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasDraggable ERuby
  where attrSetDraggable p g       = g { _eRubyGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasHidden ERuby
  where attrSetHidden p g          = g { _eRubyGlobals = Just (attrSetHidden p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasId ERuby
  where attrSetId p g              = g { _eRubyGlobals = Just (attrSetId p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasLang ERuby
  where attrSetLang p g            = g { _eRubyGlobals = Just (attrSetLang p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasRole ERuby
  where attrSetRole p g            = g { _eRubyGlobals = Just (attrSetRole p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasSlot ERuby
  where attrSetSlot p g            = g { _eRubyGlobals = Just (attrSetSlot p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasSpellCheck ERuby
  where attrSetSpellCheck p g      = g { _eRubyGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasStyle ERuby
  where attrSetStyle p g           = g { _eRubyGlobals = Just (attrSetStyle p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasTabIndex ERuby
  where attrSetTabIndex p g        = g { _eRubyGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasTitle ERuby
  where attrSetTitle p g           = g { _eRubyGlobals = Just (attrSetTitle p (fromMaybe gDef (_eRubyGlobals g))) }
instance AttrHasTranslate ERuby
  where attrSetTranslate p g       = g { _eRubyGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eRubyGlobals g))) }

instance AttrGetClassName ERuby where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eRubyGlobals g)

instance AttrHasCustom ERuby where attrSetCustom p g       = g { _eRubyCustom = Just p }

eRuby' :: forall t m a. DomBuilder t m => ERuby -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRuby' b  = elAttr' "ruby" (attrMap b)

eRuby :: forall t m a. DomBuilder t m => ERuby -> m a -> m a
eRuby b children = snd <$> eRuby' b children

eRubyN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRubyN' = el' "ruby"

eRubyN :: forall t m a. DomBuilder t m => m a -> m a
eRubyN children = snd <$> eRubyN' children

eRubyD' :: forall t m a. MonadWidget t m => Dynamic t ERuby -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eRubyD' b  = elDynAttr' "ruby" (attrMap <$> b)

eRubyD :: forall t m a. MonadWidget t m => Dynamic t ERuby -> m a -> m a
eRubyD b children = snd <$> eRubyD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | S-element has only the global attributes.
data ES = ES
  { _eSGlobals :: Maybe Globals
  , _eSCustom  :: Maybe Attr
  }


instance AttrMap ES where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSGlobals b
    ] <> maybe [] (\a -> [a]) (_eSCustom b)


instance Default ES where
  def = ES def def

instance Monoid ES where
  mempty = def
  mappend (ES a1 a2) (ES b1 b2) = ES (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ES where
   attrSetGlobals p b = b { _eSGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ES
  where attrSetAccessKey p g = g { _eSGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasAnmval ES
  where attrSetAnmval p g = g { _eSGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasContentEditable ES
  where attrSetContentEditable p g = g  { _eSGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasContextMenu ES
  where attrSetContextMenu p g     = g { _eSGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasClass ES
  where attrSetClassName p g           = g { _eSGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasDnmval ES
  where attrSetDnmval p g           = g { _eSGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasDir ES
  where attrSetDir p g             = g { _eSGlobals = Just (attrSetDir p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasDraggable ES
  where attrSetDraggable p g       = g { _eSGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasHidden ES
  where attrSetHidden p g          = g { _eSGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasId ES
  where attrSetId p g              = g { _eSGlobals = Just (attrSetId p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasLang ES
  where attrSetLang p g            = g { _eSGlobals = Just (attrSetLang p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasRole ES
  where attrSetRole p g            = g { _eSGlobals = Just (attrSetRole p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasSlot ES
  where attrSetSlot p g            = g { _eSGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasSpellCheck ES
  where attrSetSpellCheck p g      = g { _eSGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasStyle ES
  where attrSetStyle p g           = g { _eSGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasTabIndex ES
  where attrSetTabIndex p g        = g { _eSGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasTitle ES
  where attrSetTitle p g           = g { _eSGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSGlobals g))) }
instance AttrHasTranslate ES
  where attrSetTranslate p g       = g { _eSGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSGlobals g))) }

instance AttrGetClassName ES where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSGlobals g)

instance AttrHasCustom ES where attrSetCustom p g       = g { _eSCustom = Just p }

eS' :: forall t m a. DomBuilder t m => ES -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eS' b  = elAttr' "s" (attrMap b)

eS :: forall t m a. DomBuilder t m => ES -> m a -> m a
eS b children = snd <$> eS' b children

eSN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSN' = el' "s"

eSN :: forall t m a. DomBuilder t m => m a -> m a
eSN children = snd <$> eSN' children

eSD' :: forall t m a. MonadWidget t m => Dynamic t ES -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSD' b  = elDynAttr' "s" (attrMap <$> b)

eSD :: forall t m a. MonadWidget t m => Dynamic t ES -> m a -> m a
eSD b children = snd <$> eSD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Samp-element has only the global attributes.
data ESamp = ESamp
  { _eSampGlobals :: Maybe Globals
  , _eSampCustom  :: Maybe Attr
  }


instance AttrMap ESamp where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSampGlobals b
    ] <> maybe [] (\a -> [a]) (_eSampCustom b)


instance Default ESamp where
  def = ESamp def def

instance Monoid ESamp where
  mempty = def
  mappend (ESamp a1 a2) (ESamp b1 b2) = ESamp (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESamp where
   attrSetGlobals p b = b { _eSampGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESamp
  where attrSetAccessKey p g = g { _eSampGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasAnmval ESamp
  where attrSetAnmval p g = g { _eSampGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasContentEditable ESamp
  where attrSetContentEditable p g = g  { _eSampGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasContextMenu ESamp
  where attrSetContextMenu p g     = g { _eSampGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasClass ESamp
  where attrSetClassName p g           = g { _eSampGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasDnmval ESamp
  where attrSetDnmval p g           = g { _eSampGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasDir ESamp
  where attrSetDir p g             = g { _eSampGlobals = Just (attrSetDir p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasDraggable ESamp
  where attrSetDraggable p g       = g { _eSampGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasHidden ESamp
  where attrSetHidden p g          = g { _eSampGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasId ESamp
  where attrSetId p g              = g { _eSampGlobals = Just (attrSetId p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasLang ESamp
  where attrSetLang p g            = g { _eSampGlobals = Just (attrSetLang p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasRole ESamp
  where attrSetRole p g            = g { _eSampGlobals = Just (attrSetRole p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasSlot ESamp
  where attrSetSlot p g            = g { _eSampGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasSpellCheck ESamp
  where attrSetSpellCheck p g      = g { _eSampGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasStyle ESamp
  where attrSetStyle p g           = g { _eSampGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasTabIndex ESamp
  where attrSetTabIndex p g        = g { _eSampGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasTitle ESamp
  where attrSetTitle p g           = g { _eSampGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSampGlobals g))) }
instance AttrHasTranslate ESamp
  where attrSetTranslate p g       = g { _eSampGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSampGlobals g))) }

instance AttrGetClassName ESamp where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSampGlobals g)

instance AttrHasCustom ESamp where attrSetCustom p g       = g { _eSampCustom = Just p }

eSamp' :: forall t m a. DomBuilder t m => ESamp -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSamp' b  = elAttr' "samp" (attrMap b)

eSamp :: forall t m a. DomBuilder t m => ESamp -> m a -> m a
eSamp b children = snd <$> eSamp' b children

eSampN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSampN' = el' "samp"

eSampN :: forall t m a. DomBuilder t m => m a -> m a
eSampN children = snd <$> eSampN' children

eSampD' :: forall t m a. MonadWidget t m => Dynamic t ESamp -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSampD' b  = elDynAttr' "samp" (attrMap <$> b)

eSampD :: forall t m a. MonadWidget t m => Dynamic t ESamp -> m a -> m a
eSampD b children = snd <$> eSampD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Small-element has only the global attributes.
data ESmall = ESmall
  { _eSmallGlobals :: Maybe Globals
  , _eSmallCustom  :: Maybe Attr
  }


instance AttrMap ESmall where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSmallGlobals b
    ] <> maybe [] (\a -> [a]) (_eSmallCustom b)


instance Default ESmall where
  def = ESmall def def

instance Monoid ESmall where
  mempty = def
  mappend (ESmall a1 a2) (ESmall b1 b2) = ESmall (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESmall where
   attrSetGlobals p b = b { _eSmallGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESmall
  where attrSetAccessKey p g = g { _eSmallGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasAnmval ESmall
  where attrSetAnmval p g = g { _eSmallGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasContentEditable ESmall
  where attrSetContentEditable p g = g  { _eSmallGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasContextMenu ESmall
  where attrSetContextMenu p g     = g { _eSmallGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasClass ESmall
  where attrSetClassName p g           = g { _eSmallGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasDnmval ESmall
  where attrSetDnmval p g           = g { _eSmallGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasDir ESmall
  where attrSetDir p g             = g { _eSmallGlobals = Just (attrSetDir p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasDraggable ESmall
  where attrSetDraggable p g       = g { _eSmallGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasHidden ESmall
  where attrSetHidden p g          = g { _eSmallGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasId ESmall
  where attrSetId p g              = g { _eSmallGlobals = Just (attrSetId p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasLang ESmall
  where attrSetLang p g            = g { _eSmallGlobals = Just (attrSetLang p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasRole ESmall
  where attrSetRole p g            = g { _eSmallGlobals = Just (attrSetRole p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasSlot ESmall
  where attrSetSlot p g            = g { _eSmallGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasSpellCheck ESmall
  where attrSetSpellCheck p g      = g { _eSmallGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasStyle ESmall
  where attrSetStyle p g           = g { _eSmallGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasTabIndex ESmall
  where attrSetTabIndex p g        = g { _eSmallGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasTitle ESmall
  where attrSetTitle p g           = g { _eSmallGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSmallGlobals g))) }
instance AttrHasTranslate ESmall
  where attrSetTranslate p g       = g { _eSmallGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSmallGlobals g))) }

instance AttrGetClassName ESmall where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSmallGlobals g)

instance AttrHasCustom ESmall where attrSetCustom p g       = g { _eSmallCustom = Just p }

eSmall' :: forall t m a. DomBuilder t m => ESmall -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSmall' b  = elAttr' "small" (attrMap b)

eSmall :: forall t m a. DomBuilder t m => ESmall -> m a -> m a
eSmall b children = snd <$> eSmall' b children

eSmallN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSmallN' = el' "small"

eSmallN :: forall t m a. DomBuilder t m => m a -> m a
eSmallN children = snd <$> eSmallN' children

eSmallD' :: forall t m a. MonadWidget t m => Dynamic t ESmall -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSmallD' b  = elDynAttr' "small" (attrMap <$> b)

eSmallD :: forall t m a. MonadWidget t m => Dynamic t ESmall -> m a -> m a
eSmallD b children = snd <$> eSmallD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Span-element has only the global attributes.
data ESpan = ESpan
  { _eSpanGlobals :: Maybe Globals
  , _eSpanCustom  :: Maybe Attr
  }


instance AttrMap ESpan where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSpanGlobals b
    ] <> maybe [] (\a -> [a]) (_eSpanCustom b)


instance Default ESpan where
  def = ESpan def def

instance Monoid ESpan where
  mempty = def
  mappend (ESpan a1 a2) (ESpan b1 b2) = ESpan (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESpan where
   attrSetGlobals p b = b { _eSpanGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESpan
  where attrSetAccessKey p g = g { _eSpanGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasAnmval ESpan
  where attrSetAnmval p g = g { _eSpanGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasContentEditable ESpan
  where attrSetContentEditable p g = g  { _eSpanGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasContextMenu ESpan
  where attrSetContextMenu p g     = g { _eSpanGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasClass ESpan
  where attrSetClassName p g           = g { _eSpanGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasDnmval ESpan
  where attrSetDnmval p g           = g { _eSpanGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasDir ESpan
  where attrSetDir p g             = g { _eSpanGlobals = Just (attrSetDir p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasDraggable ESpan
  where attrSetDraggable p g       = g { _eSpanGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasHidden ESpan
  where attrSetHidden p g          = g { _eSpanGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasId ESpan
  where attrSetId p g              = g { _eSpanGlobals = Just (attrSetId p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasLang ESpan
  where attrSetLang p g            = g { _eSpanGlobals = Just (attrSetLang p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasRole ESpan
  where attrSetRole p g            = g { _eSpanGlobals = Just (attrSetRole p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasSlot ESpan
  where attrSetSlot p g            = g { _eSpanGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasSpellCheck ESpan
  where attrSetSpellCheck p g      = g { _eSpanGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasStyle ESpan
  where attrSetStyle p g           = g { _eSpanGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasTabIndex ESpan
  where attrSetTabIndex p g        = g { _eSpanGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasTitle ESpan
  where attrSetTitle p g           = g { _eSpanGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSpanGlobals g))) }
instance AttrHasTranslate ESpan
  where attrSetTranslate p g       = g { _eSpanGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSpanGlobals g))) }

instance AttrGetClassName ESpan where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSpanGlobals g)

instance AttrHasCustom ESpan where attrSetCustom p g       = g { _eSpanCustom = Just p }

eSpan' :: forall t m a. DomBuilder t m => ESpan -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSpan' b  = elAttr' "span" (attrMap b)

eSpan :: forall t m a. DomBuilder t m => ESpan -> m a -> m a
eSpan b children = snd <$> eSpan' b children

eSpanN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSpanN' = el' "span"

eSpanN :: forall t m a. DomBuilder t m => m a -> m a
eSpanN children = snd <$> eSpanN' children

eSpanD' :: forall t m a. MonadWidget t m => Dynamic t ESpan -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSpanD' b  = elDynAttr' "span" (attrMap <$> b)

eSpanD :: forall t m a. MonadWidget t m => Dynamic t ESpan -> m a -> m a
eSpanD b children = snd <$> eSpanD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Strong-element has only the global attributes.
data EStrong = EStrong
  { _eStrongGlobals :: Maybe Globals
  , _eStrongCustom  :: Maybe Attr
  }


instance AttrMap EStrong where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eStrongGlobals b
    ] <> maybe [] (\a -> [a]) (_eStrongCustom b)


instance Default EStrong where
  def = EStrong def def

instance Monoid EStrong where
  mempty = def
  mappend (EStrong a1 a2) (EStrong b1 b2) = EStrong (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EStrong where
   attrSetGlobals p b = b { _eStrongGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EStrong
  where attrSetAccessKey p g = g { _eStrongGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasAnmval EStrong
  where attrSetAnmval p g = g { _eStrongGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasContentEditable EStrong
  where attrSetContentEditable p g = g  { _eStrongGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasContextMenu EStrong
  where attrSetContextMenu p g     = g { _eStrongGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasClass EStrong
  where attrSetClassName p g           = g { _eStrongGlobals = Just (attrSetClassName p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasDnmval EStrong
  where attrSetDnmval p g           = g { _eStrongGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasDir EStrong
  where attrSetDir p g             = g { _eStrongGlobals = Just (attrSetDir p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasDraggable EStrong
  where attrSetDraggable p g       = g { _eStrongGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasHidden EStrong
  where attrSetHidden p g          = g { _eStrongGlobals = Just (attrSetHidden p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasId EStrong
  where attrSetId p g              = g { _eStrongGlobals = Just (attrSetId p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasLang EStrong
  where attrSetLang p g            = g { _eStrongGlobals = Just (attrSetLang p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasRole EStrong
  where attrSetRole p g            = g { _eStrongGlobals = Just (attrSetRole p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasSlot EStrong
  where attrSetSlot p g            = g { _eStrongGlobals = Just (attrSetSlot p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasSpellCheck EStrong
  where attrSetSpellCheck p g      = g { _eStrongGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasStyle EStrong
  where attrSetStyle p g           = g { _eStrongGlobals = Just (attrSetStyle p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasTabIndex EStrong
  where attrSetTabIndex p g        = g { _eStrongGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasTitle EStrong
  where attrSetTitle p g           = g { _eStrongGlobals = Just (attrSetTitle p (fromMaybe gDef (_eStrongGlobals g))) }
instance AttrHasTranslate EStrong
  where attrSetTranslate p g       = g { _eStrongGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eStrongGlobals g))) }

instance AttrGetClassName EStrong where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eStrongGlobals g)

instance AttrHasCustom EStrong where attrSetCustom p g       = g { _eStrongCustom = Just p }

eStrong' :: forall t m a. DomBuilder t m => EStrong -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStrong' b  = elAttr' "strong" (attrMap b)

eStrong :: forall t m a. DomBuilder t m => EStrong -> m a -> m a
eStrong b children = snd <$> eStrong' b children

eStrongN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStrongN' = el' "strong"

eStrongN :: forall t m a. DomBuilder t m => m a -> m a
eStrongN children = snd <$> eStrongN' children

eStrongD' :: forall t m a. MonadWidget t m => Dynamic t EStrong -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStrongD' b  = elDynAttr' "strong" (attrMap <$> b)

eStrongD :: forall t m a. MonadWidget t m => Dynamic t EStrong -> m a -> m a
eStrongD b children = snd <$> eStrongD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Sub-element has only the global attributes.
data ESub = ESub
  { _eSubGlobals :: Maybe Globals
  , _eSubCustom  :: Maybe Attr
  }


instance AttrMap ESub where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSubGlobals b
    ] <> maybe [] (\a -> [a]) (_eSubCustom b)


instance Default ESub where
  def = ESub def def

instance Monoid ESub where
  mempty = def
  mappend (ESub a1 a2) (ESub b1 b2) = ESub (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESub where
   attrSetGlobals p b = b { _eSubGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESub
  where attrSetAccessKey p g = g { _eSubGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasAnmval ESub
  where attrSetAnmval p g = g { _eSubGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasContentEditable ESub
  where attrSetContentEditable p g = g  { _eSubGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasContextMenu ESub
  where attrSetContextMenu p g     = g { _eSubGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasClass ESub
  where attrSetClassName p g           = g { _eSubGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasDnmval ESub
  where attrSetDnmval p g           = g { _eSubGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasDir ESub
  where attrSetDir p g             = g { _eSubGlobals = Just (attrSetDir p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasDraggable ESub
  where attrSetDraggable p g       = g { _eSubGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasHidden ESub
  where attrSetHidden p g          = g { _eSubGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasId ESub
  where attrSetId p g              = g { _eSubGlobals = Just (attrSetId p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasLang ESub
  where attrSetLang p g            = g { _eSubGlobals = Just (attrSetLang p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasRole ESub
  where attrSetRole p g            = g { _eSubGlobals = Just (attrSetRole p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasSlot ESub
  where attrSetSlot p g            = g { _eSubGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasSpellCheck ESub
  where attrSetSpellCheck p g      = g { _eSubGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasStyle ESub
  where attrSetStyle p g           = g { _eSubGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasTabIndex ESub
  where attrSetTabIndex p g        = g { _eSubGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasTitle ESub
  where attrSetTitle p g           = g { _eSubGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSubGlobals g))) }
instance AttrHasTranslate ESub
  where attrSetTranslate p g       = g { _eSubGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSubGlobals g))) }

instance AttrGetClassName ESub where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSubGlobals g)

instance AttrHasCustom ESub where attrSetCustom p g       = g { _eSubCustom = Just p }

eSub' :: forall t m a. DomBuilder t m => ESub -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSub' b  = elAttr' "sub" (attrMap b)

eSub :: forall t m a. DomBuilder t m => ESub -> m a -> m a
eSub b children = snd <$> eSub' b children

eSubN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSubN' = el' "sub"

eSubN :: forall t m a. DomBuilder t m => m a -> m a
eSubN children = snd <$> eSubN' children

eSubD' :: forall t m a. MonadWidget t m => Dynamic t ESub -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSubD' b  = elDynAttr' "sub" (attrMap <$> b)

eSubD :: forall t m a. MonadWidget t m => Dynamic t ESub -> m a -> m a
eSubD b children = snd <$> eSubD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Summary-element has only the global attributes.
data ESummary = ESummary
  { _eSummaryGlobals :: Maybe Globals
  , _eSummaryCustom  :: Maybe Attr
  }


instance AttrMap ESummary where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSummaryGlobals b
    ] <> maybe [] (\a -> [a]) (_eSummaryCustom b)


instance Default ESummary where
  def = ESummary def def

instance Monoid ESummary where
  mempty = def
  mappend (ESummary a1 a2) (ESummary b1 b2) = ESummary (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESummary where
   attrSetGlobals p b = b { _eSummaryGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESummary
  where attrSetAccessKey p g = g { _eSummaryGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasAnmval ESummary
  where attrSetAnmval p g = g { _eSummaryGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasContentEditable ESummary
  where attrSetContentEditable p g = g  { _eSummaryGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasContextMenu ESummary
  where attrSetContextMenu p g     = g { _eSummaryGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasClass ESummary
  where attrSetClassName p g           = g { _eSummaryGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasDnmval ESummary
  where attrSetDnmval p g           = g { _eSummaryGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasDir ESummary
  where attrSetDir p g             = g { _eSummaryGlobals = Just (attrSetDir p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasDraggable ESummary
  where attrSetDraggable p g       = g { _eSummaryGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasHidden ESummary
  where attrSetHidden p g          = g { _eSummaryGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasId ESummary
  where attrSetId p g              = g { _eSummaryGlobals = Just (attrSetId p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasLang ESummary
  where attrSetLang p g            = g { _eSummaryGlobals = Just (attrSetLang p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasRole ESummary
  where attrSetRole p g            = g { _eSummaryGlobals = Just (attrSetRole p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasSlot ESummary
  where attrSetSlot p g            = g { _eSummaryGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasSpellCheck ESummary
  where attrSetSpellCheck p g      = g { _eSummaryGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasStyle ESummary
  where attrSetStyle p g           = g { _eSummaryGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasTabIndex ESummary
  where attrSetTabIndex p g        = g { _eSummaryGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasTitle ESummary
  where attrSetTitle p g           = g { _eSummaryGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSummaryGlobals g))) }
instance AttrHasTranslate ESummary
  where attrSetTranslate p g       = g { _eSummaryGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSummaryGlobals g))) }

instance AttrGetClassName ESummary where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSummaryGlobals g)

instance AttrHasCustom ESummary where attrSetCustom p g       = g { _eSummaryCustom = Just p }

eSummary' :: forall t m a. DomBuilder t m => ESummary -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSummary' b  = elAttr' "summary" (attrMap b)

eSummary :: forall t m a. DomBuilder t m => ESummary -> m a -> m a
eSummary b children = snd <$> eSummary' b children

eSummaryN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSummaryN' = el' "summary"

eSummaryN :: forall t m a. DomBuilder t m => m a -> m a
eSummaryN children = snd <$> eSummaryN' children

eSummaryD' :: forall t m a. MonadWidget t m => Dynamic t ESummary -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSummaryD' b  = elDynAttr' "summary" (attrMap <$> b)

eSummaryD :: forall t m a. MonadWidget t m => Dynamic t ESummary -> m a -> m a
eSummaryD b children = snd <$> eSummaryD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Sup-element has only the global attributes.
data ESup = ESup
  { _eSupGlobals :: Maybe Globals
  , _eSupCustom  :: Maybe Attr
  }


instance AttrMap ESup where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSupGlobals b
    ] <> maybe [] (\a -> [a]) (_eSupCustom b)


instance Default ESup where
  def = ESup def def

instance Monoid ESup where
  mempty = def
  mappend (ESup a1 a2) (ESup b1 b2) = ESup (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESup where
   attrSetGlobals p b = b { _eSupGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESup
  where attrSetAccessKey p g = g { _eSupGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasAnmval ESup
  where attrSetAnmval p g = g { _eSupGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasContentEditable ESup
  where attrSetContentEditable p g = g  { _eSupGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasContextMenu ESup
  where attrSetContextMenu p g     = g { _eSupGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasClass ESup
  where attrSetClassName p g           = g { _eSupGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasDnmval ESup
  where attrSetDnmval p g           = g { _eSupGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasDir ESup
  where attrSetDir p g             = g { _eSupGlobals = Just (attrSetDir p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasDraggable ESup
  where attrSetDraggable p g       = g { _eSupGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasHidden ESup
  where attrSetHidden p g          = g { _eSupGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasId ESup
  where attrSetId p g              = g { _eSupGlobals = Just (attrSetId p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasLang ESup
  where attrSetLang p g            = g { _eSupGlobals = Just (attrSetLang p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasRole ESup
  where attrSetRole p g            = g { _eSupGlobals = Just (attrSetRole p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasSlot ESup
  where attrSetSlot p g            = g { _eSupGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasSpellCheck ESup
  where attrSetSpellCheck p g      = g { _eSupGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasStyle ESup
  where attrSetStyle p g           = g { _eSupGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasTabIndex ESup
  where attrSetTabIndex p g        = g { _eSupGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasTitle ESup
  where attrSetTitle p g           = g { _eSupGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSupGlobals g))) }
instance AttrHasTranslate ESup
  where attrSetTranslate p g       = g { _eSupGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSupGlobals g))) }

instance AttrGetClassName ESup where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSupGlobals g)

instance AttrHasCustom ESup where attrSetCustom p g       = g { _eSupCustom = Just p }

eSup' :: forall t m a. DomBuilder t m => ESup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSup' b  = elAttr' "sup" (attrMap b)

eSup :: forall t m a. DomBuilder t m => ESup -> m a -> m a
eSup b children = snd <$> eSup' b children

eSupN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSupN' = el' "sup"

eSupN :: forall t m a. DomBuilder t m => m a -> m a
eSupN children = snd <$> eSupN' children

eSupD' :: forall t m a. MonadWidget t m => Dynamic t ESup -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSupD' b  = elDynAttr' "sup" (attrMap <$> b)

eSupD :: forall t m a. MonadWidget t m => Dynamic t ESup -> m a -> m a
eSupD b children = snd <$> eSupD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Time-element
data ETime = ETime
  { _eTimeGlobals  :: Maybe Globals
  , _eTimeDateTime :: Maybe DateTime
  , _eTimeCustom   :: Maybe Attr
  }


instance AttrMap ETime where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTimeGlobals b
    , attrMap <$> _eTimeDateTime b
    ] <> maybe [] (\a -> [a]) (_eTimeCustom b)


instance Default ETime where
  def = ETime def def def

instance Monoid ETime where
  mempty = def
  mappend (ETime a1 a2 a3) (ETime b1 b2 b3)
    = ETime (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals ETime where
   attrSetGlobals p b = b { _eTimeGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETime
  where attrSetAccessKey p g = g { _eTimeGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasAnmval ETime
  where attrSetAnmval p g = g { _eTimeGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasContentEditable ETime
  where attrSetContentEditable p g = g  { _eTimeGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasContextMenu ETime
  where attrSetContextMenu p g     = g { _eTimeGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasClass ETime
  where attrSetClassName p g           = g { _eTimeGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasDnmval ETime
  where attrSetDnmval p g           = g { _eTimeGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasDir ETime
  where attrSetDir p g             = g { _eTimeGlobals = Just (attrSetDir p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasDraggable ETime
  where attrSetDraggable p g       = g { _eTimeGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasHidden ETime
  where attrSetHidden p g          = g { _eTimeGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasId ETime
  where attrSetId p g              = g { _eTimeGlobals = Just (attrSetId p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasLang ETime
  where attrSetLang p g            = g { _eTimeGlobals = Just (attrSetLang p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasRole ETime
  where attrSetRole p g            = g { _eTimeGlobals = Just (attrSetRole p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasSlot ETime
  where attrSetSlot p g            = g { _eTimeGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasSpellCheck ETime
  where attrSetSpellCheck p g      = g { _eTimeGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasStyle ETime
  where attrSetStyle p g           = g { _eTimeGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasTabIndex ETime
  where attrSetTabIndex p g        = g { _eTimeGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasTitle ETime
  where attrSetTitle p g           = g { _eTimeGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTimeGlobals g))) }
instance AttrHasTranslate ETime
  where attrSetTranslate p g       = g { _eTimeGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTimeGlobals g))) }

instance AttrGetClassName ETime where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTimeGlobals g)

instance AttrHasDateTime ETime where attrSetDateTime p g = g {_eTimeDateTime = Just p }

instance AttrHasCustom ETime where attrSetCustom p g       = g { _eTimeCustom = Just p }

eTime' :: forall t m a. DomBuilder t m => ETime -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTime' b  = elAttr' "time" (attrMap b)

eTime :: forall t m a. DomBuilder t m => ETime -> m a -> m a
eTime b children = snd <$> eTime' b children

eTimeN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTimeN' = el' "time"

eTimeN :: forall t m a. DomBuilder t m => m a -> m a
eTimeN children = snd <$> eTimeN' children

eTimeD' :: forall t m a. MonadWidget t m => Dynamic t ETime -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTimeD' b  = elDynAttr' "time" (attrMap <$> b)

eTimeD :: forall t m a. MonadWidget t m => Dynamic t ETime -> m a -> m a
eTimeD b children = snd <$> eTimeD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | U-element has only the global attributes.
data EU = EU
  { _eUGlobals :: Maybe Globals
  , _eUCustom  :: Maybe Attr
  }


instance AttrMap EU where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eUGlobals b
    ] <> maybe [] (\a -> [a]) (_eUCustom b)


instance Default EU where
  def = EU def def

instance Monoid EU where
  mempty = def
  mappend (EU a1 a2) (EU b1 b2) = EU (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EU where
   attrSetGlobals p b = b { _eUGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EU
  where attrSetAccessKey p g = g { _eUGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasAnmval EU
  where attrSetAnmval p g = g { _eUGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasContentEditable EU
  where attrSetContentEditable p g = g  { _eUGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasContextMenu EU
  where attrSetContextMenu p g     = g { _eUGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasClass EU
  where attrSetClassName p g           = g { _eUGlobals = Just (attrSetClassName p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasDnmval EU
  where attrSetDnmval p g           = g { _eUGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasDir EU
  where attrSetDir p g             = g { _eUGlobals = Just (attrSetDir p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasDraggable EU
  where attrSetDraggable p g       = g { _eUGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasHidden EU
  where attrSetHidden p g          = g { _eUGlobals = Just (attrSetHidden p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasId EU
  where attrSetId p g              = g { _eUGlobals = Just (attrSetId p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasLang EU
  where attrSetLang p g            = g { _eUGlobals = Just (attrSetLang p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasRole EU
  where attrSetRole p g            = g { _eUGlobals = Just (attrSetRole p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasSlot EU
  where attrSetSlot p g            = g { _eUGlobals = Just (attrSetSlot p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasSpellCheck EU
  where attrSetSpellCheck p g      = g { _eUGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasStyle EU
  where attrSetStyle p g           = g { _eUGlobals = Just (attrSetStyle p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasTabIndex EU
  where attrSetTabIndex p g        = g { _eUGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasTitle EU
  where attrSetTitle p g           = g { _eUGlobals = Just (attrSetTitle p (fromMaybe gDef (_eUGlobals g))) }
instance AttrHasTranslate EU
  where attrSetTranslate p g       = g { _eUGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eUGlobals g))) }

instance AttrGetClassName EU where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eUGlobals g)

instance AttrHasCustom EU where attrSetCustom p g       = g { _eUCustom = Just p }

eU' :: forall t m a. DomBuilder t m => EU -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eU' b  = elAttr' "u" (attrMap b)

eU :: forall t m a. DomBuilder t m => EU -> m a -> m a
eU b children = snd <$> eU' b children

eUN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eUN' = el' "u"

eUN :: forall t m a. DomBuilder t m => m a -> m a
eUN children = snd <$> eUN' children

eUD' :: forall t m a. MonadWidget t m => Dynamic t EU -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eUD' b  = elDynAttr' "u" (attrMap <$> b)

eUD :: forall t m a. MonadWidget t m => Dynamic t EU -> m a -> m a
eUD b children = snd <$> eUD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ul-element has only the global attributes.
data EUl = EUl
  { _eUlGlobals :: Maybe Globals
  , _eUlCustom  :: Maybe Attr
  }


instance AttrMap EUl where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eUlGlobals b
    ] <> maybe [] (\a -> [a]) (_eUlCustom b)


instance Default EUl where
  def = EUl def def

instance Monoid EUl where
  mempty = def
  mappend (EUl a1 a2) (EUl b1 b2) = EUl (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EUl where
   attrSetGlobals p b = b { _eUlGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EUl
  where attrSetAccessKey p g = g { _eUlGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasAnmval EUl
  where attrSetAnmval p g = g { _eUlGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasContentEditable EUl
  where attrSetContentEditable p g = g  { _eUlGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasContextMenu EUl
  where attrSetContextMenu p g     = g { _eUlGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasClass EUl
  where attrSetClassName p g           = g { _eUlGlobals = Just (attrSetClassName p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasDnmval EUl
  where attrSetDnmval p g           = g { _eUlGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasDir EUl
  where attrSetDir p g             = g { _eUlGlobals = Just (attrSetDir p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasDraggable EUl
  where attrSetDraggable p g       = g { _eUlGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasHidden EUl
  where attrSetHidden p g          = g { _eUlGlobals = Just (attrSetHidden p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasId EUl
  where attrSetId p g              = g { _eUlGlobals = Just (attrSetId p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasLang EUl
  where attrSetLang p g            = g { _eUlGlobals = Just (attrSetLang p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasRole EUl
  where attrSetRole p g            = g { _eUlGlobals = Just (attrSetRole p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasSlot EUl
  where attrSetSlot p g            = g { _eUlGlobals = Just (attrSetSlot p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasSpellCheck EUl
  where attrSetSpellCheck p g      = g { _eUlGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasStyle EUl
  where attrSetStyle p g           = g { _eUlGlobals = Just (attrSetStyle p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasTabIndex EUl
  where attrSetTabIndex p g        = g { _eUlGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasTitle EUl
  where attrSetTitle p g           = g { _eUlGlobals = Just (attrSetTitle p (fromMaybe gDef (_eUlGlobals g))) }
instance AttrHasTranslate EUl
  where attrSetTranslate p g       = g { _eUlGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eUlGlobals g))) }

instance AttrGetClassName EUl where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eUlGlobals g)

instance AttrHasCustom EUl where attrSetCustom p g       = g { _eUlCustom = Just p }

eUl' :: forall t m a. DomBuilder t m => EUl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eUl' b  = elAttr' "ul" (attrMap b)

eUl :: forall t m a. DomBuilder t m => EUl -> m a -> m a
eUl b children = snd <$> eUl' b children

eUlN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eUlN' = el' "ul"

eUlN :: forall t m a. DomBuilder t m => m a -> m a
eUlN children = snd <$> eUlN' children

eUlD' :: forall t m a. MonadWidget t m => Dynamic t EUl -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eUlD' b  = elDynAttr' "ul" (attrMap <$> b)

eUlD :: forall t m a. MonadWidget t m => Dynamic t EUl -> m a -> m a
eUlD b children = snd <$> eUlD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Var-element has only the global attributes.
data EVar = EVar
  { _eVarGlobals :: Maybe Globals
  , _eVarCustom  :: Maybe Attr
  }


instance AttrMap EVar where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eVarGlobals b
    ] <> maybe [] (\a -> [a]) (_eVarCustom b)


instance Default EVar where
  def = EVar def def

instance Monoid EVar where
  mempty = def
  mappend (EVar a1 a2) (EVar b1 b2) = EVar (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EVar where
   attrSetGlobals p b = b { _eVarGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EVar
  where attrSetAccessKey p g = g { _eVarGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasAnmval EVar
  where attrSetAnmval p g = g { _eVarGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasContentEditable EVar
  where attrSetContentEditable p g = g  { _eVarGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasContextMenu EVar
  where attrSetContextMenu p g     = g { _eVarGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasClass EVar
  where attrSetClassName p g           = g { _eVarGlobals = Just (attrSetClassName p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasDnmval EVar
  where attrSetDnmval p g           = g { _eVarGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasDir EVar
  where attrSetDir p g             = g { _eVarGlobals = Just (attrSetDir p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasDraggable EVar
  where attrSetDraggable p g       = g { _eVarGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasHidden EVar
  where attrSetHidden p g          = g { _eVarGlobals = Just (attrSetHidden p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasId EVar
  where attrSetId p g              = g { _eVarGlobals = Just (attrSetId p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasLang EVar
  where attrSetLang p g            = g { _eVarGlobals = Just (attrSetLang p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasRole EVar
  where attrSetRole p g            = g { _eVarGlobals = Just (attrSetRole p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasSlot EVar
  where attrSetSlot p g            = g { _eVarGlobals = Just (attrSetSlot p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasSpellCheck EVar
  where attrSetSpellCheck p g      = g { _eVarGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasStyle EVar
  where attrSetStyle p g           = g { _eVarGlobals = Just (attrSetStyle p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasTabIndex EVar
  where attrSetTabIndex p g        = g { _eVarGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasTitle EVar
  where attrSetTitle p g           = g { _eVarGlobals = Just (attrSetTitle p (fromMaybe gDef (_eVarGlobals g))) }
instance AttrHasTranslate EVar
  where attrSetTranslate p g       = g { _eVarGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eVarGlobals g))) }

instance AttrGetClassName EVar where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eVarGlobals g)

instance AttrHasCustom EVar where attrSetCustom p g       = g { _eVarCustom = Just p }

eVar' :: forall t m a. DomBuilder t m => EVar -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVar' b  = elAttr' "var" (attrMap b)

eVar :: forall t m a. DomBuilder t m => EVar -> m a -> m a
eVar b children = snd <$> eVar' b children

eVarN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVarN' = el' "var"

eVarN :: forall t m a. DomBuilder t m => m a -> m a
eVarN children = snd <$> eVarN' children

eVarD' :: forall t m a. MonadWidget t m => Dynamic t EVar -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVarD' b  = elDynAttr' "var" (attrMap <$> b)

eVarD :: forall t m a. MonadWidget t m => Dynamic t EVar -> m a -> m a
eVarD b children = snd <$> eVarD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Wbr-element has only the global attributes.
data EWbr = EWbr
  { _eWbrGlobals :: Maybe Globals
  , _eWbrCustom  :: Maybe Attr
  }


instance AttrMap EWbr where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eWbrGlobals b
    ] <> maybe [] (\a -> [a]) (_eWbrCustom b)


instance Default EWbr where
  def = EWbr def def

instance Monoid EWbr where
  mempty = def
  mappend (EWbr a1 a2) (EWbr b1 b2) = EWbr (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EWbr where
   attrSetGlobals p b = b { _eWbrGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EWbr
  where attrSetAccessKey p g = g { _eWbrGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasAnmval EWbr
  where attrSetAnmval p g = g { _eWbrGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasContentEditable EWbr
  where attrSetContentEditable p g = g  { _eWbrGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasContextMenu EWbr
  where attrSetContextMenu p g     = g { _eWbrGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasClass EWbr
  where attrSetClassName p g           = g { _eWbrGlobals = Just (attrSetClassName p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasDnmval EWbr
  where attrSetDnmval p g           = g { _eWbrGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasDir EWbr
  where attrSetDir p g             = g { _eWbrGlobals = Just (attrSetDir p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasDraggable EWbr
  where attrSetDraggable p g       = g { _eWbrGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasHidden EWbr
  where attrSetHidden p g          = g { _eWbrGlobals = Just (attrSetHidden p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasId EWbr
  where attrSetId p g              = g { _eWbrGlobals = Just (attrSetId p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasLang EWbr
  where attrSetLang p g            = g { _eWbrGlobals = Just (attrSetLang p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasRole EWbr
  where attrSetRole p g            = g { _eWbrGlobals = Just (attrSetRole p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasSlot EWbr
  where attrSetSlot p g            = g { _eWbrGlobals = Just (attrSetSlot p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasSpellCheck EWbr
  where attrSetSpellCheck p g      = g { _eWbrGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasStyle EWbr
  where attrSetStyle p g           = g { _eWbrGlobals = Just (attrSetStyle p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasTabIndex EWbr
  where attrSetTabIndex p g        = g { _eWbrGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasTitle EWbr
  where attrSetTitle p g           = g { _eWbrGlobals = Just (attrSetTitle p (fromMaybe gDef (_eWbrGlobals g))) }
instance AttrHasTranslate EWbr
  where attrSetTranslate p g       = g { _eWbrGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eWbrGlobals g))) }

instance AttrGetClassName EWbr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eWbrGlobals g)

instance AttrHasCustom EWbr where attrSetCustom p g       = g { _eWbrCustom = Just p }

eWbr' :: forall t m a. DomBuilder t m => EWbr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eWbr' b  = elAttr' "wbr" (attrMap b)

eWbr :: forall t m a. DomBuilder t m => EWbr -> m a -> m a
eWbr b children = snd <$> eWbr' b children

eWbrN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eWbrN' = el' "wbr"

eWbrN :: forall t m a. DomBuilder t m => m a -> m a
eWbrN children = snd <$> eWbrN' children

eWbrD' :: forall t m a. MonadWidget t m => Dynamic t EWbr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eWbrD' b  = elDynAttr' "wbr" (attrMap <$> b)

eWbrD :: forall t m a. MonadWidget t m => Dynamic t EWbr -> m a -> m a
eWbrD b children = snd <$> eWbrD' b children

eWbr_ :: forall t m. DomBuilder t m => m ()
eWbr_ = eWbr def blank

------------------------------------------------------------------------------
------------------------------------------------------------------------------

