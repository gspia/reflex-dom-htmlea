{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Elements.Interactive
Description : HTML5 interactive elements
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Interactive

This module contains the following elements:
button; details; embed; iframe; keygen; label; select; textarea;
and a-element.

Note that tabindex-attribute (in globals) can make any element into
interactive content.

Audio, Iframe, Img, Video elements are in Embedded-module. This
choice is arbitrary.

The naming convention sligthly differs from that of others by
adding 2 other functions for each element.

* eAC - takes attributes and 'm ()', give 'm (Event t ())'
* eACD - takes dynamic attributes and 'm ()', give 'm (Event t ())'

TBD: are the inputs and outputs ok and should something similar
be provided for all elements as the tabindex-attribute can make any
element interactive. (hmm)

'ELabel' has 'eLabelCForId' convenience function.

== Note

You probably don't want to use these definitions but rather the
widgets defined either in reflex-dom or reflex-dom-contrib (or in
some other) library.

(Occasionally, the elements given in this module are convenient,
anyway.)

-}

module Reflex.Dom.HTML5.Elements.Interactive where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, MonadWidget, elAttr',
                       elDynAttr', Event, EventName (Click),
                       domEvent, blank, el')

import Reflex.Dom.HTML5.Attrs
  ( AttrMap, attrMap, AttrHasGlobals, AttrHasDownload, AttrHasHref,
  AttrHasHrefLang, AttrHasRel, AttrHasAccessKey, AttrHasTarget,
  AttrHasButtonType, AttrHasMediaType,
  AttrHasHrefLang, AttrHasRel, AttrHasAnmval, AttrHasTarget,
  AttrHasButtonType, AttrHasMediaType,
  AttrHasAccept, AttrHasAlt, AttrHasAutoComplete,
  AttrHasAutoFocus, AttrHasContentEditable,
  AttrHasContextMenu, AttrHasCols, AttrHasClass,
  AttrHasDir, AttrHasDirName, AttrHasDisabled, AttrHasDraggable,
  AttrHasContextMenu, AttrHasCols, AttrHasDnmval,
  AttrHasDir, AttrHasDirName, AttrHasDisabled, AttrHasDraggable,
  AttrHasDownload, AttrHasId, AttrHasInputMode, AttrHasInputType,
  AttrHasHidden, AttrHasLang, AttrHasList,
  AttrHasSize, AttrHasSpellCheck, AttrHasStyle, AttrHasTabIndex,
  AttrHasHidden, AttrHasRole, AttrHasList,
  AttrHasSize, AttrHasSpellCheck, AttrHasStyle, AttrHasTabIndex,
  AttrHasHidden, AttrHasSlot, AttrHasList,
  AttrHasSize, AttrHasSpellCheck, AttrHasStyle, AttrHasTabIndex,
  AttrHasTranslate, AttrGetClassName, AttrHasTitle, AttrHasSrc,
  AttrHasForm, AttrHasFormAction, AttrHasFormEncType,
  AttrHasFormMethod, AttrHasFormNoValidate, AttrHasFormTarget, AttrHasForId,
  AttrHasName, AttrHasValueText, AttrHasHeight, AttrHasWidth, AttrHasChecked,
  AttrHasMin, AttrHasMax, AttrHasMaxLength, AttrHasMinLength, AttrHasMultiple,
  AttrHasPattern, AttrHasPlaceholder, AttrHasReadOnly, AttrHasRequired,
  AttrHasRel, AttrHasStep, AttrHasRows, AttrHasWrap,
  AttrHasOpen,
  AttrHasAnmval, AttrHasDnmval, AttrHasRole, AttrHasSlot,
  Globals, Download, Href, HrefLang, Rel, Target, MediaType, Src,
  AutoFocus, Disabled, FormAction,
  FormEncType, FormNoValidate, FormTarget, Name, ButtonType, Form, ValueText,
  Height, Width, Width, Height, Alt_,
  Accept, AutoComplete, Checked, DirName,
  FormMethod, InputMode, List, Max, MaxLength, Min, MinLength, Multiple,
  Pattern, Placeholder, ReadOnly, Required, Size, Step, InputType, ForId,
  Cols, Rows, Wrap, Open,
  attrSetAccessKey, attrSetContentEditable, attrSetContextMenu,
  attrSetClassName, attrSetDir, attrSetDraggable, attrSetHidden, attrSetId,
  attrSetLang, attrSetSpellCheck, attrSetStyle, attrSetTabIndex, attrSetTitle,
  attrSetTranslate, attrGetClassName, attrSetDownload, attrSetHref, attrSetHrefLang,
  attrSetRel, attrSetTarget, attrSetMediaType, attrSetSrc,
  attrSetAutoFocus, attrSetDisabled, attrSetForm, attrSetFormAction,
  attrSetFormEncType, attrSetFormNoValidate, attrSetFormTarget, attrSetName,
  attrSetButtonType, attrSetValueText, attrSetHeight, attrSetWidth,
  attrSetAlt, attrSetAccept,
  attrSetAutoComplete, attrSetChecked, attrSetDirName, attrSetFormMethod,
  attrSetInputMode, attrSetList, attrSetMax, attrSetMaxLength, attrSetMultiple,
  attrSetPattern, attrSetPlaceholder, attrSetReadOnly, attrSetRequired,
  attrSetSize, attrSetStep, attrSetInputType, attrSetForId, attrSetGlobals,
  attrSetMin, attrSetMinLength, attrSetCols, attrSetRows, attrSetWrap,
  attrSetOpen,
  attrSetAnmval, attrSetDnmval, attrSetRole, attrSetSlot,
  gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | A-element is an interactive element, if its Href-attribute is defined.
-- A-element has the following attributes:
-- globals; href; target; download; rel; hreflang; type
-- Type-attribute is of media_type.
-- Look at IANA Media Types for a complete list of standard media types.
-- http://www.iana.org/assignments/media-types/media-types.xhtml
--
-- Media-attribute?
--
-- Charset, coords, name, rev and shape attributes are not supported in HTML5.
--
data EA = EA
  { _eAGlobals   :: Maybe Globals
  , _eADownload  :: Maybe Download
  , _eAHref      :: Maybe Href
  , _eAHrefLang  :: Maybe HrefLang
  , _eARel       :: Maybe Rel
  , _eATarget    :: Maybe Target
  , _eAMediaType :: Maybe MediaType
  , _eACustom    :: Maybe Attr
  }

instance AttrMap EA where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAGlobals b
    , attrMap <$> _eADownload b
    , attrMap <$> _eAHref b
    , attrMap <$> _eAHrefLang b
    , attrMap <$> _eARel b
    , attrMap <$> _eATarget b
    , attrMap <$> _eAMediaType b
    ] <> maybe [] (\a -> [a]) (_eACustom b)


instance Default EA where
  def = EA def def def def def def def def

instance Monoid EA where
  mempty = def
  mappend (EA a1 a2 a3 a4 a5 a6 a7 a8) (EA b1 b2 b3 b4 b5 b6 b7 b8)
    = EA (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8)


instance AttrHasGlobals EA where
   attrSetGlobals p b = b { _eAGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EA
  where attrSetAccessKey p g = g { _eAGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasAnmval EA
  where attrSetAnmval p g = g { _eAGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasContentEditable EA
  where attrSetContentEditable p g = g  { _eAGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasContextMenu EA
  where attrSetContextMenu p g     = g { _eAGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasClass EA
  where attrSetClassName p g           = g { _eAGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasDnmval EA
  where attrSetDnmval p g           = g { _eAGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasDir EA
  where attrSetDir p g             = g { _eAGlobals = Just (attrSetDir p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasDraggable EA
  where attrSetDraggable p g       = g { _eAGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasHidden EA
  where attrSetHidden p g          = g { _eAGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasId EA
  where attrSetId p g              = g { _eAGlobals = Just (attrSetId p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasLang EA
  where attrSetLang p g            = g { _eAGlobals = Just (attrSetLang p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasRole EA
  where attrSetRole p g            = g { _eAGlobals = Just (attrSetRole p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasSlot EA
  where attrSetSlot p g            = g { _eAGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasSpellCheck EA
  where attrSetSpellCheck p g      = g { _eAGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasStyle EA
  where attrSetStyle p g           = g { _eAGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasTabIndex EA
  where attrSetTabIndex p g        = g { _eAGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasTitle EA
  where attrSetTitle p g           = g { _eAGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAGlobals g))) }
instance AttrHasTranslate EA
  where attrSetTranslate p g       = g { _eAGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAGlobals g))) }

instance AttrGetClassName EA where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAGlobals g)

instance AttrHasDownload  EA where attrSetDownload p g  = g { _eADownload  = Just p }
instance AttrHasHref      EA where attrSetHref p g      = g { _eAHref      = Just p }
instance AttrHasHrefLang  EA where attrSetHrefLang p g  = g { _eAHrefLang  = Just p }
instance AttrHasRel       EA where attrSetRel p g       = g { _eARel       = Just p }
instance AttrHasTarget    EA where attrSetTarget p g    = g { _eATarget    = Just p }
instance AttrHasMediaType EA where attrSetMediaType p g = g { _eAMediaType = Just p }

instance AttrHasCustom EA where attrSetCustom p g       = g { _eACustom = Just p }

-- instance (Reflex t, AttrHasAccessKey a) => AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) => AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (attrSetAnmval c)

eA' :: forall t m a. DomBuilder t m => EA -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eA' b  = elAttr' "a" (attrMap b)

eA :: forall t m a. DomBuilder t m => EA -> m a -> m a
eA b children = snd <$> eA' b children

eAN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAN' = el' "a"

eAN :: forall t m a. DomBuilder t m => m a -> m a
eAN children = snd <$> eAN' children

eAD' :: forall t m a. MonadWidget t m => Dynamic t EA -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAD' b  = elDynAttr' "a" (attrMap <$> b)

eAD :: forall t m a. MonadWidget t m => Dynamic t EA -> m a -> m a
eAD b children = snd <$> eAD' b children


eAC :: MonadWidget t m => EA -> m () -> m (Event t ())
eAC b children = do
  (e,_) <- elAttr' "a" (attrMap b) children
  return $ domEvent Click e
eACD :: MonadWidget t m => Dynamic t EA -> m () -> m (Event t ())
eACD bDyn children = do
  (e,_) <- elDynAttr' "a" (attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Button-element has the following attributes:
-- autofocus; disabled; form; formaction; formenctype; formmethod;
-- formnovalidate; formtarget; name; type; value
--
-- Value of button is Text (inside Value_).
--
data EButton = EButton
  { _eButtonGlobals        :: Maybe Globals
  , _eButtonAutoFocus      :: Maybe AutoFocus
  , _eButtonDisabled       :: Maybe Disabled
  , _eButtonForm           :: Maybe Form
  , _eButtonFormAction     :: Maybe FormAction
  , _eButtonFormEncType    :: Maybe FormEncType
  , _eButtonFormNoValidate :: Maybe FormNoValidate
  , _eButtonFormTarget     :: Maybe FormTarget
  , _eButtonName           :: Maybe Name
  , _eButtonType           :: Maybe ButtonType
  , _eButtonValueText      :: Maybe ValueText
  , _eButtonCustom         :: Maybe Attr
  }


instance AttrMap EButton where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eButtonGlobals b
    , attrMap <$> _eButtonAutoFocus b
    , attrMap <$> _eButtonDisabled b
    , attrMap <$> _eButtonForm b
    , attrMap <$> _eButtonFormAction b
    , attrMap <$> _eButtonFormEncType b
    , attrMap <$> _eButtonFormNoValidate b
    , attrMap <$> _eButtonFormTarget b
    , attrMap <$> _eButtonName b
    , attrMap <$> _eButtonType b
    , attrMap <$> _eButtonValueText b
    ] <> maybe [] (\a -> [a]) (_eButtonCustom b)

instance Default EButton where
  def = EButton def def def def def def def def def def def def

instance Monoid EButton where
  mempty = def
  mappend (EButton a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (EButton b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = EButton (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <>b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)


instance AttrHasGlobals EButton where
   attrSetGlobals p b = b { _eButtonGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EButton
  where attrSetAccessKey p g = g { _eButtonGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasAnmval EButton
  where attrSetAnmval p g = g { _eButtonGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasContentEditable EButton
  where attrSetContentEditable p g = g  { _eButtonGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasContextMenu EButton
  where attrSetContextMenu p g     = g { _eButtonGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasClass EButton
  where attrSetClassName p g           = g { _eButtonGlobals = Just (attrSetClassName p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasDnmval EButton
  where attrSetDnmval p g           = g { _eButtonGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasDir EButton
  where attrSetDir p g             = g { _eButtonGlobals = Just (attrSetDir p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasDraggable EButton
  where attrSetDraggable p g       = g { _eButtonGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasHidden EButton
  where attrSetHidden p g          = g { _eButtonGlobals = Just (attrSetHidden p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasId EButton
  where attrSetId p g              = g { _eButtonGlobals = Just (attrSetId p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasLang EButton
  where attrSetLang p g            = g { _eButtonGlobals = Just (attrSetLang p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasRole EButton
  where attrSetRole p g            = g { _eButtonGlobals = Just (attrSetRole p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasSlot EButton
  where attrSetSlot p g            = g { _eButtonGlobals = Just (attrSetSlot p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasSpellCheck EButton
  where attrSetSpellCheck p g      = g { _eButtonGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasStyle EButton
  where attrSetStyle p g           = g { _eButtonGlobals = Just (attrSetStyle p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasTabIndex EButton
  where attrSetTabIndex p g        = g { _eButtonGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasTitle EButton
  where attrSetTitle p g           = g { _eButtonGlobals = Just (attrSetTitle p (fromMaybe gDef (_eButtonGlobals g))) }
instance AttrHasTranslate EButton
  where attrSetTranslate p g       = g { _eButtonGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eButtonGlobals g))) }

instance AttrGetClassName EButton where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eButtonGlobals g)


instance AttrHasAutoFocus      EButton where attrSetAutoFocus p g   = g { _eButtonAutoFocus   = Just p }
instance AttrHasDisabled       EButton where attrSetDisabled p g    = g { _eButtonDisabled    = Just p }
instance AttrHasForm           EButton where attrSetForm p g        = g { _eButtonForm        = Just p }
instance AttrHasFormAction     EButton where attrSetFormAction p g  = g { _eButtonFormAction  = Just p }
instance AttrHasFormEncType    EButton where attrSetFormEncType p g = g { _eButtonFormEncType = Just p }
instance AttrHasFormNoValidate EButton where attrSetFormNoValidate p g  = g { _eButtonFormNoValidate  = Just p }
instance AttrHasFormTarget     EButton where attrSetFormTarget p g  = g { _eButtonFormTarget  = Just p }
instance AttrHasName           EButton where attrSetName p g        = g { _eButtonName        = Just p }
instance AttrHasButtonType     EButton where attrSetButtonType p g  = g { _eButtonType        = Just p }
instance AttrHasValueText      EButton where attrSetValueText p g       = g { _eButtonValueText   = Just p }

instance AttrHasCustom EButton where attrSetCustom p g       = g { _eButtonCustom = Just p }

-- instance (Reflex t, AttrHasAccessKey a) => AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) => AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (attrSetAnmval c)

eButton' :: forall t m a. DomBuilder t m => EButton -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eButton' b  = elAttr' "button" (attrMap b)

eButton :: forall t m a. DomBuilder t m => EButton -> m a -> m a
eButton b children = snd <$> eButton' b children

eButtonN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eButtonN' = el' "button"

eButtonN :: forall t m a. DomBuilder t m => m a -> m a
eButtonN children = snd <$> eButtonN' children

eButtonD' :: forall t m a. MonadWidget t m => Dynamic t EButton -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eButtonD' b  = elDynAttr' "button" (attrMap <$> b)

eButtonD :: forall t m a. MonadWidget t m => Dynamic t EButton -> m a -> m a
eButtonD b children = snd <$> eButtonD' b children


eButtonC :: MonadWidget t m => EButton -> m () -> m (Event t ())
eButtonC b children = do
  (e,_) <- elAttr' "button" (attrMap b) children
  return $ domEvent Click e

eButtonCD :: MonadWidget t m => Dynamic t EButton -> m () -> m (Event t ())
eButtonCD bDyn children = do
  (e,_) <- elDynAttr' "button" (attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Details-element
data EDetails = EDetails
  { _eDetailsGlobals :: Maybe Globals
  , _eDetailsOpen    :: Maybe Open
  , _eDetailsCustom  :: Maybe Attr
  }


instance AttrMap EDetails where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eDetailsGlobals b
    , attrMap <$> _eDetailsOpen b
    ] <> maybe [] (\a -> [a]) (_eDetailsCustom b)

instance Default EDetails where
  def = EDetails def def def

instance Monoid EDetails where
  mempty = def
  mappend (EDetails a1 a2 a3) (EDetails b1 b2 b3)
    = EDetails (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EDetails where
   attrSetGlobals p b = b { _eDetailsGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EDetails
  where attrSetAccessKey p g = g { _eDetailsGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasAnmval EDetails
  where attrSetAnmval p g = g { _eDetailsGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasContentEditable EDetails
  where attrSetContentEditable p g = g  { _eDetailsGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasContextMenu EDetails
  where attrSetContextMenu p g     = g { _eDetailsGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasClass EDetails
  where attrSetClassName p g           = g { _eDetailsGlobals = Just (attrSetClassName p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasDnmval EDetails
  where attrSetDnmval p g           = g { _eDetailsGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasDir EDetails
  where attrSetDir p g             = g { _eDetailsGlobals = Just (attrSetDir p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasDraggable EDetails
  where attrSetDraggable p g       = g { _eDetailsGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasHidden EDetails
  where attrSetHidden p g          = g { _eDetailsGlobals = Just (attrSetHidden p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasId EDetails
  where attrSetId p g              = g { _eDetailsGlobals = Just (attrSetId p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasLang EDetails
  where attrSetLang p g            = g { _eDetailsGlobals = Just (attrSetLang p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasRole EDetails
  where attrSetRole p g            = g { _eDetailsGlobals = Just (attrSetRole p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasSlot EDetails
  where attrSetSlot p g            = g { _eDetailsGlobals = Just (attrSetSlot p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasSpellCheck EDetails
  where attrSetSpellCheck p g      = g { _eDetailsGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasStyle EDetails
  where attrSetStyle p g           = g { _eDetailsGlobals = Just (attrSetStyle p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasTabIndex EDetails
  where attrSetTabIndex p g        = g { _eDetailsGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasTitle EDetails
  where attrSetTitle p g           = g { _eDetailsGlobals = Just (attrSetTitle p (fromMaybe gDef (_eDetailsGlobals g))) }
instance AttrHasTranslate EDetails
  where attrSetTranslate p g       = g { _eDetailsGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eDetailsGlobals g))) }

instance AttrGetClassName EDetails where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eDetailsGlobals g)

instance AttrHasOpen EDetails where attrSetOpen p g = g {_eDetailsOpen = Just p }

instance AttrHasCustom EDetails where attrSetCustom p g       = g { _eDetailsCustom = Just p }

eDetails' :: forall t m a. DomBuilder t m => EDetails -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDetails' b  = elAttr' "details" (attrMap b)

eDetails :: forall t m a. DomBuilder t m => EDetails -> m a -> m a
eDetails b children = snd <$> eDetails' b children

eDetailsN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDetailsN' = el' "details"

eDetailsN :: forall t m a. DomBuilder t m => m a -> m a
eDetailsN children = snd <$> eDetailsN' children

eDetailsD' :: forall t m a. MonadWidget t m => Dynamic t EDetails -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eDetailsD' b  = elDynAttr' "details" (attrMap <$> b)

eDetailsD :: forall t m a. MonadWidget t m => Dynamic t EDetails -> m a -> m a
eDetailsD b children = snd <$> eDetailsD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Embed-element has the following attributes:
-- globals; src; type; width; height; any*
-- Type-attribute is of media_type.
--
-- Any namespace-less attribute other than name, align, hspace, and vspace may
-- be specified on the embed element, so long as its name is XML-compatible and
-- contains no uppercase ASCII letters. These attributes are then passed as
-- parameters to the plugin.
--
data EEmbed = EEmbed
  { _eEmbedGlobals   :: Maybe Globals
  , _eEmbedHeight    :: Maybe Height
  , _eEmbedMediaType :: Maybe MediaType
  , _eEmbedSrc       :: Maybe Src
  , _eEmbedWidth     :: Maybe Width
  , _eEmbedCustom    :: Maybe Attr
  }


instance AttrMap EEmbed where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eEmbedGlobals b
    , attrMap <$> _eEmbedHeight b
    , attrMap <$> _eEmbedMediaType b
    , attrMap <$> _eEmbedSrc b
    , attrMap <$> _eEmbedWidth b
    ] <> maybe [] (\a -> [a]) (_eEmbedCustom b)

instance Default EEmbed where
  def = EEmbed def def def def def def

instance Monoid EEmbed where
  mempty = def
  mappend (EEmbed a1 a2 a3 a4 a5 a6) (EEmbed b1 b2 b3 b4 b5 b6)
    = EEmbed (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)


instance AttrHasGlobals EEmbed where
   attrSetGlobals p b = b { _eEmbedGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EEmbed
  where attrSetAccessKey p g = g { _eEmbedGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasAnmval EEmbed
  where attrSetAnmval p g = g { _eEmbedGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasContentEditable EEmbed
  where attrSetContentEditable p g = g  { _eEmbedGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasContextMenu EEmbed
  where attrSetContextMenu p g     = g { _eEmbedGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasClass EEmbed
  where attrSetClassName p g           = g { _eEmbedGlobals = Just (attrSetClassName p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasDnmval EEmbed
  where attrSetDnmval p g           = g { _eEmbedGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasDir EEmbed
  where attrSetDir p g             = g { _eEmbedGlobals = Just (attrSetDir p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasDraggable EEmbed
  where attrSetDraggable p g       = g { _eEmbedGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasHidden EEmbed
  where attrSetHidden p g          = g { _eEmbedGlobals = Just (attrSetHidden p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasId EEmbed
  where attrSetId p g              = g { _eEmbedGlobals = Just (attrSetId p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasLang EEmbed
  where attrSetLang p g            = g { _eEmbedGlobals = Just (attrSetLang p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasRole EEmbed
  where attrSetRole p g            = g { _eEmbedGlobals = Just (attrSetRole p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasSlot EEmbed
  where attrSetSlot p g            = g { _eEmbedGlobals = Just (attrSetSlot p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasSpellCheck EEmbed
  where attrSetSpellCheck p g      = g { _eEmbedGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasStyle EEmbed
  where attrSetStyle p g           = g { _eEmbedGlobals = Just (attrSetStyle p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasTabIndex EEmbed
  where attrSetTabIndex p g        = g { _eEmbedGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasTitle EEmbed
  where attrSetTitle p g           = g { _eEmbedGlobals = Just (attrSetTitle p (fromMaybe gDef (_eEmbedGlobals g))) }
instance AttrHasTranslate EEmbed
  where attrSetTranslate p g       = g { _eEmbedGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eEmbedGlobals g))) }

instance AttrGetClassName EEmbed where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eEmbedGlobals g)

instance AttrHasHeight    EEmbed where attrSetHeight p g    = g { _eEmbedHeight    = Just p }
instance AttrHasMediaType EEmbed where attrSetMediaType p g = g { _eEmbedMediaType = Just p }
instance AttrHasSrc       EEmbed where attrSetSrc p g       = g { _eEmbedSrc       = Just p }
instance AttrHasWidth     EEmbed where attrSetWidth p g     = g { _eEmbedWidth     = Just p }

instance AttrHasCustom EEmbed where attrSetCustom p g       = g { _eEmbedCustom = Just p }

-- instance (Reflex t, AttrHasAccessKey a) => AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) => AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (attrSetAnmval c)

eEmbed' :: forall t m a. DomBuilder t m => EEmbed -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEmbed' b  = elAttr' "embed" (attrMap b)

eEmbed :: forall t m a. DomBuilder t m => EEmbed -> m a -> m a
eEmbed b children = snd <$> eEmbed' b children

eEmbedN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEmbedN' = el' "embed"

eEmbedN :: forall t m a. DomBuilder t m => m a -> m a
eEmbedN children = snd <$> eEmbedN' children

eEmbedD' :: forall t m a. MonadWidget t m => Dynamic t EEmbed -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eEmbedD' b  = elDynAttr' "embed" (attrMap <$> b)

eEmbedD :: forall t m a. MonadWidget t m => Dynamic t EEmbed -> m a -> m a
eEmbedD b children = snd <$> eEmbedD' b children


-- uiEmbedC :: MonadWidget t m => EEmbed -> m () -> m (Event t ())
-- uiEmbedC b children = do
eEmbedC :: MonadWidget t m => EEmbed -> m (Event t ())
eEmbedC b = do
  (e,_) <- elAttr' "embed" (attrMap b) blank
  return $ domEvent Click e

-- uiEmbedD :: MonadWidget t m => Dynamic t EEmbed -> m () -> m (Event t ())
-- uiEmbedD bDyn children = do
eEmbedCD :: MonadWidget t m => Dynamic t EEmbed -> m (Event t ())
eEmbedCD bDyn = do
  (e,_) <- elDynAttr' "embed" (attrMap <$> bDyn) blank
  return $ domEvent Click e




------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Input-element is interactive  if the type-attribute is not in hidden
-- state).
-- globals; accept; alt; autocomplete; autofocus; checked; dirname; disabled;
-- form; formaction; formenctype; formmethod; formnovalidate; formtarget;
-- height; inputmode; list; max; maxlength; min; minlength; multiple; name;
-- pattern; placeholder; readonly; required; size; src; step; type; value;
-- width
--
data EInput = EInput
  { _eInputGlobals        :: Maybe Globals        -- 1.
  , _eInputAccept         :: Maybe Accept         -- 2.
  , _eInputAlt            :: Maybe Alt_           -- 3.
  , _eInputAutoComplete   :: Maybe AutoComplete   -- 4.
  , _eInputAutoFocus      :: Maybe AutoFocus      -- 5.
  , _eInputChecked        :: Maybe Checked        -- 6.
  , _eInputDirName        :: Maybe DirName        -- 7.
  , _eInputDisabled       :: Maybe Disabled       -- 8.
  , _eInputForm           :: Maybe Form           -- 9.
  , _eInputFormAction     :: Maybe FormAction     -- 10.
  , _eInputFormEncType    :: Maybe FormEncType    -- 11.
  , _eInputFormMethod     :: Maybe FormMethod     -- 12.
  , _eInputFormNoValidate :: Maybe FormNoValidate -- 13.
  , _eInputFormTarget     :: Maybe FormTarget     -- 14.
  , _eInputHeight         :: Maybe Height         -- 15.
  , _eInputInputMode      :: Maybe InputMode      -- 16.
  , _eInputList           :: Maybe List           -- 17.
  , _eInputMax            :: Maybe Max            -- 18.
  , _eInputMaxLength      :: Maybe MaxLength      -- 19.
  , _eInputMin            :: Maybe Min            -- 20.
  , _eInputMinLength      :: Maybe MinLength      -- 21.
  , _eInputMultiple       :: Maybe Multiple       -- 22.
  , _eInputName           :: Maybe Name           -- 23.
  , _eInputPattern        :: Maybe Pattern        -- 24.
  , _eInputPlaceholder    :: Maybe Placeholder    -- 25.
  , _eInputReadOnly       :: Maybe ReadOnly       -- 26.
  , _eInputRequired       :: Maybe Required       -- 27.
  , _eInputSize           :: Maybe Size           -- 28.
  , _eInputSrc            :: Maybe Src            -- 29.
  , _eInputStep           :: Maybe Step           -- 30.
  , _eInputType           :: Maybe InputType      -- 31.
  , _eInputValueText      :: Maybe ValueText      -- 32.
  , _eInputWidth          :: Maybe Width          -- 33.
  , _eInputCustom         :: Maybe Attr           -- 34.
  }



instance AttrMap EInput where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eInputGlobals b
    , attrMap <$> _eInputAccept b
    , attrMap <$> _eInputAlt b
    , attrMap <$> _eInputAutoComplete b
    , attrMap <$> _eInputAutoFocus b
    , attrMap <$> _eInputChecked b
    , attrMap <$> _eInputDirName b
    , attrMap <$> _eInputDisabled b
    , attrMap <$> _eInputForm b
    , attrMap <$> _eInputFormAction b
    , attrMap <$> _eInputFormEncType b
    , attrMap <$> _eInputFormMethod b
    , attrMap <$> _eInputFormNoValidate b
    , attrMap <$> _eInputFormTarget b
    , attrMap <$> _eInputHeight b
    , attrMap <$> _eInputInputMode b
    , attrMap <$> _eInputList b
    , attrMap <$> _eInputMax b
    , attrMap <$> _eInputMaxLength b
    , attrMap <$> _eInputMin b
    , attrMap <$> _eInputMinLength b
    , attrMap <$> _eInputMultiple b
    , attrMap <$> _eInputName b
    , attrMap <$> _eInputPattern b
    , attrMap <$> _eInputPlaceholder b
    , attrMap <$> _eInputReadOnly b
    , attrMap <$> _eInputRequired b
    , attrMap <$> _eInputSize b
    , attrMap <$> _eInputSrc b
    , attrMap <$> _eInputStep b
    , attrMap <$> _eInputType b
    , attrMap <$> _eInputValueText b
    , attrMap <$> _eInputWidth b
    ] <> maybe [] (\a -> [a]) (_eInputCustom b)

instance Default EInput where
  def = EInput def def def def def def def def def def def def
                def def def def def def def def def def def def
                def def def def def def def def def def

instance Monoid EInput where
  mempty = def
  mappend
    (EInput a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17
      a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34)
    (EInput b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17
      b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34)
    = EInput (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)
        (a11 <> b11) (a12 <> b12) (a13 <> b13) (a14 <> b14) (a15 <> b15)
        (a16 <> b16) (a17 <> b17) (a18 <> b18) (a19 <> b19) (a20 <> b20)
        (a21 <> b21) (a22 <> b22) (a23 <> b23) (a24 <> b24) (a25 <> b25)
        (a26 <> b26) (a27 <> b27) (a28 <> b28) (a29 <> b29) (a30 <> b30)
        (a31 <> b31) (a32 <> b32) (a33 <> b33) (a34 <> b34)


instance AttrHasGlobals EInput where
   attrSetGlobals p b = b { _eInputGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EInput
  where attrSetAccessKey p g = g { _eInputGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasAnmval EInput
  where attrSetAnmval p g = g { _eInputGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasContentEditable EInput
  where attrSetContentEditable p g = g  { _eInputGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasContextMenu EInput
  where attrSetContextMenu p g     = g { _eInputGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasClass EInput
  where attrSetClassName p g           = g { _eInputGlobals = Just (attrSetClassName p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasDnmval EInput
  where attrSetDnmval p g           = g { _eInputGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasDir EInput
  where attrSetDir p g             = g { _eInputGlobals = Just (attrSetDir p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasDraggable EInput
  where attrSetDraggable p g       = g { _eInputGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasHidden EInput
  where attrSetHidden p g          = g { _eInputGlobals = Just (attrSetHidden p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasId EInput
  where attrSetId p g              = g { _eInputGlobals = Just (attrSetId p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasLang EInput
  where attrSetLang p g            = g { _eInputGlobals = Just (attrSetLang p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasRole EInput
  where attrSetRole p g            = g { _eInputGlobals = Just (attrSetRole p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasSlot EInput
  where attrSetSlot p g            = g { _eInputGlobals = Just (attrSetSlot p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasSpellCheck EInput
  where attrSetSpellCheck p g      = g { _eInputGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasStyle EInput
  where attrSetStyle p g           = g { _eInputGlobals = Just (attrSetStyle p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasTabIndex EInput
  where attrSetTabIndex p g        = g { _eInputGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasTitle EInput
  where attrSetTitle p g           = g { _eInputGlobals = Just (attrSetTitle p (fromMaybe gDef (_eInputGlobals g))) }
instance AttrHasTranslate EInput
  where attrSetTranslate p g       = g { _eInputGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eInputGlobals g))) }

instance AttrGetClassName EInput where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eInputGlobals g)

instance AttrHasAccept         EInput where attrSetAccept p g         = g { _eInputAccept         = Just p }
instance AttrHasAlt            EInput where attrSetAlt p g            = g { _eInputAlt            = Just p }
instance AttrHasAutoComplete   EInput where attrSetAutoComplete p g   = g { _eInputAutoComplete   = Just p }
instance AttrHasAutoFocus      EInput where attrSetAutoFocus p g      = g { _eInputAutoFocus      = Just p }
instance AttrHasChecked        EInput where attrSetChecked p g        = g { _eInputChecked        = Just p }
instance AttrHasDirName        EInput where attrSetDirName p g        = g { _eInputDirName        = Just p }
instance AttrHasDisabled       EInput where attrSetDisabled p g       = g { _eInputDisabled       = Just p }
instance AttrHasForm           EInput where attrSetForm p g           = g { _eInputForm           = Just p }
instance AttrHasFormAction     EInput where attrSetFormAction p g     = g { _eInputFormAction     = Just p }
instance AttrHasFormEncType    EInput where attrSetFormEncType p g    = g { _eInputFormEncType    = Just p }
instance AttrHasFormMethod     EInput where attrSetFormMethod p g     = g { _eInputFormMethod     = Just p }
instance AttrHasFormNoValidate EInput where attrSetFormNoValidate p g = g { _eInputFormNoValidate = Just p }
instance AttrHasFormTarget     EInput where attrSetFormTarget p g     = g { _eInputFormTarget     = Just p }
instance AttrHasHeight         EInput where attrSetHeight p g         = g { _eInputHeight         = Just p }
instance AttrHasInputMode      EInput where attrSetInputMode p g      = g { _eInputInputMode      = Just p }
instance AttrHasList           EInput where attrSetList p g           = g { _eInputList           = Just p }
instance AttrHasMax            EInput where attrSetMax p g            = g { _eInputMax            = Just p }
instance AttrHasMaxLength      EInput where attrSetMaxLength p g      = g { _eInputMaxLength      = Just p }
instance AttrHasMin            EInput where attrSetMin p g            = g { _eInputMin            = Just p }
instance AttrHasMinLength      EInput where attrSetMinLength p g      = g { _eInputMinLength      = Just p }
instance AttrHasMultiple       EInput where attrSetMultiple p g       = g { _eInputMultiple       = Just p }
instance AttrHasName           EInput where attrSetName p g           = g { _eInputName           = Just p }
instance AttrHasPattern        EInput where attrSetPattern p g        = g { _eInputPattern        = Just p }
instance AttrHasPlaceholder    EInput where attrSetPlaceholder p g    = g { _eInputPlaceholder    = Just p }
instance AttrHasReadOnly       EInput where attrSetReadOnly p g       = g { _eInputReadOnly       = Just p }
instance AttrHasRequired       EInput where attrSetRequired p g       = g { _eInputRequired       = Just p }
instance AttrHasSize           EInput where attrSetSize p g           = g { _eInputSize           = Just p }
instance AttrHasSrc            EInput where attrSetSrc p g            = g { _eInputSrc            = Just p }
instance AttrHasStep           EInput where attrSetStep p g           = g { _eInputStep           = Just p }
instance AttrHasInputType      EInput where attrSetInputType p g      = g { _eInputType           = Just p }
instance AttrHasValueText      EInput where attrSetValueText p g      = g { _eInputValueText      = Just p }
instance AttrHasWidth          EInput where attrSetWidth p g          = g { _eInputWidth          = Just p }


instance AttrHasCustom EInput where attrSetCustom p g       = g { _eInputCustom = Just p }

eInput' :: forall t m a. DomBuilder t m => EInput -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eInput' b  = elAttr' "input" (attrMap b)

eInput :: forall t m a. DomBuilder t m => EInput -> m a -> m a
eInput b children = snd <$> eInput' b children

eInputN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eInputN' = el' "input"

eInputN :: forall t m a. DomBuilder t m => m a -> m a
eInputN children = snd <$> eInputN' children

eInputD' :: forall t m a. MonadWidget t m => Dynamic t EInput -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eInputD' b  = elDynAttr' "input" (attrMap <$> b)

eInputD :: forall t m a. MonadWidget t m => Dynamic t EInput -> m a -> m a
eInputD b children = snd <$> eInputD' b children


eInputC :: MonadWidget t m => EInput -> m () -> m (Event t ())
eInputC b children = do
  (e,_) <- elAttr' "input" (attrMap b) children
  return $ domEvent Click e

eInputCD :: MonadWidget t m => Dynamic t EInput -> m () -> m (Event t ())
eInputCD bDyn children = do
  (e,_) <- elDynAttr' "input" (attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Keygen is likely to be removed from HTML 5.2.
{-
-- | Keygen-element has the following attributes:
-- autofocus; challenge; disabled; form; keytype; name
--
-- You should avoid using the <keygen> element, it will be removed in future versions of HTML.
-- Challenge-attribute not implemented yet. (If you need it, append it in with <>.)
--
data EKeygen = EKeygen
  { _eKeygenGlobals      :: Maybe Globals
  , _eKeygenAutoFocus    :: Maybe AutoFocus
  -- , _eKeygenChallenge :: Maybe Challenge
  , _eKeygenDisabled     :: Maybe Disabled
  , _eKeygenForm         :: Maybe Form
  , _eKeygenKeyType      :: Maybe KeyType
  , _eKeygenName         :: Maybe Name
  , _eKeygenCustom       :: Maybe Attr
  }


instance AttrMap EKeygen where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eKeygenGlobals b
    , attrMap <$> _eKeygenAutoFocus b
    -- , attrMap <$> _eKeygenChallenge b
    , attrMap <$> _eKeygenDisabled b
    , attrMap <$> _eKeygenForm b
    , attrMap <$> _eKeygenKeyType b
    , attrMap <$> _eKeygenName b
    ] <> maybe [] (\a -> [a]) (_eKeygenCustom b)

instance Default EKeygen where
  def = EKeygen def def def def def def def

instance Monoid EKeygen where
  mempty = def
  mappend (EKeygen a1 a2 a3 a4 a5 a6 a7) (EKeygen b1 b2 b3 b4 b5 b6 b7)
    = EKeygen (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)


instance AttrHasGlobals EKeygen where
   attrSetGlobals p b = b { _eKeygenGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EKeygen
  where attrSetAccessKey p g = g { _eKeygenGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasAnmval EKeygen
  where attrSetAnmval p g = g { _eKeygenGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasContentEditable EKeygen
  where attrSetContentEditable p g = g  { _eKeygenGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasContextMenu EKeygen
  where attrSetContextMenu p g     = g { _eKeygenGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasClass EKeygen
  where attrSetClassName p g           = g { _eKeygenGlobals = Just (attrSetClassName p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasDnmval EKeygen
  where attrSetDnmval p g           = g { _eKeygenGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasDir EKeygen
  where attrSetDir p g             = g { _eKeygenGlobals = Just (attrSetDir p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasDraggable EKeygen
  where attrSetDraggable p g       = g { _eKeygenGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasHidden EKeygen
  where attrSetHidden p g          = g { _eKeygenGlobals = Just (attrSetHidden p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasId EKeygen
  where attrSetId p g              = g { _eKeygenGlobals = Just (attrSetId p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasLang EKeygen
  where attrSetLang p g            = g { _eKeygenGlobals = Just (attrSetLang p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasRole EKeygen
  where attrSetRole p g            = g { _eKeygenGlobals = Just (attrSetRole p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasSlot EKeygen
  where attrSetSlot p g            = g { _eKeygenGlobals = Just (attrSetSlot p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasSpellCheck EKeygen
  where attrSetSpellCheck p g      = g { _eKeygenGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasStyle EKeygen
  where attrSetStyle p g           = g { _eKeygenGlobals = Just (attrSetStyle p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasTabIndex EKeygen
  where attrSetTabIndex p g        = g { _eKeygenGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasTitle EKeygen
  where attrSetTitle p g           = g { _eKeygenGlobals = Just (attrSetTitle p (fromMaybe gDef (_eKeygenGlobals g))) }
instance AttrHasTranslate EKeygen
  where attrSetTranslate p g       = g { _eKeygenGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eKeygenGlobals g))) }

instance AttrGetClassName EKeygen where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eKeygenGlobals g)

instance AttrHasAutoFocus EKeygen where attrSetAutoFocus p g = g { _eKeygenAutoFocus = Just p }
-- instance AttrHasChallenge EKeygen where attrSetChallenge p g = g { _eKeygenChallenge = Just p }
instance AttrHasDisabled  EKeygen where attrSetDisabled p g  = g { _eKeygenDisabled  = Just p }
instance AttrHasForm      EKeygen where attrSetForm p g      = g { _eKeygenForm      = Just p }
instance AttrHasKeyType   EKeygen where attrSetKeyType p g   = g { _eKeygenKeyType   = Just p }
instance AttrHasName      EKeygen where attrSetName p g      = g { _eKeygenName      = Just p }


-- instance (Reflex t, AttrHasAccessKey a) => AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) => AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (attrSetAnmval c)

eKeygen :: MonadWidget t m => EKeygen -> m () -> m (Event t ())
eKeygen b children = do
  (e,_) <- elAttr' "keygen" (attrMap b) children
  return $ domEvent Click e

eKeygenD :: MonadWidget t m => Dynamic t EKeygen -> m () -> m (Event t ())
eKeygenD bDyn children = do
  (e,_) <- elDynAttr' "keygen" (attrMap <$> bDyn) children
  return $ domEvent Click e

-}


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Label-element has the following attributes:
-- globals; form; for
--
data ELabel = ELabel
  { _eLabelGlobals :: Maybe Globals
  -- , _eLabelForm :: Maybe Form -- Not in HTML 5.2.
  , _eLabelForId   :: Maybe ForId
  , _eLabelCustom  :: Maybe Attr
  }


instance AttrMap ELabel where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eLabelGlobals b
    -- , attrMap <$> _eLabelForm b
    , attrMap <$> _eLabelForId b
    ] <> maybe [] (\a -> [a]) (_eLabelCustom b)

instance Default ELabel where
  def = ELabel def def def

instance Monoid ELabel where
  mempty = def
  mappend (ELabel a1 a2 a3) (ELabel b1 b2 b3)
    = ELabel (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals ELabel where
   attrSetGlobals p b = b { _eLabelGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ELabel
  where attrSetAccessKey p g = g { _eLabelGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasAnmval ELabel
  where attrSetAnmval p g = g { _eLabelGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasContentEditable ELabel
  where attrSetContentEditable p g = g  { _eLabelGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasContextMenu ELabel
  where attrSetContextMenu p g     = g { _eLabelGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasClass ELabel
  where attrSetClassName p g           = g { _eLabelGlobals = Just (attrSetClassName p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasDnmval ELabel
  where attrSetDnmval p g           = g { _eLabelGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasDir ELabel
  where attrSetDir p g             = g { _eLabelGlobals = Just (attrSetDir p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasDraggable ELabel
  where attrSetDraggable p g       = g { _eLabelGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasHidden ELabel
  where attrSetHidden p g          = g { _eLabelGlobals = Just (attrSetHidden p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasId ELabel
  where attrSetId p g              = g { _eLabelGlobals = Just (attrSetId p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasLang ELabel
  where attrSetLang p g            = g { _eLabelGlobals = Just (attrSetLang p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasRole ELabel
  where attrSetRole p g            = g { _eLabelGlobals = Just (attrSetRole p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasSlot ELabel
  where attrSetSlot p g            = g { _eLabelGlobals = Just (attrSetSlot p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasSpellCheck ELabel
  where attrSetSpellCheck p g      = g { _eLabelGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasStyle ELabel
  where attrSetStyle p g           = g { _eLabelGlobals = Just (attrSetStyle p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasTabIndex ELabel
  where attrSetTabIndex p g        = g { _eLabelGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasTitle ELabel
  where attrSetTitle p g           = g { _eLabelGlobals = Just (attrSetTitle p (fromMaybe gDef (_eLabelGlobals g))) }
instance AttrHasTranslate ELabel
  where attrSetTranslate p g       = g { _eLabelGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eLabelGlobals g))) }

instance AttrGetClassName ELabel where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eLabelGlobals g)

-- instance AttrHasForm      ELabel where attrSetForm p g      = g { _eLabelForm      = Just p }
instance AttrHasForId     ELabel where attrSetForId p g      = g { _eLabelForId     = Just p }

instance AttrHasCustom ELabel where attrSetCustom p g       = g { _eLabelCustom = Just p }


eLabel' :: forall t m a. DomBuilder t m => ELabel -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLabel' b  = elAttr' "label" (attrMap b)

eLabel :: forall t m a. DomBuilder t m => ELabel -> m a -> m a
eLabel b children = snd <$> eLabel' b children

eLabelN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLabelN' = el' "label"

eLabelN :: forall t m a. DomBuilder t m => m a -> m a
eLabelN children = snd <$> eLabelN' children

eLabelD' :: forall t m a. MonadWidget t m => Dynamic t ELabel -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLabelD' b  = elDynAttr' "label" (attrMap <$> b)

eLabelD :: forall t m a. MonadWidget t m => Dynamic t ELabel -> m a -> m a
eLabelD b children = snd <$> eLabelD' b children


eLabelC :: MonadWidget t m => ELabel -> m () -> m (Event t ())
eLabelC b children = do
  (e,_) <- elAttr' "label" (attrMap b) children
  return $ domEvent Click e

eLabelCD :: MonadWidget t m => Dynamic t ELabel -> m () -> m (Event t ())
eLabelCD bDyn children = do
  (e,_) <- elDynAttr' "label" (attrMap <$> bDyn) children
  return $ domEvent Click e

eLabelCForId ::  MonadWidget t m => Maybe Globals -> ForId -> m (Event t ())
eLabelCForId b f = eLabelC (ELabel b (Just f) Nothing) blank



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Select-element has the following attributes:
-- globals; autofocus; disabled; form; multiple; name; required; size
--
data ESelect = ESelect
  { _eSelectGlobals   :: Maybe Globals
  , _eSelectAutoFocus :: Maybe AutoFocus
  , _eSelectDisabled  :: Maybe Disabled
  , _eSelectForm      :: Maybe Form
  , _eSelectMultiple  :: Maybe Multiple
  , _eSelectName      :: Maybe Name
  , _eSelectRequired  :: Maybe Required
  , _eSelectSize      :: Maybe Size
  , _eSelectCustom    :: Maybe Attr
  }


instance AttrMap ESelect where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSelectGlobals b
    , attrMap <$> _eSelectAutoFocus b
    , attrMap <$> _eSelectDisabled b
    , attrMap <$> _eSelectForm b
    , attrMap <$> _eSelectMultiple b
    , attrMap <$> _eSelectName b
    , attrMap <$> _eSelectRequired b
    , attrMap <$> _eSelectSize b
    ] <> maybe [] (\a -> [a]) (_eSelectCustom b)

instance Default ESelect where
  def = ESelect def def def def def def def def def

instance Monoid ESelect where
  mempty = def
  mappend (ESelect a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (ESelect b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = ESelect (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

instance AttrHasGlobals ESelect where
   attrSetGlobals p b = b { _eSelectGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESelect
  where attrSetAccessKey p g = g { _eSelectGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasAnmval ESelect
  where attrSetAnmval p g = g { _eSelectGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasContentEditable ESelect
  where attrSetContentEditable p g = g  { _eSelectGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasContextMenu ESelect
  where attrSetContextMenu p g     = g { _eSelectGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasClass ESelect
  where attrSetClassName p g           = g { _eSelectGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasDnmval ESelect
  where attrSetDnmval p g           = g { _eSelectGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasDir ESelect
  where attrSetDir p g             = g { _eSelectGlobals = Just (attrSetDir p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasDraggable ESelect
  where attrSetDraggable p g       = g { _eSelectGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasHidden ESelect
  where attrSetHidden p g          = g { _eSelectGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasId ESelect
  where attrSetId p g              = g { _eSelectGlobals = Just (attrSetId p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasLang ESelect
  where attrSetLang p g            = g { _eSelectGlobals = Just (attrSetLang p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasRole ESelect
  where attrSetRole p g            = g { _eSelectGlobals = Just (attrSetRole p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasSlot ESelect
  where attrSetSlot p g            = g { _eSelectGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasSpellCheck ESelect
  where attrSetSpellCheck p g      = g { _eSelectGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasStyle ESelect
  where attrSetStyle p g           = g { _eSelectGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasTabIndex ESelect
  where attrSetTabIndex p g        = g { _eSelectGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasTitle ESelect
  where attrSetTitle p g           = g { _eSelectGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSelectGlobals g))) }
instance AttrHasTranslate ESelect
  where attrSetTranslate p g       = g { _eSelectGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSelectGlobals g))) }

instance AttrGetClassName ESelect where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSelectGlobals g)


instance AttrHasAutoFocus ESelect where attrSetAutoFocus p g = g { _eSelectAutoFocus = Just p }
instance AttrHasDisabled  ESelect where attrSetDisabled p g  = g { _eSelectDisabled  = Just p }
instance AttrHasForm      ESelect where attrSetForm p g      = g { _eSelectForm      = Just p }
instance AttrHasMultiple  ESelect where attrSetMultiple p g  = g { _eSelectMultiple  = Just p }
instance AttrHasName      ESelect where attrSetName p g      = g { _eSelectName      = Just p }
instance AttrHasRequired  ESelect where attrSetRequired p g  = g { _eSelectRequired  = Just p }
instance AttrHasSize      ESelect where attrSetSize p g      = g { _eSelectSize      = Just p }

instance AttrHasCustom ESelect where attrSetCustom p g       = g { _eSelectCustom = Just p }


eSelect' :: forall t m a. DomBuilder t m => ESelect -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSelect' b  = elAttr' "select" (attrMap b)

eSelect :: forall t m a. DomBuilder t m => ESelect -> m a -> m a
eSelect b children = snd <$> eSelect' b children

eSelectN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSelectN' = el' "select"

eSelectN :: forall t m a. DomBuilder t m => m a -> m a
eSelectN children = snd <$> eSelectN' children

eSelectD' :: forall t m a. MonadWidget t m => Dynamic t ESelect -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSelectD' b  = elDynAttr' "select" (attrMap <$> b)

eSelectD :: forall t m a. MonadWidget t m => Dynamic t ESelect -> m a -> m a
eSelectD b children = snd <$> eSelectD' b children


eSelectC :: MonadWidget t m => ESelect -> m () -> m (Event t ())
eSelectC b children = do
  (e,_) <- elAttr' "select" (attrMap b) children
  return $ domEvent Click e

eSelectCD :: MonadWidget t m => Dynamic t ESelect -> m () -> m (Event t ())
eSelectCD bDyn children = do
  (e,_) <- elDynAttr' "select" (attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | TextArea-element has the following attributes:
-- globals; autofocus; cols; dirname; disabled; form; maxlength; minlength;
-- name; placeholder; readonly; required; rows; wrap
--
data ETextArea = ETextArea
  { _eTextAreaGlobals     :: Maybe Globals
  , _eTextAreaAutoFocus   :: Maybe AutoFocus
  , _eTextAreaCols        :: Maybe Cols
  , _eTextAreaDirName     :: Maybe DirName
  , _eTextAreaDisabled    :: Maybe Disabled
  , _eTextAreaForm        :: Maybe Form
  , _eTextAreaMaxLength   :: Maybe MaxLength
  , _eTextAreaMinLength   :: Maybe MinLength
  , _eTextAreaName        :: Maybe Name
  , _eTextAreaPlaceholder :: Maybe Placeholder
  , _eTextAreaReadOnly    :: Maybe ReadOnly
  , _eTextAreaRequired    :: Maybe Required
  , _eTextAreaRows        :: Maybe Rows
  , _eTextAreaWrap        :: Maybe Wrap
  , _eTextAreaCustom      :: Maybe Attr
  }


instance AttrMap ETextArea where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTextAreaGlobals b
    , attrMap <$> _eTextAreaAutoFocus b
    , attrMap <$> _eTextAreaCols b
    , attrMap <$> _eTextAreaDirName b
    , attrMap <$> _eTextAreaDisabled b
    , attrMap <$> _eTextAreaForm b
    , attrMap <$> _eTextAreaMaxLength b
    , attrMap <$> _eTextAreaMinLength b
    , attrMap <$> _eTextAreaName b
    , attrMap <$> _eTextAreaPlaceholder b
    , attrMap <$> _eTextAreaReadOnly b
    , attrMap <$> _eTextAreaRequired b
    , attrMap <$> _eTextAreaRows b
    , attrMap <$> _eTextAreaWrap b
    ] <> maybe [] (\a -> [a]) (_eTextAreaCustom b)

instance Default ETextArea where
  def = ETextArea def def def def def def def def def def def def def def def

instance Monoid ETextArea where
  mempty = def
  mappend
    (ETextArea a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (ETextArea b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    = ETextArea (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)
        (a11 <> b11) (a12 <> b12) (a13 <> b13) (a14 <> b14) (a15 <> b15)

instance AttrHasGlobals ETextArea where
   attrSetGlobals p b = b { _eTextAreaGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETextArea
  where attrSetAccessKey p g = g { _eTextAreaGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasAnmval ETextArea
  where attrSetAnmval p g = g { _eTextAreaGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasContentEditable ETextArea
  where attrSetContentEditable p g = g  { _eTextAreaGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasContextMenu ETextArea
  where attrSetContextMenu p g     = g { _eTextAreaGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasClass ETextArea
  where attrSetClassName p g           = g { _eTextAreaGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasDnmval ETextArea
  where attrSetDnmval p g           = g { _eTextAreaGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasDir ETextArea
  where attrSetDir p g             = g { _eTextAreaGlobals = Just (attrSetDir p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasDraggable ETextArea
  where attrSetDraggable p g       = g { _eTextAreaGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasHidden ETextArea
  where attrSetHidden p g          = g { _eTextAreaGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasId ETextArea
  where attrSetId p g              = g { _eTextAreaGlobals = Just (attrSetId p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasLang ETextArea
  where attrSetLang p g            = g { _eTextAreaGlobals = Just (attrSetLang p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasRole ETextArea
  where attrSetRole p g            = g { _eTextAreaGlobals = Just (attrSetRole p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasSlot ETextArea
  where attrSetSlot p g            = g { _eTextAreaGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasSpellCheck ETextArea
  where attrSetSpellCheck p g      = g { _eTextAreaGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasStyle ETextArea
  where attrSetStyle p g           = g { _eTextAreaGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasTabIndex ETextArea
  where attrSetTabIndex p g        = g { _eTextAreaGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasTitle ETextArea
  where attrSetTitle p g           = g { _eTextAreaGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTextAreaGlobals g))) }
instance AttrHasTranslate ETextArea
  where attrSetTranslate p g       = g { _eTextAreaGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTextAreaGlobals g))) }

instance AttrGetClassName ETextArea where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTextAreaGlobals g)


instance AttrHasAutoFocus   ETextArea where attrSetAutoFocus p g   = g { _eTextAreaAutoFocus   = Just p }
instance AttrHasCols        ETextArea where attrSetCols p g        = g { _eTextAreaCols        = Just p }
instance AttrHasDirName     ETextArea where attrSetDirName p g     = g { _eTextAreaDirName     = Just p }
instance AttrHasDisabled    ETextArea where attrSetDisabled p g    = g { _eTextAreaDisabled    = Just p }
instance AttrHasForm        ETextArea where attrSetForm p g        = g { _eTextAreaForm        = Just p }
instance AttrHasMaxLength   ETextArea where attrSetMaxLength p g   = g { _eTextAreaMaxLength   = Just p }
instance AttrHasMinLength   ETextArea where attrSetMinLength p g   = g { _eTextAreaMinLength   = Just p }
instance AttrHasName        ETextArea where attrSetName p g        = g { _eTextAreaName        = Just p }
instance AttrHasPlaceholder ETextArea where attrSetPlaceholder p g = g { _eTextAreaPlaceholder = Just p }
instance AttrHasReadOnly    ETextArea where attrSetReadOnly p g    = g { _eTextAreaReadOnly    = Just p }
instance AttrHasRequired    ETextArea where attrSetRequired p g    = g { _eTextAreaRequired    = Just p }
instance AttrHasRows        ETextArea where attrSetRows p g        = g { _eTextAreaRows        = Just p }
instance AttrHasWrap        ETextArea where attrSetWrap p g        = g { _eTextAreaWrap        = Just p }

instance AttrHasCustom ETextArea where attrSetCustom p g       = g { _eTextAreaCustom = Just p }

eTextArea' :: forall t m a. DomBuilder t m => ETextArea -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTextArea' b  = elAttr' "textarea" (attrMap b)

eTextArea :: forall t m a. DomBuilder t m => ETextArea -> m a -> m a
eTextArea b children = snd <$> eTextArea' b children

eTextAreaN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTextAreaN' = el' "textarea"

eTextAreaN :: forall t m a. DomBuilder t m => m a -> m a
eTextAreaN children = snd <$> eTextAreaN' children

eTextAreaD' :: forall t m a. MonadWidget t m => Dynamic t ETextArea -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTextAreaD' b  = elDynAttr' "textarea" (attrMap <$> b)

eTextAreaD :: forall t m a. MonadWidget t m => Dynamic t ETextArea -> m a -> m a
eTextAreaD b children = snd <$> eTextAreaD' b children


eTextAreaC :: MonadWidget t m => ETextArea -> m () -> m (Event t ())
eTextAreaC b children = do
  (e,_) <- elAttr' "textarea" (attrMap b) children
  return $ domEvent Click e

eTextAreaCD :: MonadWidget t m => Dynamic t ETextArea -> m () -> m (Event t ())
eTextAreaCD bDyn children = do
  (e,_) <- elDynAttr' "textarea" (attrMap <$> bDyn) children
  return $ domEvent Click e




