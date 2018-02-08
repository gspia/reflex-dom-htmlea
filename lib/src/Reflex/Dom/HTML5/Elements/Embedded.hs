{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Elements.Embedded
Description : HTML5 embedded elements
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Embedded

This module contains the following elements:
Canvas, Object, Math, Picture, Source, Svg.
and
Param (child of object), Track (child of media elem - that is, audio, video)
Area, Map.

This also contains Audio, Iframe, Img, Video, even though
they belong to Interactive-category (when some attributes are on).
This choice is arbitrary.

Naming convention follows that of Element-module.


Wish list: math-tag and MathMl-support, and svg-tag & related things.
Those should be implemented on their own modules.

-}

module Reflex.Dom.HTML5.Elements.Embedded where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', el', Event, EventName (Click),
                       domEvent)

import Reflex.Dom.HTML5.Attrs (AttrMap, attrMap,
  attrSetGlobals, attrSetAccessKey, attrSetContentEditable,
  attrSetContextMenu, attrSetClassName, attrSetDir, attrSetDraggable,
  attrSetHidden, attrSetId, attrSetLang, attrSetSpellCheck,
  attrSetStyle, attrSetTabIndex, attrSetTitle, attrSetTranslate,
  attrSetSrcLang, attrSetSrc, attrSetKind, attrSetDefault,
  attrSetLabel, attrSetMediaType, attrSetSrcSet, attrSetSizes,
  attrSetMedia, attrSetName, attrSetTypeMustMatch, attrSetHeight,
  attrSetWidth, attrSetReferrerPolicy, attrSetForm,
  attrSetRel, attrSetDownload, attrSetTarget, attrSetHrefLang,
  attrSetSrc, attrSetWidth, attrSetSrcDoc, attrSetSandbox,
  attrSetWidth, attrSetShape, attrSetCoords, attrSetAlt,
  attrSetHref, attrSetDataUrl, attrSetValueName, attrSetIsMap,
  attrSetPreload, attrSetMuted, attrSetLoop, attrSetCrossOrigin,
  attrSetControls, attrSetAutoplay, attrSetUseMap,
  attrSetLongDesc, attrSetPoster, attrSetPreload, attrSetMuted,
  attrSetLoop, attrSetCrossOrigin, attrSetControls, attrSetAutoplay,
  attrSetAnmval, attrSetDnmval, attrSetRole, attrSetSlot,
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
  AttrHasSrcLang, AttrHasSrc, AttrHasKind, AttrHasDefault,
  AttrHasLabel, AttrHasMediaType, AttrHasSrcSet, AttrHasSizes,
  AttrHasMedia, AttrHasName, AttrHasTypeMustMatch, AttrHasHeight,
  AttrHasWidth, AttrHasReferrerPolicy, AttrHasForm,
  AttrHasRel, AttrHasDownload, AttrHasTarget, AttrHasHrefLang,
  AttrHasSrc, AttrHasWidth, AttrHasWidth, AttrHasShape,
  AttrHasCoords, AttrHasAlt, AttrHasHref, AttrHasDataUrl,
  AttrHasPreload, AttrHasMuted, AttrHasLoop, AttrHasCrossOrigin,
  AttrHasControls, AttrHasAutoplay, AttrHasUseMap, AttrHasIsMap,
  AttrHasValueName, AttrHasSandbox, AttrHasSrcDoc,
  AttrHasLongDesc, AttrHasPoster, AttrHasPreload, AttrHasMuted,
  AttrHasLoop, AttrHasCrossOrigin, AttrHasControls, AttrHasAutoplay,
  AttrHasAnmval, AttrHasDnmval, AttrHasRole, AttrHasSlot,
  SrcLang, Src, Label, Sandbox, SrcDoc, IsMap, LongDesc, Poster,
  Kind, MediaType, SrcSet, Sizes, Media, ValueName, Name, Width,
  TypeMustMatch, Height, Form, DataUrl, ReferrerPolicy, HrefLang,
  Muted, Preload, Loop, CrossOrigin, Controls, Autoplay, UseMap,
  Href, Rel, Download, Alt_, Coords, Shape, Target, Default_,
  Globals, AttrGetClassName, attrGetClassName, gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )


------------------------------------------------------------------------------

-- | Area-element
data EArea = EArea
  { _eAreaGlobals        :: Maybe Globals
  , _eAreaAlt            :: Maybe Alt_
  , _eAreaCoords         :: Maybe Coords
  , _eAreaShape          :: Maybe Shape
  , _eAreaHref           :: Maybe Href
  , _eAreaTarget         :: Maybe Target
  , _eAreaDownload       :: Maybe Download
  , _eAreaRel            :: Maybe Rel
  , _eAreaHrefLang       :: Maybe HrefLang
  , _eAreaMediaType      :: Maybe MediaType
  , _eAreaReferrerPolicy :: Maybe ReferrerPolicy
  , _eAreaCustom         :: Maybe Attr
  }


instance AttrMap EArea where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAreaGlobals b
    , attrMap <$> _eAreaAlt b
    , attrMap <$> _eAreaCoords b
    , attrMap <$> _eAreaShape b
    , attrMap <$> _eAreaHref b
    , attrMap <$> _eAreaTarget b
    , attrMap <$> _eAreaDownload b
    , attrMap <$> _eAreaRel b
    , attrMap <$> _eAreaHrefLang b
    , attrMap <$> _eAreaMediaType b
    , attrMap <$> _eAreaReferrerPolicy b
    ] <> maybeToList (_eAreaCustom b)

instance Default EArea where
  def = EArea def def def def def def def def def def def def

instance Monoid EArea where
  mempty = def
  mappend (EArea a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (EArea b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = EArea (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

instance AttrHasGlobals EArea where
   attrSetGlobals p b = b { _eAreaGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EArea
  where attrSetAccessKey p g = g { _eAreaGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasAnmval EArea
  where attrSetAnmval p g = g { _eAreaGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasContentEditable EArea
  where attrSetContentEditable p g = g  { _eAreaGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasContextMenu EArea
  where attrSetContextMenu p g     = g { _eAreaGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasClass EArea
  where attrSetClassName p g           = g { _eAreaGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasDnmval EArea
  where attrSetDnmval p g           = g { _eAreaGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasDir EArea
  where attrSetDir p g             = g { _eAreaGlobals = Just (attrSetDir p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasDraggable EArea
  where attrSetDraggable p g       = g { _eAreaGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasHidden EArea
  where attrSetHidden p g          = g { _eAreaGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasId EArea
  where attrSetId p g              = g { _eAreaGlobals = Just (attrSetId p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasLang EArea
  where attrSetLang p g            = g { _eAreaGlobals = Just (attrSetLang p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasRole EArea
  where attrSetRole p g            = g { _eAreaGlobals = Just (attrSetRole p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasSlot EArea
  where attrSetSlot p g            = g { _eAreaGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasSpellCheck EArea
  where attrSetSpellCheck p g      = g { _eAreaGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasStyle EArea
  where attrSetStyle p g           = g { _eAreaGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasTabIndex EArea
  where attrSetTabIndex p g        = g { _eAreaGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasTitle EArea
  where attrSetTitle p g           = g { _eAreaGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAreaGlobals g))) }
instance AttrHasTranslate EArea
  where attrSetTranslate p g       = g { _eAreaGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAreaGlobals g))) }

instance AttrGetClassName EArea where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAreaGlobals g)

instance AttrHasAlt EArea where attrSetAlt p g = g {_eAreaAlt = Just p }
instance AttrHasCoords EArea where attrSetCoords p g = g {_eAreaCoords = Just p }
instance AttrHasShape EArea where attrSetShape p g = g {_eAreaShape = Just p }
instance AttrHasHref EArea where attrSetHref p g = g {_eAreaHref = Just p }
instance AttrHasTarget EArea where attrSetTarget p g = g {_eAreaTarget = Just p }
instance AttrHasDownload EArea where attrSetDownload p g = g {_eAreaDownload = Just p }
instance AttrHasRel EArea where attrSetRel p g = g {_eAreaRel = Just p }
instance AttrHasHrefLang EArea where attrSetHrefLang p g = g {_eAreaHrefLang = Just p }
instance AttrHasMediaType EArea where attrSetMediaType p g = g {_eAreaMediaType = Just p }
instance AttrHasReferrerPolicy EArea where attrSetReferrerPolicy p g = g {_eAreaReferrerPolicy = Just p }

instance AttrHasCustom EArea where attrSetCustom p g       = g { _eAreaCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"area\" ... @
eArea' :: forall t m a. DomBuilder t m => EArea -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eArea' b  = elAttr' "area" (attrMap b)

-- | A short-hand notion for @ elAttr \"area\" ... @
eArea :: forall t m a. DomBuilder t m => EArea -> m a -> m a
eArea b children = snd <$> eArea' b children

-- | A short-hand notion for @ el\' \"area\" ... @
eAreaN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAreaN' = el' "area"

-- | A short-hand notion for @ el \"area\" ... @
eAreaN :: forall t m a. DomBuilder t m => m a -> m a
eAreaN children = snd <$> eAreaN' children

-- | A short-hand notion for @ elDynAttr\' \"area\" ... @
eAreaD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EArea -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAreaD' b = elDynAttr' "area" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"area\" ... @
eAreaD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EArea -> m a -> m a
eAreaD b children = snd <$> eAreaD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Audio-element is interactive  if the controls attribute is present
-- globals; src; crossorigin; preload; autoplay; loop; muted; controls
--
data EAudio = EAudio
  { _eAudioGlobals     :: Maybe Globals
  , _eAudioSrc         :: Maybe Src
  , _eAudioCrossOrigin :: Maybe CrossOrigin
  , _eAudioPreload     :: Maybe Preload
  , _eAudioAutoplay    :: Maybe Autoplay
  , _eAudioLoop        :: Maybe Loop
  , _eAudioMuted       :: Maybe Muted
  , _eAudioControls    :: Maybe Controls
  , _eAudioCustom      :: Maybe Attr
  }



instance AttrMap EAudio where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAudioGlobals b
    , attrMap <$> _eAudioSrc b
    , attrMap <$> _eAudioCrossOrigin b
    , attrMap <$> _eAudioPreload b
    , attrMap <$> _eAudioAutoplay b
    , attrMap <$> _eAudioLoop b
    , attrMap <$> _eAudioMuted b
    , attrMap <$> _eAudioControls b
    ] <> maybeToList (_eAudioCustom b)

instance Default EAudio where
  def = EAudio def def def def def def def def def

instance Monoid EAudio where
  mempty = def
  mappend (EAudio a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (EAudio b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = EAudio (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
             (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

instance AttrHasGlobals EAudio where
   attrSetGlobals p b = b { _eAudioGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EAudio
  where attrSetAccessKey p g = g { _eAudioGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasAnmval EAudio
  where attrSetAnmval p g = g { _eAudioGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasContentEditable EAudio
  where attrSetContentEditable p g = g  { _eAudioGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasContextMenu EAudio
  where attrSetContextMenu p g     = g { _eAudioGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasClass EAudio
  where attrSetClassName p g           = g { _eAudioGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasDnmval EAudio
  where attrSetDnmval p g           = g { _eAudioGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasDir EAudio
  where attrSetDir p g             = g { _eAudioGlobals = Just (attrSetDir p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasDraggable EAudio
  where attrSetDraggable p g       = g { _eAudioGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasHidden EAudio
  where attrSetHidden p g          = g { _eAudioGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasId EAudio
  where attrSetId p g              = g { _eAudioGlobals = Just (attrSetId p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasLang EAudio
  where attrSetLang p g            = g { _eAudioGlobals = Just (attrSetLang p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasRole EAudio
  where attrSetRole p g            = g { _eAudioGlobals = Just (attrSetRole p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasSlot EAudio
  where attrSetSlot p g            = g { _eAudioGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasSpellCheck EAudio
  where attrSetSpellCheck p g      = g { _eAudioGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasStyle EAudio
  where attrSetStyle p g           = g { _eAudioGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasTabIndex EAudio
  where attrSetTabIndex p g        = g { _eAudioGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasTitle EAudio
  where attrSetTitle p g           = g { _eAudioGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAudioGlobals g))) }
instance AttrHasTranslate EAudio
  where attrSetTranslate p g       = g { _eAudioGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAudioGlobals g))) }

instance AttrGetClassName EAudio where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAudioGlobals g)

instance AttrHasSrc         EAudio where attrSetSrc p g         = g { _eAudioSrc         = Just p }
instance AttrHasCrossOrigin EAudio where attrSetCrossOrigin p g = g { _eAudioCrossOrigin = Just p }
instance AttrHasPreload     EAudio where attrSetPreload p g     = g { _eAudioPreload     = Just p }
instance AttrHasAutoplay    EAudio where attrSetAutoplay p g    = g { _eAudioAutoplay    = Just p }
instance AttrHasLoop        EAudio where attrSetLoop p g        = g { _eAudioLoop        = Just p }
instance AttrHasMuted       EAudio where attrSetMuted p g       = g { _eAudioMuted       = Just p }
instance AttrHasControls    EAudio where attrSetControls p g    = g { _eAudioControls    = Just p }


instance AttrHasCustom EAudio where attrSetCustom p g       = g { _eAudioCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"audio\" ... @
eAudio' :: forall t m a. DomBuilder t m => EAudio -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAudio' b  = elAttr' "audio" (attrMap b)

-- | A short-hand notion for @ elAttr \"audio\" ... @
eAudio :: forall t m a. DomBuilder t m => EAudio -> m a -> m a
eAudio b children = snd <$> eAudio' b children

-- | A short-hand notion for @ el\' \"audio\" ... @
eAudioN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAudioN' = el' "audio"

-- | A short-hand notion for @ el \"audio\" ... @
eAudioN :: forall t m a. DomBuilder t m => m a -> m a
eAudioN children = snd <$> eAudioN' children

-- | A short-hand notion for @ elDynAttr\' \"audio\" ... @
eAudioD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EAudio -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAudioD' b  = elDynAttr' "audio" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"audio\" ... @
eAudioD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EAudio -> m a -> m a
eAudioD b children = snd <$> eAudioD' b children


eAudioC :: (DomBuilder t m, PostBuild t m) => EAudio -> m () -> m (Event t ())
eAudioC b children = do
  (e,_) <- elAttr' "audio" (attrMap b) children
  return $ domEvent Click e

eAudioCD :: (DomBuilder t m, PostBuild t m) => Dynamic t EAudio -> m () -> m (Event t ())
eAudioCD bDyn children = do
  (e,_) <- elDynAttr' "audio" (attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Canvas-element
data ECanvas = ECanvas
  { _eCanvasGlobals :: Maybe Globals
  , _eCanvasHeight  :: Maybe Height
  , _eCanvasWidth   :: Maybe Width
  , _eCanvasCustom  :: Maybe Attr
  }


instance AttrMap ECanvas where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eCanvasGlobals b
    , attrMap <$> _eCanvasHeight b
    , attrMap <$> _eCanvasWidth b
    ] <> maybeToList (_eCanvasCustom b)

instance Default ECanvas where
  def = ECanvas def def def def

instance Monoid ECanvas where
  mempty = def
  mappend (ECanvas a1 a2 a3 a4) (ECanvas b1 b2 b3 b4)
    = ECanvas (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals ECanvas where
   attrSetGlobals p b = b { _eCanvasGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ECanvas
  where attrSetAccessKey p g = g { _eCanvasGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasAnmval ECanvas
  where attrSetAnmval p g = g { _eCanvasGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasContentEditable ECanvas
  where attrSetContentEditable p g = g  { _eCanvasGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasContextMenu ECanvas
  where attrSetContextMenu p g     = g { _eCanvasGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasClass ECanvas
  where attrSetClassName p g           = g { _eCanvasGlobals = Just (attrSetClassName p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasDnmval ECanvas
  where attrSetDnmval p g           = g { _eCanvasGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasDir ECanvas
  where attrSetDir p g             = g { _eCanvasGlobals = Just (attrSetDir p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasDraggable ECanvas
  where attrSetDraggable p g       = g { _eCanvasGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasHidden ECanvas
  where attrSetHidden p g          = g { _eCanvasGlobals = Just (attrSetHidden p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasId ECanvas
  where attrSetId p g              = g { _eCanvasGlobals = Just (attrSetId p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasLang ECanvas
  where attrSetLang p g            = g { _eCanvasGlobals = Just (attrSetLang p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasRole ECanvas
  where attrSetRole p g            = g { _eCanvasGlobals = Just (attrSetRole p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasSlot ECanvas
  where attrSetSlot p g            = g { _eCanvasGlobals = Just (attrSetSlot p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasSpellCheck ECanvas
  where attrSetSpellCheck p g      = g { _eCanvasGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasStyle ECanvas
  where attrSetStyle p g           = g { _eCanvasGlobals = Just (attrSetStyle p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasTabIndex ECanvas
  where attrSetTabIndex p g        = g { _eCanvasGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasTitle ECanvas
  where attrSetTitle p g           = g { _eCanvasGlobals = Just (attrSetTitle p (fromMaybe gDef (_eCanvasGlobals g))) }
instance AttrHasTranslate ECanvas
  where attrSetTranslate p g       = g { _eCanvasGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eCanvasGlobals g))) }

instance AttrGetClassName ECanvas where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eCanvasGlobals g)

instance AttrHasHeight ECanvas where attrSetHeight p g = g {_eCanvasHeight = Just p }
instance AttrHasWidth ECanvas where attrSetWidth p g = g {_eCanvasWidth = Just p }

instance AttrHasCustom ECanvas where attrSetCustom p g       = g { _eCanvasCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"canvas\" ... @
eCanvas' :: forall t m a. DomBuilder t m => ECanvas -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCanvas' b  = elAttr' "canvas" (attrMap b)

-- | A short-hand notion for @ elAttr \"canvas\" ... @
eCanvas :: forall t m a. DomBuilder t m => ECanvas -> m a -> m a
eCanvas b children = snd <$> eCanvas' b children

-- | A short-hand notion for @ el\' \"canvas\" ... @
eCanvasN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCanvasN' = el' "canvas"

-- | A short-hand notion for @ el \"canvas\" ... @
eCanvasN :: forall t m a. DomBuilder t m => m a -> m a
eCanvasN children = snd <$> eCanvasN' children

-- | A short-hand notion for @ elDynAttr\' \"canvas\" ... @
eCanvasD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECanvas -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eCanvasD' b = elDynAttr' "canvas" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"canvas\" ... @
eCanvasD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ECanvas -> m a -> m a
eCanvasD b children = snd <$> eCanvasD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | IFrame-element has the following attributes:
-- globals; src; srcdoc; name; sandbox; width; height
--
data EIFrame = EIFrame
  { _eIFrameGlobals :: Maybe Globals
  , _eIFrameSrc     :: Maybe Src
  , _eIFrameSrcDoc  :: Maybe SrcDoc
  , _eIFrameSandbox :: Maybe Sandbox
  , _eIFrameWidth   :: Maybe Width
  , _eIFrameHeight  :: Maybe Height
  , _eIFrameCustom  :: Maybe Attr
  }


instance AttrMap EIFrame where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eIFrameGlobals b
    , attrMap <$> _eIFrameSrc b
    , attrMap <$> _eIFrameSrcDoc b
    , attrMap <$> _eIFrameSandbox b
    , attrMap <$> _eIFrameWidth b
    , attrMap <$> _eIFrameHeight b
    ] <> maybeToList (_eIFrameCustom b)

instance Default EIFrame where
  def = EIFrame def def def def def def def

instance Monoid EIFrame where
  mempty = def
  mappend (EIFrame a1 a2 a3 a4 a5 a6 a7) (EIFrame b1 b2 b3 b4 b5 b6 b7)
    = EIFrame (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)
        (a5 <> b5) (a6 <> b6) (a7 <> b7)

instance AttrHasGlobals EIFrame where
   attrSetGlobals p b = b { _eIFrameGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EIFrame
  where attrSetAccessKey p g = g { _eIFrameGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasAnmval EIFrame
  where attrSetAnmval p g = g { _eIFrameGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasContentEditable EIFrame
  where attrSetContentEditable p g = g  { _eIFrameGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasContextMenu EIFrame
  where attrSetContextMenu p g     = g { _eIFrameGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasClass EIFrame
  where attrSetClassName p g           = g { _eIFrameGlobals = Just (attrSetClassName p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasDnmval EIFrame
  where attrSetDnmval p g           = g { _eIFrameGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasDir EIFrame
  where attrSetDir p g             = g { _eIFrameGlobals = Just (attrSetDir p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasDraggable EIFrame
  where attrSetDraggable p g       = g { _eIFrameGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasHidden EIFrame
  where attrSetHidden p g          = g { _eIFrameGlobals = Just (attrSetHidden p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasId EIFrame
  where attrSetId p g              = g { _eIFrameGlobals = Just (attrSetId p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasLang EIFrame
  where attrSetLang p g            = g { _eIFrameGlobals = Just (attrSetLang p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasRole EIFrame
  where attrSetRole p g            = g { _eIFrameGlobals = Just (attrSetRole p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasSlot EIFrame
  where attrSetSlot p g            = g { _eIFrameGlobals = Just (attrSetSlot p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasSpellCheck EIFrame
  where attrSetSpellCheck p g      = g { _eIFrameGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasStyle EIFrame
  where attrSetStyle p g           = g { _eIFrameGlobals = Just (attrSetStyle p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasTabIndex EIFrame
  where attrSetTabIndex p g        = g { _eIFrameGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasTitle EIFrame
  where attrSetTitle p g           = g { _eIFrameGlobals = Just (attrSetTitle p (fromMaybe gDef (_eIFrameGlobals g))) }
instance AttrHasTranslate EIFrame
  where attrSetTranslate p g       = g { _eIFrameGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eIFrameGlobals g))) }

instance AttrGetClassName EIFrame where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eIFrameGlobals g)

instance AttrHasSrc     EIFrame where attrSetSrc p g     = g { _eIFrameSrc     = Just p }
instance AttrHasSrcDoc  EIFrame where attrSetSrcDoc p g  = g { _eIFrameSrcDoc  = Just p }
instance AttrHasSandbox EIFrame where attrSetSandbox p g = g { _eIFrameSandbox = Just p }
instance AttrHasWidth   EIFrame where attrSetWidth p g   = g { _eIFrameWidth   = Just p }
instance AttrHasHeight  EIFrame where attrSetHeight p g  = g { _eIFrameHeight  = Just p }

instance AttrHasCustom EIFrame where attrSetCustom p g       = g { _eIFrameCustom = Just p }

eIFrame' :: forall t m a. DomBuilder t m => EIFrame -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eIFrame' b  = elAttr' "iframe" (attrMap b)

eIFrame :: forall t m a. DomBuilder t m => EIFrame -> m a -> m a
eIFrame b children = snd <$> eIFrame' b children

eIFrameN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eIFrameN' = el' "iframe"

eIFrameN :: forall t m a. DomBuilder t m => m a -> m a
eIFrameN children = snd <$> eIFrameN' children

eIFrameD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EIFrame -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eIFrameD' b  = elDynAttr' "iframe" (attrMap <$> b)

eIFrameD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EIFrame -> m a -> m a
eIFrameD b children = snd <$> eIFrameD' b children


eIFrameC :: (DomBuilder t m, PostBuild t m) => EIFrame -> m () -> m (Event t ())
eIFrameC b children = do
  (e,_) <- elAttr' "iframe" (attrMap b) children
  return $ domEvent Click e

eIFrameCD :: (DomBuilder t m, PostBuild t m) => Dynamic t EIFrame -> m () -> m (Event t ())
eIFrameCD bDyn children = do
  (e,_) <- elDynAttr' "iframe" (attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Img-element is interactive  if the usemap attribute is present
-- globals; alt; src; srcset; crossorigin; usemap; ismap; longdesc; width; height; referrerpolicy
--
data EImg = EImg
  { _eImgGlobals        :: Maybe Globals
  , _eImgAlt            :: Maybe Alt_
  , _eImgCrossOrigin    :: Maybe CrossOrigin
  , _eImgHeight         :: Maybe Height
  , _eImgIsMap          :: Maybe IsMap
  , _eImgLongDesc       :: Maybe LongDesc
  , _eImgReferrerPolicy :: Maybe ReferrerPolicy
  , _eImgSrc            :: Maybe Src
  , _eImgSrcSet         :: Maybe SrcSet
  , _eImgUseMap         :: Maybe UseMap
  , _eImgWidth          :: Maybe Width
  , _eImgCustom         :: Maybe Attr
  }



instance AttrMap EImg where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eImgGlobals b
    , attrMap <$> _eImgAlt b
    , attrMap <$> _eImgCrossOrigin b
    , attrMap <$> _eImgHeight b
    , attrMap <$> _eImgIsMap b
    , attrMap <$> _eImgLongDesc b
    , attrMap <$> _eImgReferrerPolicy b
    , attrMap <$> _eImgSrc b
    , attrMap <$> _eImgSrcSet b
    , attrMap <$> _eImgUseMap b
    , attrMap <$> _eImgWidth b
    ] <> maybeToList (_eImgCustom b)

instance Default EImg where
  def = EImg def def def def def def def def def def def def

instance Monoid EImg where
  mempty = def
  mappend (EImg a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (EImg b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = EImg (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
       (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

instance AttrHasGlobals EImg where
   attrSetGlobals p b = b { _eImgGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EImg
  where attrSetAccessKey p g = g { _eImgGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasAnmval EImg
  where attrSetAnmval p g = g { _eImgGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasContentEditable EImg
  where attrSetContentEditable p g = g  { _eImgGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasContextMenu EImg
  where attrSetContextMenu p g     = g { _eImgGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasClass EImg
  where attrSetClassName p g           = g { _eImgGlobals = Just (attrSetClassName p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasDnmval EImg
  where attrSetDnmval p g           = g { _eImgGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasDir EImg
  where attrSetDir p g             = g { _eImgGlobals = Just (attrSetDir p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasDraggable EImg
  where attrSetDraggable p g       = g { _eImgGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasHidden EImg
  where attrSetHidden p g          = g { _eImgGlobals = Just (attrSetHidden p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasId EImg
  where attrSetId p g              = g { _eImgGlobals = Just (attrSetId p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasLang EImg
  where attrSetLang p g            = g { _eImgGlobals = Just (attrSetLang p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasRole EImg
  where attrSetRole p g            = g { _eImgGlobals = Just (attrSetRole p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasSlot EImg
  where attrSetSlot p g            = g { _eImgGlobals = Just (attrSetSlot p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasSpellCheck EImg
  where attrSetSpellCheck p g      = g { _eImgGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasStyle EImg
  where attrSetStyle p g           = g { _eImgGlobals = Just (attrSetStyle p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasTabIndex EImg
  where attrSetTabIndex p g        = g { _eImgGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasTitle EImg
  where attrSetTitle p g           = g { _eImgGlobals = Just (attrSetTitle p (fromMaybe gDef (_eImgGlobals g))) }
instance AttrHasTranslate EImg
  where attrSetTranslate p g       = g { _eImgGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eImgGlobals g))) }

instance AttrGetClassName EImg where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eImgGlobals g)

instance AttrHasAlt            EImg where attrSetAlt p g            = g { _eImgAlt            = Just p }
instance AttrHasCrossOrigin    EImg where attrSetCrossOrigin p g    = g { _eImgCrossOrigin    = Just p }
instance AttrHasHeight         EImg where attrSetHeight p g         = g { _eImgHeight         = Just p }
instance AttrHasIsMap          EImg where attrSetIsMap p g          = g { _eImgIsMap          = Just p }
instance AttrHasLongDesc       EImg where attrSetLongDesc p g       = g { _eImgLongDesc       = Just p }
instance AttrHasReferrerPolicy EImg where attrSetReferrerPolicy p g = g { _eImgReferrerPolicy = Just p }
instance AttrHasSrc            EImg where attrSetSrc p g            = g { _eImgSrc            = Just p }
instance AttrHasSrcSet         EImg where attrSetSrcSet p g         = g { _eImgSrcSet         = Just p }
instance AttrHasUseMap         EImg where attrSetUseMap p g         = g { _eImgUseMap         = Just p }
instance AttrHasWidth          EImg where attrSetWidth p g          = g { _eImgWidth          = Just p }


instance AttrHasCustom EImg where attrSetCustom p g       = g { _eImgCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"img\" ... @
eImg' :: forall t m a. DomBuilder t m => EImg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eImg' b  = elAttr' "img" (attrMap b)

-- | A short-hand notion for @ elAttr \"img\" ... @
eImg :: forall t m a. DomBuilder t m => EImg -> m a -> m a
eImg b children = snd <$> eImg' b children

-- | A short-hand notion for @ el\' \"img\" ... @
eImgN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eImgN' = el' "img"

-- | A short-hand notion for @ el \"img\" ... @
eImgN :: forall t m a. DomBuilder t m => m a -> m a
eImgN children = snd <$> eImgN' children

-- | A short-hand notion for @ elDynAttr\' \"img\" ... @
eImgD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EImg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eImgD' b  = elDynAttr' "img" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"img\" ... @
eImgD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EImg -> m a -> m a
eImgD b children = snd <$> eImgD' b children


eImgC :: (DomBuilder t m, PostBuild t m) => EImg -> m () -> m (Event t ())
eImgC b children = do
  (e,_) <- elAttr' "img" (attrMap b) children
  return $ domEvent Click e

eImgCD :: (DomBuilder t m, PostBuild t m) => Dynamic t EImg -> m () -> m (Event t ())
eImgCD bDyn children = do
  (e,_) <- elDynAttr' "img" (attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Map-element
data EMap = EMap
  { _eMapGlobals :: Maybe Globals
  , _eMapName    :: Maybe Name
  , _eMapCustom  :: Maybe Attr
  }


instance AttrMap EMap where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMapGlobals b
    , attrMap <$> _eMapName b
    ] <> maybeToList (_eMapCustom b)

instance Default EMap where
  def = EMap def def def

instance Monoid EMap where
  mempty = def
  mappend (EMap a1 a2 a3) (EMap b1 b2 b3) = EMap (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance AttrHasGlobals EMap where
   attrSetGlobals p b = b { _eMapGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMap
  where attrSetAccessKey p g = g { _eMapGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasAnmval EMap
  where attrSetAnmval p g = g { _eMapGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasContentEditable EMap
  where attrSetContentEditable p g = g  { _eMapGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasContextMenu EMap
  where attrSetContextMenu p g     = g { _eMapGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasClass EMap
  where attrSetClassName p g           = g { _eMapGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasDnmval EMap
  where attrSetDnmval p g           = g { _eMapGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasDir EMap
  where attrSetDir p g             = g { _eMapGlobals = Just (attrSetDir p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasDraggable EMap
  where attrSetDraggable p g       = g { _eMapGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasHidden EMap
  where attrSetHidden p g          = g { _eMapGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasId EMap
  where attrSetId p g              = g { _eMapGlobals = Just (attrSetId p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasLang EMap
  where attrSetLang p g            = g { _eMapGlobals = Just (attrSetLang p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasRole EMap
  where attrSetRole p g            = g { _eMapGlobals = Just (attrSetRole p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasSlot EMap
  where attrSetSlot p g            = g { _eMapGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasSpellCheck EMap
  where attrSetSpellCheck p g      = g { _eMapGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasStyle EMap
  where attrSetStyle p g           = g { _eMapGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasTabIndex EMap
  where attrSetTabIndex p g        = g { _eMapGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasTitle EMap
  where attrSetTitle p g           = g { _eMapGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMapGlobals g))) }
instance AttrHasTranslate EMap
  where attrSetTranslate p g       = g { _eMapGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMapGlobals g))) }

instance AttrGetClassName EMap where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMapGlobals g)

instance AttrHasName EMap where attrSetName p g = g {_eMapName = Just p }

instance AttrHasCustom EMap where attrSetCustom p g       = g { _eMapCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"map\" ... @
eMap' :: forall t m a. DomBuilder t m => EMap -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMap' b  = elAttr' "map" (attrMap b)

-- | A short-hand notion for @ elAttr \"map\" ... @
eMap :: forall t m a. DomBuilder t m => EMap -> m a -> m a
eMap b children = snd <$> eMap' b children

-- | A short-hand notion for @ el\' \"map\" ... @
eMapN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMapN' = el' "map"

-- | A short-hand notion for @ el \"map\" ... @
eMapN :: forall t m a. DomBuilder t m => m a -> m a
eMapN children = snd <$> eMapN' children

-- | A short-hand notion for @ elDynAttr\' \"map\" ... @
eMapD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EMap -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMapD' b = elDynAttr' "map" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"map\" ... @
eMapD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EMap -> m a -> m a
eMapD b children = snd <$> eMapD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Object-element
data EObject = EObject
  { _eObjectGlobals       :: Maybe Globals
  , _eObjectDataUrl       :: Maybe DataUrl
  , _eObjectForm          :: Maybe Form
  , _eObjectHeight        :: Maybe Height
  , _eObjectName          :: Maybe Name
  , _eObjectMediaType     :: Maybe MediaType  -- type
  , _eObjectTypeMustMatch :: Maybe TypeMustMatch
  , _eObjectWidth         :: Maybe Width
  , _eObjectCustom        :: Maybe Attr
  }


instance AttrMap EObject where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eObjectGlobals b
    , attrMap <$> _eObjectDataUrl b
    , attrMap <$> _eObjectForm b
    , attrMap <$> _eObjectHeight b
    , attrMap <$> _eObjectName b
    , attrMap <$> _eObjectMediaType b
    , attrMap <$> _eObjectTypeMustMatch b
    , attrMap <$> _eObjectWidth b
    ] <> maybeToList (_eObjectCustom b)

instance Default EObject where
  def = EObject def def def def def def def def def

instance Monoid EObject where
  mempty = def
  mappend (EObject a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (EObject b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = EObject (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

instance AttrHasGlobals EObject where
   attrSetGlobals p b = b { _eObjectGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EObject
  where attrSetAccessKey p g = g { _eObjectGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasAnmval EObject
  where attrSetAnmval p g = g { _eObjectGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasContentEditable EObject
  where attrSetContentEditable p g = g  { _eObjectGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasContextMenu EObject
  where attrSetContextMenu p g     = g { _eObjectGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasClass EObject
  where attrSetClassName p g           = g { _eObjectGlobals = Just (attrSetClassName p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasDnmval EObject
  where attrSetDnmval p g           = g { _eObjectGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasDir EObject
  where attrSetDir p g             = g { _eObjectGlobals = Just (attrSetDir p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasDraggable EObject
  where attrSetDraggable p g       = g { _eObjectGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasHidden EObject
  where attrSetHidden p g          = g { _eObjectGlobals = Just (attrSetHidden p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasId EObject
  where attrSetId p g              = g { _eObjectGlobals = Just (attrSetId p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasLang EObject
  where attrSetLang p g            = g { _eObjectGlobals = Just (attrSetLang p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasRole EObject
  where attrSetRole p g            = g { _eObjectGlobals = Just (attrSetRole p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasSlot EObject
  where attrSetSlot p g            = g { _eObjectGlobals = Just (attrSetSlot p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasSpellCheck EObject
  where attrSetSpellCheck p g      = g { _eObjectGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasStyle EObject
  where attrSetStyle p g           = g { _eObjectGlobals = Just (attrSetStyle p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasTabIndex EObject
  where attrSetTabIndex p g        = g { _eObjectGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasTitle EObject
  where attrSetTitle p g           = g { _eObjectGlobals = Just (attrSetTitle p (fromMaybe gDef (_eObjectGlobals g))) }
instance AttrHasTranslate EObject
  where attrSetTranslate p g       = g { _eObjectGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eObjectGlobals g))) }

instance AttrGetClassName EObject where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eObjectGlobals g)

instance AttrHasDataUrl EObject where attrSetDataUrl p g = g {_eObjectDataUrl = Just p }
instance AttrHasForm EObject where attrSetForm p g = g {_eObjectForm = Just p }
instance AttrHasHeight EObject where attrSetHeight p g = g {_eObjectHeight = Just p }
instance AttrHasName EObject where attrSetName p g = g {_eObjectName = Just p }
instance AttrHasMediaType EObject where attrSetMediaType p g = g {_eObjectMediaType = Just p }
instance AttrHasTypeMustMatch EObject where attrSetTypeMustMatch p g = g {_eObjectTypeMustMatch = Just p }
instance AttrHasWidth EObject where attrSetWidth p g = g {_eObjectWidth = Just p }

instance AttrHasCustom EObject where attrSetCustom p g       = g { _eObjectCustom = Just p }


-- | A short-hand notion for @ elAttr\' \"object\" ... @
eObject' :: forall t m a. DomBuilder t m => EObject -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eObject' b  = elAttr' "object" (attrMap b)

-- | A short-hand notion for @ elAttr \"object\" ... @
eObject :: forall t m a. DomBuilder t m => EObject -> m a -> m a
eObject b children = snd <$> eObject' b children

-- | A short-hand notion for @ el\' \"object\" ... @
eObjectN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eObjectN' = el' "object"

-- | A short-hand notion for @ el \"object\" ... @
eObjectN :: forall t m a. DomBuilder t m => m a -> m a
eObjectN children = snd <$> eObjectN' children

-- | A short-hand notion for @ elDynAttr\' \"object\" ... @
eObjectD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EObject -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eObjectD' b  = elDynAttr' "object" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"object\" ... @
eObjectD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EObject -> m a -> m a
eObjectD b children = snd <$> eObjectD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Param-element
data EParam = EParam
  { _eParamGlobals   :: Maybe Globals
  , _eParamName      :: Maybe Name
  , _eParamValueName :: Maybe ValueName
  , _eParamCustom    :: Maybe Attr
  }


instance AttrMap EParam where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eParamGlobals b
    , attrMap <$> _eParamName b
    , attrMap <$> _eParamValueName b
    ] <> maybeToList (_eParamCustom b)

instance Default EParam where
  def = EParam def def def def

instance Monoid EParam where
  mempty = def
  mappend (EParam a1 a2 a3 a4) (EParam b1 b2 b3 b4)
    = EParam (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance AttrHasGlobals EParam where
   attrSetGlobals p b = b { _eParamGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EParam
  where attrSetAccessKey p g = g { _eParamGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasAnmval EParam
  where attrSetAnmval p g = g { _eParamGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasContentEditable EParam
  where attrSetContentEditable p g = g  { _eParamGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasContextMenu EParam
  where attrSetContextMenu p g     = g { _eParamGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasClass EParam
  where attrSetClassName p g           = g { _eParamGlobals = Just (attrSetClassName p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasDnmval EParam
  where attrSetDnmval p g           = g { _eParamGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasDir EParam
  where attrSetDir p g             = g { _eParamGlobals = Just (attrSetDir p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasDraggable EParam
  where attrSetDraggable p g       = g { _eParamGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasHidden EParam
  where attrSetHidden p g          = g { _eParamGlobals = Just (attrSetHidden p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasId EParam
  where attrSetId p g              = g { _eParamGlobals = Just (attrSetId p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasLang EParam
  where attrSetLang p g            = g { _eParamGlobals = Just (attrSetLang p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasRole EParam
  where attrSetRole p g            = g { _eParamGlobals = Just (attrSetRole p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasSlot EParam
  where attrSetSlot p g            = g { _eParamGlobals = Just (attrSetSlot p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasSpellCheck EParam
  where attrSetSpellCheck p g      = g { _eParamGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasStyle EParam
  where attrSetStyle p g           = g { _eParamGlobals = Just (attrSetStyle p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasTabIndex EParam
  where attrSetTabIndex p g        = g { _eParamGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasTitle EParam
  where attrSetTitle p g           = g { _eParamGlobals = Just (attrSetTitle p (fromMaybe gDef (_eParamGlobals g))) }
instance AttrHasTranslate EParam
  where attrSetTranslate p g       = g { _eParamGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eParamGlobals g))) }

instance AttrGetClassName EParam where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eParamGlobals g)

instance AttrHasName EParam where attrSetName p g  = g { _eParamName  = Just p }

instance AttrHasValueName    EParam where attrSetValueName p g  = g { _eParamValueName  = Just p }

instance AttrHasCustom EParam where attrSetCustom p g       = g { _eParamCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"param\" ... @
eParam' :: forall t m a. DomBuilder t m => EParam -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eParam' b  = elAttr' "param" (attrMap b)

-- | A short-hand notion for @ elAttr \"param\" ... @
eParam :: forall t m a. DomBuilder t m => EParam -> m a -> m a
eParam b children = snd <$> eParam' b children

-- | A short-hand notion for @ el\' \"param\" ... @
eParamN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eParamN' = el' "param"

-- | A short-hand notion for @ el \"param\" ... @
eParamN :: forall t m a. DomBuilder t m => m a -> m a
eParamN children = snd <$> eParamN' children

-- | A short-hand notion for @ elDynAttr\' \"param\" ... @
eParamD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EParam -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eParamD' b  = elDynAttr' "param" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"param\" ... @
eParamD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EParam -> m a -> m a
eParamD b children = snd <$> eParamD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Picture-element has only the global attributes.
data EPicture = EPicture
  { _ePictureGlobals   :: Maybe Globals
  , _ePictureCustom    :: Maybe Attr
  }


instance AttrMap EPicture where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _ePictureGlobals b
    ] <> maybeToList (_ePictureCustom b)

instance Default EPicture where
  def = EPicture def def

instance Monoid EPicture where
  mempty = def
  mappend (EPicture a1 a2) (EPicture b1 b2) = EPicture (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EPicture where
   attrSetGlobals p b = b { _ePictureGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EPicture
  where attrSetAccessKey p g = g { _ePictureGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasAnmval EPicture
  where attrSetAnmval p g = g { _ePictureGlobals = Just (attrSetAnmval p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasContentEditable EPicture
  where attrSetContentEditable p g = g  { _ePictureGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasContextMenu EPicture
  where attrSetContextMenu p g     = g { _ePictureGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasClass EPicture
  where attrSetClassName p g           = g { _ePictureGlobals = Just (attrSetClassName p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasDnmval EPicture
  where attrSetDnmval p g           = g { _ePictureGlobals = Just (attrSetDnmval p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasDir EPicture
  where attrSetDir p g             = g { _ePictureGlobals = Just (attrSetDir p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasDraggable EPicture
  where attrSetDraggable p g       = g { _ePictureGlobals = Just (attrSetDraggable p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasHidden EPicture
  where attrSetHidden p g          = g { _ePictureGlobals = Just (attrSetHidden p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasId EPicture
  where attrSetId p g              = g { _ePictureGlobals = Just (attrSetId p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasLang EPicture
  where attrSetLang p g            = g { _ePictureGlobals = Just (attrSetLang p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasRole EPicture
  where attrSetRole p g            = g { _ePictureGlobals = Just (attrSetRole p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasSlot EPicture
  where attrSetSlot p g            = g { _ePictureGlobals = Just (attrSetSlot p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasSpellCheck EPicture
  where attrSetSpellCheck p g      = g { _ePictureGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasStyle EPicture
  where attrSetStyle p g           = g { _ePictureGlobals = Just (attrSetStyle p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasTabIndex EPicture
  where attrSetTabIndex p g        = g { _ePictureGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasTitle EPicture
  where attrSetTitle p g           = g { _ePictureGlobals = Just (attrSetTitle p (fromMaybe gDef (_ePictureGlobals g))) }
instance AttrHasTranslate EPicture
  where attrSetTranslate p g       = g { _ePictureGlobals = Just (attrSetTranslate p (fromMaybe gDef (_ePictureGlobals g))) }

instance AttrGetClassName EPicture where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_ePictureGlobals g)

instance AttrHasCustom EPicture where attrSetCustom p g       = g { _ePictureCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"picture\" ... @
ePicture' :: forall t m a. DomBuilder t m => EPicture -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePicture' b  = elAttr' "picture" (attrMap b)

-- | A short-hand notion for @ elAttr \"picture\" ... @
ePicture :: forall t m a. DomBuilder t m => EPicture -> m a -> m a
ePicture b children = snd <$> ePicture' b children

-- | A short-hand notion for @ el\' \"picture\" ... @
ePictureN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePictureN' = el' "picture"

-- | A short-hand notion for @ el \"picture\" ... @
ePictureN :: forall t m a. DomBuilder t m => m a -> m a
ePictureN children = snd <$> ePictureN' children

-- | A short-hand notion for @ elDynAttr\' \"picture\" ... @
ePictureD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EPicture -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ePictureD' b  = elDynAttr' "picture" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"picture\" ... @
ePictureD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EPicture -> m a -> m a
ePictureD b children = snd <$> ePictureD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Source-element
data ESource = ESource
  { _eSourceGlobals   :: Maybe Globals
  , _eSourceMedia     :: Maybe Media
  , _eSourceSizes     :: Maybe Sizes
  , _eSourceSrc       :: Maybe Src
  , _eSourceSrcSet    :: Maybe SrcSet
  , _eSourceMediaType :: Maybe MediaType
  , _eSourceCustom    :: Maybe Attr
  }


instance AttrMap ESource where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSourceGlobals b
    , attrMap <$> _eSourceMedia b
    , attrMap <$> _eSourceSizes b
    , attrMap <$> _eSourceSrc b
    , attrMap <$> _eSourceSrcSet b
    , attrMap <$> _eSourceMediaType b
    ] <> maybeToList (_eSourceCustom b)

instance Default ESource where
  def = ESource def def def def def def def

instance Monoid ESource where
  mempty = def
  mappend (ESource a1 a2 a3 a4 a5 a6 a7) (ESource b1 b2 b3 b4 b5 b6 b7)
    = ESource (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

instance AttrHasGlobals ESource where
   attrSetGlobals p b = b { _eSourceGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESource
  where attrSetAccessKey p g = g { _eSourceGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasAnmval ESource
  where attrSetAnmval p g = g { _eSourceGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasContentEditable ESource
  where attrSetContentEditable p g = g  { _eSourceGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasContextMenu ESource
  where attrSetContextMenu p g     = g { _eSourceGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasClass ESource
  where attrSetClassName p g           = g { _eSourceGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasDnmval ESource
  where attrSetDnmval p g           = g { _eSourceGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasDir ESource
  where attrSetDir p g             = g { _eSourceGlobals = Just (attrSetDir p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasDraggable ESource
  where attrSetDraggable p g       = g { _eSourceGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasHidden ESource
  where attrSetHidden p g          = g { _eSourceGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasId ESource
  where attrSetId p g              = g { _eSourceGlobals = Just (attrSetId p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasLang ESource
  where attrSetLang p g            = g { _eSourceGlobals = Just (attrSetLang p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasRole ESource
  where attrSetRole p g            = g { _eSourceGlobals = Just (attrSetRole p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasSlot ESource
  where attrSetSlot p g            = g { _eSourceGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasSpellCheck ESource
  where attrSetSpellCheck p g      = g { _eSourceGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasStyle ESource
  where attrSetStyle p g           = g { _eSourceGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasTabIndex ESource
  where attrSetTabIndex p g        = g { _eSourceGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasTitle ESource
  where attrSetTitle p g           = g { _eSourceGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSourceGlobals g))) }
instance AttrHasTranslate ESource
  where attrSetTranslate p g       = g { _eSourceGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSourceGlobals g))) }

instance AttrGetClassName ESource where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSourceGlobals g)

instance AttrHasMedia ESource where attrSetMedia p g = g {_eSourceMedia = Just p }
instance AttrHasSizes ESource where attrSetSizes p g = g {_eSourceSizes = Just p }
instance AttrHasSrc ESource where attrSetSrc p g = g {_eSourceSrc = Just p }
instance AttrHasSrcSet ESource where attrSetSrcSet p g = g {_eSourceSrcSet = Just p }
instance AttrHasMediaType ESource where attrSetMediaType p g = g {_eSourceMediaType = Just p }

instance AttrHasCustom ESource where attrSetCustom p g       = g { _eSourceCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"source\" ... @
eSource' :: forall t m a. DomBuilder t m => ESource -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSource' b  = elAttr' "source" (attrMap b)

-- | A short-hand notion for @ elAttr \"source\" ... @
eSource :: forall t m a. DomBuilder t m => ESource -> m a -> m a
eSource b children = snd <$> eSource' b children

-- | A short-hand notion for @ el\' \"source\" ... @
eSourceN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSourceN' = el' "source"

-- | A short-hand notion for @ el \"source\" ... @
eSourceN :: forall t m a. DomBuilder t m => m a -> m a
eSourceN children = snd <$> eSourceN' children

-- | A short-hand notion for @ elDynAttr\' \"source\" ... @
eSourceD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ESource -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSourceD' b  = elDynAttr' "source" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"source\" ... @
eSourceD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ESource -> m a -> m a
eSourceD b children = snd <$> eSourceD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Track-element
data ETrack = ETrack
  { _eTrackGlobals :: Maybe Globals
  , _eTrackDefault :: Maybe Default_
  , _eTrackKind    :: Maybe Kind
  , _eTrackLabel   :: Maybe Label
  , _eTrackSrc     :: Maybe Src
  , _eTrackSrcLang :: Maybe SrcLang
  , _eTrackCustom  :: Maybe Attr
  }


instance AttrMap ETrack where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTrackGlobals b
    , attrMap <$> _eTrackDefault b
    , attrMap <$> _eTrackKind b
    , attrMap <$> _eTrackLabel b
    , attrMap <$> _eTrackSrc b
    , attrMap <$> _eTrackSrcLang b
    ] <> maybeToList (_eTrackCustom b)

instance Default ETrack where
  def = ETrack def def def def def def def

instance Monoid ETrack where
  mempty = def
  mappend (ETrack a1 a2 a3 a4 a5 a6 a7) (ETrack b1 b2 b3 b4 b5 b6 b7)
    = ETrack (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

instance AttrHasGlobals ETrack where
   attrSetGlobals p b = b { _eTrackGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETrack
  where attrSetAccessKey p g = g { _eTrackGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasAnmval ETrack
  where attrSetAnmval p g = g { _eTrackGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasContentEditable ETrack
  where attrSetContentEditable p g = g  { _eTrackGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasContextMenu ETrack
  where attrSetContextMenu p g     = g { _eTrackGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasClass ETrack
  where attrSetClassName p g           = g { _eTrackGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasDnmval ETrack
  where attrSetDnmval p g           = g { _eTrackGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasDir ETrack
  where attrSetDir p g             = g { _eTrackGlobals = Just (attrSetDir p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasDraggable ETrack
  where attrSetDraggable p g       = g { _eTrackGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasHidden ETrack
  where attrSetHidden p g          = g { _eTrackGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasId ETrack
  where attrSetId p g              = g { _eTrackGlobals = Just (attrSetId p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasLang ETrack
  where attrSetLang p g            = g { _eTrackGlobals = Just (attrSetLang p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasRole ETrack
  where attrSetRole p g            = g { _eTrackGlobals = Just (attrSetRole p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasSlot ETrack
  where attrSetSlot p g            = g { _eTrackGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasSpellCheck ETrack
  where attrSetSpellCheck p g      = g { _eTrackGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasStyle ETrack
  where attrSetStyle p g           = g { _eTrackGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasTabIndex ETrack
  where attrSetTabIndex p g        = g { _eTrackGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasTitle ETrack
  where attrSetTitle p g           = g { _eTrackGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTrackGlobals g))) }
instance AttrHasTranslate ETrack
  where attrSetTranslate p g       = g { _eTrackGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTrackGlobals g))) }

instance AttrGetClassName ETrack where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTrackGlobals g)

instance AttrHasDefault ETrack where attrSetDefault p g = g {_eTrackDefault = Just p }
instance AttrHasKind ETrack where attrSetKind p g = g {_eTrackKind = Just p }
instance AttrHasLabel ETrack where attrSetLabel p g = g {_eTrackLabel = Just p }
instance AttrHasSrc ETrack where attrSetSrc p g = g {_eTrackSrc = Just p }
instance AttrHasSrcLang ETrack where attrSetSrcLang p g = g {_eTrackSrcLang = Just p }

instance AttrHasCustom ETrack where attrSetCustom p g       = g { _eTrackCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"track\" ... @
eTrack' :: forall t m a. DomBuilder t m => ETrack -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTrack' b  = elAttr' "track" (attrMap b)

-- | A short-hand notion for @ elAttr \"track\" ... @
eTrack :: forall t m a. DomBuilder t m => ETrack -> m a -> m a
eTrack b children = snd <$> eTrack' b children

-- | A short-hand notion for @ el\' \"track\" ... @
eTrackN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTrackN' = el' "track"

-- | A short-hand notion for @ el \"track\" ... @
eTrackN :: forall t m a. DomBuilder t m => m a -> m a
eTrackN children = snd <$> eTrackN' children

-- | A short-hand notion for @ elDynAttr\' \"track\" ... @
eTrackD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETrack -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTrackD' b  = elDynAttr' "track" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"track\" ... @
eTrackD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ETrack -> m a -> m a
eTrackD b children = snd <$> eTrackD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Video-element is interactive  if the constrols attribute is present.
-- globals; src; crossorigin; poster; preload; autoplay; loop; muted; controls; width; height
--
--
data EVideo = EVideo
  { _eVideoGlobals     :: Maybe Globals
  , _eVideoAutoplay    :: Maybe Autoplay
  , _eVideoControls    :: Maybe Controls
  , _eVideoCrossOrigin :: Maybe CrossOrigin
  , _eVideoHeight      :: Maybe Height
  , _eVideoLoop        :: Maybe Loop
  , _eVideoMuted       :: Maybe Muted
  , _eVideoPoster      :: Maybe Poster
  , _eVideoPreload     :: Maybe Preload
  , _eVideoSrc         :: Maybe Src
  , _eVideoWidth       :: Maybe Width
  , _eVideoCustom      :: Maybe Attr
  }


instance AttrMap EVideo where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eVideoGlobals b
    , attrMap <$> _eVideoAutoplay b
    , attrMap <$> _eVideoControls b
    , attrMap <$> _eVideoCrossOrigin b
    , attrMap <$> _eVideoHeight b
    , attrMap <$> _eVideoLoop b
    , attrMap <$> _eVideoMuted b
    , attrMap <$> _eVideoPoster b
    , attrMap <$> _eVideoPreload b
    , attrMap <$> _eVideoSrc b
    , attrMap <$> _eVideoWidth b
    ] <> maybeToList (_eVideoCustom b)

instance Default EVideo where
  def = EVideo def def def def def def def def def def def def

instance Monoid EVideo where
  mempty = def
  mappend (EVideo a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (EVideo b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = EVideo (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

instance AttrHasGlobals EVideo where
   attrSetGlobals p b = b { _eVideoGlobals = Just p }


-- Global attributes require the following instances.
instance AttrHasAccessKey EVideo
  where attrSetAccessKey p g = g { _eVideoGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasAnmval EVideo
  where attrSetAnmval p g = g { _eVideoGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasContentEditable EVideo
  where attrSetContentEditable p g = g  { _eVideoGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasContextMenu EVideo
  where attrSetContextMenu p g     = g { _eVideoGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasClass EVideo
  where attrSetClassName p g           = g { _eVideoGlobals = Just (attrSetClassName p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasDnmval EVideo
  where attrSetDnmval p g           = g { _eVideoGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasDir EVideo
  where attrSetDir p g             = g { _eVideoGlobals = Just (attrSetDir p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasDraggable EVideo
  where attrSetDraggable p g       = g { _eVideoGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasHidden EVideo
  where attrSetHidden p g          = g { _eVideoGlobals = Just (attrSetHidden p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasId EVideo
  where attrSetId p g              = g { _eVideoGlobals = Just (attrSetId p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasLang EVideo
  where attrSetLang p g            = g { _eVideoGlobals = Just (attrSetLang p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasRole EVideo
  where attrSetRole p g            = g { _eVideoGlobals = Just (attrSetRole p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasSlot EVideo
  where attrSetSlot p g            = g { _eVideoGlobals = Just (attrSetSlot p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasSpellCheck EVideo
  where attrSetSpellCheck p g      = g { _eVideoGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasStyle EVideo
  where attrSetStyle p g           = g { _eVideoGlobals = Just (attrSetStyle p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasTabIndex EVideo
  where attrSetTabIndex p g        = g { _eVideoGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasTitle EVideo
  where attrSetTitle p g           = g { _eVideoGlobals = Just (attrSetTitle p (fromMaybe gDef (_eVideoGlobals g))) }
instance AttrHasTranslate EVideo
  where attrSetTranslate p g       = g { _eVideoGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eVideoGlobals g))) }

instance AttrGetClassName EVideo where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eVideoGlobals g)

instance AttrHasAutoplay    EVideo where attrSetAutoplay p g    = g { _eVideoAutoplay    = Just p }
instance AttrHasControls    EVideo where attrSetControls p g    = g { _eVideoControls    = Just p }
instance AttrHasCrossOrigin EVideo where attrSetCrossOrigin p g = g { _eVideoCrossOrigin = Just p }
instance AttrHasHeight      EVideo where attrSetHeight p g      = g { _eVideoHeight      = Just p }
instance AttrHasLoop        EVideo where attrSetLoop p g        = g { _eVideoLoop        = Just p }
instance AttrHasMuted       EVideo where attrSetMuted p g       = g { _eVideoMuted       = Just p }
instance AttrHasPoster      EVideo where attrSetPoster p g      = g { _eVideoPoster      = Just p }
instance AttrHasPreload     EVideo where attrSetPreload p g     = g { _eVideoPreload     = Just p }
instance AttrHasSrc         EVideo where attrSetSrc p g         = g { _eVideoSrc         = Just p }
instance AttrHasWidth       EVideo where attrSetWidth p g       = g { _eVideoWidth       = Just p }

instance AttrHasCustom EVideo where attrSetCustom p g       = g { _eVideoCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"video\" ... @
eVideo' :: forall t m a. DomBuilder t m => EVideo -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVideo' b  = elAttr' "video" (attrMap b)

-- | A short-hand notion for @ elAttr \"video\" ... @
eVideo :: forall t m a. DomBuilder t m => EVideo -> m a -> m a
eVideo b children = snd <$> eVideo' b children

-- | A short-hand notion for @ el\' \"video\" ... @
eVideoN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVideoN' = el' "video"

-- | A short-hand notion for @ el \"video\" ... @
eVideoN :: forall t m a. DomBuilder t m => m a -> m a
eVideoN children = snd <$> eVideoN' children

-- | A short-hand notion for @ elDynAttr\' \"video\" ... @
eVideoD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EVideo -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eVideoD' b  = elDynAttr' "video" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"video\" ... @
eVideoD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EVideo -> m a -> m a
eVideoD b children = snd <$> eVideoD' b children


eVideoC :: (DomBuilder t m, PostBuild t m) => EVideo -> m () -> m (Event t ())
eVideoC b children = do
  (e,_) <- elAttr' "video" (attrMap b) children
  return $ domEvent Click e

eVideoCD :: (DomBuilder t m, PostBuild t m) => Dynamic t EVideo -> m () -> m (Event t ())
eVideoCD bDyn children = do
  (e,_) <- elDynAttr' "video" (attrMap <$> bDyn) children
  return $ domEvent Click e

