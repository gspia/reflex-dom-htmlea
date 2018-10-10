{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
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

import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', el', Event, EventName (Click),
                       domEvent)

import qualified Reflex.Dom.HTML5.Attrs as A

------------------------------------------------------------------------------

-- | Area-element
data Area = Area
  { _areaGlobals        ∷ Maybe A.Globals
  , _areaAlt            ∷ Maybe A.Alt_
  , _areaCoords         ∷ Maybe A.Coords
  , _areaShape          ∷ Maybe A.Shape
  , _areaHref           ∷ Maybe A.Href
  , _areaTarget         ∷ Maybe A.Target
  , _areaDownload       ∷ Maybe A.Download
  , _areaRel            ∷ Maybe A.Rel
  , _areaHrefLang       ∷ Maybe A.HrefLang
  , _areaMediaType      ∷ Maybe A.MediaType
  , _areaReferrerPolicy ∷ Maybe A.ReferrerPolicy
  , _areaCustom         ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Area where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _areaGlobals bm
    , A.attrMap <$> _areaAlt bm
    , A.attrMap <$> _areaCoords bm
    , A.attrMap <$> _areaShape bm
    , A.attrMap <$> _areaHref bm
    , A.attrMap <$> _areaTarget bm
    , A.attrMap <$> _areaDownload bm
    , A.attrMap <$> _areaRel bm
    , A.attrMap <$> _areaHrefLang bm
    , A.attrMap <$> _areaMediaType bm
    , A.attrMap <$> _areaReferrerPolicy bm
    ] <> maybeToList (_areaCustom bm)

-- | A default value for Area.
defArea ∷ Area
defArea = Area Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Area where
  (<>) (Area a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (Area b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = Area (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

-- | An instance.
instance Monoid Area where
  mempty = defArea
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Area where
   attrSetGlobals pp bm = bm { _areaGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Area
  where attrSetAccessKey pp g = g { _areaGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Area
  where attrSetAnmval pp g = g { _areaGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Area
  where attrSetContentEditable pp g = g  { _areaGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Area
  where attrSetContextMenu pp g     = g { _areaGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasClass Area
  where attrSetClassName pp g           = g { _areaGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Area
  where attrSetDnmval pp g           = g { _areaGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasDir Area
  where attrSetDir pp g             = g { _areaGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Area
  where attrSetDraggable pp g       = g { _areaGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Area
  where attrSetHidden pp g          = g { _areaGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasId Area
  where attrSetId pp g              = g { _areaGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasLang Area
  where attrSetLang pp g            = g { _areaGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasRole Area
  where attrSetRole pp g            = g { _areaGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Area
  where attrSetSlot pp g            = g { _areaGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Area
  where attrSetSpellCheck pp g      = g { _areaGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Area
  where attrSetStyle pp g           = g { _areaGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Area
  where attrSetTabIndex pp g        = g { _areaGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Area
  where attrSetTitle pp g           = g { _areaGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_areaGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Area
  where attrSetTranslate pp g       = g { _areaGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_areaGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Area where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_areaGlobals g)

-- | An instance.
instance A.AttrHasAlt Area where attrSetAlt pp g = g {_areaAlt = Just pp }
-- | An instance.
instance A.AttrHasCoords Area where attrSetCoords pp g = g {_areaCoords = Just pp }
-- | An instance.
instance A.AttrHasShape Area where attrSetShape pp g = g {_areaShape = Just pp }
-- | An instance.
instance A.AttrHasHref Area where attrSetHref pp g = g {_areaHref = Just pp }
-- | An instance.
instance A.AttrHasTarget Area where attrSetTarget pp g = g {_areaTarget = Just pp }
-- | An instance.
instance A.AttrHasDownload Area where attrSetDownload pp g = g {_areaDownload = Just pp }
-- | An instance.
instance A.AttrHasRel Area where attrSetRel pp g = g {_areaRel = Just pp }
-- | An instance.
instance A.AttrHasHrefLang Area where attrSetHrefLang pp g = g {_areaHrefLang = Just pp }
-- | An instance.
instance A.AttrHasMediaType Area where attrSetMediaType pp g = g {_areaMediaType = Just pp }
-- | An instance.
instance A.AttrHasReferrerPolicy Area where attrSetReferrerPolicy pp g = g {_areaReferrerPolicy = Just pp }

-- | An instance.
instance A.AttrHasCustom Area where attrSetCustom pp g       = g { _areaCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"area\" ... @
area' ∷ forall t m a. DomBuilder t m ⇒ Area → m a → m (Element EventResult (DomBuilderSpace m) t, a)
area' bm  = elAttr' "area" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"area\" ... @
area ∷ forall t m a. DomBuilder t m ⇒ Area → m a → m a
area bm children = snd <$> area' bm children

-- | A short-hand notion for @ el\' \"area\" ... @
areaN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
areaN' = el' "area"

-- | A short-hand notion for @ el \"area\" ... @
areaN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
areaN children = snd <$> areaN' children

-- | A short-hand notion for @ elDynAttr\' \"area\" ... @
areaD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Area → m a → m (Element EventResult (DomBuilderSpace m) t, a)
areaD' bm = elDynAttr' "area" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"area\" ... @
areaD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Area → m a → m a
areaD bm children = snd <$> areaD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Audio-element is interactive  if the controls attribute is present
-- globals; src; crossorigin; preload; autoplay; loop; muted; controls
--
data Audio = Audio
  { _audioGlobals     ∷ Maybe A.Globals
  , _audioSrc         ∷ Maybe A.Src
  , _audioCrossOrigin ∷ Maybe A.CrossOrigin
  , _audioPreload     ∷ Maybe A.Preload
  , _audioAutoplay    ∷ Maybe A.Autoplay
  , _audioLoop        ∷ Maybe A.Loop
  , _audioMuted       ∷ Maybe A.Muted
  , _audioControls    ∷ Maybe A.Controls
  , _audioCustom      ∷ Maybe A.Attr
  }



-- | An instance.
instance A.AttrMap Audio where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _audioGlobals bm
    , A.attrMap <$> _audioSrc bm
    , A.attrMap <$> _audioCrossOrigin bm
    , A.attrMap <$> _audioPreload bm
    , A.attrMap <$> _audioAutoplay bm
    , A.attrMap <$> _audioLoop bm
    , A.attrMap <$> _audioMuted bm
    , A.attrMap <$> _audioControls bm
    ] <> maybeToList (_audioCustom bm)

-- | A default value for Audio.
defAudio ∷ Audio
defAudio = Audio Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Audio where
  (<>) (Audio a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (Audio b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = Audio (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
             (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

-- | An instance.
instance Monoid Audio where
  mempty = defAudio
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Audio where
   attrSetGlobals pp bm = bm { _audioGlobals = Just pp }


-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Audio
  where attrSetAccessKey pp g = g { _audioGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Audio
  where attrSetAnmval pp g = g { _audioGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Audio
  where attrSetContentEditable pp g = g  { _audioGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Audio
  where attrSetContextMenu pp g     = g { _audioGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasClass Audio
  where attrSetClassName pp g           = g { _audioGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Audio
  where attrSetDnmval pp g           = g { _audioGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasDir Audio
  where attrSetDir pp g             = g { _audioGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Audio
  where attrSetDraggable pp g       = g { _audioGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Audio
  where attrSetHidden pp g          = g { _audioGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasId Audio
  where attrSetId pp g              = g { _audioGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasLang Audio
  where attrSetLang pp g            = g { _audioGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasRole Audio
  where attrSetRole pp g            = g { _audioGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Audio
  where attrSetSlot pp g            = g { _audioGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Audio
  where attrSetSpellCheck pp g      = g { _audioGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Audio
  where attrSetStyle pp g           = g { _audioGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Audio
  where attrSetTabIndex pp g        = g { _audioGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Audio
  where attrSetTitle pp g           = g { _audioGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_audioGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Audio
  where attrSetTranslate pp g       = g { _audioGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_audioGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Audio where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_audioGlobals g)

-- | An instance.
instance A.AttrHasSrc         Audio where attrSetSrc pp g         = g { _audioSrc         = Just pp }
-- | An instance.
instance A.AttrHasCrossOrigin Audio where attrSetCrossOrigin pp g = g { _audioCrossOrigin = Just pp }
-- | An instance.
instance A.AttrHasPreload     Audio where attrSetPreload pp g     = g { _audioPreload     = Just pp }
-- | An instance.
instance A.AttrHasAutoplay    Audio where attrSetAutoplay pp g    = g { _audioAutoplay    = Just pp }
-- | An instance.
instance A.AttrHasLoop        Audio where attrSetLoop pp g        = g { _audioLoop        = Just pp }
-- | An instance.
instance A.AttrHasMuted       Audio where attrSetMuted pp g       = g { _audioMuted       = Just pp }
-- | An instance.
instance A.AttrHasControls    Audio where attrSetControls pp g    = g { _audioControls    = Just pp }


-- | An instance.
instance A.AttrHasCustom Audio where attrSetCustom pp g       = g { _audioCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"audio\" ... @
audio' ∷ forall t m a. DomBuilder t m ⇒ Audio → m a → m (Element EventResult (DomBuilderSpace m) t, a)
audio' bm  = elAttr' "audio" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"audio\" ... @
audio ∷ forall t m a. DomBuilder t m ⇒ Audio → m a → m a
audio bm children = snd <$> audio' bm children

-- | A short-hand notion for @ el\' \"audio\" ... @
audioN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
audioN' = el' "audio"

-- | A short-hand notion for @ el \"audio\" ... @
audioN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
audioN children = snd <$> audioN' children

-- | A short-hand notion for @ elDynAttr\' \"audio\" ... @
audioD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Audio → m a → m (Element EventResult (DomBuilderSpace m) t, a)
audioD' bm  = elDynAttr' "audio" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"audio\" ... @
audioD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Audio → m a → m a
audioD bm children = snd <$> audioD' bm children


audioC ∷ (DomBuilder t m, PostBuild t m) ⇒ Audio → m () → m (Event t ())
audioC bm children = do
  (e,_) ← elAttr' "audio" (A.attrMap bm) children
  return $ domEvent Click e

audioCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Audio → m () → m (Event t ())
audioCD bDyn children = do
  (e,_) ← elDynAttr' "audio" (A.attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Canvas-element
data Canvas = Canvas
  { _canvasGlobals ∷ Maybe A.Globals
  , _canvasHeight  ∷ Maybe A.Height
  , _canvasWidth   ∷ Maybe A.Width
  , _canvasCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Canvas where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _canvasGlobals bm
    , A.attrMap <$> _canvasHeight bm
    , A.attrMap <$> _canvasWidth bm
    ] <> maybeToList (_canvasCustom bm)

-- | A default value for Canvas.
defCanvas ∷ Canvas
defCanvas = Canvas Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Canvas where
  (<>) (Canvas a1 a2 a3 a4) (Canvas b1 b2 b3 b4)
    = Canvas (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Canvas where
  mempty = defCanvas
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Canvas where
   attrSetGlobals pp bm = bm { _canvasGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Canvas
  where attrSetAccessKey pp g = g { _canvasGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Canvas
  where attrSetAnmval pp g = g { _canvasGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Canvas
  where attrSetContentEditable pp g = g  { _canvasGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Canvas
  where attrSetContextMenu pp g     = g { _canvasGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasClass Canvas
  where attrSetClassName pp g           = g { _canvasGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Canvas
  where attrSetDnmval pp g           = g { _canvasGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasDir Canvas
  where attrSetDir pp g             = g { _canvasGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Canvas
  where attrSetDraggable pp g       = g { _canvasGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Canvas
  where attrSetHidden pp g          = g { _canvasGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasId Canvas
  where attrSetId pp g              = g { _canvasGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasLang Canvas
  where attrSetLang pp g            = g { _canvasGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasRole Canvas
  where attrSetRole pp g            = g { _canvasGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Canvas
  where attrSetSlot pp g            = g { _canvasGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Canvas
  where attrSetSpellCheck pp g      = g { _canvasGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Canvas
  where attrSetStyle pp g           = g { _canvasGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Canvas
  where attrSetTabIndex pp g        = g { _canvasGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Canvas
  where attrSetTitle pp g           = g { _canvasGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_canvasGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Canvas
  where attrSetTranslate pp g       = g { _canvasGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_canvasGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Canvas where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_canvasGlobals g)

-- | An instance.
instance A.AttrHasHeight Canvas where attrSetHeight pp g = g {_canvasHeight = Just pp }
-- | An instance.
instance A.AttrHasWidth Canvas where attrSetWidth pp g = g {_canvasWidth = Just pp }

-- | An instance.
instance A.AttrHasCustom Canvas where attrSetCustom pp g       = g { _canvasCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"canvas\" ... @
canvas' ∷ forall t m a. DomBuilder t m ⇒ Canvas → m a → m (Element EventResult (DomBuilderSpace m) t, a)
canvas' bm  = elAttr' "canvas" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"canvas\" ... @
canvas ∷ forall t m a. DomBuilder t m ⇒ Canvas → m a → m a
canvas bm children = snd <$> canvas' bm children

-- | A short-hand notion for @ el\' \"canvas\" ... @
canvasN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
canvasN' = el' "canvas"

-- | A short-hand notion for @ el \"canvas\" ... @
canvasN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
canvasN children = snd <$> canvasN' children

-- | A short-hand notion for @ elDynAttr\' \"canvas\" ... @
canvasD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Canvas → m a → m (Element EventResult (DomBuilderSpace m) t, a)
canvasD' bm = elDynAttr' "canvas" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"canvas\" ... @
canvasD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Canvas → m a → m a
canvasD bm children = snd <$> canvasD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | IFrame-element has the following attributes:
-- globals; src; srcdoc; name; sandbox; width; height
--
data IFrame = IFrame
  { _iFramglobals ∷ Maybe A.Globals
  , _iFramsrc     ∷ Maybe A.Src
  , _iFramsrcDoc  ∷ Maybe A.SrcDoc
  , _iFramsandbox ∷ Maybe A.Sandbox
  , _iFramwidth   ∷ Maybe A.Width
  , _iFramheight  ∷ Maybe A.Height
  , _iFramcustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap IFrame where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _iFramglobals bm
    , A.attrMap <$> _iFramsrc bm
    , A.attrMap <$> _iFramsrcDoc bm
    , A.attrMap <$> _iFramsandbox bm
    , A.attrMap <$> _iFramwidth bm
    , A.attrMap <$> _iFramheight bm
    ] <> maybeToList (_iFramcustom bm)

-- | A default value for IFrame.
defIFrame ∷ IFrame
defIFrame = IFrame Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup IFrame where
  (<>) (IFrame a1 a2 a3 a4 a5 a6 a7) (IFrame b1 b2 b3 b4 b5 b6 b7)
    = IFrame (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)
        (a5 <> b5) (a6 <> b6) (a7 <> b7)

-- | An instance.
instance Monoid IFrame where
  mempty = defIFrame
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals IFrame where
   attrSetGlobals pp bm = bm { _iFramglobals = Just pp }


-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey IFrame
  where attrSetAccessKey pp g = g { _iFramglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasAnmval IFrame
  where attrSetAnmval pp g = g { _iFramglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable IFrame
  where attrSetContentEditable pp g = g  { _iFramglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu IFrame
  where attrSetContextMenu pp g     = g { _iFramglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasClass IFrame
  where attrSetClassName pp g           = g { _iFramglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasDnmval IFrame
  where attrSetDnmval pp g           = g { _iFramglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasDir IFrame
  where attrSetDir pp g             = g { _iFramglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasDraggable IFrame
  where attrSetDraggable pp g       = g { _iFramglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasHidden IFrame
  where attrSetHidden pp g          = g { _iFramglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasId IFrame
  where attrSetId pp g              = g { _iFramglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasLang IFrame
  where attrSetLang pp g            = g { _iFramglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasRole IFrame
  where attrSetRole pp g            = g { _iFramglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasSlot IFrame
  where attrSetSlot pp g            = g { _iFramglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck IFrame
  where attrSetSpellCheck pp g      = g { _iFramglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasStyle IFrame
  where attrSetStyle pp g           = g { _iFramglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex IFrame
  where attrSetTabIndex pp g        = g { _iFramglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasTitle IFrame
  where attrSetTitle pp g           = g { _iFramglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_iFramglobals g))) }
-- | An instance.
instance A.AttrHasTranslate IFrame
  where attrSetTranslate pp g       = g { _iFramglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_iFramglobals g))) }

-- | An instance.
instance A.AttrGetClassName IFrame where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_iFramglobals g)

-- | An instance.
instance A.AttrHasSrc     IFrame where attrSetSrc pp g     = g { _iFramsrc     = Just pp }
-- | An instance.
instance A.AttrHasSrcDoc  IFrame where attrSetSrcDoc pp g  = g { _iFramsrcDoc  = Just pp }
-- | An instance.
instance A.AttrHasSandbox IFrame where attrSetSandbox pp g = g { _iFramsandbox = Just pp }
-- | An instance.
instance A.AttrHasWidth   IFrame where attrSetWidth pp g   = g { _iFramwidth   = Just pp }
-- | An instance.
instance A.AttrHasHeight  IFrame where attrSetHeight pp g  = g { _iFramheight  = Just pp }

-- | An instance.
instance A.AttrHasCustom IFrame where attrSetCustom pp g       = g { _iFramcustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"iframe\" ... @
iFrame' ∷ forall t m a. DomBuilder t m ⇒ IFrame → m a → m (Element EventResult (DomBuilderSpace m) t, a)
iFrame' bm  = elAttr' "iframe" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"iframe\" ... @
iFrame ∷ forall t m a. DomBuilder t m ⇒ IFrame → m a → m a
iFrame bm children = snd <$> iFrame' bm children

-- | A short-hand notion for @ el\' \"iframe\" ... @
iFrameN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
iFrameN' = el' "iframe"

-- | A short-hand notion for @ el \"iframe\" ... @
iFrameN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
iFrameN children = snd <$> iFrameN' children

-- | A short-hand notion for @ elDynAttr\' \"iframe\" ... @
iFrameD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t IFrame → m a → m (Element EventResult (DomBuilderSpace m) t, a)
iFrameD' bm  = elDynAttr' "iframe" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"iframe\" ... @
iFrameD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t IFrame → m a → m a
iFrameD bm children = snd <$> iFrameD' bm children


iFrameC ∷ (DomBuilder t m, PostBuild t m) ⇒ IFrame → m () → m (Event t ())
iFrameC bm children = do
  (e,_) ← elAttr' "iframe" (A.attrMap bm) children
  return $ domEvent Click e

iFrameCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t IFrame → m () → m (Event t ())
iFrameCD bDyn children = do
  (e,_) ← elDynAttr' "iframe" (A.attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Img-element is interactive  if the usemap attribute is present
-- globals; alt; src; srcset; crossorigin; usemap; ismap; longdesc; width; height; referrerpolicy
--
data Img = Img
  { _imgGlobals        ∷ Maybe A.Globals
  , _imgAlt            ∷ Maybe A.Alt_
  , _imgCrossOrigin    ∷ Maybe A.CrossOrigin
  , _imgHeight         ∷ Maybe A.Height
  , _imgIsMap          ∷ Maybe A.IsMap
  , _imgLongDesc       ∷ Maybe A.LongDesc
  , _imgReferrerPolicy ∷ Maybe A.ReferrerPolicy
  , _imgSrc            ∷ Maybe A.Src
  , _imgSrcSet         ∷ Maybe A.SrcSet
  , _imgUseMap         ∷ Maybe A.UseMap
  , _imgWidth          ∷ Maybe A.Width
  , _imgCustom         ∷ Maybe A.Attr
  }



-- | An instance.
instance A.AttrMap Img where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _imgGlobals bm
    , A.attrMap <$> _imgAlt bm
    , A.attrMap <$> _imgCrossOrigin bm
    , A.attrMap <$> _imgHeight bm
    , A.attrMap <$> _imgIsMap bm
    , A.attrMap <$> _imgLongDesc bm
    , A.attrMap <$> _imgReferrerPolicy bm
    , A.attrMap <$> _imgSrc bm
    , A.attrMap <$> _imgSrcSet bm
    , A.attrMap <$> _imgUseMap bm
    , A.attrMap <$> _imgWidth bm
    ] <> maybeToList (_imgCustom bm)

-- | A default value for Img.
defImg ∷ Img
defImg = Img Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Img where
  (<>) (Img a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (Img b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = Img (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
       (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

-- | An instance.
instance Monoid Img where
  mempty = defImg
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Img where
   attrSetGlobals pp bm = bm { _imgGlobals = Just pp }


-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Img
  where attrSetAccessKey pp g = g { _imgGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Img
  where attrSetAnmval pp g = g { _imgGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Img
  where attrSetContentEditable pp g = g  { _imgGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Img
  where attrSetContextMenu pp g     = g { _imgGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasClass Img
  where attrSetClassName pp g           = g { _imgGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Img
  where attrSetDnmval pp g           = g { _imgGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasDir Img
  where attrSetDir pp g             = g { _imgGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Img
  where attrSetDraggable pp g       = g { _imgGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Img
  where attrSetHidden pp g          = g { _imgGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasId Img
  where attrSetId pp g              = g { _imgGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasLang Img
  where attrSetLang pp g            = g { _imgGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasRole Img
  where attrSetRole pp g            = g { _imgGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Img
  where attrSetSlot pp g            = g { _imgGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Img
  where attrSetSpellCheck pp g      = g { _imgGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Img
  where attrSetStyle pp g           = g { _imgGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Img
  where attrSetTabIndex pp g        = g { _imgGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Img
  where attrSetTitle pp g           = g { _imgGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_imgGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Img
  where attrSetTranslate pp g       = g { _imgGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_imgGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Img where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_imgGlobals g)

-- | An instance.
instance A.AttrHasAlt            Img where attrSetAlt pp g            = g { _imgAlt            = Just pp }
-- | An instance.
instance A.AttrHasCrossOrigin    Img where attrSetCrossOrigin pp g    = g { _imgCrossOrigin    = Just pp }
-- | An instance.
instance A.AttrHasHeight         Img where attrSetHeight pp g         = g { _imgHeight         = Just pp }
-- | An instance.
instance A.AttrHasIsMap          Img where attrSetIsMap pp g          = g { _imgIsMap          = Just pp }
-- | An instance.
instance A.AttrHasLongDesc       Img where attrSetLongDesc pp g       = g { _imgLongDesc       = Just pp }
-- | An instance.
instance A.AttrHasReferrerPolicy Img where attrSetReferrerPolicy pp g = g { _imgReferrerPolicy = Just pp }
-- | An instance.
instance A.AttrHasSrc            Img where attrSetSrc pp g            = g { _imgSrc            = Just pp }
-- | An instance.
instance A.AttrHasSrcSet         Img where attrSetSrcSet pp g         = g { _imgSrcSet         = Just pp }
-- | An instance.
instance A.AttrHasUseMap         Img where attrSetUseMap pp g         = g { _imgUseMap         = Just pp }
-- | An instance.
instance A.AttrHasWidth          Img where attrSetWidth pp g          = g { _imgWidth          = Just pp }


-- | An instance.
instance A.AttrHasCustom Img where attrSetCustom pp g       = g { _imgCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"img\" ... @
img' ∷ forall t m a. DomBuilder t m ⇒ Img → m a → m (Element EventResult (DomBuilderSpace m) t, a)
img' bm  = elAttr' "img" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"img\" ... @
img ∷ forall t m a. DomBuilder t m ⇒ Img → m a → m a
img bm children = snd <$> img' bm children

-- | A short-hand notion for @ el\' \"img\" ... @
imgN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
imgN' = el' "img"

-- | A short-hand notion for @ el \"img\" ... @
imgN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
imgN children = snd <$> imgN' children

-- | A short-hand notion for @ elDynAttr\' \"img\" ... @
imgD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Img → m a → m (Element EventResult (DomBuilderSpace m) t, a)
imgD' bm  = elDynAttr' "img" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"img\" ... @
imgD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Img → m a → m a
imgD bm children = snd <$> imgD' bm children


imgC ∷ (DomBuilder t m, PostBuild t m) ⇒ Img → m () → m (Event t ())
imgC bm children = do
  (e,_) ← elAttr' "img" (A.attrMap bm) children
  return $ domEvent Click e

imgCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Img → m () → m (Event t ())
imgCD bDyn children = do
  (e,_) ← elDynAttr' "img" (A.attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Map-element
data Map = Map
  { _mapGlobals ∷ Maybe A.Globals
  , _mapName    ∷ Maybe A.Name
  , _mapCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Map where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _mapGlobals bm
    , A.attrMap <$> _mapName bm
    ] <> maybeToList (_mapCustom bm)

-- | A default value for Map.
defMap ∷ Map
defMap = Map Nothing Nothing Nothing

-- | An instance.
instance Semigroup Map where
  (<>) (Map a1 a2 a3) (Map b1 b2 b3) = Map (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Map where
  mempty = defMap
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Map where
   attrSetGlobals pp bm = bm { _mapGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Map
  where attrSetAccessKey pp g = g { _mapGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Map
  where attrSetAnmval pp g = g { _mapGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Map
  where attrSetContentEditable pp g = g  { _mapGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Map
  where attrSetContextMenu pp g     = g { _mapGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasClass Map
  where attrSetClassName pp g           = g { _mapGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Map
  where attrSetDnmval pp g           = g { _mapGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasDir Map
  where attrSetDir pp g             = g { _mapGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Map
  where attrSetDraggable pp g       = g { _mapGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Map
  where attrSetHidden pp g          = g { _mapGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasId Map
  where attrSetId pp g              = g { _mapGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasLang Map
  where attrSetLang pp g            = g { _mapGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasRole Map
  where attrSetRole pp g            = g { _mapGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Map
  where attrSetSlot pp g            = g { _mapGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Map
  where attrSetSpellCheck pp g      = g { _mapGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Map
  where attrSetStyle pp g           = g { _mapGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Map
  where attrSetTabIndex pp g        = g { _mapGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Map
  where attrSetTitle pp g           = g { _mapGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_mapGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Map
  where attrSetTranslate pp g       = g { _mapGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_mapGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Map where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_mapGlobals g)

-- | An instance.
instance A.AttrHasName Map where attrSetName pp g = g {_mapName = Just pp }

-- | An instance.
instance A.AttrHasCustom Map where attrSetCustom pp g       = g { _mapCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"map\" ... @
map' ∷ forall t m a. DomBuilder t m ⇒ Map → m a → m (Element EventResult (DomBuilderSpace m) t, a)
map' bm  = elAttr' "map" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"map\" ... @
map ∷ forall t m a. DomBuilder t m ⇒ Map → m a → m a
map bm children = snd <$> map' bm children

-- | A short-hand notion for @ el\' \"map\" ... @
mapN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
mapN' = el' "map"

-- | A short-hand notion for @ el \"map\" ... @
mapN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
mapN children = snd <$> mapN' children

-- | A short-hand notion for @ elDynAttr\' \"map\" ... @
mapD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Map → m a → m (Element EventResult (DomBuilderSpace m) t, a)
mapD' bm = elDynAttr' "map" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"map\" ... @
mapD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Map → m a → m a
mapD bm children = snd <$> mapD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Object-element
data Object = Object
  { _objectGlobals       ∷ Maybe A.Globals
  , _objectDataUrl       ∷ Maybe A.DataUrl
  , _objectForm          ∷ Maybe A.Form
  , _objectHeight        ∷ Maybe A.Height
  , _objectName          ∷ Maybe A.Name
  , _objectMediaType     ∷ Maybe A.MediaType  -- type
  , _objectTypeMustMatch ∷ Maybe A.TypeMustMatch
  , _objectWidth         ∷ Maybe A.Width
  , _objectCustom        ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Object where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _objectGlobals bm
    , A.attrMap <$> _objectDataUrl bm
    , A.attrMap <$> _objectForm bm
    , A.attrMap <$> _objectHeight bm
    , A.attrMap <$> _objectName bm
    , A.attrMap <$> _objectMediaType bm
    , A.attrMap <$> _objectTypeMustMatch bm
    , A.attrMap <$> _objectWidth bm
    ] <> maybeToList (_objectCustom bm)

-- | A default value for Object.
defObject ∷ Object
defObject = Object Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Object where
  (<>) (Object a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (Object b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = Object (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

-- | An instance.
instance Monoid Object where
  mempty = defObject
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Object where
   attrSetGlobals pp bm = bm { _objectGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Object
  where attrSetAccessKey pp g = g { _objectGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Object
  where attrSetAnmval pp g = g { _objectGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Object
  where attrSetContentEditable pp g = g  { _objectGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Object
  where attrSetContextMenu pp g     = g { _objectGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasClass Object
  where attrSetClassName pp g           = g { _objectGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Object
  where attrSetDnmval pp g           = g { _objectGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasDir Object
  where attrSetDir pp g             = g { _objectGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Object
  where attrSetDraggable pp g       = g { _objectGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Object
  where attrSetHidden pp g          = g { _objectGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasId Object
  where attrSetId pp g              = g { _objectGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasLang Object
  where attrSetLang pp g            = g { _objectGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasRole Object
  where attrSetRole pp g            = g { _objectGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Object
  where attrSetSlot pp g            = g { _objectGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Object
  where attrSetSpellCheck pp g      = g { _objectGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Object
  where attrSetStyle pp g           = g { _objectGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Object
  where attrSetTabIndex pp g        = g { _objectGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Object
  where attrSetTitle pp g           = g { _objectGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_objectGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Object
  where attrSetTranslate pp g       = g { _objectGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_objectGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Object where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_objectGlobals g)

-- | An instance.
instance A.AttrHasDataUrl Object where attrSetDataUrl pp g = g {_objectDataUrl = Just pp }
-- | An instance.
instance A.AttrHasForm Object where attrSetForm pp g = g {_objectForm = Just pp }
-- | An instance.
instance A.AttrHasHeight Object where attrSetHeight pp g = g {_objectHeight = Just pp }
-- | An instance.
instance A.AttrHasName Object where attrSetName pp g = g {_objectName = Just pp }
-- | An instance.
instance A.AttrHasMediaType Object where attrSetMediaType pp g = g {_objectMediaType = Just pp }
-- | An instance.
instance A.AttrHasTypeMustMatch Object where attrSetTypeMustMatch pp g = g {_objectTypeMustMatch = Just pp }
-- | An instance.
instance A.AttrHasWidth Object where attrSetWidth pp g = g {_objectWidth = Just pp }

-- | An instance.
instance A.AttrHasCustom Object where attrSetCustom pp g       = g { _objectCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"object\" ... @
object' ∷ forall t m a. DomBuilder t m ⇒ Object → m a → m (Element EventResult (DomBuilderSpace m) t, a)
object' bm  = elAttr' "object" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"object\" ... @
object ∷ forall t m a. DomBuilder t m ⇒ Object → m a → m a
object bm children = snd <$> object' bm children

-- | A short-hand notion for @ el\' \"object\" ... @
objectN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
objectN' = el' "object"

-- | A short-hand notion for @ el \"object\" ... @
objectN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
objectN children = snd <$> objectN' children

-- | A short-hand notion for @ elDynAttr\' \"object\" ... @
objectD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Object → m a → m (Element EventResult (DomBuilderSpace m) t, a)
objectD' bm  = elDynAttr' "object" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"object\" ... @
objectD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Object → m a → m a
objectD bm children = snd <$> objectD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Param-element
data Param = Param
  { _paramGlobals   ∷ Maybe A.Globals
  , _paramName      ∷ Maybe A.Name
  , _paramValueName ∷ Maybe A.ValueName
  , _paramCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Param where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _paramGlobals bm
    , A.attrMap <$> _paramName bm
    , A.attrMap <$> _paramValueName bm
    ] <> maybeToList (_paramCustom bm)

-- | A default value for Param.
defParam ∷ Param
defParam = Param Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Param where
  (<>) (Param a1 a2 a3 a4) (Param b1 b2 b3 b4)
    = Param (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Param where
  mempty = defParam
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Param where
   attrSetGlobals pp bm = bm { _paramGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Param
  where attrSetAccessKey pp g = g { _paramGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Param
  where attrSetAnmval pp g = g { _paramGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Param
  where attrSetContentEditable pp g = g  { _paramGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Param
  where attrSetContextMenu pp g     = g { _paramGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasClass Param
  where attrSetClassName pp g           = g { _paramGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Param
  where attrSetDnmval pp g           = g { _paramGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasDir Param
  where attrSetDir pp g             = g { _paramGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Param
  where attrSetDraggable pp g       = g { _paramGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Param
  where attrSetHidden pp g          = g { _paramGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasId Param
  where attrSetId pp g              = g { _paramGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasLang Param
  where attrSetLang pp g            = g { _paramGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasRole Param
  where attrSetRole pp g            = g { _paramGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Param
  where attrSetSlot pp g            = g { _paramGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Param
  where attrSetSpellCheck pp g      = g { _paramGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Param
  where attrSetStyle pp g           = g { _paramGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Param
  where attrSetTabIndex pp g        = g { _paramGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Param
  where attrSetTitle pp g           = g { _paramGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_paramGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Param
  where attrSetTranslate pp g       = g { _paramGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_paramGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Param where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_paramGlobals g)

-- | An instance.
instance A.AttrHasName Param where attrSetName pp g  = g { _paramName  = Just pp }

-- | An instance.
instance A.AttrHasValueName    Param where attrSetValueName pp g  = g { _paramValueName  = Just pp }

-- | An instance.
instance A.AttrHasCustom Param where attrSetCustom pp g       = g { _paramCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"param\" ... @
param' ∷ forall t m a. DomBuilder t m ⇒ Param → m a → m (Element EventResult (DomBuilderSpace m) t, a)
param' bm  = elAttr' "param" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"param\" ... @
param ∷ forall t m a. DomBuilder t m ⇒ Param → m a → m a
param bm children = snd <$> param' bm children

-- | A short-hand notion for @ el\' \"param\" ... @
paramN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
paramN' = el' "param"

-- | A short-hand notion for @ el \"param\" ... @
paramN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
paramN children = snd <$> paramN' children

-- | A short-hand notion for @ elDynAttr\' \"param\" ... @
paramD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Param → m a → m (Element EventResult (DomBuilderSpace m) t, a)
paramD' bm  = elDynAttr' "param" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"param\" ... @
paramD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Param → m a → m a
paramD bm children = snd <$> paramD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Picture-element has only the global attributes.
data Picture = Picture
  { _picturglobals   ∷ Maybe A.Globals
  , _picturcustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Picture where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _picturglobals bm
    ] <> maybeToList (_picturcustom bm)

-- | A default value for Picture.
defPicture ∷ Picture
defPicture = Picture Nothing Nothing

-- | An instance.
instance Semigroup Picture where
  (<>) (Picture a1 a2) (Picture b1 b2) = Picture (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Picture where
  mempty = defPicture
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Picture where
   attrSetGlobals pp bm = bm { _picturglobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Picture
  where attrSetAccessKey pp g = g { _picturglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Picture
  where attrSetAnmval pp g = g { _picturglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Picture
  where attrSetContentEditable pp g = g  { _picturglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Picture
  where attrSetContextMenu pp g     = g { _picturglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasClass Picture
  where attrSetClassName pp g           = g { _picturglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Picture
  where attrSetDnmval pp g           = g { _picturglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasDir Picture
  where attrSetDir pp g             = g { _picturglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Picture
  where attrSetDraggable pp g       = g { _picturglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasHidden Picture
  where attrSetHidden pp g          = g { _picturglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasId Picture
  where attrSetId pp g              = g { _picturglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasLang Picture
  where attrSetLang pp g            = g { _picturglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasRole Picture
  where attrSetRole pp g            = g { _picturglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasSlot Picture
  where attrSetSlot pp g            = g { _picturglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Picture
  where attrSetSpellCheck pp g      = g { _picturglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasStyle Picture
  where attrSetStyle pp g           = g { _picturglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Picture
  where attrSetTabIndex pp g        = g { _picturglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasTitle Picture
  where attrSetTitle pp g           = g { _picturglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_picturglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Picture
  where attrSetTranslate pp g       = g { _picturglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_picturglobals g))) }

-- | An instance.
instance A.AttrGetClassName Picture where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_picturglobals g)

-- | An instance.
instance A.AttrHasCustom Picture where attrSetCustom pp g       = g { _picturcustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"picture\" ... @
picture' ∷ forall t m a. DomBuilder t m ⇒ Picture → m a → m (Element EventResult (DomBuilderSpace m) t, a)
picture' bm  = elAttr' "picture" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"picture\" ... @
picture ∷ forall t m a. DomBuilder t m ⇒ Picture → m a → m a
picture bm children = snd <$> picture' bm children

-- | A short-hand notion for @ el\' \"picture\" ... @
picturn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
picturn' = el' "picture"

-- | A short-hand notion for @ el \"picture\" ... @
picturn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
picturn children = snd <$> picturn' children

-- | A short-hand notion for @ elDynAttr\' \"picture\" ... @
picturd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Picture → m a → m (Element EventResult (DomBuilderSpace m) t, a)
picturd' bm  = elDynAttr' "picture" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"picture\" ... @
picturd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Picture → m a → m a
picturd bm children = snd <$> picturd' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Source-element
data Source = Source
  { _sourcglobals   ∷ Maybe A.Globals
  , _sourcmedia     ∷ Maybe A.Media
  , _sourcsizes     ∷ Maybe A.Sizes
  , _sourcsrc       ∷ Maybe A.Src
  , _sourcsrcSet    ∷ Maybe A.SrcSet
  , _sourcmediaType ∷ Maybe A.MediaType
  , _sourccustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Source where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _sourcglobals bm
    , A.attrMap <$> _sourcmedia bm
    , A.attrMap <$> _sourcsizes bm
    , A.attrMap <$> _sourcsrc bm
    , A.attrMap <$> _sourcsrcSet bm
    , A.attrMap <$> _sourcmediaType bm
    ] <> maybeToList (_sourccustom bm)

-- | A default value for Source.
defSource ∷ Source
defSource = Source Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Source where
  (<>) (Source a1 a2 a3 a4 a5 a6 a7) (Source b1 b2 b3 b4 b5 b6 b7)
    = Source (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

-- | An instance.
instance Monoid Source where
  mempty = defSource
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Source where
   attrSetGlobals pp bm = bm { _sourcglobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Source
  where attrSetAccessKey pp g = g { _sourcglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Source
  where attrSetAnmval pp g = g { _sourcglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Source
  where attrSetContentEditable pp g = g  { _sourcglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Source
  where attrSetContextMenu pp g     = g { _sourcglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasClass Source
  where attrSetClassName pp g           = g { _sourcglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Source
  where attrSetDnmval pp g           = g { _sourcglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasDir Source
  where attrSetDir pp g             = g { _sourcglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Source
  where attrSetDraggable pp g       = g { _sourcglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasHidden Source
  where attrSetHidden pp g          = g { _sourcglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasId Source
  where attrSetId pp g              = g { _sourcglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasLang Source
  where attrSetLang pp g            = g { _sourcglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasRole Source
  where attrSetRole pp g            = g { _sourcglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasSlot Source
  where attrSetSlot pp g            = g { _sourcglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Source
  where attrSetSpellCheck pp g      = g { _sourcglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasStyle Source
  where attrSetStyle pp g           = g { _sourcglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Source
  where attrSetTabIndex pp g        = g { _sourcglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasTitle Source
  where attrSetTitle pp g           = g { _sourcglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_sourcglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Source
  where attrSetTranslate pp g       = g { _sourcglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_sourcglobals g))) }

-- | An instance.
instance A.AttrGetClassName Source where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_sourcglobals g)

-- | An instance.
instance A.AttrHasMedia Source where attrSetMedia pp g = g {_sourcmedia = Just pp }
-- | An instance.
instance A.AttrHasSizes Source where attrSetSizes pp g = g {_sourcsizes = Just pp }
-- | An instance.
instance A.AttrHasSrc Source where attrSetSrc pp g = g {_sourcsrc = Just pp }
-- | An instance.
instance A.AttrHasSrcSet Source where attrSetSrcSet pp g = g {_sourcsrcSet = Just pp }
-- | An instance.
instance A.AttrHasMediaType Source where attrSetMediaType pp g = g {_sourcmediaType = Just pp }

-- | An instance.
instance A.AttrHasCustom Source where attrSetCustom pp g       = g { _sourccustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"source\" ... @
source' ∷ forall t m a. DomBuilder t m ⇒ Source → m a → m (Element EventResult (DomBuilderSpace m) t, a)
source' bm  = elAttr' "source" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"source\" ... @
source ∷ forall t m a. DomBuilder t m ⇒ Source → m a → m a
source bm children = snd <$> source' bm children

-- | A short-hand notion for @ el\' \"source\" ... @
sourcn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
sourcn' = el' "source"

-- | A short-hand notion for @ el \"source\" ... @
sourcn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
sourcn children = snd <$> sourcn' children

-- | A short-hand notion for @ elDynAttr\' \"source\" ... @
sourcd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Source → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sourcd' bm  = elDynAttr' "source" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"source\" ... @
sourcd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Source → m a → m a
sourcd bm children = snd <$> sourcd' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Track-element
data Track = Track
  { _trackGlobals ∷ Maybe A.Globals
  , _trackDefault ∷ Maybe A.Default_
  , _trackKind    ∷ Maybe A.Kind
  , _trackLabel   ∷ Maybe A.Label
  , _trackSrc     ∷ Maybe A.Src
  , _trackSrcLang ∷ Maybe A.SrcLang
  , _trackCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Track where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _trackGlobals bm
    , A.attrMap <$> _trackDefault bm
    , A.attrMap <$> _trackKind bm
    , A.attrMap <$> _trackLabel bm
    , A.attrMap <$> _trackSrc bm
    , A.attrMap <$> _trackSrcLang bm
    ] <> maybeToList (_trackCustom bm)

-- | A default value for Track.
defTrack ∷ Track
defTrack = Track Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Track where
  (<>) (Track a1 a2 a3 a4 a5 a6 a7) (Track b1 b2 b3 b4 b5 b6 b7)
    = Track (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

-- | An instance.
instance Monoid Track where
  mempty = defTrack
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Track where
   attrSetGlobals pp bm = bm { _trackGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Track
  where attrSetAccessKey pp g = g { _trackGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Track
  where attrSetAnmval pp g = g { _trackGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Track
  where attrSetContentEditable pp g = g  { _trackGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Track
  where attrSetContextMenu pp g     = g { _trackGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasClass Track
  where attrSetClassName pp g           = g { _trackGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Track
  where attrSetDnmval pp g           = g { _trackGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasDir Track
  where attrSetDir pp g             = g { _trackGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Track
  where attrSetDraggable pp g       = g { _trackGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Track
  where attrSetHidden pp g          = g { _trackGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasId Track
  where attrSetId pp g              = g { _trackGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasLang Track
  where attrSetLang pp g            = g { _trackGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasRole Track
  where attrSetRole pp g            = g { _trackGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Track
  where attrSetSlot pp g            = g { _trackGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Track
  where attrSetSpellCheck pp g      = g { _trackGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Track
  where attrSetStyle pp g           = g { _trackGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Track
  where attrSetTabIndex pp g        = g { _trackGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Track
  where attrSetTitle pp g           = g { _trackGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_trackGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Track
  where attrSetTranslate pp g       = g { _trackGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_trackGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Track where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_trackGlobals g)

-- | An instance.
instance A.AttrHasDefault Track where attrSetDefault pp g = g {_trackDefault = Just pp }
-- | An instance.
instance A.AttrHasKind Track where attrSetKind pp g = g {_trackKind = Just pp }
-- | An instance.
instance A.AttrHasLabel Track where attrSetLabel pp g = g {_trackLabel = Just pp }
-- | An instance.
instance A.AttrHasSrc Track where attrSetSrc pp g = g {_trackSrc = Just pp }
-- | An instance.
instance A.AttrHasSrcLang Track where attrSetSrcLang pp g = g {_trackSrcLang = Just pp }

-- | An instance.
instance A.AttrHasCustom Track where attrSetCustom pp g       = g { _trackCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"track\" ... @
track' ∷ forall t m a. DomBuilder t m ⇒ Track → m a → m (Element EventResult (DomBuilderSpace m) t, a)
track' bm  = elAttr' "track" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"track\" ... @
track ∷ forall t m a. DomBuilder t m ⇒ Track → m a → m a
track bm children = snd <$> track' bm children

-- | A short-hand notion for @ el\' \"track\" ... @
trackN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
trackN' = el' "track"

-- | A short-hand notion for @ el \"track\" ... @
trackN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
trackN children = snd <$> trackN' children

-- | A short-hand notion for @ elDynAttr\' \"track\" ... @
trackD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Track → m a → m (Element EventResult (DomBuilderSpace m) t, a)
trackD' bm  = elDynAttr' "track" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"track\" ... @
trackD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Track → m a → m a
trackD bm children = snd <$> trackD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Video-element is interactive  if the constrols attribute is present.
-- globals; src; crossorigin; poster; preload; autoplay; loop; muted; controls; width; height
--
--
data Video = Video
  { _videoGlobals     ∷ Maybe A.Globals
  , _videoAutoplay    ∷ Maybe A.Autoplay
  , _videoControls    ∷ Maybe A.Controls
  , _videoCrossOrigin ∷ Maybe A.CrossOrigin
  , _videoHeight      ∷ Maybe A.Height
  , _videoLoop        ∷ Maybe A.Loop
  , _videoMuted       ∷ Maybe A.Muted
  , _videoPoster      ∷ Maybe A.Poster
  , _videoPreload     ∷ Maybe A.Preload
  , _videoSrc         ∷ Maybe A.Src
  , _videoWidth       ∷ Maybe A.Width
  , _videoCustom      ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Video where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _videoGlobals bm
    , A.attrMap <$> _videoAutoplay bm
    , A.attrMap <$> _videoControls bm
    , A.attrMap <$> _videoCrossOrigin bm
    , A.attrMap <$> _videoHeight bm
    , A.attrMap <$> _videoLoop bm
    , A.attrMap <$> _videoMuted bm
    , A.attrMap <$> _videoPoster bm
    , A.attrMap <$> _videoPreload bm
    , A.attrMap <$> _videoSrc bm
    , A.attrMap <$> _videoWidth bm
    ] <> maybeToList (_videoCustom bm)

-- | A default value for Video.
defVideo ∷ Video
defVideo = Video Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Video where
  (<>) (Video a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (Video b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = Video (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

-- | An instance.
instance Monoid Video where
  mempty = defVideo
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Video where
   attrSetGlobals pp bm = bm { _videoGlobals = Just pp }


-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Video
  where attrSetAccessKey pp g = g { _videoGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Video
  where attrSetAnmval pp g = g { _videoGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Video
  where attrSetContentEditable pp g = g  { _videoGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Video
  where attrSetContextMenu pp g     = g { _videoGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasClass Video
  where attrSetClassName pp g           = g { _videoGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Video
  where attrSetDnmval pp g           = g { _videoGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasDir Video
  where attrSetDir pp g             = g { _videoGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Video
  where attrSetDraggable pp g       = g { _videoGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Video
  where attrSetHidden pp g          = g { _videoGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasId Video
  where attrSetId pp g              = g { _videoGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasLang Video
  where attrSetLang pp g            = g { _videoGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasRole Video
  where attrSetRole pp g            = g { _videoGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Video
  where attrSetSlot pp g            = g { _videoGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Video
  where attrSetSpellCheck pp g      = g { _videoGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Video
  where attrSetStyle pp g           = g { _videoGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Video
  where attrSetTabIndex pp g        = g { _videoGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Video
  where attrSetTitle pp g           = g { _videoGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_videoGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Video
  where attrSetTranslate pp g       = g { _videoGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_videoGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Video where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_videoGlobals g)

-- | An instance.
instance A.AttrHasAutoplay    Video where attrSetAutoplay pp g    = g { _videoAutoplay    = Just pp }
-- | An instance.
instance A.AttrHasControls    Video where attrSetControls pp g    = g { _videoControls    = Just pp }
-- | An instance.
instance A.AttrHasCrossOrigin Video where attrSetCrossOrigin pp g = g { _videoCrossOrigin = Just pp }
-- | An instance.
instance A.AttrHasHeight      Video where attrSetHeight pp g      = g { _videoHeight      = Just pp }
-- | An instance.
instance A.AttrHasLoop        Video where attrSetLoop pp g        = g { _videoLoop        = Just pp }
-- | An instance.
instance A.AttrHasMuted       Video where attrSetMuted pp g       = g { _videoMuted       = Just pp }
-- | An instance.
instance A.AttrHasPoster      Video where attrSetPoster pp g      = g { _videoPoster      = Just pp }
-- | An instance.
instance A.AttrHasPreload     Video where attrSetPreload pp g     = g { _videoPreload     = Just pp }
-- | An instance.
instance A.AttrHasSrc         Video where attrSetSrc pp g         = g { _videoSrc         = Just pp }
-- | An instance.
instance A.AttrHasWidth       Video where attrSetWidth pp g       = g { _videoWidth       = Just pp }

-- | An instance.
instance A.AttrHasCustom Video where attrSetCustom pp g       = g { _videoCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"video\" ... @
video' ∷ forall t m a. DomBuilder t m ⇒ Video → m a → m (Element EventResult (DomBuilderSpace m) t, a)
video' bm  = elAttr' "video" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"video\" ... @
video ∷ forall t m a. DomBuilder t m ⇒ Video → m a → m a
video bm children = snd <$> video' bm children

-- | A short-hand notion for @ el\' \"video\" ... @
videoN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
videoN' = el' "video"

-- | A short-hand notion for @ el \"video\" ... @
videoN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
videoN children = snd <$> videoN' children

-- | A short-hand notion for @ elDynAttr\' \"video\" ... @
videoD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Video → m a → m (Element EventResult (DomBuilderSpace m) t, a)
videoD' bm  = elDynAttr' "video" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"video\" ... @
videoD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Video → m a → m a
videoD bm children = snd <$> videoD' bm children


videoC ∷ (DomBuilder t m, PostBuild t m) ⇒ Video → m () → m (Event t ())
videoC bm children = do
  (e,_) ← elAttr' "video" (A.attrMap bm) children
  return $ domEvent Click e

videoCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Video → m () → m (Event t ())
videoCD bDyn children = do
  (e,_) ← elDynAttr' "video" (A.attrMap <$> bDyn) children
  return $ domEvent Click e



