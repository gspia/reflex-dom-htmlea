{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Elements.Metadata
Description : HTML5 metadata elements
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Metadata

This module has elements of Metadata content catogory and
some elements associated to metadata. At the moment it seems
that some of the elements given here are better to be
written directly into the index.html. Like some script-elements
referring to certain js-libs.

Some of elements given here have no practical purpose (the
index.html is either given by the user or written by other parts
of the reflex-dom libs).

This module has the following elements:
base; link; meta; noscript; script; style; template; title
and
body, head, html, main

The naming convention follows that of Elements.

-}

module Reflex.Dom.HTML5.Elements.Metadata where

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
  attrSetHref, attrSetNonce, attrSetSrc, attrSetDefer,
  attrSetMediaType, attrSetMedia, attrSetScriptType,
  attrSetCharSet, attrSetAsync, attrSetCrossOrigin, attrSetName,
  attrSetHttpEquiv, attrSetContent, attrSetSizes,
  attrSetReferrerPolicy, attrSetHrefLang, attrSetTarget,
  attrSetRel, attrSetManifest, attrSetIntegrity,
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
  AttrHasHref, AttrHasNonce, AttrHasSrc, AttrHasDefer,
  AttrHasMediaType, AttrHasMedia, AttrHasScriptType,
  AttrHasCharSet, AttrHasAsync, AttrHasCrossOrigin, AttrHasName,
  AttrHasHttpEquiv, AttrHasContent, AttrHasSizes,
  AttrHasReferrerPolicy, AttrHasHrefLang, AttrHasTarget,
  AttrHasRel, AttrHasManifest, AttrHasIntegrity,
  AttrHasAnmval, AttrHasDnmval, AttrHasRole, AttrHasSlot,
  MediaType, Nonce, Media, ScriptType, Src, Defer, CrossOrigin,
  CharSet, Async, Name, HttpEquiv, Content, Sizes, Manifest,
  Rel, ReferrerPolicy, HrefLang, Href, Target, Integrity,
  Globals, AttrGetClassName, attrGetClassName, gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Base-element
data EBase = EBase
  { _eBaseGlobals :: Maybe Globals
  , _eBaseHref    :: Maybe Href
  , _eBaseTarget  :: Maybe Target
  , _eBaseCustom  :: Maybe Attr
  }


instance AttrMap EBase where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBaseGlobals b
    , attrMap <$> _eBaseHref b
    , attrMap <$> _eBaseTarget b
    ] <> maybe [] (\a -> [a]) (_eBaseCustom b)

instance Default EBase where
  def = EBase def def def def

instance AttrHasGlobals EBase where
   attrSetGlobals p b = b { _eBaseGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBase
  where attrSetAccessKey p g = g { _eBaseGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasAnmval EBase
  where attrSetAnmval p g = g { _eBaseGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasContentEditable EBase
  where attrSetContentEditable p g = g  { _eBaseGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasContextMenu EBase
  where attrSetContextMenu p g     = g { _eBaseGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasClass EBase
  where attrSetClassName p g           = g { _eBaseGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasDnmval EBase
  where attrSetDnmval p g           = g { _eBaseGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasDir EBase
  where attrSetDir p g             = g { _eBaseGlobals = Just (attrSetDir p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasDraggable EBase
  where attrSetDraggable p g       = g { _eBaseGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasHidden EBase
  where attrSetHidden p g          = g { _eBaseGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasId EBase
  where attrSetId p g              = g { _eBaseGlobals = Just (attrSetId p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasLang EBase
  where attrSetLang p g            = g { _eBaseGlobals = Just (attrSetLang p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasRole EBase
  where attrSetRole p g            = g { _eBaseGlobals = Just (attrSetRole p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasSlot EBase
  where attrSetSlot p g            = g { _eBaseGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasSpellCheck EBase
  where attrSetSpellCheck p g      = g { _eBaseGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasStyle EBase
  where attrSetStyle p g           = g { _eBaseGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasTabIndex EBase
  where attrSetTabIndex p g        = g { _eBaseGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasTitle EBase
  where attrSetTitle p g           = g { _eBaseGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBaseGlobals g))) }
instance AttrHasTranslate EBase
  where attrSetTranslate p g       = g { _eBaseGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBaseGlobals g))) }

instance AttrGetClassName EBase where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBaseGlobals g)

instance AttrHasHref EBase where attrSetHref p g = g {_eBaseHref = Just p }
instance AttrHasTarget EBase where attrSetTarget p g = g {_eBaseTarget = Just p }

instance AttrHasCustom EBase where attrSetCustom p g       = g { _eBaseCustom = Just p }

eBase' :: forall t m a. DomBuilder t m => EBase -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBase' b = elAttr' "base" (attrMap b)

eBase :: forall t m a. DomBuilder t m => EBase -> m a -> m a
eBase b children = snd <$> eBase' b children

eBaseN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBaseN' = el' "base"

eBaseN :: forall t m a. DomBuilder t m => m a -> m a
eBaseN children = snd <$> eBaseN' children

eBaseD' :: forall t m a. MonadWidget t m => Dynamic t EBase -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBaseD' b = elDynAttr' "base" (attrMap <$> b)

eBaseD :: forall t m a. MonadWidget t m => Dynamic t EBase -> m a -> m a
eBaseD b children = snd <$> eBaseD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Body-element has only the global attributes.
data EBody = EBody
  { _eBodyGlobals :: Maybe Globals
  , _eBodyCustom  :: Maybe Attr
  }


instance AttrMap EBody where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eBodyGlobals b
    ] <> maybe [] (\a -> [a]) (_eBodyCustom b)

instance Default EBody where
  def = EBody def def

instance AttrHasGlobals EBody where
   attrSetGlobals p b = b { _eBodyGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EBody
  where attrSetAccessKey p g = g { _eBodyGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasAnmval EBody
  where attrSetAnmval p g = g { _eBodyGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasContentEditable EBody
  where attrSetContentEditable p g = g  { _eBodyGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasContextMenu EBody
  where attrSetContextMenu p g     = g { _eBodyGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasClass EBody
  where attrSetClassName p g           = g { _eBodyGlobals = Just (attrSetClassName p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasDnmval EBody
  where attrSetDnmval p g           = g { _eBodyGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasDir EBody
  where attrSetDir p g             = g { _eBodyGlobals = Just (attrSetDir p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasDraggable EBody
  where attrSetDraggable p g       = g { _eBodyGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasHidden EBody
  where attrSetHidden p g          = g { _eBodyGlobals = Just (attrSetHidden p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasId EBody
  where attrSetId p g              = g { _eBodyGlobals = Just (attrSetId p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasLang EBody
  where attrSetLang p g            = g { _eBodyGlobals = Just (attrSetLang p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasRole EBody
  where attrSetRole p g            = g { _eBodyGlobals = Just (attrSetRole p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasSlot EBody
  where attrSetSlot p g            = g { _eBodyGlobals = Just (attrSetSlot p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasSpellCheck EBody
  where attrSetSpellCheck p g      = g { _eBodyGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasStyle EBody
  where attrSetStyle p g           = g { _eBodyGlobals = Just (attrSetStyle p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasTabIndex EBody
  where attrSetTabIndex p g        = g { _eBodyGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasTitle EBody
  where attrSetTitle p g           = g { _eBodyGlobals = Just (attrSetTitle p (fromMaybe gDef (_eBodyGlobals g))) }
instance AttrHasTranslate EBody
  where attrSetTranslate p g       = g { _eBodyGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eBodyGlobals g))) }

instance AttrGetClassName EBody where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eBodyGlobals g)

instance AttrHasCustom EBody where attrSetCustom p g       = g { _eBodyCustom = Just p }

eBody' :: forall t m a. DomBuilder t m => EBody -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBody' b = elAttr' "body" (attrMap b)

eBody :: forall t m a. DomBuilder t m => EBody -> m a -> m a
eBody b children = snd <$> eBody' b children

eBodyN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBodyN' = el' "body"

eBodyN :: forall t m a. DomBuilder t m => m a -> m a
eBodyN children = snd <$> eBodyN' children

eBodyD' :: forall t m a. MonadWidget t m => Dynamic t EBody -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eBodyD' b = elDynAttr' "body" (attrMap <$> b)

eBodyD :: forall t m a. MonadWidget t m => Dynamic t EBody -> m a -> m a
eBodyD b children = snd <$> eBodyD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Head-element has only the global attributes.
data EHead = EHead
  { _eHeadGlobals :: Maybe Globals
  , _eHeadCustom  :: Maybe Attr
  }


instance AttrMap EHead where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eHeadGlobals b
    ] <> maybe [] (\a -> [a]) (_eHeadCustom b)

instance Default EHead where
  def = EHead def def

instance AttrHasGlobals EHead where
   attrSetGlobals p b = b { _eHeadGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EHead
  where attrSetAccessKey p g = g { _eHeadGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasAnmval EHead
  where attrSetAnmval p g = g { _eHeadGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasContentEditable EHead
  where attrSetContentEditable p g = g  { _eHeadGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasContextMenu EHead
  where attrSetContextMenu p g     = g { _eHeadGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasClass EHead
  where attrSetClassName p g           = g { _eHeadGlobals = Just (attrSetClassName p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasDnmval EHead
  where attrSetDnmval p g           = g { _eHeadGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasDir EHead
  where attrSetDir p g             = g { _eHeadGlobals = Just (attrSetDir p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasDraggable EHead
  where attrSetDraggable p g       = g { _eHeadGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasHidden EHead
  where attrSetHidden p g          = g { _eHeadGlobals = Just (attrSetHidden p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasId EHead
  where attrSetId p g              = g { _eHeadGlobals = Just (attrSetId p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasLang EHead
  where attrSetLang p g            = g { _eHeadGlobals = Just (attrSetLang p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasRole EHead
  where attrSetRole p g            = g { _eHeadGlobals = Just (attrSetRole p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasSlot EHead
  where attrSetSlot p g            = g { _eHeadGlobals = Just (attrSetSlot p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasSpellCheck EHead
  where attrSetSpellCheck p g      = g { _eHeadGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasStyle EHead
  where attrSetStyle p g           = g { _eHeadGlobals = Just (attrSetStyle p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasTabIndex EHead
  where attrSetTabIndex p g        = g { _eHeadGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasTitle EHead
  where attrSetTitle p g           = g { _eHeadGlobals = Just (attrSetTitle p (fromMaybe gDef (_eHeadGlobals g))) }
instance AttrHasTranslate EHead
  where attrSetTranslate p g       = g { _eHeadGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eHeadGlobals g))) }

instance AttrGetClassName EHead where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eHeadGlobals g)

instance AttrHasCustom EHead where attrSetCustom p g       = g { _eHeadCustom = Just p }

eHead' :: forall t m a. DomBuilder t m => EHead -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHead' b = elAttr' "head" (attrMap b)

eHead :: forall t m a. DomBuilder t m => EHead -> m a -> m a
eHead b children = snd <$> eHead' b children

eHeadN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHeadN' = el' "head"

eHeadN :: forall t m a. DomBuilder t m => m a -> m a
eHeadN children = snd <$> eHeadN' children

eHeadD' :: forall t m a. MonadWidget t m => Dynamic t EHead -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHeadD' b = elDynAttr' "head" (attrMap <$> b)

eHeadD :: forall t m a. MonadWidget t m => Dynamic t EHead -> m a -> m a
eHeadD b children = snd <$> eHeadD' b children

eHead_ :: forall t m. DomBuilder t m => m ()
eHead_ = eHead def blank


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Html-element
data EHtml = EHtml
  { _eHtmlGlobals  :: Maybe Globals
  , _eHtmlManifest :: Maybe Manifest
  , _eHtmlCustom   :: Maybe Attr
  }


instance AttrMap EHtml where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eHtmlGlobals b
    , attrMap <$> _eHtmlManifest b
    ] <> maybe [] (\a -> [a]) (_eHtmlCustom b)

instance Default EHtml where
  def = EHtml def def def

instance AttrHasGlobals EHtml where
   attrSetGlobals p b = b { _eHtmlGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EHtml
  where attrSetAccessKey p g = g { _eHtmlGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasAnmval EHtml
  where attrSetAnmval p g = g { _eHtmlGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasContentEditable EHtml
  where attrSetContentEditable p g = g  { _eHtmlGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasContextMenu EHtml
  where attrSetContextMenu p g     = g { _eHtmlGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasClass EHtml
  where attrSetClassName p g           = g { _eHtmlGlobals = Just (attrSetClassName p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasDnmval EHtml
  where attrSetDnmval p g           = g { _eHtmlGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasDir EHtml
  where attrSetDir p g             = g { _eHtmlGlobals = Just (attrSetDir p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasDraggable EHtml
  where attrSetDraggable p g       = g { _eHtmlGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasHidden EHtml
  where attrSetHidden p g          = g { _eHtmlGlobals = Just (attrSetHidden p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasId EHtml
  where attrSetId p g              = g { _eHtmlGlobals = Just (attrSetId p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasLang EHtml
  where attrSetLang p g            = g { _eHtmlGlobals = Just (attrSetLang p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasRole EHtml
  where attrSetRole p g            = g { _eHtmlGlobals = Just (attrSetRole p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasSlot EHtml
  where attrSetSlot p g            = g { _eHtmlGlobals = Just (attrSetSlot p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasSpellCheck EHtml
  where attrSetSpellCheck p g      = g { _eHtmlGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasStyle EHtml
  where attrSetStyle p g           = g { _eHtmlGlobals = Just (attrSetStyle p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasTabIndex EHtml
  where attrSetTabIndex p g        = g { _eHtmlGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasTitle EHtml
  where attrSetTitle p g           = g { _eHtmlGlobals = Just (attrSetTitle p (fromMaybe gDef (_eHtmlGlobals g))) }
instance AttrHasTranslate EHtml
  where attrSetTranslate p g       = g { _eHtmlGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eHtmlGlobals g))) }

instance AttrGetClassName EHtml where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eHtmlGlobals g)

instance AttrHasManifest EHtml where attrSetManifest p g = g {_eHtmlManifest = Just p }

instance AttrHasCustom EHtml where attrSetCustom p g       = g { _eHtmlCustom = Just p }

eHtml' :: forall t m a. DomBuilder t m => EHtml -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHtml' b = elAttr' "html" (attrMap b)

eHtml :: forall t m a. DomBuilder t m => EHtml -> m a -> m a
eHtml b children = snd <$> eHtml' b children

eHtmlN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHtmlN' = el' "html"

eHtmlN :: forall t m a. DomBuilder t m => m a -> m a
eHtmlN children = snd <$> eHtmlN' children

eHtmlD' :: forall t m a. MonadWidget t m => Dynamic t EHtml -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eHtmlD' b = elDynAttr' "html" (attrMap <$> b)

eHtmlD :: forall t m a. MonadWidget t m => Dynamic t EHtml -> m a -> m a
eHtmlD b children = snd <$> eHtmlD' b children

eHtml_ :: forall t m. DomBuilder t m => m ()
eHtml_ = eHtml def blank




------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Link-element
-- Note that integrity is not in HTML 5.3
data ELink = ELink
  { _eLinkGlobals        :: Maybe Globals
  , _eLinkCrossOrigin    :: Maybe CrossOrigin
  , _eLinkHref           :: Maybe Href
  , _eLinkHrefLang       :: Maybe HrefLang
  , _eLinkIntegrity      :: Maybe Integrity
  , _eLinkMedia          :: Maybe Media
  , _eLinkNonce          :: Maybe Nonce
  , _eLinkReferrerPolicy :: Maybe ReferrerPolicy
  , _eLinkRel            :: Maybe Rel
  , _eLinkMediaType      :: Maybe MediaType
  , _eLinkSizes          :: Maybe Sizes
  , _eLinkCustom         :: Maybe Attr
  }


instance AttrMap ELink where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eLinkGlobals b
    , attrMap <$> _eLinkCrossOrigin b
    , attrMap <$> _eLinkHref b
    , attrMap <$> _eLinkHrefLang b
    , attrMap <$> _eLinkIntegrity b
    , attrMap <$> _eLinkMedia b
    , attrMap <$> _eLinkNonce b
    , attrMap <$> _eLinkReferrerPolicy b
    , attrMap <$> _eLinkRel b
    , attrMap <$> _eLinkMediaType b
    , attrMap <$> _eLinkSizes b
    ] <> maybe [] (\a -> [a]) (_eLinkCustom b)

instance Default ELink where
  def = ELink def def def def def def def def def def def def

instance AttrHasGlobals ELink where
   attrSetGlobals p b = b { _eLinkGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ELink
  where attrSetAccessKey p g = g { _eLinkGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasAnmval ELink
  where attrSetAnmval p g = g { _eLinkGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasContentEditable ELink
  where attrSetContentEditable p g = g  { _eLinkGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasContextMenu ELink
  where attrSetContextMenu p g     = g { _eLinkGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasClass ELink
  where attrSetClassName p g           = g { _eLinkGlobals = Just (attrSetClassName p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasDnmval ELink
  where attrSetDnmval p g           = g { _eLinkGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasDir ELink
  where attrSetDir p g             = g { _eLinkGlobals = Just (attrSetDir p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasDraggable ELink
  where attrSetDraggable p g       = g { _eLinkGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasHidden ELink
  where attrSetHidden p g          = g { _eLinkGlobals = Just (attrSetHidden p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasId ELink
  where attrSetId p g              = g { _eLinkGlobals = Just (attrSetId p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasLang ELink
  where attrSetLang p g            = g { _eLinkGlobals = Just (attrSetLang p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasRole ELink
  where attrSetRole p g            = g { _eLinkGlobals = Just (attrSetRole p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasSlot ELink
  where attrSetSlot p g            = g { _eLinkGlobals = Just (attrSetSlot p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasSpellCheck ELink
  where attrSetSpellCheck p g      = g { _eLinkGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasStyle ELink
  where attrSetStyle p g           = g { _eLinkGlobals = Just (attrSetStyle p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasTabIndex ELink
  where attrSetTabIndex p g        = g { _eLinkGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasTitle ELink
  where attrSetTitle p g           = g { _eLinkGlobals = Just (attrSetTitle p (fromMaybe gDef (_eLinkGlobals g))) }
instance AttrHasTranslate ELink
  where attrSetTranslate p g       = g { _eLinkGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eLinkGlobals g))) }

instance AttrGetClassName ELink where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eLinkGlobals g)

instance AttrHasCrossOrigin ELink where attrSetCrossOrigin p g = g {_eLinkCrossOrigin = Just p }
instance AttrHasHref ELink where attrSetHref p g = g {_eLinkHref = Just p }
instance AttrHasHrefLang ELink where attrSetHrefLang p g = g {_eLinkHrefLang = Just p }
instance AttrHasIntegrity ELink where attrSetIntegrity p g = g { _eLinkIntegrity = Just p}
instance AttrHasMedia ELink where attrSetMedia p g = g {_eLinkMedia = Just p }
instance AttrHasNonce ELink where attrSetNonce p g = g {_eLinkNonce = Just p }
instance AttrHasReferrerPolicy ELink where attrSetReferrerPolicy p g = g {_eLinkReferrerPolicy = Just p }
instance AttrHasRel ELink where attrSetRel p g = g {_eLinkRel = Just p }
instance AttrHasMediaType ELink where attrSetMediaType p g = g {_eLinkMediaType = Just p }
instance AttrHasSizes ELink where attrSetSizes p g = g {_eLinkSizes = Just p }

instance AttrHasCustom ELink where attrSetCustom p g       = g { _eLinkCustom = Just p }

eLink' :: forall t m a. DomBuilder t m => ELink -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLink' b  = elAttr' "link" (attrMap b)

eLink :: forall t m a. DomBuilder t m => ELink -> m a -> m a
eLink b children = snd <$> eLink' b children

eLinkN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLinkN' = el' "link"

eLinkN :: forall t m a. DomBuilder t m => m a -> m a
eLinkN children = snd <$> eLinkN' children

eLinkD' :: forall t m a. MonadWidget t m => Dynamic t ELink -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eLinkD' b  = elDynAttr' "link" (attrMap <$> b)

eLinkD :: forall t m a. MonadWidget t m => Dynamic t ELink -> m a -> m a
eLinkD b children = snd <$> eLinkD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Meta-element
data EMeta = EMeta
  { _eMetaGlobals   :: Maybe Globals
  , _eMetaCharSet   :: Maybe CharSet
  , _eMetaContent   :: Maybe Content
  , _eMetaHttpEquiv :: Maybe HttpEquiv
  , _eMetaName      :: Maybe Name
  , _eMetaCustom    :: Maybe Attr
  }


instance AttrMap EMeta where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMetaGlobals b
    , attrMap <$> _eMetaCharSet b
    , attrMap <$> _eMetaContent b
    , attrMap <$> _eMetaHttpEquiv b
    , attrMap <$> _eMetaName b
    ] <> maybe [] (\a -> [a]) (_eMetaCustom b)

instance Default EMeta where
  def = EMeta def def def def def def

instance AttrHasGlobals EMeta where
   attrSetGlobals p b = b { _eMetaGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMeta
  where attrSetAccessKey p g = g { _eMetaGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasAnmval EMeta
  where attrSetAnmval p g = g { _eMetaGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasContentEditable EMeta
  where attrSetContentEditable p g = g  { _eMetaGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasContextMenu EMeta
  where attrSetContextMenu p g     = g { _eMetaGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasClass EMeta
  where attrSetClassName p g           = g { _eMetaGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasDnmval EMeta
  where attrSetDnmval p g           = g { _eMetaGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasDir EMeta
  where attrSetDir p g             = g { _eMetaGlobals = Just (attrSetDir p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasDraggable EMeta
  where attrSetDraggable p g       = g { _eMetaGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasHidden EMeta
  where attrSetHidden p g          = g { _eMetaGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasId EMeta
  where attrSetId p g              = g { _eMetaGlobals = Just (attrSetId p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasLang EMeta
  where attrSetLang p g            = g { _eMetaGlobals = Just (attrSetLang p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasRole EMeta
  where attrSetRole p g            = g { _eMetaGlobals = Just (attrSetRole p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasSlot EMeta
  where attrSetSlot p g            = g { _eMetaGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasSpellCheck EMeta
  where attrSetSpellCheck p g      = g { _eMetaGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasStyle EMeta
  where attrSetStyle p g           = g { _eMetaGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasTabIndex EMeta
  where attrSetTabIndex p g        = g { _eMetaGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasTitle EMeta
  where attrSetTitle p g           = g { _eMetaGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMetaGlobals g))) }
instance AttrHasTranslate EMeta
  where attrSetTranslate p g       = g { _eMetaGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMetaGlobals g))) }

instance AttrGetClassName EMeta where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMetaGlobals g)

instance AttrHasCharSet EMeta where attrSetCharSet p g = g {_eMetaCharSet = Just p }
instance AttrHasContent EMeta where attrSetContent p g = g {_eMetaContent = Just p }
instance AttrHasHttpEquiv EMeta where attrSetHttpEquiv p g = g {_eMetaHttpEquiv = Just p }
instance AttrHasName EMeta where attrSetName p g = g {_eMetaName = Just p }

instance AttrHasCustom EMeta where attrSetCustom p g       = g { _eMetaCustom = Just p }

eMeta' :: forall t m a. DomBuilder t m => EMeta -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMeta' b  = elAttr' "meta" (attrMap b)

eMeta :: forall t m a. DomBuilder t m => EMeta -> m a -> m a
eMeta b children = snd <$> eMeta' b children

eMetaN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMetaN' = el' "meta"

eMetaN :: forall t m a. DomBuilder t m => m a -> m a
eMetaN children = snd <$> eMetaN' children

eMetaD' :: forall t m a. MonadWidget t m => Dynamic t EMeta -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMetaD' b  = elDynAttr' "meta" (attrMap <$> b)

eMetaD :: forall t m a. MonadWidget t m => Dynamic t EMeta -> m a -> m a
eMetaD b children = snd <$> eMetaD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | NoScript-element has only the global attributes.
data ENoScript = ENoScript
  { _eNoScriptGlobals :: Maybe Globals
  , _eNoScriptCustom  :: Maybe Attr
  }


instance AttrMap ENoScript where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eNoScriptGlobals b
    ] <> maybe [] (\a -> [a]) (_eNoScriptCustom b)

instance Default ENoScript where
  def = ENoScript def def

instance AttrHasGlobals ENoScript where
   attrSetGlobals p b = b { _eNoScriptGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ENoScript
  where attrSetAccessKey p g = g { _eNoScriptGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasAnmval ENoScript
  where attrSetAnmval p g = g { _eNoScriptGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasContentEditable ENoScript
  where attrSetContentEditable p g = g  { _eNoScriptGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasContextMenu ENoScript
  where attrSetContextMenu p g     = g { _eNoScriptGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasClass ENoScript
  where attrSetClassName p g           = g { _eNoScriptGlobals = Just (attrSetClassName p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasDnmval ENoScript
  where attrSetDnmval p g           = g { _eNoScriptGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasDir ENoScript
  where attrSetDir p g             = g { _eNoScriptGlobals = Just (attrSetDir p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasDraggable ENoScript
  where attrSetDraggable p g       = g { _eNoScriptGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasHidden ENoScript
  where attrSetHidden p g          = g { _eNoScriptGlobals = Just (attrSetHidden p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasId ENoScript
  where attrSetId p g              = g { _eNoScriptGlobals = Just (attrSetId p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasLang ENoScript
  where attrSetLang p g            = g { _eNoScriptGlobals = Just (attrSetLang p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasRole ENoScript
  where attrSetRole p g            = g { _eNoScriptGlobals = Just (attrSetRole p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasSlot ENoScript
  where attrSetSlot p g            = g { _eNoScriptGlobals = Just (attrSetSlot p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasSpellCheck ENoScript
  where attrSetSpellCheck p g      = g { _eNoScriptGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasStyle ENoScript
  where attrSetStyle p g           = g { _eNoScriptGlobals = Just (attrSetStyle p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasTabIndex ENoScript
  where attrSetTabIndex p g        = g { _eNoScriptGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasTitle ENoScript
  where attrSetTitle p g           = g { _eNoScriptGlobals = Just (attrSetTitle p (fromMaybe gDef (_eNoScriptGlobals g))) }
instance AttrHasTranslate ENoScript
  where attrSetTranslate p g       = g { _eNoScriptGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eNoScriptGlobals g))) }

instance AttrGetClassName ENoScript where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eNoScriptGlobals g)

instance AttrHasCustom ENoScript where attrSetCustom p g       = g { _eNoScriptCustom = Just p }

eNoScript' :: forall t m a. DomBuilder t m => ENoScript -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNoScript' b  = elAttr' "noscript" (attrMap b)

eNoScript :: forall t m a. DomBuilder t m => ENoScript -> m a -> m a
eNoScript b children = snd <$> eNoScript' b children

eNoScriptN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNoScriptN' = el' "noscript"

eNoScriptN :: forall t m a. DomBuilder t m => m a -> m a
eNoScriptN children = snd <$> eNoScriptN' children

eNoScriptD' :: forall t m a. MonadWidget t m => Dynamic t ENoScript -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNoScriptD' b  = elDynAttr' "noscript" (attrMap <$> b)

eNoScriptD :: forall t m a. MonadWidget t m => Dynamic t ENoScript -> m a -> m a
eNoScriptD b children = snd <$> eNoScriptD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Script-element
data EScript = EScript
  { _eScriptGlobals     :: Maybe Globals
  , _eScriptAsync       :: Maybe Async
  , _eScriptCharSet     :: Maybe CharSet
  , _eScriptCrossOrigin :: Maybe CrossOrigin
  , _eScriptDefer       :: Maybe Defer
  , _eScriptNonce       :: Maybe Nonce
  , _eScriptSrc         :: Maybe Src
  , _eScriptType        :: Maybe ScriptType
  , _eScriptCustom      :: Maybe Attr
  }


instance AttrMap EScript where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eScriptGlobals b
    , attrMap <$> _eScriptAsync b
    , attrMap <$> _eScriptCharSet b
    , attrMap <$> _eScriptCrossOrigin b
    , attrMap <$> _eScriptDefer b
    , attrMap <$> _eScriptNonce b
    , attrMap <$> _eScriptSrc b
    , attrMap <$> _eScriptType b
    ] <> maybe [] (\a -> [a]) (_eScriptCustom b)

instance Default EScript where
  def = EScript def def def def def def def def def

instance AttrHasGlobals EScript where
   attrSetGlobals p b = b { _eScriptGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EScript
  where attrSetAccessKey p g = g { _eScriptGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasAnmval EScript
  where attrSetAnmval p g = g { _eScriptGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasContentEditable EScript
  where attrSetContentEditable p g = g  { _eScriptGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasContextMenu EScript
  where attrSetContextMenu p g     = g { _eScriptGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasClass EScript
  where attrSetClassName p g           = g { _eScriptGlobals = Just (attrSetClassName p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasDnmval EScript
  where attrSetDnmval p g           = g { _eScriptGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasDir EScript
  where attrSetDir p g             = g { _eScriptGlobals = Just (attrSetDir p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasDraggable EScript
  where attrSetDraggable p g       = g { _eScriptGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasHidden EScript
  where attrSetHidden p g          = g { _eScriptGlobals = Just (attrSetHidden p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasId EScript
  where attrSetId p g              = g { _eScriptGlobals = Just (attrSetId p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasLang EScript
  where attrSetLang p g            = g { _eScriptGlobals = Just (attrSetLang p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasRole EScript
  where attrSetRole p g            = g { _eScriptGlobals = Just (attrSetRole p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasSlot EScript
  where attrSetSlot p g            = g { _eScriptGlobals = Just (attrSetSlot p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasSpellCheck EScript
  where attrSetSpellCheck p g      = g { _eScriptGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasStyle EScript
  where attrSetStyle p g           = g { _eScriptGlobals = Just (attrSetStyle p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasTabIndex EScript
  where attrSetTabIndex p g        = g { _eScriptGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasTitle EScript
  where attrSetTitle p g           = g { _eScriptGlobals = Just (attrSetTitle p (fromMaybe gDef (_eScriptGlobals g))) }
instance AttrHasTranslate EScript
  where attrSetTranslate p g       = g { _eScriptGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eScriptGlobals g))) }

instance AttrGetClassName EScript where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eScriptGlobals g)

instance AttrHasAsync EScript where attrSetAsync p g = g {_eScriptAsync = Just p }
instance AttrHasCharSet EScript where attrSetCharSet p g = g {_eScriptCharSet = Just p }
instance AttrHasCrossOrigin EScript where attrSetCrossOrigin p g = g {_eScriptCrossOrigin = Just p }
instance AttrHasDefer EScript where attrSetDefer p g = g {_eScriptDefer = Just p }
instance AttrHasNonce EScript where attrSetNonce p g = g {_eScriptNonce = Just p }
instance AttrHasSrc EScript where attrSetSrc p g = g {_eScriptSrc = Just p }
instance AttrHasScriptType EScript where attrSetScriptType p g = g {_eScriptType = Just p }

instance AttrHasCustom EScript where attrSetCustom p g       = g { _eScriptCustom = Just p }

eScript' :: forall t m a. DomBuilder t m => EScript -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eScript' b  = elAttr' "script" (attrMap b)

eScript :: forall t m a. DomBuilder t m => EScript -> m a -> m a
eScript b children = snd <$> eScript' b children

eScriptN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eScriptN' = el' "script"

eScriptN :: forall t m a. DomBuilder t m => m a -> m a
eScriptN children = snd <$> eScriptN' children

eScriptD' :: forall t m a. MonadWidget t m => Dynamic t EScript -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eScriptD' b  = elDynAttr' "script" (attrMap <$> b)

eScriptD :: forall t m a. MonadWidget t m => Dynamic t EScript -> m a -> m a
eScriptD b children = snd <$> eScriptD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Style-element
data EStyle = EStyle
  { _eStyleGlobals   :: Maybe Globals
  , _eStyleMedia     :: Maybe Media
  , _eStyleNonce     :: Maybe Nonce
  , _eStyleMediaType :: Maybe MediaType
  , _eStyleCustom    :: Maybe Attr
  }


instance AttrMap EStyle where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eStyleGlobals b
    , attrMap <$> _eStyleMedia b
    , attrMap <$> _eStyleNonce b
    , attrMap <$> _eStyleMediaType b
    ] <> maybe [] (\a -> [a]) (_eStyleCustom b)

instance Default EStyle where
  def = EStyle def def def def def

instance AttrHasGlobals EStyle where
   attrSetGlobals p b = b { _eStyleGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EStyle
  where attrSetAccessKey p g = g { _eStyleGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasAnmval EStyle
  where attrSetAnmval p g = g { _eStyleGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasContentEditable EStyle
  where attrSetContentEditable p g = g  { _eStyleGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasContextMenu EStyle
  where attrSetContextMenu p g     = g { _eStyleGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasClass EStyle
  where attrSetClassName p g           = g { _eStyleGlobals = Just (attrSetClassName p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasDnmval EStyle
  where attrSetDnmval p g           = g { _eStyleGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasDir EStyle
  where attrSetDir p g             = g { _eStyleGlobals = Just (attrSetDir p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasDraggable EStyle
  where attrSetDraggable p g       = g { _eStyleGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasHidden EStyle
  where attrSetHidden p g          = g { _eStyleGlobals = Just (attrSetHidden p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasId EStyle
  where attrSetId p g              = g { _eStyleGlobals = Just (attrSetId p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasLang EStyle
  where attrSetLang p g            = g { _eStyleGlobals = Just (attrSetLang p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasRole EStyle
  where attrSetRole p g            = g { _eStyleGlobals = Just (attrSetRole p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasSlot EStyle
  where attrSetSlot p g            = g { _eStyleGlobals = Just (attrSetSlot p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasSpellCheck EStyle
  where attrSetSpellCheck p g      = g { _eStyleGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasStyle EStyle
  where attrSetStyle p g           = g { _eStyleGlobals = Just (attrSetStyle p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasTabIndex EStyle
  where attrSetTabIndex p g        = g { _eStyleGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasTitle EStyle
  where attrSetTitle p g           = g { _eStyleGlobals = Just (attrSetTitle p (fromMaybe gDef (_eStyleGlobals g))) }
instance AttrHasTranslate EStyle
  where attrSetTranslate p g       = g { _eStyleGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eStyleGlobals g))) }

instance AttrGetClassName EStyle where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eStyleGlobals g)

instance AttrHasMedia EStyle where attrSetMedia p g = g {_eStyleMedia = Just p }
instance AttrHasNonce EStyle where attrSetNonce p g = g {_eStyleNonce = Just p }
instance AttrHasMediaType EStyle where attrSetMediaType p g = g {_eStyleMediaType = Just p }

instance AttrHasCustom EStyle where attrSetCustom p g       = g { _eStyleCustom = Just p }

eStyle' :: forall t m a. DomBuilder t m => EStyle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStyle' b  = elAttr' "style" (attrMap b)

eStyle :: forall t m a. DomBuilder t m => EStyle -> m a -> m a
eStyle b children = snd <$> eStyle' b children

eStyleN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStyleN' = el' "style"

eStyleN :: forall t m a. DomBuilder t m => m a -> m a
eStyleN children = snd <$> eStyleN' children

eStyleD' :: forall t m a. MonadWidget t m => Dynamic t EStyle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eStyleD' b  = elDynAttr' "style" (attrMap <$> b)

eStyleD :: forall t m a. MonadWidget t m => Dynamic t EStyle -> m a -> m a
eStyleD b children = snd <$> eStyleD' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Main-element has only the global attributes.
data EMain = EMain
  { _eMainGlobals :: Maybe Globals
  , _eMainCustom  :: Maybe Attr
  }


instance AttrMap EMain where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eMainGlobals b
    ] <> maybe [] (\a -> [a]) (_eMainCustom b)

instance Default EMain where
  def = EMain def def

instance AttrHasGlobals EMain where
   attrSetGlobals p b = b { _eMainGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EMain
  where attrSetAccessKey p g = g { _eMainGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasAnmval EMain
  where attrSetAnmval p g = g { _eMainGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasContentEditable EMain
  where attrSetContentEditable p g = g  { _eMainGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasContextMenu EMain
  where attrSetContextMenu p g     = g { _eMainGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasClass EMain
  where attrSetClassName p g           = g { _eMainGlobals = Just (attrSetClassName p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasDnmval EMain
  where attrSetDnmval p g           = g { _eMainGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasDir EMain
  where attrSetDir p g             = g { _eMainGlobals = Just (attrSetDir p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasDraggable EMain
  where attrSetDraggable p g       = g { _eMainGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasHidden EMain
  where attrSetHidden p g          = g { _eMainGlobals = Just (attrSetHidden p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasId EMain
  where attrSetId p g              = g { _eMainGlobals = Just (attrSetId p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasLang EMain
  where attrSetLang p g            = g { _eMainGlobals = Just (attrSetLang p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasRole EMain
  where attrSetRole p g            = g { _eMainGlobals = Just (attrSetRole p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasSlot EMain
  where attrSetSlot p g            = g { _eMainGlobals = Just (attrSetSlot p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasSpellCheck EMain
  where attrSetSpellCheck p g      = g { _eMainGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasStyle EMain
  where attrSetStyle p g           = g { _eMainGlobals = Just (attrSetStyle p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasTabIndex EMain
  where attrSetTabIndex p g        = g { _eMainGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasTitle EMain
  where attrSetTitle p g           = g { _eMainGlobals = Just (attrSetTitle p (fromMaybe gDef (_eMainGlobals g))) }
instance AttrHasTranslate EMain
  where attrSetTranslate p g       = g { _eMainGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eMainGlobals g))) }

instance AttrGetClassName EMain where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eMainGlobals g)

instance AttrHasCustom EMain where attrSetCustom p g       = g { _eMainCustom = Just p }

eMain' :: forall t m a. DomBuilder t m => EMain -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMain' b  = elAttr' "main" (attrMap b)

eMain :: forall t m a. DomBuilder t m => EMain -> m a -> m a
eMain b children = snd <$> eMain' b children

eMainN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMainN' = el' "main"

eMainN :: forall t m a. DomBuilder t m => m a -> m a
eMainN children = snd <$> eMainN' children

eMainD' :: forall t m a. MonadWidget t m => Dynamic t EMain -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eMainD' b  = elDynAttr' "main" (attrMap <$> b)

eMainD :: forall t m a. MonadWidget t m => Dynamic t EMain -> m a -> m a
eMainD b children = snd <$> eMainD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Template-element has only the global attributes.
data ETemplate = ETemplate
  { _eTemplateGlobals :: Maybe Globals
  , _eTemplateCustom  :: Maybe Attr
  }



instance AttrMap ETemplate where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTemplateGlobals b
    ] <> maybe [] (\a -> [a]) (_eTemplateCustom b)

instance Default ETemplate where
  def = ETemplate def def

instance AttrHasGlobals ETemplate where
   attrSetGlobals p b = b { _eTemplateGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETemplate
  where attrSetAccessKey p g = g { _eTemplateGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasAnmval ETemplate
  where attrSetAnmval p g = g { _eTemplateGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasContentEditable ETemplate
  where attrSetContentEditable p g = g  { _eTemplateGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasContextMenu ETemplate
  where attrSetContextMenu p g     = g { _eTemplateGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasClass ETemplate
  where attrSetClassName p g           = g { _eTemplateGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasDnmval ETemplate
  where attrSetDnmval p g           = g { _eTemplateGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasDir ETemplate
  where attrSetDir p g             = g { _eTemplateGlobals = Just (attrSetDir p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasDraggable ETemplate
  where attrSetDraggable p g       = g { _eTemplateGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasHidden ETemplate
  where attrSetHidden p g          = g { _eTemplateGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasId ETemplate
  where attrSetId p g              = g { _eTemplateGlobals = Just (attrSetId p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasLang ETemplate
  where attrSetLang p g            = g { _eTemplateGlobals = Just (attrSetLang p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasRole ETemplate
  where attrSetRole p g            = g { _eTemplateGlobals = Just (attrSetRole p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasSlot ETemplate
  where attrSetSlot p g            = g { _eTemplateGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasSpellCheck ETemplate
  where attrSetSpellCheck p g      = g { _eTemplateGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasStyle ETemplate
  where attrSetStyle p g           = g { _eTemplateGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasTabIndex ETemplate
  where attrSetTabIndex p g        = g { _eTemplateGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasTitle ETemplate
  where attrSetTitle p g           = g { _eTemplateGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTemplateGlobals g))) }
instance AttrHasTranslate ETemplate
  where attrSetTranslate p g       = g { _eTemplateGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTemplateGlobals g))) }

instance AttrGetClassName ETemplate where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTemplateGlobals g)

instance AttrHasCustom ETemplate where attrSetCustom p g       = g { _eTemplateCustom = Just p }

eTemplate' :: forall t m a. DomBuilder t m => ETemplate -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTemplate' b  = elAttr' "template" (attrMap b)

eTemplate :: forall t m a. DomBuilder t m => ETemplate -> m a -> m a
eTemplate b children = snd <$> eTemplate' b children

eTemplateN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTemplateN' = el' "template"

eTemplateN :: forall t m a. DomBuilder t m => m a -> m a
eTemplateN children = snd <$> eTemplateN' children

eTemplateD' :: forall t m a. MonadWidget t m => Dynamic t ETemplate -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTemplateD' b  = elDynAttr' "template" (attrMap <$> b)

eTemplateD :: forall t m a. MonadWidget t m => Dynamic t ETemplate -> m a -> m a
eTemplateD b children = snd <$> eTemplateD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Title-element has only the global attributes.
data ETitle = ETitle
  { _eTitleGlobals :: Maybe Globals
  , _eTitleCustom  :: Maybe Attr
  }



instance AttrMap ETitle where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eTitleGlobals b
    ] <> maybe [] (\a -> [a]) (_eTitleCustom b)

instance Default ETitle where
  def = ETitle def def

instance AttrHasGlobals ETitle where
   attrSetGlobals p b = b { _eTitleGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ETitle
  where attrSetAccessKey p g = g { _eTitleGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasAnmval ETitle
  where attrSetAnmval p g = g { _eTitleGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasContentEditable ETitle
  where attrSetContentEditable p g = g  { _eTitleGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasContextMenu ETitle
  where attrSetContextMenu p g     = g { _eTitleGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasClass ETitle
  where attrSetClassName p g           = g { _eTitleGlobals = Just (attrSetClassName p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasDnmval ETitle
  where attrSetDnmval p g           = g { _eTitleGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasDir ETitle
  where attrSetDir p g             = g { _eTitleGlobals = Just (attrSetDir p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasDraggable ETitle
  where attrSetDraggable p g       = g { _eTitleGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasHidden ETitle
  where attrSetHidden p g          = g { _eTitleGlobals = Just (attrSetHidden p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasId ETitle
  where attrSetId p g              = g { _eTitleGlobals = Just (attrSetId p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasLang ETitle
  where attrSetLang p g            = g { _eTitleGlobals = Just (attrSetLang p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasRole ETitle
  where attrSetRole p g            = g { _eTitleGlobals = Just (attrSetRole p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasSlot ETitle
  where attrSetSlot p g            = g { _eTitleGlobals = Just (attrSetSlot p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasSpellCheck ETitle
  where attrSetSpellCheck p g      = g { _eTitleGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasStyle ETitle
  where attrSetStyle p g           = g { _eTitleGlobals = Just (attrSetStyle p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasTabIndex ETitle
  where attrSetTabIndex p g        = g { _eTitleGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasTitle ETitle
  where attrSetTitle p g           = g { _eTitleGlobals = Just (attrSetTitle p (fromMaybe gDef (_eTitleGlobals g))) }
instance AttrHasTranslate ETitle
  where attrSetTranslate p g       = g { _eTitleGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eTitleGlobals g))) }

instance AttrGetClassName ETitle where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eTitleGlobals g)

instance AttrHasCustom ETitle where attrSetCustom p g       = g { _eTitleCustom = Just p }

eTitle' :: forall t m a. DomBuilder t m => ETitle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTitle' b  = elAttr' "title" (attrMap b)

eTitle :: forall t m a. DomBuilder t m => ETitle -> m a -> m a
eTitle b children = snd <$> eTitle' b children

eTitleN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTitleN' = el' "title"

eTitleN :: forall t m a. DomBuilder t m => m a -> m a
eTitleN children = snd <$> eTitleN' children

eTitleD' :: forall t m a. MonadWidget t m => Dynamic t ETitle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eTitleD' b  = elDynAttr' "title" (attrMap <$> b)

eTitleD :: forall t m a. MonadWidget t m => Dynamic t ETitle -> m a -> m a
eTitleD b children = snd <$> eTitleD' b children

