{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Elements.SectionHead
Description : HTML5 sectioning and heading elements
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Section and Head

This module contains sectioning and heading contents
and has the following elements.

Sectioning tags: article, aside, nav, section
Heading tags: h1 - h6

The naming convention follows that of Elements.

-}

module Reflex.Dom.HTML5.Elements.SectionHead
  where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', el')

import Reflex.Dom.HTML5.Attrs (AttrMap, attrMap,
  attrSetGlobals, attrSetAccessKey, attrSetContentEditable,
  attrSetContextMenu, attrSetClassName, attrSetDir, attrSetDraggable,
  attrSetHidden, attrSetId, attrSetLang, attrSetSpellCheck,
  attrSetStyle, attrSetTabIndex, attrSetTitle, attrSetTranslate,
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
  AttrHasAnmval, AttrHasDnmval, AttrHasRole, AttrHasSlot,
  Globals, AttrGetClassName, attrGetClassName, gDef, ClassName (ClassName),
  AttrHasCustom (attrSetCustom), Attr
  )



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Article-element has only the global attributes.
data EArticle = EArticle
  { _eArticleGlobals :: Maybe Globals
  , _eArticleCustom  :: Maybe Attr
  }


instance AttrMap EArticle where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eArticleGlobals b
    ] <> maybeToList (_eArticleCustom b)

instance Default EArticle where
  def = EArticle def def

instance Monoid EArticle where
  mempty = def
  mappend (EArticle a1 a2) (EArticle b1 b2) = EArticle (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EArticle where
   attrSetGlobals p b = b { _eArticleGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EArticle
  where attrSetAccessKey p g = g { _eArticleGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasAnmval EArticle
  where attrSetAnmval p g = g { _eArticleGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasContentEditable EArticle
  where attrSetContentEditable p g = g  { _eArticleGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasContextMenu EArticle
  where attrSetContextMenu p g     = g { _eArticleGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasClass EArticle
  where attrSetClassName p g           = g { _eArticleGlobals = Just (attrSetClassName p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasDnmval EArticle
  where attrSetDnmval p g           = g { _eArticleGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasDir EArticle
  where attrSetDir p g             = g { _eArticleGlobals = Just (attrSetDir p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasDraggable EArticle
  where attrSetDraggable p g       = g { _eArticleGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasHidden EArticle
  where attrSetHidden p g          = g { _eArticleGlobals = Just (attrSetHidden p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasId EArticle
  where attrSetId p g              = g { _eArticleGlobals = Just (attrSetId p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasLang EArticle
  where attrSetLang p g            = g { _eArticleGlobals = Just (attrSetLang p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasRole EArticle
  where attrSetRole p g            = g { _eArticleGlobals = Just (attrSetRole p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasSlot EArticle
  where attrSetSlot p g            = g { _eArticleGlobals = Just (attrSetSlot p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasSpellCheck EArticle
  where attrSetSpellCheck p g      = g { _eArticleGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasStyle EArticle
  where attrSetStyle p g           = g { _eArticleGlobals = Just (attrSetStyle p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasTabIndex EArticle
  where attrSetTabIndex p g        = g { _eArticleGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasTitle EArticle
  where attrSetTitle p g           = g { _eArticleGlobals = Just (attrSetTitle p (fromMaybe gDef (_eArticleGlobals g))) }
instance AttrHasTranslate EArticle
  where attrSetTranslate p g       = g { _eArticleGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eArticleGlobals g))) }

instance AttrGetClassName EArticle where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eArticleGlobals g)

instance AttrHasCustom EArticle where attrSetCustom p g       = g { _eArticleCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"article\" ... @
eArticle' :: forall t m a. DomBuilder t m => EArticle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eArticle' b = elAttr' "article" (attrMap b)

-- | A short-hand notion for @ elAttr \"article\" ... @
eArticle :: forall t m a. DomBuilder t m => EArticle -> m a -> m a
eArticle b children = snd <$> eArticle' b children

-- | A short-hand notion for @ el\' \"article\" ... @
eArticleN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eArticleN' = el' "article"

-- | A short-hand notion for @ el \"article\" ... @
eArticleN :: forall t m a. DomBuilder t m => m a -> m a
eArticleN children = snd <$> eArticleN' children

-- | A short-hand notion for @ elDynAttr\' \"article\" ... @
eArticleD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EArticle -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eArticleD' b = elDynAttr' "article" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"article\" ... @
eArticleD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EArticle -> m a -> m a
eArticleD b children = snd <$> eArticleD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Aside-element has only the global attributes.
data EAside = EAside
  { _eAsideGlobals :: Maybe Globals
  , _eAsideCustom  :: Maybe Attr
  }


instance AttrMap EAside where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eAsideGlobals b
    ] <> maybeToList (_eAsideCustom b)

instance Default EAside where
  def = EAside def def

instance Monoid EAside where
  mempty = def
  mappend (EAside a1 a2) (EAside b1 b2) = EAside (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EAside where
   attrSetGlobals p b = b { _eAsideGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EAside
  where attrSetAccessKey p g = g { _eAsideGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasAnmval EAside
  where attrSetAnmval p g = g { _eAsideGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasContentEditable EAside
  where attrSetContentEditable p g = g  { _eAsideGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasContextMenu EAside
  where attrSetContextMenu p g     = g { _eAsideGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasClass EAside
  where attrSetClassName p g           = g { _eAsideGlobals = Just (attrSetClassName p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasDnmval EAside
  where attrSetDnmval p g           = g { _eAsideGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasDir EAside
  where attrSetDir p g             = g { _eAsideGlobals = Just (attrSetDir p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasDraggable EAside
  where attrSetDraggable p g       = g { _eAsideGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasHidden EAside
  where attrSetHidden p g          = g { _eAsideGlobals = Just (attrSetHidden p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasId EAside
  where attrSetId p g              = g { _eAsideGlobals = Just (attrSetId p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasLang EAside
  where attrSetLang p g            = g { _eAsideGlobals = Just (attrSetLang p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasRole EAside
  where attrSetRole p g            = g { _eAsideGlobals = Just (attrSetRole p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasSlot EAside
  where attrSetSlot p g            = g { _eAsideGlobals = Just (attrSetSlot p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasSpellCheck EAside
  where attrSetSpellCheck p g      = g { _eAsideGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasStyle EAside
  where attrSetStyle p g           = g { _eAsideGlobals = Just (attrSetStyle p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasTabIndex EAside
  where attrSetTabIndex p g        = g { _eAsideGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasTitle EAside
  where attrSetTitle p g           = g { _eAsideGlobals = Just (attrSetTitle p (fromMaybe gDef (_eAsideGlobals g))) }
instance AttrHasTranslate EAside
  where attrSetTranslate p g       = g { _eAsideGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eAsideGlobals g))) }

instance AttrGetClassName EAside where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eAsideGlobals g)

instance AttrHasCustom EAside where attrSetCustom p g       = g { _eAsideCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"aside\" ... @
eAside' :: forall t m a. DomBuilder t m => EAside -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAside' b = elAttr' "aside" (attrMap b)

-- | A short-hand notion for @ elAttr \"aside\" ... @
eAside :: forall t m a. DomBuilder t m => EAside -> m a -> m a
eAside b children = snd <$> eAside' b children

-- | A short-hand notion for @ el\' \"aside\" ... @
eAsideN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAsideN' = el' "aside"

-- | A short-hand notion for @ el \"aside\" ... @
eAsideN :: forall t m a. DomBuilder t m => m a -> m a
eAsideN children = snd <$> eAsideN' children

-- | A short-hand notion for @ elDynAttr\' \"aside\" ... @
eAsideD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EAside -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eAsideD' b = elDynAttr' "aside" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"aside\" ... @
eAsideD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EAside -> m a -> m a
eAsideD b children = snd <$> eAsideD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Nav-element has only the global attributes.
data ENav = ENav
  { _eNavGlobals :: Maybe Globals
  , _eNavCustom  :: Maybe Attr
  }


instance AttrMap ENav where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eNavGlobals b
    ] <> maybeToList (_eNavCustom b)

instance Default ENav where
  def = ENav def def

instance Monoid ENav where
  mempty = def
  mappend (ENav a1 a2) (ENav b1 b2) = ENav (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ENav where
   attrSetGlobals p b = b { _eNavGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ENav
  where attrSetAccessKey p g = g { _eNavGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasAnmval ENav
  where attrSetAnmval p g = g { _eNavGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasContentEditable ENav
  where attrSetContentEditable p g = g  { _eNavGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasContextMenu ENav
  where attrSetContextMenu p g     = g { _eNavGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasClass ENav
  where attrSetClassName p g           = g { _eNavGlobals = Just (attrSetClassName p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasDnmval ENav
  where attrSetDnmval p g           = g { _eNavGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasDir ENav
  where attrSetDir p g             = g { _eNavGlobals = Just (attrSetDir p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasDraggable ENav
  where attrSetDraggable p g       = g { _eNavGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasHidden ENav
  where attrSetHidden p g          = g { _eNavGlobals = Just (attrSetHidden p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasId ENav
  where attrSetId p g              = g { _eNavGlobals = Just (attrSetId p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasLang ENav
  where attrSetLang p g            = g { _eNavGlobals = Just (attrSetLang p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasRole ENav
  where attrSetRole p g            = g { _eNavGlobals = Just (attrSetRole p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasSlot ENav
  where attrSetSlot p g            = g { _eNavGlobals = Just (attrSetSlot p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasSpellCheck ENav
  where attrSetSpellCheck p g      = g { _eNavGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasStyle ENav
  where attrSetStyle p g           = g { _eNavGlobals = Just (attrSetStyle p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasTabIndex ENav
  where attrSetTabIndex p g        = g { _eNavGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasTitle ENav
  where attrSetTitle p g           = g { _eNavGlobals = Just (attrSetTitle p (fromMaybe gDef (_eNavGlobals g))) }
instance AttrHasTranslate ENav
  where attrSetTranslate p g       = g { _eNavGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eNavGlobals g))) }

instance AttrGetClassName ENav where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eNavGlobals g)

instance AttrHasCustom ENav where attrSetCustom p g       = g { _eNavCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"nav\" ... @
eNav' :: forall t m a. DomBuilder t m => ENav -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNav' b  = elAttr' "nav" (attrMap b)

-- | A short-hand notion for @ elAttr \"nav\" ... @
eNav :: forall t m a. DomBuilder t m => ENav -> m a -> m a
eNav b children = snd <$> eNav' b children

-- | A short-hand notion for @ el\' \"nav\" ... @
eNavN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNavN' = el' "nav"

-- | A short-hand notion for @ el \"nav\" ... @
eNavN :: forall t m a. DomBuilder t m => m a -> m a
eNavN children = snd <$> eNavN' children

-- | A short-hand notion for @ elDynAttr\' \"nav\" ... @
eNavD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ENav -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eNavD' b  = elDynAttr' "nav" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"nav\" ... @
eNavD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ENav -> m a -> m a
eNavD b children = snd <$> eNavD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Section-element has only the global attributes.
data ESection = ESection
  { _eSectionGlobals :: Maybe Globals
  , _eSectionCustom  :: Maybe Attr
  }


instance AttrMap ESection where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eSectionGlobals b
    ] <> maybeToList (_eSectionCustom b)

instance Default ESection where
  def = ESection def def

instance Monoid ESection where
  mempty = def
  mappend (ESection a1 a2) (ESection b1 b2) = ESection (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals ESection where
   attrSetGlobals p b = b { _eSectionGlobals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey ESection
  where attrSetAccessKey p g = g { _eSectionGlobals = Just (attrSetAccessKey p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasAnmval ESection
  where attrSetAnmval p g = g { _eSectionGlobals = Just (attrSetAnmval p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasContentEditable ESection
  where attrSetContentEditable p g = g  { _eSectionGlobals = Just (attrSetContentEditable p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasContextMenu ESection
  where attrSetContextMenu p g     = g { _eSectionGlobals = Just (attrSetContextMenu p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasClass ESection
  where attrSetClassName p g           = g { _eSectionGlobals = Just (attrSetClassName p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasDnmval ESection
  where attrSetDnmval p g           = g { _eSectionGlobals = Just (attrSetDnmval p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasDir ESection
  where attrSetDir p g             = g { _eSectionGlobals = Just (attrSetDir p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasDraggable ESection
  where attrSetDraggable p g       = g { _eSectionGlobals = Just (attrSetDraggable p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasHidden ESection
  where attrSetHidden p g          = g { _eSectionGlobals = Just (attrSetHidden p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasId ESection
  where attrSetId p g              = g { _eSectionGlobals = Just (attrSetId p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasLang ESection
  where attrSetLang p g            = g { _eSectionGlobals = Just (attrSetLang p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasRole ESection
  where attrSetRole p g            = g { _eSectionGlobals = Just (attrSetRole p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasSlot ESection
  where attrSetSlot p g            = g { _eSectionGlobals = Just (attrSetSlot p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasSpellCheck ESection
  where attrSetSpellCheck p g      = g { _eSectionGlobals = Just (attrSetSpellCheck p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasStyle ESection
  where attrSetStyle p g           = g { _eSectionGlobals = Just (attrSetStyle p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasTabIndex ESection
  where attrSetTabIndex p g        = g { _eSectionGlobals = Just (attrSetTabIndex p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasTitle ESection
  where attrSetTitle p g           = g { _eSectionGlobals = Just (attrSetTitle p (fromMaybe gDef (_eSectionGlobals g))) }
instance AttrHasTranslate ESection
  where attrSetTranslate p g       = g { _eSectionGlobals = Just (attrSetTranslate p (fromMaybe gDef (_eSectionGlobals g))) }

instance AttrGetClassName ESection where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eSectionGlobals g)

instance AttrHasCustom ESection where attrSetCustom p g       = g { _eSectionCustom = Just p }

-- | A short-hand notion for @ elAttr\' \"section\" ... @
eSection' :: forall t m a. DomBuilder t m => ESection -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSection' b  = elAttr' "section" (attrMap b)

-- | A short-hand notion for @ elAttr \"section\" ... @
eSection :: forall t m a. DomBuilder t m => ESection -> m a -> m a
eSection b children = snd <$> eSection' b children

-- | A short-hand notion for @ el\' \"section\" ... @
eSectionN' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSectionN' = el' "section"

-- | A short-hand notion for @ el \"section\" ... @
eSectionN :: forall t m a. DomBuilder t m => m a -> m a
eSectionN children = snd <$> eSectionN' children

-- | A short-hand notion for @ elDynAttr\' \"section\" ... @
eSectionD' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ESection -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eSectionD' b  = elDynAttr' "section" (attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"section\" ... @
eSectionD :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t ESection -> m a -> m a
eSectionD b children = snd <$> eSectionD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H1-element has only the global attributes.
data EH1 = EH1
  { _eH1Globals :: Maybe Globals
  , _eH1Custom  :: Maybe Attr
  }


instance AttrMap EH1 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH1Globals b
    ] <> maybeToList (_eH1Custom b)

instance Default EH1 where
  def = EH1 def def

instance Monoid EH1 where
  mempty = def
  mappend (EH1 a1 a2) (EH1 b1 b2) = EH1 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH1 where
   attrSetGlobals p b = b { _eH1Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH1
  where attrSetAccessKey p g = g { _eH1Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasAnmval EH1
  where attrSetAnmval p g = g { _eH1Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasContentEditable EH1
  where attrSetContentEditable p g = g  { _eH1Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasContextMenu EH1
  where attrSetContextMenu p g     = g { _eH1Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasClass EH1
  where attrSetClassName p g           = g { _eH1Globals = Just (attrSetClassName p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasDnmval EH1
  where attrSetDnmval p g           = g { _eH1Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasDir EH1
  where attrSetDir p g             = g { _eH1Globals = Just (attrSetDir p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasDraggable EH1
  where attrSetDraggable p g       = g { _eH1Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasHidden EH1
  where attrSetHidden p g          = g { _eH1Globals = Just (attrSetHidden p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasId EH1
  where attrSetId p g              = g { _eH1Globals = Just (attrSetId p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasLang EH1
  where attrSetLang p g            = g { _eH1Globals = Just (attrSetLang p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasRole EH1
  where attrSetRole p g            = g { _eH1Globals = Just (attrSetRole p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasSlot EH1
  where attrSetSlot p g            = g { _eH1Globals = Just (attrSetSlot p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasSpellCheck EH1
  where attrSetSpellCheck p g      = g { _eH1Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasStyle EH1
  where attrSetStyle p g           = g { _eH1Globals = Just (attrSetStyle p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasTabIndex EH1
  where attrSetTabIndex p g        = g { _eH1Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasTitle EH1
  where attrSetTitle p g           = g { _eH1Globals = Just (attrSetTitle p (fromMaybe gDef (_eH1Globals g))) }
instance AttrHasTranslate EH1
  where attrSetTranslate p g       = g { _eH1Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH1Globals g))) }

instance AttrGetClassName EH1 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH1Globals g)

instance AttrHasCustom EH1 where attrSetCustom p g       = g { _eH1Custom = Just p }

eH1' :: forall t m a. DomBuilder t m => EH1 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH1' b  = elAttr' "h1" (attrMap b)

eH1 :: forall t m a. DomBuilder t m => EH1 -> m a -> m a
eH1 b children = snd <$> eH1' b children

eH1N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH1N' = el' "h1"

eH1N :: forall t m a. DomBuilder t m => m a -> m a
eH1N children = snd <$> eH1N' children

eH1D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH1 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH1D' b  = elDynAttr' "h1" (attrMap <$> b)

eH1D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH1 -> m a -> m a
eH1D b children = snd <$> eH1D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H2-element has only the global attributes.
data EH2 = EH2
  { _eH2Globals :: Maybe Globals
  , _eH2Custom  :: Maybe Attr
  }


instance AttrMap EH2 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH2Globals b
    ] <> maybeToList (_eH2Custom b)

instance Default EH2 where
  def = EH2 def def

instance Monoid EH2 where
  mempty = def
  mappend (EH2 a1 a2) (EH2 b1 b2) = EH2 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH2 where
   attrSetGlobals p b = b { _eH2Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH2
  where attrSetAccessKey p g = g { _eH2Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasAnmval EH2
  where attrSetAnmval p g = g { _eH2Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasContentEditable EH2
  where attrSetContentEditable p g = g  { _eH2Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasContextMenu EH2
  where attrSetContextMenu p g     = g { _eH2Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasClass EH2
  where attrSetClassName p g           = g { _eH2Globals = Just (attrSetClassName p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasDnmval EH2
  where attrSetDnmval p g           = g { _eH2Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasDir EH2
  where attrSetDir p g             = g { _eH2Globals = Just (attrSetDir p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasDraggable EH2
  where attrSetDraggable p g       = g { _eH2Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasHidden EH2
  where attrSetHidden p g          = g { _eH2Globals = Just (attrSetHidden p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasId EH2
  where attrSetId p g              = g { _eH2Globals = Just (attrSetId p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasLang EH2
  where attrSetLang p g            = g { _eH2Globals = Just (attrSetLang p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasRole EH2
  where attrSetRole p g            = g { _eH2Globals = Just (attrSetRole p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasSlot EH2
  where attrSetSlot p g            = g { _eH2Globals = Just (attrSetSlot p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasSpellCheck EH2
  where attrSetSpellCheck p g      = g { _eH2Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasStyle EH2
  where attrSetStyle p g           = g { _eH2Globals = Just (attrSetStyle p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasTabIndex EH2
  where attrSetTabIndex p g        = g { _eH2Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasTitle EH2
  where attrSetTitle p g           = g { _eH2Globals = Just (attrSetTitle p (fromMaybe gDef (_eH2Globals g))) }
instance AttrHasTranslate EH2
  where attrSetTranslate p g       = g { _eH2Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH2Globals g))) }

instance AttrGetClassName EH2 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH2Globals g)

instance AttrHasCustom EH2 where attrSetCustom p g       = g { _eH2Custom = Just p }

eH2' :: forall t m a. DomBuilder t m => EH2 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH2' b  = elAttr' "h2" (attrMap b)

eH2 :: forall t m a. DomBuilder t m => EH2 -> m a -> m a
eH2 b children = snd <$> eH2' b children

eH2N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH2N' = el' "h2"

eH2N :: forall t m a. DomBuilder t m => m a -> m a
eH2N children = snd <$> eH2N' children

eH2D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH2 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH2D' b  = elDynAttr' "h2" (attrMap <$> b)

eH2D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH2 -> m a -> m a
eH2D b children = snd <$> eH2D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H3-element has only the global attributes.
data EH3 = EH3
  { _eH3Globals :: Maybe Globals
  , _eH3Custom  :: Maybe Attr
  }


instance AttrMap EH3 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH3Globals b
    ] <> maybeToList (_eH3Custom b)

instance Default EH3 where
  def = EH3 def def

instance Monoid EH3 where
  mempty = def
  mappend (EH3 a1 a2) (EH3 b1 b2) = EH3 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH3 where
   attrSetGlobals p b = b { _eH3Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH3
  where attrSetAccessKey p g = g { _eH3Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasAnmval EH3
  where attrSetAnmval p g = g { _eH3Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasContentEditable EH3
  where attrSetContentEditable p g = g  { _eH3Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasContextMenu EH3
  where attrSetContextMenu p g     = g { _eH3Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasClass EH3
  where attrSetClassName p g           = g { _eH3Globals = Just (attrSetClassName p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasDnmval EH3
  where attrSetDnmval p g           = g { _eH3Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasDir EH3
  where attrSetDir p g             = g { _eH3Globals = Just (attrSetDir p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasDraggable EH3
  where attrSetDraggable p g       = g { _eH3Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasHidden EH3
  where attrSetHidden p g          = g { _eH3Globals = Just (attrSetHidden p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasId EH3
  where attrSetId p g              = g { _eH3Globals = Just (attrSetId p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasLang EH3
  where attrSetLang p g            = g { _eH3Globals = Just (attrSetLang p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasRole EH3
  where attrSetRole p g            = g { _eH3Globals = Just (attrSetRole p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasSlot EH3
  where attrSetSlot p g            = g { _eH3Globals = Just (attrSetSlot p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasSpellCheck EH3
  where attrSetSpellCheck p g      = g { _eH3Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasStyle EH3
  where attrSetStyle p g           = g { _eH3Globals = Just (attrSetStyle p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasTabIndex EH3
  where attrSetTabIndex p g        = g { _eH3Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasTitle EH3
  where attrSetTitle p g           = g { _eH3Globals = Just (attrSetTitle p (fromMaybe gDef (_eH3Globals g))) }
instance AttrHasTranslate EH3
  where attrSetTranslate p g       = g { _eH3Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH3Globals g))) }

instance AttrGetClassName EH3 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH3Globals g)

instance AttrHasCustom EH3 where attrSetCustom p g       = g { _eH3Custom = Just p }

eH3' :: forall t m a. DomBuilder t m => EH3 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH3' b  = elAttr' "h3" (attrMap b)

eH3 :: forall t m a. DomBuilder t m => EH3 -> m a -> m a
eH3 b children = snd <$> eH3' b children

eH3N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH3N' = el' "h3"

eH3N :: forall t m a. DomBuilder t m => m a -> m a
eH3N children = snd <$> eH3N' children

eH3D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH3 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH3D' b  = elDynAttr' "h3" (attrMap <$> b)

eH3D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH3 -> m a -> m a
eH3D b children = snd <$> eH3D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H4-element has only the global attributes.
data EH4 = EH4
  { _eH4Globals :: Maybe Globals
  , _eH4Custom  :: Maybe Attr
  }


instance AttrMap EH4 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH4Globals b
    ] <> maybeToList (_eH4Custom b)

instance Default EH4 where
  def = EH4 def def

instance Monoid EH4 where
  mempty = def
  mappend (EH4 a1 a2) (EH4 b1 b2) = EH4 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH4 where
   attrSetGlobals p b = b { _eH4Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH4
  where attrSetAccessKey p g = g { _eH4Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasAnmval EH4
  where attrSetAnmval p g = g { _eH4Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasContentEditable EH4
  where attrSetContentEditable p g = g  { _eH4Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasContextMenu EH4
  where attrSetContextMenu p g     = g { _eH4Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasClass EH4
  where attrSetClassName p g           = g { _eH4Globals = Just (attrSetClassName p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasDnmval EH4
  where attrSetDnmval p g           = g { _eH4Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasDir EH4
  where attrSetDir p g             = g { _eH4Globals = Just (attrSetDir p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasDraggable EH4
  where attrSetDraggable p g       = g { _eH4Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasHidden EH4
  where attrSetHidden p g          = g { _eH4Globals = Just (attrSetHidden p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasId EH4
  where attrSetId p g              = g { _eH4Globals = Just (attrSetId p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasLang EH4
  where attrSetLang p g            = g { _eH4Globals = Just (attrSetLang p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasRole EH4
  where attrSetRole p g            = g { _eH4Globals = Just (attrSetRole p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasSlot EH4
  where attrSetSlot p g            = g { _eH4Globals = Just (attrSetSlot p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasSpellCheck EH4
  where attrSetSpellCheck p g      = g { _eH4Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasStyle EH4
  where attrSetStyle p g           = g { _eH4Globals = Just (attrSetStyle p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasTabIndex EH4
  where attrSetTabIndex p g        = g { _eH4Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasTitle EH4
  where attrSetTitle p g           = g { _eH4Globals = Just (attrSetTitle p (fromMaybe gDef (_eH4Globals g))) }
instance AttrHasTranslate EH4
  where attrSetTranslate p g       = g { _eH4Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH4Globals g))) }

instance AttrGetClassName EH4 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH4Globals g)

instance AttrHasCustom EH4 where attrSetCustom p g       = g { _eH4Custom = Just p }

eH4' :: forall t m a. DomBuilder t m => EH4 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH4' b  = elAttr' "h4" (attrMap b)

eH4 :: forall t m a. DomBuilder t m => EH4 -> m a -> m a
eH4 b children = snd <$> eH4' b children

eH4N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH4N' = el' "h4"

eH4N :: forall t m a. DomBuilder t m => m a -> m a
eH4N children = snd <$> eH4N' children

eH4D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH4 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH4D' b  = elDynAttr' "h4" (attrMap <$> b)

eH4D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH4 -> m a -> m a
eH4D b children = snd <$> eH4D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H5-element has only the global attributes.
data EH5 = EH5
  { _eH5Globals :: Maybe Globals
  , _eH5Custom  :: Maybe Attr
  }


instance AttrMap EH5 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH5Globals b
    ] <> maybeToList (_eH5Custom b)

instance Default EH5 where
  def = EH5 def def

instance Monoid EH5 where
  mempty = def
  mappend (EH5 a1 a2) (EH5 b1 b2) = EH5 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH5 where
   attrSetGlobals p b = b { _eH5Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH5
  where attrSetAccessKey p g = g { _eH5Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasAnmval EH5
  where attrSetAnmval p g = g { _eH5Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasContentEditable EH5
  where attrSetContentEditable p g = g  { _eH5Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasContextMenu EH5
  where attrSetContextMenu p g     = g { _eH5Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasClass EH5
  where attrSetClassName p g           = g { _eH5Globals = Just (attrSetClassName p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasDnmval EH5
  where attrSetDnmval p g           = g { _eH5Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasDir EH5
  where attrSetDir p g             = g { _eH5Globals = Just (attrSetDir p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasDraggable EH5
  where attrSetDraggable p g       = g { _eH5Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasHidden EH5
  where attrSetHidden p g          = g { _eH5Globals = Just (attrSetHidden p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasId EH5
  where attrSetId p g              = g { _eH5Globals = Just (attrSetId p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasLang EH5
  where attrSetLang p g            = g { _eH5Globals = Just (attrSetLang p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasRole EH5
  where attrSetRole p g            = g { _eH5Globals = Just (attrSetRole p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasSlot EH5
  where attrSetSlot p g            = g { _eH5Globals = Just (attrSetSlot p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasSpellCheck EH5
  where attrSetSpellCheck p g      = g { _eH5Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasStyle EH5
  where attrSetStyle p g           = g { _eH5Globals = Just (attrSetStyle p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasTabIndex EH5
  where attrSetTabIndex p g        = g { _eH5Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasTitle EH5
  where attrSetTitle p g           = g { _eH5Globals = Just (attrSetTitle p (fromMaybe gDef (_eH5Globals g))) }
instance AttrHasTranslate EH5
  where attrSetTranslate p g       = g { _eH5Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH5Globals g))) }

instance AttrGetClassName EH5 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH5Globals g)

instance AttrHasCustom EH5 where attrSetCustom p g       = g { _eH5Custom = Just p }

eH5' :: forall t m a. DomBuilder t m => EH5 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH5' b  = elAttr' "h5" (attrMap b)

eH5 :: forall t m a. DomBuilder t m => EH5 -> m a -> m a
eH5 b children = snd <$> eH5' b children

eH5N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH5N' = el' "h5"

eH5N :: forall t m a. DomBuilder t m => m a -> m a
eH5N children = snd <$> eH5N' children

eH5D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH5 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH5D' b  = elDynAttr' "h5" (attrMap <$> b)

eH5D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH5 -> m a -> m a
eH5D b children = snd <$> eH5D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H6-element has only the global attributes.
data EH6 = EH6
  { _eH6Globals :: Maybe Globals
  , _eH6Custom  :: Maybe Attr
  }


instance AttrMap EH6 where
  attrMap b = fold $ catMaybes
    [ attrMap <$> _eH6Globals b
    ] <> maybeToList (_eH6Custom b)

instance Default EH6 where
  def = EH6 def def

instance Monoid EH6 where
  mempty = def
  mappend (EH6 a1 a2) (EH6 b1 b2) = EH6 (a1 <> b1) (a2 <> b2)

instance AttrHasGlobals EH6 where
   attrSetGlobals p b = b { _eH6Globals = Just p }

-- Global attributes require the following instances.
instance AttrHasAccessKey EH6
  where attrSetAccessKey p g = g { _eH6Globals = Just (attrSetAccessKey p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasAnmval EH6
  where attrSetAnmval p g = g { _eH6Globals = Just (attrSetAnmval p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasContentEditable EH6
  where attrSetContentEditable p g = g  { _eH6Globals = Just (attrSetContentEditable p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasContextMenu EH6
  where attrSetContextMenu p g     = g { _eH6Globals = Just (attrSetContextMenu p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasClass EH6
  where attrSetClassName p g           = g { _eH6Globals = Just (attrSetClassName p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasDnmval EH6
  where attrSetDnmval p g           = g { _eH6Globals = Just (attrSetDnmval p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasDir EH6
  where attrSetDir p g             = g { _eH6Globals = Just (attrSetDir p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasDraggable EH6
  where attrSetDraggable p g       = g { _eH6Globals = Just (attrSetDraggable p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasHidden EH6
  where attrSetHidden p g          = g { _eH6Globals = Just (attrSetHidden p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasId EH6
  where attrSetId p g              = g { _eH6Globals = Just (attrSetId p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasLang EH6
  where attrSetLang p g            = g { _eH6Globals = Just (attrSetLang p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasRole EH6
  where attrSetRole p g            = g { _eH6Globals = Just (attrSetRole p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasSlot EH6
  where attrSetSlot p g            = g { _eH6Globals = Just (attrSetSlot p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasSpellCheck EH6
  where attrSetSpellCheck p g      = g { _eH6Globals = Just (attrSetSpellCheck p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasStyle EH6
  where attrSetStyle p g           = g { _eH6Globals = Just (attrSetStyle p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasTabIndex EH6
  where attrSetTabIndex p g        = g { _eH6Globals = Just (attrSetTabIndex p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasTitle EH6
  where attrSetTitle p g           = g { _eH6Globals = Just (attrSetTitle p (fromMaybe gDef (_eH6Globals g))) }
instance AttrHasTranslate EH6
  where attrSetTranslate p g       = g { _eH6Globals = Just (attrSetTranslate p (fromMaybe gDef (_eH6Globals g))) }

instance AttrGetClassName EH6 where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_eH6Globals g)

instance AttrHasCustom EH6 where attrSetCustom p g       = g { _eH6Custom = Just p }

eH6' :: forall t m a. DomBuilder t m => EH6 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH6' b  = elAttr' "h6" (attrMap b)

eH6 :: forall t m a. DomBuilder t m => EH6 -> m a -> m a
eH6 b children = snd <$> eH6' b children

eH6N' :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH6N' = el' "h6"

eH6N :: forall t m a. DomBuilder t m => m a -> m a
eH6N children = snd <$> eH6N' children

eH6D' :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH6 -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
eH6D' b  = elDynAttr' "h6" (attrMap <$> b)

eH6D :: forall t m a. (DomBuilder t m, PostBuild t m) => Dynamic t EH6 -> m a -> m a
eH6D b children = snd <$> eH6D' b children

