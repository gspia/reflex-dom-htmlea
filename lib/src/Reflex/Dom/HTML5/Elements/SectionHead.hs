{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
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

import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', el')

import qualified Reflex.Dom.HTML5.Attrs as A


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Article-element has only the global attributes.
data Article = Article
  { _articleGlobals ∷ Maybe A.Globals
  , _articleCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Article where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _articleGlobals b
    ] <> maybeToList (_articleCustom b)

-- | Default value for has no attributes set (no globals nor other).
defArticle ∷ Article
defArticle = Article Nothing Nothing

-- | An instance.
instance Monoid Article where
  mempty = defArticle
  mappend (Article a1 a2) (Article b1 b2) = Article (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Article where
   attrSetGlobals pp b = b { _articleGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Article
  where attrSetAccessKey pp g = g { _articleGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Article
  where attrSetAnmval pp g = g { _articleGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Article
  where attrSetContentEditable pp g = g  { _articleGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Article
  where attrSetContextMenu pp g     = g { _articleGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasClass Article
  where attrSetClassName pp g           = g { _articleGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Article
  where attrSetDnmval pp g           = g { _articleGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasDir Article
  where attrSetDir pp g             = g { _articleGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Article
  where attrSetDraggable pp g       = g { _articleGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Article
  where attrSetHidden pp g          = g { _articleGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasId Article
  where attrSetId pp g              = g { _articleGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasLang Article
  where attrSetLang pp g            = g { _articleGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasRole Article
  where attrSetRole pp g            = g { _articleGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Article
  where attrSetSlot pp g            = g { _articleGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Article
  where attrSetSpellCheck pp g      = g { _articleGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Article
  where attrSetStyle pp g           = g { _articleGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Article
  where attrSetTabIndex pp g        = g { _articleGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Article
  where attrSetTitle pp g           = g { _articleGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_articleGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Article
  where attrSetTranslate pp g       = g { _articleGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_articleGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Article where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_articleGlobals g)

-- | An instance.
instance A.AttrHasCustom Article where attrSetCustom pp g       = g { _articleCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"article\" ... @
article' ∷ forall t m a. DomBuilder t m ⇒ Article → m a → m (Element EventResult (DomBuilderSpace m) t, a)
article' b = elAttr' "article" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"article\" ... @
article ∷ forall t m a. DomBuilder t m ⇒ Article → m a → m a
article b children = snd <$> article' b children

-- | A short-hand notion for @ el\' \"article\" ... @
articleN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
articleN' = el' "article"

-- | A short-hand notion for @ el \"article\" ... @
articleN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
articleN children = snd <$> articleN' children

-- | A short-hand notion for @ elDynAttr\' \"article\" ... @
articleD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Article → m a → m (Element EventResult (DomBuilderSpace m) t, a)
articleD' b = elDynAttr' "article" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"article\" ... @
articleD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Article → m a → m a
articleD b children = snd <$> articleD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Aside-element has only the global attributes.
data Aside = Aside
  { _asideGlobals ∷ Maybe A.Globals
  , _asideCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Aside where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _asideGlobals b
    ] <> maybeToList (_asideCustom b)

-- | Default value for has no attributes set (no globals nor other).
defAside ∷ Aside
defAside = Aside Nothing Nothing

-- | An instance.
instance Monoid Aside where
  mempty = defAside
  mappend (Aside a1 a2) (Aside b1 b2) = Aside (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Aside where
   attrSetGlobals pp b = b { _asideGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Aside
  where attrSetAccessKey pp g = g { _asideGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Aside
  where attrSetAnmval pp g = g { _asideGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Aside
  where attrSetContentEditable pp g = g  { _asideGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Aside
  where attrSetContextMenu pp g     = g { _asideGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasClass Aside
  where attrSetClassName pp g           = g { _asideGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Aside
  where attrSetDnmval pp g           = g { _asideGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasDir Aside
  where attrSetDir pp g             = g { _asideGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Aside
  where attrSetDraggable pp g       = g { _asideGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Aside
  where attrSetHidden pp g          = g { _asideGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasId Aside
  where attrSetId pp g              = g { _asideGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasLang Aside
  where attrSetLang pp g            = g { _asideGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasRole Aside
  where attrSetRole pp g            = g { _asideGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Aside
  where attrSetSlot pp g            = g { _asideGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Aside
  where attrSetSpellCheck pp g      = g { _asideGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Aside
  where attrSetStyle pp g           = g { _asideGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Aside
  where attrSetTabIndex pp g        = g { _asideGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Aside
  where attrSetTitle pp g           = g { _asideGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_asideGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Aside
  where attrSetTranslate pp g       = g { _asideGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_asideGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Aside where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_asideGlobals g)

-- | An instance.
instance A.AttrHasCustom Aside where attrSetCustom pp g       = g { _asideCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"aside\" ... @
aside' ∷ forall t m a. DomBuilder t m ⇒ Aside → m a → m (Element EventResult (DomBuilderSpace m) t, a)
aside' b = elAttr' "aside" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"aside\" ... @
aside ∷ forall t m a. DomBuilder t m ⇒ Aside → m a → m a
aside b children = snd <$> aside' b children

-- | A short-hand notion for @ el\' \"aside\" ... @
asideN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
asideN' = el' "aside"

-- | A short-hand notion for @ el \"aside\" ... @
asideN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
asideN children = snd <$> asideN' children

-- | A short-hand notion for @ elDynAttr\' \"aside\" ... @
asideD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Aside → m a → m (Element EventResult (DomBuilderSpace m) t, a)
asideD' b = elDynAttr' "aside" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"aside\" ... @
asideD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Aside → m a → m a
asideD b children = snd <$> asideD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Nav-element has only the global attributes.
data Nav = Nav
  { _navGlobals ∷ Maybe A.Globals
  , _navCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Nav where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _navGlobals b
    ] <> maybeToList (_navCustom b)

-- | Default value for has no attributes set (no globals nor other).
defNav ∷ Nav
defNav = Nav  Nothing Nothing

-- | An instance.
instance Monoid Nav where
  mempty = defNav
  mappend (Nav a1 a2) (Nav b1 b2) = Nav (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Nav where
   attrSetGlobals pp b = b { _navGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Nav
  where attrSetAccessKey pp g = g { _navGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Nav
  where attrSetAnmval pp g = g { _navGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Nav
  where attrSetContentEditable pp g = g  { _navGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Nav
  where attrSetContextMenu pp g     = g { _navGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasClass Nav
  where attrSetClassName pp g           = g { _navGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Nav
  where attrSetDnmval pp g           = g { _navGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasDir Nav
  where attrSetDir pp g             = g { _navGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Nav
  where attrSetDraggable pp g       = g { _navGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Nav
  where attrSetHidden pp g          = g { _navGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasId Nav
  where attrSetId pp g              = g { _navGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasLang Nav
  where attrSetLang pp g            = g { _navGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasRole Nav
  where attrSetRole pp g            = g { _navGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Nav
  where attrSetSlot pp g            = g { _navGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Nav
  where attrSetSpellCheck pp g      = g { _navGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Nav
  where attrSetStyle pp g           = g { _navGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Nav
  where attrSetTabIndex pp g        = g { _navGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Nav
  where attrSetTitle pp g           = g { _navGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_navGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Nav
  where attrSetTranslate pp g       = g { _navGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_navGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Nav where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_navGlobals g)

-- | An instance.
instance A.AttrHasCustom Nav where attrSetCustom pp g       = g { _navCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"nav\" ... @
nav' ∷ forall t m a. DomBuilder t m ⇒ Nav → m a → m (Element EventResult (DomBuilderSpace m) t, a)
nav' b  = elAttr' "nav" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"nav\" ... @
nav ∷ forall t m a. DomBuilder t m ⇒ Nav → m a → m a
nav b children = snd <$> nav' b children

-- | A short-hand notion for @ el\' \"nav\" ... @
navN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
navN' = el' "nav"

-- | A short-hand notion for @ el \"nav\" ... @
navN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
navN children = snd <$> navN' children

-- | A short-hand notion for @ elDynAttr\' \"nav\" ... @
navD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Nav → m a → m (Element EventResult (DomBuilderSpace m) t, a)
navD' b  = elDynAttr' "nav" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"nav\" ... @
navD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Nav → m a → m a
navD b children = snd <$> navD' b children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Section-element has only the global attributes.
data Section = Section
  { _sectionGlobals ∷ Maybe A.Globals
  , _sectionCustom  ∷ Maybe A.Attr
  }

-- | An instance.
instance A.AttrMap Section where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _sectionGlobals b
    ] <> maybeToList (_sectionCustom b)

-- | Default value for has no attributes set (no globals nor other).
defSection ∷ Section
defSection = Section  Nothing Nothing

-- | An instance.
instance Monoid Section where
  mempty = defSection
  mappend (Section a1 a2) (Section b1 b2) = Section (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals Section where
   attrSetGlobals pp b = b { _sectionGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Section
  where attrSetAccessKey pp g = g { _sectionGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Section
  where attrSetAnmval pp g = g { _sectionGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Section
  where attrSetContentEditable pp g = g  { _sectionGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Section
  where attrSetContextMenu pp g     = g { _sectionGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasClass Section
  where attrSetClassName pp g           = g { _sectionGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Section
  where attrSetDnmval pp g           = g { _sectionGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasDir Section
  where attrSetDir pp g             = g { _sectionGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Section
  where attrSetDraggable pp g       = g { _sectionGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Section
  where attrSetHidden pp g          = g { _sectionGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasId Section
  where attrSetId pp g              = g { _sectionGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasLang Section
  where attrSetLang pp g            = g { _sectionGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasRole Section
  where attrSetRole pp g            = g { _sectionGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Section
  where attrSetSlot pp g            = g { _sectionGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Section
  where attrSetSpellCheck pp g      = g { _sectionGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Section
  where attrSetStyle pp g           = g { _sectionGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Section
  where attrSetTabIndex pp g        = g { _sectionGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Section
  where attrSetTitle pp g           = g { _sectionGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_sectionGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Section
  where attrSetTranslate pp g       = g { _sectionGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_sectionGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Section where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_sectionGlobals g)

-- | An instance.
instance A.AttrHasCustom Section where attrSetCustom pp g       = g { _sectionCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"section\" ... @
section' ∷ forall t m a. DomBuilder t m ⇒ Section → m a → m (Element EventResult (DomBuilderSpace m) t, a)
section' b  = elAttr' "section" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"section\" ... @
section ∷ forall t m a. DomBuilder t m ⇒ Section → m a → m a
section b children = snd <$> section' b children

-- | A short-hand notion for @ el\' \"section\" ... @
sectionN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
sectionN' = el' "section"

-- | A short-hand notion for @ el \"section\" ... @
sectionN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
sectionN children = snd <$> sectionN' children

-- | A short-hand notion for @ elDynAttr\' \"section\" ... @
sectionD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Section → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sectionD' b  = elDynAttr' "section" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"section\" ... @
sectionD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Section → m a → m a
sectionD b children = snd <$> sectionD' b children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H1-element has only the global attributes.
data H1 = H1
  { _h1Globals ∷ Maybe A.Globals
  , _h1Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H1 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h1Globals b
    ] <> maybeToList (_h1Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH1 ∷ H1
defH1 = H1  Nothing Nothing

-- | An instance.
instance Monoid H1 where
  mempty = defH1
  mappend (H1 a1 a2) (H1 b1 b2) = H1 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H1 where
   attrSetGlobals pp b = b { _h1Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H1
  where attrSetAccessKey pp g = g { _h1Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H1
  where attrSetAnmval pp g = g { _h1Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H1
  where attrSetContentEditable pp g = g  { _h1Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H1
  where attrSetContextMenu pp g     = g { _h1Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasClass H1
  where attrSetClassName pp g           = g { _h1Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H1
  where attrSetDnmval pp g           = g { _h1Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasDir H1
  where attrSetDir pp g             = g { _h1Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H1
  where attrSetDraggable pp g       = g { _h1Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasHidden H1
  where attrSetHidden pp g          = g { _h1Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasId H1
  where attrSetId pp g              = g { _h1Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasLang H1
  where attrSetLang pp g            = g { _h1Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasRole H1
  where attrSetRole pp g            = g { _h1Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasSlot H1
  where attrSetSlot pp g            = g { _h1Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H1
  where attrSetSpellCheck pp g      = g { _h1Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasStyle H1
  where attrSetStyle pp g           = g { _h1Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H1
  where attrSetTabIndex pp g        = g { _h1Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasTitle H1
  where attrSetTitle pp g           = g { _h1Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h1Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H1
  where attrSetTranslate pp g       = g { _h1Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h1Globals g))) }

-- | An instance.
instance A.AttrGetClassName H1 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h1Globals g)

-- | An instance.
instance A.AttrHasCustom H1 where attrSetCustom pp g       = g { _h1Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h1\" ... @
h1' ∷ forall t m a. DomBuilder t m ⇒ H1 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h1' b  = elAttr' "h1" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h1\" ... @
h1 ∷ forall t m a. DomBuilder t m ⇒ H1 → m a → m a
h1 b children = snd <$> h1' b children

-- | A short-hand notion for @ el\' \"h1\" ... @
h1N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h1N' = el' "h1"

-- | A short-hand notion for @ el \"h1\" ... @
h1N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h1N children = snd <$> h1N' children

-- | A short-hand notion for @ elDynAttr\' \"h1\" ... @
h1D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H1 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h1D' b  = elDynAttr' "h1" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr\' \"h1\" ... @
h1D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H1 → m a → m a
h1D b children = snd <$> h1D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H2-element has only the global attributes.
data H2 = H2
  { _h2Globals ∷ Maybe A.Globals
  , _h2Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H2 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h2Globals b
    ] <> maybeToList (_h2Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH2 ∷ H2
defH2 = H2  Nothing Nothing

-- | An instance.
instance Monoid H2 where
  mempty = defH2
  mappend (H2 a1 a2) (H2 b1 b2) = H2 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H2 where
   attrSetGlobals pp b = b { _h2Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H2
  where attrSetAccessKey pp g = g { _h2Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H2
  where attrSetAnmval pp g = g { _h2Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H2
  where attrSetContentEditable pp g = g  { _h2Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H2
  where attrSetContextMenu pp g     = g { _h2Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasClass H2
  where attrSetClassName pp g           = g { _h2Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H2
  where attrSetDnmval pp g           = g { _h2Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasDir H2
  where attrSetDir pp g             = g { _h2Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H2
  where attrSetDraggable pp g       = g { _h2Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasHidden H2
  where attrSetHidden pp g          = g { _h2Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasId H2
  where attrSetId pp g              = g { _h2Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasLang H2
  where attrSetLang pp g            = g { _h2Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasRole H2
  where attrSetRole pp g            = g { _h2Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasSlot H2
  where attrSetSlot pp g            = g { _h2Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H2
  where attrSetSpellCheck pp g      = g { _h2Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasStyle H2
  where attrSetStyle pp g           = g { _h2Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H2
  where attrSetTabIndex pp g        = g { _h2Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasTitle H2
  where attrSetTitle pp g           = g { _h2Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h2Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H2
  where attrSetTranslate pp g       = g { _h2Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h2Globals g))) }

-- | An instance.
instance A.AttrGetClassName H2 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h2Globals g)

-- | An instance.
instance A.AttrHasCustom H2 where attrSetCustom pp g       = g { _h2Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h2\" ... @
h2' ∷ forall t m a. DomBuilder t m ⇒ H2 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h2' b  = elAttr' "h2" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h2\" ... @
h2 ∷ forall t m a. DomBuilder t m ⇒ H2 → m a → m a
h2 b children = snd <$> h2' b children

-- | A short-hand notion for @ el\' \"h2\" ... @
h2N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h2N' = el' "h2"

-- | A short-hand notion for @ el \"h2\" ... @
h2N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h2N children = snd <$> h2N' children

-- | A short-hand notion for @ elDynAttr\' \"h2\" ... @
h2D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H2 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h2D' b  = elDynAttr' "h2" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr\' \"h2\" ... @
h2D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H2 → m a → m a
h2D b children = snd <$> h2D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H3-element has only the global attributes.
data H3 = H3
  { _h3Globals ∷ Maybe A.Globals
  , _h3Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H3 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h3Globals b
    ] <> maybeToList (_h3Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH3 ∷ H3
defH3 = H3  Nothing Nothing

-- | An instance.
instance Monoid H3 where
  mempty = defH3
  mappend (H3 a1 a2) (H3 b1 b2) = H3 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H3 where
   attrSetGlobals pp b = b { _h3Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H3
  where attrSetAccessKey pp g = g { _h3Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H3
  where attrSetAnmval pp g = g { _h3Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H3
  where attrSetContentEditable pp g = g  { _h3Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H3
  where attrSetContextMenu pp g     = g { _h3Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasClass H3
  where attrSetClassName pp g           = g { _h3Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H3
  where attrSetDnmval pp g           = g { _h3Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasDir H3
  where attrSetDir pp g             = g { _h3Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H3
  where attrSetDraggable pp g       = g { _h3Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasHidden H3
  where attrSetHidden pp g          = g { _h3Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasId H3
  where attrSetId pp g              = g { _h3Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasLang H3
  where attrSetLang pp g            = g { _h3Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasRole H3
  where attrSetRole pp g            = g { _h3Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasSlot H3
  where attrSetSlot pp g            = g { _h3Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H3
  where attrSetSpellCheck pp g      = g { _h3Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasStyle H3
  where attrSetStyle pp g           = g { _h3Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H3
  where attrSetTabIndex pp g        = g { _h3Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasTitle H3
  where attrSetTitle pp g           = g { _h3Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h3Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H3
  where attrSetTranslate pp g       = g { _h3Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h3Globals g))) }

-- | An instance.
instance A.AttrGetClassName H3 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h3Globals g)

-- | An instance.
instance A.AttrHasCustom H3 where attrSetCustom pp g       = g { _h3Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h3\" ... @
h3' ∷ forall t m a. DomBuilder t m ⇒ H3 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h3' b  = elAttr' "h3" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h3\" ... @
h3 ∷ forall t m a. DomBuilder t m ⇒ H3 → m a → m a
h3 b children = snd <$> h3' b children

-- | A short-hand notion for @ el\' \"h3\" ... @
h3N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h3N' = el' "h3"

-- | A short-hand notion for @ el \"h3\" ... @
h3N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h3N children = snd <$> h3N' children

-- | A short-hand notion for @ elDynAttr\' \"h3\" ... @
h3D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H3 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h3D' b  = elDynAttr' "h3" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr\' \"h3\" ... @
h3D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H3 → m a → m a
h3D b children = snd <$> h3D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H4-element has only the global attributes.
data H4 = H4
  { _h4Globals ∷ Maybe A.Globals
  , _h4Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H4 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h4Globals b
    ] <> maybeToList (_h4Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH4 ∷ H4
defH4 = H4  Nothing Nothing

-- | An instance.
instance Monoid H4 where
  mempty = defH4
  mappend (H4 a1 a2) (H4 b1 b2) = H4 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H4 where
   attrSetGlobals pp b = b { _h4Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H4
  where attrSetAccessKey pp g = g { _h4Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H4
  where attrSetAnmval pp g = g { _h4Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H4
  where attrSetContentEditable pp g = g  { _h4Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H4
  where attrSetContextMenu pp g     = g { _h4Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasClass H4
  where attrSetClassName pp g           = g { _h4Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H4
  where attrSetDnmval pp g           = g { _h4Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasDir H4
  where attrSetDir pp g             = g { _h4Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H4
  where attrSetDraggable pp g       = g { _h4Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasHidden H4
  where attrSetHidden pp g          = g { _h4Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasId H4
  where attrSetId pp g              = g { _h4Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasLang H4
  where attrSetLang pp g            = g { _h4Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasRole H4
  where attrSetRole pp g            = g { _h4Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasSlot H4
  where attrSetSlot pp g            = g { _h4Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H4
  where attrSetSpellCheck pp g      = g { _h4Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasStyle H4
  where attrSetStyle pp g           = g { _h4Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H4
  where attrSetTabIndex pp g        = g { _h4Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasTitle H4
  where attrSetTitle pp g           = g { _h4Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h4Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H4
  where attrSetTranslate pp g       = g { _h4Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h4Globals g))) }

-- | An instance.
instance A.AttrGetClassName H4 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h4Globals g)

-- | An instance.
instance A.AttrHasCustom H4 where attrSetCustom pp g       = g { _h4Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h4\" ... @
h4' ∷ forall t m a. DomBuilder t m ⇒ H4 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h4' b  = elAttr' "h4" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h4\" ... @
h4 ∷ forall t m a. DomBuilder t m ⇒ H4 → m a → m a
h4 b children = snd <$> h4' b children

-- | A short-hand notion for @ el\' \"h4\" ... @
h4N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h4N' = el' "h4"

-- | A short-hand notion for @ el \"h4\" ... @
h4N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h4N children = snd <$> h4N' children

-- | A short-hand notion for @ elDynAttr\' \"h4\" ... @
h4D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H4 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h4D' b  = elDynAttr' "h4" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr\' \"h4\" ... @
h4D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H4 → m a → m a
h4D b children = snd <$> h4D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H5-element has only the global attributes.
data H5 = H5
  { _h5Globals ∷ Maybe A.Globals
  , _h5Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H5 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h5Globals b
    ] <> maybeToList (_h5Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH5 ∷ H5
defH5 = H5 Nothing Nothing

-- | An instance.
instance Monoid H5 where
  mempty = defH5
  mappend (H5 a1 a2) (H5 b1 b2) = H5 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H5 where
   attrSetGlobals pp b = b { _h5Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H5
  where attrSetAccessKey pp g = g { _h5Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H5
  where attrSetAnmval pp g = g { _h5Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H5
  where attrSetContentEditable pp g = g  { _h5Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H5
  where attrSetContextMenu pp g     = g { _h5Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasClass H5
  where attrSetClassName pp g           = g { _h5Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H5
  where attrSetDnmval pp g           = g { _h5Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasDir H5
  where attrSetDir pp g             = g { _h5Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H5
  where attrSetDraggable pp g       = g { _h5Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasHidden H5
  where attrSetHidden pp g          = g { _h5Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasId H5
  where attrSetId pp g              = g { _h5Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasLang H5
  where attrSetLang pp g            = g { _h5Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasRole H5
  where attrSetRole pp g            = g { _h5Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasSlot H5
  where attrSetSlot pp g            = g { _h5Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H5
  where attrSetSpellCheck pp g      = g { _h5Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasStyle H5
  where attrSetStyle pp g           = g { _h5Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H5
  where attrSetTabIndex pp g        = g { _h5Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasTitle H5
  where attrSetTitle pp g           = g { _h5Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h5Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H5
  where attrSetTranslate pp g       = g { _h5Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h5Globals g))) }

-- | An instance.
instance A.AttrGetClassName H5 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h5Globals g)

-- | An instance.
instance A.AttrHasCustom H5 where attrSetCustom pp g       = g { _h5Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h5\" ... @
h5' ∷ forall t m a. DomBuilder t m ⇒ H5 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h5' b  = elAttr' "h5" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h5\" ... @
h5 ∷ forall t m a. DomBuilder t m ⇒ H5 → m a → m a
h5 b children = snd <$> h5' b children

-- | A short-hand notion for @ el\' \"h5\" ... @
h5N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h5N' = el' "h5"

-- | A short-hand notion for @ el \"h5\" ... @
h5N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h5N children = snd <$> h5N' children

-- | A short-hand notion for @ elDynAttr\' \"h5\" ... @
h5D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H5 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h5D' b  = elDynAttr' "h5" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr\' \"h5\" ... @
h5D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H5 → m a → m a
h5D b children = snd <$> h5D' b children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | H6-element has only the global attributes.
data H6 = H6
  { _h6Globals ∷ Maybe A.Globals
  , _h6Custom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap H6 where
  attrMap b = fold $ catMaybes
    [ A.attrMap <$> _h6Globals b
    ] <> maybeToList (_h6Custom b)

-- | Default value for has no attributes set (no globals nor other).
defH6 ∷ H6
defH6 = H6  Nothing Nothing

-- | An instance.
instance Monoid H6 where
  mempty = defH6
  mappend (H6 a1 a2) (H6 b1 b2) = H6 (a1 <> b1) (a2 <> b2)

-- | An instance.
instance A.AttrHasGlobals H6 where
   attrSetGlobals pp b = b { _h6Globals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey H6
  where attrSetAccessKey pp g = g { _h6Globals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasAnmval H6
  where attrSetAnmval pp g = g { _h6Globals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasContentEditable H6
  where attrSetContentEditable pp g = g  { _h6Globals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasContextMenu H6
  where attrSetContextMenu pp g     = g { _h6Globals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasClass H6
  where attrSetClassName pp g           = g { _h6Globals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasDnmval H6
  where attrSetDnmval pp g           = g { _h6Globals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasDir H6
  where attrSetDir pp g             = g { _h6Globals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasDraggable H6
  where attrSetDraggable pp g       = g { _h6Globals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasHidden H6
  where attrSetHidden pp g          = g { _h6Globals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasId H6
  where attrSetId pp g              = g { _h6Globals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasLang H6
  where attrSetLang pp g            = g { _h6Globals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasRole H6
  where attrSetRole pp g            = g { _h6Globals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasSlot H6
  where attrSetSlot pp g            = g { _h6Globals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasSpellCheck H6
  where attrSetSpellCheck pp g      = g { _h6Globals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasStyle H6
  where attrSetStyle pp g           = g { _h6Globals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasTabIndex H6
  where attrSetTabIndex pp g        = g { _h6Globals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasTitle H6
  where attrSetTitle pp g           = g { _h6Globals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_h6Globals g))) }
-- | An instance.
instance A.AttrHasTranslate H6
  where attrSetTranslate pp g       = g { _h6Globals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_h6Globals g))) }

-- | An instance.
instance A.AttrGetClassName H6 where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_h6Globals g)

-- | An instance.
instance A.AttrHasCustom H6 where attrSetCustom pp g       = g { _h6Custom = Just pp }

-- | A short-hand notion for @ elAttr\' \"h6\" ... @
h6' ∷ forall t m a. DomBuilder t m ⇒ H6 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h6' b  = elAttr' "h6" (A.attrMap b)

-- | A short-hand notion for @ elAttr \"h6\" ... @
h6 ∷ forall t m a. DomBuilder t m ⇒ H6 → m a → m a
h6 b children = snd <$> h6' b children

-- | A short-hand notion for @ el\' \"h6\" ... @
h6N' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
h6N' = el' "h6"

-- | A short-hand notion for @ el \"h6\" ... @
h6N ∷ forall t m a. DomBuilder t m ⇒ m a → m a
h6N children = snd <$> h6N' children

-- | A short-hand notion for @ elDynAttr\' \"h6\" ... @
h6D' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H6 → m a → m (Element EventResult (DomBuilderSpace m) t, a)
h6D' b  = elDynAttr' "h6" (A.attrMap <$> b)

-- | A short-hand notion for @ elDynAttr \"h6\" ... @
h6D ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t H6 → m a → m a
h6D b children = snd <$> h6D' b children

