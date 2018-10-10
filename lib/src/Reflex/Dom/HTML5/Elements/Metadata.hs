{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

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

import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', blank, el')
import qualified Reflex.Dom.HTML5.Attrs as A
import           Prelude hiding (head)


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Base-element
data Base = Base
  { _baseGlobals ∷ Maybe A.Globals
  , _baseHref    ∷ Maybe A.Href
  , _baseTarget  ∷ Maybe A.Target
  , _baseCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Base where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _baseGlobals bm
    , A.attrMap <$> _baseHref bm
    , A.attrMap <$> _baseTarget bm
    ] <> maybeToList (_baseCustom bm)

-- | A default value for Base
defBase ∷ Base
defBase = Base Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Base where
  (<>) (Base a1 a2 a3 a4) (Base b1 b2 b3 b4)
    = Base (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Base where
  mempty = defBase
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Base where
   attrSetGlobals pp bm = bm { _baseGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Base
  where attrSetAccessKey pp g = g { _baseGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Base
  where attrSetAnmval pp g = g { _baseGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Base
  where attrSetContentEditable pp g = g  { _baseGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Base
  where attrSetContextMenu pp g     = g { _baseGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasClass Base
  where attrSetClassName pp g           = g { _baseGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Base
  where attrSetDnmval pp g           = g { _baseGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasDir Base
  where attrSetDir pp g             = g { _baseGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Base
  where attrSetDraggable pp g       = g { _baseGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Base
  where attrSetHidden pp g          = g { _baseGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasId Base
  where attrSetId pp g              = g { _baseGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasLang Base
  where attrSetLang pp g            = g { _baseGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasRole Base
  where attrSetRole pp g            = g { _baseGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Base
  where attrSetSlot pp g            = g { _baseGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Base
  where attrSetSpellCheck pp g      = g { _baseGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Base
  where attrSetStyle pp g           = g { _baseGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Base
  where attrSetTabIndex pp g        = g { _baseGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Base
  where attrSetTitle pp g           = g { _baseGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_baseGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Base
  where attrSetTranslate pp g       = g { _baseGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_baseGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Base where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_baseGlobals g)

-- | An instance.
instance A.AttrHasHref Base where attrSetHref pp g = g {_baseHref = Just pp }
-- | An instance.
instance A.AttrHasTarget Base where attrSetTarget pp g = g {_baseTarget = Just pp }

-- | An instance.
instance A.AttrHasCustom Base where attrSetCustom pp g       = g { _baseCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"base\" ... @
base' ∷ forall t m a. DomBuilder t m ⇒ Base → m a → m (Element EventResult (DomBuilderSpace m) t, a)
base' bm = elAttr' "base" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"base\" ... @
base ∷ forall t m a. DomBuilder t m ⇒ Base → m a → m a
base bm children = snd <$> base' bm children

-- | A short-hand notion for @ el\' \"base\" ... @
baseN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
baseN' = el' "base"

-- | A short-hand notion for @ el \"base\" ... @
baseN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
baseN children = snd <$> baseN' children

-- | A short-hand notion for @ elDynAttr\' \"base\" ... @
baseD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Base → m a → m (Element EventResult (DomBuilderSpace m) t, a)
baseD' bm = elDynAttr' "base" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"base\" ... @
baseD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Base → m a → m a
baseD bm children = snd <$> baseD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Body-element has only the global attributes.
data Body = Body
  { _bodyGlobals ∷ Maybe A.Globals
  , _bodyCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Body where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _bodyGlobals bm
    ] <> maybeToList (_bodyCustom bm)

-- | A default value for Body.
defBody ∷ Body
defBody = Body Nothing Nothing

-- | An instance.
instance Semigroup Body where
  (<>) (Body a1 a2) (Body b1 b2) = Body (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Body where
  mempty = defBody
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Body where
   attrSetGlobals pp bm = bm { _bodyGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Body
  where attrSetAccessKey pp g = g { _bodyGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Body
  where attrSetAnmval pp g = g { _bodyGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Body
  where attrSetContentEditable pp g = g  { _bodyGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Body
  where attrSetContextMenu pp g     = g { _bodyGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasClass Body
  where attrSetClassName pp g           = g { _bodyGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Body
  where attrSetDnmval pp g           = g { _bodyGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasDir Body
  where attrSetDir pp g             = g { _bodyGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Body
  where attrSetDraggable pp g       = g { _bodyGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Body
  where attrSetHidden pp g          = g { _bodyGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasId Body
  where attrSetId pp g              = g { _bodyGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasLang Body
  where attrSetLang pp g            = g { _bodyGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasRole Body
  where attrSetRole pp g            = g { _bodyGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Body
  where attrSetSlot pp g            = g { _bodyGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Body
  where attrSetSpellCheck pp g      = g { _bodyGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Body
  where attrSetStyle pp g           = g { _bodyGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Body
  where attrSetTabIndex pp g        = g { _bodyGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Body
  where attrSetTitle pp g           = g { _bodyGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_bodyGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Body
  where attrSetTranslate pp g       = g { _bodyGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_bodyGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Body where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_bodyGlobals g)

-- | An instance.
instance A.AttrHasCustom Body where attrSetCustom pp g       = g { _bodyCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"body\" ... @
body' ∷ forall t m a. DomBuilder t m ⇒ Body → m a → m (Element EventResult (DomBuilderSpace m) t, a)
body' bm = elAttr' "body" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"body\" ... @
body ∷ forall t m a. DomBuilder t m ⇒ Body → m a → m a
body bm children = snd <$> body' bm children

-- | A short-hand notion for @ el\' \"body\" ... @
bodyN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
bodyN' = el' "body"

-- | A short-hand notion for @ el \"body\" ... @
bodyN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
bodyN children = snd <$> bodyN' children

-- | A short-hand notion for @ elDynAttr\' \"body\" ... @
bodyD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Body → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bodyD' bm = elDynAttr' "body" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"body\" ... @
bodyD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Body → m a → m a
bodyD bm children = snd <$> bodyD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Head-element has only the global attributes.
data Head = Head
  { _headGlobals ∷ Maybe A.Globals
  , _headCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Head where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _headGlobals bm
    ] <> maybeToList (_headCustom bm)

-- | A default value for Head.
defHead ∷ Head
defHead = Head Nothing Nothing

-- | An instance.
instance Semigroup Head where
  (<>) (Head a1 a2) (Head b1 b2) = Head (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Head where
  mempty = defHead
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Head where
   attrSetGlobals pp bm = bm { _headGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Head
  where attrSetAccessKey pp g = g { _headGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Head
  where attrSetAnmval pp g = g { _headGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Head
  where attrSetContentEditable pp g = g  { _headGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Head
  where attrSetContextMenu pp g     = g { _headGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasClass Head
  where attrSetClassName pp g           = g { _headGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Head
  where attrSetDnmval pp g           = g { _headGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasDir Head
  where attrSetDir pp g             = g { _headGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Head
  where attrSetDraggable pp g       = g { _headGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Head
  where attrSetHidden pp g          = g { _headGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasId Head
  where attrSetId pp g              = g { _headGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasLang Head
  where attrSetLang pp g            = g { _headGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasRole Head
  where attrSetRole pp g            = g { _headGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Head
  where attrSetSlot pp g            = g { _headGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Head
  where attrSetSpellCheck pp g      = g { _headGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Head
  where attrSetStyle pp g           = g { _headGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Head
  where attrSetTabIndex pp g        = g { _headGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Head
  where attrSetTitle pp g           = g { _headGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_headGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Head
  where attrSetTranslate pp g       = g { _headGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_headGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Head where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_headGlobals g)

-- | An instance.
instance A.AttrHasCustom Head where attrSetCustom pp g       = g { _headCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"head\" ... @
head' ∷ forall t m a. DomBuilder t m ⇒ Head → m a → m (Element EventResult (DomBuilderSpace m) t, a)
head' bm = elAttr' "head" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"head\" ... @
elHead ∷ forall t m a. DomBuilder t m ⇒ Head → m a → m a
elHead bm children = snd <$> head' bm children

-- | A short-hand notion for @ el\' \"head\" ... @
headN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
headN' = el' "head"

-- | A short-hand notion for @ el \"head\" ... @
headN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
headN children = snd <$> headN' children

-- | A short-hand notion for @ elDynAttr\' \"head\" ... @
headD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Head → m a → m (Element EventResult (DomBuilderSpace m) t, a)
headD' bm = elDynAttr' "head" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"head\" ... @
headD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Head → m a → m a
headD bm children = snd <$> headD' bm children

head_ ∷ forall t m. DomBuilder t m ⇒ m ()
head_ = elHead defHead blank


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Html-element
data Html = Html
  { _htmlGlobals  ∷ Maybe A.Globals
  , _htmlManifest ∷ Maybe A.Manifest
  , _htmlCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Html where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _htmlGlobals bm
    , A.attrMap <$> _htmlManifest bm
    ] <> maybeToList (_htmlCustom bm)

-- | A default value for Html.
defHtml ∷ Html
defHtml = Html Nothing Nothing Nothing

-- | An instance.
instance Semigroup Html where
  (<>) (Html a1 a2 a3) (Html b1 b2 b3)
    = Html (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Html where
  mempty = defHtml
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Html where
   attrSetGlobals pp bm = bm { _htmlGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Html
  where attrSetAccessKey pp g = g { _htmlGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Html
  where attrSetAnmval pp g = g { _htmlGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Html
  where attrSetContentEditable pp g = g  { _htmlGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Html
  where attrSetContextMenu pp g     = g { _htmlGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasClass Html
  where attrSetClassName pp g           = g { _htmlGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Html
  where attrSetDnmval pp g           = g { _htmlGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasDir Html
  where attrSetDir pp g             = g { _htmlGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Html
  where attrSetDraggable pp g       = g { _htmlGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Html
  where attrSetHidden pp g          = g { _htmlGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasId Html
  where attrSetId pp g              = g { _htmlGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasLang Html
  where attrSetLang pp g            = g { _htmlGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasRole Html
  where attrSetRole pp g            = g { _htmlGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Html
  where attrSetSlot pp g            = g { _htmlGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Html
  where attrSetSpellCheck pp g      = g { _htmlGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Html
  where attrSetStyle pp g           = g { _htmlGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Html
  where attrSetTabIndex pp g        = g { _htmlGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Html
  where attrSetTitle pp g           = g { _htmlGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_htmlGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Html
  where attrSetTranslate pp g       = g { _htmlGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_htmlGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Html where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_htmlGlobals g)

-- | An instance.
instance A.AttrHasManifest Html where attrSetManifest pp g = g {_htmlManifest = Just pp }

-- | An instance.
instance A.AttrHasCustom Html where attrSetCustom pp g       = g { _htmlCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"html\" ... @
html' ∷ forall t m a. DomBuilder t m ⇒ Html → m a → m (Element EventResult (DomBuilderSpace m) t, a)
html' bm = elAttr' "html" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"html\" ... @
html ∷ forall t m a. DomBuilder t m ⇒ Html → m a → m a
html bm children = snd <$> html' bm children

-- | A short-hand notion for @ el\' \"html\" ... @
htmlN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
htmlN' = el' "html"

-- | A short-hand notion for @ el \"html\" ... @
htmlN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
htmlN children = snd <$> htmlN' children

-- | A short-hand notion for @ elDynAttr\' \"html\" ... @
htmlD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Html → m a → m (Element EventResult (DomBuilderSpace m) t, a)
htmlD' bm = elDynAttr' "html" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"html\" ... @
htmlD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Html → m a → m a
htmlD bm children = snd <$> htmlD' bm children

html_ ∷ forall t m. DomBuilder t m ⇒ m ()
html_ = html defHtml blank




------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Link-element
-- Note that integrity is not in HTML 5.3
data Link = Link
  { _linkGlobals        ∷ Maybe A.Globals
  , _linkCrossOrigin    ∷ Maybe A.CrossOrigin
  , _linkHref           ∷ Maybe A.Href
  , _linkHrefLang       ∷ Maybe A.HrefLang
  , _linkIntegrity      ∷ Maybe A.Integrity
  , _linkMedia          ∷ Maybe A.Media
  , _linkNonce          ∷ Maybe A.Nonce
  , _linkReferrerPolicy ∷ Maybe A.ReferrerPolicy
  , _linkRel            ∷ Maybe A.Rel
  , _linkMediaType      ∷ Maybe A.MediaType
  , _linkSizes          ∷ Maybe A.Sizes
  , _linkCustom         ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Link where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _linkGlobals bm
    , A.attrMap <$> _linkCrossOrigin bm
    , A.attrMap <$> _linkHref bm
    , A.attrMap <$> _linkHrefLang bm
    , A.attrMap <$> _linkIntegrity bm
    , A.attrMap <$> _linkMedia bm
    , A.attrMap <$> _linkNonce bm
    , A.attrMap <$> _linkReferrerPolicy bm
    , A.attrMap <$> _linkRel bm
    , A.attrMap <$> _linkMediaType bm
    , A.attrMap <$> _linkSizes bm
    ] <> maybeToList (_linkCustom bm)

-- | A default value for Link.
defLink ∷ Link
defLink = Link Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Link where
  (<>)
    (Link a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
    (Link b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = Link (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)
        (a11 <> b11) (a12 <> b12)

-- | An instance.
instance Monoid Link where
  mempty = defLink
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Link where
   attrSetGlobals pp bm = bm { _linkGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Link
  where attrSetAccessKey pp g = g { _linkGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Link
  where attrSetAnmval pp g = g { _linkGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Link
  where attrSetContentEditable pp g = g  { _linkGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Link
  where attrSetContextMenu pp g     = g { _linkGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasClass Link
  where attrSetClassName pp g           = g { _linkGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Link
  where attrSetDnmval pp g           = g { _linkGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasDir Link
  where attrSetDir pp g             = g { _linkGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Link
  where attrSetDraggable pp g       = g { _linkGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Link
  where attrSetHidden pp g          = g { _linkGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasId Link
  where attrSetId pp g              = g { _linkGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasLang Link
  where attrSetLang pp g            = g { _linkGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasRole Link
  where attrSetRole pp g            = g { _linkGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Link
  where attrSetSlot pp g            = g { _linkGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Link
  where attrSetSpellCheck pp g      = g { _linkGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Link
  where attrSetStyle pp g           = g { _linkGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Link
  where attrSetTabIndex pp g        = g { _linkGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Link
  where attrSetTitle pp g           = g { _linkGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_linkGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Link
  where attrSetTranslate pp g       = g { _linkGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_linkGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Link where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_linkGlobals g)

-- | An instance.
instance A.AttrHasCrossOrigin Link where attrSetCrossOrigin pp g = g {_linkCrossOrigin = Just pp }
-- | An instance.
instance A.AttrHasHref Link where attrSetHref pp g = g {_linkHref = Just pp }
-- | An instance.
instance A.AttrHasHrefLang Link where attrSetHrefLang pp g = g {_linkHrefLang = Just pp }
-- | An instance.
instance A.AttrHasIntegrity Link where attrSetIntegrity pp g = g { _linkIntegrity = Just pp}
-- | An instance.
instance A.AttrHasMedia Link where attrSetMedia pp g = g {_linkMedia = Just pp }
-- | An instance.
instance A.AttrHasNonce Link where attrSetNonce pp g = g {_linkNonce = Just pp }
-- | An instance.
instance A.AttrHasReferrerPolicy Link where attrSetReferrerPolicy pp g = g {_linkReferrerPolicy = Just pp }
-- | An instance.
instance A.AttrHasRel Link where attrSetRel pp g = g {_linkRel = Just pp }
-- | An instance.
instance A.AttrHasMediaType Link where attrSetMediaType pp g = g {_linkMediaType = Just pp }
-- | An instance.
instance A.AttrHasSizes Link where attrSetSizes pp g = g {_linkSizes = Just pp }

-- | An instance.
instance A.AttrHasCustom Link where attrSetCustom pp g       = g { _linkCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"link\" ... @
link' ∷ forall t m a. DomBuilder t m ⇒ Link → m a → m (Element EventResult (DomBuilderSpace m) t, a)
link' bm  = elAttr' "link" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"link\" ... @
link ∷ forall t m a. DomBuilder t m ⇒ Link → m a → m a
link bm children = snd <$> link' bm children

-- | A short-hand notion for @ el\' \"link\" ... @
linkN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
linkN' = el' "link"

-- | A short-hand notion for @ el \"link\" ... @
linkN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
linkN children = snd <$> linkN' children

-- | A short-hand notion for @ elDynAttr\' \"link\" ... @
linkD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Link → m a → m (Element EventResult (DomBuilderSpace m) t, a)
linkD' bm  = elDynAttr' "link" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"link\" ... @
linkD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Link → m a → m a
linkD bm children = snd <$> linkD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Meta-element
data Meta = Meta
  { _metaGlobals   ∷ Maybe A.Globals
  , _metaCharSet   ∷ Maybe A.CharSet
  , _metaContent   ∷ Maybe A.Content
  , _metaHttpEquiv ∷ Maybe A.HttpEquiv
  , _metaName      ∷ Maybe A.Name
  , _metaCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Meta where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _metaGlobals bm
    , A.attrMap <$> _metaCharSet bm
    , A.attrMap <$> _metaContent bm
    , A.attrMap <$> _metaHttpEquiv bm
    , A.attrMap <$> _metaName bm
    ] <> maybeToList (_metaCustom bm)

-- | A default value for Meta.
defMeta ∷ Meta
defMeta = Meta Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Meta where
  (<>) (Meta a1 a2 a3 a4 a5 a6) (Meta b1 b2 b3 b4 b5 b6)
    = Meta (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

-- | An instance.
instance Monoid Meta where
  mempty = defMeta
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Meta where
   attrSetGlobals pp bm = bm { _metaGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Meta
  where attrSetAccessKey pp g = g { _metaGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Meta
  where attrSetAnmval pp g = g { _metaGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Meta
  where attrSetContentEditable pp g = g  { _metaGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Meta
  where attrSetContextMenu pp g     = g { _metaGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasClass Meta
  where attrSetClassName pp g           = g { _metaGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Meta
  where attrSetDnmval pp g           = g { _metaGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasDir Meta
  where attrSetDir pp g             = g { _metaGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Meta
  where attrSetDraggable pp g       = g { _metaGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Meta
  where attrSetHidden pp g          = g { _metaGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasId Meta
  where attrSetId pp g              = g { _metaGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasLang Meta
  where attrSetLang pp g            = g { _metaGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasRole Meta
  where attrSetRole pp g            = g { _metaGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Meta
  where attrSetSlot pp g            = g { _metaGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Meta
  where attrSetSpellCheck pp g      = g { _metaGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Meta
  where attrSetStyle pp g           = g { _metaGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Meta
  where attrSetTabIndex pp g        = g { _metaGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Meta
  where attrSetTitle pp g           = g { _metaGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_metaGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Meta
  where attrSetTranslate pp g       = g { _metaGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_metaGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Meta where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_metaGlobals g)

-- | An instance.
instance A.AttrHasCharSet Meta where attrSetCharSet pp g = g {_metaCharSet = Just pp }
-- | An instance.
instance A.AttrHasContent Meta where attrSetContent pp g = g {_metaContent = Just pp }
-- | An instance.
instance A.AttrHasHttpEquiv Meta where attrSetHttpEquiv pp g = g {_metaHttpEquiv = Just pp }
-- | An instance.
instance A.AttrHasName Meta where attrSetName pp g = g {_metaName = Just pp }

-- | An instance.
instance A.AttrHasCustom Meta where attrSetCustom pp g       = g { _metaCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"meta\" ... @
meta' ∷ forall t m a. DomBuilder t m ⇒ Meta → m a → m (Element EventResult (DomBuilderSpace m) t, a)
meta' bm  = elAttr' "meta" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"meta\" ... @
meta ∷ forall t m a. DomBuilder t m ⇒ Meta → m a → m a
meta bm children = snd <$> meta' bm children

-- | A short-hand notion for @ el\' \"meta\" ... @
metaN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
metaN' = el' "meta"

-- | A short-hand notion for @ el \"meta\" ... @
metaN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
metaN children = snd <$> metaN' children

-- | A short-hand notion for @ elDynAttr\' \"meta\" ... @
metaD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Meta → m a → m (Element EventResult (DomBuilderSpace m) t, a)
metaD' bm  = elDynAttr' "meta" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"meta\" ... @
metaD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Meta → m a → m a
metaD bm children = snd <$> metaD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | NoScript-element has only the global attributes.
data NoScript = NoScript
  { _noScriptGlobals ∷ Maybe A.Globals
  , _noScriptCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap NoScript where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _noScriptGlobals bm
    ] <> maybeToList (_noScriptCustom bm)

-- | A default value for NoScript.
defNoScript ∷ NoScript
defNoScript = NoScript Nothing Nothing

-- | An instance.
instance Semigroup NoScript where
  (<>) (NoScript a1 a2) (NoScript b1 b2) = NoScript (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid NoScript where
  mempty = defNoScript
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals NoScript where
   attrSetGlobals pp bm = bm { _noScriptGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey NoScript
  where attrSetAccessKey pp g = g { _noScriptGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval NoScript
  where attrSetAnmval pp g = g { _noScriptGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable NoScript
  where attrSetContentEditable pp g = g  { _noScriptGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu NoScript
  where attrSetContextMenu pp g     = g { _noScriptGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasClass NoScript
  where attrSetClassName pp g           = g { _noScriptGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval NoScript
  where attrSetDnmval pp g           = g { _noScriptGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasDir NoScript
  where attrSetDir pp g             = g { _noScriptGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable NoScript
  where attrSetDraggable pp g       = g { _noScriptGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasHidden NoScript
  where attrSetHidden pp g          = g { _noScriptGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasId NoScript
  where attrSetId pp g              = g { _noScriptGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasLang NoScript
  where attrSetLang pp g            = g { _noScriptGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasRole NoScript
  where attrSetRole pp g            = g { _noScriptGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasSlot NoScript
  where attrSetSlot pp g            = g { _noScriptGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck NoScript
  where attrSetSpellCheck pp g      = g { _noScriptGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasStyle NoScript
  where attrSetStyle pp g           = g { _noScriptGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex NoScript
  where attrSetTabIndex pp g        = g { _noScriptGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasTitle NoScript
  where attrSetTitle pp g           = g { _noScriptGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate NoScript
  where attrSetTranslate pp g       = g { _noScriptGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_noScriptGlobals g))) }

-- | An instance.
instance A.AttrGetClassName NoScript where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_noScriptGlobals g)

-- | An instance.
instance A.AttrHasCustom NoScript where attrSetCustom pp g       = g { _noScriptCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"noscript\" ... @
noScript' ∷ forall t m a. DomBuilder t m ⇒ NoScript → m a → m (Element EventResult (DomBuilderSpace m) t, a)
noScript' bm  = elAttr' "noscript" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"noscript\" ... @
noScript ∷ forall t m a. DomBuilder t m ⇒ NoScript → m a → m a
noScript bm children = snd <$> noScript' bm children

-- | A short-hand notion for @ el\' \"noscript\" ... @
noScriptN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
noScriptN' = el' "noscript"

-- | A short-hand notion for @ el \"noscript\" ... @
noScriptN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
noScriptN children = snd <$> noScriptN' children

-- | A short-hand notion for @ elDynAttr\' \"noscript\" ... @
noScriptD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t NoScript → m a → m (Element EventResult (DomBuilderSpace m) t, a)
noScriptD' bm  = elDynAttr' "noscript" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"noscript\" ... @
noScriptD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t NoScript → m a → m a
noScriptD bm children = snd <$> noScriptD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Script-element
data Script = Script
  { _scriptGlobals     ∷ Maybe A.Globals
  , _scriptAsync       ∷ Maybe A.Async
  , _scriptCharSet     ∷ Maybe A.CharSet
  , _scriptCrossOrigin ∷ Maybe A.CrossOrigin
  , _scriptDefer       ∷ Maybe A.Defer
  , _scriptNonce       ∷ Maybe A.Nonce
  , _scriptSrc         ∷ Maybe A.Src
  , _scriptType        ∷ Maybe A.ScriptType
  , _scriptCustom      ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Script where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _scriptGlobals bm
    , A.attrMap <$> _scriptAsync bm
    , A.attrMap <$> _scriptCharSet bm
    , A.attrMap <$> _scriptCrossOrigin bm
    , A.attrMap <$> _scriptDefer bm
    , A.attrMap <$> _scriptNonce bm
    , A.attrMap <$> _scriptSrc bm
    , A.attrMap <$> _scriptType bm
    ] <> maybeToList (_scriptCustom bm)

-- | A default value for Script.
defScript ∷ Script
defScript = Script Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Script where
  (<>)
    (Script a1 a2 a3 a4 a5 a6 a7 a8 a9)
    (Script b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = Script (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

-- | An instance.
instance Monoid Script where
  mempty = defScript
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Script where
   attrSetGlobals pp bm = bm { _scriptGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Script
  where attrSetAccessKey pp g = g { _scriptGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Script
  where attrSetAnmval pp g = g { _scriptGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Script
  where attrSetContentEditable pp g = g  { _scriptGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Script
  where attrSetContextMenu pp g     = g { _scriptGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasClass Script
  where attrSetClassName pp g           = g { _scriptGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Script
  where attrSetDnmval pp g           = g { _scriptGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasDir Script
  where attrSetDir pp g             = g { _scriptGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Script
  where attrSetDraggable pp g       = g { _scriptGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Script
  where attrSetHidden pp g          = g { _scriptGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasId Script
  where attrSetId pp g              = g { _scriptGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasLang Script
  where attrSetLang pp g            = g { _scriptGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasRole Script
  where attrSetRole pp g            = g { _scriptGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Script
  where attrSetSlot pp g            = g { _scriptGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Script
  where attrSetSpellCheck pp g      = g { _scriptGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Script
  where attrSetStyle pp g           = g { _scriptGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Script
  where attrSetTabIndex pp g        = g { _scriptGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Script
  where attrSetTitle pp g           = g { _scriptGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_scriptGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Script
  where attrSetTranslate pp g       = g { _scriptGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_scriptGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Script where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_scriptGlobals g)

-- | An instance.
instance A.AttrHasAsync Script where attrSetAsync pp g = g {_scriptAsync = Just pp }
-- | An instance.
instance A.AttrHasCharSet Script where attrSetCharSet pp g = g {_scriptCharSet = Just pp }
-- | An instance.
instance A.AttrHasCrossOrigin Script where attrSetCrossOrigin pp g = g {_scriptCrossOrigin = Just pp }
-- | An instance.
instance A.AttrHasDefer Script where attrSetDefer pp g = g {_scriptDefer = Just pp }
-- | An instance.
instance A.AttrHasNonce Script where attrSetNonce pp g = g {_scriptNonce = Just pp }
-- | An instance.
instance A.AttrHasSrc Script where attrSetSrc pp g = g {_scriptSrc = Just pp }
-- | An instance.
instance A.AttrHasScriptType Script where attrSetScriptType pp g = g {_scriptType = Just pp }

-- | An instance.
instance A.AttrHasCustom Script where attrSetCustom pp g       = g { _scriptCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"script\" ... @
script' ∷ forall t m a. DomBuilder t m ⇒ Script → m a → m (Element EventResult (DomBuilderSpace m) t, a)
script' bm  = elAttr' "script" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"script\" ... @
script ∷ forall t m a. DomBuilder t m ⇒ Script → m a → m a
script bm children = snd <$> script' bm children

-- | A short-hand notion for @ el\' \"script\" ... @
scriptN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
scriptN' = el' "script"

-- | A short-hand notion for @ el \"script\" ... @
scriptN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
scriptN children = snd <$> scriptN' children

-- | A short-hand notion for @ elDynAttr\' \"script\" ... @
scriptD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Script → m a → m (Element EventResult (DomBuilderSpace m) t, a)
scriptD' bm  = elDynAttr' "script" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"script\" ... @
scriptD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Script → m a → m a
scriptD bm children = snd <$> scriptD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Style-element
data Style = Style
  { _styleGlobals   ∷ Maybe A.Globals
  , _styleMedia     ∷ Maybe A.Media
  , _styleNonce     ∷ Maybe A.Nonce
  , _styleMediaType ∷ Maybe A.MediaType
  , _styleCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Style where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _styleGlobals bm
    , A.attrMap <$> _styleMedia bm
    , A.attrMap <$> _styleNonce bm
    , A.attrMap <$> _styleMediaType bm
    ] <> maybeToList (_styleCustom bm)

-- | A default value for Style.
defStyle ∷ Style
defStyle = Style Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Style where
  (<>) (Style a1 a2 a3 a4 a5) (Style b1 b2 b3 b4 b5)
    = Style (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | An instance.
instance Monoid Style where
  mempty = defStyle
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Style where
   attrSetGlobals pp bm = bm { _styleGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Style
  where attrSetAccessKey pp g = g { _styleGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Style
  where attrSetAnmval pp g = g { _styleGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Style
  where attrSetContentEditable pp g = g  { _styleGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Style
  where attrSetContextMenu pp g     = g { _styleGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasClass Style
  where attrSetClassName pp g           = g { _styleGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Style
  where attrSetDnmval pp g           = g { _styleGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasDir Style
  where attrSetDir pp g             = g { _styleGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Style
  where attrSetDraggable pp g       = g { _styleGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Style
  where attrSetHidden pp g          = g { _styleGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasId Style
  where attrSetId pp g              = g { _styleGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasLang Style
  where attrSetLang pp g            = g { _styleGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasRole Style
  where attrSetRole pp g            = g { _styleGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Style
  where attrSetSlot pp g            = g { _styleGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Style
  where attrSetSpellCheck pp g      = g { _styleGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Style
  where attrSetStyle pp g           = g { _styleGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Style
  where attrSetTabIndex pp g        = g { _styleGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Style
  where attrSetTitle pp g           = g { _styleGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_styleGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Style
  where attrSetTranslate pp g       = g { _styleGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_styleGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Style where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_styleGlobals g)

-- | An instance.
instance A.AttrHasMedia Style where attrSetMedia pp g = g {_styleMedia = Just pp }
-- | An instance.
instance A.AttrHasNonce Style where attrSetNonce pp g = g {_styleNonce = Just pp }
-- | An instance.
instance A.AttrHasMediaType Style where attrSetMediaType pp g = g {_styleMediaType = Just pp }

-- | An instance.
instance A.AttrHasCustom Style where attrSetCustom pp g       = g { _styleCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"style\" ... @
style' ∷ forall t m a. DomBuilder t m ⇒ Style → m a → m (Element EventResult (DomBuilderSpace m) t, a)
style' bm  = elAttr' "style" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"style\" ... @
style ∷ forall t m a. DomBuilder t m ⇒ Style → m a → m a
style bm children = snd <$> style' bm children

-- | A short-hand notion for @ el\' \"style\" ... @
styleN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
styleN' = el' "style"

-- | A short-hand notion for @ el \"style\" ... @
styleN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
styleN children = snd <$> styleN' children

-- | A short-hand notion for @ elDynAttr\' \"style\" ... @
styleD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Style → m a → m (Element EventResult (DomBuilderSpace m) t, a)
styleD' bm  = elDynAttr' "style" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"style\" ... @
styleD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Style → m a → m a
styleD bm children = snd <$> styleD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Main-element has only the global attributes.
data Main = Main
  { _mainGlobals ∷ Maybe A.Globals
  , _mainCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Main where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _mainGlobals bm
    ] <> maybeToList (_mainCustom bm)

-- | A default value for Main.
defMain ∷ Main
defMain = Main Nothing Nothing

-- | An instance.
instance Semigroup Main where
  (<>) (Main a1 a2) (Main b1 b2) = Main (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Main where
  mempty = defMain
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Main where
   attrSetGlobals pp bm = bm { _mainGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Main
  where attrSetAccessKey pp g = g { _mainGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Main
  where attrSetAnmval pp g = g { _mainGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Main
  where attrSetContentEditable pp g = g  { _mainGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Main
  where attrSetContextMenu pp g     = g { _mainGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasClass Main
  where attrSetClassName pp g           = g { _mainGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Main
  where attrSetDnmval pp g           = g { _mainGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasDir Main
  where attrSetDir pp g             = g { _mainGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Main
  where attrSetDraggable pp g       = g { _mainGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Main
  where attrSetHidden pp g          = g { _mainGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasId Main
  where attrSetId pp g              = g { _mainGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasLang Main
  where attrSetLang pp g            = g { _mainGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasRole Main
  where attrSetRole pp g            = g { _mainGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Main
  where attrSetSlot pp g            = g { _mainGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Main
  where attrSetSpellCheck pp g      = g { _mainGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Main
  where attrSetStyle pp g           = g { _mainGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Main
  where attrSetTabIndex pp g        = g { _mainGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Main
  where attrSetTitle pp g           = g { _mainGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_mainGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Main
  where attrSetTranslate pp g       = g { _mainGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_mainGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Main where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_mainGlobals g)

-- | An instance.
instance A.AttrHasCustom Main where attrSetCustom pp g       = g { _mainCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"main\" ... @
main' ∷ forall t m a. DomBuilder t m ⇒ Main → m a → m (Element EventResult (DomBuilderSpace m) t, a)
main' bm  = elAttr' "main" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"main\" ... @
main ∷ forall t m a. DomBuilder t m ⇒ Main → m a → m a
main bm children = snd <$> main' bm children

-- | A short-hand notion for @ el\' \"main\" ... @
mainN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
mainN' = el' "main"

-- | A short-hand notion for @ el \"main\" ... @
mainN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
mainN children = snd <$> mainN' children

-- | A short-hand notion for @ elDynAttr\' \"main\" ... @
mainD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Main → m a → m (Element EventResult (DomBuilderSpace m) t, a)
mainD' bm  = elDynAttr' "main" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"main\" ... @
mainD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Main → m a → m a
mainD bm children = snd <$> mainD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Template-element has only the global attributes.
data Template = Template
  { _templateGlobals ∷ Maybe A.Globals
  , _templateCustom  ∷ Maybe A.Attr
  }

-- | An instance.
instance A.AttrMap Template where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _templateGlobals bm
    ] <> maybeToList (_templateCustom bm)

-- | A default value for Template.
defTemplate ∷ Template
defTemplate = Template Nothing Nothing

-- | An instance.
instance Semigroup Template where
  (<>) (Template a1 a2) (Template b1 b2) = Template (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Template where
  mempty = defTemplate
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Template where
   attrSetGlobals pp bm = bm { _templateGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Template
  where attrSetAccessKey pp g = g { _templateGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Template
  where attrSetAnmval pp g = g { _templateGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Template
  where attrSetContentEditable pp g = g  { _templateGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Template
  where attrSetContextMenu pp g     = g { _templateGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasClass Template
  where attrSetClassName pp g           = g { _templateGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Template
  where attrSetDnmval pp g           = g { _templateGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasDir Template
  where attrSetDir pp g             = g { _templateGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Template
  where attrSetDraggable pp g       = g { _templateGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Template
  where attrSetHidden pp g          = g { _templateGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasId Template
  where attrSetId pp g              = g { _templateGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasLang Template
  where attrSetLang pp g            = g { _templateGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasRole Template
  where attrSetRole pp g            = g { _templateGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Template
  where attrSetSlot pp g            = g { _templateGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Template
  where attrSetSpellCheck pp g      = g { _templateGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Template
  where attrSetStyle pp g           = g { _templateGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Template
  where attrSetTabIndex pp g        = g { _templateGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Template
  where attrSetTitle pp g           = g { _templateGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_templateGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Template
  where attrSetTranslate pp g       = g { _templateGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_templateGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Template where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_templateGlobals g)

-- | An instance.
instance A.AttrHasCustom Template where attrSetCustom pp g       = g { _templateCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"template\" ... @
template' ∷ forall t m a. DomBuilder t m ⇒ Template → m a → m (Element EventResult (DomBuilderSpace m) t, a)
template' bm  = elAttr' "template" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"template\" ... @
template ∷ forall t m a. DomBuilder t m ⇒ Template → m a → m a
template bm children = snd <$> template' bm children

-- | A short-hand notion for @ el\' \"template\" ... @
templateN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
templateN' = el' "template"

-- | A short-hand notion for @ el \"template\" ... @
templateN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
templateN children = snd <$> templateN' children

-- | A short-hand notion for @ elDynAttr\' \"template\" ... @
templateD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Template → m a → m (Element EventResult (DomBuilderSpace m) t, a)
templateD' bm  = elDynAttr' "template" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"template\" ... @
templateD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Template → m a → m a
templateD bm children = snd <$> templateD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Title-element has only the global attributes.
data Title = Title
  { _titleGlobals ∷ Maybe A.Globals
  , _titleCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Title where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _titleGlobals bm
    ] <> maybeToList (_titleCustom bm)

-- | A default value for Title.
defTitle ∷ Title
defTitle = Title Nothing Nothing

-- | An instance.
instance Semigroup Title where
  (<>) (Title a1 a2) (Title b1 b2) = Title (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Title where
  mempty = defTitle
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Title where
   attrSetGlobals pp bm = bm { _titleGlobals = Just pp }

-- Global attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Title
  where attrSetAccessKey pp g = g { _titleGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Title
  where attrSetAnmval pp g = g { _titleGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Title
  where attrSetContentEditable pp g = g  { _titleGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Title
  where attrSetContextMenu pp g     = g { _titleGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasClass Title
  where attrSetClassName pp g           = g { _titleGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Title
  where attrSetDnmval pp g           = g { _titleGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasDir Title
  where attrSetDir pp g             = g { _titleGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Title
  where attrSetDraggable pp g       = g { _titleGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Title
  where attrSetHidden pp g          = g { _titleGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasId Title
  where attrSetId pp g              = g { _titleGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasLang Title
  where attrSetLang pp g            = g { _titleGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasRole Title
  where attrSetRole pp g            = g { _titleGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Title
  where attrSetSlot pp g            = g { _titleGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Title
  where attrSetSpellCheck pp g      = g { _titleGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Title
  where attrSetStyle pp g           = g { _titleGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Title
  where attrSetTabIndex pp g        = g { _titleGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Title
  where attrSetTitle pp g           = g { _titleGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_titleGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Title
  where attrSetTranslate pp g       = g { _titleGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_titleGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Title where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_titleGlobals g)

-- | An instance.
instance A.AttrHasCustom Title where attrSetCustom pp g       = g { _titleCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"title\" ... @
title' ∷ forall t m a. DomBuilder t m ⇒ Title → m a → m (Element EventResult (DomBuilderSpace m) t, a)
title' bm  = elAttr' "title" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"title\" ... @
title ∷ forall t m a. DomBuilder t m ⇒ Title → m a → m a
title bm children = snd <$> title' bm children

-- | A short-hand notion for @ el\' \"title\" ... @
titleN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
titleN' = el' "title"

-- | A short-hand notion for @ el \"title\" ... @
titleN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
titleN children = snd <$> titleN' children

-- | A short-hand notion for @ elDynAttr\' \"title\" ... @
titleD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Title → m a → m (Element EventResult (DomBuilderSpace m) t, a)
titleD' bm  = elDynAttr' "title" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"title\" ... @
titleD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Title → m a → m a
titleD bm children = snd <$> titleD' bm children

