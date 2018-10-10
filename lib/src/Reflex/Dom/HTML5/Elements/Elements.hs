{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
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
there is a data-structure that contains the A.attributes the
element can have, and some functions to produce the element.

An example is easiest way to show how to use. Take a look of the other
examples as well as there are many of those.
@
dialogEx ∷ forall t m. MonadWidget t m ⇒ m ()
dialogEx = do
    E.h2N $ text "Dialog example"
    E.div (A.setClasses [A.ClassName "hmm"] $ A.id_ "myId" $ E.defDiv) $ do
        -- Note: Interactive elements have functions that end with C and
        -- CD, those return Click-events.
        evBtn ← E.buttonC E.defButton $ text "Open/Close dialog"
        -- We could use other events and the associated information as well:
        -- (evResBtn,_) ← E.button' E.defButton $ text "Open/Close dialog"
        -- let evBtn = domEvent Mouseup evResBtn
        rec
            dOpen ← foldDyn ($) False
                $ mergeWith (.) [ (\b → not b) <$ evBtn
                                , (\_ → False) <$ evCls
                                ]
            let dAttrs ∷ Dynamic t E.Dialog
                  = (\b →
                    if b
                       then E.defDialog
                        & A.title "Open title" & A.setClasses [A.ClassName "hmm"]
                        & A.open
                       else E.defDialog
                        & A.title "My closed title"
                    ) <$> dOpen
            evCls ← E.dialogD dAttrs $ do
                E.h2N $ text "Dialog-box"
                text "A user defined dialog (take a look of the attributes)"
                E.buttonC E.defButton $ text "Close dialog"
        pure ()
@

Naming convention:

    * abbr'  - takes A.attributes and returns 'm (El, a)'
    * abbr   - takes A.attributes and returns 'm a'
    * abbrN' - no A.attributes and returns 'm (El, a)'
    * abbrN  - no A.attributes and returns 'm a'
    * abbrD' - dynamic A.attributes and returns 'm (El, a)'
    * abbrD  - dynamic A.attributes and returns 'm a'


Element br has 'br_' function that uses default A.attributes
(that is, no A.attributes) and blank widget (that is, no children
elements). Other elements don't have '_'-ending functions.
Is there need for those?

Some of the elements can be found from the other element modules,
namely interactive, embedded, sectioning, tabular and meta-elements.

For a list of elements, see
[w3c element index](https://w3c.github.io/html/fullindex.html#index-elements).

-}

module Reflex.Dom.HTML5.Elements.Elements where

import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       PostBuild, DomBuilderSpace, elAttr',
                       elDynAttr', blank, el')
import qualified Reflex.Dom.HTML5.Attrs as A


------------------------------------------------------------------------------

-- | Abbr-element has only the global A.attributes.
data Abbr = Abbr
  { _abbrGlobals ∷ Maybe A.Globals
  , _abbrCustom  ∷ Maybe A.Attr
  }

-- | An instance.
instance A.AttrMap Abbr where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _abbrGlobals bm
    ] <> maybeToList (_abbrCustom bm)


-- | Default value for 'Abbr'.
defAbbr ∷ Abbr
defAbbr = Abbr Nothing Nothing

-- | An instance.
instance Semigroup Abbr where
  (<>) (Abbr a1 a2) (Abbr b1 b2) = Abbr (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Abbr where
  mempty = defAbbr
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Abbr where
   attrSetGlobals pp bm = bm { _abbrGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Abbr
  where attrSetAccessKey pp g = g { _abbrGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Abbr
  where attrSetAnmval pp g = g { _abbrGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Abbr
  where attrSetContentEditable pp g = g  { _abbrGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Abbr
  where attrSetContextMenu pp g     = g { _abbrGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasClass Abbr
  where attrSetClassName pp g           = g { _abbrGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Abbr
  where attrSetDnmval pp g           = g { _abbrGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasDir Abbr
  where attrSetDir pp g             = g { _abbrGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Abbr
  where attrSetDraggable pp g       = g { _abbrGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Abbr
  where attrSetHidden pp g          = g { _abbrGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasId Abbr
  where attrSetId pp g              = g { _abbrGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasLang Abbr
  where attrSetLang pp g            = g { _abbrGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasRole Abbr
  where attrSetRole pp g            = g { _abbrGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Abbr
  where attrSetSlot pp g            = g { _abbrGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Abbr
  where attrSetSpellCheck pp g      = g { _abbrGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Abbr
  where attrSetStyle pp g           = g { _abbrGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Abbr
  where attrSetTabIndex pp g        = g { _abbrGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Abbr
  where attrSetTitle pp g           = g { _abbrGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_abbrGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Abbr
  where attrSetTranslate pp g       = g { _abbrGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_abbrGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Abbr where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_abbrGlobals g)


-- | An instance.
instance A.AttrHasCustom Abbr where attrSetCustom pp g       = g { _abbrCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"abbr\" ... @
abbr' ∷ forall t m a. DomBuilder t m ⇒ Abbr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
abbr' bm = elAttr' "abbr" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"abbr\" ... @
abbr ∷ forall t m a. DomBuilder t m ⇒ Abbr → m a → m a
abbr bm children = snd <$> abbr' bm children

-- | A short-hand notion for @ el\' \"abbr\" ... @
abbrN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
abbrN' = el' "abbr"

-- | A short-hand notion for @ el \"abbr\" ... @
abbrN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
abbrN children = snd <$> abbrN' children

-- | A short-hand notion for @ elDynAttr\' \"abbr\" ... @
abbrD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Abbr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
abbrD' bm = elDynAttr' "abbr" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"abbr\" ... @
abbrD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Abbr → m a → m a
abbrD bm children = snd <$> abbrD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Address-element has only the global A.attributes.
data Address = Address
  { _addressGlobals ∷ Maybe A.Globals
  , _addressCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Address where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _addressGlobals bm
    ] <> maybeToList (_addressCustom bm)


defAddress ∷ Address
defAddress = Address Nothing Nothing

-- | An instance.
instance Semigroup Address where
  (<>) (Address a1 a2) (Address b1 b2) = Address (a1 <> b1) (a2 <> b2)


-- | An instance.
instance Monoid Address where
  mempty = defAddress
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Address where
   attrSetGlobals pp bm = bm { _addressGlobals = Just pp }

-- Global A.attributes require the following instances.

-- | An instance.
instance A.AttrHasAccessKey Address
  where attrSetAccessKey pp g = g { _addressGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Address
  where attrSetAnmval pp g = g { _addressGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Address
  where attrSetContentEditable pp g = g  { _addressGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Address
  where attrSetContextMenu pp g     = g { _addressGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasClass Address
  where attrSetClassName pp g           = g { _addressGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Address
  where attrSetDnmval pp g           = g { _addressGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasDir Address
  where attrSetDir pp g             = g { _addressGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Address
  where attrSetDraggable pp g       = g { _addressGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Address
  where attrSetHidden pp g          = g { _addressGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasId Address
  where attrSetId pp g              = g { _addressGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasLang Address
  where attrSetLang pp g            = g { _addressGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasRole Address
  where attrSetRole pp g            = g { _addressGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Address
  where attrSetSlot pp g            = g { _addressGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Address
  where attrSetSpellCheck pp g      = g { _addressGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Address
  where attrSetStyle pp g           = g { _addressGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Address
  where attrSetTabIndex pp g        = g { _addressGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Address
  where attrSetTitle pp g           = g { _addressGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_addressGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Address
  where attrSetTranslate pp g       = g { _addressGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_addressGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Address where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_addressGlobals g)

-- | An instance.
instance A.AttrHasCustom Address where attrSetCustom pp g       = g { _addressCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"address\" ... @
address' ∷ forall t m a. DomBuilder t m ⇒ Address → m a → m (Element EventResult (DomBuilderSpace m) t, a)
address' bm = elAttr' "address" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"address\" ... @
address ∷ forall t m a. DomBuilder t m ⇒ Address → m a → m a
address bm children = snd <$> address' bm children

-- | A short-hand notion for @ el\' \"address\" ... @
addressN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
addressN' = el' "address"

-- | A short-hand notion for @ el \"address\" ... @
addressN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
addressN children = snd <$> addressN' children

-- | A short-hand notion for @ elDynAttr\' \"address\" ... @
addressD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Address → m a → m (Element EventResult (DomBuilderSpace m) t, a)
addressD' bm = elDynAttr' "address" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"address\" ... @
addressD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Address → m a → m a
addressD bm children = snd <$> addressD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | B-element has only the global A.attributes.
data B = B
  { _bGlobals ∷ Maybe A.Globals
  , _bCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap B where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _bGlobals bm
    ] <> maybeToList (_bCustom bm)

-- | A default value for B.
defB ∷ B
defB  = B Nothing Nothing

-- | An instance.
instance Semigroup B where
  (<>) (B a1 a2) (B b1 b2) = B (a1 <> b1) (a2 <> b2)


-- | An instance.
instance Monoid B where
  mempty = defB
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals B where
   attrSetGlobals pp bm = bm { _bGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey B
  where attrSetAccessKey pp g = g { _bGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval B
  where attrSetAnmval pp g = g { _bGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable B
  where attrSetContentEditable pp g = g  { _bGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu B
  where attrSetContextMenu pp g     = g { _bGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasClass B
  where attrSetClassName pp g           = g { _bGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval B
  where attrSetDnmval pp g           = g { _bGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasDir B
  where attrSetDir pp g             = g { _bGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable B
  where attrSetDraggable pp g       = g { _bGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasHidden B
  where attrSetHidden pp g          = g { _bGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasId B
  where attrSetId pp g              = g { _bGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasLang B
  where attrSetLang pp g            = g { _bGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasRole B
  where attrSetRole pp g            = g { _bGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasSlot B
  where attrSetSlot pp g            = g { _bGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck B
  where attrSetSpellCheck pp g      = g { _bGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasStyle B
  where attrSetStyle pp g           = g { _bGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex B
  where attrSetTabIndex pp g        = g { _bGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasTitle B
  where attrSetTitle pp g           = g { _bGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_bGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate B
  where attrSetTranslate pp g       = g { _bGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_bGlobals g))) }

-- | An instance.
instance A.AttrGetClassName B where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_bGlobals g)

-- | An instance.
instance A.AttrHasCustom B where attrSetCustom pp g       = g { _bCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"b\" ... @
b' ∷ forall t m a. DomBuilder t m ⇒ B → m a → m (Element EventResult (DomBuilderSpace m) t, a)
b' bm = elAttr' "b" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"b\" ... @
b ∷ forall t m a. DomBuilder t m ⇒ B → m a → m a
b bm children = snd <$> b' bm children

-- | A short-hand notion for @ el\' \"bm\" ... @
bN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
bN' = el' "bm"

-- | A short-hand notion for @ el \"bm\" ... @
bN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
bN children = snd <$> bN' children

-- | A short-hand notion for @ elDynAttr\' \"bm\" ... @
bD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t B → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bD' bm = elDynAttr' "bm" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"bm\" ... @
bD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t B → m a → m a
bD bm children = snd <$> bD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Bdi-element has only the global A.attributes.
data Bdi = Bdi
  { _bdiGlobals ∷ Maybe A.Globals
  , _bdiCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Bdi where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _bdiGlobals bm
    ] <> maybeToList (_bdiCustom bm)

-- | A default value for Bdi.
defBdi ∷ Bdi
defBdi = Bdi Nothing Nothing

-- | An instance.
instance Semigroup Bdi where
  (<>) (Bdi a1 a2) (Bdi b1 b2) = Bdi (a1 <> b1) (a2 <> b2)


-- | An instance.
instance Monoid Bdi where
  mempty = defBdi
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Bdi where
   attrSetGlobals pp bm = bm { _bdiGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Bdi
  where attrSetAccessKey pp g = g { _bdiGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Bdi
  where attrSetAnmval pp g = g { _bdiGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Bdi
  where attrSetContentEditable pp g = g  { _bdiGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Bdi
  where attrSetContextMenu pp g     = g { _bdiGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasClass Bdi
  where attrSetClassName pp g           = g { _bdiGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Bdi
  where attrSetDnmval pp g           = g { _bdiGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasDir Bdi
  where attrSetDir pp g             = g { _bdiGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Bdi
  where attrSetDraggable pp g       = g { _bdiGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Bdi
  where attrSetHidden pp g          = g { _bdiGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasId Bdi
  where attrSetId pp g              = g { _bdiGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasLang Bdi
  where attrSetLang pp g            = g { _bdiGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasRole Bdi
  where attrSetRole pp g            = g { _bdiGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Bdi
  where attrSetSlot pp g            = g { _bdiGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Bdi
  where attrSetSpellCheck pp g      = g { _bdiGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Bdi
  where attrSetStyle pp g           = g { _bdiGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Bdi
  where attrSetTabIndex pp g        = g { _bdiGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Bdi
  where attrSetTitle pp g           = g { _bdiGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_bdiGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Bdi
  where attrSetTranslate pp g       = g { _bdiGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_bdiGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Bdi where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_bdiGlobals g)

-- | An instance.
instance A.AttrHasCustom Bdi where attrSetCustom pp g       = g { _bdiCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"bdi\" ... @
bdi' ∷ forall t m a. DomBuilder t m ⇒ Bdi → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdi' bm = elAttr' "bdi" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"bdi\" ... @
bdi ∷ forall t m a. DomBuilder t m ⇒ Bdi → m a → m a
bdi bm children = snd <$> bdi' bm children

-- | A short-hand notion for @ el\' \"bdi\" ... @
bdiN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdiN' = el' "bdi"

-- | A short-hand notion for @ el \"bdi\" ... @
bdiN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
bdiN children = snd <$> bdiN' children

-- | A short-hand notion for @ elDynAttr\' \"bdi\" ... @
bdiD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Bdi → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdiD' bm = elDynAttr' "bdi" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"bdi\" ... @
bdiD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Bdi → m a → m a
bdiD bm children = snd <$> bdiD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Bdo-element has only the global A.attributes.
data Bdo = Bdo
  { _bdoGlobals ∷ Maybe A.Globals
  , _bdoCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Bdo where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _bdoGlobals bm
    ] <> maybeToList (_bdoCustom bm)

-- | A default value for Bdo.
defBdo ∷ Bdo
defBdo = Bdo Nothing Nothing

-- | An instance.
instance Semigroup Bdo where
  (<>) (Bdo a1 a2) (Bdo b1 b2) = Bdo (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Bdo where
  mempty = defBdo
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Bdo where
   attrSetGlobals pp bm = bm { _bdoGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Bdo
  where attrSetAccessKey pp g = g { _bdoGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Bdo
  where attrSetAnmval pp g = g { _bdoGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Bdo
  where attrSetContentEditable pp g = g  { _bdoGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Bdo
  where attrSetContextMenu pp g     = g { _bdoGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasClass Bdo
  where attrSetClassName pp g           = g { _bdoGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Bdo
  where attrSetDnmval pp g           = g { _bdoGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasDir Bdo
  where attrSetDir pp g             = g { _bdoGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Bdo
  where attrSetDraggable pp g       = g { _bdoGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Bdo
  where attrSetHidden pp g          = g { _bdoGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasId Bdo
  where attrSetId pp g              = g { _bdoGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasLang Bdo
  where attrSetLang pp g            = g { _bdoGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasRole Bdo
  where attrSetRole pp g            = g { _bdoGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Bdo
  where attrSetSlot pp g            = g { _bdoGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Bdo
  where attrSetSpellCheck pp g      = g { _bdoGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Bdo
  where attrSetStyle pp g           = g { _bdoGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Bdo
  where attrSetTabIndex pp g        = g { _bdoGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Bdo
  where attrSetTitle pp g           = g { _bdoGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_bdoGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Bdo
  where attrSetTranslate pp g       = g { _bdoGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_bdoGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Bdo where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_bdoGlobals g)

-- | An instance.
instance A.AttrHasCustom Bdo where attrSetCustom pp g       = g { _bdoCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"bdo\" ... @
bdo' ∷ forall t m a. DomBuilder t m ⇒ Bdo → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdo' bm = elAttr' "bdo" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"bdo\" ... @
bdo ∷ forall t m a. DomBuilder t m ⇒ Bdo → m a → m a
bdo bm children = snd <$> bdo' bm children

-- | A short-hand notion for @ el\' \"bdo\" ... @
bdoN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdoN' = el' "bdo"

-- | A short-hand notion for @ el \"bdo\" ... @
bdoN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
bdoN children = snd <$> bdoN' children

-- | A short-hand notion for @ elDynAttr\' \"bdo\" ... @
bdoD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Bdo → m a → m (Element EventResult (DomBuilderSpace m) t, a)
bdoD' bm = elDynAttr' "bdo" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"bdo\" ... @
bdoD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Bdo → m a → m a
bdoD bm children = snd <$> bdoD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | BlockQuote-element
data BlockQuote = BlockQuote
  { _blockQuoteGlobals ∷ Maybe A.Globals
  , _blockQuoteCite    ∷ Maybe A.Cite
  , _blockQuoteCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap BlockQuote where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _blockQuoteGlobals bm
    , A.attrMap <$> _blockQuoteCite bm
    ] <> maybeToList (_blockQuoteCustom bm)

-- | A default value for BlockQuote.
defBlockQuote ∷ BlockQuote
defBlockQuote = BlockQuote Nothing Nothing Nothing

-- | An instance.
instance Semigroup BlockQuote where
  (<>) (BlockQuote a1 a2 a3) (BlockQuote b1 b2 b3)
    = BlockQuote (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid BlockQuote where
  mempty = defBlockQuote
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals BlockQuote where
   attrSetGlobals pp bm = bm { _blockQuoteGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey BlockQuote
  where attrSetAccessKey pp g = g { _blockQuoteGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval BlockQuote
  where attrSetAnmval pp g = g { _blockQuoteGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable BlockQuote
  where attrSetContentEditable pp g = g  { _blockQuoteGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu BlockQuote
  where attrSetContextMenu pp g     = g { _blockQuoteGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasClass BlockQuote
  where attrSetClassName pp g           = g { _blockQuoteGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval BlockQuote
  where attrSetDnmval pp g           = g { _blockQuoteGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasDir BlockQuote
  where attrSetDir pp g             = g { _blockQuoteGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable BlockQuote
  where attrSetDraggable pp g       = g { _blockQuoteGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasHidden BlockQuote
  where attrSetHidden pp g          = g { _blockQuoteGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasId BlockQuote
  where attrSetId pp g              = g { _blockQuoteGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasLang BlockQuote
  where attrSetLang pp g            = g { _blockQuoteGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasRole BlockQuote
  where attrSetRole pp g            = g { _blockQuoteGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasSlot BlockQuote
  where attrSetSlot pp g            = g { _blockQuoteGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck BlockQuote
  where attrSetSpellCheck pp g      = g { _blockQuoteGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasStyle BlockQuote
  where attrSetStyle pp g           = g { _blockQuoteGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex BlockQuote
  where attrSetTabIndex pp g        = g { _blockQuoteGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasTitle BlockQuote
  where attrSetTitle pp g           = g { _blockQuoteGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate BlockQuote
  where attrSetTranslate pp g       = g { _blockQuoteGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_blockQuoteGlobals g))) }

-- | An instance.
instance A.AttrGetClassName BlockQuote where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_blockQuoteGlobals g)

-- | An instance.
instance A.AttrHasCite BlockQuote where attrSetCite pp g = g {_blockQuoteCite = Just pp }

-- | An instance.
instance A.AttrHasCustom BlockQuote where attrSetCustom pp g       = g { _blockQuoteCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"blockquote\" ... @
blockQuote' ∷ forall t m a. DomBuilder t m ⇒ BlockQuote → m a → m (Element EventResult (DomBuilderSpace m) t, a)
blockQuote' bm  = elAttr' "blockquote" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"blockquote\" ... @
blockQuote ∷ forall t m a. DomBuilder t m ⇒ BlockQuote → m a → m a
blockQuote bm children = snd <$> blockQuote' bm children

-- | A short-hand notion for @ el\' \"blockquote\" ... @
blockQuoteN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
blockQuoteN' = el' "blockquote"

-- | A short-hand notion for @ el \"blockquote\" ... @
blockQuoteN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
blockQuoteN children = snd <$> blockQuoteN' children

-- | A short-hand notion for @ elDynAttr\' \"blockquote\" ... @
blockQuoteD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t BlockQuote → m a → m (Element EventResult (DomBuilderSpace m) t, a)
blockQuoteD' bm = elDynAttr' "blockquote" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"blockquote\" ... @
blockQuoteD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t BlockQuote → m a → m a
blockQuoteD bm children = snd <$> blockQuoteD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Br is non-conforming element in HTML 5.2 and according to it, should be not
-- used (use css instead). See obsolete features -section at W3.

-- | Br-element has only the global A.attributes.
data Br = Br
  { _brGlobals ∷ Maybe A.Globals
  , _brCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Br where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _brGlobals bm
    ] <> maybeToList (_brCustom bm)

-- | A default value for Br.
defBr ∷ Br
defBr = Br Nothing Nothing

-- | An instance.
instance Semigroup Br where
  (<>) (Br a1 a2) (Br b1 b2) = Br (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Br where
  mempty = defBr
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Br where
   attrSetGlobals pp bm = bm { _brGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Br
  where attrSetAccessKey pp g = g { _brGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Br
  where attrSetAnmval pp g = g { _brGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Br
  where attrSetContentEditable pp g = g  { _brGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Br
  where attrSetContextMenu pp g     = g { _brGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasClass Br
  where attrSetClassName pp g           = g { _brGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Br
  where attrSetDnmval pp g           = g { _brGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasDir Br
  where attrSetDir pp g             = g { _brGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Br
  where attrSetDraggable pp g       = g { _brGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Br
  where attrSetHidden pp g          = g { _brGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasId Br
  where attrSetId pp g              = g { _brGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasLang Br
  where attrSetLang pp g            = g { _brGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasRole Br
  where attrSetRole pp g            = g { _brGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Br
  where attrSetSlot pp g            = g { _brGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Br
  where attrSetSpellCheck pp g      = g { _brGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Br
  where attrSetStyle pp g           = g { _brGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Br
  where attrSetTabIndex pp g        = g { _brGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Br
  where attrSetTitle pp g           = g { _brGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_brGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Br
  where attrSetTranslate pp g       = g { _brGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_brGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Br where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_brGlobals g)

-- | An instance.
instance A.AttrHasCustom Br where attrSetCustom pp g       = g { _brCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"br\" ... @
br' ∷ forall t m a. DomBuilder t m ⇒ Br → m a → m (Element EventResult (DomBuilderSpace m) t, a)
br' bm = elAttr' "br" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"br\" ... @
br ∷ forall t m a. DomBuilder t m ⇒ Br → m a → m a
br bm children = snd <$> br' bm children

-- | A short-hand notion for @ el\' \"br\" ... @
brN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
brN' = el' "br"

-- | A short-hand notion for @ el \"br\" ... @
brN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
brN children = snd <$> brN' children

-- | A short-hand notion for @ elDynAttr\' \"br\" ... @
brD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Br → m a → m (Element EventResult (DomBuilderSpace m) t, a)
brD' bm = elDynAttr' "br" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"br\" ... @
brD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Br → m a → m a
brD bm children = snd <$> brD' bm children

br_ ∷ forall t m. DomBuilder t m ⇒ m ()
br_ = br defBr blank



------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- | Cite-element has only the global A.attributes.
data Cite = Cite
  { _citglobals ∷ Maybe A.Globals
  , _citcustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Cite where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _citglobals bm
    ] <> maybeToList (_citcustom bm)

-- | A default value for Cite.
defCite ∷ Cite
defCite = Cite Nothing Nothing

-- | An instance.
instance Semigroup Cite where
  (<>) (Cite a1 a2) (Cite b1 b2) = Cite (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Cite where
  mempty = defCite
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Cite where
   attrSetGlobals pp bm = bm { _citglobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Cite
  where attrSetAccessKey pp g = g { _citglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Cite
  where attrSetAnmval pp g = g { _citglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Cite
  where attrSetContentEditable pp g = g  { _citglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Cite
  where attrSetContextMenu pp g     = g { _citglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasClass Cite
  where attrSetClassName pp g           = g { _citglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Cite
  where attrSetDnmval pp g           = g { _citglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasDir Cite
  where attrSetDir pp g             = g { _citglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Cite
  where attrSetDraggable pp g       = g { _citglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasHidden Cite
  where attrSetHidden pp g          = g { _citglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasId Cite
  where attrSetId pp g              = g { _citglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasLang Cite
  where attrSetLang pp g            = g { _citglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasRole Cite
  where attrSetRole pp g            = g { _citglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasSlot Cite
  where attrSetSlot pp g            = g { _citglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Cite
  where attrSetSpellCheck pp g      = g { _citglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasStyle Cite
  where attrSetStyle pp g           = g { _citglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Cite
  where attrSetTabIndex pp g        = g { _citglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasTitle Cite
  where attrSetTitle pp g           = g { _citglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_citglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Cite
  where attrSetTranslate pp g       = g { _citglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_citglobals g))) }

-- | An instance.
instance A.AttrGetClassName Cite where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_citglobals g)

-- | An instance.
instance A.AttrHasCustom Cite where attrSetCustom pp g       = g { _citcustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"cite\" ... @
cite' ∷ forall t m a. DomBuilder t m ⇒ Cite → m a → m (Element EventResult (DomBuilderSpace m) t, a)
cite' bm  = elAttr' "cite" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"cite\" ... @
cite ∷ forall t m a. DomBuilder t m ⇒ Cite → m a → m a
cite bm children = snd <$> cite' bm children

-- | A short-hand notion for @ el\' \"cite\" ... @
citn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
citn' = el' "cite"

-- | A short-hand notion for @ el \"cite\" ... @
citn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
citn children = snd <$> citn' children

-- | A short-hand notion for @ elDynAttr\' \"cite\" ... @
citd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Cite → m a → m (Element EventResult (DomBuilderSpace m) t, a)
citd' bm  = elDynAttr' "cite" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"cite\" ... @
citd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Cite → m a → m a
citd bm children = snd <$> citd' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Code-element has only the global A.attributes.
data Code = Code
  { _codglobals ∷ Maybe A.Globals
  , _codcustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Code where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _codglobals bm
    ] <> maybeToList (_codcustom bm)

-- | A default value for Code.
defCode ∷ Code
defCode = Code Nothing Nothing

-- | An instance.
instance Semigroup Code where
  (<>) (Code a1 a2) (Code b1 b2) = Code (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Code where
  mempty = defCode
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Code where
   attrSetGlobals pp bm = bm { _codglobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Code
  where attrSetAccessKey pp g = g { _codglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Code
  where attrSetAnmval pp g = g { _codglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Code
  where attrSetContentEditable pp g = g  { _codglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Code
  where attrSetContextMenu pp g     = g { _codglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasClass Code
  where attrSetClassName pp g           = g { _codglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Code
  where attrSetDnmval pp g           = g { _codglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasDir Code
  where attrSetDir pp g             = g { _codglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Code
  where attrSetDraggable pp g       = g { _codglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasHidden Code
  where attrSetHidden pp g          = g { _codglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasId Code
  where attrSetId pp g              = g { _codglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasLang Code
  where attrSetLang pp g            = g { _codglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasRole Code
  where attrSetRole pp g            = g { _codglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasSlot Code
  where attrSetSlot pp g            = g { _codglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Code
  where attrSetSpellCheck pp g      = g { _codglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasStyle Code
  where attrSetStyle pp g           = g { _codglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Code
  where attrSetTabIndex pp g        = g { _codglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasTitle Code
  where attrSetTitle pp g           = g { _codglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_codglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Code
  where attrSetTranslate pp g       = g { _codglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_codglobals g))) }

-- | An instance.
instance A.AttrGetClassName Code where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_codglobals g)

-- | An instance.
instance A.AttrHasCustom Code where attrSetCustom pp g       = g { _codcustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"code\" ... @
code' ∷ forall t m a. DomBuilder t m ⇒ Code → m a → m (Element EventResult (DomBuilderSpace m) t, a)
code' bm  = elAttr' "code" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"code\" ... @
code ∷ forall t m a. DomBuilder t m ⇒ Code → m a → m a
code bm children = snd <$> code' bm children

-- | A short-hand notion for @ el\' \"code\" ... @
codn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
codn' = el' "code"

-- | A short-hand notion for @ el \"code\" ... @
codn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
codn children = snd <$> codn' children

-- | A short-hand notion for @ elDynAttr\' \"code\" ... @
codd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Code → m a → m (Element EventResult (DomBuilderSpace m) t, a)
codd' bm  = elDynAttr' "code" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"code\" ... @
codd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Code → m a → m a
codd bm children = snd <$> codd' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Data-element
-- Is value-attribute ok here?
data Data = Data
  { _dataGlobals   ∷ Maybe A.Globals
  , _dataValueText ∷ Maybe A.ValueText
  , _dataCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Data where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dataGlobals bm
    , A.attrMap <$> _dataValueText bm
    ] <> maybeToList (_dataCustom bm)

-- | A default value for Data.
defData ∷ Data
defData = Data Nothing Nothing Nothing

-- | An instance.
instance Semigroup Data where
  (<>) (Data a1 a2 a3) (Data b1 b2 b3)
    = Data (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Data where
  mempty = defData
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Data where
   attrSetGlobals pp bm = bm { _dataGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Data
  where attrSetAccessKey pp g = g { _dataGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Data
  where attrSetAnmval pp g = g { _dataGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Data
  where attrSetContentEditable pp g = g  { _dataGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Data
  where attrSetContextMenu pp g     = g { _dataGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasClass Data
  where attrSetClassName pp g           = g { _dataGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Data
  where attrSetDnmval pp g           = g { _dataGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasDir Data
  where attrSetDir pp g             = g { _dataGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Data
  where attrSetDraggable pp g       = g { _dataGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Data
  where attrSetHidden pp g          = g { _dataGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasId Data
  where attrSetId pp g              = g { _dataGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasLang Data
  where attrSetLang pp g            = g { _dataGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasRole Data
  where attrSetRole pp g            = g { _dataGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Data
  where attrSetSlot pp g            = g { _dataGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Data
  where attrSetSpellCheck pp g      = g { _dataGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Data
  where attrSetStyle pp g           = g { _dataGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Data
  where attrSetTabIndex pp g        = g { _dataGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Data
  where attrSetTitle pp g           = g { _dataGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dataGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Data
  where attrSetTranslate pp g       = g { _dataGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dataGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Data where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dataGlobals g)

-- | An instance.
instance A.AttrHasValueText Data where attrSetValueText pp g = g {_dataValueText = Just pp }

-- | An instance.
instance A.AttrHasCustom Data where attrSetCustom pp g       = g { _dataCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"data\" ... @
data' ∷ forall t m a. DomBuilder t m ⇒ Data → m a → m (Element EventResult (DomBuilderSpace m) t, a)
data' bm  = elAttr' "data" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"data\" ... @
data_ ∷ forall t m a. DomBuilder t m ⇒ Data → m a → m a
data_ bm children = snd <$> data' bm children

-- | A short-hand notion for @ el\' \"data\" ... @
dataN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dataN' = el' "data"

-- | A short-hand notion for @ el \"data\" ... @
dataN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dataN children = snd <$> dataN' children

-- | A short-hand notion for @ elDynAttr\' \"data\" ... @
dataD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Data → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dataD' bm = elDynAttr' "data" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"data\" ... @
dataD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Data → m a → m a
dataD bm children = snd <$> dataD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | DataList-element has only the global A.attributes.
data DataList = DataList
  { _dataListGlobals ∷ Maybe A.Globals
  , _dataListCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap DataList where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dataListGlobals bm
    ] <> maybeToList (_dataListCustom bm)

-- | A default value for DataList.
defDataList ∷ DataList
defDataList = DataList Nothing Nothing

-- | An instance.
instance Semigroup DataList where
  (<>) (DataList a1 a2) (DataList b1 b2) = DataList (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid DataList where
  mempty = defDataList
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals DataList where
   attrSetGlobals pp bm = bm { _dataListGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey DataList
  where attrSetAccessKey pp g = g { _dataListGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval DataList
  where attrSetAnmval pp g = g { _dataListGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable DataList
  where attrSetContentEditable pp g = g  { _dataListGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu DataList
  where attrSetContextMenu pp g     = g { _dataListGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasClass DataList
  where attrSetClassName pp g           = g { _dataListGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval DataList
  where attrSetDnmval pp g           = g { _dataListGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasDir DataList
  where attrSetDir pp g             = g { _dataListGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable DataList
  where attrSetDraggable pp g       = g { _dataListGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasHidden DataList
  where attrSetHidden pp g          = g { _dataListGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasId DataList
  where attrSetId pp g              = g { _dataListGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasLang DataList
  where attrSetLang pp g            = g { _dataListGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasRole DataList
  where attrSetRole pp g            = g { _dataListGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasSlot DataList
  where attrSetSlot pp g            = g { _dataListGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck DataList
  where attrSetSpellCheck pp g      = g { _dataListGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasStyle DataList
  where attrSetStyle pp g           = g { _dataListGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex DataList
  where attrSetTabIndex pp g        = g { _dataListGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasTitle DataList
  where attrSetTitle pp g           = g { _dataListGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dataListGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate DataList
  where attrSetTranslate pp g       = g { _dataListGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dataListGlobals g))) }

-- | An instance.
instance A.AttrGetClassName DataList where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dataListGlobals g)

-- | An instance.
instance A.AttrHasCustom DataList where attrSetCustom pp g       = g { _dataListCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"datalist\" ... @
dataList' ∷ forall t m a. DomBuilder t m ⇒ DataList → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dataList' bm  = elAttr' "datalist" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"datalist\" ... @
dataList ∷ forall t m a. DomBuilder t m ⇒ DataList → m a → m a
dataList bm children = snd <$> dataList' bm children

-- | A short-hand notion for @ el\' \"datalist\" ... @
dataListN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dataListN' = el' "datalist"

-- | A short-hand notion for @ el \"datalist\" ... @
dataListN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dataListN children = snd <$> dataListN' children

-- | A short-hand notion for @ elDynAttr\' \"datalist\" ... @
dataListD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t DataList → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dataListD' bm  = elDynAttr' "datalist" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"datalist\" ... @
dataListD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t DataList → m a → m a
dataListD bm children = snd <$> dataListD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Del-element
data Del = Del
  { _delGlobals  ∷ Maybe A.Globals
  , _delCite     ∷ Maybe A.Cite
  , _delDateTime ∷ Maybe A.DateTime
  , _delCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Del where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _delGlobals bm
    , A.attrMap <$> _delCite bm
    , A.attrMap <$> _delDateTime bm
    ] <> maybeToList (_delCustom bm)

-- | A default value Del.
defDel ∷  Del
defDel = Del Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Del where
  (<>) (Del a1 a2 a3 a4) (Del b1 b2 b3 b4)
    = Del (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Del where
  mempty = defDel
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Del where
   attrSetGlobals pp bm = bm { _delGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Del
  where attrSetAccessKey pp g = g { _delGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Del
  where attrSetAnmval pp g = g { _delGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Del
  where attrSetContentEditable pp g = g  { _delGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Del
  where attrSetContextMenu pp g     = g { _delGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasClass Del
  where attrSetClassName pp g           = g { _delGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Del
  where attrSetDnmval pp g           = g { _delGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasDir Del
  where attrSetDir pp g             = g { _delGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Del
  where attrSetDraggable pp g       = g { _delGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Del
  where attrSetHidden pp g          = g { _delGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasId Del
  where attrSetId pp g              = g { _delGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasLang Del
  where attrSetLang pp g            = g { _delGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasRole Del
  where attrSetRole pp g            = g { _delGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Del
  where attrSetSlot pp g            = g { _delGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Del
  where attrSetSpellCheck pp g      = g { _delGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Del
  where attrSetStyle pp g           = g { _delGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Del
  where attrSetTabIndex pp g        = g { _delGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Del
  where attrSetTitle pp g           = g { _delGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_delGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Del
  where attrSetTranslate pp g       = g { _delGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_delGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Del where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_delGlobals g)

-- | An instance.
instance A.AttrHasCite Del where attrSetCite pp g = g {_delCite = Just pp }
-- | An instance.
instance A.AttrHasDateTime Del where attrSetDateTime pp g = g {_delDateTime = Just pp }

-- | An instance.
instance A.AttrHasCustom Del where attrSetCustom pp g       = g { _delCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"del\" ... @
del' ∷ forall t m a. DomBuilder t m ⇒ Del → m a → m (Element EventResult (DomBuilderSpace m) t, a)
del' bm  = elAttr' "del" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"del\" ... @
del ∷ forall t m a. DomBuilder t m ⇒ Del → m a → m a
del bm children = snd <$> del' bm children

-- | A short-hand notion for @ el\' \"del\" ... @
delN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
delN' = el' "del"

-- | A short-hand notion for @ el \"del\" ... @
delN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
delN children = snd <$> delN' children

-- | A short-hand notion for @ elDynAttr\' \"del\" ... @
delD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Del → m a → m (Element EventResult (DomBuilderSpace m) t, a)
delD' bm = elDynAttr' "del" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"del\" ... @
delD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Del → m a → m a
delD bm children = snd <$> delD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | DD-element has only the global A.attributes.
data Dd = Dd
  { _ddGlobals ∷ Maybe A.Globals
  , _ddCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Dd where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _ddGlobals bm
    ] <> maybeToList (_ddCustom bm)

-- | A default value Dd.
defDd ∷ Dd
defDd = Dd Nothing Nothing

-- | An instance.
instance Semigroup Dd where
  (<>) (Dd a1 a2) (Dd b1 b2) = Dd (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Dd where
  mempty = defDd
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Dd where
   attrSetGlobals pp bm = bm { _ddGlobals = Just pp }

-- Global A.attributes reqere the following instances.
-- | An instance.
instance A.AttrHasAccessKey Dd
  where attrSetAccessKey pp g = g { _ddGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Dd
  where attrSetAnmval pp g = g { _ddGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Dd
  where attrSetContentEditable pp g = g  { _ddGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Dd
  where attrSetContextMenu pp g     = g { _ddGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasClass Dd
  where attrSetClassName pp g           = g { _ddGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Dd
  where attrSetDnmval pp g           = g { _ddGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasDir Dd
  where attrSetDir pp g             = g { _ddGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Dd
  where attrSetDraggable pp g       = g { _ddGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Dd
  where attrSetHidden pp g          = g { _ddGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasId Dd
  where attrSetId pp g              = g { _ddGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasLang Dd
  where attrSetLang pp g            = g { _ddGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasRole Dd
  where attrSetRole pp g            = g { _ddGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Dd
  where attrSetSlot pp g            = g { _ddGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Dd
  where attrSetSpellCheck pp g      = g { _ddGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Dd
  where attrSetStyle pp g           = g { _ddGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Dd
  where attrSetTabIndex pp g        = g { _ddGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Dd
  where attrSetTitle pp g           = g { _ddGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_ddGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Dd
  where attrSetTranslate pp g       = g { _ddGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_ddGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Dd where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_ddGlobals g)

-- | An instance.
instance A.AttrHasCustom Dd where attrSetCustom pp g       = g { _ddCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"dd\" ... @
dd' ∷ forall t m a. DomBuilder t m ⇒ Dd → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dd' bm  = elAttr' "dd" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"dd\" ... @
dd ∷ forall t m a. DomBuilder t m ⇒ Dd → m a → m a
dd bm children = snd <$> dd' bm children

-- | A short-hand notion for @ el\' \"dd\" ... @
ddN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
ddN' = el' "dd"

-- | A short-hand notion for @ el \"dd\" ... @
ddN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
ddN children = snd <$> ddN' children

-- | A short-hand notion for @ elDynAttr\' \"dd\" ... @
ddD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dd → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ddD' bm  = elDynAttr' "dd" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"dd\" ... @
ddD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dd → m a → m a
ddD bm children = snd <$> ddD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dfn-element has only the global A.attributes.
data Dfn = Dfn
  { _dfnGlobals ∷ Maybe A.Globals
  , _dfnCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Dfn where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dfnGlobals bm
    ] <> maybeToList (_dfnCustom bm)

-- | A default value for Dfn.
defDfn ∷ Dfn
defDfn = Dfn Nothing Nothing

-- | An instance.
instance Semigroup Dfn where
  (<>) (Dfn a1 a2) (Dfn b1 b2) = Dfn (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Dfn where
  mempty = defDfn
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Dfn where
   attrSetGlobals pp bm = bm { _dfnGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Dfn
  where attrSetAccessKey pp g = g { _dfnGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Dfn
  where attrSetAnmval pp g = g { _dfnGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Dfn
  where attrSetContentEditable pp g = g  { _dfnGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Dfn
  where attrSetContextMenu pp g     = g { _dfnGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasClass Dfn
  where attrSetClassName pp g           = g { _dfnGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Dfn
  where attrSetDnmval pp g           = g { _dfnGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasDir Dfn
  where attrSetDir pp g             = g { _dfnGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Dfn
  where attrSetDraggable pp g       = g { _dfnGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Dfn
  where attrSetHidden pp g          = g { _dfnGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasId Dfn
  where attrSetId pp g              = g { _dfnGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasLang Dfn
  where attrSetLang pp g            = g { _dfnGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasRole Dfn
  where attrSetRole pp g            = g { _dfnGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Dfn
  where attrSetSlot pp g            = g { _dfnGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Dfn
  where attrSetSpellCheck pp g      = g { _dfnGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Dfn
  where attrSetStyle pp g           = g { _dfnGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Dfn
  where attrSetTabIndex pp g        = g { _dfnGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Dfn
  where attrSetTitle pp g           = g { _dfnGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dfnGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Dfn
  where attrSetTranslate pp g       = g { _dfnGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dfnGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Dfn where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dfnGlobals g)

-- | An instance.
instance A.AttrHasCustom Dfn where attrSetCustom pp g       = g { _dfnCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"dfn\" ... @
dfn' ∷ forall t m a. DomBuilder t m ⇒ Dfn → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dfn' bm  = elAttr' "dfn" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"dfn\" ... @
dfn ∷ forall t m a. DomBuilder t m ⇒ Dfn → m a → m a
dfn bm children = snd <$> dfn' bm children

-- | A short-hand notion for @ el\' \"dfn\" ... @
dfnN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dfnN' = el' "dfn"

-- | A short-hand notion for @ el \"dfn\" ... @
dfnN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dfnN children = snd <$> dfnN' children

-- | A short-hand notion for @ elDynAttr\' \"dfn\" ... @
dfnD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dfn → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dfnD' bm  = elDynAttr' "dfn" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"dfn\" ... @
dfnD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dfn → m a → m a
dfnD bm children = snd <$> dfnD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dialog-element
data Dialog = Dialog
  { _dialogGlobals ∷ Maybe A.Globals
  , _dialogOpen    ∷ Maybe A.Open
  , _dialogCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Dialog where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dialogGlobals bm
    , A.attrMap <$> _dialogOpen bm
    ] <> maybeToList (_dialogCustom bm)

-- | A default value for Dialog.
defDialog ∷ Dialog
defDialog = Dialog Nothing Nothing Nothing

-- | An instance.
instance Semigroup Dialog where
  (<>) (Dialog a1 a2 a3) (Dialog b1 b2 b3)
    = Dialog (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Dialog where
  mempty = defDialog
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Dialog where
   attrSetGlobals pp bm = bm { _dialogGlobals = Just pp }

-- | An instance.
-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Dialog
  where attrSetAccessKey pp g = g { _dialogGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Dialog
  where attrSetAnmval pp g = g { _dialogGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Dialog
  where attrSetContentEditable pp g = g  { _dialogGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Dialog
  where attrSetContextMenu pp g     = g { _dialogGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasClass Dialog
  where attrSetClassName pp g           = g { _dialogGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Dialog
  where attrSetDnmval pp g           = g { _dialogGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasDir Dialog
  where attrSetDir pp g             = g { _dialogGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Dialog
  where attrSetDraggable pp g       = g { _dialogGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Dialog
  where attrSetHidden pp g          = g { _dialogGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasId Dialog
  where attrSetId pp g              = g { _dialogGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasLang Dialog
  where attrSetLang pp g            = g { _dialogGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasRole Dialog
  where attrSetRole pp g            = g { _dialogGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Dialog
  where attrSetSlot pp g            = g { _dialogGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Dialog
  where attrSetSpellCheck pp g      = g { _dialogGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Dialog
  where attrSetStyle pp g           = g { _dialogGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Dialog
  where attrSetTabIndex pp g        = g { _dialogGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Dialog
  where attrSetTitle pp g           = g { _dialogGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dialogGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Dialog
  where attrSetTranslate pp g       = g { _dialogGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dialogGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Dialog where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dialogGlobals g)

-- | An instance.
instance A.AttrHasOpen Dialog where attrSetOpen pp g = g {_dialogOpen = Just pp }

-- | An instance.
instance A.AttrHasCustom Dialog where attrSetCustom pp g       = g { _dialogCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"dialog\" ... @
dialog' ∷ forall t m a. DomBuilder t m ⇒ Dialog → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dialog' bm  = elAttr' "dialog" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"dialog\" ... @
dialog ∷ forall t m a. DomBuilder t m ⇒ Dialog → m a → m a
dialog bm children = snd <$> dialog' bm children

-- | A short-hand notion for @ el\' \"dialog\" ... @
dialogN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dialogN' = el' "dialog"

-- | A short-hand notion for @ el \"dialog\" ... @
dialogN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dialogN children = snd <$> dialogN' children

-- | A short-hand notion for @ elDynAttr\' \"dialog\" ... @
dialogD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dialog → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dialogD' bm = elDynAttr' "dialog" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"dialog\" ... @
dialogD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dialog → m a → m a
dialogD bm children = snd <$> dialogD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Div-element has only the global A.attributes.
data Div = Div
  { _divGlobals ∷ Maybe A.Globals
  , _divCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Div where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _divGlobals bm
    ] <> maybeToList (_divCustom bm)

-- | A default value for Div.
defDiv ∷ Div
defDiv = Div Nothing Nothing

-- | An instance.
instance Semigroup Div where
  (<>) (Div a1 a2) (Div b1 b2) = Div (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Div where
  mempty = defDiv
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Div where
   attrSetGlobals pp bm = bm { _divGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Div
  where attrSetAccessKey pp g = g { _divGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Div
  where attrSetAnmval pp g = g { _divGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Div
  where attrSetContentEditable pp g = g  { _divGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Div
  where attrSetContextMenu pp g     = g { _divGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasClass Div
  where attrSetClassName pp g           = g { _divGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Div
  where attrSetDnmval pp g           = g { _divGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasDir Div
  where attrSetDir pp g             = g { _divGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Div
  where attrSetDraggable pp g       = g { _divGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Div
  where attrSetHidden pp g          = g { _divGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasId Div
  where attrSetId pp g              = g { _divGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasLang Div
  where attrSetLang pp g            = g { _divGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasRole Div
  where attrSetRole pp g            = g { _divGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Div
  where attrSetSlot pp g            = g { _divGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Div
  where attrSetSpellCheck pp g      = g { _divGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Div
  where attrSetStyle pp g           = g { _divGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Div
  where attrSetTabIndex pp g        = g { _divGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Div
  where attrSetTitle pp g           = g { _divGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_divGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Div
  where attrSetTranslate pp g       = g { _divGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_divGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Div where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_divGlobals g)

-- | An instance.
instance A.AttrHasCustom Div where attrSetCustom pp g       = g { _divCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"div\" ... @
div' ∷ forall t m a. DomBuilder t m ⇒ Div → m a → m (Element EventResult (DomBuilderSpace m) t, a)
div' bm  = elAttr' "div" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"div\" ... @
div ∷ forall t m a. DomBuilder t m ⇒ Div → m a → m a
div bm children = snd <$> div' bm children

-- | A short-hand notion for @ el\' \"div\" ... @
divN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
divN' = el' "div"

-- | A short-hand notion for @ el \"div\" ... @
divN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
divN children = snd <$> divN' children

-- | A short-hand notion for @ elDynAttr\' \"div\" ... @
divD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Div → m a → m (Element EventResult (DomBuilderSpace m) t, a)
divD' bm  = elDynAttr' "div" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"div\" ... @
divD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Div → m a → m a
divD bm children = snd <$> divD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dl-element has only the global A.attributes.
data Dl = Dl
  { _dlGlobals ∷ Maybe A.Globals
  , _dlCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Dl where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dlGlobals bm
    ] <> maybeToList (_dlCustom bm)

-- | A default value for Dl.
defDl ∷ Dl
defDl = Dl Nothing Nothing

-- | An instance.
instance Semigroup Dl where
  (<>) (Dl a1 a2) (Dl b1 b2) = Dl (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Dl where
  mempty = defDl
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Dl where
   attrSetGlobals pp bm = bm { _dlGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Dl
  where attrSetAccessKey pp g = g { _dlGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Dl
  where attrSetAnmval pp g = g { _dlGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Dl
  where attrSetContentEditable pp g = g  { _dlGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Dl
  where attrSetContextMenu pp g     = g { _dlGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasClass Dl
  where attrSetClassName pp g           = g { _dlGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Dl
  where attrSetDnmval pp g           = g { _dlGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasDir Dl
  where attrSetDir pp g             = g { _dlGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Dl
  where attrSetDraggable pp g       = g { _dlGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Dl
  where attrSetHidden pp g          = g { _dlGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasId Dl
  where attrSetId pp g              = g { _dlGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasLang Dl
  where attrSetLang pp g            = g { _dlGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasRole Dl
  where attrSetRole pp g            = g { _dlGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Dl
  where attrSetSlot pp g            = g { _dlGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Dl
  where attrSetSpellCheck pp g      = g { _dlGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Dl
  where attrSetStyle pp g           = g { _dlGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Dl
  where attrSetTabIndex pp g        = g { _dlGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Dl
  where attrSetTitle pp g           = g { _dlGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dlGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Dl
  where attrSetTranslate pp g       = g { _dlGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dlGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Dl where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dlGlobals g)

-- | An instance.
instance A.AttrHasCustom Dl where attrSetCustom pp g       = g { _dlCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"dl\" ... @
dl' ∷ forall t m a. DomBuilder t m ⇒ Dl → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dl' bm  = elAttr' "dl" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"dl\" ... @
dl ∷ forall t m a. DomBuilder t m ⇒ Dl → m a → m a
dl bm children = snd <$> dl' bm children

-- | A short-hand notion for @ el\' \"dl\" ... @
dlN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dlN' = el' "dl"

-- | A short-hand notion for @ el \"dl\" ... @
dlN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dlN children = snd <$> dlN' children

-- | A short-hand notion for @ elDynAttr\' \"dl\" ... @
dlD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dl → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dlD' bm  = elDynAttr' "dl" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"dl\" ... @
dlD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dl → m a → m a
dlD bm children = snd <$> dlD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Dt-element has only the global A.attributes.
data Dt = Dt
  { _dtGlobals ∷ Maybe A.Globals
  , _dtCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Dt where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _dtGlobals bm
    ] <> maybeToList (_dtCustom bm)

-- | A default value for Dt.
defDt ∷ Dt
defDt = Dt Nothing Nothing

-- | An instance.
instance Semigroup Dt where
  (<>) (Dt a1 a2) (Dt b1 b2) = Dt (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Dt where
  mempty = defDt
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Dt where
   attrSetGlobals pp bm = bm { _dtGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Dt
  where attrSetAccessKey pp g = g { _dtGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Dt
  where attrSetAnmval pp g = g { _dtGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Dt
  where attrSetContentEditable pp g = g  { _dtGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Dt
  where attrSetContextMenu pp g     = g { _dtGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasClass Dt
  where attrSetClassName pp g           = g { _dtGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Dt
  where attrSetDnmval pp g           = g { _dtGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasDir Dt
  where attrSetDir pp g             = g { _dtGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Dt
  where attrSetDraggable pp g       = g { _dtGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Dt
  where attrSetHidden pp g          = g { _dtGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasId Dt
  where attrSetId pp g              = g { _dtGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasLang Dt
  where attrSetLang pp g            = g { _dtGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasRole Dt
  where attrSetRole pp g            = g { _dtGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Dt
  where attrSetSlot pp g            = g { _dtGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Dt
  where attrSetSpellCheck pp g      = g { _dtGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Dt
  where attrSetStyle pp g           = g { _dtGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Dt
  where attrSetTabIndex pp g        = g { _dtGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Dt
  where attrSetTitle pp g           = g { _dtGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_dtGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Dt
  where attrSetTranslate pp g       = g { _dtGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_dtGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Dt where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_dtGlobals g)

-- | An instance.
instance A.AttrHasCustom Dt where attrSetCustom pp g       = g { _dtCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"dt\" ... @
dt' ∷ forall t m a. DomBuilder t m ⇒ Dt → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dt' bm  = elAttr' "dt" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"dt\" ... @
dt ∷ forall t m a. DomBuilder t m ⇒ Dt → m a → m a
dt bm children = snd <$> dt' bm children

-- | A short-hand notion for @ el\' \"dt\" ... @
dtN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
dtN' = el' "dt"

-- | A short-hand notion for @ el \"dt\" ... @
dtN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
dtN children = snd <$> dtN' children

-- | A short-hand notion for @ elDynAttr\' \"dt\" ... @
dtD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dt → m a → m (Element EventResult (DomBuilderSpace m) t, a)
dtD' bm  = elDynAttr' "dt" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"dt\" ... @
dtD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Dt → m a → m a
dtD bm children = snd <$> dtD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Em-element has only the global A.attributes.
data Em = Em
  { _emGlobals ∷ Maybe A.Globals
  , _emCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Em where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _emGlobals bm
    ] <> maybeToList (_emCustom bm)

-- | A default value for Em.
defEm ∷ Em
defEm = Em Nothing Nothing

-- | An instance.
instance Semigroup Em where
  (<>) (Em a1 a2) (Em b1 b2) = Em (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Em where
  mempty = defEm
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Em where
   attrSetGlobals pp bm = bm { _emGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Em
  where attrSetAccessKey pp g = g { _emGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Em
  where attrSetAnmval pp g = g { _emGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Em
  where attrSetContentEditable pp g = g  { _emGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Em
  where attrSetContextMenu pp g     = g { _emGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasClass Em
  where attrSetClassName pp g           = g { _emGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Em
  where attrSetDnmval pp g           = g { _emGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasDir Em
  where attrSetDir pp g             = g { _emGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Em
  where attrSetDraggable pp g       = g { _emGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Em
  where attrSetHidden pp g          = g { _emGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasId Em
  where attrSetId pp g              = g { _emGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasLang Em
  where attrSetLang pp g            = g { _emGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasRole Em
  where attrSetRole pp g            = g { _emGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Em
  where attrSetSlot pp g            = g { _emGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Em
  where attrSetSpellCheck pp g      = g { _emGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Em
  where attrSetStyle pp g           = g { _emGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Em
  where attrSetTabIndex pp g        = g { _emGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Em
  where attrSetTitle pp g           = g { _emGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_emGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Em
  where attrSetTranslate pp g       = g { _emGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_emGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Em where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_emGlobals g)

-- | An instance.
instance A.AttrHasCustom Em where attrSetCustom pp g       = g { _emCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"em\" ... @
em' ∷ forall t m a. DomBuilder t m ⇒ Em → m a → m (Element EventResult (DomBuilderSpace m) t, a)
em' bm  = elAttr' "em" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"em\" ... @
em ∷ forall t m a. DomBuilder t m ⇒ Em → m a → m a
em bm children = snd <$> em' bm children

-- | A short-hand notion for @ el\' \"em\" ... @
emN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
emN' = el' "em"

-- | A short-hand notion for @ el \"em\" ... @
emN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
emN children = snd <$> emN' children

-- | A short-hand notion for @ elDynAttr\' \"em\" ... @
emD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Em → m a → m (Element EventResult (DomBuilderSpace m) t, a)
emD' bm  = elDynAttr' "em" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"em\" ... @
emD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Em → m a → m a
emD bm children = snd <$> emD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | FieldSet-element
data FieldSet = FieldSet
  { _fieldSetGlobals  ∷ Maybe A.Globals
  , _fieldSetDisabled ∷ Maybe A.Disabled
  , _fieldSetForm     ∷ Maybe A.Form
  , _fieldSetName     ∷ Maybe A.Name
  , _fieldSetCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap FieldSet where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _fieldSetGlobals bm
    , A.attrMap <$> _fieldSetDisabled bm
    , A.attrMap <$> _fieldSetForm bm
    , A.attrMap <$> _fieldSetName bm
    ] <> maybeToList (_fieldSetCustom bm)

-- | A default value for FieldSet.
defFieldSet ∷ FieldSet
defFieldSet = FieldSet Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup FieldSet where
  (<>) (FieldSet a1 a2 a3 a4 a5) (FieldSet b1 b2 b3 b4 b5)
    = FieldSet (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | An instance.
instance Monoid FieldSet where
  mempty = defFieldSet
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals FieldSet where
   attrSetGlobals pp bm = bm { _fieldSetGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey FieldSet
  where attrSetAccessKey pp g = g { _fieldSetGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval FieldSet
  where attrSetAnmval pp g = g { _fieldSetGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable FieldSet
  where attrSetContentEditable pp g = g  { _fieldSetGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu FieldSet
  where attrSetContextMenu pp g     = g { _fieldSetGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasClass FieldSet
  where attrSetClassName pp g           = g { _fieldSetGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval FieldSet
  where attrSetDnmval pp g           = g { _fieldSetGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasDir FieldSet
  where attrSetDir pp g             = g { _fieldSetGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable FieldSet
  where attrSetDraggable pp g       = g { _fieldSetGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasHidden FieldSet
  where attrSetHidden pp g          = g { _fieldSetGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasId FieldSet
  where attrSetId pp g              = g { _fieldSetGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasLang FieldSet
  where attrSetLang pp g            = g { _fieldSetGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasRole FieldSet
  where attrSetRole pp g            = g { _fieldSetGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasSlot FieldSet
  where attrSetSlot pp g            = g { _fieldSetGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck FieldSet
  where attrSetSpellCheck pp g      = g { _fieldSetGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasStyle FieldSet
  where attrSetStyle pp g           = g { _fieldSetGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex FieldSet
  where attrSetTabIndex pp g        = g { _fieldSetGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasTitle FieldSet
  where attrSetTitle pp g           = g { _fieldSetGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate FieldSet
  where attrSetTranslate pp g       = g { _fieldSetGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_fieldSetGlobals g))) }

-- | An instance.
instance A.AttrGetClassName FieldSet where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_fieldSetGlobals g)

-- | An instance.
instance A.AttrHasDisabled FieldSet where attrSetDisabled pp g = g {_fieldSetDisabled = Just pp }
-- | An instance.
instance A.AttrHasForm FieldSet where attrSetForm pp g = g {_fieldSetForm = Just pp }
-- | An instance.
instance A.AttrHasName FieldSet where attrSetName pp g = g {_fieldSetName = Just pp }

-- | An instance.
instance A.AttrHasCustom FieldSet where attrSetCustom pp g       = g { _fieldSetCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"fieldset\" ... @
fieldSet' ∷ forall t m a. DomBuilder t m ⇒ FieldSet → m a → m (Element EventResult (DomBuilderSpace m) t, a)
fieldSet' bm  = elAttr' "fieldset" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"fieldset\" ... @
fieldSet ∷ forall t m a. DomBuilder t m ⇒ FieldSet → m a → m a
fieldSet bm children = snd <$> fieldSet' bm children

-- | A short-hand notion for @ el\' \"fieldset\" ... @
fieldSetN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
fieldSetN' = el' "fieldset"

-- | A short-hand notion for @ el \"fieldset\" ... @
fieldSetN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
fieldSetN children = snd <$> fieldSetN' children

-- | A short-hand notion for @ elDynAttr\' \"fieldset\" ... @
fieldSetD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t FieldSet → m a → m (Element EventResult (DomBuilderSpace m) t, a)
fieldSetD' bm = elDynAttr' "fieldset" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"fieldset\" ... @
fieldSetD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t FieldSet → m a → m a
fieldSetD bm children = snd <$> fieldSetD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | FigCaption-element has only the global A.attributes.
data FigCaption = FigCaption
  { _figCaptionGlobals ∷ Maybe A.Globals
  , _figCaptionCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap FigCaption where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _figCaptionGlobals bm
    ] <> maybeToList (_figCaptionCustom bm)

-- | A default value for FigCaption.
defFigCaption ∷ FigCaption
defFigCaption = FigCaption Nothing Nothing

-- | An instance.
instance Semigroup FigCaption where
  (<>) (FigCaption a1 a2) (FigCaption b1 b2)
    = FigCaption (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid FigCaption where
  mempty = defFigCaption
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals FigCaption where
   attrSetGlobals pp bm = bm { _figCaptionGlobals = Just pp }

-- | An instance.
-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey FigCaption
  where attrSetAccessKey pp g = g { _figCaptionGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval FigCaption
  where attrSetAnmval pp g = g { _figCaptionGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable FigCaption
  where attrSetContentEditable pp g = g  { _figCaptionGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu FigCaption
  where attrSetContextMenu pp g     = g { _figCaptionGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasClass FigCaption
  where attrSetClassName pp g           = g { _figCaptionGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval FigCaption
  where attrSetDnmval pp g           = g { _figCaptionGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasDir FigCaption
  where attrSetDir pp g             = g { _figCaptionGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable FigCaption
  where attrSetDraggable pp g       = g { _figCaptionGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasHidden FigCaption
  where attrSetHidden pp g          = g { _figCaptionGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasId FigCaption
  where attrSetId pp g              = g { _figCaptionGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasLang FigCaption
  where attrSetLang pp g            = g { _figCaptionGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasRole FigCaption
  where attrSetRole pp g            = g { _figCaptionGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasSlot FigCaption
  where attrSetSlot pp g            = g { _figCaptionGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck FigCaption
  where attrSetSpellCheck pp g      = g { _figCaptionGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasStyle FigCaption
  where attrSetStyle pp g           = g { _figCaptionGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex FigCaption
  where attrSetTabIndex pp g        = g { _figCaptionGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasTitle FigCaption
  where attrSetTitle pp g           = g { _figCaptionGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate FigCaption
  where attrSetTranslate pp g       = g { _figCaptionGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_figCaptionGlobals g))) }

-- | An instance.
instance A.AttrGetClassName FigCaption where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_figCaptionGlobals g)

-- | An instance.
instance A.AttrHasCustom FigCaption where attrSetCustom pp g       = g { _figCaptionCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"figcaption\" ... @
figCaption' ∷ forall t m a. DomBuilder t m ⇒ FigCaption → m a → m (Element EventResult (DomBuilderSpace m) t, a)
figCaption' bm  = elAttr' "figcaption" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"figcaption\" ... @
figCaption ∷ forall t m a. DomBuilder t m ⇒ FigCaption → m a → m a
figCaption bm children = snd <$> figCaption' bm children

-- | A short-hand notion for @ el\' \"figcaption\" ... @
figCaptionN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
figCaptionN' = el' "figcaption"

-- | A short-hand notion for @ el \"figcaption\" ... @
figCaptionN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
figCaptionN children = snd <$> figCaptionN' children

-- | A short-hand notion for @ elDynAttr\' \"figcaption\" ... @
figCaptionD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t FigCaption → m a → m (Element EventResult (DomBuilderSpace m) t, a)
figCaptionD' bm  = elDynAttr' "figcaption" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"figcaption\" ... @
figCaptionD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t FigCaption → m a → m a
figCaptionD bm children = snd <$> figCaptionD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Figure-element has only the global A.attributes.
data Figure = Figure
  { _figurglobals ∷ Maybe A.Globals
  , _figurcustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Figure where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _figurglobals bm
    ] <> maybeToList (_figurcustom bm)

-- | A default value for Figure.
defFigure ∷ Figure
defFigure = Figure Nothing Nothing

-- | An instance.
instance Semigroup Figure where
  (<>) (Figure a1 a2) (Figure b1 b2) = Figure (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Figure where
  mempty = defFigure
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Figure where
   attrSetGlobals pp bm = bm { _figurglobals = Just pp }

-- | An instance.
-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Figure
  where attrSetAccessKey pp g = g { _figurglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Figure
  where attrSetAnmval pp g = g { _figurglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Figure
  where attrSetContentEditable pp g = g  { _figurglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Figure
  where attrSetContextMenu pp g     = g { _figurglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasClass Figure
  where attrSetClassName pp g           = g { _figurglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Figure
  where attrSetDnmval pp g           = g { _figurglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasDir Figure
  where attrSetDir pp g             = g { _figurglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Figure
  where attrSetDraggable pp g       = g { _figurglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasHidden Figure
  where attrSetHidden pp g          = g { _figurglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasId Figure
  where attrSetId pp g              = g { _figurglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasLang Figure
  where attrSetLang pp g            = g { _figurglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasRole Figure
  where attrSetRole pp g            = g { _figurglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasSlot Figure
  where attrSetSlot pp g            = g { _figurglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Figure
  where attrSetSpellCheck pp g      = g { _figurglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasStyle Figure
  where attrSetStyle pp g           = g { _figurglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Figure
  where attrSetTabIndex pp g        = g { _figurglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasTitle Figure
  where attrSetTitle pp g           = g { _figurglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_figurglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Figure
  where attrSetTranslate pp g       = g { _figurglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_figurglobals g))) }

-- | An instance.
instance A.AttrGetClassName Figure where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_figurglobals g)

-- | An instance.
instance A.AttrHasCustom Figure where attrSetCustom pp g       = g { _figurcustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"figure\" ... @
figure' ∷ forall t m a. DomBuilder t m ⇒ Figure → m a → m (Element EventResult (DomBuilderSpace m) t, a)
figure' bm  = elAttr' "figure" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"figure\" ... @
figure ∷ forall t m a. DomBuilder t m ⇒ Figure → m a → m a
figure bm children = snd <$> figure' bm children

-- | A short-hand notion for @ el\' \"figure\" ... @
figurn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
figurn' = el' "figure"

-- | A short-hand notion for @ el \"figure\" ... @
figurn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
figurn children = snd <$> figurn' children

-- | A short-hand notion for @ elDynAttr\' \"figure\" ... @
figurd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Figure → m a → m (Element EventResult (DomBuilderSpace m) t, a)
figurd' bm  = elDynAttr' "figure" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"figure\" ... @
figurd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Figure → m a → m a
figurd bm children = snd <$> figurd' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Footer-element has only the global A.attributes.
data Footer = Footer
  { _footerGlobals ∷ Maybe A.Globals
  , _footerCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Footer where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _footerGlobals bm
    ] <> maybeToList (_footerCustom bm)

-- | A default value for Footer.
defFooter ∷ Footer
defFooter = Footer Nothing Nothing

-- | An instance.
instance Semigroup Footer where
  (<>) (Footer a1 a2) (Footer b1 b2) = Footer (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Footer where
  mempty = defFooter
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Footer where
   attrSetGlobals pp bm = bm { _footerGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Footer
  where attrSetAccessKey pp g = g { _footerGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Footer
  where attrSetAnmval pp g = g { _footerGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Footer
  where attrSetContentEditable pp g = g  { _footerGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Footer
  where attrSetContextMenu pp g     = g { _footerGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasClass Footer
  where attrSetClassName pp g           = g { _footerGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Footer
  where attrSetDnmval pp g           = g { _footerGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasDir Footer
  where attrSetDir pp g             = g { _footerGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Footer
  where attrSetDraggable pp g       = g { _footerGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Footer
  where attrSetHidden pp g          = g { _footerGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasId Footer
  where attrSetId pp g              = g { _footerGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasLang Footer
  where attrSetLang pp g            = g { _footerGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasRole Footer
  where attrSetRole pp g            = g { _footerGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Footer
  where attrSetSlot pp g            = g { _footerGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Footer
  where attrSetSpellCheck pp g      = g { _footerGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Footer
  where attrSetStyle pp g           = g { _footerGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Footer
  where attrSetTabIndex pp g        = g { _footerGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Footer
  where attrSetTitle pp g           = g { _footerGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_footerGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Footer
  where attrSetTranslate pp g       = g { _footerGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_footerGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Footer where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_footerGlobals g)

-- | An instance.
instance A.AttrHasCustom Footer where attrSetCustom pp g       = g { _footerCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"footer\" ... @
footer' ∷ forall t m a. DomBuilder t m ⇒ Footer → m a → m (Element EventResult (DomBuilderSpace m) t, a)
footer' bm  = elAttr' "footer" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"footer\" ... @
footer ∷ forall t m a. DomBuilder t m ⇒ Footer → m a → m a
footer bm children = snd <$> footer' bm children

-- | A short-hand notion for @ el\' \"footer\" ... @
footerN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
footerN' = el' "footer"

-- | A short-hand notion for @ el \"footer\" ... @
footerN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
footerN children = snd <$> footerN' children

-- | A short-hand notion for @ elDynAttr\' \"footer\" ... @
footerD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Footer → m a → m (Element EventResult (DomBuilderSpace m) t, a)
footerD' bm  = elDynAttr' "footer" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"footer\" ... @
footerD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Footer → m a → m a
footerD bm children = snd <$> footerD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Form-element
data Form = Form
  { _formGlobals       ∷ Maybe A.Globals
  , _formAcceptCharSet ∷ Maybe A.AcceptCharSet
  , _formAction        ∷ Maybe A.Action
  , _formAutoComplete  ∷ Maybe A.AutoComplete
  , _formEncType       ∷ Maybe A.EncType
  , _formMethod        ∷ Maybe A.Method
  , _formName          ∷ Maybe A.Name
  , _formNoValidate    ∷ Maybe A.NoValidate
  , _formTarget        ∷ Maybe A.Target
  , _formCustom        ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Form where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _formGlobals bm
    , A.attrMap <$> _formAcceptCharSet bm
    , A.attrMap <$> _formAction bm
    , A.attrMap <$> _formAutoComplete bm
    , A.attrMap <$> _formEncType bm
    , A.attrMap <$> _formMethod bm
    , A.attrMap <$> _formName bm
    , A.attrMap <$> _formNoValidate bm
    , A.attrMap <$> _formTarget bm
    ] <> maybeToList (_formCustom bm)

-- | A default value for Form.
defForm ∷ Form
defForm = Form Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Form where
  (<>) (Form a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    (Form b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
      = Form (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
              (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)

-- | An instance.
instance Monoid Form where
  mempty = defForm
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Form where
   attrSetGlobals pp bm = bm { _formGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Form
  where attrSetAccessKey pp g = g { _formGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Form
  where attrSetAnmval pp g = g { _formGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Form
  where attrSetContentEditable pp g = g  { _formGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Form
  where attrSetContextMenu pp g     = g { _formGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasClass Form
  where attrSetClassName pp g           = g { _formGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Form
  where attrSetDnmval pp g           = g { _formGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasDir Form
  where attrSetDir pp g             = g { _formGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Form
  where attrSetDraggable pp g       = g { _formGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Form
  where attrSetHidden pp g          = g { _formGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasId Form
  where attrSetId pp g              = g { _formGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasLang Form
  where attrSetLang pp g            = g { _formGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasRole Form
  where attrSetRole pp g            = g { _formGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Form
  where attrSetSlot pp g            = g { _formGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Form
  where attrSetSpellCheck pp g      = g { _formGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Form
  where attrSetStyle pp g           = g { _formGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Form
  where attrSetTabIndex pp g        = g { _formGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Form
  where attrSetTitle pp g           = g { _formGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_formGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Form
  where attrSetTranslate pp g       = g { _formGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_formGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Form where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_formGlobals g)

-- | An instance.
instance A.AttrHasAcceptCharSet Form where attrSetAcceptCharSet pp g = g {_formAcceptCharSet = Just pp }
-- | An instance.
instance A.AttrHasAction Form where attrSetAction pp g = g {_formAction = Just pp }
-- | An instance.
instance A.AttrHasAutoComplete Form where attrSetAutoComplete pp g = g {_formAutoComplete = Just pp }
-- | An instance.
instance A.AttrHasEncType Form where attrSetEncType pp g = g {_formEncType = Just pp }
-- | An instance.
instance A.AttrHasMethod Form where attrSetMethod pp g = g {_formMethod = Just pp }
-- | An instance.
instance A.AttrHasName Form where attrSetName pp g = g {_formName = Just pp }
-- | An instance.
instance A.AttrHasNoValidate Form where attrSetNoValidate pp g = g {_formNoValidate = Just pp }
-- | An instance.
instance A.AttrHasTarget Form where attrSetTarget pp g = g {_formTarget = Just pp }

-- | An instance.
instance A.AttrHasCustom Form where attrSetCustom pp g       = g { _formCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"form\" ... @
form' ∷ forall t m a. DomBuilder t m ⇒ Form → m a → m (Element EventResult (DomBuilderSpace m) t, a)
form' bm  = elAttr' "form" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"form\" ... @
form ∷ forall t m a. DomBuilder t m ⇒ Form → m a → m a
form bm children = snd <$> form' bm children

-- | A short-hand notion for @ el\' \"form\" ... @
formN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
formN' = el' "form"

-- | A short-hand notion for @ el \"form\" ... @
formN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
formN children = snd <$> formN' children

-- | A short-hand notion for @ elDynAttr\' \"form\" ... @
formD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Form → m a → m (Element EventResult (DomBuilderSpace m) t, a)
formD' bm = elDynAttr' "form" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"form\" ... @
formD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Form → m a → m a
formD bm children = snd <$> formD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Header-element has only the global A.attributes.
data Header = Header
  { _headerGlobals ∷ Maybe A.Globals
  , _headerCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Header where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _headerGlobals bm
    ] <> maybeToList (_headerCustom bm)

-- | A default value for Header.
defHeader ∷ Header
defHeader = Header Nothing Nothing

-- | An instance.
instance Semigroup Header where
  (<>) (Header a1 a2) (Header b1 b2) = Header (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Header where
  mempty = defHeader
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Header where
   attrSetGlobals pp bm = bm { _headerGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Header
  where attrSetAccessKey pp g = g { _headerGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Header
  where attrSetAnmval pp g = g { _headerGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Header
  where attrSetContentEditable pp g = g  { _headerGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Header
  where attrSetContextMenu pp g     = g { _headerGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasClass Header
  where attrSetClassName pp g           = g { _headerGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Header
  where attrSetDnmval pp g           = g { _headerGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasDir Header
  where attrSetDir pp g             = g { _headerGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Header
  where attrSetDraggable pp g       = g { _headerGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Header
  where attrSetHidden pp g          = g { _headerGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasId Header
  where attrSetId pp g              = g { _headerGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasLang Header
  where attrSetLang pp g            = g { _headerGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasRole Header
  where attrSetRole pp g            = g { _headerGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Header
  where attrSetSlot pp g            = g { _headerGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Header
  where attrSetSpellCheck pp g      = g { _headerGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Header
  where attrSetStyle pp g           = g { _headerGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Header
  where attrSetTabIndex pp g        = g { _headerGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Header
  where attrSetTitle pp g           = g { _headerGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_headerGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Header
  where attrSetTranslate pp g       = g { _headerGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_headerGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Header where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_headerGlobals g)

-- | An instance.
instance A.AttrHasCustom Header where attrSetCustom pp g       = g { _headerCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"header\" ... @
header' ∷ forall t m a. DomBuilder t m ⇒ Header → m a → m (Element EventResult (DomBuilderSpace m) t, a)
header' bm  = elAttr' "header" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"header\" ... @
header ∷ forall t m a. DomBuilder t m ⇒ Header → m a → m a
header bm children = snd <$> header' bm children

-- | A short-hand notion for @ el\' \"header\" ... @
headerN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
headerN' = el' "header"

-- | A short-hand notion for @ el \"header\" ... @
headerN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
headerN children = snd <$> headerN' children

-- | A short-hand notion for @ elDynAttr\' \"header\" ... @
headerD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Header → m a → m (Element EventResult (DomBuilderSpace m) t, a)
headerD' bm  = elDynAttr' "header" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"header\" ... @
headerD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Header → m a → m a
headerD bm children = snd <$> headerD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Hr-element has only the global A.attributes.
data Hr = Hr
  { _hrGlobals ∷ Maybe A.Globals
  , _hrCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Hr where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _hrGlobals bm
    ] <> maybeToList (_hrCustom bm)

-- | A default value for Hr.
defHr ∷ Hr
defHr = Hr Nothing Nothing

-- | An instance.
instance Semigroup Hr where
  (<>) (Hr a1 a2) (Hr b1 b2) = Hr (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Hr where
  mempty = defHr
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Hr where
   attrSetGlobals pp bm = bm { _hrGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Hr
  where attrSetAccessKey pp g = g { _hrGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Hr
  where attrSetAnmval pp g = g { _hrGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Hr
  where attrSetContentEditable pp g = g  { _hrGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Hr
  where attrSetContextMenu pp g     = g { _hrGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasClass Hr
  where attrSetClassName pp g           = g { _hrGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Hr
  where attrSetDnmval pp g           = g { _hrGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasDir Hr
  where attrSetDir pp g             = g { _hrGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Hr
  where attrSetDraggable pp g       = g { _hrGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Hr
  where attrSetHidden pp g          = g { _hrGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasId Hr
  where attrSetId pp g              = g { _hrGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasLang Hr
  where attrSetLang pp g            = g { _hrGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasRole Hr
  where attrSetRole pp g            = g { _hrGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Hr
  where attrSetSlot pp g            = g { _hrGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Hr
  where attrSetSpellCheck pp g      = g { _hrGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Hr
  where attrSetStyle pp g           = g { _hrGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Hr
  where attrSetTabIndex pp g        = g { _hrGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Hr
  where attrSetTitle pp g           = g { _hrGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_hrGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Hr
  where attrSetTranslate pp g       = g { _hrGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_hrGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Hr where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_hrGlobals g)

-- | An instance.
instance A.AttrHasCustom Hr where attrSetCustom pp g       = g { _hrCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"hr\" ... @
hr' ∷ forall t m a. DomBuilder t m ⇒ Hr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
hr' bm  = elAttr' "hr" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"hr\" ... @
hr ∷ forall t m a. DomBuilder t m ⇒ Hr → m a → m a
hr bm children = snd <$> hr' bm children

-- | A short-hand notion for @ el\' \"hr\" ... @
hrN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
hrN' = el' "hr"

-- | A short-hand notion for @ el \"hr\" ... @
hrN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
hrN children = snd <$> hrN' children

-- | A short-hand notion for @ elDynAttr\' \"hr\" ... @
hrD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Hr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
hrD' bm  = elDynAttr' "hr" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"hr\" ... @
hrD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Hr → m a → m a
hrD bm children = snd <$> hrD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | I-element has only the global A.attributes.
data I = I
  { _iGlobals ∷ Maybe A.Globals
  , _iCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap I where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _iGlobals bm
    ] <> maybeToList (_iCustom bm)

-- | A default value for I.
defI ∷ I
defI = I Nothing Nothing

-- | An instance.
instance Semigroup I where
  (<>) (I a1 a2) (I b1 b2) = I (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid I where
  mempty = defI
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals I where
   attrSetGlobals pp bm = bm { _iGlobals = Just pp }

-- | An instance.
-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey I
  where attrSetAccessKey pp g = g { _iGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval I
  where attrSetAnmval pp g = g { _iGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable I
  where attrSetContentEditable pp g = g  { _iGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu I
  where attrSetContextMenu pp g     = g { _iGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasClass I
  where attrSetClassName pp g           = g { _iGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval I
  where attrSetDnmval pp g           = g { _iGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasDir I
  where attrSetDir pp g             = g { _iGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable I
  where attrSetDraggable pp g       = g { _iGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasHidden I
  where attrSetHidden pp g          = g { _iGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasId I
  where attrSetId pp g              = g { _iGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasLang I
  where attrSetLang pp g            = g { _iGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasRole I
  where attrSetRole pp g            = g { _iGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasSlot I
  where attrSetSlot pp g            = g { _iGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck I
  where attrSetSpellCheck pp g      = g { _iGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasStyle I
  where attrSetStyle pp g           = g { _iGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex I
  where attrSetTabIndex pp g        = g { _iGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasTitle I
  where attrSetTitle pp g           = g { _iGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_iGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate I
  where attrSetTranslate pp g       = g { _iGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_iGlobals g))) }

-- | An instance.
instance A.AttrGetClassName I where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_iGlobals g)

-- | An instance.
instance A.AttrHasCustom I where attrSetCustom pp g       = g { _iCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"i\" ... @
i' ∷ forall t m a. DomBuilder t m ⇒ I → m a → m (Element EventResult (DomBuilderSpace m) t, a)
i' bm  = elAttr' "i" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"i\" ... @
i ∷ forall t m a. DomBuilder t m ⇒ I → m a → m a
i bm children = snd <$> i' bm children

-- | A short-hand notion for @ el\' \"i\" ... @
iN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
iN' = el' "i"

-- | A short-hand notion for @ el \"i\" ... @
iN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
iN children = snd <$> iN' children

-- | A short-hand notion for @ elDynAttr\' \"i\" ... @
iD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t I → m a → m (Element EventResult (DomBuilderSpace m) t, a)
iD' bm  = elDynAttr' "i" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"i\" ... @
iD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t I → m a → m a
iD bm children = snd <$> iD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | details-element
data Ins = Ins
  { _insGlobals  ∷ Maybe A.Globals
  , _insCite     ∷ Maybe A.Cite
  , _insDateTime ∷ Maybe A.DateTime
  , _insCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Ins where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _insGlobals bm
    , A.attrMap <$> _insCite bm
    , A.attrMap <$> _insDateTime bm
    ] <> maybeToList (_insCustom bm)

-- | A default value for Ins.
defIns ∷ Ins
defIns = Ins Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Ins where
  (<>) (Ins a1 a2 a3 a4) (Ins b1 b2 b3 b4)
    = Ins (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Ins where
  mempty = defIns
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Ins where
   attrSetGlobals pp bm = bm { _insGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Ins
  where attrSetAccessKey pp g = g { _insGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Ins
  where attrSetAnmval pp g = g { _insGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Ins
  where attrSetContentEditable pp g = g  { _insGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Ins
  where attrSetContextMenu pp g     = g { _insGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasClass Ins
  where attrSetClassName pp g           = g { _insGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Ins
  where attrSetDnmval pp g           = g { _insGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasDir Ins
  where attrSetDir pp g             = g { _insGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Ins
  where attrSetDraggable pp g       = g { _insGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Ins
  where attrSetHidden pp g          = g { _insGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasId Ins
  where attrSetId pp g              = g { _insGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasLang Ins
  where attrSetLang pp g            = g { _insGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasRole Ins
  where attrSetRole pp g            = g { _insGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Ins
  where attrSetSlot pp g            = g { _insGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Ins
  where attrSetSpellCheck pp g      = g { _insGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Ins
  where attrSetStyle pp g           = g { _insGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Ins
  where attrSetTabIndex pp g        = g { _insGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Ins
  where attrSetTitle pp g           = g { _insGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_insGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Ins
  where attrSetTranslate pp g       = g { _insGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_insGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Ins where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_insGlobals g)

-- | An instance.
instance A.AttrHasCite Ins where attrSetCite pp g = g {_insCite = Just pp }
-- | An instance.
instance A.AttrHasDateTime Ins where attrSetDateTime pp g = g {_insDateTime = Just pp }

-- | An instance.
instance A.AttrHasCustom Ins where attrSetCustom pp g       = g { _insCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"ins\" ... @
ins' ∷ forall t m a. DomBuilder t m ⇒ Ins → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ins' bm  = elAttr' "ins" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"ins\" ... @
ins ∷ forall t m a. DomBuilder t m ⇒ Ins → m a → m a
ins bm children = snd <$> ins' bm children

-- | A short-hand notion for @ el\' \"ins\" ... @
insN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
insN' = el' "ins"

-- | A short-hand notion for @ el \"ins\" ... @
insN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
insN children = snd <$> insN' children

-- | A short-hand notion for @ elDynAttr\' \"ins\" ... @
insD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ins → m a → m (Element EventResult (DomBuilderSpace m) t, a)
insD' bm = elDynAttr' "ins" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"ins\" ... @
insD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ins → m a → m a
insD bm children = snd <$> insD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Kbd-element has only the global A.attributes.
data Kbd = Kbd
  { _kbdGlobals ∷ Maybe A.Globals
  , _kbdCustom  ∷ Maybe A.Attr
  }

-- | An instance.
instance A.AttrMap Kbd where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _kbdGlobals bm
    ] <> maybeToList (_kbdCustom bm)

-- | A default value for Kbd.
defKbd ∷ Kbd
defKbd = Kbd Nothing Nothing

-- | An instance.
instance Semigroup Kbd where
  (<>) (Kbd a1 a2) (Kbd b1 b2) = Kbd (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Kbd where
  mempty = defKbd
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Kbd where
   attrSetGlobals pp bm = bm { _kbdGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Kbd
  where attrSetAccessKey pp g = g { _kbdGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Kbd
  where attrSetAnmval pp g = g { _kbdGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Kbd
  where attrSetContentEditable pp g = g  { _kbdGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Kbd
  where attrSetContextMenu pp g     = g { _kbdGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasClass Kbd
  where attrSetClassName pp g           = g { _kbdGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Kbd
  where attrSetDnmval pp g           = g { _kbdGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasDir Kbd
  where attrSetDir pp g             = g { _kbdGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Kbd
  where attrSetDraggable pp g       = g { _kbdGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Kbd
  where attrSetHidden pp g          = g { _kbdGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasId Kbd
  where attrSetId pp g              = g { _kbdGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasLang Kbd
  where attrSetLang pp g            = g { _kbdGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasRole Kbd
  where attrSetRole pp g            = g { _kbdGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Kbd
  where attrSetSlot pp g            = g { _kbdGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Kbd
  where attrSetSpellCheck pp g      = g { _kbdGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Kbd
  where attrSetStyle pp g           = g { _kbdGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Kbd
  where attrSetTabIndex pp g        = g { _kbdGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Kbd
  where attrSetTitle pp g           = g { _kbdGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_kbdGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Kbd
  where attrSetTranslate pp g       = g { _kbdGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_kbdGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Kbd where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_kbdGlobals g)

-- | An instance.
instance A.AttrHasCustom Kbd where attrSetCustom pp g       = g { _kbdCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"kbd\" ... @
kbd' ∷ forall t m a. DomBuilder t m ⇒ Kbd → m a → m (Element EventResult (DomBuilderSpace m) t, a)
kbd' bm  = elAttr' "kbd" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"kbd\" ... @
kbd ∷ forall t m a. DomBuilder t m ⇒ Kbd → m a → m a
kbd bm children = snd <$> kbd' bm children

-- | A short-hand notion for @ el\' \"kbd\" ... @
kbdN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
kbdN' = el' "kbd"

-- | A short-hand notion for @ el \"kbd\" ... @
kbdN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
kbdN children = snd <$> kbdN' children

-- | A short-hand notion for @ elDynAttr\' \"kbd\" ... @
kbdD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Kbd → m a → m (Element EventResult (DomBuilderSpace m) t, a)
kbdD' bm  = elDynAttr' "kbd" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"kbd\" ... @
kbdD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Kbd → m a → m a
kbdD bm children = snd <$> kbdD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Li-element
data Li = Li
  { _liGlobals   ∷ Maybe A.Globals
  , _liValueOlLi ∷ Maybe A.ValueOlLi
  , _liCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Li where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _liGlobals bm
    , A.attrMap <$> _liValueOlLi bm
    ] <> maybeToList (_liCustom bm)

-- | A default value for Li.
defLi ∷ Li
defLi = Li Nothing Nothing Nothing

-- | An instance.
instance Semigroup Li where
  (<>) (Li a1 a2 a3) (Li b1 b2 b3) = Li (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Li where
  mempty = defLi
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Li where
   attrSetGlobals pp bm = bm { _liGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Li
  where attrSetAccessKey pp g = g { _liGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Li
  where attrSetAnmval pp g = g { _liGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Li
  where attrSetContentEditable pp g = g  { _liGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Li
  where attrSetContextMenu pp g     = g { _liGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasClass Li
  where attrSetClassName pp g           = g { _liGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Li
  where attrSetDnmval pp g           = g { _liGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasDir Li
  where attrSetDir pp g             = g { _liGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Li
  where attrSetDraggable pp g       = g { _liGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Li
  where attrSetHidden pp g          = g { _liGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasId Li
  where attrSetId pp g              = g { _liGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasLang Li
  where attrSetLang pp g            = g { _liGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasRole Li
  where attrSetRole pp g            = g { _liGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Li
  where attrSetSlot pp g            = g { _liGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Li
  where attrSetSpellCheck pp g      = g { _liGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Li
  where attrSetStyle pp g           = g { _liGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Li
  where attrSetTabIndex pp g        = g { _liGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Li
  where attrSetTitle pp g           = g { _liGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_liGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Li
  where attrSetTranslate pp g       = g { _liGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_liGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Li where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_liGlobals g)

-- | An instance.
instance A.AttrHasValueOlLi  Li where attrSetValueOlLi pp g  = g { _liValueOlLi  = Just pp }

-- | An instance.
instance A.AttrHasCustom Li where attrSetCustom pp g       = g { _liCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"li\" ... @
li' ∷ forall t m a. DomBuilder t m ⇒ Li → m a → m (Element EventResult (DomBuilderSpace m) t, a)
li' bm  = elAttr' "li" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"li\" ... @
li ∷ forall t m a. DomBuilder t m ⇒ Li → m a → m a
li bm children = snd <$> li' bm children

-- | A short-hand notion for @ el\' \"li\" ... @
liN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
liN' = el' "li"

-- | A short-hand notion for @ el \"li\" ... @
liN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
liN children = snd <$> liN' children

-- | A short-hand notion for @ elDynAttr\' \"li\" ... @
liD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Li → m a → m (Element EventResult (DomBuilderSpace m) t, a)
liD' bm  = elDynAttr' "li" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"li\" ... @
liD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Li → m a → m a
liD bm children = snd <$> liD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Legend-element has only the global A.attributes.
data Legend = Legend
  { _legendGlobals ∷ Maybe A.Globals
  , _legendCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Legend where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _legendGlobals bm
    ] <> maybeToList (_legendCustom bm)

-- | A default value for Legend.
defLegend ∷ Legend
defLegend = Legend Nothing Nothing

-- | An instance.
instance Semigroup Legend where
  (<>) (Legend a1 a2) (Legend b1 b2) = Legend (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Legend where
  mempty = defLegend
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Legend where
   attrSetGlobals pp bm = bm { _legendGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Legend
  where attrSetAccessKey pp g = g { _legendGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Legend
  where attrSetAnmval pp g = g { _legendGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Legend
  where attrSetContentEditable pp g = g  { _legendGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Legend
  where attrSetContextMenu pp g     = g { _legendGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasClass Legend
  where attrSetClassName pp g           = g { _legendGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Legend
  where attrSetDnmval pp g           = g { _legendGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasDir Legend
  where attrSetDir pp g             = g { _legendGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Legend
  where attrSetDraggable pp g       = g { _legendGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Legend
  where attrSetHidden pp g          = g { _legendGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasId Legend
  where attrSetId pp g              = g { _legendGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasLang Legend
  where attrSetLang pp g            = g { _legendGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasRole Legend
  where attrSetRole pp g            = g { _legendGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Legend
  where attrSetSlot pp g            = g { _legendGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Legend
  where attrSetSpellCheck pp g      = g { _legendGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Legend
  where attrSetStyle pp g           = g { _legendGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Legend
  where attrSetTabIndex pp g        = g { _legendGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Legend
  where attrSetTitle pp g           = g { _legendGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_legendGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Legend
  where attrSetTranslate pp g       = g { _legendGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_legendGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Legend where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_legendGlobals g)

-- | An instance.
instance A.AttrHasCustom Legend where attrSetCustom pp g       = g { _legendCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"legend\" ... @
legend' ∷ forall t m a. DomBuilder t m ⇒ Legend → m a → m (Element EventResult (DomBuilderSpace m) t, a)
legend' bm  = elAttr' "legend" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"legend\" ... @
legend ∷ forall t m a. DomBuilder t m ⇒ Legend → m a → m a
legend bm children = snd <$> legend' bm children

-- | A short-hand notion for @ el\' \"legend\" ... @
legendN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
legendN' = el' "legend"

-- | A short-hand notion for @ el \"legend\" ... @
legendN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
legendN children = snd <$> legendN' children

-- | A short-hand notion for @ elDynAttr\' \"legend\" ... @
legendD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Legend → m a → m (Element EventResult (DomBuilderSpace m) t, a)
legendD' bm  = elDynAttr' "legend" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"legend\" ... @
legendD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Legend → m a → m a
legendD bm children = snd <$> legendD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Mark-element has only the global A.attributes.
data Mark = Mark
  { _markGlobals ∷ Maybe A.Globals
  , _markCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Mark where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _markGlobals bm
    ] <> maybeToList (_markCustom bm)

-- | A default value for Mark.
defMark ∷ Mark
defMark = Mark Nothing Nothing

-- | An instance.
instance Semigroup Mark where
  (<>) (Mark a1 a2) (Mark b1 b2) = Mark (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Mark where
  mempty = defMark
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Mark where
   attrSetGlobals pp bm = bm { _markGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Mark
  where attrSetAccessKey pp g = g { _markGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Mark
  where attrSetAnmval pp g = g { _markGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Mark
  where attrSetContentEditable pp g = g  { _markGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Mark
  where attrSetContextMenu pp g     = g { _markGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasClass Mark
  where attrSetClassName pp g           = g { _markGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Mark
  where attrSetDnmval pp g           = g { _markGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasDir Mark
  where attrSetDir pp g             = g { _markGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Mark
  where attrSetDraggable pp g       = g { _markGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Mark
  where attrSetHidden pp g          = g { _markGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasId Mark
  where attrSetId pp g              = g { _markGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasLang Mark
  where attrSetLang pp g            = g { _markGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasRole Mark
  where attrSetRole pp g            = g { _markGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Mark
  where attrSetSlot pp g            = g { _markGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Mark
  where attrSetSpellCheck pp g      = g { _markGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Mark
  where attrSetStyle pp g           = g { _markGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Mark
  where attrSetTabIndex pp g        = g { _markGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Mark
  where attrSetTitle pp g           = g { _markGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_markGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Mark
  where attrSetTranslate pp g       = g { _markGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_markGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Mark where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_markGlobals g)

-- | An instance.
instance A.AttrHasCustom Mark where attrSetCustom pp g       = g { _markCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"mark\" ... @
mark' ∷ forall t m a. DomBuilder t m ⇒ Mark → m a → m (Element EventResult (DomBuilderSpace m) t, a)
mark' bm  = elAttr' "mark" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"mark\" ... @
mark ∷ forall t m a. DomBuilder t m ⇒ Mark → m a → m a
mark bm children = snd <$> mark' bm children

-- | A short-hand notion for @ el\' \"mark\" ... @
markN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
markN' = el' "mark"

-- | A short-hand notion for @ el \"mark\" ... @
markN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
markN children = snd <$> markN' children

-- | A short-hand notion for @ elDynAttr\' \"mark\" ... @
markD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Mark → m a → m (Element EventResult (DomBuilderSpace m) t, a)
markD' bm  = elDynAttr' "mark" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"mark\" ... @
markD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Mark → m a → m a
markD bm children = snd <$> markD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Menu-element
data Menu = Menu
  { _menuGlobals ∷ Maybe A.Globals
  , _menuLabel   ∷ Maybe A.Label
  , _menuType    ∷ Maybe A.MenuType
  , _menuCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Menu where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _menuGlobals bm
    , A.attrMap <$> _menuLabel bm
    , A.attrMap <$> _menuType bm
    ] <> maybeToList (_menuCustom bm)

-- | A default value for Menu.
defMenu ∷ Menu
defMenu = Menu Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Menu where
  (<>) (Menu a1 a2 a3 a4) (Menu b1 b2 b3 b4)
    = Menu (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Menu where
  mempty = defMenu
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Menu where
   attrSetGlobals pp bm = bm { _menuGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Menu
  where attrSetAccessKey pp g = g { _menuGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Menu
  where attrSetAnmval pp g = g { _menuGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Menu
  where attrSetContentEditable pp g = g  { _menuGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Menu
  where attrSetContextMenu pp g     = g { _menuGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasClass Menu
  where attrSetClassName pp g           = g { _menuGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Menu
  where attrSetDnmval pp g           = g { _menuGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasDir Menu
  where attrSetDir pp g             = g { _menuGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Menu
  where attrSetDraggable pp g       = g { _menuGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Menu
  where attrSetHidden pp g          = g { _menuGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasId Menu
  where attrSetId pp g              = g { _menuGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasLang Menu
  where attrSetLang pp g            = g { _menuGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasRole Menu
  where attrSetRole pp g            = g { _menuGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Menu
  where attrSetSlot pp g            = g { _menuGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Menu
  where attrSetSpellCheck pp g      = g { _menuGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Menu
  where attrSetStyle pp g           = g { _menuGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Menu
  where attrSetTabIndex pp g        = g { _menuGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Menu
  where attrSetTitle pp g           = g { _menuGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_menuGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Menu
  where attrSetTranslate pp g       = g { _menuGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_menuGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Menu where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_menuGlobals g)

-- | An instance.
instance A.AttrHasMenuType Menu where attrSetMenuType pp g = g { _menuType = Just pp }
-- | An instance.
instance A.AttrHasLabel  Menu where attrSetLabel pp g  = g { _menuLabel  = Just pp }

-- | An instance.
instance A.AttrHasCustom Menu where attrSetCustom pp g       = g { _menuCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"menu\" ... @
menu' ∷ forall t m a. DomBuilder t m ⇒ Menu → m a → m (Element EventResult (DomBuilderSpace m) t, a)
menu' bm  = elAttr' "menu" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"menu\" ... @
menu ∷ forall t m a. DomBuilder t m ⇒ Menu → m a → m a
menu bm children = snd <$> menu' bm children

-- | A short-hand notion for @ el\' \"menu\" ... @
menuN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
menuN' = el' "menu"

-- | A short-hand notion for @ el \"menu\" ... @
menuN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
menuN children = snd <$> menuN' children

-- | A short-hand notion for @ elDynAttr\' \"menu\" ... @
menuD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Menu → m a → m (Element EventResult (DomBuilderSpace m) t, a)
menuD' bm  = elDynAttr' "menu" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"menu\" ... @
menuD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Menu → m a → m a
menuD bm children = snd <$> menuD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | MenuItem-element
data MenuItem = MenuItem
  { _menuItemGlobals  ∷ Maybe A.Globals
  , _menuItemIcon     ∷ Maybe A.Icon
  , _menuItemDisabled ∷ Maybe A.Disabled
  , _menuItemDefault  ∷ Maybe A.Default_
  , _menuItemLabel    ∷ Maybe A.Label
  , _menuItemType     ∷ Maybe A.MenuItemType
  , _menuItemCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap MenuItem where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _menuItemGlobals bm
    , A.attrMap <$> _menuItemIcon bm
    , A.attrMap <$> _menuItemDisabled bm
    , A.attrMap <$> _menuItemDefault bm
    , A.attrMap <$> _menuItemLabel bm
    , A.attrMap <$> _menuItemType bm
    ] <> maybeToList (_menuItemCustom bm)

-- | A default value for MenuItem.
defMenuItem ∷ MenuItem
defMenuItem = MenuItem Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup MenuItem where
  (<>) (MenuItem a1 a2 a3 a4 a5 a6 a7) (MenuItem b1 b2 b3 b4 b5 b6 b7)
    = MenuItem (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7)

-- | An instance.
instance Monoid MenuItem where
  mempty = defMenuItem
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals MenuItem where
   attrSetGlobals pp bm = bm { _menuItemGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey MenuItem
  where attrSetAccessKey pp g = g { _menuItemGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval MenuItem
  where attrSetAnmval pp g = g { _menuItemGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable MenuItem
  where attrSetContentEditable pp g = g  { _menuItemGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu MenuItem
  where attrSetContextMenu pp g     = g { _menuItemGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasClass MenuItem
  where attrSetClassName pp g           = g { _menuItemGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval MenuItem
  where attrSetDnmval pp g           = g { _menuItemGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasDir MenuItem
  where attrSetDir pp g             = g { _menuItemGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable MenuItem
  where attrSetDraggable pp g       = g { _menuItemGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasHidden MenuItem
  where attrSetHidden pp g          = g { _menuItemGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasId MenuItem
  where attrSetId pp g              = g { _menuItemGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasLang MenuItem
  where attrSetLang pp g            = g { _menuItemGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasRole MenuItem
  where attrSetRole pp g            = g { _menuItemGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasSlot MenuItem
  where attrSetSlot pp g            = g { _menuItemGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck MenuItem
  where attrSetSpellCheck pp g      = g { _menuItemGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasStyle MenuItem
  where attrSetStyle pp g           = g { _menuItemGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex MenuItem
  where attrSetTabIndex pp g        = g { _menuItemGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasTitle MenuItem
  where attrSetTitle pp g           = g { _menuItemGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate MenuItem
  where attrSetTranslate pp g       = g { _menuItemGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_menuItemGlobals g))) }

-- | An instance.
instance A.AttrGetClassName MenuItem where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_menuItemGlobals g)

-- | An instance.
instance A.AttrHasIcon  MenuItem where attrSetIcon pp g  = g { _menuItemIcon  = Just pp }
-- | An instance.
instance A.AttrHasDisabled  MenuItem where attrSetDisabled pp g  = g { _menuItemDisabled  = Just pp }
-- | An instance.
instance A.AttrHasDefault  MenuItem where attrSetDefault pp g  = g { _menuItemDefault  = Just pp }
-- | An instance.
instance A.AttrHasLabel  MenuItem where attrSetLabel pp g  = g { _menuItemLabel  = Just pp }
-- | An instance.
instance A.AttrHasMenuItemType MenuItem where attrSetMenuItemType pp g = g { _menuItemType = Just pp }

-- | An instance.
instance A.AttrHasCustom MenuItem where attrSetCustom pp g       = g { _menuItemCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"menuitem\" ... @
menuItem' ∷ forall t m a. DomBuilder t m ⇒ MenuItem → m a → m (Element EventResult (DomBuilderSpace m) t, a)
menuItem' bm  = elAttr' "menuitem" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"menuitem\" ... @
menuItem ∷ forall t m a. DomBuilder t m ⇒ MenuItem → m a → m a
menuItem bm children = snd <$> menuItem' bm children

-- | A short-hand notion for @ el\' \"menuitem\" ... @
menuItemN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
menuItemN' = el' "menuitem"

-- | A short-hand notion for @ el \"menuitem\" ... @
menuItemN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
menuItemN children = snd <$> menuItemN' children

-- | A short-hand notion for @ elDynAttr\' \"menuitem\" ... @
menuItemD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t MenuItem → m a → m (Element EventResult (DomBuilderSpace m) t, a)
menuItemD' bm  = elDynAttr' "menuitem" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"menuitem\" ... @
menuItemD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t MenuItem → m a → m a
menuItemD bm children = snd <$> menuItemD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Meter-element
data Meter = Meter
  { _meterGlobals     ∷ Maybe A.Globals
  , _meterHigh        ∷ Maybe A.High
  , _meterMin         ∷ Maybe A.Min
  , _meterMax         ∷ Maybe A.Max
  , _meterLow         ∷ Maybe A.Low
  , _meterOptimum     ∷ Maybe A.Optimum
  , _meterValueNumber ∷ Maybe A.ValueNumber
  , _meterCustom      ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Meter where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _meterGlobals bm
    , A.attrMap <$> _meterHigh bm
    , A.attrMap <$> _meterMin bm
    , A.attrMap <$> _meterMax bm
    , A.attrMap <$> _meterLow bm
    , A.attrMap <$> _meterOptimum bm
    , A.attrMap <$> _meterValueNumber bm
    ] <> maybeToList (_meterCustom bm)

-- | A default value for Meter.
defMeter ∷ Meter
defMeter = Meter Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Meter where
  (<>) (Meter a1 a2 a3 a4 a5 a6 a7 a8) (Meter b1 b2 b3 b4 b5 b6 b7 b8)
    = Meter (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
             (a6 <> b6) (a7 <> b7) (a8 <> b8)

-- | An instance.
instance Monoid Meter where
  mempty = defMeter
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Meter where
   attrSetGlobals pp bm = bm { _meterGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Meter
  where attrSetAccessKey pp g = g { _meterGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Meter
  where attrSetAnmval pp g = g { _meterGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Meter
  where attrSetContentEditable pp g = g  { _meterGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Meter
  where attrSetContextMenu pp g     = g { _meterGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasClass Meter
  where attrSetClassName pp g           = g { _meterGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Meter
  where attrSetDnmval pp g           = g { _meterGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasDir Meter
  where attrSetDir pp g             = g { _meterGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Meter
  where attrSetDraggable pp g       = g { _meterGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Meter
  where attrSetHidden pp g          = g { _meterGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasId Meter
  where attrSetId pp g              = g { _meterGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasLang Meter
  where attrSetLang pp g            = g { _meterGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasRole Meter
  where attrSetRole pp g            = g { _meterGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Meter
  where attrSetSlot pp g            = g { _meterGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Meter
  where attrSetSpellCheck pp g      = g { _meterGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Meter
  where attrSetStyle pp g           = g { _meterGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Meter
  where attrSetTabIndex pp g        = g { _meterGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Meter
  where attrSetTitle pp g           = g { _meterGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_meterGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Meter
  where attrSetTranslate pp g       = g { _meterGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_meterGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Meter where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_meterGlobals g)

-- | An instance.
instance A.AttrHasHigh Meter where attrSetHigh pp g = g {_meterHigh = Just pp }
-- | An instance.
instance A.AttrHasMin Meter where attrSetMin pp g = g {_meterMin = Just pp }
-- | An instance.
instance A.AttrHasMax Meter where attrSetMax pp g = g {_meterMax = Just pp }
-- | An instance.
instance A.AttrHasLow Meter where attrSetLow pp g = g {_meterLow = Just pp }
-- | An instance.
instance A.AttrHasOptimum Meter where attrSetOptimum pp g = g {_meterOptimum = Just pp }
-- | An instance.
instance A.AttrHasValueNumber Meter where attrSetValueNumber pp g = g {_meterValueNumber = Just pp }

-- | An instance.
instance A.AttrHasCustom Meter where attrSetCustom pp g       = g { _meterCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"meter\" ... @
meter' ∷ forall t m a. DomBuilder t m ⇒ Meter → m a → m (Element EventResult (DomBuilderSpace m) t, a)
meter' bm  = elAttr' "meter" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"meter\" ... @
meter ∷ forall t m a. DomBuilder t m ⇒ Meter → m a → m a
meter bm children = snd <$> meter' bm children

-- | A short-hand notion for @ el\' \"meter\" ... @
meterN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
meterN' = el' "meter"

-- | A short-hand notion for @ el \"meter\" ... @
meterN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
meterN children = snd <$> meterN' children

-- | A short-hand notion for @ elDynAttr\' \"meter\" ... @
meterD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Meter → m a → m (Element EventResult (DomBuilderSpace m) t, a)
meterD' bm  = elDynAttr' "meter" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"meter\" ... @
meterD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Meter → m a → m a
meterD bm children = snd <$> meterD' bm children



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ol-element
data Ol = Ol
  { _olGlobals  ∷ Maybe A.Globals
  , _olReversed ∷ Maybe A.Reversed
  , _olStart    ∷ Maybe A.Start
  , _olType     ∷ Maybe A.OlType
  , _olCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Ol where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _olGlobals bm
    , A.attrMap <$> _olReversed bm
    , A.attrMap <$> _olStart bm
    , A.attrMap <$> _olType bm
    ] <> maybeToList (_olCustom bm)

-- | A default value for Ol.
defOl ∷ Ol
defOl = Ol Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Ol where
  (<>) (Ol a1 a2 a3 a4 a5) (Ol b1 b2 b3 b4 b5)
    = Ol (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | An instance.
instance Monoid Ol where
  mempty = defOl
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Ol where
   attrSetGlobals pp bm = bm { _olGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Ol
  where attrSetAccessKey pp g = g { _olGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Ol
  where attrSetAnmval pp g = g { _olGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Ol
  where attrSetContentEditable pp g = g  { _olGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Ol
  where attrSetContextMenu pp g     = g { _olGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasClass Ol
  where attrSetClassName pp g           = g { _olGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Ol
  where attrSetDnmval pp g           = g { _olGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasDir Ol
  where attrSetDir pp g             = g { _olGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Ol
  where attrSetDraggable pp g       = g { _olGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Ol
  where attrSetHidden pp g          = g { _olGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasId Ol
  where attrSetId pp g              = g { _olGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasLang Ol
  where attrSetLang pp g            = g { _olGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasRole Ol
  where attrSetRole pp g            = g { _olGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Ol
  where attrSetSlot pp g            = g { _olGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Ol
  where attrSetSpellCheck pp g      = g { _olGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Ol
  where attrSetStyle pp g           = g { _olGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Ol
  where attrSetTabIndex pp g        = g { _olGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Ol
  where attrSetTitle pp g           = g { _olGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_olGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Ol
  where attrSetTranslate pp g       = g { _olGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_olGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Ol where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_olGlobals g)

-- | An instance.
instance A.AttrHasReversed Ol where attrSetReversed pp g  = g { _olReversed  = Just pp }

-- | An instance.
instance A.AttrHasStart    Ol where attrSetStart pp g  = g { _olStart  = Just pp }

-- | An instance.
instance A.AttrHasOlType   Ol where attrSetOlType pp g  = g { _olType  = Just pp }

-- | An instance.
instance A.AttrHasCustom Ol where attrSetCustom pp g       = g { _olCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"ol\" ... @
ol' ∷ forall t m a. DomBuilder t m ⇒ Ol → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ol' bm  = elAttr' "ol" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"ol\" ... @
ol ∷ forall t m a. DomBuilder t m ⇒ Ol → m a → m a
ol bm children = snd <$> ol' bm children

-- | A short-hand notion for @ el\' \"ol\" ... @
olN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
olN' = el' "ol"

-- | A short-hand notion for @ el \"ol\" ... @
olN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
olN children = snd <$> olN' children

-- | A short-hand notion for @ elDynAttr\' \"ol\" ... @
olD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ol → m a → m (Element EventResult (DomBuilderSpace m) t, a)
olD' bm  = elDynAttr' "ol" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"ol\" ... @
olD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ol → m a → m a
olD bm children = snd <$> olD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | OptGroup-element
data OptGroup = OptGroup
  { _optGroupGlobals  ∷ Maybe A.Globals
  , _optGroupDisabled ∷ Maybe A.Disabled
  , _optGroupLabel    ∷ Maybe A.Label
  , _optGroupCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap OptGroup where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _optGroupGlobals bm
    , A.attrMap <$> _optGroupDisabled bm
    , A.attrMap <$> _optGroupLabel bm
    ] <> maybeToList (_optGroupCustom bm)

-- | A default value for OptGroup.
defOptGroup ∷ OptGroup
defOptGroup = OptGroup Nothing Nothing Nothing Nothing

-- | An instance.
instance A.AttrHasGlobals OptGroup where
   attrSetGlobals pp bm = bm { _optGroupGlobals = Just pp }

-- | An instance.
instance Semigroup OptGroup where
  (<>) (OptGroup a1 a2 a3 a4) (OptGroup b1 b2 b3 b4)
    = OptGroup (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid OptGroup where
  mempty = defOptGroup
  mappend = (<>)

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey OptGroup
  where attrSetAccessKey pp g = g { _optGroupGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval OptGroup
  where attrSetAnmval pp g = g { _optGroupGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable OptGroup
  where attrSetContentEditable pp g = g  { _optGroupGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu OptGroup
  where attrSetContextMenu pp g     = g { _optGroupGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasClass OptGroup
  where attrSetClassName pp g           = g { _optGroupGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval OptGroup
  where attrSetDnmval pp g           = g { _optGroupGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDir OptGroup
  where attrSetDir pp g             = g { _optGroupGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable OptGroup
  where attrSetDraggable pp g       = g { _optGroupGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasHidden OptGroup
  where attrSetHidden pp g          = g { _optGroupGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasId OptGroup
  where attrSetId pp g              = g { _optGroupGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasLang OptGroup
  where attrSetLang pp g            = g { _optGroupGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasRole OptGroup
  where attrSetRole pp g            = g { _optGroupGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasSlot OptGroup
  where attrSetSlot pp g            = g { _optGroupGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck OptGroup
  where attrSetSpellCheck pp g      = g { _optGroupGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasStyle OptGroup
  where attrSetStyle pp g           = g { _optGroupGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex OptGroup
  where attrSetTabIndex pp g        = g { _optGroupGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTitle OptGroup
  where attrSetTitle pp g           = g { _optGroupGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate OptGroup
  where attrSetTranslate pp g       = g { _optGroupGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_optGroupGlobals g))) }

-- | An instance.
instance A.AttrGetClassName OptGroup where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_optGroupGlobals g)

-- | An instance.
instance A.AttrHasDisabled OptGroup where attrSetDisabled pp g  = g { _optGroupDisabled  = Just pp }

-- | An instance.
instance A.AttrHasLabel    OptGroup where attrSetLabel pp g  = g { _optGroupLabel  = Just pp }

-- | An instance.
instance A.AttrHasCustom OptGroup where attrSetCustom pp g       = g { _optGroupCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"optgroup\" ... @
optGroup' ∷ forall t m a. DomBuilder t m ⇒ OptGroup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
optGroup' bm  = elAttr' "optgroup" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"optgroup\" ... @
optGroup ∷ forall t m a. DomBuilder t m ⇒ OptGroup → m a → m a
optGroup bm children = snd <$> optGroup' bm children

-- | A short-hand notion for @ el\' \"optgroup\" ... @
optGroupN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
optGroupN' = el' "optgroup"

-- | A short-hand notion for @ el \"optgroup\" ... @
optGroupN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
optGroupN children = snd <$> optGroupN' children

-- | A short-hand notion for @ elDynAttr\' \"optgroup\" ... @
optGroupD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t OptGroup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
optGroupD' bm  = elDynAttr' "optgroup" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"optgroup\" ... @
optGroupD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t OptGroup → m a → m a
optGroupD bm children = snd <$> optGroupD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Option-element
data Option = Option
  { _optionGlobals   ∷ Maybe A.Globals
  , _optionDisabled  ∷ Maybe A.Disabled
  , _optionLabel     ∷ Maybe A.Label
  , _optionSelected  ∷ Maybe A.Selected
  , _optionValueText ∷ Maybe A.ValueText
  , _optionCustom    ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Option where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _optionGlobals bm
    , A.attrMap <$> _optionDisabled bm
    , A.attrMap <$> _optionLabel bm
    , A.attrMap <$> _optionSelected bm
    , A.attrMap <$> _optionValueText bm
    ] <> maybeToList (_optionCustom bm)

-- | A default value for Option.
defOption ∷ Option
defOption = Option Nothing Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Option where
  (<>) (Option a1 a2 a3 a4 a5 a6) (Option b1 b2 b3 b4 b5 b6)
    = Option (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

-- | An instance.
instance Monoid Option where
  mempty = defOption
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Option where
   attrSetGlobals pp bm = bm { _optionGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Option
  where attrSetAccessKey pp g = g { _optionGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Option
  where attrSetAnmval pp g = g { _optionGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Option
  where attrSetContentEditable pp g = g  { _optionGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Option
  where attrSetContextMenu pp g     = g { _optionGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasClass Option
  where attrSetClassName pp g           = g { _optionGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Option
  where attrSetDnmval pp g           = g { _optionGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasDir Option
  where attrSetDir pp g             = g { _optionGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Option
  where attrSetDraggable pp g       = g { _optionGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Option
  where attrSetHidden pp g          = g { _optionGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasId Option
  where attrSetId pp g              = g { _optionGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasLang Option
  where attrSetLang pp g            = g { _optionGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasRole Option
  where attrSetRole pp g            = g { _optionGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Option
  where attrSetSlot pp g            = g { _optionGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Option
  where attrSetSpellCheck pp g      = g { _optionGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Option
  where attrSetStyle pp g           = g { _optionGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Option
  where attrSetTabIndex pp g        = g { _optionGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Option
  where attrSetTitle pp g           = g { _optionGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_optionGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Option
  where attrSetTranslate pp g       = g { _optionGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_optionGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Option where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_optionGlobals g)

-- | An instance.
instance A.AttrHasDisabled Option where attrSetDisabled pp g  = g { _optionDisabled  = Just pp }

-- | An instance.
instance A.AttrHasLabel    Option where attrSetLabel pp g  = g { _optionLabel  = Just pp }
-- | An instance.
instance A.AttrHasSelected    Option where attrSetSelected pp g  = g { _optionSelected  = Just pp }
-- | An instance.
instance A.AttrHasValueText    Option where attrSetValueText pp g  = g { _optionValueText  = Just pp }

-- | An instance.
instance A.AttrHasCustom Option where attrSetCustom pp g       = g { _optionCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"option\" ... @
option' ∷ forall t m a. DomBuilder t m ⇒ Option → m a → m (Element EventResult (DomBuilderSpace m) t, a)
option' bm  = elAttr' "option" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"option\" ... @
option ∷ forall t m a. DomBuilder t m ⇒ Option → m a → m a
option bm children = snd <$> option' bm children

-- | A short-hand notion for @ el\' \"option\" ... @
optionN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
optionN' = el' "option"

-- | A short-hand notion for @ el \"option\" ... @
optionN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
optionN children = snd <$> optionN' children

-- | A short-hand notion for @ elDynAttr\' \"option\" ... @
optionD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Option → m a → m (Element EventResult (DomBuilderSpace m) t, a)
optionD' bm  = elDynAttr' "option" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"option\" ... @
optionD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Option → m a → m a
optionD bm children = snd <$> optionD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Output-element
data Output = Output
  { _outputGlobals ∷ Maybe A.Globals
  , _outputForId   ∷ Maybe A.ForId
  , _outputForm    ∷ Maybe A.Form
  , _outputName    ∷ Maybe A.Name
  , _outputCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Output where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _outputGlobals bm
    , A.attrMap <$> _outputForId bm
    , A.attrMap <$> _outputForm bm
    , A.attrMap <$> _outputName bm
    ] <> maybeToList (_outputCustom bm)

-- | A default value for Output.
defOutput ∷ Output
defOutput = Output Nothing Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Output where
  (<>) (Output a1 a2 a3 a4 a5) (Output b1 b2 b3 b4 b5)
    = Output (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

-- | An instance.
instance Monoid Output where
  mempty = defOutput
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Output where
   attrSetGlobals pp bm = bm { _outputGlobals = Just pp }

-- | An instance.
-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Output
  where attrSetAccessKey pp g = g { _outputGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Output
  where attrSetAnmval pp g = g { _outputGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Output
  where attrSetContentEditable pp g = g  { _outputGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Output
  where attrSetContextMenu pp g     = g { _outputGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasClass Output
  where attrSetClassName pp g           = g { _outputGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Output
  where attrSetDnmval pp g           = g { _outputGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasDir Output
  where attrSetDir pp g             = g { _outputGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Output
  where attrSetDraggable pp g       = g { _outputGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Output
  where attrSetHidden pp g          = g { _outputGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasId Output
  where attrSetId pp g              = g { _outputGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasLang Output
  where attrSetLang pp g            = g { _outputGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasRole Output
  where attrSetRole pp g            = g { _outputGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Output
  where attrSetSlot pp g            = g { _outputGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Output
  where attrSetSpellCheck pp g      = g { _outputGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Output
  where attrSetStyle pp g           = g { _outputGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Output
  where attrSetTabIndex pp g        = g { _outputGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Output
  where attrSetTitle pp g           = g { _outputGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_outputGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Output
  where attrSetTranslate pp g       = g { _outputGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_outputGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Output where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_outputGlobals g)

-- | An instance.
instance A.AttrHasForId Output where attrSetForId pp g  = g { _outputForId  = Just pp }
-- | An instance.
instance A.AttrHasForm Output where attrSetForm pp g  = g { _outputForm  = Just pp }

-- | An instance.
instance A.AttrHasName    Output where attrSetName pp g  = g { _outputName  = Just pp }

-- | An instance.
instance A.AttrHasCustom Output where attrSetCustom pp g       = g { _outputCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"output\" ... @
output' ∷ forall t m a. DomBuilder t m ⇒ Output → m a → m (Element EventResult (DomBuilderSpace m) t, a)
output' bm  = elAttr' "output" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"output\" ... @
output ∷ forall t m a. DomBuilder t m ⇒ Output → m a → m a
output bm children = snd <$> output' bm children

-- | A short-hand notion for @ el\' \"output\" ... @
outputN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
outputN' = el' "output"

-- | A short-hand notion for @ el \"output\" ... @
outputN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
outputN children = snd <$> outputN' children

-- | A short-hand notion for @ elDynAttr\' \"output\" ... @
outputD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Output → m a → m (Element EventResult (DomBuilderSpace m) t, a)
outputD' bm  = elDynAttr' "output" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"output\" ... @
outputD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Output → m a → m a
outputD bm children = snd <$> outputD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | P-element has only the global A.attributes.
data P = P
  { _pGlobals ∷ Maybe A.Globals
  , _pCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap P where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _pGlobals bm
    ] <> maybeToList (_pCustom bm)

-- | A default value for P.
defP ∷ P
defP = P Nothing Nothing

-- | An instance.
instance Semigroup P where
  (<>) (P a1 a2) (P b1 b2) = P (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid P where
  mempty = defP
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals P where
   attrSetGlobals pp bm = bm { _pGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey P
  where attrSetAccessKey pp g = g { _pGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval P
  where attrSetAnmval pp g = g { _pGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable P
  where attrSetContentEditable pp g = g  { _pGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu P
  where attrSetContextMenu pp g     = g { _pGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasClass P
  where attrSetClassName pp g           = g { _pGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval P
  where attrSetDnmval pp g           = g { _pGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasDir P
  where attrSetDir pp g             = g { _pGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable P
  where attrSetDraggable pp g       = g { _pGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasHidden P
  where attrSetHidden pp g          = g { _pGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasId P
  where attrSetId pp g              = g { _pGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasLang P
  where attrSetLang pp g            = g { _pGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasRole P
  where attrSetRole pp g            = g { _pGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasSlot P
  where attrSetSlot pp g            = g { _pGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck P
  where attrSetSpellCheck pp g      = g { _pGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasStyle P
  where attrSetStyle pp g           = g { _pGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex P
  where attrSetTabIndex pp g        = g { _pGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasTitle P
  where attrSetTitle pp g           = g { _pGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_pGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate P
  where attrSetTranslate pp g       = g { _pGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_pGlobals g))) }

-- | An instance.
instance A.AttrGetClassName P where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_pGlobals g)

-- | An instance.
instance A.AttrHasCustom P where attrSetCustom pp g       = g { _pCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"p\" ... @
p' ∷ forall t m a. DomBuilder t m ⇒ P → m a → m (Element EventResult (DomBuilderSpace m) t, a)
p' bm  = elAttr' "p" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"p\" ... @
p ∷ forall t m a. DomBuilder t m ⇒ P → m a → m a
p bm children = snd <$> p' bm children

-- | A short-hand notion for @ el\' \"p\" ... @
pN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
pN' = el' "p"

-- | A short-hand notion for @ el \"p\" ... @
pN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
pN children = snd <$> pN' children

-- | A short-hand notion for @ elDynAttr\' \"p\" ... @
pD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t P → m a → m (Element EventResult (DomBuilderSpace m) t, a)
pD' bm  = elDynAttr' "p" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"p\" ... @
pD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t P → m a → m a
pD bm children = snd <$> pD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Pre-element has only the global A.attributes.
data Pre = Pre
  { _prglobals ∷ Maybe A.Globals
  , _prcustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Pre where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _prglobals bm
    ] <> maybeToList (_prcustom bm)

-- | A default value for Pre.
defPre ∷ Pre
defPre = Pre Nothing Nothing

-- | An instance.
instance Semigroup Pre where
  (<>) (Pre a1 a2) (Pre b1 b2) = Pre (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Pre where
  mempty = defPre
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Pre where
   attrSetGlobals pp bm = bm { _prglobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Pre
  where attrSetAccessKey pp g = g { _prglobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasAnmval Pre
  where attrSetAnmval pp g = g { _prglobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Pre
  where attrSetContentEditable pp g = g  { _prglobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Pre
  where attrSetContextMenu pp g     = g { _prglobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasClass Pre
  where attrSetClassName pp g           = g { _prglobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasDnmval Pre
  where attrSetDnmval pp g           = g { _prglobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasDir Pre
  where attrSetDir pp g             = g { _prglobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasDraggable Pre
  where attrSetDraggable pp g       = g { _prglobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasHidden Pre
  where attrSetHidden pp g          = g { _prglobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasId Pre
  where attrSetId pp g              = g { _prglobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasLang Pre
  where attrSetLang pp g            = g { _prglobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasRole Pre
  where attrSetRole pp g            = g { _prglobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasSlot Pre
  where attrSetSlot pp g            = g { _prglobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Pre
  where attrSetSpellCheck pp g      = g { _prglobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasStyle Pre
  where attrSetStyle pp g           = g { _prglobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Pre
  where attrSetTabIndex pp g        = g { _prglobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasTitle Pre
  where attrSetTitle pp g           = g { _prglobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_prglobals g))) }
-- | An instance.
instance A.AttrHasTranslate Pre
  where attrSetTranslate pp g       = g { _prglobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_prglobals g))) }

-- | An instance.
instance A.AttrGetClassName Pre where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_prglobals g)

-- | An instance.
instance A.AttrHasCustom Pre where attrSetCustom pp g       = g { _prcustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"pre\" ... @
pre' ∷ forall t m a. DomBuilder t m ⇒ Pre → m a → m (Element EventResult (DomBuilderSpace m) t, a)
pre' bm  = elAttr' "pre" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"pre\" ... @
pre ∷ forall t m a. DomBuilder t m ⇒ Pre → m a → m a
pre bm children = snd <$> pre' bm children

-- | A short-hand notion for @ el\' \"pre\" ... @
prn' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
prn' = el' "pre"

-- | A short-hand notion for @ el \"pre\" ... @
prn ∷ forall t m a. DomBuilder t m ⇒ m a → m a
prn children = snd <$> prn' children

-- | A short-hand notion for @ elDynAttr\' \"pre\" ... @
prd' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Pre → m a → m (Element EventResult (DomBuilderSpace m) t, a)
prd' bm  = elDynAttr' "pre" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"pre\" ... @
prd ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Pre → m a → m a
prd bm children = snd <$> prd' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Progress-element
data Progress = Progress
  { _progressGlobals     ∷ Maybe A.Globals
  , _progressMax         ∷ Maybe A.Max
  , _progressValueNumber ∷ Maybe A.ValueNumber
  , _progressCustom      ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Progress where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _progressGlobals bm
    , A.attrMap <$> _progressMax bm
    , A.attrMap <$> _progressValueNumber bm
    ] <> maybeToList (_progressCustom bm)

-- | A default value for Progress.
defProgress ∷ Progress
defProgress = Progress Nothing Nothing Nothing Nothing

-- | An instance.
instance Semigroup Progress where
  (<>) (Progress a1 a2 a3 a4) (Progress b1 b2 b3 b4)
    = Progress (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

-- | An instance.
instance Monoid Progress where
  mempty = defProgress
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Progress where
   attrSetGlobals pp bm = bm { _progressGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Progress
  where attrSetAccessKey pp g = g { _progressGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Progress
  where attrSetAnmval pp g = g { _progressGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Progress
  where attrSetContentEditable pp g = g  { _progressGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Progress
  where attrSetContextMenu pp g     = g { _progressGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasClass Progress
  where attrSetClassName pp g           = g { _progressGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Progress
  where attrSetDnmval pp g           = g { _progressGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasDir Progress
  where attrSetDir pp g             = g { _progressGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Progress
  where attrSetDraggable pp g       = g { _progressGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Progress
  where attrSetHidden pp g          = g { _progressGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasId Progress
  where attrSetId pp g              = g { _progressGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasLang Progress
  where attrSetLang pp g            = g { _progressGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasRole Progress
  where attrSetRole pp g            = g { _progressGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Progress
  where attrSetSlot pp g            = g { _progressGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Progress
  where attrSetSpellCheck pp g      = g { _progressGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Progress
  where attrSetStyle pp g           = g { _progressGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Progress
  where attrSetTabIndex pp g        = g { _progressGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Progress
  where attrSetTitle pp g           = g { _progressGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_progressGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Progress
  where attrSetTranslate pp g       = g { _progressGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_progressGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Progress where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_progressGlobals g)

-- | An instance.
instance A.AttrHasMax Progress where attrSetMax pp g  = g { _progressMax  = Just pp }

-- | An instance.
instance A.AttrHasValueNumber    Progress where attrSetValueNumber pp g  = g { _progressValueNumber  = Just pp }

-- | An instance.
instance A.AttrHasCustom Progress where attrSetCustom pp g       = g { _progressCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"progress\" ... @
progress' ∷ forall t m a. DomBuilder t m ⇒ Progress → m a → m (Element EventResult (DomBuilderSpace m) t, a)
progress' bm  = elAttr' "progress" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"progress\" ... @
progress ∷ forall t m a. DomBuilder t m ⇒ Progress → m a → m a
progress bm children = snd <$> progress' bm children

-- | A short-hand notion for @ el\' \"progress\" ... @
progressN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
progressN' = el' "progress"

-- | A short-hand notion for @ el \"progress\" ... @
progressN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
progressN children = snd <$> progressN' children

-- | A short-hand notion for @ elDynAttr\' \"progress\" ... @
progressD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Progress → m a → m (Element EventResult (DomBuilderSpace m) t, a)
progressD' bm  = elDynAttr' "progress" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"progress\" ... @
progressD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Progress → m a → m a
progressD bm children = snd <$> progressD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Q-element
data Q_ = Q_
  { _qGlobals ∷ Maybe A.Globals
  , _qCite    ∷ Maybe A.Cite
  , _qCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Q_ where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _qGlobals bm
    , A.attrMap <$> _qCite bm
    ] <> maybeToList (_qCustom bm)

-- | A default value for Q_.
defQ_ ∷ Q_
defQ_ = Q_ Nothing Nothing Nothing

-- | An instance.
instance Semigroup Q_ where
  (<>) (Q_ a1 a2 a3) (Q_ b1 b2 b3) = Q_ (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Q_ where
  mempty = defQ_
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Q_ where
   attrSetGlobals pp bm = bm { _qGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Q_
  where attrSetAccessKey pp g = g { _qGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Q_
  where attrSetAnmval pp g = g { _qGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Q_
  where attrSetContentEditable pp g = g  { _qGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Q_
  where attrSetContextMenu pp g     = g { _qGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasClass Q_
  where attrSetClassName pp g           = g { _qGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Q_
  where attrSetDnmval pp g           = g { _qGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasDir Q_
  where attrSetDir pp g             = g { _qGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Q_
  where attrSetDraggable pp g       = g { _qGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Q_
  where attrSetHidden pp g          = g { _qGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasId Q_
  where attrSetId pp g              = g { _qGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasLang Q_
  where attrSetLang pp g            = g { _qGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasRole Q_
  where attrSetRole pp g            = g { _qGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Q_
  where attrSetSlot pp g            = g { _qGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Q_
  where attrSetSpellCheck pp g      = g { _qGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Q_
  where attrSetStyle pp g           = g { _qGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Q_
  where attrSetTabIndex pp g        = g { _qGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Q_
  where attrSetTitle pp g           = g { _qGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_qGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Q_
  where attrSetTranslate pp g       = g { _qGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_qGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Q_ where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_qGlobals g)

-- | An instance.
instance A.AttrHasCite Q_ where attrSetCite pp g  = g { _qCite  = Just pp }

-- | An instance.
instance A.AttrHasCustom Q_ where attrSetCustom pp g       = g { _qCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"q\" ... @
q' ∷ forall t m a. DomBuilder t m ⇒ Q_ → m a → m (Element EventResult (DomBuilderSpace m) t, a)
q' bm  = elAttr' "q" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"q\" ... @
q ∷ forall t m a. DomBuilder t m ⇒ Q_ → m a → m a
q bm children = snd <$> q' bm children

-- | A short-hand notion for @ el\' \"q\" ... @
qN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
qN' = el' "q"

-- | A short-hand notion for @ el \"q\" ... @
qN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
qN children = snd <$> qN' children

-- | A short-hand notion for @ elDynAttr\' \"q\" ... @
qD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Q_ → m a → m (Element EventResult (DomBuilderSpace m) t, a)
qD' bm  = elDynAttr' "q" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"q\" ... @
qD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Q_ → m a → m a
qD bm children = snd <$> qD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rb-element
data Rb = Rb
  { _rbGlobals ∷ Maybe A.Globals
  , _rbCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Rb where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _rbGlobals bm
    ] <> maybeToList (_rbCustom bm)

-- | A default value for Rb.
defRb ∷ Rb
defRb = Rb Nothing Nothing

-- | An instance.
instance Semigroup Rb where
  (<>) (Rb a1 a2) (Rb b1 b2) = Rb (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Rb where
  mempty = defRb
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Rb where
   attrSetGlobals pp bm = bm { _rbGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Rb
  where attrSetAccessKey pp g = g { _rbGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Rb
  where attrSetAnmval pp g = g { _rbGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Rb
  where attrSetContentEditable pp g = g  { _rbGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Rb
  where attrSetContextMenu pp g     = g { _rbGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasClass Rb
  where attrSetClassName pp g           = g { _rbGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Rb
  where attrSetDnmval pp g           = g { _rbGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasDir Rb
  where attrSetDir pp g             = g { _rbGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Rb
  where attrSetDraggable pp g       = g { _rbGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Rb
  where attrSetHidden pp g          = g { _rbGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasId Rb
  where attrSetId pp g              = g { _rbGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasLang Rb
  where attrSetLang pp g            = g { _rbGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasRole Rb
  where attrSetRole pp g            = g { _rbGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Rb
  where attrSetSlot pp g            = g { _rbGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Rb
  where attrSetSpellCheck pp g      = g { _rbGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Rb
  where attrSetStyle pp g           = g { _rbGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Rb
  where attrSetTabIndex pp g        = g { _rbGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Rb
  where attrSetTitle pp g           = g { _rbGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_rbGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Rb
  where attrSetTranslate pp g       = g { _rbGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_rbGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Rb where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_rbGlobals g)

-- | An instance.
instance A.AttrHasCustom Rb where attrSetCustom pp g       = g { _rbCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"rb\" ... @
rb' ∷ forall t m a. DomBuilder t m ⇒ Rb → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rb' bm  = elAttr' "rb" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"rb\" ... @
rb ∷ forall t m a. DomBuilder t m ⇒ Rb → m a → m a
rb bm children = snd <$> rb' bm children

-- | A short-hand notion for @ el\' \"rb\" ... @
rbN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
rbN' = el' "rb"

-- | A short-hand notion for @ el \"rb\" ... @
rbN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
rbN children = snd <$> rbN' children

-- | A short-hand notion for @ elDynAttr\' \"rb\" ... @
rbD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rb → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rbD' bm  = elDynAttr' "rb" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"rb\" ... @
rbD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rb → m a → m a
rbD bm children = snd <$> rbD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rp-element has only the global A.attributes.
data Rp = Rp
  { _rpGlobals ∷ Maybe A.Globals
  , _rpCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Rp where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _rpGlobals bm
    ] <> maybeToList (_rpCustom bm)

-- | A default value for Rp.
defRp ∷ Rp
defRp = Rp Nothing Nothing

-- | An instance.
instance Semigroup Rp where
  (<>) (Rp a1 a2) (Rp b1 b2) = Rp (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Rp where
  mempty = defRp
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Rp where
   attrSetGlobals pp bm = bm { _rpGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Rp
  where attrSetAccessKey pp g = g { _rpGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Rp
  where attrSetAnmval pp g = g { _rpGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Rp
  where attrSetContentEditable pp g = g  { _rpGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Rp
  where attrSetContextMenu pp g     = g { _rpGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasClass Rp
  where attrSetClassName pp g           = g { _rpGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Rp
  where attrSetDnmval pp g           = g { _rpGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasDir Rp
  where attrSetDir pp g             = g { _rpGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Rp
  where attrSetDraggable pp g       = g { _rpGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Rp
  where attrSetHidden pp g          = g { _rpGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasId Rp
  where attrSetId pp g              = g { _rpGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasLang Rp
  where attrSetLang pp g            = g { _rpGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasRole Rp
  where attrSetRole pp g            = g { _rpGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Rp
  where attrSetSlot pp g            = g { _rpGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Rp
  where attrSetSpellCheck pp g      = g { _rpGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Rp
  where attrSetStyle pp g           = g { _rpGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Rp
  where attrSetTabIndex pp g        = g { _rpGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Rp
  where attrSetTitle pp g           = g { _rpGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_rpGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Rp
  where attrSetTranslate pp g       = g { _rpGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_rpGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Rp where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_rpGlobals g)

-- | An instance.
instance A.AttrHasCustom Rp where attrSetCustom pp g       = g { _rpCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"rp\" ... @
rp' ∷ forall t m a. DomBuilder t m ⇒ Rp → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rp' bm  = elAttr' "rp" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"rp\" ... @
rp ∷ forall t m a. DomBuilder t m ⇒ Rp → m a → m a
rp bm children = snd <$> rp' bm children

-- | A short-hand notion for @ el\' \"rp\" ... @
rpN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
rpN' = el' "rp"

-- | A short-hand notion for @ el \"rp\" ... @
rpN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
rpN children = snd <$> rpN' children

-- | A short-hand notion for @ elDynAttr\' \"rp\" ... @
rpD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rp → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rpD' bm  = elDynAttr' "rp" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"rp\" ... @
rpD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rp → m a → m a
rpD bm children = snd <$> rpD' bm children


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rt-element has only the global A.attributes.
data Rt = Rt
  { _rtGlobals ∷ Maybe A.Globals
  , _rtCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Rt where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _rtGlobals bm
    ] <> maybeToList (_rtCustom bm)

-- | A default value for Rt.
defRt ∷ Rt
defRt = Rt Nothing Nothing

-- | An instance.
instance Semigroup Rt where
  (<>) (Rt a1 a2) (Rt b1 b2) = Rt (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Rt where
  mempty = defRt
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Rt where
   attrSetGlobals pp bm = bm { _rtGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Rt
  where attrSetAccessKey pp g = g { _rtGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Rt
  where attrSetAnmval pp g = g { _rtGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Rt
  where attrSetContentEditable pp g = g  { _rtGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Rt
  where attrSetContextMenu pp g     = g { _rtGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasClass Rt
  where attrSetClassName pp g           = g { _rtGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Rt
  where attrSetDnmval pp g           = g { _rtGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasDir Rt
  where attrSetDir pp g             = g { _rtGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Rt
  where attrSetDraggable pp g       = g { _rtGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Rt
  where attrSetHidden pp g          = g { _rtGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasId Rt
  where attrSetId pp g              = g { _rtGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasLang Rt
  where attrSetLang pp g            = g { _rtGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasRole Rt
  where attrSetRole pp g            = g { _rtGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Rt
  where attrSetSlot pp g            = g { _rtGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Rt
  where attrSetSpellCheck pp g      = g { _rtGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Rt
  where attrSetStyle pp g           = g { _rtGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Rt
  where attrSetTabIndex pp g        = g { _rtGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Rt
  where attrSetTitle pp g           = g { _rtGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_rtGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Rt
  where attrSetTranslate pp g       = g { _rtGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_rtGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Rt where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_rtGlobals g)

-- | An instance.
instance A.AttrHasCustom Rt where attrSetCustom pp g       = g { _rtCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"rt\" ... @
rt' ∷ forall t m a. DomBuilder t m ⇒ Rt → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rt' bm  = elAttr' "rt" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"rt\" ... @
rt ∷ forall t m a. DomBuilder t m ⇒ Rt → m a → m a
rt bm children = snd <$> rt' bm children

-- | A short-hand notion for @ el\' \"rt\" ... @
rtN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
rtN' = el' "rt"

-- | A short-hand notion for @ el \"rt\" ... @
rtN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
rtN children = snd <$> rtN' children

-- | A short-hand notion for @ elDynAttr\' \"rt\" ... @
rtD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rt → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rtD' bm  = elDynAttr' "rt" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"rt\" ... @
rtD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rt → m a → m a
rtD bm children = snd <$> rtD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Rtc-element has only the global A.attributes.
data Rtc = Rtc
  { _rtcGlobals ∷ Maybe A.Globals
  , _rtcCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Rtc where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _rtcGlobals bm
    ] <> maybeToList (_rtcCustom bm)

-- | A default value for Rtc.
defRtc ∷ Rtc
defRtc = Rtc Nothing Nothing

-- | An instance.
instance Semigroup Rtc where
  (<>) (Rtc a1 a2) (Rtc b1 b2) = Rtc (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Rtc where
  mempty = defRtc
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Rtc where
   attrSetGlobals pp bm = bm { _rtcGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Rtc
  where attrSetAccessKey pp g = g { _rtcGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Rtc
  where attrSetAnmval pp g = g { _rtcGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Rtc
  where attrSetContentEditable pp g = g  { _rtcGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Rtc
  where attrSetContextMenu pp g     = g { _rtcGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasClass Rtc
  where attrSetClassName pp g           = g { _rtcGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Rtc
  where attrSetDnmval pp g           = g { _rtcGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasDir Rtc
  where attrSetDir pp g             = g { _rtcGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Rtc
  where attrSetDraggable pp g       = g { _rtcGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Rtc
  where attrSetHidden pp g          = g { _rtcGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasId Rtc
  where attrSetId pp g              = g { _rtcGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasLang Rtc
  where attrSetLang pp g            = g { _rtcGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasRole Rtc
  where attrSetRole pp g            = g { _rtcGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Rtc
  where attrSetSlot pp g            = g { _rtcGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Rtc
  where attrSetSpellCheck pp g      = g { _rtcGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Rtc
  where attrSetStyle pp g           = g { _rtcGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Rtc
  where attrSetTabIndex pp g        = g { _rtcGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Rtc
  where attrSetTitle pp g           = g { _rtcGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_rtcGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Rtc
  where attrSetTranslate pp g       = g { _rtcGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_rtcGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Rtc where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_rtcGlobals g)

-- | An instance.
instance A.AttrHasCustom Rtc where attrSetCustom pp g       = g { _rtcCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"rtc\" ... @
rtc' ∷ forall t m a. DomBuilder t m ⇒ Rtc → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rtc' bm  = elAttr' "rtc" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"rtc\" ... @
rtc ∷ forall t m a. DomBuilder t m ⇒ Rtc → m a → m a
rtc bm children = snd <$> rtc' bm children

-- | A short-hand notion for @ el\' \"rtc\" ... @
rtcN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
rtcN' = el' "rtc"

-- | A short-hand notion for @ el \"rtc\" ... @
rtcN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
rtcN children = snd <$> rtcN' children

-- | A short-hand notion for @ elDynAttr\' \"rtc\" ... @
rtcD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rtc → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rtcD' bm  = elDynAttr' "rtc" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"rtc\" ... @
rtcD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Rtc → m a → m a
rtcD bm children = snd <$> rtcD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ruby-element has only the global A.attributes.
data Ruby = Ruby
  { _rubyGlobals ∷ Maybe A.Globals
  , _rubyCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Ruby where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _rubyGlobals bm
    ] <> maybeToList (_rubyCustom bm)

-- | A default value for Ruby.
defRuby ∷ Ruby
defRuby = Ruby Nothing Nothing

-- | An instance.
instance Semigroup Ruby where
  (<>) (Ruby a1 a2) (Ruby b1 b2) = Ruby (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Ruby where
  mempty = defRuby
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Ruby where
   attrSetGlobals pp bm = bm { _rubyGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Ruby
  where attrSetAccessKey pp g = g { _rubyGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Ruby
  where attrSetAnmval pp g = g { _rubyGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Ruby
  where attrSetContentEditable pp g = g  { _rubyGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Ruby
  where attrSetContextMenu pp g     = g { _rubyGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasClass Ruby
  where attrSetClassName pp g           = g { _rubyGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Ruby
  where attrSetDnmval pp g           = g { _rubyGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasDir Ruby
  where attrSetDir pp g             = g { _rubyGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Ruby
  where attrSetDraggable pp g       = g { _rubyGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Ruby
  where attrSetHidden pp g          = g { _rubyGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasId Ruby
  where attrSetId pp g              = g { _rubyGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasLang Ruby
  where attrSetLang pp g            = g { _rubyGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasRole Ruby
  where attrSetRole pp g            = g { _rubyGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Ruby
  where attrSetSlot pp g            = g { _rubyGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Ruby
  where attrSetSpellCheck pp g      = g { _rubyGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Ruby
  where attrSetStyle pp g           = g { _rubyGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Ruby
  where attrSetTabIndex pp g        = g { _rubyGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Ruby
  where attrSetTitle pp g           = g { _rubyGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_rubyGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Ruby
  where attrSetTranslate pp g       = g { _rubyGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_rubyGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Ruby where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_rubyGlobals g)

-- | An instance.
instance A.AttrHasCustom Ruby where attrSetCustom pp g       = g { _rubyCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"ruby\" ... @
ruby' ∷ forall t m a. DomBuilder t m ⇒ Ruby → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ruby' bm  = elAttr' "ruby" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"ruby\" ... @
ruby ∷ forall t m a. DomBuilder t m ⇒ Ruby → m a → m a
ruby bm children = snd <$> ruby' bm children

-- | A short-hand notion for @ el\' \"ruby\" ... @
rubyN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
rubyN' = el' "ruby"

-- | A short-hand notion for @ el \"ruby\" ... @
rubyN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
rubyN children = snd <$> rubyN' children

-- | A short-hand notion for @ elDynAttr\' \"ruby\" ... @
rubyD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ruby → m a → m (Element EventResult (DomBuilderSpace m) t, a)
rubyD' bm  = elDynAttr' "ruby" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"ruby\" ... @
rubyD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ruby → m a → m a
rubyD bm children = snd <$> rubyD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | S-element has only the global A.attributes.
data S = S
  { _sGlobals ∷ Maybe A.Globals
  , _sCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap S where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _sGlobals bm
    ] <> maybeToList (_sCustom bm)

-- | A default value for S.
defS ∷ S
defS = S Nothing Nothing

-- | An instance.
instance Semigroup S where
  (<>) (S a1 a2) (S b1 b2) = S (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid S where
  mempty = defS
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals S where
   attrSetGlobals pp bm = bm { _sGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey S
  where attrSetAccessKey pp g = g { _sGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval S
  where attrSetAnmval pp g = g { _sGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable S
  where attrSetContentEditable pp g = g  { _sGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu S
  where attrSetContextMenu pp g     = g { _sGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasClass S
  where attrSetClassName pp g           = g { _sGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval S
  where attrSetDnmval pp g           = g { _sGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasDir S
  where attrSetDir pp g             = g { _sGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable S
  where attrSetDraggable pp g       = g { _sGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasHidden S
  where attrSetHidden pp g          = g { _sGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasId S
  where attrSetId pp g              = g { _sGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasLang S
  where attrSetLang pp g            = g { _sGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasRole S
  where attrSetRole pp g            = g { _sGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasSlot S
  where attrSetSlot pp g            = g { _sGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck S
  where attrSetSpellCheck pp g      = g { _sGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasStyle S
  where attrSetStyle pp g           = g { _sGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex S
  where attrSetTabIndex pp g        = g { _sGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasTitle S
  where attrSetTitle pp g           = g { _sGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_sGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate S
  where attrSetTranslate pp g       = g { _sGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_sGlobals g))) }

-- | An instance.
instance A.AttrGetClassName S where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_sGlobals g)

-- | An instance.
instance A.AttrHasCustom S where attrSetCustom pp g       = g { _sCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"s\" ... @
s' ∷ forall t m a. DomBuilder t m ⇒ S → m a → m (Element EventResult (DomBuilderSpace m) t, a)
s' bm  = elAttr' "s" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"s\" ... @
s ∷ forall t m a. DomBuilder t m ⇒ S → m a → m a
s bm children = snd <$> s' bm children

-- | A short-hand notion for @ el\' \"s\" ... @
sN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
sN' = el' "s"

-- | A short-hand notion for @ el \"s\" ... @
sN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
sN children = snd <$> sN' children

-- | A short-hand notion for @ elDynAttr\' \"s\" ... @
sD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t S → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sD' bm  = elDynAttr' "s" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"s\" ... @
sD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t S → m a → m a
sD bm children = snd <$> sD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Samp-element has only the global A.attributes.
data Samp = Samp
  { _sampGlobals ∷ Maybe A.Globals
  , _sampCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Samp where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _sampGlobals bm
    ] <> maybeToList (_sampCustom bm)

-- | A default value for Samp.
defSamp ∷ Samp
defSamp = Samp Nothing Nothing

-- | An instance.
instance Semigroup Samp where
  (<>) (Samp a1 a2) (Samp b1 b2) = Samp (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Samp where
  mempty = defSamp
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Samp where
   attrSetGlobals pp bm = bm { _sampGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Samp
  where attrSetAccessKey pp g = g { _sampGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Samp
  where attrSetAnmval pp g = g { _sampGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Samp
  where attrSetContentEditable pp g = g  { _sampGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Samp
  where attrSetContextMenu pp g     = g { _sampGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasClass Samp
  where attrSetClassName pp g           = g { _sampGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Samp
  where attrSetDnmval pp g           = g { _sampGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasDir Samp
  where attrSetDir pp g             = g { _sampGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Samp
  where attrSetDraggable pp g       = g { _sampGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Samp
  where attrSetHidden pp g          = g { _sampGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasId Samp
  where attrSetId pp g              = g { _sampGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasLang Samp
  where attrSetLang pp g            = g { _sampGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasRole Samp
  where attrSetRole pp g            = g { _sampGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Samp
  where attrSetSlot pp g            = g { _sampGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Samp
  where attrSetSpellCheck pp g      = g { _sampGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Samp
  where attrSetStyle pp g           = g { _sampGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Samp
  where attrSetTabIndex pp g        = g { _sampGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Samp
  where attrSetTitle pp g           = g { _sampGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_sampGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Samp
  where attrSetTranslate pp g       = g { _sampGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_sampGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Samp where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_sampGlobals g)

-- | An instance.
instance A.AttrHasCustom Samp where attrSetCustom pp g       = g { _sampCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"samp\" ... @
samp' ∷ forall t m a. DomBuilder t m ⇒ Samp → m a → m (Element EventResult (DomBuilderSpace m) t, a)
samp' bm  = elAttr' "samp" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"samp\" ... @
samp ∷ forall t m a. DomBuilder t m ⇒ Samp → m a → m a
samp bm children = snd <$> samp' bm children

-- | A short-hand notion for @ el\' \"samp\" ... @
sampN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
sampN' = el' "samp"

-- | A short-hand notion for @ el \"samp\" ... @
sampN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
sampN children = snd <$> sampN' children

-- | A short-hand notion for @ elDynAttr\' \"samp\" ... @
sampD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Samp → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sampD' bm  = elDynAttr' "samp" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"samp\" ... @
sampD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Samp → m a → m a
sampD bm children = snd <$> sampD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Small-element has only the global A.attributes.
data Small = Small
  { _smallGlobals ∷ Maybe A.Globals
  , _smallCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Small where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _smallGlobals bm
    ] <> maybeToList (_smallCustom bm)

-- | A default value for Small.
defSmall ∷ Small
defSmall = Small Nothing Nothing

-- | An instance.
instance Semigroup Small where
  (<>) (Small a1 a2) (Small b1 b2) = Small (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Small where
  mempty = defSmall
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Small where
   attrSetGlobals pp bm = bm { _smallGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Small
  where attrSetAccessKey pp g = g { _smallGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Small
  where attrSetAnmval pp g = g { _smallGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Small
  where attrSetContentEditable pp g = g  { _smallGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Small
  where attrSetContextMenu pp g     = g { _smallGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasClass Small
  where attrSetClassName pp g           = g { _smallGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Small
  where attrSetDnmval pp g           = g { _smallGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasDir Small
  where attrSetDir pp g             = g { _smallGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Small
  where attrSetDraggable pp g       = g { _smallGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Small
  where attrSetHidden pp g          = g { _smallGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasId Small
  where attrSetId pp g              = g { _smallGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasLang Small
  where attrSetLang pp g            = g { _smallGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasRole Small
  where attrSetRole pp g            = g { _smallGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Small
  where attrSetSlot pp g            = g { _smallGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Small
  where attrSetSpellCheck pp g      = g { _smallGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Small
  where attrSetStyle pp g           = g { _smallGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Small
  where attrSetTabIndex pp g        = g { _smallGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Small
  where attrSetTitle pp g           = g { _smallGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_smallGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Small
  where attrSetTranslate pp g       = g { _smallGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_smallGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Small where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_smallGlobals g)

-- | An instance.
instance A.AttrHasCustom Small where attrSetCustom pp g       = g { _smallCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"small\" ... @
small' ∷ forall t m a. DomBuilder t m ⇒ Small → m a → m (Element EventResult (DomBuilderSpace m) t, a)
small' bm  = elAttr' "small" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"small\" ... @
small ∷ forall t m a. DomBuilder t m ⇒ Small → m a → m a
small bm children = snd <$> small' bm children

-- | A short-hand notion for @ el\' \"small\" ... @
smallN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
smallN' = el' "small"

-- | A short-hand notion for @ el \"small\" ... @
smallN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
smallN children = snd <$> smallN' children

-- | A short-hand notion for @ elDynAttr\' \"small\" ... @
smallD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Small → m a → m (Element EventResult (DomBuilderSpace m) t, a)
smallD' bm  = elDynAttr' "small" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"small\" ... @
smallD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Small → m a → m a
smallD bm children = snd <$> smallD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Span-element has only the global A.attributes.
data Span = Span
  { _spanGlobals ∷ Maybe A.Globals
  , _spanCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Span where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _spanGlobals bm
    ] <> maybeToList (_spanCustom bm)

-- | A default value for Span.
defSpan ∷ Span
defSpan = Span Nothing Nothing

-- | An instance.
instance Semigroup Span where
  (<>) (Span a1 a2) (Span b1 b2) = Span (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Span where
  mempty = defSpan
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Span where
   attrSetGlobals pp bm = bm { _spanGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Span
  where attrSetAccessKey pp g = g { _spanGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Span
  where attrSetAnmval pp g = g { _spanGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Span
  where attrSetContentEditable pp g = g  { _spanGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Span
  where attrSetContextMenu pp g     = g { _spanGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasClass Span
  where attrSetClassName pp g           = g { _spanGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Span
  where attrSetDnmval pp g           = g { _spanGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasDir Span
  where attrSetDir pp g             = g { _spanGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Span
  where attrSetDraggable pp g       = g { _spanGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Span
  where attrSetHidden pp g          = g { _spanGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasId Span
  where attrSetId pp g              = g { _spanGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasLang Span
  where attrSetLang pp g            = g { _spanGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasRole Span
  where attrSetRole pp g            = g { _spanGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Span
  where attrSetSlot pp g            = g { _spanGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Span
  where attrSetSpellCheck pp g      = g { _spanGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Span
  where attrSetStyle pp g           = g { _spanGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Span
  where attrSetTabIndex pp g        = g { _spanGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Span
  where attrSetTitle pp g           = g { _spanGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_spanGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Span
  where attrSetTranslate pp g       = g { _spanGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_spanGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Span where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_spanGlobals g)

-- | An instance.
instance A.AttrHasCustom Span where attrSetCustom pp g       = g { _spanCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"span\" ... @
span' ∷ forall t m a. DomBuilder t m ⇒ Span → m a → m (Element EventResult (DomBuilderSpace m) t, a)
span' bm  = elAttr' "span" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"span\" ... @
span ∷ forall t m a. DomBuilder t m ⇒ Span → m a → m a
span bm children = snd <$> span' bm children

-- | A short-hand notion for @ el\' \"span\" ... @
spanN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
spanN' = el' "span"

-- | A short-hand notion for @ el \"span\" ... @
spanN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
spanN children = snd <$> spanN' children

-- | A short-hand notion for @ elDynAttr\' \"span\" ... @
spanD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Span → m a → m (Element EventResult (DomBuilderSpace m) t, a)
spanD' bm  = elDynAttr' "span" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"span\" ... @
spanD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Span → m a → m a
spanD bm children = snd <$> spanD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Strong-element has only the global A.attributes.
data Strong = Strong
  { _strongGlobals ∷ Maybe A.Globals
  , _strongCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Strong where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _strongGlobals bm
    ] <> maybeToList (_strongCustom bm)

-- | A default value for Strong.
defStrong ∷ Strong
defStrong = Strong Nothing Nothing

-- | An instance.
instance Semigroup Strong where
  (<>) (Strong a1 a2) (Strong b1 b2) = Strong (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Strong where
  mempty = defStrong
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Strong where
   attrSetGlobals pp bm = bm { _strongGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Strong
  where attrSetAccessKey pp g = g { _strongGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Strong
  where attrSetAnmval pp g = g { _strongGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Strong
  where attrSetContentEditable pp g = g  { _strongGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Strong
  where attrSetContextMenu pp g     = g { _strongGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasClass Strong
  where attrSetClassName pp g           = g { _strongGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Strong
  where attrSetDnmval pp g           = g { _strongGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasDir Strong
  where attrSetDir pp g             = g { _strongGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Strong
  where attrSetDraggable pp g       = g { _strongGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Strong
  where attrSetHidden pp g          = g { _strongGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasId Strong
  where attrSetId pp g              = g { _strongGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasLang Strong
  where attrSetLang pp g            = g { _strongGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasRole Strong
  where attrSetRole pp g            = g { _strongGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Strong
  where attrSetSlot pp g            = g { _strongGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Strong
  where attrSetSpellCheck pp g      = g { _strongGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Strong
  where attrSetStyle pp g           = g { _strongGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Strong
  where attrSetTabIndex pp g        = g { _strongGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Strong
  where attrSetTitle pp g           = g { _strongGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_strongGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Strong
  where attrSetTranslate pp g       = g { _strongGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_strongGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Strong where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_strongGlobals g)

-- | An instance.
instance A.AttrHasCustom Strong where attrSetCustom pp g       = g { _strongCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"strong\" ... @
strong' ∷ forall t m a. DomBuilder t m ⇒ Strong → m a → m (Element EventResult (DomBuilderSpace m) t, a)
strong' bm  = elAttr' "strong" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"strong\" ... @
strong ∷ forall t m a. DomBuilder t m ⇒ Strong → m a → m a
strong bm children = snd <$> strong' bm children

-- | A short-hand notion for @ el\' \"strong\" ... @
strongN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
strongN' = el' "strong"

-- | A short-hand notion for @ el \"strong\" ... @
strongN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
strongN children = snd <$> strongN' children

-- | A short-hand notion for @ elDynAttr\' \"strong\" ... @
strongD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Strong → m a → m (Element EventResult (DomBuilderSpace m) t, a)
strongD' bm  = elDynAttr' "strong" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"strong\" ... @
strongD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Strong → m a → m a
strongD bm children = snd <$> strongD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Sub-element has only the global A.attributes.
data Sub = Sub
  { _subGlobals ∷ Maybe A.Globals
  , _subCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Sub where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _subGlobals bm
    ] <> maybeToList (_subCustom bm)

-- | A default value for Sub.
defSub ∷ Sub
defSub = Sub Nothing Nothing

-- | An instance.
instance Semigroup Sub where
  (<>) (Sub a1 a2) (Sub b1 b2) = Sub (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Sub where
  mempty = defSub
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Sub where
   attrSetGlobals pp bm = bm { _subGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Sub
  where attrSetAccessKey pp g = g { _subGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Sub
  where attrSetAnmval pp g = g { _subGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Sub
  where attrSetContentEditable pp g = g  { _subGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Sub
  where attrSetContextMenu pp g     = g { _subGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasClass Sub
  where attrSetClassName pp g           = g { _subGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Sub
  where attrSetDnmval pp g           = g { _subGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasDir Sub
  where attrSetDir pp g             = g { _subGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Sub
  where attrSetDraggable pp g       = g { _subGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Sub
  where attrSetHidden pp g          = g { _subGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasId Sub
  where attrSetId pp g              = g { _subGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasLang Sub
  where attrSetLang pp g            = g { _subGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasRole Sub
  where attrSetRole pp g            = g { _subGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Sub
  where attrSetSlot pp g            = g { _subGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Sub
  where attrSetSpellCheck pp g      = g { _subGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Sub
  where attrSetStyle pp g           = g { _subGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Sub
  where attrSetTabIndex pp g        = g { _subGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Sub
  where attrSetTitle pp g           = g { _subGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_subGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Sub
  where attrSetTranslate pp g       = g { _subGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_subGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Sub where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_subGlobals g)

-- | An instance.
instance A.AttrHasCustom Sub where attrSetCustom pp g       = g { _subCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"sub\" ... @
sub' ∷ forall t m a. DomBuilder t m ⇒ Sub → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sub' bm  = elAttr' "sub" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"sub\" ... @
sub ∷ forall t m a. DomBuilder t m ⇒ Sub → m a → m a
sub bm children = snd <$> sub' bm children

-- | A short-hand notion for @ el\' \"sub\" ... @
subN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
subN' = el' "sub"

-- | A short-hand notion for @ el \"sub\" ... @
subN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
subN children = snd <$> subN' children

-- | A short-hand notion for @ elDynAttr\' \"sub\" ... @
subD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Sub → m a → m (Element EventResult (DomBuilderSpace m) t, a)
subD' bm  = elDynAttr' "sub" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"sub\" ... @
subD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Sub → m a → m a
subD bm children = snd <$> subD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Summary-element has only the global A.attributes.
data Summary = Summary
  { _summaryGlobals ∷ Maybe A.Globals
  , _summaryCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Summary where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _summaryGlobals bm
    ] <> maybeToList (_summaryCustom bm)

-- | A default value for Summary.
defSummary ∷ Summary
defSummary = Summary Nothing Nothing

-- | An instance.
instance Semigroup Summary where
  (<>) (Summary a1 a2) (Summary b1 b2) = Summary (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Summary where
  mempty = defSummary
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Summary where
   attrSetGlobals pp bm = bm { _summaryGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Summary
  where attrSetAccessKey pp g = g { _summaryGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Summary
  where attrSetAnmval pp g = g { _summaryGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Summary
  where attrSetContentEditable pp g = g  { _summaryGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Summary
  where attrSetContextMenu pp g     = g { _summaryGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasClass Summary
  where attrSetClassName pp g           = g { _summaryGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Summary
  where attrSetDnmval pp g           = g { _summaryGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasDir Summary
  where attrSetDir pp g             = g { _summaryGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Summary
  where attrSetDraggable pp g       = g { _summaryGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Summary
  where attrSetHidden pp g          = g { _summaryGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasId Summary
  where attrSetId pp g              = g { _summaryGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasLang Summary
  where attrSetLang pp g            = g { _summaryGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasRole Summary
  where attrSetRole pp g            = g { _summaryGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Summary
  where attrSetSlot pp g            = g { _summaryGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Summary
  where attrSetSpellCheck pp g      = g { _summaryGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Summary
  where attrSetStyle pp g           = g { _summaryGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Summary
  where attrSetTabIndex pp g        = g { _summaryGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Summary
  where attrSetTitle pp g           = g { _summaryGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_summaryGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Summary
  where attrSetTranslate pp g       = g { _summaryGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_summaryGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Summary where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_summaryGlobals g)

-- | An instance.
instance A.AttrHasCustom Summary where attrSetCustom pp g       = g { _summaryCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"summary\" ... @
summary' ∷ forall t m a. DomBuilder t m ⇒ Summary → m a → m (Element EventResult (DomBuilderSpace m) t, a)
summary' bm  = elAttr' "summary" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"summary\" ... @
summary ∷ forall t m a. DomBuilder t m ⇒ Summary → m a → m a
summary bm children = snd <$> summary' bm children

-- | A short-hand notion for @ el\' \"summary\" ... @
summaryN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
summaryN' = el' "summary"

-- | A short-hand notion for @ el \"summary\" ... @
summaryN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
summaryN children = snd <$> summaryN' children

-- | A short-hand notion for @ elDynAttr\' \"summary\" ... @
summaryD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Summary → m a → m (Element EventResult (DomBuilderSpace m) t, a)
summaryD' bm  = elDynAttr' "summary" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"summary\" ... @
summaryD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Summary → m a → m a
summaryD bm children = snd <$> summaryD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Sup-element has only the global A.attributes.
data Sup = Sup
  { _supGlobals ∷ Maybe A.Globals
  , _supCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Sup where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _supGlobals bm
    ] <> maybeToList (_supCustom bm)

-- | A default value for Sup.
defSup ∷ Sup
defSup = Sup Nothing Nothing

-- | An instance.
instance Semigroup Sup where
  (<>) (Sup a1 a2) (Sup b1 b2) = Sup (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Sup where
  mempty = defSup
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Sup where
   attrSetGlobals pp bm = bm { _supGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Sup
  where attrSetAccessKey pp g = g { _supGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Sup
  where attrSetAnmval pp g = g { _supGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Sup
  where attrSetContentEditable pp g = g  { _supGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Sup
  where attrSetContextMenu pp g     = g { _supGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasClass Sup
  where attrSetClassName pp g           = g { _supGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Sup
  where attrSetDnmval pp g           = g { _supGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasDir Sup
  where attrSetDir pp g             = g { _supGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Sup
  where attrSetDraggable pp g       = g { _supGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Sup
  where attrSetHidden pp g          = g { _supGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasId Sup
  where attrSetId pp g              = g { _supGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasLang Sup
  where attrSetLang pp g            = g { _supGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasRole Sup
  where attrSetRole pp g            = g { _supGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Sup
  where attrSetSlot pp g            = g { _supGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Sup
  where attrSetSpellCheck pp g      = g { _supGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Sup
  where attrSetStyle pp g           = g { _supGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Sup
  where attrSetTabIndex pp g        = g { _supGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Sup
  where attrSetTitle pp g           = g { _supGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_supGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Sup
  where attrSetTranslate pp g       = g { _supGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_supGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Sup where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_supGlobals g)

-- | An instance.
instance A.AttrHasCustom Sup where attrSetCustom pp g       = g { _supCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"sup\" ... @
sup' ∷ forall t m a. DomBuilder t m ⇒ Sup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
sup' bm  = elAttr' "sup" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"sup\" ... @
sup ∷ forall t m a. DomBuilder t m ⇒ Sup → m a → m a
sup bm children = snd <$> sup' bm children

-- | A short-hand notion for @ el\' \"sup\" ... @
supN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
supN' = el' "sup"

-- | A short-hand notion for @ el \"sup\" ... @
supN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
supN children = snd <$> supN' children

-- | A short-hand notion for @ elDynAttr\' \"sup\" ... @
supD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Sup → m a → m (Element EventResult (DomBuilderSpace m) t, a)
supD' bm  = elDynAttr' "sup" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"sup\" ... @
supD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Sup → m a → m a
supD bm children = snd <$> supD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Time-element
data Time = Time
  { _timeGlobals  ∷ Maybe A.Globals
  , _timeDateTime ∷ Maybe A.DateTime
  , _timeCustom   ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Time where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _timeGlobals bm
    , A.attrMap <$> _timeDateTime bm
    ] <> maybeToList (_timeCustom bm)

-- |  A default value for Time.
defTime ∷ Time
defTime = Time Nothing Nothing Nothing

-- | An instance.
instance Semigroup Time where
  (<>) (Time a1 a2 a3) (Time b1 b2 b3)
    = Time (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- | An instance.
instance Monoid Time where
  mempty = defTime
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Time where
   attrSetGlobals pp bm = bm { _timeGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Time
  where attrSetAccessKey pp g = g { _timeGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Time
  where attrSetAnmval pp g = g { _timeGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Time
  where attrSetContentEditable pp g = g  { _timeGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Time
  where attrSetContextMenu pp g     = g { _timeGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasClass Time
  where attrSetClassName pp g           = g { _timeGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Time
  where attrSetDnmval pp g           = g { _timeGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasDir Time
  where attrSetDir pp g             = g { _timeGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Time
  where attrSetDraggable pp g       = g { _timeGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Time
  where attrSetHidden pp g          = g { _timeGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasId Time
  where attrSetId pp g              = g { _timeGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasLang Time
  where attrSetLang pp g            = g { _timeGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasRole Time
  where attrSetRole pp g            = g { _timeGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Time
  where attrSetSlot pp g            = g { _timeGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Time
  where attrSetSpellCheck pp g      = g { _timeGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Time
  where attrSetStyle pp g           = g { _timeGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Time
  where attrSetTabIndex pp g        = g { _timeGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Time
  where attrSetTitle pp g           = g { _timeGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_timeGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Time
  where attrSetTranslate pp g       = g { _timeGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_timeGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Time where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_timeGlobals g)

-- | An instance.
instance A.AttrHasDateTime Time where attrSetDateTime pp g = g {_timeDateTime = Just pp }

-- | An instance.
instance A.AttrHasCustom Time where attrSetCustom pp g       = g { _timeCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"time\" ... @
time' ∷ forall t m a. DomBuilder t m ⇒ Time → m a → m (Element EventResult (DomBuilderSpace m) t, a)
time' bm  = elAttr' "time" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"time\" ... @
time ∷ forall t m a. DomBuilder t m ⇒ Time → m a → m a
time bm children = snd <$> time' bm children

-- | A short-hand notion for @ el\' \"time\" ... @
timeN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
timeN' = el' "time"

-- | A short-hand notion for @ el \"time\" ... @
timeN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
timeN children = snd <$> timeN' children

-- | A short-hand notion for @ elDynAttr\' \"time\" ... @
timeD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Time → m a → m (Element EventResult (DomBuilderSpace m) t, a)
timeD' bm  = elDynAttr' "time" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"time\" ... @
timeD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Time → m a → m a
timeD bm children = snd <$> timeD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | U-element has only the global A.attributes.
data U = U
  { _uGlobals ∷ Maybe A.Globals
  , _uCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap U where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _uGlobals bm
    ] <> maybeToList (_uCustom bm)

-- | A default value for U.
defU ∷ U
defU = U Nothing Nothing

-- | An instance.
instance Semigroup U where
  (<>) (U a1 a2) (U b1 b2) = U (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid U where
  mempty = defU
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals U where
   attrSetGlobals pp bm = bm { _uGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey U
  where attrSetAccessKey pp g = g { _uGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval U
  where attrSetAnmval pp g = g { _uGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable U
  where attrSetContentEditable pp g = g  { _uGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu U
  where attrSetContextMenu pp g     = g { _uGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasClass U
  where attrSetClassName pp g           = g { _uGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval U
  where attrSetDnmval pp g           = g { _uGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasDir U
  where attrSetDir pp g             = g { _uGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable U
  where attrSetDraggable pp g       = g { _uGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasHidden U
  where attrSetHidden pp g          = g { _uGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasId U
  where attrSetId pp g              = g { _uGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasLang U
  where attrSetLang pp g            = g { _uGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasRole U
  where attrSetRole pp g            = g { _uGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasSlot U
  where attrSetSlot pp g            = g { _uGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck U
  where attrSetSpellCheck pp g      = g { _uGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasStyle U
  where attrSetStyle pp g           = g { _uGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex U
  where attrSetTabIndex pp g        = g { _uGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasTitle U
  where attrSetTitle pp g           = g { _uGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_uGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate U
  where attrSetTranslate pp g       = g { _uGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_uGlobals g))) }

-- | An instance.
instance A.AttrGetClassName U where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_uGlobals g)

-- | An instance.
instance A.AttrHasCustom U where attrSetCustom pp g       = g { _uCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"u\" ... @
u' ∷ forall t m a. DomBuilder t m ⇒ U → m a → m (Element EventResult (DomBuilderSpace m) t, a)
u' bm  = elAttr' "u" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"u\" ... @
u ∷ forall t m a. DomBuilder t m ⇒ U → m a → m a
u bm children = snd <$> u' bm children

-- | A short-hand notion for @ el\' \"u\" ... @
uN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
uN' = el' "u"

-- | A short-hand notion for @ el \"u\" ... @
uN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
uN children = snd <$> uN' children

-- | A short-hand notion for @ elDynAttr\' \"u\" ... @
uD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t U → m a → m (Element EventResult (DomBuilderSpace m) t, a)
uD' bm  = elDynAttr' "u" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"u\" ... @
uD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t U → m a → m a
uD bm children = snd <$> uD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ul-element has only the global A.attributes.
data Ul = Ul
  { _ulGlobals ∷ Maybe A.Globals
  , _ulCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Ul where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _ulGlobals bm
    ] <> maybeToList (_ulCustom bm)

-- | A default value for Ul.
defUl ∷ Ul
defUl = Ul Nothing Nothing

-- | An instance.
instance Semigroup Ul where
  (<>) (Ul a1 a2) (Ul b1 b2) = Ul (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Ul where
  mempty = defUl
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Ul where
   attrSetGlobals pp bm = bm { _ulGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Ul
  where attrSetAccessKey pp g = g { _ulGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Ul
  where attrSetAnmval pp g = g { _ulGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Ul
  where attrSetContentEditable pp g = g  { _ulGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Ul
  where attrSetContextMenu pp g     = g { _ulGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasClass Ul
  where attrSetClassName pp g           = g { _ulGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Ul
  where attrSetDnmval pp g           = g { _ulGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasDir Ul
  where attrSetDir pp g             = g { _ulGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Ul
  where attrSetDraggable pp g       = g { _ulGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Ul
  where attrSetHidden pp g          = g { _ulGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasId Ul
  where attrSetId pp g              = g { _ulGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasLang Ul
  where attrSetLang pp g            = g { _ulGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasRole Ul
  where attrSetRole pp g            = g { _ulGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Ul
  where attrSetSlot pp g            = g { _ulGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Ul
  where attrSetSpellCheck pp g      = g { _ulGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Ul
  where attrSetStyle pp g           = g { _ulGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Ul
  where attrSetTabIndex pp g        = g { _ulGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Ul
  where attrSetTitle pp g           = g { _ulGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_ulGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Ul
  where attrSetTranslate pp g       = g { _ulGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_ulGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Ul where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_ulGlobals g)

-- | An instance.
instance A.AttrHasCustom Ul where attrSetCustom pp g       = g { _ulCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"ul\" ... @
ul' ∷ forall t m a. DomBuilder t m ⇒ Ul → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ul' bm  = elAttr' "ul" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"ul\" ... @
ul ∷ forall t m a. DomBuilder t m ⇒ Ul → m a → m a
ul bm children = snd <$> ul' bm children

-- | A short-hand notion for @ el\' \"ul\" ... @
ulN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
ulN' = el' "ul"

-- | A short-hand notion for @ el \"ul\" ... @
ulN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
ulN children = snd <$> ulN' children

-- | A short-hand notion for @ elDynAttr\' \"ul\" ... @
ulD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ul → m a → m (Element EventResult (DomBuilderSpace m) t, a)
ulD' bm  = elDynAttr' "ul" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"ul\" ... @
ulD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Ul → m a → m a
ulD bm children = snd <$> ulD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Var-element has only the global A.attributes.
data Var = Var
  { _varGlobals ∷ Maybe A.Globals
  , _varCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Var where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _varGlobals bm
    ] <> maybeToList (_varCustom bm)

-- | A default value for Var.
defVar ∷ Var
defVar = Var Nothing Nothing

-- | An instance.
instance Semigroup Var where
  (<>) (Var a1 a2) (Var b1 b2) = Var (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Var where
  mempty = defVar
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Var where
   attrSetGlobals pp bm = bm { _varGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Var
  where attrSetAccessKey pp g = g { _varGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Var
  where attrSetAnmval pp g = g { _varGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Var
  where attrSetContentEditable pp g = g  { _varGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Var
  where attrSetContextMenu pp g     = g { _varGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasClass Var
  where attrSetClassName pp g           = g { _varGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Var
  where attrSetDnmval pp g           = g { _varGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasDir Var
  where attrSetDir pp g             = g { _varGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Var
  where attrSetDraggable pp g       = g { _varGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Var
  where attrSetHidden pp g          = g { _varGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasId Var
  where attrSetId pp g              = g { _varGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasLang Var
  where attrSetLang pp g            = g { _varGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasRole Var
  where attrSetRole pp g            = g { _varGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Var
  where attrSetSlot pp g            = g { _varGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Var
  where attrSetSpellCheck pp g      = g { _varGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Var
  where attrSetStyle pp g           = g { _varGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Var
  where attrSetTabIndex pp g        = g { _varGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Var
  where attrSetTitle pp g           = g { _varGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_varGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Var
  where attrSetTranslate pp g       = g { _varGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_varGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Var where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_varGlobals g)

-- | An instance.
instance A.AttrHasCustom Var where attrSetCustom pp g       = g { _varCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"var\" ... @
var' ∷ forall t m a. DomBuilder t m ⇒ Var → m a → m (Element EventResult (DomBuilderSpace m) t, a)
var' bm  = elAttr' "var" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"var\" ... @
var ∷ forall t m a. DomBuilder t m ⇒ Var → m a → m a
var bm children = snd <$> var' bm children

-- | A short-hand notion for @ el\' \"var\" ... @
varN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
varN' = el' "var"

-- | A short-hand notion for @ el \"var\" ... @
varN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
varN children = snd <$> varN' children

-- | A short-hand notion for @ elDynAttr\' \"var\" ... @
varD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Var → m a → m (Element EventResult (DomBuilderSpace m) t, a)
varD' bm  = elDynAttr' "var" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"var\" ... @
varD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Var → m a → m a
varD bm children = snd <$> varD' bm children

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Wbr-element has only the global A.attributes.
data Wbr = Wbr
  { _wbrGlobals ∷ Maybe A.Globals
  , _wbrCustom  ∷ Maybe A.Attr
  }


-- | An instance.
instance A.AttrMap Wbr where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _wbrGlobals bm
    ] <> maybeToList (_wbrCustom bm)

-- | A default value for Wbr.
defWbr ∷ Wbr
defWbr = Wbr Nothing Nothing

-- | An instance.
instance Semigroup Wbr where
  (<>) (Wbr a1 a2) (Wbr b1 b2) = Wbr (a1 <> b1) (a2 <> b2)

-- | An instance.
instance Monoid Wbr where
  mempty = defWbr
  mappend = (<>)

-- | An instance.
instance A.AttrHasGlobals Wbr where
   attrSetGlobals pp bm = bm { _wbrGlobals = Just pp }

-- Global A.attributes require the following instances.
-- | An instance.
instance A.AttrHasAccessKey Wbr
  where attrSetAccessKey pp g = g { _wbrGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasAnmval Wbr
  where attrSetAnmval pp g = g { _wbrGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasContentEditable Wbr
  where attrSetContentEditable pp g = g  { _wbrGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasContextMenu Wbr
  where attrSetContextMenu pp g     = g { _wbrGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasClass Wbr
  where attrSetClassName pp g           = g { _wbrGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasDnmval Wbr
  where attrSetDnmval pp g           = g { _wbrGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasDir Wbr
  where attrSetDir pp g             = g { _wbrGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasDraggable Wbr
  where attrSetDraggable pp g       = g { _wbrGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasHidden Wbr
  where attrSetHidden pp g          = g { _wbrGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasId Wbr
  where attrSetId pp g              = g { _wbrGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasLang Wbr
  where attrSetLang pp g            = g { _wbrGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasRole Wbr
  where attrSetRole pp g            = g { _wbrGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasSlot Wbr
  where attrSetSlot pp g            = g { _wbrGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasSpellCheck Wbr
  where attrSetSpellCheck pp g      = g { _wbrGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasStyle Wbr
  where attrSetStyle pp g           = g { _wbrGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasTabIndex Wbr
  where attrSetTabIndex pp g        = g { _wbrGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasTitle Wbr
  where attrSetTitle pp g           = g { _wbrGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_wbrGlobals g))) }
-- | An instance.
instance A.AttrHasTranslate Wbr
  where attrSetTranslate pp g       = g { _wbrGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_wbrGlobals g))) }

-- | An instance.
instance A.AttrGetClassName Wbr where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_wbrGlobals g)

-- | An instance.
instance A.AttrHasCustom Wbr where attrSetCustom pp g       = g { _wbrCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"wbr\" ... @
wbr' ∷ forall t m a. DomBuilder t m ⇒ Wbr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
wbr' bm  = elAttr' "wbr" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"wbr\" ... @
wbr ∷ forall t m a. DomBuilder t m ⇒ Wbr → m a → m a
wbr bm children = snd <$> wbr' bm children

-- | A short-hand notion for @ el\' \"wbr\" ... @
wbrN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
wbrN' = el' "wbr"

-- | A short-hand notion for @ el \"wbr\" ... @
wbrN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
wbrN children = snd <$> wbrN' children

-- | A short-hand notion for @ elDynAttr\' \"wbr\" ... @
wbrD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Wbr → m a → m (Element EventResult (DomBuilderSpace m) t, a)
wbrD' bm  = elDynAttr' "wbr" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"wbr\" ... @
wbrD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Wbr → m a → m a
wbrD bm children = snd <$> wbrD' bm children

wbr_ ∷ forall t m. DomBuilder t m ⇒ m ()
wbr_ = wbr defWbr blank

------------------------------------------------------------------------------
------------------------------------------------------------------------------

