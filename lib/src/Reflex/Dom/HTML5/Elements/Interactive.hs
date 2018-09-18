{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
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

    * aC - takes attributes and 'm ()', gives 'm (Event t ())'
    * aCD - takes dynamic attributes and 'm ()', gives 'm (Event t ())'


TBD: are the inputs and outputs ok and should something similar
be provided for all elements as the tabindex-attribute can make any
element interactive. (hmm)

'Label' has 'labelCForId' convenience function.


== Note

You probably don't want to use these definitions but rather the
widgets defined either in reflex-dom or reflex-dom-contrib (or in
some other) library.

(Occasionally, the elements given in this module are convenient,
anyway.)

-}

module Reflex.Dom.HTML5.Elements.Interactive where

import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
                       DomBuilderSpace, PostBuild, elAttr',
                       elDynAttr', Event, EventName (Click),
                       domEvent, blank, el')
import qualified Reflex.Dom.HTML5.Attrs as A 


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
data A = A
  { _aGlobals   ∷ Maybe A.Globals
  , _aDownload  ∷ Maybe A.Download
  , _aHref      ∷ Maybe A.Href
  , _aHrefLang  ∷ Maybe A.HrefLang
  , _aRel       ∷ Maybe A.Rel
  , _aTarget    ∷ Maybe A.Target
  , _aMediaType ∷ Maybe A.MediaType
  , _aCustom    ∷ Maybe A.Attr
  }

instance A.AttrMap A where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _aGlobals bm
    , A.attrMap <$> _aDownload bm
    , A.attrMap <$> _aHref bm
    , A.attrMap <$> _aHrefLang bm
    , A.attrMap <$> _aRel bm
    , A.attrMap <$> _aTarget bm
    , A.attrMap <$> _aMediaType bm
    ] <> maybeToList (_aCustom bm)


defA ∷ A
defA = A Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid A where
  mempty = defA
  mappend (A a1 a2 a3 a4 a5 a6 a7 a8) (A b1 b2 b3 b4 b5 b6 b7 b8)
    = A (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8)


instance A.AttrHasGlobals A where
   attrSetGlobals pp bm = bm { _aGlobals = Just pp }


-- Global attributes require the following instances.
instance A.AttrHasAccessKey A
  where attrSetAccessKey pp g = g { _aGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasAnmval A
  where attrSetAnmval pp g = g { _aGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasContentEditable A
  where attrSetContentEditable pp g = g  { _aGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasContextMenu A
  where attrSetContextMenu pp g     = g { _aGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasClass A
  where attrSetClassName pp g           = g { _aGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasDnmval A
  where attrSetDnmval pp g           = g { _aGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasDir A
  where attrSetDir pp g             = g { _aGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasDraggable A
  where attrSetDraggable pp g       = g { _aGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasHidden A
  where attrSetHidden pp g          = g { _aGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasId A
  where attrSetId pp g              = g { _aGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasLang A
  where attrSetLang pp g            = g { _aGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasRole A
  where attrSetRole pp g            = g { _aGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasSlot A
  where attrSetSlot pp g            = g { _aGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasSpellCheck A
  where attrSetSpellCheck pp g      = g { _aGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasStyle A
  where attrSetStyle pp g           = g { _aGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasTabIndex A
  where attrSetTabIndex pp g        = g { _aGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasTitle A
  where attrSetTitle pp g           = g { _aGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_aGlobals g))) }
instance A.AttrHasTranslate A
  where attrSetTranslate pp g       = g { _aGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_aGlobals g))) }

instance A.AttrGetClassName A where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_aGlobals g)

instance A.AttrHasDownload  A where attrSetDownload pp g  = g { _aDownload  = Just pp }
instance A.AttrHasHref      A where attrSetHref pp g      = g { _aHref      = Just pp }
instance A.AttrHasHrefLang  A where attrSetHrefLang pp g  = g { _aHrefLang  = Just pp }
instance A.AttrHasRel       A where attrSetRel pp g       = g { _aRel       = Just pp }
instance A.AttrHasTarget    A where attrSetTarget pp g    = g { _aTarget    = Just pp }
instance A.AttrHasMediaType A where attrSetMediaType pp g = g { _aMediaType = Just pp }

instance A.AttrHasCustom A where attrSetCustom pp g       = g { _aCustom = Just pp }

-- instance (Reflex t, AttrHasAccessKey a) ⇒ AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (A.attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) ⇒ AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (A.attrSetAnmval c)

-- | A short-hand notion for @ elAttr\' \"a\" ... @
a' ∷ forall t m a. DomBuilder t m ⇒ A → m a → m (Element EventResult (DomBuilderSpace m) t, a)
a' bm  = elAttr' "a" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"a\" ... @
a ∷ forall t m a. DomBuilder t m ⇒ A → m a → m a
a bm children = snd <$> a' bm children

-- | A short-hand notion for @ el\' \"a\" ... @
aN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
aN' = el' "a"

-- | A short-hand notion for @ el \"a\" ... @
aN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
aN children = snd <$> aN' children

-- | A short-hand notion for @ elDynAttr\' \"a\" ... @
aD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t A → m a → m (Element EventResult (DomBuilderSpace m) t, a)
aD' bm  = elDynAttr' "a" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"a\" ... @
aD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t A → m a → m a
aD bm children = snd <$> aD' bm children


aC ∷ (DomBuilder t m, PostBuild t m) ⇒ A → m () → m (Event t ())
aC bm children = do
  (e,_) ← elAttr' "a" (A.attrMap bm) children
  return $ domEvent Click e
aCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t A → m () → m (Event t ())
aCD bDyn children = do
  (e,_) ← elDynAttr' "a" (A.attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Button-element has the following attributes:
-- autofocus; disabled; form; formaction; formenctype; formmethod;
-- formnovalidate; formtarget; name; type; value
--
-- Value of button is Text (inside Value_).
--
data Button = Button
  { _buttonGlobals        ∷ Maybe A.Globals
  , _buttonAutoFocus      ∷ Maybe A.AutoFocus
  , _buttonDisabled       ∷ Maybe A.Disabled
  , _buttonForm           ∷ Maybe A.Form
  , _buttonFormAction     ∷ Maybe A.FormAction
  , _buttonFormEncType    ∷ Maybe A.FormEncType
  , _buttonFormNoValidate ∷ Maybe A.FormNoValidate
  , _buttonFormTarget     ∷ Maybe A.FormTarget
  , _buttonName           ∷ Maybe A.Name
  , _buttonType           ∷ Maybe A.ButtonType
  , _buttonValueText      ∷ Maybe A.ValueText
  , _buttonCustom         ∷ Maybe A.Attr
  }


instance A.AttrMap Button where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _buttonGlobals bm
    , A.attrMap <$> _buttonAutoFocus bm
    , A.attrMap <$> _buttonDisabled bm
    , A.attrMap <$> _buttonForm bm
    , A.attrMap <$> _buttonFormAction bm
    , A.attrMap <$> _buttonFormEncType bm
    , A.attrMap <$> _buttonFormNoValidate bm
    , A.attrMap <$> _buttonFormTarget bm
    , A.attrMap <$> _buttonName bm
    , A.attrMap <$> _buttonType bm
    , A.attrMap <$> _buttonValueText bm
    ] <> maybeToList (_buttonCustom bm)

defButton ∷ Button
defButton = Button Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid Button where
  mempty = defButton
  mappend (Button a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
          (Button b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = Button (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <>b5) (a6 <> b6)
        (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)


instance A.AttrHasGlobals Button where
   attrSetGlobals pp bm = bm { _buttonGlobals = Just pp }


-- Global attributes require the following instances.
instance A.AttrHasAccessKey Button
  where attrSetAccessKey pp g = g { _buttonGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasAnmval Button
  where attrSetAnmval pp g = g { _buttonGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasContentEditable Button
  where attrSetContentEditable pp g = g  { _buttonGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasContextMenu Button
  where attrSetContextMenu pp g     = g { _buttonGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasClass Button
  where attrSetClassName pp g           = g { _buttonGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasDnmval Button
  where attrSetDnmval pp g           = g { _buttonGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasDir Button
  where attrSetDir pp g             = g { _buttonGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasDraggable Button
  where attrSetDraggable pp g       = g { _buttonGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasHidden Button
  where attrSetHidden pp g          = g { _buttonGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasId Button
  where attrSetId pp g              = g { _buttonGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasLang Button
  where attrSetLang pp g            = g { _buttonGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasRole Button
  where attrSetRole pp g            = g { _buttonGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasSlot Button
  where attrSetSlot pp g            = g { _buttonGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasSpellCheck Button
  where attrSetSpellCheck pp g      = g { _buttonGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasStyle Button
  where attrSetStyle pp g           = g { _buttonGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasTabIndex Button
  where attrSetTabIndex pp g        = g { _buttonGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasTitle Button
  where attrSetTitle pp g           = g { _buttonGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_buttonGlobals g))) }
instance A.AttrHasTranslate Button
  where attrSetTranslate pp g       = g { _buttonGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_buttonGlobals g))) }

instance A.AttrGetClassName Button where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_buttonGlobals g)


instance A.AttrHasAutoFocus      Button where attrSetAutoFocus pp g   = g { _buttonAutoFocus   = Just pp }
instance A.AttrHasDisabled       Button where attrSetDisabled pp g    = g { _buttonDisabled    = Just pp }
instance A.AttrHasForm           Button where attrSetForm pp g        = g { _buttonForm        = Just pp }
instance A.AttrHasFormAction     Button where attrSetFormAction pp g  = g { _buttonFormAction  = Just pp }
instance A.AttrHasFormEncType    Button where attrSetFormEncType pp g = g { _buttonFormEncType = Just pp }
instance A.AttrHasFormNoValidate Button where attrSetFormNoValidate pp g  = g { _buttonFormNoValidate  = Just pp }
instance A.AttrHasFormTarget     Button where attrSetFormTarget pp g  = g { _buttonFormTarget  = Just pp }
instance A.AttrHasName           Button where attrSetName pp g        = g { _buttonName        = Just pp }
instance A.AttrHasButtonType     Button where attrSetButtonType pp g  = g { _buttonType        = Just pp }
instance A.AttrHasValueText      Button where attrSetValueText pp g       = g { _buttonValueText   = Just pp }

instance A.AttrHasCustom Button where attrSetCustom pp g       = g { _buttonCustom = Just pp }

-- instance (Reflex t, AttrHasAccessKey a) ⇒ AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (A.attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) ⇒ AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (A.attrSetAnmval c)

-- | A short-hand notion for @ elAttr\' \"button\" ... @
button' ∷ forall t m a. DomBuilder t m ⇒ Button → m a → m (Element EventResult (DomBuilderSpace m) t, a)
button' bm  = elAttr' "button" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"button\" ... @
button ∷ forall t m a. DomBuilder t m ⇒ Button → m a → m a
button bm children = snd <$> button' bm children

-- | A short-hand notion for @ el\' \"button\" ... @
buttonN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
buttonN' = el' "button"

-- | A short-hand notion for @ el \"button\" ... @
buttonN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
buttonN children = snd <$> buttonN' children

-- | A short-hand notion for @ elDynAttr\' \"button\" ... @
buttonD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Button → m a → m (Element EventResult (DomBuilderSpace m) t, a)
buttonD' bm  = elDynAttr' "button" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"button\" ... @
buttonD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Button → m a → m a
buttonD bm children = snd <$> buttonD' bm children


buttonC ∷ (DomBuilder t m, PostBuild t m) ⇒ Button → m () → m (Event t ())
buttonC bm children = do
  (e,_) ← elAttr' "button" (A.attrMap bm) children
  return $ domEvent Click e

buttonCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Button → m () → m (Event t ())
buttonCD bDyn children = do
  (e,_) ← elDynAttr' "button" (A.attrMap <$> bDyn) children
  return $ domEvent Click e


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Details-element
data Details = Details
  { _detailsGlobals ∷ Maybe A.Globals
  , _detailsOpen    ∷ Maybe A.Open
  , _detailsCustom  ∷ Maybe A.Attr
  }


instance A.AttrMap Details where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _detailsGlobals bm
    , A.attrMap <$> _detailsOpen bm
    ] <> maybeToList (_detailsCustom bm)

defDetails ∷ Details
defDetails = Details Nothing Nothing Nothing

instance Monoid Details where
  mempty = defDetails
  mappend (Details a1 a2 a3) (Details b1 b2 b3)
    = Details (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance A.AttrHasGlobals Details where
   attrSetGlobals pp bm = bm { _detailsGlobals = Just pp }

-- Global attributes require the following instances.
instance A.AttrHasAccessKey Details
  where attrSetAccessKey pp g = g { _detailsGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasAnmval Details
  where attrSetAnmval pp g = g { _detailsGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasContentEditable Details
  where attrSetContentEditable pp g = g  { _detailsGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasContextMenu Details
  where attrSetContextMenu pp g     = g { _detailsGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasClass Details
  where attrSetClassName pp g           = g { _detailsGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasDnmval Details
  where attrSetDnmval pp g           = g { _detailsGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasDir Details
  where attrSetDir pp g             = g { _detailsGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasDraggable Details
  where attrSetDraggable pp g       = g { _detailsGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasHidden Details
  where attrSetHidden pp g          = g { _detailsGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasId Details
  where attrSetId pp g              = g { _detailsGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasLang Details
  where attrSetLang pp g            = g { _detailsGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasRole Details
  where attrSetRole pp g            = g { _detailsGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasSlot Details
  where attrSetSlot pp g            = g { _detailsGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasSpellCheck Details
  where attrSetSpellCheck pp g      = g { _detailsGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasStyle Details
  where attrSetStyle pp g           = g { _detailsGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasTabIndex Details
  where attrSetTabIndex pp g        = g { _detailsGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasTitle Details
  where attrSetTitle pp g           = g { _detailsGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_detailsGlobals g))) }
instance A.AttrHasTranslate Details
  where attrSetTranslate pp g       = g { _detailsGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_detailsGlobals g))) }

instance A.AttrGetClassName Details where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_detailsGlobals g)

instance A.AttrHasOpen Details where attrSetOpen pp g = g {_detailsOpen = Just pp }

instance A.AttrHasCustom Details where attrSetCustom pp g       = g { _detailsCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"details\" ... @
details' ∷ forall t m a. DomBuilder t m ⇒ Details → m a → m (Element EventResult (DomBuilderSpace m) t, a)
details' bm  = elAttr' "details" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"details\" ... @
details ∷ forall t m a. DomBuilder t m ⇒ Details → m a → m a
details bm children = snd <$> details' bm children

-- | A short-hand notion for @ el\' \"details\" ... @
detailsN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
detailsN' = el' "details"

-- | A short-hand notion for @ el \"details\" ... @
detailsN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
detailsN children = snd <$> detailsN' children

-- | A short-hand notion for @ elDynAttr\' \"details\" ... @
detailsD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Details → m a → m (Element EventResult (DomBuilderSpace m) t, a)
detailsD' bm  = elDynAttr' "details" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"details\" ... @
detailsD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Details → m a → m a
detailsD bm children = snd <$> detailsD' bm children


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
data Embed = Embed
  { _embedGlobals   ∷ Maybe A.Globals
  , _embedHeight    ∷ Maybe A.Height
  , _embedMediaType ∷ Maybe A.MediaType
  , _embedSrc       ∷ Maybe A.Src
  , _embedWidth     ∷ Maybe A.Width
  , _embedCustom    ∷ Maybe A.Attr
  }


instance A.AttrMap Embed where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _embedGlobals bm
    , A.attrMap <$> _embedHeight bm
    , A.attrMap <$> _embedMediaType bm
    , A.attrMap <$> _embedSrc bm
    , A.attrMap <$> _embedWidth bm
    ] <> maybeToList (_embedCustom bm)

defEmbed ∷ Embed
defEmbed = Embed Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid Embed where
  mempty = defEmbed
  mappend (Embed a1 a2 a3 a4 a5 a6) (Embed b1 b2 b3 b4 b5 b6)
    = Embed (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)


instance A.AttrHasGlobals Embed where
   attrSetGlobals pp bm = bm { _embedGlobals = Just pp }


-- Global attributes require the following instances.
instance A.AttrHasAccessKey Embed
  where attrSetAccessKey pp g = g { _embedGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasAnmval Embed
  where attrSetAnmval pp g = g { _embedGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasContentEditable Embed
  where attrSetContentEditable pp g = g  { _embedGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasContextMenu Embed
  where attrSetContextMenu pp g     = g { _embedGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasClass Embed
  where attrSetClassName pp g           = g { _embedGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasDnmval Embed
  where attrSetDnmval pp g           = g { _embedGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasDir Embed
  where attrSetDir pp g             = g { _embedGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasDraggable Embed
  where attrSetDraggable pp g       = g { _embedGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasHidden Embed
  where attrSetHidden pp g          = g { _embedGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasId Embed
  where attrSetId pp g              = g { _embedGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasLang Embed
  where attrSetLang pp g            = g { _embedGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasRole Embed
  where attrSetRole pp g            = g { _embedGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasSlot Embed
  where attrSetSlot pp g            = g { _embedGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasSpellCheck Embed
  where attrSetSpellCheck pp g      = g { _embedGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasStyle Embed
  where attrSetStyle pp g           = g { _embedGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasTabIndex Embed
  where attrSetTabIndex pp g        = g { _embedGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasTitle Embed
  where attrSetTitle pp g           = g { _embedGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_embedGlobals g))) }
instance A.AttrHasTranslate Embed
  where attrSetTranslate pp g       = g { _embedGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_embedGlobals g))) }

instance A.AttrGetClassName Embed where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_embedGlobals g)

instance A.AttrHasHeight    Embed where attrSetHeight pp g    = g { _embedHeight    = Just pp }
instance A.AttrHasMediaType Embed where attrSetMediaType pp g = g { _embedMediaType = Just pp }
instance A.AttrHasSrc       Embed where attrSetSrc pp g       = g { _embedSrc       = Just pp }
instance A.AttrHasWidth     Embed where attrSetWidth pp g     = g { _embedWidth     = Just pp }

instance A.AttrHasCustom Embed where attrSetCustom pp g       = g { _embedCustom = Just pp }

-- instance (Reflex t, AttrHasAccessKey a) ⇒ AttrHasAccessKey (Dynamic t a) where
--   attrSetAccessKey c = fmap (A.attrSetAccessKey c)
-- instance (Reflex t, AttrHasAnmval a) ⇒ AttrHasAnmval (Dynamic t a) where
--   attrSetAnmval c = fmap (A.attrSetAnmval c)

-- | A short-hand notion for @ elAttr\' \"embed\" ... @
embed' ∷ forall t m a. DomBuilder t m ⇒ Embed → m a → m (Element EventResult (DomBuilderSpace m) t, a)
embed' bm  = elAttr' "embed" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"embed\" ... @
embed ∷ forall t m a. DomBuilder t m ⇒ Embed → m a → m a
embed bm children = snd <$> embed' bm children

-- | A short-hand notion for @ el\' \"embed\" ... @
embedN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
embedN' = el' "embed"

-- | A short-hand notion for @ el \"embed\" ... @
embedN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
embedN children = snd <$> embedN' children

-- | A short-hand notion for @ elDynAttr\' \"embed\" ... @
embedD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Embed → m a → m (Element EventResult (DomBuilderSpace m) t, a)
embedD' bm  = elDynAttr' "embed" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"embed\" ... @
embedD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Embed → m a → m a
embedD bm children = snd <$> embedD' bm children


-- uiEmbedC ∷ (DomBuilder t m, PostBuild t m) ⇒ Embed → m () → m (Event t ())
-- uiEmbedC bm children = do
embedC ∷ (DomBuilder t m, PostBuild t m) ⇒ Embed → m (Event t ())
embedC bm = do
  (e,_) ← elAttr' "embed" (A.attrMap bm) blank
  return $ domEvent Click e

-- uiEmbedD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Embed → m () → m (Event t ())
-- uiEmbedD bDyn children = do
embedCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Embed → m (Event t ())
embedCD bDyn = do
  (e,_) ← elDynAttr' "embed" (A.attrMap <$> bDyn) blank
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
data Input = Input
  { _inputGlobals        ∷ Maybe A.Globals        -- 1.
  , _inputAccept         ∷ Maybe A.Accept         -- 2.
  , _inputAlt            ∷ Maybe A.Alt_           -- 3.
  , _inputAutoComplete   ∷ Maybe A.AutoComplete   -- 4.
  , _inputAutoFocus      ∷ Maybe A.AutoFocus      -- 5.
  , _inputChecked        ∷ Maybe A.Checked        -- 6.
  , _inputDirName        ∷ Maybe A.DirName        -- 7.
  , _inputDisabled       ∷ Maybe A.Disabled       -- 8.
  , _inputForm           ∷ Maybe A.Form         -- 9.
  , _inputFormAction     ∷ Maybe A.FormAction     -- 10.
  , _inputFormEncType    ∷ Maybe A.FormEncType    -- 11.
  , _inputFormMethod     ∷ Maybe A.FormMethod     -- 12.
  , _inputFormNoValidate ∷ Maybe A.FormNoValidate -- 13.
  , _inputFormTarget     ∷ Maybe A.FormTarget     -- 14.
  , _inputHeight         ∷ Maybe A.Height         -- 15.
  , _inputInputMode      ∷ Maybe A.InputMode      -- 16.
  , _inputList           ∷ Maybe A.List           -- 17.
  , _inputMax            ∷ Maybe A.Max            -- 18.
  , _inputMaxLength      ∷ Maybe A.MaxLength      -- 19.
  , _inputMin            ∷ Maybe A.Min            -- 20.
  , _inputMinLength      ∷ Maybe A.MinLength      -- 21.
  , _inputMultiple       ∷ Maybe A.Multiple       -- 22.
  , _inputName           ∷ Maybe A.Name           -- 23.
  , _inputPattern        ∷ Maybe A.Pattern        -- 24.
  , _inputPlaceholder    ∷ Maybe A.Placeholder    -- 25.
  , _inputReadOnly       ∷ Maybe A.ReadOnly       -- 26.
  , _inputRequired       ∷ Maybe A.Required       -- 27.
  , _inputSize           ∷ Maybe A.Size           -- 28.
  , _inputSrc            ∷ Maybe A.Src            -- 29.
  , _inputStep           ∷ Maybe A.Step           -- 30.
  , _inputType           ∷ Maybe A.InputType      -- 31.
  , _inputValueText      ∷ Maybe A.ValueText      -- 32.
  , _inputWidth          ∷ Maybe A.Width          -- 33.
  , _inputCustom         ∷ Maybe A.Attr           -- 34.
  }



instance A.AttrMap Input where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _inputGlobals bm
    , A.attrMap <$> _inputAccept bm
    , A.attrMap <$> _inputAlt bm
    , A.attrMap <$> _inputAutoComplete bm
    , A.attrMap <$> _inputAutoFocus bm
    , A.attrMap <$> _inputChecked bm
    , A.attrMap <$> _inputDirName bm
    , A.attrMap <$> _inputDisabled bm
    , A.attrMap <$> _inputForm bm
    , A.attrMap <$> _inputFormAction bm
    , A.attrMap <$> _inputFormEncType bm
    , A.attrMap <$> _inputFormMethod bm
    , A.attrMap <$> _inputFormNoValidate bm
    , A.attrMap <$> _inputFormTarget bm
    , A.attrMap <$> _inputHeight bm
    , A.attrMap <$> _inputInputMode bm
    , A.attrMap <$> _inputList bm
    , A.attrMap <$> _inputMax bm
    , A.attrMap <$> _inputMaxLength bm
    , A.attrMap <$> _inputMin bm
    , A.attrMap <$> _inputMinLength bm
    , A.attrMap <$> _inputMultiple bm
    , A.attrMap <$> _inputName bm
    , A.attrMap <$> _inputPattern bm
    , A.attrMap <$> _inputPlaceholder bm
    , A.attrMap <$> _inputReadOnly bm
    , A.attrMap <$> _inputRequired bm
    , A.attrMap <$> _inputSize bm
    , A.attrMap <$> _inputSrc bm
    , A.attrMap <$> _inputStep bm
    , A.attrMap <$> _inputType bm
    , A.attrMap <$> _inputValueText bm
    , A.attrMap <$> _inputWidth bm
    ] <> maybeToList (_inputCustom bm)

defInput ∷ Input
defInput = Input Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid Input where
  mempty = defInput
  mappend
    (Input a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17
      a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34)
    (Input b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17
      b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34)
    = Input (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)
        (a11 <> b11) (a12 <> b12) (a13 <> b13) (a14 <> b14) (a15 <> b15)
        (a16 <> b16) (a17 <> b17) (a18 <> b18) (a19 <> b19) (a20 <> b20)
        (a21 <> b21) (a22 <> b22) (a23 <> b23) (a24 <> b24) (a25 <> b25)
        (a26 <> b26) (a27 <> b27) (a28 <> b28) (a29 <> b29) (a30 <> b30)
        (a31 <> b31) (a32 <> b32) (a33 <> b33) (a34 <> b34)


instance A.AttrHasGlobals Input where
   attrSetGlobals pp bm = bm { _inputGlobals = Just pp }


-- Global attributes require the following instances.
instance A.AttrHasAccessKey Input
  where attrSetAccessKey pp g = g { _inputGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasAnmval Input
  where attrSetAnmval pp g = g { _inputGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasContentEditable Input
  where attrSetContentEditable pp g = g  { _inputGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasContextMenu Input
  where attrSetContextMenu pp g     = g { _inputGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasClass Input
  where attrSetClassName pp g           = g { _inputGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasDnmval Input
  where attrSetDnmval pp g           = g { _inputGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasDir Input
  where attrSetDir pp g             = g { _inputGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasDraggable Input
  where attrSetDraggable pp g       = g { _inputGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasHidden Input
  where attrSetHidden pp g          = g { _inputGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasId Input
  where attrSetId pp g              = g { _inputGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasLang Input
  where attrSetLang pp g            = g { _inputGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasRole Input
  where attrSetRole pp g            = g { _inputGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasSlot Input
  where attrSetSlot pp g            = g { _inputGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasSpellCheck Input
  where attrSetSpellCheck pp g      = g { _inputGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasStyle Input
  where attrSetStyle pp g           = g { _inputGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasTabIndex Input
  where attrSetTabIndex pp g        = g { _inputGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasTitle Input
  where attrSetTitle pp g           = g { _inputGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_inputGlobals g))) }
instance A.AttrHasTranslate Input
  where attrSetTranslate pp g       = g { _inputGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_inputGlobals g))) }

instance A.AttrGetClassName Input where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_inputGlobals g)

instance A.AttrHasAccept         Input where attrSetAccept pp g         = g { _inputAccept         = Just pp }
instance A.AttrHasAlt            Input where attrSetAlt pp g            = g { _inputAlt            = Just pp }
instance A.AttrHasAutoComplete   Input where attrSetAutoComplete pp g   = g { _inputAutoComplete   = Just pp }
instance A.AttrHasAutoFocus      Input where attrSetAutoFocus pp g      = g { _inputAutoFocus      = Just pp }
instance A.AttrHasChecked        Input where attrSetChecked pp g        = g { _inputChecked        = Just pp }
instance A.AttrHasDirName        Input where attrSetDirName pp g        = g { _inputDirName        = Just pp }
instance A.AttrHasDisabled       Input where attrSetDisabled pp g       = g { _inputDisabled       = Just pp }
instance A.AttrHasForm           Input where attrSetForm pp g           = g { _inputForm           = Just pp }
instance A.AttrHasFormAction     Input where attrSetFormAction pp g     = g { _inputFormAction     = Just pp }
instance A.AttrHasFormEncType    Input where attrSetFormEncType pp g    = g { _inputFormEncType    = Just pp }
instance A.AttrHasFormMethod     Input where attrSetFormMethod pp g     = g { _inputFormMethod     = Just pp }
instance A.AttrHasFormNoValidate Input where attrSetFormNoValidate pp g = g { _inputFormNoValidate = Just pp }
instance A.AttrHasFormTarget     Input where attrSetFormTarget pp g     = g { _inputFormTarget     = Just pp }
instance A.AttrHasHeight         Input where attrSetHeight pp g         = g { _inputHeight         = Just pp }
instance A.AttrHasInputMode      Input where attrSetInputMode pp g      = g { _inputInputMode      = Just pp }
instance A.AttrHasList           Input where attrSetList pp g           = g { _inputList           = Just pp }
instance A.AttrHasMax            Input where attrSetMax pp g            = g { _inputMax            = Just pp }
instance A.AttrHasMaxLength      Input where attrSetMaxLength pp g      = g { _inputMaxLength      = Just pp }
instance A.AttrHasMin            Input where attrSetMin pp g            = g { _inputMin            = Just pp }
instance A.AttrHasMinLength      Input where attrSetMinLength pp g      = g { _inputMinLength      = Just pp }
instance A.AttrHasMultiple       Input where attrSetMultiple pp g       = g { _inputMultiple       = Just pp }
instance A.AttrHasName           Input where attrSetName pp g           = g { _inputName           = Just pp }
instance A.AttrHasPattern        Input where attrSetPattern pp g        = g { _inputPattern        = Just pp }
instance A.AttrHasPlaceholder    Input where attrSetPlaceholder pp g    = g { _inputPlaceholder    = Just pp }
instance A.AttrHasReadOnly       Input where attrSetReadOnly pp g       = g { _inputReadOnly       = Just pp }
instance A.AttrHasRequired       Input where attrSetRequired pp g       = g { _inputRequired       = Just pp }
instance A.AttrHasSize           Input where attrSetSize pp g           = g { _inputSize           = Just pp }
instance A.AttrHasSrc            Input where attrSetSrc pp g            = g { _inputSrc            = Just pp }
instance A.AttrHasStep           Input where attrSetStep pp g           = g { _inputStep           = Just pp }
instance A.AttrHasInputType      Input where attrSetInputType pp g      = g { _inputType           = Just pp }
instance A.AttrHasValueText      Input where attrSetValueText pp g      = g { _inputValueText      = Just pp }
instance A.AttrHasWidth          Input where attrSetWidth pp g          = g { _inputWidth          = Just pp }


instance A.AttrHasCustom Input where attrSetCustom pp g       = g { _inputCustom = Just pp }

-- | A short-hand notion for @ elAttr\' \"input\" ... @
input' ∷ forall t m a. DomBuilder t m ⇒ Input → m a → m (Element EventResult (DomBuilderSpace m) t, a)
input' bm  = elAttr' "input" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"input\" ... @
input ∷ forall t m a. DomBuilder t m ⇒ Input → m a → m a
input bm children = snd <$> input' bm children

-- | A short-hand notion for @ el\' \"input\" ... @
inputN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
inputN' = el' "input"

-- | A short-hand notion for @ el \"input\" ... @
inputN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
inputN children = snd <$> inputN' children

-- | A short-hand notion for @ elDynAttr\' \"input\" ... @
inputD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Input → m a → m (Element EventResult (DomBuilderSpace m) t, a)
inputD' bm  = elDynAttr' "input" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"input\" ... @
inputD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Input → m a → m a
inputD bm children = snd <$> inputD' bm children


inputC ∷ (DomBuilder t m, PostBuild t m) ⇒ Input → m () → m (Event t ())
inputC bm children = do
  (e,_) ← elAttr' "input" (A.attrMap bm) children
  return $ domEvent Click e

inputCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Input → m () → m (Event t ())
inputCD bDyn children = do
  (e,_) ← elDynAttr' "input" (A.attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Keygen is likely to be removed from HTML 5.2.
-- Thus, we don't have it here.

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Label-element has the following attributes:
-- globals; form; for
--
data Label = Label
  { _labelGlobals ∷ Maybe A.Globals
  -- , _labelForm ∷ Maybe Form -- Not in HTML 5.2.
  , _labelForId   ∷ Maybe A.ForId
  , _labelCustom  ∷ Maybe A.Attr
  }


instance A.AttrMap Label where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _labelGlobals bm
    -- , A.attrMap <$> _labelForm bm
    , A.attrMap <$> _labelForId bm
    ] <> maybeToList (_labelCustom bm)

defLabel ∷ Label
defLabel = Label Nothing Nothing Nothing

instance Monoid Label where
  mempty = defLabel
  mappend (Label a1 a2 a3) (Label b1 b2 b3)
    = Label (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance A.AttrHasGlobals Label where
   attrSetGlobals pp bm = bm { _labelGlobals = Just pp }

-- Global attributes require the following instances.
instance A.AttrHasAccessKey Label
  where attrSetAccessKey pp g = g { _labelGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasAnmval Label
  where attrSetAnmval pp g = g { _labelGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasContentEditable Label
  where attrSetContentEditable pp g = g  { _labelGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasContextMenu Label
  where attrSetContextMenu pp g     = g { _labelGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasClass Label
  where attrSetClassName pp g           = g { _labelGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasDnmval Label
  where attrSetDnmval pp g           = g { _labelGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasDir Label
  where attrSetDir pp g             = g { _labelGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasDraggable Label
  where attrSetDraggable pp g       = g { _labelGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasHidden Label
  where attrSetHidden pp g          = g { _labelGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasId Label
  where attrSetId pp g              = g { _labelGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasLang Label
  where attrSetLang pp g            = g { _labelGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasRole Label
  where attrSetRole pp g            = g { _labelGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasSlot Label
  where attrSetSlot pp g            = g { _labelGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasSpellCheck Label
  where attrSetSpellCheck pp g      = g { _labelGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasStyle Label
  where attrSetStyle pp g           = g { _labelGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasTabIndex Label
  where attrSetTabIndex pp g        = g { _labelGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasTitle Label
  where attrSetTitle pp g           = g { _labelGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_labelGlobals g))) }
instance A.AttrHasTranslate Label
  where attrSetTranslate pp g       = g { _labelGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_labelGlobals g))) }

instance A.AttrGetClassName Label where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_labelGlobals g)

-- instance A.AttrHasForm      Label where attrSetForm pp g      = g { _labelForm      = Just pp }
instance A.AttrHasForId     Label where attrSetForId pp g      = g { _labelForId     = Just pp }

instance A.AttrHasCustom Label where attrSetCustom pp g       = g { _labelCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"label\" ... @
label' ∷ forall t m a. DomBuilder t m ⇒ Label → m a → m (Element EventResult (DomBuilderSpace m) t, a)
label' bm  = elAttr' "label" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"label\" ... @
label ∷ forall t m a. DomBuilder t m ⇒ Label → m a → m a
label bm children = snd <$> label' bm children

-- | A short-hand notion for @ el\' \"label\" ... @
labelN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
labelN' = el' "label"

-- | A short-hand notion for @ el \"label\" ... @
labelN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
labelN children = snd <$> labelN' children

-- | A short-hand notion for @ elDynAttr\' \"label\" ... @
labelD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Label → m a → m (Element EventResult (DomBuilderSpace m) t, a)
labelD' bm  = elDynAttr' "label" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"label\" ... @
labelD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Label → m a → m a
labelD bm children = snd <$> labelD' bm children


labelC ∷ (DomBuilder t m, PostBuild t m) ⇒ Label → m () → m (Event t ())
labelC bm children = do
  (e,_) ← elAttr' "label" (A.attrMap bm) children
  return $ domEvent Click e

labelCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Label → m () → m (Event t ())
labelCD bDyn children = do
  (e,_) ← elDynAttr' "label" (A.attrMap <$> bDyn) children
  return $ domEvent Click e

labelCForId ∷  (DomBuilder t m, PostBuild t m) ⇒ Maybe A.Globals → A.ForId → m (Event t ())
labelCForId bm f = labelC (Label bm (Just f) Nothing) blank



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Select-element has the following attributes:
-- globals; autofocus; disabled; form; multiple; name; required; size
--
data Select = Select
  { _selectGlobals   ∷ Maybe A.Globals
  , _selectAutoFocus ∷ Maybe A.AutoFocus
  , _selectDisabled  ∷ Maybe A.Disabled
  , _selectForm      ∷ Maybe A.Form
  , _selectMultiple  ∷ Maybe A.Multiple
  , _selectName      ∷ Maybe A.Name
  , _selectRequired  ∷ Maybe A.Required
  , _selectSize      ∷ Maybe A.Size
  , _selectCustom    ∷ Maybe A.Attr
  }


instance A.AttrMap Select where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _selectGlobals bm
    , A.attrMap <$> _selectAutoFocus bm
    , A.attrMap <$> _selectDisabled bm
    , A.attrMap <$> _selectForm bm
    , A.attrMap <$> _selectMultiple bm
    , A.attrMap <$> _selectName bm
    , A.attrMap <$> _selectRequired bm
    , A.attrMap <$> _selectSize bm
    ] <> maybeToList (_selectCustom bm)

defSelect ∷ Select
defSelect = Select Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid Select where
  mempty = defSelect
  mappend (Select a1 a2 a3 a4 a5 a6 a7 a8 a9)
          (Select b1 b2 b3 b4 b5 b6 b7 b8 b9)
    = Select (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)

instance A.AttrHasGlobals Select where
   attrSetGlobals pp bm = bm { _selectGlobals = Just pp }

-- Global attributes require the following instances.
instance A.AttrHasAccessKey Select
  where attrSetAccessKey pp g = g { _selectGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasAnmval Select
  where attrSetAnmval pp g = g { _selectGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasContentEditable Select
  where attrSetContentEditable pp g = g  { _selectGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasContextMenu Select
  where attrSetContextMenu pp g     = g { _selectGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasClass Select
  where attrSetClassName pp g           = g { _selectGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasDnmval Select
  where attrSetDnmval pp g           = g { _selectGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasDir Select
  where attrSetDir pp g             = g { _selectGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasDraggable Select
  where attrSetDraggable pp g       = g { _selectGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasHidden Select
  where attrSetHidden pp g          = g { _selectGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasId Select
  where attrSetId pp g              = g { _selectGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasLang Select
  where attrSetLang pp g            = g { _selectGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasRole Select
  where attrSetRole pp g            = g { _selectGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasSlot Select
  where attrSetSlot pp g            = g { _selectGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasSpellCheck Select
  where attrSetSpellCheck pp g      = g { _selectGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasStyle Select
  where attrSetStyle pp g           = g { _selectGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasTabIndex Select
  where attrSetTabIndex pp g        = g { _selectGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasTitle Select
  where attrSetTitle pp g           = g { _selectGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_selectGlobals g))) }
instance A.AttrHasTranslate Select
  where attrSetTranslate pp g       = g { _selectGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_selectGlobals g))) }

instance A.AttrGetClassName Select where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_selectGlobals g)


instance A.AttrHasAutoFocus Select where attrSetAutoFocus pp g = g { _selectAutoFocus = Just pp }
instance A.AttrHasDisabled  Select where attrSetDisabled pp g  = g { _selectDisabled  = Just pp }
instance A.AttrHasForm      Select where attrSetForm pp g      = g { _selectForm      = Just pp }
instance A.AttrHasMultiple  Select where attrSetMultiple pp g  = g { _selectMultiple  = Just pp }
instance A.AttrHasName      Select where attrSetName pp g      = g { _selectName      = Just pp }
instance A.AttrHasRequired  Select where attrSetRequired pp g  = g { _selectRequired  = Just pp }
instance A.AttrHasSize      Select where attrSetSize pp g      = g { _selectSize      = Just pp }

instance A.AttrHasCustom Select where attrSetCustom pp g       = g { _selectCustom = Just pp }


-- | A short-hand notion for @ elAttr\' \"select\" ... @
select' ∷ forall t m a. DomBuilder t m ⇒ Select → m a → m (Element EventResult (DomBuilderSpace m) t, a)
select' bm  = elAttr' "select" (A.attrMap bm)

-- | A short-hand notion for @ elAttr \"select\" ... @
select ∷ forall t m a. DomBuilder t m ⇒ Select → m a → m a
select bm children = snd <$> select' bm children

-- | A short-hand notion for @ el\' \"select\" ... @
selectN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
selectN' = el' "select"

-- | A short-hand notion for @ el \"select\" ... @
selectN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
selectN children = snd <$> selectN' children

-- | A short-hand notion for @ elDynAttr\' \"select\" ... @
selectD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Select → m a → m (Element EventResult (DomBuilderSpace m) t, a)
selectD' bm  = elDynAttr' "select" (A.attrMap <$> bm)

-- | A short-hand notion for @ elDynAttr \"select\" ... @
selectD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Select → m a → m a
selectD bm children = snd <$> selectD' bm children


selectC ∷ (DomBuilder t m, PostBuild t m) ⇒ Select → m () → m (Event t ())
selectC bm children = do
  (e,_) ← elAttr' "select" (A.attrMap bm) children
  return $ domEvent Click e

selectCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t Select → m () → m (Event t ())
selectCD bDyn children = do
  (e,_) ← elDynAttr' "select" (A.attrMap <$> bDyn) children
  return $ domEvent Click e



------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | TextArea-element has the following attributes:
-- globals; autofocus; cols; dirname; disabled; form; maxlength; minlength;
-- name; placeholder; readonly; required; rows; wrap
--
data TextArea = TextArea
  { _textAreaGlobals     ∷ Maybe A.Globals
  , _textAreaAutoFocus   ∷ Maybe A.AutoFocus
  , _textAreaCols        ∷ Maybe A.Cols
  , _textAreaDirName     ∷ Maybe A.DirName
  , _textAreaDisabled    ∷ Maybe A.Disabled
  , _textAreaForm        ∷ Maybe A.Form
  , _textAreaMaxLength   ∷ Maybe A.MaxLength
  , _textAreaMinLength   ∷ Maybe A.MinLength
  , _textAreaName        ∷ Maybe A.Name
  , _textAreaPlaceholder ∷ Maybe A.Placeholder
  , _textAreaReadOnly    ∷ Maybe A.ReadOnly
  , _textAreaRequired    ∷ Maybe A.Required
  , _textAreaRows        ∷ Maybe A.Rows
  , _textAreaWrap        ∷ Maybe A.Wrap
  , _textAreaCustom      ∷ Maybe A.Attr
  }


instance A.AttrMap TextArea where
  attrMap bm = fold $ catMaybes
    [ A.attrMap <$> _textAreaGlobals bm
    , A.attrMap <$> _textAreaAutoFocus bm
    , A.attrMap <$> _textAreaCols bm
    , A.attrMap <$> _textAreaDirName bm
    , A.attrMap <$> _textAreaDisabled bm
    , A.attrMap <$> _textAreaForm bm
    , A.attrMap <$> _textAreaMaxLength bm
    , A.attrMap <$> _textAreaMinLength bm
    , A.attrMap <$> _textAreaName bm
    , A.attrMap <$> _textAreaPlaceholder bm
    , A.attrMap <$> _textAreaReadOnly bm
    , A.attrMap <$> _textAreaRequired bm
    , A.attrMap <$> _textAreaRows bm
    , A.attrMap <$> _textAreaWrap bm
    ] <> maybeToList (_textAreaCustom bm)

defTextArea ∷ TextArea
defTextArea = TextArea Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid TextArea where
  mempty = defTextArea
  mappend
    (TextArea a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (TextArea b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    = TextArea (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)
        (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)
        (a11 <> b11) (a12 <> b12) (a13 <> b13) (a14 <> b14) (a15 <> b15)

instance A.AttrHasGlobals TextArea where
   attrSetGlobals pp bm = bm { _textAreaGlobals = Just pp }

-- Global attributes require the following instances.
instance A.AttrHasAccessKey TextArea
  where attrSetAccessKey pp g = g { _textAreaGlobals = Just (A.attrSetAccessKey pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasAnmval TextArea
  where attrSetAnmval pp g = g { _textAreaGlobals = Just (A.attrSetAnmval pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasContentEditable TextArea
  where attrSetContentEditable pp g = g  { _textAreaGlobals = Just (A.attrSetContentEditable pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasContextMenu TextArea
  where attrSetContextMenu pp g     = g { _textAreaGlobals = Just (A.attrSetContextMenu pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasClass TextArea
  where attrSetClassName pp g           = g { _textAreaGlobals = Just (A.attrSetClassName pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasDnmval TextArea
  where attrSetDnmval pp g           = g { _textAreaGlobals = Just (A.attrSetDnmval pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasDir TextArea
  where attrSetDir pp g             = g { _textAreaGlobals = Just (A.attrSetDir pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasDraggable TextArea
  where attrSetDraggable pp g       = g { _textAreaGlobals = Just (A.attrSetDraggable pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasHidden TextArea
  where attrSetHidden pp g          = g { _textAreaGlobals = Just (A.attrSetHidden pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasId TextArea
  where attrSetId pp g              = g { _textAreaGlobals = Just (A.attrSetId pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasLang TextArea
  where attrSetLang pp g            = g { _textAreaGlobals = Just (A.attrSetLang pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasRole TextArea
  where attrSetRole pp g            = g { _textAreaGlobals = Just (A.attrSetRole pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasSlot TextArea
  where attrSetSlot pp g            = g { _textAreaGlobals = Just (A.attrSetSlot pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasSpellCheck TextArea
  where attrSetSpellCheck pp g      = g { _textAreaGlobals = Just (A.attrSetSpellCheck pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasStyle TextArea
  where attrSetStyle pp g           = g { _textAreaGlobals = Just (A.attrSetStyle pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasTabIndex TextArea
  where attrSetTabIndex pp g        = g { _textAreaGlobals = Just (A.attrSetTabIndex pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasTitle TextArea
  where attrSetTitle pp g           = g { _textAreaGlobals = Just (A.attrSetTitle pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }
instance A.AttrHasTranslate TextArea
  where attrSetTranslate pp g       = g { _textAreaGlobals = Just (A.attrSetTranslate pp (fromMaybe A.defGlobals (_textAreaGlobals g))) }

instance A.AttrGetClassName TextArea where attrGetClassName g = maybe (A.ClassName T.empty) A.attrGetClassName (_textAreaGlobals g)


instance A.AttrHasAutoFocus   TextArea where attrSetAutoFocus pp g   = g { _textAreaAutoFocus   = Just pp }
instance A.AttrHasCols        TextArea where attrSetCols pp g        = g { _textAreaCols        = Just pp }
instance A.AttrHasDirName     TextArea where attrSetDirName pp g     = g { _textAreaDirName     = Just pp }
instance A.AttrHasDisabled    TextArea where attrSetDisabled pp g    = g { _textAreaDisabled    = Just pp }
instance A.AttrHasForm        TextArea where attrSetForm pp g        = g { _textAreaForm        = Just pp }
instance A.AttrHasMaxLength   TextArea where attrSetMaxLength pp g   = g { _textAreaMaxLength   = Just pp }
instance A.AttrHasMinLength   TextArea where attrSetMinLength pp g   = g { _textAreaMinLength   = Just pp }
instance A.AttrHasName        TextArea where attrSetName pp g        = g { _textAreaName        = Just pp }
instance A.AttrHasPlaceholder TextArea where attrSetPlaceholder pp g = g { _textAreaPlaceholder = Just pp }
instance A.AttrHasReadOnly    TextArea where attrSetReadOnly pp g    = g { _textAreaReadOnly    = Just pp }
instance A.AttrHasRequired    TextArea where attrSetRequired pp g    = g { _textAreaRequired    = Just pp }
instance A.AttrHasRows        TextArea where attrSetRows pp g        = g { _textAreaRows        = Just pp }
instance A.AttrHasWrap        TextArea where attrSetWrap pp g        = g { _textAreaWrap        = Just pp }

instance A.AttrHasCustom TextArea where attrSetCustom pp g       = g { _textAreaCustom = Just pp }

textArea' ∷ forall t m a. DomBuilder t m ⇒ TextArea → m a → m (Element EventResult (DomBuilderSpace m) t, a)
textArea' bm  = elAttr' "textarea" (A.attrMap bm)

textArea ∷ forall t m a. DomBuilder t m ⇒ TextArea → m a → m a
textArea bm children = snd <$> textArea' bm children

textAreaN' ∷ forall t m a. DomBuilder t m ⇒ m a → m (Element EventResult (DomBuilderSpace m) t, a)
textAreaN' = el' "textarea"

textAreaN ∷ forall t m a. DomBuilder t m ⇒ m a → m a
textAreaN children = snd <$> textAreaN' children

textAreaD' ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t TextArea → m a → m (Element EventResult (DomBuilderSpace m) t, a)
textAreaD' bm  = elDynAttr' "textarea" (A.attrMap <$> bm)

textAreaD ∷ forall t m a. (DomBuilder t m, PostBuild t m) ⇒ Dynamic t TextArea → m a → m a
textAreaD bm children = snd <$> textAreaD' bm children


textAreaC ∷ (DomBuilder t m, PostBuild t m) ⇒ TextArea → m () → m (Event t ())
textAreaC bm children = do
  (e,_) ← elAttr' "textarea" (A.attrMap bm) children
  return $ domEvent Click e

textAreaCD ∷ (DomBuilder t m, PostBuild t m) ⇒ Dynamic t TextArea → m () → m (Event t ())
textAreaCD bDyn children = do
  (e,_) ← elDynAttr' "textarea" (A.attrMap <$> bDyn) children
  return $ domEvent Click e




