{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs.Attrs
Description : HTML5 Attributes with some helper functions.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Attributes

This module defines html5 attributes and with corresponding
elements, the compiler can help to avoid some of the writing
typos that were otherwise to be found on runtime.

To make an attribute map, write e.g.
@
  let a = href (URL "localhost:8000") $ className "myClass" $ id_ "myId" def
@

An example can be found in example-directory.

The element constructing functions don't allow non-conforming
HTML to be constructed directly. There are a few ways to by pass,
in case there is some need:
- el "element name" attrs ... can be used and combined with these
- see AnyAttr (it is, however, incomplete)
- addCustom can be used to add anything



== Notes


For a list of attributes, see e.g.
[w3c element list with attributes](https://w3c.github.io/html/fullindex.html#index-elements)
or
[w3 attribute list](https://www.w3.org/TR/html5/index.html#attributes-1)

The following attributes have different parameter types depending on
the element, in which it is used.

    * Value
    * Type
    * Min
    * Max


Type-attribute has been splitted to

    * ButtonType constants (btSubmit, btReset, btButton, btMenu)
    * InputType constants (itButton, itCheckbox, itColor, etc.)
    * MediaType (textHtml, mimeTypeTxt)
    * MenuItemType constant (mitCommand)
    * MenuItem constant (mtContext)
    * OlType constants (olDec, olLowLat, olUpLat, olLowRoman, olUpRoman)
    * ScriptType constants and STother (stJs, stModule, stOther)

Value-attribute has been splitted to

    * DataValue, takes Text (dataValue)
    * ValueText, takes Text (valueText)
    * ValueName, takes Double (valueName)
    * ValueNumber, takes Double (valueNumber)
    * ValueOlLi, takes Int (valueOlLi)

Min-attribute (max-attribute similarly) has been splitted to

    * Min (minI, minD, minDT, minT)
    * MinLength (minLength)


Attributes not supperted in HTML5 are not implemented.
(E.g. align, bgcolor, border, color.)


== Caveats, TODOs


In consideration (for splitting up or for something else todo):

    * for (label and output elements),
    * href (a, area; link; base; current way to use is not good)
    * max (input vs meter and progress, check this)
    * min (same but no progress)
    * name (several elements: may benefit if splitted up somehow, even
      though all are of type text, check iframe and object elements)
    * target (similarly has same type but quite different elements)
    * title (same here)


In many cases, we could let the compiler help more than at the current
state.

E.g, we could have a enums for media types. For list, see:
[media type list}(https://www.iana.org/assignments/media-types/media-types.xml)

There are many TODOs below. See also the todo-list of this package.

Some of the parameters could use data types introduced in
other packages (URL, datatime etc). Or if it is difficult to decide
what is the right package, maybe some smart constructors?

-}

module Reflex.Dom.HTML5.Attrs.Attrs
  where

import Data.Default (Default, def)
import Data.Foldable (fold)
import qualified Data.Map as Map
-- import Data.Monoid ((<>))
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core (Reflex, (=:))
import Reflex.Pure (Dynamic)

import Reflex.Dom.HTML5.Attrs.Common (AttrMap, attrMap, URL(URL))

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Alternative label to use for the header cell when referencing the cell in
-- other contexts, used with th elements with tables.
newtype Abbr = Abbr Text
  deriving (Show, Read, Eq, Ord)

instance Default Abbr where
  def = Abbr T.empty

instance AttrMap Abbr where
  attrMap (Abbr i) = "accept-charset" =: i

class AttrHasAbbr a where
  attrSetAbbr :: Abbr -> a -> a

-- |
-- @
-- abbr txt def
-- @
abbr :: AttrHasAbbr a => Text -> a -> a
abbr i = attrSetAbbr (Abbr i)

instance Semigroup Abbr where
  (<>) (Abbr a) (Abbr b) = Abbr (a <> " " <> b)

instance Monoid Abbr where
  mappend = (<>)
  mempty = def

instance (Reflex t, AttrHasAbbr a) => AttrHasAbbr (Dynamic t a) where
  attrSetAbbr c = fmap (attrSetAbbr c)


------------------------------------------------------------------------------


-- |
-- AcceptExt Text takes file extension without leading dot.
-- Wish list: mime types enumerated.
data AcceptToken = AcceptAudio | AcceptVideo | AcceptImage | AcceptMime Text
                 | AcceptExt Text
  deriving (Show, Read, Eq, Ord)

newtype Accept = Accept [AcceptToken]
  deriving (Show, Read, Eq, Ord)

instance Default Accept where
  def = Accept []

accT2txt :: AcceptToken -> Text
accT2txt AcceptAudio = "audio/*"
accT2txt AcceptVideo = "video/*"
accT2txt AcceptImage = "image/*"
accT2txt (AcceptMime m) = m
accT2txt (AcceptExt m) = "." <> m

instance AttrMap Accept where
  attrMap (Accept i) = "accept" =: foldr (\at b -> b <> "," <> accT2txt at) T.empty i

instance Semigroup Accept where
  (<>) (Accept a) (Accept b) = Accept (a ++ b)

instance Monoid Accept where
  mappend = (<>)
  mempty = def

class AttrHasAccept a where
  attrSetAccept :: Accept -> a -> a

class AttrGetAccept a where
  attrGetAccept :: a -> Accept

-- |
-- @
-- addAccept AcceptAudio def
-- @
addAccept :: (AttrHasAccept a, AttrGetAccept a) => AcceptToken -> a -> a
addAccept t i = attrSetAccept (Accept $ t:tLst) i
  where
    (Accept tLst) = attrGetAccept i

instance (Reflex t, AttrHasAccept a) => AttrHasAccept (Dynamic t a) where
  attrSetAccept c = fmap (attrSetAccept c)

------------------------------------------------------------------------------

-- |
-- Wish list: implement the set of unique tokens.
-- See: https://encoding.spec.whatwg.org/
newtype AcceptCharSet = AcceptCharSet Text
  deriving (Show, Read, Eq, Ord)

instance Default AcceptCharSet where
  def = AcceptCharSet T.empty

instance AttrMap AcceptCharSet where
  attrMap (AcceptCharSet i) = "accept-charset" =: i

instance Semigroup AcceptCharSet where
  (<>) (AcceptCharSet a) (AcceptCharSet b) = AcceptCharSet (a <> " " <> b)

instance Monoid AcceptCharSet where
  mappend = (<>)
  mempty = AcceptCharSet T.empty

class AttrHasAcceptCharSet a where
  attrSetAcceptCharSet :: AcceptCharSet -> a -> a

-- |
-- @
-- acceptCharSet txt def
-- @
acceptCharSet :: AttrHasAcceptCharSet a => Text -> a -> a
acceptCharSet i = attrSetAcceptCharSet (AcceptCharSet i)

instance (Reflex t, AttrHasAcceptCharSet a) => AttrHasAcceptCharSet (Dynamic t a) where
  attrSetAcceptCharSet c = fmap (attrSetAcceptCharSet c)

------------------------------------------------------------------------------

-- |  Send the form-data when a form is submitted to the given URL.
newtype Action = Action URL
  deriving (Show, Read, Eq, Ord)

instance Default Action where
  def = Action (URL T.empty)

instance AttrMap Action where
  attrMap (Action (URL h)) = "action" =: h

instance Semigroup Action where
  (<>) _a b = b

instance Monoid Action where
  mappend = (<>)
  mempty = def

class AttrHasAction a where
  attrSetAction :: Action -> a -> a

-- |
-- @
-- action url def
-- @
action :: AttrHasAction a => URL -> a -> a
action u = attrSetAction (Action u)

instance (Reflex t, AttrHasAction a) => AttrHasAction (Dynamic t a) where
  attrSetAction c = fmap (attrSetAction c)

------------------------------------------------------------------------------

-- | Allow iframe-elements contents to use requestFullscreen().
data AllowFullScreen = AllowFullScreen
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default AllowFullScreen where
  def = AllowFullScreen

instance AttrMap AllowFullScreen where
  attrMap AllowFullScreen = "allowfullscreen" =: T.empty

instance Semigroup AllowFullScreen where
  (<>) _ b = b

instance Monoid AllowFullScreen where
  mappend = (<>)
  mempty = def

class AttrHasAllowFullScreen a where
  attrSetAllowFullScreen :: AllowFullScreen -> a -> a

-- | This sets the attribute.
allowFullScreen :: AttrHasAllowFullScreen a => a -> a
allowFullScreen = attrSetAllowFullScreen AllowFullScreen

instance (Reflex t, AttrHasAllowFullScreen a) => AttrHasAllowFullScreen (Dynamic t a) where
  attrSetAllowFullScreen c = fmap (attrSetAllowFullScreen c)

------------------------------------------------------------------------------

-- |
-- If area element has an href attribute, then it should have an alt attribute.
-- https://w3c.github.io/html/semantics-embedded-content.html#element-attrdef-area-alt
newtype Alt_ = Alt_ Text
  deriving (Show, Read, Eq, Ord)

instance Default Alt_ where
  def = Alt_ T.empty

instance AttrMap Alt_ where
  attrMap (Alt_ t) = "alt" =: t

instance Semigroup Alt_ where
  (<>) _ b = b

instance Monoid Alt_ where
  mappend = (<>)
  mempty = def

class AttrHasAlt a where
  attrSetAlt :: Alt_ -> a -> a

-- |
alt :: AttrHasAlt a => Text -> a -> a
alt t = attrSetAlt (Alt_ t)

instance (Reflex t, AttrHasAlt a) => AttrHasAlt (Dynamic t a) where
  attrSetAlt c = fmap (attrSetAlt c)

------------------------------------------------------------------------------

-- |
data Async = Async
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Async where
  def = Async

instance AttrMap Async where
  attrMap Async = "async" =: T.empty

instance Semigroup Async where
  (<>) _ b = b

instance Monoid Async where
  mappend = (<>)
  mempty = def

class AttrHasAsync a where
  attrSetAsync :: Async -> a -> a

-- | This sets the attribute.
async :: AttrHasAsync a => a -> a
async = attrSetAsync Async

instance (Reflex t, AttrHasAsync a) => AttrHasAsync (Dynamic t a) where
  attrSetAsync c = fmap (attrSetAsync c)

------------------------------------------------------------------------------

-- | Hint for form autofill feature, used with input, select and textarea
-- elements.
--
-- Wish list: implement the autofill tokens.
-- See: https://w3c.github.io/html/sec-forms.html#element-attrdef-autocompleteelements-autocomplete
newtype AutoComplete = AutoComplete Text
  deriving (Show, Read, Eq, Ord)

instance Default AutoComplete where
  def = AutoComplete T.empty

instance AttrMap AutoComplete where
  attrMap (AutoComplete i) = "autocomplete" =: i

instance Semigroup AutoComplete where
  (<>) (AutoComplete a) (AutoComplete b) = AutoComplete (a <> " " <> b)

instance Monoid AutoComplete where
  mappend = (<>)
  mempty = def

class AttrHasAutoComplete a where
  attrSetAutoComplete :: AutoComplete -> a -> a

-- |
autoComplete :: AttrHasAutoComplete a => Text -> a -> a
autoComplete i = attrSetAutoComplete (AutoComplete i)

instance (Reflex t, AttrHasAutoComplete a) => AttrHasAutoComplete (Dynamic t a) where
  attrSetAutoComplete c = fmap (attrSetAutoComplete c)

------------------------------------------------------------------------------

-- | Automatically focus the form control when the page is loaded, used
-- with button, input, select and textarea elements.
data AutoFocus = AutoFocus
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Default AutoFocus where
  def = AutoFocus

instance AttrMap AutoFocus where
  attrMap AutoFocus = "autofocus" =: T.empty

instance Semigroup AutoFocus where
  (<>) _ b = b

instance Monoid AutoFocus where
  mappend = (<>)
  mempty = def

class AttrHasAutoFocus a where
  attrSetAutoFocus :: AutoFocus -> a -> a

-- | This sets the attribute.
autoFocus :: AttrHasAutoFocus a => a -> a
autoFocus = attrSetAutoFocus AutoFocus

instance (Reflex t, AttrHasAutoFocus a) => AttrHasAutoFocus (Dynamic t a) where
  attrSetAutoFocus c = fmap (attrSetAutoFocus c)

------------------------------------------------------------------------------

-- | Hint that the media resource can be started automatically when the page
-- is loaded, used with audio and video elements.
data Autoplay = Autoplay
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Autoplay where
  def = Autoplay

instance AttrMap Autoplay where
  attrMap Autoplay = "autoplay" =: T.empty

instance Semigroup Autoplay where
  (<>) _ b = b

instance Monoid Autoplay where
  mappend = (<>)
  mempty = def

class AttrHasAutoplay a where
  attrSetAutoplay :: Autoplay -> a -> a

-- | This sets the attribute.
autoplay :: AttrHasAutoplay a => a -> a
autoplay = attrSetAutoplay Autoplay

instance (Reflex t, AttrHasAutoplay a) => AttrHasAutoplay (Dynamic t a) where
  attrSetAutoplay c = fmap (attrSetAutoplay c)

------------------------------------------------------------------------------

-- |
data ButtonType = BTsubmit | BTreset | BTbutton | BTmenu
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default ButtonType where
  def = BTbutton

instance AttrMap ButtonType where
  attrMap BTsubmit = "type" =: "submit"
  attrMap BTreset  = "type" =: "reset"
  attrMap BTbutton = "type" =: "button"
  attrMap BTmenu   = "type" =: "menu"

instance Semigroup ButtonType where
  (<>)_ b = b

instance Monoid ButtonType where
  mappend = (<>)
  mempty = def

class AttrHasButtonType a where
  attrSetButtonType :: ButtonType -> a -> a

-- | These set the corresponding type-attribute.
btSubmit, btReset, btButton, btMenu :: AttrHasButtonType a => a -> a
btSubmit = attrSetButtonType BTsubmit
btReset  = attrSetButtonType BTreset
btButton = attrSetButtonType BTbutton
btMenu   = attrSetButtonType BTmenu

instance (Reflex t, AttrHasButtonType a) => AttrHasButtonType (Dynamic t a) where
  attrSetButtonType c = fmap (attrSetButtonType c)

------------------------------------------------------------------------------

-- |
-- Parameter is a character encoding declaration.
-- See https://w3c.github.io/html/document-metadata.html#character-encoding-declaration
-- E.g. "uft-8"
newtype CharSet = CharSet Text
  deriving (Show, Read, Eq, Ord)

instance Default CharSet where
  def = CharSet "utf-8"

instance AttrMap CharSet where
  attrMap (CharSet t) = "charset" =: t

instance Semigroup CharSet where
  (<>) _ b = b

instance Monoid CharSet where
  mappend = (<>)
  mempty = def

class AttrHasCharSet a where
  attrSetCharSet :: CharSet -> a -> a

-- |
charSet :: AttrHasCharSet a => Text -> a -> a
charSet t = attrSetCharSet (CharSet t)

instance (Reflex t, AttrHasCharSet a) => AttrHasCharSet (Dynamic t a) where
  attrSetCharSet c = fmap (attrSetCharSet c)

------------------------------------------------------------------------------

-- | Whether the command or control is checked, used with input element.
data Checked = Checked
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Checked where
  def = Checked

instance AttrMap Checked where
  attrMap Checked = "checked" =: T.empty

instance Semigroup Checked where
  (<>) _ b = b

instance Monoid Checked where
  mappend = (<>)
  mempty = def

class AttrHasChecked a where
  attrSetChecked :: Checked -> a -> a

-- | This sets the attribute.
checked :: AttrHasChecked a => a -> a
checked = attrSetChecked Checked

instance (Reflex t, AttrHasChecked a) => AttrHasChecked (Dynamic t a) where
  attrSetChecked c = fmap (attrSetChecked c)

------------------------------------------------------------------------------

-- |
newtype Cite = Cite URL
  deriving (Show, Read, Eq, Ord)

instance Default Cite where
  def = Cite (URL T.empty)

instance AttrMap Cite where
  attrMap (Cite (URL i)) = "cite" =: i

instance Semigroup Cite where
  (<>) _ b = b

instance Monoid Cite where
  mappend = (<>)
  mempty = def

class AttrHasCite a where
  attrSetCite :: Cite -> a -> a

-- |
cite :: AttrHasCite a => URL -> a -> a
cite i = attrSetCite (Cite i)

instance (Reflex t, AttrHasCite a) => AttrHasCite (Dynamic t a) where
  attrSetCite c = fmap (attrSetCite c)

------------------------------------------------------------------------------

-- | Maximum number of characters per line in textarea element.
-- Non-negative integer greater than zero.
newtype Cols = Cols Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Cols where
  attrMap (Cols i) = "cols" =: T.pack (show i)

instance Semigroup Cols where
  (<>)_ b = b

instance Monoid Cols where
  mappend = (<>)
  mempty = Cols 1

class AttrHasCols a where
  attrSetCols :: Cols -> a -> a

-- |
cols :: AttrHasCols a => Int -> a -> a
cols i = attrSetCols (Cols i)

instance (Reflex t, AttrHasCols a) => AttrHasCols (Dynamic t a) where
  attrSetCols c = fmap (attrSetCols c)

------------------------------------------------------------------------------

-- | Number of columns that the cell is to span, used with td and th elements
-- with tables.
--
-- Non-negative integer greater than zero.
newtype ColSpan = ColSpan Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap ColSpan where
  attrMap (ColSpan i) = "colspan" =: T.pack (show i)

instance Semigroup ColSpan where
  (<>) _ b = b

instance Monoid ColSpan where
  mappend = (<>)
  mempty = ColSpan 1

class AttrHasColSpan a where
  attrSetColSpan :: ColSpan -> a -> a

-- | Set the colspan attribute.
colSpan :: AttrHasColSpan a => Int -> a -> a
colSpan i = attrSetColSpan (ColSpan i)

instance (Reflex t, AttrHasColSpan a) => AttrHasColSpan (Dynamic t a) where
  attrSetColSpan c = fmap (attrSetColSpan c)

------------------------------------------------------------------------------

-- |
-- E.g. "width=device-width, initial-scale=1, shrink-to-fit=no" together
-- with name="viewport"
newtype Content = Content Text
  deriving (Show, Read, Eq, Ord)

instance Default Content where
  def = Content T.empty

instance AttrMap Content where
  attrMap (Content t) = "content" =: t

instance Semigroup Content where
  (<>) (Content a) (Content b) = Content (a <> ", " <> b)

instance Monoid Content where
  mappend = (<>)
  mempty = def

class AttrHasContent a where
  attrSetContent :: Content -> a -> a

-- |
content :: AttrHasContent a => Text -> a -> a
content t = attrSetContent (Content t)

instance (Reflex t, AttrHasContent a) => AttrHasContent (Dynamic t a) where
  attrSetContent c = fmap (attrSetContent c)

------------------------------------------------------------------------------

-- | Show user agent controls, used with audio and video elements.
data Controls = Controls
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Controls where
  def = Controls

instance AttrMap Controls where
  attrMap Controls = "controls" =: T.empty

instance Semigroup Controls where
  (<>) _ b = b

instance Monoid Controls where
  mappend = (<>)
  mempty = def

class AttrHasControls a where
  attrSetControls :: Controls -> a -> a

-- | This sets the attribute.
controls :: AttrHasControls a => a -> a
controls = attrSetControls Controls

instance (Reflex t, AttrHasControls a) => AttrHasControls (Dynamic t a) where
  attrSetControls c = fmap (attrSetControls c)

------------------------------------------------------------------------------

-- |
data CoordPair = CoordPair Double Double
  deriving (Show, Read, Eq, Ord)

cp2txt :: CoordPair -> Text
cp2txt (CoordPair x y) = T.pack (show x) <> "," <> T.pack (show y)

type CoordPairs = [CoordPair]

-- Polygon should have at least three pairs.
-- Wish list: ensure with types that there is enough coordinates.
data Coords = CoordsCircle CoordPair Double | CoordsDefault
  | CoordsPolygon CoordPairs | CoordsRectangle CoordPair CoordPair
  deriving (Show, Read, Eq, Ord)

instance Default Coords where
  def = CoordsDefault

instance AttrMap Coords where
  attrMap CoordsDefault             = "coords" =: T.empty
  attrMap (CoordsCircle cp rad)     = "coords" =: (cp2txt cp <> "," <> T.pack (show rad))
  attrMap (CoordsPolygon cplst)     = "coords" =: fold (cp2txt <$> cplst)
  attrMap (CoordsRectangle cp1 cp2) = "coords" =: (cp2txt cp1 <> cp2txt cp2)

instance Semigroup Coords where
  (<>) (CoordsPolygon as) (CoordsPolygon bs) = CoordsPolygon (as ++ bs)
  (<>) _ b = b

instance Monoid Coords where
  mappend = (<>)
  mempty = def

class AttrHasCoords a where
  attrSetCoords :: Coords -> a -> a


-- |
coordsDefault   :: AttrHasCoords a => a -> a
coordsCircle    :: AttrHasCoords a => CoordPair -> Double -> a -> a
coordsRectangle :: AttrHasCoords a => CoordPair -> CoordPair -> a -> a
coordsPolygon   :: AttrHasCoords a => CoordPairs -> a -> a
coordsDefault           = attrSetCoords CoordsDefault
coordsCircle cp rad     = attrSetCoords (CoordsCircle cp rad)
coordsRectangle cp1 cp2 = attrSetCoords (CoordsRectangle cp1 cp2)
coordsPolygon cplst     = attrSetCoords (CoordsPolygon cplst)

instance (Reflex t, AttrHasCoords a) => AttrHasCoords (Dynamic t a) where
  attrSetCoords c = fmap (attrSetCoords c)

------------------------------------------------------------------------------

-- | How the element handles crossorigin requests, used with audio, img, link,
-- script and video elements.
data CrossOrigin = COanon | COusecredentials | COnocors
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default CrossOrigin where
  def = COnocors

instance AttrMap CrossOrigin where
  attrMap COanon           = "crossorigin" =: "anonymous"
  attrMap COusecredentials = "crossorigin" =: "use-credentials"
  attrMap COnocors         = "crossorigin" =: T.empty

instance Semigroup CrossOrigin where
  (<>) _ b = b

instance Monoid CrossOrigin where
  mappend = (<>)
  mempty = def

class AttrHasCrossOrigin a where
  attrSetCrossOrigin :: CrossOrigin -> a -> a

-- | These set the corresponding attribute.
corsAnon, corsCred, noCors :: AttrHasCrossOrigin a => a -> a
corsAnon = attrSetCrossOrigin COanon
corsCred = attrSetCrossOrigin COusecredentials
noCors   = attrSetCrossOrigin COnocors

instance (Reflex t, AttrHasCrossOrigin a) => AttrHasCrossOrigin (Dynamic t a) where
  attrSetCrossOrigin c = fmap (attrSetCrossOrigin c)

------------------------------------------------------------------------------

-- |
newtype DataUrl = DataUrl URL
  deriving (Show, Read, Eq, Ord)

instance Default DataUrl where
  def = DataUrl (URL T.empty)

instance AttrMap DataUrl where
  attrMap (DataUrl (URL i)) = "data" =: i

instance Semigroup DataUrl where
  (<>) _ b = b

instance Monoid DataUrl where
  mappend = (<>)
  mempty = def

class AttrHasDataUrl a where
  attrSetDataUrl :: DataUrl -> a -> a

-- |
dataUrl :: AttrHasDataUrl a => URL -> a -> a
dataUrl i = attrSetDataUrl (DataUrl i)

instance (Reflex t, AttrHasDataUrl a) => AttrHasDataUrl (Dynamic t a) where
  attrSetDataUrl c = fmap (attrSetDataUrl c)


------------------------------------------------------------------------------

-- |
-- Or is this same as Value? This should be in machine-readable format.
-- Should or could we use BinaryString? Or somethings else?
newtype DataValue = DataValue Text
  deriving (Show, Read, Eq, Ord)

instance Default DataValue where
  def = DataValue T.empty

instance AttrMap DataValue where
  attrMap (DataValue i) = "value" =: i

instance Semigroup DataValue where
  (<>) _ b = b

instance Monoid DataValue where
  mappend = (<>)
  mempty = def

class AttrHasDataValue a where
  attrSetDataValue :: DataValue -> a -> a

-- |
dataValue :: AttrHasDataValue a => Text -> a -> a
dataValue i = attrSetDataValue (DataValue i)

instance (Reflex t, AttrHasDataValue a) => AttrHasDataValue (Dynamic t a) where
  attrSetDataValue c = fmap (attrSetDataValue c)

------------------------------------------------------------------------------

-- |
-- Text should be a valid date string with optional time.
-- Wish list: time-package works here / some other package?
-- Is name ok?
newtype DateTime = DateTime Text
  deriving (Show, Read, Eq, Ord)

instance Default DateTime where
  def = DateTime T.empty

instance AttrMap DateTime where
  attrMap (DateTime i) = "datetime" =: i

instance Semigroup DateTime where
  (<>) _ b = b

instance Monoid DateTime where
  mappend = (<>)
  mempty = def

class AttrHasDateTime a where
  attrSetDateTime :: DateTime -> a -> a

-- |
dateTime :: AttrHasDateTime a => Text -> a -> a
dateTime i = attrSetDateTime (DateTime i)

instance (Reflex t, AttrHasDateTime a) => AttrHasDateTime (Dynamic t a) where
  attrSetDateTime c = fmap (attrSetDateTime c)

------------------------------------------------------------------------------

-- | Enable the track if no other text track is more suitable, used with
-- track element.
--
-- Should be not an empty text.
newtype Default_ = Default_ Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Default_ where
  attrMap (Default_ t) = "default" =: t

-- Is this ok at all?
instance Semigroup Default_ where
  (<>) _ b = b

instance Monoid Default_ where
  mappend = (<>)
  mempty = Default_ "default"

class AttrHasDefault a where
  attrSetDefault :: Default_ -> a -> a

-- |
default_ :: AttrHasDefault a => Text -> a -> a
default_ t = attrSetDefault (Default_ t)

instance (Reflex t, AttrHasDefault a) => AttrHasDefault (Dynamic t a) where
  attrSetDefault c = fmap (attrSetDefault c)

------------------------------------------------------------------------------

-- |
data Defer = Defer
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Defer where
  def = Defer

instance AttrMap Defer where
  attrMap Defer = "defer" =: T.empty

instance Semigroup Defer where
  (<>) _ b = b

instance Monoid Defer where
  mappend = (<>)
  mempty = def

class AttrHasDefer a where
  attrSetDefer :: Defer -> a -> a

-- | This sets the attribute.
defer :: AttrHasDefer a => a -> a
defer = attrSetDefer Defer

instance (Reflex t, AttrHasDefer a) => AttrHasDefer (Dynamic t a) where
  attrSetDefer c = fmap (attrSetDefer c)

------------------------------------------------------------------------------

-- | Name of form field to use for sending the element’s directionality in
-- form submission, and used with input and textarea elements.
--
-- Should be not an empty text.
newtype DirName = DirName Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap DirName where
  attrMap (DirName t) = "dirname" =: t

instance Semigroup DirName where
  (<>) _ b = b

instance Monoid DirName where
  mappend = (<>)
  mempty = DirName "dirname" -- Is this ok at all?

class AttrHasDirName a where
  attrSetDirName :: DirName -> a -> a

-- |
dirName :: AttrHasDirName a => Text -> a -> a
dirName t = attrSetDirName (DirName t)

instance (Reflex t, AttrHasDirName a) => AttrHasDirName (Dynamic t a) where
  attrSetDirName c = fmap (attrSetDirName c)

------------------------------------------------------------------------------

-- | Whether the form control is disabled, used with button, fieldset, input,
-- optgroup, option, select and textarea elements.
data Disabled = Disabled
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Default Disabled where
  def = Disabled

instance AttrMap Disabled where
  attrMap Disabled = "disabled" =: T.empty

instance Semigroup Disabled where
  (<>) _ b = b

instance Monoid Disabled where
  mappend = (<>)
  mempty = def

class AttrHasDisabled a where
  attrSetDisabled :: Disabled -> a -> a

-- | This sets the attribute.
disabled :: AttrHasDisabled a => a -> a
disabled = attrSetDisabled Disabled

instance (Reflex t, AttrHasDisabled a) => AttrHasDisabled (Dynamic t a) where
  attrSetDisabled c = fmap (attrSetDisabled c)

------------------------------------------------------------------------------

-- | Whether to download the resource instead of navigating to it, and its
-- file name if so. Used with a and area elements.
-- Text gives a default file name for the downloaded resource.
newtype Download = Download Text
  deriving (Show, Read, Eq, Ord)

instance Default Download where
  def = Download T.empty

instance AttrMap Download where
  attrMap (Download t) = "download" =: t

instance Semigroup Download where
  (<>) _ b = b

instance Monoid Download where
  mappend = (<>)
  mempty = def

class AttrHasDownload a where
  attrSetDownload :: Download -> a -> a

-- |
download :: AttrHasDownload a => Text -> a -> a
download t = attrSetDownload (Download t)

instance (Reflex t, AttrHasDownload a) => AttrHasDownload (Dynamic t a) where
  attrSetDownload c = fmap (attrSetDownload c)

------------------------------------------------------------------------------

-- |
data EncType = ETapp | ETmulti | ETtext
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default EncType where
  def = ETapp

instance AttrMap EncType where
  attrMap ETapp   = "enctype" =: "application/x-www-form-urlencoded"
  attrMap ETmulti = "enctype" =: "multipart/form-data"
  attrMap ETtext  = "enctype" =: "text/plain"

instance Semigroup EncType where
  (<>) _ b = b

instance Monoid EncType where
  mappend = (<>)
  mempty = def

class AttrHasEncType a where
  attrSetEncType :: EncType -> a -> a

-- | These set the corresponding attribute.
etUrlEncoded, etData, etPlain :: AttrHasEncType a => a -> a
etUrlEncoded = attrSetEncType ETapp
etData       = attrSetEncType ETmulti
etPlain      = attrSetEncType ETtext

instance (Reflex t, AttrHasEncType a) => AttrHasEncType (Dynamic t a) where
  attrSetEncType c = fmap (attrSetEncType c)

------------------------------------------------------------------------------

-- |
-- Text must be an Id of a labelable element.
newtype ForId = ForId Text
  deriving (Show, Read, Eq, Ord)

instance Default ForId where
  def = ForId T.empty

instance AttrMap ForId where
  attrMap (ForId f) = "for" =: f

instance Semigroup ForId where
  (<>) _ b = b

instance Monoid ForId where
  mappend = (<>)
  mempty = def

class AttrHasForId a where
  attrSetForId :: ForId -> a -> a

-- |
forId :: AttrHasForId a => Text -> a -> a
forId t = attrSetForId (ForId t)

instance (Reflex t, AttrHasForId a) => AttrHasForId (Dynamic t a) where
  attrSetForId c = fmap (attrSetForId c)

------------------------------------------------------------------------------

-- | Associates the control with a form element, used with button, fieldset,
-- input, label, object, output, select and textarea elements.
--
-- There are rules how the form-id should be constructed. We wrap this to a
-- separate type so that it can be more easily replaced later with a proper
-- construction.
-- On a wish-list: if it is possible to ensure with the type-system that form
-- id's are used correctly, then that kind of solution here.
newtype FormId = FormId Text
  deriving (Show, Read, Eq, Ord)

newtype Form = Form FormId
  deriving (Show, Read, Eq, Ord)

instance Default Form where
  def = Form (FormId T.empty)

instance AttrMap Form where
  attrMap (Form (FormId f)) = "form" =: f

instance Semigroup Form where
  (<>) _ b = b

instance Monoid Form where
  mappend = (<>)
  mempty = def

class AttrHasForm a where
  attrSetForm :: Form -> a -> a

-- | Set form id.
form :: AttrHasForm a => FormId -> a -> a
form fi = attrSetForm (Form fi)

instance (Reflex t, AttrHasForm a) => AttrHasForm (Dynamic t a) where
  attrSetForm c = fmap (attrSetForm c)

------------------------------------------------------------------------------

-- | URL to use for form submission, used with button and input elements.
newtype FormAction = FormAction URL
  deriving (Show, Read, Eq, Ord)

instance Default FormAction where
  def = FormAction (URL T.empty)

instance AttrMap FormAction where
  attrMap (FormAction (URL h)) = "formaction" =: h

instance Semigroup FormAction where
  (<>) _ b = b

instance Monoid FormAction where
  mappend = (<>)
  mempty = def

class AttrHasFormAction a where
  attrSetFormAction :: FormAction -> a -> a

-- |
formAction :: AttrHasFormAction a => URL -> a -> a
formAction u = attrSetFormAction (FormAction u)

instance (Reflex t, AttrHasFormAction a) => AttrHasFormAction (Dynamic t a) where
  attrSetFormAction c = fmap (attrSetFormAction c)

------------------------------------------------------------------------------

-- | Form data set encoding type to use for form submission, used with
-- button and input elements.
data FormEncType = FETapp | FETmulti | FETtext
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default FormEncType where
  def = FETapp

instance AttrMap FormEncType where
  attrMap FETapp   = "formenctype" =: "application/x-www-form-urlencoded"
  attrMap FETmulti = "formenctype" =: "multipart/form-data"
  attrMap FETtext  = "formenctype" =: "text/plain"

instance Semigroup FormEncType where
  (<>) _ b = b

instance Monoid FormEncType where
  mappend = (<>)
  mempty = def

class AttrHasFormEncType a where
  attrSetFormEncType :: FormEncType -> a -> a

-- | These set the corresponding attribute.
formUrlEncoded, formData, formPlain :: AttrHasFormEncType a => a -> a
formUrlEncoded = attrSetFormEncType FETapp
formData       = attrSetFormEncType FETmulti
formPlain      = attrSetFormEncType FETtext

instance (Reflex t, AttrHasFormEncType a) => AttrHasFormEncType (Dynamic t a) where
  attrSetFormEncType c = fmap (attrSetFormEncType c)

------------------------------------------------------------------------------

-- | HTTP method to use for form submission, used with button and input
-- elements.
data FormMethod = FMget | FMpost | FMdialog
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default FormMethod where
  def = FMget

instance AttrMap FormMethod where
  attrMap FMget    = "formmethod" =: "get"
  attrMap FMpost   = "formmethod" =: "post"
  attrMap FMdialog = "formmethod" =: "dialog"

instance Semigroup FormMethod where
  (<>) _ b = b

instance Monoid FormMethod where
  mappend = (<>)
  mempty = def

class AttrHasFormMethod a where
  attrSetFormMethod :: FormMethod -> a -> a

-- | These set the corresponding attribute.
formGet, formPost, formDialog :: AttrHasFormMethod a => a -> a
formGet    = attrSetFormMethod FMget
formPost   = attrSetFormMethod FMpost
formDialog = attrSetFormMethod FMdialog

instance (Reflex t, AttrHasFormMethod a) => AttrHasFormMethod (Dynamic t a) where
  attrSetFormMethod c = fmap (attrSetFormMethod c)

------------------------------------------------------------------------------

-- | Bypass form control validation for form submission, used with
-- button and input elements.
data FormNoValidate = FormNoValidate

instance Default FormNoValidate where
  def = FormNoValidate

instance AttrMap FormNoValidate where
  attrMap FormNoValidate = "formnovalidate" =: T.empty

instance Semigroup FormNoValidate where
  (<>) _ b = b

instance Monoid FormNoValidate where
  mappend = (<>)
  mempty = def

class AttrHasFormNoValidate a where
  attrSetFormNoValidate :: FormNoValidate -> a -> a

-- | This sets the attribute.
formNoValidate :: AttrHasFormNoValidate a => a -> a
formNoValidate = attrSetFormNoValidate FormNoValidate

instance (Reflex t, AttrHasFormNoValidate a) => AttrHasFormNoValidate (Dynamic t a) where
  attrSetFormNoValidate c = fmap (attrSetFormNoValidate c)

------------------------------------------------------------------------------

-- |
-- FTname is a valid browser context name, that is, a name that does not start
-- with U+005F LOW LINE character (underscore).
-- Wish list: use types to ensure that the FTname is correct.
data FormTarget = FTblank | FTself | FTparent | FTtop | FTname Text
  deriving (Show, Read, Eq, Ord)

instance Default FormTarget where
  def = FTname T.empty

instance AttrMap FormTarget where
  attrMap FTblank    = "formtarget" =: "_blank"
  attrMap FTself     = "formtarget" =: "_self"
  attrMap FTparent   = "formtarget" =: "_parent"
  attrMap FTtop      = "formtarget" =: "_top"
  attrMap (FTname t) = "formtarget" =: t

instance Semigroup FormTarget where
  (<>) _ b = b

instance Monoid FormTarget where
  mappend = (<>)
  mempty = def

class AttrHasFormTarget a where
  attrSetFormTarget :: FormTarget -> a -> a

-- | These set the corresponding attribute.
formTargetBlank, formTargetSelf, formTargetParent, formTargetTop
  :: AttrHasFormTarget a => a -> a
formTargetBlank  = attrSetFormTarget FTblank
formTargetSelf   = attrSetFormTarget FTself
formTargetParent = attrSetFormTarget FTparent
formTargetTop    = attrSetFormTarget FTtop

formTargetName :: AttrHasFormTarget a => Text -> a -> a
formTargetName t = attrSetFormTarget (FTname t)

instance (Reflex t, AttrHasFormTarget a) => AttrHasFormTarget (Dynamic t a) where
  attrSetFormTarget c = fmap (attrSetFormTarget c)

------------------------------------------------------------------------------

-- | The header cells for this cell, used with td and th elements with tables.
-- Text is an id-list.
-- Wish list: handle id's differently.
newtype Headers = Headers Text
  deriving (Show, Read, Eq, Ord)

instance Default Headers where
  def = Headers T.empty

instance AttrMap Headers where
  attrMap (Headers i) = "headers" =: i

instance Semigroup Headers where
  (<>) (Headers a) (Headers b) = Headers (a <> " " <> b)

instance Monoid Headers where
  mappend = (<>)
  mempty = def

class AttrHasHeaders a where
  attrSetHeaders :: Headers -> a -> a

class AttrGetHeaders a where
  attrGetHeaders :: a -> Headers

-- |
headers :: AttrHasHeaders a => Text -> a -> a
headers i = attrSetHeaders (Headers i)

-- |
addHeaders :: (AttrHasHeaders a, AttrGetHeaders a) => Text -> a -> a
addHeaders h o = attrSetHeaders (Headers $ oh <> " " <> h) o
  where
    Headers oh = attrGetHeaders o

instance (Reflex t, AttrHasHeaders a) => AttrHasHeaders (Dynamic t a) where
  attrSetHeaders c = fmap (attrSetHeaders c)

------------------------------------------------------------------------------

-- | Vertical dimension used with canvas, embed, iframe, img, input, object
-- and video elements.
newtype Height = Height Int
  deriving (Show, Read, Eq, Ord)

instance Default Height where
  def = Height 300

instance AttrMap Height where
  attrMap (Height i) = "height" =: T.pack (show i)

instance Semigroup Height where
  (<>) _ b = b

instance Monoid Height where
  mappend = (<>)
  mempty = def

class AttrHasHeight a where
  attrSetHeight :: Height -> a -> a

-- | Set height.
height :: AttrHasHeight a => Int -> a -> a
height i = attrSetHeight (Height i)

instance (Reflex t, AttrHasHeight a) => AttrHasHeight (Dynamic t a) where
  attrSetHeight c = fmap (attrSetHeight c)

------------------------------------------------------------------------------

-- |
newtype High = High Double
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap High where
  attrMap (High d) = "high" =: T.pack (show d)

instance Semigroup High where
  (<>) _ b = b

instance Monoid High where
  mappend = (<>)
  mempty = High 0 -- Is this ok?

class AttrHasHigh a where
  attrSetHigh :: High -> a -> a

-- |
high :: AttrHasHigh a => Double -> a -> a
high d = attrSetHigh (High d)

instance (Reflex t, AttrHasHigh a) => AttrHasHigh (Dynamic t a) where
  attrSetHigh c = fmap (attrSetHigh c)

------------------------------------------------------------------------------

-- | Address. Note that URL is something we define ourselves. This is probaly
-- going to change. TODO
newtype Href = Href URL
  deriving (Show, Read, Eq, Ord)

instance Default Href where
  def = Href (URL T.empty)

instance AttrMap Href where
  attrMap (Href (URL h)) = "href" =: h

instance Semigroup Href where
  (<>) _ b = b

instance Monoid Href where
  mappend = (<>)
  mempty = def

class AttrHasHref a where
  attrSetHref :: Href -> a -> a

-- |
href :: AttrHasHref a => URL -> a -> a
href u = attrSetHref (Href u)

instance (Reflex t, AttrHasHref a) => AttrHasHref (Dynamic t a) where
  attrSetHref c = fmap (attrSetHref c)

------------------------------------------------------------------------------

-- |
-- This is purely advisory attribute.
-- https://w3c.github.io/html/links.html#element-attrdef-a-hreflang
-- https://tools.ietf.org/html/bcp47
newtype HrefLang = HrefLang Text
  deriving (Show, Read, Eq, Ord)

instance Default HrefLang where
  def = HrefLang T.empty

instance AttrMap HrefLang where
  attrMap (HrefLang t) = "hreflang" =: t

instance Semigroup HrefLang where
  (<>) _ b = b

instance Monoid HrefLang where
  mappend = (<>)
  mempty = def

class AttrHasHrefLang a where
  attrSetHrefLang :: HrefLang -> a -> a

-- |
hreflang :: AttrHasHrefLang a => Text -> a -> a
hreflang t = attrSetHrefLang (HrefLang t)

instance (Reflex t, AttrHasHrefLang a) => AttrHasHrefLang (Dynamic t a) where
  attrSetHrefLang c = fmap (attrSetHrefLang c)

------------------------------------------------------------------------------

-- |
-- E.g. "utf-8"
-- See https://w3c.github.io/html/document-metadata.html#element-attrdef-meta-http-equiv
newtype HttpEquiv = HttpEquiv Text
  deriving (Show, Read, Eq, Ord)

instance Default HttpEquiv where
  def = HttpEquiv T.empty

instance AttrMap HttpEquiv where
  attrMap (HttpEquiv t) = "http-equiv" =: t

instance Semigroup HttpEquiv where
  (<>) _ b = b

instance Monoid HttpEquiv where
  mappend = (<>)
  mempty = def

class AttrHasHttpEquiv a where
  attrSetHttpEquiv :: HttpEquiv -> a -> a

-- |
httpEquiv :: AttrHasHttpEquiv a => Text -> a -> a
httpEquiv t = attrSetHttpEquiv (HttpEquiv t)

instance (Reflex t, AttrHasHttpEquiv a) => AttrHasHttpEquiv (Dynamic t a) where
  attrSetHttpEquiv c = fmap (attrSetHttpEquiv c)

------------------------------------------------------------------------------

-- |
newtype Icon = Icon Text
  deriving (Show, Read, Eq, Ord)

instance Default Icon where
  def = Icon T.empty

instance AttrMap Icon where
  attrMap (Icon t) = "icon" =: t

instance Semigroup Icon where
  (<>) _ b = b

instance Monoid Icon where
  mappend = (<>)
  mempty = def

class AttrHasIcon a where
  attrSetIcon :: Icon -> a -> a

-- |
icon :: AttrHasIcon a => Text -> a -> a
icon t = attrSetIcon (Icon t)

instance (Reflex t, AttrHasIcon a) => AttrHasIcon (Dynamic t a) where
  attrSetIcon c = fmap (attrSetIcon c)

------------------------------------------------------------------------------

-- |
data InputMode = IMverbatim | IMlatin | IMlatinname | IMlatinprose
  | IMfullwidthlatin | IMkana | IMkananame | IMkatakana | IMnumeric | IMtel
  | IMemail | IMurl
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- No default.

instance AttrMap InputMode where
  attrMap IMverbatim       = "inputmode" =: "verbatim"
  attrMap IMlatin          = "inputmode" =: "latin"
  attrMap IMlatinname      = "inputmode" =: "latinname"
  attrMap IMlatinprose     = "inputmode" =: "latinprose"
  attrMap IMfullwidthlatin = "inputmode" =: "fullwidthlatin"
  attrMap IMkana           = "inputmode" =: "kana"
  attrMap IMkananame       = "inputmode" =: "kananame"
  attrMap IMkatakana       = "inputmode" =: "katakana"
  attrMap IMnumeric        = "inputmode" =: "numeric"
  attrMap IMtel            = "inputmode" =: "tel"
  attrMap IMemail          = "inputmode" =: "email"
  attrMap IMurl            = "inputmode" =: "url"

instance Semigroup InputMode where
  (<>) _ b = b

instance Monoid InputMode where
  mappend = (<>)
  mempty = IMlatin -- Is this ok at all?

class AttrHasInputMode a where
  attrSetInputMode :: InputMode -> a -> a

-- | These set the corresponding attribute.
imVerbatim, imLatin, imLatinName, imLatinProse, imFullWidthLatin, imKana,
  imKanaName, imKatakana, imNumeric, imTel, imEmail, imUrl
    :: AttrHasInputMode a => a -> a
imVerbatim       = attrSetInputMode IMverbatim
imLatin          = attrSetInputMode IMlatin
imLatinName      = attrSetInputMode IMlatinname
imLatinProse     = attrSetInputMode IMlatinprose
imFullWidthLatin = attrSetInputMode IMfullwidthlatin
imKana           = attrSetInputMode IMkana
imKanaName       = attrSetInputMode IMkananame
imKatakana       = attrSetInputMode IMkatakana
imNumeric        = attrSetInputMode IMnumeric
imTel            = attrSetInputMode IMtel
imEmail          = attrSetInputMode IMemail
imUrl            = attrSetInputMode IMurl

instance (Reflex t, AttrHasInputMode a) => AttrHasInputMode (Dynamic t a) where
  attrSetInputMode c = fmap (attrSetInputMode c)

------------------------------------------------------------------------------

-- |
-- Input-type can be one of the following:
--  button checkbox color date datetime-local email file hidden image month
--  number password radio range reset search submit tel text time url week
data InputType = ITbutton | ITcheckbox | ITcolor | ITdate | ITdatetimeLocal
               | ITemail | ITfile | IThidden | ITimage | ITmonth | ITnumber
               | ITpassword | ITradio | ITrange | ITreset | ITsearch
               | ITsubmit | ITtel | ITtext | ITtime | ITurl | ITweek
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default InputType where
  def = ITtext

instance AttrMap InputType where
  attrMap ITbutton         = "type" =: "button"
  attrMap ITcheckbox       = "type" =: "checkbox"
  attrMap ITcolor          = "type" =: "color"
  attrMap ITdate           = "type" =: "date"
  attrMap ITdatetimeLocal  = "type" =: "datetime-local"
  attrMap ITemail          = "type" =: "email"
  attrMap ITfile           = "type" =: "file"
  attrMap IThidden         = "type" =: "hidden"
  attrMap ITimage          = "type" =: "image"
  attrMap ITmonth          = "type" =: "month"
  attrMap ITnumber         = "type" =: "number"
  attrMap ITpassword       = "type" =: "password"
  attrMap ITradio          = "type" =: "radio"
  attrMap ITrange          = "type" =: "range"
  attrMap ITreset          = "type" =: "reset"
  attrMap ITsearch         = "type" =: "search"
  attrMap ITsubmit         = "type" =: "submit"
  attrMap ITtel            = "type" =: "tel"
  attrMap ITtext           = "type" =: "text"
  attrMap ITtime           = "type" =: "time"
  attrMap ITurl            = "type" =: "url"
  attrMap ITweek           = "type" =: "week"

instance Semigroup InputType where
  (<>) _ b = b

instance Monoid InputType where
  mappend = (<>)
  mempty = def

class AttrHasInputType a where
  attrSetInputType :: InputType -> a -> a

-- | These set the corresponding attribute.
itButton, itCheckbox, itColor, itDate, itDatetimeLocal, itEmail, itFile,
  itHidden, itImage, itMonth, itNumber, itPassword, itRadio, itRange, itReset,
  itSearch, itSubmit, itTel, itText, itTime, itUrl, itWeek
    :: AttrHasInputType a => a -> a
itButton        = attrSetInputType ITbutton
itCheckbox      = attrSetInputType ITcheckbox
itColor         = attrSetInputType ITcolor
itDate          = attrSetInputType ITdate
itDatetimeLocal = attrSetInputType ITdatetimeLocal
itEmail         = attrSetInputType ITemail
itFile          = attrSetInputType ITfile
itHidden        = attrSetInputType IThidden
itImage         = attrSetInputType ITimage
itMonth         = attrSetInputType ITmonth
itNumber        = attrSetInputType ITnumber
itPassword      = attrSetInputType ITpassword
itRadio         = attrSetInputType ITradio
itRange         = attrSetInputType ITrange
itReset         = attrSetInputType ITreset
itSearch        = attrSetInputType ITsearch
itSubmit        = attrSetInputType ITsubmit
itTel           = attrSetInputType ITtel
itText          = attrSetInputType ITtext
itTime          = attrSetInputType ITtime
itUrl           = attrSetInputType ITurl
itWeek          = attrSetInputType ITweek


instance (Reflex t, AttrHasInputType a) => AttrHasInputType (Dynamic t a) where
  attrSetInputType c = fmap (attrSetInputType c)


------------------------------------------------------------------------------

-- |
-- This is not in HTML 5 standard (but used, e.g., by cdn:s)
newtype Integrity = Integrity Text
  deriving (Show, Read, Eq, Ord)

instance Default Integrity where
  def = Integrity T.empty

instance AttrMap Integrity where
  attrMap (Integrity i) = "integrity" =: i

instance Semigroup Integrity where
  (<>) _ b = b

instance Monoid Integrity where
  mappend = (<>)
  mempty = def

class AttrHasIntegrity a where
  attrSetIntegrity :: Integrity -> a -> a

-- |
integrity :: AttrHasIntegrity a => Text -> a -> a
integrity i = attrSetIntegrity (Integrity i)

instance (Reflex t, AttrHasIntegrity a) => AttrHasIntegrity (Dynamic t a) where
  attrSetIntegrity c = fmap (attrSetIntegrity c)

------------------------------------------------------------------------------

-- | Is the image a server-side image map in img element.
data IsMap = IsMap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default IsMap where
  def = IsMap

instance AttrMap IsMap where
  attrMap IsMap = "ismap" =: T.empty

instance Semigroup IsMap where
  (<>) _ b = b

instance Monoid IsMap where
  mappend = (<>)
  mempty = def

class AttrHasIsMap a where
  attrSetIsMap :: IsMap -> a -> a

-- | This sets the attribute.
isMap :: AttrHasIsMap a => a -> a
isMap = attrSetIsMap IsMap

instance (Reflex t, AttrHasIsMap a) => AttrHasIsMap (Dynamic t a) where
  attrSetIsMap c = fmap (attrSetIsMap c)

------------------------------------------------------------------------------

-- | KeyType
-- See: https://www.w3schools.com/tags/att_keygen_keytype.asp
-- and
-- https://www.w3.org/TR/html5/forms.html#attr-keygen-keytype
data KeyType = KTrsa | KTdsa | KTec
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default KeyType where
  def = KTrsa

instance AttrMap KeyType where
  attrMap KTrsa = "keytype" =: "rsa"
  attrMap KTdsa = "keytype" =: "dsa"
  attrMap KTec  = "keytype" =: "ec"

instance Semigroup KeyType where
  (<>) _ b = b

instance Monoid KeyType where
  mappend = (<>)
  mempty = def

class AttrHasKeyType a where
  attrSetKeyType :: KeyType -> a -> a

-- | These set the corresponding attribute.
keyTypeRSA, keyTypeDSA, keyTypeEC
  :: AttrHasKeyType a => a -> a
keyTypeRSA = attrSetKeyType KTrsa
keyTypeDSA = attrSetKeyType KTdsa
keyTypeEC  = attrSetKeyType KTec

instance (Reflex t, AttrHasKeyType a) => AttrHasKeyType (Dynamic t a) where
  attrSetKeyType c = fmap (attrSetKeyType c)

------------------------------------------------------------------------------

-- | The type of text track used with track element.
data Kind = KindSubtitles | KindCaptions | KindDescs | KindChapters
          | KindMetadata
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Kind where
  def = KindSubtitles

instance AttrMap Kind where
  attrMap KindSubtitles = "kind" =: "subtitles"
  attrMap KindCaptions  = "kind" =: "captions"
  attrMap KindDescs     = "kind" =: "descriptions"
  attrMap KindChapters  = "kind" =: "chapters"
  attrMap KindMetadata  = "kind" =: "metadata"

instance Semigroup Kind where
  (<>) _ b = b

instance Monoid Kind where
  mappend = (<>)
  mempty = def

class AttrHasKind a where
  attrSetKind :: Kind -> a -> a

-- | These set the corresponding attribute.
kindSubtitles, kindCaptions, kindDescs, kindChapters, kindMeta
  :: AttrHasKind a => a -> a
kindSubtitles = attrSetKind KindSubtitles
kindCaptions  = attrSetKind KindCaptions
kindDescs     = attrSetKind KindDescs
kindChapters  = attrSetKind KindChapters
kindMeta      = attrSetKind KindMetadata

instance (Reflex t, AttrHasKind a) => AttrHasKind (Dynamic t a) where
  attrSetKind c = fmap (attrSetKind c)

------------------------------------------------------------------------------

-- | User-visible label, used with optgroup, option and track elements.
newtype Label = Label Text
  deriving (Show, Read, Eq, Ord)

instance Default Label where
  def = Label T.empty

instance AttrMap Label where
  attrMap (Label t) = "label" =: t

instance Semigroup Label where
  (<>) _ b = b

instance Monoid Label where
  mappend = (<>)
  mempty = def

class AttrHasLabel a where
  attrSetLabel :: Label -> a -> a

-- |
label :: AttrHasLabel a => Text -> a -> a
label i = attrSetLabel (Label i)

instance (Reflex t, AttrHasLabel a) => AttrHasLabel (Dynamic t a) where
  attrSetLabel c = fmap (attrSetLabel c)

------------------------------------------------------------------------------

-- | List of autocomplete options, used with input element.
-- Wish list: handle id names
newtype List = List Text
  deriving (Show, Read, Eq, Ord)

instance Default List where
  def = List T.empty

instance AttrMap List where
  attrMap (List t) = "list" =: t

instance Semigroup List where
  (<>) _ b = b

instance Monoid List where
  mappend = (<>)
  mempty = def

class AttrHasList a where
  attrSetList :: List -> a -> a

-- |
list :: AttrHasList a => Text -> a -> a
list i = attrSetList (List i)

instance (Reflex t, AttrHasList a) => AttrHasList (Dynamic t a) where
  attrSetList c = fmap (attrSetList c)

------------------------------------------------------------------------------

-- | A link to a fuller description of the image, used with img element.
newtype LongDesc = LongDesc URL
  deriving (Show, Read, Eq, Ord)

instance Default LongDesc where
  def = LongDesc (URL T.empty)

instance AttrMap LongDesc where
  attrMap (LongDesc (URL u)) = "longdesc" =: u

instance Semigroup LongDesc where
  (<>) _ b = b

instance Monoid LongDesc where
  mappend = (<>)
  mempty = def

class AttrHasLongDesc a where
  attrSetLongDesc :: LongDesc -> a -> a

-- |
longDesc :: AttrHasLongDesc a => URL -> a -> a
longDesc u = attrSetLongDesc (LongDesc u)

instance (Reflex t, AttrHasLongDesc a) => AttrHasLongDesc (Dynamic t a) where
  attrSetLongDesc c = fmap (attrSetLongDesc c)

------------------------------------------------------------------------------

-- |  Whether to loop the media resource, used with video and audio elements.
data Loop = Loop
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Loop where
  def = Loop

instance AttrMap Loop where
  attrMap Loop = "loop" =: T.empty

instance Semigroup Loop where
  (<>) _ b = b

instance Monoid Loop where
  mappend = (<>)
  mempty = def

class AttrHasLoop a where
  attrSetLoop :: Loop -> a -> a

-- | This sets the attribute.
loop :: AttrHasLoop a => a -> a
loop = attrSetLoop Loop

-- |
instance (Reflex t, AttrHasLoop a) => AttrHasLoop (Dynamic t a) where
  attrSetLoop c = fmap (attrSetLoop c)

------------------------------------------------------------------------------

-- |
newtype Low = Low Double
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Low where
  attrMap (Low d) = "low" =: T.pack (show d)

instance Semigroup Low where
  (<>) _ b = b

instance Monoid Low where
  mappend = (<>)
  mempty = Low 0

class AttrHasLow a where
  attrSetLow :: Low -> a -> a

-- |
low :: AttrHasLow a => Double -> a -> a
low d = attrSetLow (Low d)

instance (Reflex t, AttrHasLow a) => AttrHasLow (Dynamic t a) where
  attrSetLow c = fmap (attrSetLow c)

------------------------------------------------------------------------------

-- |
newtype Manifest = Manifest URL
  deriving (Show, Read, Eq, Ord)

instance AttrMap Manifest where
  attrMap (Manifest (URL u)) = "manifest" =: u

instance Semigroup Manifest where
  (<>) _ b = b

instance Monoid Manifest where
  mappend = (<>)
  mempty = Manifest (URL "")  -- Is this ok at all?

class AttrHasManifest a where
  attrSetManifest :: Manifest -> a -> a

-- |
manifest :: AttrHasManifest a => URL -> a -> a
manifest u = attrSetManifest (Manifest u)

instance (Reflex t, AttrHasManifest a) => AttrHasManifest (Dynamic t a) where
  attrSetManifest c = fmap (attrSetManifest c)

------------------------------------------------------------------------------

-- |
-- MaxDT - text should be a valid date.
-- MaxT - is it needed? Check & TODO!
-- Wish list: MaxDT param could be a Date/DateTime or something similar.
data Max = MaxDT Text | MaxI Int | MaxD Double | MaxT Text
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Max where
  attrMap (MaxDT i) = "max" =: i
  attrMap (MaxI i) = "max" =: T.pack (show i)
  attrMap (MaxD i) = "max" =: T.pack (show i)
  attrMap (MaxT i) = "max" =: i

instance Semigroup Max where
  (<>) _ b = b

instance Monoid Max where
  mappend = (<>)
  mempty = MaxT ""

class AttrHasMax a where
  attrSetMax :: Max -> a -> a

-- |
maxI :: AttrHasMax a => Int -> a -> a
maxI i = attrSetMax (MaxI i)
maxD :: AttrHasMax a => Double -> a -> a
maxD i = attrSetMax (MaxD i)
maxDT :: AttrHasMax a => Text -> a -> a -- Replace Text with Date. TODO!
maxDT i = attrSetMax (MaxDT i)
maxT :: AttrHasMax a => Text -> a -> a -- This is possibly removed.
maxT i = attrSetMax (MaxT i)

instance (Reflex t, AttrHasMax a) => AttrHasMax (Dynamic t a) where
  attrSetMax c = fmap (attrSetMax c)

------------------------------------------------------------------------------

-- | Maximum length of value in input and textarea elements.
newtype MaxLength = MaxLength Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap MaxLength where
  attrMap (MaxLength i) = "max" =: T.pack (show i)

instance Semigroup MaxLength where
  (<>) _ b = b

instance Monoid MaxLength where
  mappend = (<>)
  mempty = MaxLength 0

class AttrHasMaxLength a where
  attrSetMaxLength :: MaxLength -> a -> a

-- | Set the maximum length of value in input and textarea elements.
maxLength :: AttrHasMaxLength a => Int -> a -> a
maxLength i = attrSetMaxLength (MaxLength i)

instance (Reflex t, AttrHasMaxLength a) => AttrHasMaxLength (Dynamic t a) where
  attrSetMaxLength c = fmap (attrSetMaxLength c)

------------------------------------------------------------------------------

-- |
-- Media query list
-- HTML4:
-- ‘aural’, ‘braille’, ‘handheld’, ‘print’, ‘projection’, ‘screen’, ‘tty’, ‘tv’.
-- See: https://www.w3.org/TR/css3-mediaqueries/
-- Wish list: Implement approriately.
-- Should we support adding easily media queries into a list?
data MQL = MQaural | MQbraille | MQhandheld | MQprint | MQprojection
         | MQscreen | MQtty | MQtv | MQ Text
  deriving (Show, Read, Eq, Ord)

-- | Applicable media used with link and style elements.
newtype Media = Media MQL
  deriving (Show, Read, Eq, Ord)

instance Default Media where
  def = Media MQscreen

instance AttrMap Media where
  attrMap (Media MQaural)      = "media" =: "aural"
  attrMap (Media MQbraille)    = "media" =: "braille"
  attrMap (Media MQhandheld)   = "media" =: "handheld"
  attrMap (Media MQprint)      = "media" =: "print"
  attrMap (Media MQprojection) = "media" =: "projection"
  attrMap (Media MQscreen)     = "media" =: "screen"
  attrMap (Media MQtty)        = "media" =: "tty"
  attrMap (Media MQtv)         = "media" =: "tv"
  attrMap (Media (MQ t))       = "media" =: t

instance Semigroup Media where
  (<>) _ b = b

instance Monoid Media where
  mappend = (<>)
  mempty = def

class AttrHasMedia a where
  attrSetMedia :: Media -> a -> a

-- | These set the corresponding attribute.
mediaAural, mediaBraille, mediaHandheld, mediaPrint, mediaProjection,
  mediaScreen, mediaTty, mediaTv :: AttrHasMedia a => a -> a
mediaAural      = attrSetMedia (Media MQaural)
mediaBraille    = attrSetMedia (Media MQbraille)
mediaHandheld   = attrSetMedia (Media MQhandheld)
mediaPrint      = attrSetMedia (Media MQprint)
mediaProjection = attrSetMedia (Media MQprojection)
mediaScreen     = attrSetMedia (Media MQscreen)
mediaTty        = attrSetMedia (Media MQtty)
mediaTv         = attrSetMedia (Media MQtv)

-- |
media :: AttrHasMedia a => Text -> a -> a
media m = attrSetMedia (Media (MQ m))

instance (Reflex t, AttrHasMedia a) => AttrHasMedia (Dynamic t a) where
  attrSetMedia c = fmap (attrSetMedia c)

------------------------------------------------------------------------------

-- |
-- See rfc 7231
-- https://tools.ietf.org/html/rfc7231#section-3.1.1.1
-- http://www.iana.org/assignments/media-types/media-types.xhtml
-- e.g. "text/html"
-- Wish list: most common or all (given at iana site) as types with helpers.
newtype MediaType = MediaType Text
  deriving (Show, Read, Eq, Ord)

instance Default MediaType where
  def = MediaType T.empty
instance AttrMap MediaType where
  attrMap (MediaType t) = "type" =: t

instance Semigroup MediaType where
  (<>) _ b = b

instance Monoid MediaType where
  mappend = (<>)
  mempty = def

class AttrHasMediaType a where
  attrSetMediaType :: MediaType -> a -> a

-- | This sets the attribute.
textHtml :: AttrHasMediaType a => a -> a
textHtml = attrSetMediaType (MediaType "text/html")

-- |
mimeTypeTxt :: AttrHasMediaType a => Text -> a -> a
mimeTypeTxt t = attrSetMediaType (MediaType t)

instance (Reflex t, AttrHasMediaType a) => AttrHasMediaType (Dynamic t a) where
  attrSetMediaType c = fmap (attrSetMediaType c)

------------------------------------------------------------------------------

-- |
-- There are rules, how to use menu-attributes.
newtype MenuId = MenuId Text
  deriving (Show, Read, Eq, Ord)

newtype Menu = Menu MenuId
  deriving (Show, Read, Eq, Ord)

instance Default Menu where
  def = Menu (MenuId T.empty)

instance AttrMap Menu where
  attrMap (Menu (MenuId i)) = "menu" =: i

instance Semigroup Menu where
  (<>) _ b = b

instance Monoid Menu where
  mappend = (<>)
  mempty = def

class AttrHasMenu a where
  attrSetMenu :: Menu -> a -> a

-- |
menu :: AttrHasMenu a => MenuId -> a -> a
menu i = attrSetMenu (Menu i)

instance (Reflex t, AttrHasMenu a) => AttrHasMenu (Dynamic t a) where
  attrSetMenu c = fmap (attrSetMenu c)

------------------------------------------------------------------------------

-- |
data MenuItemType = MITcommand
  deriving (Show, Read, Eq, Ord)

instance Default MenuItemType where
  def = MITcommand

instance AttrMap MenuItemType where
  attrMap MITcommand = "type" =: "command"

instance Semigroup MenuItemType where
  (<>) _ b = b

instance Monoid MenuItemType where
  mappend = (<>)
  mempty = def

class AttrHasMenuItemType a where
  attrSetMenuItemType :: MenuItemType -> a -> a

-- | This sets the attribute.
mitCommand :: AttrHasMenuItemType a => a -> a
mitCommand = attrSetMenuItemType MITcommand

instance (Reflex t, AttrHasMenuItemType a) => AttrHasMenuItemType (Dynamic t a) where
  attrSetMenuItemType c = fmap (attrSetMenuItemType c)

------------------------------------------------------------------------------

-- |
data MenuType = MTContext
  deriving (Show, Read, Eq, Ord)

instance Default MenuType where
  def = MTContext

instance AttrMap MenuType where
  attrMap MTContext = "type" =: "context"

instance Semigroup MenuType where
  (<>) _ b = b

instance Monoid MenuType where
  mappend = (<>)
  mempty = def

class AttrHasMenuType a where
  attrSetMenuType :: MenuType -> a -> a

-- | This sets the attribute.
mtContext :: AttrHasMenuType a => a -> a
mtContext = attrSetMenuType MTContext

instance (Reflex t, AttrHasMenuType a) => AttrHasMenuType (Dynamic t a) where
  attrSetMenuType c = fmap (attrSetMenuType c)

------------------------------------------------------------------------------

-- | HTTP method to use for form submission (form element).
data Method = Mget | Mpost | Mdialog
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Method where
  def = Mget

instance AttrMap Method where
  attrMap Mget    = "method" =: "get"
  attrMap Mpost   = "method" =: "post"
  attrMap Mdialog = "method" =: "dialog"

instance Semigroup Method where
  (<>) _ b = b

instance Monoid Method where
  mappend = (<>)
  mempty = def

class AttrHasMethod a where
  attrSetMethod :: Method -> a -> a

-- | These set the corresponding attribute.
mGet, mPost, mDialog :: AttrHasMethod a => a -> a
mGet    = attrSetMethod Mget
mPost   = attrSetMethod Mpost
mDialog = attrSetMethod Mdialog

instance (Reflex t, AttrHasMethod a) => AttrHasMethod (Dynamic t a) where
  attrSetMethod c = fmap (attrSetMethod c)

------------------------------------------------------------------------------

-- |
-- MinDT - text should be a valid date.
-- MinT - is it needed? Check & TODO!
-- Wish list: MinDT param as Date/DateTime or something similar.
data Min = MinDT Text | MinI Int | MinD Double | MinT Text
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Min where
  attrMap (MinDT i) = "min" =: i
  attrMap (MinI i) = "min" =: T.pack (show i)
  attrMap (MinD i) = "min" =: T.pack (show i)
  attrMap (MinT i) = "min" =: i

instance Semigroup Min where
  (<>) _ b = b

instance Monoid Min where
  mappend = (<>)
  mempty = MinT ""  -- Is this ok at all?

class AttrHasMin a where
  attrSetMin :: Min -> a -> a

-- |
minI :: AttrHasMin a => Int -> a -> a
minI i = attrSetMin (MinI i)
minD :: AttrHasMin a => Double -> a -> a
minD d = attrSetMin (MinD d)
minDT :: AttrHasMin a => Text -> a -> a -- Replace Text with Date. TODO!
minDT i = attrSetMin (MinDT i)
minT :: AttrHasMin a => Text -> a -> a -- This is possibly removed.
minT i = attrSetMin (MinT i)

instance (Reflex t, AttrHasMin a) => AttrHasMin (Dynamic t a) where
  attrSetMin c = fmap (attrSetMin c)

------------------------------------------------------------------------------

-- | Minimum length of value in input and textarea elements.
newtype MinLength = MinLength Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap MinLength where
  attrMap (MinLength i) = "min" =: T.pack (show i)

instance Semigroup MinLength where
  (<>) _ b = b

instance Monoid MinLength where
  mappend = (<>)
  mempty = MinLength 0

class AttrHasMinLength a where
  attrSetMinLength :: MinLength -> a -> a

-- |
minLength :: AttrHasMinLength a => Int -> a -> a
minLength i = attrSetMinLength (MinLength i)

instance (Reflex t, AttrHasMinLength a) => AttrHasMinLength (Dynamic t a) where
  attrSetMinLength c = fmap (attrSetMinLength c)

------------------------------------------------------------------------------

-- | Allow multiple values in input and select elements.
data Multiple = Multiple
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Multiple where
  def = Multiple

instance AttrMap Multiple where
  attrMap Multiple = "multiple" =: T.empty

instance Semigroup Multiple where
  (<>) _ b = b

instance Monoid Multiple where
  mappend = (<>)
  mempty = def

class AttrHasMultiple a where
  attrSetMultiple :: Multiple -> a -> a

-- | This sets the attribute.
multiple :: AttrHasMultiple a => a -> a
multiple = attrSetMultiple Multiple

instance (Reflex t, AttrHasMultiple a) => AttrHasMultiple (Dynamic t a) where
  attrSetMultiple c = fmap (attrSetMultiple c)

------------------------------------------------------------------------------

-- | Whether to mute the media resource by default, used with audio and video
-- elements.
data Muted = Muted
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Muted where
  def = Muted

instance AttrMap Muted where
  attrMap Muted = "muted" =: T.empty

instance Semigroup Muted where
  (<>) _ b = b

instance Monoid Muted where
  mappend = (<>)
  mempty = def

class AttrHasMuted a where
  attrSetMuted :: Muted -> a -> a

-- | This sets the attribute.
muted :: AttrHasMuted a => a -> a
muted = attrSetMuted Muted

instance (Reflex t, AttrHasMuted a) => AttrHasMuted (Dynamic t a) where
  attrSetMuted c = fmap (attrSetMuted c)

------------------------------------------------------------------------------

-- | Many elements can use name-attribute.
-- Name cannot be empty.
-- Wish list: use types to ensure that name is not empty.
data Name = Name Text | NameCharset
  deriving (Show, Read, Eq, Ord)

instance Default Name where
  def = Name "defaultname"

instance AttrMap Name where
  attrMap NameCharset = "name" =: "_charset_"
  attrMap (Name h)    = "name" =: h

instance Semigroup Name where
  (<>) _ b = b

instance Monoid Name where
  mappend = (<>)
  mempty = def

class AttrHasName a where
  attrSetName :: Name -> a -> a

-- |
name :: AttrHasName a => Text -> a -> a
name t = attrSetName (Name t)

-- | This sets the attribute.
nameCharSet :: AttrHasName a => a -> a
nameCharSet = attrSetName NameCharset

instance (Reflex t, AttrHasName a) => AttrHasName (Dynamic t a) where
  attrSetName c = fmap (attrSetName c)

------------------------------------------------------------------------------

-- |
-- Cryptographic nonce ("number used once"). Link, script and style elements.
newtype Nonce = Nonce Text

instance Default Nonce where
  def = Nonce T.empty

instance AttrMap Nonce where
  attrMap (Nonce t) = "nonce" =: t

instance Semigroup Nonce where
  (<>) _ b = b

instance Monoid Nonce where
  mappend = (<>)
  mempty = def

class AttrHasNonce a where
  attrSetNonce :: Nonce -> a -> a

-- |
nonce :: AttrHasNonce a => Text -> a -> a
nonce t = attrSetNonce (Nonce t)

instance (Reflex t, AttrHasNonce a) => AttrHasNonce (Dynamic t a) where
  attrSetNonce c = fmap (attrSetNonce c)

------------------------------------------------------------------------------

-- | Bypass form control validation for form submission and used with
-- form element.
data NoValidate = NoValidate

instance Default NoValidate where
  def = NoValidate

instance AttrMap NoValidate where
  attrMap NoValidate = "novalidate" =: T.empty

instance Semigroup NoValidate where
  (<>) _ b = b

instance Monoid NoValidate where
  mappend = (<>)
  mempty = def

class AttrHasNoValidate a where
  attrSetNoValidate :: NoValidate -> a -> a

-- | This sets the attribute.
noValidate :: AttrHasNoValidate a => a -> a
noValidate = attrSetNoValidate NoValidate

instance (Reflex t, AttrHasNoValidate a) => AttrHasNoValidate (Dynamic t a) where
  attrSetNoValidate c = fmap (attrSetNoValidate c)

------------------------------------------------------------------------------

-- |
data OlType = OlDec | OlLowLat | OlUpLat | OlLowRoman | OlUpRoman
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default OlType where
  def = OlDec

instance AttrMap OlType where
  attrMap OlDec      = "type" =: "1"
  attrMap OlLowLat   = "type" =: "a"
  attrMap OlUpLat    = "type" =: "A"
  attrMap OlLowRoman = "type" =: "i"
  attrMap OlUpRoman  = "type" =: "I"

instance Semigroup OlType where
  (<>) _ b = b

instance Monoid OlType where
  mappend = (<>)
  mempty = def

class AttrHasOlType a where
  attrSetOlType :: OlType -> a -> a

-- | These set the corresponding attribute.
olDec, olLowLat, olUpLat, olLowRoman, olUpRoman :: AttrHasOlType a => a -> a
olDec      = attrSetOlType OlDec
olLowLat   = attrSetOlType OlLowLat
olUpLat    = attrSetOlType OlUpLat
olLowRoman = attrSetOlType OlLowRoman
olUpRoman  = attrSetOlType OlUpRoman

instance (Reflex t, AttrHasOlType a) => AttrHasOlType (Dynamic t a) where
  attrSetOlType c = fmap (attrSetOlType c)

------------------------------------------------------------------------------

-- | Can be used with details (details visible) and dialog elements (is dialog
-- box showing).
data Open = Open
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Open where
  def = Open

instance AttrMap Open where
  attrMap Open = "open" =: T.empty

instance Semigroup Open where
  (<>) _ b = b

instance Monoid Open where
  mappend = (<>)
  mempty = def

class AttrHasOpen a where
  attrSetOpen :: Open -> a -> a

-- | This sets the attribute.
open :: AttrHasOpen a => a -> a
open = attrSetOpen Open

instance (Reflex t, AttrHasOpen a) => AttrHasOpen (Dynamic t a) where
  attrSetOpen c = fmap (attrSetOpen c)

------------------------------------------------------------------------------

-- | Optimum value used in meter element.
newtype Optimum = Optimum Double
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Optimum where
  attrMap (Optimum d) = "optimum" =: T.pack (show d)

instance Semigroup Optimum where
  (<>) _ b = b

instance Monoid Optimum where
  mappend = (<>)
  mempty = Optimum 0

class AttrHasOptimum a where
  attrSetOptimum :: Optimum -> a -> a

-- |
optimum :: AttrHasOptimum a => Double -> a -> a
optimum d = attrSetOptimum (Optimum d)

instance (Reflex t, AttrHasOptimum a) => AttrHasOptimum (Dynamic t a) where
  attrSetOptimum c = fmap (attrSetOptimum c)

------------------------------------------------------------------------------

-- | Pattern to be matched by the form control’s value and used with input
-- element.
-- Wish list: replace text with appropriate regular expression type.
newtype Pattern = Pattern Text
  deriving (Show, Read, Eq, Ord)

instance Default Pattern where
  def = Pattern T.empty

instance AttrMap Pattern where
  attrMap (Pattern t) = "pattern" =: t

instance Semigroup Pattern where
  (<>) _ b = b

instance Monoid Pattern where
  mappend = (<>)
  mempty = def

class AttrHasPattern a where
  attrSetPattern :: Pattern -> a -> a

-- |
ptrn :: AttrHasPattern a => Text -> a -> a
ptrn t = attrSetPattern (Pattern t)

instance (Reflex t, AttrHasPattern a) => AttrHasPattern (Dynamic t a) where
  attrSetPattern c = fmap (attrSetPattern c)

------------------------------------------------------------------------------

-- | User-visible label to be placed within the form control with input and
-- textarea elements.
-- The attribute, if specified, must have a value that contains no U+000A LINE
-- FEED (LF) or U+000D CARRIAGE RETURN (CR) characters.
newtype Placeholder = Placeholder Text
  deriving (Show, Read, Eq, Ord)

instance Default Placeholder where
  def = Placeholder T.empty

instance AttrMap Placeholder where
  attrMap (Placeholder t) = "placeholder" =: t

instance Semigroup Placeholder where
  (<>) _ b = b

instance Monoid Placeholder where
  mappend = (<>)
  mempty = def

class AttrHasPlaceholder a where
  attrSetPlaceholder :: Placeholder -> a -> a

-- |
placeholder :: AttrHasPlaceholder a => Text -> a -> a
placeholder t = attrSetPlaceholder (Placeholder t)

instance (Reflex t, AttrHasPlaceholder a) => AttrHasPlaceholder (Dynamic t a) where
  attrSetPlaceholder c = fmap (attrSetPlaceholder c)

------------------------------------------------------------------------------

-- | Poster frame to show prior to video playback with video element.
newtype Poster = Poster URL
  deriving (Show, Read, Eq, Ord)

instance AttrMap Poster where
  attrMap (Poster (URL u)) = "poster" =: u

instance Semigroup Poster where
  (<>) _ b = b

instance Monoid Poster where
  mappend = (<>)
  mempty = Poster (URL "")

class AttrHasPoster a where
  attrSetPoster :: Poster -> a -> a

-- |
poster :: AttrHasPoster a => URL -> a -> a
poster u = attrSetPoster (Poster u)

instance (Reflex t, AttrHasPoster a) => AttrHasPoster (Dynamic t a) where
  attrSetPoster c = fmap (attrSetPoster c)

------------------------------------------------------------------------------

-- | Hints how much buffering the media resource will likely need with audio
-- and video elements.
data Preload = PLnone | PLmetadata | PLauto
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Preload where
  def = PLmetadata

instance AttrMap Preload where
  attrMap PLnone     = "preload" =: "none"
  attrMap PLmetadata = "preload" =: "metadata"
  attrMap PLauto     = "preload" =: "auto"

instance Semigroup Preload where
  (<>) _ b = b

instance Monoid Preload where
  mappend = (<>)
  mempty = def

class AttrHasPreload a where
  attrSetPreload :: Preload -> a -> a

-- | These set the corresponding attribute.
preLoadNone, preLoadMeta, preLoadAuto :: AttrHasPreload a => a -> a
preLoadNone = attrSetPreload PLnone
preLoadMeta = attrSetPreload PLmetadata
preLoadAuto = attrSetPreload PLauto

instance (Reflex t, AttrHasPreload a) => AttrHasPreload (Dynamic t a) where
  attrSetPreload c = fmap (attrSetPreload c)

------------------------------------------------------------------------------

-- | Whether to allow the value to be edited by the user with input and
-- textarea elements.
data ReadOnly = ReadOnly
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default ReadOnly where
  def = ReadOnly

instance AttrMap ReadOnly where
  attrMap ReadOnly = "readonly" =: T.empty

instance Semigroup ReadOnly where
  (<>) _ b = b

instance Monoid ReadOnly where
  mappend = (<>)
  mempty = def

class AttrHasReadOnly a where
  attrSetReadOnly :: ReadOnly -> a -> a

-- | This sets the attribute.
readOnly :: AttrHasReadOnly a => a -> a
readOnly = attrSetReadOnly ReadOnly

instance (Reflex t, AttrHasReadOnly a) => AttrHasReadOnly (Dynamic t a) where
  attrSetReadOnly c = fmap (attrSetReadOnly c)

------------------------------------------------------------------------------
-- Rel-attribute

-- |
-- Link types for both links and a's and areas.
data LinkTypeC = LTalternate | LTauthor | LThelp | LTlicense | LTnext
  | LTprefetch | LTprev | LTsearch
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Link types for links.
data LinkTypeL = LTicon | LTstylesheet
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Link types for a's and areas.
data LinkTypeA = LTbookmark | LTexternal | LTnofollow | LTnoopener
  | LTnoreferrer | LTtag
  deriving (Show, Read, Eq, Ord, Bounded, Enum)


-- |
-- Wish list: add support for the "other link types".
data Rel = RelC LinkTypeC | RelA LinkTypeA | RelL LinkTypeL | RelNo
  deriving (Show, Read, Eq, Ord)

instance Default Rel where
  def = RelNo -- Rel has no default value, this is same as not specifying Rel.

instance AttrMap Rel where
  attrMap RelNo = mempty
  attrMap (RelC LTalternate)  = "rel" =: "alternate"
  attrMap (RelC LTauthor)     = "rel" =: "author"
  attrMap (RelA LTbookmark)   = "rel" =: "bookmark"
  attrMap (RelA LTexternal)   = "rel" =: "external"
  attrMap (RelC LThelp)       = "rel" =: "help"
  attrMap (RelL LTicon)       = "rel" =: "icon"
  attrMap (RelC LTlicense)    = "rel" =: "license"
  attrMap (RelC LTnext)       = "rel" =: "next"
  attrMap (RelA LTnofollow)   = "rel" =: "nofollow"
  attrMap (RelA LTnoopener)   = "rel" =: "noopener"
  attrMap (RelA LTnoreferrer) = "rel" =: "noreferrer"
  attrMap (RelC LTprefetch)   = "rel" =: "prefetch"
  attrMap (RelC LTprev)       = "rel" =: "prev"
  attrMap (RelC LTsearch)     = "rel" =: "search"
  attrMap (RelL LTstylesheet) = "rel" =: "stylesheet"
  attrMap (RelA LTtag)        = "rel" =: "tag"

instance Semigroup Rel where
  (<>) _ b = b

instance Monoid Rel where
  mappend = (<>)
  mempty = def

-- Wish list: change so that we can support html elements appropriately.
-- That is, make separation for links, and a's and areas.
class AttrHasRel a where
  attrSetRel :: Rel -> a -> a

-- | These set the corresponding attribute.
ltAlternate, ltAuthor, ltBookmark, ltExternal, ltHelp , ltIcon, ltLicense,
  ltNext, ltNofollow, ltNoopener, ltNoreferrer, ltPrefetch, ltPrev, ltSearch,
  ltStylesheet, ltTag :: AttrHasRel a => a -> a
ltAlternate  = attrSetRel (RelC LTalternate)
ltAuthor     = attrSetRel (RelC LTauthor)
ltBookmark   = attrSetRel (RelA LTbookmark)
ltExternal   = attrSetRel (RelA LTexternal)
ltHelp       = attrSetRel (RelC LThelp)
ltIcon       = attrSetRel (RelL LTicon)
ltLicense    = attrSetRel (RelC LTlicense)
ltNext       = attrSetRel (RelC LTnext)
ltNofollow   = attrSetRel (RelA LTnofollow)
ltNoopener   = attrSetRel (RelA LTnoopener)
ltNoreferrer = attrSetRel (RelA LTnoreferrer)
ltPrefetch   = attrSetRel (RelC LTprefetch)
ltPrev       = attrSetRel (RelC LTprev)
ltSearch     = attrSetRel (RelC LTsearch)
ltStylesheet = attrSetRel (RelL LTstylesheet)
ltTag        = attrSetRel (RelA LTtag)

instance (Reflex t, AttrHasRel a) => AttrHasRel (Dynamic t a) where
  attrSetRel c = fmap (attrSetRel c)

------------------------------------------------------------------------------

-- |
-- https://www.w3.org/TR/referrer-policy/
data ReferrerPolicy = RPempty | RPnoreferrer | RPnoreferrerwhendowngrade
  | RPsameorigin | RPorigin | RPstrictorigin | RPoriginwhencrossorigin
  | RPstrictoriginwhencrossorigin | RPunsafeurl
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default ReferrerPolicy where
  def = RPempty

instance AttrMap ReferrerPolicy where
  attrMap RPempty                       = "referrerpolicy" =: T.empty
  attrMap RPnoreferrer                  = "referrerpolicy" =: "no-referrer"
  attrMap RPnoreferrerwhendowngrade     = "referrerpolicy" =: "no-referrer-when-downgrade"
  attrMap RPsameorigin                  = "referrerpolicy" =: "same-origin"
  attrMap RPorigin                      = "referrerpolicy" =: "origin"
  attrMap RPstrictorigin                = "referrerpolicy" =: "strict-origin"
  attrMap RPoriginwhencrossorigin       = "referrerpolicy" =: "origin-when-cross-origin"
  attrMap RPstrictoriginwhencrossorigin = "referrerpolicy" =: "strict-origin-when-cross-origin"
  attrMap RPunsafeurl                   = "referrerpolicy" =: "unsafe-url"

instance Semigroup ReferrerPolicy where
  (<>) _ b = b

instance Monoid ReferrerPolicy where
  mappend = (<>)
  mempty = def

class AttrHasReferrerPolicy a where
  attrSetReferrerPolicy :: ReferrerPolicy -> a -> a


-- | These set the corresponding attribute.
rpEmpty, rpNoReferrer, rpNoReferrerWhenDowngrade, rpSameOrigin, rpOrigin,
  rpStrictOrigin, rpOriginWhenCrossOrigin, rpStrictOriginWhenCrossOrigin,
  rpUnsafeUrl :: AttrHasReferrerPolicy a => a -> a

rpEmpty                       = attrSetReferrerPolicy RPempty
rpNoReferrer                  = attrSetReferrerPolicy RPnoreferrer
rpNoReferrerWhenDowngrade     = attrSetReferrerPolicy RPnoreferrerwhendowngrade
rpSameOrigin                  = attrSetReferrerPolicy RPsameorigin
rpOrigin                      = attrSetReferrerPolicy RPorigin
rpStrictOrigin                = attrSetReferrerPolicy RPstrictorigin
rpOriginWhenCrossOrigin       = attrSetReferrerPolicy RPoriginwhencrossorigin
rpStrictOriginWhenCrossOrigin = attrSetReferrerPolicy RPstrictoriginwhencrossorigin
rpUnsafeUrl                   = attrSetReferrerPolicy RPunsafeurl

instance (Reflex t, AttrHasReferrerPolicy a) => AttrHasReferrerPolicy (Dynamic t a) where
  attrSetReferrerPolicy c = fmap (attrSetReferrerPolicy c)

------------------------------------------------------------------------------

-- | If the control is required for form submission in input, select or
-- textarea element.
data Required = Required
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Required where
  def = Required

instance AttrMap Required where
  attrMap Required = "required" =: T.empty

instance Semigroup Required where
  (<>) _ b = b

instance Monoid Required where
  mappend = (<>)
  mempty = def

class AttrHasRequired a where
  attrSetRequired :: Required -> a -> a

-- | This sets the attribute.
required :: AttrHasRequired a => a -> a
required = attrSetRequired Required

instance (Reflex t, AttrHasRequired a) => AttrHasRequired (Dynamic t a) where
  attrSetRequired c = fmap (attrSetRequired c)

------------------------------------------------------------------------------

-- | Number the ol-element list backwards.
data Reversed = Reversed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Reversed where
  def = Reversed

instance AttrMap Reversed where
  attrMap Reversed = "reversed" =: T.empty
  -- attrMap Reversed = "reversed" =: "reversed"

instance Semigroup Reversed where
  (<>) _ b = b

instance Monoid Reversed where
  mappend = (<>)
  mempty = def

class AttrHasReversed a where
  attrSetReversed :: Reversed -> a -> a

-- | This sets the attribute.
reversed :: AttrHasReversed a => a -> a
reversed = attrSetReversed Reversed

instance (Reflex t, AttrHasReversed a) => AttrHasReversed (Dynamic t a) where
  attrSetReversed c = fmap (attrSetReversed c)

------------------------------------------------------------------------------

-- | Number of lines to show in textarea element.
-- Non-negative integer greater than zero.
newtype Rows = Rows Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Rows where
  attrMap (Rows i) = "rows" =: T.pack (show i)

instance Semigroup Rows where
  (<>) _ b = b

instance Monoid Rows where
  mappend = (<>)
  mempty = Rows 1

class AttrHasRows a where
  attrSetRows :: Rows -> a -> a

-- | Set the number of lines to show in textarea element.
rows :: AttrHasRows a => Int -> a -> a
rows i = attrSetRows (Rows i)

instance (Reflex t, AttrHasRows a) => AttrHasRows (Dynamic t a) where
  attrSetRows c = fmap (attrSetRows c)

------------------------------------------------------------------------------

-- | Number of rows that the cell is to span to be used with th and td
-- elements.
-- Non-negative integer.
newtype RowSpan = RowSpan Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap RowSpan where
  attrMap (RowSpan i) = "rowspan" =: T.pack (show i)

instance Semigroup RowSpan where
  (<>) _ b = b

instance Monoid RowSpan where
  mappend = (<>)
  mempty = RowSpan 1

class AttrHasRowSpan a where
  attrSetRowSpan :: RowSpan -> a -> a

-- |
rowSpan :: AttrHasRowSpan a => Int -> a -> a
rowSpan i = attrSetRowSpan (RowSpan i)

instance (Reflex t, AttrHasRowSpan a) => AttrHasRowSpan (Dynamic t a) where
  attrSetRowSpan c = fmap (attrSetRowSpan c)

------------------------------------------------------------------------------

-- | Security rules for nested content to be used with iframe element.
-- allow-forms, allow-pointer-lock, allow-popups, allow-presentation,
-- allow-same-origin, allow-scripts, allow-top-navigation
data SandboxToken = SBAllowForms | SBAllowPointerLock | SBAllowPopups
                  | SBAllowPresentation | SBAllowSameOrigin | SBAllowScripts
                  | SBAllowTopNavigation
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

sbText :: SandboxToken -> Text
sbText SBAllowForms         = "allow-form"
sbText SBAllowPointerLock   = "allow-pointer-lock"
sbText SBAllowPopups        = "allow-popups"
sbText SBAllowPresentation  = "allow-presentation"
sbText SBAllowSameOrigin    = "allow-same-origin"
sbText SBAllowScripts       = "allow-scripts"
sbText SBAllowTopNavigation = "allow-top-navigation"

newtype Sandbox = Sandbox [SandboxToken]
  deriving (Show, Read, Eq, Ord)

instance Default Sandbox where
  def = Sandbox []

instance AttrMap Sandbox where
  attrMap (Sandbox []) = Map.empty
  attrMap (Sandbox sbTokenLst) = "sandbox" =:
    foldMap (\t -> sbText t <> " ") sbTokenLst

instance Semigroup Sandbox where
  (<>) _ b = b

instance Monoid Sandbox where
  mappend = (<>)
  mempty = def

class AttrHasSandbox a where
  attrSetSandbox :: Sandbox -> a -> a

class AttrGetSandbox a where
  attrGetSandbox :: a -> Sandbox

-- | These set the corresponding attribute.
sbAllowForms, sbAllowPointerLock, sbAllowPopups, sbAllowPresentation,
  sbAllowSameOrigin, sbAllowScripts, sbAllowTopNavigatiion ::
  (AttrHasSandbox a) => a -> a
sbAllowForms          = attrSetSandbox (Sandbox [SBAllowForms])
sbAllowPointerLock    = attrSetSandbox (Sandbox [SBAllowPointerLock])
sbAllowPopups         = attrSetSandbox (Sandbox [SBAllowPopups])
sbAllowPresentation   = attrSetSandbox (Sandbox [SBAllowPresentation])
sbAllowSameOrigin     = attrSetSandbox (Sandbox [SBAllowSameOrigin])
sbAllowScripts        = attrSetSandbox (Sandbox [SBAllowScripts])
sbAllowTopNavigatiion = attrSetSandbox (Sandbox [SBAllowTopNavigation])


-- |
addSandboxToken :: (AttrHasSandbox a, AttrGetSandbox a) => SandboxToken -> a -> a
addSandboxToken i s = attrSetSandbox (Sandbox $ i:oldList) s
  where
    (Sandbox oldList) = attrGetSandbox s

instance (Reflex t, AttrHasSandbox a) => AttrHasSandbox (Dynamic t a) where
  attrSetSandbox c = fmap (attrSetSandbox c)
  -- attrGetSandbox c = fmap (attrGetSandbox c)

------------------------------------------------------------------------------

-- | Specifies which cells the header cell applies to, is related to
-- th-elements and tables.
data Scope = ScopeRow | ScopeCol | ScopeRowGroup | ScopeColGroup | ScopeAuto
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- No default.

instance AttrMap Scope where
  attrMap ScopeRow = "scope" =: "row"
  attrMap ScopeCol = "scope" =: "col"
  attrMap ScopeRowGroup = "scope" =: "rowgroup"
  attrMap ScopeColGroup = "scope" =: "colgroup"
  attrMap ScopeAuto = "scope" =: "auto"

instance Semigroup Scope where
  (<>) _ b = b

instance Monoid Scope where
  mappend = (<>)
  mempty = ScopeAuto  -- Is this ok at all?

class AttrHasScope a where
  attrSetScope :: Scope -> a -> a

-- | These set the corresponding attribute.
scopeRow, scopeCol, scopeRowGroup, scopeColGroup, scopeAuto
  :: AttrHasScope a => a -> a
scopeRow = attrSetScope ScopeRow
scopeCol = attrSetScope ScopeCol
scopeRowGroup = attrSetScope ScopeRowGroup
scopeColGroup = attrSetScope ScopeColGroup
scopeAuto = attrSetScope ScopeAuto

instance (Reflex t, AttrHasScope a) => AttrHasScope (Dynamic t a) where
  attrSetScope c = fmap (attrSetScope c)

------------------------------------------------------------------------------

-- |
-- Wish list, allow other javascript mime types??
-- (They can be inserted with STother, though.)
data ScriptType = STjs | STmodule | STother MediaType
  deriving (Show, Read, Eq, Ord)

instance Default ScriptType where
  def = STjs

instance AttrMap ScriptType where
  attrMap STjs                     = "type" =: "application/javascript"
  attrMap STmodule                 = "type" =: "module"
  attrMap (STother (MediaType mt)) = "type" =: mt

instance Semigroup ScriptType where
  (<>) _ b = b

instance Monoid ScriptType where
  mappend = (<>)
  mempty = def

class AttrHasScriptType a where
  attrSetScriptType :: ScriptType -> a -> a

-- | These set the corresponding attribute.
stJs, stModule :: AttrHasScriptType a => a -> a
stJs = attrSetScriptType STjs
stModule = attrSetScriptType STmodule

stOther :: AttrHasScriptType a => MediaType -> a -> a
stOther mi = attrSetScriptType (STother mi)


instance (Reflex t, AttrHasScriptType a) => AttrHasScriptType (Dynamic t a) where
  attrSetScriptType c = fmap (attrSetScriptType c)

------------------------------------------------------------------------------

-- | Is option selected by default (option element).
data Selected = Selected
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Selected where
  def = Selected

instance AttrMap Selected where
  attrMap Selected = "selected" =: T.empty

instance Semigroup Selected where
  (<>) _ b = b

instance Monoid Selected where
  mappend = (<>)
  mempty = def

class AttrHasSelected a where
  attrSetSelected :: Selected -> a -> a

-- | This sets the attribute.
selected :: AttrHasSelected a => a -> a
selected = attrSetSelected Selected

instance (Reflex t, AttrHasSelected a) => AttrHasSelected (Dynamic t a) where
  attrSetSelected c = fmap (attrSetSelected c)

------------------------------------------------------------------------------

-- | The kind of shape to be created in an image map (with area element).
data Shape = ShapeCircle | ShapeDefault | ShapePolygon | ShapeRectangle
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default Shape where
  def = ShapeRectangle

instance AttrMap Shape where
  attrMap ShapeCircle    = "shape" =: "circle"
  attrMap ShapeDefault   = "shape" =: "default"
  attrMap ShapePolygon   = "shape" =: "poly"
  attrMap ShapeRectangle = "shape" =: "rect"

instance Semigroup Shape where
  (<>) _ b = b

instance Monoid Shape where
  mappend = (<>)
  mempty = def

class AttrHasShape a where
  attrSetShape :: Shape -> a -> a

-- | These set the corresponding attribute.
shapeCircle, shapeDefault, shapePolygon, shapeRectangle
  :: AttrHasShape a => a -> a
shapeCircle    = attrSetShape ShapeCircle
shapeDefault   = attrSetShape ShapeDefault
shapePolygon   = attrSetShape ShapePolygon
shapeRectangle = attrSetShape ShapeRectangle

instance (Reflex t, AttrHasShape a) => AttrHasShape (Dynamic t a) where
  attrSetShape c = fmap (attrSetShape c)

------------------------------------------------------------------------------

-- | Size of the control (input and select elements).
newtype Size = Size Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Size where
  attrMap (Size i) = "size" =: T.pack (show i)

instance Semigroup Size where
  (<>) _ b = b

instance Monoid Size where
  mappend = (<>)
  mempty = Size 0

class AttrHasSize a where
  attrSetSize :: Size -> a -> a

-- |
size :: AttrHasSize a => Int -> a -> a
size i = attrSetSize (Size i)

instance (Reflex t, AttrHasSize a) => AttrHasSize (Dynamic t a) where
  attrSetSize c = fmap (attrSetSize c)

------------------------------------------------------------------------------

-- |
-- Integers should non-negative integers.
data SizeToken = SizesAny | SizesI Int Int
  deriving (Show, Read, Eq, Ord)

-- | Sizes of the icons, to be used with link element.
newtype Sizes = Sizes [SizeToken]
  deriving (Show, Read, Eq, Ord)

-- No default.

st2txt :: SizeToken -> Text
st2txt SizesAny = "any"
st2txt (SizesI i j) = T.pack (show i) <> "x" <> T.pack (show j)


instance AttrMap Sizes where
  attrMap (Sizes iLst) = "sizes" =: foldr (\a b -> b <> " " <> st2txt a) T.empty iLst

class AttrHasSizes a where
  attrSetSizes :: Sizes -> a -> a
  -- attrGetSizes :: a -> Sizes

-- If we had a Dynamic t a -> a, we wouldn't need this class.
-- Is there other & better ways to build "add"-ops?
class AttrGetSizes a where
  attrGetSizes :: a -> Sizes

instance Semigroup Sizes where
    (<>) (Sizes a) (Sizes b) = Sizes (a <> b)

instance Monoid Sizes where
  mappend = (<>)
  mempty = Sizes []


-- |
addSizesAny :: (AttrGetSizes a, AttrHasSizes a) => a -> a
addSizesAny o = attrSetSizes (Sizes (SizesAny:old)) o
  where
    Sizes old = attrGetSizes o
-- addSizesAny o = attrSetSizes (o <> u)
--   where
--     u = attrSetSizes (Sizes [SizesAny]) o

-- |
addSizes :: (AttrGetSizes a, AttrHasSizes a) => Int -> Int -> a -> a
addSizes i j o = attrSetSizes (Sizes (SizesI i j :old)) o
  where
    Sizes old = attrGetSizes o
-- addSizes i j o = attrSetSizes (o <> (Sizes [SizesI i j]))

instance (Reflex t, AttrHasSizes a) => AttrHasSizes (Dynamic t a) where
  attrSetSizes c = fmap (attrSetSizes c)

  -- attrGetSizes (Dynamic c) = fst (unDynamic c)
  -- attrGetSizes d = \t -> let (cur, _) = unDynamic d t
  --                        in cur

------------------------------------------------------------------------------

-- | Number of columns spanned, used with col and colgroup elements.
-- Non-negative integer greater than zero.
newtype Span = Span Int
  deriving (Show, Read, Eq, Ord)

instance Default Span where
  def = Span 300

instance AttrMap Span where
  attrMap (Span i) = "span" =: T.pack (show i)

instance Semigroup Span where
  (<>) _ b = b

instance Monoid Span where
  mappend = (<>)
  mempty = def

class AttrHasSpan a where
  attrSetSpan :: Span -> a -> a

-- | Set the number of columns to be spanned.
span :: AttrHasSpan a => Int -> a -> a
span i = attrSetSpan (Span i)

instance (Reflex t, AttrHasSpan a) => AttrHasSpan (Dynamic t a) where
  attrSetSpan c = fmap (attrSetSpan c)

------------------------------------------------------------------------------

-- | Address, used with audio, embed, iframe, img, input, script, source,
-- track and video elements.
newtype Src = Src URL
  deriving (Show, Read, Eq, Ord)

instance Default Src where
  def = Src (URL T.empty)

-- |
instance AttrMap Src where
  attrMap (Src (URL t)) = "src" =: t

instance Semigroup Src where
  (<>) _ b = b

instance Monoid Src where
  mappend = (<>)
  mempty = def

class AttrHasSrc a where
  attrSetSrc :: Src -> a -> a

-- | Set the address (the src-attribute).
src :: AttrHasSrc a => URL -> a -> a
src t = attrSetSrc (Src t)

instance (Reflex t, AttrHasSrc a) => AttrHasSrc (Dynamic t a) where
  attrSetSrc c = fmap (attrSetSrc c)

------------------------------------------------------------------------------

-- | A document to render in the iframe.
-- The text should contain valid html.
newtype SrcDoc = SrcDoc Text
  deriving (Show, Read, Eq, Ord)

instance Default SrcDoc where
  def = SrcDoc T.empty

instance AttrMap SrcDoc where
  attrMap (SrcDoc i) = "srcdoc" =: i

-- ok?
instance Semigroup SrcDoc where
  (<>) (SrcDoc a) (SrcDoc b) = SrcDoc (a <> " " <> b)

instance Monoid SrcDoc where
  mappend = (<>)
  mempty = SrcDoc T.empty

class AttrHasSrcDoc a where
  attrSetSrcDoc :: SrcDoc -> a -> a

-- |
srcDoc :: AttrHasSrcDoc a => Text -> a -> a
srcDoc i = attrSetSrcDoc (SrcDoc i)

instance (Reflex t, AttrHasSrcDoc a) => AttrHasSrcDoc (Dynamic t a) where
  attrSetSrcDoc c = fmap (attrSetSrcDoc c)

------------------------------------------------------------------------------

-- | Text track language to be used with track element.
-- Wish list: implement BCP 47.
newtype SrcLang = SrcLang Text
  deriving (Show, Read, Eq, Ord)

instance Default SrcLang where
  def = SrcLang T.empty

instance AttrMap SrcLang where
  attrMap (SrcLang i) = "srclang" =: i

instance Semigroup SrcLang where
  (<>) _ b = b

instance Monoid SrcLang where
  mappend = (<>)
  mempty = def

class AttrHasSrcLang a where
  attrSetSrcLang :: SrcLang -> a -> a

-- |
srcLang :: AttrHasSrcLang a => Text -> a -> a
srcLang i = attrSetSrcLang (SrcLang i)

instance (Reflex t, AttrHasSrcLang a) => AttrHasSrcLang (Dynamic t a) where
  attrSetSrcLang c = fmap (attrSetSrcLang c)

------------------------------------------------------------------------------


-- |
-- Its value must consist of one or more image candidate strings, each
-- separated from the next by a U+002C COMMA character (,).
-- If width or pixel densities are given, int should be non-negative and
-- double greater than zero.
newtype SrcSet  = SrcSet  [URL]        deriving (Show, Read, Eq, Ord)
newtype SrcSetW = SrcSetW [(URL, Int)] deriving (Show, Read, Eq, Ord)
newtype SrcSetP = SrcSetP [(URL, Double)] deriving (Show, Read, Eq, Ord)

instance Default SrcSet  where def = SrcSet []
instance Default SrcSetW where def = SrcSetW []
instance Default SrcSetP where def = SrcSetP []

instance AttrMap SrcSet where
  attrMap (SrcSet icsLst) = "srcset" =: foldr (\(URL a) b -> a <> "," <> b) T.empty icsLst
instance AttrMap SrcSetW where
  attrMap (SrcSetW icsLst) = "srcset" =:
    foldr (\(URL u,i) a -> a <> ", " <> u <> " " <> T.pack (show i) <> "w") T.empty icsLst
instance AttrMap SrcSetP where
  attrMap (SrcSetP icsLst) = "srcset" =:
    foldr (\(URL u,d) a -> a <> ", " <> u <> " " <> T.pack (show d) <> "x") T.empty icsLst

-- TODO! TODO! TODO!
instance Semigroup SrcSet where
  (<>) _ b = b
instance Monoid SrcSet where
  mappend = (<>)
  mempty = def

-- TODO! TODO! TODO!
instance Semigroup SrcSetW where
  (<>) _ b = b
instance Monoid SrcSetW where
  mappend = (<>)
  mempty = def

-- TODO! TODO! TODO!
instance Semigroup SrcSetP where
  (<>) _ b = b
instance Monoid SrcSetP where
  mappend = (<>)
  mempty = def

class AttrHasSrcSet a where
  attrSetSrcSet :: SrcSet -> a -> a

class AttrGetSrcSet a where
  attrGetSrcSet :: a -> SrcSet

class AttrHasSrcSetW a where
  attrSetSrcSetW :: SrcSetW -> a -> a

class AttrGetSrcSetW a where
  attrGetSrcSetW :: a -> SrcSetW

class AttrHasSrcSetP a where
  attrSetSrcSetP :: SrcSetP -> a -> a

class AttrGetSrcSetP a where
  attrGetSrcSetP :: a -> SrcSetP

-- |
addSrc :: (AttrHasSrcSet a, AttrGetSrcSet a) => URL -> a -> a
addSrc t a = attrSetSrcSet (SrcSet (t:ou)) a
  where
    (SrcSet ou) = attrGetSrcSet a

-- |
addSrcW :: (AttrHasSrcSetW a, AttrGetSrcSetW a) => URL -> Int -> a -> a
addSrcW t i a = attrSetSrcSetW (SrcSetW ((t,i):ou)) a
  where
    (SrcSetW ou) = attrGetSrcSetW a

-- |
addSrcP :: (AttrHasSrcSetP a, AttrGetSrcSetP a) => URL -> Double -> a -> a
addSrcP t d a = attrSetSrcSetP (SrcSetP ((t,d):ou)) a
  where
    (SrcSetP ou) = attrGetSrcSetP a

instance (Reflex t, AttrHasSrcSet a) => AttrHasSrcSet (Dynamic t a) where
  attrSetSrcSet c = fmap (attrSetSrcSet c)
instance (Reflex t, AttrHasSrcSetW a) => AttrHasSrcSetW (Dynamic t a) where
  attrSetSrcSetW c = fmap (attrSetSrcSetW c)
instance (Reflex t, AttrHasSrcSetP a) => AttrHasSrcSetP (Dynamic t a) where
  attrSetSrcSetP c = fmap (attrSetSrcSetP c)

------------------------------------------------------------------------------

-- | Value of the first item used with ol-element.
newtype Start = Start Int
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Start where
  attrMap (Start i) = "start" =: T.pack (show i)

instance Semigroup Start where
  (<>) _ b = b

instance Monoid Start where
  mappend = (<>)
  mempty = Start 0

class AttrHasStart a where
  attrSetStart :: Start -> a -> a

-- |
start :: AttrHasStart a => Int -> a -> a
start i = attrSetStart (Start i)

instance (Reflex t, AttrHasStart a) => AttrHasStart (Dynamic t a) where
  attrSetStart c = fmap (attrSetStart c)

------------------------------------------------------------------------------

-- | Input element can be given a granularity to be matched by the form
-- control’s value.
data Step = StepAny | StepI Int | StepD Double
  deriving (Show, Read, Eq, Ord)

-- No default.

instance AttrMap Step where
  attrMap StepAny   = "step" =: "any"
  attrMap (StepI i) = "step" =: T.pack (show i)
  attrMap (StepD i) = "step" =: T.pack (show i)

instance Semigroup Step where
  (<>) _ b = b

instance Monoid Step where
  mappend = (<>)
  mempty = StepAny

class AttrHasStep a where
  attrSetStep :: Step -> a -> a

-- |
stepAny :: AttrHasStep a => a -> a
stepAny = attrSetStep StepAny
stepI :: AttrHasStep a => Int -> a -> a
stepI i = attrSetStep (StepI i)
stepD :: AttrHasStep a => Double -> a -> a
stepD i = attrSetStep (StepD i)

instance (Reflex t, AttrHasStep a) => AttrHasStep (Dynamic t a) where
  attrSetStep c = fmap (attrSetStep c)

------------------------------------------------------------------------------

-- | A valid browsing context name or keyword used with form, base, a and
-- area elements.
-- See FormTarget.
data Target = Tblank | Tself | Tparent | Ttop | Tname Text
  deriving (Show, Read, Eq, Ord)

instance Default Target where
  def = Tname T.empty

instance AttrMap Target where
  attrMap Tblank    = "target" =: "_blank"
  attrMap Tself     = "target" =: "_self"
  attrMap Tparent   = "target" =: "_parent"
  attrMap Ttop      = "target" =: "_top"
  attrMap (Tname t) = "target" =: t

instance Semigroup Target where
  (<>) _ b = b

instance Monoid Target where
  mappend = (<>)
  mempty = def

class AttrHasTarget a where
  attrSetTarget :: Target -> a -> a

-- | These set the corresponding attribute.
targetBlank, targetSelf, targetParent, targetTop
  :: AttrHasTarget a => a -> a
targetBlank  = attrSetTarget Tblank
targetSelf   = attrSetTarget Tself
targetParent = attrSetTarget Tparent
targetTop    = attrSetTarget Ttop

-- | Parameter is a (browsing context) name.
targetName :: AttrHasTarget a => Text -> a -> a
targetName t = attrSetTarget (Tname t)

instance (Reflex t, AttrHasTarget a) => AttrHasTarget (Dynamic t a) where
  attrSetTarget c = fmap (attrSetTarget c)

------------------------------------------------------------------------------

-- | Should the type attribute and the Content-Type value need to match for
-- the resource to be used. This is used with the object element.
data TypeMustMatch = TypeMustMatch
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Default TypeMustMatch where
  def = TypeMustMatch

instance AttrMap TypeMustMatch where
  attrMap TypeMustMatch = "typemustmatch" =: T.empty

instance Semigroup TypeMustMatch where
  (<>) _ b = b

instance Monoid TypeMustMatch where
  mappend = (<>)
  mempty = def

class AttrHasTypeMustMatch a where
  attrSetTypeMustMatch :: TypeMustMatch -> a -> a

-- | These set the corresponding attribute.
typeMustMatch :: AttrHasTypeMustMatch a => a -> a
typeMustMatch = attrSetTypeMustMatch TypeMustMatch

instance (Reflex t, AttrHasTypeMustMatch a) => AttrHasTypeMustMatch (Dynamic t a) where
  attrSetTypeMustMatch c = fmap (attrSetTypeMustMatch c)

------------------------------------------------------------------------------

-- |  Name of image map to use with img element.
--
-- Wish list: use types to ensure that this uses the name of a map.
newtype UseMap = UseMap Text
  deriving (Show, Read, Eq, Ord)

instance Default UseMap where
  def = UseMap T.empty

instance AttrMap UseMap where
  attrMap (UseMap t) = "usemap" =: t

instance Semigroup UseMap where
  (<>) _ b = b

instance Monoid UseMap where
  mappend = (<>)
  mempty = def

class AttrHasUseMap a where
  attrSetUseMap :: UseMap -> a -> a

-- |
useMap :: AttrHasUseMap a => Text -> a -> a
useMap t = attrSetUseMap (UseMap t)

instance (Reflex t, AttrHasUseMap a) => AttrHasUseMap (Dynamic t a) where
  attrSetUseMap c = fmap (attrSetUseMap c)

------------------------------------------------------------------------------

-- |
-- Can be used to specify the initial value.
newtype ValueText = ValueText Text
  deriving (Show, Read, Eq, Ord)

instance Default ValueText where
  def = ValueText T.empty

instance AttrMap ValueText where
  attrMap (ValueText t) = "value" =: t

instance Semigroup ValueText where
  (<>) (ValueText a) (ValueText b) = ValueText (a <> " " <> b)

instance Monoid ValueText where
  mappend = (<>)
  mempty = def

class AttrHasValueText a where
  attrSetValueText :: ValueText -> a -> a

-- |
valueText :: AttrHasValueText a => Text -> a -> a
valueText t = attrSetValueText (ValueText t)

instance (Reflex t, AttrHasValueText a) => AttrHasValueText (Dynamic t a) where
  attrSetValueText c = fmap (attrSetValueText c)

------------------------------------------------------------------------------

-- |
-- value of name-attribute of object-tag.
newtype ValueName = ValueName Double
  deriving (Show, Read, Eq, Ord)

instance AttrMap ValueName where
  attrMap (ValueName t) = "value" =: T.pack (show t)

instance Semigroup ValueName where
  (<>) _ b = b

instance Monoid ValueName where
  mappend = (<>)
  mempty = ValueName 0  -- Is this ok? TODO!

class AttrHasValueName a where
  attrSetValueName :: ValueName -> a -> a

-- |
valueName :: AttrHasValueName a => Double -> a -> a
valueName t = attrSetValueName (ValueName t)

instance (Reflex t, AttrHasValueName a) => AttrHasValueName (Dynamic t a) where
  attrSetValueName c = fmap (attrSetValueName c)

------------------------------------------------------------------------------

-- |
-- Meter and progress - element's values are numbers.
newtype ValueNumber = ValueNumber Double
  deriving (Show, Read, Eq, Ord)

instance AttrMap ValueNumber where
  attrMap (ValueNumber t) = "value" =: T.pack (show t)

instance Semigroup ValueNumber where
  (<>) _ b = b

instance Monoid ValueNumber where
  mappend = (<>)
  mempty = ValueNumber 0

class AttrHasValueNumber a where
  attrSetValueNumber :: ValueNumber -> a -> a

-- |
valueNumber :: AttrHasValueNumber a => Double -> a -> a
valueNumber t = attrSetValueNumber (ValueNumber t)

instance (Reflex t, AttrHasValueNumber a) => AttrHasValueNumber (Dynamic t a) where
  attrSetValueNumber c = fmap (attrSetValueNumber c)

------------------------------------------------------------------------------

-- |
-- Li-elements inside ol-element can have values (ordinal value, integers).
-- Wish list: is it possible to ensure that values form an ordinal list inside
-- ol-element?
newtype ValueOlLi = ValueOlLi Int
  deriving (Show, Read, Eq, Ord)

instance AttrMap ValueOlLi where
  attrMap (ValueOlLi t) = "value" =: T.pack (show t)

instance Semigroup ValueOlLi where
  (<>) _ b = b

instance Monoid ValueOlLi where
  mappend = (<>)
  mempty = ValueOlLi 0

class AttrHasValueOlLi a where
  attrSetValueOlLi :: ValueOlLi -> a -> a

-- |
valueOlLi :: AttrHasValueOlLi a => Int -> a -> a
valueOlLi t = attrSetValueOlLi (ValueOlLi t)

instance (Reflex t, AttrHasValueOlLi a) => AttrHasValueOlLi (Dynamic t a) where
  attrSetValueOlLi c = fmap (attrSetValueOlLi c)

------------------------------------------------------------------------------

-- | Horizontal dimension (non-negative integer) used with canvas, embed,
-- iframe, img, input, object and video elements.
newtype Width = Width Int
  deriving (Show, Read, Eq, Ord)

instance Default Width where
  def = Width 300

instance AttrMap Width where
  attrMap (Width i) = "width" =: T.pack (show i)

instance Semigroup Width where
  (<>) _ b = b

instance Monoid Width where
  mappend = (<>)
  mempty = def

class AttrHasWidth a where
  attrSetWidth :: Width -> a -> a

-- |
width :: AttrHasWidth a => Int -> a -> a
width i = attrSetWidth (Width i)

instance (Reflex t, AttrHasWidth a) => AttrHasWidth (Dynamic t a) where
  attrSetWidth c = fmap (attrSetWidth c)

------------------------------------------------------------------------------

-- |
-- How the value of the form control is to be wrapped for form submission
-- that is used with textarea-element.
data Wrap = WrapSoft | WrapHard
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- No default.

instance AttrMap Wrap where
  attrMap WrapSoft = "wrap" =: "soft"
  attrMap WrapHard = "wrap" =: "hard"

instance Semigroup Wrap where
  (<>)_ b = b

instance Monoid Wrap where
  mappend = (<>)
  mempty = WrapSoft  -- Is this ok at all?

class AttrHasWrap a where
  attrSetWrap :: Wrap -> a -> a

-- | These set the corresponding attribute.
wrapSoft, wrapHard :: AttrHasWrap a => a -> a
wrapSoft = attrSetWrap WrapSoft
wrapHard = attrSetWrap WrapHard

instance (Reflex t, AttrHasWrap a) => AttrHasWrap (Dynamic t a) where
  attrSetWrap c = fmap (attrSetWrap c)


