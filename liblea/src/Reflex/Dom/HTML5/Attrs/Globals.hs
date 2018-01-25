{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs.Globals
Description : Support for global attributes
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Global attributes


This module defines html5 attributes and with corresponding
elements, the compiler can help to avoid some of the writing
typos that were otherwise to be found on runtime.

To make an attribute map, write e.g.
@
  let a = title "My Title" $ className "myClass" $ id_ "myId" def
@

An example can be found in example-directory.

The element constructing functions don't allow non-conforming
HTML to be constructed directly. There are a few ways to by pass,
in case there is some need:
- el "element name" attrs ... can be used and combined with these
- see AnyAttr (it is, however, incomplete)
- addCustom can be used to add anything


== Notes

For a list of attributes, see
[w3c list](https://w3c.github.io/html/fullindex.html#index-elements).

This file contains the global attributes that all elements have.
 * AccessKey       -- 1.
 * ContentEditable -- 3.
 * ContextMenu     -- 4.
 * ClassName       -- 5. Monoid
 * Dir             -- 7.
 * Draggable       -- 8.
 * Hidden          -- 9.
 * Id_             -- 10.
 * Lang            -- 11.
 * SpellCheck      -- 14.
 * Style           -- 15. Monoid
 * TabIndex        -- 16.
 * Title           -- 17. Monoid
 * Translate       -- 18.

Further, the global attributes include
 * aria-* family of attributes  -- 2.
 * data-* family of attributes  -- 6.
 * role -- 12.
 * slot -- 13.

-}

module Reflex.Dom.HTML5.Attrs.Globals where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup (Semigroup, (<>))
-- import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core (Reflex, Dynamic, (=:))

import Reflex.Dom.HTML5.Attrs.Common (AttrMap, attrMap)


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | 'AccessKey' belongs to Globals.
-- @
-- accessKey 'c' $ def
-- @
-- Wish list: Change to allow a string (text) of one UTF code point length.
-- Wish list: For a Document, allow only unique case-insensitive values.
newtype AccessKey = AccessKey Char
  deriving (Show, Read, Eq, Ord)

instance AttrMap AccessKey where
  attrMap (AccessKey c) = "accesskey" =: T.singleton c

-- No default.

class AttrHasAccessKey a where
  attrSetAccessKey :: AccessKey -> a -> a

-- |
accessKey :: AttrHasAccessKey a => Char -> a -> a
accessKey c = attrSetAccessKey (AccessKey c)

instance (Reflex t, AttrHasAccessKey a) => AttrHasAccessKey (Dynamic t a) where
  attrSetAccessKey c = fmap (attrSetAccessKey c)

------------------------------------------------------------------------------

-- |
-- Anmval - aria - name - value can be used to set any "aria-name" = "value"
-- attribute.
-- Some of the predefined aria-* attributes are in Aria-module.
data Anmval = Anmval
  { _anm  :: Text
  , _aval :: Text
  } deriving (Show, Read, Eq, Ord)

instance Default Anmval where def = Anmval T.empty T.empty

instance AttrMap Anmval where
  attrMap (Anmval n v) = ("aria-" <> n) =: v

class AttrHasAnmval a where
  attrSetAnmval :: Anmval -> a -> a

-- | Function 'aNmVal' is used to set aria-* = "str" -attributes.
-- @
-- aNmVal "toggle" "myMenu"
-- @
-- Note that 'dToggle "myMenu"' and 'dTarget "myMenu"' are already
-- defined in DataAttrs-module.
aNmVal :: AttrHasAnmval a => Text -> Text -> a -> a
aNmVal n v = attrSetAnmval (Anmval n v)

instance (Reflex t, AttrHasAnmval a) => AttrHasAnmval (Dynamic t a) where
  attrSetAnmval c = fmap (attrSetAnmval c)


------------------------------------------------------------------------------

-- | 'ContentEditable' gives some constants.
data ContentEditable = CEtrue | CEfalse | CEinherit
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance AttrMap ContentEditable where
  attrMap CEtrue    = "contentEditable" =: "true"
  attrMap CEfalse   = "contentEditable" =: "false"
  attrMap CEinherit = "contentEditable" =: "inherit"

instance Default ContentEditable where
  def = CEinherit

class AttrHasContentEditable a where
  attrSetContentEditable :: ContentEditable -> a -> a

-- | Functions 'contentEditable', 'contentNotEditable'
-- and 'contentInheritEditable' can be used to set
-- corresponding attributes.
-- @
-- contentEditable def
-- contentNotEditable def
-- contentInheritEditable def
-- @
contentEditable, contentNotEditable, contentInheritEditable
  :: AttrHasContentEditable a => a -> a
contentEditable        = attrSetContentEditable CEtrue
contentNotEditable     = attrSetContentEditable CEfalse
contentInheritEditable = attrSetContentEditable CEinherit
-- Hmm, quite long names. Drop content-prefix?

instance (Reflex t, AttrHasContentEditable a) => AttrHasContentEditable (Dynamic t a) where
  attrSetContentEditable c = fmap (attrSetContentEditable c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
newtype ContextMenu = ContextMenu Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap ContextMenu where
  attrMap (ContextMenu t) = "contextMenu" =: t

-- No default.

class AttrHasContextMenu a where
  attrSetContextMenu :: ContextMenu -> a -> a

-- | Function 'contextMenu t':
-- value must be the id of a menu element in the same tree whose
-- type attribute is in the context menu state.
-- @
-- contextMenu "myId" $ def
-- @
contextMenu :: AttrHasContextMenu a => Text -> a -> a
contextMenu t = attrSetContextMenu (ContextMenu t)

instance (Reflex t, AttrHasContextMenu a) => AttrHasContextMenu (Dynamic t a) where
  attrSetContextMenu c = fmap (attrSetContextMenu c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
newtype ClassName = ClassName Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap ClassName where
  attrMap (ClassName t) = "class" =: t

--  No default.

class AttrHasClass a where
  attrSetClassName :: ClassName -> a -> a

class AttrGetClassName a where
  attrGetClassName :: a -> ClassName

-- | Function 'className t' can be used to set class
-- or classes for an element.
-- @
-- className "blue big myHover" $ def
-- @
-- See reflex-dom-themes -package for a set of
-- pre-defined ClassNames.
className :: AttrHasClass a => Text -> a -> a
className t = attrSetClassName (ClassName t)

-- | Function 'addClass t' adds t the class-attributes
-- of an element. It doesn't check if the t was already there.
-- @
-- addClass "myHover" $ setClass "blue big" $ def
-- @
addClass :: (AttrHasClass a, AttrGetClassName a) => Text -> a -> a
addClass t a = attrSetClassName tt a
  where
    tt = ClassName t <> attrGetClassName a

-- | Function 'setClasses t' takes a foldable structure
-- containing class-names and sets them for an element.
-- @
-- setClasses [cDef1, cDef2, cDef3] $ def
-- @
-- where cDefs are pre-defined ClassNames.
-- See reflex-dom-themes -package for a set of
-- pre-defined ClassNames and examples.
setClasses :: (AttrHasClass a, Foldable t) => t ClassName -> a -> a
setClasses cls = attrSetClassName (fold cls)

instance Semigroup ClassName where
    (<>) (ClassName a) (ClassName b) = ClassName $ a <> " " <> b

instance Monoid ClassName where
  mappend (ClassName a) (ClassName b) = ClassName $ a <> " " <> b
  mempty = ClassName T.empty

instance (Reflex t, AttrHasClass a) => AttrHasClass (Dynamic t a) where
  attrSetClassName c = fmap (attrSetClassName c)

------------------------------------------------------------------------------

-- | -- Dnmval - data -name - value can be used to set any "data-name" = "value"
-- attribute.
-- Note that the first param should be lowercase and should contain at least
-- one character. This are not checked at the moment. TODO
-- Some predefined data-* attributes are in DataAttrs-module (e.g. attributes
-- defined by some popular css-theme libs).
data Dnmval = Dnmval
  { _dnm  :: Text
  , _dval :: Text
  } deriving (Show, Read, Eq, Ord)

instance Default Dnmval where def = Dnmval T.empty T.empty

instance AttrMap Dnmval where
  attrMap (Dnmval n v) = ("data-" <> n) =: v

class AttrHasDnmval a where
  attrSetDnmval :: Dnmval -> a -> a

-- | Function 'dNmVal nm val' can be used to set "data-name" = "value"
-- @
-- dNmVal "target" "xyz" $ def
-- @
dNmVal :: AttrHasDnmval a => Text -> Text -> a -> a
dNmVal n v = attrSetDnmval (Dnmval n v)

instance (Reflex t, AttrHasDnmval a) => AttrHasDnmval (Dynamic t a) where
  attrSetDnmval c = fmap (attrSetDnmval c)


------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
data Dir = DirLTR | DirRTL | DirAuto
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
-- makeClassy ''Dir

instance AttrMap Dir where
  attrMap DirLTR  = "dir" =: "ltr"
  attrMap DirRTL  = "dir" =: "rtl"
  attrMap DirAuto = "dir" =: "auto"

-- No default.

class AttrHasDir a where
  attrSetDir :: Dir -> a -> a

-- | Functions 'ltr', 'rtl' and 'dirAuto' can be used to
-- set corresponding attributes.
-- @
-- ltr $ def
-- rtl $ def
-- dirAuto $ def
-- @
ltr, rtl, dirAuto :: AttrHasDir a => a -> a
ltr     = attrSetDir DirLTR
rtl     = attrSetDir DirRTL
dirAuto = attrSetDir DirAuto

instance (Reflex t, AttrHasDir a) => AttrHasDir (Dynamic t a) where
  attrSetDir c = fmap (attrSetDir c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
data Draggable = Dtrue | Dfalse | Dauto
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance AttrMap Draggable where
  attrMap Dtrue  = "draggable" =: "true"
  attrMap Dfalse = "draggable" =: "false"
  attrMap Dauto  = "draggable" =: T.empty -- Does this work? TODO!

instance Default Draggable where
  def = Dauto

class AttrHasDraggable a where
  attrSetDraggable :: Draggable -> a -> a

-- | Functions 'draggable' and 'notDraggable' can be used
-- to set corresponding attributes.
draggable, notDraggable :: AttrHasDraggable a => a -> a
draggable    = attrSetDraggable Dtrue
notDraggable = attrSetDraggable Dfalse

instance (Reflex t, AttrHasDraggable a) => AttrHasDraggable (Dynamic t a) where
  attrSetDraggable c = fmap (attrSetDraggable c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
-- Wish list: to implement "unHidden", it needs to be removed from the Map.
data Hidden = Hidden
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance AttrMap Hidden where
  attrMap Hidden = "hidden" =: T.empty -- Does this work? TODO!

instance Default Hidden where
  def = Hidden

class AttrHasHidden a where
  attrSetHidden :: Hidden -> a -> a

-- | Function 'hidden' can be used to set the corresponding attribute.
-- @
-- hidden def
-- @
hidden :: AttrHasHidden a => a -> a
hidden = attrSetHidden Hidden

instance (Reflex t, AttrHasHidden a) => AttrHasHidden (Dynamic t a) where
  attrSetHidden c = fmap (attrSetHidden c)

------------------------------------------------------------------------------

-- |
-- An Id should be unique among element id's and should not contain any
-- space-characters.
-- Belongs to Globals.
newtype Id_ = Id_ Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Id_ where
  attrMap (Id_ t) = "id" =: t

-- No default. No monoid.

class AttrHasId a where
  attrSetId :: Id_ -> a -> a

-- | Function 'id_ t' can be used to set id-attribute.
-- @
-- id_ "myId" $ def
-- @
id_ :: AttrHasId a => Text -> a -> a
id_ t = attrSetId (Id_ t)

instance (Reflex t, AttrHasId a) => AttrHasId (Dynamic t a) where
  attrSetId c = fmap (attrSetId c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals. This should a language tag as of BCP 47 or an empty
-- Text.
-- Wish list: BCP 47.
-- https://tools.ietf.org/html/bcp47
newtype Lang = Lang Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Lang where
  attrMap (Lang t) = "lang" =: t

instance Default Lang where
  def = Lang T.empty

-- No monoid.

class AttrHasLang a where
  attrSetLang :: Lang -> a -> a

-- | Function 'lang t' can be used to set the language.
-- For applicaple values t, see
-- [BCP 47](https://tools.ietf.org/html/bcp47).
lang :: AttrHasLang a => Text -> a -> a
lang t = attrSetLang (Lang t)

instance (Reflex t, AttrHasLang a) => AttrHasLang (Dynamic t a) where
  attrSetLang c = fmap (attrSetLang c)

------------------------------------------------------------------------------

-- |
-- Role can be any of the following:
--  alert alertdialog application article banner button cell checkbox
--  columnheader combobox complementary contentinfo definition dialog
--  directory document feed figure form grid gridcell group heading img link
--  list listbox listitem log main marquee math menu menubar menuitem
--  menuitemcheckbox menuitemradio navigation none note option presentation
--  progressbar radio radiogroup region row rowgroup rowheader scrollbar
--  search searchbox separator slider spinbutton status switch tab table
--  tablist tabpanel term textbox timer toolbar tooltip tree treegrid
--  treeitem

data RoleAttr =
 Ralert | Ralertdialog | Rapplication | Rarticle | Rbanner | Rbutton | Rcell
   | Rcheckbox | Rcolumnheader | Rcombobox | Rcomplementary | Rcontentinfo
   | Rdefinition | Rdialog | Rdirectory | Rdocument | Rfeed | Rfigure | Rform
   | Rgrid | Rgridcell | Rgroup | Rheading | Rimg | Rlink | Rlist | Rlistbox
   | Rlistitem | Rlog | Rmain | Rmarquee | Rmath | Rmenu | Rmenubar | Rmenuitem
   | Rmenuitemcheckbox | Rmenuitemradio | Rnavigation | Rnone | Rnote | Roption
   | Rpresentation | Rprogressbar | Rradio | Rradiogroup | Rregion | Rrow
   | Rrowgroup | Rrowheader | Rscrollbar | Rsearch | Rsearchbox | Rseparator
   | Rslider | Rspinbutton | Rstatus | Rswitch | Rtab | Rtable | Rtablist
   | Rtabpanel | Rterm | Rtextbox | Rtimer | Rtoolbar | Rtooltip | Rtree
   | Rtreegrid | Rtreeitem
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Role = RoleT Text | RoleA RoleAttr
  deriving (Show, Read, Eq, Ord)

instance AttrMap Role where
  attrMap (RoleT t)          = "role" =: t
  attrMap (RoleA Ralert)             = "role" =: "alert"
  attrMap (RoleA Ralertdialog)       = "role" =: "alertdialog"
  attrMap (RoleA Rapplication)       = "role" =: "application"
  attrMap (RoleA Rarticle)           = "role" =: "article"
  attrMap (RoleA Rbanner)            = "role" =: "banner"
  attrMap (RoleA Rbutton)            = "role" =: "button"
  attrMap (RoleA Rcell)              = "role" =: "cell"
  attrMap (RoleA Rcheckbox)          = "role" =: "checkbox"
  attrMap (RoleA Rcolumnheader)      = "role" =: "columnheader"
  attrMap (RoleA Rcombobox)          = "role" =: "combobox"
  attrMap (RoleA Rcomplementary)     = "role" =: "complementary"
  attrMap (RoleA Rcontentinfo)       = "role" =: "contentinfo"
  attrMap (RoleA Rdefinition)        = "role" =: "definition"
  attrMap (RoleA Rdialog)            = "role" =: "dialog"
  attrMap (RoleA Rdirectory)         = "role" =: "directory"
  attrMap (RoleA Rdocument)          = "role" =: "document"
  attrMap (RoleA Rfeed)              = "role" =: "feed"
  attrMap (RoleA Rfigure)            = "role" =: "figure"
  attrMap (RoleA Rform)              = "role" =: "form"
  attrMap (RoleA Rgrid)              = "role" =: "grid"
  attrMap (RoleA Rgridcell)          = "role" =: "gridcell"
  attrMap (RoleA Rgroup)             = "role" =: "group"
  attrMap (RoleA Rheading)           = "role" =: "heading"
  attrMap (RoleA Rimg)               = "role" =: "img"
  attrMap (RoleA Rlink)              = "role" =: "link"
  attrMap (RoleA Rlist)              = "role" =: "list"
  attrMap (RoleA Rlistbox)           = "role" =: "listbox"
  attrMap (RoleA Rlistitem)          = "role" =: "listitem"
  attrMap (RoleA Rlog)               = "role" =: "log"
  attrMap (RoleA Rmain)              = "role" =: "main"
  attrMap (RoleA Rmarquee)           = "role" =: "marquee"
  attrMap (RoleA Rmath)              = "role" =: "math"
  attrMap (RoleA Rmenu)              = "role" =: "menu"
  attrMap (RoleA Rmenubar)           = "role" =: "menubar"
  attrMap (RoleA Rmenuitem)          = "role" =: "menuitem"
  attrMap (RoleA Rmenuitemcheckbox)  = "role" =: "menuitemcheckbox"
  attrMap (RoleA Rmenuitemradio)     = "role" =: "menuitemradio"
  attrMap (RoleA Rnavigation)        = "role" =: "navigation"
  attrMap (RoleA Rnone)              = "role" =: "none"
  attrMap (RoleA Rnote)              = "role" =: "note"
  attrMap (RoleA Roption)            = "role" =: "option"
  attrMap (RoleA Rpresentation)      = "role" =: "presentation"
  attrMap (RoleA Rprogressbar)       = "role" =: "progressbar"
  attrMap (RoleA Rradio)             = "role" =: "radio"
  attrMap (RoleA Rradiogroup)        = "role" =: "radiogroup"
  attrMap (RoleA Rregion)            = "role" =: "region"
  attrMap (RoleA Rrow)               = "role" =: "row"
  attrMap (RoleA Rrowgroup)          = "role" =: "rowgroup"
  attrMap (RoleA Rrowheader)         = "role" =: "rowheader"
  attrMap (RoleA Rscrollbar)         = "role" =: "scrollbar"
  attrMap (RoleA Rsearch)            = "role" =: "search"
  attrMap (RoleA Rsearchbox)         = "role" =: "searchbox"
  attrMap (RoleA Rseparator)         = "role" =: "separator"
  attrMap (RoleA Rslider)            = "role" =: "slider"
  attrMap (RoleA Rspinbutton)        = "role" =: "spinbutton"
  attrMap (RoleA Rstatus)            = "role" =: "status"
  attrMap (RoleA Rswitch)            = "role" =: "switch"
  attrMap (RoleA Rtab)               = "role" =: "tab"
  attrMap (RoleA Rtable)             = "role" =: "table"
  attrMap (RoleA Rtablist)           = "role" =: "tablist"
  attrMap (RoleA Rtabpanel)          = "role" =: "tabpanel"
  attrMap (RoleA Rterm)              = "role" =: "term"
  attrMap (RoleA Rtextbox)           = "role" =: "textbox"
  attrMap (RoleA Rtimer)             = "role" =: "timer"
  attrMap (RoleA Rtoolbar)           = "role" =: "toolbar"
  attrMap (RoleA Rtooltip)           = "role" =: "tooltip"
  attrMap (RoleA Rtree)              = "role" =: "tree"
  attrMap (RoleA Rtreegrid)          = "role" =: "treegrid"
  attrMap (RoleA Rtreeitem)          = "role" =: "treeitem"


instance Default Role where
  def = RoleT T.empty

class AttrHasRole a where
  attrSetRole :: Role -> a -> a

-- | Function 'role t' can be used to set any text to role-attribute.
-- Below you may find some pre-defined role-definitions.
role :: AttrHasRole a => Text -> a -> a
role t = attrSetRole (RoleT t)

-- | Pre-defined role-definitions.
-- @
-- rAlert def
-- @
rAlert, rAlertdialog, rApplication, rArticle, rBanner, rButton, rCell, rCheckbox,
  rColumnheader, rCombobox, rComplementary, rContentinfo, rDefinition, rDialog,
  rDirectory, rDocument, rFeed, rFigure, rForm, rGrid, rGridcell, rGroup,
  rHeading, rImg, rLink, rList, rListbox, rListitem, rLog, rMain, rMarquee,
  rMath, rMenu, rMenubar, rMenuitem, rMenuitemcheckbox, rMenuitemradio,
  rNavigation, rNone, rNote, rOption, rPresentation, rProgressbar, rRadio,
  rRadiogroup, rRegion, rRow, rRowgroup, rRowheader, rScrollbar, rSearch,
  rSearchbox, rSeparator, rSlider, rSpinbutton, rStatus, rSwitch, rTab, rTable,
  rTablist, rTabpanel, rTerm, rTextbox, rTimer, rToolbar, rTooltip, rTree,
  rTreegrid, rTreeitem  :: AttrHasRole a => a -> a
rAlert            = attrSetRole (RoleA Ralert)
rAlertdialog      = attrSetRole (RoleA Ralertdialog)
rApplication      = attrSetRole (RoleA Rapplication)
rArticle          = attrSetRole (RoleA Rarticle)
rBanner           = attrSetRole (RoleA Rbanner)
rButton           = attrSetRole (RoleA Rbutton)
rCell             = attrSetRole (RoleA Rcell)
rCheckbox         = attrSetRole (RoleA Rcheckbox)
rColumnheader     = attrSetRole (RoleA Rcolumnheader)
rCombobox         = attrSetRole (RoleA Rcombobox)
rComplementary    = attrSetRole (RoleA Rcomplementary)
rContentinfo      = attrSetRole (RoleA Rcontentinfo)
rDefinition       = attrSetRole (RoleA Rdefinition)
rDialog           = attrSetRole (RoleA Rdialog)
rDirectory        = attrSetRole (RoleA Rdirectory)
rDocument         = attrSetRole (RoleA Rdocument)
rFeed             = attrSetRole (RoleA Rfeed)
rFigure           = attrSetRole (RoleA Rfigure)
rForm             = attrSetRole (RoleA Rform)
rGrid             = attrSetRole (RoleA Rgrid)
rGridcell         = attrSetRole (RoleA Rgridcell)
rGroup            = attrSetRole (RoleA Rgroup)
rHeading          = attrSetRole (RoleA Rheading)
rImg              = attrSetRole (RoleA Rimg)
rLink             = attrSetRole (RoleA Rlink)
rList             = attrSetRole (RoleA Rlist)
rListbox          = attrSetRole (RoleA Rlistbox)
rListitem         = attrSetRole (RoleA Rlistitem)
rLog              = attrSetRole (RoleA Rlog)
rMain             = attrSetRole (RoleA Rmain)
rMarquee          = attrSetRole (RoleA Rmarquee)
rMath             = attrSetRole (RoleA Rmath)
rMenu             = attrSetRole (RoleA Rmenu)
rMenubar          = attrSetRole (RoleA Rmenubar)
rMenuitem         = attrSetRole (RoleA Rmenuitem)
rMenuitemcheckbox = attrSetRole (RoleA Rmenuitemcheckbox)
rMenuitemradio    = attrSetRole (RoleA Rmenuitemradio)
rNavigation       = attrSetRole (RoleA Rnavigation)
rNone             = attrSetRole (RoleA Rnone)
rNote             = attrSetRole (RoleA Rnote)
rOption           = attrSetRole (RoleA Roption)
rPresentation     = attrSetRole (RoleA Rpresentation)
rProgressbar      = attrSetRole (RoleA Rprogressbar)
rRadio            = attrSetRole (RoleA Rradio)
rRadiogroup       = attrSetRole (RoleA Rradiogroup)
rRegion           = attrSetRole (RoleA Rregion)
rRow              = attrSetRole (RoleA Rrow)
rRowgroup         = attrSetRole (RoleA Rrowgroup)
rRowheader        = attrSetRole (RoleA Rrowheader)
rScrollbar        = attrSetRole (RoleA Rscrollbar)
rSearch           = attrSetRole (RoleA Rsearch)
rSearchbox        = attrSetRole (RoleA Rsearchbox)
rSeparator        = attrSetRole (RoleA Rseparator)
rSlider           = attrSetRole (RoleA Rslider)
rSpinbutton       = attrSetRole (RoleA Rspinbutton)
rStatus           = attrSetRole (RoleA Rstatus)
rSwitch           = attrSetRole (RoleA Rswitch)
rTab              = attrSetRole (RoleA Rtab)
rTable            = attrSetRole (RoleA Rtable)
rTablist          = attrSetRole (RoleA Rtablist)
rTabpanel         = attrSetRole (RoleA Rtabpanel)
rTerm             = attrSetRole (RoleA Rterm)
rTextbox          = attrSetRole (RoleA Rtextbox)
rTimer            = attrSetRole (RoleA Rtimer)
rToolbar          = attrSetRole (RoleA Rtoolbar)
rTooltip          = attrSetRole (RoleA Rtooltip)
rTree             = attrSetRole (RoleA Rtree)
rTreegrid         = attrSetRole (RoleA Rtreegrid)
rTreeitem         = attrSetRole (RoleA Rtreeitem)

instance (Reflex t, AttrHasRole a) => AttrHasRole (Dynamic t a) where
  attrSetRole c = fmap (attrSetRole c)

------------------------------------------------------------------------------

-- | Assigns a slot in a shadow DOM shadow tree to an element.
-- See [MDN slot desc](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/slot).
newtype Slot = Slot Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Slot where
  attrMap (Slot t) = "slot" =: t

instance Default Slot where
  def = Slot T.empty

-- No monoid.

class AttrHasSlot a where
  attrSetSlot :: Slot -> a -> a

-- | Function 'slot t' can be used to set slot-attribute.
-- See [MDN slot desc](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/slot).
slot :: AttrHasSlot a => Text -> a -> a
slot t = attrSetSlot (Slot t)

instance (Reflex t, AttrHasSlot a) => AttrHasSlot (Dynamic t a) where
  attrSetSlot c = fmap (attrSetSlot c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
data SpellCheck = SCtrue | SCfalse
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance AttrMap SpellCheck where
  attrMap SCtrue = "spellcheck" =: "true"
  attrMap SCfalse = "spellcheck" =: "false"

instance Default SpellCheck where
  def = SCtrue

class AttrHasSpellCheck a where
  attrSetSpellCheck :: SpellCheck -> a -> a

-- | Functions 'spellcheck' and 'noSpellCheck' can be used
-- to set corresponding attributes.
spellcheck, noSpellCheck :: AttrHasSpellCheck a => a -> a
spellcheck = attrSetSpellCheck SCtrue
noSpellCheck = attrSetSpellCheck SCfalse

instance (Reflex t, AttrHasSpellCheck a) => AttrHasSpellCheck (Dynamic t a) where
  attrSetSpellCheck c = fmap (attrSetSpellCheck c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
-- Wish list: implement CSS style attributes specification. (hmm)
-- See e.g. https://www.w3.org/TR/css-style-attr/
newtype Style = Style Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Style where
  attrMap (Style t) = "style" =: t

instance Default Style where
  def = Style T.empty

class AttrHasStyle a where
  attrSetStyle :: Style -> a -> a

-- | Function 'style t' can be used to set style-attribute.
-- @
-- style "color:blue;text-align:center" def
-- @
style :: AttrHasStyle a => Text -> a -> a
style t = attrSetStyle (Style t)

instance Semigroup Style where
  (<>) (Style a) (Style b) = Style $ a <> ";" <> b

-- Do we need some separator here? TODO! Check this.
instance Monoid Style where
  mappend (Style a) (Style b) = Style $ a <> ";" <> b
  mempty = Style T.empty

instance (Reflex t, AttrHasStyle a) => AttrHasStyle (Dynamic t a) where
  attrSetStyle c = fmap (attrSetStyle c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
newtype TabIndex = TabIndex Int
  deriving (Show, Read, Eq, Ord, Bounded)

instance AttrMap TabIndex where
  attrMap (TabIndex i) = "tabindex" =: T.pack (show i)

instance Default TabIndex where
  def = TabIndex 0

class AttrHasTabIndex a where
  attrSetTabIndex :: TabIndex -> a -> a

-- | Function 'tabIndex i' can be used to set the corresponding attribute.
tabIndex :: AttrHasTabIndex a => Int -> a -> a
tabIndex t = attrSetTabIndex (TabIndex t)

instance (Reflex t, AttrHasTabIndex a) => AttrHasTabIndex (Dynamic t a) where
  attrSetTabIndex c = fmap (attrSetTabIndex c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
newtype Title = Title Text
  deriving (Show, Read, Eq, Ord)

instance AttrMap Title where
  attrMap (Title t) = "title" =: t

-- No default.

class AttrHasTitle a where
  attrSetTitle :: Title -> a -> a

-- | Function 'title t' can be used to set the title-attribute.
title :: AttrHasTitle a => Text -> a -> a
title t = attrSetTitle (Title t)

instance Semigroup Title where
  (<>) (Title a) (Title b) = Title $ a <> b

instance Monoid Title where
  mappend (Title a) (Title b) = Title $ a <> b
  mempty = Title T.empty

instance (Reflex t, AttrHasTitle a) => AttrHasTitle (Dynamic t a) where
  attrSetTitle c = fmap (attrSetTitle c)

------------------------------------------------------------------------------

-- |
-- Belongs to Globals.
data Translate = TRyes | TRno | TRinherit
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance AttrMap Translate where
  attrMap TRyes     = "translate" =: "yes"
  attrMap TRno      = "translate" =: "no"
  attrMap TRinherit = "translate" =: "inherit"

instance Default Translate where
  def = TRinherit

class AttrHasTranslate a where
  attrSetTranslate :: Translate -> a -> a

-- | Functions 'translate', 'noTranslate', 'inheritTranslate' can
-- be used to set the corresponding attribute.
translate, noTranslate, inheritTranslate :: AttrHasTranslate a => a -> a
translate = attrSetTranslate TRyes
noTranslate = attrSetTranslate TRno
inheritTranslate = attrSetTranslate TRinherit

instance (Reflex t, AttrHasTranslate a) => AttrHasTranslate (Dynamic t a) where
  attrSetTranslate c = fmap (attrSetTranslate c)


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | This is used to collect all global attributes and is
-- transformed into an attribute-map that any element can eat.
data Globals = Globals
  { _globalsAccessKey       :: Maybe AccessKey       -- 1.
  , _globalsAnmval          :: [Anmval]              -- 2. Monoid
  , _globalsContentEditable :: Maybe ContentEditable -- 3.
  , _globalsContextMenu     :: Maybe ContextMenu     -- 4.
  , _globalsClass           :: Maybe ClassName       -- 5. Monoid
  , _globalsDnmval          :: [Dnmval]              -- 6. Monoid
  , _globalsDir             :: Maybe Dir             -- 7.
  , _globalsDraggable       :: Maybe Draggable       -- 8.
  , _globalsHidden          :: Maybe Hidden          -- 9.
  , _globalsId              :: Maybe Id_             -- 10.
  , _globalsLang            :: Maybe Lang            -- 11.
  , _globalsRole            :: Maybe Role            -- 12.
  , _globalsSlot            :: Maybe Slot            -- 13.
  , _globalsSpellCheck      :: Maybe SpellCheck      -- 14.
  , _globalsStyle           :: Maybe Style           -- 15. Monoid
  , _globalsTabIndex        :: Maybe TabIndex        -- 16.
  , _globalsTitle           :: Maybe Title           -- 17. Monoid
  , _globalsTranslate       :: Maybe Translate       -- 18.
  } deriving (Eq, Show, Read, Ord)

instance AttrMap Globals where
  attrMap g = fold $ catMaybes
    [ attrMap <$> _globalsAccessKey g
    , attrMap <$> _globalsContentEditable g
    , attrMap <$> _globalsContextMenu g
    , attrMap <$> _globalsClass g
    , attrMap <$> _globalsDir g
    , attrMap <$> _globalsDraggable g
    , attrMap <$> _globalsHidden g
    , attrMap <$> _globalsId g
    , attrMap <$> _globalsLang g
    , attrMap <$> _globalsRole g
    , attrMap <$> _globalsSlot g
    , attrMap <$> _globalsSpellCheck g
    , attrMap <$> _globalsStyle g
    , attrMap <$> _globalsTabIndex g
    , attrMap <$> _globalsTitle g
    , attrMap <$> _globalsTranslate g
    ] ++ (attrMap <$> _globalsAnmval g)
      ++ (attrMap <$> _globalsDnmval g)

instance Default Globals where
  def = Globals def def def def def def def def def def
                def def def def def def def def

instance AttrHasAccessKey Globals       where attrSetAccessKey p g       = g { _globalsAccessKey       = Just p }
instance AttrHasContentEditable Globals where attrSetContentEditable p g = g { _globalsContentEditable = Just p }
instance AttrHasContextMenu Globals     where attrSetContextMenu p g     = g { _globalsContextMenu     = Just p }
instance AttrHasClass Globals           where attrSetClassName p g           = g { _globalsClass           = Just p }
instance AttrHasDir Globals             where attrSetDir p g             = g { _globalsDir             = Just p }
instance AttrHasDraggable Globals       where attrSetDraggable p g       = g { _globalsDraggable       = Just p }
instance AttrHasHidden Globals          where attrSetHidden p g          = g { _globalsHidden          = Just p }
instance AttrHasId Globals              where attrSetId p g              = g { _globalsId              = Just p }
instance AttrHasLang Globals            where attrSetLang p g            = g { _globalsLang            = Just p }
instance AttrHasRole Globals            where attrSetRole p g            = g { _globalsRole            = Just p }
instance AttrHasSlot Globals            where attrSetSlot p g            = g { _globalsSlot            = Just p }
instance AttrHasSpellCheck Globals      where attrSetSpellCheck p g      = g { _globalsSpellCheck      = Just p }
instance AttrHasStyle Globals           where attrSetStyle p g           = g { _globalsStyle           = Just p }
instance AttrHasTabIndex Globals        where attrSetTabIndex p g        = g { _globalsTabIndex        = Just p }
instance AttrHasTitle Globals           where attrSetTitle p g           = g { _globalsTitle           = Just p }
instance AttrHasTranslate Globals       where attrSetTranslate p g       = g { _globalsTranslate       = Just p }

instance AttrHasAnmval Globals
  where attrSetAnmval p g = g { _globalsAnmval = _globalsAnmval g <> [p] }

instance AttrHasDnmval Globals
  where attrSetDnmval p g = g { _globalsDnmval = _globalsDnmval g <> [p] }


instance AttrGetClassName Globals where
  attrGetClassName g = fromMaybe (ClassName T.empty) (_globalsClass g)
    -- attrGetClassName g = case _globalsClass g of
    --                    Nothing -> ClassName T.empty
    --                    Just g' -> g'


-- | Every element has global attributes.
class ( AttrHasAccessKey a, AttrHasAnmval a, AttrHasContentEditable a
      , AttrHasContextMenu a, AttrHasClass a, AttrHasDnmval a, AttrHasDir a
      , AttrHasDraggable a, AttrHasHidden a, AttrHasId a, AttrHasLang a
      , AttrHasRole a, AttrHasSlot a, AttrHasSpellCheck a, AttrHasStyle a
      , AttrHasTabIndex a, AttrHasTitle a, AttrHasTranslate a
      ) => AttrHasGlobals a where
  attrSetGlobals :: Globals -> a -> a


instance Semigroup Globals where
  (<>)
    (Globals
      _a1 a2 _a3 _a4 a5 a6 _a7 _a8 _a9 _a10 _a11 _a12 _a13 _a14 a15 _a16 a17 _a18)
    (Globals
       b1 b2  b3  b4 b5 b6  b7  b8  b9  b10  b11  b12  b13  b14 b15  b16 b17  b18)
      = Globals c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
    where
      c1 = b1
      c2 = a2 <> b2
      c3 = b3
      c4 = b4
      c5 = a5 <> b5
      c6 = a6 <> b6
      c7 = b7
      c8 = b8
      c9 = b9
      c10 = b10
      c11 = b11
      c12 = b12
      c13 = b13
      c14 = b14
      c15 = a15 <> b15
      c16 = b16
      c17 = a17 <> b17
      c18 = b18

-- We can implement the monoid behaviour either here or for each attribute.
-- It is done here as the mappend is meaningless for some of the fields.
-- For meaningless we choose the right most value, e.g. in a <> b that is a.
-- ClassName, Style and Title attributes have monoid instances.
-- Or should this be provided at all?
instance Monoid Globals where
  mempty = def
  mappend = (<>)
    -- (Globals
    --   _a1 a2 _a3 _a4 a5 a6 _a7 _a8 _a9 _a10 _a11 _a12 _a13 _a14 a15 _a16 a17 _a18)
    -- (Globals
    --    b1 b2  b3  b4 b5 b6  b7  b8  b9  b10  b11  b12  b13  b14 b15  b16 b17  b18)
    --   = Globals c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
    -- where
    --   c1 = b1
    --   c2 = a2 <> b2
    --   c3 = b3
    --   c4 = b4
    --   c5 = a5 <> b5
    --   c6 = a6 <> b6
    --   c7 = b7
    --   c8 = b8
    --   c9 = b9
    --   c10 = b10
    --   c11 = b11
    --   c12 = b12
    --   c13 = b13
    --   c14 = b14
    --   c15 = a15 <> b15
    --   c16 = b16
    --   c17 = a17 <> b17
    --   c18 = b18
  -- mappend (Globals a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
  --         (Globals b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14)
  --   = Globals (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
  --       (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)
  --       (a13 <> b13) (a14 <> b14)


-- | 'gDef' is a shorthand for 'def :: Globals'.
gDef :: Globals
gDef = def

------------------------------------------------------------------------------
------------------------------------------------------------------------------

