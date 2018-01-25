{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs.Aria
Description : Some aria-* related constants.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Aria attributes

This module defines some of the most common aria-* attributes.


-}

module Reflex.Dom.HTML5.Attrs.Aria where

import Data.Text (Text)
import Reflex.Dom.HTML5.Attrs.Globals (AttrHasAnmval, aNmVal)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
-- aria-activedescendant aria-atomic aria-autocomplete aria-busy aria-checked
-- aria-colcount aria-colindex aria-colspan aria-controls aria-current
-- aria-describedby aria-details aria-dialog aria-disabled aria-dropeffect
-- aria-errormessage aria-expanded aria-flowto aria-grabbed aria-haspopup
-- aria-hidden aria-invalid aria-keyshortcuts aria-label aria-labelledby
-- aria-level aria-live aria-multiline aria-multiselectable aria-orientation
-- aria-owns aria-placeholder aria-posinset aria-pressed aria-readonly
-- aria-relevant aria-required aria-roledescription aria-rowcount
-- aria-rowindex aria-rowspan aria-selected aria-setsize aria-sort
-- aria-valuemax aria-valuemin aria-valuenow aria-valuetext
--
------------------------------------------------------------------------------
------------------------------------------------------------------------------

aActivedescendant, aAtomic, aAutocomplete, aBusy, aChecked, aColcount,
 aColindex, aColspan, aControls, aCurrent, aDescribedby, aDetails,
 aDialog, aDisabled, aDropeffect, aErrormessage, aExpanded, aFlowto,
 aGrabbed, aHaspopup, aHidden, aInvalid, aKeyshortcuts, aLabel,
 aLabelledby, aLevel, aLive, aMultiline, aMultiselectable, aOrientation,
 aOwns, aPlaceholder, aPosinset, aPressed, aReadonly, aRelevant,
 aRequired, aRoledescription, aRowcount, aRowindex, aRowspan, aSelected,
 aSetsize, aSort, aValuemax, aValuemin, aValuenow, aValuetext
   :: AttrHasAnmval a => Text -> a -> a

aActivedescendant = aNmVal "activedescendant"
aAtomic           = aNmVal "atomic"
aAutocomplete     = aNmVal "autocomplete"
aBusy             = aNmVal "busy"
aChecked          = aNmVal "checked"
aColcount         = aNmVal "colcount"
aColindex         = aNmVal "colindex"
aColspan          = aNmVal "colspan"
aControls         = aNmVal "controls"
aCurrent          = aNmVal "current"
aDescribedby      = aNmVal "describedby"
aDetails          = aNmVal "details"
aDialog           = aNmVal "dialog"
aDisabled         = aNmVal "disabled"
aDropeffect       = aNmVal "dropeffect"
aErrormessage     = aNmVal "errormessage"
aExpanded         = aNmVal "expanded"
aFlowto           = aNmVal "flowto"
aGrabbed          = aNmVal "grabbed"
aHaspopup         = aNmVal "haspopup"
aHidden           = aNmVal "hidden"
aInvalid          = aNmVal "invalid"
aKeyshortcuts     = aNmVal "keyshortcuts"
aLabel            = aNmVal "label"
aLabelledby       = aNmVal "labelledby"
aLevel            = aNmVal "level"
aLive             = aNmVal "live"
aMultiline        = aNmVal "multiline"
aMultiselectable  = aNmVal "multiselectable"
aOrientation      = aNmVal "orientation"
aOwns             = aNmVal "owns"
aPlaceholder      = aNmVal "placeholder"
aPosinset         = aNmVal "posinset"
aPressed          = aNmVal "pressed"
aReadonly         = aNmVal "readonly"
aRelevant         = aNmVal "relevant"
aRequired         = aNmVal "required"
aRoledescription  = aNmVal "roledescription"
aRowcount         = aNmVal "rowcount"
aRowindex         = aNmVal "rowindex"
aRowspan          = aNmVal "rowspan"
aSelected         = aNmVal "selected"
aSetsize          = aNmVal "setsize"
aSort             = aNmVal "sort"
aValuemax         = aNmVal "valuemax"
aValuemin         = aNmVal "valuemin"
aValuenow         = aNmVal "valuenow"
aValuetext        = aNmVal "valuetext"

------------------------------------------------------------------------------
