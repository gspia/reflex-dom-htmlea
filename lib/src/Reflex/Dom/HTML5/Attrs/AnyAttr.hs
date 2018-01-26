{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Attrs.AnyAttr
Description : Used to add any attribute to an attribute set.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= AnyAttr

This is not finished. And actually should be not used even
if this were ready. (Any thus maybe gets removed as addCustom
can always be used if there is a need to mix things.)

-}

module Reflex.Dom.HTML5.Attrs.AnyAttr where

import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T

import Reflex.Dom.HTML5.Attrs.Common
import Reflex.Dom.HTML5.Attrs.Globals
import Reflex.Dom.HTML5.Attrs.Attrs


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | This is not up-to-date. TODO
data AnyAttr = AnyAttr
  { _anyGlobal          :: Maybe Globals         --  0.
  , _anyAbbr            :: Maybe Abbr            --  1.
  , _anyAccept          :: Maybe Accept          --  2.
  , _anyAcceptCharSet   :: Maybe AcceptCharSet   --  3.
  , _anyAction          :: Maybe Action          --  4.
  , _anyAllowFullScreen :: Maybe AllowFullScreen --  5.
  , _anyAlt             :: Maybe Alt_            --  6.
  , _anyAsync           :: Maybe Async           --  7.
  , _anyAutoComplete    :: Maybe AutoComplete    --  8.
  , _anyAutoFocus       :: Maybe AutoFocus       --  9.
  , _anyAutoplay        :: Maybe Autoplay        -- 10.
  , _anyButtonType      :: Maybe ButtonType      -- 92.
  , _anyCharSet         :: Maybe CharSet         -- 11.
  , _anyChecked         :: Maybe Checked         -- 12.
  , _anyCite            :: Maybe Cite            -- 13.
  , _anyCols            :: Maybe Cols            -- 14.
  , _anyColSpan         :: Maybe ColSpan         -- 15.
  , _anyContent         :: Maybe Content         -- 16.
  , _anyControls        :: Maybe Controls        -- 17.
  , _anyCoords          :: Maybe Coords          -- 18.
  , _anyCrossOrigin     :: Maybe CrossOrigin     -- 19.
  , _anyDataValue       :: Maybe DataValue       -- 20.
  , _anyDateTime        :: Maybe DateTime        -- 21.
  , _anyDefault         :: Maybe Default_        -- 22.
  , _anyDefer           :: Maybe Defer           -- 23.
  , _anyDirName         :: Maybe DirName         -- 24.
  , _anyDisabled        :: Maybe Disabled        -- 25.
  , _anyDownload        :: Maybe Download        -- 26.
  , _anyEncType         :: Maybe EncType         -- 27.
  , _anyForId           :: Maybe ForId           -- 28.
  , _anyForm            :: Maybe Form            -- 29.
  , _anyFormAction      :: Maybe FormAction      -- 30.
  , _anyFormEncType     :: Maybe FormEncType     -- 31.
  , _anyFormMethod      :: Maybe FormMethod      -- 32.
  , _anyFormNoValidate  :: Maybe FormNoValidate  -- 33.
  , _anyFormTarget      :: Maybe FormTarget      -- 34.
  , _anyHeaders         :: Maybe Headers         -- 35.
  , _anyHeight          :: Maybe Height          -- 36.
  , _anyHigh            :: Maybe High            -- 37.
  , _anyHref            :: Maybe Href            -- 38.
  , _anyHrefLang        :: Maybe HrefLang        -- 39.
  , _anyHttpEquiv       :: Maybe HttpEquiv       -- 40.
  , _anyIcon            :: Maybe Icon            -- 41.
  , _anyInputMode       :: Maybe InputMode       -- 42.
  , _anyIsMap           :: Maybe IsMap           -- 43.
  , _anyKind            :: Maybe Kind            -- 44.
  , _anyLabel           :: Maybe Label           -- 45.
  , _anyList            :: Maybe List            -- 46.
  , _anyLongDesc        :: Maybe LongDesc        -- 47.
  , _anyLoop            :: Maybe Loop            -- 48.
  , _anyLow             :: Maybe Low             -- 49.
  , _anyMax             :: Maybe Max             -- 50.
  , _anyMaxLength       :: Maybe MaxLength       -- 51.
  , _anyMedia           :: Maybe Media           -- 52.
  , _anyMediaType       :: Maybe MediaType       -- 53.
  , _anyMenu            :: Maybe Menu            -- 54.
  , _anyMethod          :: Maybe Method          -- 55.
  , _anyMin             :: Maybe Min             -- 56.
  , _anyMinLength       :: Maybe MinLength       -- 57. --
  , _anyMuted           :: Maybe Muted           -- 59.
  , _anyName            :: Maybe Name            -- 60.
  , _anyNonce           :: Maybe Nonce           -- 61.
  , _anyNoValidate      :: Maybe NoValidate      -- 62.
  , _anyOpen            :: Maybe Open            -- 63.
  , _anyOptimum         :: Maybe Optimum         -- 64.
  , _anyPattern         :: Maybe Pattern         -- 65. --
  , _anyPoster          :: Maybe Poster          -- 67.
  , _anyPreload         :: Maybe Preload         -- 68.
  , _anyReadOnly        :: Maybe ReadOnly        -- 69.
  , _anyRel             :: Maybe Rel             -- 70.
  , _anyReferrerPolicy  :: Maybe ReferrerPolicy  -- 71.
  , _anyRequired        :: Maybe Required        -- 72.
  , _anyReversed        :: Maybe Reversed        -- 73.
  , _anyRows            :: Maybe Rows            -- 74.
  , _anyRowSpan         :: Maybe RowSpan         -- 75.
  , _anySandbox         :: Maybe Sandbox         -- 76.
  , _anyScope           :: Maybe Scope           -- 77.
  , _anySelected        :: Maybe Selected        -- 78.
  , _anyShape           :: Maybe Shape           -- 79.
  , _anySize            :: Maybe Size            -- 80.
  , _anySizes           :: Maybe Sizes           -- 81.
  , _anySpan            :: Maybe Span            -- 82.
  , _anySrc             :: Maybe Src             -- 83.
  , _anySrcDoc          :: Maybe SrcDoc          -- 84.
  , _anySrcLang         :: Maybe SrcLang         -- 85.
  , _anySrcSet          :: Maybe SrcSet          -- 86.
  , _anySrcSetW         :: Maybe SrcSetW         -- 87.
  , _anySrcSetP         :: Maybe SrcSetP         -- 88.
  , _anyStart           :: Maybe Start           -- 89.
  , _anyStep            :: Maybe Step            -- 90.
  , _anyTarget          :: Maybe Target          -- 91.
  , _anyTypeMustMatch   :: Maybe TypeMustMatch   -- 93.
  , _anyUseMap          :: Maybe UseMap          -- 94.
  , _anyValueText       :: Maybe ValueText       -- 95.
  , _anyValueNumber     :: Maybe ValueNumber     -- 96.
  , _anyValueOlLi       :: Maybe ValueOlLi       -- 97.
  , _anyWidth           :: Maybe Width           -- 98.
  , _anyWrap            :: Maybe Wrap            -- 99.
  }

instance AttrMap AnyAttr where
  attrMap g = fold $ catMaybes
    [ attrMap <$> _anyGlobal          g --  0.
    , attrMap <$> _anyAbbr            g --  1.
    , attrMap <$> _anyAccept          g --  2.
    , attrMap <$> _anyAcceptCharSet   g --  3.
    , attrMap <$> _anyAction          g --  4.
    , attrMap <$> _anyAllowFullScreen g --  5.
    , attrMap <$> _anyAlt             g --  6.
    , attrMap <$> _anyAsync           g --  7.
    , attrMap <$> _anyAutoComplete    g --  8.
    , attrMap <$> _anyAutoFocus       g --  9.
    , attrMap <$> _anyAutoplay        g -- 10.
    , attrMap <$> _anyButtonType    g -- 92.
    , attrMap <$> _anyCharSet         g -- 11.
    , attrMap <$> _anyChecked         g -- 12.
    , attrMap <$> _anyCite            g -- 13.
    , attrMap <$> _anyCols            g -- 14.
    , attrMap <$> _anyColSpan         g -- 15.
    , attrMap <$> _anyContent         g -- 16.
    , attrMap <$> _anyControls        g -- 17.
    , attrMap <$> _anyCoords          g -- 18.
    , attrMap <$> _anyCrossOrigin     g -- 19.
    , attrMap <$> _anyDataValue       g -- 20.
    , attrMap <$> _anyDateTime        g -- 21.
    , attrMap <$> _anyDefault         g -- 22.
    , attrMap <$> _anyDefer           g -- 23.
    , attrMap <$> _anyDirName         g -- 24.
    , attrMap <$> _anyDisabled        g -- 25.
    , attrMap <$> _anyDownload        g -- 26.
    , attrMap <$> _anyEncType         g -- 27.
    , attrMap <$> _anyForId           g -- 28.
    , attrMap <$> _anyForm            g -- 29.
    , attrMap <$> _anyFormAction      g -- 30.
    , attrMap <$> _anyFormEncType     g -- 31.
    , attrMap <$> _anyFormMethod      g -- 32.
    , attrMap <$> _anyFormNoValidate  g -- 33.
    , attrMap <$> _anyFormTarget      g -- 34.
    , attrMap <$> _anyHeaders         g -- 35.
    , attrMap <$> _anyHeight          g -- 36.
    , attrMap <$> _anyHigh            g -- 37.
    , attrMap <$> _anyHref            g -- 38.
    , attrMap <$> _anyHrefLang        g -- 39.
    , attrMap <$> _anyHttpEquiv       g -- 40.
    , attrMap <$> _anyIcon            g -- 41.
    , attrMap <$> _anyInputMode       g -- 42.
    , attrMap <$> _anyIsMap           g -- 43.
    , attrMap <$> _anyKind            g -- 44.
    , attrMap <$> _anyLabel           g -- 45.
    , attrMap <$> _anyList            g -- 46.
    , attrMap <$> _anyLongDesc        g -- 47.
    , attrMap <$> _anyLoop            g -- 48.
    , attrMap <$> _anyLow             g -- 49.
    , attrMap <$> _anyMax             g -- 50.
    , attrMap <$> _anyMaxLength       g -- 51.
    , attrMap <$> _anyMedia           g -- 52.
    , attrMap <$> _anyMediaType       g -- 53.
    , attrMap <$> _anyMenu            g -- 54.
    , attrMap <$> _anyMethod          g -- 55.
    , attrMap <$> _anyMin             g -- 56.
    , attrMap <$> _anyMinLength       g -- 57.
    , attrMap <$> _anyMuted           g -- 59.
    , attrMap <$> _anyName            g -- 60.
    , attrMap <$> _anyNonce           g -- 61.
    , attrMap <$> _anyNoValidate      g -- 62.
    , attrMap <$> _anyOpen            g -- 63.
    , attrMap <$> _anyOptimum         g -- 64.
    , attrMap <$> _anyPattern         g -- 65.
    , attrMap <$> _anyPoster          g -- 67.
    , attrMap <$> _anyPreload         g -- 68.
    , attrMap <$> _anyReadOnly        g -- 69.
    , attrMap <$> _anyRel             g -- 70.
    , attrMap <$> _anyReferrerPolicy  g -- 71.
    , attrMap <$> _anyRequired        g -- 72.
    , attrMap <$> _anyReversed        g -- 73.
    , attrMap <$> _anyRows            g -- 74.
    , attrMap <$> _anyRowSpan         g -- 75.
    , attrMap <$> _anySandbox         g -- 76.
    , attrMap <$> _anyScope           g -- 77.
    , attrMap <$> _anySelected        g -- 78.
    , attrMap <$> _anyShape           g -- 79.
    , attrMap <$> _anySize            g -- 80.
    , attrMap <$> _anySizes           g -- 81.
    , attrMap <$> _anySpan            g -- 82.
    , attrMap <$> _anySrc             g -- 83.
    , attrMap <$> _anySrcDoc          g -- 84.
    , attrMap <$> _anySrcLang         g -- 85.
    , attrMap <$> _anySrcSet          g -- 86.
    , attrMap <$> _anySrcSetW         g -- 87.
    , attrMap <$> _anySrcSetP         g -- 88.
    , attrMap <$> _anyStart           g -- 89.
    , attrMap <$> _anyStep            g -- 90.
    , attrMap <$> _anyTarget          g -- 91.
    , attrMap <$> _anyTypeMustMatch   g -- 93.
    , attrMap <$> _anyUseMap          g -- 94.
    , attrMap <$> _anyValueText       g -- 95.
    , attrMap <$> _anyValueNumber     g -- 96.
    , attrMap <$> _anyValueOlLi       g -- 97.
    , attrMap <$> _anyWidth           g -- 98.
    , attrMap <$> _anyWrap            g -- 99.
    ]

instance Default AnyAttr where
  def = AnyAttr def def def def def def def def def def def def def def def def
    def def def def def def def def def def def def def def def def def def def
    def def def def def def def def def def def def def def def def def def def
    def def def def def def def def def def def def def def def def def def def
    def def def def def def def def def def def def def def def def def def def
    def def def def def def


-- Global attributes require the following instances.
instance AttrHasAccessKey AnyAttr
  -- where attrSetAccessKey p g = g { _anyGlobal = Just (attrSetAccessKey p (maybe gDef id (_anyGlobal g))) }
  where attrSetAccessKey p g = g { _anyGlobal = Just (attrSetAccessKey p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasAnmval AnyAttr
  where attrSetAnmval p g = g { _anyGlobal = Just (attrSetAnmval p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasContentEditable AnyAttr
  where attrSetContentEditable p g = g  { _anyGlobal = Just (attrSetContentEditable p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasContextMenu AnyAttr
  where attrSetContextMenu p g     = g { _anyGlobal = Just (attrSetContextMenu p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasClass AnyAttr
  where attrSetClassName p g           = g { _anyGlobal = Just (attrSetClassName p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasDnmval AnyAttr
  where attrSetDnmval p g           = g { _anyGlobal = Just (attrSetDnmval p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasDir AnyAttr
  where attrSetDir p g             = g { _anyGlobal = Just (attrSetDir p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasDraggable AnyAttr
  where attrSetDraggable p g       = g { _anyGlobal = Just (attrSetDraggable p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasHidden AnyAttr
  where attrSetHidden p g          = g { _anyGlobal = Just (attrSetHidden p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasId AnyAttr
  where attrSetId p g              = g { _anyGlobal = Just (attrSetId p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasLang AnyAttr
  where attrSetLang p g            = g { _anyGlobal = Just (attrSetLang p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasRole AnyAttr
  where attrSetRole p g            = g { _anyGlobal = Just (attrSetRole p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasSlot AnyAttr
  where attrSetSlot p g            = g { _anyGlobal = Just (attrSetSlot p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasSpellCheck AnyAttr
  where attrSetSpellCheck p g      = g { _anyGlobal = Just (attrSetSpellCheck p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasStyle AnyAttr
  where attrSetStyle p g           = g { _anyGlobal = Just (attrSetStyle p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasTabIndex AnyAttr
  where attrSetTabIndex p g        = g { _anyGlobal = Just (attrSetTabIndex p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasTitle AnyAttr
  where attrSetTitle p g           = g { _anyGlobal = Just (attrSetTitle p (fromMaybe gDef (_anyGlobal g))) }
instance AttrHasTranslate AnyAttr
  where attrSetTranslate p g       = g { _anyGlobal = Just (attrSetTranslate p (fromMaybe gDef (_anyGlobal g))) }

instance AttrGetClassName AnyAttr where attrGetClassName g = maybe (ClassName T.empty) attrGetClassName (_anyGlobal g)
      -- case _globalsClass g of
      --                  Nothing -> ClassName T.empty
      --                  Just g' -> g'



instance AttrHasGlobals         AnyAttr where attrSetGlobals p g         = g { _anyGlobal          = Just p } --  0.
instance AttrHasAbbr            AnyAttr where attrSetAbbr p g            = g { _anyAbbr            = Just p } --  1.
instance AttrHasAccept          AnyAttr where attrSetAccept p g          = g { _anyAccept          = Just p } --  2.
instance AttrHasAcceptCharSet   AnyAttr where attrSetAcceptCharSet p g   = g { _anyAcceptCharSet   = Just p } --  3.
instance AttrHasAction          AnyAttr where attrSetAction p g          = g { _anyAction          = Just p } --  4.
instance AttrHasAllowFullScreen AnyAttr where attrSetAllowFullScreen p g = g { _anyAllowFullScreen = Just p } --  5.
instance AttrHasAlt             AnyAttr where attrSetAlt p g             = g { _anyAlt             = Just p } --  6.
instance AttrHasAsync           AnyAttr where attrSetAsync p g           = g { _anyAsync           = Just p } --  7.
instance AttrHasAutoComplete    AnyAttr where attrSetAutoComplete p g    = g { _anyAutoComplete    = Just p } --  8.
instance AttrHasAutoFocus       AnyAttr where attrSetAutoFocus p g       = g { _anyAutoFocus       = Just p } --  9.
instance AttrHasAutoplay        AnyAttr where attrSetAutoplay p g        = g { _anyAutoplay        = Just p } -- 10.
instance AttrHasButtonType      AnyAttr where attrSetButtonType p g      = g { _anyButtonType      = Just p } -- 92.
instance AttrHasCharSet         AnyAttr where attrSetCharSet p g         = g { _anyCharSet         = Just p } -- 11.
instance AttrHasChecked         AnyAttr where attrSetChecked p g         = g { _anyChecked         = Just p } -- 12.
instance AttrHasCite            AnyAttr where attrSetCite p g            = g { _anyCite            = Just p } -- 13.
instance AttrHasCols            AnyAttr where attrSetCols p g            = g { _anyCols            = Just p } -- 14.
instance AttrHasColSpan         AnyAttr where attrSetColSpan p g         = g { _anyColSpan         = Just p } -- 15.
instance AttrHasContent         AnyAttr where attrSetContent p g         = g { _anyContent         = Just p } -- 16.
instance AttrHasControls        AnyAttr where attrSetControls p g        = g { _anyControls        = Just p } -- 17.
instance AttrHasCoords          AnyAttr where attrSetCoords p g          = g { _anyCoords          = Just p } -- 18.
instance AttrHasCrossOrigin     AnyAttr where attrSetCrossOrigin p g     = g { _anyCrossOrigin     = Just p } -- 19.
instance AttrHasDataValue       AnyAttr where attrSetDataValue p g       = g { _anyDataValue       = Just p } -- 20.
instance AttrHasDateTime        AnyAttr where attrSetDateTime p g        = g { _anyDateTime        = Just p } -- 21.
instance AttrHasDefault         AnyAttr where attrSetDefault p g         = g { _anyDefault         = Just p } -- 22.
instance AttrHasDefer           AnyAttr where attrSetDefer p g           = g { _anyDefer           = Just p } -- 23.
instance AttrHasDirName         AnyAttr where attrSetDirName p g         = g { _anyDirName         = Just p } -- 24.
instance AttrHasDisabled        AnyAttr where attrSetDisabled p g        = g { _anyDisabled        = Just p } -- 25.
instance AttrHasDownload        AnyAttr where attrSetDownload p g        = g { _anyDownload        = Just p } -- 26.
instance AttrHasEncType         AnyAttr where attrSetEncType p g         = g { _anyEncType         = Just p } -- 27.
instance AttrHasForId           AnyAttr where attrSetForId p g           = g { _anyForId           = Just p } -- 28.
instance AttrHasForm            AnyAttr where attrSetForm p g            = g { _anyForm            = Just p } -- 29.
instance AttrHasFormAction      AnyAttr where attrSetFormAction p g      = g { _anyFormAction      = Just p } -- 30.
instance AttrHasFormEncType     AnyAttr where attrSetFormEncType p g     = g { _anyFormEncType     = Just p } -- 31.
instance AttrHasFormMethod      AnyAttr where attrSetFormMethod p g      = g { _anyFormMethod      = Just p } -- 32.
instance AttrHasFormNoValidate  AnyAttr where attrSetFormNoValidate p g  = g { _anyFormNoValidate  = Just p } -- 33.
instance AttrHasFormTarget      AnyAttr where attrSetFormTarget p g      = g { _anyFormTarget      = Just p } -- 34.
instance AttrHasHeaders         AnyAttr where attrSetHeaders p g         = g { _anyHeaders         = Just p } -- 35.
instance AttrHasHeight          AnyAttr where attrSetHeight p g          = g { _anyHeight          = Just p } -- 36.
instance AttrHasHigh            AnyAttr where attrSetHigh p g            = g { _anyHigh            = Just p } -- 37.
instance AttrHasHref            AnyAttr where attrSetHref p g            = g { _anyHref            = Just p } -- 38.
instance AttrHasHrefLang        AnyAttr where attrSetHrefLang p g        = g { _anyHrefLang        = Just p } -- 39.
instance AttrHasHttpEquiv       AnyAttr where attrSetHttpEquiv p g       = g { _anyHttpEquiv       = Just p } -- 40.
instance AttrHasIcon            AnyAttr where attrSetIcon p g            = g { _anyIcon            = Just p } -- 41.
instance AttrHasInputMode       AnyAttr where attrSetInputMode p g       = g { _anyInputMode       = Just p } -- 42.
instance AttrHasIsMap           AnyAttr where attrSetIsMap p g           = g { _anyIsMap           = Just p } -- 43.
instance AttrHasKind            AnyAttr where attrSetKind p g            = g { _anyKind            = Just p } -- 44.
instance AttrHasLabel           AnyAttr where attrSetLabel p g           = g { _anyLabel           = Just p } -- 45.
instance AttrHasList            AnyAttr where attrSetList p g            = g { _anyList            = Just p } -- 46.
instance AttrHasLongDesc        AnyAttr where attrSetLongDesc p g        = g { _anyLongDesc        = Just p } -- 47.
instance AttrHasLoop            AnyAttr where attrSetLoop p g            = g { _anyLoop            = Just p } -- 48.
instance AttrHasLow             AnyAttr where attrSetLow p g             = g { _anyLow             = Just p } -- 49.
instance AttrHasMax             AnyAttr where attrSetMax p g             = g { _anyMax             = Just p } -- 50.
instance AttrHasMaxLength       AnyAttr where attrSetMaxLength p g       = g { _anyMaxLength       = Just p } -- 51.
instance AttrHasMedia           AnyAttr where attrSetMedia p g           = g { _anyMedia           = Just p } -- 52.
instance AttrHasMediaType       AnyAttr where attrSetMediaType p g       = g { _anyMediaType       = Just p } -- 53.
instance AttrHasMenu            AnyAttr where attrSetMenu p g            = g { _anyMenu            = Just p } -- 54.
instance AttrHasMethod          AnyAttr where attrSetMethod p g          = g { _anyMethod          = Just p } -- 55.
instance AttrHasMin             AnyAttr where attrSetMin p g             = g { _anyMin             = Just p } -- 56.
instance AttrHasMinLength       AnyAttr where attrSetMinLength p g       = g { _anyMinLength       = Just p } -- 57.
instance AttrHasMuted           AnyAttr where attrSetMuted p g           = g { _anyMuted           = Just p } -- 59.
instance AttrHasName            AnyAttr where attrSetName p g            = g { _anyName            = Just p } -- 60.
instance AttrHasNonce           AnyAttr where attrSetNonce p g           = g { _anyNonce           = Just p } -- 61.
instance AttrHasNoValidate      AnyAttr where attrSetNoValidate p g      = g { _anyNoValidate      = Just p } -- 62.
instance AttrHasOpen            AnyAttr where attrSetOpen p g            = g { _anyOpen            = Just p } -- 63.
instance AttrHasOptimum         AnyAttr where attrSetOptimum p g         = g { _anyOptimum         = Just p } -- 64.
instance AttrHasPattern         AnyAttr where attrSetPattern p g         = g { _anyPattern         = Just p } -- 65.
instance AttrHasPoster          AnyAttr where attrSetPoster p g          = g { _anyPoster          = Just p } -- 67.
instance AttrHasPreload         AnyAttr where attrSetPreload p g         = g { _anyPreload         = Just p } -- 68.
instance AttrHasReadOnly        AnyAttr where attrSetReadOnly p g        = g { _anyReadOnly        = Just p } -- 69.
instance AttrHasRel             AnyAttr where attrSetRel p g             = g { _anyRel             = Just p } -- 70.
instance AttrHasReferrerPolicy  AnyAttr where attrSetReferrerPolicy p g  = g { _anyReferrerPolicy  = Just p } -- 71.
instance AttrHasRequired        AnyAttr where attrSetRequired p g        = g { _anyRequired        = Just p } -- 72.
instance AttrHasReversed        AnyAttr where attrSetReversed p g        = g { _anyReversed        = Just p } -- 73.
instance AttrHasRows            AnyAttr where attrSetRows p g            = g { _anyRows            = Just p } -- 74.
instance AttrHasRowSpan         AnyAttr where attrSetRowSpan p g         = g { _anyRowSpan         = Just p } -- 75.
instance AttrHasSandbox         AnyAttr where attrSetSandbox p g         = g { _anySandbox         = Just p } -- 76.
instance AttrHasScope           AnyAttr where attrSetScope p g           = g { _anyScope           = Just p } -- 77.
instance AttrHasSelected        AnyAttr where attrSetSelected p g        = g { _anySelected        = Just p } -- 78.
instance AttrHasShape           AnyAttr where attrSetShape p g           = g { _anyShape           = Just p } -- 79.
instance AttrHasSize            AnyAttr where attrSetSize p g            = g { _anySize            = Just p } -- 80.
instance AttrHasSizes           AnyAttr where attrSetSizes p g           = g { _anySizes           = Just p } -- 81.
instance AttrHasSpan            AnyAttr where attrSetSpan p g            = g { _anySpan            = Just p } -- 82.
instance AttrHasSrc             AnyAttr where attrSetSrc p g             = g { _anySrc             = Just p } -- 83.
instance AttrHasSrcDoc          AnyAttr where attrSetSrcDoc p g          = g { _anySrcDoc          = Just p } -- 84.
instance AttrHasSrcLang         AnyAttr where attrSetSrcLang p g         = g { _anySrcLang         = Just p } -- 85.
instance AttrHasSrcSet          AnyAttr where attrSetSrcSet p g          = g { _anySrcSet          = Just p } -- 86.
instance AttrHasSrcSetW         AnyAttr where attrSetSrcSetW p g         = g { _anySrcSetW         = Just p } -- 87.
instance AttrHasSrcSetP         AnyAttr where attrSetSrcSetP p g         = g { _anySrcSetP         = Just p } -- 88.
instance AttrHasStart           AnyAttr where attrSetStart p g           = g { _anyStart           = Just p } -- 89.
instance AttrHasStep            AnyAttr where attrSetStep p g            = g { _anyStep            = Just p } -- 90.
instance AttrHasTarget          AnyAttr where attrSetTarget p g          = g { _anyTarget          = Just p } -- 91.
instance AttrHasTypeMustMatch   AnyAttr where attrSetTypeMustMatch p g   = g { _anyTypeMustMatch   = Just p } -- 93.
instance AttrHasUseMap          AnyAttr where attrSetUseMap p g          = g { _anyUseMap          = Just p } -- 94.
instance AttrHasValueText           AnyAttr where attrSetValueText p g           = g { _anyValueText           = Just p } -- 95.
instance AttrHasValueNumber     AnyAttr where attrSetValueNumber p g     = g { _anyValueNumber     = Just p } -- 96.
instance AttrHasValueOlLi       AnyAttr where attrSetValueOlLi   p g     = g { _anyValueOlLi       = Just p } -- 97.
instance AttrHasWidth           AnyAttr where attrSetWidth p g           = g { _anyWidth           = Just p } -- 98.
instance AttrHasWrap            AnyAttr where attrSetWrap p g            = g { _anyWrap            = Just p } -- 99.


-- | A shorthand for def :: AnyAttr
aDef :: AnyAttr
aDef = def

