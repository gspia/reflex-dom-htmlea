{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}



{-|
Module      : Reflex.Dom.HTML5.Attrs.Common
Description : Common definitions for attributes.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Common

This module contains some common definitions.

-}

module Reflex.Dom.HTML5.Attrs.Common
  ( Attr
  , AttrMap (attrMap)
  , AttrHasCustom (attrSetCustom)
  , addCustom
  , URL(URL)
  ) where

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)

type Attr = Map.Map Text Text


------------------------------------------------------------------------------
-- | A type class for converting data types into appropriate HtmlAttrs
-- attribute map that can be used with Reflex-Dom -elements.
class AttrMap a where
  attrMap :: a -> Attr

------------------------------------------------------------------------------
-- | Passthrough instance for Either.
instance (AttrMap a, AttrMap b) => AttrMap (Either a b) where
  attrMap (Left a) = attrMap a
  attrMap (Right b) = attrMap b


class AttrHasCustom a where
  attrSetCustom :: Attr -> a -> a

------------------------------------------------------------------------------
-- | Helper function for adding an attribute-map to a custom map.
addCustom :: Attr -> Maybe Attr -> Maybe Attr
addCustom attrs Nothing = Just attrs
addCustom attrs (Just a) = Just $ attrs <> a


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Wish list: use types to ensure that URL is valid.
newtype URL = URL Text
  deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

