{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- NOT IN USE YET
--

{-|
Module      : Reflex.Dom.HTML5.Elements.Class
Description : Class definitions
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Class

NOT IN USE YET

-}

module Reflex.Dom.HTML5.Elements.Class where

import           Data.Text (Text)
-- import qualified Data.Text as T

-- class AnyContent a
-- instance AnyContent Text
-- instance AnyContent ()

-- class AnyContent a => FlowContent a

class FlowContent a

instance FlowContent Text
instance FlowContent ()

{-
   The following elements are flow content. Instances are provided in the
   modules containing the corresponding definitions.

   a abbr address area (if it is a descendant of a map element) article aside
   audio b bdi bdo blockquote br button canvas cite code data datalist del
   details dfn div dl em embed fieldset figure footer form h1 h2 h3 h4 h5 h6
   header hr i iframe img input ins kbd keygen label main map mark math menu
   meter nav noscript object ol output p picture pre progress q ruby s samp
   script section select small span strong sub sup svg table template textarea
   time u ul var video wbr Text
 -}
