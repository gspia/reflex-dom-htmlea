{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Common.RfpUtils
Description : Helpers
Copyright   : (c) gspia 2018
License     : BSD
Maintainer  : gspia

= RfpUtils

This module defines random helpers.

-}

module Reflex.Dom.HTML5.Component.Common.RfpUtils
    ( switcherDyn
    ) where


import           Control.Monad (join)
import           Reflex        (Reflex, MonadHold, Dynamic, Event, holdDyn)

--------------------------------------------------------------------------------

-- | This is basically a holdDyn, made along the lines of "switcher".
switcherDyn ∷ forall a t m. (Reflex t, MonadHold t m)
           ⇒ Dynamic t a → Event t (Dynamic t a) → m (Dynamic t a)
switcherDyn d ed = pure . join =<< holdDyn d ed


--------------------------------------------------------------------------------

-- Hmm probably will be removed
-- switchDyn ∷ forall t a. Reflex t ⇒ Dynamic t (Event t a) → Event t a
-- switchDyn d = switch (current d)

