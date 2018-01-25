{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.ThComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TableCommon

-}

module Reflex.Dom.HTML5.Component.Table.ThComb where

import           Control.Monad.Fix
import           Data.Text                      (Text)
import           Reflex
import           Reflex.Dom.Core
import           Language.Javascript.JSaddle


import           Reflex.Dom.HTML5.Elements
import           Reflex.Dom.HTML5.Component.Table.Common


-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'mkTableV' can use ThFuns when building a table.
data ThFuns t m = ThFuns
    { _thfADEs :: CommonADEfuns t m Text -- ^ See 'CommonADEfuns'
    , _thfCombFun :: ThFuns t m
              → ActElem
              → Text
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ _thfCombFun, a function that combines the above fours functions,
    -- i.e. 'thComb
    , _thfThAttr :: ActElem → Dynamic t ETh
    -- ^ _thfThAttr, i.e. a function that sets the td-attributes.
    , _thfTrAttr :: ActElem → Dynamic t ETr
    -- ^ _thfTrAttr, i.e. a function that sets the tr-attributes.
    }

-- makeLenses ''ThFuns

--------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table header.
-- This function takes a data type 'ThFuns' having functions that can
--   - define behaviour on mouse (and other) events
--   - define, how to draw the contents of a cell of a table
--   - define, what and how to return
thComb ∷ forall t m . (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                      , PostBuild t m, DomBuilder t m,  MonadJSM m)
      ⇒ ThFuns t m
      → ActElem                 -- ^ "me" (to be able to diff to eTB src)
      → Text                    -- ^ input content (dwawEl-fun uses)
      → Event t (TableEvent t)  -- ^ an event somewhere on the table (eTB)
      → TableState t
      → m (TableEvent t)        -- ^ events we want this cell to return
thComb thFuns me txt eTB tblSt = mdo
    -- _actSt, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that "listening" and "activation" should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu . _thfADEs) thFuns eTB tblSt $ (_actSt . _thfADEs) thFuns me
    e <- (_drawEl . _thfADEs) thFuns me txt actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- th-element (that is, a "column header").
    (_cellEv . _thfADEs) thFuns actS e
    -- _cellEv, i.e. 'cellEv' - what events we are listening inside
    -- th-elements.

--------------------------------------------------------------------------------

defaultThFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                 , DomBuilder t m, PostBuild t m, MonadJSM m
                 , DomBuilderSpace m ~ GhcjsDomSpace)
              ⇒ ThFuns t m
defaultThFuns = ThFuns defaultCommonADE thComb (const def) (const def)

