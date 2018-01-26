{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.TfootComb
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TfootComb

-}

module Reflex.Dom.HTML5.Component.Table.TfootComb where

import           Control.Monad.Fix
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core


import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Elements

-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table foot section.
-- Function 'mkTableV' can use TfootFuns when building a table.
data TfootFuns t m = TfootFuns
    { _tfootADEs :: CommonADEfuns t m Text -- ^ See 'CommonADEfuns'
    , _tfootCombFun :: TfootFuns t m
              → ActElem
              → Text
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ _tfootCombFun, a function that combines the above fours functions.
    -- I.e. 'tfootComb'
    , _tfootTdAttr :: ActElem → Dynamic t ETd
    -- ^ _tfootTdAttr, i.e. a function that sets the td-attributes.
    , _tfootTrAttr :: ActElem → Dynamic t ETr
    -- ^ _tfootTrAttr, i.e. a function that sets the tr-attributes.
    }

-- makeLenses ''TfootFuns

----------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table.
-- This function takes a data type 'TfootFuns' having functions that can
--   - define behaviour on mouse (and other) events
--   - define, how to draw the contents of a cell of a table
--   - define, what and how to return
tfootComb ∷ forall t m. (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                        , PostBuild t m, DomBuilder t m,  MonadJSM m)
          ⇒ TfootFuns t m
          → ActElem                 -- ^ "me" (to be able to diff to eTB src)
          → Text                    -- ^ input content (dwawEl-fun uses)
          → Event t (TableEvent t)  -- ^ an event somewhere on the table (eTB)
          → TableState t
          → m (TableEvent t)        -- ^ events we want this cell to return
tfootComb tfootFuns me a eTB tblSt = mdo
    -- _actSt, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that "listening" and "activation" should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu ._tfootADEs) tfootFuns eTB tblSt
        $ (_actSt . _tfootADEs ) tfootFuns me
    e <- (_drawEl. _tfootADEs) tfootFuns me a actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- td-element (that is, a "cell") inside tfoot row (inside tr-elements).
    (_cellEv . _tfootADEs) tfootFuns actS e
    -- _cellEv, i.e. 'cellEvF' - what events we are listening inside
    -- td-elements. Note that using other events here means that many
    -- other functions in this module should be changed or given alternative
    -- implementations.

----------------------------------------------------------------------------------

-- | Like 'tfootComb' but doesn't return any events (just default that is
-- full of never's).
tfootCombLstner ∷ forall t m. (Reflex t, MonadHold t m, TriggerEvent t m
                              , MonadFix m
                              , PostBuild t m, DomBuilder t m,  MonadJSM m)
              ⇒ TfootFuns t m
              → ActElem                 -- ^ "me" (to be able to diff to eTB src)
              → Text                    -- ^ input content (dwawEl-fun uses)
              → Event t (TableEvent t)  -- ^ an event somewhere on the table
              → TableState t
              → m (TableEvent t)        -- ^ This returns default
tfootCombLstner tfootFuns me a eTB tblSt = mdo
    actS <- (_actFu . _tfootADEs) tfootFuns eTB tblSt
        $ (_actSt . _tfootADEs) tfootFuns me
    _ <- (_drawEl . _tfootADEs) tfootFuns me a actS
    pure def


--------------------------------------------------------------------------------

-- Hmm using this isn't that obvious. Some class constraints missing?
-- (Try changing the tdElm. Even changing it to tdElm again doesn't seem to work.)
defaultTfootFuns ∷ forall t m.  (Reflex t, MonadHold t m, TriggerEvent t m
                                , MonadFix m, DomBuilder t m, PostBuild t m
                                , MonadJSM m
                                , DomBuilderSpace m ~ GhcjsDomSpace)
                 ⇒ TfootFuns t m
defaultTfootFuns =
    TfootFuns defaultCommonADE tfootComb (const def) (const def)

--------------------------------------------------------------------------------
