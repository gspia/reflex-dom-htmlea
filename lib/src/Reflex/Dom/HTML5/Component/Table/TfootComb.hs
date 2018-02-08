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

Function 'tfootComb' makes a cell on a table with the associated behavior, given
with 'TfootFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.TfootComb
    ( TfootFuns (..)
    , tfootADEs
    , tfootCombFun
    , tfootTdAttr
    , tfootTrAttr
    , tfootComb
    , tfootCombLstner
    , defTfootAttrF
    -- , defaultTfootFuns
    ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Default
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core


import           Reflex.Dom.HTML5.Attrs
import           Reflex.Dom.HTML5.Component.Table.StateInfo
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
              → Dynamic t (Maybe [ActiveState t])
              → ActiveState t
              → Text
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ A function that combines the above fours functions.
    -- I.e. 'tfootComb'.
    , _tfootTdAttr :: ActiveState t → Dynamic t ETd
    -- ^ A function that sets the td-attributes.
    , _tfootTrAttr :: ActElem → Dynamic t ETr
    -- ^ A function that sets the tr-attributes.
    }

-- | A lens.
tfootADEs :: Lens' (TfootFuns t m) (CommonADEfuns t m Text)
tfootADEs f (TfootFuns f1 f2 f3 f4) = fmap (\g -> TfootFuns g f2 f3 f4) (f f1)

-- | A lens.
tfootCombFun :: Lens' (TfootFuns t m )
  (TfootFuns t m  → Dynamic t (Maybe [ActiveState t])
  → ActiveState t
  → Text → Event t (TableEvent t) → TableState t → m (TableEvent t))
tfootCombFun f (TfootFuns f1 f2 f3 f4) = fmap (\g -> TfootFuns f1 g f3 f4) (f f2)

-- | A lens.
tfootTdAttr :: Lens' (TfootFuns t m ) (ActiveState t -> Dynamic t ETd)
tfootTdAttr f (TfootFuns f1 f2 f3 f4) = fmap (\g -> TfootFuns f1 f2 g f4) (f f3)

-- | A lens.
tfootTrAttr :: Lens' (TfootFuns t m ) (ActElem -> Dynamic t ETr)
tfootTrAttr f (TfootFuns f1 f2 f3 f4) = fmap (TfootFuns f1 f2 f3) (f f4)

-- makeLenses ''TfootFuns

----------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table.
-- This function takes a data type 'TfootFuns' having functions that can
--
--     * define behaviour on mouse (and other) events
--     * define, how to draw the contents of a cell of a table
--     * define, what and how to return
tfootComb ∷ forall t m. (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                        , PostBuild t m, DomBuilder t m,  MonadJSM m)
          ⇒ TfootFuns t m
          → Dynamic t (Maybe [ActiveState t])
          → ActiveState t
          -- ^  This tfoot-element (to be able to differentiate to eTB
          -- event sources).
          → Text                    -- ^ Input content (dwawEl-fun uses).
          → Event t (TableEvent t)  -- ^ An event somewhere on the table (eTB).
          → TableState t
          → m (TableEvent t)        -- ^ Events we want this cell to return.
tfootComb tfootFuns mae me a eTB tblSt = mdo
    -- _actListen, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that listening and activation should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu ._tfootADEs) tfootFuns mae eTB tblSt
        $ (_actListen . _tfootADEs ) tfootFuns me
    e <- (_drawEl. _tfootADEs) tfootFuns a actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- td-element (that is, a cell) inside tfoot row (inside tr-elements).
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
              → Dynamic t (Maybe [ActiveState t])
              → ActiveState t
              -- ^ Tis tfoot-elements (to be able to diff to eTB src)
              → Text                    -- ^ input content (dwawEl-fun uses)
              → Event t (TableEvent t)  -- ^ an event somewhere on the table
              → TableState t
              → m (TableEvent t)        -- ^ This returns default
tfootCombLstner tfootFuns mae me a eTB tblSt = mdo
    actS <- (_actFu . _tfootADEs) tfootFuns mae eTB tblSt
        $ (_actListen . _tfootADEs) tfootFuns me
    _ <- (_drawEl . _tfootADEs) tfootFuns a actS
    pure def


--------------------------------------------------------------------------------

instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
         , PostBuild t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Default (TfootFuns t m)
  where
    def = TfootFuns (def :: CommonADEfuns t m Text)
                    tfootComb (const def) (const def)

-- Hmm using this isn't that obvious. Some class constraints missing?
-- (Try changing the tdElm. Even changing it to tdElm again doesn't seem to work.)
-- defaultTfootFuns ∷ forall t m.  (Reflex t, MonadHold t m, TriggerEvent t m
--                                 , MonadFix m, DomBuilder t m, PostBuild t m
--                                 , MonadJSM m
--                                 , DomBuilderSpace m ~ GhcjsDomSpace)
--                  ⇒ TfootFuns t m
-- defaultTfootFuns =
--     TfootFuns def tfootComb (const def) (const def)
    -- TfootFuns defaultCommonADE tfootComb (const def) (const def)

--------------------------------------------------------------------------------


-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defTfootAttrF :: forall t. Reflex t => ActiveState t → Dynamic t ETfoot
defTfootAttrF ast =
    let dA = view activeStateActive ast
        dAv = view activeStateActivable ast
        dAGl = view activeStateActiveGl ast
        dNAGl = view activeStateNotActiveGl ast
        dNAvGl = view activeStateNotActivableGl ast
     in (\bA bAv gA gNa gNav -> attrSetGlobals
         (if bAv
            then if bA
                then gA
                else gNa
            else gNav
         ) def
        ) <$> dA <*> dAv <*> dAGl <*> dNAGl <*> dNAvGl


--------------------------------------------------------------------------------
