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

= ThComb

Function 'thComb' makes a cell on a table with the associated behavior, given
with 'ThFuns'.

-}

module Reflex.Dom.HTML5.Component.Table.ThComb
    ( ThFuns (..)
    , thfADEs
    , thfCombFun
    , thfThAttr
    , thfTrAttr
    , thComb
    , defThAttrF
    ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Default
import           Data.Text                      (Text)
import           Reflex
import           Reflex.Dom.Core
import           Language.Javascript.JSaddle


import           Reflex.Dom.HTML5.Attrs
import           Reflex.Dom.HTML5.Component.Table.StateInfo
import           Reflex.Dom.HTML5.Elements
import           Reflex.Dom.HTML5.Component.Table.Common


-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | Declare the look and feel and behaviour of a table.
-- Function 'Reflex.Dom.HTML5.Component.Table.TableV.mkTableV'
-- can use ThFuns when building a table.
data ThFuns t m = ThFuns
    { _thfADEs :: CommonADEfuns t m Text -- ^ See 'CommonADEfuns'
    , _thfCombFun :: ThFuns t m
              → Dynamic t (Maybe [ActiveState t])
              → ActiveState t
              → Text
              → Event t (TableEvent t)
              → TableState t
              → m (TableEvent t)
    -- ^ A function that combines the above fours functions,
    -- i.e. 'thComb'.
    , _thfThAttr :: ActiveState t → Dynamic t ETh
    -- ^ A function that sets the td-attributes.
    , _thfTrAttr :: ActElem → Dynamic t ETr
    -- ^ A function that sets the tr-attributes.
    }

thfADEs :: Lens' (ThFuns t m) (CommonADEfuns t m Text)
thfADEs f (ThFuns f1 f2 f3 f4) = fmap (\g -> ThFuns g f2 f3 f4) (f f1)
thfCombFun :: Lens' (ThFuns t m )
  (ThFuns t m  → Dynamic t (Maybe [ActiveState t])
  → ActiveState t
  → Text → Event t (TableEvent t) → TableState t → m (TableEvent t))
thfCombFun f (ThFuns f1 f2 f3 f4) = fmap (\g -> ThFuns f1 g f3 f4) (f f2)
thfThAttr :: Lens' (ThFuns t m ) (ActiveState t -> Dynamic t ETh)
thfThAttr f (ThFuns f1 f2 f3 f4) = fmap (\g -> ThFuns f1 f2 g f4) (f f3)
thfTrAttr :: Lens' (ThFuns t m ) (ActElem -> Dynamic t ETr)
thfTrAttr f (ThFuns f1 f2 f3 f4) = fmap (ThFuns f1 f2 f3) (f f4)

-- makeLenses ''ThFuns

--------------------------------------------------------------------------------

-- | A function that is used to build up a single cell in a table header.
-- This function takes a data type 'ThFuns' having functions that can
--
--     * define behaviour on mouse (and other) events
--     * define, how to draw the contents of a cell of a table
--     * define, what and how to return
thComb ∷ forall t m . (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                      , PostBuild t m, DomBuilder t m,  MonadJSM m)
      ⇒ ThFuns t m
      → Dynamic t (Maybe [ActiveState t])
      → ActiveState t
      -- ^ This th-element (to be able to difference to eTB event sources).
      → Text                    -- ^ Input content (dwawEl-fun uses).
      → Event t (TableEvent t)  -- ^ An event somewhere on the table (eTB).
      → TableState t
      → m (TableEvent t)        -- ^ Events we want this cell to return.
thComb thFuns mae me txt eTB tblSt = mdo
    -- _actListen, i.e. listenMe - what cells we are listening and how
    -- _actFu, i.e. actMU or actMD - how to activate the cells
    -- Note that listening and activation should work together.
    -- Together, they return a new ActiveState that can be used when
    -- drawing below.
    actS <- (_actFu . _thfADEs) thFuns mae eTB tblSt
                $ (_actListen . _thfADEs) thFuns me
    e <- (_drawEl . _thfADEs) thFuns txt actS
    -- _drawEl, i.e. 'drawDiv' - how to draw / make the contents of a
    -- th-element (that is, a column header).
    (_cellEv . _thfADEs) thFuns actS e
    -- _cellEv, i.e. 'cellEv' - what events we are listening inside
    -- th-elements.


--------------------------------------------------------------------------------

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defThAttrF :: forall t. Reflex t => ActiveState t → Dynamic t ETh
defThAttrF ast =
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

instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
         , PostBuild t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Default (ThFuns t m)
  where
    def = ThFuns def thComb defThAttrF (const def)



