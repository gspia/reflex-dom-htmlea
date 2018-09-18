{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Common.CompEvent
Description : Helpers to build components.
Copyright   : (c) gspia 2017
License     : BSD3
Maintainer  : gspia

= Common

This module defines some of the functionality used with the components.

Idea shortly, if the defaults are not providing what is needed:

    * Try different cell drawing function, like 'drawDivContent',
      write your own (take one as a starting point).
      Note that the drawing-function is most likely closely tied
      to the input the 'mkTable' is given.
    * Try different listen-functions, like 'listenMe',
      write your own (take one as a starting point).
    * Try different act-functions to handle state management,
      like 'actMU', write you own (take one as a starting point).
    * And it is easy to write the function that uses listening,
      drawing and other functions, see 'elemEvF' (self made can add to
      existing elemEvF or be made to work totally in other way).

You can also override the functions that combine state management,
drawing and event listening. The function that takes the input vectors
and table definitions, is reasonable short as well (so that it is easy to
write a new one if the above ones don't give enough flexibility).

The combining functions are defined in the modules (for tables):

    * "TdComb"
    * "ThComb"
    * "TfootComb"

and (for trees)

    * "RootCombNS"
    * "LevelNCombNS"


TODO: there is a bit duplication (state management, switching methods).

-}

module Reflex.Dom.HTML5.Component.Common.CompEvent
    (
    -- * Data structures
    --
    -- ** Common function used to listen, draw and handle some of the events
    CommonADEfuns (..)
    , adeListen
    , adeActivate
    , adeDraw
    , adeEvent
    , defCommonADEfuns
    , silentPlainADE
    , nothingADE
    , adeCombFun
    , adeCombLstner
    , adeCombSilentPlain

    -- ** CompEvent and state management for a table
    , CompEvent (..)
    , defCompEvent
    , ceMe
    , ceMDown
    , ceMUp
    , ceMEnter
    , ceRetval
    , CompState (..)
    , defCompState
    , csCompEvent
    , csDynMInsideBody
    , csDynMOutsideBody
    , csDynDOn
    , csDynUOn
    , csDynDPress
    , csDynURelease
    , csDynEnter
    , csDynUpLDownR
    , CompOtherEvs (..)
    , defCompOtherEvs
    , eMEnterBody
    , eMLeaveBody
    , updateComponentState  -- hide possibly TODO
    -- , coincOnCompEvent     -- hide possibly TODO

    -- * A drawing function
    , drawNothing

    -- * State management
    --
    -- ** State management for the normal usage
    , actMD
    , actMU
    , actMDOnListened
    , actMUOnListened
    , actMUFollowListened
    , act1MUSwitch
    , act1MUSwitchAbsorb
    , actSwitchMU
    , actNone
    , actAreaMDUsel
    , actSwitchMDUsel
    -- | See examples on how to use primitives to construct tables to see,
    -- how to use the following three functions.
    , actUniqPrim
    , actSwitchPrim
    , actGroupsPrim

    -- * Event helpers
    , compEventsEnLe
    , compEventsNone
    , compEvFiring
    , elemEvF
    , elemEvF2
    , elemNoEvs
    , mouseInEvent
    , mouseOutEvent

    ) where


import           Control.Arrow                  ((***))
import           Control.Lens
import           Control.Monad                  (join)
import           Control.Monad.Fix
-- import           Data.Semigroup                 ((<>))
import qualified Data.Set                       as Set
-- import           Data.Text                      (Text)
-- import qualified Data.Text                      as T
import qualified GHCJS.DOM.EventM               as DOM
import qualified GHCJS.DOM.GlobalEventHandlers  as DOM
import qualified GHCJS.DOM.HTMLElement          as DOM
import           Reflex
-- import           Reflex.Class (switchHold)
import           Reflex.Dom.Core
-- import qualified GHCJS.DOM.MouseEvent      as DOM
import qualified GHCJS.DOM.Types                as DOM
import           Language.Javascript.JSaddle


-- import           Reflex.Dom.HTML5.Component.Table.ActElem
import           Reflex.Dom.HTML5.Component.Common.StateInfo
import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E


--------------------------------------------------------------------------------



-- | Common functions interface for tbody (table cell) and headers.
-- This way headers can have their own functions compared to the tbody.
--
-- About naming:
--
--     * A is for activity (a kind of state)
--     * D is for draw
--     * E is for event
--
-- Type parameters:
--
--     * @t@ is a Reflex convention for time.
--     * @m@ is a Reflex convention for associated Monads.
--     * @a@ is an input type of the data given to the component..
--     * @e@ is an element id.
--     * @r@ is a return type.
--
-- Drawing can take @a@, @e@ and @r@ and it can return @e@ and @r@.
-- Other functions can use @e@ and @r@.
data CommonADEfuns t m a e r = CommonADEfuns
    { _adeListen ∷ ActiveState t e r → ActiveState t e r
    -- ^ I.e. 'listenMe' - what cells we are listening and how.
    , _adeActivate ∷ Dynamic t (Maybe [ActiveState t e r])
             → Event t (CompEvent t e r)
             → CompState t e r
             → ActiveState t e r → m (ActiveState t e r)
    -- ^ I.e. 'actMU' or 'actMD' - how to activate the cells.
    , _adeDraw ∷ Dynamic t a → ActiveState t e r
              → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
    -- ^ I.e. 'drawDivAECnt', how to draw a cell.
    , _adeEvent ∷ ActiveState t e r → Element EventResult (DomBuilderSpace m) t
              → m (CompEvent t e r)
    -- ^ I.e. 'elemEvF'. How to produce an event to be delivered further
    -- to the caller based on user-actions made on component.
    }

-- | A lens.
adeListen ∷ Lens' (CommonADEfuns t m a e r) (ActiveState t e r
                                         → ActiveState t e r)
adeListen f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g → CommonADEfuns g f2 f3 f4) (f f1)

-- | A lens.
adeActivate ∷ Lens' (CommonADEfuns t m a e r)
    (Dynamic t (Maybe [ActiveState t e r])
    → Event t (CompEvent t e r)
    → CompState t e r
    → ActiveState t e r
    → m (ActiveState t e r))
adeActivate f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g → CommonADEfuns f1 g f3 f4) (f f2)

-- | A lens.
adeDraw ∷ Lens (CommonADEfuns t m a e r) (CommonADEfuns t m b e r)
    (Dynamic t a → ActiveState t e r → m
        (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
    (Dynamic t b → ActiveState t e r → m
        (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
adeDraw f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g → CommonADEfuns f1 f2 g f4) (f f3)

-- | A lens.
adeEvent ∷ Lens' (CommonADEfuns t m a e r)
    (ActiveState t e r
    → Element EventResult (DomBuilderSpace m) t → m (CompEvent t e r))
adeEvent f (CommonADEfuns f1 f2 f3 f4) = fmap (CommonADEfuns f1 f2 f3) (f f4)


-- | A function that combines the four functions given in the
-- CommonADEfuns -structure.
--
-- This function takes a data type 'CommonADEfuns' having functions that can
--
--     * define behaviour on mouse (and other) events
--     * define, how to draw the contents of a cell of a tree
--     * define, what and how to return
--
-- To see, how this is used with tables, take a look of 'mkRow' or other
-- mk-functions in the same module. For trees, see 'mkTree' and 'mkRoot'.
adeCombFun ∷ forall t m a e r. (Reflex t, MonadHold t m, TriggerEvent t m
                       , PostBuild t m, DomBuilder t m,  MonadJSM m
                       , ActSelem e)
       ⇒ CommonADEfuns t m a e r
       -- Functions: listening, activity management, drawing and event capturing.
       → Dynamic t (Maybe [ActiveState t e r])
       -- ^ Settings from the component conf. (Like default or initial values.)
       -- These are passed to the activate-function, like 'actMU'.
       → ActiveState t e r
       -- ^ This element (to be able to differentiate against eTB event sources)
       -- we are making (e.g. li or td).
       → Dynamic t a
       -- ^ Input content (adeDraw-fun uses).
       → Event t (CompEvent t e r)
       -- ^ An event somewhere on the component (table or tree).
       → CompState t e r
       -- ^ Some activation functions need 'CompState'-information.
       -- See e.g. 'actAreaSelCommon'.
       → m (CompEvent t e r)
adeCombFun funs mae me dA eTB cmpSt = do
    -- Notes:
    -- _adeListen, i.e. 'listenMe' - what nodes we are listening and how
    -- _adeActivate, i.e. 'actMU' or 'actMD' - how to activate the nodes
    -- Note that listening and activation should work together.
    -- Together, they return a new 'ActiveState' that can be used when
    -- drawing below.
    actS ∷ ActiveState t e r
        ← _adeActivate funs mae eTB cmpSt $ _adeListen funs me
    -- outputActiveState me "in tdco, me"
    -- outputActiveState actS "in tdco, actS"
    (e,dRetVal) ∷ (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
        ← _adeDraw funs dA actS
    let actS2 = actS { _activeStateRetVal = dRetVal }
    -- _adeDraw, i.e. 'drawDiv' - how to draw / make the contents of a
    -- li-element (that is, a leaf-node) inside nav tree (ul-elements).
    _adeEvent funs actS2 e
    -- _adeEvent, i.e. 'elemEvF' - what events we are listening inside
    -- li-elements. Note that using other events here means that many
    -- other functions in this module should be changed or given alternative
    -- implementations.

----------------------------------------------------------------------------------

-- | Like 'adeCombFun' but doesn't return any events (just default that is
-- full of never's). This just calls the activation, listening,
-- and drawing functions.
adeCombLstner ∷ forall t m a e r. (Reflex t, MonadHold t m, TriggerEvent t m
                                , PostBuild t m, DomBuilder t m,  MonadJSM m
                                , ActSelem e)
      ⇒ CommonADEfuns t m a e r
      -- ^ Functions: listening, activity management, drawing and event capturing.
      → Dynamic t (Maybe [ActiveState t e r])
      -- ^ Settings from the component conf. (Like default or initial values.)
      -- These are passed to the activate-function, like 'actMU'.
      → ActiveState t e r
      -- ^ This element (to be able to differentiate against eTB event sources)
      -- we are making (e.g. li or td).
      → Dynamic t a
      -- ^ Input content (adeDraw-fun uses).
      → Event t (CompEvent t e r)
      -- ^ An event somewhere on the component (table or tree).
      → CompState t e r
       -- ^ Some activation functions need CompState-information.
       -- See e.g. 'actAreaSelCommon'.
      → m (CompEvent t e r)
      -- ^ This returns default.
adeCombLstner funs mae me dA eTB cmpSt = do
    actS ← _adeActivate funs mae eTB cmpSt $ _adeListen funs me
    (e,dRetVal) ← _adeDraw funs dA actS
    let actS2 = actS { _activeStateRetVal = dRetVal }
    e2 ← _adeEvent funs actS2 e
    -- pure (defCompEvent & set ceMe me)
    pure (e2 & set ceMDown never & set ceMUp never & set ceMEnter never)

----------------------------------------------------------------------------------

-- | Like 'adeCombFun' but doesn't return any events (just default that is
-- full of never's) nor doesn't handle states. This just calls the
-- drawing function.
adeCombSilentPlain ∷ forall t m a e r. (Reflex t, MonadHold t m, TriggerEvent t m
                                     , PostBuild t m, DomBuilder t m,  MonadJSM m
                                     , ActSelem e, ActSretval r)
      ⇒ CommonADEfuns t m a e r
      -- Functions: listening, activity management, drawing and event capturing.
      → Dynamic t (Maybe [ActiveState t e r])
      -- ^ Settings from the component conf. (Like default or initial values.)
      -- These are passed to the activate-function, like 'actMU'.
      → ActiveState t e r
      -- ^ This element (to be able to differentiate against eTB event sources)
      -- we are making (e.g. li or td).
      → Dynamic t a
      -- ^ Input content (adeDraw-fun uses).
      → Event t (CompEvent t e r)
      -- ^ An event somewhere on the component (table or tree).
      → CompState t e r
       -- ^ Some activation functions need CompState-information.
       -- See e.g. 'actAreaSelCommon'.
      → m (CompEvent t e r)
      -- ^ This returns default.
adeCombSilentPlain funs _mae _me dA _eTB _cmpSt = do
    _ ← _adeDraw funs dA defActiveState
    pure defCompEvent


--------------------------------------------------------------------------------

-- | This draws nothing and is mainly used as a default value in
-- 'defCommonADEfuns', 'silentPlainADE' and 'nothingADE'.
--
-- See "DrawFuns" for useable drawing functions and pick one of them.
drawNothing ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                               -- , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                               , DomBuilderSpace m ~ GhcjsDomSpace
                               , ActSelem e, ActSretval r)
                ⇒ Dynamic t a → ActiveState t e r
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawNothing _ _ = do
    -- Hmm, TODO TODO TODO. Any other way to make the return type that is
    -- not going to fire ever?
    (e,_) ← E.div' (A.hidden E.defDiv) blank
    -- text "drawNothing"
    -- Maybe change everything to use:
    -- EventSelector
    -- EventSelector (const never)
    -- select Click (_element_events e)
    -- Note that elemEvF uses preventDefault, which needs the element.
    pure (e, defRetval)

--------------------------------------------------------------------------------

-- | Default value for CommonADEfuns.
-- Note that this does not have a Show-instance.
-- Thus, to show something, the default drawing has to be overridden.
defCommonADEfuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                   -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                   , DomBuilder t m, PostBuild t m, MonadJSM m
                   , DomBuilderSpace m ~ GhcjsDomSpace
                   , ActSelem e, ActSretval r, Ord e)
                 ⇒ CommonADEfuns  t m a e r
defCommonADEfuns = CommonADEfuns listenMe actMU drawNothing elemEvF
-- defCommonADEfuns = CommonADEfuns listenMe actMU drawDivContentS elemEvF

--------------------------------------------------------------------------------

-- | No cell is listening for events and no cell is having activity management.
-- Note that this does not have a Show-instance.
-- Thus, to show something, the default drawing has to be overridden.
silentPlainADE ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                  , DomBuilder t m, PostBuild t m, MonadJSM m
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSelem e, ActSretval r)
               ⇒ CommonADEfuns t m a e r
silentPlainADE = CommonADEfuns listenNoOne actNone drawNothing elemNoEvs
-- silentPlainADE = CommonADEfuns listenNoOne actNone drawDivContentS elemNoEvs


-- | No cell is listening for events and no cell is having activity management.
-- Draws nothing.
nothingADE ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  -- , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                  , DomBuilder t m, PostBuild t m, MonadJSM m
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSelem e, ActSretval r)
               ⇒ CommonADEfuns t m a e r
nothingADE = CommonADEfuns listenNoOne actNone drawNothing elemNoEvs

--------------------------------------------------------------------------------


-- | CompEvent-data collects events into one place.
-- This is part the result that table-component returns (see 'CompState').
-- It tells the cell and event that is happening on the cell.
data CompEvent t e r = CompEvent
    { _ceMe     ∷ ActiveState t e r
    -- ^ "Name of the cell", a bit duplication as the same information is
    -- contained in the events below.
    , _ceMDown  ∷ Event t (ActiveState t e r)
    -- ^ MouseDown event firing on component @e@.
    , _ceMUp    ∷ Event t (ActiveState t e r)
    -- ^ MouseUp event firing on component @e@.
    , _ceMEnter ∷ Event t (ActiveState t e r)
    -- ^ MouseEnter event firing on component @e@.
    , _ceRetval ∷ Event t r
    -- ^ The widget producing r fires an event.
    }

    -- , _ceOther ∷ Event t (ActiveState t e r)
    -- Above events but reserved for listeners.


-- | A lens.
ceMe ∷ Lens' (CompEvent t e r) (ActiveState t e r)
ceMe f (CompEvent f1 f2 f3 f4 f5) = fmap (\g → CompEvent g f2 f3 f4 f5) (f f1)

-- | A lens.
ceMDown ∷ Lens' (CompEvent t e r) (Event t (ActiveState t e r))
ceMDown f (CompEvent f1 f2 f3 f4 f5) = fmap (\g → CompEvent f1 g f3 f4 f5) (f f2)

-- | A lens.
ceMUp ∷ Lens' (CompEvent t e r) (Event t (ActiveState t e r))
ceMUp f (CompEvent f1 f2 f3 f4 f5) = fmap (\g → CompEvent f1 f2 g f4 f5) (f f3)

-- | A lens.
ceMEnter ∷ Lens' (CompEvent t e r) (Event t (ActiveState t e r))
ceMEnter f (CompEvent f1 f2 f3 f4 f5) = fmap (\g → CompEvent f1 f2 f3 g f5) (f f4)

-- | A lens.
ceRetval ∷ Lens' (CompEvent t e r) (Event t r)
ceRetval f (CompEvent f1 f2 f3 f4 f5) = fmap (CompEvent f1 f2 f3 f4) (f f5)

-- ceOther ∷ Lens' (CompEvent t e r) (Event t (ActiveState t e r))
-- ceOther f (CompEvent f1 f2 f3 f4 f5) = fmap (CompEvent f1 f2 f3 f4) (f f5)

-- | A default value for 'CompEvent'. This has default 'ActiveState',
-- that is, 'defActiveState', and the events are initialized to 'never'.
defCompEvent ∷ (Reflex t, ActSelem e, ActSretval r) ⇒ CompEvent t e r
defCompEvent = CompEvent defActiveState never never never never


--------------------------------------------------------------------------------


-- | CompState-data is the result the component (table/tree) gives outside.
data CompState t e r = CompState
    { _csCompEvent    ∷ Event t (CompEvent t e r) -- ^ See 'CompEvent'
    , _csDynMInsideBody  ∷ Dynamic t Bool -- ^ Fires when mouse enters the comp.
    , _csDynMOutsideBody ∷ Dynamic t Bool -- ^ Fires when mouse leaves the comp.
    , _csDynDOn          ∷ Dynamic t Bool -- ^ Mouse button state, true if down.
    , _csDynUOn          ∷ Dynamic t Bool -- ^ Mouse button state, true if up.
    , _csDynDPress       ∷ Dynamic t (ActiveState t e r)
    -- ^ Last ActiveState (containing component @e@) where MouseDown occurred.
    , _csDynURelease     ∷ Dynamic t (ActiveState t e r)
    -- ^ Last ActiveState (containing component @e@) where MouseUp occurred.
    , _csDynEnter        ∷ Dynamic t (ActiveState t e r)
    -- ^ Last ActiveState (containing component @e@) where enter event occurred.
    , _csDynUpLDownR     ∷ Dynamic t (ActiveState t e r, ActiveState t e r)
    -- ^ ActiveState-pair giving the area of cells bounded during the
    -- mouse movement with button down.
    }

-- | A lens.
csCompEvent ∷ Lens' (CompState t e r) (Event t (CompEvent t e r))
csCompEvent f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState g f2 f3 f4 f5 f6 f7 f8 f9) (f f1)

-- | A lens.
csDynMInsideBody ∷ Lens' (CompState t e r) (Dynamic t Bool)
csDynMInsideBody f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 g f3 f4 f5 f6 f7 f8 f9) (f f2)

-- | A lens.
csDynMOutsideBody ∷ Lens' (CompState t e r) (Dynamic t Bool)
csDynMOutsideBody f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 g f4 f5 f6 f7 f8 f9) (f f3)

-- | A lens.
csDynDOn ∷ Lens' (CompState t e r) (Dynamic t Bool)
csDynDOn f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 f3 g f5 f6 f7 f8 f9) (f f4)

-- | A lens.
csDynUOn ∷ Lens' (CompState t e r) (Dynamic t Bool)
csDynUOn f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 f3 f4 g f6 f7 f8 f9) (f f5)

-- | A lens.
csDynDPress ∷ Lens' (CompState t e r) (Dynamic t (ActiveState t e r))
csDynDPress f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 f3 f4 f5 g f7 f8 f9) (f f6)

-- | A lens.
csDynURelease ∷ Lens' (CompState t e r) (Dynamic t (ActiveState t e r))
csDynURelease f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 f3 f4 f5 f6 g f8 f9) (f f7)

-- | A lens.
csDynEnter ∷ Lens' (CompState t e r) (Dynamic t (ActiveState t e r))
csDynEnter f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g → CompState f1 f2 f3 f4 f5 f6 f7 g f9) (f f8)

-- | A lens.
csDynUpLDownR ∷ Lens' (CompState t e r)
    (Dynamic t (ActiveState t e r, ActiveState t e r))
csDynUpLDownR f (CompState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (CompState f1 f2 f3 f4 f5 f6 f7 f8) (f f9)



-- Note that the default is in inconsistent state.  Hmm, what this means? TODO
-- Function updateCompState takes care of constructing a consistent one.

-- | Default value for 'CompState.
defCompState ∷ (Reflex t, ActSelem e, ActSretval r) ⇒  CompState t e r
defCompState = CompState never dF dF
    dF dF dAc dAc dAc (constDyn (defActiveState, defActiveState) )
    where
      dF = constDyn False
      dAc = constDyn defActiveState


--------------------------------------------------------------------------------

{-
-- helper
coincOnCompEvent ∷ forall t e. (Reflex t, ActSelem e, ActSretval r)
             ⇒ Event t (CompEvent t e r)
             → (Event t (ActiveState t e r), Event t (ActiveState t e r),
                 Event t (ActiveState t e r), Event t (ActiveState t e r))
coincOnCompEvent evTbl = (me, evD, evU, evE)
  where
      me =  _ceMe <$> evTbl ∷ Event t (ActiveState t e r)
      evD = c oincidence $ _ceMDown  <$> evTbl ∷ Event t (ActiveState t e)
      evU = c oincidence $ _ceMUp    <$> evTbl ∷ Event t (ActiveState t e)
      evE = c oincidence $ _ceMEnter <$> evTbl ∷ Event t (ActiveState t e)
-}

--------------------------------------------------------------------------------

-- | Other events, e.g., tracking the mouse entering and leaving the component.
data CompOtherEvs t = CompOtherEvs
    { _eMEnterBody ∷ Event t () -- ^ Fires when mouse enters the tbody.
    , _eMLeaveBody ∷ Event t () -- ^ Fires when mouse leaves the tbody.
    }

-- | A lens.
eMEnterBody ∷ Lens' (CompOtherEvs t) (Event t ())
eMEnterBody f (CompOtherEvs f1 f2) = fmap (`CompOtherEvs` f2) (f f1)

-- | A lens.
eMLeaveBody ∷ Lens' (CompOtherEvs t) (Event t ())
eMLeaveBody f (CompOtherEvs f1 f2) = fmap (CompOtherEvs f1) (f f2)


-- makeLenses ''CompOtherEvs

-- | Default value for 'CompOtherEvs'.
defCompOtherEvs ∷ Reflex t ⇒ CompOtherEvs t
defCompOtherEvs = CompOtherEvs never never


--------------------------------------------------------------------------------

-- |
-- Note that if we are going to use variants of this methods, then
-- the "activeState" update methods possibly need variants that work together
-- with the modified updateCompState -method. See the actMD and actMU etc.
-- This method forms the result structure that can be returned from
-- the table-element.
-- "Each row returns a set of CompEvents, from which the leftmost is
-- delivered here. See eg. mkTable and _tdCombFun."
updateComponentState ∷ forall t m e r. (Reflex t, MonadHold t m
                                       , ActSelem e, ActSretval r)
             ⇒ ( Dynamic t (ActiveState t e r, ActiveState t e r)
               → Dynamic t (ActiveState t e r, ActiveState t e r))
             → CompOtherEvs t → Event t (CompEvent t e r)
             → m (CompState t e r)
updateComponentState duHelp tblOevs eCompEvent = do
    -- Hmm, this is somewhat complex and needs rewriting. TODO!
    let _eResTxt = _ceMe <$> eCompEvent -- this is ActiveState
    evD ∷ Event t (ActiveState t e r)
        ← switchHold never $ _ceMDown  <$> eCompEvent
        -- ← switchPromptly never $ _ceMDown  <$> eCompEvent
    evU ∷ Event t (ActiveState t e r)
        ← switchHold never $ _ceMUp    <$> eCompEvent
        -- ← switchPromptly never $ _ceMUp    <$> eCompEvent
    evE ∷ Event t (ActiveState t e r)
        ← switchHold never $ _ceMEnter <$> eCompEvent
        -- ← switchPromptly never $ _ceMEnter <$> eCompEvent
    let eIn = _eMEnterBody tblOevs
        eOut = _eMLeaveBody tblOevs
        eDOn = leftmost [True <$ evD, False <$ evU, False <$ eOut]
    dDOn ← holdDyn False eDOn
    dDPress ← holdDyn defActiveState evD
    dEnter ← holdDyn defActiveState evE
    -- The following keeps True if still pressing down and moving out.
    dDOn2 ← holdDyn False $ leftmost [True <$ evD, False <$ evU, False <$ eIn]
    let dEOODOn = (\b ae → if b then ae else defActiveState) <$> dDOn2 <*> dEnter
        eOutIf = gate (current dDOn2) eOut
    dURelease ← holdDyn defActiveState
        $ leftmost [evU, tagPromptlyDyn dEOODOn eOutIf]
    dInside  ← holdDyn False $ leftmost [True <$ eIn, False <$ eOut]
    let
        dUOn = fmap not dDOn
        dOutside = fmap not dInside
        dDEn = zipDynWith (\b eu → if b then eu else defActiveState) dDOn dEnter
        dDU2 = zipDynWith (,) dDPress dDEn -- when moving
        dDU3 = zipDynWith (,) dDPress dURelease -- when releasing
        dDU4 = (\b mo rel → if b then rel else mo )
          <$> dUOn <*> dDU2 <*> dDU3  -- move and release
        dDU = duHelp dDU4 -- standardized
    pure $ CompState eCompEvent dInside dOutside
        dDOn dUOn dDPress dURelease dEnter dDU


--------------------------------------------------------------------------------

-- TODO! Change the following to work the TableResults instead of current way of
-- having three parameters affecting the ActiveState changing function.
-- This way, adding new parameters (or using states already formed) etc will
-- become match easier.
-- ???


-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseDown events and enabling listening
-- those mouseDown events on a group of elements (cells / nodes).
actMD ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
      ⇒ Dynamic t (Maybe [ActiveState t e r])
      -- ^ Information about activity states.
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Not used here.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actMD ma evTbl _tblSt ac = do
    evD ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMDown <$> evTbl
    -- evD ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMDown <$> evTbl
    actMDMUcommon ma evD ac


-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of elemetns (cells nodes).
actMU ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
      ⇒ Dynamic t (Maybe [ActiveState t e r])
      -- ^ Information about activity states. These are default or initial values.
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Not used here.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actMU ma evTbl _tblSt ac = do
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMUp <$> evTbl
    -- evU ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMUp <$> evTbl
    actMDMUcommon ma evU ac

-- | Common parts of the 'actMD' and 'actMU'.
actMDMUcommon ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                              , PostBuild t m, ActSelem e, Ord e)
              ⇒ Dynamic t (Maybe [ActiveState t e r])
              -- ^ Information about activity states.
              → Event t (ActiveState t e r)
              -- ^ contains the cell info, who is firing an ev.
              → ActiveState t e r
              -- ^ contains the cell info, whose state we are changing
              → m (ActiveState t e r) -- ^ Return the changed state
actMDMUcommon dma ev ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        ev2 = gate (current dAvable) $ fmap _activeStateElemId ev
        ev3 = attachPromptlyDyn (_activeStateListen ac) ev2
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) ev3
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) → Set.notMember ae ags) ev3
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eNM = gate bIsActive eNotMyLstG
        eM = gate bIsNot eMyLstG
        -- eNM = traceEvent "Common, eNM" eNM2
    -- dActive ← holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNM]
    dActive ← holdDyn False $ leftmost [True <$ eM, False <$ eNM, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac
    -- if ma is not nothing
    --    if a is empty, then bAvable = true
    --    if a is not empty, then
    --      if ac inside any a elem
    --         then bAvable = a's value and dActive = a's value
    --         else use the ac's current value
    -- else use ac's current value

-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of elements (cells / nodes). 
-- This makes changes only
-- on clicks on the tracked elements. See also 'actMD'.
actMDOnListened ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
      ⇒ Dynamic t (Maybe [ActiveState t e r])
      -- ^ Information about activity states. These are default or initial values.
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Not used here.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actMDOnListened ma evTbl _tblSt ac = do
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMDown <$> evTbl
    -- evU ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMDown <$> evTbl
    actMDMUOnListenedCommon ma evU ac

-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of elements (cells / nodes).
-- This makes changes only
-- on clicks on the tracked elements. See also 'actMU'.
actMUOnListened ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
      ⇒ Dynamic t (Maybe [ActiveState t e r])
      -- ^ Information about activity states. These are default or initial values.
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Not used here.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actMUOnListened ma evTbl _tblSt ac = do
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMUp <$> evTbl
    -- evU ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMUp <$> evTbl
    actMDMUOnListenedCommon ma evU ac


-- | Common parts of the 'actMDOnListened' and 'actMUOnListened'.
actMDMUOnListenedCommon ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                              , PostBuild t m, ActSelem e, Ord e)
              ⇒ Dynamic t (Maybe [ActiveState t e r])
              -- ^ Information about activity states.
              → Event t (ActiveState t e r)
              -- ^ contains the cell info, who is firing an ev.
              → ActiveState t e r
              -- ^ contains the cell info, whose state we are changing
              → m (ActiveState t e r) -- ^ Return the changed state
actMDMUOnListenedCommon dma ev ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        ev2 = gate (current dAvable) $ fmap _activeStateElemId ev
        ev3 = attachPromptlyDyn (_activeStateListen ac) ev2
        eMyLstG = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) ev3
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eM = gate bIsNot eMyLstG
        eNM = gate bIsActive eMyLstG
    dActive ← holdDyn False $ leftmost [True <$ eM, False <$ eNM, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac



-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of elements (cells / nodes).
-- This makes changes only
-- on clicks on the tracked elements. And this changes to the state
-- of the tracked and clicked element.
actMUFollowListened ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
      ⇒ Dynamic t (Maybe [ActiveState t e r])
      -- ^ Information about activity states. These are default or initial values.
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Not used here.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actMUFollowListened ma evTbl _tblSt ac = do
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMUp <$> evTbl
    -- evU ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMUp <$> evTbl
    actMDMUFollowListenedCommon ma evU ac

actMDMUFollowListenedCommon
    ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m, ActSelem e, Ord e)
    ⇒ Dynamic t (Maybe [ActiveState t e r])
    -- ^ Information about activity states.
    → Event t (ActiveState t e r)
    -- ^ contains the cell info, who is firing an ev.
    → ActiveState t e r
    -- ^ contains the cell info, whose state we are changing
    → m (ActiveState t e r) -- ^ Return the changed state
actMDMUFollowListenedCommon dma ev ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        evId = gate (current dAvable) $ fmap _activeStateElemId ev
        -- Are we changing a cell that fires (*):
        evId2 = ffilter (== _activeStateElemId ac) evId 
        ev2b = gate (current dAvable) ev
        ev2 = difference ev2b evId2
        ev3 = attachPromptlyDyn (_activeStateListen ac) ev2
        eMyLstG = ffilter (\(ActiveGroup ags,ast)
            → Set.member (_activeStateElemId ast) ags) ev3
        -- The following three are used only on (*), and then we
        -- check that "me" is in the set of listened elements (indirectly).
        ev22 = gate (current dAvable) $ fmap _activeStateElemId ev
        ev32 = attachPromptlyDyn (_activeStateListen ac) ev22
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) → Set.notMember ae ags) ev32
        edAct ∷ Event t (Dynamic t Bool)
            = (_activeStateActive . snd) <$> eMyLstG
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        e2F = gate bIsActive (difference evId2 eNotMyLstG)
        e2T = gate bIsNot (difference evId2 eNotMyLstG)
    -- Active state of this element:
    ddAct ← holdDyn (constDyn False) edAct
    let dAct = join ddAct
    -- dActive ← holdDyn False $ leftmost [eNewA, eAveOnPB]
    dActive ← holdDyn False $ leftmost [updated dAct
                                       , True <$ e2T
                                       , False <$ e2F
                                       , eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac





-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events, and swithing between
-- true and false on each click.
-- Only one element (or group) can be active at a time.  The active group or
-- element can be de-activated.
act1MUSwitch ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                           , PostBuild t m, ActSelem e, Ord e)
            ⇒  Dynamic t (Maybe [ActiveState t e r])
            -- ^ Information about activity states.
            → Event t (CompEvent t e r)
            -- ^ contains the cell info, who is firing an ev.
            → CompState t e r
            -- ^ Not used here.
            → ActiveState t e r
            -- ^ contains the cell info, whose state we are changing
            → m (ActiveState t e r)
act1MUSwitch dma ev _tblSt ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
    eMe3 ← switchHold never $ _ceMUp <$> ev
    -- eMe3 ← switchPromptly never $ _ceMUp <$> ev
    let eMe = gate (current dAvable) $  _activeStateElemId <$> eMe3
        eMe2 = attachPromptlyDyn (_activeStateListen ac) eMe
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) eMe2
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) → Set.notMember ae ags) eMe2
        eMe2T = ffilter (==False) $ tag (current dActive) eMyLstG
        eMe2F = ffilter (==True) $ tag (current dActive) eMyLstG
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eMT = gate bIsNot eMe2T
        eMF = gate bIsActive eMe2F
        eNM = gate bIsActive eNotMyLstG
        -- eM = gate bIsNot eMyLstG
    dActive ← holdDyn False
        -- leftmost [True <$ eM, False <$ eNM, eAveOnPB]
        -- leftmost [True <$ eM, False <$ eNM, False <$ eMF, eAveOnPB]
        $ leftmost [True <$ eMT, False <$ eMF, False <$ eNM, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac


-- | This allows one element or group to be active at a time.  The
-- active can be switched to non-active state.
--
-- This has a bit annoying property, that is, if clicking on other elements,
-- not in the group, while the other group is active, this first de-activates
-- the other group but does not change the state of group where clicked.
-- (A snd click is required.)
-- TODO find out a way to activate the group on 1st click. (That is, just like
-- the actMU but with the possibility to de-activate the active group.)
--
-- The 'act1MUSwitch' is similar but there the new group, if it contains the
-- already active group as a subset, switches all states, that is, the
-- already active becomes non-active.
act1MUSwitchAbsorb ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                           , PostBuild t m, ActSelem e, Ord e)
            ⇒  Dynamic t (Maybe [ActiveState t e r])
            -- ^ Information about activity states.
            → Event t (CompEvent t e r)
            -- ^ contains the cell info, who is firing an ev.
            → CompState t e r
            -- ^ Not used here. This is the result that the component gives
            -- out.
            → ActiveState t e r
            -- ^ contains the cell info, whose state we are changing
            → m (ActiveState t e r)
act1MUSwitchAbsorb dma ev _tblSt ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        -- dMUOrig = join $ _activeStateActive <$> _csDynURelease _tblSt
            -- ∷ Dynamic t Bool
            -- ∷ Dynamic t (ActiveState t e)
    eMe3 ← switchHold never $ _ceMUp <$> ev
    -- eMe3 ← switchPromptly never $ _ceMUp <$> ev
    let eMe = gate (current dAvable) $  _activeStateElemId <$> eMe3
        -- evdMe ∷ Event t Bool = c oincidence $ updated
            -- <$> _activeStateActive <$> c oincidence (_ceMUp <$> ev)
        -- hmm ∷ Event t (Dynamic t Bool)
            -- = _activeStateActive <$> c oincidence (_ceMUp <$> ev)
        ebOrig = (current . _activeStateActive) <$> eMe3
            ∷ Event t (Behavior t Bool)
    -- dEvdMe ← holdDyn False evdMe
    bMUOrig ← switcher (constant False) ebOrig
    let eMe2 = attachPromptlyDyn (_activeStateListen ac) eMe
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) eMe2
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) → Set.notMember ae ags) eMe2
        -- eOrig = tag (current dEvdMe) eMyLstG
        -- eOrig = tag (current dMUOrig) eMyLstG
        bIsActive = current dActive
        -- bIsNot = fmap not bIsActive
        -- bOrigIsActive = current dEvdMe
        -- bOrigIsActive = current dMUOrig
        bOrigIsActive = bMUOrig
        bOrigIsNot = fmap not bOrigIsActive
        e2F = gate bOrigIsActive eMyLstG
        e2T = gate bOrigIsNot eMyLstG
        eNM = gate bIsActive eNotMyLstG
        -- eM = gate bIsNot eMyLstG
        -- e2F = gate bOrigIsActive eM
        -- e2T = gate bOrigIsNot eM
    -- dUseOrig ← holdDyn False
        --  leftmost [True <$ eMyLstG, False <$ eNotMyLstG]
    dActive ← holdDyn False
        -- leftmost [True <$ e2T, False <$ e2F, eAveOnPB]
        $ leftmost [True <$ e2T, False <$ e2F, False <$ eNM, eAveOnPB]
        -- leftmost [False <$ e2F, True <$ e2T, True <$ eM, False <$ eNM, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac


-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking mouseUp events, and swithing between
-- true and false on each click.
actSwitchMU ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                           , PostBuild t m, ActSelem e, Ord e)
            ⇒  Dynamic t (Maybe [ActiveState t e r])
            -- ^ Information about activity states.
            → Event t (CompEvent t e r)
            -- ^ contains the cell info, who is firing an ev.
            → CompState t e r
            -- ^ Not used here.
            → ActiveState t e r
            -- ^ contains the cell info, whose state we are changing
            → m (ActiveState t e r)
actSwitchMU dma ev _tblSt ac = mdo
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        -- meac = _activeStateElemId ac ∷ ActElem
        -- eMe = gate (current dAvable) $ ffilter (==meac) $ _activeStateElemId <$>
        --     c oincidence (_ceMUp <$> ev)
    eMe3 ← switchHold never $ _ceMUp <$> ev
    -- eMe3 ← switchPromptly never $ _ceMUp <$> ev
    let eMe = gate (current dAvable) $  _activeStateElemId <$> eMe3
        eMe2 = attachPromptlyDyn (_activeStateListen ac) eMe
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) eMe2
        eMe2T = ffilter (==False) $ tag (current dActive) eMyLstG
        eMe2F = ffilter (==True) $ tag (current dActive) eMyLstG
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eMT = gate bIsNot eMe2T
        eMF = gate bIsActive eMe2F
    -- dActive ← holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNotMyLstG]
    -- dActive ← holdDyn False $ leftmost [True <$ eMe2T, False <$ eMe2F]
    dActive ← holdDyn False $ leftmost [True <$ eMT, False <$ eMF, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac



-- | No states are tracked. This forwards the input state without changing it.
actNone ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                          , ActSelem e)
      ⇒  Dynamic t (Maybe [ActiveState t e r])
      → Event t (CompEvent t e r)
      → CompState t e r
      → ActiveState t e r
      → m (ActiveState t e r)
actNone _dma _evTbl _tblSt = pure



-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking both mouseDown and mouseUp events,
-- and swithing between true and false on all elements (cells / nodes)
-- bounded by the elements (cells /nodes) firing down and up (and enter) events.
-- This can be used to select a single area from the table or a tree. Old
-- selection will be initialized to not-selected when new
-- mouseDown is occurring.
actAreaMDUsel ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                             , ActSelem e, Eq e)
              ⇒  Dynamic t (Maybe [ActiveState t e r])
              → Event t (CompEvent t e r)
              → CompState t e r
              → ActiveState t e r
              → m (ActiveState t e r)
actAreaMDUsel = actAreaSelCommon cmnF
  where
    cmnF _ma2 eInsider eOutsider eMeDown evD evU = do
        a ← foldDyn (\f b → f b) (Left False) $
            leftmost [ initLR2OnMDC <$ eMeDown
                     , initLR <$ evD
                     , updLROnEnter <$ eInsider
                     , fromR2L <$ eOutsider
                     , updLR2L <$ evU
                     ]
        pure a

-- | State change management working with CompEvent events.
-- ActiveState changes only by tracking both mouseDown and mouseUp events,
-- and swithing between true and false on all elements (cells / nodes)
-- bounded by the elements (cells /nodes) firing down and up (and enter) events.
-- This can be used to select a several areas from the table or tree.
-- When selecting overlapping area, the states of the overlapped cells are
-- switched.
actSwitchMDUsel ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
                               , ActSelem e, Eq e)
                ⇒ Dynamic t (Maybe [ActiveState t e r])
                → Event t (CompEvent t e r)
                → CompState t e r
                → ActiveState t e r
                → m (ActiveState t e r)
actSwitchMDUsel = actAreaSelCommon cmnF
  where
    cmnF _ma2 eInsider eOutsider eMeDown evD evU = do
        a ← foldDyn (\f b → f b) (Left False) $
            leftmost [ updLR2ROnMDC <$ eMeDown
                 , updLR2L <$ evD
                 , updLROnEnter <$ eInsider
                 , fromR2L <$ eOutsider
                 , updLR2L <$ evU
                 ]
        pure a

-- | A common functionality for 'actSwitchMDUsel' and 'actAreaMDUsel'
actAreaSelCommon ∷ forall m t e r . (Reflex t, MonadFix m, MonadHold t m
                                , ActSelem e, Eq e)
                 ⇒
                 ( Dynamic t (Maybe [ActiveState t e r])
                 → Event t Bool → Event t Bool → Event t (ActiveState t e r)
                 → Event t (ActiveState t e r)
                 → Event t (ActiveState t e r)
                 → m (Dynamic t (Either Bool Bool)))
      → Dynamic t (Maybe [ActiveState t e r])
      → Event t (CompEvent t e r)
      -- ^ contains the cell info, who is firing an ev.
      → CompState t e r
      -- ^ Contains state of the component (i.e. on tables, gives info
      -- if we leave the table area). This is the result that the component
      -- gives outside.
      → ActiveState t e r
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t e r)
actAreaSelCommon cmnF dma evTbl tblSt ac = do
-- Hmm, it is possible to release mouse button on an area between cells.
-- We don't have an event for that situation.
-- This contains the common parts for the 'actAreaMDUsel' and 'actSwitchMDUsel'.
--
-- Note that we don't have activity initializing here yet. TODO! TODO!
    let dAvable = isActivableDyn dma ac
        -- eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        -- evTbl2 = gate (current dAvable) evTbl
    evD ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMDown  <$> evTbl
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMUp    <$> evTbl
    -- evD ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMDown  <$> evTbl
    -- evU ∷ Event t (ActiveState t e r) ← switchPromptly never $ _ceMUp    <$> evTbl
    -- (_evMe', evD, evU, _evE) = coincOnCompEvent evTbl2
    let dDU = _csDynUpLDownR tblSt
            ∷ Dynamic t (ActiveState t e r, ActiveState t e r)
        dDUAE = fmap (_activeStateElemId *** _activeStateElemId) dDU
            ∷ Dynamic t (e, e)
        -- dMoLe = _dMOutsideBody tblSt
        -- eMoLe = updated dMoLe
        eMoLe = mouseOutEvent tblSt
        meac = _activeStateElemId ac ∷ e
        eMeDown = ffilter (\ast → meac == _activeStateElemId ast) evD
    bMoDo ← hold False $ leftmost [True <$ evD, False <$ evU, False <$ eMoLe]
    -- let dIsInside = fmap (`isInside` meac) dDUAE
    let dIsInside = fmap (`isInsidePair` meac) dDUAE
        eInsider = gate bMoDo $ ffilter (==True) $ updated dIsInside
        eOutsider = gate bMoDo $ ffilter (==False) $ updated dIsInside
    dActive2 ← cmnF dma eInsider eOutsider eMeDown evD evU
    let dActive = fmap fromEither dActive2
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac


--------------------------------------------------------------------------------

-- | Allows to build a simple state managing behaviour.
-- At most one cell can be active when changing states with 'actUniqPrim'.
-- Note: see the prim-examples on how to use this one.
actUniqPrim ∷ (Reflex t, MonadFix m, MonadHold t m
              , ActSelem e, Eq e)
        ⇒ Event t e → ActiveState t e r → m (ActiveState t e r)
actUniqPrim ev ac = mdo
  let me = _activeStateElemId ac
      --eAlreadyActive = ffilter (==True) $ tag (current dActive) ev
      eAlreadyNotActive =ffilter (==False) $ tag (current dActive) ev
      eMe = ffilter (==me) ev
      eNotMe = difference (ffilter (/=me) ev) eAlreadyNotActive
  dActive ← holdDyn False $ leftmost [True <$ eMe, False <$ eNotMe]
  pure $ ac {_activeStateActive = dActive }

-- | Allows to build a simple state managing behaviour.
-- Each cell is switchable separately with 'actSwitchPrim'.
-- Note: see the prim-examples on how to use this one.
actSwitchPrim ∷ (Reflex t, MonadFix m, MonadHold t m
                , ActSelem e, Eq e)
          ⇒ Event t e → ActiveState t e r → m (ActiveState t e r)
actSwitchPrim ev ac = mdo
  let me = _activeStateElemId ac
      eMe = ffilter (==me) ev
      eMe2T = ffilter (==False) $ tag (current dActive) eMe
      eMe2F = ffilter (==True) $ tag (current dActive) eMe
  dActive ← holdDyn False $ leftmost [True <$ eMe2T, False <$ eMe2F]
  pure $ ac {_activeStateActive = dActive }

-- | Allows to build a simple state managing behaviour.
-- A group activation: allows a cell to listen for the state of
-- other cells and change own state accordingly.
-- Note: see the prim-examples on how to use this one.
actGroupsPrim ∷  (Reflex t, MonadFix m, MonadHold t m
                 , ActSelem e, Eq e, Ord e)
          ⇒ Event t e → ActiveState t e r → m (ActiveState t e r)
actGroupsPrim ev ac = mdo
  let me = _activeStateElemId ac
      eAlreadyNotActive =ffilter (==False) $ tag (current dActive) ev
      eNotMe = difference (ffilter (/=me) ev) eAlreadyNotActive
      ev2 = attachPromptlyDyn (_activeStateListen ac) ev
      eMyLstG    = ffilter (\(ActiveGroup ags,ae) → Set.member ae ags) ev2
  dActive ← holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNotMe]
  pure $ ac {_activeStateActive = dActive }




--------------------------------------------------------------------------------

-- | This brings in events that are used with tables but not given by "cells".
-- See 'mkTable' on a use. Helper.
compEventsEnLe ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                           , DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Element EventResult (DomBuilderSpace m) t
            → m (CompOtherEvs t)
compEventsEnLe h = do
    let ht = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw h
    eMoEn ← wrapDomEvent ht (`DOM.on` DOM.mouseEnter) DOM.preventDefault
    eMoLe ← wrapDomEvent ht (`DOM.on` DOM.mouseLeave) DOM.preventDefault
    pure $ CompOtherEvs eMoEn eMoLe
    -- let eMoEn = domEvent Mouseenter htTable
    --     eMoLe = domEvent Mouseleave htTable
    -- pure $ CompOtherEvs eMoEn eMoLe

--------------------------------------------------------------------------------

-- | This brings in events that are used with tables but not given by "cells".
-- See 'mkTable' on a use. Helper.
compEventsNone ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                           , DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Element EventResult (DomBuilderSpace m) t
            → m (CompOtherEvs t)
compEventsNone _ = pure defCompOtherEvs


--------------------------------------------------------------------------------

-- | Here we define, what events to track and how (e.g. we use preventDefault
-- so that when selecting several cells, the browser's text selection is not
-- used).
--
-- This tracks mouse up, down and enter events and events originated from
-- widget drawn during 'adeDraw' in 'adeCombFun'.
elemEvF ∷ forall t m e r. (Reflex t, TriggerEvent t m, MonadJSM m
                       , DomBuilderSpace m ~ GhcjsDomSpace
                       , ActSelem e)
        ⇒ ActiveState t e r → Element EventResult (DomBuilderSpace m) t
        → m (CompEvent t e r)
elemEvF me e = do
    let htT = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw e
    eMoDo ← wrapDomEvent htT (`DOM.on` DOM.mouseDown) DOM.preventDefault
    eMoUp ← wrapDomEvent htT (`DOM.on` DOM.mouseUp) DOM.preventDefault
    pure $ CompEvent me
            (me <$ eMoDo)
            (me <$ eMoUp)
            (me <$ domEvent Mouseenter e)
            (updated $ _activeStateRetVal me)

-- | Like 'elemEvF', but without preventDefaults.
--
-- This tracks mouse up, down and enter events and events originated from
-- widget drawn during 'adeDraw' in 'adeCombFun'.
elemEvF2 ∷ forall t m e r. (Reflex t, TriggerEvent t m, MonadJSM m
                       , DomBuilderSpace m ~ GhcjsDomSpace
                       , ActSelem e)
        ⇒ ActiveState t e r → Element EventResult (DomBuilderSpace m) t
        → m (CompEvent t e r)
elemEvF2 me e = do
    pure $ CompEvent me
            (me <$ domEvent Mousedown e)
            (me <$ domEvent Mouseup e)
            (me <$ domEvent Mouseenter e)
            (updated $ _activeStateRetVal me)

-- | This does not track any events.
elemNoEvs ∷ forall t m e r. (Reflex t, TriggerEvent t m, MonadJSM m
                        , DomBuilderSpace m ~ GhcjsDomSpace
                        , ActSelem e, ActSretval r)
        ⇒ ActiveState t e r → Element EventResult (DomBuilderSpace m) t
        → m (CompEvent t e r)
elemNoEvs me _e = pure $ set ceMe me defCompEvent


--------------------------------------------------------------------------------

-- | Helper. 'mkTable' uses this one.
compEvFiring ∷ forall t e r. (Reflex t, ActSelem e, ActSretval r)
            ⇒ [CompEvent t e r] → Event t (CompEvent t e r)
compEvFiring l = leftmost evl
  where
    evl ∷ [Event t (CompEvent t e r)]
    evl = fmap tblEv2Ev l
    tblEv2Ev ∷ CompEvent t e r → Event t (CompEvent t e r)
    tblEv2Ev tbl@(CompEvent _me ec ed en er) =
        tbl <$ leftmost [ () <$ ec, () <$ ed, () <$ en, () <$ er]

--------------------------------------------------------------------------------

-- | Capture events when moving mouse out of table or in.
-- These are used to handle, e.g.,  the behavior when pressing mouse button down
-- and moving out of table area.
mouseInEvent, mouseOutEvent ∷ (Reflex t, ActSelem e)
                            ⇒ CompState t e r → Event t ()
mouseInEvent tblSt = () <$ ffilter (== True) (updated $ _csDynMInsideBody tblSt)
mouseOutEvent tblSt = () <$ ffilter (== True) (updated $ _csDynMOutsideBody tblSt)


--------------------------------------------------------------------------------

{-
-- A Helper function to be used with 'mkRow' function. This is declared
-- in the 'TableConf', see tableRowEvFilter.
ceFilterMD ∷ forall t e r. (Reflex t, ActSelem e)
           ⇒ Event t (CompEvent t e r) → Event t e
-- ceFilterMD evTE = _activeStateElemId <$> c oincidence (_ceMDown <$> evTE )
ceFilterMD _ = never

-- A Helper function to be used with 'mkRow' function. This is declared
-- in the 'TableConf', see tableRowEvFilter.
ceFilterMU ∷ forall t e r. (Reflex t, ActSelem e)
           ⇒ Event t (CompEvent t e r) → Event t e
-- ceFilterMU evTE =  _activeStateElemId <$> c oincidence (_ceMUp <$> evTE )
ceFilterMU _ = never

-- A Helper function to be used with 'mkRow' function. This is declared
-- in the 'TableConf', see tableRowEvFilter.
ceFilterMEnter ∷ forall t e r. (Reflex t, ActSelem e)
               ⇒ Event t (CompEvent t e r) → Event t e
-- ceFilterMEnter evTE =  _activeStateElemId <$> c oincidence (_ceMEnter <$> evTE )
ceFilterMEnter _ = never

-- A Helper function to be used with 'mkRow' function. This is declared
-- in the 'TableConf', see tableRowEvFilter.
ceFilterNone ∷ forall t e r. (Reflex t, ActSelem e)
             ⇒ Event t (CompEvent t e r) → Event t e
ceFilterNone _ = never
-}

--------------------------------------------------------------------------------

-- | helpers
fromEither ∷ Either Bool Bool → Bool
fromEither (Left b)  = b
fromEither (Right b) = b

-- | helpers
fromR2L ∷ Either Bool Bool → Either Bool Bool
fromR2L (Right b) = Left (not b)
fromR2L (Left b)  = Left b

-- | helpers
updLR2ROnMDC ∷ Either Bool Bool → Either Bool Bool
updLR2ROnMDC (Right b) = Right (not b)
updLR2ROnMDC (Left b)  = Right (not b)

-- | helpers
updLR2L ∷ Either Bool Bool → Either Bool Bool
updLR2L (Right b) = Left b
updLR2L (Left b)  = Left b

-- | helpers
initLR ∷ Either Bool Bool → Either Bool Bool
initLR _ = Left False
-- | helpers
initLR2OnMDC ∷ Either Bool Bool → Either Bool Bool
initLR2OnMDC _  = Right True

-- | helpers
updLROnEnter ∷ Either Bool Bool → Either Bool Bool
updLROnEnter (Left b)  = Right (not b)
updLROnEnter (Right b) = Right b

