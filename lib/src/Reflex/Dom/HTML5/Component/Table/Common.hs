{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.Common
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TableCommon

This module defines most of the functionality used with the tables.

Idea shortly, if the defaults are not providing what is needed:

    * Try different cell drawing function, like 'drawDivContent',
      write your own (take one as a starting point).
      Note that the drawing-function is most likely closely tied
      to the input the 'mkTableV' is given.
    * Try different listen-functions, like 'listenMe',
      write your own (take one as a starting point).
    * Try different act-functions to handle state management,
      like 'actMU', write you own (take one as a starting point).

You can also override the functions that combine state management,
drawing and event listening. The function that takes the input vectors
and table definitions, is reasonable short as well (so that it is easy to
write a new one if the above ones don't give enough flexibility).

The combining functions are defined in the modules:

    * "TdComb"
    * "ThComb"
    * "TfootComb"

-}

module Reflex.Dom.HTML5.Component.Table.Common
    (
    -- * Data structures
    --
    -- ** Captions and table behavior
    CaptionConf (..)
    , captionConfText
    , captionConfAttrs
    , mkCaption
    , CommonADEfuns (..)
    , actListen
    , actFu
    , drawEl
    , cellEv
    , silentPlainADE
    -- , defaultCommonADE

    -- ** TableEvent and state management for a table
    , TableEvent (..)
    , teMe
    , teMDown
    , teMUp
    , teMEnter
    , TableState (..)
    , tsTableEvent
    , tsDynMInsideBody
    , tsDynMOutsideBody
    , tsDynDOn
    , tsDynUOn
    , tsDynDPress
    , tsDynURelease
    , tsDynEnter
    , tsDynUpLDownR
    , TableOtherEvs (..)
    , eMEnterBody
    , eMLeaveBody
    , updateTableState

    -- * Cell drawing functions
    , drawDivContent
    , drawDivContentS
    , drawDivContentEx
    , drawDivActElemEx
    , drawDivAECntEx

    -- * State management
    --
    -- ** State management for the normal usage
    , actMD
    , actMU
    , actSwitchMU
    , actAreaMDUsel
    , actSwitchMDUsel
    , actNone

    -- ** Simple primitives
    --
    -- | See examples on how to use primitives to construct tables to see,
    -- how to use the following three functions.
    , actUniqPrim
    , actSwitchPrim
    , actGroupsPrim


    -- * Event helpers
    , tableEventsEnLe
    , tableEventsNone
    , tblEvFiring
    , cellEvF
    , cellNoEvs
    , mouseInEvent
    , mouseOutEvent

    -- ** Event helpers for mkRow
    , teFilterMD
    , teFilterMU
    , teFilterMEnter
    , teFilterNone

    ) where

import           Control.Arrow                  ((***))
import           Control.Lens
import           Control.Monad.Fix
import           Data.Default                   (Default, def)
import           Data.Semigroup                 ((<>))
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified GHCJS.DOM.EventM               as DOM
import qualified GHCJS.DOM.GlobalEventHandlers  as DOM
import qualified GHCJS.DOM.HTMLElement          as DOM
import           Reflex
import           Reflex.Dom.Core
-- import qualified GHCJS.DOM.MouseEvent      as DOM
import qualified GHCJS.DOM.Types                as DOM
import           Language.Javascript.JSaddle


import           Reflex.Dom.HTML5.Component.Table.StateInfo
import           Reflex.Dom.HTML5.Attrs.Globals
import           Reflex.Dom.HTML5.Elements

-- import Reflex.Dom.HTML5.Elements.Tabular

-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Caption-element definitions (attributes and text).
data CaptionConf t = CaptionConf
    { _captionConfText  :: Text  -- ^ caption text
    , _captionConfAttrs :: Dynamic t ECaption -- ^ caption attributes
    }

-- | A lens.
captionConfText :: Lens' (CaptionConf t) Text
captionConfText f (CaptionConf txt attrs) = fmap (`CaptionConf` attrs) (f txt)

-- | A lens.
captionConfAttrs :: Lens' (CaptionConf t) (Dynamic t ECaption)
captionConfAttrs f (CaptionConf txt attrs) = fmap (CaptionConf txt) (f attrs)

-- | Construct the html for the caption, if present.
mkCaption ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
          ⇒ Maybe (CaptionConf t) -- ^ Caption definitions, see 'CaptionConf'.
          → m ()
mkCaption mcapdef =
    case mcapdef of
        Just capdef -> eCaptionD (_captionConfAttrs capdef) $
            text $ _captionConfText capdef
        Nothing     -> blank

--------------------------------------------------------------------------------

-- | Common functions interface for tbody (table cell) and headers.
-- This way headers can have their own functions compared to the tbody.
data CommonADEfuns t m a = CommonADEfuns
    { _actListen :: ActiveState t → ActiveState t
    -- ^ I.e. 'listenMe' - what cells we are listening and how.
    , _actFu :: Dynamic t (Maybe [ActiveState t])
             → Event t (TableEvent t)
             → TableState t
             → ActiveState t → m (ActiveState t)
    -- ^ I.e. 'actMU' or 'actMD' - how to activate the cells.
    , _drawEl :: a → ActiveState t
              → m (Element EventResult (DomBuilderSpace m) t)
    -- ^ I.e. 'drawDivAECnt', how to draw a cell.
    , _cellEv :: ActiveState t → Element EventResult (DomBuilderSpace m) t
              → m (TableEvent t)
    -- ^ I.e. 'cellEvF'.
    }

-- | A lens.
actListen :: Lens' (CommonADEfuns t m a) (ActiveState t -> ActiveState t)
actListen f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g -> CommonADEfuns g f2 f3 f4) (f f1)

-- | A lens.
actFu :: Lens' (CommonADEfuns t m a)
    (Dynamic t (Maybe [ActiveState t])
    -> Event t (TableEvent t)
    -> TableState t
    -> ActiveState t
    -> m (ActiveState t))
actFu f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g -> CommonADEfuns f1 g f3 f4) (f f2)

-- | A lens.
drawEl :: Lens (CommonADEfuns t m a) (CommonADEfuns t m b)
    (a → ActiveState t → m
        (Element EventResult (DomBuilderSpace m) t))
    (b → ActiveState t → m
        (Element EventResult (DomBuilderSpace m) t))
drawEl f (CommonADEfuns f1 f2 f3 f4) =
    fmap (\g -> CommonADEfuns f1 f2 g f4) (f f3)

-- | A lens.
cellEv :: Lens' (CommonADEfuns t m a)
    (ActiveState t → Element EventResult (DomBuilderSpace m) t → m (TableEvent t))
cellEv f (CommonADEfuns f1 f2 f3 f4) = fmap (CommonADEfuns f1 f2 f3) (f f4)

-- makeLenses ''CommonADEfuns

--------------------------------------------------------------------------------

instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                   , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                   , DomBuilderSpace m ~ GhcjsDomSpace)
       ⇒ Default (CommonADEfuns t m a)
 where
   def = CommonADEfuns listenMe actMU drawDivContentS cellEvF


--------------------------------------------------------------------------------

-- | No cell is listening for events and no cell is having activity management.
silentPlainADE :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                  , DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ CommonADEfuns t m a
silentPlainADE = CommonADEfuns listenNoOne actNone drawDivContentS cellNoEvs

--------------------------------------------------------------------------------

-- | TableEvent-data collects events into one place.
-- This is part the result that table-component returns (see 'TableState').
-- It tells the cell and event that is happening on the cell.
data TableEvent t = TableEvent
    { _teMe     :: ActiveState t
    -- ^ "Name of the cell", a bit duplication as the same information is
    -- contained in the events below.
    , _teMDown  :: Event t (ActiveState t)
    -- ^ MouseDown event firing on ActElem.
    , _teMUp    :: Event t (ActiveState t)
    -- ^ MouseUp event firing on ActElem.
    , _teMEnter :: Event t (ActiveState t)
    -- ^ MouseEnter event firing on ActElem.
    }

-- | A lens.
teMe :: Lens' (TableEvent t) (ActiveState t)
teMe f (TableEvent f1 f2 f3 f4) = fmap (\g -> TableEvent g f2 f3 f4) (f f1)

-- | A lens.
teMDown :: Lens' (TableEvent t) (Event t (ActiveState t))
teMDown f (TableEvent f1 f2 f3 f4) = fmap (\g -> TableEvent f1 g f3 f4) (f f2)

-- | A lens.
teMUp :: Lens' (TableEvent t) (Event t (ActiveState t))
teMUp f (TableEvent f1 f2 f3 f4) = fmap (\g -> TableEvent f1 f2 g f4) (f f3)

-- | A lens.
teMEnter :: Lens' (TableEvent t) (Event t (ActiveState t))
teMEnter f (TableEvent f1 f2 f3 f4) = fmap (TableEvent f1 f2 f3) (f f4)

-- makeLenses ''TableEvent

instance Reflex t ⇒ Default (TableEvent t) where
  def = TableEvent def never never never

--------------------------------------------------------------------------------

-- | TableState-data is the result the table-component gives outside.
data TableState t = TableState
    { _tsTableEvent    :: Event t (TableEvent t), -- ^ See 'TableEvent'
    _tsDynMInsideBody  :: Dynamic t Bool, -- ^ Fires when mouse enters the tbody.
    _tsDynMOutsideBody :: Dynamic t Bool, -- ^ Fires when mouse leaves the tbody.
    _tsDynDOn          :: Dynamic t Bool, -- ^ Mouse button state, true if down.
    _tsDynUOn          :: Dynamic t Bool, -- ^ Mouse button state, true if up.
    _tsDynDPress       :: Dynamic t (ActiveState t),
    -- ^ Last ActiveState (containing ActElem) where MouseDown occurred.
    _tsDynURelease     :: Dynamic t (ActiveState t),
    -- ^ Last ActiveState (containing ActElem) where MouseUp occurred.
    _tsDynEnter        :: Dynamic t (ActiveState t),
    -- ^ Last ActiveState (containing ActElem) where enter event occurred.
    -- _tsDynUpLDownR     :: Dynamic t (ActElem, ActElem)
    _tsDynUpLDownR     :: Dynamic t (ActiveState t, ActiveState t)
    -- ^ ActiveState-pair giving the area of cells bounded during the
    -- mouse movement with button down.
    }

-- | A lens.
tsTableEvent :: Lens' (TableState t) (Event t (TableEvent t))
tsTableEvent f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState g f2 f3 f4 f5 f6 f7 f8 f9) (f f1)

-- | A lens.
tsDynMInsideBody :: Lens' (TableState t) (Dynamic t Bool)
tsDynMInsideBody f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 g f3 f4 f5 f6 f7 f8 f9) (f f2)

-- | A lens.
tsDynMOutsideBody :: Lens' (TableState t) (Dynamic t Bool)
tsDynMOutsideBody f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 g f4 f5 f6 f7 f8 f9) (f f3)

-- | A lens.
tsDynDOn :: Lens' (TableState t) (Dynamic t Bool)
tsDynDOn f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 f3 g f5 f6 f7 f8 f9) (f f4)

-- | A lens.
tsDynUOn :: Lens' (TableState t) (Dynamic t Bool)
tsDynUOn f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 f3 f4 g f6 f7 f8 f9) (f f5)

-- | A lens.
tsDynDPress :: Lens' (TableState t) (Dynamic t (ActiveState t))
tsDynDPress f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 f3 f4 f5 g f7 f8 f9) (f f6)

-- | A lens.
tsDynURelease :: Lens' (TableState t) (Dynamic t (ActiveState t))
tsDynURelease f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 f3 f4 f5 f6 g f8 f9) (f f7)

-- | A lens.
tsDynEnter :: Lens' (TableState t) (Dynamic t (ActiveState t))
tsDynEnter f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (\g -> TableState f1 f2 f3 f4 f5 f6 f7 g f9) (f f8)

-- | A lens.
tsDynUpLDownR :: Lens' (TableState t) (Dynamic t (ActiveState t, ActiveState t))
tsDynUpLDownR f (TableState f1 f2 f3 f4 f5 f6 f7 f8 f9)
  = fmap (TableState f1 f2 f3 f4 f5 f6 f7 f8) (f f9)

-- makeLenses ''TableState

-- Note that the default is in inconsistent state.
-- Function updateTableState takes care of constructing a consistent one.
instance Reflex t ⇒ Default (TableState t) where
  def = TableState never dF dF
    dF dF dAc dAc dAc (constDyn (def, def) )
    where
      dF = constDyn False
      dAc = constDyn def




--------------------------------------------------------------------------------

data TableOtherEvs t = TableOtherEvs
    { _eMEnterBody :: Event t () -- ^ Fires when mouse enters the tbody.
    , _eMLeaveBody :: Event t () -- ^ Fires when mouse leaves the tbody.
    }

-- | A lens.
eMEnterBody :: Lens' (TableOtherEvs t) (Event t ())
eMEnterBody f (TableOtherEvs f1 f2) = fmap (`TableOtherEvs` f2) (f f1)

-- | A lens.
eMLeaveBody :: Lens' (TableOtherEvs t) (Event t ())
eMLeaveBody f (TableOtherEvs f1 f2) = fmap (TableOtherEvs f1) (f f2)


-- makeLenses ''TableOtherEvs
instance Reflex t ⇒ Default (TableOtherEvs t) where
  def = TableOtherEvs never never


--------------------------------------------------------------------------------

-- |
-- Note that if we are going to use variants of this methods, then
-- the "activeState" update methods possibly need variants that work together
-- with the modified updateTableState -method. See the actMD and actMU etc.
-- This method forms the result structure that can be returned from
-- the table-element.
-- "Each row returns a set of TableEvents, from which the leftmost is
-- delivered here. See eg. mkTableV and _tdCombFun."
updateTableState ∷ forall t m. (Reflex t, MonadHold t m)
             ⇒ TableOtherEvs t → Event t (TableEvent t) → m (TableState t)
updateTableState tblOevs eTableEvent = do
    -- Hmm, this is somewhat complex and needs rewriting. TODO!
    let _eResTxt = _teMe <$> eTableEvent -- this is ActiveState
        evD = coincidence $ _teMDown  <$> eTableEvent :: Event t (ActiveState t)
        evU = coincidence $ _teMUp    <$> eTableEvent :: Event t (ActiveState t)
        evE = coincidence $ _teMEnter <$> eTableEvent :: Event t (ActiveState t)
        eIn = _eMEnterBody tblOevs
        eOut = _eMLeaveBody tblOevs
        eDOn = leftmost [True <$ evD, False <$ evU, False <$ eOut]
    dDOn <- holdDyn False eDOn
    dDPress <- holdDyn def evD
    dEnter <- holdDyn def evE
    -- The following keeps True if still pressing down and moving out.
    dDOn2 <- holdDyn False $ leftmost [True <$ evD, False <$ evU, False <$ eIn]
    let dEOODOn = (\b ae -> if b then ae else def) <$> dDOn2 <*> dEnter
        eOutIf = gate (current dDOn2) eOut
    dURelease <- holdDyn def $ leftmost [evU, tagPromptlyDyn dEOODOn eOutIf]
    dInside  <- holdDyn False $ leftmost [True <$ eIn, False <$ eOut]
    let
        dUOn = fmap not dDOn
        dOutside = fmap not dInside
        dDEn = zipDynWith (\b eu -> if b then eu else def) dDOn dEnter
        dDU2 = zipDynWith (,) dDPress dDEn -- when moving
        dDU3 = zipDynWith (,) dDPress dURelease -- when releasing
        dDU4 = (\b mo rel -> if b then rel else mo )
          <$> dUOn <*> dDU2 <*> dDU3  -- move and release
        dDU = duStd dDU4 -- standardized
    pure $ TableState eTableEvent dInside dOutside
        dDOn dUOn dDPress dURelease dEnter dDU

--------------------------------------------------------------------------------


-- | Draw content on a div that allows events to go. The provided class-attributes
-- for not activable, active and not active cells are used.
-- Note that this is only used on how to show things.
drawDivContent ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                             , DomBuilderSpace m ~ GhcjsDomSpace)
                ⇒ Text → ActiveState t
                → m (Element EventResult (DomBuilderSpace m) t)
drawDivContent = drawDCCommon id


-- | Similar to 'drawDivContent' but the showed element is showable.
drawDivContentS ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                               , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
                ⇒ a → ActiveState t
                → m (Element EventResult (DomBuilderSpace m) t)
drawDivContentS = drawDCCommon (T.pack . show)


-- | See 'drawDivContent'.
drawDCCommon ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                             , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
                ⇒ (a -> Text) -> a → ActiveState t
                → m (Element EventResult (DomBuilderSpace m) t)
drawDCCommon txtFun elm actS = do
    let dA = _activeStateActive actS
        dNA = not <$> _activeStateActivable actS
        -- dACl = (`setClasses` def) <$> _activeStateActiveCl actS
        -- dNACl = (`setClasses` def) <$> _activeStateNotActiveCl actS
        -- dNAvCl = (`setClasses` def) <$> _activeStateNotActivableCl actS
        dAGl = (`attrSetGlobals` def) <$> _activeStateActiveGl actS
        dNAGl = (`attrSetGlobals` def) <$> _activeStateNotActiveGl actS
        dNAvGl = (`attrSetGlobals` def) <$> _activeStateNotActivableGl actS
        dUse =
            (\ba bna acl nacl navcl ->
                if bna
                   then navcl
                   else if ba
                       then acl
                       else nacl
            )
            <$> dA <*> dNA <*> dAGl <*> dNAGl <*> dNAvGl
    (e,_) <- eDivD' dUse $
        text $ txtFun elm
      -- text $ T.pack . show $ elm
    pure e

-- | An example of a function that draws a td-content. The drawing function is
-- given the "cell name" (ActElem), the "cell contents" (a), and "cell state"
-- (ActiveState) that can be used to decide, how to draw.
-- This function shows the "cell contents" only together with activity
-- information.
drawDivContentEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                                , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ a → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
drawDivContentEx elm actS = do
    (e,_) <- eDivN' $ do
        text $ (T.pack . show $ elm) <> " : "
        -- dynText $ (T.pack . show) <$> view activeStateActive actS
        dynText $ (T.pack . show) <$> _activeStateActive actS
    pure e

-- | An example of a function that draws a td-content.
-- This function shows the ActElem only together with activity information.
drawDivActElemEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                                , DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ a → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
drawDivActElemEx _a actS = do
    (e,_) <- eDivN' $ do
        text $ (T.pack . show $ view activeStateElem actS) <> " : "
        -- dynText $ (T.pack . show) <$> view activeStateActive actS
        dynText $ (T.pack . show) <$> _activeStateActive actS
    pure e

-- | An example of a function that draws a td-content.
-- This function shows both the ActElem and "cell contents".
drawDivAECntEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                              , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
             ⇒ a → ActiveState t
             → m (Element EventResult (DomBuilderSpace m) t)
drawDivAECntEx elm actS = do
    (e,_) <- eDivN' $ do
        -- text $ ps (view activeStateElem me) <> " : " <> ps elm <> " : "
        text $ ps (view activeStateElem actS) <> " : " <> ps elm <> " : "

        -- dynText $ ps <$> view activeStateActive actS
        dynText $ ps <$> _activeStateActive actS
        text " : avable "
        dynText $ ps <$> view activeStateActivable actS
    pure e
      where
        ps ∷ Show b ⇒ b → T.Text
        ps = T.pack . show

--------------------------------------------------------------------------------

-- helper
duStd ∷ forall t. Reflex t
      -- ⇒ Dynamic t (ActElem,ActElem) → Dynamic t (ActElem,ActElem)
      ⇒ Dynamic t (ActiveState t,ActiveState t)
      → Dynamic t (ActiveState t,ActiveState t)
duStd dDu = selminmax <$> dDu
  where
    selminmax ∷ (ActiveState t,ActiveState t) → (ActiveState t,ActiveState t)
    -- selminmax (ActERC (i1,j1), ActERC (i2,j2))
    selminmax (ast1,ast2) = (dsa *** dsa) aep
        where
            dsa :: ActElem -> ActiveState t
            dsa a = def & \d -> d { _activeStateElem = a }
            -- dsa a = def & set activeStateElem a
            aep ::(ActElem, ActElem)
            aep = smm (_activeStateElem ast1, _activeStateElem ast2)
            -- aep = smm (view activeStateElem ast1, view activeStateElem ast2)
            --
            smm :: (ActElem, ActElem) -> (ActElem, ActElem)
            smm (ActERC (i1,j1), ActERC (i2,j2))
              = (ActERC (min i1 i2, min j1 j2), ActERC (max i1 i2, max j1 j2))
            smm a = a

--------------------------------------------------------------------------------

-- helper
coincOnTableEvent ∷ forall t. Reflex t
             ⇒ Event t (TableEvent t)
             → (Event t (ActiveState t), Event t (ActiveState t),
                 Event t (ActiveState t), Event t (ActiveState t))
coincOnTableEvent evTbl = (me, evD, evU, evE)
  where
      me =  _teMe <$> evTbl :: Event t (ActiveState t)
      evD = coincidence $ _teMDown  <$> evTbl :: Event t (ActiveState t)
      evU = coincidence $ _teMUp    <$> evTbl :: Event t (ActiveState t)
      evE = coincidence $ _teMEnter <$> evTbl :: Event t (ActiveState t)

--------------------------------------------------------------------------------


-- TODO! Change the following to work the TableResults instead of current way of
-- having three parameters affecting the ActiveState changing function.
-- This way, adding new parameters (or using states already formed) etc will
-- become match easier.
-- ???


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseDown events and enabling listening
-- those mouseDown events on a group of cells.
actMD ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m)
      ⇒ Dynamic t (Maybe [ActiveState t])
      -- ^ Information about activity states.
      → Event t (TableEvent t)
      -- ^ contains the cell info, who is firing an ev.
      → TableState t
      -- ^ Not used here.
      → ActiveState t
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t)
actMD ma evTbl _tblSt ac = do
    let evD = coincidence $ _teMDown <$> evTbl :: Event t (ActiveState t)
    actMDMUcommon ma evD ac

-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of cells.
actMU ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m
                      , PostBuild t m)
      ⇒ Dynamic t (Maybe [ActiveState t])
      -- ^ Information about activity states.
      → Event t (TableEvent t)
      -- ^ contains the cell info, who is firing an ev.
      → TableState t
      -- ^ Not used here.
      → ActiveState t
      -- ^ contains the cell info, whose state we are changing
      → m (ActiveState t)
actMU ma evTbl _tblSt ac = do
    let evU = coincidence $ _teMUp <$> evTbl :: Event t (ActiveState t)
    actMDMUcommon ma evU ac

-- | Common parts of the 'actMD' and 'actMU'.
actMDMUcommon ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m
                              , PostBuild t m)
              ⇒ Dynamic t (Maybe [ActiveState t])
              -- ^ Information about activity states.
              → Event t (ActiveState t)
              -- ^ contains the cell info, who is firing an ev.
              → ActiveState t
              -- ^ contains the cell info, whose state we are changing
              → m (ActiveState t) -- ^ Return the changed state
actMDMUcommon dma ev ac = mdo
    evPB <- getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        ev2 = gate (current dAvable) $ fmap _activeStateElem ev
        ev3 = attachPromptlyDyn (_activeStateListen ac) ev2
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) -> Set.member ae ags) ev3
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) -> Set.notMember ae ags) ev3
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eNM = gate bIsActive eNotMyLstG
        eM = gate bIsNot eMyLstG
        -- eNM = traceEvent "Common, eNM" eNM2
    -- dActive <- holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNM]
    dActive <- holdDyn False $ leftmost [True <$ eM, False <$ eNM, eAveOnPB]
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


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseUp events, and swithing between
-- true and false on each click.
actSwitchMU ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m, PostBuild t m)
            ⇒ Dynamic t (Maybe [ActiveState t])
            -- ^ Information about activity states.
            → Event t (TableEvent t)
            -- ^ contains the cell info, who is firing an ev.
            → TableState t
            -- ^ Not used here.
            → ActiveState t
            -- ^ contains the cell info, whose state we are changing
            → m (ActiveState t)
actSwitchMU dma ev _tblSt ac = mdo
    evPB <- getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag bAve evPB
        dAve = isActiveDyn dma ac
        bAve = current dAve
        -- meac = _activeStateElem ac :: ActElem
        -- eMe = gate (current dAvable) $ ffilter (==meac) $ _activeStateElem <$>
        --     coincidence (_teMUp <$> ev)
        eMe = gate (current dAvable) $  _activeStateElem <$>
            coincidence (_teMUp <$> ev)
        eMe2 = attachPromptlyDyn (_activeStateListen ac) eMe
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) -> Set.member ae ags) eMe2
        eMe2T = ffilter (==False) $ tag (current dActive) eMyLstG
        eMe2F = ffilter (==True) $ tag (current dActive) eMyLstG
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eMT = gate bIsNot eMe2T
        eMF = gate bIsActive eMe2F
    -- dActive <- holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNotMyLstG]
    -- dActive <- holdDyn False $ leftmost [True <$ eMe2T, False <$ eMe2F]
    dActive <- holdDyn False $ leftmost [True <$ eMT, False <$ eMF, eAveOnPB]
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking both mouseDown and mouseUp events,
-- and swithing between true and false on all cells
-- bounded by the cells firing down and up (and enter) events.
-- This can be used to select a single area from the table. Old
-- selection will be initialized to not-selected when new
-- mouseDown is occurring.
actAreaMDUsel ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
              ⇒ Dynamic t (Maybe [ActiveState t])
              → Event t (TableEvent t)
              → TableState t
              → ActiveState t
              → m (ActiveState t)
actAreaMDUsel = actAreaSelCommon cmnF
  where
    cmnF _ma2 eInsider eOutsider eMeDown evD evU = do
        a <- foldDyn (\f b -> f b) (Left False) $
            leftmost [ initLR2OnMDC <$ eMeDown
                     , initLR <$ evD
                     , updLROnEnter <$ eInsider
                     , fromR2L <$ eOutsider
                     , updLR2L <$ evU
                     ]
        pure a

-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking both mouseDown and mouseUp events,
-- and swithing between true and false on all cells
-- bounded by the cells firing down and up (and enter) events.
-- This can be used to select a several areas from the table.
-- When selecting overlapping area, the states of the overlapped cells are
-- switched.
actSwitchMDUsel ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
                ⇒ Dynamic t (Maybe [ActiveState t])
                → Event t (TableEvent t)
                → TableState t
                → ActiveState t
                → m (ActiveState t)
actSwitchMDUsel = actAreaSelCommon cmnF
  where
    cmnF _ma2 eInsider eOutsider eMeDown evD evU = do
        a <- foldDyn (\f b -> f b) (Left False) $
            leftmost [ updLR2ROnMDC <$ eMeDown
                 , updLR2L <$ evD
                 , updLROnEnter <$ eInsider
                 , fromR2L <$ eOutsider
                 , updLR2L <$ evU
                 ]
        pure a

-- | A common functionality for 'actSwitchMDUsel' and 'actAreaMDUsel'
actAreaSelCommon ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
                 ⇒ (Dynamic t (Maybe [ActiveState t])
                 → Event t Bool → Event t Bool → Event t (ActiveState t)
                 → Event t (ActiveState t) → Event t (ActiveState t)
                 → m (Dynamic t (Either Bool Bool)))
      → Dynamic t (Maybe [ActiveState t])
      → Event t (TableEvent t) → TableState t
      → ActiveState t
      → m (ActiveState t)
actAreaSelCommon cmnF dma evTbl tblSt ac = do
-- Hmm, it is possible to release mouse button on an area between cells.
-- We don't have an event for that situation.
-- This contains the common parts for the 'actAreaMDUsel' and 'actSwitchMDUsel'.
--
-- Note that we don't have activity initializing here yet. TODO! TODO!
    let dAvable = isActivableDyn dma ac
        -- dAve = isActiveDyn dma ac
        -- bAve = current dAve
        -- eAveOnPB = tag bAve evPB
        evTbl2 = gate (current dAvable) evTbl
        (_evMe', evD, evU, _evE) = coincOnTableEvent evTbl2
        dDU = _tsDynUpLDownR tblSt :: Dynamic t (ActiveState t, ActiveState t)
        dDUAE = fmap (_activeStateElem *** _activeStateElem) dDU
            :: Dynamic t (ActElem, ActElem)
        -- dMoLe = _dMOutsideBody tblSt
        -- eMoLe = updated dMoLe
        eMoLe = mouseOutEvent tblSt
        -- meac = view activeStateElem ac :: ActElem
        meac = _activeStateElem ac :: ActElem
        eMeDown = ffilter (\ast -> meac == _activeStateElem ast) evD
    bMoDo <- hold False $ leftmost [True <$ evD, False <$ evU, False <$ eMoLe]
    let dIsInside = fmap (`isInside` meac) dDUAE
        eInsider = gate bMoDo $ ffilter (==True) $ updated dIsInside
        eOutsider = gate bMoDo $ ffilter (==False) $ updated dIsInside
    dActive2 <- cmnF dma eInsider eOutsider eMeDown evD evU
    let dActive = fmap fromEither dActive2
    pure $ set activeStateNotActivableGl (getNotActivableGl dma ac)
         $ set activeStateNotActiveGl (getNotActiveGl dma ac)
         $ set activeStateActiveGl (getActiveGl dma ac)
         $ set activeStateActivable dAvable
         $ set activeStateActive dActive ac


-- | No states are tracked. This forwards the input state without changing it.
actNone ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
      ⇒ Dynamic t (Maybe [ActiveState t]) → Event t (TableEvent t)
      → TableState t → ActiveState t → m (ActiveState t)
actNone _dma _evTbl _tblSt = pure



--------------------------------------------------------------------------------

-- | Allows to build a simple state managing behaviour.
-- At most one cell can be active when changing states with 'actUniqPrim'.
-- Note: see the prim-examples on how to use this one.
actUniqPrim ∷ (Reflex t, MonadFix m, MonadHold t m)
        ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actUniqPrim ev ac = mdo
  -- let me = view activeStateElem ac
  let me = _activeStateElem ac
      --eAlreadyActive = ffilter (==True) $ tag (current dActive) ev
      eAlreadyNotActive =ffilter (==False) $ tag (current dActive) ev
      eMe = ffilter (==me) ev
      eNotMe = difference (ffilter (/=me) ev) eAlreadyNotActive
  dActive <- holdDyn False $ leftmost [True <$ eMe, False <$ eNotMe]
  pure $ ac {_activeStateActive = dActive }
  -- pure $ set activeStateActive dActive ac

-- | Allows to build a simple state managing behaviour.
-- Each cell is switchable separately with 'actSwitchPrim'.
-- Note: see the prim-examples on how to use this one.
actSwitchPrim ∷ (Reflex t, MonadFix m, MonadHold t m)
          ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actSwitchPrim ev ac = mdo
  -- let me = view activeStateElem ac
  let me = _activeStateElem ac
      eMe = ffilter (==me) ev
      eMe2T = ffilter (==False) $ tag (current dActive) eMe
      eMe2F = ffilter (==True) $ tag (current dActive) eMe
  dActive <- holdDyn False $ leftmost [True <$ eMe2T, False <$ eMe2F]
  pure $ ac {_activeStateActive = dActive }
  -- pure $ set activeStateActive dActive ac

-- | Allows to build a simple state managing behaviour.
-- A group activation: allows a cell to listen for the state of
-- other cells and change own state accordingly.
-- Note: see the prim-examples on how to use this one.
actGroupsPrim ∷  (Reflex t, MonadFix m, MonadHold t m)
          ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actGroupsPrim ev ac = mdo
  -- let me = view activeStateElem ac
  let me = _activeStateElem ac
      eAlreadyNotActive =ffilter (==False) $ tag (current dActive) ev
      eNotMe = difference (ffilter (/=me) ev) eAlreadyNotActive
      --
      -- ev2 = attachPromptlyDyn (view activeStateListen ac) ev
      ev2 = attachPromptlyDyn (_activeStateListen ac) ev
      eMyLstG    = ffilter (\(ActiveGroup ags,ae) -> Set.member ae ags) ev2
  dActive <- holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNotMe]
  pure $ ac {_activeStateActive = dActive }
  -- pure $ set activeStateActive dActive ac


--------------------------------------------------------------------------------

-- helpers
fromEither ∷ Either Bool Bool → Bool
fromEither (Left b)  = b
fromEither (Right b) = b
fromR2L ∷ Either Bool Bool → Either Bool Bool
fromR2L (Right b) = Left (not b)
fromR2L (Left b)  = Left b

updLR2ROnMDC ∷ Either Bool Bool → Either Bool Bool
updLR2ROnMDC (Right b) = Right (not b)
updLR2ROnMDC (Left b)  = Right (not b)

updLR2L ∷ Either Bool Bool → Either Bool Bool
updLR2L (Right b) = Left b
updLR2L (Left b)  = Left b

initLR ∷ Either Bool Bool → Either Bool Bool
initLR _ = Left False
initLR2OnMDC ∷ Either Bool Bool → Either Bool Bool
initLR2OnMDC _  = Right True

updLROnEnter ∷ Either Bool Bool → Either Bool Bool
updLROnEnter (Left b)  = Right (not b)
updLROnEnter (Right b) = Right b


--------------------------------------------------------------------------------

-- | This brings in events that are used with tables but not given by "cells".
-- See 'mkTableV' on a use. Helper.
tableEventsEnLe ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                           , DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Element EventResult (DomBuilderSpace m) t
            → m (TableOtherEvs t)
tableEventsEnLe h = do
    let ht = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw h
    eMoEn <- wrapDomEvent ht (`DOM.on` DOM.mouseEnter) DOM.preventDefault
    eMoLe <- wrapDomEvent ht (`DOM.on` DOM.mouseLeave) DOM.preventDefault
    pure $ TableOtherEvs eMoEn eMoLe
    -- let eMoEn = domEvent Mouseenter htTable
    --     eMoLe = domEvent Mouseleave htTable
    -- pure $ TableOtherEvs eMoEn eMoLe

--------------------------------------------------------------------------------

-- | This brings in events that are used with tables but not given by "cells".
-- See 'mkTableV' on a use. Helper.
tableEventsNone ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                           , DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Element EventResult (DomBuilderSpace m) t
            → m (TableOtherEvs t)
tableEventsNone _ = pure def


--------------------------------------------------------------------------------

-- | Here we define, what events to track and how (e.g. we use preventDefault
-- so that when selecting several cells, the browser's text selection is not
-- used).
cellEvF ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m,
                       DomBuilderSpace m ~ GhcjsDomSpace)
        ⇒ ActiveState t → Element EventResult (DomBuilderSpace m) t
        → m (TableEvent t)
cellEvF me e = do
    let htT = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw e
    eMoDo <- wrapDomEvent htT (`DOM.on` DOM.mouseDown) DOM.preventDefault
    eMoUp <- wrapDomEvent htT (`DOM.on` DOM.mouseUp) DOM.preventDefault
    pure $ TableEvent me
            (me <$ eMoDo)
            (me <$ eMoUp)
            -- (me <$ domEvent Mousedown e)
            -- (me <$ domEvent Mouseup e)
            (me <$ domEvent Mouseenter e)

-- | This does not track any events.
cellNoEvs ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                        , DomBuilderSpace m ~ GhcjsDomSpace)
        ⇒ ActiveState t → Element EventResult (DomBuilderSpace m) t
        → m (TableEvent t)
cellNoEvs me _e = pure $ set teMe me def

--------------------------------------------------------------------------------

-- | Helper. 'mkTableV' uses this one.
tblEvFiring ∷ forall t. Reflex t ⇒ [TableEvent t] → Event t (TableEvent t)
tblEvFiring l = leftmost evl
  where
    evl ∷ [Event t (TableEvent t)]
    evl = fmap tblEv2Ev l
    tblEv2Ev ∷ TableEvent t → Event t (TableEvent t)
    tblEv2Ev tbl@(TableEvent _me ec ed en) = tbl <$ leftmost [ec,ed,en]

--------------------------------------------------------------------------------

-- | Capture events when moving mouse out of table or in.
-- These are used to handle, e.g.,  the behavior when pressing mouse button down
-- and moving out of table area.
mouseInEvent, mouseOutEvent ∷ Reflex t ⇒ TableState t → Event t ()
mouseInEvent tblSt = () <$ ffilter (== True) (updated $ _tsDynMInsideBody tblSt)
mouseOutEvent tblSt = () <$ ffilter (== True) (updated $ _tsDynMOutsideBody tblSt)


--------------------------------------------------------------------------------

-- | A Helper function to be used with 'mkRow' function. This is declared
-- int the 'TableConf', see tableRowEvFilter.
teFilterMD ∷ forall t. Reflex t ⇒ Event t (TableEvent t) → Event t ActElem
teFilterMD evTE = _activeStateElem <$> coincidence (_teMDown <$> evTE )

-- | A Helper function to be used with 'mkRow' function. This is declared
-- int the 'TableConf', see tableRowEvFilter.
teFilterMU ∷ forall t. Reflex t ⇒ Event t (TableEvent t) → Event t ActElem
teFilterMU evTE =  _activeStateElem <$> coincidence (_teMUp <$> evTE )

-- | A Helper function to be used with 'mkRow' function. This is declared
-- int the 'TableConf', see tableRowEvFilter.
teFilterMEnter ∷ forall t. Reflex t ⇒ Event t (TableEvent t) → Event t ActElem
teFilterMEnter evTE =  _activeStateElem <$> coincidence (_teMEnter <$> evTE )

-- | A Helper function to be used with 'mkRow' function. This is declared
-- int the 'TableConf', see tableRowEvFilter.
teFilterNone ∷ forall t. Reflex t ⇒ Event t (TableEvent t) → Event t ActElem
teFilterNone _ = never

