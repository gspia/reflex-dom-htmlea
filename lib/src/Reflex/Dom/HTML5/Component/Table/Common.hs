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

-}

module Reflex.Dom.HTML5.Component.Table.Common where

import           Control.Arrow                  ((***))
import           Control.Monad.Fix
import           Data.Default                   (Default, def)
import           Data.Semigroup                 ((<>))
import           Data.Set                       (Set)
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


import           Reflex.Dom.HTML5.Attrs.Globals
import           Reflex.Dom.HTML5.Elements

-- import Reflex.Dom.HTML5.Elements.Tabular

-- td-element represents data cell in a table.
-- th-element represents header cell in a table


--------------------------------------------------------------------------------

-- | Caption-element definitions (attributes and text).
data CaptionDef t = CaptionDef
    { _captionDefText  :: Text  -- ^ caption text
    , _captionDefAttrs :: Dynamic t ECaption -- ^ caption attributes
    }

-- | Construct the html for the caption, if present.
mkCaption ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
          ⇒ Maybe (CaptionDef t) -- ^ Caption definitions, see 'CaptionDef'.
          → m ()
mkCaption mcapdef =
    case mcapdef of
        Just capdef ->
            eCaptionD (_captionDefAttrs capdef) $ text $ _captionDefText capdef
        Nothing     -> blank

--------------------------------------------------------------------------------

-- | ActElem represents a table cell, row or column. It also has a value for
-- no cell. The main use is to keep track, which cells have been "selected".
data ActElem
    = ActERC (Int, Int) -- ^ A single cell in a table (in body), (row, column).
    | ActErow Int       -- ^ A single row
    | ActEcolh Int       -- ^ A column header-cell
    | ActEcols Int       -- ^ A column sum-cell
    | ActEnone          -- ^ This refers to not any cell in a particular table.
    deriving (Eq, Show, Ord)

instance Default ActElem where
    def = ActEnone


-- | ActiveGroup contains a set of 'ActElem'-data.
-- The main use is to have allow to listen states and state changes of
-- a group of ActElems, see below.
newtype ActiveGroup =
    ActiveGroup (Set ActElem)
    deriving (Eq, Show, Ord)

instance Default ActiveGroup where
    def = ActiveGroup def

-- | ActiveState keeps track if the ActElem is active, activable etc.
data ActiveState t = ActiveState
    { _activeStateMe        :: ActElem
    , _activeStateActive    :: Dynamic t Bool
    , _activeStateActivable :: Dynamic t Bool
    , _activeStateListen    :: Dynamic t ActiveGroup -- ^ A group to listen.
    , _activeStateActiveCl  :: Dynamic t [ClassName]
    , _activeStateNotActiveCl  :: Dynamic t [ClassName]
    , _activeStateNotActivableCl  :: Dynamic t [ClassName]
    -- ^ Html class attributes. This could be used to allow individual
    -- activation effects.
    -- If there is need beyond class-attributes (to set other attributes based
    -- on if the cell is active or activable), then it is better to implement
    -- a variant of drawXX-function and use it.
    }

-- makeLenses ''ActiveState

instance Reflex t ⇒ Default (ActiveState t) where
    def = ActiveState
            ActEnone
            (constDyn False) -- active
            (constDyn True)  -- activable
            (constDyn def)
            (constDyn []) (constDyn []) (constDyn [])

--------------------------------------------------------------------------------

-- | Activate a cell when clicked.
listenMe ∷ forall t. Reflex t ⇒ ActElem → ActiveState t
listenMe me = actstate me & \d -> d {_activeStateListen = constDyn ag }
  where
    ag = ActiveGroup $ Set.singleton me
    actstate txt = def { _activeStateMe = txt }
    -- actstate txt = def & set activeStateMe txt

-- | Activate several cells at a time: based on activation of another cell,
-- the cell using this is activated.
listenList ∷ forall t. Reflex t ⇒ [ActElem] → ActElem → ActiveState t
-- listenList lst me = actstate me & set activeStateListen (constDyn ag)
listenList lst me = actstate me & \d -> d { _activeStateListen = constDyn ag }
  where
    ag = ActiveGroup $ Set.fromList $ me : lst
    actstate txt = def { _activeStateMe = txt }
    -- actstate txt = def & set activeStateMe txt

-- | Activate another cell based on a click somewhere else.
-- A cell using this cannot be activated directly by clicking on it.
listenListNotMe ∷ forall t. Reflex t ⇒ [ActElem] → ActElem → ActiveState t
listenListNotMe lst me = actstate me & \d -> d { _activeStateListen = constDyn ag}
-- listenListNotMe lst me = actstate me & set activeStateListen (constDyn ag)
  where
    ag = ActiveGroup $ Set.fromList lst
    actstate txt = def { _activeStateMe = txt }
    -- actstate txt = def & set activeStateMe txt


-- | Common parts for the 'listenHeadMe' and 'listenFootMe'.
listenHeadFoot ∷ forall t. Reflex t ⇒ (ActElem → [ActElem])
               → ActElem → ActiveState t
listenHeadFoot f me = actstate me & \d -> d {_activeStateListen = constDyn ag}
-- listenHeadFoot f me = actstate me & set activeStateListen (constDyn ag)
  where
    ag = ActiveGroup $ Set.fromList $ me: f me
    actstate txt = def { _activeStateMe = txt }
    -- actstate txt = def & set activeStateMe txt

-- | Activate a cell when clicked or when the column header is clicked.
listenHeadMe ∷ forall t. Reflex t ⇒ ActElem → ActiveState t
listenHeadMe = listenHeadFoot myHead
  where
    myHead ∷ ActElem → [ActElem]
    myHead (ActERC (_,j)) = [ActEcolh j]
    myHead a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header is clicked.
listenFootMe ∷ forall t. Reflex t ⇒ ActElem → ActiveState t
listenFootMe = listenHeadFoot myFoot
  where
    myFoot ∷ ActElem → [ActElem]
    myFoot (ActERC (_,j)) = [ActEcols j]
    myFoot a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header or foot is clicked.
listenHeadFootMe ∷ forall t. Reflex t ⇒ ActElem → ActiveState t
listenHeadFootMe = listenHeadFoot myHF
  where
    myHF ∷ ActElem → [ActElem]
    myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
    myHF a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a column header when clicked on any of the column cells
-- (including sum-cells on footer) or on the column header.
listenMyCol ∷ forall t. Reflex t ⇒ Int → ActElem → ActiveState t
listenMyCol rows me = actstate me & \d -> d {_activeStateListen = constDyn ag}
-- listenMyCol rows me = actstate me & set activeStateListen (constDyn ag)
  where
    -- ag = ActiveGroup $ Set.fromList $ me: genElems
    ag = ActiveGroup $ Set.fromList elems2listen
    actstate txt = def { _activeStateMe = txt }
    -- actstate txt = def & set activeStateMe txt
    col = case me of
                ActEcolh j   -> j
                ActEcols j   -> j
                ActErow _    -> -1
                ActERC (_,j) -> j
                ActEnone     -> -1
    elems2listen = if col < 0
                      then []
                      else [ ActERC (x,col) | x <- [0..(rows-1)]]
                          ++ [ActEcolh col, ActEcols col]

--------------------------------------------------------------------------------

-- | TableEvent-data collects events into one place.
-- This is part the result that table-component returns (see 'TableState').
-- It tells the cell and event that is happening on the cell.
data TableEvent t = TableEvent
    { _teMe     :: ActiveState t -- ^ "Name of the cell", a bit duplication as
                                 -- the same information is contained in
                                 -- the events below.
    , _teMDown  :: Event t (ActiveState t) -- ^ MouseDown event firing on ActElem.
    , _teMUp    :: Event t (ActiveState t) -- ^ MouseUp event firing on ActElem.
    , _teMEnter :: Event t (ActiveState t) -- ^ MouseEnter event firing on ActElem.
    }

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

-- makeLenses ''TableState

-- Note that the default is in inconsistent state.
-- Function updateTableState takes care of constructing a consistent one.
instance Reflex t ⇒ Default (TableState t) where
  def = TableState never dF dF
    dF dF dAc dAc dAc (constDyn (def, def) )
    where
      dF = constDyn False
      dAc = constDyn def



mouseInEvent, mouseOutEvent ∷ Reflex t ⇒ TableState t → Event t ()
mouseInEvent tblSt = () <$ ffilter (== True) (updated $ _tsDynMInsideBody tblSt)
mouseOutEvent tblSt = () <$ ffilter (== True) (updated $ _tsDynMOutsideBody tblSt)

--------------------------------------------------------------------------------

data TableOtherEvs t = TableOtherEvs
    { _eMEnterBody :: Event t (), -- ^ Fires when mouse enters the tbody.
    _eMLeaveBody   :: Event t ()    -- ^ Fires when mouse leaves the tbody.
    }

-- makeLenses ''TableOtherEvs
instance Reflex t ⇒ Default (TableOtherEvs t) where
  def = TableOtherEvs never never

--------------------------------------------------------------------------------

data CommonADEfuns t m a = CommonADEfuns
    { _actSt :: ActElem → ActiveState t
    -- ^ _actSt, i.e. 'listenMe' - what cells we are listening and how
    , _actFu :: Event t (TableEvent t) → TableState t
             → ActiveState t → m (ActiveState t)
    -- ^ _actFu, i.e. 'actMU' or 'actMD' - how to activate the cells
    , _drawEl :: ActElem → a → ActiveState t
              → m (Element EventResult (DomBuilderSpace m) t)
    -- ^ _drawEl, i.e. 'drawDivAECnt'
    , _cellEv :: ActiveState t → Element EventResult (DomBuilderSpace m) t
              → m (TableEvent t)
    -- ^ _cellEv, i.e. 'cellEvF'
    }

-- makeLenses ''CommonADEfuns

--------------------------------------------------------------------------------


defaultCommonADE ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                    , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                    , DomBuilderSpace m ~ GhcjsDomSpace)
                 ⇒ CommonADEfuns t m a
defaultCommonADE = CommonADEfuns listenMe actMU drawDivAECntEx cellEvF


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
drawDivContent ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                               , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ ActElem → a → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
drawDivContent _me elm actS = do
    -- let dA = view activeStateActive actS
    --     dNA = not <$> view activeStateActivable actS
    --     dACl = (`setClasses` def) <$> view activeStateActiveCl actS
    --     dNACl = (`setClasses` def) <$> view activeStateNotActiveCl actS
    --     dNAvCl = (`setClasses` def) <$> view activeStateNotActivableCl actS
    let dA = _activeStateActive actS
        dNA = not <$> _activeStateActivable actS
        dACl = (`setClasses` def) <$> _activeStateActiveCl actS
        dNACl = (`setClasses` def) <$> _activeStateNotActiveCl actS
        dNAvCl = (`setClasses` def) <$> _activeStateNotActivableCl actS
        dUse =
            (\ba bna acl nacl navcl ->
                if bna
                   then navcl
                   else if ba
                       then acl
                       else nacl
            )
            <$> dA <*> dNA <*> dACl <*> dNACl <*> dNAvCl
    (e,_) <- eDivD' dUse $
      text $ T.pack . show $ elm
    pure e

-- | An example of a function that draws a td-content. The drawing function is
-- given the "cell name" (ActElem), the "cell contents" (a), and "cell state"
-- (ActiveState) that can be used to decide, how to draw.
-- This function shows the "cell contents" only together with activity
-- information.
drawDivContentEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                                , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ ActElem → a → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
drawDivContentEx _me elm actS = do
    (e,_) <- eDivN' $ do
      text $ T.pack . show $ elm
      -- dynText $ (T.pack . show) <$> view activeStateActive actS
      dynText $ (T.pack . show) <$> _activeStateActive actS
    pure e

-- | An example of a function that draws a td-content.
-- This function shows the ActElem only together with activity information.
drawDivActElemEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                                , DomBuilderSpace m ~ GhcjsDomSpace)
               ⇒ ActElem → a → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
drawDivActElemEx me _a actS = do
    (e,_) <- eDivN' $ do
      text $ T.pack . show $ me
      -- dynText $ (T.pack . show) <$> view activeStateActive actS
      dynText $ (T.pack . show) <$> _activeStateActive actS
    pure e

-- | An example of a function that draws a td-content.
-- This function shows both the ActElem and "cell contents".
drawDivAECntEx ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
                              , Show a, DomBuilderSpace m ~ GhcjsDomSpace)
             ⇒ ActElem → a → ActiveState t
             → m (Element EventResult (DomBuilderSpace m) t)
drawDivAECntEx me elm actS = do
    (e,_) <- eDivN' $ do
        text $ ps me <> ":" <> ps elm
        -- dynText $ ps <$> view activeStateActive actS
        dynText $ ps <$> _activeStateActive actS
    pure e
      where
        ps ∷ Show b ⇒ b → T.Text
        ps = T.pack . show

--------------------------------------------------------------------------------

-- | Here we define, what events to track and how (e.g. we use preventDefault
-- so that when selecting several cells, the browser's text selection is not
-- used).
cellEvF ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m,
                       DomBuilderSpace m ~ GhcjsDomSpace)
        -- ⇒ ActElem → Element EventResult (DomBuilderSpace m) t
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
            dsa a = def & \d -> d { _activeStateMe = a }
            -- dsa a = def & set activeStateMe a
            aep ::(ActElem, ActElem)
            aep = smm (_activeStateMe ast1, _activeStateMe ast2)
            -- aep = smm (view activeStateMe ast1, view activeStateMe ast2)
            --
            smm :: (ActElem, ActElem) -> (ActElem, ActElem)
            smm (ActERC (i1,j1), ActERC (i2,j2))
              = (ActERC (min i1 i2, min j1 j2), ActERC (max i1 i2, max j1 j2))
            smm a = a

--------------------------------------------------------------------------------

-- helper
tblEvFiring ∷ forall t. Reflex t ⇒ [TableEvent t] → Event t (TableEvent t)
tblEvFiring l = leftmost evl
  where
    evl ∷ [Event t (TableEvent t)]
    evl = fmap tblEv2Ev l
    tblEv2Ev ∷ TableEvent t → Event t (TableEvent t)
    tblEv2Ev tbl@(TableEvent _me ec ed en) = tbl <$ leftmost [ec,ed,en]

--------------------------------------------------------------------------------

-- | Allows to build a simple state managing behaviour.
-- At most one cell can be active when changing states with actUniq.
-- Note: see the prim-examples on how to use this one.
actUniq ∷ (Reflex t, MonadFix m, MonadHold t m)
        ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actUniq ev ac = mdo
  -- let me = view activeStateMe ac
  let me = _activeStateMe ac
      --eAlreadyActive = ffilter (==True) $ tag (current dActive) ev
      eAlreadyNotActive =ffilter (==False) $ tag (current dActive) ev
      eMe = ffilter (==me) ev
      eNotMe = difference (ffilter (/=me) ev) eAlreadyNotActive
  dActive <- holdDyn False $ leftmost [True <$ eMe, False <$ eNotMe]
  pure $ ac {_activeStateActive = dActive }
  -- pure $ set activeStateActive dActive ac

-- | Allows to build a simple state managing behaviour.
-- Each cell is switchable separately with 'actSwitch'.
-- Note: see the prim-examples on how to use this one.
actSwitch ∷ (Reflex t, MonadFix m, MonadHold t m)
          ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actSwitch ev ac = mdo
  -- let me = view activeStateMe ac
  let me = _activeStateMe ac
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
actGroups ∷  (Reflex t, MonadFix m, MonadHold t m)
          ⇒ Event t ActElem → ActiveState t → m (ActiveState t)
actGroups ev ac = mdo
  -- let me = view activeStateMe ac
  let me = _activeStateMe ac
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


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseDown events and enabling listening
-- those mouseDown events on a group of cells.
actMD ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
      ⇒ Event t (TableEvent t) → TableState t → ActiveState t → m (ActiveState t)
actMD evTbl _tblSt ac = do
    let evD = coincidence $ _teMDown <$> evTbl :: Event t (ActiveState t)
    actMDMUcommon evD ac

-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseUp events and enabling listening
-- those mouseUp events on a group of cells.
actMU ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
      ⇒ Event t (TableEvent t) → TableState t → ActiveState t → m (ActiveState t)
actMU evTbl _tblSt ac = do
    let evU = coincidence $ _teMUp <$> evTbl :: Event t (ActiveState t)
    actMDMUcommon evU ac

-- Common parts of the 'actMD' and 'actMU'.
actMDMUcommon ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
              ⇒ Event t (ActiveState t)
              -- ^ contains the cell info, who is firing an ev.
              → ActiveState t
              -- ^ contains the cell info, whose state we are changing
              → m (ActiveState t) -- ^ Return the changed state
actMDMUcommon ev ac = mdo
    -- let bAvable = current (view activeStateActivable ac)
    let bAvable = current (_activeStateActivable ac)
        -- ev2 = gate bAvable $ fmap (view activeStateMe) ev
        -- ev3 = attachPromptlyDyn (view activeStateListen ac) ev2
        ev2 = gate bAvable $ fmap _activeStateMe ev
        ev3 = attachPromptlyDyn (_activeStateListen ac) ev2
        eMyLstG    = ffilter (\(ActiveGroup ags,ae) -> Set.member ae ags) ev3
        eNotMyLstG = ffilter (\(ActiveGroup ags,ae) -> Set.notMember ae ags) ev3
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eNM = gate bIsActive eNotMyLstG
        eM = gate bIsNot eMyLstG
        -- eNM = traceEvent "Common, eNM" eNM2
    -- dActive <- holdDyn False $ leftmost [True <$ eMyLstG, False <$ eNM]
    dActive <- holdDyn False $ leftmost [True <$ eM, False <$ eNM]
    pure $ ac {_activeStateActive = dActive }
    -- pure $ set activeStateActive dActive ac


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking mouseUp events, and swithing between
-- true and false on each click.
actSwitchMU ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
            ⇒ Event t (TableEvent t) → TableState t
            → ActiveState t
            → m (ActiveState t)
actSwitchMU evTbl _tblSt ac = mdo
    -- let bAvable = current (view activeStateActivable ac)
    let bAvable = current (_activeStateActivable ac)
        -- (_me', _evD, evU, _evE) = coincOnTableEvent evTbl
        --meac = view activeStateMe ac :: ActElem
        meac = _activeStateMe ac :: ActElem
        -- ev = coincidence $ me' <$ evU :: Event t ActElem
        eMe = gate bAvable $ ffilter (==meac) $ _activeStateMe <$>
            coincidence (_teMUp <$> evTbl)
        eMe2T = ffilter (==False) $ tag (current dActive) eMe
        eMe2F = ffilter (==True) $ tag (current dActive) eMe
        bIsActive = current dActive
        bIsNot = fmap not bIsActive
        eMT = gate bIsNot eMe2T
        eMF = gate bIsActive eMe2F
    -- dActive <- holdDyn False $ leftmost [True <$ eMe2T, False <$ eMe2F]
    dActive <- holdDyn False $ leftmost [True <$ eMT, False <$ eMF]
    pure $ ac {_activeStateActive = dActive }
    -- pure $ set activeStateActive dActive ac


-- | State change management working with TableEvent events.
-- ActiveState changes only by tracking both mouseDown and mouseUp events,
-- and swithing between true and false on all cells
-- bounded by the cells firing down and up (and enter) events.
-- This can be used to select a single area from the table. Old
-- selection will be initialized to not-selected when new
-- mouseDown is occurring.
actAreaMDUsel ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
              ⇒ Event t (TableEvent t) → TableState t
              → ActiveState t
              → m (ActiveState t)
actAreaMDUsel = actAreaSelCommon cmnF
  where
    cmnF eInsider eOutsider eMeDown evD evU = do
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
                ⇒ Event t (TableEvent t) → TableState t
                → ActiveState t
                → m (ActiveState t)
actSwitchMDUsel = actAreaSelCommon cmnF
  where
    cmnF eInsider eOutsider eMeDown evD evU = do
        a <- foldDyn (\f b -> f b) (Left False) $
            leftmost [ updLR2ROnMDC <$ eMeDown
                 , updLR2L <$ evD
                 , updLROnEnter <$ eInsider
                 , fromR2L <$ eOutsider
                 , updLR2L <$ evU
                 ]
        pure a


-- Hmm, it is possible to release mouse button on an area between cells.
-- We don't have an event for that situation.
-- This contains the common parts for the 'actAreaMDUsel' and 'actSwitchMDUsel'.
actAreaSelCommon ∷ forall m t. (Reflex t, MonadFix m, MonadHold t m)
                 ⇒ (Event t Bool → Event t Bool → Event t (ActiveState t)
                 → Event t (ActiveState t) → Event t (ActiveState t)
      → m (Dynamic t (Either Bool Bool)))
      → Event t (TableEvent t) → TableState t
      → ActiveState t
      → m (ActiveState t)
actAreaSelCommon cmnF evTbl tblSt ac = do
    -- let bAvable = current (view activeStateActivable ac)
    let bAvable = current (_activeStateActivable ac)
        evTbl2 = gate bAvable evTbl
        (_evMe', evD, evU, _evE) = coincOnTableEvent evTbl2
        dDU = _tsDynUpLDownR tblSt :: Dynamic t (ActiveState t, ActiveState t)
        dDUAE = fmap (_activeStateMe *** _activeStateMe) dDU
            :: Dynamic t (ActElem, ActElem)
        -- dMoLe = _dMOutsideBody tblSt
        -- eMoLe = updated dMoLe
        eMoLe = mouseOutEvent tblSt
        -- meac = view activeStateMe ac :: ActElem
        meac = _activeStateMe ac :: ActElem
        eMeDown = ffilter (\ast -> meac == _activeStateMe ast) evD
    bMoDo <- hold False $ leftmost [True <$ evD, False <$ evU, False <$ eMoLe]
    let dIsInside = fmap (`isInside` meac) dDUAE
        eInsider = gate bMoDo $ ffilter (==True) $ updated dIsInside
        eOutsider = gate bMoDo $ ffilter (==False) $ updated dIsInside
    dActive2 <- cmnF eInsider eOutsider eMeDown evD evU
    let dActive = fmap fromEither dActive2
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

-- Not OK TODO! TODO! TODO! When column and row headers arrive, this should
-- be modified to take into account also those.
isInside ∷ (ActElem,ActElem) → ActElem → Bool
isInside (ActERC (xm,ym), ActERC (xM,yM)) (ActERC (x,y))
    = xm <= x && x <= xM && ym <= y && y <= yM
isInside _ _ = False

--------------------------------------------------------------------------------

-- | This brings in events that are used with tables but not given by "cells".
-- See 'mkTableV' on a use. Helper.
tableEvents ∷ forall t m. (Reflex t, TriggerEvent t m, MonadJSM m
                           , DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Element EventResult (DomBuilderSpace m) t
            → m (TableOtherEvs t)
tableEvents h = do
    let ht = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw h
    eMoEn <- wrapDomEvent ht (`DOM.on` DOM.mouseEnter) DOM.preventDefault
    eMoLe <- wrapDomEvent ht (`DOM.on` DOM.mouseLeave) DOM.preventDefault
    pure $ TableOtherEvs eMoEn eMoLe
    -- let eMoEn = domEvent Mouseenter htTable
    --     eMoLe = domEvent Mouseleave htTable
    -- pure $ TableOtherEvs eMoEn eMoLe

