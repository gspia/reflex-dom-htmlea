{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MainW3 where

import           Control.Arrow                          ((***))
import           Control.Lens
import           Control.Monad                          ((<=<), join, forM)
import           Control.Monad.Fix
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.List                              ( isPrefixOf,inits,union
                                                        , foldl')
import           Data.Semigroup                         ((<>))
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Tree
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                             hiding (mainWidget)
import           Reflex.Dom.Core                        (mainWidget)


--------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs                 as A
import qualified Reflex.Dom.HTML5.Elements              as E
import           Reflex.Dom.HTML5.Component.Tree
import           Reflex.Dom.HTML5.Component.Common.RfpUtils

import           Data.Maybe
import Data.List (nub)

--------------------------------------------------------------------------------


mainW ∷ JSM ()
mainW = mainWidget examples

--------------------------------------------------------------------------------

examples ∷ MonadWidget t m ⇒ m ()
examples = do
    E.h1N $ text "Welcome to reflex-dom-htmlea"
    intro
    caveats
    E.h1N $ text "Simple menu structure"
    -- showSelectContent
    exampleT1
    exampleT2
    exampleT3
    exampleT3b
    exampleT4
    exampleT5
    exampleT6
    exampleT7
    exampleT8a
    exampleT8b
    exampleT8c
    exampleT8d
    exampleT9
    exampleT10
    exampleT11
    exampleT12

intro ∷ MonadWidget t m ⇒ m ()
intro =
    E.divN $ do
        E.pN $ text "Here we give examples on how to use tree-components."
        E.pN $ text $
          "Component-module defines different ways to " <>
          "construct, declare, initialize and use tree-like structures. " <>
          "Those can be used to build menu-structures and drop-down menu. "


caveats ∷ MonadWidget t m ⇒ m ()
caveats = do
    E.h2N $ text "Caveats"
    E.divN $
      E.pN $ text "Renamings are possible..."


--------------------------------------------------------------------------------

showRes ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                       , MonadHold t m, MonadFix m)
        ⇒ Tree Text → CompState t ActNode r → m ()
showRes tree res = do
    let eTxt = (_activeStateElemId . _ceMe) <$> _csCompEvent res
        -- eU = fmap _activeStateElemId $ coincidence $ _ceMUp <$> _csCompEvent res
            -- ::Event t (ActiveState t ActElem)
        -- dUD = _csDynUpLDownR res
        ePth = getPath <$> eTxt
        eNAP = (getNodeAtPath tree) <$> ePth
        eLAP = (isLeafAtPath tree) <$> ePth
    dElm ← holdDyn ActNnone eTxt
    dNAP ← holdDyn Nothing eNAP
    -- The way we use eLAP here is a bit too straightforward, it doesn't
    -- make difference on internal nodes and things not related to tree-nodes.
    -- (E.g when leaving the tree, this says it is an internal node.)
    dLAP ← holdDyn False eLAP
    -- dUpElm ← holdDyn ActNnone eU
    E.pN $ do
        text "Text on last node having some event: "
        dynText $ (T.pack . show) <$> dNAP
        text ". And it is "
        dyn $ ffor dLAP $ \b → do
            if b
                then text "a leaf."
                else text "an internal node."
    E.pN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateElemId) <$> _csDynEnter res
        text "."
    E.pN $ do
        let dUact = _csDynURelease res
        text "Released up on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dUact
        text "."
    E.pN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _csDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _csDynUOn res
        text "."
    let dOut = _csDynMOutsideBody res
        dIn  = _csDynMInsideBody res
    dLInt ∷ Dynamic t Int ← count $ mouseOutEvent res
    dEInt ∷ Dynamic t Int ← count $ mouseInEvent res
    E.pN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
        text "."
    E.pN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt
        text "."


--------------------------------------------------------------------------------

exTree ∷ Tree Text
exTree = Node "Root"
    [ Node "Level 1, n00"
        [ Node "Level 2, n000" []
        , Node "Level 2, n001" []
        , Node "Level 2, n002" []
        ]
    , Node "Level 1, n01" []
    , Node "Level 1, n02"
        [ Node "Level 2, n020"
            [ Node "Level 3, n0200" []
            ]
        , Node "Level 2, n021" []
        ]
    , Node "Level 1, n03"
        [ Node "Level 2, n030"
            [ Node "Level 3, n0300" []
            , Node "Level 3, n0301" []
            , Node "Level 3, n0302" []
            ]
        , Node "Level 2, n031"
            [ Node "Level 3, n0310" []
            , Node "Level 3, n0311" []
            , Node "Level 3, n0312" []
            ]
        , Node "Level 2, n032"
            [ Node "Level 3, n0320" []
            , Node "Level 3, n0321" []
            , Node "Level 3, n0322" []
            ]
        ]
    ]

--------------------------------------------------------------------------------


exampleT1 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT1 = do
    let treeConf2 = silentPlainTreeConf
            & set (treeCRootC . rootfWrapAttr)
              (const $ const $ pure $
                  A.attrMap $ A.style "background-color: lightgrey" E.defNav)
        treeConf = setTreeDraw drawDivContentS treeConf2
    E.h2N $ text "exampleT1"
    E.pN $ text $ "Using li-attributes without events nor node-states. Note "
        <> "that result should not change when clicking or trying to select. "
        <> "Function silentPlainTreeConf can be used when only styling is "
        <> "applied. "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res


--------------------------------------------------------------------------------

exampleT2 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT2 = do
    let lvlFs = defLevelNUlLiFuns & set levelNDraw drawDivContentEx
        rootConf = defRootNavDivFuns & set rootDraw drawDivContentEx
            & set rootfnext lvlFs
        treeConf = defTreeConf 
            & set treeCRootC rootConf
    E.h2N $ text "exampleT2"
    E.pN $ text $ "Using draw-function example that show part of the internal "
        <> "state. The default listener (listenMe) is ok. But for the activity  "
        <> "handling the non-switching default is a bit problematic for trees. "
        <> "The default behaviour is set in the defCommonADEfuns, which is used "
        <> "by the defRootNavDivFuns and devLevelNFuns."
    E.pN $ text $ "This example has non-switching states and one is selected all "
        <> "the time."
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res


--------------------------------------------------------------------------------


exampleT3 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT3 = do
    let fs = CommonADEfuns
            listenMe  -- each node listens only events happening on it
            actSwitchMU -- we want state switching nodes
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlFs = defLevelNUlLiFuns
                & set levelNfADEs fs
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfnext lvlFs
        treeConf = defTreeConf 
            & set treeCRootC rootConf
    E.h2N $ text "exampleT3"
    E.pN $ text $ "This example has switching states, several can be selected."
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res


exampleT3b ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT3b = do
    let fsLN = CommonADEfuns
            listenMe
            -- listenNoOne
            -- actMU
            actSwitchMU
            drawDivContentEx
            -- drawDivActElemEx
            elemEvF
        lvlFsN = defLevelNUlLiFuns & set levelNfADEs fsLN
    let fsL1 = CommonADEfuns
            -- listenMe
            -- (listenListNotMe [ActNpath [0]])
            (listenList [ActNpath [0]])
            -- actMUOnListened
            actMUFollowListened
            -- actSwitchMU
            -- drawDivContentEx
            drawDivActElemEx
            elemEvF
        lvlFs1 = defLevelNUlLiFuns
                & set levelNfADEs fsL1
                & set levelNfnext (Just lvlFsN)
    let fsR = CommonADEfuns
            listenMe
            -- actMU
            actSwitchMU
            drawDivContentEx
            -- drawDivActElemEx
            elemEvF
        rootConf = defRootNavDivFuns & set rootfADEs fsR & set rootfnext lvlFs1
        treeConf = defTreeConf
            & set treeCRootC rootConf
    E.h2N $ text "exampleT3b"
    E.pN $ text $ "Different methods for root, first level and other levels. "
        <> "Example of actMUFollowListened. "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res


--------------------------------------------------------------------------------

exampleT4 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT4 = do
    let fs = CommonADEfuns
            listenMe  -- each node listens only events happening on it
            actAreaMDUsel -- we want state switching nodes
            -- actSwitchMDUsel -- this allows several areas to be selected
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr
            & set rootfnext lvlFs
        treeConf = defTreeConf 
            & set treeCRootC rootConf
    E.h2N $ text "exampleT4"
    E.pN $ text $ "State handling on a user selected area (single area). "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

--------------------------------------------------------------------------------


-- | exampleT5
--
-- If B is used instead of A, then the whole path is activated (and there is no
-- need for C or D).
-- With A and C, the root level node can be activated. If clicked on other nodes,
-- path without the root node is activated.
-- With A and D, the root and 1st level nodes can be activated.
-- If clicked on other nodes, only the path under 1st level is activated
-- With A, C and D, the root node is not activated, unless clicked on it.
-- 
-- TODO B and E' why? E' seems to override B allways! TODO
-- With B and E', the root node cannot be activated.
exampleT5 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT5 = do
    let fs = CommonADEfuns
            -- listenMe  -- A
            (listenMySubTrees exTree) -- B
            actMU -- on mouse up (only one active area)
            -- actSwitchMU -- on mouse up (switching - allows several areas)
            -- act1MUSwitch -- on mouse up (only one active, can be switched off )
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlNFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree) -- D
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                & set levelNfnext (Just lvlNFs)
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree) -- C
        rootConf = defRootNavDivFuns
            -- & set rootfADEs fs  -- E
            & set rootfADEs (set adeListen listenNoOne fs) -- E'
            & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
        treeConf = defTreeConf
            & set treeCRootC rootConf
    E.h2N $ text "exampleT5"
    E.pN $ text $ "Activate the whole path at a time. Note that by combining the "
        <> "sub-tree listeners in a bit different way, it is easy to state "
        <> "that the root-node or the 1st level nodes on a path are not "
        <> "activated. (See the code.) "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

--------------------------------------------------------------------------------


-- | exampleT6
--
-- If only B is used, then the subtree is activated in each non-leaf node
-- (and there is no need for C or D).
-- By using A,C, D, E or E' appropriately, it is possible to not have this 
-- sub-tree activation,
-- e.g., for root node or for 1st level nodes.
--
-- Almost like exampleT5, but only one area can be active at a time and the
-- active area can be de-activated.
-- TODO: the functionality is a bit annoying at the moment, e.g., it is not
-- possible to select a new region right away, but requires two clicks.
-- De-activating an active region works with one click (anywhere), which makes
-- one of those two clicks.
-- See 'act1MUSwitchAbsorb'. Note: it might require larger mod's in order to
-- remove the annoying part.
--
-- The 'act1MUSwitch' "negates the regions" so neither it is ok.
exampleT6 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT6 = do
    let fs = CommonADEfuns
            -- listenMe  -- A
            listenMyAncestors -- B
            -- actMU -- on mouse up (only one active area)
            -- actSwitchMU -- on mouse up (switching - allows several areas)
            -- act1MUSwitch -- on mouse up (only one active, can be switched off )
            act1MUSwitchAbsorb -- on mouse up (only one active, can be switched off)
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlNFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree) -- D
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                & set levelNfnext (Just lvlNFs)
                & set (levelNfADEs . adeListen) (listenMySubTrees exTree) -- C
        rootConf = defRootNavDivFuns 
            & set rootfADEs fs  -- E
            -- & set rootfADEs (set adeListen listenNoOne fs) -- E'
            & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
        treeConf = defTreeConf
            & set treeCRootC rootConf
    E.h2N $ text "exampleT6"
    E.pN $ text $ "Activate the whole subtree at a time, if clicked on a non-leaf. "
        <> "This also has a switching state (you can de-activate the activated "
        <> "nodes). "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    showRes exTree res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

--------------------------------------------------------------------------------

exampleT7 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT7 = do
    let
        fs = CommonADEfuns
            -- listenMe
            (listenMySubTrees exTree)
            actMU -- on mouse up (only one active area)
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr
            & set rootfnext lvlFs
        treeInitC = defTreeConf & set treeCRootC rootConf
        defElm = defActiveStateTree & set activeStateActive (constDyn True)
        initPath = [0,3,0,0] ∷ [Int]
    E.h2N $ text "exampleT7"
    E.pN $ text $ "Initializing (activating) a path."
    rec
        let
            dAstInit ∷ Dynamic t (Maybe [ActiveState t ActNode ()])
                = initializePath defElm <$> dPath
            treeConf = treeInitC & set treeCActivityConf dAstInit
        res ∷ CompState t ActNode () ← mkTree treeConf exTree
        let dUpAst = view csDynURelease res
            ePath = -- traceEvent "ePath" $
                updated $ (getPath . _activeStateElemId) <$> dUpAst
        dPath ∷ Dynamic t [Int] ← holdDyn initPath ePath
    showRes exTree res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

--------------------------------------------------------------------------------

-- TODO TODO
-- This generates spurious events (see traces) with ghc (both on ghc-warp and
-- webkit-gtk). On ghcjs, this shows no extra events.
-- The number of extra events increases with the number of clicks on
-- the buttons and the tree-labels.
-- It is possible, that these are shown only on traces but no real
-- spurious events is ever made.
--
-- TODO part of the event handling is probably done in a bit better in
-- example8d - found out if some of it could be used here.
exampleT8a ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT8a = do
    let fs = CommonADEfuns
            -- listenMe
            listenMyAncestors -- activate sub-trees
            -- (listenMySubTrees exTree) -- activate paths
            actMU -- on mouse up (only one active area)
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr
            & set rootfnext lvlFs
        treeInitC = defTreeConf & set treeCRootC rootConf
        defElmT = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelAll = initializeAllNodes exTree defElmT
        mAstDeSelAll = initializeAllNodes exTree defElmF
    E.h2N $ text "exampleT8a"
    E.pN $ text $ "Initializing (activating) a sub-tree. And external events. "
        <> "This uses widgetHold."
    (evSelAll,_) ← E.button' E.defButton $ text "Select All"
    (evUnSel,_) ← E.button' E.defButton $ text "Un-select All"
    rec
        dAstI2 ← holdDyn mAstDeSelAll
            $ leftmost [ mAstSelAll <$ domEvent Mouseup evSelAll
                       , mAstDeSelAll <$ domEvent Mouseup evUnSel
                       , eAstI
                       ]
        let treeConf ∷ TreeConf t m Text ()
                = treeInitC & set treeCActivityConf dAstI2
        dRes ∷ Dynamic t (CompState t ActNode ()) ←
            widgetHold (mkTree treeConf exTree)
             $ leftmost [ mkTree treeConf exTree <$ domEvent Mouseup evSelAll
                        , mkTree treeConf exTree <$ domEvent Mouseup evUnSel
                        ]
        let dUpAst = join $ _csDynURelease <$> dRes
            ePath = traceEvent "ePath" $ 
                updated $ (getPath . _activeStateElemId) <$> dUpAst
        -- dPath ∷ Dynamic t [Int] ← holdDyn initPath ePath
        let eAstI = initializeSubtree exTree defElmT <$> ePath
    -- widgetHold (showRes exTree defCompState) $
        -- ffor (updated dRes) $ \res → showRes exTree res
    dyn $ ffor dRes $ \res → showRes exTree res
    pure ()
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))


-- TODO TODO
-- This generates spurious events (see traces) with ghc (both on ghc-warp and
-- webkit-gtk). On ghcjs, this shows no extra events.
-- The number of extra events increases with the number of clicks on
-- the buttons and the tree-labels.
-- It is possible, that these are shown only on traces but no real
-- spurious events is ever made.
--
-- TODO part of the event handling is probably done in a bit better in
-- example8d - found out if some of it could be used here.
exampleT8b ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT8b = do
    let fs = CommonADEfuns listenMyAncestors actMU drawDivContentEx elemEvF
        lvlFs = defLevelNUlLiFuns & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr & set rootfnext lvlFs
        treeInitC = defTreeConf & set treeCRootC rootConf
        defElmT = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelAll = initializeAllNodes exTree defElmT
        mAstDeSelAll = initializeAllNodes exTree defElmF
    E.h2N $ text "exampleT8b"
    E.pN $ text $ "Initializing (activating) a sub-tree. And external events. "
        <> "This uses events to choose a dynamic, which is fed into the "
        <> "tree-making function."
    (evSelAll,_) ← E.button' E.defButton $ text "Select All"
    (evUnSel,_) ← E.button' E.defButton $ text "Un-select All"
    let tCI ∷ TreeConf t m Text ()
            = treeInitC & set treeCActivityConf (constDyn mAstDeSelAll)
        dTCSelAll ∷ Dynamic t (TreeConf t m Text ())
            = constDyn $ set treeCActivityConf (constDyn mAstSelAll) $ treeInitC
        dTCDeSelAll ∷ Dynamic t (TreeConf t m Text ())
            = constDyn $ set treeCActivityConf (constDyn mAstDeSelAll) $ treeInitC
    rec
        dAstI ∷ Dynamic t (Maybe [ActiveState t ActNode ()]) ←
            holdDyn mAstDeSelAll eAstI
        dTCUserSel ∷ Dynamic t (TreeConf t m Text ()) ←
            holdDyn tCI ((treeInitC & set treeCActivityConf dAstI) <$ eAstI)
        dTreeConf ∷ Dynamic t (TreeConf t m Text ()) ←
            switcherDyn dTCDeSelAll $
                leftmost [ dTCDeSelAll <$ domEvent Mouseup evUnSel
                         , dTCSelAll   <$ domEvent Mouseup evSelAll
                         , dTCUserSel  <$ eAstI
                         ]
        eRes ∷ Event t (CompState t ActNode ()) ← mkTreeD dTreeConf exTree
        dRes ← holdDyn defCompState eRes
        let dUpAst = join $ _csDynURelease <$> dRes
            eUpPth =
                traceEventWith (const "8b: eUpPth") $
                updated $ (getPath . _activeStateElemId) <$> dUpAst
        -- dUpAst ← holdUniqDynBy (\b c → _activeStateElemId b == _activeStateElemId c)
            --  join $ (view csDynURelease) <$> dRes
        let ePath = traceEvent "ePath" $
                ffilter (/= []) $ eUpPth
        let eAstI ∷ Event t (Maybe [ActiveState t ActNode ()]) =
                traceEventWith (\an → "eAstI" ++
                                   show (fmap (fmap _activeStateElemId) an)
                               ) $
                    initializeSubtree exTree defElmT <$> ePath
    dyn $ ffor dRes $ \res → showRes exTree res
    pure ()
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))



-- TODO TODO
-- This generates spurious events (see traces) with ghc (both on ghc-warp and
-- webkit-gtk). On ghcjs, this shows no extra events.
-- The number of extra events increases with the number of clicks on
-- the buttons and the tree-labels.
-- It is possible, that these are shown only on traces but no real
-- spurious events is ever made.
--
-- TODO part of the event handling is probably done in a bit better in
-- example8d - found out if some of it could be used here.
exampleT8c ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT8c = do
    let fs = CommonADEfuns listenMyAncestors actMU drawDivContentEx elemEvF
        lvlFs = defLevelNUlLiFuns & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr & set rootfnext lvlFs
        treeInitC = defTreeConf & set treeCRootC rootConf
        defElmT = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelAll = initializeAllNodes exTree defElmT
        mAstDeSelAll = initializeAllNodes exTree defElmF
    E.h2N $ text "exampleT8c"
    E.pN $ text $ "Initializing (activating) a sub-tree. And external events. "
        <> "This uses holdDyn's to build a dynamic that is fed for tree-making "
        <> "function."
    (evSelAll,_) ← E.button' E.defButton $ text "Select All"
    (evUnSel,_) ← E.button' E.defButton $ text "Un-select All"
    let tCI ∷ TreeConf t m Text ()
            = treeInitC & set treeCActivityConf (constDyn mAstDeSelAll)
    rec
        let eMAstSelAll = traceEventWith (const "Sel all") $
                mAstSelAll <$ domEvent Mouseup evSelAll
            eMAstDeSelAll = traceEventWith (const "Unsel all") $
                mAstDeSelAll <$ domEvent Mouseup evUnSel
        dAstI2 ∷ Dynamic t (Maybe [ActiveState t ActNode ()]) ←
            holdDyn mAstDeSelAll $ leftmost [ eMAstSelAll, eMAstDeSelAll, eAstI ]
        let eTC ∷ Event t (TreeConf t m Text ())
                = (treeInitC & set treeCActivityConf dAstI2) <$
                    (updated dAstI2)
        dTreeConf ∷ Dynamic t (TreeConf t m Text ()) ← holdDyn tCI eTC
        eRes ∷ Event t (CompState t ActNode ()) ← mkTreeD dTreeConf exTree
        dRes ← holdDyn defCompState eRes
        let dUpAst = join $ _csDynURelease <$> dRes
            eUpPth =
                traceEventWith (const "8c: eUpPth") $
                updated $ (getPath . _activeStateElemId) <$> dUpAst
        -- dUpAst ← holdUniqDynBy (\b c → _activeStateElemId b == _activeStateElemId c)
            --  join $ (view csDynURelease) <$> dRes
        let ePath = traceEvent "ePath" $
                ffilter (/= []) $ eUpPth
        let eAstI ∷ Event t (Maybe [ActiveState t ActNode ()]) =
                -- traceEventWith (\an → "eAstI" ++
                                   -- show (fmap (fmap _activeStateElemId) an)
                               -- ) $
                    initializeSubtree exTree defElmT <$> ePath
    dyn $ ffor dRes $ \res → showRes exTree res
    pure ()
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))


-- TODO TODO
-- This generates spurious events (see traces) with ghc (both on ghc-warp and
-- webkit-gtk). On ghcjs, this shows no extra events.
-- The number of extra events increases with the number of clicks on
-- the buttons and the tree-labels.
-- It is possible, that these are shown only on traces but no real
-- spurious events is ever made.
exampleT8d ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT8d = do
    let fs = CommonADEfuns listenMyAncestors actMU drawDivContentEx elemEvF
        lvlFs = defLevelNUlLiFuns & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
        rootConf = defRootNavDivFuns & set rootfADEs fs
            & set rootfNodeAttr myAAttr & set rootfnext lvlFs
        treeInitC = defTreeConf & set treeCRootC rootConf
        defElmT = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF = defActiveStateTree & set activeStateActive (constDyn False)
        dmAstDeSelAllI = constDyn $ initializeAllNodes exTree defElmF
        tCI ∷ TreeConf t m Text ()
            = treeInitC & set treeCActivityConf dmAstDeSelAllI
    E.h2N $ text "exampleT8d"
    E.pN $ text $ "Initializing (activating) a sub-tree. And external events. "
        <> "This uses dynamic tree items. "
    (evSelAll,_) ← E.button' E.defButton $ text "Select All"
    (evUnSel,_) ← E.button' E.defButton $ text "Un-select All"
    rec
        let dmAstSelAll = (\tr → initializeAllNodes tr defElmT) <$> dExTree
            dmAstDeSelAll = (\tr → initializeAllNodes tr defElmF) <$> dExTree
        dAstI ← holdDyn Nothing eAstI
        dAstI2 ∷ Dynamic t (Maybe [ActiveState t ActNode ()]) ←
            switcherDyn dmAstDeSelAll
                $ leftmost [ dmAstSelAll <$ domEvent Mouseup evSelAll
                           , dmAstDeSelAll <$ domEvent Mouseup evUnSel
                           , dAstI <$ eAstI ]
        let eTC ∷ Event t (TreeConf t m Text ())
                = (treeInitC & set treeCActivityConf dAstI2) <$
                    (updated dAstI2)
        dTreeConf ∷ Dynamic t (TreeConf t m Text ()) ← holdDyn tCI eTC
        eRes ∷ Event t (CompState t ActNode ()) ← mkTreeDD dTreeConf dExTree
        let eeRes ∷ Event t (Event t [Int])
                = updated . (fmap (getPath . _activeStateElemId))
                          . _csDynURelease <$> eRes
        dRes ← holdDyn defCompState eRes
        eUpPth ← switchHold never eeRes
        dUpPth ← holdUniqDyn =<< holdDyn [] eUpPth
        let eUp = traceEvent "eUp" $ updated dUpPth
        let eAny = traceEventWith (const "8d: eAny") $
                   leftmost [ () <$ domEvent Mouseup evSelAll
                   -- mergeWith (\e _ → e) [ () <$ domEvent Mouseup evSelAll
                            , () <$ domEvent Mouseup evUnSel
                            -- , () <$ eUpPth
                            , () <$ eUp
                            ]
        dInt ∷ Dynamic t Int ← foldDyn (+) 0 $ mergeWith (+) [1 <$ eAny]
        let dIntTree = zipDyn dInt (constDyn exTree)
            eAstI ∷ Event t (Maybe [ActiveState t ActNode ()]) =
                attachWith (\exTr pth →
                            initializeSubtree exTr defElmT pth
                           ) (current dExTree) eUp
            eExTree ∷ Event t (Dynamic t (Tree Text))
                = ((\(i,e) → mkExtree i e) <$> dIntTree) <$ eAny
        dExTree ← switcherDyn (constDyn exTree) eExTree
        dyn $ ffor dRes $ \res →
            dyn $ ffor dExTree $ \exTr → showRes exTr res
    pure ()
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

mkExtree ∷ Int → Tree Text → Tree Text
mkExtree i = fmap ((T.pack . show) i <>)

--------------------------------------------------------------------------------


exampleT9 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT9 = do
    let
        fs = CommonADEfuns
            -- listenMe
            -- listenMyAncestors
            (listenMySubTrees exTree)
            actMU -- on mouse up (only one active area)
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlNFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree)
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                & set levelNfnext (Just lvlNFs)
                -- & set levelNfMkLevelNF ownMkLevelN
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree)
        rootConf = defRootNavDivFuns
            -- & set rootfADEs fs
            & set rootfADEs nothingADE
            -- & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
            & set rootfMkLevelNF own9MkLevelN
        treeConf = defTreeConf & set treeCRootC rootConf
    E.h2N $ text "exampleT9"
    E.pN $ text $ "Defining and using own version of mkLevelN for the 1st level "
        <> "of nodes. This example doesn't show the root node "
        <> "(root uses nothingADE). "
    res ∷ CompState t ActNode () ← mkTree treeConf exTree
    -- This example uses the Component's way of delivering events by inserting
    -- "extra nodes" to be delivered from the Tree-component.
    -- We could use other ways here, as well.
    -- E.g. (not tried / thought out very well)
    --   - IOrefs
    --   - modify mkTree a bit to
    --      - use EventWriter
    --      - return information about other events
    --      - etc.
    -- mkTree is a short and simple method (less than 10 lines) so it should be
    -- straightforward to modify it to other use cases.
    let eNd ∷ Event t ActNode = -- traceEvent "eNd" $
            (_activeStateElemId . _ceMe) <$> _csCompEvent res
        eA = ffilter (==ActNpath [998]) eNd
        eB = ffilter (==ActNpath [999]) eNd
        eO = ffilter (\n → n /= ActNpath [998] && n /= ActNpath [999]) eNd
    _ ← widgetHold blank $ leftmost
        [ (E.p (A.style "color:red" E.defP) $ text "Link pressed") <$ eA
        , (E.p (A.style "color:red" E.defP) $ text "Button pressed") <$ eB
        , blank <$ eO -- note, this will be applied also on enter/leave events
        ]
    showRes exTree res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

-- | See 'exampleT9'.
own9MkLevelN ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
       ⇒ LevelNFuns t m a r
       -- ^ Funs and decorations
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode r)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode r
       -- ^ A function combining XX's can use the current 'CompState'.
       -- → (ActiveState t ActNode, a)
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode r, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode r]
own9MkLevelN lvlFs dmlst evB trSt pth treeE = do
    (evA,_) ← E.aN' $ text "my pre-link"
    cas ← mkLevelN lvlFs dmlst evB trSt pth treeE
    (evBtn,_) ← E.buttonN' $ text "and button-after"
    -- This one example, how we can deliver events from link or button
    -- to the caller.
    let astA ∷ ActiveState t ActNode r
             = defActiveStateTree { _activeStateElemId = ActNpath [998] }
        astB ∷ ActiveState t ActNode r
             = defActiveStateTree { _activeStateElemId = ActNpath [999] }
        cevA ∷ Event t (ActiveState t ActNode r) = astA <$ domEvent Mouseup evA
        cevB ∷ Event t (ActiveState t ActNode r) = astB <$ domEvent Mouseup evBtn
        ceA ∷ CompEvent t ActNode r = defCompEvent { _ceMe = astA
                                                 , _ceMUp = cevA
                                                 }
        ceB ∷ CompEvent t ActNode r = defCompEvent { _ceMe = astB
                                                 , _ceMUp = cevB
                                                 }
    pure $ ceA : ceB : cas

--------------------------------------------------------------------------------

-- TODO is there way to make this behave similarly but keeping the state 
-- handling inside tree-component.
exampleT10 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT10 = do
    -- let rootCf = CommonADEfuns listenNoOne actNone own10Draw elemEvF
    let rootCf = CommonADEfuns (listenMySubTrees exTree) actNone own10Draw elemEvF
        lvl1Cf = CommonADEfuns
            -- (listenMySubTrees exTree)
            -- listenMyParent
            listenMe
            -- act1MUSwitchAbsorb 
            -- act1MUSwitch
            -- actMUOnP
            -- actMU
            actSwitchMU own10Draw elemEvF
        lvl1Fs = defLevelNUlLiFuns & set levelNfADEs lvl1Cf
            & set levelNfNodeAttr myLiAttr & set levelNfWrapAttr myUlAttr1
        rootConf = defRootNavDivFuns & set rootfADEs rootCf
            & set rootfNodeAttr myAAttr & set rootfnext lvl1Fs
        treeConf2 = defTreeConf & set treeCRootC rootConf
        defElmT = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelP exLst = initializeAllNodesWith exTree exLst defElmF defElmT
        mAstInit = initializeAllNodesWith exTree [[0],[0,3]] defElmF defElmT
        trc0 = treeConf2 & set treeCActivityConf (constDyn $ mAstSelP [[0],[0,0]])
        trc1 = treeConf2 & set treeCActivityConf (constDyn $ mAstSelP [[0],[0,1]])
        trc2 = treeConf2 & set treeCActivityConf (constDyn $ mAstSelP [[0],[0,2]])
        trc3 = treeConf2 & set treeCActivityConf (constDyn $ mAstSelP [[0],[0,3]])
        tr0 = mkTree trc0 exTree
        tr1 = mkTree trc1 exTree
        tr2 = mkTree trc2 exTree
        -- tr3 = mkTree trc3 exTree
        trI = mkTree trc3 exTree
    E.h2N $ text "exampleT10"
    E.pN $ text $ "Handle events and states outside the component: this is "
        <> "close to the way if not using the tree-component at all. "
        <> "It suits cases where the pre-defined functionalities are not enough. "
    let d03SI ∷ Dynamic t (Set [Int]) = constDyn Set.empty
    rec
        dAstInit ← holdDyn mAstInit (mAstSelP <$> e03P)
        let treeConf = treeConf2 & set treeCActivityConf dAstInit
            trD = mkTree treeConf exTree
        -- bWdgFresh ← hold True $ leftmost [ True <$ eNdEv, False <$ eN03X ]
        -- let evRefresh = traceEvent "evRefresh" $ gate bWdgFresh eN03X
        -- If using evRefresh, there ain't so many spurious events as with
        -- using eN03X directly in widgetHold below.
        -- Note that the initialization isn't correct with evRefresh.
        -- In the 03-branch, the first opened sub-branch will be initialized but
        -- not the next ones.
        -- The extra events seem to occur with ghc, not with ghcjs.
        -- TODO
        dRes ∷ Dynamic t (CompState t ActNode ()) ←
            widgetHold trI $
                leftmost [ tr0 <$ eN00
                         , tr1 <$ eN01
                         , tr2 <$ eN02
                         -- , tr3 <$ eN03
                         -- , trD <$ evRefresh
                         , trD <$ eN03X
                         ]
        let dUpAst = join $ (view csDynURelease) <$> dRes
        -- dUpAst ← holdUniqDynBy (\b c → _activeStateElemId b == _activeStateElemId c)
                    --  join $ (view csDynURelease) <$> dRes
        -- let dUpAst = traceDynWith
                    -- (\d → "dUpAst:" ++ (show $ getPath $ _activeStateElemId d)) $
                    -- join $ (view csDynURelease) <$> dRes
        let ePath = traceEvent "ePath:" $
                ffilter (/= []) $
                updated $ (getPath . _activeStateElemId) <$> dUpAst
            dIsAct = join $ _activeStateActive <$> dUpAst
            eN00 = ffilter (== [0,0]) ePath
            eN01 = ffilter (== [0,1]) ePath
            eN02 = ffilter (== [0,2]) ePath
            -- eN03 = ffilter (== [0,3]) ePath
            eN03X ∷ Event t [Int] = traceEvent "eN03X:" $
                ffilter (\n → [0,3] `isPrefixOf` n && length n > 2) ePath
            eNdEv = leftmost [eN00, eN01, eN02]
        -- eN03Inits is a helper. We probably could do the state keeping 
        -- in a bit less complex way. d03S keeps state below and
        -- e03P is used to initialize the tree.
        -- TODO
        let eN03Inits ∷ Event t [[Int]] =
                fmap (\(bA,p) →
                        let ips = inits p
                            lp = length p
                         in if lp == 3 || lp == 4
                            then if bA
                                 then drop 1 ips
                                 else take (length ips - 2) $ drop 1 ips
                            else if length ips > 1
                                 then drop 1 ips
                                 else ips
                     )
                $ attachPromptlyDyn dIsAct eN03X
        -- Set keeps up information, which nodes are active on 03X-branches.
        -- Note that the path in eN03X is longer than 2 as we have filter above.
        d03S ∷ Dynamic t (Set [Int]) ←
            foldDyn
                (\n swt →
                if Set.member n swt 
                    then Set.delete n swt
                    else if length n /= 3
                            then Set.insert n swt
                            else swt
                ) Set.empty eN03X
        let eSetPrefs = traceEvent "eSetPrefs:" $
                attachPromptlyDyn d03S eN03Inits
        -- e03P has the full path from root to the clicked node (or one 
        -- above in the path) and is used
        -- to build the configuration for the tree. The contents of the
        -- d03S affect on how e03P is made.
        let e03P ∷ Event t [[Int]] = traceEvent "e03P:" $
                fmap (\(s,prf) →
                        let lng = if null prf -- this was clicked
                                     then []
                                     else last prf
                            lng2 = if null lng
                                      then []
                                      else take (length lng - 1) lng
                            s2 = case length lng of
                                    3 → Set.filter (\e → lng `isPrefixOf` e) s
                                    4 → Set.filter (\e → lng `isPrefixOf` e
                                                   || lng2 `isPrefixOf` e
                                                   ) s
                                    otherwise → s
                         in filter (/=[]) $ union prf (Set.toList s2)
                     ) eSetPrefs
    -- showRes exTree res
    dyn $ ffor dRes $ \res → showRes exTree res
    pure ()
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (-- A.setClasses [A.ClassName "show"]
       A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myUlAttr1 ∷ Dynamic t (ActiveState t ActNode ()) -- event
             → ActNode -- what we are making now (this can be found in dAst, too)
             → Dynamic t A.Attr
    myUlAttr1 dAst _an = mkUl <$> join (_activeStateActive <$> dAst)
      where
        -- dact = traceDyn "elem:" $ (_activeStateElemId) <$> dAst
        mkUl ∷ Bool → A.Attr
        mkUl bA =
              if bA
                  then A.attrMap $ A.setClasses [A.ClassName "show"]
                        $ A.style "color: blue" E.defUl
                  else A.attrMap $ A.hidden $ A.style "color: green" E.defUl
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))


-- | See 'exampleT10'.
own10MkLevelN ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
       ⇒ LevelNFuns t m a r
       -- ^ Funs and decorations
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode r)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode r
       -- ^ A function combining XX's can use the current 'CompState'.
       -- → (ActiveState t ActNode, a)
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode r, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode r]
own10MkLevelN lvlFs dmlst evB trSt pth treeE = do
    cas ← mkLevelN lvlFs dmlst evB trSt pth treeE
    pure $ cas



-- | See 'exampleT10'. This borrows most of the code from 'drawDCCommon'.
own10Draw ∷ forall t m e r. (Reflex t, DomBuilder t m, PostBuild t m
                          , DomBuilderSpace m ~ GhcjsDomSpace, ActSelem e
                          , ActSretval r)
                ⇒ Dynamic t Text → ActiveState t e r
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
own10Draw dTxt actS = do
    let dUse = elemAttrs E.defDiv actS
    (e,_) ← E.divD' dUse $
        dynText $ dTxt
    pure (e,defRetval)



tree2map ∷ Tree ([Int],a) → Map [Int] a
tree2map (Node (rP,rV) sf) = foldl' (<>) (rP =: rV) $ fmap tree2map sf

--------------------------------------------------------------------------------

-- Make a textInput to be used inside a tree node.
mkTI ∷ (Reflex t, DomBuilder t m, PostBuild t m
       , DomBuilderSpace m ~ GhcjsDomSpace)
     ⇒ [Int] → m (Dynamic t Text)
mkTI pth = do
    let pTxt = "path " <> (T.pack . show) pth
    ti ← textInput def { _textInputConfig_initialValue = pTxt}
    dynText $ value ti
    pure $ value ti

exTree2 ∷ Tree [Int]
exTree2 =
    Node [0]
        [ Node [0,1] []
        , Node [0,2] []
        , Node [0,3] []
        , Node [0,4] []
        ]

-- Make a tree
mkTItree ∷ (Reflex t, DomBuilder t m, PostBuild t m
          , DomBuilderSpace m ~ GhcjsDomSpace)
        ⇒ Tree [Int] → Tree ([Int], ([Int] → m (Dynamic t Text)))
mkTItree tr = fmap (\p → (p, mkTI)) tr

exampleT11 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT11 = do
    let fs = CommonADEfuns listenMyAncestors act1MUSwitchAbsorb 
            own11Draw elemEvF2
        -- lvlNFs = defLevelNUlLiFuns
                -- & set levelNfADEs fs
                -- & set levelNfNodeAttr myLiAttr
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree)
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                -- & set levelNfnext (Just lvlNFs)
                -- & set levelNfnext Nothing
                & set (levelNfADEs . adeListen) listenMe
        rootConf = defRootNavDivFuns
            & set rootfADEs fs
            & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
        treeConf = defTreeConf & set treeCRootC rootConf
            ∷ TreeConf t m ([Int], [Int] → m (Dynamic t Text)) Text
        exTr ∷ Tree ([Int], ([Int] → m (Dynamic t Text))) = mkTItree exTree2
    E.h2N $ text "exampleT11"
    E.pN $ text $ "A tree of TextInputs. We have to define our own drawing "
        <> "for this. "
    res ∷ CompState t ActNode Text ← mkTree treeConf exTr
    let evRV ∷ Event t (Dynamic t Text)
            = (_activeStateRetVal . _ceMe) <$> _csCompEvent res
        evId = (T.pack . show . _activeStateElemId . _ceMe) <$> _csCompEvent res
    dTxt ← switcherDyn (constDyn "(-)") evRV
    dId ← holdDyn "(-)" evId
    E.p ( A.style "color: red; background-color: lightgray" E.defP ) $ do
        text "Text was modified at "
        dynText dId
        text ". and now it is: "
        dynText dTxt
  where
    myLiAttr ∷ ActiveState t ActNode Text → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode Text → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))

own11Draw ∷ forall t m e . (Reflex t, DomBuilder t m, PostBuild t m
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSelem e, MonadHold t m) -- , ActSretval r)
                ⇒ Dynamic t ([Int], ([Int] → m (Dynamic t Text)))
                → ActiveState t e Text
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t Text)
own11Draw dPtiF actS = do
    let dUse = elemAttrs E.defDiv actS
        dP = (\(r,_) → r) <$> dPtiF
        dPt = T.pack . show <$> dP
        dPt2 = ("Path: " <>) <$> dPt
    (e,edResTxt ∷ Event t (Dynamic t Text)) ← E.divD' dUse $ do
        dynText dPt2
        dyn $ ffor dPtiF $ \(p,tiF) → tiF p
    dResTxt ← switcherDyn (constDyn "") edResTxt
    pure (e, dResTxt)


--------------------------------------------------------------------------------

-- Make a Tree [Int] to be used inside a tree node.
mkTrI ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m
                    , TriggerEvent t m, MonadJSM m, MonadFix m
                    , DomBuilderSpace m ~ GhcjsDomSpace)
      ⇒ [Int] → m (Dynamic t Text)
mkTrI pth = do
    let txt = "(-)" ∷ Text
        exTr = Node pth [ Node (pth ++ [p]) [] | p ← [0..3::Int]]
    -- let fsLN = CommonADEfuns
            -- listenMe
            -- listenNoOne
            -- actMU
            -- actSwitchMU
            -- drawDivContentEx
            -- drawDivActElemEx
            -- elemEvF
        -- lvlFsN = defLevelNUlLiFuns & set levelNfADEs fsLN
                -- & set levelNfNodeAttr myLiAttr
                -- & set levelNfWrapAttr myUlAttr
    let fsL = CommonADEfuns
            -- listenMe
            (listenList [ActNpath [0]])
            -- listenNoOne
            -- listenMyAncestors
            actMUFollowListened
            -- act1MUSwitchAbsorb
            drawDivContentEx
            -- drawDivActElemEx
            elemEvF
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fsL
                & set levelNfNodeAttr myLiAttr
                & set levelNfWrapAttr myUlAttr
                -- & set levelNfCombFun exComb
                -- & set levelNfnext (Just lvlFsN)
                -- & set levelNfnext Nothing
        fsR = CommonADEfuns
            listenMe
            -- listenNoOne
            -- listenMyAncestors
            -- act1MUSwitchAbsorb
            actSwitchMU
            -- drawDivContentEx
            drawDivActElemEx
            elemEvF
        rootConf = defRootNavDivFuns
            & set rootfADEs fsR
            & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
            -- & set rootfCombFun exComb
        treeConf = defTreeConf & set treeCRootC rootConf
            -- ∷ TreeConf t m ([Int], [Int] → m (Dynamic t Text)) ()
    res ∷ CompState t ActNode () ← mkTree treeConf exTr
    let
        evId =
            (("Internal tree " <>) . T.pack . show . _activeStateElemId . _ceMe)
            <$> _csCompEvent res
    dId ← holdDyn txt evId
    pure dId
  where
    myUlAttr ∷ Dynamic t (ActiveState t ActNode ()) -- event
             → ActNode -- what we are making now (this can be found in dAst, too)
             → Dynamic t A.Attr
    myUlAttr dAst _an = mkUl <$> join (_activeStateActive <$> dAst)
      where
        mkUl ∷ Bool → A.Attr
        mkUl bA =
              if bA
                  then A.attrMap $ -- A.setClasses [A.ClassName "show"] $
                        A.style "color: brown" E.defUl
                  else A.attrMap $ -- A.hidden $
                        A.style "color: green" E.defUl
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: brown" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: green" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl ( -- A.setClasses [A.ClassName "show"] $
                                     A.style "color: cyan" $ pure A.defGlobals)
      & set activeStateNotActiveGl ( -- A.hidden $
                                   A.style "color: yellow" $ pure A.defGlobals
                                   ))


mkTItree2 ∷ (Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m
            , TriggerEvent t m, MonadJSM m, MonadFix m
            , DomBuilderSpace m ~ GhcjsDomSpace)
          ⇒ Tree [Int] → Tree ([Int], ([Int] → m (Dynamic t Text)))
mkTItree2 tr = fmap (\p → (p, mkTrI)) tr


exampleT12 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT12 = do
    let txt = "Root" ∷ Text
    let fs = CommonADEfuns listenMyAncestors act1MUSwitchAbsorb 
            own12Draw elemEvF2
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                & set levelNfNodeAttr myLiAttr
                -- & set levelNfnext (Just lvlNFs)
                -- & set levelNfnext Nothing
                & set (levelNfADEs . adeListen) listenMe
        rootConf = defRootNavDivFuns 
            & set rootfADEs fs
            & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
        treeConf = defTreeConf & set treeCRootC rootConf
            ∷ TreeConf t m ([Int], [Int] → m (Dynamic t Text)) Text
        exTr ∷ Tree ([Int], ([Int] → m (Dynamic t Text))) = -- mkTItree2 exTree2
            Node ([0], const (pure $ constDyn txt))
                [ Node ([0,0], mkTrI) []
                , Node ([0,1], mkTrI) []
                ]
    E.h2N $ text "exampleT12"
    E.pN $ text $ "A tree of trees. We have to define our own drawing "
        <> "for this. "
    res ∷ CompState t ActNode Text ← mkTree treeConf exTr
    let evRV ∷ Event t (Dynamic t Text)
            = (_activeStateRetVal . _ceMe) <$> _csCompEvent res
        evId = (T.pack . show . _activeStateElemId . _ceMe) <$> _csCompEvent res
    dTxt ← switcherDyn (constDyn "(-)") evRV
    dId ← holdDyn "(-)" evId
    E.p ( A.style "color: red; background-color: lightgray" E.defP ) $ do
        text "Event at "
        dynText dId
        text ". The delivered text is : "
        dynText dTxt
        text $ "Note how the element id refers to the outer tree while "
            <> "payload is delevered with the retval's."
  where
    myLiAttr ∷ ActiveState t ActNode Text → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: blue" $ pure A.defGlobals))
    myAAttr ∷ ActiveState t ActNode Text → Dynamic t A.Attr
    myAAttr ast = defALeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: blue" $ pure A.defGlobals))


own12Draw ∷ forall t m e . (Reflex t, DomBuilder t m, PostBuild t m
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSelem e, MonadHold t m) -- , ActSretval r)
                ⇒ Dynamic t ([Int], ([Int] → m (Dynamic t Text)))
                → ActiveState t e Text
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t Text)
own12Draw dPtiF actS = do
    let dUse = elemAttrs E.defDiv actS
        dP = (\(r,_) → r) <$> dPtiF
        dPt = T.pack . show <$> dP
        dPt2 = ("Path: " <>) <$> dPt
    (e,edResTxt ∷ Event t (Dynamic t Text)) ← E.divD' dUse $ do
        dynText dPt2
        dyn $ ffor dPtiF $ \(p,tiF) → tiF p
    dResTxt ← switcherDyn (constDyn "") edResTxt
    pure (e, dResTxt)






--------------------------------------------------------------------------------

{-

data MenuTag = LangChoice | LangGb | LangDe | LangSe
  deriving (Eq, Show, Ord)

instance ActSretval MenuTag where defRetval = LangGb


-- showSelectContent ∷ forall t m. MonadWidget t m ⇒ m ()
showSelectContent ∷ forall t m. (MonadWidget t m) ⇒ m ()
showSelectContent = do
    evPB ← getPostBuild
    let trDD = miDD -- ∷ Tree (MenuItemConfig t DDtags DDgroups)
        dLa = constDyn LaGB
    let eDDdef = tag (current (getDDMiLangMap <$> dLa)) evPB
        eUiLangdef = tag (current dLa) evPB
    dLangDD ← holdDyn (getDDMiLangMap LaGB) $ leftmost [eDDdef]
    E.h2N $ text "A selection example (drowdowns)"
    E.pN $ do
        text "Some text "
    let  dDd2 = constDyn [Select1DD]
    eDDtCSEv ← mkDropdown trDD dDd2 dLangDD updLang
    eDDt2 ← compStateEv2TagEv eDDtCSEv (Right trDD)
    let eDDt = ffilter (/= SelectDD) eDDt2 
    dDDt ← holdDyn Select0DD $ leftmost [eDDt] 
    let dDDtxt = fmap (T.pack . show) dDDt
    E.pN $ do
        text "And the selected dropdown item was: "
        dynText dDDtxt
    blank










mkDropdown ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a,
                                  DomBuilderSpace m ~ GhcjsDomSpace)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t [a]
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkDropdown trMic dInitElms dLang updLang = do
    evPB ← getPostBuild
    lrTree ∷ Tree (MenuItem t a) ← micTr2miTr trMic
    let ogs ∷ Map b [a] = findOneOfGroups (trMic ∷ Tree (MenuItemConfig t a b))
    let fs1Lvl = CommonADEfuns listenMe actMU drawDDMI elemEvF
        fsNLvl = CommonADEfuns listenMe actMU drawDDMI elemEvF
        fsRoot = CommonADEfuns listenMe actMU drawRootI elemEvF
        rootConf = defRootDivButtonFuns
            & set rootfADEs fsRoot 
            & set rootfNodeAttr buttonRootNAttrs
            & set rootfWrapAttr divRootWAttrs
            & set rootfnext lvl1Fs
            & set rootfMkLevelNF customMkLevelDD
        lvl1Fs = defLevelNDivAFuns
            & set levelNfADEs fs1Lvl
            & set levelNfNodeAttr aAttr
            & set levelNfWrapAttr (divAttr lrTree)
            & set levelNfnext (Just lvlNFs)
            & set levelNfMkLevelNF customMkLevelDD
        lvlNFs = defLevelNDivAFuns --
            & set levelNfADEs fsNLvl
            -- & set levelNfnext (Just lvlNFs)
            -- & set levelNfNodeAttr aAttr
            -- & set levelNfWrapAttr (divAttr lrTree)
            -- & set levelNfNodeAttr aLvl1Attrs
            -- & set levelNfWrapAttr divLvl1Attrs
            -- & set levelNfWrapAttr divAttr 
        -- lvl1FsB = setLevenNDraws drawDivContentEx lvl1Fs
        treeConf ∷ TreeConf t m (MenuItem t a) (MenuItem t a)
            = defTreeConf  & set treeCRootC rootConf
        defElmT ∷ ActiveState t ActNode (MenuItem t a)
            = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF ∷ ActiveState t ActNode (MenuItem t a)
            = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelP exLst = initializeAllNodesWith lrTree exLst defElmF defElmT
        mAstNoSels = initializeAllNodes lrTree defElmT
        pthTree ∷ Tree [Int] = mkPathsTree lrTree
        pthTagTree ∷ Tree (a, [Int])
            = biTreeMap (\mi pth → (_menuItemMenuTag mi,pth)) lrTree pthTree
        parsOOs ∷ Map b (a,[Int])
            = findParents4OneOfs
                (trMic ∷ Tree (MenuItemConfig t a b))
                $ findParents pthTagTree
        prPths ∷ [[Int]] = fmap snd . Map.elems $ parsOOs
        -- chPths ∷ [[Int]] = fmap ( (++ [0]) . snd) . Map.elems $ parsOOs
        pthTagLst = flatten pthTagTree
        diNds = fmap (\initElms → filter (\(tg,_pth) → tg `elem` initElms) pthTagLst
                    ) dInitElms
        initPth ∷ [[Int]] = [] -- Initial menu selection
        -- initPth ∷ [[Int]] = [[0]] -- Initial menu selection
        dInitPth ∷ Dynamic t [[Int]] -- Initials, ch. Menu
            = fmap (fmap snd) diNds -- Initial menu selection
        mAstInit = initializeAllNodesWith lrTree initPth defElmF defElmT
        dLrTree = constDyn lrTree
        -- dTreeConf = constDyn (treeConf & set treeCActivityConf (constDyn mAstInit))
        dmAstDeSelAllI = constDyn $ initializeAllNodes lrTree defElmF
        tCI ∷ TreeConf t m (MenuItem t a) (MenuItem t a)
             = treeConf & set treeCActivityConf dmAstDeSelAllI
        -- initState ∷ Map [Int] [Int]
            -- = Map.fromList $ zip prPths chPths
        dInitState ∷ Dynamic t (Map [Int] [Int])
            = fmap (\ipth → Map.fromList $ zip prPths ipth) dInitPth
    -- display dInitPth
    rec
        -- let ePth = [[0,1]] <$ evPB
        -- let ePth = [[0,7],[0,7,1]] <$ evPB
        dOth ∷ Dynamic t [[Int]] ← holdDyn initPth eOth
        -- let dOth ∷ Dynamic t [[Int]] = dInitPth -- ch. Menu
        -- dOth ∷ Dynamic t [[Int]]
            -- ← switcherDyn dInitPth (dInitPth <$ evPB)
            -- ← switcherDyn dInitPth (dInitPth <$ ePar)
            -- ← switcherDyn dInitPth (constDyn [[0]] <$ ePar)
            -- ← switcherDyn (constDyn initPth) ( dInitPth <$ ePar)
        -- display dOth
        dAInit ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                ← holdDyn mAstNoSels $
                    leftmost [ mAstInit <$ evPB
                             , eAstI
                             ]
        -- let dAInit ∷ Event t (Maybe [ActiveState t ActNode (MenuItem t a)])
                -- = mAstSelP <$> eAstI
        let dAInit2 ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                = mAstSelP <$> dOth
            -- dAInit2 = zipDynWith (\b mb →
                                 -- if b
                                     -- then mb
                                     -- else Nothing
                                 -- ) dParVis dAInit3
        -- let dInitPth = fmap (fmap (fmap _activeStateElemId)) dAInit
        -- let dInitB ∷ Dynamic t (Maybe [Dynamic t Bool])
                -- = fmap (fmap (fmap _activeStateActive)) dAInit
            -- dInitB2  ∷ Dynamic t [Dynamic t Bool] = fmap (\ml →
                           -- case ml of
                               -- Just l → l
                               -- Nothing → []
                           -- ) dInitB
        -- display dInitPth
        -- dyn $ ffor dInitB2 $ \lstD → do
            -- mapM (\eD → dynText (T.pack . show <$> eD)) lstD
        let dLrTree2 = fmap (fmap (updLang dLang)) dLrTree
            -- dTreeConf2 = fmap (set treeCActivityConf dAInit) dTreeConf
        dAstI2 ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)]) ←
            switcherDyn (constDyn mAstNoSels)
            -- switcherDyn dAInit2
                $ leftmost [ dAInit2 <$ eOOChPth
                           , dAInit <$ eAstI
                           , dAInit2 <$ evPB -- ch. Menu
                           ]
        let eTC ∷ Event t (TreeConf t m (MenuItem t a) (MenuItem t a))
                = -- traceEventWith (const "eTC") $
                    (treeConf & set treeCActivityConf dAstI2) <$ (updated dAstI2 )
                -- = (treeConf & set treeCActivityConf dAstI2) <$ (updated dAInit )
                -- = (treeConf & set treeCActivityConf dAInit) <$ (updated dAInit )
        dTreeConf ∷ Dynamic t (TreeConf t m (MenuItem t a) (MenuItem t a))
            ← holdDyn tCI eTC
        -- display dParVis
        eCsAnMi ∷ Event t (CompState t ActNode (MenuItem t a))
            ← mkTreeDD dTreeConf dLrTree2
        let eCe ∷ Event t (Event t [Int]) =
                -- traceEventWith (const "eCe") $
                  (coincidence . fmap (fmap (getPath . _activeStateElemId)))
                  . fmap _ceMUp . _csCompEvent <$> eCsAnMi
        eUpd ∷ Event t [Int] ← switchHold never eCe
        let eMt ∷ Event t a = -- traceEventWith (\a → "eMt:" ++ show a) $
                fmap _menuItemMenuTag $ fmapMaybe (getNodeAtPath lrTree) eUpd
        let eTgPth ∷ Event t (a, [Int]) =
                -- traceEventWith (\a → "eTgPth: " ++ show a) $
                fmap (\mt → head $ filter ((mt ==) . fst)
                                   (flatten pthTagTree)
                      ) eMt
        let eListOneOfs ∷ [Event t (a, [Int])] = evs4OneOfs2 ogs eTgPth
            eOO = leftmost eListOneOfs
            eOOChPth ∷ Event t [[Int]] = -- traceEvent "eOOChPth" $
                fmap ((:[]).snd) eOO
            eParEvs ∷ [Event t (a, [Int])] = parentEvents parsOOs eTgPth
            ePar = -- traceEvent "ePar" $
                leftmost eParEvs
            ePar2 = fmap ( (\p → drop 1 $ inits p) . snd ) ePar
            eUpd2 = fmap (:[]) eUpd
            eOth = -- traceEvent "eOth" $
                difference eUpd2 $ leftmost [ePar2,eOOChPth]
        dParVis ← toggle False $ leftmost [() <$ ePar, () <$ eOO]
        let eStateMap ∷ Event t (Map [Int] [Int]) =
              attachPromptlyDynWith
                (\swt n →
                let par = reverse . drop 1 . reverse $ n
                 in Map.alter (const $ Just n) par swt
                ) dInitState (fmap snd eOO)
        dStateMap ← holdDyn Map.empty eStateMap
        let dStateMap2 = zipDynWith
                (\swt n →
                let par = reverse . drop 1 . reverse $ n
                 in Map.alter (const $ Just n) par swt
                ) dInitState (fmap head dInitPth)
        dStateMap3 ← switcherDyn dStateMap2 (dStateMap <$ eOO)
        -- display dStateMap
        let eParII = -- traceEvent "eParII" $
                attachPromptlyDynWith
                (\mp par →
                    case Map.lookup par mp of
                        -- Just p → [[0],[0,7],[0,7,1]]
                        Just p → drop 1 $ inits p
                        Nothing → [par]
                ) dStateMap3 (fmap snd ePar)
            dTrB = zipDyn dLrTree2 dParVis
            eAstI ∷ Event t (Maybe [ActiveState t ActNode (MenuItem t a)]) =
                -- traceEventWith (const "eAstI") $
                attachWith (\(exTr,b) pth →
                            if b -- why this way, it feels a bit un-intuitive TODO
                                then Nothing
                                else initializeAllNodesWith exTr pth defElmF defElmT
                           -- ) (current dLrTree2) eParII
                           ) (current dTrB) eParII
    -- let eIsAct ∷ Event t (Dynamic t Bool) =
    --         coincidence $
    --         fmap (_activeStateActive . _ceMe ) . _csCompEvent <$> eCsAnMi
    -- ddIsAct ← holdDyn (constDyn False) eIsAct
    -- let dIsAct = join ddIsAct
    pure eCsAnMi
  where
    divRootWAttrs ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
         → ActNode → Dynamic t A.Attr
    divRootWAttrs _dAst _an = pure $ A.attrMap $  A.setClasses
        [A.ClassName "B.dropdown"] E.defDiv
    buttonRootNAttrs ∷ ActiveState t ActNode (MenuItem t a)
         → Dynamic t A.Attr
    buttonRootNAttrs ast = pure (A.attrMap
                                  ( A.setClasses [A.ClassName "button"]
                                  $ A.aHaspopup "true" $ A.aControls "dropdown-menu"
                                  $ E.defButton)
                                )
    -- Next one is for drop-down menu items (that is, level 2 or further).
    -- This is for the node elements that are individual items.
    aAttr ∷ ActiveState t ActNode (MenuItem t a)
          → Dynamic t A.Attr
    aAttr ast = pure ("href" =: "#") <> defALeafNodeAttrF
        (ast & set activeStateActiveGl
          (A.setClasses [A.ClassName "B.dropdown-item", A.ClassName "B.isActive"] $ pure A.defGlobals)
          -- (pure A.defGlobals)
           & set activeStateNotActiveGl
          ( A.setClasses [A.ClassName "B.dropdown-item"] $ pure A.defGlobals)
          -- ( pure A.defGlobals)
        )
    -- Next one is for drop-down menu items (that is, level 2 or further).
    -- This is for the wrapping element that collects items.
    divAttr ∷ Tree (MenuItem t a)
            → Dynamic t (ActiveState t ActNode (MenuItem t a))
            -- last node where event occurred
            → ActNode
            -- what we are now making
            → Dynamic t A.Attr
    divAttr trA dAst an = fmap (\cls → A.attrMap $ A.setClasses cls
                               --  A.aLabelledby lblName
                               --  A.id_ "dropdown-menu"
                               --  A.rMenu
                               $ E.defDiv
                               ) dCls
      where
        dCls = fmap (\b → if b then [A.ClassName "B.dropdownContent, B.isActive"]
                               else [A.ClassName "B.dropdownContent"]
                    ) dIsAct
        dIsAct = join $
              (\an2 → if an == _activeStateElemId an2
                          then _activeStateActive an2
                          else constDyn False
              ) <$> dAst 
        mNd = getNodeAtPath trA (getPath an)
        lblName = -- "dropdown" <> (T.pack . show . getPath $ an)
            case mNd of
              Just nd → "dropdown" <> (T.pack . show $ _menuItemMenuTag nd)
              -- Just nd → "dropdownMenuLink"
              Nothing → "dropdownUnknown"




--------------------------------------------------------------------------------

drawRootI ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawRootI dMic actS = do
    let -- dTxt = (T.pack . show . _menuItemMenuTag) <$> dMic
        dIco = _menuItemIcons <$> dMic
        -- dIsDD = _menuItemDD <$> dMic -- True indicates a dropdown
        -- dActive = _activeStateActive actS
        -- pth = getPath $ _activeStateElemId actS
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    (e,_) ← E.spanN' $ do
        dynText dLbl -- how to get I18n to work?
    E.span (A.setClasses [A.ClassName "icon"] E.defSpan) $ do
        E.i ( A.setClasses [A.ClassName "fas"]
            $ A.aHidden "true" E.defI) blank
    pure (e, dMic)


drawDDMI ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawDDMI dMic actS = do
    let -- dTxt = (T.pack . show . _menuItemMenuTag) <$> dMic
        dIco = _menuItemIcons <$> dMic
        -- dIsDD = _menuItemDD <$> dMic -- True indicates a dropdown
        -- dActive = _activeStateActive actS
        -- pth = getPath $ _activeStateElemId actS
        -- dLblName =
        --     (("dropdown" <> ) . T.pack . show . _menuItemMenuTag) <$> dMic
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
        dIco2 = fmap (\ico → if null ico then E.defSpan else A.setClasses ico E.defSpan) dIco
    (e,_) ← E.spanD' dIco2 blank
    dynText dLbl
    -- text "hmm DDMI"
    pure (e, dMic)

-- Hmm, this can be found from Menu.
drawMILeaf ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                                                 , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawMILeaf dMic actS = do
    let dUse = elemAttrs E.defDiv actS
        dIco = _menuItemIcons <$> dMic
        dActive = _activeStateActive actS
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    -- (e,_) ← drawDivWdt (constDyn ) dMic actS
    -- (e,_) ← drawDivContentS dMic actS
    -- (e,_) ← drawDivActElemEx dMic actS
    E.brN blank
    text "drawMILeaf"
    E.brN blank
    (e,_) ← E.divD' dUse $ do
        dyn $ ffor dIco $ \ico → do
            if null ico
               then blank
               else E.span (A.setClasses ico E.defSpan) blank
        dynText dLbl
        text "hmm MILeaf"
        -- dyn $ ffor dActive $ \b →
        --     if b
        --        then E.span (A.setClasses [bsSrOnly] E.defSpan) $ text "(current)"
        --        else blank
    pure (e, dMic)

--------------------------------------------------------------------------------


customMkLevelDD ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, Eq a, Show a
     , ActSretval a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
       ⇒ LevelNFuns t m a a
       -- ^ Funs and decorations
       →  Dynamic t (Maybe [ActiveState t ActNode a])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode a)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode a
       -- ^ A function combining XX's can use the current 'CompState'.
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode a, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode a]
customMkLevelDD lvlFs dmlst evB trSt pth treeE = do
    let (anR ∷ ActiveState t ActNode a , dR ∷ Dynamic t a ) = rootLabel treeE
        dLbl = (T.pack . show) <$> dR
        isAct = _activeStateActive anR
    text "customMkLevelDD, dLbl = "
    dynText dLbl
    rec
        -- dOpen ← toggle False $ leftmost [() <$ _ceMUp ceB, () <$ eClose]
        dOpen ← toggle False $ leftmost [() <$ eClose]
        let -- sf = subForest treeE
            dDivCl =
                fmap (\o →
                     if o
                         then A.setClasses [A.ClassName "dropdown-menu", A.ClassName "is-active"]
                                         $ A.id_ "dropdown-menu"
                                         $ A.rMenu E.defDiv
                         else A.setClasses [A.ClassName "dropdown-menu"]
                                         $ A.id_ "dropdown-menu"
                                         $ A.rMenu E.defDiv
                     ) dOpen
        let eCas = () <$ leftmost (fmap _ceMUp $ concat cas)
            eClose = gate (current dOpen)
                $ leftmost [ eCas ]
        cas ∷ [[CompEvent t ActNode a]] ← 
                E.divD dDivCl $ 
                        mapM (mkLevelN lvlFs dmlst evB trSt pth) [treeE]
                        -- mapM ( mkLevelN lvlFs dmlst evB trSt pth ) $ subForest treeE
    -- pure $ ceA : ceB : concat cas
    pure $ concat cas

--------------------------------------------------------------------------------





type LangMap a = Map.Map a Text

mtLabel :: Ord a => a -> LangMap a -> Maybe Text
mtLabel = Map.lookup



data MenuItemConfig t a b = MenuItemConfig
    { _menuItemConfigMenuTag ∷ a
    , _menuItemConfigIcons ∷ [A.ClassName]
    , _menuItemConfigSetLabel ∷ Event t Text
    -- , _menuItemConfigDD ∷ Bool
    -- ^ True, if it is a drop down
    , _menuItemConfigOneOfGroup ∷ Maybe b
    -- ^ Only one of the items in the same group can be active at a time.
    }
    | MICSeparator
      { _MICSeparatorIcons ∷ [A.ClassName]
      }

instance Eq a ⇒ Eq (MenuItemConfig t a b) where
  (==) (MenuItemConfig t1 _ _ _) (MenuItemConfig t2 _ _ _) = t1 == t2
  (==) (MICSeparator _) (MICSeparator _) = True
  (==) _ _ = False

instance Show a ⇒ Show (MenuItemConfig t a b) where
  show (MenuItemConfig t1 _ _ _) = show t1
  show (MICSeparator _) = "mic sep"

instance (Reflex t, ActSretval a) ⇒ ActSretval (MenuItemConfig t a b) where
    defRetval = defMenuItemConfig defRetval

-- | Default value with input: no icons, events, is leaf, belongs to no groups.
defMenuItemConfig ∷ (Reflex t) ⇒ a → MenuItemConfig t a b
defMenuItemConfig a = MenuItemConfig a [] never Nothing

--------------------------------------------------------------------------------

data MenuItem t a = MenuItem
    { _menuItemMenuTag ∷ a
    , _menuItemIcons ∷ [A.ClassName]
    , _menuItemDD ∷ Bool -- True, if it is a drop down
    , _menuItemLabel ∷ Dynamic t Text
    }
    | MenuItemSep
      { _menuItemSepIcons ∷ [A.ClassName]
      }

instance Eq a ⇒ Eq (MenuItem t a) where
    (==) (MenuItem t1 _ _ _ ) (MenuItem t2 _ _ _) = t1 == t2
    (==) (MenuItemSep _) (MenuItemSep _) = True
    (==) _ _ = False
instance Show a ⇒ Show (MenuItem t a) where
  show (MenuItem t1 _ _ _) = show t1
  show (MenuItemSep _) = "mi sep"

instance (Reflex t, ActSretval a) ⇒ ActSretval (MenuItem t a) where
    defRetval = defMenuItem defRetval

-- | Default 'MenuItem' for tag @a@.
defMenuItem ∷ (Reflex t) ⇒ a → MenuItem t a
defMenuItem a = MenuItem a [] False (constDyn "")

--------------------------------------------------------------------------------

mic2mi ∷ (Reflex t, MonadHold t m, Show a)
       ⇒ MenuItemConfig t a b
       → m (MenuItem t a)
mic2mi mic2@MenuItemConfig{} = do
    dLbl ← holdDyn (T.pack . show $ _menuItemConfigMenuTag mic2) $
            _menuItemConfigSetLabel mic2
    pure $
        MenuItem
            (_menuItemConfigMenuTag mic2)
            (_menuItemConfigIcons mic2)
            False -- (_menuItemConfigDD mic2)
            dLbl
mic2mi (MICSeparator ico) = pure $ MenuItemSep ico

-- | This builds up a MenuItem tree from configs.
micTr2miTr ∷ (Reflex t, MonadHold t m, Show a)
                ⇒ Tree (MenuItemConfig t a b)
                → m (Tree (MenuItem t a))
micTr2miTr tr = do
    tr2 ← mapM mic2mi $ tr
    let ntr = mapTree (\r s → if null s
                                  then r
                                  else case r of
                                        MenuItem t i _d l → MenuItem t i True l
                                        MenuItemSep _     → r
                       ) tr2
        rl = rootLabel ntr
        sf = subForest ntr
        ret = case rl of
                MenuItem t i _d l → MenuItem t i False l
                MenuItemSep _     → rl
    pure $ Node ret sf -- don't mark the root node

mapTree ∷ (a → [Tree a] → b) → Tree a → Tree b
mapTree f tr = Node (f rl sf) (fmap (mapTree f) sf)
  where
    rl = rootLabel tr
    sf = subForest tr


-- | Collect from a configuration tree information about
-- groups, that is, each indivual group as a map element where
-- the key is the group name (user given tag @b@) and
-- value is a list of menutag (also user given @a@).
findOneOfGroups ∷ forall t a b. (Eq b, Ord b)
                ⇒ Tree (MenuItemConfig t a b) → Map b [a]
findOneOfGroups tr = Map.fromList $ zip uniqBs bGrps
  where
    -- bWithGroup = zip uniqBs bGrps
    bGrps = fmap (\b →
                    fmap _menuItemConfigMenuTag
                    $ filter ((==b) . fromJust
                                     . _menuItemConfigOneOfGroup
                              ) grps ) uniqBs
    uniqBs ∷ [b] = nub $ fromJust . _menuItemConfigOneOfGroup <$> grps
    grps = filter (isJust . _menuItemConfigOneOfGroup) . flatten $ tr


-- | Gives a Map where each node (except root) is a key
-- with parent node (menutag,path) as a value.
findParents ∷ forall a. (Eq a, Ord a)
            ⇒ Tree (a, [Int]) → Map a (a,[Int])
findParents tr = Map.fromList . mkList $ tr
  where
    -- r = rootLabel tr
    mkList ∷ Tree (a,[Int]) → [(a,(a,[Int]))]
    mkList tr2 = prLst
        ++ if null sf
            then []
            else concatMap mkList sf
      where
        rl = rootLabel tr2
        sf = subForest tr2
        sfNds = fmap rootLabel sf
        prLst = fmap (\n → (fst n,rl)) sfNds

-- | Gives a Map where each OneOfGroup is a key
-- with a parent node (menutag, path) as a value.
findParents4OneOfs ∷ forall t a b. (Eq a, Ord a, Eq b, Ord b)
                ⇒ Tree (MenuItemConfig t a b)
                → Map a (a,[Int])
                → Map b (a,[Int])
findParents4OneOfs tr mp = Map.fromList lst2
  where
    lst2 = fmap (\(k,v) →
                 let mv = lookup k uniqABs
                  in (fromJust mv, v)
                ) lst
    lst ∷ [(a,(a,[Int]))] = filter (\(k,_v) → k `elem` uniqAs) $ Map.toList mp
    uniqAs ∷ [a] = fmap fst uniqABs
    uniqABs ∷ [(a,b)] = nub $
        (\i →
          (_menuItemConfigMenuTag i
          , fromJust . _menuItemConfigOneOfGroup $ i
          )
        ) <$> grps
    grps = filter (isJust . _menuItemConfigOneOfGroup) . flatten $ tr



--------------------------------------------------------------------------------

data LRMenu2 a = LRMenu2
  { _lrMenu2Name ∷ Text
  , _lrMenu2Tree ∷ Tree a
  }

--------------------------------------------------------------------------------



evs4OneOfs2 ∷ forall a b t. (Reflex t, Show a, Eq a, Ord a, Show b, Eq b, Ord b)
             ⇒ Map b [a]
             → Event t (a, [Int])
             → [Event t (a, [Int])]
evs4OneOfs2 ogs eTgPth = eList
  where
    ks = Map.keys ogs
    eList ∷ [Event t (a, [Int])] =
            fmap (\k → 
                    let mVal = Map.lookup k ogs
                     in case mVal of
                         Just v →
                            -- traceEventWith (const "evs4OneOfs2") $
                            ffilter
                                (\n →
                                    let pg = fst n
                                     in pg `elem` v
                                ) eTgPth
                         Nothing → never
                 ) ks

parentEvents ∷ (Reflex t, Show a, Eq a, Ord a, Show b, Eq b, Ord b)
             ⇒ Map b (a,[Int]) → Event t (a, [Int])
             → [Event t (a, [Int])]
parentEvents mp eTgPth =
    if null elms
        then []
        else fmap (\e → ffilter ( (== e) . fst) eTgPth ) elms
  where
    elms = fmap fst . Map.elems $ mp

--------------------------------------------------------------------------------

compStateEv2TagEv ∷ forall a b t m. (Reflex t, MonadHold t m)
                  ⇒ Event t (CompState t ActNode (MenuItem t a))
                  → Either [MenuItemConfig t a b] (Tree (MenuItemConfig t a b))
                  → m (Event t a)
compStateEv2TagEv eCsAnMi eLstTr = do
    let eCe ∷ Event t (Event t [Int]) =
              (coincidence . fmap (fmap (getPath . _activeStateElemId)))
              . fmap _ceMUp . _csCompEvent <$> eCsAnMi
    eUpd ∷ Event t [Int] ← switchHold never eCe
    let fmd =
            case eLstTr of
                Left lst →
                    fmapMaybe (\iLst →
                                let i = head iLst
                                 in if i < length lst && i>=0
                                    then Just (lst Prelude.!! i)
                                    else Nothing
                            ) (fmap (drop 1) eUpd)
                Right tr →  fmapMaybe (getNodeAtPath tr) eUpd
    let eMt ∷ Event t a =
            -- traceEventWith (\a → "body, eMt:" ++ show a) $
            fmap _menuItemConfigMenuTag fmd
    pure eMt

--------------------------------------------------------------------------------

data UiLang = LaGB | LaDE | LaSE | LaIso Text
  deriving (Eq, Show, Ord)
data DDtags = Select0DD | Select1DD | Select2DD | Select3DD
        | SelectDD
    deriving (Eq,Show,Ord)

data DDgroups = DDgroups
    deriving (Eq,Show,Ord)

instance ActSretval DDtags where defRetval = Select0DD

miDD ∷ Reflex t ⇒ Tree (MenuItemConfig t DDtags DDgroups)
miDD =
    Node (MenuItemConfig SelectDD [] never Nothing)
        [ Node (MenuItemConfig Select0DD [] never Nothing) []
        , Node (MenuItemConfig Select1DD [] never Nothing) []
        , Node (MenuItemConfig Select2DD [] never Nothing) []
        , Node (MenuItemConfig Select3DD [] never Nothing) []
        ]  
        -- [ Node (MenuItemConfig Select0DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select1DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select2DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select3DD [] never (Just DDgroups)) []


miDDEn ∷ LangMap DDtags
miDDEn = Map.fromList [ (SelectDD, "Select")
                      , (Select0DD, "Select 0")
                      , (Select1DD, "Select 1")
                      , (Select2DD, "Select 2")
                      , (Select3DD, "Select 3")
                      ]

miDDSe ∷ LangMap DDtags
miDDSe = Map.fromList [ (SelectDD , "Markera")
                      , (Select0DD, "Markera 0")
                      , (Select1DD, "Markera 1")
                      , (Select2DD, "Markera 2")
                      , (Select3DD, "Markera 3")
                      ]

miDDDe ∷ LangMap DDtags
miDDDe = Map.fromList [ (SelectDD , "Auswahl")
                      , (Select0DD, "Auswahl 0")
                      , (Select1DD, "Auswahl 1")
                      , (Select2DD, "Auswahl 2")
                      , (Select3DD, "Auswahl 3")
                      ]


getDDMiLangMap ∷ UiLang → LangMap DDtags
getDDMiLangMap LaGB = miDDEn
getDDMiLangMap LaDE = miDDDe
getDDMiLangMap LaSE = miDDSe
getDDMiLangMap _    = miDDEn -- default


updLang ∷ forall t a. (Reflex t, Ord a, Show a)
        ⇒ Dynamic t (LangMap a) → MenuItem t a → MenuItem t a
updLang dLM mi =
    mi { _menuItemLabel
            = fmap (\(lm,oTxt) →
                   case chLb lm of
                    Just nTxt → nTxt
                    Nothing → oTxt
                   ) $ zipDyn dLM (_menuItemLabel mi)
       }
  where
    chLb ∷ LangMap a → Maybe Text
    chLb lm = mtLabel (_menuItemMenuTag mi) lm

-}

--------------------------------------------------------------------------------
