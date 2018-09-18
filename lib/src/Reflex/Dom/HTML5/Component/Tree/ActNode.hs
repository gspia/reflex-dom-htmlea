{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecursiveDo   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Tree.ActNode
Description : Helpers to build tree-components.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Tree.ActNode

This module defines helpers to manage states inside nodes of a tree.

-}

module Reflex.Dom.HTML5.Component.Tree.ActNode
    (
    -- * Data structures
    --
    ActNode (..)
    , defActNode
    , defActiveStateTree
    , tree2ActTree
    , getPath
    , isAncestor
    , isParent

    -- * Functions for ActNode
    , initializePath
    , initializeNodes
    , initializeAllNodes
    , initializeAllNodesWith
    , initializeSubtree
    , actMUOnP

    -- * Listening conventions
    , listenMySubTrees
    , listenMyAncestors
    , listenMyParent

    -- * Other functions
    , mkPathsTree
    , pathsFromTree
    , biTreeMap

    -- * StateInfo and ActiveGroup
    , module Reflex.Dom.HTML5.Component.Common.StateInfo

    ) where


import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Maybe                     (catMaybes)
import           Data.List                      (inits, isPrefixOf)
import qualified Data.Set                       as Set
import           Data.Tree
import           Reflex
-- import           Reflex.Dom.Core                (DomBuilder, text, dynText)

-- import           Reflex.Dom.HTML5.Attrs.Globals
-- import           Reflex.Dom.HTML5.Elements      (pN)
import           Reflex.Dom.HTML5.Component.Common.StateInfo
import           Reflex.Dom.HTML5.Component.Common.CompEvent

--------------------------------------------------------------------------------

-- | ActNode represents a tree node, path between tree nodes,
-- or sub-tree. It also has a value for
-- no node. The main use is to keep track, which nodes have been "selected".
--
-- Note: the path-arguments are integer coded: first number is allways 0 as
-- it is the root node of the whole tree. If on the depth 1 there are 5 sub-trees,
-- the second number is between 0 and 4. Etc. The longest path from root to
-- any leaf is also the maximum length of the Int-list.
data ActNode
    = ActNpath [Int]  -- ^ A single node in a tree (with unique id-path).
    | ActNsubt [Int]  -- ^ A single sub-tree starting with node (path pointing
                      -- to the root).
    | ActNnone      -- ^ This refers to not any node in a particular tree.
    deriving (Eq, Show, Ord)

-- | Default value for a tree node ('ActNode').
defActNode ∷ ActNode
defActNode = ActNnone

--------------------------------------------------------------------------------

instance ActSelem ActNode where
    defElem = ActNnone
    -- defBelong = ANBelem
    -- | Can be used to check if the first is inside of the second, or other
    -- way around. That is, if the cells are either on the same row, column
    -- or if they are the same cells.
    belongElem ae1 ae2 = ae1 == ae2
    -- belongElem ab ae1 ae2 =
        -- case ab of
            -- ANBpath → getPath ae1 `isPrefixOf` getPath ae2
            -- ANBsubt → getPath ae2 `isPrefixOf` getPath ae1
            -- ANBelem → ae1 == ae2 -- do we need sameSubTree or something similar?
    isInsidePair = isInside

--------------------------------------------------------------------------------

-- | Get the path from 'ActNode'. If 'ActNnone', returns an empty list.
getPath ∷ ActNode → [Int]
getPath (ActNpath p) = p
-- getPath (ActNsubt _) = []
getPath (ActNsubt p) = p
getPath _ = []

-- getSubt ∷ ActNode → [Int]
-- getSubt (ActNpath _) = []
-- getSubt (ActNsubt p) = p
-- getSubt _ = []


-- | Can be used to check, if the area of cells contains the given cell.
--
-- Not OK ATM & TODO! This does not handle header info, yet. (The area
-- selection methods use this, see 'actAreaSelCommon'.)
isInside ∷ (ActNode,ActNode) → ActNode → Bool
isInside (ActNpath p1, ActNpath p2) (ActNpath p3) = pathInside p1 p2 p3
isInside (ActNsubt p1, ActNsubt p2) (ActNpath p3) = pathInside p1 p2 p3
isInside _ _ = False

-- | Check if the third one is inside the area bounded by the first two paths.
-- We can rely on the ordering of int-lists here.
pathInside ∷ [Int] → [Int] → [Int] → Bool
pathInside p1 p2 p3 = p1 <= p3 && p3 <= p2


-- | Is the first an ancestor of the second? (Or, is the first in the path of
-- the second.)
isAncestor ∷ ActNode → ActNode → Bool
isAncestor n1 n2 = getPath n1 `isPrefixOf` getPath n2

-- | Is the first a parent of the second? (Or, is the first in the path of
-- the second.)
isParent ∷ ActNode → ActNode → Bool
isParent n1 n2 = p1 `isPrefixOf` p2 && length p1 == length p2 - 1
  where
    p1 = getPath n1
    p2 = getPath n2

--------------------------------------------------------------------------------


-- | Default value for tree node 'ActiveState'.
defActiveStateTree ∷ (Reflex t, ActSretval r) ⇒ ActiveState t ActNode r
defActiveStateTree = defActiveState


--------------------------------------------------------------------------------


-- | Can be used to give initial state for a path.
-- Note: this doesn't check that the tree has the given path. TODO
initializePath ∷ Reflex t ⇒ ActiveState t ActNode r
               →  [Int] → Maybe [ActiveState t ActNode r]
initializePath defe p = Just
    [ defe & set activeStateElemId (ActNpath p') | p' ← inits p ]

-- | Can be used to give initial state for whole tree.
initializeNodes ∷ Reflex t ⇒ ActiveState t ActNode r
                → [[Int]] → Maybe [ActiveState t ActNode r]
initializeNodes defe pths =
    Just $ fmap (\pp →  defe & set activeStateElemId (ActNpath pp)) pths

-- | Can be used to give initial state for whole tree.
initializeAllNodes ∷ (Reflex t, Eq a) ⇒ Tree a
                → ActiveState t ActNode r → Maybe [ActiveState t ActNode r]
initializeAllNodes tree defe =
    Just $ fmap (\pp →  defe & set activeStateElemId (ActNpath pp)) pths
  where
    pths = pathsFromTree $ mkPathsTree tree


-- | Can be used to give initial state for whole tree.
initializeAllNodesWith ∷ (Reflex t, Eq a)
                       ⇒ Tree a
                       → [[Int]]
                       → ActiveState t ActNode r
                       → ActiveState t ActNode r
                → Maybe [ActiveState t ActNode r]
initializeAllNodesWith tree exPaths defe defExElm =
    Just $ fmap (\pp →
                 if pp `elem` exPaths
                     then defExElm & set activeStateElemId (ActNpath pp)
                     else defe & set activeStateElemId (ActNpath pp)
                ) pths
  where
    pths = pathsFromTree $ mkPathsTree tree


-- | Can be used to give initial state for whole tree.
initializeSubtree ∷ (Reflex t, Eq a) ⇒ Tree a
                → ActiveState t ActNode r
                → [Int]
                → Maybe [ActiveState t ActNode r]
initializeSubtree tree defe p1 =
    Just $ fmap (\p2 →  defe & set activeStateElemId (ActNpath p2)) paths
  where
    pTree = mkPathsTree tree
    msf = findSubTree (ActNpath p1) pTree
    paths ∷ [[Int]] =
        case msf of
            Just sf → pathsFromTree sf
            Nothing → []

--------------------------------------------------------------------------------

-- | Activate a node when clicked on any of the nodes in the subtree of
-- this node. Thus, this activates a path from root to the clicked node.
listenMySubTrees ∷ forall t a r. (Reflex t, Eq a)
                 ⇒ Tree a → ActiveState t ActNode r
                 → ActiveState t ActNode r
listenMySubTrees acElms me = me & \d → d {_activeStateListen = constDyn ag}
  where
    tr = mkPathsTree acElms
    -- Be sure to use the same naming convention what is used in mkTree-method.
    msf = findSubTree (_activeStateElemId me) tr
    paths ∷ [[Int]] =
        case msf of
            Just sf → pathsFromTree sf
            Nothing → []
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen =
        if null paths
            then [ActNnone]
            else fmap ActNpath paths

-- | Activate a subtree when clicked on a parent or any node on the path
-- to the root.
listenMyAncestors ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActNode r
                 → ActiveState t ActNode r
listenMyAncestors me = me & \d → d {_activeStateListen = constDyn ag}
  where
    pth = getPath (_activeStateElemId me)
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen =
        if null pth
            then [ActNnone]
            else ActNpath <$> inits pth

-- | Activate a subtree when clicked on a immediate parent. This doesn't
-- listen any other ancestors. If you need those, use 'listenMyAncestors'.
listenMyParent ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActNode r
                 → ActiveState t ActNode r
listenMyParent me = me & \d → d {_activeStateListen = constDyn ag}
  where
    pth = getPath (_activeStateElemId me)
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen =
        if null pth
            then [ActNnone]
            else ActNpath <$> [take (length pth - 1) pth, pth]

--------------------------------------------------------------------------------

-- | Internal helper.
findSubTree ∷ ActNode → Tree [Int] → Maybe (Tree [Int])
findSubTree a t@(Node rl sf)
    | p == rl = Just t
    | null ms = Nothing
    | otherwise = Just $ head ms
  where
    p = getPath a
    ms = catMaybes $ fmap (findSubTree a) sf

-- | Internal helper.
-- 
-- Hmm, is this same as flatten? TODO
pathsFromTree ∷ Tree [Int] → [[Int]]
pathsFromTree (Node n sf) = n : concatMap pathsFromTree sf
  -- where
    -- nn = _activeStateElemId n

--------------------------------------------------------------------------------

-- | Attach 'ActiveState' -states into the tree.
-- tree2ActTree ∷ forall t a r. (Reflex t, Eq a, ActSretval r)
tree2ActTree ∷ forall t a r. (Reflex t, ActSretval r)
             ⇒ Tree a → Tree (ActiveState t ActNode r, Dynamic t a)
tree2ActTree acElms = biTreeMap mkA4subs (mkPathsTree acElms) acElms
  where
    -- TODO TODO TODO use the provided activity information in tc
    -- (or is the current way of using it in the functions making rows enough)
    ast ∷ ActiveState t ActNode r
    ast = defActiveState
    mkA4subs ∷ [Int] →  a → (ActiveState t ActNode r, Dynamic t a)
    mkA4subs p e = (ast & set activeStateElemId (ActNpath p)
                   , constDyn e)


--------------------------------------------------------------------------------
-- Helpers

-- liftA2 will produce all combinations if given two lists,
-- which is the reason it doesn't work here.

-- | This map maintains the structure (say, tree form) of the Trees,
-- whose structures are assumed to be indentical (not checked).
biTreeMap :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
biTreeMap f (Node r1 fo1) (Node r2 fo2) =
    Node (f r1 r2) $ uncurry (biTreeMap f) <$> zip fo1 fo2

-- | Mainly internal.
pathsTree ∷ Tree a → State (Int,[Int]) (Tree [Int])
pathsTree (Node _ []) = do
    (i,iLst) ← get
    put (i+1,iLst)
    pure (Node (iLst++[i]) [])
pathsTree (Node _ forest) = do
    (i,iLst) ← get
    let nLst = iLst ++ [i]
    put (0,nLst)
    subf ← mapM pathsTree forest
    put (i+1,iLst)
    pure  (Node nLst subf)

-- | Mainly internal.
mkPathsTree ∷ Tree a → Tree [Int]
mkPathsTree t = evalState (pathsTree t) (0,[])

-- numTree2 :: (Eq a) => Tree a -> (Tree Int, Int)
-- numTree2 t = runState (numberTree t) 0

--------------------------------------------------------------------------------


-- | A tree-specific activation method. Not used atm.
-- TODO
actMUOnP ∷ forall m t e r. (Reflex t, MonadFix m, MonadHold t m
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
actMUOnP dma evTbl _tblSt ac = mdo
    evU ∷ Event t (ActiveState t e r) ← switchHold never $ _ceMUp <$> evTbl
    -- evD ∷ Event t (ActiveState t e) ← switchHold never $ _ceMDown <$> evTbl
    evPB ← getPostBuild
    let dAvable = isActivableDyn dma ac
        eAveOnPB = tag (current $ isActiveDyn dma ac) evPB
        ev2 = gate (current dAvable) $ fmap _activeStateElemId evU
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




