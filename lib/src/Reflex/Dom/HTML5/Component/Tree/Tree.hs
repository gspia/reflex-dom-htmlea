{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Tree.Tree
Description : Helpers to build tree-based components.
Copyright   : (c) gspia 2018 -
License     : BSD
Maintainer  : gspia

= Tree


Function 'mkTree' needs

    * A Tree giving showable inputs.
    * A set of functions that defines, how things are shown up and what happens on
      events (e.g. on mouse button click).
-}

module Reflex.Dom.HTML5.Component.Tree.Tree
    ( TreeConf (..)
    , treeCRootC
    , treeCEvents
    , treeCActivityConf
    , treeCMkRootF
    , setTreeDraw
    , defTreeConf
    , silentPlainTreeConf
    , mkTree
    , mkTreeD
    , mkTreeDD
    , getNodeAtPath
    , isLeafAtPath

    , module Reflex.Dom.HTML5.Component.Common.CompEvent
    , module Reflex.Dom.HTML5.Component.Common.DrawFuns
    )
    where

import           Control.Lens
-- import           Control.Monad (forM)
import           Control.Monad.Fix
-- import           Control.Monad.State.Lazy
-- import           Data.Maybe (fromMaybe)
-- import           Data.Semigroup ((<>))
-- import           Data.Text                     (Text)
-- import qualified Data.Text                     as T
import           Data.Tree

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

-- import qualified Reflex.Dom.HTML5.Attrs as A
-- import qualified Reflex.Dom.HTML5.Elements as E

import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Common.DrawFuns
import           Reflex.Dom.HTML5.Component.Tree.ActNode
-- import           Reflex.Dom.HTML5.Component.Tree.Common
import           Reflex.Dom.HTML5.Component.Tree.LevelNCombNS
import           Reflex.Dom.HTML5.Component.Tree.RootCombNS

--------------------------------------------------------------------------------


-- | A data-structure used to define a tree-structure.
data TreeConf t m a r = TreeConf
    { _treeCRootC ∷ RootFuns t m a r
    -- ^ If configured, the root node (of the whole tree) will be shown.
    -- E.g. 'defRootNavDivFuns'. 
    , _treeCEvents ∷ Element EventResult (DomBuilderSpace m) t
                   →  m (CompOtherEvs t)
    -- ^ Events originating from tree (and not from nodes). At the moment,
    -- this is used to catch the enter and leave events.
    -- E.g. 'compEventsEnLe'.
    , _treeCActivityConf ∷ Dynamic t (Maybe [ActiveState t ActNode r])
    -- ^ Empty list means no node is activated initially. This can also activate
    -- a sub-tree or path with a single 'ActNode' inside 'ActiveState'.
    -- Default value is constDyn Nothing.
    , _treeCMkRootF ∷ (RootFuns t m a r
       → Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m ( Element EventResult (DomBuilderSpace m) t
           , Event t (CompEvent t ActNode r)))
    -- ^ e.g. 'mkRoot'
    }

    -- , _treeCLevelEvFilter ∷ Event t (CompEvent t ActNode) → Event t ActNode
    -- A filtering function that row making function uses. This affects
    -- to the dynamic ActNode that is given to the function that makes
    -- tr-attributes. HMM, what attributes TODO?

-- | A lens.
treeCRootC ∷ Lens' (TreeConf t m a r) (RootFuns t m a r)
treeCRootC f (TreeConf f1 f2 f3 f4) =
    fmap (\g → TreeConf g f2 f3 f4) (f f1)

-- | A lens.
treeCEvents ∷ Lens' (TreeConf t m a r)
    (Element EventResult (DomBuilderSpace m) t →  m (CompOtherEvs t))
treeCEvents f (TreeConf f1 f2 f3 f4) =
    fmap (\g → TreeConf f1 g f3 f4) (f f2)

-- A lens.
-- treeCLevelEvFilter ∷ Lens' (TreeConf t m a r)
    -- (Event t (CompEvent t ActNode r) → Event t ActNode)
-- treeCLevelEvFilter f (TreeConf f1 f2 f3 f4 f5) =
    -- fmap (\g → TreeConf f1 f2 g f4 f5) (f f3)

-- | A lens.
treeCActivityConf ∷ Lens' (TreeConf t m a r)
    (Dynamic t (Maybe [ActiveState t ActNode r]))
treeCActivityConf f (TreeConf f1 f2 f3 f4) =
    fmap (\g → TreeConf f1 f2 g f4) (f f3)

-- | A lens.
treeCMkRootF ∷ Lens' (TreeConf t m a r)
    (RootFuns t m a r
       → Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m ( Element EventResult (DomBuilderSpace m) t
           , Event t (CompEvent t ActNode r)))
treeCMkRootF f (TreeConf f1 f2 f3 f4) =
    fmap (TreeConf f1 f2 f3) (f f4)


--------------------------------------------------------------------------------

-- | Node drawing for whole tree.
-- (that are they xx-elements, to be clear).
-- TODO this sets only drawing for root level, change so that this
-- sets
setTreeDraw ∷ (Dynamic t a → ActiveState t ActNode r
              → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
            → TreeConf t m a r
            → TreeConf t m a r
setTreeDraw fd tc = tc3
  where
    tc2 = set (treeCRootC . rootDraw) fd tc
    tc3 = over (treeCRootC . rootfnext) (setLevelNDraws fd) tc2
    -- lf2 = setLevelNDraws lf fd


--------------------------------------------------------------------------------


-- | A default value for 'TreeConf'.
-- See also 'silentPlainTreeConf'.
defTreeConf ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
              , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
              , DomBuilderSpace m ~ GhcjsDomSpace
              , ActSretval r)
            ⇒ TreeConf t m a r
defTreeConf = TreeConf defRootNavDivFuns compEventsEnLe
    (constDyn Nothing) mkRoot
    -- ceFilterMU (constDyn Nothing) mkRoot

--------------------------------------------------------------------------------

-- | This is easier to start with if just needing a decorated table.
-- This allows normal selection and copying from the table.
-- See also 'defTreeConf'.
silentPlainTreeConf ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                      , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                      , DomBuilderSpace m ~ GhcjsDomSpace
                      , ActSretval r)
                    ⇒ TreeConf t m a r
silentPlainTreeConf = TreeConf silentPlainRootNavDivFuns compEventsNone
    (constDyn Nothing) mkRoot
    -- ceFilterNone (constDyn Nothing) mkRoot


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Make (build) a tree that returns node information.
mkTree ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m -- , Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
         ⇒  TreeConf t m a r
         -- ^ 'TreeConf' contains definitions, that is, functions and dynamic
         -- attributes that are used to construct the html-elements.
         → Tree a
         -- ^ Contents of the tree.
         → m (CompState t ActNode r)
         -- ^ 'CompState' has event and state variables.
mkTree tc acElms =  mdo
    let elms = tree2ActTree acElms
        mkFun = _treeCMkRootF tc
    -- liftJSM $ eval ("console.log('mkTree')" ∷ Text)
    (elEvRes,evR) ← mkFun (view treeCRootC tc)
                 (view treeCActivityConf tc)
                 evR treeSt elms
    treeOE ←  _treeCEvents tc elEvRes
    treeSt ← updateTreeCompState treeOE evR
    pure treeSt

--------------------------------------------------------------------------------

-- | Make (build) a tree that returns node information.
-- This takes dynamic configuration and returns an event with the state.
mkTreeD ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m -- , Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
         ⇒  Dynamic t (TreeConf t m a r)
         -- ^ Dynamic 'TreeConf' contains definitions, that is, 
         -- functions and dynamic
         -- attributes that are used to construct the html-elements.
         → Tree a
         -- ^ Contents of the tree.
         → m (Event t (CompState t ActNode r))
         -- ^ 'CompState'-event has events and state variables inside.
mkTreeD dtc acElms =  do
    let elms = tree2ActTree acElms
    eCs ∷ Event t (CompState t ActNode r) ← dyn $ ffor dtc $ \tc → mdo
        evPB ← getPostBuild
        let mkFun = _treeCMkRootF tc -- e.g. mkRoot
            evR = leftmost [defCompEvent <$ evPB, evR2] -- Is this ok?
        (elEvRes,evR2 ∷ Event t (CompEvent t ActNode r))
            ← mkFun (view treeCRootC tc) -- e.g. defRootNavDivFuns
                     (view treeCActivityConf tc) -- e.g. constDyn Nothing
                     evR treeSt elms
        -- _treeCEvents tc -- e.g. compEventsEnLe or compEventsNone
        treeOE ∷ CompOtherEvs t ←  _treeCEvents tc elEvRes
        treeSt ← updateTreeCompState treeOE evR2
        pure treeSt
    pure eCs


-- | Make (build) a tree that returns node information.
-- This takes dynamic configuration and dynamic input value,
-- and returns an event with the state.
mkTreeDD ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m -- , Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
         ⇒  Dynamic t (TreeConf t m a r)
         -- ^ Dynamic 'TreeConf' contains definitions, that is, 
         -- functions and dynamic
         -- attributes that are used to construct the html-elements.
         → Dynamic t (Tree a)
         -- ^ Dynamic contents of the tree.
         → m (Event t (CompState t ActNode r))
         -- ^ 'CompState'-event has events and state variables inside.
mkTreeDD dtc dElms =  do
    let dEs ∷ Dynamic t (Tree (ActiveState t ActNode r, Dynamic t a))
            = tree2ActTree <$> dElms
    eCs ∷ Event t (Event t (CompState t ActNode r)) ← dyn $ ffor dtc $ \tc → do
        dyn $ ffor dEs $ \elms → mdo
            let mkFun = _treeCMkRootF tc
            (elEvRes,evR) ← mkFun (view treeCRootC tc)
                         (view treeCActivityConf tc)
                         evR treeSt elms
            treeOE ←  _treeCEvents tc elEvRes
            treeSt ← updateTreeCompState treeOE evR
            pure treeSt
    ev ← switchHold never eCs
    pure ev
    -- pure never


--------------------------------------------------------------------------------

-- This should return a dynamic or event
-- getNodeFromCompState ∷ Tree a → CompState t ActNode → Maybe a
-- getNodeFromCompState tree cs =


-- | This can be used to find out, what tree element corresponds to
-- the $ ActiveState t ActNode $ that is inside 'CompState'.
getNodeAtPath ∷ Tree a → [Int] → Maybe a
getNodeAtPath tree iLst
    | null iLst || head iLst /= 0  = Nothing
    | not (null iLst) && head iLst == 0 && length iLst == 1 =
      Just $ rootLabel tree
    | otherwise = findInSF (drop 1 dl) ( sf Prelude.!! (head dl) )
  where
    dl = drop 1 iLst
    sf = subForest tree
    findInSF [] tr = Just $ rootLabel tr
    findInSF (j:jLst) tr =
        if j < length sfTr
            then findInSF jLst ( sfTr Prelude.!! j )
            else Nothing
      where
        sfTr = subForest tr

-- | Check is a given path is a leaf node in the tree.
isLeafAtPath ∷ Tree a → [Int] → Bool
isLeafAtPath tree iLst
    | null iLst || head iLst /= 0  = False
    | not (null iLst) && head iLst == 0 && length iLst == 1 =
          null sf
    | otherwise = checkInSF (drop 1 dl) ( sf Prelude.!! (head dl) )
  where
    dl = drop 1 iLst
    sf = subForest tree
    checkInSF [] tr = null (subForest tr)
    checkInSF (j:jLst) tr =
        if j < length sfTr
            then checkInSF jLst ( sfTr Prelude.!! j )
            else False
      where
        sfTr = subForest tr

--------------------------------------------------------------------------------

-- | Tree-component uses this one.
updateTreeCompState ∷ forall t m r. (Reflex t, MonadHold t m, ActSretval r)
             ⇒ CompOtherEvs t → Event t (CompEvent t ActNode r)
             → m (CompState t ActNode r)
updateTreeCompState tblOevs eCompEvent =
    updateComponentState duTreeStd tblOevs eCompEvent

-- | Helper: given a pair of 'ActNodes's, return two 'ActNode's that bound the
-- same area as original but in which the elems are in order.
-- TODO TODO
duTreeStd ∷ forall t r. Reflex t
      ⇒ Dynamic t (ActiveState t ActNode r, ActiveState t ActNode r)
      → Dynamic t (ActiveState t ActNode r, ActiveState t ActNode r)
duTreeStd d = orderPair <$> d
  where
    orderPair ∷ (ActiveState t ActNode r,ActiveState t ActNode r)
              → (ActiveState t ActNode r,ActiveState t ActNode r)
    orderPair (ast1,ast2) =
        if _activeStateElemId ast1 <= _activeStateElemId ast2
            then (ast1,ast2)
            else (ast2,ast1)

