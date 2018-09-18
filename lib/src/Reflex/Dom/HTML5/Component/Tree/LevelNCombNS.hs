{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Tree.LevelNCombNS
Description : Helpers to build tree-based components.
Copyright   : (c) gspia 2018 -
License     : BSD
Maintainer  : gspia

= Tree.LevelNCombNS

This contains structures to define the behavior of the nodes having
depth larger than 1 or 1 (root is 0, and level below root has depth 1).

-}
module Reflex.Dom.HTML5.Component.Tree.LevelNCombNS
    ( LevelNFuns (..)
    , levelNfADEs
    , levelNfCombFun
    , levelNfNodeAttr
    , levelNfWrapAttr
    , levelNfEUP
    , levelNfnext
    , levelNfMkLevelNF

    , defLevelNUlLiFuns
    , silentPlainLevelNUlLiFuns
    , defLevelNDivAFuns
    , silentPlainLevelNDivAFuns
    , defLevelNDivDivFuns
    , silentPlainLevelNDivDivFuns
    , defLevelNNoneDivFuns
    , silentPlainLevelNNoneDivFuns
    , defLevelNNoneAFuns
    , silentPlainLevelNNoneAFuns
    , levelNListener
    , levelNDraw
    , levelNActivity
    , setLevelNDraws

    , mkLevelN
    ) where


import           Control.Lens
import           Control.Monad (forM)
import           Control.Monad.Fix
-- import qualified Data.Map as Map
import           Data.Tree
-- import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import qualified Reflex.Dom.HTML5.Attrs as A
-- import qualified Reflex.Dom.HTML5.Elements as E -- TODO

-- import           Reflex.Dom.HTML5.Component.Common.StateInfo
import           Reflex.Dom.HTML5.Component.Common.CompEvent
-- import           Reflex.Dom.HTML5.Component.Common.DrawFuns
import           Reflex.Dom.HTML5.Component.Tree.Common
import           Reflex.Dom.HTML5.Component.Tree.ActNode


-- import Data.Monoid -- TODO DD

--------------------------------------------------------------------------------

-- | A data structure used to specify the non-root-node behavior.
data LevelNFuns t m a r = LevelNFuns
    { _levelNfADEs ∷ CommonADEfuns t m a ActNode r
    -- ^ See 'CommonADEfuns'. This contains four functions that
    -- change the 'ActiveState', decide on how to activate nodes,
    -- draw nodes and how the nodes produce events.
    , _levelNfCombFun ∷ CommonADEfuns t m a ActNode r
              → Dynamic t (Maybe [ActiveState t ActNode r])
              → ActiveState t ActNode r
              → Dynamic t a
              → Event t (CompEvent t ActNode r)
              → CompState t ActNode r
              → m (CompEvent t ActNode r)
    -- ^ A function that combines the above fours functions given in
    -- the 'CommonADfuns'.
    -- See 'adeCombFun'.
    , _levelNfNodeAttr ∷ ActiveState t ActNode r → Dynamic t A.Attr
    -- ^ A function that sets the attributes for a node in a level n of a tree
    -- (where n > 1).
    -- , _levelNfWrapAttr ∷ Dynamic t (ActiveState t ActNode r)
    -- → ActNode → Dynamic t E.Ul
    , _levelNfWrapAttr ∷ Dynamic t (ActiveState t ActNode r)
                     → ActNode → Dynamic t A.Attr
    -- ^ A function that sets the attributes. First param is the last node
    -- where an event occurred (with state info) and the second parameter tells,
    -- which node we are making (drawing, building).
    , _levelNfEUP ∷ ElemUse
    -- ^ This gives the elements that we use. The wrapper wraps the nodes of
    -- a level. E.g. ul-element is a wrapper and li-element is the node item.
    , _levelNfnext ∷ Maybe (LevelNFuns t m a r)
    -- ^ If given a new set of funs, then those are applied on the next
    -- level when constructing the nodes. If this is not given and the tree
    -- is deeper (has futher levels of nodes), then we continue to use the 
    -- current 'LevelNFuns' on those nodes.
    , _levelNfMkLevelNF ∷ (LevelNFuns t m a r
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → [Int]
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m [CompEvent t ActNode r])
    -- ^ The function that makes the Nth level of nodes (not 1st nor root).
    }

--------------------------------------------------------------------------------

-- | A lens.
levelNfADEs ∷ Lens' (LevelNFuns t m a r) (CommonADEfuns t m a ActNode r)
levelNfADEs f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns g f2 f3 f4 f5 f6 f7) (f f1)

-- | A lens.
levelNfCombFun ∷ Lens' (LevelNFuns t m a r)
    (CommonADEfuns t m a ActNode r → Dynamic t (Maybe [ActiveState t ActNode r])
    → ActiveState t ActNode r →
    Dynamic t a → Event t (CompEvent t ActNode r)
    → CompState t ActNode r → m (CompEvent t ActNode r))
levelNfCombFun f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns f1 g f3 f4 f5 f6 f7) (f f2)

-- | A lens.
levelNfNodeAttr ∷ Lens' (LevelNFuns t m a r)
    (ActiveState t ActNode r → Dynamic t A.Attr)
levelNfNodeAttr f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns f1 f2 g f4 f5 f6 f7) (f f3)

-- | A lens.
levelNfWrapAttr ∷ Lens' (LevelNFuns t m a r)
                      (Dynamic t (ActiveState t ActNode r)
              → ActNode → Dynamic t A.Attr)
levelNfWrapAttr f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns f1 f2 f3 g f5 f6 f7) (f f4)

-- | A lens.
levelNfEUP ∷ Lens' (LevelNFuns t m a r) ElemUse
levelNfEUP f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns f1 f2 f3 f4 g f6 f7) (f f5)

-- | A lens.
levelNfnext ∷ Lens' (LevelNFuns t m a r) (Maybe (LevelNFuns t m a r))
levelNfnext f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → LevelNFuns f1 f2 f3 f4 f5 g f7) (f f6)

-- | A lens.
levelNfMkLevelNF ∷ Lens' (LevelNFuns t m a r)
    (LevelNFuns t m a r
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → [Int] 
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m [CompEvent t ActNode r])
levelNfMkLevelNF f (LevelNFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (LevelNFuns f1 f2 f3 f4 f5 f6) (f f7)

--------------------------------------------------------------------------------

-- | Default value for 'LevelNFuns'. Uses ul and li.
defLevelNUlLiFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                , DomBuilderSpace m ~ GhcjsDomSpace
                , ActSretval r)
          ⇒ LevelNFuns t m a r
defLevelNUlLiFuns = LevelNFuns defCommonADEfuns adeCombFun defLiLeafNodeAttrF
        defUlSFWrapAttrF
        elemUseUlLi
        Nothing -- no next level in the default
        mkLevelN

-- | Silent value for 'LevelNFuns'. Uses ul and li.
silentPlainLevelNUlLiFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ LevelNFuns t m a r
silentPlainLevelNUlLiFuns =
    LevelNFuns silentPlainADE adeCombSilentPlain defLiLeafNodeAttrF
        defUlSFWrapAttrF
        elemUseUlLi
        Nothing -- no next level in the default
        mkLevelN


-- | Default value for 'LevelNFuns'. Uses div and a.
defLevelNDivAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                , DomBuilderSpace m ~ GhcjsDomSpace
                , ActSretval r)
          ⇒ LevelNFuns t m a r
defLevelNDivAFuns = LevelNFuns defCommonADEfuns adeCombFun defALeafNodeAttrF
        defDivSFWrapAttrF
        elemUseDivA
        Nothing -- no next level in the default
        mkLevelN

-- | Silent value for 'LevelNFuns'. Uses div and a.
silentPlainLevelNDivAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ LevelNFuns t m a r
silentPlainLevelNDivAFuns =
    LevelNFuns silentPlainADE adeCombSilentPlain defALeafNodeAttrF
        defDivSFWrapAttrF
        elemUseDivA
        Nothing -- no next level in the default
        mkLevelN



-- | Default value for 'LevelNFuns'. Uses div and div.
defLevelNDivDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                , DomBuilderSpace m ~ GhcjsDomSpace
                , ActSretval r)
          ⇒ LevelNFuns t m a r
defLevelNDivDivFuns = LevelNFuns defCommonADEfuns adeCombFun defDivLeafNodeAttrF
        defDivSFWrapAttrF
        elemUseDivDiv
        Nothing -- no next level in the default
        mkLevelN

-- | Silent value for 'LevelNFuns'. Uses div and div.
silentPlainLevelNDivDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ LevelNFuns t m a r
silentPlainLevelNDivDivFuns =
    LevelNFuns silentPlainADE adeCombSilentPlain defDivLeafNodeAttrF
        defDivSFWrapAttrF
        elemUseDivDiv
        Nothing -- no next level in the default
        mkLevelN

-- | Default value for 'LevelNFuns'. Uses div for the elements and nothing 
-- for the wrapper.
defLevelNNoneDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                , DomBuilderSpace m ~ GhcjsDomSpace
                , ActSretval r)
          ⇒ LevelNFuns t m a r
defLevelNNoneDivFuns = LevelNFuns defCommonADEfuns adeCombFun defDivLeafNodeAttrF
        defDivSFWrapAttrF -- this is not used, can be any of the AttrFs.
        elemUseNoneDiv
        Nothing -- no next level in the default
        mkLevelN

-- | Silent value for 'LevelNFuns'. Uses div for the elements and nothing
-- for the wrapper.
silentPlainLevelNNoneDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ LevelNFuns t m a r
silentPlainLevelNNoneDivFuns =
    LevelNFuns silentPlainADE adeCombSilentPlain defDivLeafNodeAttrF
        defDivSFWrapAttrF -- this is not used, can be any of the AttrFs.
        elemUseNoneDiv
        Nothing -- no next level in the default
        mkLevelN

-- | Default value for 'LevelNFuns'. Uses a for the elements and nothing
-- for the wrapper.
defLevelNNoneAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                , DomBuilderSpace m ~ GhcjsDomSpace
                , ActSretval r)
          ⇒ LevelNFuns t m a r
defLevelNNoneAFuns = LevelNFuns defCommonADEfuns adeCombFun defALeafNodeAttrF
        defDivSFWrapAttrF -- this is not used, can be any of the AttrFs.
        elemUseNoneA
        Nothing -- no next level in the default
        mkLevelN

-- | Silent value for 'LevelNFuns'. Uses div for the elements and nothing
-- for the wrapper.
silentPlainLevelNNoneAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                     , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , ActSretval r)
                  ⇒ LevelNFuns t m a r
silentPlainLevelNNoneAFuns =
    LevelNFuns silentPlainADE adeCombSilentPlain defALeafNodeAttrF
        defDivSFWrapAttrF -- this is not used, can be any of the AttrFs.
        elemUseNoneA
        Nothing -- no next level in the default
        mkLevelN


--------------------------------------------------------------------------------

-- | Listening-lens for levelN-nodes.
levelNListener ∷ Lens' (LevelNFuns t m a r)
                     (ActiveState t ActNode r → ActiveState t ActNode r)
levelNListener = levelNfADEs . adeListen

-- | Node drawing -lens for levelN-nodes.
levelNDraw ∷ Lens' (LevelNFuns t m a r)
    (Dynamic t a → ActiveState t ActNode r
    → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
levelNDraw = levelNfADEs . adeDraw

-- | Node activity function -lens for levelN-nodes.
levelNActivity ∷ Lens' (LevelNFuns t m a r)
    (Dynamic t (Maybe [ActiveState t ActNode r])
             → Event t (CompEvent t ActNode r)
             → CompState t ActNode r
             → ActiveState t ActNode r → m (ActiveState t ActNode r))
levelNActivity = levelNfADEs . adeActivate


-- | Recursively set a drawing function to each 'LevelNFuns' used.
setLevelNDraws ∷  (Dynamic t a → ActiveState t ActNode r
                 → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
               → LevelNFuns t m a r
               → LevelNFuns t m a r
setLevelNDraws fd lf =
    case n of
        Just nn →
            let sublf = setLevelNDraws fd nn
             in lf2 { _levelNfnext = Just sublf }
        Nothing → lf2
  where
    lf2 = set levelNDraw fd lf
    n = _levelNfnext lf2



--------------------------------------------------------------------------------

-- | A function to make a level of nodes that have depth larger than 1 or 1.
-- It means specifying, how to draw and handle events.
--
-- Note: this doesn't show the root label of a tree.
mkLevelN ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m -- , Eq a, Show a
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
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode r, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode r]
mkLevelN lvlFs dmlst evB trSt pth treeE = do
    let combF = _levelNfCombFun lvlFs (_levelNfADEs lvlFs)
        mNextL = _levelNfnext lvlFs
        mkFun = _levelNfMkLevelNF lvlFs
        sFelms = subForest treeE
        -- Note: these are pairs and have the content that we will show.
        -- We take the forest and show the root of each tree in the forest.
        -- dmlst = _treeCActivityConf tc ∷ Dynamic t (Maybe [ActiveState t ActNode])
        astD = defActiveStateTree & set activeStateElemId (ActNpath pth)
        useEltxt = _euLeafNode . _levelNfEUP $ lvlFs -- text
        -- useEltxtAlt = _euLNodeDD . _levelNfEUP $ lvlFs -- text
        useWraptxt = _euSFWrap . _levelNfEUP $ lvlFs -- text
        -- defElm = _euDef . _levelNfEUP $ lvlFs ∷ A.Attr
            -- ∷ ActiveState t ActNode
    actS ∷ ActiveState t ActNode r
          ← (_adeActivate . _levelNfADEs) lvlFs dmlst evB trSt
             $ (_adeListen . _levelNfADEs) lvlFs astD
    -- evTE ← E.ulD (_levelNfWrapAttr lvlFs (constDyn actS) $ ActNpath pth) $ do
    -- text "mkLevelN"
    evTE ← if not (T.null useWraptxt)
        then do
            -- text "not null wrap (mkLevelN)."
            -- text $ " and useWraptxt = " <> useWraptxt
            -- text $ ". Len sFelms = " <> (T.pack . show . length) sFelms
            elDynAttr useWraptxt
               (_levelNfWrapAttr lvlFs (constDyn actS) $ ActNpath pth) $ 
                    edsF sFelms useEltxt combF mNextL mkFun 
        else do
            -- text "null wrap (mkLevelN)"
            -- text $ ". Len sFelms = " <> (T.pack . show . length) sFelms
            edsF sFelms useEltxt combF mNextL mkFun 
        -- eds ∷ [Event t (CompEvent t ActNode)] ← forM sFelms
    pure evTE
  where
    edsF sFelms useEltxt combF mNextL mkFun = do
        -- text "edsF"
        eds ∷ [[CompEvent t ActNode r]] ← forM sFelms
            ( \elmTr → do -- for each tree in forest, do:
            let (elm,ast2) = rootLabel elmTr -- and its node and content
                -- sFelms2 = subForest elmTr
                pth2 = getPath $ _activeStateElemId elm
            -- text $ "pth2 = " <> (T.pack . show) pth2
            -- text $ "useEltxt = " <> useEltxt
            -- Here we have to check if this a leaf or does this have a
            -- sub-tree.
            if isLeaf elmTr
                then mdo
                    -- text $ "leaf elmTr"
                    -- te ← E.liD (_levelNfNodeAttr lvlFs $ _ceMe te) $
                    te ∷ CompEvent t ActNode r
                       -- ← elDynAttr useEltxt (_levelNfNodeAttr lvlFs $ _ceMe te) $
                       ← elDynAttr useEltxt (_levelNfNodeAttr lvlFs $ _ceMe te) $
                         combF dmlst elm ast2 evB trSt
                    -- let te2 = compEvFiring [te]
                    -- pure te2
                    pure [te]
                else mdo -- This is a tree (a sub-tree).
                    -- text $ "non-leaf elmTr"
                    -- E.brN blank
                    -- Thus, draw the root node and recurse into the tree.
                    let dAttr = _levelNfNodeAttr lvlFs $ _ceMe te
                    -- evTe ∷ Event t (CompEvent t ActNode)
                    (te,teRst) ∷ (CompEvent t ActNode r, [CompEvent t ActNode r])
                      ← elDynAttr useEltxt dAttr $ do
                        te3 ∷ CompEvent t ActNode r ← combF dmlst elm ast2 evB trSt
                        -- and next the forest
                        ce ∷ [CompEvent t ActNode r] ← case mNextL of
                                Just nFs → mkFun nFs dmlst evB trSt pth2 elmTr
                                Nothing → mkFun lvlFs dmlst evB trSt pth2 elmTr
                        pure (te3, ce)
                    pure $ te : teRst
            )
        pure $ concat eds


