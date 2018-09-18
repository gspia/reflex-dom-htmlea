{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Tree.RootCombNS
Description : Helpers to build tree-based components.
Copyright   : (c) gspia 2018 -
License     : BSD
Maintainer  : gspia

= Tree.RootCombNS

This contains structures to define the behavior of the nodes having
depth 1 (root is 0, and level below root has depth 1).

-}

module Reflex.Dom.HTML5.Component.Tree.RootCombNS
    ( RootFuns (..)
    , rootfADEs
    , rootfCombFun
    , rootfNodeAttr
    , rootfWrapAttr
    , rootfEUP
    , rootfnext
    , rootfMkLevelNF
    , rootListener
    , rootDraw
    , rootActivity
    , defRootDivAFuns
    , silentPlainRootDivAFuns
    , defRootNavDivFuns
    , silentPlainRootNavDivFuns
    , defRootNavDivFunsB
    , silentPlainRootNavDivFunsB
    , defRootDivDivFuns
    , silentPlainRootDivDivFuns
    , defRootDivButtonFuns
    , silentPlainRootDivButtonFuns
    , mkRoot

    , module Reflex.Dom.HTML5.Component.Common.CompEvent
    , module Reflex.Dom.HTML5.Component.Common.DrawFuns
    )
    where

import           Control.Lens
-- import           Control.Monad (forM)
import           Control.Monad.Fix
-- import           Control.Monad.State.Lazy
-- import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Tree

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import qualified Reflex.Dom.HTML5.Attrs as A
-- import qualified Reflex.Dom.HTML5.Elements as E -- TODO DD

-- import           Reflex.Dom.HTML5.Component.Common.StateInfo
import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Common.DrawFuns
import           Reflex.Dom.HTML5.Component.Tree.Common
import           Reflex.Dom.HTML5.Component.Tree.ActNode
import           Reflex.Dom.HTML5.Component.Tree.LevelNCombNS
-- import           Reflex.Dom.HTML5.Component.Tree.LiComb

-- import           Data.Monoid -- TODO DD

--------------------------------------------------------------------------------

-- | A data-structure used to specify the root node behavior.
data RootFuns t m a r = RootFuns
    { _rootfADEs ∷ CommonADEfuns t m a ActNode r
    -- ^ See 'CommonADEfuns'. This contains four functions that
    -- change the 'ActiveState', decide on how to activate nodes,
    -- draw nodes and how the nodes produce events.
    , _rootfCombFun ∷ CommonADEfuns t m a ActNode r
              → Dynamic t (Maybe [ActiveState t ActNode r])
              → ActiveState t ActNode r
              → Dynamic t a
              → Event t (CompEvent t ActNode r)
              → CompState t ActNode r
              → m (CompEvent t ActNode r)
    -- ^ A function that combines the above fours functions given in
    -- the 'CommonADfuns'.
    -- See 'adeCombFun'.
    , _rootfNodeAttr ∷ ActiveState t ActNode r → Dynamic t A.Attr
    -- ^ A function that sets the attributes. E.g. for "a"-element
    -- or what element the node is using.
    , _rootfWrapAttr ∷ Dynamic t (ActiveState t ActNode r)
                     → ActNode → Dynamic t A.Attr
    -- ^ A function that sets the attributes for the wrapping element.
    , _rootfEUP ∷ ElemUse
    -- ^ This gives the elements that we use. The wrapper wraps the nodes of
    -- a level. E.g. ul-element is a wrapper and li-element is the node item.
    , _rootfnext ∷ LevelNFuns t m a r
    -- ^ If given a new set of funs, then those are applied on the next
    -- level when constructing the nodes. If this is not given and the tree
    -- is deeper (has futher levels of nodes), then we continue to use the 
    -- current 'LevelNFuns' on those nodes.
    , _rootfMkLevelNF ∷ (LevelNFuns t m a r
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → [Int] 
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m [CompEvent t ActNode r])
    -- ^ The function that makes the 1st level right after the root node.
    }

-- | A lens.
rootfADEs ∷ Lens' (RootFuns t m a r) (CommonADEfuns t m a ActNode r)
rootfADEs f (RootFuns f1 f2 f3 f4 f5 f6 f7)
    = fmap (\g → RootFuns g f2 f3 f4 f5 f6 f7) (f f1)

-- | A lens.
rootfCombFun ∷ Lens' (RootFuns t m a r)
    (CommonADEfuns t m a ActNode r → Dynamic t (Maybe [ActiveState t ActNode r])
    → ActiveState t ActNode r
    → Dynamic t a → Event t (CompEvent t ActNode r)
    → CompState t ActNode r → m (CompEvent t ActNode r))
rootfCombFun f (RootFuns f1 f2 f3 f4 f5 f6 f7)
    = fmap (\g → RootFuns f1 g f3 f4 f5 f6 f7) (f f2)

-- | A lens.
rootfNodeAttr ∷ Lens' (RootFuns t m a r)
                      (ActiveState t ActNode r → Dynamic t A.Attr)
rootfNodeAttr f (RootFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → RootFuns f1 f2 g f4 f5 f6 f7) (f f3)

-- | A lens.
rootfWrapAttr ∷ Lens' (RootFuns t m a r)
   (Dynamic t (ActiveState t ActNode r) → ActNode → Dynamic t A.Attr)
rootfWrapAttr f (RootFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → RootFuns f1 f2 f3 g f5 f6 f7) (f f4)

-- | A lens.
rootfEUP ∷ Lens' (RootFuns t m a r) ElemUse
rootfEUP f (RootFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → RootFuns f1 f2 f3 f4 g f6 f7) (f f5)

-- | A lens.
rootfnext ∷ Lens' (RootFuns t m a r) (LevelNFuns t m a r)
rootfnext f (RootFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (\g → RootFuns f1 f2 f3 f4 f5 g f7) (f f6)

-- | A lens.
rootfMkLevelNF ∷ Lens' (RootFuns t m a r)
    (LevelNFuns t m a r
       → Dynamic t (Maybe [ActiveState t ActNode r])
       → Event t (CompEvent t ActNode r)
       → CompState t ActNode r
       → [Int] 
       → Tree (ActiveState t ActNode r, Dynamic t a)
       → m [CompEvent t ActNode r])
rootfMkLevelNF f (RootFuns f1 f2 f3 f4 f5 f6 f7) =
    fmap (RootFuns f1 f2 f3 f4 f5 f6) (f f7)

--------------------------------------------------------------------------------

-- | Listening-lens for root-nodes.
rootListener ∷ Lens' (RootFuns t m a r)
                     (ActiveState t ActNode r → ActiveState t ActNode r)
rootListener = rootfADEs . adeListen

-- | Node drawing -lens for 
-- (that are they xx-elements, to be clear).
rootDraw ∷ Lens' (RootFuns t m a r)
    (Dynamic t a → ActiveState t ActNode r
    → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r))
rootDraw = rootfADEs . adeDraw

-- | Node activity function -lens for root-nodes.
rootActivity ∷ Lens' (RootFuns t m a r)
    (Dynamic t (Maybe [ActiveState t ActNode r])
             → Event t (CompEvent t ActNode r)
             → CompState t ActNode r
             → ActiveState t ActNode r → m (ActiveState t ActNode r))
rootActivity = rootfADEs . adeActivate


--------------------------------------------------------------------------------

-- | Default value for 'RootFuns'. Uses div and a.
-- And further levels use ul and li.
defRootDivAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSretval r)
                ⇒ RootFuns t m a r
defRootDivAFuns = RootFuns defCommonADEfuns adeCombFun defALeafNodeAttrF
    defDivSFWrapAttrF elemUseDivA defLevelNUlLiFuns mkLevelN

-- | Silent value for 'RootFuns'. Uses div and a.
-- And further levels use ul and li.
silentPlainRootDivAFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                          , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSretval r)
                        ⇒ RootFuns t m a r
silentPlainRootDivAFuns = RootFuns silentPlainADE adeCombSilentPlain
    defALeafNodeAttrF
    defDivSFWrapAttrF elemUseDivA silentPlainLevelNUlLiFuns mkLevelN

--------------------------------------------------------------------------------

-- | Default value for 'RootFuns'. Uses nav and div.
-- And further levels use ul and li.
defRootNavDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSretval r)
                ⇒ RootFuns t m a r
defRootNavDivFuns = RootFuns defCommonADEfuns adeCombFun
    defDivLeafNodeAttrF
    defNavSFWrapAttrF elemUseNavDiv defLevelNUlLiFuns mkLevelN

-- | Silent value for 'RootFuns'. Uses nav and div.
-- And further levels use ul and li.
silentPlainRootNavDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                          , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSretval r)
                        ⇒ RootFuns t m a r
silentPlainRootNavDivFuns = RootFuns silentPlainADE adeCombSilentPlain
    defDivLeafNodeAttrF defNavSFWrapAttrF elemUseNavDiv
    silentPlainLevelNUlLiFuns mkLevelN


-- | Default value for 'RootFuns'. Uses nav and div.
-- And further levels use none and a.
defRootNavDivFunsB ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSretval r)
                ⇒ RootFuns t m a r
defRootNavDivFunsB = RootFuns defCommonADEfuns adeCombFun
    defDivLeafNodeAttrF
    defNavSFWrapAttrF elemUseNavDiv defLevelNNoneAFuns mkLevelN

-- | Silent value for 'RootFuns'. Uses nav and div.
-- And further levels use none and a.
silentPlainRootNavDivFunsB ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                          , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSretval r)
                        ⇒ RootFuns t m a r
silentPlainRootNavDivFunsB = RootFuns silentPlainADE adeCombSilentPlain
    defDivLeafNodeAttrF defNavSFWrapAttrF elemUseNavDiv
    silentPlainLevelNNoneAFuns mkLevelN

--------------------------------------------------------------------------------

-- | Default value for 'RootFuns'. Uses div and a.
-- And further levels use ul and li.
defRootDivDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSretval r)
                ⇒ RootFuns t m a r
defRootDivDivFuns = RootFuns defCommonADEfuns adeCombFun defDivLeafNodeAttrF
    defDivSFWrapAttrF elemUseDivDiv defLevelNUlLiFuns mkLevelN

-- | Silent value for 'LevelNFuns'. Uses div and a.
-- And further levels use ul and li.
silentPlainRootDivDivFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                          , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSretval r)
                        ⇒ RootFuns t m a r
silentPlainRootDivDivFuns = RootFuns silentPlainADE adeCombSilentPlain
    defDivLeafNodeAttrF
    defDivSFWrapAttrF elemUseDivDiv silentPlainLevelNUlLiFuns mkLevelN



-- | Default value for 'RootFuns'. Uses div and a.
-- And further levels use div and a.
defRootDivButtonFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                  , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                  , DomBuilderSpace m ~ GhcjsDomSpace
                  , ActSretval r)
                ⇒ RootFuns t m a r
defRootDivButtonFuns = RootFuns defCommonADEfuns adeCombFun defDivLeafNodeAttrF
    defDivSFWrapAttrF elemUseDivButton defLevelNDivAFuns mkLevelN

-- | Silent value for RootFuns'. Uses div and a.
-- And further levels use div and a.
silentPlainRootDivButtonFuns ∷ (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                          , DomBuilder t m, PostBuild t m, MonadJSM m -- , Show a, Eq a
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSretval r)
                        ⇒ RootFuns t m a r
silentPlainRootDivButtonFuns = RootFuns silentPlainADE adeCombSilentPlain
    defDivLeafNodeAttrF
    defDivSFWrapAttrF elemUseDivButton silentPlainLevelNDivAFuns mkLevelN




--------------------------------------------------------------------------------

-- | A function to make a root node.
-- It means specifying, how to draw and handle events.
mkRoot ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m -- , Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
       ⇒ RootFuns t m a r
       -- ^ Root-lable (of the whole tree) configurations
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode r)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode r
       -- ^ A function combining XX's can use the current 'CompState'.
       → Tree (ActiveState t ActNode r, Dynamic t a)
       -- ^ Some content...
       → m ( Element EventResult (DomBuilderSpace m) t
           , Event t (CompEvent t ActNode r))
mkRoot rFs dmlst evB trSt treeE = do
    let combF = _rootfCombFun rFs (_rootfADEs rFs)
        (elm,txt) = rootLabel treeE
        mkFun = _rootfMkLevelNF rFs
        lvlFns = _rootfnext rFs
        -- sFelms = subForest treeE  -- TODO, only for debugging
        useEltxt = _euLeafNode . _rootfEUP  $ rFs -- text
        useWraptxt = _euSFWrap . _rootfEUP  $ rFs -- text
        pth = [0] ∷ [Int]
        astD = defActiveStateTree & set activeStateElemId (ActNpath pth)
    actS ∷ ActiveState t ActNode r
          ← (_adeActivate . _rootfADEs) rFs dmlst evB trSt
             $ (_adeListen . _rootfADEs) rFs astD

    ev2 ← if not (T.null useWraptxt)
        then do
            -- text "not null wrap (mkRoot)."
            -- text $ " And  useWraptxt = " <> useWraptxt
            -- text $ " And  useEltxt = " <> useEltxt
            -- text $ ". Len sFelms = " <> (T.pack . show . length) sFelms
            elDynAttr' useWraptxt
               (_rootfWrapAttr rFs (constDyn actS) $ ActNpath pth) $ do
                    (_e,ev) ← evF combF elm txt mkFun useEltxt lvlFns
                    pure ev
        else do
            -- text "null wrap (mkRoot)."
            -- text $ " And  useEltxt = " <> useEltxt
            -- text $ " Len sFelms = " <> (T.pack . show . length) sFelms
            evF combF elm txt mkFun useEltxt lvlFns
    pure ev2
  where
    evF combF elm txt mkFun useEltxt lvlFns = mdo
        let dAttr = _rootfNodeAttr rFs $ _ceMe te
        -- E.brN blank
        -- text " mkRoot: now, combining function uses elTxt."
        (e,te) ← elDynAttr' useEltxt dAttr $
            combF dmlst elm txt evB trSt
        -- ets ← mkLevelN lvlFns dmlst evB trSt [0] treeE
        -- E.brN blank
        -- text " mkRoot: now, recurse into sub-tree. "
        ets ← mkFun lvlFns dmlst evB trSt [0] treeE
        let ev = compEvFiring $ te : ets
        pure (e,ev)

--------------------------------------------------------------------------------

