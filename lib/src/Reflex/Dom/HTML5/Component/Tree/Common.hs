{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Tree.Common
Description : Common helpers to build tree-based components.
Copyright   : (c) gspia 2018 -
License     : BSD
Maintainer  : gspia

= Tree.Common

About names: SF is for sub forest.

-}

module Reflex.Dom.HTML5.Component.Tree.Common
    ( isLeaf
    , ElemUse (..)
    , elemUseUlLi
    , elemUseDivA
    , elemUseNavDiv
    , elemUseDivDiv
    , elemUseDivButton
    , elemUseNoneDiv
    , elemUseNoneA
    , defALeafNodeAttrF
    , defLiLeafNodeAttrF
    , defDivLeafNodeAttrF
    , defUlSFWrapAttrF
    , defDivSFWrapAttrF
    , defNavSFWrapAttrF
    ) where

import           Data.Text (Text)
-- import qualified Data.Text as T
import           Data.Tree (Tree,subForest)

-- import           Language.Javascript.JSaddle
import           Reflex (Dynamic,Reflex)
-- import           Reflex.Dom.Core

import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E
import           Reflex.Dom.HTML5.Component.Tree.ActNode (ActNode,
    ActiveState, defActivationAttrF)

--------------------------------------------------------------------------------


-- | This can be used to define the wrapper-container -structure, e.g. ul
-- and li-elements.
data ElemUse = ElemUse
    { _euSFWrap ∷ Text
    -- ^ A html element as a text given to elDynAttr.
    -- This wraps nodes in a given level, like "ul".
    , _euLeafNode ∷ Text
    -- ^ A html element as a text given to elDynAttr.
    -- This is for each node in a given level, like "li".
    }


-- | To be used with 'LevelNFuns'.
elemUseUlLi ∷ ElemUse
elemUseUlLi = ElemUse "ul" "li" 

-- | To be used with 'RootFuns' and LevelNFuns.
elemUseDivA ∷ ElemUse
elemUseDivA = ElemUse "div" "a" 

-- | To be used with 'RootFuns'.
elemUseNavDiv ∷ ElemUse
elemUseNavDiv = ElemUse "nav" "div"

-- | To be used with 'RootFuns'.
elemUseDivDiv ∷ ElemUse
elemUseDivDiv = ElemUse "div" "div" 

-- | To be used with 'RootFuns'.
elemUseDivButton ∷ ElemUse
elemUseDivButton = ElemUse "div" "button"


-- | To be used with 'RootFuns' and LevelNFuns.
-- Note that this doesn't use wrapper meaning that the list
-- is output directly.
elemUseNoneDiv ∷ ElemUse
elemUseNoneDiv = ElemUse "" "div"

-- | To be used with 'RootFuns' and LevelNFuns.
-- Note that this doesn't use wrapper meaning that the list
-- is output directly.
elemUseNoneA ∷ ElemUse
elemUseNoneA = ElemUse "" "a"



--------------------------------------------------------------------------------


-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defALeafNodeAttrF ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActNode r → Dynamic t A.Attr
defALeafNodeAttrF ast = A.attrMap <$> defActivationAttrF E.defA ast
-- TODO is this right module for the above function?

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defLiLeafNodeAttrF ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActNode r → Dynamic t A.Attr
defLiLeafNodeAttrF ast = A.attrMap <$> defActivationAttrF E.defLi ast

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defDivLeafNodeAttrF ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActNode r → Dynamic t A.Attr
defDivLeafNodeAttrF ast = A.attrMap <$> defActivationAttrF E.defDiv ast

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defUlSFWrapAttrF ∷ forall t r. Reflex t
                 ⇒ Dynamic t (ActiveState t ActNode r) → ActNode → Dynamic t A.Attr
defUlSFWrapAttrF _ _ = pure $ A.attrMap E.defUl


-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defDivSFWrapAttrF ∷ forall t r. Reflex t
                 ⇒ Dynamic t (ActiveState t ActNode r) → ActNode → Dynamic t A.Attr
defDivSFWrapAttrF _ _ = pure $ A.attrMap E.defDiv

-- | A method that implements the active-activable logic and applies the
-- attributes correspondingly.
defNavSFWrapAttrF ∷ forall t r. Reflex t
                 ⇒ Dynamic t (ActiveState t ActNode r) → ActNode → Dynamic t A.Attr
defNavSFWrapAttrF _ _ = pure $ A.attrMap E.defNav

--------------------------------------------------------------------------------

-- | Helper.
isLeaf ∷ Tree a → Bool
isLeaf = null . subForest


