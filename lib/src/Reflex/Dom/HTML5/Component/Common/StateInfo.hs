{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Common.StateInfo
Description : Helpers to maintain information about the state of an
              element inside a component. (Like a cell in table or node
              in a tree structure.)
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= State information

This module defines helpers to manage states inside cells and rows, if using
table-component, and inside nodes, if using tree-component.

-}

module Reflex.Dom.HTML5.Component.Common.StateInfo
    (
    -- * Class definitions
    ActSelem  (..)
    , ActSretval  (..)

    -- * Data structures
    --
    , ActiveGroup (..)
    , defActiveGroup
    , ActiveState (..)
    , defActiveState

    -- * Functions for ActiveState
    , activeStateElemId
    , activeStateRetVal
    , activeStateActive
    , activeStateActivable
    , activeStateListen
    , activeStateActiveGl
    , activeStateNotActiveGl
    , activeStateNotActivableGl

    -- , fstInsideASt
    , isActiveDyn
    , isActivableDyn
    , getActiveBoolField
    , getActiveGl
    , getNotActiveGl
    , getNotActivableGl

    -- * Listening conventions
    , listenMe
    , listenList
    , listenListNotMe
    , listenWithFun
    -- , listenListWithFilter
    , listenNoOne

    -- * Helper to build attribute modifying functions for HTML-elements.
    , defActivationAttrF

    -- * Debug aid
    , outputActiveState
    , outputActiveStateDyn
    ) where

import           Control.Lens
import           Control.Monad                  (join)
import           Data.Maybe                     (listToMaybe)
import           Data.Semigroup                 ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Reflex
import           Reflex.Dom.Core                (DomBuilder, text, dynText)

import           Reflex.Dom.HTML5.Attrs.Globals
import           Reflex.Dom.HTML5.Elements      (pN)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | A set of methods that we need from "activity items" for the
-- 'ActiveState' to work, that is, to initiate it's structures and
-- change the state..
class ActSelem e where
    defElem ∷ e
    -- ^ A default value of @e@. This is used as an id-value of an element.
    -- As the elements in the components are collections that may have
    -- similar visible appearance, we need a way to distinguish between 
    -- elements in the collections.
    belongElem ∷ e → e → Bool
    -- ^ This gives a similarity of the elements, see 'ActElem' and
    -- 'fstInsideASt'. This can be used to listen the
    -- activity of other elements and act on the changes on those.
    isInsidePair ∷ (e,e) → e → Bool
    -- ^ This can be used with area selections, where an area is defined
    -- with a pair. In tables the pair correspondes to two coordinates.
    -- In trees, it can be a path between the elements, any element in the
    -- subtree bounded by the path between the elements, or elements
    -- between the elements, if the elements are on the same level (depth
    -- and subtree).

-- | The drawing functions draw things that use specify, containing possibly
-- other html elements together with user specified logic.
-- Components deliver results that are instances of this class.
class ActSretval r where
    defRetval ∷ r
    -- ^ A default value of @r@. This is the type returned by drawing function
    -- and is included in the returned component (event/dynamic).

-- |
instance (Reflex t) ⇒ ActSretval (Event t a) where defRetval = never
instance (Reflex t, ActSretval a) ⇒ ActSretval (Dynamic t a)
    where defRetval = constDyn defRetval

instance ActSretval () where defRetval = ()
instance ActSretval Bool where defRetval = False
instance ActSretval Int where defRetval = 0
instance ActSretval Double where defRetval = 0.0
instance ActSretval Char where defRetval = ' '
instance ActSretval Text where defRetval = T.empty

-- | ActiveGroup contains a set of elements that deliver activity information.
-- The main use is to have allow to listen states and state changes of
-- a group of elements, see the listening functions. This way, e.g., we can
-- select a whole row or column in the case tables.
-- (For a concrete example, see 'ActElem'-data).
newtype ActiveGroup a = ActiveGroup (Set a)
    deriving (Eq, Show, Ord)

-- | The default value for ActiveGroup is an empty set.
defActiveGroup ∷ ActSelem e ⇒ ActiveGroup e
defActiveGroup = ActiveGroup Set.empty

--------------------------------------------------------------------------------


-- | ActiveState keeps track if the element @e@ is active, activable etc.
-- Element @e@ should be an instance of 'ActSelem'-class.
--
-- The dynamic globals set attributes. These can be used to allow individual
-- activation effects inside drawing functions.
--
-- If there is a need beyond global-attributes (to set other attributes based
-- on if the element is active or activable), then it is better to implement
-- a variant of drawXX-function and use it. Note: as 'Globals' can be used
-- with any html element, the drawing functions can implement div's, a's,
-- ul's, li's etc. and use these attributes.
data ActiveState t e r = ActiveState
    { _activeStateElemId       ∷ e
    -- ^ This is used as an id-value of an element.
    -- As the elements in the components are collections that may have
    -- similar visible appearance, we need a way to distinguish between 
    -- elements in the collections.
    , _activeStateRetVal       ∷ Dynamic t r
    -- ^ This is used as a return value of a component.
    , _activeStateActive       ∷ Dynamic t Bool
    -- ^ True, if element is active. Default value is False.
    , _activeStateActivable    ∷ Dynamic t Bool
    -- ^ True, if element is activable. Default value is True.
    , _activeStateListen       ∷ Dynamic t (ActiveGroup e)
    -- ^ A group of other elements to listen.
    , _activeStateActiveGl     ∷ Dynamic t Globals
    -- ^ Attributes to use, when '_activeStateActive' is True.
    -- The drawing methods use these attributes.
    , _activeStateNotActiveGl  ∷ Dynamic t Globals
    -- ^ Attributes to use, when '_activeStateActive' is False.
    -- The drawing methods use these attributes.
    , _activeStateNotActivableGl ∷ Dynamic t Globals
    -- ^ Attributes to use, when '_activeStateActivable' is False.
    -- If is is True, then the above '_activeStateActive' and
    -- '_activeStateActive' as in use.
    -- The drawing methods use these attributes.
    }

    --  _activeStateElemId       ∷ Dynamic t e

--------------------------------------------------------------------------------

-- | Default value for 'ActiveState'. As @e@ refers to the 'ActSelem'-class,
-- it may be nice to define concrete defaults for the component in hand.
-- See, e.g, 'defActiveStateTbl'.
defActiveState ∷ (ActSelem e, ActSretval r, Reflex t) ⇒ ActiveState t e r
defActiveState = ActiveState defElem (constDyn defRetval)
    (constDyn False) -- active
    (constDyn True)  -- activable
    (constDyn defActiveGroup)
    (constDyn defGlobals) (constDyn defGlobals) (constDyn defGlobals)


--------------------------------------------------------------------------------

-- | A lens.
activeStateElemId ∷ Lens' (ActiveState t e r) e
-- activeStateElemId ∷ Lens' (ActiveState t e r) (Dynamic t e)
activeStateElemId f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState g f2 f3 f4 f5 f6 f7 f8) (f f1)

-- | A lens.
activeStateRetVal ∷ Lens' (ActiveState t e r) (Dynamic t r)
activeStateRetVal f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 g f3 f4 f5 f6 f7 f8) (f f2)

-- | A lens.
activeStateActive ∷ Lens' (ActiveState t e r) (Dynamic t Bool)
activeStateActive f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 f2 g f4 f5 f6 f7 f8) (f f3)

-- | A lens.
activeStateActivable ∷ Lens' (ActiveState t e r) (Dynamic t Bool)
activeStateActivable f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 f2 f3 g f5 f6 f7 f8) (f f4)

-- | A lens.
activeStateListen ∷ Lens' (ActiveState t e r) (Dynamic t (ActiveGroup e))
activeStateListen f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 f2 f3 f4 g f6 f7 f8) (f f5)

-- | A lens.
activeStateActiveGl ∷ Lens' (ActiveState t e r) (Dynamic t Globals)
activeStateActiveGl f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 f2 f3 f4 f5 g f7 f8) (f f6)

-- | A lens.
activeStateNotActiveGl ∷ Lens' (ActiveState t e r) (Dynamic t Globals)
activeStateNotActiveGl f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (\g → ActiveState f1 f2 f3 f4 f5 f6 g f8) (f f7)

-- | A lens.
activeStateNotActivableGl ∷ Lens' (ActiveState t e r) (Dynamic t Globals)
activeStateNotActivableGl f (ActiveState f1 f2 f3 f4 f5 f6 f7 f8)
  = fmap (ActiveState f1 f2 f3 f4 f5 f6 f7) (f f8)


--------------------------------------------------------------------------------

-- | This returns the first element ('ActState') from the list that covers
-- the first parameter (element in ActState).
fstInsideASt ∷ ActSelem e
             ⇒ ActiveState t e r → [ActiveState t e r] → Maybe (ActiveState t e r)
fstInsideASt st1 st2 = listToMaybe $
    filter (belongElem (_activeStateElemId st1) . _activeStateElemId) st2


    {-
fstInsideASt ∷ ActSelem e ⇒ ActiveState t e r
             → Dynamic t (Maybe ([ActiveState t e r]))
             → Dynamic t (Maybe (ActiveState t e r))
fstInsideASt st1 st2 =
    listToMaybe $
    filter (belongElem (_activeStateElemId st1) . _activeStateElemId) st2
  where
    dE1 = _activeStateElemId st1

    pLst ∷ [(ActiveState t e r, ActiveState t e r)] = fmap (\s → (st1,s) st2
    pLst2 ∷ [(Dynamic t Bool, ActiveState t e r, ActiveState t e r)]
        = fmap (\(s1,s2) →
                let dE1 = _activeStateElemId s1
                    dE2 = _activeStateElemId s2
                    dB = zipDynWith belongElem dE1 dE2
                 in (dB,s1,s2)
               ) pLst
-}


--------------------------------------------------------------------------------

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
isActiveDyn ∷ forall t e r. (Reflex t, ActSelem e)
            ⇒ Dynamic t (Maybe [ActiveState t e r])
            → ActiveState t e r
            → Dynamic t Bool
isActiveDyn = getActiveBoolField _activeStateActive

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
isActivableDyn ∷ forall t e r. (Reflex t, ActSelem e)
               ⇒ Dynamic t (Maybe [ActiveState t e r])
               → ActiveState t e r
               → Dynamic t Bool
isActivableDyn = getActiveBoolField _activeStateActivable

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
--
-- If the provided maybe-list is empty, then use the last param to
-- get a dynamic value. Otherwise, try to find from the maybe-list
-- and apply it.
getActiveBoolField ∷ forall t e r. (Reflex t, ActSelem e)
               ⇒ (ActiveState t e r → Dynamic t Bool)
               → Dynamic t (Maybe [ActiveState t e r])
               → ActiveState t e r
               → Dynamic t Bool
getActiveBoolField f dma ac = join $ fmap
    (\mast →
        case mast of
            Nothing → f ac
            Just astLst →
                if null astLst
                   then f ac
                   -- then constDyn True
                   -- then constDyn False
                   else case fstInsideASt ac astLst of
                            Just ast → f ast
                            Nothing  → f ac
    ) dma


-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
getNotActivableGl ∷ forall t e r. (Reflex t, ActSelem e)
                  ⇒ Dynamic t (Maybe [ActiveState t e r])
                  → ActiveState t e r → Dynamic t Globals
getNotActivableGl = getActiveFieldCmn _activeStateNotActivableGl

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
getNotActiveGl ∷ forall t e r. (Reflex t, ActSelem e)
               ⇒ Dynamic t (Maybe [ActiveState t e r])
               → ActiveState t e r
               → Dynamic t Globals
getNotActiveGl = getActiveFieldCmn _activeStateNotActiveGl

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
getActiveGl ∷ forall t e r. (Reflex t, ActSelem e)
               ⇒ Dynamic t (Maybe [ActiveState t e r])
               → ActiveState t e r
               → Dynamic t Globals
getActiveGl = getActiveFieldCmn _activeStateActiveGl

-- | Used with act-functions. In the case of tables, see 'actMDMUcommon'.
-- And 'mkRow'.
getActiveFieldCmn ∷ forall t e r. (Reflex t, ActSelem e)
                  ⇒ (ActiveState t e r → Dynamic t Globals)
                  → Dynamic t (Maybe [ActiveState t e r])
                  → ActiveState t e r
                  → Dynamic t Globals
getActiveFieldCmn f dma ac = join $ fmap
    (\mast →
        case mast of
            Nothing → f ac
            Just astLst →
                if null astLst
                   then f ac
                   -- then constDyn defGlobals
                   else case fstInsideASt ac astLst of
                            Just ast → f ast
                            Nothing  → f ac
    ) dma


--------------------------------------------------------------------------------

{-
-- | This gets the user given attribute configurations and updates the
-- 'ActiveState' with them.
confAst2Ast ∷ forall t e r. (Reflex t, ActSelem e)
            ⇒ Dynamic t (Maybe [ActiveState t e r])
            → ActiveState t e r
            → ActiveState t e r
confAst2Ast mae meAst =
    let dBoAv ∷ Dynamic t Bool = isActivableDyn mae meAst
        dBoA ∷ Dynamic t Bool = isActiveDyn  mae meAst
        dNAvaGl = getNotActivableGl mae meAst
        dNAGl = getNotActiveGl mae meAst
        dAGl = getActiveGl  mae meAst
     in meAst
            & set activeStateActive dBoA
            & set activeStateActivable dBoAv
            & set activeStateActiveGl dAGl
            & set activeStateNotActiveGl dNAGl
            & set activeStateNotActivableGl dNAvaGl
-}

--------------------------------------------------------------------------------


-- | Activate an element when clicked.
listenMe ∷ forall t e r. Reflex t ⇒ ActiveState t e r → ActiveState t e r
listenMe me = me & \d → d {_activeStateListen = constDyn ag }
  where
    aeMe = view activeStateElemId me
    ag = ActiveGroup $ Set.singleton aeMe


-- | Activate also another element at a time: based on activation of
-- an element,
-- the element using this is activated.
listenList ∷ forall t e r. (Ord e, Reflex t)
           ⇒ [e] → ActiveState t e r → ActiveState t e r
listenList lst me = me & \d → d { _activeStateListen = constDyn ag }
  where
    aeMe = view activeStateElemId me
    ag = ActiveGroup $ Set.fromList $ aeMe : lst

-- | Activate another element based on a click somewhere else.
-- An element using this cannot be activated directly by clicking on it.
listenListNotMe ∷ forall t e r. (Ord e, Reflex t)
                ⇒ [e] → ActiveState t e r → ActiveState t e r
listenListNotMe lst me = me & \d → d { _activeStateListen = constDyn ag}
  where
    ag = ActiveGroup $ Set.fromList lst


-- | Common parts for the 'listenHeadMe' and 'listenFootMe'. See Table-component
-- and there 'ActElem'.
listenWithFun ∷ forall t e r. (Ord e, Reflex t)
              ⇒ (e → [e]) → ActiveState t e r → ActiveState t e r
listenWithFun f me = me & \d → d {_activeStateListen = constDyn ag}
  where
    -- aeMe = view activeStateElemId me ∷ ActElem
    aeMe = view activeStateElemId me ∷ e
    ag = ActiveGroup $ Set.fromList $ aeMe: f aeMe

{-
-- Use list of elements to listen with the function
-- given in the first param.
listenListWithFilter ∷ forall t e r. (Ord e, Reflex t)
                    ⇒ [e] → (e → Bool)
                    → ActiveState t e r → ActiveState t e r
listenListWithFilter fPos fNeg me = me & \d → d {_activeStateListen = constDyn ag}
  where
    -- aeMe = view activeStateElemId me ∷ ActElem
    aeMe = view activeStateElemId me ∷ e
    initSet = aeMe: fPos aeMe
    tobeRemoved = fNeg aeMe
    useSet = filter (`notElem` tobeRemoved) initSet
    ag = ActiveGroup $ Set.fromList $ useSet
    -- ag = ActiveGroup $ Set.fromList $ aeMe: fPos aeMe
-}


-- | Listen no element (including self).
listenNoOne ∷ forall t e r. (ActSelem e, ActSretval r, Reflex t)
            ⇒ ActiveState t e r → ActiveState t e r
listenNoOne _ = defActiveState

--------------------------------------------------------------------------------


-- | Default logic for attributes based on activation state.  This sets
-- attributes for activated, not activated and non-activable states.
-- To make a concrete element, feed in a html-element structure,
-- i.e. 'defLi' or 'defTd'.
-- See, e.g., 'defLiAttrF' or 'defTdAttrF'.
defActivationAttrF ∷ forall t elm e r. (AttrHasGlobals elm, Reflex t, ActSelem e)
                   ⇒ elm → ActiveState t e r → Dynamic t elm
defActivationAttrF elm ast =
    let dA = view activeStateActive ast
        dAv = view activeStateActivable ast
        dAGl = view activeStateActiveGl ast
        dNAGl = view activeStateNotActiveGl ast
        dNAvGl = view activeStateNotActivableGl ast
     in (\bA bAv gA gNa gNav → attrSetGlobals
         (if bAv
            then if bA
                then gA
                else gNa
            else gNav
         ) elm
        ) <$> dA <*> dAv <*> dAGl <*> dNAGl <*> dNAvGl



--------------------------------------------------------------------------------


-- | To help debugging.
outputActiveState ∷ forall t m e r.
                    (Reflex t, ActSelem e, Show e, DomBuilder t m, PostBuild t m)
                  ⇒ ActiveState t e r → Text → m ()
outputActiveState aSt txt = pN $ do
    text $ txt <> ": ActElem "
    text $ (T.pack . show . _activeStateElemId) aSt
    text ", active "
    dynText $ (T.pack . show) <$> _activeStateActive aSt
    text ", activable "
    dynText $ (T.pack . show) <$> _activeStateActivable aSt
    -- text ", activeGl "
    -- dynText $ (T.pack . show) <$> _activeStateActiveGl aSt
    -- text ", notActiveGl "
    -- dynText $ (T.pack . show) <$> _activeStateNotActiveGl aSt
    text ";"

-- | To help debugging.
outputActiveStateDyn ∷ forall t m e r.
                       (Reflex t, ActSelem e, Show e, DomBuilder t m, PostBuild t m)
                     ⇒ Dynamic t (ActiveState t e r) → Text → m ()
outputActiveStateDyn daSt txt = pN $ do
    text $ txt <> ": ActElem "
    dynText $ (T.pack . show . _activeStateElemId) <$> daSt
    -- dynText $ (T.pack . show) <$> join (_activeStateElemId) <$> daSt)
    text ", active "
    dynText $ (T.pack . show) <$> join (_activeStateActive <$> daSt)
    text ", activable "
    dynText $ (T.pack . show) <$> join (_activeStateActivable <$> daSt)
    text ", activeGl "
    dynText $ (T.pack . show) <$> join (_activeStateActiveGl <$> daSt)
    -- text ", notActiveGl "
    -- dynText $ (T.pack . show) <$> _activeStateNotActiveGl aSt
    text ";"


