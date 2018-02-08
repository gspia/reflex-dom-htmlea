{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.StateInfo
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= State information

This module defines helpers to manage states inside cells and rows.

-}

module Reflex.Dom.HTML5.Component.Table.StateInfo
    (
    -- * Data structures
    --
    ActElem (..)
    , ActiveGroup (..)
    , ActiveState (..)

    -- * Functions for ActElem
    , sameRowAE
    , sameColAE
    , belongAE
    , rowNum
    , colNum
    , isInside

    -- * Functions for ActiveState
    , activeStateElem
    , activeStateActive
    , activeStateActivable
    , activeStateListen
    , activeStateActiveGl
    , activeStateNotActiveGl
    , activeStateNotActivableGl
    , isInsideASt
    , fstInsideASt
    , confAst2Ast
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
    , listenHeadMe
    , listenFootMe
    , listenHeadFootMe
    , listenMyCol
    , listenMyRow
    , listenNoOne

    -- * Debug aid
    , outputActiveState
    , outputActiveStateDyn
    ) where

import           Control.Lens
import           Control.Monad                  (join)
import           Data.Default                   (Default, def)
import           Data.Maybe                     (listToMaybe)
import           Data.Semigroup                 ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Reflex
import           Reflex.Dom.Core                (DomBuilder, text, dynText)

import           Reflex.Dom.HTML5.Attrs.Globals
import           Reflex.Dom.HTML5.Elements      (ePN)

--------------------------------------------------------------------------------

-- | ActElem represents a table cell, row or column. It also has a value for
-- no cell. The main use is to keep track, which cells have been "selected".
data ActElem
    = ActERC (Int, Int) -- ^ A single cell in a table (in body), (row, column).
    | ActErow Int       -- ^ A single row
    | ActEcolh Int      -- ^ A column header-cell
    | ActEcols Int      -- ^ A column sum-cell
    | ActEnone          -- ^ This refers to not any cell in a particular table.
    deriving (Eq, Show, Ord)

instance Default ActElem where
    def = ActEnone

--------------------------------------------------------------------------------


-- | Can be used to check if two cells are on the same row.
sameRowAE :: ActElem -> ActElem -> Bool
sameRowAE (ActERC (i,_)) (ActERC (k,_)) = i == k
sameRowAE (ActERC (i,_)) (ActErow k) = i == k
sameRowAE (ActErow k) (ActERC (i,_)) = i == k
sameRowAE (ActErow i) (ActErow k)    = i == k
sameRowAE _           _              = False

-- | Can be used to check if two cells are on the same column.
sameColAE :: ActElem -> ActElem -> Bool
sameColAE (ActERC (_,j)) (ActERC (k,_)) = j == k
sameColAE (ActERC (_,j)) (ActEcols k) = j == k
sameColAE (ActEcols k) (ActERC (_,j)) = j == k
sameColAE (ActEcols j) (ActEcols k)   = j == k
sameColAE (ActERC (_,j)) (ActEcolh k) = j == k
sameColAE (ActEcolh k) (ActERC (_,j)) = j == k
sameColAE (ActEcolh j) (ActEcolh k)   = j == k
sameColAE (ActEcols j) (ActEcolh k)   = j == k
sameColAE (ActEcolh j) (ActEcols k)   = j == k
sameColAE _           _               = False

-- | Can be used to check if the first is inside of the second, or other
-- way around. That is, if the cells are either on the same row, column
-- or if they are the same cells.
belongAE :: ActElem -> ActElem -> Bool
belongAE ae1 ae2 = sameRowAE ae1 ae2 || sameColAE ae1 ae2 || ae1 == ae2


-- | Can be used to check, if the area of cells contains the given cell.
--
-- Not OK ATM & TODO! This does not handle header info, yet. (The area
-- selection methods use this, see 'actAreaSelCommon'.)
isInside ∷ (ActElem,ActElem) → ActElem → Bool
isInside (ActERC (xm,ym), ActERC (xM,yM)) (ActERC (x,y))
    = xm <= x && x <= xM && ym <= y && y <= yM
isInside _ _ = False



-- | Get the row number.
rowNum :: ActElem -> Maybe Int
rowNum ae = case ae of
    ActEcolh _   -> Nothing
    ActEcols _   -> Nothing
    ActErow i    -> Just i
    ActERC (i,_) -> Just i
    ActEnone     -> Nothing

-- | Get the column number.
colNum :: ActElem -> Maybe Int
colNum ae = case ae of
    ActEcolh j   -> Just j
    ActEcols j   -> Just j
    ActERC (_,j) -> Just j
    ActErow _    -> Nothing
    ActEnone     -> Nothing


--------------------------------------------------------------------------------

-- | ActiveGroup contains a set of 'ActElem'-data.
-- The main use is to have allow to listen states and state changes of
-- a group of ActElems, see the listening functions.
newtype ActiveGroup =
    ActiveGroup (Set ActElem)
    deriving (Eq, Show, Ord)

instance Default ActiveGroup where
    def = ActiveGroup def

--------------------------------------------------------------------------------


-- | ActiveState keeps track if the ActElem is active, activable etc.
data ActiveState t = ActiveState
    { _activeStateElem        :: ActElem
    , _activeStateActive    :: Dynamic t Bool
    , _activeStateActivable :: Dynamic t Bool
    , _activeStateListen    :: Dynamic t ActiveGroup -- ^ A group to listen.
    , _activeStateActiveGl  :: Dynamic t Globals
    , _activeStateNotActiveGl  :: Dynamic t Globals
    , _activeStateNotActivableGl  :: Dynamic t Globals
    -- ^ Html class attributes. This could be used to allow individual
    -- activation effects.
    -- If there is need beyond class-attributes (to set other attributes based
    -- on if the cell is active or activable), then it is better to implement
    -- a variant of drawXX-function and use it.
    }


--------------------------------------------------------------------------------

-- | A lens.
activeStateElem :: Lens' (ActiveState t) ActElem
activeStateElem f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState g f2 f3 f4 f5 f6 f7) (f f1)

-- | A lens.
activeStateActive :: Lens' (ActiveState t) (Dynamic t Bool)
activeStateActive f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState f1 g f3 f4 f5 f6 f7) (f f2)

-- | A lens.
activeStateActivable :: Lens' (ActiveState t) (Dynamic t Bool)
activeStateActivable f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState f1 f2 g f4 f5 f6 f7) (f f3)

-- | A lens.
activeStateListen :: Lens' (ActiveState t) (Dynamic t ActiveGroup)
activeStateListen f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState f1 f2 f3 g f5 f6 f7) (f f4)

-- | A lens.
activeStateActiveGl :: Lens' (ActiveState t) (Dynamic t Globals)
activeStateActiveGl f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState f1 f2 f3 f4 g f6 f7) (f f5)

-- | A lens.
activeStateNotActiveGl :: Lens' (ActiveState t) (Dynamic t Globals)
activeStateNotActiveGl f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (\g -> ActiveState f1 f2 f3 f4 f5 g f7) (f f6)

-- | A lens.
activeStateNotActivableGl :: Lens' (ActiveState t) (Dynamic t Globals)
activeStateNotActivableGl f (ActiveState f1 f2 f3 f4 f5 f6 f7)
  = fmap (ActiveState f1 f2 f3 f4 f5 f6) (f f7)


--------------------------------------------------------------------------------

instance Reflex t ⇒ Default (ActiveState t) where
    def = ActiveState
            ActEnone
            (constDyn False) -- active
            (constDyn True)  -- activable
            (constDyn def)
            (constDyn def) (constDyn def) (constDyn def)

--------------------------------------------------------------------------------


-- | Can be used to check if the first ActElem (in the first ActiveState)
-- is covered by the list of ActElem's of the second parameter.
isInsideASt :: ActiveState t -> [ActiveState t] -> Bool
isInsideASt st1 = any (belongAE (_activeStateElem st1) . _activeStateElem)

-- | This returns the first cell (ActState) from the list that covers
-- the first cell (ActElem in ActState).
fstInsideASt :: ActiveState t -> [ActiveState t] -> Maybe (ActiveState t)
fstInsideASt st1 st2 = listToMaybe $
    filter (belongAE (_activeStateElem st1) . _activeStateElem) st2


--------------------------------------------------------------------------------

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
isActiveDyn :: forall t. Reflex t
               => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Bool
isActiveDyn = getActiveBoolField _activeStateActive

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
isActivableDyn :: forall t. Reflex t
               => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Bool
isActivableDyn = getActiveBoolField _activeStateActivable

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
getActiveBoolField :: forall t. Reflex t
               => (ActiveState t -> Dynamic t Bool)
               -> Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Bool
getActiveBoolField f dma ac = join $ fmap
    (\mast ->
        case mast of
            Nothing -> f ac
            Just astLst ->
                if null astLst
                   then constDyn True
                   else case fstInsideASt ac astLst of
                            Just ast ->
                                f ast
                            Nothing ->
                                f ac
    ) dma

-- isActivableDyn :: forall t. Reflex t
--                => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
--                -> Dynamic t Bool
-- isActivableDyn dma ac = join $ fmap
--     (\mast ->
--         case mast of
--             Nothing -> _activeStateActivable ac
--             Just astLst ->
--                 if null astLst
--                    then constDyn True
--                    else case fstInsideASt ac astLst of
--                             Just ast ->
--                                 _activeStateActivable ast
--                             Nothing ->
--                                 _activeStateActivable ac
--     ) dma

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
getNotActivableGl :: forall t. Reflex t
               => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Globals
getNotActivableGl = getActiveFieldCmn _activeStateNotActivableGl

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
getNotActiveGl :: forall t. Reflex t
               => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Globals
getNotActiveGl = getActiveFieldCmn _activeStateNotActiveGl

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
getActiveGl :: forall t. Reflex t
               => Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Globals
getActiveGl = getActiveFieldCmn _activeStateActiveGl

-- | Used with act-functions (see 'actMDMUcommon'). And in 'mkRow'
-- (see 'confAst2Ast').
getActiveFieldCmn :: forall t. Reflex t
                  => (ActiveState t -> Dynamic t Globals)
               -> Dynamic t (Maybe [ActiveState t]) -> ActiveState t
               -> Dynamic t Globals
getActiveFieldCmn f dma ac = join $ fmap
    (\mast ->
        case mast of
            Nothing -> f ac
            Just astLst ->
                if null astLst
                   then constDyn def
                   else case fstInsideASt ac astLst of
                            Just ast -> f ast
                            Nothing  -> f ac
    ) dma


--------------------------------------------------------------------------------

-- | This gets the user given attribute configurations and updates the
-- 'ActiveState' with them.
confAst2Ast :: forall t. Reflex t
            => Dynamic t (Maybe [ActiveState t])
            -> ActiveState t -> ActiveState t
confAst2Ast mae meAst =
    let dBoAv :: Dynamic t Bool = isActivableDyn mae meAst
        dBoA :: Dynamic t Bool = isActiveDyn mae meAst
        dNAvaGl = getNotActivableGl mae meAst
        dNAGl = getNotActiveGl mae meAst
        dAGl = getActiveGl mae meAst
     in meAst
            & set activeStateActive dBoA
            & set activeStateActivable dBoAv
            & set activeStateActiveGl dAGl
            & set activeStateNotActiveGl dNAGl
            & set activeStateNotActivableGl dNAvaGl


--------------------------------------------------------------------------------


-- | Activate a cell when clicked.
listenMe ∷ forall t. Reflex t ⇒ ActiveState t → ActiveState t
listenMe me = me & \d -> d {_activeStateListen = constDyn ag }
  where
    aeMe = view activeStateElem me
    ag = ActiveGroup $ Set.singleton aeMe

-- | Activate several cells at a time: based on activation of another cell,
-- the cell using this is activated.
listenList ∷ forall t. Reflex t ⇒ [ActElem] → ActiveState t → ActiveState t
listenList lst me = me & \d -> d { _activeStateListen = constDyn ag }
  where
    aeMe = view activeStateElem me
    ag = ActiveGroup $ Set.fromList $ aeMe : lst

-- | Activate another cell based on a click somewhere else.
-- A cell using this cannot be activated directly by clicking on it.
listenListNotMe ∷ forall t. Reflex t ⇒ [ActElem] → ActiveState t → ActiveState t
listenListNotMe lst me = me & \d -> d { _activeStateListen = constDyn ag}
  where
    ag = ActiveGroup $ Set.fromList lst


-- | Common parts for the 'listenHeadMe' and 'listenFootMe'.
listenHeadFoot ∷ forall t. Reflex t ⇒ (ActElem → [ActElem])
               → ActiveState t → ActiveState t
listenHeadFoot f me = me & \d -> d {_activeStateListen = constDyn ag}
  where
    aeMe = view activeStateElem me :: ActElem
    ag = ActiveGroup $ Set.fromList $ aeMe: f aeMe

-- | Activate a cell when clicked or when the column header is clicked.
listenHeadMe ∷ forall t. Reflex t ⇒ ActiveState t → ActiveState t
listenHeadMe = listenHeadFoot myHead
  where
    myHead ∷ ActElem  → [ActElem]
    myHead (ActERC (_,j)) = [ActEcolh j]
    myHead a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header is clicked.
listenFootMe ∷ forall t. Reflex t ⇒ ActiveState t → ActiveState t
listenFootMe = listenHeadFoot myFoot
  where
    myFoot ∷ ActElem → [ActElem]
    myFoot (ActERC (_,j)) = [ActEcols j]
    myFoot a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header or foot is clicked.
listenHeadFootMe ∷ forall t. Reflex t ⇒ ActiveState t → ActiveState t
listenHeadFootMe = listenHeadFoot myHF
  where
    myHF ∷ ActElem → [ActElem]
    myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
    myHF a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked on any of the column cells
-- (including sum-cells on footer) or on the column header.
listenMyCol ∷ forall t. Reflex t ⇒ Int → ActiveState t → ActiveState t
listenMyCol rows me = me & \d -> d {_activeStateListen = constDyn ag}
  where
    aeMe = view activeStateElem me :: ActElem
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen = case colNum aeMe of
        Nothing  -> []
        Just col -> [ ActERC (x,col) | x <- [0..(rows-1)]]
                    ++ [ActEcolh col, ActEcols col]

-- | Activate a cell when clicked on any of the row cells.
listenMyRow ∷ forall t. Reflex t ⇒ Int → ActiveState t → ActiveState t
listenMyRow cols me = me & \d -> d {_activeStateListen = constDyn ag}
  where
    aeMe = view activeStateElem me :: ActElem
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen = case rowNum aeMe of
        Nothing  -> []
        Just row -> [ ActERC (row, y) | y <- [0..(cols-1)]]
                    ++ [ActErow row]

-- | Listen no cell (including self).
listenNoOne ∷ forall t. Reflex t ⇒ ActiveState t → ActiveState t
listenNoOne _ = def


--------------------------------------------------------------------------------

outputActiveState :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
                  => ActiveState t -> Text -> m ()
outputActiveState aSt txt = ePN $ do
    text $ txt <> ": ActElem "
    text $ (T.pack . show) $ _activeStateElem aSt
    text ", active "
    dynText $ (T.pack . show) <$> _activeStateActive aSt
    text ", activable "
    dynText $ (T.pack . show) <$> _activeStateActivable aSt
    -- text ", activeGl "
    -- dynText $ (T.pack . show) <$> _activeStateActiveGl aSt
    -- text ", notActiveGl "
    -- dynText $ (T.pack . show) <$> _activeStateNotActiveGl aSt
    text ";"

outputActiveStateDyn :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
                     => Dynamic t (ActiveState t) -> Text -> m ()
outputActiveStateDyn daSt txt = ePN $ do
    text $ txt <> ": ActElem "
    dynText $ (T.pack . show . _activeStateElem) <$> daSt
    text ", active "
    dynText $ (T.pack . show) <$> join (_activeStateActive <$> daSt)
    text ", activable "
    dynText $ (T.pack . show) <$> join (_activeStateActivable <$> daSt)
    text ", activeGl "
    dynText $ (T.pack . show) <$> join (_activeStateActiveGl <$> daSt)
    -- text ", notActiveGl "
    -- dynText $ (T.pack . show) <$> _activeStateNotActiveGl aSt
    text ";"

