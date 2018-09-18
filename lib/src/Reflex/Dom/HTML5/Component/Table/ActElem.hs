{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Table.ActElem
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= State information

This module defines helpers to manage states of cells, columns and rows.

About state handling:
We may want to be able to initialize a single cell, and thus the 'belongElem'
has to be able to handle that kind of situation. At the moment, states are
checked cell-by-cell, thus the the other argument in 'belongElem' is always a
cell. So is not enough to have a version of 'sameRowAE' or 'sameColAE' where
comparion between row and a cell is not permitted.

This can be solved in two ways. One is to have (a separate structures) states
also for columns and rows. And the other one is to handle groups of cells.

-}

module Reflex.Dom.HTML5.Component.Table.ActElem
    (
    -- * Data structures
    --
    ActElem (..)
    , defActElem
    , defActiveStateTbl

    -- * Functions for ActElem
    , activateCol
    , activateRow
    , activateCells
    , initializeCol
    , initializeRow
    , initializeCells
    , sameRowAE
    , sameColAE
    , rowNum
    , colNum
    , rowColNum
    -- , isInside

    -- * Listening conventions
    , listenHeadMe
    , listenFootMe
    , listenHeadFootMe
    , listenMyCol
    , listenMyRow

    -- * StateInfo and ActiveGroup
    , module Reflex.Dom.HTML5.Component.Common.StateInfo

    ) where


import           Control.Lens
import qualified Data.Set                       as Set
import           Reflex
-- import           Reflex.Dom.Core                (DomBuilder, text, dynText)

-- import           Reflex.Dom.HTML5.Attrs.Globals
-- import           Reflex.Dom.HTML5.Elements      (pN)
import           Reflex.Dom.HTML5.Component.Common.StateInfo

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


instance ActSelem ActElem where
    defElem = ActEnone
    -- | Can be used to check if the first is inside of the second, or other
    -- way around. That is, if the cells are either on the same row, column
    -- or if they are the same cells.
    belongElem ae1 ae2 = ae1 == ae2
    -- belongElem ae1 ae2 = sameRowAE ae1 ae2 || sameColAE ae1 ae2 || ae1 == ae2
    -- belongElem bf ae1 ae2 =
        -- case bf of
            -- ASEBRow → sameRowAE ae1 ae2
            -- ASEBCol → sameColAE ae1 ae2
            -- ASEBRC  → ae1 == ae2
            -- ASEB_R_C → sameRowAE ae1 ae2 || sameColAE ae1 ae2 || ae1 == ae2
            -- _ → False
    isInsidePair = isInside


-- | Can be used to check, if the area of cells contains the given cell.
--
-- Not OK ATM & TODO! This does not handle header info, yet. (The area
-- selection methods use this, see 'actAreaSelCommon'.)
isInside ∷ (ActElem,ActElem) → ActElem → Bool
isInside (ActERC (xm,ym), ActERC (xM,yM)) (ActERC (x,y))
    = xm <= x && x <= xM && ym <= y && y <= yM
isInside _ _ = False


-- | Default value for 'ActElem'.
defActElem ∷ ActElem
defActElem = ActEnone


-- | Default value for 'ActiveState' with ActElem.
defActiveStateTbl ∷ (Reflex t, ActSretval r) ⇒ ActiveState t ActElem r
defActiveStateTbl = defActiveState


--------------------------------------------------------------------------------


-- | Use with '_tableActivityConfV' in 'TableConfV', that is, this can be used
-- to set initial state for a table.
--
-- Note: often it is desirable to decorate at the same time. Make a version of
-- this function that has an extra parameter. See 'initializeCol'.
-- (Also: just make a copy of this function and implement your own version.)
activateCol ∷ (Reflex t, ActSretval r)
            ⇒ Int → Maybe Int → Maybe [ActiveState t ActElem r]
activateCol _ Nothing = Just [defActiveStateTbl & set activeStateElemId ActEnone]
activateCol rws (Just j) = Just
    [ defActiveStateTbl
    -- & set activeStateElemId (ActEcols i)
    -- above one doesn't work, as we want to be able to initialize also single
    -- cells, and thus the belongElem has to be able to handle that kind of
    -- situations. At the moment, states are checked cell-by-cell, thus the
    -- the other argument in belongElem is always a cell. So that is not enough
    -- to modify sameRowAE or sameColAE. 
    & set activeStateElemId (ActERC (i,j))
    & set activeStateActive (constDyn True)
    | i ← [0..rws-1]
    ]

-- | Like 'activateCol' but with a user given decorated element.
initializeCol ∷ Reflex t ⇒ Int → Maybe Int
              → ActiveState t ActElem r → Maybe [ActiveState t ActElem r]
initializeCol _ Nothing defe = Just [defe & set activeStateElemId ActEnone]
initializeCol rws (Just j) defe =
    Just [ defe & set activeStateElemId (ActERC (i,j)) | i ← [0..rws-1] ]

-- | Use with '_tableActivityConfV' in 'TableConfV', that is, this can be used
-- to set initial state for a table.
activateRow ∷ (Reflex t, ActSretval r)
            ⇒ Int → Maybe Int → Maybe [ActiveState t ActElem r]
activateRow _ Nothing = Just [defActiveStateTbl & set activeStateElemId ActEnone]
activateRow cls (Just i) = Just
    [ defActiveStateTbl & set activeStateElemId (ActERC (i,j))
        & set activeStateActive (constDyn True)
    | j ← [0..cls-1]
    ]

-- | Like 'activateCol' but with a user given decorated element.
initializeRow ∷ Reflex t ⇒ Int → Maybe Int
              → ActiveState t ActElem r → Maybe [ActiveState t ActElem r]
initializeRow _ Nothing defe = Just [defe & set activeStateElemId ActEnone]
initializeRow cls (Just i) defe =
    Just [ defe & set activeStateElemId (ActERC (i,j)) | j ← [0..cls-1] ]

-- | Initialize cells with the given element state.
-- If the size input params
-- correspond to the size of the table, all cells will be initialized.
-- Otherwise the cells from the upper corner to the bound indicated by
-- size parameters.
initializeCells ∷ Reflex t ⇒ Int → Int
                → ActiveState t ActElem r → Maybe [ActiveState t ActElem r]
initializeCells rws cls defe =
    Just [ defe & set activeStateElemId (ActERC (i,j))
         | i ← [0..rws-1], j ← [0..cls-1]
         ]

-- | Produce a list of cells to be activated. 
-- If the input params
-- correspond to the size of the table, all cells will be activated.
-- Otherwise the cells from the upper corner to the bound indicated by
-- parameters.
activateCells ∷ (Reflex t, ActSretval r)
              ⇒ Int → Int → Maybe [ActiveState t ActElem r]
activateCells rws cls =
    Just [ defActiveStateTbl & set activeStateElemId (ActERC (i,j))
            & set activeStateActive (constDyn True)
         | i ← [0..rws-1], j ← [0..cls-1]
         ]

--------------------------------------------------------------------------------

-- Do we need ActErow or ActEcols? Where are those used? TODO TODO


-- | Can be used to check if two cells are on the same row.
-- Note: we don't allow cell-row comparison here.
sameRowAE ∷ ActElem → ActElem → Bool
sameRowAE (ActERC (i,_)) (ActERC (k,_)) = i == k
-- sameRowAE (ActERC (i,_)) (ActErow k) = i == k
-- sameRowAE (ActErow k) (ActERC (i,_)) = i == k
sameRowAE (ActErow i) (ActErow k)    = i == k
sameRowAE _           _              = False

-- | Can be used to check if two cells are on the same column.
-- Note: we don't allow cell-col comparison here, but col header and
-- footer can be compared.
sameColAE ∷ ActElem → ActElem → Bool
sameColAE (ActERC (_,j)) (ActERC (k,_)) = j == k
-- sameColAE (ActERC (_,j)) (ActEcols k) = j == k
-- sameColAE (ActEcols k) (ActERC (_,j)) = j == k
sameColAE (ActEcols j) (ActEcols k)   = j == k
-- sameColAE (ActERC (_,j)) (ActEcolh k) = j == k
-- sameColAE (ActEcolh k) (ActERC (_,j)) = j == k
sameColAE (ActEcolh j) (ActEcolh k)   = j == k
sameColAE (ActEcols j) (ActEcolh k)   = j == k
sameColAE (ActEcolh j) (ActEcols k)   = j == k
sameColAE _           _               = False


-- | Get the row number.
rowNum ∷ ActElem → Maybe Int
rowNum ae = case ae of
    ActEcolh _   → Nothing
    ActEcols _   → Nothing
    ActErow i    → Just i
    ActERC (i,_) → Just i
    ActEnone     → Nothing

-- | Get the column number.
colNum ∷ ActElem → Maybe Int
colNum ae = case ae of
    ActEcolh j   → Just j
    ActEcols j   → Just j
    ActERC (_,j) → Just j
    ActErow _    → Nothing
    ActEnone     → Nothing

-- | Get the row and column number.
rowColNum ∷ ActElem → (Maybe Int,Maybe Int)
rowColNum ae = case ae of
    ActEcolh j   → (Nothing, Just j)
    ActEcols j   → (Nothing, Just j)
    ActERC (i,j) → (Just i, Just j)
    ActErow i    → (Just i, Nothing)
    ActEnone     → (Nothing,Nothing)

--------------------------------------------------------------------------------


-- | Activate a cell when clicked or when the column header is clicked.
listenHeadMe ∷ forall t r. Reflex t
             ⇒ ActiveState t ActElem r → ActiveState t ActElem r
listenHeadMe = listenWithFun myHead
  where
    myHead ∷ ActElem  → [ActElem]
    myHead (ActERC (_,j)) = [ActEcolh j]
    myHead a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header is clicked.
listenFootMe ∷ forall t r. Reflex t
             ⇒ ActiveState t ActElem r → ActiveState t ActElem r
listenFootMe = listenWithFun myFoot
  where
    myFoot ∷ ActElem → [ActElem]
    myFoot (ActERC (_,j)) = [ActEcols j]
    myFoot a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked or when the column header or foot is clicked.
listenHeadFootMe ∷ forall t r. Reflex t
                 ⇒ ActiveState t ActElem r
                 → ActiveState t ActElem r
listenHeadFootMe = listenWithFun myHF
  where
    myHF ∷ ActElem → [ActElem]
    myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
    myHF a              = [a]
    -- On other cases, we don't have additional sources to listen.

-- | Activate a cell when clicked on any of the column cells
-- (including sum-cells on footer) or on the column header.
listenMyCol ∷ forall t r. Reflex t
            ⇒ Int → ActiveState t ActElem r
            → ActiveState t ActElem r
listenMyCol rows me = me & \d → d {_activeStateListen = constDyn ag}
  where
    aeMe = view activeStateElemId me ∷ ActElem
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen = case colNum aeMe of
        Nothing  → []
        Just col → [ ActERC (x,col) | x ← [0..(rows-1)]]
                    ++ [ActEcolh col, ActEcols col]

-- | Activate a cell when clicked on any of the row cells.
listenMyRow ∷ forall t r. Reflex t
            ⇒ Int → ActiveState t ActElem r
            → ActiveState t ActElem r
listenMyRow cols me = me & \d → d {_activeStateListen = constDyn ag}
  where
    aeMe = view activeStateElemId me ∷ ActElem
    ag = ActiveGroup $ Set.fromList elems2listen
    elems2listen = case rowNum aeMe of
        Nothing  → []
        Just row → [ ActERC (row, y) | y ← [0..(cols-1)]]
                    ++ [ActErow row]


--------------------------------------------------------------------------------

