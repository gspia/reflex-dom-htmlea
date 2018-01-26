{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MainW2 where

import           Control.Arrow ((***))
-- import           Control.Lens
import           Control.Monad ((<=<), join)
import           Control.Monad.Fix
import           Data.Default
import           Data.Semigroup                         ((<>))
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                             hiding (mainWidget)
-- import           Reflex.Dom.Core                        (mainWidget)

import           Data.Finite
import           Data.Singletons
-- import           Data.Singletons.Decide
import           Data.Singletons.TypeLits
import qualified Data.Vector.Sized                      as V
import qualified Data.Vector                            as W

-- import GHC.TypeLits

--------------------------------------------------------------------------------

import           Reflex.Dom.HTML5.Attrs                 as A
import           Reflex.Dom.HTML5.Elements              as E
import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Component.TableV
import           Reflex.Dom.HTML5.Component.TableVS
import           Reflex.Dom.HTML5.Component.Table.TableVSHo
import           Reflex.Dom.HTML5.Elements.Tabular      as T

--------------------------------------------------------------------------------

mainW ∷ MonadWidget t m => m ()
mainW = do
    eH1N $ text "Welcome to reflex-dom-htmlea"
    intro
    caveats
    eH1N $ text "Vector input"
    exampleV1
    exampleV2a
    exampleV2b
    exampleV2c
    exampleV3
    exampleV4
    exampleV5
    eH1N $ text "Vector-sized input"
    exampleVS1
    exampleVS2
    eH1N $ text "Random primitive trials"
    examplePrim1
    examplePrim2
    examplePrim3
    eH1N $ text "Another vector-sized thing (WIP - or to be removed)"
    exampleVSHo1

intro :: MonadWidget t m => m ()
intro =
    eDivN $ do
        ePN $ text "Here we give examples, on how to use table-components."
        ePN $ text $
          "Component-module defines different ways to " <>
          "construct, declare, initialize and use tables. " <>
          "Vector-Sized helps to ensure that header has the correct " <>
          "number of rows."


caveats :: MonadWidget t m => m ()
caveats = do
    eH2N $ text "Caveats"
    eDivN $ do
      ePN $ text "Renamings are possible..."
      ePN $ text $ "If you want to try these on android, then vector-sized "
        <> "version (mkTableVS) cannot be used "
        <> "(singletons-package has problems with cross-compiling). "
        <> "Those parts may get separated some day to another package. "


--------------------------------------------------------------------------------

showRes :: forall t m. MonadWidget t m => TableState t -> m ()
showRes res = do
    let eTxt = (_activeStateMe . _teMe) <$> _tsTableEvent res
        eU = fmap _activeStateMe $
            coincidence $ _teMUp <$> _tsTableEvent res -- ::Event t (ActiveState t)
        dUD = _tsDynUpLDownR res
    dElm <- holdDyn ActEnone eTxt
    dUpElm <- holdDyn ActEnone eU
    -- dElm <- holdDyn def eTxt
    -- dUpElm <- holdDyn def eU
    ePN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateMe) <$> _tsDynEnter res
    ePN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _tsDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _tsDynUOn res
    ePN $ do
        let dDact = _tsDynDPress res
        let dUact = _tsDynURelease res
        text "Pressed down on "
        dynText $ (T.pack . show . _activeStateMe) <$> dDact
        text "."
        text " Pressdown cell is active is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dDact)
        text " and activable is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dDact)
        text ". Released up on "
        dynText $ (T.pack . show . _activeStateMe) <$> dUact
        text "."
        text " Releaseup cell is active is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dUact)
        text " and activable is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dUact)
        text ". (Releaseup should be same as "
        dynText $ (T.pack . show) <$> dUpElm
        text ", except when releasing btn outside table.)"
    ePN $ do
        text "upper left / lower right dyn is on "
        dynText $ (T.pack . show .  (_activeStateMe *** _activeStateMe)) <$> dUD
    let dOut = _tsDynMOutsideBody res
        dIn  = _tsDynMInsideBody res
    dLInt :: Dynamic t Int <- count $ mouseOutEvent res
    dEInt :: Dynamic t Int <- count $ mouseInEvent res
    ePN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
    ePN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt


--------------------------------------------------------------------------------

-- i elems on a row, j rows.
mkMat :: Int -> Int -> W.Vector (W.Vector (Int,Int))
mkMat i j = W.fromList $ fmap W.fromList lstMat
  where
    lstMat = [[ (a,b) | a <- [0..(i-1)]] | b <- [0..(j-1)]]

mkChrMat :: Int -> Int -> W.Vector (W.Vector (Char,Char))
mkChrMat i j =
    W.imap (\i' ev -> W.imap (\j' _ -> (chrs W.! i', chrs W.! j') ) ev) $ mkMat i j
  where
    chrs = W.fromList $ ['a'..'z'] ++ ['A'..'Z']

--------------------------------------------------------------------------------

exampleV1 :: forall t m. (MonadWidget t m) => m ()
exampleV1 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        fns = defaultTdFuns { _tdfADEs = fs }
        actelems :: W.Vector (W.Vector (Int,Int))
        actelems = mkMat 3 8 -- if you change 3, change txtVec's below, too.
        colVec :: W.Vector (Dynamic t ECol)
        colVec = W.fromList $ constDyn <$>
            [ style "background-color: lightgrey" def
            , style "background-color: darkgrey" def
            , style "background-color: lightgrey" def ]
            -- [ style "background-color: yellow" def
            -- , style "background-color: green" def
            -- , style "background-color: red" def ]
        txtVec :: W.Vector Text
        txtVec = W.fromList ["col h1", " snd h", "colh3"]
        txtSumVec :: W.Vector Text
        txtSumVec = W.fromList ["sum 1", "sum 2", "sum 3"]
        colDfs :: ColHeaderV t
        colDfs = ColHeaderV colVec txtVec def
        cPair = Just (defaultThFuns, colDfs)
        sumDfs :: FootDefsV t
        sumDfs = FootDefsV txtSumVec def
        fPair = Just (defaultTfootFuns, sumDfs)
        capDfs = Just (CaptionDef "This is the first table example" $
            style ("background-color: black; color: white; " <>
                   "letter-spacing: 2px; font-weight: bold")
            def)
    eH2N $ text "exampleV1"
    ePN $ text $ "Click and enter events, vector interface. Colgroup useage " <>
        "and straigthforward decorations."
    res :: TableState t <- mkTableV fns capDfs cPair fPair actelems
    showRes res

--------------------------------------------------------------------------------

exampleV2a :: forall t m. (MonadWidget t m) => m ()
exampleV2a = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMU -- we want state switching cells
            drawDivContentEx -- we draw only the content into the div's
            cellEvF
        fns = defaultTdFuns { _tdfADEs = fs }
        actelems :: W.Vector (W.Vector (Char,Char))
        actelems = mkChrMat 3 3
    eH2N $ text "exampleV2a"
    ePN $ text "Click and enter events, state swithing, vector interface:"
    res :: TableState t <- mkTableV fns Nothing Nothing Nothing actelems
    showRes res

-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV2b :: forall t m. (MonadWidget t m) => m ()
exampleV2b = do
    let fs = CommonADEfuns
            listenMe
            actAreaMDUsel
            drawDivActElemEx -- we draw only the "cell meta info" into the div's
            cellEvF
        fns = defaultTdFuns { _tdfADEs = fs }
    eH2N $ text "exampleV2b"
    ePN $ text "State selection on a user selected area (single area)"
    res :: TableState t <- mkTableV fns Nothing Nothing Nothing $ mkMat 4 4
    showRes res

-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV2c :: forall t m. (MonadWidget t m) => m ()
exampleV2c = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMDUsel
            drawDivActElemEx
            cellEvF
        fns = defaultTdFuns { _tdfADEs = fs }
    eH2N $ text "exampleV2c"
    ePN $ text "State swithing on a user selected area (multiple, overlapping)"
    res :: TableState t <- mkTableV fns Nothing Nothing Nothing $ mkMat 4 4
    showRes res

--------------------------------------------------------------------------------

-- This example corresponds to the examplesPrim3 (almost).
exampleV3 :: forall t m. MonadWidget t m
            => m ()
exampleV3 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and just mouse up -events
            drawDivActElemEx   -- how to draw/make each cell
            cellEvF   -- how to construct the events cell is tracking/using
        -- tdF = exTdElm vlen :: TdFuns t m a -> ActElem -> a
        --                    -> Event t (TableEvent t) -> TableState t
        --                    -> m (TableEvent t)
        -- fns = defaultTdFuns { _tdADEs = fs, _tdComb = exTdElm vlen }
        -- fns = defaultTdFuns { _tdADEs = fs, _tdComb = tdF } :: TdFuns t m a
        -- fns = defaultTdFuns { _tdADEs = fs, _tdComb = tdComb }
        -- fns = defaultTdFuns { _tdADEs = fs } -- Works. Why the above ones don't?
        fns = TdFuns fs (exTdElm vlen) (const def) (const def) def def
        actelemr1, actelemr0 :: W.Vector (Int,Int)
        actelemr0 = W.fromList [(0,0), (0,1), (0,2), (0,3), (0,4)]
        actelemr1 = W.fromList [(1,0), (1,1), (1,2), (1,3), (1,4)]
        actelems :: W.Vector (W.Vector (Int,Int))
        actelems = W.fromList [actelemr0, actelemr1]
        vlen = W.length actelemr0
    eH2N $ text "exampleV3"
    ePN $ text $ "Click and enter events, custom elements inside td. "
        <> "Rightmost cell is listening the cells on the left. "
        <> "This example corresponds to the examplesPrim3"
    res :: TableState t <- mkTableV fns Nothing Nothing Nothing actelems
    showRes res
      where
        -- See tdComb for the signature. This has one extra param to give
        -- the length of a vector.
        exTdElm :: (Reflex t, MonadHold t m, TriggerEvent t m
                   , PostBuild t m, DomBuilder t m,  MonadJSM m
                   , DomBuilderSpace m ~ GhcjsDomSpace)
          => Int -> TdFuns t m a -> ActElem -> a -> Event t (TableEvent t)
          -> TableState t -> m (TableEvent t)
        exTdElm vl tdFuns me ipair eTB tblSt = do
            let ActERC (x,y) = me
            if y < (vl-1)
               then tdComb tdFuns me ipair eTB tblSt
               else do
                   -- here we make an extra td that listens to the others
                   let aercs = ActERC <$> [(x,0), (x,1), (x,2), (x,3)]
                       tdades = _tdfADEs tdFuns
                       tdF = tdFuns { _tdfADEs
                         = tdades {_actSt = listenListNotMe aercs }}
                       -- tdF = set (tdfADEs . actSt)
                       -- (listenListNotMe aercs) tdFuns
                   tdCombLstner tdF me ipair eTB tblSt

--------------------------------------------------------------------------------

exampleV4 :: forall t m. (MonadWidget t m) => m ()
exampleV4 = do
    let cols = 4 :: Int
        rows = 4 :: Int
        -- hns = set (thfADEs . actSt) (listenMyCol rows) defaultThFuns
        -- foots = set (tfootADEs . actSt) (listenMyCol rows) defaultTfootFuns
        hnsades = _thfADEs defaultThFuns
        footades = _tfootADEs defaultTfootFuns
        hns = defaultThFuns {_thfADEs = hnsades {_actSt = listenMyCol rows}}
        foots = defaultTfootFuns {_tfootADEs = footades { _actSt =listenMyCol rows}}
        fs = CommonADEfuns
            listenHeadFootMe  -- each cell listens head, foot, and itself
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        fns = defaultTdFuns { _tdfADEs = fs }
        actelems :: W.Vector (W.Vector (Int,Int))
        actelems = mkMat cols rows
        colVec :: W.Vector (Dynamic t ECol)
        colVec = W.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: darkgrey" def
            ) <$> [1..cols]
        txtVec :: W.Vector Text
        txtVec = W.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs :: ColHeaderV t
        colDfs = ColHeaderV colVec txtVec def
        txtSumVec :: W.Vector Text
        txtSumVec = W.fromList $ (\i -> "Sum " <> (T.pack . show ) i) <$> [1..cols]
        sumDfs :: FootDefsV t
        sumDfs = FootDefsV txtSumVec def
        cPair = Just (hns, colDfs)
        fPair = Just (foots, sumDfs)
    eH2N $ text "exampleV4"
    ePN $ text "Header events "
    res :: TableState t <- mkTableV fns Nothing cPair fPair actelems
    showRes res

--------------------------------------------------------------------------------

exampleV5 :: forall t m. (MonadWidget t m) => m ()
exampleV5 = do
    let cols = 3 :: Int
        rows = 6 :: Int
        -- We use our own version of drawDivContent. Note that the header input
        -- is Text-type so there is no need to show it, while cell-contents are
        -- int-pairs and there is a need for showing and packing.
        -- hns = set thfThAttr (const $ constDyn
        --         $ style "width: 100px" def) $
        --     set (thfADEs . drawEl) (drawDivContent2 id)  $
        --     set (thfADEs . actSt) (listenMyCol rows) defaultThFuns
        hnsades = _thfADEs defaultThFuns
        hns = defaultThFuns
            { _thfThAttr = const $ constDyn $ style "width: 100px" def
            , _thfADEs = hnsades
                { _drawEl = drawDivContent2 id
                , _actSt = listenMyCol rows}
            }
        fs = CommonADEfuns
            -- listenHeadFootMe actMU
            ownListen actMU
            (drawDivContent2 (T.pack . show) )
            cellEvF
        fns2 = defaultTdFuns { _tdfADEs = fs }
        -- fns2 = set tdfADEs fs defaultTdFuns
        actelms :: W.Vector (W.Vector (Int,Int))
        actelms = mkMat cols rows
        colVc :: W.Vector (Dynamic t ECol)
        colVc = W.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: white" def
            ) <$> [1..cols]
        txtVc :: W.Vector Text
        txtVc = W.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs :: ColHeaderV t
        colDfs = ColHeaderV colVc txtVc def
        cPair = Just (hns, colDfs)
        capDfs = Just (CaptionDef "Table example with class-attrs" $
            style ("background-color: black; color: white; " <>
                   "font-weight: bold") def)
    eH2N $ text "exampleV5"
    ePN $ text "Using class-attributes with the events and cell-states"
    res :: TableState t <- mkTableV fns2 capDfs cPair Nothing actelms
    showRes res
      where
        -- ownListen ae = actstate ae & set activeStateListen (constDyn ag)
        ownListen ae = actstate ae & \d -> d {_activeStateListen = constDyn ag }
            where
                isFstRow (ActERC (x,y)) = x == 0
                isFstRow _ = False
                ag = ActiveGroup $ Set.fromList $ ae: myHF ae
                -- actstate txt = def & set activeStateMe txt
                actstate txt = def & \d -> d {_activeStateMe = txt }
                    -- & set activeStateActivable
                    & \g -> g { _activeStateActivable =
                        if isFstRow ae
                            then constDyn False
                            else constDyn True
                        }
                myHF ∷ ActElem → [ActElem]
                myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
                myHF a              = [a]



drawDivContent2 fat _me elm actS = do
    -- let dA = view activeStateActive actS
    --     dNA = not <$> view activeStateActivable actS
    --     dACl = view activeStateActiveCl actS
    --     dNACl = view activeStateNotActiveCl actS
    --     dNAvCl = view activeStateNotActivableCl actS
    let dA = _activeStateActive actS
        dNA = not <$> _activeStateActivable actS
        dACl = _activeStateActiveCl actS
        dNACl = _activeStateNotActiveCl actS
        dNAvCl = _activeStateNotActivableCl actS
        dUse = (\ba bna acl nacl navcl ->
            let (st,cl)
                    | bna = (style "background-color:darkgray", navcl)
                    | ba  = (style "color: red", acl)
                    | otherwise = (style "color: black", nacl)
            in setClasses cl $ style "text-align: center" <> st $ def
                ) <$> dA <*> dNA <*> dACl <*> dNACl <*> dNAvCl
    (e,_) <- eDivD' dUse $
      text $ fat elm
    pure e


--------------------------------------------------------------------------------

-- i elems on a row, j rows.
mkVSMat :: forall r c. (KnownNat r, KnownNat c)
        => V.Vector r (V.Vector c (Int,Int))
mkVSMat = V.generate mkA4r
  where
    mkA4r ∷ KnownNat c => Int → V.Vector c (Int,Int)
    mkA4r i = V.generate (\j -> (i,j))

---- mkChrMat :: Int -> Int -> W.Vector (W.Vector (Char,Char))
---- mkChrMat i j =
----     W.imap (\i' ev -> W.imap (\j' _
----     -> (chrs W.! i', chrs W.! j') ) ev) $ mkMat i j
----   where
----     chrs = W.fromList $ ['a'..'z'] ++ ['A'..'Z']

---- mkVSVec :: KnownNat n => Sing n -> V.Vector n Text
---- mkVSVec s = V.generate (\i -> "Hmm " <> (T.pack . show) i)

exampleVS1 :: forall t m. (MonadWidget t m) => m ()
exampleVS1 = do
   let colsS1 = toSing 4
       rowsS1 = toSing 8
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS1_ rowS colS)

exampleVS1_ :: forall colS rowS t m. (KnownNat colS, KnownNat rowS
                                     , MonadWidget t m)
            => Sing rowS -> Sing colS -> m ()
exampleVS1_ rowS colS = do
   let
       rws = fromIntegral (fromSing rowS)
       -- hns = set (thfADEs . actSt) (listenMyCol rws) defaultThFuns
       hnsades = _thfADEs defaultThFuns
       hns = defaultThFuns {_thfADEs = hnsades {_actSt = listenMyCol rws}}
       fs = CommonADEfuns
           listenHeadMe  -- each cell listens head and itself
           actMU     -- and change states on mouse up -events
           drawDivActElemEx -- how to draw/make each cell
           cellEvF
       fns = defaultTdFuns { _tdfADEs = fs }
       -- actelems :: V.Vector 4 (V.Vector 4 (Int,Int))
       actelems :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
       actelems = mkVSMat
       colVec :: KnownNat colS => V.Vector colS (Dynamic t ECol)
       colVec = V.generate
           (\i -> constDyn $ if even i
                  then style "background-color: yellow" def
                  else style "background-color: green" def)
                  -- then style "background-color: lightgrey" def
                  -- else style "background-color: darkgrey" def)
       txtVec :: KnownNat colS => V.Vector colS Text
       txtVec = V.generate (\i -> "Col H " <> (T.pack . show ) i)
       colDfs :: KnownNat colS => ColHeaderVS colS t
       colDfs = ColHeaderVS colVec txtVec def
   eH2N $ text "exampleVS1"
   ePN $ text "Header events, vector-sized inputs "
   -- res :: TableState t <- mkTableVS fns Nothing actelems
   -- If using above, hns should not be defined.
   res :: TableState t
       <- mkTableVS fns Nothing (Just (hns,colDfs)) Nothing actelems
   -- res :: TableState t
   --     <- mkTableVS_ rowS colS fns (Just (hns,colDfs)) actelems
   showRes res

--------------------------------------------------------------------------------

exampleVS2 :: forall t m. (MonadWidget t m) => m ()
exampleVS2 = do
   let colsS1 = toSing 4
       rowsS1 = toSing 6
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS2_ rowS colS)

exampleVS2_ :: forall colS rowS t m. (KnownNat colS, KnownNat rowS
                                     , MonadWidget t m)
            => Sing rowS -> Sing colS -> m ()
exampleVS2_ rowS colS = do
   eH2N $ text "exampleVS2"
   ePN $ text "Header and footer events, vector-sized inputs, use of class-attrs."
   res <- mkTableVS fns capDfs (Just (hns,colDfs)) fPair actelms
   showRes res
     where
       rws = fromIntegral (fromSing rowS)
       -- hns = set thfThAttr (const $ constDyn
       --         $ style "width: 80px" def) $
       --     set (thfADEs . drawEl) (drawDivContent2 id)  $
       --     set (thfADEs . actSt) (listenMyCol rws) defaultThFuns
       hnsades = _thfADEs defaultThFuns
       hns = defaultThFuns
           { _thfThAttr = const $ constDyn $ style "width: 80px" def
           , _thfADEs = hnsades
               { _drawEl = drawDivContent2 id
               , _actSt = listenMyCol rws
               }
           }
       fs = CommonADEfuns listenHeadFootMe actMU -- select a single cell
       -- fs = CommonADEfuns (listenMyCol rws) actMU -- select the whole row
           (drawDivContent2 (T.pack . show) ) cellEvF
       fns = defaultTdFuns {_tdfADEs = fs }
       -- fns = set tdfADEs fs defaultTdFuns
       actelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
       actelms = mkVSMat
       colVec :: KnownNat colS => V.Vector colS (Dynamic t ECol)
       colVec = V.generate
           (\i -> constDyn $ if even i
                  then style "background-color: lightgrey" def
                  else style "background-color: darkgrey" def)
       txtVc :: KnownNat colS => V.Vector colS Text
       txtVc = V.generate (\i -> "Col H " <> (T.pack . show ) i)
       colDfs :: KnownNat colS => ColHeaderVS colS t
       colDfs = ColHeaderVS colVec txtVc def
       capDfs = Just (CaptionDef "Table example with class-attrs" $
           style ("background-color: black; color: white; " <>
                  "font-weight: bold") def)
       sumVec :: KnownNat colS => V.Vector colS Text
       sumVec = V.generate (\i -> "Sum C" <> (T.pack . show ) i)
       sumDfs :: FootDefsVS colS t
       sumDfs = FootDefsVS sumVec def
       -- dff = defaultTfootFuns & set (tfootADEs . drawEl) (drawDivContent2 id)
       --         & set (tfootADEs . actSt) (listenMyCol rws)
       dfftfoo = _tfootADEs defaultTfootFuns
       dff = defaultTfootFuns
               & \d -> d { _tfootADEs = dfftfoo {_drawEl = drawDivContent2 id}}
               & \g -> g {_tfootADEs = dfftfoo {_actSt = listenMyCol rws}}
       fPair = Just (dff, sumDfs)

--------------------------------------------------------------------------------

examplePrim1 :: forall t m. (MonadWidget t m) => m ()
examplePrim1 = do
    -- let actstate txt = def & set activeStateMe txt
  let actstate txt = def & \d -> d {_activeStateMe = txt}
  let tblR = def :: TableState t
  eH2N $ text "examplePrim1"
  ePN $ text "Building table with html-elems and using helpers to handle events."
  eDivN $ do
    res <- eTableN $ mdo
      (htTb,eTB) <- eTbodyN' $ do
        let dDU = _tsDynUpLDownR tblR
        et1 <- eTrN $ do
          ed1 <- eTdN $ tdFun actMD (ActERC (0,0)) eTB tblR
          ed2 <- eTdN $ tdFun actMD (ActERC (0,1)) eTB tblR
          ed3 <- eTdN $ tdFun actMD (ActERC (0,2)) eTB tblR
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        et2 <- eTrN $ do
          ed1 <- eTdN $ tdFun actMD (ActERC (1,0)) eTB tblR
          ed2 <- eTdN $ tdFun actMD (ActERC (1,1)) eTB tblR
          ed3 <- eTdN $ tdFun actMD (ActERC (1,2)) eTB tblR
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        et3 <- eTrN $ do
          ed1 <- eTdN $ tdFun actMD (ActERC (2,0)) eTB tblR
          ed2 <- eTdN $ tdFun actMD (ActERC (2,1)) eTB tblR
          ed3 <- eTdN $ tdFun actMD (ActERC (2,2)) eTB tblR
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        pure $ leftmost [et1,et2,et3]
      tblR <- updateTableState def eTB
      pure tblR
    showRes res


--------------------------------------------------------------------------------

evAGS :: forall t m. MonadWidget t m
      => Event t ActElem -> ActiveState t -> m (ActiveState t)
-- evAGS = actUniq
-- evAGS = actSwitch
evAGS = actGroups

tdElm2 ::
       forall t m. MonadWidget t m
    => (Event t ActElem -> ActiveState t -> m (ActiveState t))
    -> Event t ActElem
    -> ActElem
    -> m (Event t ActElem)
tdElm2 fEvAGS eAe me = do
    --  let actstate txt = def & set activeStateMe txt
    let actstate txt = def & \d -> d {_activeStateMe = txt}
    let ag = ActiveGroup $ Set.singleton me
        -- ac = actstate me & set activeStateListen (constDyn ag)
        ac = actstate me & \d -> d { _activeStateListen = constDyn ag }
    actS <- fEvAGS eAe ac
    (e, _) <-
        eDivN' $ do
            text $ T.pack . show $ me
            -- dynText $ (T.pack . show) <$> view activeStateActive actS
            dynText $ (T.pack . show) <$> _activeStateActive actS
    pure $ me <$ domEvent Click e


-- Note that we don't set the events tracking going inside and outside the
-- table body area.
examplePrim2 :: forall t m. (MonadWidget t m) => m ()
examplePrim2 = do
    -- let actstate txt = def & set activeStateMe txt
    let actstate txt = def & \d -> d {_activeStateMe = txt}
    eH2N $ text "examplePrim2"
    ePN $ text $
        "Building table with html-elems and using helpers to handle " <>
        "events. We define the cell-drawing method here. "
    eDivN $ do
      eRes <- eTableN $ mdo
        eB <- eTbodyN $ do
          et1 <- eTrN $ do
            ed1 <- eTdN $ tdElm2 evAGS eB $ ActERC (0,0)
            ed2 <- eTdN $ tdElm2 evAGS eB $ ActERC (0,1)
            ed3 <- eTdN $ tdElm2 evAGS eB $ ActERC (0,2)
            eTdN $ do
              let me = ActERC (0,3)
                  ag = ActiveGroup $ Set.fromList $
                      ActERC <$> [(0,0), (0,1), (0,2)]
                  ac = actstate me & \d -> d { _activeStateListen = constDyn ag }
                  --  ac = actstate me & set activeStateListen (constDyn ag)
              actS <- evAGS eB ac
              eDivN $ do
                text $ T.pack . show $ me
                -- dynText $ (T.pack . show) <$> view activeStateActive actS
                dynText $ (T.pack . show) <$> _activeStateActive actS
            pure $ leftmost [ed1,ed2,ed3]
          et2 <- eTrN $ do
            ed1 <- eTdN $ tdElm2 evAGS eB $ ActERC (1,0)
            ed2 <- eTdN $ tdElm2 evAGS eB $ ActERC (1,1)
            ed3 <- eTdN $ tdElm2 evAGS eB $ ActERC (1,2)
            eTdN $ do
              let me = ActERC (1,3)
                  rg = ActiveGroup $ Set.fromList $
                      ActERC <$> [(1,0), (1,1), (1,2)]
                  -- ac = actstate me & set activeStateListen (constDyn rg)
                  ac = actstate me & \d -> d { _activeStateListen = constDyn rg }
              actS <- evAGS eB ac
              eDivN $ do
                text $ T.pack . show $ me
                -- dynText $ (T.pack . show) <$> view activeStateActive actS
                dynText $ (T.pack . show) <$> _activeStateActive actS
            pure $ leftmost [ed1,ed2,ed3]
          pure $ leftmost [et1,et2]
        pure eB
      -- dAG <- holdDyn (ActiveGroup $ Set.singleton "None") eRes
      dAElm <- holdDyn ActEnone eRes
      ePN $ do
        text "last press is "
        -- dynText $ (T.pack . show) <$> dAG
        dynText $ (T.pack . show) <$> dAElm


--------------------------------------------------------------------------------


-- Note that we don't set the events tracking going inside and outside the
-- table body area.
examplePrim3 :: forall t m. (MonadWidget t m) => m ()
examplePrim3 = do
    -- let actstate txt = def & set activeStateMe txt
    let actstate txt = def & \d -> d {_activeStateMe = txt}
    let tblR = def :: TableState t
        r1 = ActERC <$> [(0, 0), (0, 1), (0, 2)] -- row 1
        r2 = ActERC <$> [(1, 0), (1, 1), (1, 2)] -- row 2
        -- tdFs = TdFuns listenMe actMU drawDivActElemEx cellEv tdComb
        --         (const def) (const def) def def
        fs = CommonADEfuns
            listenMe  -- each cell listens head and itself
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        tdFs = defaultTdFuns { _tdfADEs = fs }
        i = (Prelude.!!)
    eH2N $ text "examplePrim3"
    ePN $ text $
        "Building table with html-elems and using helpers to handle " <>
        "events. This uses the data structure " <>
        "that collects the functions and attribute declarations used with the " <>
        "table This also adds the fourth cell to listen the first three ones. "
    eDivN $ do
        eRes <- eTableD (_tdfTableAttr tdFs) $ mdo
            (htTb, eB) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
                let dDU = _tsDynUpLDownR tblR
                et1 <- eTrD (_tdfTrAttr tdFs $ ActErow 0) $ do
                    ed1 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 0) $
                        tdComb tdFs (r1 `i` 0) () eB tblR
                    ed2 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 1) $
                        tdComb tdFs (r1 `i` 1) () eB tblR
                    ed3 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 2) $
                        tdComb tdFs (r1 `i` 2) () eB tblR
                    eTdN $ do
                        let me = ActERC (0, 3)
                            ac = listenListNotMe r1 me
                        actS <- (_actFu . _tdfADEs) tdFs eB tblR ac
                        (_drawEl . _tdfADEs) tdFs me () actS
                    pure $ tblEvFiring [ed1, ed2, ed3]
                et2 <- eTrD (_tdfTrAttr tdFs $ ActErow 1) $ do
                    ed1 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 0) $
                        tdComb tdFs (r2 `i` 0) () eB tblR
                    ed2 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 1) $
                        tdComb tdFs (r2 `i` 1) () eB tblR
                    ed3 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 2) $
                        tdComb tdFs (r2 `i` 2) () eB tblR
                    eTdN $ do
                        let me = ActERC (1, 3)
                            ac = listenListNotMe r2 me
                        actS <- (_actFu . _tdfADEs) tdFs eB tblR ac
                        (_drawEl . _tdfADEs) tdFs me () actS
                    pure $ tblEvFiring [ed1, ed2, ed3]
                pure $ leftmost [et1, et2]
            tblR <- updateTableState def eB
            pure tblR
        showRes eRes

--------------------------------------------------------------------------------


genFR :: (KnownNat n, KnownNat m) => Finite m -> Finite n -> Text
genFR r c = f r <> ":" <> f c
  where
    f = T.pack . show . (+1) . getFinite

genVec :: (KnownNat n, KnownNat m) => Finite m -> V.Vector n Text
genVec m = V.generate_ (genFR m)

type MyCell t = TdConfHo t Text
type MyRow t = TRowConfHo t 5 Text Text
type MyFoot t = TFConfHo t 5 Text Text
type MyBody t = TBConfHo t 10 5 Text Text Text

type MyTable t = TTConfHo t 10 5 Text Text Text Text Text Text


genCell :: (KnownNat n, KnownNat m, Reflex t)
        => Finite m -> Finite n -> MyCell t
genCell r c = TdConfHo (genFR r c) ac def
  where
    -- ag = ActiveGroup $ fromInteger (getFinite c)
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show (getFinite c)
    ag = ActiveGroup $ Set.singleton $ ActErow $ fromInteger (getFinite c)
    ac = def { _activeStateListen = constDyn ag}

genRow :: forall t m. (KnownNat m, Reflex t)
       -- => Finite m -> Finite n -> MyRow t
       => Finite m -> MyRow t
genRow r = mkTrConf ("myRow " <> rt) ag def vec
  where
    rt = T.pack $ show $ getFinite r
    vec = V.generate_ (genCell r)
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show (getFinite r)
    ag = ActiveGroup $ Set.singleton $ ActEcolh $ fromInteger (getFinite r)
    --  ac = ActiveConf never never (constDyn True) (constDyn ag)

genFCell :: Reflex t => Int -> MyCell t
genFCell c = TdConfHo (T.pack $ show c) ac def
  where
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show c
    ag = ActiveGroup $ Set.singleton $ ActEcolh c
    ac = def { _activeStateListen = constDyn ag}

genFoot :: forall t. (Reflex t)
        => MyFoot t
genFoot = mkTfootConf "my foot" (ActiveGroup $ Set.singleton ActEnone) def vec
  where
    vec = V.generate genFCell


genBody' :: (KnownNat m, Reflex t) => V.Vector m (MyRow t)
genBody' = V.generate_ genRow

genBody :: forall t. (Reflex t) => MyBody t
genBody = mkTBodyConf "myTable" def genBody'
-- genBody = mkTBodyConf "myTable" def (V.generate_ genRow :: V.Vector m (MyRow t))


cs = finites :: [Finite 5]
rs = finites :: [Finite 10]
m9 = V.generate_ genRow :: Reflex (SpiderTimeline Global)
                        => V.Vector 10 (MyRow (SpiderTimeline Global))


exampleVSHo1 :: forall t m. (MonadWidget t m) => m ()
exampleVSHo1 = do
    eH2N $ text "Tables with similar columns using vector-sized"
    ePN $ text $ "Note that this interface is still heavily in WIP-state. "
        <> "This example is also in WIP-state. "
    eBN $ text "Use mkTableVS instead."
    eDivN $ do
        -- let c = 10 :: Int
        --     r = 20 :: Int
        let c = 9 :: Finite 10
            r = 19 :: Finite 20
            -- cols = [1..c]
            -- rows = [1..r]
            cols = finites :: [Finite 5]
            rows = finites :: [Finite 10]
            colNames = fmap (T.pack . show . getFinite) cols
            rowNames = fmap (T.pack . show . getFinite) rows
            txt = genFR (head rows) (head cols)
            v1 = V.generate_ (genFR (head rows)) :: V.Vector 5 Text
            m1 = V.generate_ genVec :: V.Vector 10 (V.Vector 5 Text)
            m2 = V.generate_ genRow :: V.Vector 10 (MyRow t)
            capDef = Just $ CaptionConf "tCaption" def
            footDef = genFoot :: MyFoot t
            bodyConf = genBody :: MyBody t
            tblConf =
                mkTableConf ("tTag" :: Text) capDef (Just footDef) bodyConf def
        -- ePN $ do
        --   text "txt is "
        --   text txt
        ePN $ text "table is "
        tElms <- mkTableElem tblConf
        ePN $ text "Is table ok?"



