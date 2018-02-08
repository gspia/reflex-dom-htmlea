{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MainW2 where

import           Control.Arrow                          ((***))
import           Control.Lens
import           Control.Monad                          ((<=<), join)
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
import           Reflex.Dom.Core                        (mainWidget)

import qualified Data.Vector                            as W

--------------------------------------------------------------------------------

import           Reflex.Dom.HTML5.Attrs                 as A
import           Reflex.Dom.HTML5.Elements              as E
import           Reflex.Dom.HTML5.Component.Table
import           Reflex.Dom.HTML5.Elements.Tabular      as T

--------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidget examples

--------------------------------------------------------------------------------

examples ∷ MonadWidget t m => m ()
examples = do
    eH1N $ text "Welcome to reflex-dom-htmlea"
    intro
    caveats
    eH1N $ text "Vector input"
    exampleV1
    exampleV2
    exampleV3
    exampleV4
    exampleV5
    exampleV6
    exampleV7
    exampleV8
    exampleV9
    exampleV10
    exampleV11
    exampleV12
    exampleV13
    exampleV14
    eH1N $ text "Random primitive trials"
    examplePrim1
    examplePrim2
    examplePrim3

intro :: MonadWidget t m => m ()
intro =
    eDivN $ do
        ePN $ text "Here we give examples, on how to use table-components."
        ePN $ text $
          "Component-module defines different ways to " <>
          "construct, declare, initialize and use tables. "


caveats :: MonadWidget t m => m ()
caveats = do
    eH2N $ text "Caveats"
    eDivN $
      ePN $ text "Renamings are possible..."


--------------------------------------------------------------------------------

showRes :: forall t m.  (Reflex t, DomBuilder t m, PostBuild t m
                        , MonadHold t m, MonadFix m)
        => TableState t -> m ()
showRes res = do
    let eTxt = (_activeStateElem . _teMe) <$> _tsTableEvent res
        eU = fmap _activeStateElem $
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
        dynText $ (T.pack . show . _activeStateElem) <$> _tsDynEnter res
    ePN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _tsDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _tsDynUOn res
    ePN $ do
        let dDact = _tsDynDPress res
        let dUact = _tsDynURelease res
        text "Pressed down on "
        dynText $ (T.pack . show . _activeStateElem) <$> dDact
        text "."
        text " Pressdown cell is active is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dDact)
        text " and activable is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dDact)
        text ". Released up on "
        dynText $ (T.pack . show . _activeStateElem) <$> dUact
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
        dynText $ (T.pack . show .  (_activeStateElem *** _activeStateElem)) <$> dUD
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
    lstMat = [[ (b,a) | a <- [0..(i-1)]] | b <- [0..(j-1)]]

mkChrMat :: Int -> Int -> W.Vector (W.Vector (Char,Char))
mkChrMat i j =
    W.imap (\i' ev -> W.imap (\j' _ -> (chrs W.! i', chrs W.! j') ) ev) $ mkMat i j
  where
    chrs = W.fromList $ ['a'..'z'] ++ ['A'..'Z']

--------------------------------------------------------------------------------

exampleV1 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV1 = do
    let cols = 3 :: Int
        rows = 4 :: Int
        matelms :: W.Vector (W.Vector (Int,Int))
        matelms = mkMat cols rows
        tableConf = silentPlainTableConfV
            & set tableTableAttrV (style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTdAttr)
              (const $ style "padding: 3px; background-color: lightgrey" def)
    eH2N $ text "exampleV1"
    ePN $ text $ "Using td-attributes without events nor cell-states. Note "
        <> "that result should not change when clicking or trying to select. "
        <> "Function silentPlainTableConf can be used when only styling is "
        <> "applied. "
    res :: TableState t <- mkTableV tableConf matelms
    showRes res


--------------------------------------------------------------------------------

exampleV2 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV2 = do
    let cols = 6 :: Int
        rows = 4 :: Int
        tConf = set cellListenerBodyV (listenMyRow cols)
            $ set tableTableAttrV (style "border-collapse: collapse" def)
            $ set cellDrawBodyV drawDivContentEx def
    eH2N $ text "exampleV2"
    ePN $ text $ "Activating the whole row. No stylings, using draw-function "
        <> "example that shows part of the internal state. "
    res :: TableState t <- mkTableV tConf $ mkMat cols rows
    showRes res

--------------------------------------------------------------------------------

exampleV3 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV3 = do
    let cols = 6 :: Int
        rows = 4 :: Int
        tConf = def & set cellListenerBodyV (listenMyRow cols)
            & set cellDrawBodyV drawDivContentS
            & set tableTableAttrV (style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTrAttr) trAttrfun
            & set (tableTdFunsV . tdfTdAttr) (const $ style "padding: 5px" def)
    eH2N $ text "exampleV3"
    ePN $ text "Coloring the whole row based on clicks with other decorations. "
    res :: TableState t <- mkTableV tConf $ mkMat cols rows
    showRes res


--------------------------------------------------------------------------------

exampleV4 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV4 = do
    let cols = 6 :: Int
        rows = 4 :: Int
        tConf = def & set cellListenerBodyV (listenMyCol rows)
            & set cellDrawBodyV drawDivContentS
            -- & set cellDrawBodyV drawDivAECntEx
            & set tableTableAttrV (style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTdAttr) myTdAttrF
    eH2N $ text "exampleV4"
    ePN $ text "Coloring the whole column based on clicks with other decorations. "
    res :: TableState t <- mkTableV tConf $ mkMat cols rows
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: blue" def))

--------------------------------------------------------------------------------

exampleV5 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV5 = do
    let cols = 3 :: Int
        rows = 6 :: Int
        -- We use our own version of drawDivContent. Note that the header input
        -- is Text-type so there is no need to show it, while cell-contents are
        -- int-pairs and there is a need for showing and packing.
        hns = set thfThAttr (const $ constDyn $ style "width: 100px" def) $
            set (thfADEs . drawEl) drawDivContent  $
            set (thfADEs . actListen) (listenMyCol rows) def
                -- set (thfADEs . drawEl) (drawDivContent2 id)  $
        fs = CommonADEfuns
            listenHeadFootMe actMU
            -- drawDivAECntEx
            drawDivContentS
            cellEvF
            -- ownListen actMU
            -- (drawDivContent2 (T.pack . show) )
        fns2 = def & set tdfADEs fs & set tdfTdAttr myTdAttrF
        -- Note that the use of our own td-attribute function will possibly
        -- override the settings given in the table conf (see the not activable
        -- configuration). It is quite easy to give overlapping definitions to
        -- the same things (e.g. td-attributes of cells) and it can lead to
        -- confusing situations...
        matelms :: W.Vector (W.Vector (Int,Int))
        matelms = mkMat cols rows
        colVc :: W.Vector (Dynamic t ECol)
        colVc = W.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: white" def
            ) <$> [1..cols]
        txtVc :: W.Vector Text
        txtVc = W.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs :: Maybe (HeaderConfV t m)
        colDfs = Just $ HeaderConfV hns colVc txtVc def
        capDfs = Just (CaptionConf "Table example with td-attrs" $
            style ("background-color: black; color: white; " <>
                   "font-weight: bold") def)
        -- tableConf = TableConf fns2 capDfs colDfs Nothing def tableEventsEnLe
        tableConf = set tableTdFunsV fns2 $ set tableCaptionV capDfs
            $ set tableHeaderV colDfs
            -- $ set (tableTdFunsV . tdfTdAttr) mytdattrF -- hmm, see 2 lines above:
            -- since we set fns2 later, it will override the earlier one.
            $ set tableActivityConfV (constDyn $ Just [astNotActivable])
            def
    eH2N $ text "exampleV5"
    ePN $ text $ "Using td-attributes with the events and cell-states. The first "
        <> "row is not activable. "
    res :: TableState t <- mkTableV tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: black" def)
                  & set activeStateNotActivableGl
                    (style "color: white; background-color: darkgray" def))
        astNotActivable = def
            & set activeStateElem (ActErow 0)
            & set activeStateActivable (constDyn False)
            & set activeStateNotActivableGl (style "background-color: blue" def)
            -- The following won't take any effect as for cells that are not
            -- activable, only the notActivableGl attributes will be used.
            -- & set activeStateActiveGl (style "color: green" def)
            -- & set activeStateNotActiveGl (style "color: red" def)

      -- An example to play with:
      --   ownListen ae = actstate ae & set activeStateListen (constDyn ag)
      --       where
      --           isFstRow (ActERC (x,y)) = x == 0
      --           isFstRow _ = False
      --           ag = ActiveGroup $ Set.fromList $ ae: myHF ae
      --           actstate txt = def & set activeStateElem txt
      --               & set activeStateActivable (
      --                   if isFstRow ae
      --                       then constDyn False
      --                       else constDyn True)
      --           myHF ∷ ActElem → [ActElem]
      --           myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
      --           myHF a              = [a]


-- An example to play with:
-- drawDivContent2 fat _me elm actS = do
--     let dA = view activeStateActive actS
--         dNA = not <$> view activeStateActivable actS
--         dAGl = view activeStateActiveGl actS
--         dNAGl = view activeStateNotActiveGl actS
--         dNAvGl = view activeStateNotActivableGl actS
--         dUse = (\ba bna acl nacl navcl ->
--             let (st,cl)
--                     | bna = (style "background-color:darkgray", navcl)
--                     | ba  = (style "color: red", acl)
--                     | otherwise = (style "color: black", nacl)
--             -- in style "text-align: center" <> st $ def
--             in style "text-align: center" <> st $ attrSetGlobals cl def
--                 ) <$> dA <*> dNA <*> dAGl <*> dNAGl <*> dNAvGl
--     (e,_) <- eDivD' dUse $
--       text $ fat elm
--     pure e

--------------------------------------------------------------------------------

exampleV6 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV6 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        fns = def { _tdfADEs = fs } :: TdFuns t m (Int,Int)
        elemMat :: W.Vector (W.Vector (Int,Int))
        elemMat = mkMat 3 8 -- if you change 3, change txtVec's below, too.
        colVec :: W.Vector (Dynamic t ECol)
        colVec = W.fromList $ constDyn <$>
            [ style "background-color: lightgrey" def
            , style "background-color: darkgrey" def
            , style "background-color: lightgrey" def ]
        txtVec :: W.Vector Text
        txtVec = W.fromList ["col h1", " snd h", "colh3"]
        txtSumVec :: W.Vector Text
        txtSumVec = W.fromList ["sum 1", "sum 2", "sum 3"]
        ades = CommonADEfuns listenMe actMU drawDivAECntEx cellEvF
            :: CommonADEfuns t m Text
        colDfs :: Maybe (HeaderConfV t m)
        colDfs = Just $ HeaderConfV (def {_thfADEs = ades}) colVec txtVec def
        sumDfs :: Maybe (FootConfV t m)
        sumDfs = Just $ FootConfV (def {_tfootADEs = ades}) txtSumVec def
        capDfs = Just (CaptionConf "A table example" $
            style ("background-color: black; color: white; " <>
                   "letter-spacing: 2px; font-weight: bold")
            def)
        tableConf = set tableTdFunsV fns $ set tableCaptionV capDfs
            $ set tableHeaderV colDfs $ set tableFooterV sumDfs def
    eH2N $ text "exampleV6"
    ePN $ text $ "Colgroup usage " <>
        "and straigthforward decorations. Yet another way to listen events. "
    res :: TableState t <- mkTableV tableConf elemMat
    showRes res

--------------------------------------------------------------------------------

exampleV7 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV7 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMU -- we want state switching cells
            drawDivContentEx -- we draw only the content into the div's
            cellEvF
        fns = def & set tdfADEs fs & set tdfTdAttr myTdAttrF
            :: TdFuns t m (Char,Char)
        elemMat :: W.Vector (W.Vector (Char,Char))
        elemMat = mkChrMat 3 3
        tableConf = set tableTdFunsV fns def
    eH2N $ text "exampleV7"
    ePN $ text "State switching (click on different cells)."
    res :: TableState t <- mkTableV tableConf elemMat
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid purple" def)
              & set activeStateNotActiveGl (style "border: 3px solid purple" def))


--------------------------------------------------------------------------------


-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV8 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV8 = do
    let fs = CommonADEfuns
            listenMe
            actAreaMDUsel
            drawDivActElemEx -- we draw only the "cell meta info" into the div's
            cellEvF
        fns = def & set tdfADEs fs & set tdfTdAttr myTdAttrF :: TdFuns t m (Int,Int)
        tableConf = set tableTdFunsV fns def
    eH2N $ text "exampleV8"
    ePN $ text "State selection on a user selected area (single area)."
    res :: TableState t <- mkTableV tableConf $ mkMat 4 4
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid cyan" def)
              & set activeStateNotActiveGl (style "border: 3px solid cyan" def))

--------------------------------------------------------------------------------

-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV9 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV9 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMDUsel
            drawDivActElemEx
            cellEvF
        fns = def { _tdfADEs = fs } & set tdfTdAttr myTdAttrF
            :: TdFuns t m (Int,Int)
        tableConf = set tableTdFunsV fns def
    eH2N $ text "exampleV9"
    ePN $ text "State swithing on a user selected area (multiple, overlapping)"
    res :: TableState t <- mkTableV tableConf $ mkMat 4 4
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid cyan" def)
              & set activeStateNotActiveGl (style "border: 3px solid cyan" def))

--------------------------------------------------------------------------------

-- This example corresponds to the examplesPrim3 (almost).
exampleV10 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                          , TriggerEvent t m, MonadJSM m, MonadFix m
                          , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                          ) => m ()
exampleV10 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and just mouse up -events
            drawDivActElemEx   -- how to draw/make each cell
            cellEvF   -- how to construct the events cell is tracking/using
        fns = TdFuns fs (exTdElm vlen) myTdAttrF (const def) def
        -- (constDyn []) (constDyn [])
        elemr1, elemr0 :: W.Vector (Int,Int)
        elemr0 = W.fromList [(0,0), (0,1), (0,2), (0,3), (0,4)]
        elemr1 = W.fromList [(1,0), (1,1), (1,2), (1,3), (1,4)]
        elemMat :: W.Vector (W.Vector (Int,Int))
        elemMat = W.fromList [elemr0, elemr1]
        vlen = W.length elemr0
        -- tableConf = TableConf fns Nothing Nothing Nothing def tableEventsEnLe
        tableConf = def & set tableTdFunsV fns
                        & set tableTableAttrV (style "border: 3px solid purple" def)
    eH2N $ text "exampleV10"
    ePN $ text $ "Click and enter events, custom elements inside td. "
        <> "Rightmost cell is listening the cells on the left but doesn't deliver "
        <> "events. This example corresponds to the examplesPrim3"
    res :: TableState t <- mkTableV tableConf elemMat
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid purple" def)
              & set activeStateNotActiveGl (style "border: 3px solid purple" def))
        -- See tdComb for the signature. This has one extra param to give
        -- the length of a vector.
        exTdElm :: (Reflex t, MonadHold t m, TriggerEvent t m
                   , PostBuild t m, DomBuilder t m,  MonadJSM m
                   , DomBuilderSpace m ~ GhcjsDomSpace)
          => Int -> TdFuns t m a
          -> Dynamic t (Maybe [ActiveState t])
          -> ActiveState t -> a -> Event t (TableEvent t)
          -> TableState t -> m (TableEvent t)
        exTdElm vl tdFuns ae me ipair eTB tblSt = do
            let aeMe = view activeStateElem me
                ActERC (x,y) = aeMe
            if y < (vl-1)
               then tdComb tdFuns ae me ipair eTB tblSt
               else do
                   -- here we make an extra td that listens to the others
                   let aercs = ActERC <$> [(x,0), (x,1), (x,2), (x,3)]
                       tdades = _tdfADEs tdFuns
                       tdF = set (tdfADEs . actListen)
                                 (listenListNotMe aercs) tdFuns
                   tdCombLstner tdF ae me ipair eTB tblSt

--------------------------------------------------------------------------------

exampleV11 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                          , TriggerEvent t m, MonadJSM m, MonadFix m
                          , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                          ) => m ()
exampleV11 = do
    let cols = 4 :: Int
        rows = 4 :: Int
        hns = set (thfADEs . actListen) (listenMyCol rows)
            $ set (thfADEs . drawEl) drawDivAECntEx def
        ades = CommonADEfuns (listenMyCol rows) actMU drawDivAECntEx cellEvF
        foots = def {_tfootADEs = ades}
        fs = CommonADEfuns
            listenHeadFootMe -- each cell listens head, foot, and itself
            actMU            -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF          -- how to construct the events cell is tracking/using
        fns = def { _tdfADEs = fs } :: TdFuns t m (Int,Int)
        elemMat :: W.Vector (W.Vector (Int,Int))
        elemMat = mkMat cols rows
        colVec :: W.Vector (Dynamic t ECol)
        colVec = W.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: darkgrey" def
            ) <$> [1..cols]
        txtVec :: W.Vector Text
        txtVec = W.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs :: Maybe (HeaderConfV t m)
        colDfs = Just $ HeaderConfV hns colVec txtVec def
        txtSumVec :: W.Vector Text
        txtSumVec = W.fromList $ (\i -> "Sum " <> (T.pack . show ) i) <$> [1..cols]
        sumDfs :: Maybe (FootConfV t m)
        sumDfs = Just $ FootConfV foots txtSumVec def
        -- tableConf = TableConf fns Nothing colDfs sumDfs def tableEventsEnLe
        tableConf = set tableTdFunsV fns $ set tableHeaderV colDfs
            $ set tableFooterV sumDfs def
    eH2N $ text "exampleV11"
    ePN $ text "Header events, clicking on header or footer select a column. "
    res :: TableState t <- mkTableV tableConf elemMat
    showRes res

--------------------------------------------------------------------------------

exampleV12 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV12 = do
    let cols = 3 :: Int
        rows = 4 :: Int
        rNum = Just 1 :: Maybe Int
    -- If a row is initialized (say, to active), and if that is used just as
    -- a initial condition, then we have to maintain the state and give the
    -- updated row num back.
    -- To be able to tell the activated row from outside, is clearly needed.
    -- Is there need for a single initial action? That way, could build this
    -- example without rec-loop.
    rec
        let
            -- dAstInit = constDyn $ Just
            --             [ def & set activeStateElem (ActErow 0)
            dAstInit = (\mi -> Just
                        [ def & set activeStateElem
                            (case mi of
                                Just i  -> ActErow i
                                Nothing -> ActEnone)
                          & set activeStateActive (constDyn True)
                          -- & set activeStateActivable (constDyn False)
                          -- & set activeStateActiveGl
                          --     (style "background-color:grey" def)
                          -- & set activeStateNotActiveGl
                          --     (style "background-color: lightgrey" def)
                        -- , def
                        --   & set activeStateActiveGl
                        --       (style "background-color:grey" def)
                        --   & set activeStateNotActiveGl
                        --       (style "background-color: lightgrey" def)
                        ]) <$> dRow
            tConf = def & set cellListenerBodyV (listenMyRow cols)
                & set cellDrawBodyV drawDivContentS
                -- & set cellDrawBodyV drawDivAECntEx
                & set tableActivityConfV dAstInit
                & set tableTableAttrV (style "border-collapse: collapse" def)
                & set (tableTdFunsV . tdfTrAttr) trAttrfun
                -- TODO TODO remove trAttrFun
                -- Use *either* the following or the one above at dAstInit.
                -- & set (tableTdFunsV . tdfTdAttr)
                --     (\ast -> defTdAttrF (ast
                --         & set activeStateActiveGl
                --             (style "background-color:grey" def)
                --         & set activeStateNotActiveGl
                --             (style "background-color: lightgrey" def)
                --                        )
                --     )
        -- set activeStateNotActiveGl (style "border: 3px solid purple" def))
        eH2N $ text "exampleV12"
        ePN $ text $ "Coloring the whole row based on clicks with other "
            <> "decorations. "
            <> "This is otherwise the same as example 3, but initializes a row. "
        res :: TableState t <- mkTableV tConf $ mkMat cols rows
        let dUpAst = view tsDynURelease res
            dAE = _activeStateElem <$> dUpAst
            dRow2 = rowNum <$> dAE
            eRow = updated dRow2
        dRow <- holdDyn rNum eRow
    showRes res
      -- where
        -- astNotActivable = def
        --     & set activeStateElem (ActErow 0)
        --     & set activeStateActivable (constDyn False)
        -- The following colors the clicked row always, independent of
        -- its state.
        -- trAttrfun :: Dynamic t (ActiveState t) → ActElem → Dynamic t ETr
        -- trAttrfun dAst ae =
        --     let dAE = _activeStateElem <$> dAst
        --      in mkETr <$> dAE <*> pure ae
        -- mkETr :: ActElem -> ActElem -> ETr
        -- mkETr ae1 ae2 = if sameRowAE ae1 ae2
        --                    then style "background-color: grey" def
        --                    else style "background-color: lightgrey" def


--------------------------------------------------------------------------------


exampleV13 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV13 = do
    let cols = 3 :: Int
        rows = 5 :: Int
    let
        dAstInit = constDyn $ Just
                    [ def
                        & set activeStateActiveGl
                            (style "background-color:grey" def)
                        & set activeStateNotActiveGl
                            (style "background-color: lightgrey" def)
                    ]
        tConf = def & set cellListenerBodyV (listenMyRow cols)
            & set cellDrawBodyV drawDivContentS
            -- & set cellDrawBodyV drawDivAECntEx
            & set cellActivityBodyV actSwitchMU
            & set tableActivityConfV dAstInit
            & set tableTableAttrV (style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTrAttr) trAttrfun
    eH2N $ text "exampleV13"
    ePN $ text "Row selection with switching states."
    res :: TableState t <- mkTableV tConf $ mkMat cols rows
    -- let dUpAst = view tsDynURelease res
    --     dAE = _activeStateElem <$> dUpAst
    --     dRow2 = rowNum <$> dAE
    --     eRow = updated dRow2
    -- dRow <- holdDyn rNum eRow
    showRes res


trAttrfun :: forall t. Reflex t
          => Dynamic t (ActiveState t) → ActElem → Dynamic t ETr
    -- First param is the last row where an event occurred (with state info)
    -- and the second parameter tells, which row we are making (drawing,
    -- building).
trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
  where
    mkETr :: Bool -> ETr
    mkETr b =
        if b
           then style "background-color: grey" def
           else style "background-color: lightgrey" def

--------------------------------------------------------------------------------


exampleV14 :: forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) => m ()
exampleV14 = do
    let cols = 10 :: Int
        rows = 3 :: Int
        cNum = Just 3 :: Maybe Int
    rec
        let
            dAstInit = (\mi -> Just
                        [ def & set activeStateElem
                            (case mi of
                                Just i  -> ActEcols i
                                Nothing -> ActEnone)
                          & set activeStateActive (constDyn True)
                        ]) <$> dCol
            tConf = def & set cellListenerBodyV (listenMyCol rows)
                & set cellDrawBodyV drawDivContentS
                -- & set cellDrawBodyV drawDivAECntEx
                & set tableActivityConfV dAstInit
                -- & set tableTableAttrV (style "border-collapse: collapse" def)
                & set (tableTdFunsV . tdfTdAttr) myTdAttrF
        eH2N $ text "exampleV14"
        ePN $ text "Coloring the whole column based with initial selected column. "
        res :: TableState t <- mkTableV tConf $ mkMat cols rows
        let dUpAst = view tsDynURelease res
            eCol = updated $ (colNum . _activeStateElem ) <$> dUpAst
        dCol <- holdDyn cNum eCol
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: blue" def))



--------------------------------------------------------------------------------

examplePrim1 :: forall t m. (MonadWidget t m) => m ()
examplePrim1 = do
  let actstate ae = def & set activeStateElem ae
  let tblR = def :: TableState t
      ae = constDyn def
      tdntdf eTB x = eTdN $ tdFun actMD ae (def {_activeStateElem = x}) eTB tblR
  eH2N $ text "examplePrim1"
  ePN $ text "Building table with html-elems and using helpers to handle events."
  eDivN $ do
    res <- eTableN $ mdo
      (htTb,eTB) <- eTbodyN' $ do
        let dDU = _tsDynUpLDownR tblR
        et1 <- eTrN $ do
          ed1 <- tdntdf eTB $ ActERC (0,0)
          ed2 <- tdntdf eTB $ ActERC (0,1)
          ed3 <- tdntdf eTB $ ActERC (0,2)
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        et2 <- eTrN $ do
          ed1 <- tdntdf eTB $ ActERC (1,0)
          ed2 <- tdntdf eTB $ ActERC (1,1)
          ed3 <- tdntdf eTB $ ActERC (1,2)
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        et3 <- eTrN $ do
          ed1 <- tdntdf eTB $ ActERC (2,0)
          ed2 <- tdntdf eTB $ ActERC (2,1)
          ed3 <- tdntdf eTB $ ActERC (2,2)
          -- pure $ tblEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ tblEvFiring [ed1,ed2,ed3]
        pure $ leftmost [et1,et2,et3]
      tblR <- updateTableState def eTB
      pure tblR
    showRes res


--------------------------------------------------------------------------------

evAGS :: forall t m. MonadWidget t m
      => Event t ActElem -> ActiveState t -> m (ActiveState t)
-- evAGS = actUniqPrim
-- evAGS = actSwitchPrim
evAGS = actGroupsPrim

tdElm2 ::
       forall t m. MonadWidget t m
    => (Event t ActElem -> ActiveState t -> m (ActiveState t))
    -> Event t ActElem
    -> ActElem
    -> m (Event t ActElem)
tdElm2 fEvAGS eAe me = do
    --  let actstate txt = def & set activeStateElem txt
    let actstate txt = def & \d -> d {_activeStateElem = txt}
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
    -- let actstate txt = def & set activeStateElem txt
    let actstate txt = def & \d -> d {_activeStateElem = txt}
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
    -- let actstate txt = def & set activeStateElem txt
    let actstate txt = def & \d -> d {_activeStateElem = txt}
    let tblR = def :: TableState t
        r1 = (\e -> set activeStateElem e def) <$>
            (ActERC <$> [(0, 0), (0, 1), (0, 2)]) -- row 1
        r2 = (\e -> set activeStateElem e def) <$>
            (ActERC <$> [(1, 0), (1, 1), (1, 2)]) -- row 2
        r1' = ActERC <$> [(0, 0), (0, 1), (0, 2)] -- row 1
        r2' = ActERC <$> [(1, 0), (1, 1), (1, 2)] -- row 2
        -- tdFs = TdFuns listenMe actMU drawDivActElemEx cellEv tdComb
        --         (const def) (const def) def def
        fs = CommonADEfuns
            listenMe  -- each cell listens head and itself
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        tdFs = def { _tdfADEs = fs } :: TdFuns t m ()
        i = (Prelude.!!)
        ae = constDyn def
    eH2N $ text "examplePrim3"
    ePN $ text $
        "Building table with html-elems and using helpers to handle " <>
        "events. This uses the data structure " <>
        "that collects the functions and attribute declarations used with the " <>
        "table This also adds the fourth cell to listen the first three ones. "
    eDivN $ do
        eRes <- eTableN $ mdo
            (htTb, eB) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
                let dDU = _tsDynUpLDownR tblR
                -- et1 <- eTrD (_tdfTrAttr tdFs $ ActErow 0) $ do
                et1 <- eTrN $ do
                    ed1 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 0) $
                        tdComb tdFs ae (r1 `i` 0) () eB tblR
                    ed2 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 1) $
                        tdComb tdFs ae (r1 `i` 1) () eB tblR
                    ed3 <- eTdD (_tdfTdAttr tdFs $ r1 `i` 2) $
                        tdComb tdFs ae (r1 `i` 2) () eB tblR
                    eTdN $ do
                        -- let me = ActERC (0, 3)
                        let me = def & set activeStateElem (ActERC (0, 3))
                            ac = listenListNotMe r1' me
                        actS <- (_actFu . _tdfADEs) tdFs ae eB tblR ac
                        (_drawEl . _tdfADEs) tdFs () actS
                    pure $ tblEvFiring [ed1, ed2, ed3]
                -- et2 <- eTrD (_tdfTrAttr tdFs $ ActErow 1) $ do
                et2 <- eTrN $ do
                    ed1 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 0) $
                        tdComb tdFs ae (r2 `i` 0) () eB tblR
                    ed2 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 1) $
                        tdComb tdFs ae (r2 `i` 1) () eB tblR
                    ed3 <- eTdD (_tdfTdAttr tdFs $ r2 `i` 2) $
                        tdComb tdFs ae (r2 `i` 2) () eB tblR
                    eTdN $ do
                        let me = def & set activeStateElem (ActERC (1, 3))
                            ac = listenListNotMe r2' me
                        actS <- (_actFu . _tdfADEs) tdFs ae eB tblR ac
                        (_drawEl . _tdfADEs) tdFs () actS
                    pure $ tblEvFiring [ed1, ed2, ed3]
                pure $ leftmost [et1, et2]
            tblR <- updateTableState def eB
            pure tblR
        showRes eRes

--------------------------------------------------------------------------------

