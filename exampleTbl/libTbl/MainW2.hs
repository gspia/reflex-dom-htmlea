{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE FlexibleContexts    #-} -- because of delay (MonadIO (Performable m))

module MainW2 where

import           Control.Arrow                          ((***))
import           Control.Lens
import           Control.Monad                          ((<=<), join)
import           Control.Monad.Fix
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Semigroup                         ((<>))
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import qualified Data.Text                              as T
-- import           Data.Void
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                             hiding (mainWidget)
import           Reflex.Dom.Core                        (mainWidget)

import qualified Data.Vector                            as W

--------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs                 as A
import qualified Reflex.Dom.HTML5.Elements              as E
import qualified Reflex.Dom.HTML5.Elements.Tabular      as T
import           Reflex.Dom.HTML5.Component.Table
import           Reflex.Dom.HTML5.Component.Common.RfpUtils
-- import           Reflex.Dom.HTML5.Component.Common.CompEvent

--------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidget examples

--------------------------------------------------------------------------------

examples ∷ MonadWidget t m ⇒ m ()
examples = do
    E.h1N $ text "Welcome to reflex-dom-htmlea"
    intro
    caveats
    E.h1N $ text "Vector input"
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
    exampleV15
    exampleV16
    E.h1N $ text "Random primitive trials"
    examplePrim1
    examplePrim2
    examplePrim3

intro ∷ MonadWidget t m ⇒ m ()
intro =
    E.divN $ do
        E.pN $ text "Here we give examples, on how to use table-components."
        E.pN $ text $
          "Component-module defines different ways to " <>
          "construct, declare, initialize and use tables. "


caveats ∷ MonadWidget t m ⇒ m ()
caveats = do
    E.h2N $ text "Caveats"
    E.divN $
      E.pN $ text "Renamings are possible..."


--------------------------------------------------------------------------------

showRes ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                        , MonadHold t m, MonadFix m)
        ⇒ CompState t ActElem r → m ()
showRes res = do
    let eTxt = (_activeStateElemId . _ceMe) <$> _csCompEvent res
    eU2 ← switchHold never $ _ceMUp <$> _csCompEvent res
    -- eU2 ← switchPromptly never $ _ceMUp <$> _csCompEvent res
    let eU = fmap _activeStateElemId $ eU2
        -- coincidence $ _ceMUp <$> _csCompEvent res
            -- ::Event t (ActiveState t ActElem)
        dUD = _csDynUpLDownR res
    dElm ← holdDyn ActEnone eTxt
    dUpElm ← holdDyn ActEnone eU
    -- dElm ← holdDyn def eTxt
    -- dUpElm ← holdDyn def eU
    E.pN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateElemId) <$> _csDynEnter res
    E.pN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _csDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _csDynUOn res
    E.pN $ do
        let dDact = _csDynDPress res
        let dUact = _csDynURelease res
        text "Pressed down on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dDact
        text "."
        text " Pressdown cell is active is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dDact)
        text " and activable is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dDact)
        text ". Released up on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dUact
        text "."
        text " Releaseup cell is active is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dUact)
        text " and activable is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dUact)
        text ". (Releaseup should be same as "
        dynText $ (T.pack . show) <$> dUpElm
        text ", except when releasing btn outside table.)"
    E.pN $ do
        text "upper left / lower right dyn is on "
        dynText $ (T.pack . show .  (_activeStateElemId *** _activeStateElemId)) <$> dUD
    let dOut = _csDynMOutsideBody res
        dIn  = _csDynMInsideBody res
    dLInt ∷ Dynamic t Int ← count $ mouseOutEvent res
    dEInt ∷ Dynamic t Int ← count $ mouseInEvent res
    E.pN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
    E.pN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt


--------------------------------------------------------------------------------

-- i elems on a row, j rows.
mkMat ∷ Int → Int → W.Vector (W.Vector (Int,Int))
mkMat i j = W.fromList $ fmap W.fromList lstMat
  where
    lstMat = [[ (b,a) | a ← [0..(i-1)]] | b ← [0..(j-1)]]

mkChrMat ∷ Int → Int → W.Vector (W.Vector (Char,Char))
mkChrMat i j =
    W.imap (\i' ev → W.imap (\j' _ → (chrs W.! i', chrs W.! j') ) ev) $ mkMat i j
  where
    chrs = W.fromList $ ['a'..'z'] ++ ['A'..'Z']

--------------------------------------------------------------------------------

exampleV1 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV1 = do
    let cols = 3 ∷ Int
        rows = 4 ∷ Int
        matelms ∷ W.Vector (W.Vector (Int,Int))
        matelms = mkMat cols rows
        tableConf = silentPlainTableConf
            & set cellDrawBody drawDivContentS
            & set tableTableAttr (A.style "border-collapse: collapse"
                                 $ pure E.defTable)
            & set (tableTdFuns . tdfTdAttr)
              (const $ A.style "padding: 3px; background-color: lightgrey"
              $ pure E.defTd)
    E.h2N $ text "exampleV1"
    E.pN $ text $ "Using td-attributes without events nor cell-states. Note "
        <> "that result should not change when clicking or trying to select. "
        <> "Function silentPlainTableConf can be used when only styling is "
        <> "applied. "
    res ∷ CompState t ActElem () ← mkTable tableConf matelms
    showRes res


--------------------------------------------------------------------------------

exampleV2 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV2 = do
    let cols = 6 ∷ Int
        rows = 4 ∷ Int
        tConf = set cellListenerBody (listenMyRow cols)
            $ set tableTableAttr (A.style "border-collapse: collapse"
                                  $ pure E.defTable)
            $ set cellDrawBody drawDivContentEx defTableConf
    E.h2N $ text "exampleV2"
    E.pN $ text $ "Activating the whole row. No stylings, using draw-function "
        <> "example that shows part of the internal state. "
    res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
    showRes res

--------------------------------------------------------------------------------

exampleV3 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV3 = do
    let cols = 6 ∷ Int
        rows = 4 ∷ Int
        tConf = defTableConf & set cellListenerBody (listenMyRow cols)
            & set cellDrawBody drawDivContentS
            & set tableTableAttr (A.style "border-collapse: collapse"
                                 $ pure E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
            & set (tableTdFuns . tdfTdAttr) (const $ A.style "padding: 5px"
                                             $ pure E.defTd)
    E.h2N $ text "exampleV3"
    E.pN $ text "Coloring the whole row based on clicks with other decorations. "
    res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
    showRes res


--------------------------------------------------------------------------------

exampleV4 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV4 = do
    let cols = 6 ∷ Int
        rows = 4 ∷ Int
        tConf = defTableConf & set cellListenerBody (listenMyCol rows)
            & set cellDrawBody drawDivContentS
            -- & set cellDrawBody drawDivAECntEx
            & set tableTableAttr (A.style "border-collapse: collapse"
                                  $ pure E.defTable)
            & set (tableTdFuns . tdfTdAttr) myTdAttrF
    E.h2N $ text "exampleV4"
    E.pN $ text $ "Coloring the whole column based on clicks with other "
        <> "decorations. (Try: modify to show state information.) "
    res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
      ( ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: blue" $ pure A.defGlobals))

--------------------------------------------------------------------------------

exampleV5 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV5 = do
    let cols = 3 ∷ Int
        rows = 6 ∷ Int
        -- We use our own version of drawDivContent. Note that the header input
        -- is Text-type so there is no need to show it, while cell-contents are
        -- int-pairs and there is a need for showing and packing.
        hns = set thfThAttr (const $ constDyn $ A.style "width: 100px" E.defTh) $
            set (thfADEs . adeDraw) drawDivContent  $
            set (thfADEs . adeListen) (listenMyCol rows) defThFuns
                -- set (thfADEs . adeDraw) (drawDivContent2 id)  $
            ∷ ThFuns t m ()
        fs = CommonADEfuns
            -- ownListen actMU
            listenHeadFootMe actMU
            -- drawDivAECntEx
            -- (drawDivContent2 (T.pack . show) )
            drawDivContentS
            elemEvF
        fns2 = defTdFuns & set tdfADEs fs & set tdfTdAttr myTdAttrF
        -- Note that the use of our own td-attribute function will possibly
        -- override the settings given in the table conf (see the not activable
        -- configuration). It is quite easy to give overlapping definitions to
        -- the same things (e.g. td-attributes of cells) and it can lead to
        -- confusing situations...
        matelms ∷ W.Vector (W.Vector (Int,Int))
        matelms = mkMat cols rows
        colVc ∷ W.Vector (Dynamic t E.Col)
        colVc = W.fromList $
            (\i → constDyn $ if even i
                   then A.style "background-color: lightgrey" E.defCol
                   else A.style "background-color: white" E.defCol
            ) <$> [1..cols]
        txtVc ∷ W.Vector Text
        txtVc = W.fromList $ (\i → "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs ∷ Maybe (HeaderConf t m ())
        colDfs = Just $ HeaderConf hns colVc txtVc (pure E.defThead)
        capDfs = Just (CaptionConf "Table example with td-attrs" $
            A.style ("background-color: black; color: white; " <>
                   "font-weight: bold") $ pure E.defCaption)
        initRow0 = initializeRow cols (Just 0) astNotActivable
        -- tableConf = TableConf fns2 capDfs colDfs Nothing def tableEventsEnLe
        tableConf = set tableTdFuns fns2 $ set tableCaption capDfs
            $ set tableHeader colDfs
            --  set (tableTdFuns . tdfTdAttr) myTdAttrF -- hmm, see 2 lines above:
            -- since we set fns2 later, it will override the earlier one.
            --  set tableActivityConf (constDyn $ Just [astNotActivable])
            $ set tableActivityConf (constDyn initRow0)
            defTableConf
    E.h2N $ text "exampleV5"
    E.pN $ text $ "Using td-attributes with the events and cell-states. The first "
        <> "row is not activable. Header-cell events. (See the code and play "
        <> "with the example mod's for listener and drawing.) "
    res ∷ CompState t ActElem () ← mkTable tableConf matelms
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
      ( ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals)
      & set activeStateNotActivableGl
        (A.style "color: white; background-color: darkgray"
        $ pure A.defGlobals))
    astNotActivable = defActiveStateTbl
        & set activeStateActivable (constDyn False)
        & set activeStateNotActivableGl (A.style "background-color: blue"
                                        $ pure A.defGlobals)
        -- The following won't take any effect as for cells that are not
        -- activable, only the notActivableGl attributes will be used.
        --  set activeStateActiveGl (style "color: green" A.defGlobals)
        --  set activeStateNotActiveGl (style "color: red" A.defGlobals)

    -- An example to play with:
    {-
    ownListen ae2 = actstate ae & set activeStateListen (constDyn ag)
      where
                ae = view activeStateElemId ae2
        isFstRow (ActERC (x,y)) = x == 0
        isFstRow _ = False
        ag = ActiveGroup $ Set.fromList $ ae: myHF ae
        actstate txt = defActiveState & set activeStateElemId txt
             set activeStateActivable (
                if isFstRow ae
                    then constDyn False
                    else constDyn True)
        myHF ∷ ActElem → [ActElem]
        myHF (ActERC (_,j)) = [ActEcolh j, ActEcols j]
        myHF a              = [a]
    -}

    -- An example to play with:
    {-
    drawDivContent2 fat elm actS = do
        let dA = view activeStateActive actS
            dNA = not <$> view activeStateActivable actS
            dAGl = view activeStateActiveGl actS
            dNAGl = view activeStateNotActiveGl actS
            dNAvGl = view activeStateNotActivableGl actS
            dUse = (\ba bna acl nacl navcl →
                    let (st,cl)
                          | bna = (A.style "background-color:darkgray"
                                  , navcl)
                          | ba  = (A.style "color: red"
                                  , acl)
                          | otherwise = (A.style "color: black", nacl)
                    in A.style "text-align: center"
                        <> st $ A.attrSetGlobals cl E.defDiv
                   ) <$> dA <*> dNA <*> dAGl <*> dNAGl <*> dNAvGl
        (e,_) ← E.divD' dUse $ text $ fat elm
        pure e
        -}

--------------------------------------------------------------------------------

exampleV6 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV6 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            elemEvF    -- how to construct the events cell is tracking/using
        fns = defTdFuns { _tdfADEs = fs } ∷ TdFuns t m (Int,Int) ()
        elemMat ∷ W.Vector (W.Vector (Int,Int))
        elemMat = mkMat 3 8 -- if you change 3, change txtVec's below, too.
        colVec ∷ W.Vector (Dynamic t E.Col)
        colVec = W.fromList $ constDyn <$>
            [ A.style "background-color: lightgrey" E.defCol
            , A.style "background-color: darkgrey"  E.defCol
            , A.style "background-color: lightgrey" E.defCol ]
        txtVec ∷ W.Vector Text
        txtVec = W.fromList ["col h1", " snd h", "colh3"]
        txtSumVec ∷ W.Vector Text
        txtSumVec = W.fromList ["sum 1", "sum 2", "sum 3"]
        ades = CommonADEfuns listenMe actMU drawDivAECntEx elemEvF
            ∷ CommonADEfuns t m Text ActElem ()
        colDfs ∷ Maybe (HeaderConf t m ())
        colDfs = Just $ HeaderConf
            (defThFuns {_thfADEs = ades}) colVec txtVec (pure E.defThead)
        sumDfs ∷ Maybe (FootConf t m ())
        sumDfs = Just $ FootConf
            (defTfootFuns {_tfootADEs = ades}) txtSumVec (pure E.defTfoot)
        capDfs = Just (CaptionConf "A table example" $
            A.style ("background-color: black; color: white; " <>
                   "letter-spacing: 2px; font-weight: bold")
            $ pure E.defCaption)
        tableConf = set tableTdFuns fns $ set tableCaption capDfs
            $ set tableHeader colDfs $ set tableFooter sumDfs defTableConf
    E.h2N $ text "exampleV6"
    E.pN $ text $ "Colgroup usage " <>
        "and straigthforward decorations. Yet another way to listen events. "
    res ∷ CompState t ActElem () ← mkTable tableConf elemMat
    showRes res

--------------------------------------------------------------------------------

exampleV7 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV7 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMU -- we want state switching cells
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        fns = defTdFuns & set tdfADEs fs & set tdfTdAttr myTdAttrF
            ∷ TdFuns t m (Char,Char) ()
        elemMat ∷ W.Vector (W.Vector (Char,Char))
        elemMat = mkChrMat 3 3
        tableConf = set tableTdFuns fns defTableConf
    E.h2N $ text "exampleV7"
    E.pN $ text "State switching (click on different cells)."
    res ∷ CompState t ActElem () ← mkTable tableConf elemMat
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
      (ast & set activeStateActiveGl (A.style "border: 3px solid purple"
                                     $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "border: 3px solid purple"
                                   $ pure A.defGlobals))


--------------------------------------------------------------------------------


-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV8 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV8 = do
    let fs = CommonADEfuns
            listenMe
            actAreaMDUsel
            drawDivActElemEx -- we draw only the "cell meta info" into the div's
            elemEvF
        fns = defTdFuns & set tdfADEs fs
                & set tdfTdAttr myTdAttrF ∷ TdFuns t m (Int,Int) ()
        tableConf = set tableTdFuns fns defTableConf
    E.h2N $ text "exampleV8"
    E.pN $ text "State selection on a user selected area (single area)."
    res ∷ CompState t ActElem () ← mkTable tableConf $ mkMat 4 4
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
          (ast & set activeStateActiveGl (A.style "border: 3px solid cyan"
                                         $ pure A.defGlobals)
          & set activeStateNotActiveGl (A.style "border: 3px solid cyan"
                                       $ pure A.defGlobals))

--------------------------------------------------------------------------------

-- Switch the whole area of cells bounded by _dUpLDownR (upper left and
-- lower right corner or the mouse pressing).
exampleV9 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV9 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMDUsel
            drawDivActElemEx
            elemEvF
        fns = defTdFuns { _tdfADEs = fs } & set tdfTdAttr myTdAttrF
            ∷ TdFuns t m (Int,Int) ()
        tableConf = set tableTdFuns fns defTableConf
    E.h2N $ text "exampleV9"
    E.pN $ text "State swithing on a user selected area (multiple, overlapping)"
    res ∷ CompState t ActElem () ← mkTable tableConf $ mkMat 4 4
    -- The following takes couple of seconds to render:
    -- res ∷ CompState t ActElem ← mkTable tableConf $ mkMat 30 200
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
        (ast & set activeStateActiveGl (A.style "border: 3px solid cyan"
                                       $ pure A.defGlobals)
        & set activeStateNotActiveGl (A.style "border: 3px solid cyan"
                                     $ pure A.defGlobals))

--------------------------------------------------------------------------------

-- This example corresponds to the examplesPrim3 (almost).
exampleV10 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV10 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and just mouse up -events
            drawDivActElemEx   -- how to draw/make each cell
            elemEvF   -- how to construct the events cell is tracking/using
        fns = TdFuns fs (exTdComb vlen) myTdAttrF
                (const $ const $ pure E.defTr) (pure E.defTbody)
        elemr1, elemr0 ∷ W.Vector (Int,Int)
        elemr0 = W.fromList [(0,0), (0,1), (0,2), (0,3), (0,4)]
        elemr1 = W.fromList [(1,0), (1,1), (1,2), (1,3), (1,4)]
        elemMat ∷ W.Vector (W.Vector (Int,Int))
        elemMat = W.fromList [elemr0, elemr1]
        vlen = W.length elemr0
        -- tableConf = TableConf fns Nothing Nothing Nothing def tableEventsEnLe
        tableConf = defTableConf & set tableTdFuns fns
                    & set tableTableAttr (A.style "border: 3px solid purple"
                                          $ pure E.defTable)
    E.h2N $ text "exampleV10"
    E.pN $ text $ "Click and enter events, custom elements inside td. "
        <> "Rightmost cell is listening the cells on the left but doesn't deliver "
        <> "events. This example corresponds to the examplesPrim3"
    -- E.pN $ text $ "Note: decorating a listening cell by the state of the cell "
        -- <> "seem's to be a bit difficult atm (that is, a problem to be solved "
        -- <> "somehow). "
    res ∷ CompState t ActElem () ← mkTable tableConf elemMat
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
        (ast & set activeStateActiveGl (A.style "border: 3px solid red"
                                       $ pure A.defGlobals)
         & set activeStateNotActiveGl (A.style "border: 3px solid purple"
                                      $ pure A.defGlobals))
    -- See 'adeCombFun' for the signature. This has one extra param to give
    -- the length of a vector.
    exTdComb ∷ (Reflex t, MonadHold t m, TriggerEvent t m
               , PostBuild t m, DomBuilder t m,  MonadJSM m
               , DomBuilderSpace m ~ GhcjsDomSpace)
      ⇒ Int → CommonADEfuns t m a ActElem ()
      → Dynamic t (Maybe [ActiveState t ActElem ()])
      → ActiveState t ActElem ()
      → Dynamic t a → Event t (CompEvent t ActElem ())
      → CompState t ActElem () → m (CompEvent t ActElem ())
    exTdComb vl adeFuns ae me ipair eTB tblSt = do
        let aeMe = view activeStateElemId me
            ActERC (x,y) = aeMe -- hmm, a bit dangerous ...
        if y < (vl-1)
           then adeCombFun adeFuns ae me ipair eTB tblSt
           else do
               -- Here we make an extra td that listens to the others.
               -- (Note: we could use vl in the following.)
               let aercs = ActERC <$> [(x,0), (x,1), (x,2), (x,3)]
                   adeF = set adeListen (listenListNotMe aercs) adeFuns
               adeCombLstner adeF ae me ipair eTB tblSt
               -- The following show's that tdD-attributes can be changed 
               -- by listening other cell's. But this also delivers events
               -- that are originated from the rightmost cell's (we would
               -- like them to be just listening in this example).
               -- adeCombFun adeF ae me ipair eTB tblSt



--------------------------------------------------------------------------------

exampleV11 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV11 = do
    let cols = 4 ∷ Int
        rows = 4 ∷ Int
        hns = set (thfADEs . adeListen) (listenMyCol rows)
            $ set (thfADEs . adeDraw) drawDivAECntEx defThFuns
            ∷ ThFuns t m ()
        ades = CommonADEfuns (listenMyCol rows) actMU drawDivAECntEx elemEvF
        foots = defTfootFuns {_tfootADEs = ades}
        fs = CommonADEfuns
            listenHeadFootMe -- each cell listens head, foot, and itself
            actMU            -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            elemEvF          -- how to construct the events cell is tracking/using
        fns = defTdFuns { _tdfADEs = fs } ∷ TdFuns t m (Int,Int) ()
        elemMat ∷ W.Vector (W.Vector (Int,Int))
        elemMat = mkMat cols rows
        colVec ∷ W.Vector (Dynamic t E.Col)
        colVec = W.fromList $
            (\i → constDyn $ if even i
                   then A.style "background-color: lightgrey" E.defCol
                   else A.style "background-color: darkgrey" E.defCol
            ) <$> [1..cols]
        txtVec ∷ W.Vector Text
        txtVec = W.fromList $ (\i → "Col H " <> (T.pack . show ) i) <$> [1..cols]
        colDfs ∷ Maybe (HeaderConf t m ())
        colDfs = Just $ HeaderConf hns colVec txtVec (pure E.defThead)
        txtSumVec ∷ W.Vector Text
        txtSumVec = W.fromList $ (\i → "Sum " <> (T.pack . show ) i) <$> [1..cols]
        sumDfs ∷ Maybe (FootConf t m ())
        sumDfs = Just $ FootConf foots txtSumVec (pure E.defTfoot)
        -- tableConf = TableConf fns Nothing colDfs sumDfs def tableEventsEnLe
        tableConf = set tableTdFuns fns $ set tableHeader colDfs
            $ set tableFooter sumDfs defTableConf
    E.h2N $ text "exampleV11"
    E.pN $ text "Header events, clicking on header or footer selects a column. "
    res ∷ CompState t ActElem () ← mkTable tableConf elemMat
    showRes res

--------------------------------------------------------------------------------

exampleV12 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV12 = do
    let cols = 3 ∷ Int
        rows = 4 ∷ Int
        rNum = Just 1 ∷ Maybe Int
    rec
        let
            -- dAstInit = constDyn $ Just
            --             [ def & set activeStateElemId (ActErow 0)
            dAstInit = (\mi → Just
                        [ defActiveStateTbl & set activeStateElemId
                            (case mi of
                                Just i  → ActErow i
                                Nothing → ActEnone)
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
            tConf = defTableConf
                & set cellListenerBody (listenMyRow cols)
                & set cellDrawBody drawDivContentS
                -- & set cellDrawBody drawDivAECntEx
                & set tableActivityConf dAstInit
                & set tableTableAttr (A.style "border-collapse: collapse"
                                     $ pure E.defTable)
                & set (tableTdFuns . tdfTrAttr) trAttrfun
                -- TODO TODO remove trAttrFun
                -- Use *either* the following or the one above at dAstInit.
                -- & set (tableTdFuns . tdfTdAttr)
                --     (\ast → defTdAttrF (ast
                --         & set activeStateActiveGl
                --             (style "background-color:grey" def)
                --         & set activeStateNotActiveGl
                --             (style "background-color: lightgrey" def)
                --                        )
                --     )
        -- set activeStateNotActiveGl (style "border: 3px solid purple" def))
        E.h2N $ text "exampleV12"
        E.pN $ text $ "Coloring the whole row based on clicks with other "
            <> "decorations. "
            <> "This is otherwise the same as example 3, but initializes a row. "
        E.pN $ text $ "Note: we have to maintain state outside 'mkTable'  "
            <> "in order to be able to initialize row (because otherwise "
            <> "we would be initializing the same row all the time). "
        res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
        let dUpAst = view csDynURelease res
            dAE = _activeStateElemId <$> dUpAst
            dRow2 = rowNum <$> dAE
            eRow = updated dRow2
        dRow ← holdDyn rNum eRow
    showRes res
      -- where
        -- astNotActivable = def
        --     & set activeStateElemId (ActErow 0)
        --     & set activeStateActivable (constDyn False)
        -- The following colors the clicked row always, independent of
        -- its state.
        -- trAttrfun ∷ Dynamic t (ActiveState t ActElem r) 
        --    → ActElem → Dynamic t ETr
        -- trAttrfun dAst ae =
        --     let dAE = _activeStateElemId <$> dAst
        --      in mkETr <$> dAE <*> pure ae
        -- mkETr ∷ ActElem → ActElem → ETr
        -- mkETr ae1 ae2 = if sameRowAE ae1 ae2
        --                    then style "background-color: grey" def
        --                    else style "background-color: lightgrey" def


--------------------------------------------------------------------------------


exampleV13 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV13 = do
    let cols = 3 ∷ Int
        rows = 5 ∷ Int
    let
        dAstInit = constDyn $ Just
                    [ defActiveStateTbl
                        & set activeStateActiveGl
                            (A.style "background-color:grey"
                            $ pure A.defGlobals)
                        & set activeStateNotActiveGl
                            (A.style "background-color: lightgrey"
                            $ pure A.defGlobals)
                    ]
        tConf = defTableConf
            & set cellListenerBody (listenMyRow cols)
            & set cellDrawBody drawDivContentS
            -- & set cellDrawBody drawDivAECntEx
            & set cellActivityBody actSwitchMU
            & set tableActivityConf dAstInit
            & set tableTableAttr (A.style "border-collapse: collapse"
                                 $ pure E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
    E.h2N $ text "exampleV13"
    E.pN $ text "Row selection with switching states. "
    res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
    -- let dUpAst = view csDynURelease res
    --     dAE = _activeStateElemId <$> dUpAst
    --     dRow2 = rowNum <$> dAE
    --     eRow = updated dRow2
    -- dRow ← holdDyn rNum eRow
    showRes res


trAttrfun ∷ forall t r. Reflex t
          ⇒ Dynamic t (ActiveState t ActElem r) → ActElem → Dynamic t E.Tr
    -- First param is the last row where an event occurred (with state info)
    -- and the second parameter tells, which row we are making (drawing,
    -- building).
trAttrfun dAst _ae = mkTr <$> join (_activeStateActive <$> dAst)
  where
    mkTr ∷ Bool → E.Tr
    mkTr b =
        if b
           then A.style "background-color: grey" E.defTr
           else A.style "background-color: lightgrey" E.defTr

--------------------------------------------------------------------------------


exampleV14 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV14 = do
    let cols = 10 ∷ Int
        rows = 3 ∷ Int
        cNum = Just 3 ∷ Maybe Int
    E.h2N $ text "exampleV14"
    E.pN $ text "Coloring the whole column with initial selected column. "
    rec
        let
            dAstInit = activateCol rows <$> dCol
            tConf = defTableConf
                & set cellListenerBody (listenMyCol rows)
                & set cellDrawBody drawDivContentS
                -- & set cellDrawBody drawDivAECntEx
                & set tableActivityConf dAstInit
                -- & set tableTableAttr (style "border-collapse: collapse" def)
                & set (tableTdFuns . tdfTdAttr) myTdAttrF
        res ∷ CompState t ActElem () ← mkTable tConf $ mkMat cols rows
        let dUpAst = view csDynURelease res
            eCol = updated $ (colNum . _activeStateElemId ) <$> dUpAst
        dCol ← holdDyn cNum eCol
    showRes res
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
      ( ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: blue" $ pure A.defGlobals))


--------------------------------------------------------------------------------


exampleV15 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                         , TriggerEvent t m, MonadJSM m, MonadFix m
                         , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                         ) ⇒ m ()
exampleV15 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMU -- we want state switching cells
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        fns = defTdFuns & set tdfADEs fs & set tdfTdAttr myTdAttrF
            ∷ TdFuns t m (Char,Char) ()
        elemMat ∷ W.Vector (W.Vector (Char,Char))
        elemMat = mkChrMat 3 3
        mAstSelAll ∷ Maybe [ActiveState t ActElem ()]
        -- mAstSelAll = activateCells 3 3
        mAstSelAll = initializeCells 3 3
            (defActiveStateTbl & set activeStateActive (constDyn True))
        mAstDeSelAll ∷ Maybe [ActiveState t ActElem ()]
        mAstDeSelAll = initializeCells 3 3
            (defActiveStateTbl & set activeStateActive (constDyn False))
        mAstInit ∷ Dynamic t Bool → (Maybe Int, Maybe Int)
                    → Maybe [ActiveState t ActElem ()]
        mAstInit dRCA mij = Just
            [ defActiveStateTbl & set activeStateElemId
                (case mij of
                    (Just i, Just j) → ActERC (i,j)
                    _                → ActEnone
                )
              & set activeStateActive dRCA -- (constDyn True)
            ]
    E.h2N $ text "exampleV15"
    E.pN $ text $ "State switching (click on different cells) with "
                <> "external events (buttons). "
    (evElSelAll,_) ← E.button' E.defButton $ text "Select All"
    (evElUnSel,_) ← E.button' E.defButton $ text "Un-select All"
    let evSelAll = domEvent Mouseup evElSelAll
        evUnSel = domEvent Mouseup evElUnSel
        evAnyBtn = leftmost [evSelAll, evUnSel]
        tc0 = set tableTdFuns fns defTableConf
    rec
        dAstI2 ← holdDyn mAstDeSelAll
            $ leftmost [ mAstSelAll <$  evSelAll
                       , mAstDeSelAll <$  evUnSel
                       ]
        let tc1,tc2 ∷ (TableConf t m (Char,Char) ())
            tc1 = set tableActivityConf (constDyn mAstSelAll) tc0
            tc2 = set tableActivityConf (constDyn mAstDeSelAll) tc0
            dRes1 ∷ m (CompState t ActElem ()) = mkTable tc1 elemMat
            dRes2 ∷ m (CompState t ActElem ()) = mkTable tc2 elemMat
        dRes ← widgetHold dRes2 $ leftmost
                [ dRes1 <$ evSelAll
                , dRes2 <$ evUnSel
                ]
    -- showRes2 dRes
    -- Which way is better: map a function with dyn here (as below) or change
    -- all other to feed a dynamic and use the showRes2-function?
    -- dyn $ fmap showRes dRes
    -- Or, should we use widgetHold? That would mean we should prepare an
    -- initial value (a computation) and watch for updated dRes.
    -- It seems that widgetHold is preferred over dyn, see docs.reflex-frp.org.
    widgetHold (showRes (defCompState ∷ CompState t ActElem ())) $
        ffor (updated dRes) $ \res → showRes res
    pure ()
  where
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
          (ast & set activeStateActiveGl (A.style "border: 3px solid red"
                                         $ pure A.defGlobals)
          & set activeStateNotActiveGl (A.style "border: 3px solid black"
                                       $ pure A.defGlobals))


-- | Otherwise same as 'showRes' but takes in a dynamic.
-- (This isn't used atm, as widgetHold -way to use showRes with an initial value
-- can be used.)
showRes2 ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                        , MonadHold t m, MonadFix m)
        ⇒ Dynamic t (CompState t ActElem r) → m ()
showRes2 dRes = do
    let eTxt = (_activeStateElemId . _ceMe) <$> (switchDyn $ _csCompEvent <$> dRes)
    eU2 ← switchHold never $ _ceMUp <$> (switchDyn $ _csCompEvent <$> dRes)
    -- eU2 ← switchPromptly never $ _ceMUp <$> (switchDyn $ _csCompEvent <$> dRes)
    let eU = fmap _activeStateElemId eU2
            -- coincidence $ _ceMUp <$> (switchDyn $ _csCompEvent <$> dRes)
        dUD = join $ _csDynUpLDownR <$> dRes
    dElm ← holdDyn ActEnone eTxt
    dUpElm ← holdDyn ActEnone eU
    E.pN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateElemId)
            <$> (join $ _csDynEnter <$> dRes)
    E.pN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> (join $ _csDynDOn <$> dRes)
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> (join $ _csDynUOn <$> dRes)
    E.pN $ do
        let dDact = join $ _csDynDPress <$> dRes
        let dUact = join $ _csDynURelease <$> dRes
        text "Pressed down on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dDact
        text "."
        text " Pressdown cell is active is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dDact)
        text " and activable is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dDact)
        text ". Released up on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dUact
        text "."
        text " Releaseup cell is active is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dUact)
        text " and activable is "
        E.bN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dUact)
        text ". (Releaseup should be same as "
        dynText $ (T.pack . show) <$> dUpElm
        text ", except when releasing btn outside table.)"
    E.pN $ do
        text "upper left / lower right dyn is on "
        dynText $ (T.pack.show .  (_activeStateElemId *** _activeStateElemId)) <$> dUD
    let dOut = join $ _csDynMOutsideBody <$> dRes
        dIn  = join $ _csDynMInsideBody <$> dRes
    dLInt ∷ Dynamic t Int ← count $ switchDyn $ mouseOutEvent <$> dRes
    dEInt ∷ Dynamic t Int ← count $ switchDyn $ mouseInEvent <$> dRes
    E.pN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
    E.pN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt


--------------------------------------------------------------------------------

-- Make a textInput to be used inside a matrix cell.
mkTI ∷ (Reflex t, DomBuilder t m, PostBuild t m
       , DomBuilderSpace m ~ GhcjsDomSpace)
     ⇒ Int → Int → m (Dynamic t Text)
mkTI r c = do
    let rcTxt = "row " <> (T.pack . show) r <> ", col " <> (T.pack . show) c
    ti ← textInput def { _textInputConfig_initialValue = rcTxt}
    dynText $ value ti
    pure $ value ti

-- Make a matrix
mkTIMat ∷ (Reflex t, DomBuilder t m, PostBuild t m
          , DomBuilderSpace m ~ GhcjsDomSpace)
        ⇒ Int → Int
        → W.Vector (W.Vector (Int,Int, (Int → Int → m (Dynamic t Text))))
mkTIMat i j = W.fromList $ fmap W.fromList lstMat
  where
    lstMat = [[ (b,a, mkTI) | a ← [0..(i-1)]] | b ← [0..(j-1)]]

exampleV16 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleV16 = do
    let fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- activate on click (mouse up)
            own16Draw -- make custom drawing
            elemEvF2
        fns = defTdFuns & set tdfADEs fs & set tdfTdAttr myTdAttrF
            ∷ TdFuns t m (Int,Int, (Int → Int → m (Dynamic t Text))) Text
        elemMat2 ∷ W.Vector (W.Vector (Int,Int, (Int → Int → m (Dynamic t Text))))
        -- elemMat2 = mkTIMat 3 3
        elemMat2 = mkTIMat 5 10
        -- elemMat2 = mkTIMat 10 200 -- 4-7 secs with warp
        -- ghcjs takes about 15 sec to start, responses slower than warp
        -- (there is some lag)
        -- elemMat2 = mkTIMat 20 200 -- 8-11 secs with warp
        -- elemMat2 = mkTIMat 30 200 -- ca 15 secs with warp
        -- ghcjs takes about 2 min to start, clearly slower that warp
        -- (considerable lag)
        -- elemMat2 = mkTIMat 30 300 -- ca 30 secs with warp / ghcjs failure
        tableConf = set tableTdFuns fns defTableConf
    E.h2N $ text "exampleV16"
    E.pN $ text $ "Using a user defined drawing function with extra "
        <> "elements and handling inside. Active cells are shown in purple "
        <> "if activated with mouse. (Keypresses not handled, yet.) "
    res ∷ CompState t ActElem Text ← mkTable tableConf elemMat2
    showRes res
    let evRV ∷ Event t (Dynamic t Text)
            = (_activeStateRetVal . _ceMe) <$> _csCompEvent res
        evId = (T.pack . show . _activeStateElemId . _ceMe) <$> _csCompEvent res
    dTxt ← switcherDyn (constDyn "(-)") evRV
    dId ← holdDyn "(-)" evId
    E.p ( A.style "color: red; background-color: lightgray" E.defP ) $ do
        text "Text was modified at "
        dynText dId
        text ". and now it is: "
        dynText dTxt

    pure ()
  where
    myTdAttrF ∷ ActiveState t ActElem r → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
          (ast & set activeStateActiveGl (A.style "border: 3px solid purple"
                                         $ pure A.defGlobals)
          & set activeStateNotActiveGl (A.style "border: 3px solid black"
                                       $ pure A.defGlobals))

-- | See 'exampleV16'. This borrows most of the code from 'drawDCCommon'.
own16Draw ∷ forall t m e . (Reflex t, DomBuilder t m, PostBuild t m
                          , DomBuilderSpace m ~ GhcjsDomSpace
                          , ActSelem e, MonadHold t m) -- , ActSretval r)
                -- ⇒ (Int,Int, (Int → Int → m (Dynamic t Text)))
                ⇒ Dynamic t (Int,Int, (Int → Int → m (Dynamic t Text)))
                → ActiveState t e Text
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t Text)
-- own16Draw (r,c, tiF) actS = do
own16Draw dRCtiF actS = do
    let dUse = elemAttrs E.defDiv actS
        dR = (\(r,_,_) → r) <$> dRCtiF
        dC = (\(_,c,_) → c) <$> dRCtiF
        -- dtiF = (\(_,_,x) → x) <$> dRCtiF
        dCt = T.pack . show <$> dC
        dRt = T.pack . show <$> dR
        dRt2 = ("row " <>) <$> dRt
        dCt2 = (", col " <>) <$> dCt
        drcTxt = zipDynWith (<>) dRt2 dCt2
        -- rcTxt = "row " <> (T.pack . show) r <> ", col " <> (T.pack . show) c
    (e,edResTxt ∷ Event t (Dynamic t Text)) ← E.divD' dUse $ do
    -- (e,_) ← E.divD' dUse $ do
        dynText drcTxt
        dyn $ ffor dRCtiF $ \(r,c,tiF) → tiF r c
        -- tiF r c
    dResTxt ← switcherDyn (constDyn "") edResTxt
    pure (e, dResTxt)
    -- pure (e, defRetval)

--------------------------------------------------------------------------------

examplePrim1 ∷ forall t m r. (MonadWidget t m) ⇒ m ()
examplePrim1 = do
  -- let actstate ae = defActiveStateTbl & set activeStateElemId ae
  let tblR = defCompState ∷ CompState t ActElem Int
      ae = constDyn Nothing
      tdntdf eTB x = E.tdN $ tdFunPre actMD ae
        (defActiveStateTbl {_activeStateElemId = x}) eTB tblR
  E.h2N $ text "examplePrim1"
  E.pN $ text "Building table with html-elems and using helpers to handle events."
  E.divN $ do
    res ← E.tableN $ mdo
      (htTb,eTB) ← E.tbodyN' $ do
        let dDU = _csDynUpLDownR tblR
        et1 ← E.trN $ do
          ed1 ← tdntdf eTB $ ActERC (0,0)
          ed2 ← tdntdf eTB $ ActERC (0,1)
          ed3 ← tdntdf eTB $ ActERC (0,2)
          -- pure $ compEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ compEvFiring [ed1,ed2,ed3]
        et2 ← E.trN $ do
          ed1 ← tdntdf eTB $ ActERC (1,0)
          ed2 ← tdntdf eTB $ ActERC (1,1)
          ed3 ← tdntdf eTB $ ActERC (1,2)
          -- pure $ compEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ compEvFiring [ed1,ed2,ed3]
        et3 ← E.trN $ do
          ed1 ← tdntdf eTB $ ActERC (2,0)
          ed2 ← tdntdf eTB $ ActERC (2,1)
          ed3 ← tdntdf eTB $ ActERC (2,2)
          -- pure $ compEvFiring $ W.fromList [ed1,ed2,ed3]
          pure $ compEvFiring [ed1,ed2,ed3]
        pure $ leftmost [et1,et2,et3]
      tblR ← updateCompState defCompOtherEvs eTB
      pure tblR
    showRes res


--------------------------------------------------------------------------------

evAGS ∷ forall t m. MonadWidget t m
      ⇒ Event t ActElem
      → ActiveState t ActElem Int
      → m (ActiveState t ActElem Int)
-- evAGS = actUniqPrim
-- evAGS = actSwitchPrim
evAGS = actGroupsPrim

tdElm2 ∷
       forall t m. (MonadWidget t m)
    ⇒ (Event t ActElem → ActiveState t ActElem Int
      → m (ActiveState t ActElem Int))
    → Event t ActElem
    → ActElem
    → m (Event t ActElem)
tdElm2 fEvAGS eAe me = do
    --  let actstate txt = def & set activeStateElemId txt
    let actstate txt = defActiveStateTbl & \d → d {_activeStateElemId = txt}
    let ag = ActiveGroup $ Set.singleton me
        -- ac = actstate me & set activeStateListen (constDyn ag)
        ac = actstate me & \d → d { _activeStateListen = constDyn ag }
    actS ← fEvAGS eAe ac
    (e, _) ←
        E.divN' $ do
            text $ T.pack . show $ me
            -- dynText $ (T.pack . show) <$> view activeStateActive actS
            dynText $ (T.pack . show) <$> _activeStateActive actS
    pure $ me <$ domEvent Click e


-- Note that we don't set the events tracking going inside and outside the
-- table body area.
examplePrim2 ∷ forall t m. (MonadWidget t m) ⇒ m ()
examplePrim2 = do
    let actstate txt = defActiveStateTbl & \d → d {_activeStateElemId = txt}
    E.h2N $ text "examplePrim2"
    E.pN $ text $
        "Building table with html-elems and using helpers to handle " <>
        "events. We define the cell-drawing method here. "
    E.divN $ do
      eRes ← E.tableN $ mdo
        eB ← E.tbodyN $ do
          et1 ← E.trN $ do
            ed1 ← E.tdN $ tdElm2 evAGS eB $ ActERC (0,0)
            ed2 ← E.tdN $ tdElm2 evAGS eB $ ActERC (0,1)
            ed3 ← E.tdN $ tdElm2 evAGS eB $ ActERC (0,2)
            E.tdN $ do
              let me = ActERC (0,3)
                  ag = ActiveGroup $ Set.fromList $
                      ActERC <$> [(0,0), (0,1), (0,2)]
                  ac = actstate me & \d → d { _activeStateListen = constDyn ag }
                  --  ac = actstate me & set activeStateListen (constDyn ag)
              actS ← evAGS eB ac
              E.divN $ do
                text $ T.pack . show $ me
                -- dynText $ (T.pack . show) <$> view activeStateActive actS
                dynText $ (T.pack . show) <$> _activeStateActive actS
            pure $ leftmost [ed1,ed2,ed3]
          et2 ← E.trN $ do
            ed1 ← E.tdN $ tdElm2 evAGS eB $ ActERC (1,0)
            ed2 ← E.tdN $ tdElm2 evAGS eB $ ActERC (1,1)
            ed3 ← E.tdN $ tdElm2 evAGS eB $ ActERC (1,2)
            E.tdN $ do
              let me = ActERC (1,3)
                  rg = ActiveGroup $ Set.fromList $
                      ActERC <$> [(1,0), (1,1), (1,2)]
                  -- ac = actstate me & set activeStateListen (constDyn rg)
                  ac = actstate me & \d → d { _activeStateListen = constDyn rg }
              actS ← evAGS eB ac
              E.divN $ do
                text $ T.pack . show $ me
                -- dynText $ (T.pack . show) <$> view activeStateActive actS
                dynText $ (T.pack . show) <$> _activeStateActive actS
            pure $ leftmost [ed1,ed2,ed3]
          pure $ leftmost [et1,et2]
        pure eB
      -- dAG ← holdDyn (ActiveGroup $ Set.singleton "None") eRes
      dAElm ← holdDyn ActEnone eRes
      E.pN $ do
        text "last press is "
        -- dynText $ (T.pack . show) <$> dAG
        dynText $ (T.pack . show) <$> dAElm


--------------------------------------------------------------------------------


-- Note that we don't set the events tracking going inside and outside the
-- table body area.
examplePrim3 ∷ forall t m. (MonadWidget t m) ⇒ m ()
examplePrim3 = do
    -- let actstate txt = def & set activeStateElemId txt
    let actstate txt = defActiveStateTbl & \d → d {_activeStateElemId = txt}
        tblR = defCompState ∷ CompState t ActElem Int
        astDf = defActiveStateTbl ∷ ActiveState t ActElem Int
        r1 = (\e → set activeStateElemId e astDf) <$>
            (ActERC <$> [(0, 0), (0, 1), (0, 2)]) -- row 1
        r2 = (\e → set activeStateElemId e astDf) <$>
            (ActERC <$> [(1, 0), (1, 1), (1, 2)]) -- row 2
        r1' = ActERC <$> [(0, 0), (0, 1), (0, 2)] -- row 1
        r2' = ActERC <$> [(1, 0), (1, 1), (1, 2)] -- row 2
        -- tdFs = TdFuns listenMe actMU drawDivActElemEx adeEvent tdComb
        --         (const def) (const def) def def
        fs = CommonADEfuns
            listenMe  -- each cell listens head and itself
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            elemEvF    -- how to construct the events cell is tracking/using
        tdFs = defTdFuns { _tdfADEs = fs } -- ∷ TdFuns t m ()
        i = (Prelude.!!)
        ae = constDyn Nothing
        combF = adeCombFun (_tdfADEs tdFs)
    E.h2N $ text "examplePrim3"
    E.pN $ text $
        "Building table with html-elems and using helpers to handle " <>
        "events. This uses the data structure " <>
        "that collects the functions and attribute declarations used with the " <>
        "table This also adds the fourth cell to listen the first three ones. "
    E.divN $ do
        eRes ← E.tableN $ mdo
            (htTb, eB) ← E.tbodyD' (_tdfTbodyAttr tdFs) $ do
                let dDU = _csDynUpLDownR tblR
                -- et1 ← eTrD (_tdfTrAttr tdFs $ ActErow 0) $ do
                et1 ← E.trN $ do
                    ed1 ← E.tdD (_tdfTdAttr tdFs $ r1 `i` 0) $
                        combF ae (r1 `i` 0) (constDyn ()) eB tblR
                    ed2 ← E.tdD (_tdfTdAttr tdFs $ r1 `i` 1) $
                        combF ae (r1 `i` 1) (constDyn ()) eB tblR
                    ed3 ← E.tdD (_tdfTdAttr tdFs $ r1 `i` 2) $
                        combF ae (r1 `i` 2) (constDyn ()) eB tblR
                    E.tdN $ do
                        -- let me = ActERC (0, 3)
                        let me = defActiveStateTbl
                                & set activeStateElemId (ActERC (0, 3))
                            ac = listenListNotMe r1' me
                        actS ← (_adeActivate . _tdfADEs) tdFs ae eB tblR ac
                        (_adeDraw . _tdfADEs) tdFs (constDyn ()) actS
                    pure $ compEvFiring [ed1, ed2, ed3]
                -- et2 ← eTrD (_tdfTrAttr tdFs $ ActErow 1) $ do
                et2 ← E.trN $ do
                    ed1 ← E.tdD (_tdfTdAttr tdFs $ r2 `i` 0) $
                        combF ae (r2 `i` 0) (constDyn ()) eB tblR
                    ed2 ← E.tdD (_tdfTdAttr tdFs $ r2 `i` 1) $
                        combF ae (r2 `i` 1) (constDyn ()) eB tblR
                    ed3 ← E.tdD (_tdfTdAttr tdFs $ r2 `i` 2) $
                        combF ae (r2 `i` 2) (constDyn ()) eB tblR
                    E.tdN $ do
                        let me = defActiveStateTbl
                                & set activeStateElemId (ActERC (1, 3))
                            ac = listenListNotMe r2' me
                        actS ← (_adeActivate . _tdfADEs) tdFs ae eB tblR ac
                        (_adeDraw . _tdfADEs) tdFs (constDyn ()) actS
                    pure $ compEvFiring [ed1, ed2, ed3]
                pure $ leftmost [et1, et2]
            tblR ← updateCompState defCompOtherEvs eB
            pure tblR
        showRes eRes

--------------------------------------------------------------------------------

