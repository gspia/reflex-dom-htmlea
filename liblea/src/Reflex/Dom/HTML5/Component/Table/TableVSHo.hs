{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.HTML5.Component.Table.TableVSHo
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= Table

-}

module Reflex.Dom.HTML5.Component.Table.TableVSHo where

import           Data.Finite
-- import qualified Data.Singletons as S
-- import qualified Data.Singletons.Decide as S
import qualified Data.Singletons.TypeLits as S
import qualified Data.Vector.Sized as V

import Data.Default (def)
-- import Data.Foldable (fold)
-- import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup ((<>))
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
-- import Reflex.Dom.Core (DomBuilder, Element, EventResult, Dynamic,
--                        DomBuilderSpace, MonadWidget, elAttr',
--                        elDynAttr', el', text, domEvent, EventName (Click))

import Reflex
import Reflex.Dom.Core
-- import qualified GHCJS.DOM.HTMLElement     as DOM
-- import qualified GHCJS.DOM.Element         as DOM
-- import qualified GHCJS.DOM.EventM          as DOM
-- import qualified GHCJS.DOM.GlobalEventHandlers as DOM
-- -- import qualified GHCJS.DOM.MouseEvent      as DOM
-- import qualified GHCJS.DOM.Types           as DOM -- (uncheckedCastTo)
-- import Language.Javascript.JSaddle -- (JSM)


-- import Reflex.Dom.HTML5.Attrs.Globals
-- import Reflex.Dom.HTML5.Elements.Tabular
import Reflex.Dom.HTML5.Elements
import Reflex.Dom.HTML5.Component.Table.Common

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data TdConfHo t a = TdConfHo
  { _tdConfHoContent :: a
  , _tdConfHoActiveState :: ActiveState t
  , _tdConfHoTdAttrs :: Dynamic t ETd
  } --deriving (Eq)


data TdElemHo t a = TdElemHo
  { _tdElemHoContent :: a
  , _tdElemHoActiveState :: ActiveState t
  , _tdElemHoDTd :: Dynamic t ETd
  , _tdElemHoEMouseC :: Event t a
  } --deriving (Eq)

-- makeLenses ''TdConfHo
-- makeLenses ''TdElemHo

instance Show a => Show (TdConfHo t a)
  where
    show (TdConfHo a _ _) = show a
instance Show a => Show (TdElemHo t a)
  where
    show (TdElemHo a _ _ _) = show a

mkTdConf :: forall a t.  (Reflex t, Eq a, Show a)
         => a -> ActiveGroup -> ETd -> TdConfHo t a
mkTdConf c ag td = TdConfHo c agr (constDyn td)
  where
    agr = def { _activeStateListen = constDyn ag}
    --  ActiveConf never never (constDyn True) (constDyn ag)


-- This is just one example on how to make a table cell.
-- Change to accept a function that takes conf and gives m a
-- or td-attributes. TODO! TODO! TODO!
mkTdElem :: forall a t m. (DomBuilder t m, PostBuild t m, MonadHold t m,
  Show a, Eq a)
    => (TdConfHo t a -> ActiveState t -> m ()) -> TdConfHo t a -> m (TdElemHo t a)
mkTdElem f tdc@(TdConfHo co ac dTda) = do
  -- acts <- holdAC ac
  -- (e,_) <-eTdD' dTda $ f tdc acts
  (e,_) <-eTdD' dTda $ f tdc ac
              --text $ T.pack $ show co
        -- text "act:"
        -- dynText $ T.pack . show <$> dActive
  -- (e,_) <- ide
  let eClick = co <$ domEvent Click e
  pure $ TdElemHo co ac dTda eClick

--------------------------------------------------------------------------------

data TrConfHo t n a b = TrConfHo
  { _trConfHoTag :: a
  , _trConfHoActiveState :: ActiveState t
  , _trConfHoTrAttrs :: Dynamic t ETr
  , _trConfHoVec :: V.Vector n b -- content is here
  } --deriving (Eq)

data TrElemHo t n a b = TrElemHo
  { _trElemHoTag :: a
  , _trElemHoActiveState :: ActiveState t
  , _trElemHoDTr :: Dynamic t ETr
  , _trElemHoVec :: V.Vector n b -- content is here
  -- , _trElemHoVec :: Dynamic t (Vector n b) -- content is here
  --, _trElemHo_eMouseC :: Event t a
  } --deriving (Eq)

-- makeLenses ''TrConfHo
-- makeLenses ''TrElemHo

instance (Show a, Show b) => Show (TrConfHo t n a b)
  where
    show (TrConfHo a _ _ v) = show a ++ ": " ++ show v
instance (Show a, Show b) => Show (TrElemHo t n a b)
  where
    show (TrElemHo a _ _ v) = show a ++ ": " ++ show v

type TRowConfHo t n a b = TrConfHo t n a (TdConfHo t b)
type TRowElemHo t n a b = TrElemHo t n a (TdElemHo t b)

-- -> TrConfHo t n a (TdConfHo t b)
mkTrConf :: forall a b n t. (Reflex t, Show a, Eq a, Show b, Eq b, S.KnownNat n)
         => a -> ActiveGroup -> ETr -> V.Vector n (TdConfHo t b)
         -> TRowConfHo t n a b
mkTrConf trtag ag tr = TrConfHo trtag agr (constDyn tr)
  where
    agr = def { _activeStateListen = constDyn ag}
    -- agr = ActiveConf never never (constDyn True) (constDyn ag)


mkTrElem :: forall a b n t m.
  (DomBuilder t m, PostBuild t m, MonadHold t m,
  Show a, Eq a, Show b, Eq b, S.KnownNat n)
    => TRowConfHo t n a b -> m (TRowElemHo t n a b)
mkTrElem _trc@(TrConfHo tg ac dTra vec) = do
  -- acts <- holdAC ac
  elms <- eTrD dTra $
          -- text $ T.pack $ show tg
          -- show the vec content here
          V.forM vec
            (mkTdElem (\tdc actst -> do
              text $ T.pack $ show $ _tdConfHoContent tdc
              -- let dasa = view activeStateActive actst
              --     davle = view activeStateActivable actst
              --     dlstn = view activeStateListen actst
              --     dcls = view activeStateActiveCl actst
              let dasa = _activeStateActive actst
                  davle = _activeStateActivable actst
                  dlstn = _activeStateListen actst
                  dcls = _activeStateActiveCl actst
                  dtxt = ffor dasa (\bo ->
                    if bo
                       then "act on: "
                       else "act off: "
                    )
                  dtxt2 = ffor davle (\bo ->
                    if bo
                       then "avable on: "
                       else "avable off: "
                                     )
                  dtxt3 = fmap (T.pack . show) dlstn
                  dtxt4 = fmap (\c -> (": "::Text) <> (T.pack . show) c) dcls
              dynText dtxt
              dynText dtxt2
              dynText dtxt3
              dynText dtxt4
              -- (_tdConfHoActivateConf  )
                      ))
  -- Next we have to handle the events inside the element-vector.
  -- let evs = V.map (\elm -> tag ((current . getActivable) elm) (getEvent elm)) elms
      -- eRow = Z.zip (fmap (mkEvRow $ Z.zip elms evs) elms) numRow
      -- eForest = Z.zip (fmap (mkEvTree $ Z.zip menuLR eLRs) menuLR) nLR
      -- make event logic on evs: which are activated and which not.
  -- let eClick = co <$ domEvent Click e
  -- pure $ TrElemHo tg acts dTra elms
  pure $ TrElemHo tg ac dTra elms
    -- where
    --   getEvent :: TdElemHo t b -> Event t b
    --   getEvent = _tdElemHoEMouseC
    --   getActivable :: TdElemHo t b -> Dynamic t Bool
    --   getActivable = ActiveState_activable  . _tdElemHoActiveState

--------------------------------------------------------------------------------

data TBodyConfHo t n a b = TBodyConfHo
  { _tbodyConfHoTag :: a
  , _tbodyConfHoBodyAttrs :: Dynamic t ETbody
  , _tbodyConfHoBodyContents :: V.Vector n b
  } -- derivine (Eq,Show)

data TBodyElemHo t n a b = TBodyElemHo
  { _tbodyElemHoTag :: a
  , _tbodyElemHoBodyAttrs :: Dynamic t ETbody
  , _tbodyElemHoBodyContents :: V.Vector n b
  } -- deriving (Eq,Show)

-- makeLenses ''TBodyConfHo
-- makeLenses ''TBodyElemHo

type TBConfHo t n1 n2 a b c = TBodyConfHo t n1 a (TRowConfHo t n2 b c)
  -- = TBodyConfHo t n1 a (TrConfHo t n2 b (TdConfHo t c))
type TBElemHo t n1 n2 a b c = TBodyElemHo t n1 a (TRowElemHo t n2 b c)

mkTBodyConf :: forall t n1 n2 a b c.
  (Reflex t, Show a, Eq a, Show b, Eq b, Show c, Eq c
  , S.KnownNat n1, S.KnownNat n2)
  => a -> ETbody -> V.Vector n1 (TRowConfHo t n2 b c)
  -> TBConfHo t n1 n2 a b c
mkTBodyConf tg tba = TBodyConfHo tg (constDyn tba)

mkTBodyElem :: forall a b c n1 n2 t m.
  (DomBuilder t m, PostBuild t m, MonadHold t m,
  Show a, Eq a, Show b, Eq b, Show c, Eq c, S.KnownNat n1, S.KnownNat n2)
  => TBConfHo t n1 n2 a b c
  -> m (TBElemHo t n1 n2 a b c)
mkTBodyElem _tblc@(TBodyConfHo tg dtba vec) = do
  rows <- eTbodyD dtba $
    V.forM vec mkTrElem
  pure $ TBodyElemHo tg dtba rows


--------------------------------------------------------------------------------

data CaptionConf t = CaptionConf
  { _captionConfText :: Text
  , _captionConfCaptionAttrs :: Dynamic t ECaption
  }

data CaptionElem t = CaptionElem
  { _captionElemText :: Text
  , _captionElemCaptionAttrs :: Dynamic t ECaption
  }

-- makeLenses ''CaptionConf
-- makeLenses ''CaptionElem

-- mkCaptionElem :: MonadWidget t m => CaptionConf t -> m (CaptionElem t)
mkCaptionElem :: (DomBuilder t m, PostBuild t m)
              => CaptionConf t -> m (CaptionElem t)
mkCaptionElem _cc@(CaptionConf txt dCapAttrs) = do
  eCaptionD dCapAttrs $ text txt
  pure $ CaptionElem txt dCapAttrs


--------------------------------------------------------------------------------

data TfootConfHo t n a b = TfootConfHo
  { _tfootConfHoTag :: a
  , _tfootConfHoActiveState :: ActiveState t
  , _tfootConfHoTrAttrs :: Dynamic t ETfoot
  , _tfootConfHoVec :: V.Vector n b -- content is here
  }

data TfootElemHo t n a b = TfootElemHo
  { _tfootElemHoTag :: a
  , _tfootElemHoActiveState :: ActiveState t
  , _tfootElemHoDTfoot :: Dynamic t ETfoot
  , _tfootElemHoVec :: V.Vector n b -- content is here
  }

-- makeLenses ''TfootConfHo
-- makeLenses ''TfootElemHo

instance (Show a, Show b) => Show (TfootConfHo t n a b)
  where
    show (TfootConfHo a _ _ v) = show a ++ ": " ++ show v
instance (Show a, Show b) => Show (TfootElemHo t n a b)
  where
    show (TfootElemHo a _ _ v) = show a ++ ": " ++ show v

type TFConfHo t n a b = TfootConfHo t n a (TdConfHo t b)
type TFElemHo t n a b = TfootElemHo t n a (TdElemHo t b)

mkTfootConf :: forall a b n t. (Reflex t, Show a, Eq a, Show b, Eq b, S.KnownNat n)
         => a -> ActiveGroup -> ETfoot -> V.Vector n (TdConfHo t b)
         -> TFConfHo t n a b
mkTfootConf tfoottag ag tf = TfootConfHo tfoottag agr (constDyn tf)
  where
    agr = def { _activeStateListen = constDyn ag}


mkTfootElem :: forall a b n t m. (DomBuilder t m, PostBuild t m, MonadHold t m,
  Show a, Eq a, Show b, Eq b, S.KnownNat n)
  => TFConfHo t n a b -> m (TFElemHo t n a b)
mkTfootElem _tfootc@(TfootConfHo tg ac dTfoota vec) = do
  -- acts <- holdAC ac
  elms <-eTfootD dTfoota $
    eTrN $
      -- text $ T.pack $ show tg
      -- show the vec content here
      V.forM vec
        (mkTdElem (\tdc _ -> text $ T.pack $ show $ _tdConfHoContent tdc))
    -- Next we have to handle the events inside the element-vector.
    -- let eClick = co <$ domEvent Click e
  -- pure $ TfootElemHo tg acts dTfoota elms
  pure $ TfootElemHo tg ac dTfoota elms


--------------------------------------------------------------------------------

-- type TBConfHo t n1 n2 a b c = TBodyConfHo t n1 a (TRowConfHo t n2 b c)
-- type TBElemHo t n1 n2 a b c = TBodyElemHo t n1 a (TRowElemHo t n2 b c)
-- type TFConfHo t n a b = TfootConfHo t n a (TdConfHo t b)
-- type TFElemHo t n a b = TfootElemHo t n a (TdElemHo t b)

data TableConfHo t n1 n2 a b c d e = TableConfHo
  { _tableConfHoTag :: a
  , _tableConfHoCaption :: Maybe (CaptionConf t)
  -- , TableConf_colgroup :: a -- before head, can contain col-els.
  -- , TableConf_head :: a -- can contain tr, which can contain td-els.
  , _tableConfHoFoot :: Maybe (TFConfHo t n2 d e)
  , _tableConfHoBody :: TBodyConfHo t n1 b c
  , _tableConfHoTableAttrs :: Dynamic t ETable
  -- , TableConf_colconf :: a
  } -- deriving (Eq,Show)

data TableElemHo t n1 n2 a b c d e = TableElemHo
  { _tableElemHoTag :: a
  , _tableElemHoCaption :: Maybe (CaptionElem t)
  -- , TableElem_colgroup :: a -- before head, can contain col-els.
  -- , TableElem_head :: a -- can contain tr, which can contain td-els.
  , _tableElemHoFoot :: Maybe (TFElemHo t n2 d e)
  , _tableElemHoBody :: TBodyElemHo t n1 b c
  , _tableElemHoTableAttrs :: Dynamic t ETable
  -- , TableElem_colconf :: a
  } -- deriving (Eq,Show)

-- makeLenses ''TableConfHo
-- makeLenses ''TableElemHo

type TTConfHo t n1 n2 a b c d e f =
  TableConfHo t n1 n2 a b (TRowConfHo t n2 c d) d (TRowConfHo t n2 e f)
  -- TableConfHo t n1 n2 a b c d (TRowConfHo t n2 e f)
type TTElemHo t n1 n2 a b c d e f=
  TableElemHo t n1 n2 a b (TRowElemHo t n2 c d) d (TRowElemHo t n2 e f)
  -- TableElemHo t n1 n2 a b c d (TRowElemHo t n2 e f)


mkTableConf :: forall t n1 n2 a b c d e f.
  (Reflex t, Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d
    , Show e, Eq e, Show f, Eq f, S.KnownNat n1, S.KnownNat n2)
    => a -> Maybe (CaptionConf t) -> Maybe (TFConfHo t n2 e f)
    -> TBConfHo t n1 n2 b c d -> ETable
    -> TableConfHo t n1 n2 a b (TRowConfHo t n2 c d) e f
-- -> TTConfHo t n1 n2 a b c d e f
mkTableConf tg mcap mfoot body tblA =
  TableConfHo tg mcap mfoot body (constDyn tblA)



-- Is this used somewhere??
-- type TRowConfHo t n a b = TrConfHo t n a (TdConfHo t b)
updateTableConf :: forall t n1 n2 a b c d e f.
  (Reflex t, Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d
    , Show e, Eq e, Show f, Eq f, S.KnownNat n1, S.KnownNat n2)
  => TableConfHo t n1 n2 a b (TRowConfHo t n2 c d) e f
  -> Int -> Int -> (TdConfHo t d -> TdConfHo t d)
  -> TableConfHo t n1 n2 a b (TRowConfHo t n2 c d) e f
updateTableConf tc i j tdf =
    -- set (tableConfHoBody . tbodyConfHoBodyContents) irow tc
  -- tc {  _tableConfHoBody . _tbodyConfHoBodyContents = irow }
  tc {  _tableConfHoBody = tccb { _tbodyConfHoBodyContents = irow } }
  where
    tccb = _tableConfHoBody tc
    irow = vv V.// [(i, updRow vi)]
    vi :: TRowConfHo t n2 c d
    vi = V.index vv finiI
    vv :: V.Vector n1 (TRowConfHo t n2 c d)
    -- vv = view (tableConfHoBody . tbodyConfHoBodyContents) tc
    vv = (_tbodyConfHoBodyContents . _tableConfHoBody ) tc
    updRow :: TRowConfHo t n2 c d -> TRowConfHo t n2 c d
    -- updRow trch = set trConfHoVec ntrc trch
    updRow trch = trch {_trConfHoVec = ntrc}
      where
        trc = _trConfHoVec trch
        tde = tdelm trc finiJ
        ntrc = trc V.// [(j, tdf tde)]
        tdelm :: V.Vector n2 (TdConfHo t d) -> Finite n2 -> TdConfHo t d
        tdelm = V.index
    finiI = finite . fromIntegral $ i :: Finite n1
    finiJ = finite . fromIntegral $ j :: Finite n2


mkTableElem :: forall m t n1 n2 a b c d e f.
  (DomBuilder t m, PostBuild t m, MonadHold t m,
  Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d,
  Show e, Eq e, Show f, Eq f, S.KnownNat n1, S.KnownNat n2)
    => TableConfHo t n1 n2 a b (TRowConfHo t n2 c d) e f
    -> m (TableElemHo t n1 n2 a b (TRowElemHo t n2 c d) e f)
    -- => TTConfHo t n1 n2 a b c d e f
    -- -> m (TTElemHo t n1 n2 a b c d e f)
mkTableElem _tConf@(TableConfHo tg mcapc mfootc bodyC dTblA) =
  eTableD dTblA $ do
    -- elcap <- sequence $ fmap mkCaptionElem mcapc
    elcap <- traverse mkCaptionElem mcapc
    bodyE <- mkTBodyElem bodyC
    -- elfoot <- sequence $ fmap mkTfootElem mfootc
    elfoot <- traverse mkTfootElem mfootc
    let
        bodyMat :: V.Vector n1 (V.Vector n2 (TdElemHo t d))
        bodyMat = V.map _trElemHoVec $ _tbodyElemHoBodyContents bodyE
        ibodyM :: V.Vector n1 (Int, V.Vector n2 (Int, TdElemHo t d))
        ibodyM = V.indexed $ V.map V.indexed bodyMat
        --
        _eClickBools :: V.Vector n1 (V.Vector n2 (Event t Bool))
        _eClickBools = V.map (V.map processTdElems) bodyMat
        eIxTag :: Event t (Int,Int,d)
        eIxTag = V.foldr foldRows never ibodyM
        iIxs = fromIntegral . getFinite <$> (finites :: [Finite n1]) :: [Int]
        jIxs = fromIntegral . getFinite <$> (finites :: [Finite n2]) :: [Int]
        _ijIxs = [(i,j) | i <- iIxs, j <- jIxs]
        -- ijNC = filter ( \(i2,j2) /= ())
        _eIx :: Event t (Int,Int)
        _eIx = (\(i,j,_) -> (i,j)) <$> eIxTag
        _eTg :: Event t d
        _eTg = (\(_,_,d) -> d) <$> eIxTag
        -- eBodyC2 = ffilter (\ix -> mkEvIxs ijIxs ix) eIx
        -- eRow = Z.zip (fmap (mkEvRow $ Z.zip elms evs) elms) numRow
        -- make event logic on evs: which are activated and which not.
    pure $ TableElemHo tg elcap elfoot bodyE dTblA
  where
    -- updNewActive :: Event t d -> TdConfHo t d -> TdConfHo t d
    -- updNewActive ev tdc@(TdConfHo cnt ac tda) = TdConfHo cnt nac ntda
    --   where
    --     nac = set ActiveConf_setActivable
    --     ntda = undefined
    -- mkEvIxs :: [(Int,Int)] -> (Int,Int) ->
    processTdElems :: TdElemHo t d -> Event t Bool
    processTdElems elm = tag ((current . getActivable) elm) (getEvent elm)
    foldRows :: (Int,V.Vector n2 (Int,TdElemHo t d))
             -> Event t (Int,Int,d) -> Event t (Int,Int,d)
    foldRows row ev = leftmost [ev, foldTds row]
    foldTds :: (Int,V.Vector n2 (Int,TdElemHo t d)) -> Event t (Int,Int,d)
    foldTds (i,row) = V.foldr (foldTdF i) never row
    foldTdF :: Int -> (Int,TdElemHo t d)
            -> Event t (Int,Int,d) -> Event t (Int,Int,d)
    foldTdF i (j,elm) ev = leftmost [ev, (\e -> (i,j,e)) <$> getEvent elm]
    getEvent :: TdElemHo t d -> Event t d
    getEvent = _tdElemHoEMouseC
    getActivable :: TdElemHo t d -> Dynamic t Bool
    getActivable = _activeStateActivable  . _tdElemHoActiveState
    -- getTrVec :: TRowElemHo t n2 c d -> V.Vector n2 (TdElemHo t d)
    -- getTrVec = _trElemHoVec


