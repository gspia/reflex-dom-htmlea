{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-|
Module      : Reflex.Dom.HTML5.Component.Common.DrawFuns
Description : Helpers to build components.
Copyright   : (c) gspia 2018
License     : BSD3
Maintainer  : gspia

= Drawing functions

Use these or take one as starting point to build your own.
Function 'drawNothing' is defined in module "CompEvent".

The drawing is done in adeCombFun that is in "CompEvent"-module.
There are variants of that function, too. Those put in place the
listening, drawing and using of events together.

-}


module Reflex.Dom.HTML5.Component.Common.DrawFuns
    (
    -- * Drawing functions for components (table cells, tree nodes, etc)
    drawDivContent
    , drawDivContentS
    , drawDCCommon
    , drawDivWdt
    , drawDivContentEx
    , drawDivActElemEx
    , drawDivAECntEx
    , elemAttrs
    ) where

import           Control.Lens
import           Control.Monad                  (join)
import           Data.Semigroup                 ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

-- import           Reflex.Dom.HTML5.Component.Table.ActElem
-- import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Common.StateInfo
-- import           Reflex.Dom.HTML5.Component.Common.CompEvent
import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E

--------------------------------------------------------------------------------

-- | This forms Dynamic attributes for element @elm@.
elemAttrs ∷ (Reflex t, A.AttrHasGlobals elm)
          ⇒ elm → ActiveState t e r → Dynamic t elm
elemAttrs elm actS = dUse
  where
    dA = _activeStateActive actS
    dNA = not <$> _activeStateActivable actS
    dAGl = (`A.attrSetGlobals` elm) <$> _activeStateActiveGl actS
    dNAGl = (`A.attrSetGlobals` elm) <$> _activeStateNotActiveGl actS
    dNAvGl = (`A.attrSetGlobals` elm) <$> _activeStateNotActivableGl actS
    dUse =
        (\ba bna acl nacl navcl →
            if bna
               then navcl
               else if ba
                   then acl
                   else nacl
        )
        <$> dA <*> dNA <*> dAGl <*> dNAGl <*> dNAvGl


--------------------------------------------------------------------------------


-- | Draw content on a div that allows events to go. The provided class-attributes
-- for not activable, active and not active elements (cells/nodes) are used.
-- Note that this is only used on how to show things.
drawDivContent ∷ forall t m e r. (Reflex t, DomBuilder t m, PostBuild t m
                             , DomBuilderSpace m ~ GhcjsDomSpace
                             , ActSelem e, ActSretval r)
                ⇒ Dynamic t Text
                -- ^ The element that we are drawing (the dynamic text of it).
                → ActiveState t e r
                -- ^ The activity information of the element @e@ together with 
                -- the value to be returned (of type @Dynamic t r@).
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
                -- ^ Reflex element with dynamic value (of type @r@).
                -- (The dynamic value is not used in this example, 
                -- it is default value.)
drawDivContent = drawDCCommon id


-- | Similar to 'drawDivContent' but the showed element is showable.
drawDivContentS ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                               , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                               , ActSelem e, ActSretval r)
                ⇒ Dynamic t a
                → ActiveState t e r
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDivContentS = drawDCCommon (T.pack . show)



-- | See 'drawDivContent'.
drawDCCommon ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                             -- , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                             , DomBuilderSpace m ~ GhcjsDomSpace
                             , ActSelem e, ActSretval r)
                ⇒ (a → Text)
                → Dynamic t a
                → ActiveState t e r
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDCCommon txtFun dElm actS = do
    let dUse = elemAttrs E.defDiv actS
    -- let dA = _activeStateActive actS
        -- dNA = not <$> _activeStateActivable actS
        -- dAGl = (`A.attrSetGlobals` E.defDiv) <$> _activeStateActiveGl actS
        -- dNAGl = (`A.attrSetGlobals` E.defDiv) <$> _activeStateNotActiveGl actS
        -- dNAvGl = (`A.attrSetGlobals` E.defDiv) <$> _activeStateNotActivableGl actS
        -- dUse =
            -- (\ba bna acl nacl navcl →
                -- if bna
                   -- then navcl
                   -- else if ba
                       -- then acl
                       -- else nacl
            -- )
            -- <$> dA <*> dNA <*> dAGl <*> dNAGl <*> dNAvGl
    (eRes,_) ← E.divD' dUse $
        -- text $ txtFun elm
        dynText $ txtFun <$> dElm
    pure (eRes, defRetval)


-- | See 'drawDivContent' and 'drawDCCommon'.
drawDivWdt ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                             -- , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                             , DomBuilderSpace m ~ GhcjsDomSpace
                             , ActSelem e, ActSretval r)
                ⇒ (a → Dynamic t Text)
                → Dynamic t a
                → ActiveState t e r
                → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDivWdt txtFun dElm actS = do
    let dUse = elemAttrs E.defDiv actS
    (eRes,_) ← E.divD' dUse $
        dynText $ join $ txtFun <$> dElm
    pure (eRes, defRetval)


-- | An example of a function that draws a some content for, e.g. li- or td-
-- elements.
--
-- The drawing function is
-- given the "cell name" (ActElem), the "cell contents" (a), and "cell state"
-- (ActiveState) that can be used to decide, how to draw.
-- This function shows the "cell contents" only together with activity
-- information. And similarly for nodes in the trees.
drawDivContentEx ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                                , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                                , ActSelem e, ActSretval r)
               ⇒ Dynamic t a
               → ActiveState t e r
               → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDivContentEx dElm actS = do
    (e,_) ← E.divN' $ do
        -- text $ (T.pack . show $ elm) <> " : "
        dynText $ T.pack . show <$> dElm
        text " : "
        dynText $ (T.pack . show) <$> _activeStateActive actS
    pure (e, defRetval)

-- | An example of a function that draws a li- or td-content.
-- This function shows the ActElem only together with activity information.
drawDivActElemEx ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                                , DomBuilderSpace m ~ GhcjsDomSpace
                                , ActSelem e, ActSretval r, Show e)
               ⇒ Dynamic t a
               → ActiveState t e r
               → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDivActElemEx _dA actS = do
    (e,_) ← E.divN' $ do
        text $ (T.pack . show $ view activeStateElemId actS) <> " : "
        dynText $ (T.pack . show) <$> _activeStateActive actS
    pure (e, defRetval)

-- | An example of a function that draws a td-content.
-- This function shows both the ActElem and "cell contents".
drawDivAECntEx ∷ forall t m a e r. (Reflex t, DomBuilder t m, PostBuild t m
                              , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                              , ActSelem e, ActSretval r, Show e)
             ⇒ Dynamic t a
             → ActiveState t e r
             → m (Element EventResult (DomBuilderSpace m) t, Dynamic t r)
drawDivAECntEx dElm actS = do
    (e,_) ← E.divN' $ do
        text $ psh (view activeStateElemId actS) <> " : "
        let dTxt = psh <$> dElm
        -- text $ psh (view activeStateElemId actS) <> " : " <> psh elm <> " : "
        dynText dTxt
        text $ " : "
        dynText $ psh <$> _activeStateActive actS
        text " : avable "
        dynText $ psh <$> view activeStateActivable actS
    pure (e, defRetval)
      where
        psh ∷ Show b ⇒ b → T.Text
        psh = T.pack . show

