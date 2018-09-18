{-# LANGUAGE OverloadedStrings, UnicodeSyntax, RecursiveDo   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainW where

import           Reflex.Dom (text, display, count, elAttr, (=:), (&), domEvent
                            , MonadWidget, foldDyn, Dynamic, constDyn
                            , EventName (..), mergeWith, holdDyn)
import           Reflex.Dom.Core (mainWidget)
import           Data.Monoid     ((<>))
import           Language.Javascript.JSaddle (JSM)

import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E

--------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidget $ do
  E.h1N $ text "Welcome to reflex-dom-htmlea"
  intro
  caveats
  examples

intro ∷ MonadWidget t m ⇒ m ()
intro = do
  E.divN $ do
    E.pN $ do
      text "Why? Reflex-dom-htmlea -lib "
      E.bN $ text " helps "
      text $ " to avoid some of the run-time "
       <> "errors that are very easy to introduce with typos in attribute or "
       <> "element names. Simply said: I hate runtime debugging! Especially "
       <> "if the compiler is able to help."
    E.pN $
      text $ "In addition to that, the aim is to provide smart "
       <> "constructors "
       <> "for some of the common usage-patterns in order to help avoid combining "
       <> "the elements in non-conforming way. "
       <> "At the moment there ain't much support for that. "
    E.pN $ do
      text "There is some support to"
      E.ulN $ do
        E.liN $ text "let elements have only those attributes the specification says"
        E.liN $ text "use a set of pre-defined element and attribute names"
    E.pN $ do
      text "This example app uses the "
      E.a ( A.href ( A.URL "https://gist.github.com/3noch/ee335c94b92ea01b7fee9e6291e833be") E.defA)
        $ text  "turbo-charged ghcid settings (auto-reloading)"
      text " to enable fluent work flow."
    E.pN $ do
      text $ ("Note that the structure of this lib is simple and "
        <> " it is very easy to use. ")

caveats ∷ MonadWidget t m ⇒ m ()
caveats = do
  E.h2N $ text "Caveats"
  E.divN $ do
    E.pN $ text "See README.md"
    E.pN $ text $ "This lib is probably going to go through some "
            <> "renaming, reshaping and other things as this lib "
            <> "is still in its infancy. "
            <> "Author is open to suggestions, PRs etc. (with usual disclaimers). "
    E.pN $ text $ "And bugs are more than likely. There ain't any "
            <> "test cases (just example programs) so some of the "
            <> "element or attribute names may have typos atm. "
            <> "Anyhow, given with little time, they are probably going to get "
            <> "removed. "

examples ∷ MonadWidget t m ⇒ m ()
examples = do
  E.h2N $ text "Other examples (take a look at the code)"
  E.divN $ do
    E.pN $ text "Note that you can "
    E.ulN $ do
      elAttr "li" (A.attrMap $ A.id_ "li1" $ A.defGlobals)
        $ text "combine"
      elAttr "li" (A.attrMap $ A.id_ "li2" $ A.defGlobals)
        $ text "the defined elements"
      elAttr "li" (A.attrMap li3Attrs) $ text "and attributes with"
      -- Combining the Maps and provided combinators is possible.
      elAttr "li" ("badAttr" =: "badValue" <> A.attrMap li3Attrs)
        $ text "el-family of functions."
    E.p (A.title "hmm" $ A.className "pclass" E.defP) $ text "A paragraph example."
    -- E.p ( A.href (A.URL "hmm") $ A.className "pclass" $ E.defP) 
        --  text "Paragraph3"
    -- No instance of p-tag has href-attr. So it cannot be used.
    dialogEx
    linkEx
    embedEx
    listEx
  where
    li3Attrs ∷ A.Globals
    li3Attrs = A.className "myclass" $ A.id_ "li3" A.defGlobals

dialogEx ∷ forall t m. MonadWidget t m ⇒ m ()
dialogEx = do
    E.h2N $ text "Dialog example"
    E.div (A.setClasses [A.ClassName "hmm"] $ A.id_ "myId" $ E.defDiv) $ do
        -- Note: Interactive elements have functions that end with C and
        -- CD, those return Click-events.
        evBtn ← E.buttonC E.defButton $ text "Open/Close dialog"
        -- We could use other events and the associated information as well:
        -- (evResBtn,_) ← E.button' E.defButton $ text "Open/Close dialog"
        -- let evBtn = domEvent Mouseup evResBtn
        rec
            dOpen ← foldDyn ($) False
                $ mergeWith (.) [ (\b → not b) <$ evBtn
                                , (\_ → False) <$ evCls
                                ]
            let dAttrs ∷ Dynamic t E.Dialog
                  = (\b →
                    if b
                       then E.defDialog
                        & A.title "Open title" & A.setClasses [A.ClassName "hmm"]
                        & A.open
                       else E.defDialog
                        & A.title "My closed title"
                    ) <$> dOpen
            evCls ← E.dialogD dAttrs $ do
                E.h2N $ text "Dialog-box"
                text "A user defined dialog (take a look of the attributes)"
                E.buttonC E.defButton $ text "Close dialog"
        pure ()


linkEx ∷ MonadWidget t m ⇒ m ()
linkEx = do
  E.h2N $ text "Link example"
  elAttr "p" (A.attrMap p1Attrs) $ do
    text ("Paragraph-example with badly formed attributes. "
      <> "(Look at the attributes of this paragraph.) ")
    ev ← E.aC ( A.href (A.URL "#") $ A.className "myWarning" $ E.defA
              ) $ text "Here is a link with counter."
    numbs ← foldDyn (+) (0 ∷ Int)  (1 <$ ev)
    text " Clicked the previous link "
    display numbs
    text " times and the following "
    cnt ∷ Dynamic t Int
      ← count =<< (E.aC (A.href (A.URL "#") E.defA) $ text "link")
    display cnt
    text " times."
    E.br_
  where
    -- Note that by using AnyAttr's, it is still easy to write non-valid
    -- attributes (p-element doesn't have href-attributes).
    p1Attrs ∷ A.AnyAttr
    p1Attrs = A.href (A.URL "http://localhost") $ A.id_ "p1" $ A.defAnyAttr
    -- By using the element-related attribute-structures, that wouldn't be 
    -- possible. E.g. try the   E.p .. text "Paragraph3" above at "examples".

embedEx ∷ MonadWidget t m ⇒ m ()
embedEx = do
  E.h2N $ text "Embdedded content examples"
  E.divN $ do
    text "Embedded content div "
    fcnt ∷ Dynamic t Int
      ← count =<<
        E.embedC ( A.width 200
                 $ A.src (A.URL "./figs/eveninglandscape.jpg") E.defEmbed)
    text "Clicked the previous fig "
    display fcnt
    text " times."
    E.br_
    _ev ← E.iFrameC
      ( A.sbAllowSameOrigin
      $ A.srcDoc "<p>Some HTML.</p><br><div>And more of it.</div>" E.defIFrame
      ) $ text "Alternative text if iframe not shown."
    E.br_
  E.divN $ do
    text "Text input: "
    _ev2 ← E.input (A.placeholder "your input" $ E.defInput) $ text " An input. "
    E.br_
  E.divN $ do
    E.pN $ do
      text "Note that you probably want to use the functions in "
      E.a ( A.href (A.URL "https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom-core/src/Reflex/Dom/Widget/Input.hs") E.defA)
        $ text "Reflex.Dom.Widget.Input"
      text " or in "
      E.a (A.href (A.URL "https://github.com/reflex-frp/reflex-dom-contrib") E.defA)
        $ text "reflex-dom-contrib -package"
      text (" instead of the ones defined in "
        <> "the Interactive-module of this lib.")
    E.br_

listEx ∷ MonadWidget t m ⇒  m()
listEx = do
  E.h2N $ text "List examples"
  E.ulN $ do
    E.liN $ do
      text "A list (1), reversed with given values"
      E.ol (A.reversed E.defOl) $ do
        E.li (A.valueOlLi 1 E.defLi) $ text "Value 1 given"
        E.li (A.valueOlLi 2 E.defLi) $ text "Value 2 given"
        E.li (A.valueOlLi 3 E.defLi) $ text "Value 3 given"
    E.liN $ do
      text "A list (2), reversed without given values"
      E.ol (A.reversed E.defOl) $ do
        E.liN $ text "No value given (1st in list)"
        E.liN $ text "No value given (2st in list)"
        E.liN $ text "No value given (3st in list)"
    E.liN $ do
      text "A list (3), default attributes"
      E.olN $ do
        E.liN $ text "No value given (1st in list)"
        E.liN $ text "No value given (2st in list)"
        E.liN $ text "No value given (3st in list)"
    E.liN $ do
      text "A list (4), with given values "
      E.olN $ do
        E.li (A.valueOlLi 1 E.defLi) $ text "Value 1 given"
        E.li (A.valueOlLi 4 E.defLi) $ text "Value 4 given"
        E.li (A.valueOlLi 3 E.defLi) $ text "Value 3 given"
        E.li (A.valueOlLi 4 E.defLi) $ text "Value 4 given"

------------------------------------------------------------------------------
{-
--
hmm1 ∷ Globals
hmm1 = className "cl2" $ className "cl1" $ title "hmm1" def

hmm2 ∷ Globals
hmm2 = className "cl3" $ title "hmm2" def

hmm3 ∷ Globals
hmm3 = className "cl4" $  contentInheritEditable $ def

hmg1 ∷ Globals
hmg1 = hmm1 <> hmm2

hmg2 ∷ Globals
hmg2 = hmm2 <> hmm1

hmg3 ∷ Globals
hmg3 = hmm3 <> hmm1

hmg4 ∷ Globals
hmg4 = hmm1 <> hmm3

hmg5 ∷ Globals
hmg5 = hmm3 <> hmm1 <> hmm2 <> hmm3
-}
