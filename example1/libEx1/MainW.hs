{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainW where

import Reflex.Dom      (text, display, count, elAttr, (=:), (&),
                       MonadWidget, foldDyn, Dynamic)
import Reflex.Dom.Core (mainWidget)
import Data.Default    (def)
import Data.Monoid     ((<>))
import Language.Javascript.JSaddle (JSM)

import Reflex.Dom.HTML5.Attrs as A (Globals, AnyAttr, id_, URL(URL), className,
                                   placeholder, srcDoc, src, width, href,
                                   sbAllowSameOrigin, title, attrMap,
                                   valueOlLi, gDef, reversed)
import Reflex.Dom.HTML5.Elements as E (eInput, eEmbedC, eIFrameC, eBN, eA,
                                      eP, eAC, ePN, eH1N, eDivN, eUlN, eLiN,
                                      eLi, eOlN, eOl, eBr_, eH2N)

--------------------------------------------------------------------------------

mainW :: JSM ()
mainW = mainWidget $ do
  eH1N $ text "Welcome to reflex-dom-htmlea"
  intro
  caveats
  examples

intro :: MonadWidget t m => m ()
intro = do
  eDivN $ do
    ePN $ do
      text "Why? Reflex-dom-htmlea -lib "
      eBN $ text " helps "
      text $ " to avoid some of the run-time "
       <> "errors that are very easy to introduce with typos in attribute or "
       <> "element names. Simply said: I hate runtime debugging! Especially "
       <> "if the compiler is able to help."
    ePN $
      text $ "In addition to that, the aim is to provide smart "
       <> "constructors "
       <> "for some of the common usage-patterns in order to help avoid combining "
       <> "the elements in non-conforming way. "
       <> "At the moment there ain't much support for that. "
    ePN $ do
      text "There is some support to"
      eUlN $ do
        eLiN $ text "let elements have only those attributes the specification says"
        eLiN $ text "use a set of pre-defined element and attribute names"
    ePN $ do
      text "This example app uses the "
      eA (href (URL "https://gist.github.com/3noch/ee335c94b92ea01b7fee9e6291e833be") def)
        $ text  "turbo-charged ghcid settings (auto-reloading)"
      text " to enable fluent work flow."
    ePN $ do
      text $ ("Note that the structure of this lib is simple and "
        <> " it is very easy to use. ")

caveats :: MonadWidget t m => m ()
caveats = do
  eH2N $ text "Caveats"
  eDivN $ do
    ePN $ text "See README.md"
    ePN $ text $ "This lib is probably going to go through some "
            <> "renaming, reshaping and other things as this lib "
            <> "is still in its infancy. "
            <> "Author is open to suggestions, PRs etc. (with usual disclaimers). "
    ePN $ text $ "And bugs are more than likely. There ain't any "
            <> "test cases (just an example program) so some of the "
            <> "element or attribute names may have typos atm. "
            <> "Anyhow, given with little time, they are probably going to get "
            <> "removed. "

examples :: MonadWidget t m => m ()
examples = do
  eH2N $ text "Other examples (take a look at the code)"
  eDivN $ do
    ePN $ text "Note that you can "
    eUlN $ do
      elAttr "li" (attrMap $ id_ "li1" $ (def :: Globals)) $ text "combine"
      elAttr "li" (attrMap $ id_ "li2" $ gDef) $ text "the defined elements"
      elAttr "li" (attrMap li3Attrs) $ text "and attributes with"
      -- Combining the Maps and provided combinators is possible.
      elAttr "li" ("badAttr" =: "badValue" <> attrMap li3Attrs)
        $ text "el-family of functions."
    eP (title "hmm" $ className "pclass" def) $ text "A paragraph example."
    -- eP ( href (URL "hmm") $ className "pclass" $ def) $ text "Paragraph3"
    -- No instance as p-tag doesn't have href-attr.
    linkEx
    embedEx
    listEx
  where
    li3Attrs :: Globals
    li3Attrs = className "myclass" $ id_ "li3" $ def

linkEx :: MonadWidget t m => m ()
linkEx = do
  eH2N $ text "Link example"
  elAttr "p" (attrMap p1Attrs) $ do
    text ("Paragraph-example with badly formed attributes. "
      <> "(Look at the attributes of this paragraph.) ")
    ev <- eAC ( href (URL "#") $ className "myWarning" $ def
              ) $ text "Here is a link with counter."
    numbs <- foldDyn (+) (0 :: Int)  (1 <$ ev)
    text " Clicked the previous link "
    display numbs
    text " times and the following "
    cnt :: Dynamic t Int
      <- count =<< (eAC (href (URL "#") def) $ text "link")
    display cnt
    text " times."
    eBr_
  where
    -- Note that by using AnyAttr's, it is still easy to write non-valid
    -- attributes (p-element doesn't have href-attributes).
    p1Attrs :: AnyAttr
    p1Attrs = href (URL "http://localhost") $ id_ "p1" $ def

embedEx :: MonadWidget t m => m ()
embedEx = do
  eH2N $ text "Embdedded content examples"
  eDivN $ do
    text "Embedded content div "
    fcnt :: Dynamic t Int
      <- count =<<
        eEmbedC (width 200 $  src (URL "./figs/eveninglandscape.jpg") def)
    text "Clicked the previous fig "
    display fcnt
    text " times."
    eBr_
    _ev <- eIFrameC
      (sbAllowSameOrigin $
       srcDoc "<p>Some HTML.</p><br><div>And more of it.</div>" def
      ) $ text "Alternative text if iframe not shown."
    eBr_
  eDivN $ do
    text "Text input: "
    _ev2 <- eInput (placeholder "your input" $ def) $ text " An input. "
    eBr_
  eDivN $ do
    ePN $ do
      text "Note that you probably want to use the functions in "
      eA (href (URL "https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom-core/src/Reflex/Dom/Widget/Input.hs") def)
        $ text "Reflex.Dom.Widget.Input"
      text " or in "
      eA (href (URL "https://github.com/reflex-frp/reflex-dom-contrib") def)
        $ text "reflex-dom-contrib -package"
      text (" instead of the ones defined in "
        <> "the Interactive-module of this lib.")
    eBr_

listEx :: MonadWidget t m =>  m()
listEx = do
  eH2N $ text "List examples"
  eUlN $ do
    eLiN $ do
      text "A list (1), reversed with given values"
      eOl (reversed def) $ do
        eLi (valueOlLi 1 def) $ text "Value 1 given"
        eLi (valueOlLi 2 def) $ text "Value 2 given"
        eLi (valueOlLi 3 def) $ text "Value 3 given"
    eLiN $ do
      text "A list (2), reversed without given values"
      eOl (reversed def) $ do
        eLiN $ text "No value given (1st in list)"
        eLiN $ text "No value given (2st in list)"
        eLiN $ text "No value given (3st in list)"
    eLiN $ do
      text "A list (3), default attributes"
      eOlN $ do
        eLiN $ text "No value given (1st in list)"
        eLiN $ text "No value given (2st in list)"
        eLiN $ text "No value given (3st in list)"
    eLiN $ do
      text "A list (4), with given values "
      eOlN $ do
        eLi (valueOlLi 1 def) $ text "Value 1 given"
        eLi (valueOlLi 4 def) $ text "Value 4 given"
        eLi (valueOlLi 3 def) $ text "Value 3 given"
        eLi (valueOlLi 4 def) $ text "Value 4 given"

------------------------------------------------------------------------------
{-
--
hmm1 :: Globals
hmm1 = className "cl2" $ className "cl1" $ title "hmm1" def

hmm2 :: Globals
hmm2 = className "cl3" $ title "hmm2" def

hmm3 :: Globals
hmm3 = className "cl4" $  contentInheritEditable $ def

hmg1 :: Globals
hmg1 = hmm1 <> hmm2

hmg2 :: Globals
hmg2 = hmm2 <> hmm1

hmg3 :: Globals
hmg3 = hmm3 <> hmm1

hmg4 :: Globals
hmg4 = hmm1 <> hmm3

hmg5 :: Globals
hmg5 = hmm3 <> hmm1 <> hmm2 <> hmm3
-}
