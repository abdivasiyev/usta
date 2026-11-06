{-# LANGUAGE OverloadedStrings #-}

-- | Markdown's text as usta shows it: inline markup that Pango takes,
-- the HTML forges allow, heading anchors, and where a link may go.
module Usta.UI.Components.MarkdownTests (tests) where

import Control.Exception (SomeException, try)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Pango as Pango
import Hedgehog (Gen, Property, annotate, assert, evalIO, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.UI.Components.Markdown (Destination (..), destinationOf, htmlMarkup, inlineMarkup, plainOf, slug, withoutLinks)

tests :: TestTree
tests =
  testGroup
    "Markdown"
    [ testGroup
        "links go"
        [ testCase "to a heading" $ destinationOf "#getting-started" @?= Heading "getting-started"
        , testCase "to the web" $ destinationOf "https://github.com/a/b" @?= Web "https://github.com/a/b"
        , testCase "to mail" $ destinationOf "mailto:a@b.c" @?= Mail "mailto:a@b.c"
        , testCase "beside the text" $ destinationOf "docs/DESIGN.md" @?= Relative "docs/DESIGN.md"
        , testCase "nowhere for a file" $ destinationOf "file:///Applications/Calculator.app" @?= Ignored
        , testCase "nowhere for an app's own scheme" $ destinationOf "vscode://vscode.git/clone?url=x" @?= Ignored
        , testCase "nowhere for script" $ destinationOf "javascript:alert(1)" @?= Ignored
        , testCase "nowhere for a share" $ destinationOf "smb://server/share" @?= Ignored
        , testCase "a scheme in capitals is the same" $ destinationOf "HTTPS://x.y" @?= Web "HTTPS://x.y"
        ]
    , testGroup
        "inline markup"
        [ testCase "emphasis, strong, struck" $ inlineMarkup "*a* **b** ~~c~~" @?= "<i>a</i> <b>b</b> <s>c</s>"
        , testCase "code" $ assertBool "no code" ("font_family=\"monospace\"" `T.isInfixOf` inlineMarkup "`x < y`" && "x &lt; y" `T.isInfixOf` inlineMarkup "`x < y`")
        , testCase "a link" $ inlineMarkup "[usta](https://usta.azizovich.uz)" @?= "<a href=\"https://usta.azizovich.uz\">usta</a>"
        , testCase "a bare link" $ inlineMarkup "see https://x.y/z" @?= "see <a href=\"https://x.y/z\">https://x.y/z</a>"
        , testCase "text is escaped" $ inlineMarkup "a < b & c" @?= "a &lt; b &amp; c"
        , testCase "a quote in a link's address" $ inlineMarkup "[x](https://a.b/\"c)" @?= "<a href=\"https://a.b/&quot;c\">x</a>"
        , testCase "a tag left open closes at the paragraph's end" $ inlineMarkup "<i>open and **bold**" @?= "<i>open and <b>bold</b></i>"
        , testCase "a closing tag with nothing open is dropped" $ inlineMarkup "text</b> more" @?= "text more"
        , testProperty "Pango takes any of it" (withTests 1000 pangoTakesIt)
        ]
    , testGroup
        "HTML forges allow"
        [ testCase "bold, italic, code, keys" $ htmlMarkup "<b>a</b><em>b</em><kbd>c</kbd>" @?= "<b>a</b><i>b</i><span font_family=\"monospace\" background=\"#8080a0\" bgalpha=\"18%\">c</span>"
        , testCase "a line break" $ htmlMarkup "a<br/>b" @?= "a\nb"
        , testCase "an image as its alt text, linked" $ htmlMarkup "<img src=\"logo.png\" alt=\"logo\">" @?= "<a href=\"logo.png\">logo</a>"
        , testCase "scripts and styles are dropped" $ htmlMarkup "<script>x</script><style>y</style>" @?= "xy"
        , testCase "an unclosed angle is text" $ htmlMarkup "a < b" @?= "a &lt; b"
        , testCase "a comment is dropped" $ htmlMarkup "a<!-- hidden -->b" @?= "ab"
        , testCase "entities" $ htmlMarkup "&lt;tag&gt; &amp;&nbsp;" @?= "&lt;tag&gt; &amp;\x00A0"
        ]
    , testGroup
        "anchors and text"
        [ testCase "a heading's anchor" $ slug "Getting started!" @?= "getting-started"
        , testCase "an anchor keeps letters of any script" $ slug "Über uns" @?= "über-uns"
        , testCase "text without its tags" $ plainOf "<b>a</b> &amp; <i>b</i>" @?= "a & b"
        , testCase "markup without its links" $ withoutLinks "x <a href=\"y\">z</a> w" @?= "x z w"
        ]
    ]

-- | Any inline Markdown, nested and with HTML, comes out as markup Pango
-- parses once its links are out (GTK's label reads those itself): else
-- usta would show it as plain text.
pangoTakesIt :: Property
pangoTakesIt = property $ do
  pieces <- forAll (Gen.list (Range.linear 1 12) piece)
  let source = T.concat pieces
      markup = inlineMarkup source
  annotate (T.unpack markup)
  parsed <- evalIO (try (() <$ Pango.parseMarkup (withoutLinks markup) (-1) '\0'))
  assert (isRight (parsed :: Either SomeException ()))
  where
    piece :: Gen Text
    piece = Gen.element
      [ "word ", "*em* ", "**strong** ", "~~gone~~ ", "`a<b>&c` ", "[link](https://x.y/?a=1&b=2) ", "<b>bold</b> ", "<i>open "
      , "</i> ", "<kbd>K</kbd> ", "& ", "< ", "> ", "\"q\" ", "https://bare.example/x ", "<img src=\"a\" alt=\"b\"> ", "<br> " ]
