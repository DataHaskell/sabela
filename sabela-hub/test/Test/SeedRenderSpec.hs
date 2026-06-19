{-# LANGUAGE OverloadedStrings #-}

{- | The gallery seeder's Markdown→HTML rendering: inline spans (escape, image,
link, bold, code), the notebook body block machine (code fences, quoted output
blocks, headings, paragraphs), and the page shell. Pins the emitted HTML shape
the static gallery depends on.
-}
module Test.SeedRenderSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hub.Gallery.SeedRender (inline, page, renderBody)

spec :: Spec
spec = do
    describe "inline" $ do
        it "escapes ampersands and angle brackets" $
            inline "a & <b>" `shouldBe` "a &amp; &lt;b&gt;"
        it "renders bold" $
            inline "say **hi** now" `shouldBe` "say <strong>hi</strong> now"
        it "renders inline code" $
            inline "call `foo`" `shouldBe` "call <code>foo</code>"
        it "renders links" $
            inline "see [docs](https://x.y)"
                `shouldBe` "see <a href=\"https://x.y\">docs</a>"
        it "renders images" $
            inline "![cat](c.png)" `shouldBe` "<img alt=\"cat\" src=\"c.png\">"

    describe "renderBody" $ do
        it "renders a haskell code fence as pre.code" $
            renderBody "```haskell\nx = 1\n```"
                `shouldBe` "<pre class=\"code\"><code class=\"language-haskell\">x = 1</code></pre>"
        it "renders a non-haskell fence as nohighlight" $
            renderBody "```text\nplain\n```"
                `shouldBe` "<pre class=\"code\"><code class=\"nohighlight\">plain</code></pre>"
        it "renders a plain output block as pre.out" $
            renderBody "> 17" `shouldBe` "<pre class=\"out\">17</pre>"
        it "renders an html output block as div.out, dropping the mime comment" $ do
            let out = renderBody "> <!-- scripths:mime text/html -->\n> <b>hi</b>"
            out `shouldBe` "<div class=\"out\"><b>hi</b></div>"
        it "renders a text/plain output block as pre.out, dropping the mime comment" $ do
            let out = renderBody "> <!-- scripths:mime text/plain -->\n> 17"
            out `shouldBe` "<pre class=\"out\">17</pre>"
        it "emits nothing for an output block with no content (only the mime comment)" $
            renderBody "> <!-- scripths:mime text/plain -->" `shouldBe` ""
        it "renders h2 but drops the page h1" $ do
            renderBody "## Section" `shouldBe` "<h2>Section</h2>"
            renderBody "# Title" `shouldBe` ""
        it "renders a paragraph with inline markup" $
            renderBody "hello **world**" `shouldBe` "<p>hello <strong>world</strong></p>"
        it "skips the sabela:cell prose separator" $
            renderBody "a\n\n<!-- sabela:cell -->\n\nb"
                `shouldBe` "<p>a</p>\n<p>b</p>"
        it "skips a standalone html comment (e.g. the scripths version banner)" $
            renderBody "<!-- scripths: 0.5.2.0 -->\n\n# Title\n\nhello"
                `shouldBe` "<p>hello</p>"

    describe "page" $
        it "embeds the title, byline, and slug-scoped fork action" $ do
            let html = page "My Title" "Ada" "ab120001" "<p>body</p>"
            ("<title>My Title</title>" `T.isInfixOf` html) `shouldBe` True
            ("by Ada" `T.isInfixOf` html) `shouldBe` True
            ("/_hub/fork/ab120001" `T.isInfixOf` html) `shouldBe` True
            ("<p>body</p>" `T.isInfixOf` html) `shouldBe` True
