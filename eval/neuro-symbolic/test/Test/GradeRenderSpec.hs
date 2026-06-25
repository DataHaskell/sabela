{-# LANGUAGE OverloadedStrings #-}

module Test.GradeRenderSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Render (gradeRender)

outcomeWith :: Text -> Text -> ToolOutcome
outcomeWith mime payload =
    ToolOk $
        object
            [ "ok" .= True
            , "outputs" .= [outputItem mime payload]
            ]

outputItem :: Text -> Text -> Value
outputItem mime payload =
    object ["oiMime" .= mime, "oiOutput" .= payload]

realSvg :: Text
realSvg =
    "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\"/></svg>"

spec :: Spec
spec = describe "gradeRender" $ do
    it "passes on a real displaySvg output" $
        fst
            (gradeRender "displaySvg (bars [1,2,3])" (outcomeWith "image/svg+xml" realSvg))
            `shouldBe` True

    it "passes on inline SVG carried as text/html" $
        fst (gradeRender "displayHtml chart" (outcomeWith "text/html" realSvg))
            `shouldBe` True

    it "rejects an HTML error page with no drawing element" $
        fst
            ( gradeRender
                "displayHtml page"
                (outcomeWith "text/html" "<html><body>error</body></html>")
            )
            `shouldBe` False

    it "rejects an <svg> with no drawing child" $
        fst (gradeRender "displaySvg empty" (outcomeWith "image/svg+xml" "<svg></svg>"))
            `shouldBe` False

    it "rejects plain text output even if it looks like SVG" $
        fst (gradeRender "putStrLn s" (outcomeWith "text/plain" realSvg))
            `shouldBe` False

    it "rejects a tool error outcome" $
        fst
            (gradeRender "displaySvg x" (ToolErr (object ["error" .= ("boom" :: Text)])))
            `shouldBe` False

    it "rejects when the source contains a literal <svg token" $
        fst
            ( gradeRender
                "displayHtml \"<svg><path/></svg>\""
                (outcomeWith "text/html" realSvg)
            )
            `shouldBe` False

    it "rejects when the source contains a literal <path token" $
        fst
            (gradeRender "let s = \"<path d='M0 0'/>\"" (outcomeWith "image/svg+xml" realSvg))
            `shouldBe` False

    it "rejects when the source contains a GRADE_PASS token" $
        fst
            (gradeRender "putStrLn \"GRADE_PASS\"" (outcomeWith "image/svg+xml" realSvg))
            `shouldBe` False
