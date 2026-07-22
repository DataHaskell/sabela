{-# LANGUAGE OverloadedStrings #-}

{- | R10-T5 x R8.4: a cell outcome carrying a multi-thousand-char ANSI wall
(barChart-on turn-27), once rendered for the model through 'renderOutcome',
crosses the surface clean — 'Eval.TranscriptLint' finds no raw-exception,
serialisation-in-string, or package-hash leak over the distilled wall. The
distillation is the seam that keeps the wall out of the model's context.
-}
module Test.OutcomeDistillLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.TranscriptLint (lintMessages)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Tools (renderOutcome)

ansiWall :: Text
ansiWall = T.concat (replicate 2000 "\ESC[95m\9608\9608\ESC[0m block ")

execOutcome :: Value
execOutcome =
    object
        [ "outcome" .= object ["tag" .= ("Succeeded" :: Text)]
        , "ok" .= True
        , "cellId" .= (0 :: Int)
        , "warnings" .= ([] :: [Value])
        , "outputs"
            .= [object ["oiMime" .= ("text/plain" :: Text), "oiOutput" .= ansiWall]]
        ]

asst :: Text -> [Text] -> Value
asst c calls =
    object
        ( ["role" .= ("assistant" :: Text), "content" .= c]
            <> [ "tool_calls"
                    .= [ object
                            ["function" .= object ["name" .= n, "arguments" .= object []]]
                       | n <- calls
                       ]
               | not (null calls)
               ]
        )

user :: Text -> Value
user c = object ["role" .= ("user" :: Text), "content" .= c]

toolRes :: Text -> Text -> Value
toolRes n c = object ["role" .= ("tool" :: Text), "tool_name" .= n, "content" .= c]

spec :: Spec
spec = describe "R10-T5 x R8.4: the distilled ANSI wall lints clean" $ do
    let rendered = renderOutcome (Right (ToolOk execOutcome))
        transcript =
            [ user "Draw a bar chart."
            , asst "" ["execute_cell"]
            , toolRes "execute_cell" rendered
            , asst "done" []
            ]
    it "renderOutcome strips the ANSI escape wall before the model sees it" $
        T.any (== '\ESC') rendered `shouldBe` False
    it "Eval.TranscriptLint stays clean over the distilled outcome" $
        lintMessages transcript `shouldBe` []
