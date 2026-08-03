{-# LANGUAGE OverloadedStrings #-}

{- | The law the gate's repair loop must not break: a repair may leave a cell
still failing, but it may never hand back a cell that is worse than the one it
was given. Deliberately library-agnostic — every case is stated in terms of the
diagnostic class, never a particular package.
-}
module Test.RepairNeverWorseSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (repairCandidates)
import Sabela.AI.Health (Health, healthOfResult, improvesHealthFor)
import Sabela.AI.Hints (sameNameClass)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (Guidance (..), diagnoseWith)
import Sabela.Model (CellError (..), bareCellError)
import Sabela.Parse (cellNames)

health :: [Text] -> Health
health msgs =
    healthOfResult (Right (ExecutionResult [] Nothing (map cellErr msgs) []))
  where
    cellErr = bareCellError Nothing Nothing

parseErrorText :: Text
parseErrorText =
    "parse error on input \8216=\8217\n\
    \    Suggested fix:\n\
    \      Perhaps you need a 'let' in a 'do' block?\n\
    \      e.g. 'let x = 5' instead of 'x = 5'"

{- | A "did you mean" naming a constructor for a lowercase binding, the shape
GHC emits for any package whose module exports a similarly-spelled type.
-}
notInScopeSuggesting :: Text -> Text -> Text
notInScopeSuggesting wrong suggestion =
    "<interactive>:1:1: error: [GHC-88464]\n\
    \    Variable not in scope: "
        <> wrong
        <> "\n    Suggested fix:\n      Perhaps use \8216"
        <> suggestion
        <> "\8217 (imported from Some.Module)"

spec :: Spec
spec = describe "the repair loop never hands back something worse" $ do
    describe "a candidate that stops the cell parsing is never an improvement" $ do
        it
            "refuses a parse failure that cleared a masking error, the strongest\
            \ claim to progress there is: an unparseable cell reports nothing"
            $ improvesHealthFor
                Set.empty
                (health ["Could not find module \8216Some.Module\8217"])
                (health [parseErrorText])
                `shouldBe` False

        it
            "refuses a parse failure that cleared an ambiguity, the shape that\
            \ started this: two ambiguous names traded for one parse error"
            $ improvesHealthFor
                Set.empty
                ( health
                    ["Ambiguous occurrence \8216null\8217", "Ambiguous occurrence \8216filter\8217"]
                )
                (health [parseErrorText])
                `shouldBe` False

        it "refuses a parse failure that cleared an out-of-scope error" $
            improvesHealthFor
                Set.empty
                (health ["Variable not in scope: foo"])
                (health [parseErrorText])
                `shouldBe` False

        it
            "does not punish a parse error that was already there: a candidate\
            \ is judged against what it changed, not what it inherited"
            $ improvesHealthFor
                Set.empty
                (health [parseErrorText, "Variable not in scope: foo"])
                (health [parseErrorText])
                `shouldBe` True

        it "still accepts an ordinary reduction with no parse error involved" $
            improvesHealthFor
                Set.empty
                (health ["Variable not in scope: foo", "Variable not in scope: bar"])
                (health ["Variable not in scope: bar"])
                `shouldBe` True

    describe "a rename never changes what kind of name it is" $ do
        it "a lowercase binding is never replaced by a constructor" $
            sameNameClass "medians" "Median" `shouldBe` False

        it "a constructor is never replaced by a lowercase name" $
            sameNameClass "Median" "medians" `shouldBe` False

        it "an ordinary lowercase rename is allowed" $
            sameNameClass "medians" "median" `shouldBe` True

        it
            "a qualified replacement is judged on its final component, so\
            \ disambiguating a use is still allowed"
            $ do
                sameNameClass "null" "Prelude.null" `shouldBe` True
                sameNameClass "null" "Data.Map.Strict.null" `shouldBe` True

        it "a qualified constructor is still a constructor" $
            sameNameClass "median" "Some.Module.Median" `shouldBe` False

        it "primes and underscores stay lowercase names" $ do
            sameNameClass "foldl" "foldl'" `shouldBe` True
            sameNameClass "go" "_go" `shouldBe` True

    describe "guidance is never fabricated" $ do
        it
            "says nothing about let for a parse error on =, whose own suggested\
            \ fix mentions let — even for a cell that does write one, since the\
            \ diagnostic does not name let as the offending token"
            $ map gCategory (diagnoseWith Nothing letSource parseErrorText)
                `shouldNotContain` ["top-level-let"]

        it
            "stays silent with the source in hand when it holds no top-level let:\
            \ advice about a let nobody wrote sends the reader elsewhere"
            $ map gCategory (diagnoseWith Nothing noLetSource parseErrorText)
                `shouldNotContain` ["top-level-let"]

        it "still speaks up when the cell really does open with a top-level let" $
            map
                gCategory
                ( diagnoseWith
                    Nothing
                    "let x = 5\n"
                    "<interactive>:1:5: error: parse error on input \8216let\8217"
                )
                `shouldContain` ["top-level-let"]

    describe "generated candidates are well-formed for any package" $ do
        let src = "-- cabal: build-depends: whatever\nmedians = aggregate xs\n\nmedians\n"

        it
            "never proposes a constructor for a lowercase binding, whatever\
            \ module the suggestion came from"
            $ do
                let diag = notInScopeSuggesting "medians" "Median"
                    applied = concatMap snd (repairCandidates diag src)
                filter ("-> Median" `T.isInfixOf`) applied `shouldBe` []

        it
            "may still rename a name the cell binds, as long as the replacement\
            \ can stand in a binding position"
            $ do
                let diag = notInScopeSuggesting "medians" "median"
                    sources = map fst (repairCandidates diag src)
                Set.member "medians" (fst (cellNames src)) `shouldBe` True
                sources `shouldSatisfy` any (T.isInfixOf "median = aggregate")

        it
            "never qualifies a name the cell binds: `DF.median = ...` parses no\
            \ better than a constructor does"
            $ do
                let diag = notInScopeSuggesting "medians" "DF.median"
                    sources = map fst (repairCandidates diag src)
                filter (T.isInfixOf "DF.median =") sources `shouldBe` []

        it
            "leaves a use-site rename unconstrained: a constructor is legal where\
            \ the name is only used, and GHC suggests one for a reason"
            $ do
                let useOnly = "d = datacenters org\n"
                    diag = notInScopeSuggesting "datacenters" "Datacenter"
                    sources = map fst (repairCandidates diag useOnly)
                sources `shouldSatisfy` any (T.isInfixOf "d = Datacenter org")

        it
            "still renames a name the cell does not define, which is the case\
            \ the repair exists for"
            $ do
                let useOnly = "answer = helo 1\n"
                    diag = notInScopeSuggesting "helo" "hello"
                    applied = concatMap snd (repairCandidates diag useOnly)
                any ("helo -> hello" `T.isPrefixOf`) applied `shouldBe` True

        it "no generated candidate ever renames a binder to a constructor" $ do
            let diags =
                    [ notInScopeSuggesting w s
                    | (w, s) <- [("xs", "Xs"), ("frame", "Frame"), ("plot", "Plot")]
                    ]
                bodies = concat [map fst (repairCandidates d src) | d <- diags]
            filter startsUpperBinding bodies `shouldBe` []

noLetSource :: Text
noLetSource = "Median = aggregate xs\n\nMedian\n"

-- | A cell that really does open a top-level @let@.
letSource :: Text
letSource = "let total = aggregate xs\n\ntotal\n"

startsUpperBinding :: Text -> Bool
startsUpperBinding = any upperBind . T.lines
  where
    upperBind l = case T.uncons l of
        Just (c, _) | c `elem` ['A' .. 'Z'] -> " = " `isInfixOf` T.unpack l
        _ -> False
