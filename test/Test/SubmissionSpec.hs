{-# LANGUAGE OverloadedStrings #-}

{- | Both write paths normalise a cell before the compile gate sees it. The
value they hand the gate must still carry the caller's own bytes, and every
rewrite must arrive as a note, or the model debugs text it never wrote
(LEDGER C1-4e, C2-8c).
-}
module Test.SubmissionSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.Submission (
    compiledText,
    insertSubmission,
    replaceSubmission,
    reroutedSubmission,
    submissionNotes,
    submittedText,
    verbatimSubmission,
 )
import Sabela.Model (CellType (..))
import Sabela.SessionTypes (CellLang (..))
import Test.HarnessGen (genCellSource, genIdent, genPackageName)

{- | Sources that exercise the normalisers: plain cells, cells with a cabal
line, and prose-shaped text that the insert path reclassifies.
-}
genWrite :: Gen Text
genWrite = do
    src <- genCellSource
    pkg <- genPackageName
    name <- genIdent
    kw <- elements ["data", "class", "type", "where"]
    oneof
        [ pure src
        , pure ("-- cabal: build-depends: " <> pkg <> "\n" <> src)
        , pure ("# heading\n\nsome prose about " <> pkg <> "\n")
        , pure ("let " <> name <> " = 1\n")
        , pure ("main = do\n  print (" <> name <> " :: Int)\n")
        , pure (kw <> " = 1\n")
        , pure (T.strip src)
        ]

genType :: Gen CellType
genType = elements [CodeCell, ProseCell]

spec :: Spec
spec = describe "the gate is given the caller's own bytes" $ do
    it
        "prop_insertKeepsTheSubmission: whatever the insert normaliser does, \
        \the submission it carries is the text that arrived"
        $ property
        $ forAll ((,) <$> genType <*> genWrite)
        $ \(ty, src) ->
            submittedText (snd (insertSubmission ty src)) === src

    it
        "prop_replaceKeepsTheSubmission: the same holds for a replace, whose \
        \normaliser also re-sticks the cell's cabal line"
        $ property
        $ forAll ((,,) <$> genWrite <*> genWrite <*> elements [Haskell, Python])
        $ \(oldSrc, newSrc, lang) ->
            submittedText (replaceSubmission lang oldSrc newSrc) === newSrc

    it
        "prop_everyRewriteIsDisclosed: a submission the harness rewrote never \
        \travels without a note explaining it"
        $ property
        $ forAll ((,) <$> genType <*> genWrite)
        $ \(ty, src) ->
            let sub = snd (insertSubmission ty src)
             in classify (compiledText sub /= submittedText sub) "rewritten" $
                    compiledText sub == submittedText sub || submissionNotes sub /= []

    it
        "prop_replaceRewritesAreDisclosed: likewise on the replace path, \
        \including the cabal line it puts back"
        $ property
        $ forAll ((,) <$> genWrite <*> genWrite)
        $ \(oldSrc, newSrc) ->
            let sub = replaceSubmission Haskell oldSrc newSrc
             in classify (compiledText sub /= submittedText sub) "rewritten" $
                    compiledText sub == submittedText sub || submissionNotes sub /= []

    it "the normalisers this spec exercises really do rewrite" $ do
        let rewrote src = case insertSubmission CodeCell src of
                (_, sub) -> (compiledText sub /= src, submissionNotes sub /= [])
        rewrote "let helper = 1\n" `shouldBe` (True, True)
        rewrote "main = do\n  print (1 :: Int)\n" `shouldBe` (True, True)
        rewrote "data = 1\n" `shouldBe` (True, True)

    it
        "prop_aRerouteKeepsTheOriginalSubmission: an insert turned into an \
        \overwrite still shows the caller its own text, and owes both notes"
        $ property
        $ forAll ((,,) <$> genType <*> genWrite <*> genWrite)
        $ \(ty, src, oldSrc) ->
            let sub = snd (insertSubmission ty src)
                rerouted = reroutedSubmission Haskell oldSrc sub
             in conjoin
                    [ submittedText rerouted === src
                    , counterexample
                        "the insert's own notes survive the reroute"
                        ( conjoin
                            [ property (n `elem` submissionNotes rerouted)
                            | n <- submissionNotes sub
                            ]
                        )
                    ]

    it "a verbatim submission compiles exactly what it was given" $
        property $
            forAll genWrite $
                \src ->
                    let sub = verbatimSubmission src
                     in (submittedText sub, compiledText sub, submissionNotes sub)
                            === (src, src, [])
