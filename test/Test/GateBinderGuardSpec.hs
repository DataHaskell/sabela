{-# LANGUAGE OverloadedStrings #-}

{- | The repair search may rewrite a name the cell only uses; rewriting a name
the cell BINDS rewrites its binder too. The guard that decides which is which
must not vacate when the source does not parse — that is precisely when repair
runs (LEDGER C1-4e).
-}
module Test.GateBinderGuardSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.GateRepair.Binders (boundNames)
import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (repairCandidates)
import Sabela.Parse (cellNames)
import Test.HarnessGen (genConIdent, genIdent, genModuleName)

freeName :: Text
freeName = "borrowedHelper"

{- | A cell whose binders are known by construction. The broken variant leaves
each declaration's parenthesis open, so no chunk of it parses and the AST has
nothing to say about what the cell binds.
-}
genBoundCell :: Bool -> Gen (Text, [Text])
genBoundCell broken = do
    binders <- nubbed . filter (/= freeName) <$> listOf1 genIdent
    let rhs = if broken then " xs = " <> freeName <> " (xs" else " xs = " <> freeName <> " xs"
    pure (T.unlines [b <> rhs | b <- binders], binders)
  where
    nubbed = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

{- | GHC's "perhaps use" suggestion for a name, offering a replacement of a
different syntactic class (a constructor) or a qualified name — neither can
stand where a binder stands.
-}
genIllegalRenameFor :: Text -> Gen Text
genIllegalRenameFor name = do
    con <- genConIdent
    m <- genModuleName
    q <- genIdent
    replacement <- elements [con, m <> "." <> q]
    pure
        ( "<interactive>:1:1: error: [GHC-88464]\n\
          \    Variable not in scope: "
            <> name
            <> "\n    Suggested fix: Perhaps use \8216"
            <> replacement
            <> "\8217 (imported from "
            <> m
            <> ")"
        )

rewritesBinder :: [Text] -> (Text, [Text]) -> Bool
rewritesBinder binders (_, applied) =
    or [(b <> " -> ") `T.isPrefixOf` f | b <- binders, f <- applied]

spec :: Spec
spec = describe "the repair search never rewrites a binding site" $ do
    it
        "control: the same machinery DOES rewrite a name the cell only uses, \
        \so the guard properties below are not vacuous"
        $ property
        $ forAll (elements [False, True])
        $ \broken ->
            forAll (genBoundCell broken) $ \(src, _) ->
                forAll (genIllegalRenameFor freeName) $ \diag ->
                    any (rewritesBinder [freeName]) (repairCandidates diag src)

    it
        "prop_theParserReallyDoesVacate: the broken cells this spec generates \
        \defeat the AST parser, and the lexical widening is what recovers \
        \their binders — without it the guard would have nothing to guard"
        $ property
        $ forAll (genBoundCell True)
        $ \(src, binders) ->
            conjoin
                [ counterexample
                    "the parser must find no binder here"
                    (Set.toList (fst (cellNames src)) === [])
                , counterexample
                    "boundNames must still know every binder"
                    (property (all (`Set.member` boundNames src) binders))
                ]

    it
        "prop_binderGuardHoldsOnParseableSource: no candidate renames a name \
        \the cell binds to a name that cannot stand in a binder"
        $ property
        $ forAll (genBoundCell False)
        $ \(src, binders) ->
            forAll (elements binders >>= genIllegalRenameFor) $ \diag ->
                not (any (rewritesBinder binders) (repairCandidates diag src))

    it
        "prop_binderGuardDoesNotVacateOnUnparseableSource: the same holds when \
        \the cell does not parse, which is when repair actually runs"
        $ property
        $ forAll (genBoundCell True)
        $ \(src, binders) ->
            forAll (elements binders >>= genIllegalRenameFor) $ \diag ->
                not (any (rewritesBinder binders) (repairCandidates diag src))
