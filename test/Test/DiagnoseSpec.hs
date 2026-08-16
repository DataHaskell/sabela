{-# LANGUAGE OverloadedStrings #-}

{- | Reading structured facts out of a GHC diagnostic, and naming the package
that exposes a module. What the classifier then advises is in
"Test.DiagnoseAdviceSpec".
-}
module Test.DiagnoseSpec (diagnoseSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Edit.Repair.Resolvers (ambiguousCandidates)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (
    ambiguousOccurrence,
    ambiguousOccurrences,
    couldNotFindModule,
    couldNotFindModules,
    hiddenPackagesV,
    misnamedModule,
    neededExtension,
    notInScopeName,
    packageNeedsFlag,
 )
import Sabela.Diagnose.Packages (
    findModulePackage,
    packageNameIndex,
    resolvePackageToken,
 )
import Sabela.Model (bareCellError)
import Test.Hspec

diagnoseSpec :: Spec
diagnoseSpec = describe "Sabela.Diagnose" $ do
    describe "findModulePackage (the installed package db, not a table)" $ do
        it "names the package exposing an installed module" $
            findModulePackage "Data.Text" `shouldReturn` Just "text"
        it "answers for a package no curated table ever listed" $
            findModulePackage "Bluefin.State" `shouldReturn` Just "bluefin"
        it "returns Nothing for a module nothing installed exposes" $
            findModulePackage "Totally.Not.A.Module" `shouldReturn` Nothing

    describe "hiddenPackagesV (name with the version GHC named)" $ do
        it "splits the versioned unit token" $
            hiddenPackagesV
                "It is a member of the hidden package \8216text-2.0.2\8217."
                `shouldBe` [("text", Just "2.0.2")]
        it "keeps a hyphenated name whole and dedupes repeats" $
            hiddenPackagesV
                ( "hidden package \8216base16-bytestring-1.0.2.0\8217.\n"
                    <> "hidden package \8216base16-bytestring-1.0.2.0\8217."
                )
                `shouldBe` [("base16-bytestring", Just "1.0.2.0")]
        it "answers a version-less token honestly" $
            hiddenPackagesV "hidden package \8216sabela-notebook\8217."
                `shouldBe` [("sabela-notebook", Nothing)]

    describe "neededExtension (auto-fix detector)" $ do
        it "reads the extension from GHC's bare 'intended to use' hint" $
            neededExtension "Perhaps you intended to use TemplateHaskell"
                `shouldBe` Just "TemplateHaskell"
        it "handles GHC's real backquoted `Ext' phrasing and the :set -X hint" $ do
            neededExtension "Perhaps you intended to use the `LambdaCase' extension"
                `shouldBe` Just "LambdaCase"
            neededExtension "You may enable this with:\n  :set -XTupleSections"
                `shouldBe` Just "TupleSections"
        it "reads it from a quoted suggestion" $
            neededExtension
                "Suggested fix: Perhaps you intended to use \8216OverloadedStrings\8217"
                `shouldBe` Just "OverloadedStrings"
        it "ignores a suggestion outside the known-extension allow-list" $
            neededExtension "Perhaps you intended to use NotARealExtension"
                `shouldBe` Nothing
        it "is Nothing when no extension is suggested" $
            neededExtension "Couldn't match Int with Bool" `shouldBe` Nothing

    describe "misnamedModule + packageNeedsFlag (import auto-fix)" $ do
        let misnamedErr =
                T.unlines
                    [ "Could not find module `Data.Frame'."
                    , "Perhaps you meant"
                    , "  DataFrame (needs flag -package-id dataframe-0.7.0.0)"
                    , "  DataFrame (needs flag -package-id dataframe-0.3.3.7)"
                    ]
        it "reads the wrong module and GHC's suggested correction" $
            misnamedModule misnamedErr `shouldBe` Just ("Data.Frame", "DataFrame")
        it "reads the package from the needs-flag note, version stripped" $
            packageNeedsFlag misnamedErr `shouldBe` Just "dataframe"
        it "is Nothing when GHC offered no module suggestion" $
            misnamedModule "Could not find module `Foo.Bar'." `shouldBe` Nothing
        it "couldNotFindModule reads the name even with no correction hint" $
            couldNotFindModule "Could not find module `Data.DataFrame'."
                `shouldBe` Just "Data.DataFrame"
        it "couldNotFindModule is Nothing when no module is named" $
            couldNotFindModule "Not in scope: foo" `shouldBe` Nothing

        it "couldNotFindModules reads every missing module, not just the first" $
            couldNotFindModules
                ( T.unlines
                    [ "<no location info>: error: [GHC-35235]"
                    , "    Could not find module `DataFrame'."
                    , "<no location info>: error: [GHC-35235]"
                    , "    Could not find module `DataFrame.IO.CSV'."
                    ]
                )
                `shouldBe` ["DataFrame", "DataFrame.IO.CSV"]

        it
            "the singular reader is the first of the plural, unchanged for existing callers"
            $ couldNotFindModule
                "Could not find module `DataFrame'.\nCould not find module `X'."
                `shouldBe` Just "DataFrame"

    describe "ambiguousOccurrence (name-collision auto-fix)" $ do
        let ambigErr =
                T.unlines
                    [ "cell 23, line 4: Ambiguous occurrence `take'."
                    , "It could refer to"
                    , "   either `Prelude.take',"
                    , "          imported from `Prelude'"
                    , "          (and originally defined in `GHC.Internal.List'),"
                    , "       or `DataFrame.take',"
                    , "          imported from `DataFrame'"
                    , "          (and originally defined in `DataFrame.Operations.Subset')."
                    ]
        it "reads the ambiguous name and both qualified candidates" $
            ambiguousOccurrence ambigErr
                `shouldBe` Just ("take", ["Prelude.take", "DataFrame.take"])
        it "handles GHC's smart-quote form" $
            ambiguousOccurrence
                ( T.unlines
                    [ "Ambiguous occurrence \8216filter\8217"
                    , "It could refer to"
                    , "   either \8216Prelude.filter\8217, imported from \8216Prelude\8217"
                    , "   or     \8216DataFrame.filter\8217, imported from \8216DataFrame\8217"
                    ]
                )
                `shouldBe` Just ("filter", ["Prelude.filter", "DataFrame.filter"])
        it "does not pick up the originally-defined-in module as a candidate" $
            fmap snd (ambiguousOccurrence ambigErr)
                `shouldBe` Just ["Prelude.take", "DataFrame.take"]
        it "is Nothing for an ambiguous TYPE (a different error class)" $
            ambiguousOccurrence "Ambiguous type variable `a0' arising from a use of `show'"
                `shouldBe` Nothing
        it "is Nothing for an unrelated error" $
            ambiguousOccurrence "Couldn't match Int with Bool" `shouldBe` Nothing

    describe "ambiguousOccurrences (every clash, not just the first)" $ do
        let dataFrameClash =
                T.unlines
                    [ "<interactive>:211:164: error: [GHC-87543]"
                    , "    Ambiguous occurrence \8216null\8217."
                    , "    It could refer to"
                    , "       either \8216DataFrame.null\8217,"
                    , "              imported from \8216DataFrame\8217"
                    , "              (and originally defined in \8216dataframe-core-2.1.0.0:DataFrame.Internal.DataFrame\8217),"
                    , "           or \8216Prelude.null\8217,"
                    , "              imported from \8216Prelude\8217"
                    , "              (and originally defined in \8216ghc-internal-9.1202.0:GHC.Internal.Data.Foldable\8217)."
                    , ""
                    , "<interactive>:222:16: error: [GHC-87543]"
                    , "    Ambiguous occurrence \8216filter\8217."
                    , "    It could refer to"
                    , "       either \8216DataFrame.filter\8217,"
                    , "              imported from \8216DataFrame\8217"
                    , "              (and originally defined in \8216DataFrame.Operations.Subset\8217),"
                    , "           or \8216Prelude.filter\8217,"
                    , "              imported from \8216Prelude\8217"
                    , "              (and originally defined in \8216ghc-internal-9.1202.0:GHC.Internal.List\8217)."
                    ]
        it "reads both clashes, not just the first" $
            map fst (ambiguousOccurrences dataFrameClash) `shouldBe` ["null", "filter"]

        it "keeps each clash's candidates scoped to its own block" $
            ambiguousOccurrences dataFrameClash
                `shouldBe` [ ("null", ["DataFrame.null", "Prelude.null"])
                           , ("filter", ["DataFrame.filter", "Prelude.filter"])
                           ]

        it
            "the singular reader is the first of the plural, unchanged for existing callers"
            $ ambiguousOccurrence dataFrameClash
                `shouldBe` Just ("null", ["DataFrame.null", "Prelude.null"])

        it "is empty for an unrelated error" $
            ambiguousOccurrences "Couldn't match Int with Bool" `shouldBe` []

    describe "ambiguousCandidates (span-safe qualification)" $ do
        let ambigMsg =
                T.unlines
                    [ "Ambiguous occurrence `take'."
                    , "It could refer to"
                    , "   either `Prelude.take',"
                    , "       or `DataFrame.take'."
                    ]
            src = "f = putStrLn \"take a break\" >> take 3 xs"
            withSpan =
                Right
                    (ExecutionResult [] Nothing [bareCellError (Just 1) (Just 32) ambigMsg] [])
        it "qualifies the use-site, leaving the same name in a string untouched" $
            ambiguousCandidates withSpan src
                `shouldContain` ["f = putStrLn \"take a break\" >> DataFrame.take 3 xs"]
        it "never global-replaces: a span-less error yields no candidate" $
            ambiguousCandidates (Right (ExecutionResult [] (Just ambigMsg) [] [])) src
                `shouldBe` []

    describe "notInScopeName (case-insensitive, all GHC forms)" $ do
        it "reads the name from 'Variable not in scope:'" $
            notInScopeName "Variable not in scope: foo" `shouldBe` Just "foo"
        it "reads a quoted type constructor from capital-N 'Not in scope:'" $
            notInScopeName "Not in scope: type constructor or class \8216Picture\8217"
                `shouldBe` Just "Picture"
        it "reads a quoted data constructor from capital-N 'Not in scope:'" $
            notInScopeName "Not in scope: data constructor `Picture'"
                `shouldBe` Just "Picture"

    describe "resolvePackageToken (fuzzy package-name index)" $ do
        it "passes a real package token through by exact membership" $ do
            resolvePackageToken "dataframe" `shouldBe` Just "dataframe"
            resolvePackageToken "dataframe-core" `shouldBe` Just "dataframe-core"
            resolvePackageToken "granite" `shouldBe` Just "granite"
        it "repairs a versioned typo to the real package" $
            resolvePackageToken "dataframe-2" `shouldBe` Just "dataframe"
        it "repairs an abbreviated typo to the real package" $
            resolvePackageToken "df-core" `shouldBe` Just "dataframe-core"
        it "falls through cleanly for a token that resembles nothing" $ do
            resolvePackageToken "frobnicator" `shouldBe` Nothing
            resolvePackageToken "numpy" `shouldBe` Nothing
