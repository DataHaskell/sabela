{-# LANGUAGE OverloadedStrings #-}

module Test.CabalFactsSpec (cabalFactsSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.CabalFacts (
    PkgFacts (..),
    parseCabalFacts,
    parseFactsRow,
    renderFactsRow,
 )

{- | A hodatime-shaped library: the exposed-modules block is followed by a
conditional inside the same stanza, which a naive continuation fold swallows.
-}
hodatimeCabal :: Text
hodatimeCabal =
    T.unlines
        [ "name:           hodatime"
        , "version:        1.1.0.0"
        , "homepage:       https://github.com/jason-johnson/hodatime"
        , "synopsis:       A fully featured date/time library based on Nodatime"
        , "description:"
        , "        A date/time library."
        , "        More prose here."
        , "category:       Data, Time"
        , ""
        , "library"
        , "  exposed-modules:"
        , "                   Data.HodaTime,"
        , "                   Data.HodaTime.Calendar.Gregorian,"
        , "                   Data.HodaTime.CalendarDate,"
        , "                   Data.HodaTime.Duration"
        , "  other-modules:"
        , "                   Data.HodaTime.Internal"
        , "  build-depends:"
        , "                   base >= 4.16 && < 4.21,"
        , "                   text"
        , "  if os(windows)"
        , "    build-depends: Win32 < 2.15"
        , "    extra-libraries: kernel32"
        , "  ghc-options:     -Wall"
        , ""
        , "test-suite spec"
        , "  type:            exitcode-stdio-1.0"
        , "  other-modules:   Test.Helper"
        ]

-- | A conditional that itself exposes modules, inside the public library.
conditionalCabal :: Text
conditionalCabal =
    T.unlines
        [ "name:      cond"
        , "library"
        , "  exposed-modules: Cond.Core"
        , "  if flag(extra)"
        , "    exposed-modules: Cond.Extra"
        , "  build-depends: base"
        ]

{- | The `cabal init` template, whose commented-out fields sit inside the
exposed-modules block and read as capitalised words.
-}
commentedCabal :: Text
commentedCabal =
    T.unlines
        [ "name:      fresh"
        , "library"
        , "    -- Modules exported by the library."
        , "    exposed-modules:  MyLib"
        , ""
        , "    -- Modules included in this library but not exported."
        , "    -- other-modules:"
        , ""
        , "    -- LANGUAGE extensions used by modules in this package."
        , "    -- other-extensions:"
        , ""
        , "    -- Other library packages from which modules are imported."
        , "    build-depends:    base ^>=4.18.0.0, Cabal"
        ]

-- | Fields aligned with a space before the colon, which cabal accepts.
alignedCabal :: Text
alignedCabal =
    T.unlines
        [ "name             : farmhash"
        , "library"
        , "  exposed-modules  : FarmHash"
        , "  other-modules    : FarmHash.Internal"
        , "  build-depends    : base >= 4.7 && < 5"
        , "                   , bytestring"
        , "  default-language : Haskell2010"
        ]

-- | A named sublibrary: its modules are not importable by a dependent.
sublibCabal :: Text
sublibCabal =
    T.unlines
        [ "name:      host"
        , "library"
        , "  exposed-modules: Host.Public"
        , ""
        , "library internal-bits"
        , "  exposed-modules: Host.Private"
        , ""
        , "executable host-cli"
        , "  main-is: Main.hs"
        , "  other-modules: Host.Cli.Opts"
        ]

cabalFactsSpec :: Spec
cabalFactsSpec = describe "Hackage .cabal facts" $ do
    describe "exposed-modules extraction" $ do
        it "reads the module list a folded block states" $ do
            pfModules (parseCabalFacts hodatimeCabal)
                `shouldBe` [ "Data.HodaTime"
                           , "Data.HodaTime.Calendar.Gregorian"
                           , "Data.HodaTime.CalendarDate"
                           , "Data.HodaTime.Duration"
                           ]
        it "stops at the next field rather than folding it in" $ do
            let ms = pfModules (parseCabalFacts hodatimeCabal)
            ms `shouldSatisfy` all ("Data.HodaTime" `T.isPrefixOf`)
        it "does not read other-modules as exposed" $
            pfModules (parseCabalFacts hodatimeCabal)
                `shouldSatisfy` notElem "Data.HodaTime.Internal"
        it "does not swallow a conditional's non-module fields" $ do
            let ms = pfModules (parseCabalFacts hodatimeCabal)
            ms `shouldSatisfy` notElem "Win32"
            ms `shouldSatisfy` notElem "kernel32"
        it "keeps modules a conditional inside the library exposes" $
            pfModules (parseCabalFacts conditionalCabal)
                `shouldBe` ["Cond.Core", "Cond.Extra"]
        it "omits a named sublibrary's modules, which no dependent can import" $
            pfModules (parseCabalFacts sublibCabal) `shouldBe` ["Host.Public"]
        it "omits an executable's own modules" $
            pfModules (parseCabalFacts sublibCabal)
                `shouldSatisfy` notElem "Host.Cli.Opts"
        it "reads no modules out of the comments around the block" $
            pfModules (parseCabalFacts commentedCabal) `shouldBe` ["MyLib"]
        it "stops at a commented-out field, so no dependency reads as one" $
            pfModules (parseCabalFacts commentedCabal)
                `shouldSatisfy` notElem "Cabal"
        it "stops at the next field when a space precedes its colon" $
            pfModules (parseCabalFacts alignedCabal) `shouldBe` ["FarmHash"]

    describe "package prose" $ do
        it "reads homepage and synopsis" $ do
            let f = parseCabalFacts hodatimeCabal
            pfHomepage f `shouldBe` "https://github.com/jason-johnson/hodatime"
            pfSynopsis f
                `shouldBe` "A fully featured date/time library based on Nodatime"
        it "does not fold the description into the synopsis" $
            pfSynopsis (parseCabalFacts hodatimeCabal)
                `shouldSatisfy` not . T.isInfixOf "prose"
        it "states nothing when a file names nothing" $ do
            let f = parseCabalFacts "name: bare\n"
            pfHomepage f `shouldBe` ""
            pfSynopsis f `shouldBe` ""
            pfModules f `shouldBe` []

    describe "cache row wire shape" $ do
        it "round-trips a package's facts" $ do
            let f = parseCabalFacts hodatimeCabal
            parseFactsRow (renderFactsRow "hodatime" f)
                `shouldBe` Just ("hodatime", f)
        it "is one tab-separated line per package" $ do
            let row = renderFactsRow "hodatime" (parseCabalFacts hodatimeCabal)
            T.count "\n" row `shouldBe` 0
            length (T.splitOn "\t" row) `shouldBe` 4
        it "round-trips a package with no facts at all" $ do
            let f = parseCabalFacts "name: bare\n"
            parseFactsRow (renderFactsRow "bare" f) `shouldBe` Just ("bare", f)
        it "reads no package from a blank or malformed line" $ do
            parseFactsRow "" `shouldBe` Nothing
            parseFactsRow "   " `shouldBe` Nothing
        it "keeps a row on one line when prose carries a tab or newline" $ do
            let f = parseCabalFacts "name: x\nsynopsis: a\tb\n"
                row = renderFactsRow "x" f
            T.count "\n" row `shouldBe` 0
            length (T.splitOn "\t" row) `shouldBe` 4
