{-# LANGUAGE OverloadedStrings #-}

module Test.HoogleDbSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.HoogleClient (documentedVersion, hoogleDbArgs)

genSeg :: Gen FilePath
genSeg = listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))

genPath :: Gen FilePath
genPath = ("/" ++) <$> genSeg

genVersion :: Gen T.Text
genVersion = do
    parts <- resize 4 (listOf1 (choose (0 :: Int, 99)))
    pure (T.intercalate "." (map (T.pack . show) parts))

docUrl :: T.Text -> T.Text -> T.Text
docUrl pkg ver =
    "file:///home/u/.local/share/sabela/hackage-docs/"
        <> pkg
        <> "/"
        <> ver
        <> "/doc/html/Data-Thing.html#v:go"

spec :: Spec
spec = describe "hoogle databases and what they document (intention)" $ do
    describe "hoogleDbArgs — which databases a query reaches" $ do
        it "queries hoogle's own default when no main database is chosen" $
            hoogleDbArgs Nothing [] `shouldBe` [[]]

        it "passes the main database through without checking it exists" $
            property $
                forAll genPath $ \p ->
                    hoogleDbArgs (Just p) [] `shouldBe` [["--database=" ++ p]]

        it "adds one argument set per auxiliary database present" $
            property $
                forAll ((,) <$> genPath <*> genPath) $ \(a, b) ->
                    hoogleDbArgs Nothing [Just a, Just b]
                        `shouldBe` [[], ["--database=" ++ a], ["--database=" ++ b]]

        it "omits an auxiliary database that is not there" $
            property $
                forAll genPath $ \p ->
                    hoogleDbArgs Nothing [Nothing, Just p, Nothing]
                        `shouldBe` [[], ["--database=" ++ p]]

        {- The default arm is what reaches hoogle's own Stackage-only database,
        so dropping it would silently narrow every query. -}
        it "always leaves the default arm in place" $
            property $
                forAll (resize 3 (listOf (Just <$> genPath))) $ \aux ->
                    head (hoogleDbArgs Nothing aux) `shouldBe` []

        it "reaches one database per source it was given" $
            property $
                forAll (resize 3 (listOf (Just <$> genPath))) $ \aux ->
                    length (hoogleDbArgs Nothing aux) `shouldBe` length aux + 1

    describe "documentedVersion — a signature states which release it describes" $ do
        {- The Hackage doc index holds the last release that published haddock,
        which for a dark package is often not the release cabal would install.
        A signature that cannot say which version it describes invites the
        caller to import a name that release does not have. -}
        it "reads the version out of a package's documentation path" $
            property $
                forAll ((,) <$> genSeg <*> genVersion) $ \(pkg, ver) ->
                    documentedVersion (docUrl (T.pack pkg) ver) `shouldBe` ver

        it "reads the version hodatime's own documentation states" $
            documentedVersion
                ( "file:///x/hackage-docs/hodatime/0.1.1.1/doc/html/"
                    <> "Data-HodaTime-Instant.html#v:difference"
                )
                `shouldBe` "0.1.1.1"

        it "states no version when the path holds none" $
            documentedVersion "file:///x/dist-newstyle/build/doc/html/Foo.html"
                `shouldBe` ""

        it "states no version when the segment is not one" $
            documentedVersion "file:///x/hodatime/trunk/doc/html/Foo.html"
                `shouldBe` ""

        it "states no version for a url with no documentation path at all" $
            property $
                forAll genPath $ \p ->
                    documentedVersion (T.pack p) `shouldBe` ""

        it "never invents a version that is not spelled in the url" $
            property $
                forAll ((,) <$> genSeg <*> genVersion) $ \(pkg, ver) ->
                    let v = documentedVersion (docUrl (T.pack pkg) ver)
                     in T.null v || v `T.isInfixOf` docUrl (T.pack pkg) ver
