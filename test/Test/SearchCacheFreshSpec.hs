{-# LANGUAGE OverloadedStrings #-}

module Test.SearchCacheFreshSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import Sabela.AI.SearchCache (
    SearchSource (..),
    freshnessRoot,
    localIndexFreshness,
    parseMetaEpoch,
    parseMetaPath,
 )

{- | An arbitrary path segment. No repository layout or artefact name is
load-bearing in what follows.
-}
genSeg :: Gen FilePath
genSeg = listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))

genPath :: Gen FilePath
genPath = ("/" ++) <$> genSeg

genRoots :: Gen [(FilePath, FilePath)]
genRoots = do
    roots <- resize 4 (listOf1 genPath)
    indexes <- mapM (const genPath) roots
    pure (zip roots indexes)

freshness :: FilePath -> Integer -> Integer -> Maybe SearchSource
freshness db g s = localIndexFreshness db (Just g) (Just s)

detailOf :: Maybe SearchSource -> Maybe Text
detailOf = fmap sourceDetail

spec :: Spec
spec = describe "search cache freshness (intention)" $ do
    describe "parseMetaEpoch — read only what the meta file states" $ do
        it "reads the epoch the generator wrote" $
            property $ \(NonNegative n) ->
                parseMetaEpoch
                    ( T.unlines
                        [ "# machine-produced"
                        , "generated_epoch=" <> T.pack (show (n :: Integer))
                        , "hackage_packages=1"
                        ]
                    )
                    `shouldBe` Just n

        it "reads nothing from a file that does not state one" $
            property $ \(NonNegative n) ->
                parseMetaEpoch
                    (T.unlines ["hackage_packages=" <> T.pack (show (n :: Integer))])
                    `shouldBe` Nothing

        it "reads nothing from an unparseable value" $
            parseMetaEpoch "generated_epoch=whenever" `shouldBe` Nothing

    describe "parseMetaPath — the database the generator says it built" $ do
        it "reads the path recorded under the key asked for" $
            property $
                forAll genPath $ \p ->
                    parseMetaPath
                        "hoogle_hackage_db"
                        ( T.unlines
                            [ "hoogle_local_db=/elsewhere.hoo"
                            , "hoogle_hackage_db=" <> T.pack p
                            ]
                        )
                        `shouldBe` Just p

        it "reads nothing for a key the meta does not record" $
            property $
                forAll genPath $ \p ->
                    parseMetaPath
                        "hoogle_hackage_db"
                        (T.unlines ["hoogle_local_db=" <> T.pack p])
                        `shouldBe` Nothing

        {- A key recorded with no value is not a path, and adopting "" would
        point the server at the working directory. -}
        it "reads nothing from a key recorded with an empty value" $
            parseMetaPath "hoogle_hackage_db" "hoogle_hackage_db=\n"
                `shouldBe` Nothing

        it "never reads a key that merely shares a prefix" $
            property $
                forAll genPath $ \p ->
                    parseMetaPath
                        "hoogle_hackage_db"
                        (T.unlines ["hoogle_hackage_dbx=" <> T.pack p])
                        `shouldBe` Nothing

    describe "localIndexFreshness — a claim only when both sides are known" $ do
        it "says nothing when the index has no recorded age" $
            property $ \(NonNegative s) ->
                forAll genPath $ \db ->
                    localIndexFreshness db Nothing (Just s) `shouldBe` Nothing

        it "says nothing when no source is there to compare against" $
            property $ \(NonNegative g) ->
                forAll genPath $ \db ->
                    localIndexFreshness db (Just g) Nothing `shouldBe` Nothing

        it "says nothing when neither is known" $
            property $
                forAll genPath $ \db ->
                    localIndexFreshness db Nothing Nothing `shouldBe` Nothing

        it "calls the index degraded exactly when the source is newer" $
            property $ \(NonNegative g) (NonNegative s) ->
                forAll genPath $ \db ->
                    fmap sourceUsable (freshness db g s) `shouldBe` Just (s <= g)

        it "never turns a fresh index stale by rebuilding it later" $
            property $ \(NonNegative g) (NonNegative s) (NonNegative d) ->
                forAll genPath $ \db ->
                    let usable e = fmap sourceUsable (freshness db e s)
                     in usable g == Just True ==> usable (g + d) `shouldBe` Just True

        it "labels the claim so the operator can act on it" $
            property $ \(NonNegative g) (NonNegative s) ->
                forAll genPath $ \db ->
                    fmap (T.null . sourceLabel) (freshness db g s) `shouldBe` Just False

        it "names the index the age is an age of" $
            property $ \(NonNegative g) (NonNegative s) ->
                forAll genPath $ \db ->
                    detailOf (freshness db g s)
                        `shouldSatisfy` maybe False (T.pack db `T.isInfixOf`)

    describe "freshnessRoot — an age is only ever the age of the index in use" $ do
        it "says nothing when no local database is in use" $
            property $
                forAll genRoots $ \roots ->
                    freshnessRoot Nothing roots `shouldBe` Nothing

        it "says nothing about a database that is not an index here" $
            property $
                forAll ((,) <$> genRoots <*> genPath) $ \(roots, db) ->
                    db `notElem` map snd roots ==>
                        freshnessRoot (Just db) roots === Nothing

        it "never names a root whose index is not the database in use" $
            property $
                forAll ((,) <$> genRoots <*> genPath) $ \(roots, db) ->
                    case freshnessRoot (Just db) roots of
                        Nothing -> property True
                        Just root -> ((root, db) `elem` roots) === True

        it "finds the root when the database in use is one of theirs" $
            property $
                forAll genRoots $ \roots ->
                    forAll (elements roots) $ \(_, index) ->
                        freshnessRoot (Just index) roots =/= Nothing
