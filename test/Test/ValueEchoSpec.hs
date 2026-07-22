{-# LANGUAGE OverloadedStrings #-}

{- | Evidence-backed value echo (search-api.md §9.2, testing-plan M14): a
nullary pure binding echoes its value within the pinned size\/time bounds,
elision states the exceeded bound, @= _@ is unrepresentable in the rewritten
listing, and the listing stays within the R3.9 budget.
-}
module Test.ValueEchoSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.ValueEcho (
    definedListing,
    echoCharBound,
    echoListing,
    elidedOverSize,
    elidedUnevaluated,
    holeLines,
    listingCharBudget,
    nullaryPureType,
 )

{- | Generated nullary pure bindings: names crossed with value lengths at,
below, and beyond the size bound, plus the unevaluated (timeout) case.
-}
generatedBindings :: [(Text, Int)]
generatedBindings =
    [ (name, len)
    | name <- ["jsonSum", "dateDays", "total", "x'"]
    , len <- [0, 1, echoCharBound - 1, echoCharBound, echoCharBound + 1, 4000]
    ]

valueOf :: Int -> Text
valueOf n = T.replicate n "7"

spec :: Spec
spec = describe "value echo (§9.2 evidence-backed claims)" $ do
    describe "holeLines / nullaryPureType" $ do
        it "parses the = _ hole lines with their types" $
            holeLines "jsonSum :: Int = _\nxs :: [Int] = [1,2]\nf :: Int -> Int = _"
                `shouldBe` [("jsonSum", "Int"), ("f", "Int -> Int")]
        it "classifies nullary pure vs function vs IO types" $ do
            nullaryPureType "Int" `shouldBe` True
            nullaryPureType "[Double]" `shouldBe` True
            nullaryPureType "Int -> Int" `shouldBe` False
            nullaryPureType "IO ()" `shouldBe` False
            nullaryPureType "IO" `shouldBe` False

    describe "echo at the bound, elide beyond it with the reason (generated)" $
        it "echoes values ≤ the bound verbatim, elides over-bound with the size reason" $
            mapM_
                ( \(name, len) -> do
                    let listing = name <> " :: Int = _"
                        out = echoListing (const (Just (valueOf len))) listing
                    if len <= echoCharBound
                        then
                            ((name, len), out)
                                `shouldBe` ( (name, len)
                                           , name <> " :: Int = " <> valueOf len <> "\n"
                                           )
                        else do
                            ((name, len), elidedOverSize `T.isInfixOf` out)
                                `shouldBe` ((name, len), True)
                            out `shouldSatisfy` (not . T.isInfixOf (valueOf len))
                )
                generatedBindings

    describe "unevaluated (timeout / no Show) states the time budget" $
        it "a Nothing echo elides with the time reason" $ do
            let out = echoListing (const Nothing) "slow :: Integer = _"
            out `shouldSatisfy` T.isInfixOf elidedUnevaluated

    describe "'= _' is unrepresentable in the output (generated listings)" $
        it "no rewritten listing ever contains '= _'" $
            mapM_
                ( \(echo, listing) ->
                    (listing, "= _" `T.isInfixOf` echoListing echo listing)
                        `shouldBe` (listing, False)
                )
                [ (echoFn, T.unlines (map bindingLine shapes))
                | echoFn <- [const Nothing, const (Just "42"), const (Just (valueOf 4000))]
                , shapes <- listingShapes
                ]

    describe "function and IO bindings answer with their type alone" $ do
        it "drops the placeholder instead of echoing a function" $
            echoListing (const (Just "boom")) "f :: Int -> Int = _"
                `shouldBe` "f :: Int -> Int\n"
        it "never evaluates a non-nullary binding" $
            -- The echo callback is never consulted for a function binding:
            -- an always-failing callback still yields the clean type line.
            echoListing (\n -> error ("evaluated " <> T.unpack n)) "g :: IO () = _"
                `shouldBe` "g :: IO ()\n"

    describe "R3.9: the listing stays within the 2.5k budget, echo included" $ do
        it "a wide listing of echoed bindings is bounded with disclosure" $ do
            let listing =
                    T.unlines
                        [ "b" <> T.pack (show i) <> " :: Int = _"
                        | i <- [1 .. 200 :: Int]
                        ]
                out = echoListing (const (Just (valueOf echoCharBound))) listing
            T.length out `shouldSatisfy` (<= listingCharBudget)
            out `shouldSatisfy` T.isInfixOf "more bindings"
        it "a small listing is untouched by the budget" $ do
            let out = echoListing (const (Just "60")) "jsonSum :: Int = _"
            out `shouldBe` "jsonSum :: Int = 60\n"

    describe "write-ack echo (the cell's own definitions only)" $ do
        let session =
                T.unlines
                    [ "df :: DataFrame = _"
                    , "jsonSum :: Int = _"
                    , "helper :: Int -> Int = _"
                    ]
        it "restricts the listing to the defined names" $
            definedListing ["jsonSum"] session `shouldBe` "jsonSum :: Int = _\n"
        it "the composed write echo cites the observed value (jsonSum shape)" $
            echoListing
                (const (Just "60"))
                (definedListing ["jsonSum"] session)
                `shouldBe` "jsonSum :: Int = 60\n"
        it "a cell defining nothing echoes nothing" $
            definedListing [] session `shouldBe` ""

-- | Binding-line shapes for the unrepresentability grid.
listingShapes :: [[(Text, Text, Bool)]]
listingShapes =
    [ [("a", "Int", True)]
    , [("a", "Int", True), ("f", "Int -> Int", True), ("xs", "[Int]", False)]
    , [("g", "IO ()", True), ("h", "Integer", True)]
    , []
    ]

-- | Render one binding line; a hole when the Bool says so.
bindingLine :: (Text, Text, Bool) -> Text
bindingLine (n, ty, hole) =
    n <> " :: " <> ty <> (if hole then " = _" else " = [1,2]")
