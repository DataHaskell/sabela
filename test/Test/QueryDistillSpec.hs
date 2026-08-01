{-# LANGUAGE OverloadedStrings #-}

module Test.QueryDistillSpec (spec) where

import Data.Char (isDigit)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.Query (recordDecl, typeConstructors)
import Sabela.AI.LeakShape (leakyToken, scrubLeakyToken)
import Sabela.AI.VerifierDistill (answerVerdict, distillInfo, distillTypeAnswer)

ghcJsonError :: Text
ghcJsonError =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":{\"file\":\
    \\"<interactive>\"},\"severity\":\"Error\",\"code\":76037,\"message\":\
    \[\"Not in scope: data constructor `Columnable'\"],\"hints\":[]}"

spec :: Spec
spec = describe "check_type distillation at the emitting seam (R7-T3)" $ do
    describe "recordDecl never accepts leak-shaped text" $ do
        it "the GHC JSON error is NOT a record declaration (run-181440 bug)" $
            recordDecl ghcJsonError `shouldBe` Nothing
        it "a real record declaration still passes" $
            recordDecl
                ( T.unlines
                    [ "data TreeConfig"
                    , "  = TreeConfig {maxTreeDepth :: Int}"
                    ]
                )
                `shouldNotBe` Nothing

    describe "distillInfo scrubs the :info channel before recordDecl" $ do
        it "reduces the JSON error to nothing a recordDecl could keep" $
            recordDecl (distillInfo ghcJsonError) `shouldBe` Nothing
        it "keeps a legitimate declaration, minus provenance lines" $ do
            let info =
                    T.unlines
                        [ "data TreeConfig = TreeConfig {maxTreeDepth :: Int}"
                        , "  -- Defined in \8216DataFrame.DecisionTree.Types\8217"
                        ]
            distillInfo info `shouldSatisfy` T.isInfixOf "maxTreeDepth"
            distillInfo info `shouldSatisfy` (not . T.isInfixOf "Defined in")

    describe "the composed check_type answer" $ do
        it "signature + trailing blob distills to the signature alone" $
            distillTypeAnswer
                ( "D.sum :: (D.Columnable a, Num a) => D.Expr a -> \
                  \D.DataFrame -> a\n\n"
                    <> ghcJsonError
                )
                `shouldBe` "D.sum :: (D.Columnable a, Num a) => D.Expr a -> D.DataFrame -> a"
        it "a legitimate record append survives distillation" $ do
            let out =
                    distillTypeAnswer
                        "cfg :: TreeConfig\n\ndata TreeConfig = TreeConfig \
                        \{maxTreeDepth :: Int}"
            out `shouldSatisfy` T.isInfixOf "cfg :: TreeConfig"
            out `shouldSatisfy` T.isInfixOf "maxTreeDepth"
        it "typeConstructors still reads the distilled signature" $
            typeConstructors (distillTypeAnswer "x :: Maybe Int\n\n{\"v\":1}")
                `shouldBe` ["Maybe", "Int"]

    describe "verdicts are total on the distilled surface (5.3)" $ do
        it "a clean signature is ok" $
            answerVerdict "sum :: Num a => [a] -> a" `shouldBe` "ok"
        it "a distilled diagnostic is diagnostic" $
            answerVerdict (distillTypeAnswer ghcJsonError)
                `shouldBe` "diagnostic"
        it "emptiness is could-not-run, never a silent pass" $
            answerVerdict "  " `shouldBe` "could-not-run"

    leakPredicateSpec
    signatureShapeSpec

-- C1-12b ------------------------------------------------------------------

genPkg :: Gen Text
genPkg =
    elements
        [ "containers"
        , "text"
        , "aeson"
        , "dataframe-core"
        , "ghc-internal"
        , "stratus-core"
        ]

genVersion :: Gen Text
genVersion = elements ["1.2.3", "0.11.0.0", "9.1202.0"]

genHash :: Gen Text
genHash = elements ["", "c1e52ef7", "8f3a11b0deadbeefcafe12"]

genModulePath :: Gen Text
genModulePath =
    elements
        [ "Data.Map"
        , "Data.Map.Internal"
        , "Data.Aeson.Types.Internal"
        , "GHC.Internal.Data.Typeable.Internal"
        ]

genClass :: Gen Text
genClass =
    elements
        [ "Ord"
        , "Show"
        , "Typeable"
        , "KnownSymbol"
        , "Columnable'"
        , "Integral"
        , "Fractional"
        ]

genEntity :: Gen Text
genEntity =
    oneof
        [ genClass
        , elements ["insertWith", "alterF", "decodeStrict'", "foldl'"]
        , elements ["(<>)", "(.:?)"]
        ]

{- | A GHC unit-qualified token: @pkg-ver[-hash]:Module.Entity@, plus the
clean forms it must be distinguished from.
-}
genToken :: Gen Text
genToken =
    oneof
        [ qualified
        , shelled
        , nested
        , genEntity
        , (<>) <$> genModulePath <*> ((<>) "." <$> genEntity)
        , genPkg
        , genVersion
        ]
  where
    qualified = do
        u <- genUnit
        m <- genModulePath
        e <- genEntity
        pure (u <> ":" <> m <> "." <> e)
    -- A qualifier need not start its token: GHC wraps mid-atom, leaving the
    -- unit id behind a bracket, an identifier or a comma.
    shelled = do
        q <- qualified
        (open, close) <-
            elements [("(", ")"), ("[", "]"), ("f(", ")"), ("a,", ""), ("(a,", ")")]
        pure (open <> q <> close)
    nested = do
        k <- choose (2, 6 :: Int)
        us <- vectorOf k genUnit
        m <- genModulePath
        e <- genEntity
        pure (T.intercalate ":" us <> ":" <> m <> "." <> e)

genUnit :: Gen Text
genUnit = do
    p <- genPkg
    v <- genVersion
    h <- genHash
    pure (p <> "-" <> v <> (if T.null h then "" else "-" <> h))

leakPredicateSpec :: Spec
leakPredicateSpec =
    describe "the leak predicate detects exactly what it can repair (C1-12b)" $ do
        prop "leakyToken is the detection view of scrubLeakyToken" $
            forAll genToken $ \t ->
                leakyToken t === isJust (scrubLeakyToken t)
        prop "every repaired token is itself clean (idempotence)" $
            forAll genToken $ \t ->
                not (any leakyToken (maybeToList (scrubLeakyToken t)))
        prop "a repair never widens the token" $
            forAll genToken $ \t ->
                all ((<= T.length t) . T.length) (maybeToList (scrubLeakyToken t))
        prop "a repair never empties a token" $
            forAll genToken $ \t ->
                not (any T.null (maybeToList (scrubLeakyToken t)))
        prop "a repair keeps the token's brackets and commas" $
            forAll genToken $ \t -> case scrubLeakyToken t of
                Nothing -> property True
                Just s ->
                    counterexample (T.unpack (t <> " -> " <> s)) $
                        skeleton s === skeleton t

skeleton :: Text -> Text
skeleton = T.filter (`elem` ("()[]," :: String))

-- C1-12a / C2-5c ----------------------------------------------------------

{- | A GHC-rendered constrained signature: every constraint head carries a
unit qualifier, and the whole thing is hard-wrapped as GHCi wraps it.
-}
data GenSig = GenSig
    { gsClasses :: [Text]
    , gsText :: Text
    }
    deriving (Show)

genSignature :: Gen GenSig
genSignature = do
    nm <- elements ["query", "colOf", "reduce", "D.sum", "alterF"]
    k <- choose (1, 4 :: Int)
    classes <- vectorOf k genClass
    quals <- vectorOf k genQualifier
    res <- elements ["Expr a", "Maybe (Map k a)", "[a] -> a", "IO a"]
    col <- choose (34, 74)
    indent <- choose (4, 8)
    -- GHC does not always put a space after the comma, and then the next
    -- constraint's qualifier sits behind a type variable inside one word.
    sep <- elements [", ", ","]
    let atoms =
            [ q <> c <> " a"
            | (q, c) <- zip quals classes
            ]
        one =
            nm
                <> " :: ("
                <> T.intercalate sep atoms
                <> ") => "
                <> res
    pure (GenSig classes (wrapAt col indent one))
  where
    genQualifier = do
        unit <- genUnit
        m <- genModulePath
        elements ["", unit <> ":" <> m <> "."]

wrapAt :: Int -> Int -> Text -> Text
wrapAt col indent t = case go (T.words t) of
    [] -> ""
    (l : ls) -> T.intercalate "\n" (l : map (T.replicate indent " " <>) ls)
  where
    go [] = []
    go (w : ws) = let (l, rest) = fill w ws in l : go rest
    fill w ws
        | (nx : more) <- ws
        , T.length w + 1 + T.length nx <= col =
            fill (w <> " " <> nx) more
        | otherwise = (w, ws)

hasUnitQualifier :: Text -> Bool
hasUnitQualifier t = any versionColon (T.words (T.map depunct t))
  where
    depunct c = if c `elem` ("(){}[]\",=" :: String) then ' ' else c
    versionColon w = case T.breakOn ":" w of
        (pre, post) -> not (T.null post) && versionSuffixed pre
    versionSuffixed pre =
        any
            (\p -> not (T.null p) && T.all (\c -> isDigit c || c == '.') p)
            (T.splitOn "-" pre)

balanced :: Text -> Bool
balanced t = T.count "(" t == T.count ")" t

signatureShapeSpec :: Spec
signatureShapeSpec =
    describe "a wrapped signature distils to a whole signature (C1-12a, C2-5c)" $ do
        prop "parentheses stay balanced" $
            forAll genSignature (balanced . distillTypeAnswer . gsText)
        prop "the :: the input carried survives" $
            forAll genSignature $ \gs ->
                T.isInfixOf "::" (distillTypeAnswer (gsText gs))
        prop "every constraint head survives by its bare name" $
            forAll genSignature $ \gs ->
                let out = distillTypeAnswer (gsText gs)
                 in conjoin
                        [ counterexample
                            (T.unpack (c <> " lost from " <> out))
                            (T.isInfixOf (bareOf c) out)
                        | c <- gsClasses gs
                        ]
        prop "no unit-qualified token survives" $
            forAll genSignature $ \gs ->
                not (hasUnitQualifier (distillTypeAnswer (gsText gs)))
        prop "no atom is dropped with its qualifier: commas all survive" $
            forAll genSignature $ \gs ->
                let out = distillTypeAnswer (gsText gs)
                 in counterexample (T.unpack out) $
                        T.count "," out === T.count "," (gsText gs)
        prop "verdict ok implies a whole signature (the never-lie law)" $
            forAll genSignature $ \gs ->
                let out = distillTypeAnswer (gsText gs)
                 in answerVerdict out /= "ok"
                        || (balanced out && T.isInfixOf "::" out)

bareOf :: Text -> Text
bareOf = T.dropWhile (== '(') . T.dropWhileEnd (== ')')
