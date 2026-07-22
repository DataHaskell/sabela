{-# LANGUAGE OverloadedStrings #-}

{- | R7-T3 (R3.6/R3.9/R3.10): over the (signature x trailing-blob x tool)
grid the distilled answer is exactly the writable line plus a closed verdict,
validator-clean and budgeted; list_bindings is writable-or-one-statement.
-}
module Test.VerifierSurfaceSpec (verifierSurfaceSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Char (isAlphaNum, isLower)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.ValueEcho (
    listingCharBudget,
    liveBindingsReport,
    noWritableBindings,
 )
import Sabela.AI.Verdict (verdictVocabulary)
import Sabela.AI.VerifierDistill (
    answerVerdict,
    distillBudget,
    distillTypeAnswer,
 )
import Siza.Agent.Discover.Envelope (stringViols)

-- | Generated signature grid: plain, constrained, operator and qualified.
sigGrid :: [(Text, Text)]
sigGrid =
    [ ("revenueTotal", "Double")
    , ("D.sum", "(D.Columnable a, Num a) => D.Expr a -> D.DataFrame -> a")
    , ("columnAsList", "D.Columnable a => D.Expr a -> D.DataFrame -> [a]")
    , ("(<>)", "Semigroup a => a -> a -> a")
    ]

{- | The four trailing-blob leak classes of the run-181440 grid: double-
encoded GHC JSON, a raw @:info@ dump, a package-hash atom, control chars.
-}
blobGrid :: [(Text, Text)]
blobGrid =
    [
        ( "double-encoded-json"
        , "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"severity\":\
          \\"Error\",\"code\":76037,\"message\":[\"Not in scope: data \
          \constructor `Columnable'\"],\"hints\":[]}"
        )
    ,
        ( "raw-info-dump"
        , "type D.Expr :: * -> *\ndata Expr a\n  -- Defined in \
          \`dtfrm-cr-2.0.0.0-c1e52ef7:DataFrame.Internal.Expression'\n\
          \instance Show (Expr a) -- Defined in `DataFrame.Internal'"
        )
    ,
        ( "package-hash-atom"
        , "dtfrm-cr-2.0.0.0-c1e52ef7:DataFrame.Internal.DataFrame.DataFrame"
        )
    , ("control-chars", "\SOH\STX stale probe \ETX")
    ]

-- | The run-181440 revenueTotal fixture: correct signature + raw GHC JSON.
run181440CheckType :: Text
run181440CheckType =
    "D.sum :: (D.Columnable a, Num a) => D.Expr a -> D.DataFrame -> a\n\n"
        <> snd (head blobGrid)

writableBindingLine :: Text -> Bool
writableBindingLine l =
    let (name, rest) = T.breakOn " :: " l
        name' = T.strip name
        base = last (T.splitOn "." name')
     in not (T.null rest)
            && maybe False (\(c, _) -> isLower c || c == '_') (T.uncons base)
            && T.all (\c -> isAlphaNum c || c `elem` ("._'" :: String)) name'

verifierSurfaceSpec :: Spec
verifierSurfaceSpec = describe "verifier surfaces are envelope citizens (R7-T3)" $ do
    describe "check_type distillation over the (signature x blob) grid" $ do
        it "keeps exactly the writable signature line, verdict ok" $
            forM_ sigGrid $ \(n, ty) ->
                forM_ blobGrid $ \(label, blob) -> do
                    let sig = n <> " :: " <> ty
                        out = distillTypeAnswer (sig <> "\n\n" <> blob)
                    (label, out) `shouldBe` (label, sig)
                    (label, answerVerdict out) `shouldBe` (label, "ok")
        it "every grid cell passes the envelope string validator" $
            forM_ sigGrid $ \(n, ty) ->
                forM_ blobGrid $ \(label, blob) -> do
                    let out =
                            distillTypeAnswer
                                (n <> " :: " <> ty <> "\n\n" <> blob)
                    (label, stringViols (String out)) `shouldBe` (label, [])
        it "every distilled output fits the declared budget (R3.9)" $
            forM_ sigGrid $ \(n, ty) ->
                forM_ blobGrid $ \(_, blob) ->
                    T.length (distillTypeAnswer (n <> " :: " <> ty <> "\n\n" <> blob))
                        `shouldSatisfy` (<= distillBudget)
        it "the run-181440 fixture distills to the one signature line" $
            distillTypeAnswer run181440CheckType
                `shouldBe` "D.sum :: (D.Columnable a, Num a) => D.Expr a -> D.DataFrame -> a"

    describe "a blob with no signature distills honestly" $ do
        it "validator-clean with a closed-vocabulary verdict, every class" $
            forM_ blobGrid $ \(label, blob) -> do
                let out = distillTypeAnswer blob
                (label, stringViols (String out)) `shouldBe` (label, [])
                (label, answerVerdict out `elem` verdictVocabulary)
                    `shouldBe` (label, True)
        it "a class with no writable content is ONE diagnostic line" $
            forM_ [b | b@(l, _) <- blobGrid, l /= "raw-info-dump"] $
                \(label, blob) -> do
                    let out = distillTypeAnswer blob
                    (label, T.count "\n" out) `shouldBe` (label, 0)
                    (label, "error:" `T.isPrefixOf` out) `shouldBe` (label, True)
        it "a GHC JSON diagnostic keeps its message as the one line" $ do
            let out = distillTypeAnswer (snd (head blobGrid))
            out `shouldSatisfy` T.isInfixOf "Not in scope"
            answerVerdict out `shouldBe` "diagnostic"

    describe "list_bindings renders writable lines only (R3.10)" $ do
        let echo = const (Just "42")
        it "the run-181440 hash-FQN listing never reaches the model" $ do
            let raw =
                    T.unlines
                        [ "df :: D.DataFrame = dtfrm-cr-2.0.0.0-c1e52ef7:\
                          \DataFrame.Internal.DataFrame.DataFrame"
                        , "  (vc 1010, [W] 3)"
                        , "revenueTotal :: Double = _"
                        ]
                report = liveBindingsReport ["df", "revenueTotal"] echo raw
            report `shouldSatisfy` (not . T.isInfixOf "dtfrm-cr")
            report `shouldSatisfy` T.isInfixOf "df :: D.DataFrame"
            report `shouldSatisfy` T.isInfixOf "revenueTotal :: Double = 42"
            stringViols (String report) `shouldBe` []
        it "every signature line of a report is writable, on the whole grid" $
            forM_ blobGrid $ \(label, blob) -> do
                let raw = "x :: Int = " <> T.replace "\n" " " blob <> "\n"
                    report = liveBindingsReport ["x"] (const Nothing) raw
                (label, stringViols (String report)) `shouldBe` (label, [])
                forM_ [l | l <- T.lines report, " :: " `T.isInfixOf` l] $ \l ->
                    (label, l, writableBindingLine l)
                        `shouldBe` (label, l, True)
        it "an unwritable-only listing is ONE bounded statement" $ do
            let raw = "dtfrm-cr-2.0.0.0-c1e52ef7:DataFrame.Internal = <fn>\n"
                report = liveBindingsReport [] (const Nothing) raw
            T.strip report `shouldBe` T.strip noWritableBindings
            T.count "\n" (T.strip report) `shouldBe` 0
        it "reports stay within the listing budget (R3.9)" $ do
            let raw =
                    T.unlines
                        [ "b" <> T.pack (show i) <> " :: Int = _"
                        | i <- [1 .. 400 :: Int]
                        ]
            T.length (liveBindingsReport ["b1"] echo raw)
                `shouldSatisfy` (<= listingCharBudget)
