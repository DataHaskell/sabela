{-# LANGUAGE OverloadedStrings #-}

{- | Two truths about a diagnostic: it names only binders the model wrote
(harness-truth C1-4f), and it never reports a GHCi session position as a
position in the model's cell (C1-4c).
-}
module Test.HarnessBinderSpec (spec) where

import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.CompileGate.Render (
    renderForDiagnostics,
    renderNonExecuting,
 )
import Sabela.AI.Health (namesHarnessBinder)
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.Api (RunResult (..))
import Sabela.Diagnose (Guidance (..), diagnoseWith)
import Sabela.Errors (parseErrors, scrubHarnessFrames)
import Sabela.Errors.Locate (Origin (..), attributeHeader)
import Sabela.Handlers.Exec (cellBody, parseCellResult)
import Sabela.Model (CellError (..))
import Sabela.Output (displayPrelude)
import ScriptHs.Compiled (linePragmaTag)
import Test.HarnessGen (genIdent, genModuleName)

genExpr :: Gen Text
genExpr =
    elements
        [ "Map.insert k v m"
        , "T.splitOn \",\" s"
        , "encode payload"
        , "foldr (+) 0 xs"
        , "BS.pack []"
        ]

-- | One source line of each shape the gate renderer treats differently.
genPiece :: Gen Text
genPiece =
    oneof
        [ (\i r -> i <> " = " <> r) <$> genIdent <*> genExpr
        , (<> " :: Int") <$> genIdent
        , (\m -> "import qualified " <> m <> " as M") <$> genModuleName
        , pure "{-# LANGUAGE TupleSections #-}"
        , (<> " <- readFile \"notes.txt\"") <$> genIdent
        , (\r -> "print (" <> r <> ")") <$> genExpr
        , genExpr
        , ("-- a note about " <>) <$> genIdent
        ]

genCellSource :: Gen Text
genCellSource = T.unlines <$> resize 6 (listOf1 genPiece)

-- | A cell that defines something, so it has a line for GHC to report on.
genDefiningSource :: Gen Text
genDefiningSource = do
    src <- genCellSource
    binding <- (\i r -> i <> " = " <> r) <$> genIdent <*> genExpr
    pure (src <> binding <> "\n")

{- | Top-level binders of a rendered script: the leading token of an
unindented line that binds it, with or without arguments.
-}
topLevelBinders :: Text -> [Text]
topLevelBinders src =
    [ tok
    | l <- T.lines src
    , not (T.null l)
    , not (isSpace (T.head l))
    , let (tok, rest) = T.break (== ' ') (T.strip l)
    , not (T.null tok)
    , T.all identChar tok
    , tok `notElem` keywords
    , binds rest
    ]
  where
    identChar ch = isAlphaNum ch || ch == '_' || ch == '\''
    keywords = ["data", "type", "newtype", "class", "instance", "import", "module"]
    binds rest = " = " `T.isInfixOf` T.takeWhile (/= ':') (" " <> T.stripStart rest)

-- | Names the renderer invents: present after rendering, absent before it.
inventedBinders :: (Text -> Text) -> Text -> [Text]
inventedBinders render src =
    [n | n <- topLevelBinders (render src), n `notElem` topLevelBinders src]

frameNaming :: Text -> Text
frameNaming n =
    "Couldn't match expected type \8216Int\8217 with actual type \8216[Char]\8217\n\
    \In the expression: x\n\
    \In an equation for \8216"
        <> n
        <> "\8217: "
        <> n
        <> " = x"

-- | A real diagnostic with a context frame naming a harness binder after it.
framedDiagnostic :: Text -> Text
framedDiagnostic n = "<interactive>:3:1: error: [GHC-88464]\n    " <> frameNaming n

-- | Every text a cell result puts in front of the model.
reportedTexts :: RunResult -> [CellError] -> [Text]
reportedTexts rr errs = maybe [] pure (rrError rr) <> map ceMessage errs

holeFitBlock :: [Text] -> Text
holeFitBlock names =
    T.unlines
        ( [ "<interactive>:3:1: error: [GHC-88464]"
          , "    \8226 Found hole: _ :: [Int] -> Int"
          , "      Valid hole fits include"
          ]
            <> concat [["        " <> n <> " :: [Int] -> Int"] | n <- names]
        )

{- | The plumbing the display prelude injects into every session. Hidden by
the leading underscore, and offered by GHC as an in-scope name all the same.
-}
preludeHidden :: [Text]
preludeHidden =
    [n | n <- topLevelBinders displayPrelude, "_" `T.isPrefixOf` n]

{- | Diagnostics that name a harness binder: in a context frame, and — the case
with nothing left once the frame goes — as the message itself.
-}
genHarnessDiagnostic :: Gen Text
genHarnessDiagnostic = do
    n <- elements preludeHidden
    elements
        [ "<interactive>:3:1: error: [GHC-88464]\n    " <> frameNaming n
        , "<interactive>:3:1: error: [GHC-88464]\n    Variable not in scope: " <> n
        , "<interactive>:3:1: error:\n    Ambiguous occurrence " <> n
        ]

spec :: Spec
spec = describe "a diagnostic describes the model's own source" $ do
    it "every hidden binder the session prelude injects is registered (C1-4f)" $ do
        preludeHidden `shouldNotBe` []
        filter (not . namesHarnessBinder) preludeHidden `shouldBe` []

    prop
        "a listing keeps its real offers when one names the harness (C1-4f)"
        $ forAll (listOf1 genIdent)
        $ \real ->
            let blob = holeFitBlock (real <> preludeHidden)
                offered = map hfWrite (parseHoleFits blob)
                scrubbed = scrubHarnessFrames blob
             in conjoin $
                    [ counterexample (T.unpack n) $
                        property (n `elem` offered && n `T.isInfixOf` scrubbed)
                    | n <- real
                    ]
                        <> [ counterexample (T.unpack n) $
                            property (n `notElem` offered && not (n `T.isInfixOf` scrubbed))
                           | n <- preludeHidden
                           ]

    prop
        "a diagnostic that is all harness frames is kept, not silenced (C1-4f)"
        $ forAll genHarnessDiagnostic
        $ \blob ->
            counterexample (T.unpack blob) $
                property (not (T.null (T.strip (scrubHarnessFrames blob))))

    it "a cell's body is submitted under that cell's tag (C1-4c)" $ do
        let body = cellBody 7 "alpha = 1\nbeta = alpha + 1\n"
        body `shouldSatisfy` T.isInfixOf (linePragmaTag 7)

    prop
        "any cell with a definition carries its own tag (C1-4c)"
        $ forAll (choose (1, 400 :: Int))
        $ \cid -> forAll genDefiningSource $ \src ->
            let body = cellBody cid src
             in counterexample (T.unpack body) $
                    property (linePragmaTag cid `T.isInfixOf` body)

    prop
        "every diagnostic filed under a cell tag keeps its cell line (C1-4c)"
        $ forAll (choose (1, 400 :: Int))
        $ \cid -> forAll (listOf1 (choose (1, 40 :: Int))) $ \lns ->
            forAll (choose (1, 90 :: Int)) $ \col -> forAll genIdent $ \name ->
                let one ln =
                        linePragmaTag cid
                            <> ":"
                            <> tshow ln
                            <> ":"
                            <> tshow col
                            <> ": error: [GHC-88464]\n    Variable not in scope: "
                            <> name
                    blob = T.unlines (map one lns)
                 in map ceLine (parseErrors blob) === map Just lns

    prop
        "every binder the gate renderer invents is a registered harness name (C1-4f)"
        $ forAll genCellSource
        $ \src ->
            conjoin
                [ counterexample (T.unpack n) (property (namesHarnessBinder n))
                | render <- [renderNonExecuting, renderForDiagnostics]
                , n <- inventedBinders render src
                ]

    prop
        "an untagged position is read as a session position, never a cell one (C1-4c)"
        $ forAll (choose (1, 5000 :: Int))
        $ \ln -> forAll (choose (1, 90 :: Int)) $ \col ->
            let hdr =
                    "<interactive>:"
                        <> tshow ln
                        <> ":"
                        <> tshow col
                        <> ": error: [GHC-88464]"
             in fmap fst (attributeHeader hdr) === Just (SessionPosition ln (Just col))

    prop
        "a plain-text cell result reports no binder the harness invented (C1-4f)"
        $ forAll genCellSource
        $ \src -> forAll (choose (1, 400 :: Int)) $ \cid ->
            conjoin
                [ counterexample (T.unpack n) $ ioProperty $ do
                    (rr, errs) <- parseCellResult False cid "" (framedDiagnostic n)
                    pure (not (any (n `T.isInfixOf`) (reportedTexts rr errs)))
                | n <- inventedBinders renderNonExecuting src
                ]

    prop
        "a frame naming a binder the gate renderer invented is scrubbed (C1-4f)"
        $ forAll genCellSource
        $ \src ->
            conjoin
                [ counterexample (T.unpack n) $
                    property (not (n `T.isInfixOf` scrubHarnessFrames (frameNaming n)))
                | render <- [renderNonExecuting, renderForDiagnostics]
                , n <- inventedBinders render src
                ]

    prop
        "a hole fit is never a binder the harness invented (C1-4f)"
        $ forAll genCellSource
        $ \src -> forAll (listOf1 genIdent) $ \real ->
            let invented = inventedBinders renderNonExecuting src
                offered = map hfWrite (parseHoleFits (holeFitBlock (real <> invented)))
             in conjoin
                    [counterexample (T.unpack n) (property (n `notElem` offered)) | n <- invented]

    prop
        "guidance never repeats a binder the harness invented (C1-4f)"
        $ forAll genCellSource
        $ \src ->
            conjoin
                [ counterexample (T.unpack (gMessage g)) $
                    property (not (n `T.isInfixOf` gMessage g))
                | n <- inventedBinders renderNonExecuting src
                , let err =
                        "<interactive>:9:1: error: [GHC-88464]\n\
                        \    Variable not in scope: zoop\n\
                        \    Perhaps you meant \8216"
                            <> n
                            <> "\8217"
                , g <- diagnoseWith Nothing src err
                ]

    prop
        "a GHCi session position is never reported as a cell line (C1-4c)"
        $ forAll (choose (1, 4000 :: Int))
        $ \ln -> forAll (choose (1, 200 :: Int)) $ \col -> forAll genIdent $ \name ->
            let blob =
                    "<interactive>:"
                        <> tshow ln
                        <> ":"
                        <> tshow col
                        <> ": error: [GHC-88464]\n    Variable not in scope: "
                        <> name
                errs = parseErrors blob
             in counterexample (show (map ceMessage errs)) $
                    conjoin
                        [ counterexample "claims a cell line" (property (isNothing (ceLine e)))
                            .&&. counterexample
                                "echoes a raw interactive position"
                                (property (not ("<interactive>:" `T.isInfixOf` ceMessage e)))
                        | e <- errs
                        ]

tshow :: Int -> Text
tshow = T.pack . show
