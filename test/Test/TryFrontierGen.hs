{-# LANGUAGE OverloadedStrings #-}

{- | The candidate grid the write-frontier properties range over: source shapes
crossed with packages, five installed and one invented, so no property can pass
by knowing a particular library.
-}
module Test.TryFrontierGen (
    Lib (..),
    libGrid,
    Shape (..),
    Candidate (..),
    effectShapes,
    ioExpressionShapes,
    genIdent,
    mkCandidate,
    genCandidate,
    genEffectCandidate,
    genIOExpressionCandidate,
    fullGrid,
    queryCommands,
    effectfulCommands,
    genMetaSource,
    genImportRequest,
    moduleOf,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

-- | A package the grid draws from: five installed, one invented.
data Lib = Lib
    { libPkg :: Text
    , libModule :: Text
    , libQual :: Text
    , libPure :: Text
    , libIO :: Text
    }
    deriving (Show)

libGrid :: [Lib]
libGrid =
    [ Lib
        "containers"
        "Data.Map"
        "M"
        "M.size (M.fromList [(1 :: Int, 'x')])"
        "fmap (M.fromList . zip [1 :: Int ..] . lines) (readFile \"notes.txt\")"
    , Lib
        "text"
        "Data.Text"
        "Tx"
        "Tx.length (Tx.pack \"forty-two\")"
        "fmap Tx.pack (readFile \"notes.txt\")"
    , Lib
        "aeson"
        "Data.Aeson"
        "A"
        "A.encode (1 :: Int)"
        "fmap A.encode (readFile \"notes.json\")"
    , Lib
        "mtl"
        "Control.Monad.State"
        "MS"
        "MS.execState (MS.modify (+ (1 :: Int))) 0"
        "fmap length (readFile \"notes.txt\")"
    , Lib
        "bytestring"
        "Data.ByteString"
        "B"
        "B.length (B.pack [1, 2, 3])"
        "B.readFile \"notes.bin\""
    , Lib
        "zubu-core"
        "Zubu.Artefact"
        "Z"
        "Z.describe Z.emptyArtefact"
        "Z.load \"artefact.zub\""
    ]

{- | The source shapes a code cell can take. Named so a counterexample says
which spelling broke, not just which text.
-}
data Shape
    = SDecl
    | SDeclSig
    | SImportOnly
    | SPragmaImportDecl
    | SIOBind
    | SIOBindThenExpr
    | SDoBlock
    | SFinalExpr
    | SDeclThenExpr
    | STwoExprs
    | SUnsafeDecl
    | SBindOperator
    | SIOExpr
    deriving (Bounded, Enum, Eq, Show)

{- | Shapes whose text names an effect or a purity escape that the plan can see
without running anything. These are the ones containment must be forced for.
-}
effectShapes :: [Shape]
effectShapes = [SIOBind, SIOBindThenExpr, SUnsafeDecl]

{- | Shapes whose final expression is itself IO. The plan cannot tell them from
a pure expression, so only the session's own answer says whether anything ran.
-}
ioExpressionShapes :: [Shape]
ioExpressionShapes = [SDoBlock, SBindOperator, SIOExpr]

data Candidate = Candidate
    { candShape :: Shape
    , candLib :: Lib
    , candName :: Text
    , candHeader :: Bool
    , candSource :: Text
    }

instance Show Candidate where
    show c = show (candShape c) <> " / " <> T.unpack (candSource c)

genIdent :: Gen Text
genIdent = do
    n <- choose (1, 6) :: Gen Int
    c <- elements ['a' .. 'z']
    cs <- vectorOf (n - 1) (elements (['a' .. 'z'] <> ['0' .. '9']))
    let t = T.pack (c : cs)
    if t `elem` reserved then genIdent else pure t
  where
    reserved =
        [ "do"
        , "if"
        , "in"
        , "of"
        , "let"
        , "then"
        , "else"
        , "case"
        , "data"
        , "type"
        , "where"
        , "class"
        , "module"
        , "import"
        , "instance"
        , "newtype"
        ]

mkCandidate :: Shape -> Lib -> Text -> Bool -> Candidate
mkCandidate shape lib n header =
    Candidate shape lib n header (preamble <> body)
  where
    preamble
        | header =
            "-- cabal: build-depends: "
                <> libPkg lib
                <> "\nimport qualified "
                <> libModule lib
                <> " as "
                <> libQual lib
                <> "\n\n"
        | otherwise = ""
    body = case shape of
        SDecl -> n <> " = " <> libPure lib
        SDeclSig -> n <> " :: Int\n" <> n <> " = length (show (" <> libPure lib <> "))"
        SImportOnly -> "import " <> libModule lib
        SPragmaImportDecl ->
            "{-# LANGUAGE ScopedTypeVariables #-}\nimport "
                <> libModule lib
                <> "\n"
                <> n
                <> " = "
                <> libPure lib
        SIOBind -> n <> " <- " <> libIO lib
        SIOBindThenExpr -> n <> " <- " <> libIO lib <> "\nlength (show " <> n <> ")"
        SDoBlock ->
            "do\n    "
                <> n
                <> " <- "
                <> libIO lib
                <> "\n    print (length (show "
                <> n
                <> "))"
        SFinalExpr -> libPure lib
        SDeclThenExpr -> n <> " = " <> libPure lib <> "\n" <> n
        STwoExprs -> libPure lib <> "\nlength (show (" <> libPure lib <> "))"
        SUnsafeDecl -> n <> " = unsafePerformIO (" <> libIO lib <> ")"
        SBindOperator -> libIO lib <> " >>= pure"
        SIOExpr -> libIO lib

genCandidate :: Gen Candidate
genCandidate =
    mkCandidate
        <$> elements [minBound .. maxBound]
        <*> elements libGrid
        <*> genIdent
        <*> arbitrary

genEffectCandidate :: Gen Candidate
genEffectCandidate =
    mkCandidate
        <$> elements effectShapes
        <*> elements libGrid
        <*> genIdent
        <*> arbitrary

genIOExpressionCandidate :: Gen Candidate
genIOExpressionCandidate =
    mkCandidate
        <$> elements ioExpressionShapes
        <*> elements libGrid
        <*> genIdent
        <*> arbitrary

-- | The whole deterministic cross product, for the admissibility-gap metric.
fullGrid :: [Candidate]
fullGrid =
    [ mkCandidate shape lib "probe" header
    | shape <- [minBound .. maxBound]
    , lib <- libGrid
    , header <- [False, True]
    ]

-- | Meta-commands that only read the type environment.
queryCommands :: [Text]
queryCommands = [":type", ":t", ":kind", ":k", ":info", ":i"]

{- | Meta-commands that touch the filesystem, the loaded set or process state,
plus one the harness has never heard of — an unknown effect is an effect.
-}
effectfulCommands :: [Text]
effectfulCommands =
    [":!", ":load", ":set", ":cd", ":script", ":def", ":module", ":check_type"]

genSubject :: Gen Text
genSubject =
    oneof
        [ genIdent
        , elements ["Data.Map.insert", "(<>)", "Data.Aeson.encode", "Monad", "Zubu.load"]
        ]

genMetaSource :: [Text] -> Gen (Text, Text, [Text], Text)
genMetaSource commands = do
    cmd <- elements commands
    subject <- genSubject
    nImports <- choose (0, 3) :: Gen Int
    libs <- vectorOf nImports (elements libGrid)
    let imports = ["import " <> libModule l | l <- libs]
        src = T.unlines (imports <> [cmd <> " " <> subject])
    pure (cmd, subject, imports, src)

{- | How a caller might name a module: bare, qualified, aliased, already an
import line, or padded with whitespace.
-}
genImportRequest :: Gen [Text]
genImportRequest = do
    n <- choose (1, 4) :: Gen Int
    vectorOf n (elements libGrid >>= spelling)
  where
    spelling l =
        elements
            [ libModule l
            , "import " <> libModule l
            , "qualified " <> libModule l <> " as " <> libQual l
            , "  " <> libModule l <> "  "
            ]

moduleOf :: Text -> Text
moduleOf t = case T.words (T.replace "import " "" t) of
    (w : _) | w == "qualified" -> T.unwords (drop 1 (T.words (T.replace "import " "" t)))
    (w : _) -> w
    [] -> t
