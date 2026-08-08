{-# LANGUAGE OverloadedStrings #-}

module Test.HoogleResolveSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    rankResolve,
    rankResolveTopK,
 )
import Sabela.AI.ImportRepair (addImport)
import Sabela.Diagnose (notInScopeName)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

jsonBlob :: Text
jsonBlob =
    T.concat
        [ "[{\"item\":\"runConduit :: Monad m => ConduitT () Void m r -> m r\""
        , ",\"module\":{\"name\":\"Conduit\"}"
        , ",\"package\":{\"name\":\"conduit\"}},"
        , "{\"item\":\"runConduitRes :: a\""
        , ",\"module\":{\"name\":\"Conduit\"}"
        , ",\"package\":{\"name\":\"conduit\"}},"
        , "{\"item\":\"runConduit :: a\""
        , ",\"module\":{\"name\":\"Data.Conduit.Internal\"}"
        , ",\"package\":{\"name\":\"conduit\"}}]"
        ]

jsonlBlob :: Text
jsonlBlob =
    T.unlines
        [ "{\"item\":\"runConduit :: Monad m => ConduitT () Void m r -> m r\",\"module\":{\"name\":\"Conduit\"},\"package\":{\"name\":\"conduit\"}}"
        , "{\"item\":\"runConduitRes :: a\",\"module\":{\"name\":\"Conduit\"},\"package\":{\"name\":\"conduit\"}}"
        , "{\"item\":\"runConduit :: a\",\"module\":{\"name\":\"Data.Conduit.Internal\"},\"package\":{\"name\":\"conduit\"}}"
        ]

ambiguousBlob :: Text
ambiguousBlob =
    T.concat
        [ "[{\"item\":\"decode :: FromJSON a => ByteString -> Maybe a\""
        , ",\"module\":{\"name\":\"Data.Aeson\"}"
        , ",\"package\":{\"name\":\"aeson\"}},"
        , "{\"item\":\"decode :: a\""
        , ",\"module\":{\"name\":\"Network.Pusher.Internal\"}"
        , ",\"package\":{\"name\":\"pusher-http-haskell\"}},"
        , "{\"item\":\"decode :: a\""
        , ",\"module\":{\"name\":\"Network.Pusher\"}"
        , ",\"package\":{\"name\":\"pusher-http-haskell\"}}]"
        ]

spec :: Spec
spec = describe "Sabela.AI.HoogleResolve" $ do
    describe "a type hit is indexed under its own name" $ do
        let typeBlob =
                "[{\"item\":\"data TBQueue a\",\
                \\"module\":{\"name\":\"Control.Concurrent.STM.TBQueue\"},\
                \\"package\":{\"name\":\"stm\"},\"docs\":\"\"},\
                \{\"item\":\"type POSIXTime = NominalDiffTime\",\
                \\"module\":{\"name\":\"Data.Time.Clock.POSIX\"},\
                \\"package\":{\"name\":\"time\"},\"docs\":\"\"},\
                \{\"item\":\"newtype Down a\",\
                \\"module\":{\"name\":\"Data.Ord\"},\
                \\"package\":{\"name\":\"base\"},\"docs\":\"\"},\
                \{\"item\":\"class Ord a\",\
                \\"module\":{\"name\":\"Data.Ord\"},\
                \\"package\":{\"name\":\"base\"},\"docs\":\"\"},\
                \{\"item\":\"atomically :: STM a -> IO a\",\
                \\"module\":{\"name\":\"Control.Monad.STM\"},\
                \\"package\":{\"name\":\"stm\"},\"docs\":\"\"}]"
        it "names a data/type/newtype/class hit by the type, not the keyword" $
            map hhName (parseHoogleBlob typeBlob)
                `shouldBe` ["TBQueue", "POSIXTime", "Down", "Ord", "atomically"]
        it "keeps each type's own module, which is what an import needs" $
            take 2 (map hhModule (parseHoogleBlob typeBlob))
                `shouldBe` ["Control.Concurrent.STM.TBQueue", "Data.Time.Clock.POSIX"]
        it "a type name resolves to its package" $
            map hhPackage (parseHoogleBlob typeBlob)
                `shouldBe` ["stm", "time", "base", "base", "stm"]

    statedTypeSpec

    describe "parseHoogleBlob" $ do
        it "parses a JSON array into name/package/module hits" $ do
            let hits = parseHoogleBlob jsonBlob
            map hhName hits `shouldBe` ["runConduit", "runConduitRes", "runConduit"]
            map hhPackage hits `shouldBe` ["conduit", "conduit", "conduit"]
            map hhModule hits
                `shouldBe` ["Conduit", "Conduit", "Data.Conduit.Internal"]

        it "parses JSONL (one object per line) the same way" $
            parseHoogleBlob jsonlBlob `shouldBe` parseHoogleBlob jsonBlob

        it "is empty on a hoogle no-database error line" $
            parseHoogleBlob "Error, database does not exist (run 'hoogle generate' first)"
                `shouldBe` []

        it "is empty on malformed JSON" $
            parseHoogleBlob "{not json" `shouldBe` []

    describe "rankResolve" $ do
        it "picks the exact-name hit and its (package, module), dropping .Internal" $
            rankResolve "runConduit" (parseHoogleBlob jsonBlob)
                `shouldBe` Just ("conduit", "Conduit")

        it "is Nothing when no hit's name equals the query exactly" $
            rankResolve "noSuchName" (parseHoogleBlob jsonBlob)
                `shouldBe` Nothing

        it "is Nothing on no hits" $
            rankResolve "runConduit" [] `shouldBe` Nothing

        it "is Nothing when the only exact hit is an .Internal module" $
            rankResolve
                "foo"
                [HoogleHit "foo" "pkg" "Data.Foo.Internal" "" "" ""]
                `shouldBe` Nothing

    describe "addImport" $ do
        it "inserts an import after existing imports" $
            addImport "Conduit" "import Data.Text\nx = 1"
                `shouldBe` "import Data.Text\nimport Conduit\nx = 1"

        it "is a no-op when the import is already present" $
            addImport "Conduit" "import Conduit\nx = 1"
                `shouldBe` "import Conduit\nx = 1"

        it "is idempotent across repeated calls" $
            addImport "Conduit" (addImport "Conduit" "import Data.Text\nx = 1")
                `shouldBe` addImport "Conduit" "import Data.Text\nx = 1"

        it "inserts after the -- cabal: line when the cell has no imports" $
            addImport "Conduit" "-- cabal: build-depends: conduit\nx = 1"
                `shouldBe` "-- cabal: build-depends: conduit\nimport Conduit\nx = 1"

        it "leaves a qualified import of the same module alone" $
            addImport "Conduit" "import qualified Conduit as C\nx = 1"
                `shouldBe` "import qualified Conduit as C\nx = 1"

    describe "rankResolveTopK" $ do
        it "returns an ordered shortlist of exact-name candidates" $
            rankResolveTopK 3 "runConduit" Nothing (parseHoogleBlob jsonBlob)
                `shouldBe` [("conduit", "Conduit")]

        it "drops .Internal modules from the shortlist" $
            map snd (rankResolveTopK 3 "decode" Nothing (parseHoogleBlob ambiguousBlob))
                `shouldNotContain` ["Network.Pusher.Internal"]

        it "prefers the shorter module path when nothing else separates two hits" $
            take 1 (rankResolveTopK 3 "decode" Nothing (parseHoogleBlob ambiguousBlob))
                `shouldBe` [("aeson", "Data.Aeson")]

        it "is empty when no hit's name matches exactly" $
            rankResolveTopK 3 "noSuchName" Nothing (parseHoogleBlob ambiguousBlob)
                `shouldBe` []

        it "caps the shortlist at K" $
            length (rankResolveTopK 1 "decode" Nothing (parseHoogleBlob ambiguousBlob))
                `shouldBe` 1

    describe "notInScopeName feeds the resolver" $
        it "extracts the name the resolver then queries" $
            notInScopeName "Variable not in scope: runConduit :: ConduitT a b m r"
                `shouldBe` Just "runConduit"

    describe "unionfind-point (G2 hard rule 2 — never resolve out-of-scope)" $ do
        let unionfindBlob =
                T.concat
                    [ "[{\"item\":\"Point\""
                    , ",\"module\":{\"name\":\"GHC.Data.UnionFind\"}"
                    , ",\"package\":{\"name\":\"ghc\"}}]"
                    ]
            mixedBlob =
                T.concat
                    [ "[{\"item\":\"Point\""
                    , ",\"module\":{\"name\":\"GHC.Data.UnionFind\"}"
                    , ",\"package\":{\"name\":\"ghc\"}},"
                    , "{\"item\":\"Point\""
                    , ",\"module\":{\"name\":\"Sabela.Notebook.Picture\"}"
                    , ",\"package\":{\"name\":\"sabela-notebook\"}}]"
                    ]
        it "never proposes a `ghc`-package hit, even as the only exact match" $
            rankResolveTopK 3 "Point" Nothing (parseHoogleBlob unionfindBlob)
                `shouldBe` []
        it "drops the `ghc` hit but keeps a legitimate in-scope alternative" $
            rankResolveTopK 3 "Point" Nothing (parseHoogleBlob mixedBlob)
                `shouldBe` [("sabela-notebook", "Sabela.Notebook.Picture")]

{- | An identifier the index could carry in a declaration: a type's own name,
or a name its expansion is built from.
-}
genIdent :: Gen Text
genIdent = do
    c <- elements ['A' .. 'Z']
    n <- choose (2, 7)
    cs <- vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    pure (T.pack (c : cs))

-- | One index row carrying the item text given, as hoogle publishes it.
itemBlob :: Text -> Text
itemBlob item =
    T.concat
        [ "[{\"item\":\""
        , item
        , "\",\"module\":{\"name\":\"M.Idx\"}"
        , ",\"package\":{\"name\":\"idx\"}}]"
        ]

statedOf :: Text -> [Text]
statedOf = map hhType . parseHoogleBlob . itemBlob

{- | Wave-5 item 1: the index states a synonym's expansion, the only place it
carries one, and nothing for a declaration it left unexpanded — a head restated
as a type is the entity's own name dressed as its expansion.
-}
statedTypeSpec :: Spec
statedTypeSpec = describe "the type an index item states" $ do
    prop "a synonym item states its whole declaration" $
        forAll ((,,) <$> genIdent <*> genIdent <*> genIdent) $ \(n, a, b) ->
            let decl = "type " <> n <> " = (" <> a <> ", " <> b <> ")"
             in statedOf decl === [decl]
    prop "a declaration the index left unexpanded states no type" $
        forAll ((,) <$> elements declHeads <*> genIdent) $ \(kw, n) ->
            statedOf (kw <> " " <> n <> " a") === [""]
    prop "a head that ascribes a kind to a binder states no type" $
        forAll ((,,) <$> elements declHeads <*> genIdent <*> genKind) $
            \(kw, n, k) ->
                statedOf (kw <> " " <> n <> " (f :: " <> k <> ")") === [""]
    prop "a declaration whose own head introduces a signature states it" $
        forAll ((,,) <$> genIdent <*> genIdent <*> genIdent) $ \(n, a, b) ->
            let sig = a <> " -> " <> b
             in statedOf ("pattern " <> n <> " :: " <> sig) === [sig]
    prop "a signature item still states the type after ::" $
        forAll ((,,) <$> genIdent <*> genIdent <*> genIdent) $ \(n, a, b) ->
            statedOf (n <> " :: " <> a <> " -> " <> b)
                === [a <> " -> " <> b]
    prop "a head's context is neither the entity nor its expansion" $
        forAll ((,,) <$> elements declHeads <*> genIdent <*> genContext) $
            \(kw, n, ctx) ->
                let hits = parseHoogleBlob (itemBlob (kw <> " " <> ctx <> n <> " a"))
                 in counterexample (show hits) $
                        conjoin [map hhName hits === [n], map hhType hits === [""]]
    prop "the name an item is indexed under never becomes its type" $
        forAll ((,) <$> elements declHeads <*> genIdent) $ \(kw, n) ->
            let hits = parseHoogleBlob (itemBlob (kw <> " " <> n <> " a"))
             in counterexample (show hits) $
                    conjoin
                        [ map hhName hits === [n]
                        , map hhType hits === [""]
                        ]

declHeads :: [Text]
declHeads = ["type", "data", "newtype", "class", "pattern"]

-- | A kind an index item can ascribe to one of its head's binders.
genKind :: Gen Text
genKind = elements ["Type", "Type -> Type", "k", "[Type]"]

{- | A context an index head can carry before its own name. Its @=>@ is not
the @=@ of a definition, and its first word is not the entity.
-}
genContext :: Gen Text
genContext = do
    c1 <- genIdent
    c2 <- genIdent
    elements
        [ c1 <> " a => "
        , "(" <> c1 <> " a, " <> c2 <> " a) => "
        , "(" <> c1 <> " a) => "
        ]
