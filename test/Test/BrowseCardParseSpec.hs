{-# LANGUAGE OverloadedStrings #-}

{- | C2-5a: a @:browse@ listing must become lexical entities, never fragments
of a wrapped signature, and the card head must answer the query that asked.
-}
module Test.BrowseCardParseSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.BrowseCard (browseCardFor)

-- generators ---------------------------------------------------------------

data Decl
    = ValueDecl Text Text
    | TypeDecl Text Text
    | ClassDecl Text [(Text, Text)]
    | RecordDecl Text [(Text, Text)]
    deriving (Eq, Show)

{- | Every name the declaration puts in scope. A class declares its methods and
a record declares its selectors; both are exports of the module.
-}
declNames :: Decl -> [Text]
declNames (ValueDecl n _) = [n]
declNames (TypeDecl _ n) = [n]
declNames (ClassDecl n ms) = n : map fst ms
declNames (RecordDecl n fs) = n : map fst fs

{- | GHCi's own layout: a wrapped declaration continues on an indented line,
and a class body and a record body are written as continuations of their head.
-}
renderDecl :: Int -> Int -> Decl -> [Text]
renderDecl col indent d = case d of
    ValueDecl n s -> wrapAt col indent (n <> " :: " <> s)
    TypeDecl kw n -> wrapAt col indent (kw <> " " <> n)
    ClassDecl n ms ->
        ("class " <> n <> " a where")
            : concatMap (indented . sigLine) ms
    RecordDecl n fs ->
        ("data " <> n)
            : indented ("= " <> n <> " {" <> fieldBody fs <> "}")
  where
    sigLine (n, t) = n <> " :: " <> t
    fieldBody fs = T.intercalate ", " (map sigLine fs)
    indented = map ("  " <>) . wrapAt col indent

valueNames :: [Text]
valueNames =
    [ "insertWith"
    , "alterF"
    , "splitOn"
    , "decodeStrict'"
    , "foldl'"
    , "mapAccumL"
    , "unionWith"
    , "partitionEithers"
    , "encodeUtf8"
    , "runReaderT"
    , "evalStateT"
    , "fromListWith"
    , "(<>)"
    , "(.:?)"
    , "(<|>)"
    ]

typeNames :: [Text]
typeNames =
    ["Value", "Parser", "Seq", "IntMap", "Builder", "ReadOptions", "Series"]

classNames :: [Text]
classNames = ["Renderable", "Sizable", "Groupable", "Zippy", "Foldy"]

methodNames :: [Text]
methodNames = ["renderOne", "measure", "groupOf", "zipKeys", "foldPart"]

recordNames :: [Text]
recordNames = ["Speed", "Origin", "Layout", "Budget", "Palette"]

{- | No member name is a camel-hump prefix of a head name: a selector's type
names its own record, so @originPort@ of @Origin@ would tie with @Origin@ on
coverage and the ranking of that tie is not what this generator is about.
-}
fieldNames :: [Text]
fieldNames = ["knots", "gustsPerHour", "berthCount", "labelPad", "swatches"]

genSig :: Gen Text
genSig =
    elements
        [ "(Ord k, Show a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
        , "Text -> Text -> [Text]"
        , "(Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)"
        , "Monad m => r -> ReaderT r m a -> m a"
        , "forall a b. (a -> b -> a) -> a -> [b] -> a"
        ]

genMemberSig :: Gen Text
genMemberSig =
    elements
        [ "a -> Int"
        , "[(Int, Int)]"
        , "Maybe (Map k a) -> a"
        , "Int"
        , "a -> a -> [a]"
        ]

{- | A synthetic module listing: n declarations with distinct names, each
hard-wrapped at a random column with an indented continuation, exactly as
GHCi renders @:browse@.
-}
data Listing = Listing
    { lDecls :: [Decl]
    , lRaw :: Text
    }

instance Show Listing where
    show l = T.unpack (lRaw l)

genListing :: Gen Listing
genListing = genListingOf (4, 30)

-- | Small enough that the whole listing fits inside the card's export cap.
genUncappedListing :: Gen Listing
genUncappedListing = genListingOf (2, 6)

genListingOf :: (Int, Int) -> Gen Listing
genListingOf kRange = do
    k <- choose kRange
    vs <- shuffle valueNames
    ts <- shuffle typeNames
    sigs <- vectorOf k genSig
    kws <- vectorOf k (elements ["data", "newtype", "class"])
    classes <- genMembered ClassDecl classNames methodNames
    records <- genMembered RecordDecl recordNames fieldNames
    let values = [ValueDecl n s | (n, s) <- zip vs sigs]
        types = [TypeDecl kw n | (kw, n) <- zip kws ts]
        decls = take k (interleave (interleave classes records) (interleave values types))
    col <- choose (18, 76)
    indent <- choose (2, 6)
    pure (Listing decls (T.unlines (concatMap (renderDecl col indent) decls)))
  where
    interleave (a : as) (b : bs) = a : b : interleave as bs
    interleave as bs = as ++ bs

{- | Declarations that carry members: each head takes a disjoint slice of the
member pool, so every generated name stays unique across the listing.
-}
genMembered ::
    (Text -> [(Text, Text)] -> Decl) -> [Text] -> [Text] -> Gen [Decl]
genMembered mk heads members = do
    hs <- shuffle heads
    ms <- shuffle members
    sizes <- vectorOf (length hs) (choose (1, 2 :: Int))
    tys <- vectorOf (length members) genMemberSig
    let slice _ [] = []
        slice (n : ns) rest = take n rest : slice ns (drop n rest)
        slice [] _ = []
    pure
        [ mk h grp
        | (h, grp) <- zip hs (slice sizes (zip ms tys))
        , not (null grp)
        ]

wrapAt :: Int -> Int -> Text -> [Text]
wrapAt col indent t = case go (T.words t) of
    [] -> []
    (l : ls) -> l : map (T.replicate indent " " <>) ls
  where
    go [] = []
    go (w : ws) = let (l, rest) = fill w ws in l : go rest
    fill w ws
        | (nx : more) <- ws
        , T.length w + 1 + T.length nx <= col =
            fill (w <> " " <> nx) more
        | otherwise = (w, ws)

-- the test's own notion of a lexical entity -------------------------------

entityName :: Text -> Text
entityName l
    | (kw : n : _) <- T.words l
    , kw `elem` (["type", "data", "newtype", "class", "pattern"] :: [Text]) =
        n
    | otherwise = T.strip (fst (T.breakOn " :: " l))

lexical :: Text -> Bool
lexical t = not (T.null t) && (operatorish t || qualifiedIdent t)
  where
    operatorish s =
        "(" `T.isPrefixOf` s
            && ")" `T.isSuffixOf` s
            && T.length s > 2
            && T.all (`elem` opChars) (T.init (T.drop 1 s))
    qualifiedIdent s =
        not (T.null s) && all identish (T.splitOn "." s)
    identish s =
        not (T.null s)
            && maybe False (\(c, _) -> isAlpha c || c == '_') (T.uncons s)
            && T.all (\c -> isAlphaNum c || c `elem` ("_'" :: String)) s
    opChars = "!#$%&*+./<=>?@\\^|-~:" :: String

exportsOf :: Value -> [Text]
exportsOf v = case field "exports" v of
    Just (Array es) -> [t | String t <- foldr (:) [] es]
    _ -> []

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

-- properties ---------------------------------------------------------------

spec :: Spec
spec = describe "browse listings parse to lexical entities (C2-5a)" $ do
    prop "every export names a lexical entity, never a signature fragment" $
        forAll genListing $ \l ->
            let es = exportsOf (browseCardFor Nothing "Syn.Mod" (lRaw l))
             in conjoin
                    [ counterexample (T.unpack ("not an entity: " <> e)) $
                        property (lexical (entityName e))
                    | e <- es
                    ]

    prop "total counts declared entities, not rendered lines" $
        forAll genListing $ \l ->
            intField "total" (browseCardFor Nothing "Syn.Mod" (lRaw l))
                === length (concatMap declNames (lDecls l))

    prop "a class method and a record selector are exports of their own" $
        forAll genUncappedListing $ \l ->
            let named =
                    map entityName (exportsOf (browseCardFor Nothing "Syn.Mod" (lRaw l)))
             in conjoin
                    [ counterexample
                        (T.unpack (n <> " not among " <> T.pack (show named)))
                        (property (n `elem` named))
                    | d <- lDecls l
                    , n <- declNames d
                    ]

    prop "the queried declaration is in the head of the card" $
        forAll genListing $ \l ->
            conjoin
                [ counterexample (T.unpack (n <> " not in head of card")) $
                    property (n `elem` map entityName (take 8 (headFor l n)))
                | n <- queryableNames l
                ]

    prop "distinct queries against one module get distinct heads" $
        forAll genListing $ \l ->
            let heads = [(n, take 1 (headFor l n)) | n <- queryableNames l]
                shared h = length [() | (_, x) <- heads, x == h] > 1
             in counterexample (show (filter (shared . snd) heads)) $
                    length (nub (map snd heads)) === length heads

    it "an unparsable row is disclosed, never emitted as a name" $ do
        let raw = T.unlines ["gust :: Int -> Wind", "~ Bool) =>", "-> Expr b"]
            card = browseCardFor Nothing "Syn.Mod" raw
        exportsOf card `shouldBe` ["gust :: Int -> Wind"]
        intField "total" card `shouldBe` 1
        intField "unparsed" card `shouldBe` 2

headFor :: Listing -> Text -> [Text]
headFor l n = exportsOf (browseCardFor (Just n) "Syn.Mod" (lRaw l))

{- | The names a caller could put in the query field. Operator sections are
excluded: the card's need parser tokenises alphanumerically, so @(<|>)@ carries
no term to rank on — a Search.Need gap, not a parse one.
-}
queryableNames :: Listing -> [Text]
queryableNames l =
    filter (T.all isAlphaNum) (nub (concatMap declNames (lDecls l)))
