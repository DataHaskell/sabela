{-# LANGUAGE OverloadedStrings #-}

{- | A4: a card is authority over what it states. A listing that reports the
query matched none of its entries is evidence of that, and a search cannot be
"found" on it. Stated over generated queries, modules and export rows.
-}
module Test.DiscoverCardMatchSpec (discoverCardMatchSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Affordance (withCardClashes)
import Siza.Agent.Discover.CardAuthority (cardAnswers)
import Siza.Agent.Discover.GoalEscalate (producersOfType)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.ProducerCard (producerCard, withProducerHint)
import Siza.Agent.Discover.Types (
    HackageInfo (..),
    NotebookEnv (..),
    SourceAnswer (..),
    StandingGoal (..),
    exportRow,
    exportRowName,
    okAnswer,
    seededBuiltins,
    statesDeclaration,
 )
import Test.DiscoverFixtures (field, hitsOf, stateOf)
import Test.DiscoverGen (genMatchedStampCard, genModulePair, genValueName)

discoverCardMatchSpec :: Spec
discoverCardMatchSpec = describe "a card answers, or says it did not (A4)" $ do
    predicateSpec
    envelopeSpec
    rowNameSpec
    exportRowTypeSpec

{- | A name a card row can be about: an entity's own spelling, with nothing of
the row's shape in it.
-}
genRowName :: Gen Text
genRowName = do
    c <- elements ['A' .. 'Z']
    n <- choose (2, 7)
    cs <- vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    pure (T.pack (c : cs))

{- | Wave-5 item 2: an export row states a signature or a whole declaration,
and one reader names the entity for both. Reading the row's head made the
keyword the entity, so every consumer compared the keyword to a listing.
-}
rowNameSpec :: Spec
rowNameSpec = describe "the entity an export row names" $ do
    prop "a signature row is named by its subject" $
        forAll ((,) <$> genRowName <*> genRowName) $ \(n, t) ->
            exportRowName (n <> " :: " <> t) === n
    prop "a declaration row is named by its subject, not by its keyword" $
        forAll ((,) <$> elements rowKeywords <*> genRowName) $ \(kw, n) ->
            counterexample (show (kw, n)) $
                conjoin
                    [ exportRowName (kw <> " " <> n <> " = Int") === n
                    , exportRowName (kw <> " " <> n <> " = Int")
                        `shouldNotBeQ` kw
                    ]
    prop "the name a row states is a name, never a shape word" $
        forAll ((,) <$> elements rowKeywords <*> genRowName) $ \(kw, n) ->
            let rows = [n <> " :: Int", kw <> " " <> n <> " = Int"]
             in map exportRowName rows === [n, n]
    prop "a declaration row stating a signature is named by its subject" $
        forAll ((,) <$> elements rowKeywords <*> genRowName) $ \(kw, n) ->
            exportRowName (kw <> " " <> n <> " :: Int") === n
    prop "a qualified row is named as the listing it is checked against is" $
        forAll ((,) . fst <$> genModulePair <*> genRowName) $ \(q, n) ->
            exportRowName (q <> "." <> n <> " :: Int") === n

{- | The keywords a card row can be headed by, each naming its subject after
the keyword.
-}
rowKeywords :: [Text]
rowKeywords = ["type", "data", "newtype", "class", "pattern"]

{- | The export rows a card publishes for the exports given, read back off the
card the way a caller reads them.
-}
producerExports :: Text -> [(Text, Text)] -> [Text]
producerExports goal es =
    case producerCard (StandingGoal goal "useIt" "pkg") es (length es) of
        Just v ->
            [ s
            | Just (Array rows) <- [field "card" v >>= field "exports"]
            , String s <- toList rows
            ]
        Nothing -> []

-- | One index answer stating the type text given for one entity.
indexAnswer :: Text -> Text -> Value
indexAnswer n ty = object ["hits" .= [object ["package" .= pkg, "api" .= [row]]]]
  where
    pkg = "pkg" :: Text
    row = object ["name" .= n, "type" .= ty, "module" .= ("M.Card" :: Text)]

{- | The clash a card states for a notebook already binding the name given.
The reader that finds it must see the entity through the row's own shape.
-}
clashesOf :: Text -> [Text] -> Text
clashesOf bound rows =
    case withCardClashes (NotebookEnv [] [] [] [bound] [] []) card of
        Object c | Just (String s) <- KM.lookup "clashesInScope" c -> s
        _ -> ""
  where
    card = object ["module" .= ("M.Card" :: Text), "exports" .= rows]

plainUse :: Text
plainUse = "import M.Card (useIt)"

{- | The top hit's @use@ after the harness offers a value for an argument that
hit cannot make: a producer is a value, so a declaration is not one.
-}
usedAfterHint :: Text -> (Text, Text) -> Text
usedAfterHint argTy (n, ty) =
    case withProducerHint (object ["hits" .= [top, sibling]]) of
        Object o
            | Just (Array hs) <- KM.lookup "hits" o
            , (Object t : _) <- toList hs
            , Just (String u) <- KM.lookup "use" t ->
                u
        _ -> ""
  where
    top =
        object
            [ "name" .= ("useIt" :: Text)
            , "matchKind" .= ("exact" :: Text)
            , "use" .= plainUse
            , "package" .= ("pkg" :: Text)
            , "type" .= (argTy <> " -> " <> argTy)
            ]
    sibling = object ["name" .= n, "package" .= ("pkg" :: Text), "type" .= ty]

-- | The text an export row introduces with @::@, for each row that has one.
announcedTypes :: [Text] -> [Text]
announcedTypes rows =
    [ T.strip (T.drop (T.length sigMark) rest)
    | r <- rows
    , let rest = snd (T.breakOn sigMark r)
    , not (T.null rest)
    ]
  where
    sigMark = " :: "

{- | Wave-5 item 1: an export row states whatever type text its source gave,
and a declaration is not a signature. Introducing one with @::@ announces a
type no source stated, under the entity's own name.
-}
exportRowTypeSpec :: Spec
exportRowTypeSpec = describe "the type an export row announces" $ do
    prop "a declaration is stated as itself, never announced after ::"
        $ forAll
            ((,,,) <$> genRowName <*> genRowName <*> genRowName <*> elements rowKeywords)
        $ \(g, n, a, kw) ->
            let decl = kw <> " " <> n <> " = " <> a <> " -> " <> a
                rows = producerExports g [(n, decl)]
             in counterexample (show rows) $
                    conjoin
                        [ rows === [decl]
                        , filter statesDeclaration (announcedTypes rows) === []
                        ]
    prop "a signature row still announces the type it states" $
        forAll ((,,) <$> genRowName <*> genRowName <*> genRowName) $
            \(g, n, a) ->
                let sig = a <> " -> " <> a
                 in producerExports g [(n, sig)] === [n <> " :: " <> sig]
    prop "a row whose source stated no type announces none" $
        forAll ((,) <$> genRowName <*> genRowName) $ \(g, n) ->
            producerExports g [(n, "")] === [n]
    prop "a declaration is not a producer of the goal it expands to"
        $ forAll
            ((,,,) <$> genRowName <*> genRowName <*> genRowName <*> elements rowKeywords)
        $ \(g, n, a, kw) ->
            let sig = a <> " -> " <> g
                decl = kw <> " " <> n <> " = " <> sig
             in counterexample (show (decl, sig)) $
                    conjoin
                        [ producersOfType g (indexAnswer n decl) === []
                        , producersOfType g (indexAnswer n sig)
                            === [(n, sig, "M.Card")]
                        ]
    prop "a name already in scope clashes whatever shape its row states" $
        forAll ((,,) <$> genRowName <*> genRowName <*> elements rowKeywords) $
            \(n, a, kw) ->
                let rows = [exportRow n (kw <> " " <> n <> " = " <> a)]
                 in clashesOf n rows === n
    prop "a declaration is never offered as the value an argument needs" $
        forAll ((,,) <$> genRowName <*> genRowName <*> elements rowKeywords) $
            \(a, n, kw) ->
                let decl = kw <> " " <> n <> " = " <> a
                 in counterexample (T.unpack (usedAfterHint a (n, decl))) $
                        conjoin
                            [ usedAfterHint a (n, decl) === plainUse
                            , usedAfterHint a (n, a)
                                === plainUse <> "; producer: " <> n <> " :: " <> a
                            ]
    prop "every row a card states is named for the export it was built from"
        $ forAll
            ((,,,) <$> genRowName <*> genRowName <*> genRowName <*> elements rowKeywords)
        $ \(g, n, a, kw) ->
            let tys = ["", a <> " -> " <> a, kw <> " " <> n <> " = " <> a]
                rows = concat [producerExports g [(n, ty)] | ty <- tys]
             in counterexample (show rows) $
                    map exportRowName rows === [n, n, n]

shouldNotBeQ :: (Eq a, Show a) => a -> a -> Property
shouldNotBeQ x y = counterexample (show x <> " == " <> show y) (x /= y)

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

-- | The envelope a card-only answer produces: no hits, so only the card speaks.
cardEnvelope :: Text -> Value -> Value
cardEnvelope q c =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [(okAnswer "session" []){saCard = Just c}]
        hkT

narrowOf :: Value -> Text
narrowOf v = case field "narrow" v of
    Just (String s) -> s
    _ -> ""

predicateSpec :: Spec
predicateSpec = describe "the predicate reads the card's own rows" $ do
    prop "a denial stands only while the rows do not name the query" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            counterexample (show card) $
                cardAnswers (interpret envT q) card === mentions
    prop "a card that denies nothing answers" $
        forAll ((,) <$> genValueName <*> genModulePair) $
            \(q, (m, _)) ->
                property (cardAnswers (interpret envT q) (plainCard m))

envelopeSpec :: Spec
envelopeSpec = describe "the state a card is allowed to set" $ do
    prop "a card that denies the query, with no hits, is not found" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            let v = cardEnvelope q card
             in counterexample (show v) $
                    conjoin
                        [ property (null (hitsOf v))
                        , stateOf v === (if mentions then "found" else "not_found")
                        ]
    {- Wave-5 item 6: wave 3 disclosed the lie, wave 5 stops paying for it. A
    card that does not answer is absent, and its absence is a stated count so
    the payload never reads as "the harness enumerated nothing". -}
    prop "a card that does not answer is absent, and the omission is counted" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            let v = cardEnvelope q card
             in counterexample (show v) $
                    conjoin
                        [ isJust (field "card" v) === mentions
                        , T.isInfixOf "1 card omitted" (narrowOf v) === not mentions
                        ]
    prop "a card with nothing to deny is carried, and nothing is counted out" $
        forAll ((,) <$> genValueName <*> genModulePair) $
            \(q, (m, _)) ->
                let v = cardEnvelope q (plainCard m)
                 in counterexample (show v) $
                        conjoin
                            [ stateOf v === "found"
                            , property (isJust (field "card" v))
                            , property (not (T.isInfixOf "card omitted" (narrowOf v)))
                            ]

plainCard :: Text -> Value
plainCard m =
    object
        [ "module" .= m
        , "status" .= ("ok" :: Text)
        , "exports" .= (["someExport :: Int"] :: [Text])
        ]
