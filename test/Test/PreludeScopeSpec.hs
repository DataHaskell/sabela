{-# LANGUAGE OverloadedStrings #-}

{- | Whatever the harness injects into a user's session has to survive
whatever that user's cells import. A legal committed cell whose import
shadowed a name the harness's own prelude used made the prelude itself
ambiguous and refused every later write in that notebook.
-}
module Test.PreludeScopeSpec (spec) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.Bridge (bridgePreamble, widgetPreamble)
import Sabela.Output (displayPrelude)
import Test.PreludeAmbient (AmbientModule (..), genIdent)
import Test.PreludeScope (
    capturableNames,
    capturedBy,
    definedNames,
    unqualifiedImports,
 )

spec :: Spec
spec = describe "the scope the harness injects into a user's session" $ do
    injectedSpec
    instrumentSpec

{- | Every source the harness puts into the session, with generated arguments
where it takes them, so the property is over the injection and not over one
call of it.
-}
data Injected = Injected Text Text

instance Show Injected where
    show (Injected name _) = T.unpack name

instance Arbitrary Injected where
    arbitrary =
        oneof
            [ pure (Injected "displayPrelude" displayPrelude)
            , Injected "widgetPreamble"
                <$> (widgetPreamble <$> arbitrary <*> genStringMap)
            , Injected "bridgePreamble" . bridgePreamble <$> genStringMap
            ]

genStringMap :: Gen (M.Map Text Text)
genStringMap = M.fromList <$> listOf ((,) <$> genIdent <*> genIdent)

{- | Names the harness itself binds at the prompt are not at risk: a GHCi
prompt binding shadows an import of the same name, whichever came first.
Names it looks up are, which is the whole of the measured loss.
-}
resident :: [Text]
resident = definedNames displayPrelude

injectedSpec :: Spec
injectedSpec = do
    prop "resolves no name the harness has not itself defined" $
        \(Injected _ src) ->
            filter (`notElem` resident) (capturableNames src) === []
    prop "adds no module to the shared scope unqualified" $
        \(Injected _ src) -> unqualifiedImports src === []
    prop "takes from an ambient module only names the session has bound" $
        \(Injected _ src) (AmbientModule name exports) ->
            counterexample (T.unpack ("import " <> name)) $
                capturedBy resident exports src === sortNub (boundHere src exports)

{- | The names of an ambient module the session's own prompt bindings already
own by the time this source runs: whatever the prelude bound, plus whatever
this source binds. Everything else the source must leave to the module.
-}
boundHere :: Text -> [Text] -> [Text]
boundHere src = filter (`elem` (resident <> definedNames src))

sortNub :: [Text] -> [Text]
sortNub = sort . nub

{- | The checker is not vacuous, and is not blind where it was: over arbitrary
identifiers it reports the ambient names a block leaves unqualified, counts an
instance's method bindings as ambient, and keeps one declaration's binders out
of another's.
-}
instrumentSpec :: Spec
instrumentSpec = describe "the checker itself" $ do
    prop "reports the ambient names a block leaves unqualified" $
        \(Distinct4 h f g x) ->
            capturableNames (callBlock h f g x) === sort [f, g]
    prop "reports nothing when the same block qualifies them" $
        \(Distinct4 h f g x) ->
            capturableNames (callBlock h ("Q." <> f) ("Q." <> g) x) === []
    prop "reports nothing when the block binds the names itself" $
        \(Distinct4 h f g x) ->
            capturableNames
                ( T.unlines
                    [ g <> " " <> x <> " = " <> x
                    , h <> " " <> x <> " = " <> g <> " " <> x
                    , f <> " " <> x <> " = " <> h <> " " <> x
                    ]
                )
                === []
    prop "counts a name an instance binds as a method as ambient" $
        \(Distinct4 cls m x y) ->
            capturableNames (instanceBlock cls m x y)
                `shouldContainName` m
    prop "keeps one declaration's binders out of another declaration" $
        \(Distinct4 h f x _) ->
            capturableNames
                ( T.unlines
                    [ h <> " " <> f <> " = " <> f
                    , x <> " = " <> f
                    ]
                )
                === [f]
    prop "puts every ambient name at risk under an unqualified import" $
        \(AmbientModule name exports) (Distinct4 h _ _ x) ->
            capturedBy [] exports (importBlock ("import " <> name) h x)
                === sort exports
    prop "puts none at risk once the same import is qualified" $
        \(AmbientModule name exports) (Distinct4 h _ _ x) ->
            capturedBy
                []
                (filter (/= h) exports)
                (importBlock ("import qualified " <> name <> " as Q") h x)
                === []
    prop "takes a name the block binds at the prompt, whatever the imports" $
        \(AmbientModule name _) (Distinct4 h f _ x) ->
            counterexample (T.unpack name) $
                capturedBy
                    []
                    [h, f]
                    (importBlock ("import qualified " <> name <> " as Q") h x)
                    === [h]
    prop "takes a name the session bound before it, which it never names" $
        \(AmbientModule name _) (Distinct4 h f _ x) ->
            counterexample (T.unpack name) $
                capturedBy
                    [f]
                    [f]
                    (importBlock ("import qualified " <> name <> " as Q") h x)
                    === [f]

shouldContainName :: [Text] -> Text -> Property
shouldContainName found n =
    counterexample (show found) (n `elem` found)

callBlock :: Text -> Text -> Text -> Text -> Text
callBlock h f g x =
    h <> " " <> x <> " = " <> f <> " (" <> g <> " " <> x <> ")\n"

{- | A declaration whose only binding of @m@ is as an instance method. The
method's own name in the body still resolves to the class, which is ambient,
so the block is at the mercy of whatever else exports it.
-}
instanceBlock :: Text -> Text -> Text -> Text -> Text
instanceBlock cls m x y =
    T.unlines
        [ "data " <> upper y <> " = " <> upper y
        , "instance " <> upper cls <> " " <> upper y <> " where"
        , "    " <> m <> " " <> x <> " = " <> m <> " " <> x
        ]

importBlock :: Text -> Text -> Text -> Text
importBlock importLine h x =
    T.unlines [importLine, h <> " " <> x <> " = " <> x]

upper :: Text -> Text
upper t = T.toUpper (T.take 1 t) <> T.drop 1 t

-- | Four identifiers no two of which are equal.
data Distinct4 = Distinct4 Text Text Text Text
    deriving (Show)

instance Arbitrary Distinct4 where
    arbitrary = do
        h <- genIdent
        f <- genIdent `suchThat` (/= h)
        g <- genIdent `suchThat` (`notElem` [h, f])
        x <- genIdent `suchThat` (`notElem` [h, f, g])
        pure (Distinct4 h f g x)
