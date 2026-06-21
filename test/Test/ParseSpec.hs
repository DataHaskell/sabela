{-# LANGUAGE OverloadedStrings #-}

{- | Direct unit tests for 'Sabela.Parse.cellNames'.

These specs are written against the AST-based extractor in
@Sabela.Parse@. They are deliberately stronger than the @Topo@
heuristic-era specs: most cases assert the **exact** @defs@ and @uses@
sets so it's obvious what feeds the reactivity DAG, instead of just
@shouldContain@ checks.
-}
module Test.ParseSpec (spec) where

import qualified Data.Set as S
import Sabela.Parse (CellSymbols (..), cellNames, cellSymbols)
import Test.Hspec

spec :: Spec
spec = describe "Sabela.Parse.cellNames" $ do
    -- ----------------------------------------------------------------
    -- Top-level def extraction: exact sets, no surprises
    -- ----------------------------------------------------------------
    describe "exact (defs, uses) for a notebook cell" $ do
        it "value binding: defs={x}, uses={}" $
            cellNames "x = 1"
                `shouldBe` (S.fromList ["x"], S.empty)

        it "function with one param: defs={f}, uses={} (param does NOT escape)" $
            cellNames "f x = x + 1"
                `shouldBe` (S.fromList ["f"], S.fromList ["+"])

        it "function calling external: defs={main}, uses={print, message}" $
            cellNames "main = print message"
                `shouldBe` (S.fromList ["main"], S.fromList ["print", "message"])

        it "data decl: defs={Foo, Bar, Baz}, uses={}" $
            cellNames "data Foo = Bar | Baz"
                `shouldBe` (S.fromList ["Foo", "Bar", "Baz"], S.empty)

        it "newtype: defs={Wrapper, Wrap}, uses={}" $
            cellNames "newtype Wrapper = Wrap Int"
                `shouldBe` (S.fromList ["Wrapper", "Wrap"], S.empty)

        it "type synonym: defs={Name}, uses={}" $
            cellNames "type Name = String"
                `shouldBe` (S.fromList ["Name"], S.empty)

        it "class with methods: defs={MyShow, myShow}, uses={}" $
            cellNames "class MyShow a where\n  myShow :: a -> String"
                `shouldBe` (S.fromList ["MyShow", "myShow"], S.empty)

    -- Instances own no top-level name, but they must still join the DAG:
    -- depend on the class they implement (via uses) and let method consumers
    -- depend on them (via provides). See 'Sabela.Topo.buildDepGraph'.
    describe "typeclass instances feed the reactivity DAG" $ do
        it "instance: no defs, but uses the class name" $ do
            let (defs, uses) =
                    cellNames "instance Rand Int where\n  rand' x = x * 2"
            defs `shouldBe` S.empty
            S.member "Rand" uses `shouldBe` True

        it "instance: method binders surface as provides, not defs" $ do
            let syms = cellSymbols "instance Rand Int where\n  rand' x = x * 2"
            csDefs syms `shouldBe` S.empty
            csProvides syms `shouldBe` S.fromList ["rand'"]

        it "class: method names surface as class methods" $ do
            let syms = cellSymbols "class Rand a where\n  rand' :: a -> a"
            csClassMethods syms `shouldBe` S.fromList ["rand'"]

    -- ----------------------------------------------------------------
    -- The user's headline complaint: function params must NOT leak
    -- across cells. Two cells with `f x = ...` and `g x = ...` should
    -- have entirely disjoint defs/uses; no edge between them.
    -- ----------------------------------------------------------------
    describe "function-scoped params do not leak across cells" $ do
        it "cell A: f x = x + 1 has uses={+} only, no x" $ do
            let (defs, uses) = cellNames "f x = x + 1"
            defs `shouldBe` S.fromList ["f"]
            S.member "x" uses `shouldBe` False

        it "cell B: g x = x * 2 has uses={*} only, no x" $ do
            let (defs, uses) = cellNames "g x = x * 2"
            defs `shouldBe` S.fromList ["g"]
            S.member "x" uses `shouldBe` False

        it "two-cell pair (f x, g x): neither references the other's x" $ do
            let (_, usesA) = cellNames "f x = x + 1"
                (_, usesB) = cellNames "g x = x * 2"
            -- Neither cell exposes x as a use; building the DAG over
            -- both cells therefore produces no edge between them.
            S.member "x" usesA `shouldBe` False
            S.member "x" usesB `shouldBe` False

        it "lambda params do not leak: cell with `\\x -> x` has no x in uses" $ do
            let (_, uses) = cellNames "double = \\x -> x + x"
            S.member "x" uses `shouldBe` False

        it "where-clause locals do not leak" $ do
            let src =
                    "shout msg = greet msg ++ \"!\"\n"
                        <> "  where greet m = \"Hello, \" ++ m"
                (defs, uses) = cellNames src
            defs `shouldBe` S.fromList ["shout"]
            -- `greet` and `m` are both local to `shout` — they must
            -- not appear as external uses.
            S.member "greet" uses `shouldBe` False
            S.member "m" uses `shouldBe` False
            S.member "msg" uses `shouldBe` False

        it "do-block <- binders do not leak" $ do
            let src =
                    "act = do\n"
                        <> "  line <- getLine\n"
                        <> "  putStrLn line"
                (defs, uses) = cellNames src
            defs `shouldBe` S.fromList ["act"]
            S.member "line" uses `shouldBe` False

        it "list-comprehension generators do not leak" $ do
            let (defs, uses) = cellNames "evens = [x * 2 | x <- xs]"
            defs `shouldBe` S.fromList ["evens"]
            -- `x` is a generator binder — local to the comprehension.
            S.member "x" uses `shouldBe` False
            -- `xs` is a free reference and SHOULD appear.
            S.member "xs" uses `shouldBe` True

        it "case-pat binders do not leak" $ do
            let src =
                    "describe v = case v of\n"
                        <> "  Just y  -> show y\n"
                        <> "  Nothing -> \"none\""
                (defs, uses) = cellNames src
            defs `shouldBe` S.fromList ["describe"]
            -- `y` is bound by the Just-pattern; `v` is a function
            -- param. Neither escapes.
            S.member "y" uses `shouldBe` False
            S.member "v" uses `shouldBe` False

        it "let-in binders do not leak" $ do
            let (_, uses) = cellNames "outer = let z = 99 in z + 1"
            S.member "z" uses `shouldBe` False

        it "free reference is preserved when an unrelated decl binds the same name" $ do
            -- `double x = ...` introduces local x. `main = print (double x)`
            -- has a FREE x in main's body. Per-decl scoping must keep it.
            let src =
                    "double x = x * 2\n"
                        <> "main = print (double x)"
                (_, uses) = cellNames src
            S.member "x" uses `shouldBe` True

    -- ----------------------------------------------------------------
    -- Things that should NOT show up in the reactive DAG: import,
    -- pragma, GHCi directive, comment-only.
    -- ----------------------------------------------------------------
    describe "non-decl content does not pollute the DAG" $ do
        it "imports do not contribute to defs OR uses" $ do
            cellNames "import Data.Map" `shouldBe` (S.empty, S.empty)

        it "qualified imports do not contribute to defs OR uses" $ do
            cellNames "import qualified Data.Map as M"
                `shouldBe` (S.empty, S.empty)

        it "pragmas do not contribute" $ do
            cellNames "{-# LANGUAGE OverloadedStrings #-}"
                `shouldBe` (S.empty, S.empty)

        it "comment-only cells produce empty sets" $ do
            cellNames "-- just a note about something"
                `shouldBe` (S.empty, S.empty)

        it "GHCi `:set` directives are stripped (no defs/uses)" $ do
            cellNames ":set -XTypeApplications"
                `shouldBe` (S.empty, S.empty)

        it "GHCi `:type` directives are stripped" $ do
            cellNames ":type 1 + 2" `shouldBe` (S.empty, S.empty)

        it "cabal metadata lines are stripped" $ do
            cellNames "-- cabal: build-depends: text" `shouldBe` (S.empty, S.empty)

        it "imports + decl: only the decl shows up" $ do
            let src =
                    "import Data.Text (Text)\n"
                        <> "greet :: Text -> Text\n"
                        <> "greet name = \"Hi \" <> name"
                (defs, uses) = cellNames src
            defs `shouldBe` S.fromList ["greet"]
            -- name is a param, doesn't leak. The string is a literal.
            -- `<>` is a free operator reference.
            S.member "name" uses `shouldBe` False
            S.member "<>" uses `shouldBe` True
            -- Type-sig identifiers must not show up as defs (they
            -- announce types of binders, not new names).
            S.member "Text" defs `shouldBe` False

    -- ----------------------------------------------------------------
    -- Modern extensions the heuristic mishandled
    -- ----------------------------------------------------------------
    describe "modern extensions" $ do
        it "TypeApplications: `f @Int x` references f, x — type arg ignored" $ do
            let (_, uses) = cellNames "result = f @Int x"
            S.member "f" uses `shouldBe` True
            S.member "x" uses `shouldBe` True

        it "DataKinds: promoted constructors don't sneak into defs" $ do
            let src =
                    "data Color = Red | Green | Blue\n"
                        <> "type Mix = '[ 'Red, 'Blue ]"
                (defs, _) = cellNames src
            -- Color/Red/Green/Blue come from the data decl. Mix from type.
            -- The promoted-constructor mentions in Mix's RHS are uses,
            -- not defs.
            S.member "Color" defs `shouldBe` True
            S.member "Mix" defs `shouldBe` True

        it "GADTs: each constructor name is a def" $ do
            let src =
                    "data Expr a where\n"
                        <> "  Lit :: Int -> Expr Int\n"
                        <> "  Add :: Expr Int -> Expr Int -> Expr Int"
                (defs, _) = cellNames src
            S.member "Expr" defs `shouldBe` True
            S.member "Lit" defs `shouldBe` True
            S.member "Add" defs `shouldBe` True

    -- ----------------------------------------------------------------
    -- REPL fragments (statement-form let, monadic bind, bare expr)
    -- ----------------------------------------------------------------
    describe "REPL fragments parse cleanly" $ do
        it "statement-form `let x = 1` is treated as `x = 1`" $ do
            cellNames "let x = 1" `shouldBe` (S.fromList ["x"], S.empty)

        it "monadic <- binds the LHS as a def" $ do
            let (defs, _) = cellNames "x <- readFile \"a\""
            S.member "x" defs `shouldBe` True

        it "bare expression: no defs, references go to uses" $ do
            let (defs, uses) = cellNames "print (square 4)"
            defs `shouldBe` S.empty
            S.member "print" uses `shouldBe` True
            S.member "square" uses `shouldBe` True

        it "multi-line cell with mixed shapes" $ do
            let src =
                    "import Data.Text (Text)\n"
                        <> ":set -XTypeApplications\n"
                        <> "let greeting = \"hi\"\n"
                        <> "main = putStrLn greeting"
                (defs, uses) = cellNames src
            defs `shouldBe` S.fromList ["greeting", "main"]
            -- putStrLn is the only external use: greeting is intra-cell
            -- (defined two lines above), the import is stripped.
            S.member "putStrLn" uses `shouldBe` True
            S.member "greeting" uses `shouldBe` False

    -- ----------------------------------------------------------------
    -- Literals and comments
    -- ----------------------------------------------------------------
    describe "string/char literals and comments" $ do
        it "identifiers inside strings are not extracted" $ do
            let (_, uses) = cellNames "msg = \"alpha beta gamma\""
            S.member "alpha" uses `shouldBe` False
            S.member "beta" uses `shouldBe` False
            S.member "gamma" uses `shouldBe` False

        it "identifiers inside line comments are not extracted" $ do
            let (defs, uses) =
                    cellNames "y = 1 -- secretName mentioned here"
            defs `shouldBe` S.fromList ["y"]
            S.member "secretName" uses `shouldBe` False

        it "identifiers inside block comments are not extracted" $ do
            let (_, uses) =
                    cellNames "x = 1 {- old hint about hiddenName -} + 2"
            S.member "hiddenName" uses `shouldBe` False
