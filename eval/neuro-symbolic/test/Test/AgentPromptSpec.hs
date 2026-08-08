{-# LANGUAGE OverloadedStrings #-}

module Test.AgentPromptSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Agent (systemPrompt)
import Sabela.AI.Grammar (discoverGrammarBlock, grammarPromptBlock)
import Sabela.AI.PromptCore (
    sabelaBuiltins,
    sharedPromptCore,
    sharedPromptCoreWith,
 )

surfaces :: [(String, Text)]
surfaces = ruleSurfaces ++ [("siza/eval full systemPrompt", systemPrompt)]

{- | The surfaces that state working rules. The siza/eval system prompt is not
one of them: it names the tools and the built-in library and stops there.
-}
ruleSurfaces :: [(String, Text)]
ruleSurfaces =
    [ ("product chat (sharedPromptCore)", sharedPromptCore)
    , ("product core explicit", sharedPromptCoreWith grammarPromptBlock)
    , ("siza/eval core", sharedPromptCoreWith discoverGrammarBlock)
    ]

bannedClasses :: [(String, [Text])]
bannedClasses =
    [
        ( "compiler-first-via-scratchpad mandate (R6.8/M15)"
        ,
            [ "compiler first"
            , "compile-check it"
            , "compile check it"
            , "check it with the scratchpad"
            , "compile-check with a query tool"
            ]
        )
    ,
        ( "absolute unconfirmed-name ban (R5.8)"
        ,
            [ "never write a name"
            , "do not write a name"
            , "search did not confirm"
            , "search has not confirmed"
            , "do not invent"
            ]
        )
    ]

workingRules :: Text -> [Text]
workingRules p = go [] (takeWhile (not . sectionBreak) body)
  where
    body = drop 1 (dropWhile (/= "## Working rules") (T.lines p))
    sectionBreak l = "##" `T.isPrefixOf` l
    go acc [] = reverse (map T.unwords acc)
    go acc (l : rest)
        | "- " `T.isPrefixOf` T.stripStart l =
            go ([T.strip l] : acc) rest
        | (cur : done) <- acc
        , not (T.null (T.strip l)) =
            go ((cur ++ [T.strip l]) : done) rest
        | otherwise = go acc rest

literalCallerReachesWrite :: [Text] -> Either Text ()
literalCallerReachesWrite rules = case concatMap blocking rules of
    [] -> Right ()
    (r : _) -> Left r
  where
    blocking rl =
        let r = T.toLower rl
            preWriteCompileViaIsolation =
                ("before" `T.isInfixOf` r)
                    && ("compile" `T.isInfixOf` r)
                    && ("scratchpad" `T.isInfixOf` r)
            absoluteNameBan =
                ("never write" `T.isInfixOf` r)
                    || ("do not write a name" `T.isInfixOf` r)
                    || ("search did not confirm" `T.isInfixOf` r)
         in [rl | preWriteCompileViaIsolation || absoluteNameBan]

spec :: Spec
spec = describe "shared prompt core (unified, satisfiable)" $ do
    it "embeds the built-in library block the product core also carries" $
        (sabelaBuiltins `T.isInfixOf` systemPrompt) `shouldBe` True

    it "states no working rules, so none can be asserted of it" $
        workingRules systemPrompt `shouldBe` []

    describe "prompt lint: banned phrase classes on every surface" $
        forM_ surfaces $ \(name, surface) ->
            it (name ++ " carries no banned phrase") $
                forM_ bannedClasses $ \(cls, phrases) ->
                    forM_ phrases $ \p ->
                        (cls, p, p `T.isInfixOf` T.toLower surface)
                            `shouldBe` (cls, p, False)

    describe "R6.8 satisfiability: rules can be followed verbatim" $ do
        forM_ ruleSurfaces $ \(name, surface) ->
            it (name ++ ": the working rules are non-empty") $
                workingRules surface `shouldSatisfy` (not . null)
        forM_ surfaces $ \(name, surface) ->
            it
                (name ++ ": a literal caller writing a binding-referencing cell reaches a write")
                $ literalCallerReachesWrite (workingRules surface)
                    `shouldBe` Right ()

    describe "one speculative interface guidance is present" $
        it "the shared core says to try before commit without naming legacy modes" $ do
            let core = T.toLower (sharedPromptCoreWith discoverGrammarBlock)
            core `shouldSatisfy` T.isInfixOf "try, then commit"
            core `shouldSatisfy` T.isInfixOf "at most one final expression"
            core `shouldSatisfy` T.isInfixOf "ghci meta-commands"
            core `shouldSatisfy` T.isInfixOf "compile-time escapes"
            core `shouldSatisfy` T.isInfixOf "unrestricted io"
            core `shouldSatisfy` T.isInfixOf "durable home for owned effects"
            core `shouldSatisfy` (not . T.isInfixOf "scratchpad")
            core `shouldSatisfy` (not . T.isInfixOf "eval_live")
