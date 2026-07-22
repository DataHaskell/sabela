{-# LANGUAGE OverloadedStrings #-}

{- | The eval prompt shares the product's working-rules core, the core
carries no unsatisfiable or absolute rule (R6.8, R5.8, M15), and a literal
caller obeying the rules verbatim reaches a binding-referencing write.
-}
module Test.AgentPromptSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Agent (systemPrompt)
import Sabela.AI.Grammar (discoverGrammarBlock, grammarPromptBlock)
import Sabela.AI.PromptCore (sharedPromptCore, sharedPromptCoreWith)

{- | Every prompt surface that ships the shared core: the product chat
(grammar cheat-sheet) and the siza\/eval surface (discover cheat-sheet).
-}
surfaces :: [(String, Text)]
surfaces =
    [ ("product chat (sharedPromptCore)", sharedPromptCore)
    , ("product core explicit", sharedPromptCoreWith grammarPromptBlock)
    , ("siza/eval core", sharedPromptCoreWith discoverGrammarBlock)
    , ("siza/eval full systemPrompt", systemPrompt)
    ]

{- | Banned phrase classes (lower-cased infix): the compiler-first-via-
scratchpad mandate (M15, unsatisfiable for binding-referencing cells) and
absolute never-write-unconfirmed-name rules (R5.8: misses can be wrong).
-}
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

{- | The bulleted working rules of a prompt: each @- @ bullet with its
continuation lines, up to the next bullet or section break.
-}
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

{- | The literal-minded caller of R6.8, writing a binding-referencing cell:
a rule blocks it when it demands an impossible pre-write step — a compile
check via the isolated scratchpad, or an unsatisfiable name confirmation.
-}
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
    it "embeds the shared prompt core with the discover cheat-sheet" $
        (sharedPromptCoreWith discoverGrammarBlock `T.isInfixOf` systemPrompt)
            `shouldBe` True

    describe "prompt lint: banned phrase classes on every surface" $
        forM_ surfaces $ \(name, surface) ->
            it (name ++ " carries no banned phrase") $
                forM_ bannedClasses $ \(cls, phrases) ->
                    forM_ phrases $ \p ->
                        (cls, p, p `T.isInfixOf` T.toLower surface)
                            `shouldBe` (cls, p, False)

    describe "R6.8 satisfiability: rules can be followed verbatim" $ do
        it "the working rules are non-empty (the caller has rules to obey)" $
            workingRules systemPrompt `shouldSatisfy` (not . null)
        forM_ surfaces $ \(name, surface) ->
            it
                (name ++ ": a literal caller writing a binding-referencing cell reaches a write")
                $ literalCallerReachesWrite (workingRules surface)
                    `shouldBe` Right ()

    describe "write-is-the-verifier guidance is present" $
        it "the shared core says the live notebook compile is the check" $ do
            let core = T.toLower (sharedPromptCoreWith discoverGrammarBlock)
            core `shouldSatisfy` T.isInfixOf "compile check"
