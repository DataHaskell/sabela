{-# LANGUAGE OverloadedStrings #-}

module Test.DefaultExtsSpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Sabela.Deps (mergedMeta, sabelaDefaultExts)
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Render (renderCabalScriptHeader)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }

spec :: Spec
spec = describe "default language extensions" $ do
    it "baseline includes TemplateHaskell, GADTs, DataKinds, OverloadedStrings" $ do
        S.fromList sabelaDefaultExts
            `shouldBe` S.fromList
                [ "TemplateHaskell"
                , "GADTs"
                , "DataKinds"
                , "OverloadedStrings"
                , "TypeApplications"
                , "ScopedTypeVariables"
                ]

    it "mergedMeta injects the baseline even when the notebook declares none" $ do
        let exts = S.fromList (metaExts (mergedMeta S.empty emptyMeta))
        exts `shouldSatisfy` (S.fromList sabelaDefaultExts `S.isSubsetOf`)

    it "mergedMeta preserves notebook-declared extensions alongside the baseline" $ do
        let meta = emptyMeta{metaExts = ["RankNTypes"]}
            exts = S.fromList (metaExts (mergedMeta S.empty meta))
        exts `shouldSatisfy` S.member "RankNTypes"
        exts `shouldSatisfy` (S.fromList sabelaDefaultExts `S.isSubsetOf`)

    it "mergedMeta does not duplicate an already-declared baseline extension" $ do
        let meta = emptyMeta{metaExts = ["DataKinds"]}
            exts = metaExts (mergedMeta S.empty meta)
        length (filter (== "DataKinds") exts) `shouldBe` 1

    it "export header (renderCabalScriptHeader) carries the baseline extensions" $ do
        let header = renderCabalScriptHeader (mergedMeta S.empty emptyMeta)
            line =
                case filter ("default-extensions:" `T.isPrefixOf`) (T.lines header) of
                    (l : _) -> T.unpack l
                    [] -> ""
        line `shouldContain` "TemplateHaskell"
        line `shouldContain` "GADTs"
        line `shouldContain` "DataKinds"
        line `shouldContain` "OverloadedStrings"
