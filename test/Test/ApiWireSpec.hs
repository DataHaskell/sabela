{-# LANGUAGE OverloadedStrings #-}

{- | Wire-format pinning for 'Sabela.Api' DTOs whose JSON shape is consumed
by the frontend (and is therefore part of our external contract). The
Generic-derived shapes use the record selector names (@cfPath@,
@icAfter@, …); when we hand-roll an instance to support an ADT migration
the renamed keys silently break the frontend, and there is no other test
that catches it.
-}
module Test.ApiWireSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe (mapMaybe)
import Sabela.AI.Capabilities.ToolName (parseToolName, toolWireName)
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.Api
import Sabela.Ids (EditId (..), ToolCallId (..), TurnId (..))
import Sabela.Model (CellType (..), NotebookEvent (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec

evField :: String -> NotebookEvent -> Maybe Value
evField k ev = case decode (encode ev) of
    Just (Object o) -> KM.lookup (Key.fromString k) o
    _ -> Nothing

spec :: Spec
spec = describe "Sabela.Api wire shapes" $ do
    describe "CreateFileRequest" $ do
        it "decodes the cfPath/cfContent/cfIsDir shape the frontend posts" $
            decode "{\"cfPath\":\"a.md\",\"cfContent\":\"hi\",\"cfIsDir\":false}"
                `shouldBe` Just (CreateFile "a.md" "hi")
        it "decodes the dir shape (cfIsDir=true, empty content)" $
            decode "{\"cfPath\":\"sub\",\"cfContent\":\"\",\"cfIsDir\":true}"
                `shouldBe` Just (CreateDir "sub")
        it "rejects cfIsDir=true with non-empty cfContent" $
            ( decode "{\"cfPath\":\"x\",\"cfContent\":\"y\",\"cfIsDir\":true}" ::
                Maybe CreateFileRequest
            )
                `shouldBe` Nothing
        it "round-trips CreateFile through encode/decode" $
            decode (encode (CreateFile "a.md" "hi"))
                `shouldBe` Just (CreateFile "a.md" "hi")
        it "round-trips CreateDir through encode/decode" $
            decode (encode (CreateDir "sub"))
                `shouldBe` Just (CreateDir "sub")
        it "emits cfPath/cfContent/cfIsDir keys (not bare path/content/isDir)" $
            LC8.unpack (encode (CreateFile "a.md" "hi"))
                `shouldContain` "cfPath"

    describe "FilePreview" $ do
        it "emits fpContent/fpOffset/fpReturned/fpTotalBytes/fpEof keys" $ do
            let s = LC8.unpack (encode (FilePreview "hi" 0 2 2 True))
            s `shouldContain` "fpContent"
            s `shouldContain` "fpTotalBytes"
            s `shouldContain` "fpEof"
        it "round-trips through encode/decode" $
            decode (encode (FilePreview "hi" 5 2 7 False))
                `shouldBe` Just (FilePreview "hi" 5 2 7 False)

    describe "InsertCell" $ do
        it "decodes icAfter=-1 as AtBeginning" $
            decode
                "{\"icAfter\":-1,\"icType\":\"CodeCell\",\"icLang\":\"Haskell\",\"icSource\":\"x\"}"
                `shouldBe` Just (InsertCell AtBeginning CodeCell Haskell "x")
        it "decodes icAfter=5 as After 5" $
            decode
                "{\"icAfter\":5,\"icType\":\"CodeCell\",\"icLang\":\"Haskell\",\"icSource\":\"x\"}"
                `shouldBe` Just (InsertCell (After 5) CodeCell Haskell "x")
        it "rejects icAfter=-2 (negative other than -1)" $
            ( decode
                "{\"icAfter\":-2,\"icType\":\"CodeCell\",\"icLang\":\"Haskell\",\"icSource\":\"x\"}" ::
                Maybe InsertCell
            )
                `shouldBe` Nothing
        it "emits AtBeginning as the integer -1 (wire compat)" $
            LC8.unpack (encode AtBeginning) `shouldBe` "-1"
        it "emits After 7 as the integer 7 (wire compat)" $
            LC8.unpack (encode (After 7)) `shouldBe` "7"

    describe "AI tool catalogue ↔ dispatcher coupling" $ do
        it "every chatTools tdName parses via parseToolName" $
            mapMaybe (parseToolName . tdName) chatTools
                `shouldSatisfy` ((== length chatTools) . length)
        it "toolWireName is the inverse of parseToolName for every known name" $
            mapMaybe (fmap toolWireName . parseToolName . tdName) chatTools
                `shouldBe` map tdName chatTools

    describe "NotebookEvent typed-id wire shape" $ do
        it "EvCellCompiling emits type cellCompiling with a bare cellId" $ do
            evField "type" (EvCellCompiling 4) `shouldBe` Just (String "cellCompiling")
            evField "cellId" (EvCellCompiling 4) `shouldBe` Just (Number 4)
        it "EvChatDone (TurnId 42) emits turnId as a bare integer" $
            evField "turnId" (EvChatDone (TurnId 42)) `shouldBe` Just (Number 42)
        it "EvChatError with Just (TurnId 7) emits turnId 7" $
            evField "turnId" (EvChatError (Just (TurnId 7)) "boom")
                `shouldBe` Just (Number 7)
        it "EvChatError with Nothing emits turnId as JSON null (no magic 0)" $
            evField "turnId" (EvChatError Nothing "global") `shouldBe` Just Null
        it "EvChatToolCall toolCallId emits the bare string" $
            evField
                "toolCallId"
                ( EvChatToolCall
                    (TurnId 1)
                    (ToolCallId "toolu_abc")
                    "ghci_query"
                    (object [])
                )
                `shouldBe` Just (String "toolu_abc")
        it "EvChatEditProposed editId emits the bare integer" $
            evField
                "editId"
                ( EvChatEditProposed
                    (Just (TurnId 1))
                    (EditId 99)
                    5
                    "old"
                    "new"
                )
                `shouldBe` Just (Number 99)
        it "EvChatEditProposed Nothing turnId emits null (REST-bridge edits)" $
            evField
                "turnId"
                ( EvChatEditProposed
                    Nothing
                    (EditId 99)
                    5
                    "old"
                    "new"
                )
                `shouldBe` Just Null
