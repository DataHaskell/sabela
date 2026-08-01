{-# LANGUAGE OverloadedStrings #-}

{- | What the two siza surfaces promise about @try@, against what the
capability computes. A read-only annotation a host acts on, and a description
the model routes on, are claims: they are checked here, not asserted.
-}
module Test.TrySurfaceSpec (trySurfaceSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.ToolDoc (
    checkTypeImportsDoc,
    tierTypecheckOnly,
    tryDescription,
 )
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.ToolRoute (Route (..), routeCallWith)
import Siza.Agent.Tools (offeredArgKeys)
import Siza.Agent.Tools.Catalogue (baseCatalogue)
import Siza.Mcp.Surface (mcpCatalogue, readOnlyTools)

-- | The description the chat catalogue publishes for a tool.
chatDescription :: Text -> Maybe Text
chatDescription name =
    lookup
        name
        [ (n, d)
        | Object o <- baseCatalogue
        , Just (Object f) <- [KM.lookup "function" o]
        , Just (String n) <- [KM.lookup "name" f]
        , Just (String d) <- [KM.lookup "description" f]
        ]

-- | One field of the MCP catalogue's entry for a tool.
mcpField :: Text -> Text -> Maybe Value
mcpField name key =
    lookup
        name
        [(n, o) | Object o <- mcpCatalogue, Just (String n) <- [KM.lookup "name" o]]
        >>= KM.lookup (K.fromText key)

annotationOf :: Text -> Text -> Maybe Value
annotationOf name key = case mcpField name "annotations" of
    Just (Object o) -> KM.lookup (K.fromText key) o
    _ -> Nothing

argKeysOf :: Text -> ([Text], [Text])
argKeysOf name = fromMaybe ([], []) (lookup name offeredArgKeys)

importsCall :: ToolCall
importsCall =
    ToolCall
        "check_type"
        (object ["expr" .= ("insert" :: Text), "imports" .= (["Data.Map"] :: [Text])])

renderAll :: Value -> Text
renderAll (Object o) = T.unwords (map renderAll (KM.elems o))
renderAll (String s) = s
renderAll (Array a) = T.unwords (map renderAll (foldr (:) [] a))
renderAll other = T.pack (show other)

trySurfaceSpec :: Spec
trySurfaceSpec = describe "what the surfaces promise about try" $ do
    describe "C1-5a: the promise is the capability's own" $ do
        it "publishes one description to both siza surfaces" $ do
            chatDescription "try" `shouldBe` Just tryDescription
            mcpField "try" "description" `shouldBe` Just (String tryDescription)

        it "claims read-only only where it says an effect is not performed" $ do
            "try" `shouldSatisfy` (`elem` readOnlyTools)
            annotationOf "try" "readOnlyHint" `shouldBe` Just (Bool True)
            chatDescription "try"
                `shouldSatisfy` maybe False (T.isInfixOf tierTypecheckOnly)

        it "names the field that lists what a disposable trial re-runs" $
            chatDescription "try"
                `shouldSatisfy` maybe False (T.isInfixOf "replayedCells")

        it "names the execution fact the tier is read from" $
            chatDescription "try"
                `shouldSatisfy` maybe False (T.isInfixOf "executed")

    describe "C2-checktype-imports: the scope argument reaches the server" $ do
        it "offers it on the surface the client routes against" $
            fst (argKeysOf "check_type") `shouldContain` ["imports"]

        it "routes a call that names the scope it wants" $
            case routeCallWith offeredArgKeys importsCall of
                RouteBadArgs hint -> expectationFailure (T.unpack hint)
                RouteUnknown n -> expectationFailure (T.unpack n)
                _ -> pure ()

        it "documents it in the words the other surfaces use" $
            mcpField "check_type" "inputSchema"
                `shouldSatisfy` maybe False (T.isInfixOf checkTypeImportsDoc . renderAll)
