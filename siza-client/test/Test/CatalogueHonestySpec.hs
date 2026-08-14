{-# LANGUAGE OverloadedStrings #-}

{- | A description that names a tool the surface does not offer sends the model
somewhere that does not exist — which is how a live opencode session ended up
grepping the filesystem instead of calling discover.
-}
module Test.CatalogueHonestySpec (catalogueHonestySpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isLower)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (parseToolName)
import Sabela.AI.ToolDoc (
    readFileDescription,
    readFilePathArg,
    readSourceDescription,
    readSourceModuleArg,
    readSourceNameArg,
    readSourcePackageArg,
    readSourceVersionArg,
 )
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Compact (compactSeed)
import Siza.Agent.Stack (Surface (..), SurfacePolicy (..), surfacePolicy)
import Siza.Agent.ToolRoute (Route (..), routeCallWith)
import Siza.Agent.Tools (
    catalogue,
    catalogueFor,
    catalogueWith,
    offeredArgKeys,
    offeredNames,
    toolSurfacePrompt,
 )

catalogueHonestySpec :: Spec
catalogueHonestySpec = describe "the tool catalogue's own claims" $ do
    it "never points at a tool this surface does not offer" $
        [ (name, mentioned)
        | (name, desc) <- descriptions
        , mentioned <- knownToolNames
        , mentioned `T.isInfixOf` desc
        , mentioned `notElem` offeredNames
        ]
            `shouldBe` []

    it "leads with the tools that orient a caller who knows nothing" $
        take 2 (map fst descriptions) `shouldMatchList` ["list_cells", "discover"]

    it
        "serves the shared file-read text; its one cross-pointer is read_source (C1-14b)"
        $ do
            lookup "read_file" descriptions `shouldBe` Just readFileDescription
            argDoc "read_file" "path" `shouldBe` Just readFilePathArg
            toolsNamedIn "read_file" readFileDescription `shouldBe` ["read_source"]

    it "serves the shared source-read text on both surfaces" $ do
        lookup "read_source" descriptions `shouldBe` Just readSourceDescription
        argDoc "read_source" "module" `shouldBe` Just readSourceModuleArg
        argDoc "read_source" "name" `shouldBe` Just readSourceNameArg
        argDoc "read_source" "package" `shouldBe` Just readSourcePackageArg
        argDoc "read_source" "version" `shouldBe` Just readSourceVersionArg

    it "says where the notebook's own library can be found" $
        lookup "discover" descriptions
            `shouldSatisfy` maybe False (T.isInfixOf "not on Hackage")

    it "writes no tool name into a message body that will not dispatch (C2-4a)" $
        property $
            forAll genToolTranscript $ \msgs ->
                let (seed, _) = compactSeed msgs
                 in conjoin
                        [ counterexample (T.unpack n) (dispatchable n)
                        | m <- seed
                        , Just c <- [contentOf m]
                        , n <- toolShapedTokens c
                        ]

    it "advertises no tool the surface serving it cannot dispatch (C3-4)" $
        [ (show s, n)
        | s <- [minBound .. maxBound] :: [Surface]
        , n <- surfaceNames s
        , not (dispatchable n)
        ]
            `shouldBe` []

    it "lists in the system prompt exactly the tools it catalogues (NEW-3)" $
        promptNames `shouldMatchList` offeredNames

    it "offers the recall tool only where results are elided (C2-4a)" $
        map (elem "recall_result" . surfaceNames) ([minBound .. maxBound] :: [Surface])
            `shouldBe` map (spElides . surfacePolicy) [minBound .. maxBound]

    it "serves every surface the catalogue its own policy names" $
        [ (show s, map nameOf (catalogueFor s), map nameOf (policyCatalogue s))
        | s <- [minBound .. maxBound] :: [Surface]
        , map nameOf (catalogueFor s) /= map nameOf (policyCatalogue s)
        ]
            `shouldBe` []

    it "declares one JSON type per shared parameter name (C2-10d)" $ do
        [(p, tys) | (p, tys) <- paramTypes, length tys > 1] `shouldBe` []
        length [p | (p, _) <- paramTypes, p == "cell_id"] `shouldBe` 1

    it "declares a parameter on more than one tool, so that is not vacuous" $
        [p | (p, _) <- declaredParams, p == "cell_id"]
            `shouldSatisfy` ((> 1) . length)

    it "serves the episode drivers a surface's catalogue, not a third one" $ do
        served <- catalogue
        map nameOf served `shouldBe` map nameOf (catalogueFor ChatSurface)

{- | Every parameter the offered catalogue declares, with the JSON type each
tool declares it under. A name the surface types two ways tells the model two
different things about the same argument.
-}
declaredParams :: [(Text, Text)]
declaredParams =
    [ (K.toText k, ty)
    | Object o <- catalogueWith False
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (Object params) <- [KM.lookup "parameters" f]
    , Just (Object ps) <- [KM.lookup "properties" params]
    , (k, Object spec) <- KM.toList ps
    , Just (String ty) <- [KM.lookup "type" spec]
    ]

paramTypes :: [(Text, [Text])]
paramTypes =
    [ (p, nub [ty | (q, ty) <- declaredParams, q == p])
    | p <- nub (map fst declaredParams)
    ]

{- | The tools the system prompt names. A catalogued tool the prompt omits is
one the model does not know it has.
-}
promptNames :: [Text]
promptNames =
    [ T.strip n
    | l <- T.lines toolSurfacePrompt
    , Just rest <- [T.stripPrefix "* " l]
    , n <- T.splitOn ", " (T.drop 2 (snd (T.breakOn ": " rest)))
    , not (T.null (T.strip n))
    ]

-- | The tool names a surface serves, from the catalogue it is served from.
surfaceNames :: Surface -> [Text]
surfaceNames = map nameOf . policyCatalogue

{- | The catalogue a surface's policy calls for, built here rather than read
off 'catalogueFor', so the two can disagree.
-}
policyCatalogue :: Surface -> [Value]
policyCatalogue s = catalogueWith (spElides (surfacePolicy s))

nameOf :: Value -> Text
nameOf (Object o) = case KM.lookup "function" o of
    Just (Object f) -> case KM.lookup "name" f of
        Just (String n) -> n
        _ -> ""
    _ -> ""
nameOf _ = ""

{- | A name the router can place. Bad arguments are still a placement: the
question this asks is whether the NAME reaches a tool at all.
-}
dispatchable :: Text -> Bool
dispatchable n = case routeCallWith offeredArgKeys (ToolCall n (object [])) of
    RouteUnknown _ -> False
    _ -> True

{- | Tokens shaped like a tool name. The generator's payload words carry no
underscore, so every such token in an emitted message was written by the
harness rather than echoed from a result.
-}
toolShapedTokens :: Text -> [Text]
toolShapedTokens =
    filter (\w -> T.any (== '_') w && T.all snakeChar w)
        . T.split (not . snakeChar)
  where
    snakeChar c = isLower c || c == '_'

contentOf :: Value -> Maybe Text
contentOf (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> Just s
    _ -> Nothing
contentOf _ = Nothing

{- | Tool messages whose payloads straddle the elision floor, so the harness's
own framing is what compaction adds to them.
-}
genToolTranscript :: Gen [Value]
genToolTranscript = listOf1 $ do
    name <- elements offeredNames
    n <- choose (0, 60)
    ws <- vectorOf n genWord
    pure
        ( object
            [ "role" .= ("tool" :: Text)
            , "tool_name" .= name
            , "content" .= T.unwords ws
            ]
        )
  where
    genWord = T.pack <$> ((:) <$> lower <*> vectorOf 6 lower)
    lower = elements ['a' .. 'z']

{- | Tool names that have existed on some Sabela surface. A description may
name one only if this surface actually offers it.
-}

{- | Every word of a description the router would resolve to a tool other
than the one being described. Taken from the router, so a name stays in the
check after the tool it names leaves the catalogue.
-}
toolsNamedIn :: Text -> Text -> [Text]
toolsNamedIn self desc =
    [ w
    | w <- T.split (\c -> not (isAlphaNum c || c == '_')) desc
    , w /= self
    , isJust (parseToolName w)
    ]

knownToolNames :: [Text]
knownToolNames =
    [ "find_cells_by_content"
    , "explore_result"
    , "describe_function"
    , "propose_edit"
    , "api_reference"
    , "module_card"
    , "eval_live"
    , "browse_card"
    ]

-- | What one tool's argument schema says about one argument.
argDoc :: Text -> Text -> Maybe Text
argDoc tool arg =
    lookup
        tool
        [ (name, doc)
        | Object o <- catalogueWith False
        , Just (Object f) <- [KM.lookup "function" o]
        , Just (String name) <- [KM.lookup "name" f]
        , Just (Object ps) <- [KM.lookup "parameters" f]
        , Just (Object props) <- [KM.lookup "properties" ps]
        , Just (Object a) <- [KM.lookup (K.fromText arg) props]
        , Just (String doc) <- [KM.lookup "description" a]
        ]

descriptions :: [(Text, Text)]
descriptions =
    [ (name, desc)
    | Object o <- catalogueWith False
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String name) <- [KM.lookup "name" f]
    , Just (String desc) <- [KM.lookup "description" f]
    ]
