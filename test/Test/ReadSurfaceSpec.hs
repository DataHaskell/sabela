{-# LANGUAGE OverloadedStrings #-}

{- | C1-14b: what read_file says about itself. It names the views it can
return, it sends the caller to no second tool, and it offers no example that
names a resource the model has not read.
-}
module Test.ReadSurfaceSpec (spec) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Files (execReadFile)
import Sabela.AI.Capabilities.ToolName (parseToolName)
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.State (newApp)
import Test.ArtefactGen (genArtefactText)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textOf :: Maybe Value -> Text
textOf (Just (String s)) = s
textOf _ = ""

spec :: Spec
spec = describe "what read_file says about itself (C1-14b)" $ do
    it "names every view a read can return" $
        property $
            forAll genArtefactText $ \txt ->
                ioProperty $
                    withSystemTempDirectory "read-surface" $ \root -> do
                        app <- newApp root Set.empty Nothing Nothing []
                        BS.writeFile (root </> "a.dat") (TE.encodeUtf8 txt)
                        v <-
                            toolOutcomeValue
                                <$> execReadFile app (object ["path" .= ("a.dat" :: Text)])
                        pure $
                            counterexample (show (textOf (field "view" v), readFileDoc)) $
                                textOf (field "view" v) `T.isInfixOf` readFileDoc

    it "sends the caller to no other tool" $
        toolsNamedIn (surfaceTextOf "read_file") `shouldBe` []

    it "offers no example that names a particular file" $ do
        root <- repoRoot
        onDisk <- filterM (doesFileExist . (root </>) . T.unpack) surfacePaths
        (filter namesAFile surfacePaths, onDisk) `shouldBe` ([], [])
        surfacePaths `shouldSatisfy` (not . null)
        pathArgDocs `shouldSatisfy` (not . null)

{- | The description the model actually routes on, taken from the catalogue
rather than from the constant behind it.
-}
readFileDoc :: Text
readFileDoc =
    T.concat [tdDescription t | t <- chatTools, tdName t == "read_file"]

-- | Everything a tool says about itself: its description and its schema.
surfaceTextOf :: Text -> Text
surfaceTextOf name =
    T.concat [tdDescription t <> schemaText t | t <- chatTools, tdName t == name]

schemaText :: ToolDef -> Text
schemaText = TE.decodeUtf8 . LBS.toStrict . encode . tdInputSchema

{- | Every word of a tool's surface that the router would resolve to another
tool. Taken from the router rather than from the catalogue, so a name does
not leave the check when the tool leaves the catalogue.
-}
toolsNamedIn :: Text -> [Text]
toolsNamedIn src =
    [ w
    | w <- T.split (\c -> not (isAlphaNum c || c == '_')) src
    , w /= "read_file"
    , isJust (parseToolName w)
    ]

{- | Every example the surface offers as a path: any token carrying a
separator, and any filename-shaped token where a path argument is documented,
since a name needs no separator to be a name.
-}
surfacePaths :: [Text]
surfacePaths = nub (withSeparator <> bareNames)
  where
    withSeparator =
        [ t
        | src <- concat [[tdDescription d, schemaText d] | d <- chatTools]
        , t <- tokensOf src
        , "/" `T.isInfixOf` t
        ]
    bareNames = [t | t <- concatMap tokensOf pathArgDocs, looksLikeFilename t]

tokensOf :: Text -> [Text]
tokensOf src =
    [ trimmed
    | p <- T.split (`elem` (" \"'`(),;\n" :: String)) src
    , let trimmed = T.dropWhileEnd (`elem` (".\\" :: String)) p
    , not (T.null trimmed)
    ]

{- | What every tool documents about an argument it resolves as a path. This
is where an example file would be offered, and where prose is about files.
-}
pathArgDocs :: [Text]
pathArgDocs =
    [ d
    | t <- chatTools
    , Just props <- [field "properties" (tdInputSchema t)]
    , Just arg <- [field "path" props]
    , Just (String d) <- [field "description" arg]
    ]

{- | An example that names a file rather than a place. A name on the surface
is a name the model has not read, offered by the tool whose whole job is to
stop it guessing one.
-}
namesAFile :: Text -> Bool
namesAFile p = case reverse (T.splitOn "/" p) of
    seg : _ -> "." `T.isInfixOf` seg
    [] -> False

{- | A bare token that reads as a file name. Tighter than 'namesAFile',
because prose is full of abbreviations: the stem and the extension must both
be long enough, and the extension plain enough, to be a file's.
-}
looksLikeFilename :: Text -> Bool
looksLikeFilename t =
    T.length stem > 2
        && T.length ext >= 2
        && T.length ext <= 8
        && T.all isAlphaNum ext
  where
    (stem, ext) = T.breakOnEnd "." t

-- | The tree the harness ships, found from the package it is tested in.
repoRoot :: IO FilePath
repoRoot = getCurrentDirectory >>= climb (20 :: Int)
  where
    climb 0 dir = pure dir
    climb n dir = do
        here <- doesFileExist (dir </> "sabela.cabal")
        let up = takeDirectory dir
        if here || up == dir then pure dir else climb (n - 1) up
