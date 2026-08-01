{-# LANGUAGE OverloadedStrings #-}

{- | C1-14: a read that returns no artefact states only what the harness
computed. What a listing shows and what a read admits exists are one set, and
both file tools give one account of any path.
-}
module Test.ReadMissSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (
    createDirectoryIfMissing,
    emptyPermissions,
    getPermissions,
    setPermissions,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Artefact (sampleFile)
import Sabela.AI.Capabilities.Files (execListFiles, execReadFile)
import Sabela.AI.Capabilities.Query (execPeekData)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.State (App (..), newApp)
import Test.ArtefactGen (genIdent, genTreePaths)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textOf :: Maybe Value -> Text
textOf (Just (String s)) = s
textOf _ = ""

arrayOf :: Maybe Value -> [Value]
arrayOf (Just (Array a)) = foldr (:) [] a
arrayOf _ = []

{- | The harness's own mark for "this path is not there", emitted by the miss
outcome and by nothing else, so the check reads the payload rather than prose.
-}
deniesExistence :: Value -> Bool
deniesExistence = isJust . field "missingPath"

candidatesOf :: Value -> [Text]
candidatesOf v = [t | String t <- arrayOf (field "candidates" v)]

readValue :: App -> Text -> IO Value
readValue app path =
    toolOutcomeValue <$> execReadFile app (object ["path" .= path])

peekValue :: App -> Text -> IO Value
peekValue app path =
    toolOutcomeValue <$> execPeekData app (object ["path" .= path])

-- | Every path the work-dir listing reports, which is the property's oracle.
listedFiles :: App -> IO [Text]
listedFiles app = do
    v <- toolOutcomeValue <$> execListFiles app (object ["path" .= ("" :: Text)])
    pure [textOf (field "path" e) | e <- arrayOf (field "files" v)]

-- | The directories a listed path lies under, outermost first.
ancestorsOf :: Text -> [Text]
ancestorsOf p =
    [T.intercalate "/" (take k segs) | k <- [1 .. length segs - 1]]
  where
    segs = T.splitOn "/" p

lastSegment :: Text -> Text
lastSegment = last . T.splitOn "/"

-- | The tree, its files as the listing reports them, and the places they lie in.
withTree :: [FilePath] -> (App -> [Text] -> [Text] -> IO a) -> IO a
withTree paths k = withSystemTempDirectory "read-miss" $ \root -> do
    mapM_ (writeUnder root) paths
    app <- newApp root Set.empty Nothing Nothing []
    listed <- listedFiles app
    k app listed (nub (concatMap ancestorsOf listed))

writeUnder :: FilePath -> FilePath -> IO ()
writeUnder root rel = do
    createDirectoryIfMissing True (takeDirectory (root </> rel))
    BS.writeFile (root </> rel) "a,b\n1,2\n"

spec :: Spec
spec = describe "a read reports only what it found (C1-14)" $ do
    it "never denies a path its own listing shows" $
        property $
            forAll genTreePaths $ \paths ->
                ioProperty $
                    withTree paths $ \app listed places -> do
                        answers <- mapM (readWith app) (listed <> places)
                        let denied = [(p, textOf (field "error" v)) | (p, v) <- answers, deniesExistence v]
                        pure $
                            counterexample (show (places, denied)) $
                                not (null places) && null denied

    it "answers a place with the paths under it" $
        property $
            forAll genTreePaths $ \paths ->
                ioProperty $
                    withTree paths $ \app listed places -> do
                        answers <- mapM (readWith app) places
                        let under d = [f | f <- listed, (d <> "/") `T.isPrefixOf` f]
                            shown v = map (textOf . field "path") (arrayOf (field "files" v))
                            missing =
                                [(d, under d, shown v) | (d, v) <- answers, any (`notElem` shown v) (under d)]
                        pure $
                            counterexample (show missing) $
                                not (null places) && null missing

    it "gives one account of a path, whichever file tool is asked" $
        property $
            forAll genTreePaths $ \paths ->
                ioProperty $
                    withTree paths $ \app _ places -> do
                        answers <- mapM (bothWith app) places
                        let differ = [a | a@(_, r, p) <- answers, r /= p]
                        pure $
                            counterexample (show differ) $
                                not (null places) && null differ

    it "offers the place a miss could have meant" $
        property $
            forAll ((,) <$> genTreePaths <*> genIdent) $ \(paths, elsewhere) ->
                ioProperty $
                    withTree paths $ \app listed places -> do
                        let wrong d = elsewhere <> "/" <> lastSegment d
                            asked =
                                [ (d, wrong d)
                                | d <- places
                                , wrong d `notElem` (listed <> places)
                                ]
                        answers <- mapM (\(d, w) -> (,) d <$> readValue app w) asked
                        let unoffered =
                                [ (d, candidatesOf v)
                                | (d, v) <- answers
                                , d `notElem` candidatesOf v
                                ]
                        pure $
                            counterexample (show (asked, unoffered)) $
                                not (null asked) && null unoffered

    it "answers a place from inside it, however deep the place lies" $
        withSystemTempDirectory "read-miss" $ \root -> do
            let place = foldr1 (</>) [[c] | c <- "abcdefghij"]
            writeUnder root (place </> "w.dat")
            app <- newApp root Set.empty Nothing Nothing []
            v <- readValue app (T.pack place)
            let shown = map (textOf . field "path") (arrayOf (field "files" v))
            shown `shouldBe` [T.pack (place </> "w.dat")]

    it "says nothing of a listing it did not make" $
        withSystemTempDirectory "read-miss" $ \root -> do
            createDirectoryIfMissing True (root </> "bare")
            writeUnder root ("full" </> "w.dat")
            app <- newApp root Set.empty Nothing Nothing []
            bare <- readValue app "bare"
            full <- readValue app "full"
            arrayOf (field "files" bare) `shouldBe` []
            let said p v = T.replace ("`" <> p <> "`") "`_`" (textOf (field "error" v))
            said "bare" bare `shouldNotBe` said "full" full

    it "reports a file it cannot open as unopened, not as missing" $
        withSystemTempDirectory "read-miss" $ \root -> do
            let target = root </> "shut.dat"
            BS.writeFile target "a,b\n1,2\n"
            perms <- getPermissions target
            setPermissions target emptyPermissions
            app <- newApp root Set.empty Nothing Nothing []
            v <- readValue app "shut.dat"
            opened <- sampleFile target
            setPermissions target perms
            case opened of
                Just _ -> pendingWith "this runner can open a file with no permissions"
                Nothing -> deniesExistence v `shouldBe` False

readWith :: App -> Text -> IO (Text, Value)
readWith app p = (,) p <$> readValue app p

bothWith :: App -> Text -> IO (Text, Value, Value)
bothWith app p = (,,) p <$> readValue app p <*> peekValue app p
