{-# LANGUAGE OverloadedStrings #-}

{- | The discover → read_source handoff: an absent-known package's miss or
scope note leads to the tool that shows its released source, instead of
leaving the model to guess a GitHub repository.
-}
module Test.ReadSourceHandoffSpec (readSourceHandoffSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Test.DiscoverFixtures (
    installFactsRows,
    installNamesFileWith,
    runCatArgs,
    synHackageNames,
    textField,
 )
import Test.LadderFixtures (hodatimeFactsRow)

{- | The absent-known world: hodatime with one root, and a package whose
several roots leave no entry module to mint a read_source call for.
-}
installWorld :: IO ()
installWorld = do
    installNamesFileWith (synHackageNames ++ ["hodatime", "scattered"])
    installFactsRows
        [ hodatimeFactsRow
        , "scattered\t\tBits\tAlpha.One Beta.Two\t1.0"
        ]

adviceOf :: Value -> IO Text
adviceOf args = do
    v <- runCatArgs (queryOf args) args
    pure (textField "next" v <> " " <> textField "narrow" v)
  where
    queryOf _ = "zzznotathing"

readSourceHandoffSpec :: Spec
readSourceHandoffSpec =
    before_ installWorld $
        describe "the discover -> read_source handoff" $ do
            it "an absent-known module scope names the exact call" $ do
                advice <-
                    adviceOf
                        ( object
                            [ "query" .= ("zzznotathing" :: Text)
                            , "module" .= ("Data.HodaTime" :: Text)
                            ]
                        )
                advice
                    `shouldSatisfy` T.isInfixOf
                        "read_source {module: \"Data.HodaTime\"}"

            it "an absent-known package scope uses its entry module" $ do
                advice <-
                    adviceOf
                        ( object
                            [ "query" .= ("zzznotathing" :: Text)
                            , "package" .= ("hodatime" :: Text)
                            ]
                        )
                advice
                    `shouldSatisfy` T.isInfixOf
                        "read_source {module: \"Data.HodaTime\"}"

            it "a package with no entry module offers no minted call" $ do
                advice <-
                    adviceOf
                        ( object
                            [ "query" .= ("zzznotathing" :: Text)
                            , "package" .= ("scattered" :: Text)
                            ]
                        )
                advice `shouldSatisfy` (not . T.isInfixOf "read_source")

            it "a plain miss with no absent package names no read_source" $ do
                advice <- adviceOf (object ["query" .= ("zzznotathing" :: Text)])
                advice `shouldSatisfy` (not . T.isInfixOf "read_source")
