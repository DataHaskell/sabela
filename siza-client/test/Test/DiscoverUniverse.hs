{-# LANGUAGE OverloadedStrings #-}

{- | The synthetic packages the discover specs search. Kept apart from the
runners that query them so the universe can grow without the fixture module
growing with it.
-}
module Test.DiscoverUniverse (
    SynPkg (..),
    synAbsent,
    synCatalogueOnly,
    synHackageNames,
    synHidden,
    synHoogle,
    synHoogleCatalogued,
    synInstalled,
) where

import Data.List (sort)
import Data.Text (Text)

data SynPkg = SynPkg
    { spName :: Text
    , spVersion :: Text
    , spHidden :: Bool
    , spModules :: [(Text, [(Text, Text)])]
    }

synInstalled :: [SynPkg]
synInstalled =
    [ SynPkg
        "zephyr"
        "1.2.0"
        False
        [ ("Zephyr.Core", [("gust", "Int -> Wind"), ("lull", "Wind -> Wind")])
        , ("Zephyr.Internal.Raw", [("gustRaw", "Int -> Int")])
        ]
    , SynPkg
        "stratus"
        "0.9.1"
        False
        [
            ( "Stratus.Air"
            , [("lull", "Air -> Air"), ("stratify", "[Air] -> Layered")]
            )
        ]
    ]

synHidden :: SynPkg
synHidden =
    SynPkg
        "cumulus"
        "0.3.1"
        True
        [("Cumulus.Plot", [("bars", "[(Text, Double)] -> Plot -> Text")])]

synAbsent :: SynPkg
synAbsent =
    SynPkg "nimbus" "2.0.0" False [("Nimbus.Sky", [("drizzle", "Sky -> Rain")])]

{- | A package the catalogue names and Hoogle answers for with nothing in it.
Hoogle is built from installed haddock alone, so an uninstalled package yields a
bucket with no modules and no api — the shape the hodatime episode ran onto.
-}
synCatalogueOnly :: SynPkg
synCatalogueOnly = SynPkg "hodograph" "1.0.0" False []

synHoogle :: [SynPkg]
synHoogle = synInstalled ++ [synHidden, synAbsent]

-- | 'synHoogle', plus the package Hoogle can name but not describe.
synHoogleCatalogued :: [SynPkg]
synHoogleCatalogued = synHoogle ++ [synCatalogueOnly]

synHackageNames :: [Text]
synHackageNames = sort (map spName synHoogle)
