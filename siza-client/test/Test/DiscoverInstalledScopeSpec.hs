{-# LANGUAGE OverloadedStrings #-}

{- | A scope note must not call a package absent that a source is answering for
as installed. In the 2026-08-07 22:14 episode the notebook installed @hodatime@,
the session returned its exports as @install: installed@, and the same envelope's
note still read "which is not installed" — the model read the note over the hits
beside it and spent the rest of the episode asking whether the kernel had
reloaded.

Only the Hoogle channel fills @saPkgModules@, so deciding coverage from that
alone calls a package absent whenever that one channel happens not to bucket it.
The hits state their own install state; that is the authority.
-}
module Test.DiscoverInstalledScopeSpec (discoverInstalledScopeSpec) where

import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Guidance (absentScopePackage)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Scope (..),
    SourceAnswer (..),
    mkHit,
    okAnswer,
 )

facts :: HackageInfo
facts =
    HackageInfo
        True
        ["hodatime"]
        [
            ( "hodatime"
            , PkgFacts
                "https://github.com/jason-johnson/hodatime"
                "A date/time library"
                ["Data.HodaTime", "Data.HodaTime.CalendarDate"]
                ""
            )
        ]
        []

-- | What the session answers with once the notebook has declared the package.
sessionHit :: InstallState -> DHit
sessionHit st =
    (mkHit "year" "Data.HodaTime.CalendarDate" "hodatime")
        { dhType = "(HasDate d) => d -> Year"
        , dhOrigin = "session"
        , dhInstall = st
        }

{- | The live shape: the session speaks for the package, the Hoogle channel did
not bucket it, so @saPkgModules@ is empty.
-}
answersWith :: InstallState -> [SourceAnswer]
answersWith st = [okAnswer "session" [sessionHit st], okAnswer "hoogle" []]

moduleScope :: Scope
moduleScope = Scope (Just "Data.HodaTime.CalendarDate") Nothing

packageScope :: Scope
packageScope = Scope Nothing (Just "hodatime")

discoverInstalledScopeSpec :: Spec
discoverInstalledScopeSpec =
    describe "coverage is what the hits state (live 20260807-2214)" $ do
        it "calls no package absent while a hit reports it installed" $
            absentScopePackage moduleScope (answersWith InstInstalled) facts
                `shouldBe` Nothing

        it "does so for a package scope too" $
            absentScopePackage packageScope (answersWith InstInstalled) facts
                `shouldBe` Nothing

        {- Installed-but-not-loaded is still coverage: the index can speak for
        it, so telling the caller to declare it is advice they have taken. -}
        it "counts an installed-not-loaded package as covered" $
            absentScopePackage moduleScope (answersWith InstHidden) facts
                `shouldBe` Nothing

        it "still names the package when nothing installed speaks for it" $
            absentScopePackage moduleScope (answersWith InstAbsentKnown) facts
                `shouldBe` Just "hodatime"

        it "still names it when no source answered at all" $
            absentScopePackage moduleScope [okAnswer "session" []] facts
                `shouldBe` Just "hodatime"

        it "names nothing for a module no indexed package exposes" $
            absentScopePackage
                (Scope (Just "Data.Nowhere") Nothing)
                (answersWith InstAbsentKnown)
                facts
                `shouldBe` (Nothing :: Maybe Text)
