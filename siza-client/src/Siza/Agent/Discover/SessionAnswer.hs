{-# LANGUAGE OverloadedStrings #-}

{- | What the live session answered, read as hits. A match is one entity the
session's own index found; a card is a listing it browsed. Neither is
paraphrased: every field comes from the answer that stated it.
-}
module Siza.Agent.Discover.SessionAnswer (
    sessionAnswer,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Siza.Agent.Discover.CardRows (cardHits, cardUnparsedNote)
import Siza.Agent.Discover.HitJson (
    baseHit,
    maybeTextAt,
    textAt,
    textAt',
 )
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    SourceAnswer (..),
    okAnswer,
    unavailableAnswer,
 )
import Siza.Agent.Discover.UnitName (scrubCardUnits)

sessionAnswer :: Interpreted -> Maybe Value -> SourceAnswer
sessionAnswer _ Nothing =
    unavailableAnswer
        "session"
        "session unavailable (no live kernel or transport error)"
sessionAnswer interp (Just v@(Object o))
    | Just (Array ms) <- KM.lookup "matches" o =
        okAnswer "session" (map (matchHit interp) (toList ms))
    | Just (String st) <- KM.lookup "status" o = cardAnswer interp st v
    | otherwise = okAnswer "session" []
sessionAnswer _ (Just _) = okAnswer "session" []

cardAnswer :: Interpreted -> Text -> Value -> SourceAnswer
cardAnswer interp st v0 = case (st, scrubCardUnits v0) of
    ("ok", v@(Object _)) ->
        (okAnswer "session" (cardHits interp v))
            { saCard = Just v
            , saNote = cardUnparsedNote v
            }
    (s, v@(Object o))
        | s == diagClassText ClassHiddenPackage ->
            (okAnswer "session" (hiddenHit o)){saCard = Just v}
    ("not-found", Object o) ->
        (okAnswer "session" (suggestHits o))
            { saNote = "module not found; did-you-mean listed"
            }
    (_, Object o) ->
        (okAnswer "session" []){saNote = textAt "message" o}
    _ -> okAnswer "session" []
  where
    hiddenHit o =
        [ (baseHit pkg (textAt "module" o) pkg)
            { dhInstall = InstHidden
            , dhOrigin = "session"
            , dhCabal = Just (textAt "cabal" o)
            , dhKind =
                if pkg == iName interp then MkExact else MkModule
            }
        | let pkg = textAt "package" o
        , not (T.null pkg)
        ]
    suggestHits o =
        [ (baseHit m m (textAt "package" o))
            { dhOrigin = "session"
            , dhKind = MkSynonym
            , dhCabal =
                let cabal = textAt "cabal" o
                 in if T.null cabal then Nothing else Just cabal
            }
        | Just (Array ss) <- [KM.lookup "suggestions" o]
        , String m <- toList ss
        ]

matchHit :: Interpreted -> Value -> DHit
matchHit interp m =
    (baseHit n (textAt' "module" m) (textAt' "package" m))
        { dhType = textAt' "type" m
        , dhInstall = InstInstalled
        , dhOrigin = "session"
        , dhKind = kind
        , dhUse = useLine m
        }
  where
    n = textAt' "name" m
    via = textAt' "via" m
    q = iName interp
    kind
        | via == "synonym" = MkSynonym
        | via == "type" = MkType
        | via == "module" = MkModule
        | n == q = MkExact
        | q `T.isPrefixOf` n = MkPrefix
        | q `T.isInfixOf` n = MkSubstring
        | otherwise = MkSemantic

{- | How to reach the name. Record-update syntax says what a value of some
other type contains, not how to obtain the name, so it only ever trails the
import that does.
-}
useLine :: Value -> Maybe Text
useLine m = case (maybeTextAt "import" m, maybeTextAt "field" m) of
    (Just imp, Just fld) -> Just (imp <> "; record update: " <> fld)
    (imp, _) -> imp
