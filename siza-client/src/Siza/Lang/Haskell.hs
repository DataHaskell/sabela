{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Siza.Lang.Haskell (
    haskell,
    parseHaskell,
    parseModuleE,
    cellSpanPos,
) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import qualified GHC.Hs as Hs
import qualified GHC.LanguageExtensions.Type as LE
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (PState, ParseResult (..), getPsErrorMessages)
import GHC.Types.Error (
    MsgEnvelope (..),
    defaultDiagnosticOpts,
    diagnosticMessage,
    getMessages,
 )
import GHC.Types.SrcLoc (
    SrcSpan (..),
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
 )
import GHC.Utils.Error (formatBulleted)
import GHC.Utils.Outputable (
    defaultSDocContext,
    renderWithContext,
 )

import GHC.Parser.Errors.Ppr ()
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Siza.Language (
    Diagnostic (..),
    Language (..),
    Severity (Error),
 )

haskell :: Language
haskell =
    Language
        { langName = "haskell"
        , langParse = parseHaskell
        }

parseHaskell :: Text -> Either [Diagnostic] ()
parseHaskell src = void (parseModuleE src)

parseModuleE :: Text -> Either [Diagnostic] (Hs.HsModule Hs.GhcPs)
parseModuleE src =
    case P.parseModule (moduleSrc src) dynFlags of
        POk _ m -> Right (unLoc m)
        PFailed pst -> Left (parseErrors pst)

moduleSrc :: Text -> String
moduleSrc src = "module SabelaCell where\n" <> T.unpack src

parseErrors :: PState -> [Diagnostic]
parseErrors pst =
    case map toDiagnostic (bagToList (getMessages (getPsErrorMessages pst))) of
        [] -> [Diagnostic Error Nothing Nothing "parse failed"]
        ds -> ds

toDiagnostic :: MsgEnvelope PsMessage -> Diagnostic
toDiagnostic env =
    let (mline, mcol) = cellSpanPos (errMsgSpan env)
     in Diagnostic
            { dgSeverity = Error
            , dgLine = mline
            , dgCol = mcol
            , dgMessage = renderEnvelope env
            }

cellSpanPos :: SrcSpan -> (Maybe Int, Maybe Int)
cellSpanPos = \case
    RealSrcSpan s _ -> (Just (srcSpanStartLine s - 1), Just (srcSpanStartCol s))
    UnhelpfulSpan _ -> (Nothing, Nothing)

renderEnvelope :: MsgEnvelope PsMessage -> Text
renderEnvelope env =
    T.strip . T.pack . renderWithContext defaultSDocContext $
        formatBulleted
            (diagnosticMessage (defaultDiagnosticOpts @PsMessage) (errMsgDiagnostic env))

dynFlags :: DynFlags
dynFlags = foldl xopt_set (defaultDynFlags fakeSettings) extensions

extensions :: [LE.Extension]
extensions =
    [ LE.TypeApplications
    , LE.OverloadedStrings
    , LE.TemplateHaskell
    , LE.TemplateHaskellQuotes
    , LE.DataKinds
    , LE.PolyKinds
    , LE.RankNTypes
    , LE.GADTs
    , LE.GADTSyntax
    , LE.FlexibleContexts
    , LE.FlexibleInstances
    , LE.MultiParamTypeClasses
    , LE.FunctionalDependencies
    , LE.ScopedTypeVariables
    , LE.ConstraintKinds
    , LE.KindSignatures
    , LE.StandaloneDeriving
    , LE.DeriveGeneric
    , LE.DeriveFunctor
    , LE.DeriveFoldable
    , LE.DeriveTraversable
    , LE.GeneralizedNewtypeDeriving
    , LE.LambdaCase
    , LE.MultiWayIf
    , LE.RecordWildCards
    , LE.NamedFieldPuns
    , LE.TupleSections
    , LE.ViewPatterns
    , LE.BangPatterns
    , LE.ExplicitForAll
    , LE.PatternSynonyms
    , LE.ImportQualifiedPost
    , LE.NumericUnderscores
    , LE.BlockArguments
    , LE.OverloadedRecordDot
    , LE.OverloadedRecordUpdate
    , LE.QualifiedDo
    , LE.LinearTypes
    ]
