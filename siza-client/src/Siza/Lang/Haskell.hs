{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | The Haskell 'Language' plug. Parses a cell with @ghc-lib-parser@ — the
SAME parser 'Sabela.Parse' drives, pinned to the same package version — so
the client's pre-flight parse and the server's reactivity parse stay in
lockstep (redesign 6.8).

'Sabela.Parse' only surfaces @(defs, uses)@ and swallows a parse failure
into a fallback, so it cannot report a syntax error or its location. We call
@P.parseModule@ here ourselves to recover the located error on @PFailed@.
The security scan and @:type@ annotate stages plug in alongside this in R3-3
and R3-4.
-}
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

-- | The Haskell pre-flight capability. Only 'langParse' is wired this slice.
haskell :: Language
haskell =
    Language
        { langName = "haskell"
        , langParse = parseHaskell
        }

{- | Parse a cell's Haskell source. @Right ()@ when it parses; @Left ds@
with one located 'Diagnostic' per parser error otherwise. The cell is
wrapped as a synthetic module body, matching 'Sabela.Parse'.
-}
parseHaskell :: Text -> Either [Diagnostic] ()
parseHaskell src = void (parseModuleE src)

{- | Parse a cell to its @ghc-lib-parser@ 'Hs.HsModule' for the security
scan (R3-3). @Left ds@ on a syntax error, mirroring 'parseHaskell'. The
located errors share 'parseHaskell'\'s synthetic-header line correction.
-}
parseModuleE :: Text -> Either [Diagnostic] (Hs.HsModule Hs.GhcPs)
parseModuleE src =
    case P.parseModule (moduleSrc src) dynFlags of
        POk _ m -> Right (unLoc m)
        PFailed pst -> Left (parseErrors pst)

-- | Wrap a REPL-style cell fragment as a parseable module body.
moduleSrc :: Text -> String
moduleSrc src = "module SabelaCell where\n" <> T.unpack src

-- | The located parser errors from a failed parse state.
parseErrors :: PState -> [Diagnostic]
parseErrors pst =
    case map toDiagnostic (bagToList (getMessages (getPsErrorMessages pst))) of
        [] -> [Diagnostic Error Nothing Nothing "parse failed"]
        ds -> ds

{- | One synthetic @module … where@ header line precedes the cell, so the
real-source line number is one less than the reported one.
-}
toDiagnostic :: MsgEnvelope PsMessage -> Diagnostic
toDiagnostic env =
    let (mline, mcol) = cellSpanPos (errMsgSpan env)
     in Diagnostic
            { dgSeverity = Error
            , dgLine = mline
            , dgCol = mcol
            , dgMessage = renderEnvelope env
            }

{- | A span's start position in cell coordinates. The synthetic
@module … where@ header (see 'moduleSrc') sits on line 1, so the cell's
real line is one less than the reported one. 'Nothing' for a non-real span.
-}
cellSpanPos :: SrcSpan -> (Maybe Int, Maybe Int)
cellSpanPos = \case
    RealSrcSpan s _ -> (Just (srcSpanStartLine s - 1), Just (srcSpanStartCol s))
    UnhelpfulSpan _ -> (Nothing, Nothing)

renderEnvelope :: MsgEnvelope PsMessage -> Text
renderEnvelope env =
    T.strip . T.pack . renderWithContext defaultSDocContext $
        formatBulleted
            (diagnosticMessage (defaultDiagnosticOpts @PsMessage) (errMsgDiagnostic env))

{- | Parser settings mirroring 'Sabela.Parse': enable the extensions real
Sabela notebooks rely on so the pre-flight parser never refuses input the
running GHCi would accept.
-}
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
