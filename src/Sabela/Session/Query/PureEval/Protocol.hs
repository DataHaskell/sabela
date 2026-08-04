{-# LANGUAGE OverloadedStrings #-}

{- | The two @:cmd@ programs a pure evaluation sends and the markers it reads
back. Every name is fully qualified so the candidate's own scope cannot shadow
the protocol out from under it.
-}
module Sabela.Session.Query.PureEval.Protocol (
    admissionCommand,
    evalCommand,
    framed,
    diagnostic,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Errors (scrubHarnessFrames)
import Sabela.Output (
    pureAdmittedMarker,
    pureErrorMarker,
    pureIOMarker,
    pureValueCap,
    pureValueMarker,
 )

admissionCommand :: Text -> Text
admissionCommand expr =
    ":cmd ((\\_sabelaCandidate -> if (Prelude.==) "
        <> "(Data.Typeable.typeRepTyCon (Data.Typeable.typeOf _sabelaCandidate)) "
        <> "(Data.Typeable.typeRepTyCon (Data.Typeable.typeRep "
        <> "(Data.Proxy.Proxy :: Data.Proxy.Proxy (Prelude.IO ())))) "
        <> "then (Prelude.>>) (Prelude.putStrLn "
        <> quoted pureIOMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.show (Data.Typeable.typeOf _sabelaCandidate))) (Prelude.pure \"\")) "
        <> "else (Prelude.>>) (Prelude.putStrLn "
        <> quoted pureAdmittedMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.show (Data.Typeable.typeOf _sabelaCandidate))) (Prelude.pure \"\"))) ("
        <> expr
        <> "))"

evalCommand :: Text -> Text
evalCommand expr =
    ":cmd ((\\_sabelaCandidate -> (Prelude.>>=) "
        <> "((Control.Exception.try (let rendered = Prelude.take "
        <> packed (pureValueCap + 1)
        <> " (Prelude.show _sabelaCandidate) in "
        <> "(Prelude.>>) (Control.Exception.evaluate (Prelude.length rendered)) "
        <> "((Prelude.>>) (Prelude.putStrLn "
        <> quoted pureValueMarker
        <> ") (if (Prelude.>) (Prelude.length rendered) "
        <> packed pureValueCap
        <> " then Prelude.putStrLn ((Prelude.++) (Prelude.take "
        <> packed pureValueCap
        <> " rendered) \"...(truncated)\") else Prelude.putStrLn rendered)))) "
        <> ":: Prelude.IO (Prelude.Either Control.Exception.SomeException ())) "
        <> "(\\result -> case result of { Prelude.Left e -> "
        <> "(Prelude.>>) (Prelude.putStrLn "
        <> quoted pureErrorMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.take "
        <> packed pureValueCap
        <> " (Prelude.show e))) (Prelude.pure \"\")); "
        <> "Prelude.Right () -> Prelude.pure \"\" })) ("
        <> expr
        <> "))"

packed :: (Show a) => a -> Text
packed = T.pack . show

quoted :: Text -> Text
quoted = T.pack . show . T.unpack

framed :: Text -> Text -> Maybe Text
framed marker output = do
    rest <- T.stripPrefix marker (T.stripStart output)
    pure (T.strip rest)

diagnostic :: Text -> Text -> Text
diagnostic out err =
    scrubHarnessFrames
        (T.strip (T.unlines (filter (not . T.null . T.strip) [err, out])))
