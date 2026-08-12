module Siza.Annotate (
    unsignedTopLevelBinds,
    TypeQuery,
    Annotation (..),
    AnnotateReport (..),
    annotateModule,
    annotateCell,
    renderReport,
    annotatedSource,
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHC.Hs as Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)

import Sabela.AI.Capabilities.Edit.CompileGate.Render (isGeneratedBinder)
import Siza.Lang.Haskell (parseModuleE)
import Siza.Language (Diagnostic, renderDiagnostic)

unsignedTopLevelBinds :: Hs.HsModule Hs.GhcPs -> [Text]
unsignedTopLevelBinds m =
    dedup
        [ n
        | n <- valBinds
        , not (n `S.member` signed)
        , not (isGeneratedBinder n)
        ]
  where
    decls = map unLoc (Hs.hsmodDecls m)
    valBinds = concatMap funBindName decls
    signed = S.unions (map sigNames decls)

funBindName :: Hs.HsDecl Hs.GhcPs -> [Text]
funBindName = \case
    Hs.ValD _ Hs.FunBind{Hs.fun_id = lname} -> [rdrText (unLoc lname)]
    _ -> []

sigNames :: Hs.HsDecl Hs.GhcPs -> Set Text
sigNames = \case
    Hs.SigD _ (Hs.TypeSig _ lns _) ->
        S.fromList [rdrText (unLoc ln) | ln <- lns]
    _ -> S.empty

rdrText :: RdrName -> Text
rdrText = T.pack . occNameString . rdrNameOcc

dedup :: [Text] -> [Text]
dedup = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs

type TypeQuery = Text -> IO (Either Text Text)

data Annotation
    = AnnInferred Text Text
    | AnnFailed Text Text
    deriving (Eq, Show)

data AnnotateReport
    = AnnParseError [Diagnostic]
    | AnnReport [Annotation]
    deriving (Eq, Show)

annotateModule :: TypeQuery -> Hs.HsModule Hs.GhcPs -> IO [Annotation]
annotateModule query m = mapM annotate1 (unsignedTopLevelBinds m)
  where
    annotate1 name = do
        r <- query name
        pure $ case r of
            Right ty -> AnnInferred name (T.strip ty)
            Left reason -> AnnFailed name reason

annotateCell :: TypeQuery -> Text -> IO AnnotateReport
annotateCell query src =
    case parseModuleE src of
        Left ds -> pure (AnnParseError ds)
        Right m -> AnnReport <$> annotateModule query m

renderReport :: AnnotateReport -> Text
renderReport = \case
    AnnParseError ds -> T.unlines (map renderDiagnostic ds)
    AnnReport [] -> "-- no unsigned top-level binds"
    AnnReport anns -> T.unlines (map renderAnnotation anns)

renderAnnotation :: Annotation -> Text
renderAnnotation = \case
    AnnInferred name ty -> name <> " :: " <> ty
    AnnFailed name reason -> "-- " <> name <> ": " <> reason

annotatedSource :: AnnotateReport -> Text -> Text
annotatedSource report src = case report of
    AnnParseError _ -> src
    AnnReport anns ->
        let sigs = [name <> " :: " <> ty | AnnInferred name ty <- anns]
         in if null sigs
                then src
                else T.unlines sigs <> src
