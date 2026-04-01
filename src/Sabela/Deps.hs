module Sabela.Deps (
    collectMetadata,
    collectMetadataFromContent,
    mergedMeta,
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import ScriptHs.Markdown (Segment (..), parseMarkdown)
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )

collectMetadata :: Notebook -> CabalMeta
collectMetadata nb =
    let allCode =
            filter (\c -> cellType c == CodeCell && cellLang c == ST.Haskell) (nbCells nb)
     in mergeMetas [(scriptMeta . parseScript) (cellSource c) | c <- allCode]

collectMetadataFromContent :: Text -> CabalMeta
collectMetadataFromContent content =
    let segs = parseMarkdown content
        codeSrcs = [src | CodeBlock _ src _ <- segs]
     in mergeMetas (map (scriptMeta . parseScript) codeSrcs)

mergedMeta :: Set Text -> CabalMeta -> CabalMeta
mergedMeta globalDeps meta =
    meta{metaDeps = S.toList (S.fromList (metaDeps meta) <> globalDeps)}
