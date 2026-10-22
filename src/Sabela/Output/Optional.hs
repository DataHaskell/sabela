{-# LANGUAGE OverloadedStrings #-}

{- | Display for types the prelude cannot name. An instance for a package's
type only compiles in a session that has the package, so these are injected
beside the prelude rather than inside it: offered to the sessions that can
take them, and never able to fail the prelude for the sessions that cannot.
-}
module Sabela.Output.Optional (
    OptionalDisplay (..),
    optionalDisplays,
    wantedBy,
    declaresPackage,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (collectMetadata)
import Sabela.Model (Notebook (..))
import Sabela.Output.Optional.Viz (vizChartDefs)
import ScriptHs.Parser (CabalMeta (..))

data OptionalDisplay = OptionalDisplay
    { odName :: Text
    -- ^ The package this display is for, as the debug log names it.
    , odDeclared :: Bool
    {- ^ Whether the notebook must declare that package to be offered it. The
    support library is added to every session by the harness, so it need
    not be declared; anything else must be.
    -}
    , odDefs :: Text
    -- ^ The block, injected after the prelude it extends.
    }

optionalDisplays :: [OptionalDisplay]
optionalDisplays =
    [ OptionalDisplay "sabela-notebook" False notebookDefs
    , OptionalDisplay "dataframe" True dataFrameDefs
    , OptionalDisplay "dataframe" True typedDataFrameDefs
    , OptionalDisplay "dataframe" True vizChartDefs
    ]

-- | Whether a notebook is offered one of these displays.
wantedBy :: Notebook -> OptionalDisplay -> Bool
wantedBy nb od = not (odDeclared od) || declaresPackage nb (odName od)

{- | Whether a notebook's cells ask for a package. A dependency is written
with its bounds attached, so only the name is compared.
-}
declaresPackage :: Notebook -> Text -> Bool
declaresPackage nb pkg =
    pkg `elem` map depName (metaDeps (collectMetadata nb))
  where
    depName = T.takeWhile (\c -> c /= ' ' && c /= '>' && c /= '<' && c /= '=')

{- | The notebook vocabulary's own renderables: a picture (which is what every
chart is), and the two markup wrappers it hands back.
-}
notebookDefs :: Text
notebookDefs =
    T.unlines
        [ ":{"
        , "import qualified Sabela.Notebook.Picture as SabelaPicture"
        , "import qualified Sabela.Notebook.Markup as SabelaMarkup"
        , ":}"
        , ":{"
        , "instance Display SabelaPicture.Picture where"
        , "    display p ="
        , "        display"
        , "            ( svg"
        , "                ( SabelaMarkup.unSvg"
        , "                    (SabelaPicture.renderSvg SabelaPicture.defaultCanvas p)"
        , "                )"
        , "            )"
        , "instance Display SabelaMarkup.Svg where"
        , "    display v = display (svg (SabelaMarkup.unSvg v))"
        , "instance Display SabelaMarkup.Html where"
        , "    display v = display (html (SabelaMarkup.unHtml v))"
        , ":}"
        ]

{- | The table view for a real dataframe: the frame's own column names and
column types, its rows as text, and the row count that survives capping,
handed to the same grid every other table uses.
-}
dataFrameDefs :: Text
dataFrameDefs =
    T.unlines
        [ ":{"
        , "import qualified DataFrame as SabelaDF"
        , "import qualified Data.Text as SabelaDFText"
        , ":}"
        , ":{"
        , "_sabelaDFTypes :: SabelaDF.DataFrame -> [SabelaBase.String]"
        , "_sabelaDFTypes df ="
        , "    SabelaBase.map"
        , "        (\\n -> SabelaBase.maybe \"\" SabelaBase.id (SabelaBase.lookup n described))"
        , "        (SabelaDF.columnNames df)"
        , "  where"
        , "    described ="
        , "        [ (SabelaDFText.pack (cell \"Column Name\" row), cell \"Type\" row)"
        , "        | row <- SabelaDF.toRowList (SabelaDF.describeColumns df)"
        , "        ]"
        , "    cell k row ="
        , "        SabelaBase.maybe \"\" SabelaBase.show (SabelaBase.lookup (SabelaDFText.pack k) row)"
        , ":}"
        , ":{"
        , "instance Display SabelaDF.DataFrame where"
        , "    display df ="
        , "        _sabelaFrameSlot df SabelaBase.>>= \\slot -> display"
        , "            ( _sabelaTableWith \"_sabelaDataFrameQuery\" slot"
        , "                (SabelaBase.map SabelaDFText.unpack (SabelaDF.columnNames df))"
        , "                (_sabelaDFTypes df)"
        , "                ( SabelaBase.map"
        , "                    (SabelaBase.map (\\cell -> case cell of (_, v) -> SabelaBase.show v))"
        , "                    (SabelaDF.toRowList (SabelaDF.take _sabelaRowCap df))"
        , "                )"
        , "                (case SabelaDF.dimensions df of (rows, _) -> rows)"
        , "            )"
        , "_sabelaDataFrameQuery :: (Display b) => SabelaBase.String -> (SabelaDF.DataFrame -> b) -> SabelaBase.IO ()"
        , "_sabelaDataFrameQuery = _sabelaFrameQuery"
        , ":}"
        ]

{- | The same table for a schema-typed frame, which is the untyped frame at
runtime. It renders through the instance above, so this block is offered after
it, and a dataframe too old to have @DataFrame.Typed@ loses only this one.
-}
typedDataFrameDefs :: Text
typedDataFrameDefs =
    T.unlines
        [ ":{"
        , "import qualified DataFrame.Typed as SabelaDFTyped"
        , ":}"
        , ":{"
        , "instance Display (SabelaDFTyped.TypedDataFrame cols) where"
        , "    display tdf = display (SabelaDFTyped.thaw tdf)"
        , ":}"
        ]
