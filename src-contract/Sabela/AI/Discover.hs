{-# LANGUAGE OverloadedStrings #-}

{- | Live-@:browse@ grammar discovery: parse a cell's imports, browse those
modules, and synthesise a grammar of their signatures. The browse step is
injected, so product, harness, and tests supply their own.
-}
module Sabela.AI.Discover (
    importedModules,
    rediscoverModules,
    grammarImplicated,
    discoverGrammar,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar (ImportStyle (..))
import Sabela.AI.Grammar.Synth (Surface (..), synthesizeGrammar)

-- | The (module, import-style) pairs a cell's @import@ lines bring into scope.
importedModules :: Text -> [(Text, ImportStyle)]
importedModules src =
    [m | l <- T.lines src, Just m <- [importedModule (T.strip l)]]

importedModule :: Text -> Maybe (Text, ImportStyle)
importedModule l = case T.words l of
    ("import" : "qualified" : m : rest) -> Just (m, QualifiedAs (alias m rest))
    ("import" : m : _) | m /= "qualified" -> Just (m, Unqualified)
    _ -> Nothing
  where
    alias _ ("as" : p : _) = p
    alias m _ = m

{- | Whether an error implicates the live grammar (a name the model got wrong),
so re-browsing the cell's modules is worth doing.
-}
grammarImplicated :: Text -> Bool
grammarImplicated err =
    any (`T.isInfixOf` T.toLower err) ["not in scope", "no instance for"]

{- | Modules to re-browse for a red cell: its imports, when the error implicates
the grammar; none otherwise.
-}
rediscoverModules :: Text -> Text -> [(Text, ImportStyle)]
rediscoverModules src err
    | grammarImplicated err = importedModules src
    | otherwise = []

{- | Synthesise a live grammar by browsing each module with the injected browse
function. 'Nothing' when nothing browses to a usable surface.
-}
discoverGrammar ::
    (Monad m) => (Text -> m Text) -> [(Text, ImportStyle)] -> m (Maybe Text)
discoverGrammar browse specs = do
    surfaces <- mapM browseOne specs
    let usable = filter (not . T.null . surfBrowse) surfaces
    pure (if null usable then Nothing else Just (synthesizeGrammar usable))
  where
    browseOne (m, style) = Surface m style <$> browse m
