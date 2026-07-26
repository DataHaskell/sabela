{-# LANGUAGE OverloadedStrings #-}

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

grammarImplicated :: Text -> Bool
grammarImplicated err =
    any
        (`T.isInfixOf` T.toLower err)
        ["not in scope", "could not load module", "could not find module"]

rediscoverModules :: Text -> Text -> [(Text, ImportStyle)]
rediscoverModules src err
    | grammarImplicated err = importedModules src
    | otherwise = []

discoverGrammar ::
    (Monad m) => (Text -> m Text) -> [(Text, ImportStyle)] -> m (Maybe Text)
discoverGrammar browse specs = do
    surfaces <- mapM browseOne specs
    let usable = filter (not . T.null . surfBrowse) surfaces
    pure (if null usable then Nothing else Just (synthesizeGrammar usable))
  where
    browseOne (m, style) = Surface m style <$> browse m
