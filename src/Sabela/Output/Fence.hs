{-# LANGUAGE OverloadedStrings #-}

{- | The comments an emitted widget wraps its runtime in. They sit in the
markup so the runtime can be taken back out of a saved notebook by where it
starts and ends rather than by matching the bytes a particular build embedded.
-}
module Sabela.Output.Fence (
    wrapRuntime,
    runtimeOpen,
    runtimeClose,
) where

import Data.Text (Text)

runtimeOpen :: Text -> Text
runtimeOpen name = "/*sabela:runtime:" <> name <> "*/"

runtimeClose :: Text
runtimeClose = "/*:sabela*/"

-- | A runtime fenced by the comments that name it.
wrapRuntime :: Text -> Text -> Text
wrapRuntime name js = runtimeOpen name <> js <> runtimeClose
