-- | Every MCP spec, so the runner names one entry rather than four.
module Test.AllMcpSpecs (allMcpSpecs) where

import Test.Hspec

import Test.McpAuthoringSpec (mcpAuthoringSpec)
import Test.McpBulkSpec (mcpBulkSpec)
import Test.McpCallSpec (mcpCallSpec)
import Test.McpConcurrencySpec (mcpConcurrencySpec)
import Test.McpSpec (mcpSpec)
import Test.McpSurfaceSpec (mcpSurfaceSpec)

allMcpSpecs :: Spec
allMcpSpecs = do
    mcpSpec
    mcpAuthoringSpec
    mcpBulkSpec
    mcpCallSpec
    mcpConcurrencySpec
    mcpSurfaceSpec
