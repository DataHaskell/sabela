{- | The provider-neutral cancellation token consulted between streamed chunks
and tool calls. Physically re-exported from the Anthropic request module for
now; it moves here wholesale when that module becomes an adapter (Phase 2). It
was always neutral — it just lived in the wrong place.
-}
module Sabela.LLM.Cancel (
    CancelToken,
    newCancelToken,
    cancel,
    isCancelled,
) where

import Sabela.Anthropic.Types.Request (
    CancelToken,
    cancel,
    isCancelled,
    newCancelToken,
 )
