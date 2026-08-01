# Sabela convenience targets. Most day-to-day commands are still plain
# `cabal` / `./scripts/*.sh` (see CLAUDE.md); this Makefile exists mainly
# to make rebuilding the embedded frontend and search cache discoverable.

.PHONY: help frontend frontend-check hub-assets search-cache capability-index \
        search-cache-local test test-fast payload-probe

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-16s\033[0m %s\n", $$1, $$2}'

test: ## Run the whole suite, integration specs included (slow: real cabal builds + GHCi)
	cabal test

test-fast: ## Run the suite without the integration specs (SABELA_SKIP_LIVE marks them pending)
	SABELA_SKIP_LIVE=1 cabal test

payload-probe: ## Payload-truth gate: every discover answer over the query grid, checked against ghc-pkg and :browse
	./scripts/payload-probe.sh

frontend: ## Rebuild the embedded HTML (static/*.html) + the WASM-run bundle (static/sabela-wasm-run.js) from static/src/
	node tools/build-frontend.mjs
	@echo "Now rebuild sabela (cabal build) so the embedded pages pick up changes."

frontend-check: ## Fail if any static/*.html or the WASM-run bundle is stale vs its static/src/ partials
	node tools/build-frontend.mjs --check

hub-assets: ## Refresh the hub's WASM assets (sabela-hub/static/) so it builds/deploys self-contained
	node tools/build-frontend.mjs
	cp static/sabela-wasm-run.js sabela-hub/static/sabela-wasm-run.js
	@test -f static/mhs-embed.js \
		&& cp static/mhs-embed.js sabela-hub/static/mhs-embed.js \
		&& echo "copied mhs-embed.js from static/" \
		|| echo "static/mhs-embed.js absent; keeping the committed sabela-hub/static/mhs-embed.js (vendored runtime)"
	@echo "Committed sabela-hub/static/* is what the hub Dockerfile bundles into /opt/static."

search-cache: ## Build/refresh the LOCAL Hoogle + Hackage-names cache the resolver queries (no network at run time)
	./tools/update-search-cache.sh
	@echo "Cache refreshed. Queries hit the local DB only — never the public services."

search-cache-local: ## Rebuild only data/hoogle-local.hoo (in-repo + store packages); skips the full-Hackage download
	./tools/update-search-cache.sh --local-hoogle-only
	@echo "Local index rebuilt. The server adopts it at start when SABELA_HOOGLE_LOCAL_DB is unset."

capability-index: ## Build/refresh the SHIP capability-search index (data/capability-*); needs a local ollama (nomic-embed-text)
	./tools/update-search-cache.sh --capability-index
	@echo "Capability index built under data/. search_capability uses tools/capability_search.hs over it."
