# Sabela convenience targets. Most day-to-day commands are still plain
# `cabal` / `./scripts/*.sh` (see CLAUDE.md); this Makefile exists mainly
# to make regenerating the embedded API reference discoverable.

.PHONY: help api-reference frontend frontend-check

help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-16s\033[0m %s\n", $$1, $$2}'

frontend: ## Rebuild the embedded HTML (static/*.html) + the WASM-run bundle (static/sabela-wasm-run.js) from static/src/
	node tools/build-frontend.mjs
	@echo "Now rebuild sabela (cabal build) so the embedded pages pick up changes."

frontend-check: ## Fail if any static/*.html or the WASM-run bundle is stale vs its static/src/ partials
	node tools/build-frontend.mjs --check

api-reference: ## Regenerate data/api-reference.txt from dataframe/granite (rerun when those packages change)
	./tools/gen-api-reference.sh
	@echo "Now rebuild sabela (cabal build) so the embedded card picks up changes."
