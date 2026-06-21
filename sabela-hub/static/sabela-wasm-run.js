// sabela-wasm-run.js — in-browser MicroHs runtime loader for WASM-mode shares.
//
// Placeholder: the real runtime is produced by the frontend build stream under
// static/src/ and bundled here. The hub serves this file at
// /_hub/assets/sabela-wasm-run.js with a long, immutable Cache-Control.
//
// Until the runtime ships, this no-ops so a spliced share still loads cleanly.
(function () {
  "use strict";
  // The published page embeds its scrubbed source in
  // <script type="application/notebook+markdown" id="sabela-nb-source"> and sets
  // window.SABELA_WASM_SLUG. The runtime will read both on first Run.
})();
