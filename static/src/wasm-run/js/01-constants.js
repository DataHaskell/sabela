// sabela-wasm-run.js — in-browser MicroHs runtime for WASM-mode shares.
//
// Runs on a published share's static export page. The page has, right after
// <body>: an inert source island
//   <script type="application/notebook+markdown" id="sabela-nb-source"
//           data-slug="…">…HTML-escaped markdown…</script>
// a defer-loaded reference to this file, and window.SABELA_WASM_SLUG.
//
// This file is a plain (non-module) script: all partials share one global
// scope, exactly like the editor bundles. It REUSES the shared MIME renderer
// (renderMimeOutput / mergeOutputs, concatenated ahead of these partials).
//
// ── MicroHs embed: the ONE place the real artifact is named ──────────────
// The ~2.6MB emscripten/web build of MicroHs (0.16.5.0, augustss's web-mhs) is
// a single self-contained file — wasm + base library are INLINED, no .wasm/.data
// companions. It is a vendored binary (gitignored), dropped at static/mhs-embed.js
// and served by the hub at /_hub/assets/mhs-embed.js.
//
// GLOBAL-MODULE CONTRACT (not a MODULARIZE factory): the embed begins with
//   var Module = typeof Module != "undefined" ? Module : {};
// so it ADOPTS a `Module` you set up BEFORE its <script> loads. We run it as ONE
// persistent interactive `mhsi` REPL (Module.arguments=[]): base loads once, then
// each input evaluates in ~90ms (vs ~2s re-instantiating `mhs -r` per run). See
// 03-mhs-engine.js for the session + the non-obvious requirement that its iframe
// stay RENDERED (real area, not display:none) or Chrome throttles it and the
// REPL stalls.
const MHS_EMBED = {
  // URL the runtime lazy-injects to boot the session.
  url: '/_hub/assets/mhs-embed.js',
  // CWD the base library is found relative to; preRun must chdir here.
  home: '/home/web_user',
};

// Signatures that mean "MicroHs cannot run this — needs full Sabela". Matched
// against REPL stderr. Kept conservative: a bare "undefined value" is a normal
// transient in a stateful REPL (a name not yet fed), NOT a reason to fork.
const UNSUPPORTED_STDERR = [
  /module .* not found/i,
  /cannot find module/i,
  /could not (find|load) module/i,
  /TemplateHaskell/i,
];
const UNSUPPORTED_IMPORTS = [
  /^\s*import\s+(qualified\s+)?Data\.Map\b/m,
  /^\s*import\s+(qualified\s+)?Data\.Set\b/m,
  /^\s*import\s+(qualified\s+)?System\.Random\b/m,
  /^\s*import\s+(qualified\s+)?Control\.Monad\.State\b/m,
  /^\s*import\s+(qualified\s+)?Data\.Vector\b/m,
];
// A `-- cabal:` metadata import means the cell pulls a Hackage package; MicroHs
// has no package manager, so route straight to the Fork nudge.
const CABAL_META = /^\s*--\s*cabal:/m;
