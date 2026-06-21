#!/usr/bin/env node
// Bundle the modular frontend sources into the single self-contained HTML
// files that the Haskell binary embeds (see src/Sabela/Server/Static.hs).
//
// Each page lives under static/src/<page>/ as a shell HTML that references
// local partials with ordinary relative tags:
//
//     <link rel="stylesheet" href="css/base.css" />   ← inlined
//     <script src="js/state.js"></script>             ← inlined
//     <script src="https://cdn…/x.js"></script>       ← left as-is (CDN)
//
// Consecutive *local* <link>/<script src> tags are coalesced into a single
// <style>/<script> block, so the emitted file keeps the original single-block
// shape. CDN tags (http/https/protocol-relative) pass through untouched.
//
// Usage:  node tools/build-frontend.mjs [--check]
//   (default)  write static/<page>.html for every page
//   --check    rebuild in memory and fail if any committed file is stale

import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const PAGES = ['index', 'dashboard', 'slideshow'];

// Standalone (non-page) JS bundles: an ordered list of source partials
// concatenated verbatim into one served file. Unlike the three pages there is
// no HTML shell — the order here IS the order. Paths are relative to ROOT.
// The WASM-mode share runtime reuses the shared MIME renderer, so it is listed
// first (it defines renderMimeOutput/mergeOutputs the runtime calls).
const JS_BUNDLES = {
  'static/sabela-wasm-run.js': [
    'static/src/shared/mime-render.js',
    'static/src/wasm-run/js/01-constants.js',
    'static/src/wasm-run/js/02-source-island.js',
    'static/src/wasm-run/js/03-mhs-engine.js',
    'static/src/wasm-run/js/04-program.js',
    'static/src/wasm-run/js/05-fork-nudge.js',
    'static/src/wasm-run/js/06-render-shim.js',
    'static/src/wasm-run/js/07-ui.js',
    'static/src/wasm-run/js/08-run.js',
    'static/src/wasm-run/js/09-init.js',
  ],
};

const isRemote = (url) => /^(https?:)?\/\//i.test(url) || /^data:/i.test(url);

// A local asset reference looks like one of our partials; used by the
// post-build guard to catch a reference the bundler failed to inline (e.g. a
// tag prettier wrapped across lines, so classify() didn't match it).
const LOCAL_REF = /(?:href|src)=["'](?:css|js|html)\/|<!--\s*include:/;

// Read a partial, failing with a message that names the shell and reference
// instead of a bare ENOENT stack trace (the most likely authoring mistake).
function readPartial(shellDir, page, ref) {
  try {
    return readFileSync(join(shellDir, ref), 'utf8');
  } catch {
    throw new Error(`${page}: cannot read partial "${ref}" referenced in ${page}.html`);
  }
}

// Classify a shell line as a local CSS link, a local JS script, or neither.
// Remote (CDN) tags return null so they pass through verbatim.
function classify(line) {
  const css = line.match(/^(\s*)<link\b[^>]*\bhref=["']([^"']+)["'][^>]*>\s*$/);
  if (css && /rel=["']stylesheet["']/.test(line) && !isRemote(css[2])) {
    return { type: 'style', indent: css[1], file: css[2] };
  }
  const js = line.match(/^(\s*)<script\b[^>]*\bsrc=["']([^"']+)["'][^>]*>\s*<\/script>\s*$/);
  if (js && !isRemote(js[2])) {
    return { type: 'script', indent: js[1], file: js[2] };
  }
  return null;
}

// Expand the shell for one page into the bundled HTML string.
function bundle(page) {
  const shellPath = join(ROOT, 'static', 'src', page, `${page}.html`);
  const shellDir = dirname(shellPath);
  const lines = readFileSync(shellPath, 'utf8').split('\n');

  const out = [];
  let run = null; // { type, indent, files: [] }

  const flush = () => {
    if (!run) return;
    // Force a trailing newline per partial so a missing one can never fuse two
    // files (or fuse the last JS file into the closing </script>).
    const body = run.files
      .map((f) => readPartial(shellDir, page, f))
      .map((t) => (t.endsWith('\n') ? t : t + '\n'))
      .join('');
    out.push(`${run.indent}<${run.type}>\n${body}${run.indent}</${run.type}>`);
    run = null;
  };

  for (const line of lines) {
    const inc = line.match(/^\s*<!--\s*include:\s*(\S+)\s*-->\s*$/);
    if (inc) {
      flush();
      const frag = readPartial(shellDir, page, inc[1]);
      out.push(frag.replace(/\n$/, ''));
      continue;
    }
    const hit = classify(line);
    if (hit) {
      if (run && run.type !== hit.type) flush();
      if (!run) run = { type: hit.type, indent: hit.indent, files: [] };
      run.files.push(hit.file);
    } else {
      flush();
      out.push(line);
    }
  }
  flush();
  // Banner after the doctype warns editors this file is a build artifact.
  const banner = `<!-- AUTO-GENERATED from static/src/${page}/ — do not edit. Edit the sources there and run: node tools/build-frontend.mjs -->`;
  if (out[0] && /^<!doctype/i.test(out[0])) out.splice(1, 0, banner);
  const result = out.join('\n');
  // Fail loud: a local partial reference must never survive into the output.
  const leaked = result.split('\n').find((l) => LOCAL_REF.test(l));
  if (leaked) {
    throw new Error(`${page}: unresolved local reference left in output: ${leaked.trim()}`);
  }
  return result;
}

// Concatenate an ordered list of source partials into one bundle string,
// forcing a trailing newline per file so a missing one can't fuse two.
function bundleJs(files) {
  const banner =
    '// AUTO-GENERATED from static/src/ — do not edit. ' +
    'Edit the partials and run: node tools/build-frontend.mjs\n';
  const body = files
    .map((f) => {
      try {
        return readFileSync(join(ROOT, f), 'utf8');
      } catch {
        throw new Error(`cannot read bundle partial "${f}"`);
      }
    })
    .map((t) => (t.endsWith('\n') ? t : t + '\n'))
    .join('');
  return banner + body;
}

const check = process.argv.includes('--check');
let stale = 0;

for (const page of PAGES) {
  const built = bundle(page);
  const dest = join(ROOT, 'static', `${page}.html`);
  if (check) {
    const current = readFileSync(dest, 'utf8');
    if (current !== built) {
      console.error(`✗ ${page}.html is stale — run: node tools/build-frontend.mjs`);
      stale += 1;
    }
  } else {
    writeFileSync(dest, built);
    console.log(`✓ built static/${page}.html`);
  }
}

for (const [dest, files] of Object.entries(JS_BUNDLES)) {
  const built = bundleJs(files);
  const destPath = join(ROOT, dest);
  if (check) {
    const current = readFileSync(destPath, 'utf8');
    if (current !== built) {
      console.error(`✗ ${dest} is stale — run: node tools/build-frontend.mjs`);
      stale += 1;
    }
  } else {
    writeFileSync(destPath, built);
    console.log(`✓ built ${dest}`);
  }
}

if (check && stale === 0) console.log('✓ all bundled frontend output is up to date');
process.exit(stale > 0 ? 1 : 0);
