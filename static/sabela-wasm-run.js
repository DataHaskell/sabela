// AUTO-GENERATED from static/src/ — do not edit. Edit the partials and run: node tools/build-frontend.mjs
// ── Shared MIME-aware output rendering ───────────────────────────
// Pure renderer shared by the editor (07-output.js) and the WASM-mode
// share runtime (sabela-wasm-run.js) so browser output matches the editor.
// Runtime globals it relies on are provided by whichever page hosts it:
//   iframeBaseStyle()        — editor: 19-theme.js; wasm-run: its own shim
//   renderMathInMarkdown()   — editor: 17-render-prose.js; wasm-run: shim
//   _activeTextInput         — editor only; wasm-run leaves it undefined
//   marked, katex            — loaded from CDN on every hosting page
function mergeOutputs(outputs) {
  const merged = [];
  for (const item of outputs) {
    const last = merged[merged.length - 1];
    if (last && last.oiMime === item.oiMime && item.oiMime === 'text/html') {
      merged[merged.length - 1] = {
        oiMime: item.oiMime,
        oiOutput: last.oiOutput + '\n' + item.oiOutput,
      };
    } else {
      merged.push({ ...item });
    }
  }
  return merged;
}

function renderMimeOutput(container, content, mime) {
  if (mime === 'text/html') {
    container.className = 'cell-output mime-html';
    let iframe = container.querySelector('iframe');
    const isNew = !iframe;
    if (isNew) {
      iframe = document.createElement('iframe');
      iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin');
      container.appendChild(iframe);
    }
    requestAnimationFrame(() => {
      const doc = iframe.contentDocument || iframe.contentWindow?.document;
      if (!doc) {
        console.error('Cannot access iframe document');
        return;
      }
      iframe.dataset.lastContent = content;
      if (isNew) iframe.style.height = '0px';
      doc.open();
      iframe.onload = () => {
        // Collapse to 1px before reading scrollHeight so the viewport does not
        // inflate doc.body.scrollHeight — all three assignments are synchronous
        // inside this onload callback so the browser never paints the 1px state.
        iframe.style.height = '1px';
        iframe.style.height = Math.max(60, doc.body.scrollHeight + 20) + 'px';
      };
      doc.write(iframeBaseStyle() + content);
      doc.close();
      const ati = _activeTextInput;
      if (ati && ati.cellId === container.dataset.cellId) {
        _activeTextInput = null;
        setTimeout(() => {
          const d = iframe.contentDocument || iframe.contentWindow?.document;
          const inp = d && d.querySelector('input[type=text]');
          if (inp) {
            inp.focus();
            inp.setSelectionRange(ati.sel, ati.sel);
          }
        }, 0);
      }
    });
  } else if (mime === 'text/markdown') {
    container.innerHTML = '';
    container.className = 'cell-output mime-markdown';
    // Render KaTeX before marked so $$...$$/$...$ survive marked's
    // underscore/star handling; code spans are protected so dollar signs
    // inside them are never treated as math delimiters.
    const md = renderMathInMarkdown(content);
    container.innerHTML = marked.parse(md);
    container.querySelectorAll('table').forEach((t) => {
      const w = document.createElement('div');
      w.className = 'table-wrapper';
      t.parentNode.insertBefore(w, t);
      w.appendChild(t);
    });
  } else if (mime === 'image/svg+xml') {
    container.innerHTML = '';
    container.className = 'cell-output mime-svg';
    container.innerHTML = content;
  } else if (mime.startsWith('image/') && mime.includes('base64')) {
    container.innerHTML = '';
    container.className = 'cell-output mime-image';
    const mimeClean = mime.replace(';base64', '');
    container.innerHTML = `<img src="data:${mimeClean};base64,${content.trim()}" />`;
  } else if (mime === 'application/json') {
    container.innerHTML = '';
    container.className = 'cell-output mime-json';
    try {
      container.textContent = JSON.stringify(JSON.parse(content), null, 2);
    } catch {
      container.textContent = content;
    }
  } else if (mime === 'text/latex') {
    container.innerHTML = '';
    container.className = 'cell-output mime-latex';
    try {
      katex.render(content, container, { displayMode: true, throwOnError: false });
    } catch (e) {
      container.textContent = content;
    }
  } else {
    container.innerHTML = '';
    container.className = 'cell-output';
    container.textContent = content;
  }
}
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
// ── Source island: read + parse the notebook markdown ────────────────────

// The hub HTML-escapes the markdown before embedding it in the <script> island
// (so a cell containing </script> or & can't break out). A <script> element's
// .textContent is RAW — the parser does NOT decode entities inside it — so we
// must reverse the escaping ourselves, or source like `a && b` arrives as
// `a &amp;&amp; b` and fails to compile. The textarea/innerHTML round-trip is the
// browser's own entity decoder.
function decodeEntities(s) {
  const ta = document.createElement('textarea');
  ta.innerHTML = s;
  return ta.value;
}
function readSourceIsland() {
  const el = document.getElementById('sabela-nb-source');
  if (!el) return { source: '', slug: resolveSlug(null) };
  return { source: decodeEntities(el.textContent || ''), slug: resolveSlug(el) };
}

// Slug from window.SABELA_WASM_SLUG, falling back to the island's data-slug.
function resolveSlug(islandEl) {
  if (typeof window !== 'undefined' && window.SABELA_WASM_SLUG) {
    return String(window.SABELA_WASM_SLUG);
  }
  return (islandEl && islandEl.getAttribute('data-slug')) || '';
}

// Parse fenced ```haskell blocks into ordered cells. A cell is { index, code }.
// Only haskell/hs fences become runnable cells; prose and other languages are
// ignored (the export DOM already renders them). Mirrors the notebook format:
// code blocks become executable cells.
function parseHaskellCells(source) {
  const lines = source.split('\n');
  const cells = [];
  let inFence = false;
  let buf = [];
  for (const line of lines) {
    const open = line.match(/^\s*```+\s*([A-Za-z0-9_+-]*)\s*$/);
    if (!inFence && open) {
      const lang = (open[1] || '').toLowerCase();
      if (lang === 'haskell' || lang === 'hs') {
        inFence = true;
        buf = [];
      }
      continue;
    }
    if (inFence && /^\s*```+\s*$/.test(line)) {
      inFence = false;
      cells.push({ index: cells.length, code: buf.join('\n') });
      buf = [];
      continue;
    }
    if (inFence) buf.push(line);
  }
  return cells;
}
// ── MhsEngine: ONE persistent interactive MicroHs (mhsi) session ─────────
//
// MicroHs 0.16.5.0 (augustss's web-mhs) is a classic global-Module emscripten
// build. The old design re-instantiated it per run (`mhs -r <module>`), which
// re-loads/links the base library EVERY run (~2s). Profiling: instantiation is
// 49ms; the ~2s is base processing per run.
//
// Instead we boot ONE long-lived `mhsi` REPL (Module.arguments=[]) in a single
// kept-alive iframe. Base loads ONCE; then each input evaluates in ~90ms. We feed
// input via Module._set_input_char and read output from the FS.init stdout
// callback (the iframe is same-origin, so the parent reads window.__so directly).
//
//     MhsEngine.load()           -> Promise<void>            (boot/warm; memoised)
//     MhsEngine.available        -> boolean                  (session ready)
//     MhsEngine.feed(lines)      -> Promise<{ out, err }>    (run REPL inputs)
//     MhsEngine.reset()          -> Promise<void>            (:clear all defs)
//
// feed() NEVER throws for ordinary Haskell errors — they come back in `err`
// (mhsi prints `*** Exception …` and keeps the session alive). It rejects only if
// the engine never boots / a feed times out (caller shows a Fork nudge).

const MHS_BOOT_TIMEOUT_MS = 60000; // cold start: download + instantiate + base
const MHS_FEED_TIMEOUT_MS = 60000; // a single feed (covers the first-eval warmup)

const MhsEngine = (function () {
  let frame = null;
  let bootPromise = null;
  let ready = false;
  let feedId = 0;
  let chain = Promise.resolve(); // serialise feeds so they never interleave

  // Rendered-but-invisible frame. It MUST stay schedulable (real area, near-zero
  // opacity, top z-index) or Chrome throttles the idle session and feeds stall.
  function ensureFrame() {
    if (frame && frame.isConnected) return frame;
    frame = document.createElement('iframe');
    frame.style.cssText =
      'position:fixed;right:0;bottom:0;width:200px;height:120px;opacity:0.01;' +
      'border:0;pointer-events:none;z-index:2147483647';
    document.body.appendChild(frame);
    return frame;
  }

  // The session document: boot mhsi interactive, mirror stdout/stderr into
  // window.__so/__se (the parent reads them same-origin), no exit hooks.
  function sessionDoc() {
    return [
      '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body><script>',
      '(function(){',
      '  window.__so="";window.__se="";',
      '  function so(c){if(c!==null)window.__so+=String.fromCharCode(c);}',
      '  function se(c){if(c!==null)window.__se+=String.fromCharCode(c);}',
      '  window.Module={arguments:[],preRun:[function(){',
      '    Module.FS.init(function(){return null;},so,se);',
      '    Module.FS.chdir(' + JSON.stringify(MHS_EMBED.home) + ');',
      '  }],print:function(){},printErr:function(){}};',
      '  var s=document.createElement("script");s.src=' + JSON.stringify(MHS_EMBED.url) + ';',
      '  s.onerror=function(){window.__se+="failed to load MicroHs embed\\n";};',
      '  document.body.appendChild(s);',
      '})();',
      '<' + '/script></body></html>',
    ].join('\n');
  }

  function load() {
    if (bootPromise) return bootPromise;
    bootPromise = new Promise((resolve, reject) => {
      const f = ensureFrame();
      f.srcdoc = sessionDoc();
      const poll = setInterval(() => {
        const w = f.contentWindow;
        // ready when the prompt has printed and the input hook exists
        if (
          w &&
          w.__so &&
          w.__so.indexOf('> ') >= 0 &&
          w.Module &&
          typeof w.Module._set_input_char === 'function'
        ) {
          clearInterval(poll);
          clearTimeout(to);
          ready = true;
          resolve();
        }
      }, 30);
      const to = setTimeout(() => {
        clearInterval(poll);
        reject(new Error('MicroHs session boot timed out'));
      }, MHS_BOOT_TIMEOUT_MS);
    });
    return bootPromise;
  }

  function sendLine(w, s) {
    for (let i = 0; i < s.length; i++) w.Module._set_input_char(s.charCodeAt(i));
    w.Module._set_input_char(10);
  }

  function waitFor(pred, ms) {
    return new Promise((resolve, reject) => {
      const poll = setInterval(() => {
        if (pred()) {
          clearInterval(poll);
          clearTimeout(to);
          resolve();
        }
      }, 15);
      const to = setTimeout(() => {
        clearInterval(poll);
        reject(new Error('MicroHs feed timed out'));
      }, ms);
    });
  }

  // Strip the echoed input (lines after a `> ` prompt) and blanks; route mhsi
  // error lines to `err`, everything else to `out`.
  function parseRegion(region) {
    const out = [];
    const err = [];
    for (const line of region.split('\n')) {
      if (line.startsWith('> ') || line.length === 0) continue;
      if (/\*\*\* Exception|(^|\s)error:|no location:/.test(line)) err.push(line);
      else out.push(line);
    }
    return { out: out.join('\n'), err: err.join('\n') };
  }

  // Feed REPL input lines, bracketed by printed sentinels so the captured region
  // excludes prompts/echoes cleanly. Resolves with the cell's output.
  function feedNow(lines) {
    return (async () => {
      await load();
      const w = frame.contentWindow;
      const id = ++feedId;
      const S = '@@SWS' + id + '@@';
      const E = '@@SWE' + id + '@@';
      const mark = w.__so.length;
      sendLine(w, 'putStrLn "' + S + '"');
      for (const l of lines) if (l.trim().length) sendLine(w, l);
      sendLine(w, 'putStrLn "' + E + '"');
      // The PRINTED sentinel is the token followed by a newline; the echo is the
      // token followed by a quote, so `E + "\n"` matches only the print.
      await waitFor(() => w.__so.indexOf(E + '\n', mark) >= 0, MHS_FEED_TIMEOUT_MS);
      const buf = w.__so;
      const sStart = buf.indexOf(S + '\n', mark);
      const from = sStart >= 0 ? sStart + (S + '\n').length : mark;
      const to = buf.indexOf(E + '\n', from);
      return parseRegion(buf.slice(from, to));
    })();
  }

  function feed(lines) {
    chain = chain.then(
      () => feedNow(lines),
      () => feedNow(lines)
    );
    return chain;
  }

  async function reset() {
    await load();
    return feed([':clear']);
  }

  return {
    load,
    feed,
    reset,
    get available() {
      return ready;
    },
  };
})();
// ── Cell → REPL input + dependency tracking ──────────────────────────────
//
// The runner drives ONE persistent `mhsi` REPL (see 03-mhs-engine.js). Each cell
// is fed as REPL input — definitions accumulate, expressions print. Dependency
// tracking decides which cells to (re)feed so an edit only re-runs its dependents.

// A cell is a declaration if it binds/imports/declares rather than being a bare
// expression. (Kept for classification by other partials.)
function isDeclarationCell(code) {
  const lines = code.split('\n').filter((l) => l.trim() && !/^\s*--/.test(l));
  if (lines.length === 0) return true; // empty → nothing to print
  if (/^\s*import\b/m.test(code)) return true;
  if (/^(data|newtype|type|class|instance|infix|infixl|infixr)\b/m.test(code)) return true;
  // A binding/signature at column 0 (e.g. `foo = …`, `foo :: …`, `foo x = …`).
  if (/^[A-Za-z_(][^\n]*?(::|=)/m.test(lines[0]) && !/^\s/.test(lines[0])) return true;
  return false;
}

// A ghci-style `let x = e` (no `in`): goes into main's do-block, not module
// scope (`let x = e` is illegal at the top level), and stays in scope for every
// later cell's statement.
function isLetCell(code) {
  return /^\s*let\b/.test(code) && !/\bin\b/.test(code);
}

// ── Dependency tracking (mirrors src/Sabela/Topo.hs) ─────────────────────────
// A cell depends on whichever cell first defines a name it uses; editing a cell
// only re-runs its transitive dependents, not the whole notebook.

// Top-level names a cell defines: bindings/signatures at column 0, data/newtype
// (+ their constructors), type/class names, and ghci `let x = …`.
function cellDefs(code) {
  const defs = new Set();
  for (const raw of code.split('\n')) {
    const line = raw.replace(/--.*$/, '');
    let m;
    if ((m = line.match(/^\s*(?:data|newtype)\s+([A-Z][\w']*)/))) {
      defs.add(m[1]);
      const rhs = line.slice(line.indexOf('=') + 1);
      if (line.includes('=')) for (const c of rhs.matchAll(/\b([A-Z][\w']*)\b/g)) defs.add(c[1]);
    } else if ((m = line.match(/^\s*(?:type|class)\s+([A-Z][\w']*)/))) {
      defs.add(m[1]);
    } else if ((m = line.match(/^\s*let\s+([a-z_][\w']*)\s*=/))) {
      defs.add(m[1]);
    } else if ((m = line.match(/^([a-z_][\w']*)\s*(?:::|[^=]*=(?!=))/))) {
      defs.add(m[1]);
    }
  }
  return defs;
}

// Every identifier a cell references (comments stripped).
function cellUses(code) {
  const uses = new Set();
  for (const m of code.replace(/--.*$/gm, '').matchAll(/[A-Za-z_][\w']*/g)) uses.add(m[0]);
  return uses;
}

// Dependency graph in BOTH directions. First cell to define a name owns it
// (first-wins, like Topo's defMap). revDeps: definer → its dependents (downstream,
// for "what to re-run on edit"); fwdDeps: cell → cells it depends on (upstream,
// for "what must be fed first" in the stateful REPL).
function buildDeps(cells) {
  const defMap = new Map();
  cells.forEach((c, i) => {
    for (const name of cellDefs(c.code)) if (!defMap.has(name)) defMap.set(name, i);
  });
  const revDeps = new Map();
  const fwdDeps = new Map();
  cells.forEach((c, i) => {
    for (const name of cellUses(c.code)) {
      const dk = defMap.get(name);
      if (dk !== undefined && dk !== i) {
        if (!revDeps.has(dk)) revDeps.set(dk, new Set());
        revDeps.get(dk).add(i);
        if (!fwdDeps.has(i)) fwdDeps.set(i, new Set());
        fwdDeps.get(i).add(dk);
      }
    }
  });
  return { revDeps, fwdDeps };
}

// Transitive dependents of `root` (including root itself).
function affectedFrom(root, revDeps) {
  const seen = new Set([root]);
  const queue = [root];
  while (queue.length) {
    for (const d of revDeps.get(queue.shift()) || []) {
      if (!seen.has(d)) {
        seen.add(d);
        queue.push(d);
      }
    }
  }
  return seen;
}

// Turn a cell's source into REPL input lines. The mhsi REPL is line-oriented and
// rejects a single definition split across physical lines (layout), but each
// column-0 clause is its own input and multi-clause defs accumulate — so:
//   - group indented continuation lines into their column-0 head (collapse layout,
//     which handles guards/where on separate lines),
//   - keep distinct column-0 lines as separate inputs (multi-clause functions),
//   - rewrite ghci `let x = e` to `x = e` (the REPL wants a bare DEFN).
// Known gaps: `do`/`case` blocks split across lines need `;` (collapse with a
// space breaks them) — such a cell errors visibly without killing the session.
function cellToReplLines(code) {
  const units = [];
  for (const raw of code.split('\n')) {
    const line = raw.replace(/\s+$/, '');
    if (!line.trim() || /^\s*--/.test(line)) continue;
    if (/^\s/.test(line) && units.length) units[units.length - 1] += ' ' + line.trim();
    else units.push(line.trim());
  }
  return units.map((u) => {
    const m = u.match(/^let\s+(.+)$/);
    return m && !/\bin\b/.test(u) ? m[1] : u;
  });
}
// ── Fork nudge: route unsupported cells to the full toolchain ────────────

// Does this cell need full Sabela (a Hackage import MicroHs can't satisfy)?
// Checked up front, before compiling, on the cell's own source.
function cellNeedsFork(code) {
  if (CABAL_META.test(code)) return true;
  return UNSUPPORTED_IMPORTS.some((re) => re.test(code));
}

// Does this compiler stderr look like an unsupported-feature/missing-package
// miss (vs an ordinary type error the author can fix in-browser)?
function stderrNeedsFork(stderr) {
  if (!stderr) return false;
  return UNSUPPORTED_STDERR.some((re) => re.test(stderr));
}

// A per-cell "needs full Sabela" prompt: a POST form to /_hub/fork/<slug>. The
// collection reader iframe already grants allow-forms, so the submit works
// inside it. Opens in a new tab to match the share banner's fork button.
function forkNudge(slug) {
  const wrap = document.createElement('div');
  wrap.className = 'swasm-fork';
  const msg = document.createElement('span');
  msg.className = 'swasm-fork-msg';
  msg.textContent = 'This needs full Sabela — ';
  const form = document.createElement('form');
  form.method = 'post';
  form.action = '/_hub/fork/' + encodeURIComponent(slug);
  form.target = '_blank';
  form.style.display = 'inline';
  const btn = document.createElement('button');
  btn.type = 'submit';
  btn.className = 'swasm-fork-btn';
  btn.textContent = 'Fork to run ▸';
  form.appendChild(btn);
  wrap.appendChild(msg);
  wrap.appendChild(form);
  return wrap;
}
// ── Shims for the shared MIME renderer (mime-render.js) ───────────────────
//
// The shared renderer calls a few globals that, in the editor, live in other
// partials (19-theme.js, 17-render-prose.js). The export page does not load
// those, so we provide minimal stand-ins with the same names. `_activeTextInput`
// is editor-only state; here it stays a permanently-null global so the
// renderer's focus-restore branch is simply skipped.
var _activeTextInput = null;

// Base style injected into every text/html output iframe — mirrors the editor's
// iframeBaseStyle so HTML/SVG widgets look the same in a share. Reads theme
// CSS vars when present, with editor-matching fallbacks.
function iframeBaseStyle() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => (cs.getPropertyValue(n) || d).trim();
  const bg = v('--output-html-bg', '#ffffff');
  const fg = v('--output-html-fg', '#1e1e2e');
  const accent = v('--accent', '#89b4fa');
  return (
    `<style>body{background:${bg};color:${fg};` +
    `font-family:'JetBrains Mono',ui-monospace,monospace;font-size:13px;` +
    `margin:0;padding:4px 8px;font-variant-ligatures:none;` +
    `font-feature-settings:"calt" 0,"liga" 0}` +
    `input,select,button{accent-color:${accent};font-family:inherit;` +
    `font-size:13px;cursor:pointer}</style>`
  );
}

// KaTeX-before-marked pass for text/markdown output. The export page loads
// marked + katex from CDN; if katex is absent this is a safe no-op that returns
// the source unchanged for marked to handle.
function renderMathInMarkdown(src) {
  let s = src || '';
  if (typeof katex === 'undefined') return s;
  s = s.replace(/\$\$([\s\S]+?)\$\$/g, (m, tex) => {
    try {
      return katex.renderToString(tex.trim(), { displayMode: true, throwOnError: false });
    } catch (_e) {
      return m;
    }
  });
  s = s.replace(/(^|[^\\$])\$([^\$\n]+?)\$(?!\d)/g, (m, pre, tex) => {
    try {
      return pre + katex.renderToString(tex, { displayMode: false, throwOnError: false });
    } catch (_e) {
      return m;
    }
  });
  return s;
}
// ── UI: decorate the export DOM with Run controls + editable sources ─────
//
// EVERY haskell code cell must get its own Run control, in place. The export DOM
// renders each ```haskell fence as one code-cell element, in source order, so
// the Nth code-cell element is the Nth haskell cell parsed from the island.
// Observed export shapes (see findCodeCellHosts):
//   - server seed/static export: <pre class="code"><code class="language-haskell">
//   - client notebook-mode (dashboard.html): <div class="dash-code">…</div>
// Only when NO per-cell code DOM exists (plain dashboard mode hides source) do we
// fall back to a single self-contained panel listing each cell. Each cell gets a
// <textarea> (lightly editable — NOT CodeMirror), a Run button, an output slot,
// and a status line.

// Styles for the injected controls — the Sabela editor's cell shape (card,
// gutter with cell number + `hs` chip, run button, top-bordered output) in a LIGHT
// EMBER palette: warm cream card, dark-brown text, ember-orange accents. Tokens
// are scoped to .swasm-cell so they don't leak into the export page. (--sg, the
// run-hover/accent, is ember here, not green.)
function injectRunStyles() {
  if (document.getElementById('swasm-style')) return;
  const s = document.createElement('style');
  s.id = 'swasm-style';
  s.textContent = [
    '.swasm-cell{--sb:#fbf3e4;--so:#f4e9d3;--sf:#34291a;--sd:#927f5f;--sbd:#e7d6b6;',
    '--sac:#cf7a2a;--sad:#f0e2c5;--sy:#cf7a2a;--sg:#cf7a2a;--sr:#c0573f;',
    "--sm:'JetBrains Mono',ui-monospace,SFMono-Regular,monospace;",
    'position:relative;background:var(--sb);border:1px solid var(--sbd);',
    'border-radius:8px;overflow:hidden;margin:6px 0;box-shadow:0 1px 3px rgba(90,55,20,.08)}',
    '.swasm-cell:hover{border-color:var(--sad)}',
    '.swasm-cell.running{border-color:var(--sy)}',
    '.swasm-cell.has-error{border-color:var(--sr)}',
    '.swasm-gutter{display:flex;align-items:center;gap:8px;padding:6px 12px 0;',
    'font-family:var(--sm);font-size:10px;color:var(--sd)}',
    '.swasm-num{min-width:14px;text-align:right;color:var(--sd)}',
    '.swasm-lang{background:var(--sad);color:var(--sd);padding:1px 5px;border-radius:2px;',
    'letter-spacing:.5px;text-transform:uppercase}',
    '.swasm-status{color:var(--sy)}',
    // Always visible (not hover-revealed like the editor) — this is a runner, so
    // the run affordance stays in the cell at full opacity.
    '.swasm-actions{position:absolute;top:6px;right:8px;z-index:5}',
    '.swasm-run{display:inline-flex;align-items:center;gap:4px;font-family:var(--sm);font-size:10px;',
    'padding:3px 7px;border:1px solid var(--sbd);border-radius:4px;background:var(--sb);',
    'color:var(--sd);cursor:pointer;line-height:1}',
    '.swasm-run svg{width:12px;height:12px}',
    '.swasm-run:hover{color:var(--sg);border-color:var(--sg)}',
    '.swasm-run:disabled{opacity:.5;cursor:default}',
    '.swasm-src{display:block;width:100%;box-sizing:border-box;background:transparent;border:0;',
    'resize:vertical;color:var(--sf);font-family:var(--sm);font-size:13.5px;line-height:1.55;',
    'padding:4px 14px 10px}',
    '.swasm-src:focus{outline:none}',
    '.swasm-out{border-top:1px solid var(--sbd);padding:10px 14px;background:var(--so);',
    'font-family:var(--sm);font-size:12.5px;line-height:1.5;color:var(--sf);white-space:pre-wrap;',
    'word-break:break-word;max-height:500px;overflow:auto}',
    '.swasm-out:empty{display:none}',
    '.swasm-out .cell-output{white-space:pre-wrap;font-family:inherit;color:var(--sf)}',
    '.swasm-out .cell-output.error{color:var(--sr)}',
    '.swasm-fork{font-size:12.5px;color:var(--sd)}',
    '.swasm-fork-btn{cursor:pointer;border:0;border-radius:4px;padding:3px 10px;margin-left:6px;',
    'background:var(--sac);color:#2a2017;font-family:var(--sm);font-size:11px;font-weight:600}',
  ].join('');
  document.head.appendChild(s);
}

// Build one cell's control block in the editor's shape: gutter (number + lang),
// hover-revealed Run action, editable source, output slot.
// Returns { root, textarea, out, status, runBtn } for the runner to drive.
function buildCellControls(cell) {
  const root = document.createElement('div');
  root.className = 'swasm-cell code';
  root.dataset.cellIndex = cell.index;

  const gutter = document.createElement('div');
  gutter.className = 'swasm-gutter';
  const num = document.createElement('span');
  num.className = 'swasm-num';
  num.textContent = cell.index + 1;
  const lang = document.createElement('span');
  lang.className = 'swasm-lang';
  lang.textContent = 'hs';
  const status = document.createElement('span');
  status.className = 'swasm-status';
  gutter.append(num, lang, status);

  const actions = document.createElement('div');
  actions.className = 'swasm-actions';
  const runBtn = document.createElement('button');
  runBtn.type = 'button';
  runBtn.className = 'swasm-run';
  // Sabela's run affordance: the #i-play triangle + "run" (inlined — a standalone
  // bundle has no icon sprite on the export page).
  runBtn.innerHTML =
    '<svg viewBox="0 0 24 24"><polygon points="6 4 20 12 6 20 6 4" fill="currentColor"/></svg>run';
  actions.appendChild(runBtn);

  const textarea = document.createElement('textarea');
  textarea.className = 'swasm-src';
  textarea.spellcheck = false;
  textarea.value = cell.code;
  textarea.rows = Math.min(20, Math.max(1, cell.code.split('\n').length));

  const out = document.createElement('div');
  out.className = 'swasm-out';

  root.append(gutter, actions, textarea, out);
  return { root, textarea, out, status, runBtn };
}

// The per-cell code-cell host elements in source order, or [] if this export
// shows no per-cell code DOM. Tries each known export shape; the static block to
// hide/replace is the outermost <pre>/<div> wrapper, found via closest().
function findCodeCellHosts() {
  const seed = document.querySelectorAll('code.language-haskell');
  if (seed.length) {
    return Array.from(seed).map((c) => c.closest('pre') || c.parentNode || c);
  }
  const dash = document.querySelectorAll('.dash-code');
  return Array.from(dash);
}

// Remove a cell's frozen export output — the `.out` block(s) the export rendered
// right after the code. The live runner owns output now, so the cached copy would
// just be a stale duplicate.
function dropPreseededOutput(block) {
  let sib = block.nextElementSibling;
  while (sib && sib.classList && sib.classList.contains('out')) {
    const next = sib.nextElementSibling;
    sib.remove();
    sib = next;
  }
}

// Attach controls to the page. Map the N code-cell hosts 1:1 to the N parsed
// haskell cells (same source order) and insert each cell's controls right after
// its host, hiding the read-only source. Only when the host count does NOT match
// (no per-cell code DOM, e.g. plain dashboard mode) fall back to one panel.
function mountCellControls(cells) {
  const hosts = findCodeCellHosts();
  const useInline = hosts.length === cells.length && cells.length > 0;
  const controls = cells.map(buildCellControls);
  if (useInline) {
    hosts.forEach((block, i) => {
      dropPreseededOutput(block); // the live runner replaces the frozen output
      block.style.display = 'none'; // the textarea replaces the static view
      block.parentNode.insertBefore(controls[i].root, block.nextSibling);
    });
  } else {
    const host = document.querySelector('main') || document.body;
    const panel = document.createElement('section');
    panel.className = 'swasm-panel';
    const h = document.createElement('h2');
    h.textContent = 'Run this notebook';
    h.style.cssText = 'font-size:15px;margin:18px 10px 6px';
    panel.appendChild(h);
    controls.forEach((c) => panel.appendChild(c.root));
    host.appendChild(panel);
  }
  return controls;
}

// Render a per-cell run result (or error) into its output slot, reusing the
// shared MIME renderer so output matches the editor. `outputs` is a list of
// { oiMime, oiOutput }; `error` is plain text (or empty).
function renderCellResult(slot, outputs, error) {
  slot.innerHTML = '';
  const merged = mergeOutputs(outputs || []).filter((o) => o.oiOutput && o.oiOutput.trim());
  for (const item of merged) {
    const block = document.createElement('div');
    slot.appendChild(block);
    renderMimeOutput(block, item.oiOutput, item.oiMime);
  }
  if (error && error.trim()) {
    const err = document.createElement('div');
    err.className = 'cell-output error';
    err.textContent = error;
    slot.appendChild(err);
  }
}
// ── Run orchestration: drive the persistent mhsi REPL, dependency-scoped ──
//
// Touching cell K marks it active and re-runs the AFFECTED set (K + its transitive
// dependents that are active). The REPL accumulates definitions (re-feeding a name
// ADDS a clause rather than replacing it), so each recompute `:clear`s and re-feeds
// the affected cells' dependency closure in order — bounded by what those cells
// actually need, not the whole notebook. Base stays loaded, so each feed is ~90ms.

const SwasmState = { controls: [], slug: '', active: new Set() };

// Live source of every cell, from its editable textarea.
function liveCells() {
  return SwasmState.controls.map((c, i) => ({ index: i, code: c.textarea.value }));
}

// Coalesced recompute: a run in flight collects further roots and reruns once.
let swasmBusy = false;
let swasmPending = new Set();
function recompute(idx) {
  if (idx !== undefined) {
    SwasmState.active.add(idx);
    swasmPending.add(idx);
  }
  if (swasmBusy || swasmPending.size === 0) return;
  swasmBusy = true;
  const roots = swasmPending;
  swasmPending = new Set();
  runNotebook(roots).finally(() => {
    swasmBusy = false;
    if (swasmPending.size) recompute();
  });
}

async function runNotebook(roots) {
  const live = liveCells();
  const slug = SwasmState.slug;
  const { revDeps, fwdDeps } = buildDeps(live);

  // affected = roots + their transitive dependents that are active.
  const affected = new Set(roots);
  for (const r of roots)
    for (const a of affectedFrom(r, revDeps)) if (SwasmState.active.has(a)) affected.add(a);

  const forkSet = new Set([...affected].filter((i) => cellNeedsFork(live[i].code)));

  // closure = the affected cells + their transitive upstream deps (the definitions
  // they need in scope), minus fork cells. Fed in source order after a :clear.
  const closure = new Set();
  const stack = [...affected];
  while (stack.length) {
    const i = stack.pop();
    if (forkSet.has(i) || closure.has(i)) continue;
    closure.add(i);
    for (const dep of fwdDeps.get(i) || []) stack.push(dep);
  }
  const order = [...closure].sort((a, b) => a - b);

  setRunning(true);
  for (const i of affected) {
    if (forkSet.has(i)) showForkOnly(SwasmState.controls[i], slug);
    else markCellRunning(SwasmState.controls[i]);
  }

  // Only :clear when re-feeding a definition (else the REPL keeps the old clause).
  // A closure of pure expressions needs no clear — keeping it ~one feed (~90ms).
  const needReset = order.some((i) => cellDefs(live[i].code).size > 0);
  try {
    await MhsEngine.load();
    if (needReset) await MhsEngine.reset();
  } catch (_e) {
    affected.forEach((i) => forkSet.has(i) || showEngineUnavailable(SwasmState.controls[i], slug));
    setRunning(false);
    return;
  }

  for (const i of order) {
    let res;
    try {
      res = await MhsEngine.feed(cellToReplLines(live[i].code));
    } catch (_e) {
      if (affected.has(i)) showEngineUnavailable(SwasmState.controls[i], slug);
      continue;
    }
    if (affected.has(i)) renderCell(SwasmState.controls[i], res, slug);
  }
  setRunning(false);
}

// Render one re-run cell's REPL result. A missing-module error → Fork nudge;
// otherwise text output and/or the error text, with the error border.
function renderCell(ctrl, res, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  if (stderrNeedsFork(res.err)) {
    showForkOnly(ctrl, slug);
    return;
  }
  const outputs = res.out.trim() ? [{ oiMime: 'text/plain', oiOutput: res.out }] : [];
  ctrl.root.classList.toggle('has-error', !!res.err.trim());
  renderCellResult(ctrl.out, outputs, res.err);
}

function markCellRunning(ctrl) {
  ctrl.status.textContent = MhsEngine.available ? 'running…' : 'starting Haskell…';
  ctrl.root.classList.add('running');
}

function setRunning(on) {
  SwasmState.controls.forEach((c) => {
    c.runBtn.disabled = on;
    if (!on) c.root.classList.remove('running');
  });
}

// A cell MicroHs can't run shows only the Fork nudge.
function showForkOnly(ctrl, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  ctrl.out.innerHTML = '';
  ctrl.out.appendChild(forkNudge(slug));
}

// The engine could not boot (404 / timeout) — degrade the cell to a Fork nudge.
function showEngineUnavailable(ctrl, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  ctrl.out.innerHTML = '';
  const note = document.createElement('div');
  note.className = 'cell-output';
  note.style.cssText = 'margin-bottom:6px';
  note.textContent = "Couldn't start the in-browser Haskell engine.";
  ctrl.out.appendChild(note);
  ctrl.out.appendChild(forkNudge(slug));
}
// ── Bootstrap: wire everything up on DOMContentLoaded ────────────────────

function initWasmRun() {
  const { source, slug } = readSourceIsland();
  const cells = parseHaskellCells(source);
  if (cells.length === 0) return; // nothing runnable (prose-only share)

  injectRunStyles();
  const controls = mountCellControls(cells);

  SwasmState.controls = controls;
  SwasmState.slug = slug;

  // Reactive: a Run click re-runs the cell + its active dependents; so does an
  // edit, after a short quiet period. Only affected cells re-run (dependency
  // scoped), each ~90ms against the warm persistent session.
  let editTimer = null;
  controls.forEach((ctrl, idx) => {
    ctrl.runBtn.addEventListener('click', () => {
      if (editTimer) clearTimeout(editTimer);
      recompute(idx);
    });
    ctrl.textarea.addEventListener('input', () => {
      if (editTimer) clearTimeout(editTimer);
      editTimer = setTimeout(() => recompute(idx), 700);
    });
  });

  // Warm the persistent MicroHs session in the background once the page is idle,
  // so the first Run is usually already past the ~2s boot/warmup. Reader pages
  // that are never run just pay an idle background load.
  const warm = () => MhsEngine.load().catch(() => {});
  if (typeof requestIdleCallback === 'function') requestIdleCallback(warm, { timeout: 3000 });
  else setTimeout(warm, 2000);
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initWasmRun);
} else {
  initWasmRun();
}
