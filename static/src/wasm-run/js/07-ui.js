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
