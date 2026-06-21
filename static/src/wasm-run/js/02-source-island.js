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
