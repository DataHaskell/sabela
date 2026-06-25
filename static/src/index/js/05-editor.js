// ── Error markers (gutter) ───────────────────────────────────────
function applyErrorMarkers(cellId, errors) {
  cellErrors[cellId] = errors;
  const cm = editors[cellId];
  if (!cm) return;
  // Clear old markers
  cm.clearGutter('error-gutter');
  for (let i = 0; i < cm.lineCount(); i++) cm.removeLineClass(i, 'background', 'error-line');

  for (const err of errors) {
    if (err.ceLine != null) {
      const line = err.ceLine - 1; // 0-indexed
      if (line >= 0 && line < cm.lineCount()) {
        const marker = document.createElement('div');
        marker.className = 'error-marker';
        marker.textContent = '●';
        marker.title = err.ceMessage;
        marker.onmouseenter = (e) => showErrorTooltip(e, err.ceMessage);
        marker.onmouseleave = hideErrorTooltip;
        cm.setGutterMarker(line, 'error-gutter', marker);
        cm.addLineClass(line, 'background', 'error-line');
      }
    }
  }
  // Mark the cell
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (el) el.classList.add('has-error');
}

function clearErrorMarkers(cellId) {
  cellErrors[cellId] = [];
  const cm = editors[cellId];
  if (!cm) return;
  cm.clearGutter('error-gutter');
  for (let i = 0; i < cm.lineCount(); i++) cm.removeLineClass(i, 'background', 'error-line');
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (el) el.classList.remove('has-error');
}

// ── Warning markers (gutter) ─────────────────────────────────────
// A light amber nudge: gutter dot + hover tooltip, in its own gutter so a line
// can show both a warning and an error. Warnings never print to the console.
function applyWarningMarkers(cellId, warnings) {
  cellWarnings[cellId] = warnings;
  const cm = editors[cellId];
  if (!cm) return;
  cm.clearGutter('warn-gutter');
  for (let i = 0; i < cm.lineCount(); i++) cm.removeLineClass(i, 'background', 'warn-line');

  for (const w of warnings) {
    if (w.ceLine != null) {
      const line = w.ceLine - 1; // 0-indexed
      if (line >= 0 && line < cm.lineCount()) {
        const marker = document.createElement('div');
        marker.className = 'warn-marker';
        marker.textContent = '●';
        marker.title = w.ceMessage;
        marker.onmouseenter = (e) => showErrorTooltip(e, w.ceMessage);
        marker.onmouseleave = hideErrorTooltip;
        cm.setGutterMarker(line, 'warn-gutter', marker);
        cm.addLineClass(line, 'background', 'warn-line');
      }
    }
  }
}

function clearWarningMarkers(cellId) {
  cellWarnings[cellId] = [];
  const cm = editors[cellId];
  if (!cm) return;
  cm.clearGutter('warn-gutter');
  for (let i = 0; i < cm.lineCount(); i++) cm.removeLineClass(i, 'background', 'warn-line');
}

let tooltipEl = null;
function showErrorTooltip(e, msg) {
  hideErrorTooltip();
  tooltipEl = document.createElement('div');
  tooltipEl.className = 'error-tooltip';
  tooltipEl.textContent = msg;
  document.body.appendChild(tooltipEl);
  const r = e.target.getBoundingClientRect();
  tooltipEl.style.left = r.right + 8 + 'px';
  tooltipEl.style.top = r.top - 4 + 'px';
}
function hideErrorTooltip() {
  if (tooltipEl) {
    tooltipEl.remove();
    tooltipEl = null;
  }
}

// ── Tab completions ──────────────────────────────────────────────
async function haskellHint(cm) {
  const cur = cm.getCursor();
  const lineUpToCursor = cm.getRange({ line: cur.line, ch: 0 }, cur);
  const match = lineUpToCursor.match(/[A-Za-z_'][A-Za-z0-9_']*(?:\.[A-Za-z_'][A-Za-z0-9_']*)*\.?$/);
  if (!match) return;
  const prefix = match[0];
  const start = cur.ch - prefix.length;
  try {
    const res = await api('POST', 'complete', { crPrefix: prefix });
    if (!res.crCompletions || !res.crCompletions.length) return;
    return {
      list: res.crCompletions.slice(0, 30),
      from: CodeMirror.Pos(cur.line, start),
      to: CodeMirror.Pos(cur.line, cur.ch),
    };
  } catch {
    return;
  }
}
