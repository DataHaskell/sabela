// ── Diagnostics (cell errors) ────────────────────────────────────
// The server reports a cell's errors as text, one block per error headed
// "cell N[, line M]: first line". This renders each block as a card: a header
// with the gist and its location (click to jump there) and the remaining
// GHC context lines beneath it, dimmed.

const DIAG_HEAD_RE = /^cell (\d+)(?:, line (\d+))?: ?(.*)$/;
const DIAG_CTX_RE =
  /^\s*(In (the|an|a) |arising from|Note:|Suggested fix|Relevant bindings|Possible |Probable |Perhaps|•|Where:|Expected:|Actual:|To import|You can |\(Note:)/;

function parseDiagnostics(text) {
  const items = [];
  let cur = null;
  for (const line of text.replace(/\r/g, '').split('\n')) {
    const m = DIAG_HEAD_RE.exec(line);
    if (m) {
      cur = { cell: Number(m[1]), line: m[2] ? Number(m[2]) : null, first: m[3], rest: [] };
      items.push(cur);
    } else if (cur) {
      cur.rest.push(line);
    } else {
      cur = { cell: null, line: null, first: line, rest: [] };
      items.push(cur);
    }
  }
  for (const it of items) {
    while (it.rest.length && !it.rest[it.rest.length - 1].trim()) it.rest.pop();
  }
  return items;
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

// GHC quotes names as `name' or ‘name’; both become inline code.
function formatGhcInline(s) {
  return escapeHtml(s)
    .replace(/‘([^’\n]+)’/g, '<code>$1</code>')
    .replace(/`([^'\n`]+)'/g, '<code>$1</code>');
}

function diagLocation(item, hostCellId) {
  if (item.cell == null) return '';
  const sameCell = item.cell === hostCellId;
  const where = sameCell ? '' : cellNumberLabel(item.cell);
  const line = item.line != null ? `line ${item.line}` : '';
  return [where, line].filter(Boolean).join(', ');
}

function cellNumberLabel(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  return el && el.dataset.num ? `cell ${el.dataset.num}` : `cell #${cellId}`;
}

function focusCellLine(cellId, line) {
  const cm = editors[cellId];
  if (!cm) return;
  cm.focus();
  if (line != null) cm.setCursor({ line: Math.max(0, line - 1), ch: 0 });
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (el) el.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
}

function renderDiagnostics(container, text, hostCellId) {
  container.innerHTML = '';
  const items = parseDiagnostics(text || '');
  for (const item of items) {
    const card = document.createElement('div');
    card.className = 'diag';
    const head = document.createElement('div');
    head.className = 'diag-head';
    const loc = diagLocation(item, hostCellId);
    head.innerHTML =
      `<span class="diag-dot" aria-hidden="true"></span>` +
      `<span class="diag-msg">${formatGhcInline(item.first)}</span>` +
      (loc ? `<span class="diag-loc">${escapeHtml(loc)}</span>` : '');
    if (item.cell != null && item.line != null) {
      head.classList.add('jump');
      head.title = 'Go to this line';
      head.onclick = () => focusCellLine(item.cell, item.line);
    }
    card.appendChild(head);
    if (item.rest.length) {
      const body = document.createElement('div');
      body.className = 'diag-body';
      body.innerHTML = item.rest
        .map(
          (l) =>
            `<div class="${DIAG_CTX_RE.test(l) ? 'ctx' : ''}">${formatGhcInline(l) || ' '}</div>`
        )
        .join('');
      card.appendChild(body);
    }
    container.appendChild(card);
  }
}
