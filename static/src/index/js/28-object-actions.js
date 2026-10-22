// ── Actions on an object ─────────────────────────────────────────
// A chart cell is a card: a header with its title and Design / Code / Ask
// AI, the chart below, the Haskell folded away until asked for. A table
// output grows a corner handle with Chart this and Ask AI; every cell can be
// handed to the chat with a structured description of what it is.

function vizTitleOf(cell) {
  const known = vizSpecs.get(cell.cellId) || specFromOutputDom(cell.cellId);
  if (known && known.spec && known.spec.title) return known.spec.title;
  const ref = chartDataExprOf(cell.cellId);
  const raw = known && known.spec && known.spec.mark;
  const mark = raw === 'arc' ? 'pie' : raw;
  return (
    (mark ? mark.charAt(0).toUpperCase() + mark.slice(1) + ' chart' : 'Chart') +
    (ref ? ' of ' + ref : '')
  );
}

function renderVizHead(cell) {
  const head = document.createElement('div');
  head.className = 'viz-head';
  const title = document.createElement('span');
  title.className = 'viz-title';
  title.textContent = vizTitleOf(cell);
  const actions = document.createElement('span');
  actions.className = 'viz-actions';
  const btn = (label, title, cls, onClick) => {
    const b = document.createElement('button');
    b.className = cls;
    b.textContent = label;
    b.title = title;
    b.onclick = (e) => {
      e.stopPropagation();
      onClick();
    };
    return b;
  };
  actions.append(
    btn('Edit', 'Change this chart in the Visualize panel, no code needed', 'viz-design', () =>
      openDesigner(cell.cellId)
    ),
    btn('Code', 'Show or hide the Haskell behind this chart', 'viz-code', () =>
      toggleCellCode(cell.cellId)
    ),
    btn('Ask AI', 'Ask the assistant about this chart', 'viz-ask', () => askAiAbout(cell.cellId))
  );
  head.append(title, actions);
  head.onclick = () => openDesigner(cell.cellId);
  return head;
}

function toggleCellCode(cellId, show) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  const next = show == null ? !codeShown.has(cellId) : !!show;
  if (next) codeShown.add(cellId);
  else codeShown.delete(cellId);
  if (el) el.classList.toggle('show-code', next);
  if (next && editors[cellId]) requestAnimationFrame(() => editors[cellId].refresh());
}

// Called after an output lands: a cell that just became a chart gets its
// header, and a header whose spec changed gets its title.
function syncVizCard(cellEl, cell) {
  if (!cell || objectKindOf(cell) !== 'chart') return;
  let head = cellEl.querySelector('.viz-head');
  if (!head) {
    cellEl.classList.add('viz');
    cellEl.classList.toggle('show-code', codeShown.has(cell.cellId));
    head = renderVizHead(cell);
    cellEl.insertBefore(head, cellEl.querySelector('.cell-box'));
  } else {
    head.querySelector('.viz-title').textContent = vizTitleOf(cell);
  }
}

function attachOutputHandle(cellEl, cellId) {
  const out = cellEl.querySelector('.cell-output');
  const cell = notebook && notebook.nbCells.find((c) => c.cellId === cellId);
  const kind = objectKindOf(cell);
  cellEl.querySelectorAll('.out-handle').forEach((h) => h.remove());
  syncVizCard(cellEl, cell);
  if (!out || kind !== 'table') return;
  const handle = document.createElement('div');
  handle.className = 'out-handle';
  const design = document.createElement('button');
  design.textContent = 'Chart this';
  design.title = 'Draw a chart from this table';
  design.onclick = (e) => {
    e.stopPropagation();
    chartFromTable(cellId);
  };
  const ask = document.createElement('button');
  ask.textContent = 'Ask AI';
  ask.title = 'Ask the assistant about this output';
  ask.onclick = (e) => {
    e.stopPropagation();
    askAiAbout(cellId);
  };
  handle.append(design, ask);
  cellEl.appendChild(handle);
}

// A table's binding becomes the source of a new chart cell after it; the
// full source list is consulted so an untyped frame is known to need freezing.
async function chartFromTable(cellId) {
  const sources = (await listVizSources()).filter((s) => s.cellId === cellId && !s.disabled);
  await openDesigner(null, { afterId: cellId, mark: 'bar' });
  if (sources.length) designerSetSource(sources[0]);
}

// Ask AI: the chat opens focused on the cell. The focus travels with the
// next message as its cell id; the server describes the cell to the model,
// so nothing about it has to be typed or read in the box.
function askAiAbout(cellId) {
  const panel = document.getElementById('right-panel');
  if (panel.classList.contains('collapsed')) panel.classList.remove('collapsed');
  switchTab('chat', { quiet: true });
  const cell = notebook && notebook.nbCells.find((c) => c.cellId === cellId);
  setChatFocus(cellId, objectKindOf(cell));
  const input = document.getElementById('chat-input');
  if (input) input.focus();
}

function setChatFocus(cellId, kind) {
  chatFocus = cellId == null ? null : { cellId, kind };
  const area = document.querySelector('.chat-input-area');
  if (!area) return;
  let chip = document.getElementById('chat-focus');
  if (!chatFocus) {
    if (chip) chip.remove();
    const input = document.getElementById('chat-input');
    if (input) input.placeholder = 'Message AI...';
    return;
  }
  if (!chip) {
    chip = document.createElement('div');
    chip.id = 'chat-focus';
    chip.className = 'chat-focus';
    area.insertBefore(chip, area.firstChild);
  }
  chip.innerHTML = '';
  const label = document.createElement('span');
  label.textContent = 'About cell ' + cellNumberOf(cellId) + ' · ' + kind;
  const clear = document.createElement('button');
  clear.textContent = '×';
  clear.title = 'Ask about the whole notebook instead';
  clear.onclick = () => setChatFocus(null);
  chip.append(label, clear);
  const input = document.getElementById('chat-input');
  if (input) input.placeholder = 'Ask about this ' + kind + '…';
}
