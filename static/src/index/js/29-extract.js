// ── Extract as component ─────────────────────────────────────────
// Pick cells, name the component, review the module the server would write
// and the call site the notebook keeps, then accept. The notebook goes on
// calling the component; nothing is copied.

let extractState = null;

async function startExtract(cellId) {
  const cells = ((notebook && notebook.nbCells) || []).filter((c) => c.cellType === 'CodeCell');
  if (!cells.length) return;
  const name = await askInput({
    title: 'Extract as component',
    message:
      'Name the module. It becomes Components.<Name>, importable from any notebook in this workspace.',
    label: 'Name',
    placeholder: 'Retention',
    defaultValue: suggestComponentName(cellId),
    confirmLabel: 'Preview',
    validate: (v) =>
      /^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)*$/.test(v.trim())
        ? null
        : 'Use a capitalised name like Retention or Charts.Revenue',
  });
  if (!name) return;
  extractState = { name: name.trim(), cellIds: [cellId], preview: null };
  openExtractModal();
  await refreshExtractPreview();
}

function suggestComponentName(cellId) {
  const names = cellBindingNames(cellId);
  const base = names[0] || 'Component';
  return base.charAt(0).toUpperCase() + base.slice(1).replace(/[^A-Za-z0-9_']/g, '');
}

function openExtractModal() {
  let overlay = document.getElementById('extract-modal-overlay');
  if (!overlay) {
    overlay = document.createElement('div');
    overlay.id = 'extract-modal-overlay';
    overlay.className = 'ai-modal-overlay';
    overlay.onclick = (e) => {
      if (e.target === overlay) closeExtractModal();
    };
    document.body.appendChild(overlay);
  }
  overlay.style.display = 'flex';
  renderExtractModal();
}

function closeExtractModal() {
  const overlay = document.getElementById('extract-modal-overlay');
  if (overlay) overlay.style.display = 'none';
  extractState = null;
}

async function refreshExtractPreview() {
  if (!extractState) return;
  try {
    extractState.preview = await api('POST', 'components/extract', {
      erCellIds: extractState.cellIds,
      erName: extractState.name,
      erPurpose: extractState.purpose || null,
    });
    extractState.error = null;
  } catch (e) {
    extractState.error = e.message;
  }
  renderExtractModal();
}

function renderExtractModal() {
  const overlay = document.getElementById('extract-modal-overlay');
  if (!overlay || !extractState) return;
  overlay.innerHTML = '';
  const modal = document.createElement('div');
  modal.className = 'ai-modal extract-modal';
  modal.onclick = (e) => e.stopPropagation();
  const h = document.createElement('div');
  h.className = 'extract-head';
  h.textContent = 'Extract Components.' + extractState.name;
  modal.appendChild(h);
  modal.appendChild(renderExtractCellPicker());
  const p = extractState.preview;
  if (extractState.error) {
    const err = document.createElement('div');
    err.className = 'design-notice';
    err.textContent = extractState.error;
    modal.appendChild(err);
  }
  if (p) {
    for (const w of p.epWarnings) {
      const n = document.createElement('div');
      n.className = 'design-notice';
      n.textContent = w;
      modal.appendChild(n);
    }
    const cols = document.createElement('div');
    cols.className = 'extract-cols';
    cols.append(
      extractPane('Module ' + p.epPath, 'extract-source', p.epSource),
      extractPane('The notebook keeps', 'extract-replacement', p.epReplacement)
    );
    modal.appendChild(cols);
    const meta = document.createElement('div');
    meta.className = 'design-hint';
    meta.textContent = p.epExports.length
      ? 'exports ' + p.epExports.join(', ')
      : 'exports nothing yet';
    modal.appendChild(meta);
  }
  const actions = document.createElement('div');
  actions.className = 'ai-modal-actions';
  const cancel = document.createElement('button');
  cancel.className = 'secondary';
  cancel.textContent = 'Cancel';
  cancel.onclick = closeExtractModal;
  const accept = document.createElement('button');
  accept.className = 'primary';
  accept.textContent = 'Accept';
  accept.disabled = !p;
  accept.onclick = acceptExtract;
  actions.append(cancel, accept);
  modal.appendChild(actions);
  overlay.appendChild(modal);
}

// Which cells go into the module; the first one keeps the call site.
function renderExtractCellPicker() {
  const box = document.createElement('div');
  box.className = 'extract-cells';
  const cells = ((notebook && notebook.nbCells) || []).filter((c) => c.cellType === 'CodeCell');
  for (const c of cells) {
    const row = document.createElement('label');
    row.className = 'extract-cell';
    const cb = document.createElement('input');
    cb.type = 'checkbox';
    cb.checked = extractState.cellIds.includes(c.cellId);
    cb.onchange = () => {
      if (cb.checked) extractState.cellIds.push(c.cellId);
      else extractState.cellIds = extractState.cellIds.filter((id) => id !== c.cellId);
      extractState.cellIds.sort(
        (a, b) => cells.findIndex((x) => x.cellId === a) - cells.findIndex((x) => x.cellId === b)
      );
      refreshExtractPreview();
    };
    const label = document.createElement('span');
    label.textContent =
      '[' +
      cellNumberOf(c.cellId) +
      '] ' +
      ((c.cellSource || '').split('\n').find((l) => l.trim()) || '(empty)').slice(0, 70);
    row.append(cb, label);
    box.appendChild(row);
  }
  return box;
}

function extractPane(title, id, text) {
  const pane = document.createElement('div');
  pane.className = 'extract-pane';
  const h = document.createElement('div');
  h.className = 'design-label';
  h.textContent = title;
  const ta = document.createElement('textarea');
  ta.id = id;
  ta.value = text;
  ta.spellcheck = false;
  pane.append(h, ta);
  return pane;
}

async function acceptExtract() {
  if (!extractState || !extractState.preview) return;
  const source = document.getElementById('extract-source').value;
  const replacement = document.getElementById('extract-replacement').value;
  try {
    const nb = await api('POST', 'components/accept', {
      eaCellIds: extractState.cellIds,
      eaModule: extractState.preview.epModule,
      eaSource: source,
      eaReplacement: replacement,
    });
    closeExtractModal();
    render(nb);
    setStatus('wrote ' + extractState?.preview?.epPath + '; kernel restarting', 'ok');
    if (typeof loadComponents === 'function') loadComponents();
  } catch (e) {
    extractState.error = e.message;
    renderExtractModal();
  }
}
