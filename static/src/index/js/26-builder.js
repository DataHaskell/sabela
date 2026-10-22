// ── The Design panel ─────────────────────────────────────────────
// Builds a chart spec from a data source and shelves, asks the server for
// the canonical Haskell, and writes it to the cell; the reactive run is the
// preview. The cell stays the source of truth: a hand edit shows a notice,
// and the panel offers a fresh cell instead of overwriting the user's work.

const DESIGN_APPLY_DELAY = 400;

function designerPanel() {
  return document.getElementById('panel-design');
}

// Open for an existing cell, or (cellId null) for a chart to be inserted
// after `opts.afterId` once a source is chosen.
async function openDesigner(cellId, opts) {
  opts = opts || {};
  clearTimeout(designer.timer);
  designer = {
    cellId: cellId == null ? null : cellId,
    afterId: opts.afterId == null ? lastCellId() : opts.afterId,
    spec: null,
    source: null,
    armed: null,
    timer: null,
    blocked: false,
    ownership: null,
    message: '',
    sources: [],
    columns: [],
    pickingSource: false,
  };
  document.querySelectorAll('.cell.designing').forEach((c) => c.classList.remove('designing'));
  if (cellId != null) {
    const el = document.querySelector(`.cell[data-id="${cellId}"]`);
    if (el) el.classList.add('designing');
    const known = vizSpecs.get(cellId) || specFromOutputDom(cellId);
    if (known) {
      designer.spec = JSON.parse(JSON.stringify(known.spec || {}));
      designer.columns = vizColumns(known.cols, known.types);
      const ref = chartDataExprOf(cellId);
      if (ref) designer.source = { kind: 'binding', ref, label: ref, columns: designer.columns };
    }
    if (opts.mark && designer.spec) designer.spec.mark = opts.mark;
  }
  if (!designer.spec && opts.mark) designer.spec = { mark: opts.mark };
  const panel = document.getElementById('right-panel');
  if (panel.classList.contains('collapsed')) panel.classList.remove('collapsed');
  switchTab('design');
  renderDesigner();
  await designerRefreshSources();
  if (designer.source && designer.source.ref) {
    const full = await fetchBindingColumns(designer.source.ref);
    if (full.length) {
      designer.columns = full;
      designer.source.columns = full;
      renderDesigner();
    }
  }
  if (cellId != null) designerCheckOwnership();
}

function closeDesigner() {
  clearTimeout(designer.timer);
  document.querySelectorAll('.cell.designing').forEach((c) => c.classList.remove('designing'));
  designer.cellId = null;
  designer.spec = null;
  renderDesigner();
}

function lastCellId() {
  const cells = (notebook && notebook.nbCells) || [];
  return cells.length ? cells[cells.length - 1].cellId : -1;
}

function cellNumberOf(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  return el && el.dataset.num ? el.dataset.num : String(cellId);
}

// Redraws the whole panel from `designer`; cheap, and keeps state in one
// place. Sections read top to bottom as a Metabase question: pick the data,
// pick the visualization, fill the axes; the Haskell waits behind a
// disclosure for whoever wants it.
function renderDesigner() {
  const root = designerPanel();
  if (!root) return;
  root.innerHTML = '';
  if (designer.cellId == null && !designer.spec && !designer.sources.length) {
    root.innerHTML =
      '<div class="design-empty">Chart your data without writing code. Add a dataset below, ' +
      'or run a cell that shows a table, and it appears here.</div>';
    root.appendChild(renderAddData());
    return;
  }
  root.appendChild(renderDesignHeader());
  if (designer.ownership === 'edited') root.appendChild(renderOwnershipNotice());
  root.appendChild(designSection(dataSectionLabel(), renderSourcePicker()));
  if (designer.source && designer.columns.length && !designer.pickingSource) {
    root.appendChild(designSection('Visualization', renderMarkPicker()));
    const grid = document.createElement('div');
    grid.className = 'design-grid';
    grid.appendChild(renderColumnList());
    grid.appendChild(renderShelves());
    root.appendChild(designSection('Axes', grid));
    root.appendChild(designSection('Settings', renderOptions()));
  }
  root.appendChild(renderHaskellPane());
}

// Says how much of the notebook's data the panel can see, so the tab reads
// as the place your datasets are, not an empty canvas.
function dataSectionLabel() {
  const n = designer.sources.filter((s) => s.kind === 'binding' && !s.disabled).length;
  if (!n) return 'Your data';
  return 'Your data · ' + n + (n === 1 ? ' table' : ' tables') + ' in this notebook';
}

function designSection(label, body) {
  const sec = document.createElement('section');
  sec.className = 'design-section';
  const h = document.createElement('div');
  h.className = 'design-label';
  h.textContent = label;
  sec.append(h, body);
  return sec;
}

function renderDesignHeader() {
  const h = document.createElement('div');
  h.className = 'design-head';
  const title = document.createElement('span');
  title.className = 'design-title';
  title.textContent =
    designer.cellId == null
      ? 'New chart'
      : (designer.spec && designer.spec.title) || 'Chart ' + cellNumberOf(designer.cellId);
  h.appendChild(title);
  if (designer.cellId != null) {
    const edit = designButton('Code', () => jumpToCode(designer.cellId));
    edit.title = 'Show the Haskell behind this chart';
    const ask = designButton('Ask AI', () => askAiAbout(designer.cellId));
    h.append(edit, ask);
  }
  return h;
}

function designButton(label, onClick, cls) {
  const b = document.createElement('button');
  b.className = 'design-btn' + (cls ? ' ' + cls : '');
  b.textContent = label;
  b.onclick = onClick;
  return b;
}

async function designerRefreshSources() {
  designer.sources = await listVizSources();
  if (designer.source && !designer.columns.length) {
    const match = designer.sources.find((s) => s.ref === designer.source.ref && s.columns.length);
    if (match) designer.columns = match.columns;
  }
  renderDesigner();
}

// Choosing a source: a file is loaded into a typed binding first, and an
// untyped frame is frozen to a schema the compiler derives, so the chart cell
// always reads a typed frame and reruns like any other cell.
async function designerSetSource(src) {
  if (!src || src.disabled) return;
  if (designer.afterId == null) designer.afterId = lastCellId();
  if (src.kind === 'file') {
    const name = suggestBindingName(src.ref);
    const columns = await fileSourceColumns(src.ref).catch(() => []);
    const loaded = await insertLoadCell(src.ref, { after: designer.afterId, name });
    await api('POST', 'run/' + loaded.cellId).catch(() => {});
    designer.afterId = loaded.cellId;
    src = { kind: 'binding', ref: loaded.name, label: loaded.name, columns };
  } else if (src.untyped) {
    const frozen = await insertFreezeCell(
      src.ref,
      src.cellId != null ? src.cellId : designer.afterId
    );
    await api('POST', 'run/' + frozen.cellId).catch(() => {});
    designer.afterId = frozen.cellId;
    src = { kind: 'binding', ref: frozen.name, label: frozen.name, columns: src.columns };
  }
  designer.source = src;
  designer.columns = src.columns || [];
  designer.pickingSource = false;
  if (!designer.columns.length && src.ref) {
    designer.columns = await fetchBindingColumns(src.ref);
    src.columns = designer.columns;
  }
  if (!designer.spec || !designer.spec.x)
    designer.spec = defaultSpecFor(src, designer.spec && designer.spec.mark);
  renderDesigner();
  scheduleApply(0);
}

function scheduleApply(delay) {
  clearTimeout(designer.timer);
  designer.timer = setTimeout(applySpec, delay == null ? DESIGN_APPLY_DELAY : delay);
}

// Spec → canonical source → the cell. In reactive mode the write runs the
// cell, which is the preview; deferred mode stores it and offers Apply.
async function applySpec() {
  if (!designer.spec || !designer.source || designer.blocked) return;
  let res;
  try {
    res = await api('POST', 'viz/source', {
      vsrSpec: designer.spec,
      vsrData: designer.source.ref,
      vsrCellId: designer.cellId == null ? undefined : designer.cellId,
    });
  } catch (e) {
    setStatus('chart: ' + e.message, 'error');
    return;
  }
  const src = res.vsSource;
  if (designer.cellId == null) {
    const id = await addCell(designer.afterId, 'CodeCell', 'Haskell', src);
    designer.cellId = id;
    vizLastSource.set(id, src);
    designer.ownership = 'canonical';
    const el = document.querySelector(`.cell[data-id="${id}"]`);
    if (el) el.classList.add('designing');
    api('POST', 'run/' + id).catch(() => {});
  } else {
    if (res.vsOwnership === 'edited' && designer.ownership !== 'canonical') {
      designer.ownership = 'edited';
      designer.message = res.vsMessage;
      designer.blocked = true;
      renderDesigner();
      return;
    }
    vizLastSource.set(designer.cellId, src);
    const cm = editors[designer.cellId];
    if (cm && cm.getValue() !== src) cm.setValue(src);
    const cell = notebook && notebook.nbCells.find((c) => c.cellId === designer.cellId);
    if (cell) cell.cellSource = src;
    if (runMode === 'deferred')
      await api('PUT', `cell/${designer.cellId}/source`, { ucSource: src });
    else await api('PUT', `cell/${designer.cellId}`, { ucSource: src });
  }
  renderHaskellPaneInPlace(src);
}

// Server verdict on whether the bound cell is still the builder's to rewrite.
async function designerCheckOwnership() {
  if (designer.cellId == null || !designer.spec) return;
  try {
    const res = await api('POST', 'viz/source', {
      vsrSpec: designer.spec,
      vsrData: designer.source ? designer.source.ref : undefined,
      vsrCellId: designer.cellId,
    });
    designer.ownership = res.vsOwnership;
    designer.message = res.vsMessage;
    designer.blocked = res.vsOwnership === 'edited';
    if (!designer.source && res.vsDataExpr && res.vsDataExpr !== '_data')
      designer.source = {
        kind: 'binding',
        ref: res.vsDataExpr,
        label: res.vsDataExpr,
        columns: designer.columns,
      };
    renderDesigner();
  } catch (_e) {
    /* offline; the panel still shows the spec */
  }
}

// A result landed for the bound cell: columns if none were known yet, and a
// fresh verdict. The chart's own rows only carry the columns it uses, so a
// fuller list, once learned, is kept.
function designerOnCellResult(cellId) {
  if (designer.cellId !== cellId) return;
  const known = vizSpecs.get(cellId);
  if (known && !designer.columns.length) designer.columns = vizColumns(known.cols, known.types);
  renderDesigner();
  if (designer.source && designer.source.ref)
    fetchBindingColumns(designer.source.ref).then((full) => {
      if (!full.length || designer.cellId !== cellId) return;
      designer.columns = full;
      designer.source.columns = full;
      renderDesigner();
    });
}

// The kernel knows every column of a frame binding, typed or not.
async function fetchBindingColumns(ref) {
  if (!/^[a-z_][\w']*$/.test(ref || '')) return [];
  try {
    const r = await api('GET', 'viz/columns?binding=' + encodeURIComponent(ref));
    if (r.vcError || !r.vcCols.length) return [];
    return vizColumns(r.vcCols, r.vcTypes);
  } catch (_e) {
    return [];
  }
}
