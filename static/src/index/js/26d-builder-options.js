// ── Design panel: field popover, chart options, defaults, code pane ─
// The second half of the panel's rendering; the first is in 26b.

// The per-field popover: aggregate, bin, type, sort, scale, axis title.
function openShelfPopover(anchor, key, index) {
  closeCtxMenu();
  const enc = shelfEncodings(key)[index];
  if (!enc) return;
  const menu = document.createElement('div');
  menu.className = 'ctx-menu shelf-pop';
  menu.onclick = (e) => e.stopPropagation();
  const field = (label, control) => {
    const row = document.createElement('label');
    row.className = 'shelf-pop-row';
    const l = document.createElement('span');
    l.textContent = label;
    row.append(l, control);
    menu.appendChild(row);
  };
  const select = (values, current, onChange) => {
    const s = document.createElement('select');
    for (const v of values) {
      const o = document.createElement('option');
      o.value = v;
      o.textContent = v || 'none';
      if ((current || '') === v) o.selected = true;
      s.appendChild(o);
    }
    s.onchange = () => {
      onChange(s.value || undefined);
      renderDesigner();
      scheduleApply();
    };
    return s;
  };
  const set = (k) => (v) => {
    if (v === undefined) delete enc[k];
    else enc[k] = v;
  };
  const head = document.createElement('div');
  head.className = 'shelf-pop-head';
  head.textContent = enc.field === '*' ? 'row count' : enc.field;
  menu.appendChild(head);
  field('aggregate', select(DESIGN_AGGREGATES, enc.aggregate, set('aggregate')));
  if (key === 'x' || key === 'y') {
    const bin = document.createElement('input');
    bin.type = 'checkbox';
    bin.checked = !!enc.bin;
    bin.onchange = () => {
      set('bin')(bin.checked ? 10 : undefined);
      renderDesigner();
      scheduleApply();
    };
    field('bin', bin);
  }
  field('scale', select(['', 'linear', 'log'], enc.scale, set('scale')));
  if (key === 'x' || key === 'y') {
    const zero = document.createElement('input');
    zero.type = 'checkbox';
    zero.checked = enc.zero !== false;
    zero.title = 'Start the axis at zero; off, the axis fits the data';
    zero.onchange = () => {
      set('zero')(zero.checked ? undefined : false);
      renderDesigner();
      scheduleApply();
    };
    field('from zero', zero);
  }
  const remove = designButton('Remove from shelf', () => {
    closeCtxMenu();
    clearShelf(key, index);
  });
  remove.className += ' shelf-pop-remove';
  menu.appendChild(remove);
  document.body.appendChild(menu);
  const r = anchor.getBoundingClientRect();
  menu.style.left = Math.max(8, Math.min(r.left, window.innerWidth - menu.offsetWidth - 8)) + 'px';
  menu.style.top = Math.min(r.bottom + 4, window.innerHeight - menu.offsetHeight - 8) + 'px';
  activeCtxMenu = menu;
}

function renderOptions() {
  const box = document.createElement('div');
  box.className = 'design-options';
  const s = designer.spec;
  const opt = (label, control) => {
    const row = document.createElement('label');
    row.className = 'design-opt';
    const l = document.createElement('span');
    l.className = 'design-label';
    l.textContent = label;
    row.append(l, control);
    box.appendChild(row);
  };
  const title = document.createElement('input');
  title.type = 'text';
  title.value = s.title || '';
  title.placeholder = 'untitled';
  title.oninput = () => {
    if (title.value) s.title = title.value;
    else delete s.title;
    scheduleApply();
  };
  opt('Title', title);
  const height = document.createElement('input');
  height.type = 'number';
  height.value = s.height || '';
  height.placeholder = '300';
  height.onchange = () => {
    if (height.value) s.height = Number(height.value);
    else delete s.height;
    scheduleApply();
  };
  opt('Height', height);
  return box;
}

function defaultSpecFor(source, mark) {
  const cols = source.columns || [];
  const temporal = cols.find((c) => c.type === 'temporal');
  const nominal = cols.find((c) => c.type === 'nominal');
  const quant = cols.find((c) => c.type === 'quantitative');
  const spec = { mark: mark || (temporal ? 'line' : 'bar') };
  const xcol = temporal || nominal || cols[0];
  if (xcol) spec.x = { field: xcol.name };
  if (quant && quant !== xcol)
    spec.y = {
      field: quant.name,
      aggregate: xcol && xcol.type !== 'quantitative' ? 'sum' : undefined,
    };
  else spec.y = { field: '*', aggregate: 'count' };
  if (spec.y && spec.y.aggregate === undefined) delete spec.y.aggregate;
  fitPieShelves(spec);
  fitScatterAxes(spec);
  return spec;
}

// The Haskell the panel writes, folded away: open it to read, press the
// button to edit it in the cell.
function renderHaskellPane() {
  const box = document.createElement('details');
  box.className = 'design-code';
  box.open = !!designer.codeOpen;
  box.ontoggle = () => {
    designer.codeOpen = box.open;
  };
  const h = document.createElement('summary');
  h.className = 'design-label';
  h.textContent = 'Haskell';
  const pre = document.createElement('pre');
  pre.id = 'design-haskell';
  pre.textContent =
    designer.cellId != null && editors[designer.cellId]
      ? editors[designer.cellId].getValue()
      : vizLastSource.get(designer.cellId) || '';
  box.append(h, pre);
  if (designer.cellId != null) {
    const edit = designButton('Edit in the cell', () => jumpToCode(designer.cellId));
    box.appendChild(edit);
  }
  return box;
}

function renderHaskellPaneInPlace(src) {
  const pre = document.getElementById('design-haskell');
  if (pre) pre.textContent = src;
}

// The notice shown when the bound cell was edited by hand, and the fork
// that moves the panel onto a fresh cell.
function renderOwnershipNotice() {
  const n = document.createElement('div');
  n.className = 'design-notice';
  n.innerHTML = '<div class="design-notice-text"></div>';
  n.querySelector('.design-notice-text').textContent =
    designer.message || 'This cell was edited by hand since the builder wrote it.';
  const row = document.createElement('div');
  row.className = 'design-notice-actions';
  row.append(
    designButton('Create new chart cell', () => forkDesignerCell(), 'primary'),
    designButton('Overwrite', async () => {
      const ok = await askConfirm({
        title: 'Overwrite this cell?',
        message: "The hand edits in this cell will be replaced by the builder's chart.",
        confirmLabel: 'Overwrite',
        danger: true,
      });
      if (!ok) return;
      designer.ownership = 'canonical';
      designer.blocked = false;
      renderDesigner();
      scheduleApply(0);
    }),
    designButton('Dismiss', () => {
      designer.blocked = true;
      designer.ownership = 'dismissed';
      renderDesigner();
    })
  );
  n.appendChild(row);
  return n;
}

// The builder's copy of a hand-edited chart goes into a fresh cell after it.
function forkDesignerCell() {
  const after = designer.cellId;
  designer.cellId = null;
  designer.afterId = after;
  designer.ownership = null;
  designer.blocked = false;
  document.querySelectorAll('.cell.designing').forEach((c) => c.classList.remove('designing'));
  renderDesigner();
  scheduleApply(0);
}

// A scatter's axes fit the data rather than start at zero: a point cloud
// far from the origin otherwise bunches in a corner. Other marks keep zero.
function fitScatterAxes(spec) {
  for (const key of ['x', 'y']) {
    const enc = spec && spec[key];
    if (!enc) continue;
    const col = designer.columns.find((c) => c.name === enc.field);
    const numeric = enc.aggregate || (col && col.type === 'quantitative');
    if (spec.mark === 'point' && numeric && !enc.bin) enc.zero = false;
    else if (enc.zero === false && spec.mark !== 'point') delete enc.zero;
  }
}
