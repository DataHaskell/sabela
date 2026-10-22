// ── Design panel: source picker, columns, marks, shelves, options ─
// Renders from `designer.spec`, the flat wire form the server reads:
// {mark, x, y, color, size, shape, opacity, theta, row, column, tooltip:[…],
//  layers:[…], title, width, height, select:{name,type}}.

const DESIGN_SHELVES = [
  ['x', 'X axis'],
  ['y', 'Y axis'],
  ['color', 'Color'],
  ['size', 'Size'],
  ['column', 'Facet'],
  ['tooltip', 'Tooltip'],
];
// A pie has no axes: its slices are a colour and their sizes an angle. An x
// on an arc is a position, which draws one small pie per x value.
const PIE_SHELVES = [
  ['color', 'Slices'],
  ['theta', 'Size of slice'],
  ['column', 'Facet'],
  ['tooltip', 'Tooltip'],
];
const DESIGN_MARKS = [
  ['bar', 'Bar'],
  ['line', 'Line'],
  ['area', 'Area'],
  ['point', 'Scatter'],
  ['arc', 'Pie'],
  ['boxplot', 'Box plot'],
  ['tick', 'Ticks'],
  ['rule', 'Rules'],
];
const DESIGN_AGGREGATES = ['', 'sum', 'mean', 'median', 'min', 'max', 'count'];
const DESIGN_TYPE_GLYPH = { quantitative: '#', nominal: 'A', temporal: 'T', ordinal: 'o' };

// The data picker: a list of what the notebook can chart until one is
// chosen, then a pill naming the choice with a way back to the list.
function renderSourcePicker() {
  const box = document.createElement('div');
  box.className = 'design-data';
  if (designer.source && !designer.pickingSource) {
    const pill = document.createElement('button');
    pill.className = 'data-pill';
    pill.innerHTML =
      '<svg class="icon-svg"><use href="#i-table"/></svg><span class="data-pill-name"></span><span class="data-pill-hint"></span>';
    pill.querySelector('.data-pill-name').textContent = designer.source.label;
    pill.querySelector('.data-pill-hint').textContent = designer.columns.length
      ? designer.columns.length + ' columns'
      : '';
    pill.title = 'Chart different data';
    pill.onclick = () => {
      designer.pickingSource = true;
      renderDesigner();
    };
    box.appendChild(pill);
    return box;
  }
  const list = document.createElement('div');
  list.className = 'data-choices';
  if (!designer.sources.length) {
    const empty = document.createElement('div');
    empty.className = 'design-hint';
    empty.textContent =
      'Nothing to chart yet. Run a cell that shows a table, or add a data file from the Data pane.';
    list.appendChild(empty);
  }
  for (const s of designer.sources) {
    const row = document.createElement('button');
    row.className = 'data-choice' + (s.disabled ? ' disabled' : '');
    row.disabled = !!s.disabled;
    row.innerHTML =
      '<svg class="icon-svg"><use href="#i-table"/></svg><span class="data-choice-name"></span><span class="data-choice-hint"></span>';
    row.querySelector('.data-choice-name').textContent = s.label;
    row.querySelector('.data-choice-hint').textContent =
      s.hint || (s.kind === 'file' ? 'file' : s.columns.length + ' columns');
    row.onclick = () => designerSetSource(s);
    list.appendChild(row);
  }
  const refresh = designButton('Look again', () => designerRefreshSources());
  refresh.className += ' data-refresh';
  list.appendChild(refresh);
  if (designer.source) {
    const back = designButton('Keep ' + designer.source.label, () => {
      designer.pickingSource = false;
      renderDesigner();
    });
    back.className += ' data-refresh';
    list.appendChild(back);
  }
  box.appendChild(list);
  box.appendChild(renderAddData());
  return box;
}

function renderColumnList() {
  const box = document.createElement('div');
  box.className = 'design-columns';
  const h = document.createElement('div');
  h.className = 'design-label';
  h.textContent = 'Columns';
  box.appendChild(h);
  for (const c of designer.columns) box.appendChild(makeColumnChip(c, null));
  const hint = document.createElement('div');
  hint.className = 'design-hint';
  hint.textContent = 'Drag a column onto an axis, or click one and then the axis.';
  box.appendChild(hint);
  return box;
}

function renderMarkPicker() {
  const box = document.createElement('div');
  box.className = 'mark-grid';
  for (const [id, label] of DESIGN_MARKS) {
    const b = document.createElement('button');
    b.className = 'mark-tile' + (designer.spec && designer.spec.mark === id ? ' active' : '');
    b.innerHTML = `<svg class="icon-svg large"><use href="#i-viz-${id}"/></svg><span>${label}</span>`;
    b.title = label;
    b.onclick = () => {
      designer.spec.mark = id;
      fitPieShelves(designer.spec);
      fitScatterAxes(designer.spec);
      renderDesigner();
      scheduleApply();
    };
    box.appendChild(b);
  }
  return box;
}

function renderShelves() {
  const box = document.createElement('div');
  box.className = 'design-shelves';
  const pie = designer.spec && designer.spec.mark === 'arc';
  for (const [key, label] of pie ? PIE_SHELVES : DESIGN_SHELVES)
    box.appendChild(renderShelf(key, label));
  return box;
}

/* Move columns between the axes and the pie's shelves as the mark changes:
the category becomes the slices, the value their size, summed so each
category is one slice; and back again when the pie becomes another mark. */
function fitPieShelves(spec) {
  if (spec.mark === 'arc') {
    if (spec.x && !spec.color) spec.color = spec.x;
    if (spec.y && !spec.theta) spec.theta = spec.y;
    delete spec.x;
    delete spec.y;
    if (!spec.theta) spec.theta = { field: '*', aggregate: 'count' };
    if (spec.color && !spec.theta.aggregate) spec.theta.aggregate = 'sum';
  } else {
    if (spec.color && !spec.x) {
      spec.x = spec.color;
      delete spec.color;
    }
    if (spec.theta && !spec.y) spec.y = spec.theta;
    delete spec.theta;
  }
}

function renderShelf(key, label) {
  const row = document.createElement('div');
  row.className = 'dz-row';
  const l = document.createElement('span');
  l.className = 'design-label';
  l.textContent = label;
  const zone = document.createElement('div');
  zone.className = 'dz';
  zone.dataset.shelf = key;
  const encs = shelfEncodings(key);
  if (!encs.length) {
    const empty = document.createElement('span');
    empty.className = 'dz-empty';
    empty.textContent = designer.armed ? 'Click to place' : 'Drop a column';
    zone.appendChild(empty);
  }
  for (const [i, enc] of encs.entries()) zone.appendChild(makeShelfPill(key, i, enc));
  makeShelfDropTarget(zone, key);
  row.append(l, zone);
  return row;
}

function shelfEncodings(key) {
  const s = designer.spec || {};
  if (key === 'tooltip') return Array.isArray(s.tooltip) ? s.tooltip : [];
  return s[key] ? [s[key]] : [];
}

function shelfLabel(enc) {
  const f = enc.field === '*' ? 'rows' : enc.field;
  let t = enc.aggregate ? enc.aggregate + '(' + f + ')' : enc.bin ? 'bin(' + f + ')' : f;
  if (enc.sort) t += enc.sort === 'descending' ? ' ↓' : ' ↑';
  return t;
}

function makeShelfPill(key, index, enc) {
  const pill = document.createElement('span');
  pill.className = 'dz-pill';
  pill.draggable = true;
  const col = designer.columns.find((c) => c.name === enc.field);
  const glyph = enc.type ? DESIGN_TYPE_GLYPH[enc.type] : col ? DESIGN_TYPE_GLYPH[col.type] : '';
  pill.innerHTML = `<span class="col-glyph">${escapeHtml(glyph || '')}</span>${escapeHtml(shelfLabel(enc))}`;
  pill.title = 'click for aggregate, bin, sort and scale';
  pill.onclick = (e) => {
    e.stopPropagation();
    openShelfPopover(pill, key, index);
  };
  pill.ondragstart = (e) => {
    e.dataTransfer.setData(COL_DT, JSON.stringify({ name: enc.field, from: key, index }));
    e.dataTransfer.effectAllowed = 'move';
  };
  return pill;
}

// Put a column on a shelf; every shelf holds one (the tooltip is kept as a
// one-element list, the shape the wire spec gives it).
function assignShelf(key, name, from) {
  if (!designer.spec) designer.spec = { mark: 'bar' };
  const existing = from ? takeFromShelf(from.key, from.index) : null;
  const enc = existing || { field: name };
  if (key === 'tooltip') designer.spec.tooltip = [enc];
  else designer.spec[key] = enc;
  designer.armed = null;
  renderDesigner();
  scheduleApply();
}

function takeFromShelf(key, index) {
  if (!designer.spec) return null;
  if (key === 'tooltip') {
    const list = designer.spec.tooltip || [];
    const [enc] = list.splice(index, 1);
    if (!list.length) delete designer.spec.tooltip;
    return enc || null;
  }
  const enc = designer.spec[key] || null;
  delete designer.spec[key];
  return enc;
}

function clearShelf(key, index) {
  takeFromShelf(key, index);
  renderDesigner();
  scheduleApply();
}
