// ── Data sources for the Design panel ────────────────────────────
// What a chart can be drawn from, found without parsing Haskell: tables the
// notebook has displayed (their columns are in the grid's own JSON), charts
// already drawn (their runtime posts the columns), live bindings whose type
// looks tabular, and data files from the Data pane.
//
// A source is {kind:'binding'|'file'|'frame', ref, label, columns:[{name,type,raw}]}
// where ref is the Haskell expression (a binding name) or the file path.

function vizTypeOf(raw, name, samples) {
  const r = String(raw || '')
    .trim()
    .replace(/^Maybe\s+/, '');
  if (/^(int|num|Int|Integer|Double|Float|Word|Int64|Int32|Float64|Scientific|number)$/.test(r))
    return 'quantitative';
  if (/^(bool|Bool|boolean)$/.test(r)) return 'nominal';
  if (/Day|UTCTime|Date|Time|date/.test(r)) return 'temporal';
  if (/(^|_)(date|time|day|month|year|ts)(_|$)|_at$/i.test(name || '')) {
    const vs = (samples || []).filter(Boolean).slice(0, 20);
    if (vs.length && vs.every((v) => /^\d{4}-\d{2}-\d{2}/.test(v))) return 'temporal';
  }
  if (!r && samples && samples.length) {
    const vs = samples.filter((v) => v !== '' && v != null).slice(0, 50);
    if (vs.length && vs.every((v) => !isNaN(Number(String(v).replace(/,/g, '')))))
      return 'quantitative';
  }
  return 'nominal';
}

function vizColumns(cols, types, rows) {
  return (cols || []).map((name, i) => ({
    name,
    raw: (types && types[i]) || '',
    type: vizTypeOf(
      types && types[i],
      name,
      (rows || []).map((r) => r[i])
    ),
  }));
}

// The names a cell binds: `x <- …`, `x = …`, or a bare identifier as its
// last line (the value it displays).
function cellBindingNames(cellId) {
  const cell = notebook && notebook.nbCells.find((c) => c.cellId === cellId);
  if (!cell) return [];
  const src = cell.cellSource || '';
  const names = [];
  for (const m of src.matchAll(/^([a-z_][\w']*)\s*(?:<-|=)(?!=)/gm)) names.push(m[1]);
  const lines = src.split('\n').filter((l) => l.trim() && !l.trim().startsWith('--'));
  const last = (lines[lines.length - 1] || '').trim();
  if (/^[a-z_][\w'.]*$/.test(last)) names.push(last.split('.').pop());
  return [...new Set(names)];
}

// The expression a chart cell draws from: what follows the first `C.chart`.
function chartDataExprOf(cellId) {
  const cell = notebook && notebook.nbCells.find((c) => c.cellId === cellId);
  const m = cell && (cell.cellSource || '').match(/\bC\.chart\s+(\(?[^\s|]+\)?)/);
  return m ? m[1].trim() : null;
}

// Tables the notebook shows: each grid output carries its columns and types
// in a JSON block, read from the cell's own outputs since the mounted grid
// may have replaced the markup it was born with.
function scanFrameSources() {
  const out = [];
  for (const cell of (notebook && notebook.nbCells) || []) {
    const cellEl = document.querySelector(`.cell[data-id="${cell.cellId}"]`);
    const num = cellEl ? cellEl.dataset.num : cell.cellId;
    for (const o of cell.cellOutputs || []) {
      const html = o.oiOutput || '';
      const m = html.match(/class="sabela-grid-data">([\s\S]*?)<\/script>/);
      if (!m) continue;
      let data;
      try {
        data = JSON.parse(m[1]);
      } catch (_e) {
        continue;
      }
      const names = cellBindingNames(cell.cellId);
      const columns = vizColumns(data.cols, data.types, data.rows);
      if (names.length === 1) {
        out.push({
          kind: 'binding',
          ref: names[0],
          label: names[0] + ' (table in cell ' + num + ')',
          cellId: cell.cellId,
          columns,
          rowCount: data.total,
        });
      } else {
        out.push({
          kind: 'frame',
          ref: null,
          label: 'table in cell ' + num + ' (name it to chart it)',
          cellId: cell.cellId,
          columns,
          disabled: true,
        });
      }
    }
  }
  return out;
}

// Charts already drawn: the runtime posted their columns and the cell names
// the data it draws from.
function vizOutputSources() {
  const out = [];
  vizSpecs.forEach((entry, cellId) => {
    const ref = chartDataExprOf(cellId);
    if (!ref || !/^[a-z_][\w']*$/.test(ref)) return;
    const cellEl = document.querySelector(`.cell[data-id="${cellId}"]`);
    out.push({
      kind: 'binding',
      ref,
      label: ref + ' (chart in cell ' + (cellEl ? cellEl.dataset.num : cellId) + ')',
      cellId,
      columns: vizColumns(entry.cols, entry.types),
    });
  });
  return out;
}

async function bindingSources(known) {
  let res;
  try {
    res = await api('GET', 'bindings');
  } catch (_e) {
    return [];
  }
  if (!res || res.brStatus !== 'ok') return [];
  for (const b of res.brBindings || []) {
    const isUntyped =
      /\bDataFrame\b/.test(b.beType || '') && !/\bTypedDataFrame\b/.test(b.beType || '');
    known.filter((s) => s.ref === b.beName).forEach((s) => (s.untyped = isUntyped));
  }
  return (res.brBindings || [])
    .filter(
      (b) =>
        b.beLive && b.beKind === 'value' && /\bDataFrame\b|\bTypedDataFrame\b/.test(b.beType || '')
    )
    .map((b) => {
      const match = known.find((s) => s.ref === b.beName && s.columns.length);
      return {
        kind: 'binding',
        ref: b.beName,
        label: b.beName + ' :: ' + (b.beType || '').slice(0, 40),
        columns: match ? match.columns : [],
        hint: match ? '' : 'binding',
        untyped: /\bDataFrame\b/.test(b.beType || '') && !/\bTypedDataFrame\b/.test(b.beType || ''),
      };
    });
}

async function fileSources() {
  if (typeof walkForData !== 'function') return [];
  if (!dataLoaded) {
    dataFiles = await walkForData();
    dataLoaded = true;
  }
  return dataFiles
    .filter((f) => isPreviewable(f.fePath))
    .map((f) => ({ kind: 'file', ref: f.fePath, label: f.fePath + ' (file)', columns: [] }));
}

// Every source the builder can offer, tables first; each binding once.
async function listVizSources() {
  const seen = new Set();
  const out = [];
  const push = (s) => {
    const key = s.kind + ':' + (s.ref || s.label);
    if (seen.has(key)) return;
    seen.add(key);
    out.push(s);
  };
  scanFrameSources().forEach(push);
  vizOutputSources().forEach(push);
  (await bindingSources(out)).forEach(push);
  (await fileSources()).forEach(push);
  return out;
}

// A file's columns, read from its preview; types follow the sniffer's names.
async function fileSourceColumns(path) {
  const d = await api('GET', 'dataset/preview?path=' + encodeURIComponent(path) + '&rows=25');
  if (!(d.dpColumns || []).some((c) => c.dcName)) return [];
  const cols = (d.dpColumns || []).map((c, i) => c.dcName || 'col' + (i + 1));
  const types = (d.dpColumns || []).map((c) => c.dcType || '');
  return vizColumns(cols, types, d.dpRows || []);
}

function specFromOutputDom(cellId) {
  const cellEl = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!cellEl) return null;
  for (const f of cellEl.querySelectorAll('.cell-output iframe')) {
    try {
      const holder = f.contentDocument && f.contentDocument.querySelector('script.sabela-viz-spec');
      if (holder) {
        const p = JSON.parse(holder.textContent);
        return { spec: p.sabela, cols: p.data.cols, types: p.data.types };
      }
    } catch (_e) {
      /* not readable; try the next frame */
    }
  }
  return null;
}

// The chart runtime posted a mounted chart's spec and columns.
function noteVizSpec(d) {
  if (!d || typeof d.cellId !== 'number') return;
  vizSpecs.set(d.cellId, { spec: d.spec, cols: d.cols || [], types: d.types || [] });
  const head = document.querySelector(`.cell[data-id="${d.cellId}"] .viz-title`);
  const cell = notebook && notebook.nbCells.find((c) => c.cellId === d.cellId);
  if (head && cell) head.textContent = vizTitleOf(cell);
  if (typeof designerOnCellResult === 'function') designerOnCellResult(d.cellId);
}

// What kind of object a cell is, for the actions it gets.
function objectKindOf(cell) {
  if (!cell) return 'code';
  if (cell.cellType !== 'CodeCell') return 'prose';
  const src = cell.cellSource || '';
  if (/\bC\.chart\b/.test(src) || vizSpecs.has(cell.cellId)) return 'chart';
  if (
    /^\s*\w+\s*<-\s*(?:mkWidget\s*\()?\s*(slider|dropdown|checkbox|textInput|button)\b/m.test(src)
  )
    return 'widget';
  if (/^\s*(image|video|audio)\s+"/m.test(src)) return 'media';
  if ((cell.cellOutputs || []).some((o) => (o.oiOutput || '').includes('class="sabela-grid"')))
    return 'table';
  return 'code';
}

function suggestBindingName(path) {
  const stem = path
    .split('/')
    .pop()
    .replace(/\.[^.]+$/, '');
  let name = stem.replace(/[^A-Za-z0-9_]+/g, '_').replace(/^_+|_+$/g, '');
  if (!name || !/^[a-z]/.test(name)) name = 'df_' + name;
  name = name.charAt(0).toLowerCase() + name.slice(1);
  const taken = new Set(
    (notebook ? notebook.nbCells : []).flatMap((c) => cellBindingNames(c.cellId))
  );
  let candidate = name;
  for (let i = 2; taken.has(candidate); i++) candidate = name + i;
  return candidate;
}
