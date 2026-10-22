// ── Data panel: rendering a preview, and turning it into a cell ──
// Types that read as numbers, so their columns align right like a spreadsheet.
const DATA_NUM_TYPES = ['Int', 'Double'];

function renderDataset(detail, d) {
  detail.innerHTML = '';
  detail.appendChild(datasetHead(d));
  if (!d.dpDelimited) {
    const why = document.createElement('div');
    why.className = 'data-empty';
    why.textContent = d.dpReason || 'That file does not read as a table.';
    detail.appendChild(why);
  } else {
    detail.appendChild(datasetSchema(d));
    // Parquet is read from its footer, which carries the schema and the row
    // count but no values. Headers over an empty body would read as a bug.
    if (d.dpRows.length) detail.appendChild(datasetTable(d));
    else detail.appendChild(datasetNote('Schema only — no row preview for this format.'));
  }
  detail.appendChild(datasetActions(d, detail));
}

function datasetNote(text) {
  const el = document.createElement('div');
  el.className = 'data-empty';
  el.textContent = text;
  return el;
}

function datasetHead(d) {
  const head = document.createElement('div');
  head.className = 'data-detail-head';
  const name = document.createElement('div');
  name.className = 'data-detail-name';
  name.textContent = d.dpPath;
  head.appendChild(name);
  const meta = document.createElement('div');
  meta.className = 'data-detail-meta';
  meta.textContent = datasetCounts(d);
  head.appendChild(meta);
  return head;
}

// Say which bytes a count was taken over. The sample stops at 64 KB, so a row
// count from a bigger file is a count of the sample and must not read as the
// file's own.
function datasetCounts(d) {
  const size = fmtBytes(d.dpBytes);
  if (!d.dpDelimited) return size;
  const cols = d.dpColumns.length + (d.dpColumns.length === 1 ? ' column' : ' columns');
  const rows = d.dpRowCount.toLocaleString();
  return d.dpTruncated
    ? size + ' · ' + cols + ' · ' + rows + ' rows in the first 64 KB'
    : size + ' · ' + cols + ' · ' + rows + ' rows';
}

function datasetSchema(d) {
  const wrap = document.createElement('div');
  wrap.className = 'data-schema';
  d.dpColumns.forEach((c) => {
    const row = document.createElement('div');
    row.className = 'data-col';
    const nm = document.createElement('span');
    nm.className = 'data-col-name';
    nm.textContent = c.dcName === null ? 'column ' + c.dcIndex : c.dcName;
    const ty = document.createElement('span');
    ty.className = 'data-col-type';
    ty.textContent = c.dcType;
    row.appendChild(nm);
    row.appendChild(ty);
    wrap.appendChild(row);
  });
  return wrap;
}

function datasetTable(d) {
  const scroll = document.createElement('div');
  scroll.className = 'data-table-scroll';
  const table = document.createElement('table');
  table.className = 'data-table';
  const numeric = d.dpColumns.map((c) => DATA_NUM_TYPES.includes(c.dcType));

  const thead = document.createElement('thead');
  const hr = document.createElement('tr');
  hr.appendChild(cellEl('th', '', 'data-idx'));
  d.dpColumns.forEach((c, i) => {
    hr.appendChild(
      cellEl('th', c.dcName === null ? String(c.dcIndex) : c.dcName, numeric[i] ? 'num' : '')
    );
  });
  thead.appendChild(hr);
  table.appendChild(thead);

  const tbody = document.createElement('tbody');
  d.dpRows.forEach((row, n) => {
    const tr = document.createElement('tr');
    tr.appendChild(cellEl('td', String(n), 'data-idx'));
    row.forEach((v, i) => tr.appendChild(cellEl('td', v, numeric[i] ? 'num' : '')));
    tbody.appendChild(tr);
  });
  table.appendChild(tbody);
  scroll.appendChild(table);
  return scroll;
}

function cellEl(tag, text, cls) {
  const el = document.createElement(tag);
  if (cls) el.className = cls;
  el.textContent = text;
  return el;
}

function datasetActions(d, detail) {
  const bar = document.createElement('div');
  bar.className = 'data-actions';
  const loaded = loadedBindingFor(d.dpPath);
  if (loaded) {
    const note = document.createElement('div');
    note.className = 'data-loaded-note';
    note.textContent = 'Loaded as ' + loaded.name + ' — ';
    const jump = document.createElement('a');
    jump.href = '#';
    jump.textContent = 'show cell';
    jump.onclick = (e) => {
      e.preventDefault();
      jumpToCode(loaded.cellId);
    };
    note.appendChild(jump);
    bar.appendChild(note);
  }
  const name = suggestBindingName(d.dpPath);
  const btn = document.createElement('button');
  btn.className = 'data-load-btn';
  btn.textContent = (loaded ? '＋ Load again as ' : '＋ Load as ') + name;
  btn.onclick = async () => {
    const cell = await insertLoadCell(d.dpPath, { name });
    await api('POST', 'run/' + cell.cellId).catch(() => {});
    renderDataList();
    renderDataset(detail, d);
  };
  bar.appendChild(btn);
  return bar;
}

// The reader for a path, by extension. Parquet has no sniffer on the server
// but dataframe reads it, so the cell is still worth offering.
// The deps a typed load needs: the package, and the extensions the schema
// splice and `DT.col @"name"` use.
const TYPED_DEPS_CELL =
  '-- cabal: build-depends: dataframe, text\n' +
  '-- cabal: default-extensions: DataKinds, TemplateHaskell, TypeApplications, OverloadedStrings';

// Does any cell already declare the dataframe package?
function declaresDataframe() {
  const cells = (notebook && notebook.nbCells) || [];
  return cells.some((c) => /^--\s*cabal:.*\bdataframe\b/m.test(c.cellSource || ''));
}

// Does any cell already turn on the extensions typed loading needs?
function declaresTypedExtensions() {
  const cells = (notebook && notebook.nbCells) || [];
  return cells.some((c) =>
    /^--\s*cabal:\s*default-extensions:.*\bTemplateHaskell\b/m.test(c.cellSource || '')
  );
}

// The schema type a binding's file gets: `sales` is read as type `Sales`.
function schemaNameFor(name) {
  const parts = name.split(/[^A-Za-z0-9]+/).filter(Boolean);
  const base = parts.map((p) => p.charAt(0).toUpperCase() + p.slice(1)).join('');
  return base || 'Data';
}

/* Insert the cell that loads this file as a typed frame, and the deps cell it
needs when the notebook has none. They are separate cells because re-running a
`-- cabal:` cell restarts the kernel, which would wipe the binding it just
made. The schema is derived from the file by the dataframe library's own
splice, so the cell is plain dataframe code. */
async function insertLoadCell(path, opts) {
  opts = opts || {};
  const cells = (notebook && notebook.nbCells) || [];
  let after = opts.after != null ? opts.after : cells.length ? cells[cells.length - 1].cellId : -1;
  const name = opts.name || suggestBindingName(path);
  if (!declaresDataframe() || !declaresTypedExtensions()) {
    after = await addCell(after, 'CodeCell', 'Haskell', TYPED_DEPS_CELL);
  }
  const schema = schemaNameFor(name);
  const file = './' + path.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
  const parquet = dataExt(path) === 'parquet';
  const reader = parquet ? 'TParquet.readParquet' : 'TCSV.readCsv';
  const readerImport = parquet
    ? 'import qualified DataFrame.Typed.IO.Parquet as TParquet'
    : 'import qualified DataFrame.Typed.IO.CSV as TCSV';
  const splice = parquet ? 'DT.deriveSchemaFromParquetFile' : 'DT.deriveSchemaFromCsvFile';
  const body =
    'import qualified DataFrame.Typed as DT\n' +
    readerImport +
    '\n\n$(' +
    splice +
    ' "' +
    schema +
    '" "' +
    file +
    '")\n' +
    name +
    ' <- ' +
    reader +
    ' @' +
    schema +
    ' "' +
    file +
    '"';
  const cellId = await addCell(after, 'CodeCell', 'Haskell', body);
  setStatus('inserted typed load cell for ' + path, 'ok');
  return { cellId, name };
}

/* Insert the cell that freezes an untyped frame to a schema the compiler
reads off it, so a chart can be typed against it. */
async function insertFreezeCell(name, after) {
  if (!declaresTypedExtensions()) {
    after = await addCell(after, 'CodeCell', 'Haskell', TYPED_DEPS_CELL);
  }
  const schema = schemaNameFor(name);
  const typedName = name + 'Typed';
  const cellId = await addCell(
    after,
    'CodeCell',
    'Haskell',
    'import qualified DataFrame.Typed as DT\n\n' +
      '$(DT.deriveSchema "' +
      schema +
      '" ' +
      name +
      ')\n' +
      typedName +
      ' <- DT.freezeOrThrow @' +
      schema +
      ' ' +
      name
  );
  setStatus('inserted typed frame ' + typedName, 'ok');
  return { cellId, name: typedName };
}
