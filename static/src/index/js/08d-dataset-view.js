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
  detail.appendChild(datasetActions(d));
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

function datasetActions(d) {
  const bar = document.createElement('div');
  bar.className = 'data-actions';
  const btn = document.createElement('button');
  btn.className = 'data-load-btn';
  btn.textContent = '＋ Insert load cell';
  btn.onclick = () => insertLoadCell(d.dpPath);
  bar.appendChild(btn);
  return bar;
}

// The reader for a path, by extension. Parquet has no sniffer on the server
// but dataframe reads it, so the cell is still worth offering.
function datasetReader(path) {
  return dataExt(path) === 'parquet' ? 'D.readParquet' : 'D.readCsv';
}

// Does any cell already declare the dataframe package?
function declaresDataframe() {
  const cells = (notebook && notebook.nbCells) || [];
  return cells.some((c) => /^--\s*cabal:.*\bdataframe\b/m.test(c.cellSource || ''));
}

/* Insert the cell that loads this file, and the deps cell it needs when the
notebook has none. They are separate cells because re-running a `-- cabal:`
cell restarts the kernel, which would wipe the binding it just made. */
async function insertLoadCell(path) {
  const cells = (notebook && notebook.nbCells) || [];
  let after = cells.length ? cells[cells.length - 1].cellId : -1;
  if (!declaresDataframe()) {
    after = await addCell(after, 'CodeCell', 'Haskell', '-- cabal: build-depends: dataframe');
  }
  await addCell(
    after,
    'CodeCell',
    'Haskell',
    'import qualified DataFrame as D\n\n' +
      'df <- ' +
      datasetReader(path) +
      ' "./' +
      path.replace(/\\/g, '\\\\').replace(/"/g, '\\"') +
      '"'
  );
  setStatus('inserted load cell for ' + path, 'ok');
}
