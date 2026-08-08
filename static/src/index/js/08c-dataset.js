// ── Data panel: find delimited files, show their schema, load them ──
// The sample datasets shipped with the repo, surfaced as their own group the
// way Colab surfaces sample_data/ — a new notebook has something to open.
const DATA_SAMPLE_DIR = 'examples/data';

// Extensions worth listing. Deliberately narrower than the AI layer's
// dataExtensions, which also covers .md, .txt and .png — a Data pane listing
// every log and notebook in the tree is noise, not a dataset list.
const DATA_EXTS = ['csv', 'tsv', 'parquet', 'arrow', 'feather', 'xlsx'];

// Formats the server can describe without a kernel: delimited text by
// sniffing, parquet by its footer. The rest are listed but not previewed.
const DATA_PREVIEWABLE_EXTS = ['csv', 'tsv', 'parquet'];

// Directories a data hunt never wants, and a request budget so a deep tree
// cannot fan out into hundreds of listings.
const DATA_SKIP_DIRS = ['dist-newstyle', 'node_modules', '.git', '.stack-work', '_build', '.venv'];
const DATA_WALK_BUDGET = 80;
const DATA_WALK_DEPTH = 3;

let dataFiles = [];
let dataSelected = null;
let dataLoaded = false;

// Switch the sidebar between Files and Data. The dataset walk costs a burst of
// listings, so it runs the first time the pane is actually looked at.
function selectSidePane(name) {
  document
    .querySelectorAll('.side-tab')
    .forEach((b) => b.classList.toggle('active', b.dataset.pane === name));
  document
    .querySelectorAll('.side-tab')
    .forEach((b) => b.setAttribute('aria-selected', String(b.dataset.pane === name)));
  document.getElementById('side-pane-files').style.display = name === 'files' ? '' : 'none';
  document.getElementById('side-pane-data').style.display = name === 'data' ? '' : 'none';
  document.getElementById('sidebar').classList.toggle('data-active', name === 'data');
  if (name === 'data' && !dataLoaded) {
    dataLoaded = true;
    loadDataFiles();
  }
}

function dataExt(path) {
  const dot = path.lastIndexOf('.');
  return dot < 0 ? '' : path.slice(dot + 1).toLowerCase();
}

function isPreviewable(path) {
  return DATA_PREVIEWABLE_EXTS.includes(dataExt(path));
}

function fmtBytes(n) {
  if (n < 1024) return n + ' B';
  if (n < 1024 * 1024) return (n / 1024).toFixed(1) + ' KB';
  return (n / (1024 * 1024)).toFixed(1) + ' MB';
}

/* Breadth-first walk of the work dir, bounded by DATA_WALK_BUDGET listings.
The sample directory is queued ahead of the root so a big workspace cannot
spend the whole budget before reaching the datasets that ship with it. */
async function walkForData() {
  const found = [];
  const queue = [
    { path: DATA_SAMPLE_DIR, depth: 1 },
    { path: '.', depth: 0 },
  ];
  const seen = new Set(queue.map((q) => q.path));
  let spent = 0;
  while (queue.length && spent < DATA_WALK_BUDGET) {
    const { path, depth } = queue.shift();
    spent += 1;
    let entries;
    try {
      entries = await api('GET', 'files?path=' + encodeURIComponent(path));
    } catch (e) {
      continue;
    }
    for (const e of entries) {
      if (e.feName.startsWith('.')) continue;
      if (e.feIsDir) {
        if (depth + 1 > DATA_WALK_DEPTH) continue;
        if (DATA_SKIP_DIRS.includes(e.feName) || seen.has(e.fePath)) continue;
        seen.add(e.fePath);
        queue.push({ path: e.fePath, depth: depth + 1 });
      } else if (DATA_EXTS.includes(dataExt(e.feName))) {
        found.push(e);
      }
    }
  }
  return dedupeByPath(found).sort((a, b) => a.fePath.localeCompare(b.fePath));
}

function dedupeByPath(entries) {
  const byPath = new Map();
  for (const e of entries) byPath.set(e.fePath, e);
  return [...byPath.values()];
}

async function loadDataFiles() {
  const list = document.getElementById('data-list');
  if (!list) return;
  list.innerHTML = '<div class="data-empty">Looking for datasets…</div>';
  dataFiles = await walkForData();
  renderDataList();
}

function renderDataList() {
  const list = document.getElementById('data-list');
  if (!list) return;
  list.innerHTML = '';
  if (!dataFiles.length) {
    list.innerHTML =
      '<div class="data-empty">No datasets in this workspace. ' +
      'Upload a .csv from the Files tab to get started.</div>';
    return;
  }
  const samples = dataFiles.filter((f) => f.fePath.startsWith(DATA_SAMPLE_DIR));
  const rest = dataFiles.filter((f) => !f.fePath.startsWith(DATA_SAMPLE_DIR));
  if (rest.length) list.appendChild(dataGroup('Workspace', rest));
  if (samples.length) list.appendChild(dataGroup('Sample data', samples));
}

function dataGroup(title, files) {
  const wrap = document.createElement('div');
  wrap.className = 'data-group';
  const h = document.createElement('div');
  h.className = 'data-group-title';
  h.textContent = title;
  wrap.appendChild(h);
  for (const f of files) wrap.appendChild(dataRow(f));
  return wrap;
}

function dataRow(f) {
  const row = document.createElement('div');
  row.className = 'data-row' + (dataSelected === f.fePath ? ' active' : '');
  if (!isPreviewable(f.fePath)) row.classList.add('opaque');
  row.dataset.path = f.fePath;
  row.title = f.fePath;
  const name = document.createElement('span');
  name.className = 'data-name';
  name.textContent = f.feName;
  const dir = document.createElement('span');
  dir.className = 'data-dir';
  dir.textContent = f.fePath.slice(0, Math.max(0, f.fePath.length - f.feName.length - 1));
  row.appendChild(name);
  row.appendChild(dir);
  row.onclick = () => showDataset(f.fePath);
  return row;
}

async function showDataset(path) {
  dataSelected = path;
  renderDataList();
  const detail = document.getElementById('data-detail');
  if (!detail) return;
  detail.innerHTML = '<div class="data-empty">Reading ' + escapeHtml(path) + '…</div>';
  let d;
  try {
    d = await api('GET', 'dataset/preview?path=' + encodeURIComponent(path) + '&rows=25');
  } catch (e) {
    detail.innerHTML = '<div class="data-empty">Could not read that file.</div>';
    return;
  }
  renderDataset(detail, d);
}
