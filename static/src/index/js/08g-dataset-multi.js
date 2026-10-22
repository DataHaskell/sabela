// ── Data panel: loading several datasets at once ──
// Each file is read into its own binding named after it, so a second load
// sits beside the first instead of replacing it.
const dataChecked = new Set();

// The file path a load cell spells, escaped the way insertLoadCell writes it.
function loadCellPathLiteral(path) {
  return '"./' + path.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
}

// The binding an existing load cell made from this file, or null.
function loadedBindingFor(path) {
  const lit = loadCellPathLiteral(path);
  for (const c of (notebook && notebook.nbCells) || []) {
    const src = c.cellSource || '';
    if (!src.includes(lit) || !/\bread(Csv|Parquet)\b/.test(src)) continue;
    const m = src.match(/^([a-z_][\w']*)\s*<-\s*\w+\.read(?:Csv|Parquet)\b/m);
    if (m) return { name: m[1], cellId: c.cellId };
  }
  return null;
}

function dataCheckbox(f) {
  const box = document.createElement('input');
  box.type = 'checkbox';
  box.className = 'data-check';
  box.checked = dataChecked.has(f.fePath);
  box.title = 'Select to load several datasets at once';
  box.setAttribute('aria-label', 'Select ' + f.feName);
  box.onclick = (e) => e.stopPropagation();
  box.onchange = () => {
    if (box.checked) dataChecked.add(f.fePath);
    else dataChecked.delete(f.fePath);
    renderDataLoadBar();
  };
  return box;
}

function dataLoadedBadge(path) {
  const loaded = loadedBindingFor(path);
  if (!loaded) return null;
  const badge = document.createElement('span');
  badge.className = 'data-bound';
  badge.textContent = loaded.name;
  badge.title = 'Loaded as ' + loaded.name;
  return badge;
}

function renderDataLoadBar() {
  const bar = document.getElementById('data-load-bar');
  if (!bar) return;
  bar.innerHTML = '';
  bar.style.display = dataChecked.size ? '' : 'none';
  if (!dataChecked.size) return;
  const n = dataChecked.size;
  const load = document.createElement('button');
  load.className = 'data-load-btn';
  load.textContent = '＋ Load ' + n + (n === 1 ? ' dataset' : ' datasets');
  load.onclick = () => loadCheckedDatasets(load);
  const clear = document.createElement('button');
  clear.className = 'data-clear-btn';
  clear.textContent = 'Clear';
  clear.onclick = () => {
    dataChecked.clear();
    renderDataList();
    renderDataLoadBar();
  };
  bar.append(load, clear);
}

/* Insert one load cell per checked file, each under its own name, then run
them in one pass: separate runs would each bump the generation and cancel the
one before. Files already loaded are skipped rather than read twice. */
async function loadCheckedDatasets(btn) {
  const paths = [...dataChecked].filter((p) => !loadedBindingFor(p));
  if (btn) btn.disabled = true;
  const names = [];
  try {
    for (const path of paths) {
      const loaded = await insertLoadCell(path, { name: suggestBindingName(path) });
      names.push(loaded.name);
    }
  } finally {
    dataChecked.clear();
    renderDataList();
    renderDataLoadBar();
  }
  if (!names.length) {
    setStatus('those datasets are already loaded', 'ok');
    return;
  }
  await api('POST', 'run-all').catch(() => {});
  setStatus('loading ' + names.join(', '), 'ok');
}
