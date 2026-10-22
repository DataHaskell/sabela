// ── Files dropped from the desktop ───────────────────────────────
// A CSV or Parquet file dragged from the computer onto the notebook is
// uploaded into the workspace's data folder, loaded as a typed frame in a
// cell placed where it was dropped, and shown as a table; images, video and
// audio become media cells. While a file is over the page the notebook says
// what the drop will do.

const DROP_FOLDER = 'data';

function installOsFileDrop() {
  const area = document.getElementById('notebook-area');
  if (!area || area.dataset.osDropReady) return;
  area.dataset.osDropReady = '1';
  const overlay = document.createElement('div');
  overlay.id = 'drop-overlay';
  overlay.innerHTML =
    '<div class="drop-overlay-card"><div class="drop-overlay-title">Drop to load</div>' +
    '<div class="drop-overlay-hint">CSV and Parquet files become typed frames, ready to chart. ' +
    'Images, video and audio become media cells.</div></div>';
  area.appendChild(overlay);
  // Capture phase: a code editor stops drag events from bubbling and would
  // otherwise paste the file's text into itself, so the page sees them first.
  let depth = 0;
  let busy = false;
  // A drag that started on this page (a column chip, a file row) is never a
  // desktop file, whatever extra types the browser attaches. Drop also clears
  // the flag: a re-render can detach the source before its dragend fires.
  let inPageDrag = false;
  document.addEventListener('dragstart', () => (inPageDrag = true), true);
  document.addEventListener('dragend', () => (inPageDrag = false), true);
  // Anything dragged in from outside the page: the Files type when the
  // browser gives it, and on some platforms (ChromeOS among them) only
  // opaque file-system types until the drop itself.
  const hasFiles = (e) => {
    if (inPageDrag) return false;
    const types = Array.from((e.dataTransfer && e.dataTransfer.types) || []);
    if (!types.length) return false;
    if (types.includes('Files')) return true;
    const internal = [FILE_DT, COL_DT, 'text/plain', 'text/html'];
    return types.some((t) => !internal.includes(t));
  };
  const droppedFiles = (dt) => {
    const files = Array.from((dt && dt.files) || []);
    if (files.length) return files;
    return Array.from((dt && dt.items) || [])
      .filter((i) => i.kind === 'file')
      .map((i) => i.getAsFile())
      .filter(Boolean);
  };
  const on = (type, fn) => document.addEventListener(type, fn, true);
  on('dragenter', (e) => {
    if (!hasFiles(e)) return;
    depth++;
    overlay.classList.add('show');
  });
  on('dragleave', (e) => {
    if (!hasFiles(e)) return;
    depth = Math.max(0, depth - 1);
    if (!depth) overlay.classList.remove('show');
  });
  on('dragover', (e) => {
    if (!hasFiles(e)) return;
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy';
    markNearestAddRow(e.clientY);
  });
  on('drop', async (e) => {
    depth = 0;
    overlay.classList.remove('show');
    const external = hasFiles(e);
    inPageDrag = false;
    if (!external) return;
    e.preventDefault();
    e.stopPropagation();
    const files = droppedFiles(e.dataTransfer);
    const named = namedPaths(e.dataTransfer);
    const row = nearestAddRow(e.clientY);
    clearDropMarks();
    if (!files.length && !named.length) {
      setStatus('nothing to import in that drop (drag a file from your computer)', 'error');
      return;
    }
    if (busy) return;
    busy = true;
    try {
      await dropFilesIntoNotebook(files, row ? Number(row.dataset.after) : lastCellId(), named);
    } finally {
      busy = false;
    }
  });
}

// The add row just above a point on the page: where a dropped file's cells go.
function nearestAddRow(y) {
  const rows = Array.from(document.querySelectorAll('#notebook .add-cell-row'));
  if (!rows.length) return null;
  let best = null;
  for (const r of rows) {
    const rect = r.getBoundingClientRect();
    if (rect.top <= y + rect.height / 2) best = r;
  }
  return best || rows[0];
}

function markNearestAddRow(y) {
  const row = nearestAddRow(y);
  clearDropMarks(row);
  if (row) row.classList.add('drop-ok');
}

function clearDropMarks(except) {
  document
    .querySelectorAll('#notebook .add-cell-row.drop-ok')
    .forEach((r) => r !== except && r.classList.remove('drop-ok'));
}

// The paths a drop names, read while the event is live: ChromeOS lists the
// source files under fs/sources, other desktops as file: URLs.
function namedPaths(dt) {
  const out = [];
  const read = (type) => {
    try {
      return dt.getData(type) || '';
    } catch (_e) {
      return '';
    }
  };
  for (const line of read('fs/sources').split('\n')) if (line.trim()) out.push(line.trim());
  for (const line of read('text/uri-list').split('\n')) {
    const l = line.trim();
    if (/^file:\/\//i.test(l)) out.push(decodeURIComponent(l.replace(/^file:\/\//i, '')));
  }
  return [...new Set(out)];
}

// Read every dropped file at once (a dragged file's handle can be revoked
// soon after the drop; the bytes cannot), upload, then insert each file's
// cells in order, each after the last.
async function dropFilesIntoNotebook(files, afterId, named) {
  const reads = files.map((f) =>
    f
      .arrayBuffer()
      .then((buf) => ({ name: f.name, blob: new Blob([buf]) }))
      .catch((e) => ({ name: f.name, error: e && e.message ? e.message : String(e) }))
  );
  const payloads = await Promise.all(reads);
  const unreadable = payloads.filter((p) => p.error);
  if (unreadable.length)
    setStatus(
      'could not read ' + unreadable.map((p) => p.name + ' (' + p.error + ')').join(', '),
      'error'
    );
  let written = await uploadFiles(
    DROP_FOLDER,
    payloads.filter((p) => !p.error)
  );
  if ((!written || !written.length) && named && named.length)
    written = await importNamedPaths(named);
  if (!written || !written.length) return;
  let after = afterId;
  const loaded = [];
  for (const path of written) {
    const kind = mediaKindFor(path);
    if (!kind) continue;
    const last = await handleFileDrop(after, path);
    if (last != null) after = last;
    if (kind === 'data') loaded.push(suggestBindingName(path));
  }
  if (typeof loadDataFiles === 'function') loadDataFiles().catch(() => {});
  if (loaded.length)
    setStatus(
      (loaded.length === 1 ? loaded[0] + ' is' : loaded.join(', ') + ' are') +
        ' ready as a typed frame' +
        (loaded.length === 1 ? '' : 's'),
      'ok'
    );
}

// When the browser could name the files but not hand over their bytes, the
// server, which can read the same disk, brings them in by path.
async function importNamedPaths(paths) {
  const written = [];
  for (const p of paths) {
    try {
      const r = await api('POST', 'import-path', { ipPath: p });
      if (r.iprPath) written.push(r.iprPath);
      else showErrorBanner('Could not import ' + p + ': ' + (r.iprError || 'unknown reason'));
    } catch (e) {
      showErrorBanner('Could not import ' + p + ': ' + e.message);
    }
  }
  return written;
}
