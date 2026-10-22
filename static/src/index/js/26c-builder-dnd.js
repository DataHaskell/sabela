// ── Drag and drop: columns onto shelves, files into the notebook ──
// Native HTML5 drag and drop, with a click-to-arm fallback so the shelves
// also work from the keyboard, on touch, and in tests.

const COL_DT = 'application/x-sabela-column';
const FILE_DT = 'application/x-sabela-file';

function makeColumnChip(col, from) {
  const chip = document.createElement('button');
  chip.className = 'col-chip' + (designer.armed === col.name ? ' armed' : '');
  chip.dataset.name = col.name;
  chip.dataset.type = col.type;
  chip.draggable = true;
  chip.innerHTML = `<span class="col-glyph">${escapeHtml(DESIGN_TYPE_GLYPH[col.type] || '')}</span>${escapeHtml(col.name)}`;
  chip.title = col.raw ? col.name + ' :: ' + col.raw : col.name;
  chip.ondragstart = (e) => {
    e.dataTransfer.setData(COL_DT, JSON.stringify({ name: col.name, from }));
    e.dataTransfer.effectAllowed = 'copyMove';
    chip.classList.add('dragging');
  };
  chip.ondragend = () => chip.classList.remove('dragging');
  chip.onclick = () => armChip(col.name);
  return chip;
}

// Click a column to arm it, then click a shelf to place it; Escape disarms.
function armChip(name) {
  designer.armed = designer.armed === name ? null : name;
  renderDesigner();
}

function placeArmed(key) {
  if (!designer.armed) return false;
  assignShelf(key, designer.armed, null);
  return true;
}

function makeShelfDropTarget(zone, key) {
  zone.onclick = () => placeArmed(key);
  zone.ondragenter = (e) => {
    if (hasType(e, COL_DT)) {
      e.preventDefault();
      zone.classList.add('drop-ok');
    }
  };
  zone.ondragover = (e) => {
    if (hasType(e, COL_DT)) {
      e.preventDefault();
      e.dataTransfer.dropEffect = 'move';
    }
  };
  zone.ondragleave = () => zone.classList.remove('drop-ok');
  zone.ondrop = (e) => {
    zone.classList.remove('drop-ok');
    const raw = e.dataTransfer.getData(COL_DT);
    if (!raw) return;
    e.preventDefault();
    let d;
    try {
      d = JSON.parse(raw);
    } catch (_e) {
      return;
    }
    assignShelf(key, d.name, d.from ? { key: d.from, index: d.index || 0 } : null);
  };
}

function hasType(e, type) {
  const types = e.dataTransfer && e.dataTransfer.types;
  return !!types && Array.from(types).includes(type);
}

// A file row in the Data or Files pane can be dragged into the notebook.
function makeFileDraggable(el, path) {
  el.draggable = true;
  el.addEventListener('dragstart', (e) => {
    e.dataTransfer.setData(FILE_DT, JSON.stringify({ path }));
    e.dataTransfer.effectAllowed = 'copy';
  });
}

function mediaKindFor(path) {
  const ext = (path.split('.').pop() || '').toLowerCase();
  if (['csv', 'tsv', 'parquet'].includes(ext)) return 'data';
  if (['png', 'jpg', 'jpeg', 'gif', 'svg', 'webp', 'avif'].includes(ext)) return 'image';
  if (['mp4', 'webm', 'mov'].includes(ext)) return 'video';
  if (['mp3', 'wav', 'ogg', 'm4a', 'flac'].includes(ext)) return 'audio';
  return null;
}

function fileDropLabel(path) {
  if (!path) return 'Use component';
  const kind = mediaKindFor(path);
  return (
    { data: 'Load as table', image: 'Insert image', video: 'Insert video', audio: 'Insert audio' }[
      kind
    ] || null
  );
}

// Add rows under a dragged file light up and take it: a data file becomes a
// load cell plus a table, a media file a one-line media cell.
function installNotebookDropTargets() {
  const nb = document.getElementById('notebook');
  if (!nb || nb.dataset.dropReady) return;
  nb.dataset.dropReady = '1';
  nb.addEventListener('dragover', (e) => {
    if (!hasType(e, FILE_DT)) return;
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy';
    markNearestAddRow(e.clientY);
  });
  nb.addEventListener('dragleave', (e) => {
    if (!nb.contains(e.relatedTarget)) clearDropMarks();
  });
  nb.addEventListener('drop', async (e) => {
    const raw = e.dataTransfer.getData(FILE_DT);
    if (!raw) return;
    e.preventDefault();
    const row = nearestAddRow(e.clientY);
    clearDropMarks();
    if (!row) return;
    let d;
    try {
      d = JSON.parse(raw);
    } catch (_e) {
      return;
    }
    if (d.component) {
      const c = componentsList.find((x) => x.ciModule === d.component);
      if (c) await insertComponentUse(c, c.ciExports[0], Number(row.dataset.after));
      return;
    }
    await handleFileDrop(Number(row.dataset.after), d.path);
  });
}

// Returns the id of the last cell it made, so a run of drops can chain. Only
// the load cell is run: the table cell reads its binding, so the reactive
// engine runs it next, and one run means one kernel build.
async function handleFileDrop(afterId, path) {
  const kind = mediaKindFor(path);
  if (!kind) {
    setStatus('nothing to insert for ' + path, 'error');
    return;
  }
  if (kind === 'data') {
    const name = suggestBindingName(path);
    const loaded = await insertLoadCell(path, { after: afterId, name });
    const shown = await addCell(loaded.cellId, 'CodeCell', 'Haskell', loaded.name);
    await api('POST', 'run/' + loaded.cellId).catch(() => {});
    return shown;
  }
  const id = await addCell(
    afterId,
    'CodeCell',
    'Haskell',
    kind + ' "./' + path.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"'
  );
  api('POST', 'run/' + id).catch(() => {});
  return id;
}

// ── Bound-cell bookkeeping, continued from 26-builder.js ──
// The user typed into the bound cell: the builder no longer owns it.
function designerNoteUserEdit(cellId) {
  if (designer.cellId !== cellId || designer.ownership === 'edited') return;
  const cm = editors[cellId];
  if (cm && cm.getValue().trim() === (vizLastSource.get(cellId) || '').trim()) return;
  designer.ownership = 'edited';
  designer.blocked = true;
  designer.message = 'This cell was edited by hand since the builder wrote it.';
  renderDesigner();
}

function jumpToCode(cellId) {
  toggleCellCode(cellId, true);
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (el) el.scrollIntoView({ block: 'center', behavior: 'smooth' });
  if (editors[cellId]) editors[cellId].focus();
}
