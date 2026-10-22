const editors = {}; // cellId → CodeMirror instance
const cellErrors = {}; // cellId → [CellError]
const cellWarnings = {}; // cellId → [CellError] (non-fatal diagnostics)
const collapsedCells = new Set(); // cellId → collapsed
const collapsedOutputs = new Set(); // cellId → output collapsed
const codeShown = new Set(); // chart cellIds whose Haskell is unfolded
let chatFocus = null; // {cellId, kind} the next chat message is about
const vizSpecs = new Map(); // cellId → {spec, cols, types} of a mounted chart
const vizLastSource = new Map(); // cellId → source the Design panel last wrote
let designer = {
  cellId: null,
  afterId: null,
  spec: null,
  source: null,
  armed: null,
  timer: null,
  blocked: false,
  ownership: null,
  message: '',
  sources: [],
  columns: [],
};
let notebook = null;
let evtSource = null;
let activeTab = 'info';
let didIntroReveal = false; // stagger the cell reveal once, on first paint only
let examples = [];
let unsavedChanges = false;
// Per-cell "locally edited since last render/save" flag. Only cells with
// their id set here have their local CM value preserved across re-renders;
// clean cells let the server-supplied content win, so AI-driven edits to
// cells the user didn't touch show up immediately.
const dirtyCells = new Set();
const AUTOSAVE_INTERVAL_MS = 10000;

// Set the open tab's title and mirror it into the status bar (text nodes
// only, so titles are never interpreted as HTML).
function setToolbarTitle(title) {
  const el = document.getElementById('toolbar-title');
  el.textContent = title;
  const sb = document.getElementById('sb-file-name');
  if (sb) sb.textContent = title;
}

// Status bar readouts: the focused editor's caret and the notebook's size.
function updateCursorStatus(cm) {
  const el = document.getElementById('sb-pos');
  if (!el || !cm) return;
  const c = cm.getCursor();
  el.textContent = `line ${c.line + 1} : col ${c.ch + 1}`;
  const cellEl = cm.getWrapperElement().closest('.cell');
  const total = notebook ? notebook.nbCells.length : 0;
  if (cellEl && cellEl.dataset.num && total) setCellCountStatus(total, Number(cellEl.dataset.num));
}
function setCellCountStatus(n, current) {
  const el = document.getElementById('sb-cells');
  if (!el) return;
  if (current) el.textContent = `Cell ${current} of ${n}`;
  else el.textContent = n === 1 ? '1 cell' : `${n} cells`;
}
