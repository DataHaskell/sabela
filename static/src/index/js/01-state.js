const editors = {}; // cellId → CodeMirror instance
const cellErrors = {}; // cellId → [CellError]
const cellWarnings = {}; // cellId → [CellError] (non-fatal diagnostics)
const collapsedCells = new Set(); // cellId → collapsed
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

// Set the toolbar title: the warm-lambda brand mark followed by the notebook
// title (appended as a text node, so titles are never interpreted as HTML).
function setToolbarTitle(title) {
  const el = document.getElementById('toolbar-title');
  el.innerHTML = '<span class="lam">λ</span> ';
  el.append(title);
}
