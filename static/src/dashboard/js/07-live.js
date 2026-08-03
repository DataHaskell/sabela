// ── SSE (live mode only) ─────────────────────────────────────────
function connectSSE() {
  const evtSource = new EventSource('/api/events');
  evtSource.onmessage = (e) => {
    const ev = JSON.parse(e.data);
    switch (ev.type) {
      case 'cellUpdating':
        showStatus('Running...', true);
        break;
      case 'cellResult':
        updateDashboardCell(ev.cellId, ev.outputs || [], ev.error);
        break;
      case 'executionDone':
        hideStatus();
        break;
      case 'sessionStatus':
        if (ev.state === 'ready') hideStatus();
        else showStatus(ev.message, true);
        break;
      case 'kernelError':
        showStatus(ev.message, false);
        break;
    }
  };
  evtSource.onerror = () => {
    showStatus('Disconnected', false);
  };
}

// ── Update a cell in the dashboard ───────────────────────────────
function updateDashboardCell(cellId, outputs, error) {
  if (!notebookData) return;
  const cell = (notebookData.nbCells || []).find((c) => c.cellId === cellId);
  if (!cell) return;

  cell.cellOutputs = outputs;
  cell.cellError = error || null;

  // Remove only this cell's existing DOM nodes
  const main = document.getElementById('main-content');
  main.querySelectorAll(`[data-cell-id="${cellId}"]`).forEach((el) => el.remove());

  // Find the next cell's first DOM node to insert before
  const cells = notebookData.nbCells;
  const cellIndex = cells.indexOf(cell);
  let insertBefore = null;
  for (let j = cellIndex + 1; j < cells.length; j++) {
    const nextNode = main.querySelector(`[data-cell-id="${cells[j].cellId}"]`);
    if (nextNode) {
      insertBefore = nextNode;
      break;
    }
  }

  // Re-render just this cell
  renderCellOutputs(main, cell, insertBefore);
}

// ── Widget postMessage bridge (live mode) ────────────────────────
const _widgetTimers = new Map();
window.addEventListener('message', (e) => {
  if (isStatic) return;
  const d = e.data;
  if (d && d.type === 'widget') {
    const key = d.cellId + ':' + d.name;
    clearTimeout(_widgetTimers.get(key));
    _widgetTimers.set(
      key,
      setTimeout(() => {
        _widgetTimers.delete(key);
        fetch('/api/widget', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ wuCellId: d.cellId, wuName: d.name, wuValue: d.value }),
        });
      }, 300)
    );
  }
});

// ── Status indicator ─────────────────────────────────────────────
function showStatus(msg, running) {
  const el = document.getElementById('status-indicator');
  el.textContent = msg;
  el.className = 'dash-status visible' + (running ? ' running' : '');
}
function hideStatus() {
  document.getElementById('status-indicator').className = 'dash-status';
}

// ── Export button (live mode) ────────────────────────────────────
function exportDashboard() {
  window.open('/api/export/dashboard', '_blank');
}

// ── Title formatting ─────────────────────────────────────────────
function formatTitle(path) {
  if (!path) return 'Dashboard';
  const name = path
    .split('/')
    .pop()
    .replace(/\.[^/.]+$/, '');
  return name.charAt(0).toUpperCase() + name.slice(1);
}

// ── Go ───────────────────────────────────────────────────────────
init();
