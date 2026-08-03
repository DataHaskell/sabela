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
        updateSlideCell(ev.cellId, ev.outputs || [], ev.error);
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

function updateSlideCell(cellId, outputs, error) {
  if (!notebookData) return;
  const cell = (notebookData.nbCells || []).find((c) => c.cellId === cellId);
  if (!cell) return;
  cell.cellOutputs = outputs;
  cell.cellError = error || null;
  const idx = notebookData.nbCells.indexOf(cell);
  const slide = slideEls[idx];
  if (!slide) return;
  const wasActive = slide.classList.contains('active');
  // Refresh only the output pane so an open editor (and its caret) survive.
  const outCol = slide.querySelector('.slide-output');
  if (outCol) {
    fillOutputs(outCol, cell);
  } else {
    renderSlide(slide, cell);
  }
  if (wasActive) {
    slide.classList.add('active');
    requestAnimationFrame(rerenderVisibleIframes);
  }
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
  el.className = 'slide-status visible' + (running ? ' running' : '');
}
function hideStatus() {
  document.getElementById('status-indicator').className = 'slide-status';
}

// ── Export ───────────────────────────────────────────────────────
function exportSlideshow() {
  window.open('/api/export/slideshow', '_blank');
}

// ── Title formatting ─────────────────────────────────────────────
function formatTitle(path) {
  if (!path) return 'Slideshow';
  const name = path
    .split('/')
    .pop()
    .replace(/\.[^/.]+$/, '');
  return name.charAt(0).toUpperCase() + name.slice(1);
}

init();
