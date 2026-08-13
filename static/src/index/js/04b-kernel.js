// ── Kernel state: what the kernel is doing, and what is out of date ──
// The transport dot means "the event stream is open" and stays green while the
// kernel is dead. Everything here is about the kernel itself.

let kernelEpoch = null;
let staleCellIds = new Set();
let runMode = 'reactive';
const kernelLog = [];

const KERNEL_CHIP_LABELS = {
  cold: 'no kernel',
  idle: 'idle',
  executing: 'running',
  building: 'building',
};

function renderKernelChip(state) {
  const chip = document.getElementById('kernel-chip');
  if (!chip) return;
  const tag = (state && state.state) || 'cold';
  chip.textContent = KERNEL_CHIP_LABELS[tag] || tag;
  chip.className = 'kernel-chip ' + tag;
  chip.title = 'Kernel: ' + (KERNEL_CHIP_LABELS[tag] || tag);
}

// Level-triggered resync: EventSource reconnects silently after a sleep, so the
// client needs one place to ask what is true now.
async function refreshKernelState() {
  try {
    const status = await api('GET', 'kernel');
    renderKernelChip(status.state);
    if (status.runMode) applyRunMode(status.runMode);
  } catch (_e) {
    /* leave the chip showing its last known value */
  }
}

function applyRunMode(mode) {
  runMode = mode;
  const btn = document.getElementById('btn-run-mode');
  if (btn) {
    btn.classList.toggle('deferred', mode === 'deferred');
    btn.title =
      mode === 'deferred'
        ? 'Deferred: edits only mark cells stale. Click to resume reactive runs.'
        : 'Reactive: edits run automatically. Click to defer runs.';
  }
  paintRunAllLabel();
}

// In deferred mode the Run All button doubles as the drain, so its label
// carries the size of the pending set.
function paintRunAllLabel() {
  const label = document.getElementById('run-all-label');
  if (!label) return;
  const n = staleCellIds.size;
  label.textContent = runMode === 'deferred' && n > 0 ? 'Run ' + n + ' stale' : 'Run all';
}

function applyNotebookState(epoch, staleIds) {
  if (kernelEpoch !== null && epoch !== kernelEpoch) showEpochBanner();
  kernelEpoch = epoch;
  staleCellIds = new Set(staleIds || []);
  paintStaleCells();
}

// Staleness is server-computed: it depends on the dependency graph, which the
// browser does not have.
function paintStaleCells() {
  document.querySelectorAll('.cell.code[data-id]').forEach((el) => {
    const id = parseInt(el.dataset.id, 10);
    el.classList.toggle('stale', staleCellIds.has(id));
  });
  paintRunAllLabel();
}

function showEpochBanner() {
  let banner = document.getElementById('epoch-banner');
  if (!banner) {
    banner = document.createElement('div');
    banner.id = 'epoch-banner';
    banner.className = 'notebook-banner';
    banner.innerHTML =
      '<span>The kernel restarted — nothing in this notebook has run against it.</span>' +
      '<button onclick="restartAndRunAll()">Run all</button>' +
      '<button onclick="hideEpochBanner()">Dismiss</button>';
    document.body.prepend(banner);
  }
  banner.style.display = 'flex';
}

function hideEpochBanner() {
  const banner = document.getElementById('epoch-banner');
  if (banner) banner.style.display = 'none';
}

// ── Kernel event log ──────────────────────────────────────────────
// Every kernel event is already on the bus; keeping them is what makes this
// class of bug diagnosable after the fact.
function recordKernelEvent(kind, text) {
  kernelLog.push({ at: new Date().toLocaleTimeString(), kind, text });
  if (kernelLog.length > 500) kernelLog.shift();
  const body = document.getElementById('kernel-log-body');
  if (body && document.getElementById('kernel-log').style.display !== 'none') renderKernelLog();
}

function renderKernelLog() {
  const body = document.getElementById('kernel-log-body');
  if (!body) return;
  body.innerHTML = '';
  kernelLog.forEach((e) => {
    const row = document.createElement('div');
    row.className = 'kernel-log-row ' + e.kind;
    row.textContent = `${e.at}  ${e.kind}  ${e.text}`;
    body.appendChild(row);
  });
  body.scrollTop = body.scrollHeight;
}

function toggleKernelLog() {
  const panel = document.getElementById('kernel-log');
  if (!panel) return;
  const open = panel.style.display !== 'none';
  panel.style.display = open ? 'none' : 'flex';
  if (!open) renderKernelLog();
}
