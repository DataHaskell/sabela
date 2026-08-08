// ── The build log modal ──────────────────────────────────────────
// Driven by the sessionStatus / installLog / kernelError events in 04-sse.js.

// Long enough to read the verdict, short enough not to sit in the way.
const BUILD_MODAL_LINGER_MS = 1200;
let buildModalTimer = null;
// Whether a build is running. Log lines pop the modal open only while one is,
// so a line trailing in after the verdict cannot bring it back.
let buildRunning = false;

function openBuildModal(title) {
  const modal = document.getElementById('build-modal');
  const titleEl = document.getElementById('build-modal-title');
  clearTimeout(buildModalTimer);
  buildRunning = true;
  titleEl.textContent = title;
  titleEl.className = 'build-modal-title';
  document.getElementById('build-log').innerHTML = '';
  modal.style.display = 'flex';
}

function closeBuildModal() {
  clearTimeout(buildModalTimer);
  document.getElementById('build-modal').style.display = 'none';
}

/* The build's verdict. A build that succeeded has nothing left to read, so its
log dismisses itself; a failed one is shown and stays up, because it is the
diagnostic — and the failure may be the first thing worth opening it for. */
function finishBuildModal(text, cls) {
  const modal = document.getElementById('build-modal');
  const t = document.getElementById('build-modal-title');
  clearTimeout(buildModalTimer);
  buildRunning = false;
  t.textContent = text;
  t.className = 'build-modal-title ' + cls;
  if (cls === 'ok') {
    buildModalTimer = setTimeout(closeBuildModal, BUILD_MODAL_LINGER_MS);
  } else {
    modal.style.display = 'flex';
  }
}

function appendBuildLog(line) {
  const log = document.getElementById('build-log');
  const modal = document.getElementById('build-modal');
  if (buildRunning && modal.style.display === 'none') modal.style.display = 'flex';
  const el = document.createElement('div');
  if (/error:/i.test(line)) el.className = 'build-log-error';
  el.textContent = line;
  log.appendChild(el);
  log.scrollTop = log.scrollHeight;
}

// A build that ran out of time is worth retrying at a longer budget; the others are not.
function kernelErrorTitle(phase) {
  switch (phase) {
    case 'buildTimeout':
      return 'Build timed out';
    case 'buildFailed':
      return 'Build failed';
    case 'preludeFailed':
      return 'Kernel started but the prelude failed';
    case 'crashed':
      return 'Kernel crashed';
    default:
      return 'Kernel error';
  }
}

// Mark the cells whose dependencies the failure names, so the user knows where to edit.
function markBlamedCells(cellIds) {
  document.querySelectorAll('.cell.blamed').forEach((el) => el.classList.remove('blamed'));
  cellIds.forEach((cid) => {
    const el = document.querySelector(`.cell[data-id="${cid}"]`);
    if (el) el.classList.add('blamed');
  });
}
