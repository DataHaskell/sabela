// ── SSE ──────────────────────────────────────────────────────────
function connectSSE() {
  if (evtSource) evtSource.close();
  evtSource = new EventSource('/api/events');
  evtSource.onopen = () => document.getElementById('sse-dot').classList.add('connected');
  evtSource.onerror = () => document.getElementById('sse-dot').classList.remove('connected');
  evtSource.onmessage = (e) => {
    try {
      handleSSE(JSON.parse(e.data));
    } catch (err) {
      console.error('SSE parse error:', err);
    }
  };
}

function openBuildModal(title) {
  const modal = document.getElementById('build-modal');
  const titleEl = document.getElementById('build-modal-title');
  titleEl.textContent = title;
  titleEl.className = 'build-modal-title';
  document.getElementById('build-log').innerHTML = '';
  modal.style.display = 'flex';
}
function closeBuildModal() {
  document.getElementById('build-modal').style.display = 'none';
}
function appendBuildLog(line) {
  const log = document.getElementById('build-log');
  const modal = document.getElementById('build-modal');
  if (modal.style.display === 'none') modal.style.display = 'flex';
  const el = document.createElement('div');
  if (/error:/i.test(line)) el.className = 'build-log-error';
  el.textContent = line;
  log.appendChild(el);
  log.scrollTop = log.scrollHeight;
}

function handleSSE(ev) {
  switch (ev.type) {
    case 'cellUpdating':
      setCellRunning(ev.cellId, true);
      clearPartialOutput(ev.cellId);
      break;
    case 'cellCompiling': {
      setCellRunning(ev.cellId, true);
      clearPartialOutput(ev.cellId);
      const cEl = document.querySelector(`.cell[data-id="${ev.cellId}"]`);
      if (cEl) cEl.classList.add('compiling');
      setStatus(`Compiling cell ${ev.cellId}...`, 'running');
      break;
    }
    case 'cellPartialOutput':
      appendPartialOutput(ev.cellId, ev.line);
      break;
    case 'cellResult':
      setCellRunning(ev.cellId, false);
      updateCellOutput(ev.cellId, ev.outputs || [], ev.error);
      if (ev.errors && ev.errors.length) applyErrorMarkers(ev.cellId, ev.errors);
      else clearErrorMarkers(ev.cellId);
      pulseCell(ev.cellId);
      break;
    case 'widget':
      // Kernel→browser widget sync: forward the new value into the cell's
      // output iframe(s) so the control reflects it without a full re-render.
      document
        .querySelectorAll(`.cell[data-id="${ev.cellId}"] .cell-output iframe`)
        .forEach((f) => {
          try {
            f.contentWindow.postMessage(
              { type: 'widgetUpdate', name: ev.name, value: ev.value },
              '*'
            );
          } catch (_e) {
            /* cross-origin or not ready; ignore */
          }
        });
      break;
    case 'executionDone':
      setStatus('Done', '');
      document
        .querySelectorAll('.cell.code.running')
        .forEach((el) => el.classList.remove('running'));
      updateStopButton();
      break;
    case 'sessionStatus':
      setStatus(ev.message, 'running');
      if (ev.message === 'starting session' || ev.message.startsWith('installing:')) {
        openBuildModal('Building…');
      } else if (ev.message === 'ready') {
        hideCrashBanner();
        const t = document.getElementById('build-modal-title');
        t.textContent = 'Build succeeded';
        t.className = 'build-modal-title ok';
        setTimeout(closeBuildModal, 2000);
        setStatus('Done', '');
      } else if (ev.message === 'crashed') {
        setStatus('Kernel crashed', 'error');
        showCrashBanner();
      } else if (ev.message === 'reset') {
        const t = document.getElementById('build-modal-title');
        t.textContent = 'Build failed';
        t.className = 'build-modal-title err';
      }
      break;
    case 'installLog':
      appendBuildLog(ev.line);
      break;
    // Chat events
    case 'chatTextDelta':
      appendChatTextDelta(ev.text);
      break;
    case 'chatToolCall':
      showChatToolCall(ev.toolCallId, ev.tool, ev.input);
      break;
    case 'chatToolResult':
      showChatToolResult(ev.toolCallId, ev.result);
      break;
    case 'chatEditProposed':
      showChatEditProposal(ev.editId, ev.cellId, ev.oldSource, ev.newSource);
      break;
    case 'chatDone':
      finishChatTurn();
      break;
    case 'chatCancelled':
      finishChatTurn('Cancelled');
      break;
    case 'chatError':
      showChatError(ev.message);
      finishChatTurn();
      break;
    case 'notebookChanged':
      // Fired when cells are inserted, deleted, reordered, or edited outside
      // the reactive execute path (AI mutations, accepted proposals, etc).
      if (ev.notebook) {
        notebook = ev.notebook;
        render(notebook);
        updateChatContext();
      }
      break;
    case 'chatUsageUpdate':
      recordTurnUsage(ev.turnId, ev.usage);
      break;
  }
}

let sessionUsage = { inputTokens: 0, outputTokens: 0, cacheReadInputTokens: 0, turns: 0 };
let lastTurnUsage = null;

function recordTurnUsage(turnId, usage) {
  if (!usage) return;
  lastTurnUsage = usage;
  sessionUsage.inputTokens += usage.inputTokens || 0;
  sessionUsage.outputTokens += usage.outputTokens || 0;
  sessionUsage.cacheReadInputTokens += usage.cacheReadInputTokens || 0;
  sessionUsage.turns += 1;
  renderUsageBadge();
}

function formatK(n) {
  if (n == null) return '0';
  if (n < 1000) return String(n);
  return (n / 1000).toFixed(n >= 10000 ? 0 : 1) + 'k';
}

function renderUsageBadge() {
  const el = document.getElementById('chat-usage');
  if (!el) return;
  if (sessionUsage.turns === 0) {
    el.textContent = '';
    el.removeAttribute('title');
    return;
  }
  const s = sessionUsage;
  const lu = lastTurnUsage || {};
  const line1 =
    'in ' +
    formatK(s.inputTokens) +
    (s.cacheReadInputTokens > 0 ? ' (' + formatK(s.cacheReadInputTokens) + ' cache)' : '') +
    ' / out ' +
    formatK(s.outputTokens);
  el.textContent = line1;
  el.title = [
    'Session: ' + s.turns + ' turn(s)',
    '  input ' + s.inputTokens + ' (cache-read ' + s.cacheReadInputTokens + ')',
    '  output ' + s.outputTokens,
    'Last turn:',
    '  iterations ' + (lu.iterations || 0) + ', tool calls ' + (lu.toolCalls || 0),
    '  wall ' + (lu.wallTimeMs || 0) + ' ms',
    '  input ' + (lu.inputTokens || 0) + ', output ' + (lu.outputTokens || 0),
    '  cache read ' +
      (lu.cacheReadInputTokens || 0) +
      ', write ' +
      (lu.cacheCreationInputTokens || 0),
  ].join('\n');
  el.classList.add('recent');
  setTimeout(() => el.classList.remove('recent'), 2500);
}

function resetSessionUsage() {
  sessionUsage = { inputTokens: 0, outputTokens: 0, cacheReadInputTokens: 0, turns: 0 };
  lastTurnUsage = null;
  renderUsageBadge();
}

function setCellRunning(cellId, running) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  if (running) {
    el.classList.add('running');
    setStatus(`Running cell ${cellId}...`, 'running');

    // TODO: mchavinda: Add a spinner here.
  } else {
    el.classList.remove('running');
    el.classList.remove('compiling');
  }
  updateStopButton();
}

// Show the toolbar stop button only while at least one cell is running.
function updateStopButton() {
  const btn = document.getElementById('btn-stop');
  if (!btn) return;
  btn.style.display = document.querySelector('.cell.running') ? '' : 'none';
}

// The "respond" ripple: replay it cleanly even if a cell re-fires mid-animation.
function pulseCell(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  el.classList.remove('responding');
  void el.offsetWidth; // force reflow so the animation restarts
  el.classList.add('responding');
  el.addEventListener('animationend', () => el.classList.remove('responding'), { once: true });
}
