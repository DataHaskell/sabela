// ── MIME-aware output rendering ──────────────────────────────────
// renderMimeOutput + mergeOutputs now live in the shared partial
// static/src/shared/mime-render.js (referenced before this file in the
// index shell) so the WASM-mode share runtime reuses identical rendering.

// The symmetric half of updateCellOutput's class removal. Typing marks a cell
// out of date immediately; without this nothing repaints it until a full
// render(), which only a structural change triggers.
function markCellStale(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (el) el.classList.add('dirty');
  if (notebook) {
    const cell = notebook.nbCells.find((c) => c.cellId === cellId);
    if (cell) cell.cellDirty = true;
  }
}

// Reconciles outputs in place: blocks are reused by (index, mime) and skipped when
// `_rendered` content is unchanged, so html iframes survive re-runs — theme reapply,
// the widget bridge, and _activeTextInput all rely on iframe identity persisting.
function updateCellOutput(cellId, outputs, error) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  el.classList.remove('dirty');
  // Remove streaming output — final result replaces it
  const stream = el.querySelector('.cell-stream');
  if (stream) stream.remove();
  const hasOutput =
    outputs && outputs.length > 0 && outputs.some((o) => o.oiOutput && o.oiOutput.trim());
  el.classList.toggle('has-error', !!error && !hasOutput);

  if (notebook) {
    const cell = notebook.nbCells.find((c) => c.cellId === cellId);
    if (cell) {
      cell.cellOutputs = outputs;
      cell.cellError = error;
      cell.cellDirty = false;
    }
  }

  let out = el.querySelector('.cell-output');
  const hasContent = hasOutput || (error && error.trim());
  if (!hasContent) {
    if (out) out.remove();
    detachOutputFold(el);
    return;
  }
  if (!out) {
    out = document.createElement('div');
    out.className = 'cell-output';
    el.appendChild(out);
    attachOutputFold(el, cellId);
  }
  setTimeout(() => attachOutputHandle(el, cellId), 0);

  // Pin the wrapper's height so nothing collapses mid-swap; released after
  // the new content has rendered (covers iframe onload height settling).
  if (out.offsetHeight) out.style.minHeight = out.offsetHeight + 'px';

  if (error && !hasOutput) {
    if (out._errText !== error) {
      renderDiagnostics(out, error, cellId);
      out._errText = error;
    }
    out.className = 'cell-output error';
    releaseHeightSoon(out);
    return;
  }
  out._errText = null;
  if (out.classList.contains('error')) out.innerHTML = '';
  out.className = 'cell-output';

  const items = mergeOutputs(outputs).filter((i) => i.oiOutput && i.oiOutput.trim());
  const trail = out.querySelector('.output-error-trail');
  const blocks = [...out.children].filter((c) => !c.classList.contains('output-error-trail'));
  for (let i = 0; i < items.length; i++) {
    const item = items[i];
    let block = blocks[i];
    if (block && block._mime === item.oiMime && block._rendered === item.oiOutput) {
      // Identical content: zero work; iframe focus was never lost, consume the marker
      if (item.oiMime === 'text/html' && _activeTextInput?.cellId === String(cellId)) {
        _activeTextInput = null;
      }
      continue;
    }
    if (block) {
      // Mime changed: drop stale content (e.g. an old iframe) before re-render
      if (block._mime !== item.oiMime) block.innerHTML = '';
    } else {
      block = document.createElement('div');
      block.dataset.cellId = cellId;
      out.insertBefore(block, trail);
    }
    block._mime = item.oiMime;
    block._rendered = item.oiOutput;
    renderMimeOutput(block, item.oiOutput, item.oiMime);
  }
  for (let j = items.length; j < blocks.length; j++) blocks[j].remove();

  let errDiv = out.querySelector('.output-error-trail');
  if (error && error.trim()) {
    if (!errDiv) {
      errDiv = document.createElement('div');
      errDiv.className = 'output-error-trail';
      out.appendChild(errDiv);
    }
    if (errDiv._errText !== error) {
      renderDiagnostics(errDiv, error, cellId);
      errDiv._errText = error;
    }
  } else {
    if (errDiv) errDiv.remove();
  }
  releaseHeightSoon(out);
}

// Releases the minHeight pin once the swapped-in content has painted; if the
// new output is shorter the wrapper settles in one step after the release.
function releaseHeightSoon(out) {
  clearTimeout(out._minHRelease);
  out._minHRelease = setTimeout(() => {
    out.style.minHeight = '';
  }, 350);
}

function clearPartialOutput(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  let stream = el.querySelector('.cell-stream');
  if (stream) stream.remove();
}

function appendPartialOutput(cellId, line) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  let stream = el.querySelector('.cell-stream');
  if (!stream) {
    stream = document.createElement('pre');
    stream.className = 'cell-stream';
    el.appendChild(stream);
  }
  stream.textContent += line + '\n';
  stream.scrollTop = stream.scrollHeight;
}

function renderOutputDiv(cell) {
  const outputs = cell.cellOutputs || [];
  const hasOutput = outputs.length > 0 && outputs.some((o) => o.oiOutput && o.oiOutput.trim());
  const hasContent = hasOutput || (cell.cellError && cell.cellError.trim());
  if (!hasContent) return null;
  const out = document.createElement('div');
  out.className = 'cell-output';
  if (cell.cellError && !hasOutput) {
    out.className = 'cell-output error';
    renderDiagnostics(out, cell.cellError, cell.cellId);
    out._errText = cell.cellError;
    return out;
  }
  for (const item of mergeOutputs(outputs)) {
    if (!item.oiOutput || !item.oiOutput.trim()) continue;
    const block = document.createElement('div');
    block.dataset.cellId = cell.cellId;
    block._mime = item.oiMime;
    block._rendered = item.oiOutput;
    out.appendChild(block);
    renderMimeOutput(block, item.oiOutput, item.oiMime);
  }
  if (cell.cellError && cell.cellError.trim()) {
    const errDiv = document.createElement('div');
    errDiv.className = 'output-error-trail';
    renderDiagnostics(errDiv, cell.cellError, cell.cellId);
    errDiv._errText = cell.cellError;
    out.appendChild(errDiv);
  }
  return out;
}

// VS Code-style output fold: a bar in the gutter beside the output collapses
// it to a one-line placeholder; the placeholder (or the bar) expands it again.
function attachOutputFold(el, cellId) {
  if (el.querySelector('.out-fold')) return;
  const bar = document.createElement('button');
  bar.className = 'fold-bar out-fold';
  bar.setAttribute('aria-label', 'Collapse output');
  bar.title = 'Collapse output';
  bar.onclick = () => toggleOutputCollapse(cellId);
  const note = document.createElement('div');
  note.className = 'out-placeholder';
  note.textContent = 'Output collapsed';
  note.title = 'Show output';
  note.onclick = () => toggleOutputCollapse(cellId);
  el.append(bar, note);
  setOutputCollapsed(el, collapsedOutputs.has(cellId));
}

function detachOutputFold(el) {
  el.querySelectorAll('.out-fold, .out-placeholder').forEach((n) => n.remove());
}

function setOutputCollapsed(el, collapsed) {
  el.classList.toggle('out-collapsed', collapsed);
  const bar = el.querySelector('.out-fold');
  if (!bar) return;
  bar.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
  bar.title = collapsed ? 'Expand output' : 'Collapse output';
}

function toggleOutputCollapse(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  const collapsed = !collapsedOutputs.has(cellId);
  if (collapsed) collapsedOutputs.add(cellId);
  else collapsedOutputs.delete(cellId);
  setOutputCollapsed(el, collapsed);
}
