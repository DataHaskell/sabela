// ── MIME-aware output rendering ──────────────────────────────────
// renderMimeOutput + mergeOutputs now live in the shared partial
// static/src/shared/mime-render.js (referenced before this file in the
// index shell) so the WASM-mode share runtime reuses identical rendering.

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
    return;
  }
  if (!out) {
    out = document.createElement('div');
    out.className = 'cell-output';
    el.appendChild(out);
  }

  // Pin the wrapper's height so nothing collapses mid-swap; released after
  // the new content has rendered (covers iframe onload height settling).
  if (out.offsetHeight) out.style.minHeight = out.offsetHeight + 'px';

  if (error && !hasOutput) {
    if (out._errText !== error) {
      out.textContent = error;
      out._errText = error;
    }
    out.className = 'cell-output error';
    releaseHeightSoon(out);
    return;
  }
  out._errText = null;
  if (out.classList.contains('error')) out.textContent = '';
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
    errDiv.textContent = error;
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
    out.textContent = cell.cellError;
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
    errDiv.textContent = cell.cellError;
    out.appendChild(errDiv);
  }
  return out;
}
