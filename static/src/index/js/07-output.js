// ── MIME-aware output rendering ──────────────────────────────────
function mergeOutputs(outputs) {
  const merged = [];
  for (const item of outputs) {
    const last = merged[merged.length - 1];
    if (last && last.oiMime === item.oiMime && item.oiMime === 'text/html') {
      merged[merged.length - 1] = {
        oiMime: item.oiMime,
        oiOutput: last.oiOutput + '\n' + item.oiOutput,
      };
    } else {
      merged.push({ ...item });
    }
  }
  return merged;
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

function renderMimeOutput(container, content, mime) {
  if (mime === 'text/html') {
    container.className = 'cell-output mime-html';
    let iframe = container.querySelector('iframe');
    const isNew = !iframe;
    if (isNew) {
      iframe = document.createElement('iframe');
      iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin');
      container.appendChild(iframe);
    }
    requestAnimationFrame(() => {
      const doc = iframe.contentDocument || iframe.contentWindow?.document;
      if (!doc) {
        console.error('Cannot access iframe document');
        return;
      }
      iframe.dataset.lastContent = content;
      if (isNew) iframe.style.height = '0px';
      doc.open();
      iframe.onload = () => {
        // Collapse to 1px before reading scrollHeight so the viewport does not
        // inflate doc.body.scrollHeight — all three assignments are synchronous
        // inside this onload callback so the browser never paints the 1px state.
        iframe.style.height = '1px';
        iframe.style.height = Math.max(60, doc.body.scrollHeight + 20) + 'px';
      };
      doc.write(iframeBaseStyle() + content);
      doc.close();
      const ati = _activeTextInput;
      if (ati && ati.cellId === container.dataset.cellId) {
        _activeTextInput = null;
        setTimeout(() => {
          const d = iframe.contentDocument || iframe.contentWindow?.document;
          const inp = d && d.querySelector('input[type=text]');
          if (inp) {
            inp.focus();
            inp.setSelectionRange(ati.sel, ati.sel);
          }
        }, 0);
      }
    });
  } else if (mime === 'text/markdown') {
    container.innerHTML = '';
    container.className = 'cell-output mime-markdown';
    // Render KaTeX before marked so $$...$$/$...$ survive marked's
    // underscore/star handling; code spans are protected so dollar signs
    // inside them are never treated as math delimiters.
    const md = renderMathInMarkdown(content);
    container.innerHTML = marked.parse(md);
    container.querySelectorAll('table').forEach((t) => {
      const w = document.createElement('div');
      w.className = 'table-wrapper';
      t.parentNode.insertBefore(w, t);
      w.appendChild(t);
    });
  } else if (mime === 'image/svg+xml') {
    container.innerHTML = '';
    container.className = 'cell-output mime-svg';
    container.innerHTML = content;
  } else if (mime.startsWith('image/') && mime.includes('base64')) {
    container.innerHTML = '';
    container.className = 'cell-output mime-image';
    const mimeClean = mime.replace(';base64', '');
    container.innerHTML = `<img src="data:${mimeClean};base64,${content.trim()}" />`;
  } else if (mime === 'application/json') {
    container.innerHTML = '';
    container.className = 'cell-output mime-json';
    try {
      container.textContent = JSON.stringify(JSON.parse(content), null, 2);
    } catch {
      container.textContent = content;
    }
  } else if (mime === 'text/latex') {
    container.innerHTML = '';
    container.className = 'cell-output mime-latex';
    try {
      katex.render(content, container, { displayMode: true, throwOnError: false });
    } catch (e) {
      container.textContent = content;
    }
  } else {
    container.innerHTML = '';
    container.className = 'cell-output';
    container.textContent = content;
  }
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
