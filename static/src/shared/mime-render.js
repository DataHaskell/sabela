// ── Shared MIME-aware output rendering ───────────────────────────
// Pure renderer shared by the editor (07-output.js) and the WASM-mode
// share runtime (sabela-wasm-run.js) so browser output matches the editor.
// Runtime globals it relies on are provided by whichever page hosts it:
//   iframeBaseStyle()        — editor: 19-theme.js; wasm-run: its own shim
//   renderMathInMarkdown()   — editor: 17-render-prose.js; wasm-run: shim
//   _activeTextInput         — editor only; wasm-run leaves it undefined
//   marked, katex            — loaded from CDN on every hosting page
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
