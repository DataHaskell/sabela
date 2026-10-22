// ── Render dashboard ─────────────────────────────────────────────
function renderDashboard(nb) {
  document.getElementById('dashboard-title').textContent = formatTitle(nb.nbTitle);
  document.title = formatTitle(nb.nbTitle) + ' — Sabela Dashboard';

  const main = document.getElementById('main-content');
  main.innerHTML = '';

  const cells = nb.nbCells || [];
  for (const cell of cells) {
    renderCellOutputs(main, cell, null);
  }
}

// Render KaTeX inside a markdown source before handing to marked, so
// $...$ / $$...$$ blocks survive marked's underscore/star handling.
// Safe no-op if katex isn't available.
function renderMarkdownWithMath(src) {
  let s = src || '';
  if (typeof katex !== 'undefined') {
    s = s.replace(/\$\$([\s\S]+?)\$\$/g, (_, tex) => {
      try {
        return katex.renderToString(tex.trim(), { displayMode: true, throwOnError: false });
      } catch (_e) {
        return _;
      }
    });
    s = s.replace(/(^|[^\\$])\$([^\$\n]+?)\$(?!\d)/g, (_, pre, tex) => {
      try {
        return pre + katex.renderToString(tex, { displayMode: false, throwOnError: false });
      } catch (_e) {
        return _;
      }
    });
  }
  return marked.parse(s);
}

// ── Render a single cell's outputs into the container ────────────
function renderCellOutputs(container, cell, insertBefore) {
  if (cell.cellType === 'ProseCell') {
    const section = document.createElement('div');
    section.className = 'dash-section';
    section.dataset.cellId = cell.cellId;
    section.innerHTML = renderMarkdownWithMath(cell.cellSource || '');
    section.querySelectorAll('table').forEach((t) => {
      const w = document.createElement('div');
      w.style.overflowX = 'auto';
      t.parentNode.insertBefore(w, t);
      w.appendChild(t);
    });
    container.insertBefore(section, insertBefore);
    return;
  }

  // CodeCell — in notebook (tutorial) mode, show the source above its outputs.
  if (renderMode === 'notebook' && (cell.cellSource || '').trim()) {
    const codeWrap = document.createElement('div');
    codeWrap.className = 'dash-code';
    codeWrap.dataset.cellId = cell.cellId;
    const pre = document.createElement('pre');
    const codeEl = document.createElement('code');
    codeEl.className = 'hljs';
    const src = cell.cellSource || '';
    const hl = HLJS_LANG[cell.cellLang || 'Haskell'];
    if (hl && window.hljs && hljs.getLanguage(hl)) {
      codeEl.innerHTML = hljs.highlight(src, { language: hl }).value;
    } else {
      codeEl.textContent = src;
    }
    pre.appendChild(codeEl);
    codeWrap.appendChild(pre);
    container.insertBefore(codeWrap, insertBefore);
  }

  // CodeCell — render all outputs inline in order
  const outputs = cell.cellOutputs || [];
  const merged = mergeHtmlOutputs(outputs);
  for (const o of merged) {
    if (!o.oiOutput || !o.oiOutput.trim()) continue;
    const isWidget = classifyOutput(o) === 'widget';
    const card = document.createElement('div');
    card.className = isWidget ? 'dash-widget' : 'dash-output';
    card.dataset.cellId = cell.cellId;

    if (isWidget) {
      renderWidgetOutput(card, o, cell.cellId);
    } else {
      renderOutput(card, o.oiOutput, o.oiMime);
    }
    container.insertBefore(card, insertBefore);
  }

  // Show errors
  if (cell.cellError && cell.cellError.trim()) {
    const errEl = document.createElement('div');
    errEl.className = 'dash-error';
    errEl.dataset.cellId = cell.cellId;
    errEl.textContent = cell.cellError;
    container.insertBefore(errEl, insertBefore);
  }
}

// ── Output classification ────────────────────────────────────────
function classifyOutput(item) {
  if (
    item.oiMime === 'text/html' &&
    item.oiOutput &&
    item.oiOutput.includes('class="sabela-viz"')
  ) {
    return 'content';
  }
  if (
    item.oiMime === 'text/html' &&
    item.oiOutput &&
    item.oiOutput.includes('parent.postMessage')
  ) {
    return 'widget';
  }
  return 'content';
}

// ── Merge consecutive HTML outputs (non-widget only) ─────────────
function mergeHtmlOutputs(outputs) {
  const merged = [];
  for (const o of outputs) {
    const isWidget = classifyOutput(o) === 'widget';
    const last = merged[merged.length - 1];
    if (
      !isWidget &&
      o.oiMime === 'text/html' &&
      last &&
      last.oiMime === 'text/html' &&
      classifyOutput(last) !== 'widget'
    ) {
      last.oiOutput += '\n' + o.oiOutput;
    } else {
      merged.push({ oiMime: o.oiMime, oiOutput: o.oiOutput });
    }
  }
  return merged;
}
