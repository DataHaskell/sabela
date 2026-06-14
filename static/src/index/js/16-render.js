// ── Render notebook ──────────────────────────────────────────────
function render(nb) {
  notebook = nb;
  setToolbarTitle(nb.nbTitle);
  const container = document.getElementById('notebook');

  // Staggered entrance — first render only, so reactive re-renders don't replay it.
  if (!didIntroReveal) {
    didIntroReveal = true;
    container.classList.add('intro');
    setTimeout(() => container.classList.remove('intro'), 1500);
  }

  // Capture unsaved editor content before clearing the DOM, but ONLY for
  // cells the user has actively edited (tracked in `dirtyCells`). Clean
  // cells let the server-supplied source win so AI-driven edits to
  // untouched cells show up immediately. We scan the live DOM rather
  // than the `editors` dict to dodge rAF-mount races where `editors`
  // may have been cleared before the prior render's rAF fired.
  const draftContent = {};
  container.querySelectorAll('.cell[data-id]').forEach((cellDiv) => {
    const id = parseInt(cellDiv.dataset.id);
    if (Number.isNaN(id)) return;
    if (!dirtyCells.has(id)) return;
    const cmWrapper = cellDiv.querySelector('.CodeMirror');
    if (cmWrapper && cmWrapper.CodeMirror) {
      draftContent[id] = cmWrapper.CodeMirror.getValue();
    }
  });

  container.innerHTML = '';
  Object.keys(editors).forEach((k) => delete editors[k]);

  container.appendChild(makeAddRow(-1));
  let cellNum = 0;
  for (const cell of nb.nbCells) {
    cellNum++;
    const src =
      draftContent[cell.cellId] !== undefined ? draftContent[cell.cellId] : cell.cellSource;
    const el =
      cell.cellType === 'CodeCell'
        ? renderCodeCell(
            { ...cell, cellSource: src, cellLang: cell.cellLang || 'Haskell' },
            cellNum
          )
        : renderProseCell(cell, cellNum);
    container.appendChild(el);
    container.appendChild(makeAddRow(cell.cellId));
  }
}

// Matches the `-- compile` / `-- compile: Module.Name` cell directive.
const COMPILE_DIRECTIVE_RE = /^--\s*compile(\s*:.*)?\s*$/;

function isCompiledSource(cell) {
  return (
    cell.cellLang !== 'Python' &&
    cell.cellSource.split('\n').some((l) => COMPILE_DIRECTIVE_RE.test(l))
  );
}

function renderCodeCell(cell, cellNum) {
  const div = document.createElement('div');
  div.className = 'cell code';
  div.dataset.id = cell.cellId;
  if (cell.cellLang === 'Python') div.classList.add('python');
  if (cell.cellDirty) div.classList.add('dirty');
  if (cell.cellError) div.classList.add('has-error');
  const compiled = isCompiledSource(cell);
  if (compiled) div.classList.add('compiled');

  const gutter = document.createElement('div');
  gutter.className = 'cell-gutter';
  gutter.innerHTML =
    `<span class="cell-number">${cell.cellId}</span><select class="lang-tag" onchange="setCellLang(${cell.cellId}, this.value)"><option value="Haskell"${cell.cellLang === 'Haskell' ? ' selected' : ''}>hs</option><option value="Python"${cell.cellLang === 'Python' ? ' selected' : ''}>py</option></select>` +
    (compiled
      ? `<button class="compile-tag" onclick="toggleCellCompile(${cell.cellId})" title="Compiled to native code (-O2). Click to switch back to interpreted.">⚡comp</button>`
      : '');
  div.appendChild(gutter);

  const collapsed = collapsedCells.has(cell.cellId);
  if (collapsed) div.classList.add('collapsed');

  const actions = document.createElement('div');
  actions.className = 'cell-actions';
  actions.innerHTML = `
    <button class="collapse-btn" onclick="toggleCellCollapse(${cell.cellId})" title="Hide/show cell" aria-label="Toggle cell visibility" aria-expanded="${collapsed ? 'false' : 'true'}"><svg class="icon-svg small"><use href="#${collapsed ? 'i-chev-right' : 'i-chev-down'}"/></svg></button>
    <button class="run-btn" onclick="runCell(${cell.cellId})" aria-label="Run cell" title="Run cell (${kbd('shift', 'enter')})"><svg class="icon-svg small"><use href="#i-play"/></svg> run</button>
    <button class="clear-btn" onclick="clearCellOutput(${cell.cellId})" aria-label="Clear output" title="Clear output"><svg class="icon-svg small"><use href="#i-circle-slash"/></svg></button>
    ${cell.cellLang !== 'Python' ? `<button class="export-btn" onclick="exportPipeline(${cell.cellId}, 'haskell')" aria-label="Export pipeline ending here" title="Export pipeline ending here (.hs)"><svg class="icon-svg small"><use href="#i-download"/></svg></button>` : ''}
    ${cell.cellLang !== 'Python' && !compiled ? `<button class="compile-mode-btn" onclick="toggleCellCompile(${cell.cellId})" aria-label="Compile this cell" title="Compile this cell to native code (-O2)">⚡</button>` : ''}
    <button class="delete-btn" onclick="deleteCell(${cell.cellId})" aria-label="Delete cell" title="Delete cell"><svg class="icon-svg small"><use href="#i-trash"/></svg></button>`;
  div.appendChild(actions);

  const summary = document.createElement('div');
  summary.className = 'cell-summary';
  summary.textContent = (cell.cellSource || '').split('\n')[0].trim() || '(empty)';
  gutter.appendChild(summary);

  const editorDiv = document.createElement('div');
  editorDiv.className = 'cell-editor';
  div.appendChild(editorDiv);

  const outDiv = renderOutputDiv(cell);
  if (outDiv) div.appendChild(outDiv);

  requestAnimationFrame(() => {
    // Previously guarded with `if (editors[cell.cellId]) return` to avoid
    // double-mounting. That guard was a race hazard: when render() is
    // called twice in quick succession (common under AI-driven
    // notebookChanged broadcasts), rAF callbacks from the stale render
    // would populate `editors[id]` against detached DOM, then the
    // current render's rAFs would skip mounting and leave the visible
    // `.cell-editor` empty — a 0-height "squished" cell. Always mount
    // into the current DOM; old detached CM instances are GC'd.
    if (editors[cell.cellId]) {
      // Clean up any prior instance still lingering in the dict.
      delete editors[cell.cellId];
    }
    const cm = CodeMirror(editorDiv, {
      value: cell.cellSource,
      mode: cell.cellLang === 'Python' ? 'python' : 'haskell',
      theme: cmTheme(),
      lineNumbers: false,
      viewportMargin: Infinity,
      inputStyle: window.matchMedia('(pointer: coarse)').matches ? 'contenteditable' : 'textarea',
      indentUnit: 2,
      tabSize: 2,
      lineWrapping: true,
      gutters: ['error-gutter'],
      extraKeys: {
        'Shift-Enter': () => runCell(cell.cellId),
        'Ctrl-Enter': () => runCell(cell.cellId),
        Tab: (cm) => {
          if (cm.somethingSelected()) {
            cm.indentSelection('add');
          } else {
            const cur = cm.getCursor();
            const lineUpToCursor = cm.getRange({ line: cur.line, ch: 0 }, cur);
            const match = lineUpToCursor.match(
              /[A-Za-z_'][A-Za-z0-9_']*(?:\.[A-Za-z_'][A-Za-z0-9_']*)*\.?$/
            );
            if (match) {
              cm.showHint({
                hint: haskellHint,
                completeSingle: true,
                extraKeys: { Tab: (cm, handle) => handle.pick() },
              });
            } else {
              cm.execCommand('insertSoftTab');
            }
          }
        },
        'Ctrl-Space': (cm) =>
          cm.showHint({
            hint: haskellHint,
            completeSingle: true,
            extraKeys: { Tab: (cm, handle) => handle.pick() },
          }),
        'Ctrl-I': () => lookupWordUnderCursor(),
        'Cmd-I': () => lookupWordUnderCursor(),
        'Ctrl-S': () => saveNotebook(),
        'Cmd-S': () => saveNotebook(),
      },
    });
    cm.on('change', (_cm, changeObj) => {
      unsavedChanges = true;
      // Ignore programmatic setValue() from our own mount as "user edit".
      if (changeObj && changeObj.origin !== 'setValue') {
        dirtyCells.add(cell.cellId);
      }
    });
    editors[cell.cellId] = cm;
  });
  return div;
}

// Directory of the currently open notebook (relative to the work dir).
