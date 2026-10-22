// ── The "+ Add" menu ──────────────────────────────────────────────
// One place to add any kind of object: a thought, a computation, a chart or
// table, a control, a media file. Each kind inserts a canonical cell; the
// evidence kinds open the Design panel on it.

const INSERT_KINDS = [
  { id: 'thought', group: 'Write', label: 'Thought', hint: 'markdown' },
  { id: 'haskell', group: 'Write', label: 'Computation', hint: 'Haskell' },
  { id: 'python', group: 'Write', label: 'Computation', hint: 'Python' },
  { id: 'chart', group: 'Evidence', label: 'Chart', hint: 'Visualize panel' },
  { id: 'table', group: 'Evidence', label: 'Table', hint: 'show data' },
  { id: 'slider', group: 'Controls', label: 'Slider', hint: 'n <- slider' },
  { id: 'dropdown', group: 'Controls', label: 'Dropdown', hint: 'choice <- dropdown' },
  { id: 'button', group: 'Controls', label: 'Button', hint: 'clicked <- button' },
  { id: 'textInput', group: 'Controls', label: 'Text input', hint: 'q <- textInput' },
  { id: 'image', group: 'Media', label: 'Image', hint: 'upload' },
  { id: 'video', group: 'Media', label: 'Video', hint: 'upload' },
  { id: 'audio', group: 'Media', label: 'Audio', hint: 'upload' },
  { id: 'datafile', group: 'Data', label: 'Data file…', hint: 'Data pane' },
];

function openInsertMenu(anchor, afterId) {
  closeCtxMenu();
  const menu = document.createElement('div');
  menu.className = 'ctx-menu insert-menu';
  menu.onclick = (e) => e.stopPropagation();
  const filter = document.createElement('input');
  filter.className = 'insert-filter';
  filter.placeholder = 'Add…';
  filter.setAttribute('aria-label', 'Filter object kinds');
  menu.appendChild(filter);
  const list = document.createElement('div');
  list.className = 'insert-list';
  menu.appendChild(list);
  const draw = () => {
    list.innerHTML = '';
    const q = filter.value.trim().toLowerCase();
    let group = null;
    for (const k of INSERT_KINDS) {
      if (q && !(k.label + ' ' + k.hint + ' ' + k.group).toLowerCase().includes(q)) continue;
      if (k.group !== group) {
        group = k.group;
        const g = document.createElement('div');
        g.className = 'insert-group';
        g.textContent = group;
        list.appendChild(g);
      }
      const item = document.createElement('div');
      item.className = 'ctx-menu-item insert-item';
      item.innerHTML = `<span class="nm">${escapeHtml(k.label)}</span><span class="insert-hint">${escapeHtml(k.hint)}</span>`;
      item.onclick = () => {
        closeCtxMenu();
        insertKind(k.id, afterId);
      };
      list.appendChild(item);
    }
  };
  filter.oninput = draw;
  filter.onkeydown = (e) => {
    if (e.key === 'Enter') {
      const first = list.querySelector('.insert-item');
      if (first) first.click();
    } else if (e.key === 'Escape') closeCtxMenu();
  };
  draw();
  // Shown after the click that asked for it has finished bubbling, or the
  // document's own click handler would close it at once.
  setTimeout(() => {
    document.body.appendChild(menu);
    const r = anchor.getBoundingClientRect();
    menu.style.left =
      Math.max(8, Math.min(r.left, window.innerWidth - menu.offsetWidth - 8)) + 'px';
    menu.style.top = Math.min(r.bottom + 4, window.innerHeight - menu.offsetHeight - 8) + 'px';
    activeCtxMenu = menu;
    filter.focus();
  }, 0);
}

// A control name no cell uses yet, so two sliders never share a value.
function uniqueWidgetName(base) {
  const sources = ((notebook && notebook.nbCells) || []).map((c) => c.cellSource || '').join('\n');
  let name = base;
  for (let i = 2; new RegExp('"' + name + '"').test(sources); i++) name = base + i;
  return name;
}

function widgetTemplate(kind) {
  const n = uniqueWidgetName;
  switch (kind) {
    case 'slider':
      return `n <- mkWidget (slider "${n('n')}" 5 0 10)\nn`;
    case 'dropdown':
      return `choice <- mkWidget (dropdown "${n('choice')}" ["a", "b", "c"] "a")\nchoice`;
    case 'button':
      return `clicked <- mkWidget (button "Run" "${n('go')}")\nclicked`;
    case 'textInput':
      return `q <- mkWidget (textInput "${n('q')}" "")\nq`;
    default:
      return '';
  }
}

async function insertKind(kind, afterId) {
  switch (kind) {
    case 'thought': {
      const id = await addCell(afterId, 'ProseCell', 'Haskell', '');
      requestAnimationFrame(() => {
        const el = document.querySelector(`.cell[data-id="${id}"]`);
        if (el && typeof editProse === 'function') editProse(id);
      });
      return;
    }
    case 'haskell':
    case 'python': {
      const id = await addCell(afterId, 'CodeCell', kind === 'python' ? 'Python' : 'Haskell', '');
      requestAnimationFrame(() => editors[id] && editors[id].focus());
      return;
    }
    case 'chart':
      openDesigner(null, { afterId, mark: 'bar' });
      return;
    case 'table': {
      const sources = await listVizSources();
      const first = sources.find((s) => s.kind === 'binding' && !s.disabled);
      const id = await addCell(
        afterId,
        'CodeCell',
        'Haskell',
        first ? first.ref : '-- a binding on its own line shows as a table'
      );
      if (first) api('POST', 'run/' + id).catch(() => {});
      else requestAnimationFrame(() => editors[id] && editors[id].focus());
      return;
    }
    case 'slider':
    case 'dropdown':
    case 'button':
    case 'textInput': {
      const id = await addCell(afterId, 'CodeCell', 'Haskell', widgetTemplate(kind));
      api('POST', 'run/' + id).catch(() => {});
      return;
    }
    case 'image':
    case 'video':
    case 'audio': {
      const paths = await pickAndUploadMedia(kind);
      let after = afterId;
      for (const p of paths) {
        after = await addCell(
          after,
          'CodeCell',
          'Haskell',
          kind + ' "./' + p.replace(/"/g, '\\"') + '"'
        );
        api('POST', 'run/' + after).catch(() => {});
      }
      return;
    }
    case 'datafile':
      selectSidePane('data');
      setStatus(
        'pick a file in the Data pane, then drag it onto the notebook or press Insert load cell',
        'ok'
      );
      return;
    default:
      return;
  }
}

// Upload through the hidden file input, filtered to the kind, and hand back
// the paths the server wrote.
function pickAndUploadMedia(kind) {
  const accept = { image: 'image/*', video: 'video/*', audio: 'audio/*' }[kind] || '*/*';
  return new Promise((resolve) => {
    let input = document.getElementById('hidden-upload-input');
    if (!input) {
      input = document.createElement('input');
      input.type = 'file';
      input.multiple = true;
      input.id = 'hidden-upload-input';
      input.style.display = 'none';
      document.body.appendChild(input);
    }
    input.value = '';
    input.accept = accept;
    input.onchange = async () => {
      const paths = await uploadFiles('.', Array.from(input.files));
      input.accept = '';
      resolve(paths || []);
    };
    input.click();
  });
}

function focusedCellId() {
  const el = document.querySelector('.cell.focused') || document.querySelector('.cell.designing');
  return el ? Number(el.dataset.id) : lastCellId();
}

function insertAfterFocused(kind) {
  insertKind(kind, focusedCellId());
}

function openInsertMenuAtFocused() {
  const id = focusedCellId();
  const el =
    document.querySelector(`.cell[data-id="${id}"]`) || document.getElementById('notebook');
  openInsertMenu(el, id);
}
