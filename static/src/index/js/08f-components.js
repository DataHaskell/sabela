// ── Components pane ──────────────────────────────────────────────
// The workspace's own modules under components/: list them, read what each
// exports, drop a use of one into the notebook, open its source, share it.

let componentsList = [];
let componentSelected = null;
let componentsLoaded = false;

async function loadComponents() {
  const list = document.getElementById('components-list');
  if (!list) return;
  list.innerHTML = '<div class="data-empty">Looking for components…</div>';
  try {
    componentsList = await api('GET', 'components');
  } catch (e) {
    list.innerHTML = '<div class="data-empty">Could not list components.</div>';
    return;
  }
  componentsLoaded = true;
  renderComponentsList();
  if (componentSelected) {
    const sel = componentsList.find((c) => c.ciModule === componentSelected);
    if (sel) renderComponentDetail(sel);
  }
}

function renderComponentsList() {
  const list = document.getElementById('components-list');
  if (!list) return;
  list.innerHTML = '';
  if (!componentsList.length) {
    list.innerHTML =
      '<div class="data-empty">No components yet. Hover a cell and choose <b>component</b> to extract one, ' +
      'or install a bundle someone shared.</div>';
    return;
  }
  for (const c of componentsList) {
    const row = document.createElement('div');
    row.className = 'data-row comp-row' + (componentSelected === c.ciModule ? ' active' : '');
    row.title = c.ciPurpose || c.ciModule;
    const glyph = document.createElement('span');
    glyph.className = 'comp-kind';
    glyph.textContent = { chart: '◔', widget: '⊟', function: 'ƒ' }[c.ciKind] || 'ƒ';
    const name = document.createElement('span');
    name.className = 'data-name';
    name.textContent = c.ciModule.replace(/^Components\./, '');
    const hint = document.createElement('span');
    hint.className = 'data-dir';
    hint.textContent = c.ciExports.length + ' export' + (c.ciExports.length === 1 ? '' : 's');
    row.append(glyph, name, hint);
    row.onclick = () => {
      componentSelected = c.ciModule;
      renderComponentsList();
      renderComponentDetail(c);
    };
    row.draggable = true;
    row.ondragstart = (e) => {
      e.dataTransfer.setData(FILE_DT, JSON.stringify({ component: c.ciModule }));
      e.dataTransfer.effectAllowed = 'copy';
    };
    list.appendChild(row);
  }
}

function renderComponentDetail(c) {
  const detail = document.getElementById('components-detail');
  if (!detail) return;
  detail.innerHTML = '';
  const head = document.createElement('div');
  head.className = 'data-detail-head';
  head.textContent = c.ciModule;
  detail.appendChild(head);
  if (c.ciPurpose) {
    const p = document.createElement('div');
    p.className = 'comp-purpose';
    p.textContent = c.ciPurpose;
    detail.appendChild(p);
  }
  const ex = document.createElement('div');
  ex.className = 'data-schema';
  for (const name of c.ciExports) {
    const r = document.createElement('div');
    r.className = 'data-col';
    r.innerHTML = `<span class="data-col-name">${escapeHtml(name)}</span>`;
    r.title = 'insert a use of ' + name;
    r.onclick = () => insertComponentUse(c, name);
    ex.appendChild(r);
  }
  detail.appendChild(ex);
  if (c.ciDeps.length) {
    const d = document.createElement('div');
    d.className = 'comp-deps';
    d.textContent = 'depends on ' + c.ciDeps.join(', ');
    detail.appendChild(d);
  }
  const bar = document.createElement('div');
  bar.className = 'data-actions';
  bar.append(
    actionButton('＋ Insert use', () => insertComponentUse(c, c.ciExports[0])),
    actionButton('Open source', () => showFilePreview(c.ciPath.split('/').pop(), c.ciPath)),
    actionButton('Download bundle', () => {
      window.location = '/api/components/bundle?module=' + encodeURIComponent(c.ciModule);
    }),
    actionButton('Publish', () => publishComponent(c.ciModule))
  );
  detail.appendChild(bar);
}

function actionButton(label, onClick) {
  const b = document.createElement('button');
  b.className = 'data-load-btn';
  b.textContent = label;
  b.onclick = onClick;
  return b;
}

// A cell that imports the component and names what it uses.
async function insertComponentUse(c, name, afterId) {
  const after = afterId == null ? focusedCellId() : afterId;
  const body = 'import ' + c.ciModule + '\n\n' + (name || '-- ' + c.ciExports.join(', '));
  const id = await addCell(after, 'CodeCell', 'Haskell', body);
  requestAnimationFrame(() => editors[id] && editors[id].focus());
}

// Install a bundle from a JSON file someone shared.
function installComponentFromFile() {
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = 'application/json,.json';
  input.onchange = async () => {
    const f = input.files[0];
    if (!f) return;
    let bundle;
    try {
      bundle = JSON.parse(await f.text());
    } catch (e) {
      setStatus('that file is not a component bundle', 'error');
      return;
    }
    await installComponentBundle(bundle);
  };
  input.click();
}

async function installComponentBundle(bundle, overwrite) {
  try {
    const info = await api('POST', 'components/install', {
      irBundle: bundle,
      irOverwrite: !!overwrite,
    });
    setStatus('installed ' + info.ciModule + '; kernel restarting', 'ok');
    componentSelected = info.ciModule;
    await loadComponents();
  } catch (e) {
    if (!overwrite && /already exists/.test(e.message)) {
      const ok = await askConfirm({
        title: 'Replace component?',
        message: bundle.cbModule + ' already exists in this workspace. Replace it with the bundle?',
        confirmLabel: 'Replace',
        danger: true,
      });
      if (ok) return installComponentBundle(bundle, true);
      return;
    }
    setStatus('install failed: ' + e.message, 'error');
  }
}

async function publishComponent(moduleName) {
  if (typeof hubFetch !== 'function') return;
  const err = document.getElementById('shares-modal-error');
  const list = document.getElementById('shares-modal-list');
  err.textContent = '';
  list.textContent = 'Publishing ' + moduleName + '…';
  document.getElementById('shares-modal-overlay').style.display = 'flex';
  try {
    const res = await hubFetch(
      '/_hub/publish?mode=component&module=' + encodeURIComponent(moduleName),
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ title: moduleName }),
      }
    );
    if (!res.hub) {
      list.textContent = '';
      err.textContent = HUB_ONLY_NOTE;
      return;
    }
    if (!res.ok) {
      err.textContent = res.data.error || 'Publish failed (HTTP ' + res.status + ').';
      await loadSharesList();
      return;
    }
    await loadSharesList(res.data.slug);
  } catch (e) {
    list.textContent = '';
    err.textContent = 'Could not reach the server.';
  }
}
