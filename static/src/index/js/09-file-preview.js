let activePreviewCm = null;
const PREVIEW_WINDOW = 256 * 1024;
const FULL_LOAD_WARN = 5 * 1024 * 1024;

function fmtBytes(n) {
  if (n < 1024) return n + ' B';
  if (n < 1024 * 1024) return (n / 1024).toFixed(1) + ' KB';
  return (n / (1024 * 1024)).toFixed(1) + ' MB';
}

// Open a non-markdown file as a size-safe preview: only a window of bytes
// is fetched, so a huge file can never load whole and freeze the tab. The
// editor stays read-only until the whole file is loaded — saving a
// truncated buffer would clobber the rest on disk.
async function showFilePreview(name, path) {
  const container = document.getElementById('notebook');
  Object.keys(editors).forEach((k) => delete editors[k]);
  notebook = null;
  activePreviewCm = null;
  container.innerHTML = '';
  document.getElementById('toolbar-title').textContent = '📄 ' + name;
  setStatus('Loading ' + path + '…', 'running');

  let first;
  try {
    first = await api(
      'GET',
      'file/preview?path=' + encodeURIComponent(path) + '&offset=0&limit=' + PREVIEW_WINDOW
    );
  } catch (e) {
    setStatus('Could not read file', 'error');
    return;
  }
  setStatus('', '');

  const wrapper = document.createElement('div');
  wrapper.style.cssText =
    'background: var(--bg-cell); border: 1px solid var(--border); border-radius: var(--radius); overflow: hidden;';
  container.appendChild(wrapper);
  const bar = document.createElement('div');
  bar.className = 'preview-bar';
  container.appendChild(bar);

  const state = { path, loaded: first.fpReturned, total: first.fpTotalBytes, eof: first.fpEof };

  requestAnimationFrame(() => {
    const ext = name.split('.').pop();
    const mode = ext === 'hs' ? 'haskell' : null;
    const cm = CodeMirror(wrapper, {
      value: first.fpContent,
      mode: mode,
      theme: cmTheme(),
      lineNumbers: true,
      readOnly: !first.fpEof,
      viewportMargin: Infinity,
    });
    cm._filePath = path;
    cm.on('change', () => {
      cm._dirty = true;
    });
    if (first.fpEof) enablePreviewEditing(cm);
    updatePreviewBar(bar, cm, state);
  });
}

function enablePreviewEditing(cm) {
  cm.setOption('readOnly', false);
  cm.setOption('extraKeys', {
    'Ctrl-S': () => saveArbitraryFile(cm),
    'Cmd-S': () => saveArbitraryFile(cm),
  });
  cm._editable = true;
  activePreviewCm = cm;
}

function updatePreviewBar(bar, cm, state) {
  bar.innerHTML = '';
  const info = document.createElement('span');
  info.className = 'preview-info';
  info.textContent = state.eof
    ? fmtBytes(state.total) + ' · full file' + (cm._editable ? ' · ⌘S / Ctrl-S to save' : '')
    : 'Showing ' + fmtBytes(state.loaded) + ' of ' + fmtBytes(state.total) + ' · read-only preview';
  bar.appendChild(info);
  if (!state.eof) {
    const moreBtn = document.createElement('button');
    moreBtn.className = 'preview-btn';
    moreBtn.textContent = 'Show more';
    moreBtn.onclick = () => loadMorePreview(bar, cm, state, false);
    bar.appendChild(moreBtn);
    const fullBtn = document.createElement('button');
    fullBtn.className = 'preview-btn';
    fullBtn.textContent = 'Load full file';
    fullBtn.onclick = () => loadMorePreview(bar, cm, state, true);
    bar.appendChild(fullBtn);
  }
}

async function loadMorePreview(bar, cm, state, full) {
  if (full && state.total - state.loaded > FULL_LOAD_WARN) {
    const ok = await askConfirm({
      title: 'Load full file?',
      message: 'This file is ' + fmtBytes(state.total) + '. Loading all of it may be slow.',
      confirmLabel: 'Load all',
    });
    if (!ok) return;
  }
  const limit = full ? state.total - state.loaded : PREVIEW_WINDOW;
  let res;
  try {
    res = await api(
      'GET',
      'file/preview?path=' +
        encodeURIComponent(state.path) +
        '&offset=' +
        state.loaded +
        '&limit=' +
        limit
    );
  } catch (e) {
    setStatus('Could not read more of the file', 'error');
    return;
  }
  const wasReadOnly = cm.getOption('readOnly');
  cm.setOption('readOnly', false);
  const last = cm.lastLine();
  cm.replaceRange(res.fpContent, { line: last, ch: cm.getLine(last).length });
  cm._dirty = false;
  state.loaded += res.fpReturned;
  state.eof = res.fpEof;
  if (state.eof) enablePreviewEditing(cm);
  else cm.setOption('readOnly', wasReadOnly);
  updatePreviewBar(bar, cm, state);
}

async function saveArbitraryFile(cm) {
  if (!cm._filePath) return;
  try {
    await api('POST', 'file/write', { wfPath: cm._filePath, wfContent: cm.getValue() });
    cm._dirty = false;
    flashSaved();
  } catch (e) {
    setStatus('Save failed: ' + e.message, 'error');
  }
}

function startNewFile() {
  showNewItemInput('file');
}
function startNewFolder() {
  showNewItemInput('folder');
}

function showNewItemInput(type) {
  const container = document.getElementById('new-item-input');
  container.style.display = 'block';
  container.innerHTML = '';
  const div = document.createElement('div');
  div.className = 'sidebar-input';
  const input = document.createElement('input');
  input.placeholder = type === 'folder' ? 'folder name' : 'filename.md';
  const btn = document.createElement('button');
  btn.textContent = '✓';
  btn.onclick = () => createNewItem(type, input.value);
  input.onkeydown = (e) => {
    if (e.key === 'Enter') createNewItem(type, input.value);
    if (e.key === 'Escape') {
      container.style.display = 'none';
    }
  };
  div.appendChild(input);
  div.appendChild(btn);
  container.appendChild(div);
  requestAnimationFrame(() => input.focus());
}

async function createNewItem(type, name) {
  if (!name.trim()) return;
  const container = document.getElementById('new-item-input');
  container.style.display = 'none';
  try {
    await api('POST', 'file/create', {
      cfPath: name.trim(),
      cfContent:
        type === 'folder'
          ? ''
          : name.endsWith('.md')
            ? '# ' + name.replace('.md', '') + '\n\n```haskell\nputStrLn "Hello!"\n```\n'
            : '',
      cfIsDir: type === 'folder',
    });
    refreshFiles();
    setStatus('Created ' + name.trim(), '');
  } catch (e) {
    setStatus('Create failed: ' + e.message, 'error');
  }
}

function refreshFiles() {
  loadFileTree('.', document.getElementById('file-tree'), 0);
}
