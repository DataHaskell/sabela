// ── Public shares (Phase 3a) ─────────────────────────────────────
// Publish / list / unpublish talk to the hub at /_hub/*, which intercepts
// those paths and never proxies them to the notebook backend. Running the
// editor standalone (no hub), /_hub/* falls through to the SPA catch-all and
// returns HTML, not JSON — `hubFetch` detects that (no JSON content-type) so we
// show a friendly "hub only" note instead of a misleading error.
const HUB_ONLY_NOTE = 'Public sharing is only available when running on the Sabela hub.';

async function hubFetch(path, opts) {
  const r = await fetch(
    path,
    Object.assign({ headers: { Accept: 'application/json' } }, opts || {})
  );
  const ct = r.headers.get('content-type') || '';
  if (!ct.includes('application/json'))
    return { hub: false, ok: false, status: r.status, data: {} };
  let data = {};
  try {
    data = await r.json();
  } catch (e) {}
  return { hub: true, ok: r.ok, status: r.status, data };
}

async function publishShare(mode) {
  const list = document.getElementById('shares-modal-list');
  const err = document.getElementById('shares-modal-error');
  err.textContent = '';
  list.textContent = 'Publishing the ' + mode + '…';
  document.getElementById('shares-modal-overlay').style.display = 'flex';
  try {
    const res = await hubFetch('/_hub/publish?mode=' + encodeURIComponent(mode), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ title: (notebook && notebook.nbTitle) || 'Untitled' }),
    });
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

async function openSharesModal() {
  document.getElementById('shares-modal-error').textContent = '';
  document.getElementById('shares-modal-list').textContent = 'Loading…';
  document.getElementById('shares-modal-overlay').style.display = 'flex';
  await loadSharesList();
}
function closeSharesModal() {
  document.getElementById('shares-modal-overlay').style.display = 'none';
}

async function loadSharesList(highlightSlug) {
  try {
    const res = await hubFetch('/_hub/shares');
    if (!res.hub) {
      renderSharesNote(HUB_ONLY_NOTE);
      return;
    }
    if (!res.ok) {
      renderSharesNote('Could not load shares.');
      return;
    }
    renderSharesList(res.data, highlightSlug);
  } catch (e) {
    renderSharesNote('Could not reach the server.');
  }
}

function renderSharesNote(text) {
  const list = document.getElementById('shares-modal-list');
  list.innerHTML = '';
  const p = document.createElement('p');
  p.className = 'shares-empty';
  p.textContent = text;
  list.appendChild(p);
}

// Build rows with the DOM API (no innerHTML interpolation) so a slug/mode can
// never inject markup, even though the hub already constrains them.
function renderSharesList(shares, highlightSlug) {
  if (!Array.isArray(shares) || !shares.length) {
    renderSharesNote('No public shares yet. Use Publish to create one.');
    return;
  }
  const list = document.getElementById('shares-modal-list');
  list.innerHTML = '';
  shares.sort((a, b) => (b.createdAt || '').localeCompare(a.createdAt || ''));
  for (const s of shares) {
    const url = location.origin + s.url;
    let when = s.createdAt || '';
    const d = new Date(s.createdAt);
    if (!isNaN(d.getTime())) when = d.toLocaleString();

    const row = document.createElement('div');
    row.className = 'share-row' + (s.slug === highlightSlug ? ' share-row-new' : '');

    const main = document.createElement('div');
    main.className = 'share-row-main';
    const modeEl = document.createElement('span');
    modeEl.className = 'share-mode';
    modeEl.textContent = s.mode;
    const link = document.createElement('a');
    link.className = 'share-link';
    link.href = url;
    link.target = '_blank';
    link.rel = 'noopener';
    link.textContent = url;
    const whenEl = document.createElement('span');
    whenEl.className = 'share-when';
    whenEl.textContent = when;
    main.append(modeEl, link, whenEl);

    const actions = document.createElement('div');
    actions.className = 'share-row-actions';
    const copyBtn = document.createElement('button');
    copyBtn.textContent = 'Copy link';
    copyBtn.addEventListener('click', () => copyShareUrl(url, copyBtn));
    const delBtn = document.createElement('button');
    delBtn.className = 'danger';
    delBtn.textContent = 'Unpublish';
    delBtn.addEventListener('click', () => deleteShare(s.slug));
    actions.append(copyBtn, delBtn);

    row.append(main, actions);
    list.appendChild(row);
  }
}

async function copyShareUrl(url, btn) {
  try {
    await navigator.clipboard.writeText(url);
    const old = btn.textContent;
    btn.textContent = 'Copied ✓';
    setTimeout(() => {
      btn.textContent = old;
    }, 1500);
  } catch (e) {
    /* clipboard unavailable; leave the link for manual copy */
  }
}

async function deleteShare(slug) {
  const ok = await askConfirm({
    title: 'Unpublish share',
    message: 'Remove this public link? The page will stop loading for anyone who has it.',
    confirmLabel: 'Unpublish',
    danger: true,
  });
  if (!ok) return;
  try {
    const res = await hubFetch('/_hub/shares/' + encodeURIComponent(slug), { method: 'DELETE' });
    if (res.hub && res.ok) {
      await loadSharesList();
    } else {
      document.getElementById('shares-modal-error').textContent = res.hub
        ? 'Could not unpublish.'
        : HUB_ONLY_NOTE;
    }
  } catch (e) {
    document.getElementById('shares-modal-error').textContent = 'Could not reach the server.';
  }
}

async function confirmDeleteFile(path, name, isDir) {
  const ok = await askConfirm({
    title: `Delete ${isDir ? 'folder' : 'file'}`,
    message: isDir
      ? `Delete "${name}" and all its contents? This cannot be undone.`
      : `Delete "${name}"? This cannot be undone.`,
    confirmLabel: 'Delete',
    danger: true,
  });
  if (!ok) return;
  try {
    await api('POST', 'file/delete', { dfPath: path });
    refreshFiles();
    setStatus('Deleted ' + name, '');
  } catch (e) {
    setStatus('Delete failed: ' + e.message, 'error');
  }
}

async function promptRenameFile(oldPath, oldName) {
  const newName = await askInput({
    title: 'Rename',
    label: 'New name',
    placeholder: oldName,
    defaultValue: oldName,
    confirmLabel: 'Rename',
    validate: (v) => (v === oldName ? 'Choose a different name.' : null),
  });
  if (!newName) return;
  const dir = oldPath.includes('/') ? oldPath.substring(0, oldPath.lastIndexOf('/') + 1) : '';
  renameFile(oldPath, dir + newName);
}

async function renameFile(oldPath, newPath) {
  try {
    await api('POST', 'file/rename', { rfOldPath: oldPath, rfNewPath: newPath });
    refreshFiles();
    setStatus('Renamed', '');
  } catch (e) {
    setStatus('Rename failed: ' + e.message, 'error');
  }
}

// Move via the rename endpoint, which already accepts a cross-directory
// destination. Type "." to move to the workspace root.
async function promptMoveFile(oldPath, name) {
  const curDir = oldPath.includes('/') ? oldPath.substring(0, oldPath.lastIndexOf('/')) : '.';
  const dest = await askInput({
    title: 'Move ' + name,
    label: 'Destination folder',
    placeholder: 'folder path ("." for root)',
    defaultValue: curDir,
    confirmLabel: 'Move',
  });
  if (dest === null) return;
  const raw = dest.trim().replace(/\/+$/, '');
  const cleanDir = raw === '.' ? '' : raw;
  const newPath = cleanDir ? cleanDir + '/' + name : name;
  if (newPath === oldPath) return;
  renameFile(oldPath, newPath);
}

function copyRelPath(path) {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(path).then(
      () => {
        setStatus('Copied ' + path, '');
        flashSaved();
      },
      () => setStatus('Copy failed', 'error')
    );
  } else {
    setStatus('Clipboard unavailable', 'error');
  }
}

function downloadAsset(path, name) {
  const a = document.createElement('a');
  a.href = '/api/asset?path=' + encodeURIComponent(path);
  a.download = name;
  document.body.appendChild(a);
  a.click();
  a.remove();
}

async function duplicateFile(path, name) {
  const ext = name.includes('.') ? '.' + name.split('.').pop() : '';
  const base = name.includes('.') ? name.substring(0, name.lastIndexOf('.')) : name;
  const newName = base + '-copy' + ext;
  const dir = path.includes('/') ? path.substring(0, path.lastIndexOf('/') + 1) : '';
  try {
    const content = await api('GET', 'file?path=' + encodeURIComponent(path));
    await api('POST', 'file/create', { cfPath: dir + newName, cfContent: content, cfIsDir: false });
    refreshFiles();
    setStatus('Duplicated as ' + newName, '');
  } catch (e) {
    setStatus('Duplicate failed: ' + e.message, 'error');
  }
}

async function promptNewFileIn(dirPath) {
  const name = await askInput({
    title: 'New file',
    message: dirPath ? `Inside "${dirPath}/"` : null,
    label: 'Filename',
    placeholder: 'example.md',
    confirmLabel: 'Create',
  });
  if (!name) return;
  createNewItem('file', (dirPath ? dirPath + '/' : '') + name);
}

async function promptNewFolderIn(dirPath) {
  const name = await askInput({
    title: 'New folder',
    message: dirPath ? `Inside "${dirPath}/"` : null,
    label: 'Folder name',
    placeholder: 'data',
    confirmLabel: 'Create',
  });
  if (!name) return;
  createNewItem('folder', (dirPath ? dirPath + '/' : '') + name);
}
