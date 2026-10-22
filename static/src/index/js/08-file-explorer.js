// ── File explorer ────────────────────────────────────────────────
// Which folders are open in the explorer. Persisted to localStorage so the
// tree comes back the way it was left after a reload. '' is the root.
const EXPANDED_DIRS_KEY = 'sabela:expandedDirs';
function loadExpandedDirs() {
  try {
    const raw = localStorage.getItem(EXPANDED_DIRS_KEY);
    if (raw) return new Set(['', ...JSON.parse(raw)]);
  } catch (e) {}
  return new Set(['']);
}
function persistExpandedDirs() {
  try {
    localStorage.setItem(
      EXPANDED_DIRS_KEY,
      JSON.stringify([...expandedDirs].filter((p) => p !== ''))
    );
  } catch (e) {}
}
const expandedDirs = loadExpandedDirs();

function collapseAllDirs() {
  expandedDirs.clear();
  expandedDirs.add('');
  persistExpandedDirs();
  refreshFiles();
}

// Live-filter the rendered tree by filename. Limitation: only the
// currently loaded (expanded) rows are filtered; collapsed folders aren't
// fetched just to search them.
function applyFileFilter() {
  const q = (document.getElementById('file-filter').value || '').trim().toLowerCase();
  const rows = document.getElementById('file-tree').querySelectorAll('.file-entry');
  rows.forEach((row) => {
    const name = (row.dataset.name || '').toLowerCase();
    let show = !q || name.includes(q);
    if (q && !show && row.dataset.isDir === '1') {
      const child = row.nextElementSibling;
      show = !!(
        child &&
        Array.from(child.querySelectorAll('.file-entry')).some((r) =>
          (r.dataset.name || '').toLowerCase().includes(q)
        )
      );
    }
    row.style.display = show ? '' : 'none';
  });
}

// A folder row's glyph pair: the expand chevron, then the folder.
function treeGlyph(open) {
  const chev = open ? 'i-chev-down' : 'i-chev-right';
  return `<svg class="icon-svg chev"><use href="#${chev}"/></svg><svg class="icon-svg"><use href="#i-folder"/></svg>`;
}

// The sidebar folds to nothing and remembers the choice; the top-bar button
// reflects it.
function toggleSidebar(show) {
  const sb = document.getElementById('sidebar');
  const collapsed = show == null ? !sb.classList.contains('collapsed') : !show;
  sb.classList.toggle('collapsed', collapsed);
  const btn = document.getElementById('btn-sidebar-toggle');
  if (btn) btn.setAttribute('aria-pressed', collapsed ? 'false' : 'true');
  try {
    localStorage.setItem('sabela-sidebar', collapsed ? 'closed' : 'open');
  } catch (_e) {}
}

async function loadFileTree(path, container, depth) {
  try {
    const entries = await api('GET', 'files?path=' + encodeURIComponent(path || '.'));
    container.innerHTML = '';
    for (const entry of entries) {
      // Skip hidden files
      if (entry.feName.startsWith('.')) continue;
      const row = document.createElement('div');
      row.className = 'file-entry';
      row.dataset.path = entry.fePath;
      row.dataset.name = entry.feName;
      row.dataset.isDir = entry.feIsDir ? '1' : '';
      if (notebook && notebook.nbTitle === entry.fePath) row.classList.add('active');
      if (!entry.feIsDir) makeFileDraggable(row, entry.fePath);

      const indent = document.createElement('span');
      indent.className = 'file-indent';
      indent.style.width = depth * 14 + 'px';
      row.appendChild(indent);

      const icon = document.createElement('span');
      icon.className = 'icon' + (entry.feIsDir ? ' dir' : ' file');
      icon.innerHTML = entry.feIsDir
        ? treeGlyph(expandedDirs.has(entry.fePath))
        : '<svg class="icon-svg"><use href="#i-file-text"/></svg>';
      row.appendChild(icon);

      const name = document.createElement('span');
      name.className = 'name';
      name.textContent = entry.feName;
      row.appendChild(name);

      if (entry.feIsDir) {
        const childContainer = document.createElement('div');
        row.onclick = async (e) => {
          e.stopPropagation();
          if (expandedDirs.has(entry.fePath)) {
            expandedDirs.delete(entry.fePath);
            childContainer.innerHTML = '';
            icon.innerHTML = treeGlyph(false);
          } else {
            expandedDirs.add(entry.fePath);
            icon.innerHTML = treeGlyph(true);
            await loadFileTree(entry.fePath, childContainer, depth + 1);
          }
          persistExpandedDirs();
        };
        container.appendChild(row);
        container.appendChild(childContainer);
        if (expandedDirs.has(entry.fePath)) loadFileTree(entry.fePath, childContainer, depth + 1);
      } else {
        row.onclick = () => openFile(entry);
        container.appendChild(row);
      }
    }
    applyFileFilter();
  } catch (e) {
    container.textContent = 'Error loading files';
    console.error(e);
  }
}

async function openFile(entry) {
  const ext = entry.feName.split('.').pop();
  if (ext === 'md') {
    setStatus('Loading ' + entry.fePath + '...', 'running');
    activePreviewCm = null;
    try {
      const nb = await api('POST', 'load', { lrPath: entry.fePath });
      Object.keys(editors).forEach((k) => delete editors[k]);
      render(nb);
      setStatus('Loaded ' + entry.fePath, '');
      refreshFiles();
    } catch (e) {
      setStatus('Load failed: ' + e.message, 'error');
    }
  } else {
    showFilePreview(entry.feName, entry.fePath);
  }
}

// The active non-notebook editor, tracked so periodic autosave can flush
// it. Only set once the file is fully loaded and therefore editable.
