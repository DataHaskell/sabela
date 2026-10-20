// ── File context menu ───────────────────────────────────────────
let activeCtxMenu = null;

function showFileContextMenu(e) {
  e.preventDefault();
  closeCtxMenu();
  const row = e.target.closest('.file-entry');
  if (!row) return;
  const path = row.dataset.path;
  const name = row.dataset.name;
  const isDir = row.dataset.isDir === '1';

  const menu = document.createElement('div');
  menu.className = 'ctx-menu';

  if (!isDir) {
    addCtxItem(menu, 'Open', () => openFile({ feName: name, fePath: path, feIsDir: false }));
  }
  addCtxItem(menu, 'Copy relative path', () => copyRelPath(path));
  if (!isDir) {
    if (name.endsWith('.md') && notebook && notebook.nbTitle === path) {
      addCtxItem(menu, 'Download .md', () => downloadMarkdown());
    } else {
      addCtxItem(menu, 'Download', () => downloadAsset(path, name));
    }
  }
  addCtxSep(menu);
  addCtxItem(menu, 'Rename', () => promptRenameFile(path, name));
  addCtxItem(menu, 'Move to folder…', () => promptMoveFile(path, name));
  if (!isDir) {
    addCtxItem(menu, 'Duplicate', () => duplicateFile(path, name));
  }
  addCtxSep(menu);
  if (isDir) {
    addCtxItem(menu, 'New file here', () => promptNewFileIn(path));
    addCtxItem(menu, 'New folder here', () => promptNewFolderIn(path));
    addCtxItem(menu, 'Upload files here', () => startUpload(path));
    addCtxItem(menu, 'Import from URL here', () => startImportUrl(path));
    addCtxSep(menu);
  }
  addCtxItem(menu, 'Delete', () => confirmDeleteFile(path, name, isDir));

  // Position
  menu.style.left = Math.min(e.clientX, window.innerWidth - 220) + 'px';
  menu.style.top = Math.min(e.clientY, window.innerHeight - 240) + 'px';
  document.body.appendChild(menu);
  activeCtxMenu = menu;
}

function addCtxItem(menu, label, action, active) {
  const item = document.createElement('div');
  item.className = 'ctx-menu-item' + (active ? ' active' : '');
  item.tabIndex = 0;
  item.innerHTML = `<span class="nm">${label}</span>`;
  item.onclick = () => {
    closeCtxMenu();
    action();
  };
  item.onkeydown = (e) => {
    if (e.key === 'Enter') {
      closeCtxMenu();
      action();
    }
  };
  menu.appendChild(item);
}

function addCtxSep(menu) {
  const sep = document.createElement('div');
  sep.className = 'ctx-menu-sep';
  menu.appendChild(sep);
}

function closeCtxMenu() {
  if (activeCtxMenu) {
    activeCtxMenu.remove();
    activeCtxMenu = null;
  }
}

// The cell footer's language label opens a short menu above itself.
var CELL_LANGS = ['Haskell', 'Python'];
function showLangMenu(e, cellId, current) {
  e.preventDefault();
  e.stopPropagation();
  closeCtxMenu();
  const menu = document.createElement('div');
  menu.className = 'ctx-menu lang-menu';
  CELL_LANGS.forEach((lang) => {
    addCtxItem(menu, lang, () => lang !== current && setCellLang(cellId, lang), lang === current);
  });
  document.body.appendChild(menu);
  const r = e.currentTarget.getBoundingClientRect();
  menu.style.left = Math.max(8, r.right - menu.offsetWidth) + 'px';
  menu.style.top = Math.max(8, r.top - menu.offsetHeight - 4) + 'px';
  activeCtxMenu = menu;
}

document.addEventListener('click', closeCtxMenu);
document.addEventListener('contextmenu', (e) => {
  if (!e.target.closest('.file-tree')) closeCtxMenu();
});

// Attach context menu to file tree
document.getElementById('file-tree').addEventListener('contextmenu', showFileContextMenu);
