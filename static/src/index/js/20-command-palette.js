// ── Command palette ──────────────────────────────────────────────
const PALETTE_COMMANDS = [
  {
    id: 'save',
    label: 'Save notebook',
    icon: 'i-download',
    hint: kbd('mod', 's'),
    run: () => saveNotebook(),
  },
  {
    id: 'download',
    label: 'Download as .md',
    short: 'Markdown (.md)',
    icon: 'i-download',
    run: () => downloadMarkdown(),
  },
  {
    id: 'export-hs',
    label: 'Export pipeline as .hs (cabal script)',
    short: 'Cabal script (.hs)',
    icon: 'i-download',
    run: () => exportPipeline(null, 'haskell'),
  },
  {
    id: 'export-lhs',
    label: 'Export pipeline as .lhs (literate Haskell)',
    short: 'Literate Haskell (.lhs)',
    icon: 'i-download',
    run: () => exportPipeline(null, 'lhs'),
  },
  {
    id: 'export-reactive',
    label: 'Export as reactive-banana app (headless)',
    short: 'reactive-banana app',
    icon: 'i-download',
    run: () => exportPipeline(null, 'reactive'),
  },
  {
    id: 'pdf-dashboard',
    label: 'Save dashboard as PDF (prose + outputs)',
    short: 'Dashboard PDF',
    icon: 'i-download',
    run: () => window.open('/dashboard?print=1', '_blank'),
  },
  {
    id: 'pdf-notebook',
    label: 'Save notebook as PDF (prose + code + outputs)',
    short: 'Notebook PDF',
    icon: 'i-download',
    run: () => window.open('/dashboard?mode=notebook&print=1', '_blank'),
  },
  {
    id: 'dashboard',
    label: 'View as dashboard',
    short: 'Dashboard',
    icon: 'i-eye',
    run: () => window.open('/dashboard', '_blank'),
  },
  {
    id: 'slideshow',
    label: 'View as slideshow',
    short: 'Slideshow',
    icon: 'i-book-open',
    run: () => window.open('/slideshow', '_blank'),
  },
  {
    id: 'publish-dashboard',
    label: 'Publish dashboard to a public URL',
    short: 'Publish dashboard',
    icon: 'i-copy',
    run: () => publishShare('dashboard'),
  },
  {
    id: 'publish-slideshow',
    label: 'Publish slideshow to a public URL',
    short: 'Publish slideshow',
    icon: 'i-copy',
    run: () => publishShare('slideshow'),
  },
  {
    id: 'publish-notebook',
    label: 'Publish as static notebook (tutorial)',
    short: 'Publish notebook',
    icon: 'i-copy',
    run: () => publishShare('notebook'),
  },
  {
    id: 'manage-shares',
    label: 'Manage public shares…',
    short: 'Manage shares',
    icon: 'i-copy',
    run: () => openSharesModal(),
  },
  { id: 'interrupt', label: 'Stop running cell', icon: 'i-square', run: () => interruptKernel() },
  { id: 'restart', label: 'Restart kernel', icon: 'i-rotate', run: () => restartKernel() },
  { id: 'reset', label: 'Reset notebook', icon: 'i-zap', run: () => resetNotebook() },
  {
    id: 'run-all',
    label: 'Run all cells',
    icon: 'i-play',
    hint: kbd('mod', 'shift', 'enter'),
    run: () => runAll(),
  },
  {
    id: 'sidebar',
    label: 'Toggle file sidebar',
    icon: 'i-menu',
    hint: kbd('mod', 'b'),
    run: () => toggleSidebar(),
  },
  {
    id: 'panel-info',
    label: 'Toggle info / lookup',
    icon: 'i-search',
    hint: kbd('mod', 'i'),
    run: () => togglePanel('info'),
  },
  {
    id: 'panel-examples',
    label: 'Toggle examples panel',
    icon: 'i-book-open',
    hint: kbd('mod', 'e'),
    run: () => togglePanel('examples'),
  },
  {
    id: 'panel-chat',
    label: 'Toggle AI chat',
    icon: 'i-message',
    hint: kbd('mod', 'j'),
    run: () => togglePanel('chat'),
  },
  { id: 'theme', label: 'Choose theme…', icon: 'i-sun', run: () => openThemePicker() },
  { id: 'ai-settings', label: 'AI settings…', icon: 'i-settings', run: () => openAIModal() },
];
// Structural layout for the overflow ("More") dropdown only. Entries are:
//   - a string  → command id (leaf), resolved against PALETTE_COMMANDS
//   - '---'     → separator
//   - { label, icon?, items: [ids] } → submenu group (hover flyout)
// The command palette stays FLAT and ignores this tree.
const OVERFLOW_MENU = [
  'save',
  '---',
  {
    label: 'Export',
    icon: 'i-download',
    items: [
      'download',
      'pdf-dashboard',
      'pdf-notebook',
      'export-hs',
      'export-lhs',
      'export-reactive',
    ],
  },
  { label: 'View as', icon: 'i-eye', items: ['dashboard', 'slideshow'] },
  {
    label: 'Share',
    icon: 'i-copy',
    items: ['publish-dashboard', 'publish-slideshow', 'publish-notebook', 'manage-shares'],
  },
  '---',
  'run-all',
  'restart',
  'reset',
  '---',
  'sidebar',
  'panel-info',
  'panel-examples',
  'panel-chat',
  '---',
  'theme',
  'ai-settings',
];
const _cmdById = (id) => PALETTE_COMMANDS.find((c) => c.id === id);
let _paletteFiltered = PALETTE_COMMANDS.slice();
let _paletteActive = 0;

function openPalette() {
  const overlay = document.getElementById('palette-overlay');
  const input = document.getElementById('palette-input');
  input.value = '';
  _paletteFiltered = PALETTE_COMMANDS.slice();
  _paletteActive = 0;
  renderPaletteList();
  overlay.classList.add('show');
  setTimeout(() => input.focus(), 20);
}
function closePalette() {
  document.getElementById('palette-overlay').classList.remove('show');
}
function paletteFilter(q) {
  q = q.trim().toLowerCase();
  if (!q) return PALETTE_COMMANDS.slice();
  const tokens = q.split(/\s+/);
  return PALETTE_COMMANDS.filter((c) => {
    const hay = c.label.toLowerCase();
    return tokens.every((t) => hay.includes(t));
  });
}
function renderPaletteList() {
  const list = document.getElementById('palette-list');
  if (_paletteFiltered.length === 0) {
    list.innerHTML = '<div class="palette-empty">No matching commands</div>';
    return;
  }
  list.innerHTML = _paletteFiltered
    .map(
      (c, i) =>
        `<div class="palette-item${i === _paletteActive ? ' active' : ''}" data-idx="${i}" role="option">
      <svg class="icon-svg"><use href="#${c.icon}"/></svg>
      <span class="label">${c.label}</span>
      ${c.hint ? `<span class="hint">${c.hint}</span>` : ''}
    </div>`
    )
    .join('');
  list.querySelectorAll('.palette-item').forEach((el) => {
    el.addEventListener('mouseenter', () => {
      _paletteActive = parseInt(el.dataset.idx);
      list
        .querySelectorAll('.palette-item')
        .forEach((e, i) => e.classList.toggle('active', i === _paletteActive));
    });
    el.addEventListener('click', () => runPaletteActive());
  });
  // Scroll active into view
  const active = list.querySelector('.palette-item.active');
  if (active) active.scrollIntoView({ block: 'nearest' });
}
function runPaletteActive() {
  const cmd = _paletteFiltered[_paletteActive];
  if (!cmd) return;
  closePalette();
  try {
    cmd.run();
  } catch (e) {
    console.error(e);
  }
}
document.addEventListener('DOMContentLoaded', () => {
  // Platform-aware tooltips and visible shortcut hints
  const setTitle = (id, text) => {
    const el = document.getElementById(id);
    if (el) el.title = text;
  };
  setTitle('btn-sidebar-toggle', `Toggle file explorer (${kbd('mod', 'b')})`);
  setTitle('btn-run-all', `Run all cells (${kbd('mod', 'shift', 'enter')})`);
  setTitle('btn-palette', `Command palette (${kbd('mod', 'k')})`);
  const kbdEl = document.getElementById('kbd-palette');
  if (kbdEl) kbdEl.textContent = kbd('mod', 'k');
  const kbdLookup = document.getElementById('kbd-lookup');
  if (kbdLookup) kbdLookup.textContent = kbd('mod', 'i');

  const input = document.getElementById('palette-input');
  if (!input) return;
  input.addEventListener('input', () => {
    _paletteFiltered = paletteFilter(input.value);
    _paletteActive = 0;
    renderPaletteList();
  });
  input.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      closePalette();
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      _paletteActive = Math.min(_paletteFiltered.length - 1, _paletteActive + 1);
      renderPaletteList();
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      _paletteActive = Math.max(0, _paletteActive - 1);
      renderPaletteList();
    } else if (e.key === 'Enter') {
      e.preventDefault();
      runPaletteActive();
    }
  });
  // Build the overflow menu from the OVERFLOW_MENU tree (grouped, with submenus).
  const ofList = document.getElementById('overflow-menu-list');
  if (ofList) {
    renderOverflowMenu();
    ofList.addEventListener('click', (e) => {
      const leaf = e.target.closest('.dd-item[data-cmd]');
      if (!leaf) return; // submenu parent / separator → no-op, keep menu open
      closeDropdowns();
      const cmd = _cmdById(leaf.dataset.cmd);
      if (cmd)
        try {
          cmd.run();
        } catch (err) {
          console.error(err);
        }
    });
  }
});
