// ── Command palette behaviour (tables live in 19b-command-list.js) ──
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
