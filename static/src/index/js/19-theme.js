// ── Theme handling ───────────────────────────────────────────────
const THEMES = [
  { id: 'gzim', name: 'Great Zimbabwe', mode: 'dark', bg: '#1b1a17', accent: '#d69a3c' },
  { id: 'warm', name: 'Warm Paper', mode: 'light', bg: '#f7f3ec', accent: '#c2674a' },
  { id: 'nord', name: 'Nord', mode: 'dark', bg: '#2e3440', accent: '#88c0d0' },
  { id: 'nord-light', name: 'Nord Light', mode: 'light', bg: '#eceff4', accent: '#5e81ac' },
  { id: 'nord-aurora', name: 'Aurora Purple', mode: 'dark', bg: '#2e3440', accent: '#b48ead' },
  { id: 'nord-ember', name: 'Aurora Ember', mode: 'dark', bg: '#2e3440', accent: '#d08770' },
  {
    id: 'vscode',
    name: 'VS Code Dark',
    mode: 'dark',
    bg: '#1e1e1e',
    accent: '#3794ff',
    cm: 'vscode',
  },
];
function currentTheme() {
  const t = document.documentElement.dataset.theme;
  return THEMES.some((x) => x.id === t) ? t : 'gzim';
}
function themeMode(id) {
  return (THEMES.find((x) => x.id === id) || THEMES[0]).mode;
}
// CodeMirror theme name for the active palette (used by editors on mount too).
function cmTheme() {
  const t = THEMES.find((x) => x.id === currentTheme());
  if (t && t.cm) return t.cm;
  return themeMode(currentTheme()) === 'light' ? 'idea' : 'nord';
}
function applyTheme(theme, { persist = true } = {}) {
  document.documentElement.dataset.theme = theme;
  if (persist) {
    localStorage.setItem('sabela-theme', theme);
    window.__sabelaThemePinned = true;
  }
  const cm = cmTheme();
  for (const id in editors) {
    try {
      editors[id].setOption('theme', cm);
    } catch {}
  }
  document.querySelectorAll('.cell-output.mime-html iframe').forEach((f) => {
    if (f.dataset.lastContent) {
      try {
        const doc = f.contentDocument || f.contentWindow?.document;
        if (doc) {
          doc.open();
          doc.write(iframeBaseStyle() + f.dataset.lastContent);
          doc.close();
        }
      } catch {}
    }
  });
  markActiveTheme();
}
function markActiveTheme() {
  const cur = currentTheme();
  document
    .querySelectorAll('.theme-menu-item')
    .forEach((el) => el.classList.toggle('active', el.dataset.theme === cur));
}
function buildThemeMenu() {
  let menu = document.getElementById('theme-menu');
  if (menu) return menu;
  menu = document.createElement('div');
  menu.id = 'theme-menu';
  menu.className = 'theme-menu';
  THEMES.forEach((t) => {
    const item = document.createElement('button');
    item.className = 'theme-menu-item';
    item.dataset.theme = t.id;
    item.innerHTML =
      `<span class="sw" style="background:${t.bg}"><i style="background:${t.accent}"></i></span>` +
      `<span class="nm">${t.name}</span>`;
    item.onclick = () => {
      applyTheme(t.id);
      menu.classList.remove('open');
    };
    menu.appendChild(item);
  });
  document.body.appendChild(menu);
  return menu;
}
function openThemePicker() {
  const menu = buildThemeMenu();
  const btn = document.getElementById('theme-toggle');
  if (btn) {
    const r = btn.getBoundingClientRect();
    menu.style.top = r.bottom + 6 + 'px';
    menu.style.right = window.innerWidth - r.right + 'px';
  }
  const open = menu.classList.toggle('open');
  markActiveTheme();
  if (open) {
    setTimeout(() => {
      document.addEventListener(
        'click',
        (e) => {
          if (!menu.contains(e.target) && e.target !== btn && !btn?.contains(e.target))
            menu.classList.remove('open');
        },
        { once: true }
      );
    }, 0);
  }
}
// Kept for the command palette + any keybinding: open the palette picker.
function toggleTheme() {
  openThemePicker();
}
function iframeBaseStyle() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => (cs.getPropertyValue(n) || d).trim();
  const bg = v('--output-html-bg', '#ffffff');
  const fg = v('--output-html-fg', '#1e1e2e');
  const accent = v('--accent', '#89b4fa');
  return `<style>body{background:${bg};color:${fg};font-family:'JetBrains Mono',ui-monospace,monospace;font-size:13px;margin:0;padding:4px 8px;font-variant-ligatures:none;font-feature-settings:"calt" 0,"liga" 0}input,select,button{accent-color:${accent};font-family:inherit;font-size:13px;cursor:pointer}</style>`;
}
// Initial sync (CodeMirror theme will be set once editors mount).
applyTheme(currentTheme(), { persist: false });

// ── Dirty-dot indicator (filename next to the toolbar title) ─────
function updateDirtyDot() {
  const dot = document.getElementById('dirty-dot');
  if (!dot) return;
  if (unsavedChanges) dot.classList.add('show');
  else dot.classList.remove('show');
}
setInterval(updateDirtyDot, 600);
