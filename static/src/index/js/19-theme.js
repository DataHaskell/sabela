// ── Theme handling ───────────────────────────────────────────────
const THEMES = [
  { id: 'default', name: 'Light', mode: 'light', bg: '#f9fbfc', accent: '#509ee3' },
  { id: 'dark', name: 'Dark', mode: 'dark', bg: '#2e353b', accent: '#6cb0ec' },
];
function currentTheme() {
  const t = document.documentElement.dataset.theme;
  return THEMES.some((x) => x.id === t) ? t : 'default';
}
function themeMode(id) {
  return (THEMES.find((x) => x.id === id) || THEMES[0]).mode;
}
// One CodeMirror theme serves every scheme: its colours are CSS variables
// that the active [data-theme] block supplies.
function cmTheme() {
  return 'sabela';
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
// The vendored face, so an output frame types like the notebook around it.
const IFRAME_FONT_FACE =
  "@import url('https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap');" +
  "@font-face{font-family:'Nordwand Mono';font-weight:400;font-display:swap;src:url('/vendor/fonts/NordwandMono-Regular.woff2') format('woff2')}" +
  "@font-face{font-family:'Nordwand Mono';font-weight:500;font-display:swap;src:url('/vendor/fonts/NordwandMono-Medium.woff2') format('woff2')}";
function iframeBaseStyle() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => (cs.getPropertyValue(n) || d).trim();
  const bg = v('--output-html-bg', '#ffffff');
  const fg = v('--output-html-fg', '#1e1e2e');
  const accent = v('--accent', '#89b4fa');
  return `<style>${IFRAME_FONT_FACE}:root{${notebookPaletteVars(v, bg, fg, accent)}}body{background:${bg};color:${fg};font-family:'Nordwand Mono','JetBrains Mono',ui-monospace,monospace;font-size:13px;margin:0;padding:4px 8px;font-variant-ligatures:none;font-feature-settings:"calt" 0,"liga" 0}input,select,button{accent-color:${accent};font-family:inherit;font-size:13px;cursor:pointer}</style>`;
}

// The notebook's own palette, handed to an output iframe so a rendered widget
// can look like part of the notebook rather than a white card. The canvas stays
// --output-html-bg: only a widget that opts in (the grid) paints over it.
function notebookPaletteVars(v, bg, fg, accent) {
  return [
    `--sabela-surface:${v('--bg-output', bg)}`,
    `--sabela-raised:${v('--bg-cell', bg)}`,
    `--sabela-fg:${v('--fg', fg)}`,
    `--sabela-dim:${v('--fg-dim', fg)}`,
    `--sabela-border:${v('--border', '#e2e2ea')}`,
    `--sabela-accent:${accent}`,
    `--sabela-accent-rgb:${v('--accent-rgb', '96,120,240')}`,
    '--sabela-live:1',
  ].join(';');
}
// Initial sync (CodeMirror theme will be set once editors mount).
applyTheme(currentTheme(), { persist: false });

// ── Modified indicator: the tab's dot and the status bar's file icon ──
function updateDirtyDot() {
  const dot = document.getElementById('dirty-dot');
  if (dot) dot.classList.toggle('show', !!unsavedChanges);
  const sb = document.getElementById('sb-file');
  if (sb) sb.classList.toggle('dirty', !!unsavedChanges);
}
setInterval(updateDirtyDot, 600);
