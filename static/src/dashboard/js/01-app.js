const isStatic = !!window.__SABELA_STATIC__;
const _query = new URLSearchParams(location.search);
// 'notebook' (tutorial) mode shows each code cell's source; 'dashboard'
// (default) shows only prose + outputs. Static shares pin the mode via the
// injected flag; the live editor can request notebook mode with ?mode=notebook.
const renderMode =
  window.__SABELA_RENDER_MODE__ || (_query.get('mode') === 'notebook' ? 'notebook' : 'dashboard');
// ?print=1 — open straight into the browser print dialog (used by the editor's
// "Save as PDF" menu items, which open this page in a new tab).
const autoPrint = _query.get('print') === '1';
const HLJS_LANG = { Haskell: 'haskell', Python: 'python' };
let notebookData = null;

// ── Theme handling (the same picker as the editor; 'sabela-theme' key) ──
const THEMES = [
  { id: 'warm', name: 'Warm Paper', mode: 'light', bg: '#f7f3ec', accent: '#c2674a' },
  { id: 'nord', name: 'Nord', mode: 'dark', bg: '#2e3440', accent: '#88c0d0' },
  { id: 'nord-light', name: 'Nord Light', mode: 'light', bg: '#eceff4', accent: '#5e81ac' },
  { id: 'nord-aurora', name: 'Aurora Purple', mode: 'dark', bg: '#2e3440', accent: '#b48ead' },
  { id: 'nord-ember', name: 'Aurora Ember', mode: 'dark', bg: '#2e3440', accent: '#d08770' },
  { id: 'vscode', name: 'VS Code Dark', mode: 'dark', bg: '#1e1e1e', accent: '#3794ff' },
];
function currentTheme() {
  const t = document.documentElement.dataset.theme;
  return THEMES.some((x) => x.id === t) ? t : 'warm';
}
function cssVar(name, fallback) {
  return (getComputedStyle(document.documentElement).getPropertyValue(name) || fallback).trim();
}
function applyTheme(theme, { persist = true } = {}) {
  document.documentElement.dataset.theme = theme;
  if (persist) {
    localStorage.setItem('sabela-theme', theme);
    window.__sabelaThemePinned = true;
  }
  // Re-render iframes with the current theme colors.
  document.querySelectorAll('iframe[data-last-content]').forEach((f) => {
    if (!f.dataset.lastContent) return;
    if (f.dataset.staticFrame) {
      f.srcdoc = staticSrcdoc(f.dataset.lastContent, f.dataset.iframeKind || 'content', f.id);
      return;
    }
    try {
      const doc = f.contentDocument || f.contentWindow?.document;
      if (!doc) return;
      doc.open();
      const style = f.dataset.iframeKind === 'widget' ? widgetIframeStyle() : iframeContentStyle();
      doc.write(style + f.dataset.lastContent);
      doc.close();
    } catch {}
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
// The header button's onclick is toggleTheme(); keep the name, open the picker.
function toggleTheme() {
  openThemePicker();
}
function iframeContentStyle() {
  const bg = cssVar('--bg-card', '#ffffff');
  const fg = cssVar('--fg', '#0a0a0a');
  const fgHead = cssVar('--fg-heading', '#000000');
  const muted = cssVar('--bg-muted', '#fafafa');
  const border = cssVar('--border', '#e4e4e7');
  const accent = cssVar('--accent', '#0066ff');
  const altRow = `rgba(${cssVar('--accent-rgb', '0,102,255')},0.05)`;
  return `<style>body{background:${bg};color:${fg};font-family:'Geist',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;font-size:15px;margin:0;padding:12px 16px;line-height:1.6;-webkit-font-smoothing:antialiased}img{max-width:100%}a{color:${accent}}table{border-collapse:separate;border-spacing:0;width:100%;border:1px solid ${border};border-radius:8px;overflow:hidden;margin:14px 0;font-size:14px}th,td{padding:10px 14px;text-align:left;border-bottom:1px solid ${border}}tr:last-child td{border-bottom:none}th{background:${muted};color:${fgHead};font-weight:600}tr:nth-child(even) td{background:${altRow}}input,select,button{font-family:inherit;font-size:14px;accent-color:${accent}}</style>`;
}
function widgetIframeStyle() {
  const fg = cssVar('--fg', '#0a0a0a');
  const fgDim = cssVar('--fg-dim', '#71717a');
  const accent = cssVar('--accent', '#0066ff');
  return `<style>body{background:transparent;color:${fg};font-family:'JetBrains Mono',ui-monospace,monospace;font-size:14px;margin:0;padding:6px 12px;line-height:1.2;font-variant-ligatures:none;font-feature-settings:"calt" 0,"liga" 0}input,select{font-family:inherit;font-size:14px;accent-color:${accent};vertical-align:middle}button{font-family:inherit;font-size:14px;cursor:pointer;accent-color:${accent};vertical-align:middle}label{font-weight:600;font-size:13px;color:${fgDim};display:inline-block;margin-right:8px;vertical-align:middle}</style>`;
}
