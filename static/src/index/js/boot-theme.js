// Theme bootstrap — runs before any rendering to avoid FOUC.
// 'default' is the light scheme the editor ships with; older stored ids
// ('summer', 'fall') fold into the two current schemes.
(() => {
  const MAP = { default: 'default', dark: 'dark', summer: 'default', fall: 'dark' };
  const stored = localStorage.getItem('sabela-theme');
  const theme = MAP[stored] || 'default';
  document.documentElement.dataset.theme = theme;
  window.__sabelaThemePinned = !!MAP[stored];
})();
