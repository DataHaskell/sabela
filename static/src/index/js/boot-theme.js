// Theme bootstrap — runs before any rendering to avoid FOUC.
// 'default' is the dark scheme the editor ships with.
(() => {
  const KNOWN = ['default', 'summer', 'fall'];
  const stored = localStorage.getItem('sabela-theme');
  const theme = KNOWN.includes(stored) ? stored : 'default';
  document.documentElement.dataset.theme = theme;
  window.__sabelaThemePinned = KNOWN.includes(stored);
})();
