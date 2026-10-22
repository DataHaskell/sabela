// Theme bootstrap — shared with the editor via 'sabela-theme' localStorage key.
(() => {
  const MAP = { default: 'default', dark: 'dark', summer: 'default', fall: 'dark' };
  const stored = localStorage.getItem('sabela-theme');
  document.documentElement.dataset.theme = MAP[stored] || 'default';
  window.__sabelaThemePinned = !!stored;
  if (window.self !== window.top) document.documentElement.classList.add('framed');
})();
