// Theme bootstrap — shared with the editor via 'sabela-theme' localStorage key.
(() => {
  const KNOWN = ['default', 'summer', 'fall'];
  const stored = localStorage.getItem('sabela-theme');
  document.documentElement.dataset.theme = KNOWN.includes(stored) ? stored : 'default';
  window.__sabelaThemePinned = !!stored;
  if (window.self !== window.top) document.documentElement.classList.add('framed');
})();
