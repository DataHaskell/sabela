// Theme bootstrap — shared with the editor via 'sabela-theme' localStorage key.
(() => {
  const KNOWN = ['warm', 'nord', 'nord-light', 'nord-aurora', 'nord-ember', 'vscode'];
  const stored = localStorage.getItem('sabela-theme');
  document.documentElement.dataset.theme = KNOWN.includes(stored) ? stored : 'warm';
  window.__sabelaThemePinned = !!stored;
})();
