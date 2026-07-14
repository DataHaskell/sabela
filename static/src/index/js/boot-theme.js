// Theme bootstrap — runs before any rendering to avoid FOUC.
// Default palette is the original blue (Original Dark / Light by OS).
(() => {
  const KNOWN = ['gzim', 'warm', 'nord', 'nord-light', 'nord-aurora', 'nord-ember', 'vscode'];
  const stored = localStorage.getItem('sabela-theme');
  let theme;
  if (KNOWN.includes(stored)) {
    theme = stored;
  } else {
    // Default to the Great Zimbabwe palette for everyone.
    theme = 'gzim';
  }
  document.documentElement.dataset.theme = theme;
  window.__sabelaThemePinned = !!stored;
})();
