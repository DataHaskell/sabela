// Theme bootstrap — follows the editor's scheme ('sabela-theme' key): the
// default scheme is the light deck, dark is dark; unset follows the OS.
(() => {
  const MAP = { default: 'light', summer: 'light', dark: 'dark', fall: 'dark', light: 'light' };
  const stored = localStorage.getItem('sabela-theme');
  const theme =
    MAP[stored] ||
    (window.matchMedia && window.matchMedia('(prefers-color-scheme: light)').matches
      ? 'light'
      : 'dark');
  document.documentElement.dataset.theme = theme;
  window.__sabelaThemePinned = !!MAP[stored];
})();
