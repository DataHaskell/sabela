// Theme bootstrap — follows the editor's scheme ('sabela-theme' key): Summer
// is the light deck, the other schemes are dark; unset follows the OS.
(() => {
  const MAP = { summer: 'light', default: 'dark', fall: 'dark', light: 'light', dark: 'dark' };
  const stored = localStorage.getItem('sabela-theme');
  const theme =
    MAP[stored] ||
    (window.matchMedia && window.matchMedia('(prefers-color-scheme: light)').matches
      ? 'light'
      : 'dark');
  document.documentElement.dataset.theme = theme;
  window.__sabelaThemePinned = !!MAP[stored];
})();
