// ── Keyboard shortcuts ───────────────────────────────────────────
document.addEventListener('keydown', (e) => {
  const mod = e.ctrlKey || e.metaKey;
  if (mod && e.key === 'k' && !e.shiftKey) {
    e.preventDefault();
    openPalette();
    return;
  }
  if (mod && e.key === 'b') {
    e.preventDefault();
    toggleSidebar();
    return;
  }
  if (mod && e.shiftKey && (e.key === 'A' || e.key === 'a')) {
    e.preventDefault();
    openInsertMenuAtFocused();
    return;
  }
  if (mod && e.shiftKey && (e.key === 'D' || e.key === 'd')) {
    e.preventDefault();
    openDesigner(focusedCellId());
    return;
  }
  if (mod && e.shiftKey && e.key === 'Enter') {
    e.preventDefault();
    runAll();
    return;
  }
  if (mod && e.key === 's') {
    if (!document.querySelector('.CodeMirror-focused')) {
      e.preventDefault();
      saveNotebook();
    }
    return;
  }
  // ⌘E examples, ⌘J chat — only fire when focus is not in a text input/CodeMirror
  const inField =
    document.activeElement &&
    (document.activeElement.tagName === 'INPUT' ||
      document.activeElement.tagName === 'TEXTAREA' ||
      document.querySelector('.CodeMirror-focused'));
  if (mod && !e.shiftKey && !inField && e.key === 'e') {
    e.preventDefault();
    togglePanel('examples');
    return;
  }
  if (mod && !e.shiftKey && !inField && e.key === 'j') {
    e.preventDefault();
    togglePanel('chat');
    return;
  }
});
