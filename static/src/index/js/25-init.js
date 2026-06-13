// ── Init ─────────────────────────────────────────────────────────
(async () => {
  connectSSE();
  refreshFiles();
  loadExamples();
  togglePanel('examples');
  try {
    const nb = await api('GET', 'notebook');
    render(nb);
  } catch (e) {
    setStatus('Could not connect to server', 'error');
  }
})();
