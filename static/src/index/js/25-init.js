// ── Init ─────────────────────────────────────────────────────────

// Load a notebook by work-dir-relative path, replacing the current one.
async function loadNotebookPath(path) {
  try {
    setStatus('Opening ' + path + '...', 'running');
    const nb = await api('POST', 'load', { lrPath: path });
    Object.keys(editors).forEach((k) => delete editors[k]);
    render(nb);
    setStatus('Opened ' + path, '');
    refreshFiles();
  } catch (e) {
    setStatus('Open failed: ' + e.message, 'error');
  }
}

// Fork hand-off from the gallery. `?open=<file>` opens a just-forked notebook
// (the signed-in path); the `sabela_fork=<slug>` cookie is the post-sign-in
// resume: perform the fork now that we have a session, then open the result.
async function openForkTarget() {
  const openPath = new URLSearchParams(location.search).get('open');
  if (openPath) {
    await loadNotebookPath(openPath);
    history.replaceState(null, '', location.pathname);
    return;
  }
  const m = document.cookie.match(/(?:^|;\s*)sabela_fork=([^;]+)/);
  if (!m) return;
  document.cookie = 'sabela_fork=; Path=/; Max-Age=0; SameSite=Lax';
  try {
    const r = await fetch('/_hub/fork/' + encodeURIComponent(decodeURIComponent(m[1])), {
      method: 'POST',
      headers: { Accept: 'application/json' },
    });
    if (!r.ok) return;
    const data = await r.json();
    if (data && data.notebook) await loadNotebookPath(data.notebook);
  } catch (e) {}
}

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
  await openForkTarget();
})();
installNotebookDropTargets();
installOsFileDrop();
// Wrapped lines keep their height when a side panel opens or closes, since
// CodeMirror only re-measures on a window resize; re-measure on the column's.
if (window.ResizeObserver) {
  let refreshTimer = null;
  new ResizeObserver(() => {
    clearTimeout(refreshTimer);
    refreshTimer = setTimeout(() => {
      for (const id in editors) {
        try {
          editors[id].refresh();
        } catch (_e) {}
      }
    }, 120);
  }).observe(document.getElementById('notebook-area'));
}
try {
  if (localStorage.getItem('sabela-sidebar') === 'closed') toggleSidebar(false);
} catch (_e) {}
