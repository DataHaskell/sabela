// ── Save notebook ────────────────────────────────────────────────
// The save endpoint persists SERVER state, so locally-edited cells must
// be flushed to the server first (non-executing PUT). dirtyCells entries
// are dropped per cell, and only when the editor still holds exactly the
// flushed text — keystrokes landing mid-save keep their draft protection
// against renders.
async function saveNotebook() {
  if (!notebook) return;
  try {
    const flushed = {};
    for (const id of [...dirtyCells]) {
      const cm = editors[id];
      if (!cm) continue;
      const val = cm.getValue();
      await api('PUT', `cell/${id}/source`, { ucSource: val });
      flushed[id] = val;
    }
    const nb = await api('POST', 'save', { srPath: null });
    notebook = nb;
    unsavedChanges = false;
    for (const [id, val] of Object.entries(flushed)) {
      const cm = editors[id];
      if (cm && cm.getValue() === val) dirtyCells.delete(parseInt(id));
    }
    setToolbarTitle(nb.nbTitle);
    flashSaved();
  } catch (e) {
    setStatus('Save failed: ' + e.message, 'error');
  }
}

function downloadMarkdown() {
  window.location.href = '/api/export/markdown';
}

// Export a runnable Haskell pipeline. fmt is 'haskell' (cabal script .hs) or
// 'lhs' (literate). If cellId is given, exports the backward slice ending at
// that cell; otherwise the whole notebook (last code cell).
function exportPipeline(cellId, fmt) {
  const q = cellId != null && cellId >= 0 ? '?cell=' + cellId : '';
  window.location.href = '/api/export/' + fmt + q;
}

setInterval(() => {
  if (unsavedChanges) saveNotebook();
  // Also flush a fully-loaded, edited non-notebook file. Truncated
  // previews stay read-only, so they never reach here. Drop the handle
  // once its editor has been detached (e.g. a notebook was opened).
  if (activePreviewCm) {
    const el = activePreviewCm.getWrapperElement && activePreviewCm.getWrapperElement();
    if (!el || !el.isConnected) {
      activePreviewCm = null;
    } else if (activePreviewCm._editable && activePreviewCm._dirty) {
      saveArbitraryFile(activePreviewCm);
    }
  }
}, AUTOSAVE_INTERVAL_MS);

// ── Right panel ──────────────────────────────────────────────────
function togglePanel(tab) {
  const panel = document.getElementById('right-panel');
  if (panel.classList.contains('collapsed')) {
    panel.classList.remove('collapsed');
    switchTab(tab || activeTab);
  } else if (activeTab === tab) {
    panel.classList.add('collapsed');
  } else {
    switchTab(tab);
  }
}

function closePanel() {
  document.getElementById('right-panel').classList.add('collapsed');
}

function switchTab(tab, opts) {
  activeTab = tab;
  document.querySelectorAll('.panel-tab').forEach((t) => {
    const isActive = t.dataset.tab === tab;
    t.classList.toggle('active', isActive);
    t.setAttribute('aria-selected', isActive ? 'true' : 'false');
  });
  document.getElementById('panel-info').style.display = tab === 'info' ? 'block' : 'none';
  document.getElementById('panel-examples').style.display = tab === 'examples' ? 'block' : 'none';
  document.getElementById('panel-chat').style.display = tab === 'chat' ? 'flex' : 'none';
  // Widen panel for chat via a class, so .collapsed can still override the
  // width when the drawer is toggled shut (inline styles would not let it).
  document.getElementById('right-panel').classList.toggle('chat-active', tab === 'chat');
  if (tab === 'chat') {
    updateChatContext().then(() => {
      if (!aiConfigured && !(opts && opts.quiet)) openAIModal();
    });
  }
}
