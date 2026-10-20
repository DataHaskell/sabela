// ── Info / Haddock panel ─────────────────────────────────────────
async function lookupInfo(name) {
  if (!name) name = document.getElementById('info-input').value.trim();
  if (!name) return;
  document.getElementById('info-input').value = name;
  const container = document.getElementById('info-result');
  container.innerHTML = '<div style="color:var(--fg-dim)">Looking up...</div>';

  // Open panel if closed
  const panel = document.getElementById('right-panel');
  if (panel.classList.contains('collapsed')) panel.classList.remove('collapsed');
  switchTab('info');

  try {
    const res = await api('POST', 'info', { irName: name });
    container.innerHTML = '';
    if (res.irText) renderInfoResult(container, res.irText, name);
    else container.innerHTML = '<div class="info-note">No information found.</div>';
  } catch (e) {
    container.innerHTML = '';
    const err = document.createElement('div');
    err.style.color = 'var(--red)';
    err.textContent = 'Error: ' + e.message;
    container.appendChild(err);
  }
}

function lookupWordUnderCursor() {
  // Find focused CodeMirror
  for (const [id, cm] of Object.entries(editors)) {
    if (cm.hasFocus()) {
      const cur = cm.getCursor();
      const token = cm.getTokenAt(cur);
      const word = token.string.trim();
      if (word && /^[a-zA-Z_]/.test(word)) {
        lookupInfo(word);
      }
      return;
    }
  }
}
