// ── API helpers ──────────────────────────────────────────────────
async function api(method, path, body) {
  const opts = { method, headers: { 'Content-Type': 'application/json' } };
  if (body !== undefined) opts.body = JSON.stringify(body);
  const res = await fetch('/api/' + path, opts);
  if (!res.ok) throw new Error(await res.text());
  return res.json();
}

function setStatus(text, cls) {
  const el = document.getElementById('status');
  el.textContent = text;
  el.className = 'status ' + (cls || '');
  // 'running' and 'compiling' are in-progress signals: leave them up until a
  // later event replaces them. Everything else self-clears.
  if (cls !== 'running' && cls !== 'compiling')
    setTimeout(() => {
      if (el.textContent === text) el.textContent = '';
    }, 4000);
}

function flashSaved() {
  const el = document.getElementById('save-indicator');
  el.classList.add('show');
  setTimeout(() => el.classList.remove('show'), 2000);
}
