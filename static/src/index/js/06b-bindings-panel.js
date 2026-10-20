// ── Info panel: live GHCi bindings + heap ────────────────────────
// Lists what `:show bindings` reports (the active binding of each name;
// GHCi drops shadowed ones), marks entries no current cell defines as
// stale, and shows the RTS heap figures from the last GC.
let bindingsTimer = null;
let bindingsInFlight = false;
let bindingsCollapsed = false;
try {
  bindingsCollapsed = localStorage.getItem('sabela.bindings.collapsed') === '1';
} catch (_e) {
  /* storage unavailable */
}

function scheduleBindingsRefresh(delayMs) {
  clearTimeout(bindingsTimer);
  bindingsTimer = setTimeout(refreshBindings, delayMs === undefined ? 300 : delayMs);
}

function bindingsPanelVisible() {
  const panel = document.getElementById('right-panel');
  return panel && !panel.classList.contains('collapsed') && activeTab === 'info';
}

async function refreshBindings() {
  if (!bindingsPanelVisible() || bindingsInFlight) return;
  bindingsInFlight = true;
  const meta = document.getElementById('bindings-meta');
  if (meta) meta.textContent = 'refreshing…';
  try {
    const res = await api('GET', 'bindings');
    renderBindings(res);
  } catch (e) {
    if (meta) meta.textContent = 'error: ' + e.message;
  } finally {
    bindingsInFlight = false;
  }
}

function toggleBindings() {
  bindingsCollapsed = !bindingsCollapsed;
  try {
    localStorage.setItem('sabela.bindings.collapsed', bindingsCollapsed ? '1' : '0');
  } catch (_e) {
    /* storage unavailable */
  }
  applyBindingsCollapsed();
  if (!bindingsCollapsed) scheduleBindingsRefresh(0);
}

function applyBindingsCollapsed() {
  const body = document.getElementById('bindings-body');
  const chev = document.getElementById('bindings-chevron');
  if (body) body.style.display = bindingsCollapsed ? 'none' : '';
  if (chev) chev.textContent = bindingsCollapsed ? '▸' : '▾';
}

function formatBytes(n) {
  if (n === null || n === undefined) return '—';
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let i = 0;
  let v = n;
  while (v >= 1024 && i < units.length - 1) {
    v /= 1024;
    i++;
  }
  return (i === 0 ? v : v.toFixed(v >= 100 ? 0 : 1)) + ' ' + units[i];
}

function renderBindings(res) {
  const meta = document.getElementById('bindings-meta');
  const mem = document.getElementById('bindings-memory');
  const list = document.getElementById('bindings-list');
  if (!meta || !list || !mem) return;
  if (res.brStatus !== 'ok') {
    const msg = {
      busy: 'kernel busy — will refresh when it finishes',
      timeout: 'kernel did not answer in time',
      'no-session': 'no GHCi session',
    }[res.brStatus];
    meta.textContent = msg || res.brStatus;
    return;
  }
  const live = res.brBindings.filter((b) => b.beLive);
  const stale = res.brBindings.filter((b) => !b.beLive);
  meta.textContent = live.length + ' live' + (stale.length ? ' · ' + stale.length + ' stale' : '');

  const m = res.brMemory;
  mem.innerHTML = '';
  if (m) {
    const parts = [
      ['heap', formatBytes(m.msLiveBytes), 'live heap at the last GC'],
      ['peak', formatBytes(m.msMaxLiveBytes), 'largest live heap so far'],
      ['rts', formatBytes(m.msMemInUseBytes), 'memory the RTS holds from the OS'],
      ['gcs', String(m.msGcs), 'garbage collections so far'],
    ];
    for (const [label, val, tip] of parts) {
      const s = document.createElement('span');
      s.className = 'bind-mem';
      s.title = tip;
      s.innerHTML = '<span class="bind-mem-label"></span> <span class="bind-mem-val"></span>';
      s.firstChild.textContent = label;
      s.lastChild.textContent = val;
      mem.appendChild(s);
    }
  } else {
    mem.textContent = 'heap stats unavailable (restart the kernel to enable)';
  }

  list.innerHTML = '';
  if (!res.brBindings.length) {
    const empty = document.createElement('div');
    empty.className = 'bind-empty';
    empty.textContent = 'nothing bound yet — run a cell';
    list.appendChild(empty);
    return;
  }
  live.forEach((b) => list.appendChild(bindingRow(b)));
  if (stale.length) {
    const div = document.createElement('div');
    div.className = 'bind-divider';
    div.title = 'Still bound in GHCi, but no current cell defines these';
    div.textContent = 'stale';
    list.appendChild(div);
    stale.forEach((b) => list.appendChild(bindingRow(b)));
  }
}

function bindingRow(b) {
  const row = document.createElement('div');
  row.className = 'bind-row' + (b.beLive ? '' : ' stale') + (b.beKind !== 'value' ? ' decl' : '');
  row.title = b.beType + (b.beValue ? '\n= ' + b.beValue : '');
  row.onclick = () => lookupInfo(b.beName);

  const name = document.createElement('span');
  name.className = 'bind-name';
  name.textContent = b.beName;
  row.appendChild(name);

  if (b.beKind === 'value') {
    const ty = document.createElement('span');
    ty.className = 'bind-type';
    ty.textContent = ':: ' + b.beType;
    row.appendChild(ty);
    const val = document.createElement('span');
    val.className = 'bind-val' + (b.beValue ? '' : ' thunk');
    val.textContent = b.beValue ? '= ' + b.beValue : '= _';
    if (!b.beValue) val.title = 'not evaluated yet';
    row.appendChild(val);
  } else {
    const kind = document.createElement('span');
    kind.className = 'bind-kind';
    kind.textContent = b.beKind;
    row.appendChild(kind);
  }
  return row;
}
