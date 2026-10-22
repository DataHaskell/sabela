// ── File upload ──────────────────────────────────────────────────
// One request per file: the raw File is POSTed to /api/upload, which writes
// the bytes into the work dir (binary-safe). `dir` is '.' for the toolbar
// button or a folder's relative path from the context menu.
function startUpload(dir) {
  let input = document.getElementById('hidden-upload-input');
  if (!input) {
    input = document.createElement('input');
    input.type = 'file';
    input.multiple = true;
    input.id = 'hidden-upload-input';
    input.style.display = 'none';
    document.body.appendChild(input);
  }
  input.value = '';
  input.onchange = () => uploadFiles(dir || '.', Array.from(input.files));
  input.click();
}

// Each entry is a File, or {name, blob} for bytes already read.
async function uploadFiles(dir, files) {
  if (!files.length) return [];
  setStatus(
    'Uploading ' + files.length + ' file' + (files.length === 1 ? '' : 's') + '…',
    'running'
  );
  let ok = 0;
  const failures = [];
  const written = [];
  for (const file of files) {
    try {
      const url =
        '/api/upload?dir=' + encodeURIComponent(dir) + '&name=' + encodeURIComponent(file.name);
      const r = await fetch(url, { method: 'POST', body: file.blob || file });
      if (r.ok) {
        ok++;
        written.push((dir === '.' ? '' : dir.replace(/\/+$/, '') + '/') + file.name);
      } else {
        const why = (await r.text().catch(() => '')).trim().slice(0, 160);
        failures.push(file.name + ': HTTP ' + r.status + (why ? ' ' + why : ''));
      }
    } catch (e) {
      failures.push(file.name + ': ' + (e && e.message ? e.message : String(e)));
    }
  }
  refreshFiles();
  if (failures.length) {
    console.error('upload failed:', failures);
    showErrorBanner('Upload failed. ' + failures.join(' · '));
  }
  setStatus(
    'Uploaded ' +
      ok +
      ' file' +
      (ok === 1 ? '' : 's') +
      (failures.length ? '; failed: ' + failures.join(' · ') : ''),
    failures.length ? 'error' : ''
  );
  return written;
}

// ── Import from URL ──────────────────────────────────────────────
// The server fetches the URL (the browser can't, due to CORS) and writes
// it into `dir`. GitHub/gist links are rewritten to their raw form
// server-side; a freshly imported .md opens straight away.
let _importDir = '.';
function startImportUrl(dir) {
  _importDir = dir || '.';
  const overlay = document.getElementById('import-url-overlay');
  const urlField = document.getElementById('import-url-field');
  const nameField = document.getElementById('import-url-name');
  urlField.value = '';
  nameField.value = '';
  delete nameField.dataset.touched;
  document.getElementById('import-url-error').textContent = '';
  overlay.style.display = 'flex';
  urlField.oninput = () => {
    if (!nameField.dataset.touched) nameField.value = suggestNameFromUrl(urlField.value);
  };
  nameField.oninput = () => {
    nameField.dataset.touched = '1';
  };
  document.getElementById('import-url-ok').onclick = doImportUrl;
  const onKey = (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      doImportUrl();
    } else if (e.key === 'Escape') {
      e.preventDefault();
      closeImportUrl();
    }
  };
  urlField.onkeydown = onKey;
  nameField.onkeydown = onKey;
  setTimeout(() => urlField.focus(), 30);
}

function suggestNameFromUrl(u) {
  let path = (u || '').split('#')[0].split('?')[0].replace(/\/+$/, '');
  let base = path.substring(path.lastIndexOf('/') + 1);
  if (base && !base.includes('.')) base += '.md';
  return base;
}

function closeImportUrl() {
  document.getElementById('import-url-overlay').style.display = 'none';
}

async function doImportUrl() {
  const url = document.getElementById('import-url-field').value.trim();
  const name = document.getElementById('import-url-name').value.trim();
  const errEl = document.getElementById('import-url-error');
  if (!url) {
    errEl.textContent = 'A URL is required.';
    return;
  }
  if (!name) {
    errEl.textContent = 'A save-as name is required.';
    return;
  }
  errEl.textContent = '';
  setStatus('Importing ' + name + '…', 'running');
  let result;
  try {
    const q =
      '/api/import-url?url=' +
      encodeURIComponent(url) +
      '&name=' +
      encodeURIComponent(name) +
      '&dir=' +
      encodeURIComponent(_importDir);
    const r = await fetch(q, { method: 'POST' });
    if (!r.ok) {
      const j = await r.json().catch(() => null);
      throw new Error((j && j.error) || 'Import failed');
    }
    result = await r.json();
  } catch (e) {
    errEl.textContent = e.message;
    setStatus('Import failed: ' + e.message, 'error');
    return;
  }
  closeImportUrl();
  refreshFiles();
  setStatus('Imported ' + result.path, '');
  if (result.path && result.path.endsWith('.md')) {
    openFile({ feName: result.name, fePath: result.path, feIsDir: false });
  }
}

// A full error, readable and dismissable, above the notebook; the status
// bar truncates long messages.
function showErrorBanner(text) {
  let banner = document.getElementById('error-banner');
  if (!banner) {
    banner = document.createElement('div');
    banner.id = 'error-banner';
    banner.className = 'notebook-banner';
    document.body.prepend(banner);
  }
  banner.innerHTML = '';
  const msg = document.createElement('span');
  msg.textContent = text;
  const close = document.createElement('button');
  close.textContent = 'Dismiss';
  close.onclick = () => banner.remove();
  banner.append(msg, close);
  banner.style.display = 'flex';
}
