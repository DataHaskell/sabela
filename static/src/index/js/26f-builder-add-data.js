// ── Visualize panel: bringing in data from elsewhere ──
// A file from the computer, a link (web, s3://, Google Drive or Sheets), or a
// path in another folder is copied into the workspace's data folder, loaded
// as a typed frame, and chosen as the chart's data in one step.

let addDataMode = null;

function renderAddData() {
  const box = document.createElement('div');
  box.className = 'add-data';
  const label = document.createElement('div');
  label.className = 'design-hint';
  label.textContent = 'Add data from';
  const row = document.createElement('div');
  row.className = 'add-data-row';
  const mode = (id, text, title) => {
    const b = designButton(text, () => {
      addDataMode = addDataMode === id ? null : id;
      renderDesigner();
    });
    b.title = title;
    if (addDataMode === id) b.classList.add('active');
    return b;
  };
  row.append(
    designButton('Computer', pickDataFromComputer),
    mode('link', 'Link', 'A web URL, s3://bucket/key, or a Google Drive or Sheets share link'),
    mode('path', 'Folder', 'A file anywhere on this machine, by its full path')
  );
  box.append(label, row);
  if (addDataMode) box.appendChild(addDataForm(addDataMode));
  return box;
}

function addDataForm(mode) {
  const form = document.createElement('form');
  form.className = 'add-data-form';
  const input = document.createElement('input');
  input.type = 'text';
  input.spellcheck = false;
  input.placeholder =
    mode === 'link'
      ? 'https://…, s3://bucket/key, or a Drive / Sheets link'
      : '/home/me/data/sales.csv';
  const err = document.createElement('div');
  err.className = 'modal-error';
  const go = designButton(mode === 'link' ? 'Fetch' : 'Copy in', () => {});
  go.type = 'submit';
  form.onsubmit = async (e) => {
    e.preventDefault();
    const v = input.value.trim();
    if (!v) return;
    go.disabled = true;
    err.textContent = '';
    try {
      const path = mode === 'link' ? await importDataLink(v) : await importDataPath(v);
      addDataMode = null;
      await useImportedData(path);
    } catch (ex) {
      err.textContent = ex.message;
      go.disabled = false;
    }
  };
  form.append(input, go, err);
  setTimeout(() => input.focus(), 0);
  return form;
}

// The name a link is saved under. Drive and Sheets links carry no file name,
// so a sheet is saved as CSV and a Drive file is assumed to be one.
function dataNameFromLink(url) {
  const sheet = url.match(/docs\.google\.com\/spreadsheets\/d\/([\w-]+)/);
  if (sheet) return 'sheet-' + sheet[1].slice(0, 8) + '.csv';
  const drive = url.match(/drive\.google\.com\/(?:file\/d\/|open\?id=)([\w-]+)/);
  if (drive) return 'drive-' + drive[1].slice(0, 8) + '.csv';
  const base = url.split(/[?#]/)[0].replace(/\/+$/, '').split('/').pop() || 'data';
  return /\.\w+$/.test(base) ? base : base + '.csv';
}

async function importDataLink(url) {
  const q =
    '/api/import-url?url=' +
    encodeURIComponent(url) +
    '&name=' +
    encodeURIComponent(dataNameFromLink(url)) +
    '&dir=' +
    encodeURIComponent(DROP_FOLDER);
  setStatus('Fetching ' + url + '…', 'running');
  const r = await fetch(q, { method: 'POST' });
  const j = await r.json().catch(() => null);
  if (!r.ok) throw new Error(linkErrorText(url, (j && j.error) || 'Could not fetch that link'));
  return j.path;
}

// A 403 or 404 from a cloud link nearly always means it is not public.
function linkErrorText(url, why) {
  const cloud = /^s3:\/\/|amazonaws\.com|drive\.google\.com|docs\.google\.com/.test(url);
  if (cloud && /HTTP 40[34]/.test(why))
    return (
      why +
      '. The file must be public (anyone with the link can view); private buckets and drives are not supported yet.'
    );
  return why;
}

async function importDataPath(path) {
  const r = await api('POST', 'import-path', { ipPath: path });
  if (!r.iprPath) throw new Error(r.iprError || 'Could not read that file');
  return r.iprPath;
}

function pickDataFromComputer() {
  const input = document.createElement('input');
  input.type = 'file';
  input.multiple = true;
  input.accept = DATA_EXTS.map((x) => '.' + x).join(',');
  input.onchange = async () => {
    const written = await uploadFiles(DROP_FOLDER, Array.from(input.files || []));
    for (const path of written || []) await useImportedData(path);
  };
  input.click();
}

// Load the new file and make it the chart's data; the Data pane relists so
// the file shows there too.
async function useImportedData(path) {
  if (typeof loadDataFiles === 'function') loadDataFiles().catch(() => {});
  if (!isPreviewable(path)) {
    setStatus(path + ' saved; only CSV, TSV and Parquet files can be charted', 'error');
    return;
  }
  await designerSetSource({ kind: 'file', ref: path, label: path, columns: [] });
  setStatus(path + ' added', 'ok');
}
