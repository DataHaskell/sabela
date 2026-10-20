// ── Grid: expression box over the frame a table came from ────────
// Takes the search box's place when the table was printed from a frame the
// kernel still holds (data-frame-slot) and the page is the live editor. The
// expression is Haskell over `df`; the editor's widget bridge carries it to
// /api/frame/query and posts the reply back here. In an export, where no
// kernel answers, the plain search box stays.
var sabelaGridQuerySeq = 0;
var sabelaGridQueryPending = window.sabelaGridQueryPending || new Map();
window.sabelaGridQueryPending = sabelaGridQueryPending;

if (!window.sabelaGridQueryListening) {
  window.sabelaGridQueryListening = true;
  window.addEventListener('message', (e) => {
    const d = e.data;
    if (!d || d.type !== 'frameResult') return;
    const cb = window.sabelaGridQueryPending.get(d.id);
    if (!cb) return;
    window.sabelaGridQueryPending.delete(d.id);
    cb(d);
  });
}

function sabelaGridLive() {
  if (parent === window) return false;
  return !!getComputedStyle(document.documentElement).getPropertyValue('--sabela-live').trim();
}

function sabelaGridQueryBar(state, bar) {
  const input = document.createElement('input');
  input.className = 'sg-search sg-expr';
  input.type = 'text';
  input.spellcheck = false;
  input.placeholder = sabelaGridFilterHint(state.original);
  input.title =
    'Row filter: a dataframe expression of type Expr Bool over the columns by name, ' +
    'e.g. homeplanet .==. D.lit (Just "Earth"). Enter runs it in the kernel; ' +
    'an empty filter restores the table.';
  input.value = state.expr || '';
  const status = document.createElement('span');
  status.className = 'sg-status';
  if (state.note) {
    status.textContent = state.note.text;
    status.title = state.note.full || state.note.text;
    if (state.note.isError) status.classList.add('err');
  }
  input.onkeydown = (ev) => {
    if (ev.key === 'Escape') input.value = '';
    if (ev.key !== 'Enter' && ev.key !== 'Escape') return;
    const expr = input.value.trim();
    if (!expr) {
      sabelaGridRemount(state, state.original, '', null);
      return;
    }
    status.textContent = 'running…';
    status.className = 'sg-status';
    sabelaGridRunQuery(state, expr);
  };
  bar.appendChild(input);
  bar.appendChild(status);
}

function sabelaGridRunQuery(state, expr) {
  const id = 'q' + ++sabelaGridQuerySeq + '-' + Math.random().toString(36).slice(2, 8);
  const timer = setTimeout(() => {
    if (window.sabelaGridQueryPending.delete(id)) {
      sabelaGridRemount(state, state.raw, expr, {
        text: 'no reply from the editor',
        isError: true,
      });
    }
  }, 120000);
  window.sabelaGridQueryPending.set(id, (d) => {
    clearTimeout(timer);
    sabelaGridApplyResult(state, expr, d);
  });
  parent.postMessage(
    {
      type: 'frameQuery',
      id,
      slot: state.slot,
      query: state.queryFn,
      expr,
      cols: state.original.cols,
      types: state.original.types || [],
    },
    '*'
  );
}

// A table in the reply replaces the rows; any text (a scalar, a printed
// value) or error goes on the status line and the rows stay as they were.
function sabelaGridApplyResult(state, expr, d) {
  const outputs = d.outputs || [];
  const html = outputs.find(
    (o) => o.oiMime === 'text/html' && String(o.oiOutput).includes('sabela-grid-data')
  );
  let raw = state.raw;
  if (html) {
    const m = String(html.oiOutput).match(
      /<script type="application\/json" class="sabela-grid-data">([\s\S]*?)<\/script>/
    );
    if (m) raw = sabelaGridNormalize(JSON.parse(m[1]));
  }
  const texts = outputs
    .filter((o) => o !== html)
    .map((o) => String(o.oiOutput).trim())
    .filter(Boolean);
  let note = null;
  if (d.error) note = { text: sabelaGridOneLine(d.error), full: d.error, isError: true };
  else if (texts.length)
    note = { text: sabelaGridOneLine(texts.join(' ')), full: texts.join('\n') };
  else if (!html) note = { text: 'no output' };
  sabelaGridRemount(state, raw, expr, note);
}

function sabelaGridRemount(state, raw, expr, note) {
  sabelaGrid(state.container, {
    raw,
    original: state.original,
    slot: state.slot,
    queryFn: state.queryFn,
    expr,
    note,
  });
}

function sabelaGridOneLine(s) {
  const t = String(s).replace(/\s+/g, ' ').trim();
  return t.length > 200 ? t.slice(0, 200) + '…' : t;
}

// A filter the first column would take, as the box's placeholder: a numeric
// column compared, a text column matched, a column with gaps matched on Just.
function sabelaGridFilterHint(raw) {
  const name = raw.cols && raw.cols[0];
  if (!name) return 'row filter, e.g. age .>. D.lit 30';
  const ident = sabelaGridColumnIdent(name);
  const ty = (raw.types && raw.types[0]) || '';
  if (/^Maybe /.test(ty)) return `row filter, e.g. ${ident} .==. D.lit (Just …)`;
  if (/^(Int|Double|Float|Integer|Word)$/.test(ty)) return `row filter, e.g. ${ident} .>. D.lit 0`;
  return `row filter, e.g. ${ident} .==. D.lit "…"`;
}

// The identifier the kernel binds a column as (see Sabela.Server.FrameFilter).
function sabelaGridColumnIdent(name) {
  if (/^[a-z_][A-Za-z0-9]*$/.test(name)) return name;
  const cleaned = name
    .replace(/[()]/g, '')
    .replace(/[^A-Za-z0-9]/g, '_')
    .toLowerCase();
  return /^[a-z_]/.test(cleaned) ? cleaned : '_' + cleaned + '_';
}
