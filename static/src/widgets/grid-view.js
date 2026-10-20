// ── Grid chrome: theme, stylesheet, header ───────────────────────
// The parts of the table view drawn once per render. The rows, the pager and
// the entry point live in grid-body.js.

// Relative luminance of a CSS colour written as a hex triple or an rgb() list,
// which is what a custom property and a computed background come back as.
function sabelaGridLum(color) {
  const hex = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.exec((color || '').trim());
  let m;
  if (hex) {
    const h = hex[1].length === 3 ? [...hex[1]].map((c) => c + c) : hex[1].match(/../g);
    m = h.map((c) => parseInt(c, 16));
  } else {
    m = (color || '').match(/\d+/g) || [255, 255, 255];
  }
  return (0.299 * +m[0] + 0.587 * +m[1] + 0.114 * +m[2]) / 255;
}

// The notebook hands the output iframe its palette as custom properties (see
// notebookPaletteVars); a page that supplies none — a table opened on its own —
// still gets a palette, derived from whether its background is dark or light.
function sabelaGridTheme() {
  const cs = getComputedStyle(document.body);
  const root = getComputedStyle(document.documentElement);
  const v = (n) => (root.getPropertyValue(n) || '').trim();
  const surface = v('--sabela-surface');
  const dark = sabelaGridLum(surface || cs.backgroundColor || 'rgb(255,255,255)') < 0.5;
  const scale = v('--sabela-accent-rgb') || (dark ? '147,221,250' : '252,23,133');
  return {
    dark,
    themed: !!surface,
    bg: surface || (dark ? '#2e2e32' : '#fbfbfb'),
    fg: v('--sabela-fg') || cs.color || (dark ? '#e1e1e6' : '#181818'),
    dim: v('--sabela-dim') || (dark ? '#97979c' : '#808080'),
    border: v('--sabela-border') || (dark ? '#3a3a40' : '#e8e8e8'),
    rule: dark ? 'rgba(255,255,255,0.22)' : 'rgba(0,0,0,0.28)',
    hover: `rgba(${scale},${dark ? 0.1 : 0.07})`,
    selected: `rgba(${scale},${dark ? 0.22 : 0.16})`,
    accent: `rgb(${scale})`,
    scale,
  };
}

// The look of a Shiny DT or marimo table: a search box above, a quiet header
// with the type under each name, hairline rows without vertical rules, and a
// pager below. Colour is spent only on the sort caret, the hovered row and
// the current page.
function sabelaGridStyle(t) {
  if (document.getElementById('sabela-grid-style')) return;
  const s = document.createElement('style');
  s.id = 'sabela-grid-style';
  s.textContent = `
${t.themed ? `body{background:${t.bg};color:${t.fg}}` : ''}
.sg{font-family:'Fira Sans',system-ui,-apple-system,'Segoe UI',sans-serif;font-size:12.5px;
  line-height:1.4;color:${t.fg};max-width:100%}
.sg-bar{display:flex;align-items:center;justify-content:space-between;gap:10px;padding:0 0 8px}
.sg-search{background:transparent;border:1px solid ${t.border};color:${t.fg};font:inherit;
  font-size:12px;padding:4px 10px;width:220px;max-width:100%;box-sizing:border-box}
.sg-search::placeholder{color:${t.dim}}
.sg-search:focus{outline:none;border-color:${t.accent}}
.sg-expr{width:340px;font-family:'JetBrains Mono',ui-monospace,SFMono-Regular,monospace}
.sg-status{flex:1;min-width:0;color:${t.dim};font-size:11.5px;white-space:nowrap;overflow:hidden;
  text-overflow:ellipsis}
.sg-status.err{color:#e05555}
.sg-shape{color:${t.dim};font-size:11.5px;white-space:nowrap}
.sg-scroll{overflow-x:auto}
.sg table{border-collapse:collapse;width:100%}
.sg th,.sg td{padding:6px 10px;text-align:left;white-space:nowrap;max-width:320px;overflow:hidden;
  text-overflow:ellipsis;border-bottom:1px solid ${t.border};vertical-align:middle}
.sg thead th{border-bottom:1px solid ${t.rule};font-weight:500;vertical-align:bottom;
  cursor:pointer;user-select:none}
.sg thead th:hover{color:${t.accent}}
.sg-name{display:inline-block;vertical-align:bottom;max-width:100%;overflow:hidden;
  text-overflow:ellipsis}
.sg-type{display:block;color:${t.dim};font-size:10.5px;font-weight:400;line-height:1.3}
.sg-sort{display:inline-block;margin-left:6px;color:${t.accent};font-size:10px;
  vertical-align:bottom;min-width:8px}
.sg-sort.off{color:${t.dim};visibility:hidden}
.sg thead th:hover .sg-sort.off{visibility:visible}
.sg th.sg-idx,.sg td.sg-idx{color:${t.dim};text-align:right;width:1%;
  font-variant-numeric:tabular-nums}
.sg thead th.sg-idx{cursor:default;color:${t.dim}}
.sg th.sg-num,.sg td.sg-num{text-align:right;font-variant-numeric:tabular-nums}
.sg td.sg-blank{color:${t.dim}}
.sg tbody tr:hover td{background:${t.hover}}
.sg tbody tr.sg-sel td{background:${t.selected}}
.sg-empty td{color:${t.dim};text-align:center;padding:18px 10px}
.sg-foot{display:flex;align-items:center;justify-content:space-between;gap:10px;
  padding-top:8px;color:${t.dim};font-size:11.5px;flex-wrap:wrap}
.sg-count{display:flex;align-items:center;gap:14px}
.sg-size{background:transparent;border:1px solid ${t.border};color:${t.fg};font:inherit;
  font-size:11.5px;padding:2px 4px;cursor:pointer}
.sg-pager{display:flex;align-items:center;gap:2px}
.sg-page{background:transparent;border:1px solid transparent;color:${t.fg};font:inherit;
  font-size:11.5px;min-width:24px;height:24px;padding:0 6px;cursor:pointer;box-sizing:border-box}
.sg-page:hover{border-color:${t.border}}
.sg-page[aria-current=page]{background:${t.accent};color:${t.bg};border-color:${t.accent}}
.sg-page:disabled{color:${t.dim};cursor:default;border-color:transparent}
.sg-gap{color:${t.dim};padding:0 4px}
`;
  document.head.appendChild(s);
}

// The type under a column name: the Haskell type the notebook sent, or the
// nearest Haskell name for what the grid inferred from the values.
var SABELA_GRID_TYPE = { num: 'Double', int: 'Int', str: 'Text', bool: 'Bool' };

function sabelaGridTypeLabel(col) {
  return col.type || SABELA_GRID_TYPE[col.kind] || '';
}

function sabelaGridHeader(state, refresh) {
  const thead = document.createElement('thead');
  const tr = document.createElement('tr');
  const corner = document.createElement('th');
  corner.className = 'sg-idx';
  tr.appendChild(corner);
  state.cols.forEach((col) => tr.appendChild(sabelaGridHeadCell(state, col, refresh)));
  thead.appendChild(tr);
  return thead;
}

// A column header: its name, the sort caret (shown dim on hover until the
// column is sorted), and its type on a second line. A click cycles the sort
// through ascending, descending and off.
function sabelaGridHeadCell(state, col, refresh) {
  const th = document.createElement('th');
  if (col.numeric) th.className = 'sg-num';
  th.title = sabelaGridSummary(col);
  const name = document.createElement('span');
  name.className = 'sg-name';
  name.textContent = col.name;
  th.appendChild(name);
  const on = state.sort && state.sort.col === col.index;
  const caret = document.createElement('span');
  caret.className = on ? 'sg-sort' : 'sg-sort off';
  caret.textContent = on && state.sort.desc ? '▼' : '▲';
  th.appendChild(caret);
  const type = document.createElement('span');
  type.className = 'sg-type';
  type.textContent = sabelaGridTypeLabel(col);
  th.appendChild(type);
  th.onclick = () => {
    state.sort =
      on && !state.sort.desc
        ? { col: col.index, desc: true }
        : on
          ? null
          : { col: col.index, desc: false };
    state.page = 0;
    refresh();
  };
  return th;
}

// What a column header says about itself on hover.
function sabelaGridSummary(col) {
  const st = col.stats;
  const miss = st.missing ? ', ' + st.missing + ' missing' : '';
  if (col.numeric && st.n) {
    const f = (v) => sabelaGridFormat(String(v), col, 999);
    return `${col.name}: min ${f(st.min)}, mean ${f(st.mean)}, max ${f(st.max)}${miss}`;
  }
  return `${col.name}: ${st.unique || 0} unique${miss}`;
}
