# examples/typed

```haskell
-- cabal: build-depends: dataframe, text

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Typed as DT
import DataFrame ((|>))
```

```haskell
import Data.Function

df <- D.readCsv "./examples/data/housing.csv"

df & D.take 10
```

> <!-- scripths:mime text/html -->
> <div class="sabela-grid"><script type="application/json" class="sabela-grid-data">{"cols":["longitude","latitude","housing_median_age","total_rooms","total_bedrooms","population","households","median_income","median_house_value","ocean_proximity"],"rows":[["-122.23","37.88","41.0","880.0","129.0","322.0","126.0","8.3252","452600.0","NEAR BAY"],["-122.22","37.86","21.0","7099.0","1106.0","2401.0","1138.0","8.3014","358500.0","NEAR BAY"],["-122.24","37.85","52.0","1467.0","190.0","496.0","177.0","7.2574","352100.0","NEAR BAY"],["-122.25","37.85","52.0","1274.0","235.0","558.0","219.0","5.6431000000000004","341300.0","NEAR BAY"],["-122.25","37.85","52.0","1627.0","280.0","565.0","259.0","3.8462","342200.0","NEAR BAY"],["-122.25","37.85","52.0","919.0","213.0","413.0","193.0","4.0368","269700.0","NEAR BAY"],["-122.25","37.84","52.0","2535.0","489.0","1094.0","514.0","3.6591","299200.0","NEAR BAY"],["-122.25","37.84","52.0","3104.0","687.0","1157.0","647.0","3.12","241400.0","NEAR BAY"],["-122.26","37.84","42.0","2555.0","665.0","1206.0","595.0","2.0804","226700.0","NEAR BAY"],["-122.25","37.84","52.0","3549.0","707.0","1551.0","714.0","3.6912000000000003","261100.0","NEAR BAY"]],"total":10}</script></div><script>// ── Grid model: typing, statistics, formatting, ordering ─────────
> // Pure data work for the table view. No DOM: everything here is a function
> // of the raw {cols, rows} payload the notebook emitted, so it can be reasoned
> // about (and tested) on its own.
> 
> // A column is numeric when every value that is present parses as one. Blank
> // cells do not vote: a column of numbers with gaps is still numeric.
> function sabelaGridKind(rows, i) {
>   let seen = 0;
>   let nums = 0;
>   let ints = 0;
>   let bools = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '' || v === 'null' || v === 'Nothing') continue;
>     seen++;
>     if (v === 'True' || v === 'False') bools++;
>     if (/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(v)) {
>       nums++;
>       if (/^[-+]?\d+$/.test(v)) ints++;
>     }
>   }
>   if (!seen) return 'str';
>   if (bools === seen) return 'bool';
>   if (nums < seen) return 'str';
>   return ints === seen ? 'int' : 'num';
> }
> 
> function sabelaGridNum(v) {
>   const t = (v ?? '').trim();
>   if (t === '' || t === 'null' || t === 'Nothing') return null;
>   const n = Number(t);
>   return Number.isFinite(n) ? n : null;
> }
> 
> // Column statistics, in the two shapes the header strip draws: a histogram
> // for numbers, the leading categories for everything else.
> function sabelaGridStats(rows, i, kind) {
>   const empty = { n: 0, missing: 0 };
>   if (kind === 'num' || kind === 'int') {
>     const vals = [];
>     let missing = 0;
>     for (const row of rows) {
>       const n = sabelaGridNum(row[i]);
>       if (n === null) missing++;
>       else vals.push(n);
>     }
>     if (!vals.length) return { ...empty, missing, kind };
>     let min = vals[0];
>     let max = vals[0];
>     let sum = 0;
>     for (const v of vals) {
>       if (v < min) min = v;
>       if (v > max) max = v;
>       sum += v;
>     }
>     const bins = new Array(24).fill(0);
>     const span = max - min;
>     for (const v of vals) {
>       const b = span === 0 ? 0 : Math.min(23, Math.floor(((v - min) / span) * 24));
>       bins[b]++;
>     }
>     return { kind, n: vals.length, missing, min, max, mean: sum / vals.length, bins };
>   }
>   const counts = new Map();
>   let missing = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '') missing++;
>     counts.set(v, (counts.get(v) || 0) + 1);
>   }
>   const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 6);
>   const n = rows.length - missing;
>   // A column whose every value differs is a key, not a set of categories:
>   // colouring it says nothing, so it is left uncoloured.
>   const categorical = counts.size > 1 && counts.size < n && counts.size <= 12;
>   return { kind, n, missing, unique: counts.size, top, categorical };
> }
> 
> // How many decimals a numeric column needs, capped so a wide column does not
> // print noise. Integers ask for none.
> function sabelaGridDecimals(rows, i, kind) {
>   if (kind === 'int' || kind === 'bool' || kind === 'str') return 0;
>   let want = 0;
>   for (const row of rows) {
>     const t = (row[i] ?? '').trim();
>     const dot = t.indexOf('.');
>     if (dot < 0 || /[eE]/.test(t)) continue;
>     want = Math.max(want, t.length - dot - 1);
>     if (want >= 4) return 4;
>   }
>   return want;
> }
> 
> // Datagrok's adaptive formatting: full precision when the column is wide,
> // degraded — then abbreviated — as it narrows.
> function sabelaGridFormat(v, col, width) {
>   if (col.kind === 'str' || col.kind === 'bool') return v ?? '';
>   const n = sabelaGridNum(v);
>   if (n === null) return v ?? '';
>   const abs = Math.abs(n);
>   if (abs !== 0 && (abs >= 1e9 || abs < 1e-4)) return n.toExponential(2);
>   let decimals = col.decimals;
>   if (width < 88) decimals = Math.min(decimals, 2);
>   if (width < 64) decimals = 0;
>   if (abs >= 1e6 && width < 110) {
>     return (n / 1e6).toFixed(decimals ? 1 : 0) + 'M';
>   }
>   return n.toLocaleString(undefined, {
>     minimumFractionDigits: decimals,
>     maximumFractionDigits: decimals,
>   });
> }
> 
> function sabelaGridColumns(raw) {
>   return raw.cols.map((name, i) => {
>     const kind = sabelaGridKind(raw.rows, i);
>     return {
>       name,
>       index: i,
>       kind,
>       numeric: kind === 'num' || kind === 'int',
>       decimals: sabelaGridDecimals(raw.rows, i, kind),
>       stats: sabelaGridStats(raw.rows, i, kind),
>       width: 0,
>     };
>   });
> }
> 
> // The order rows are shown in: the sort, then the search, both applied to the
> // original row indices so the row header keeps naming the source row.
> function sabelaGridOrder(raw, cols, sort, query) {
>   let order = raw.rows.map((_, n) => n);
>   const test = sabelaGridQuery(query, cols);
>   if (test) order = order.filter((n) => test(raw.rows[n]));
>   if (sort) {
>     const col = cols[sort.col];
>     const dir = sort.desc ? -1 : 1;
>     order.sort((a, b) => {
>       const ma = sabelaGridMissing(raw.rows[a], col);
>       const mb = sabelaGridMissing(raw.rows[b], col);
>       if (ma !== mb) return ma ? 1 : -1;
>       return dir * sabelaGridCompare(raw.rows[a], raw.rows[b], col);
>     });
>   }
>   return order;
> }
> 
> // A value that is not there sorts to the end whichever way the column is
> // sorted: it is absent, not small.
> function sabelaGridMissing(row, col) {
>   const v = row[col.index];
>   return col.numeric ? sabelaGridNum(v) === null : String(v ?? '').trim() === '';
> }
> 
> function sabelaGridCompare(ra, rb, col) {
>   const i = col.index;
>   if (col.numeric) {
>     const a = sabelaGridNum(ra[i]);
>     const b = sabelaGridNum(rb[i]);
>     if (a === null) return b === null ? 0 : 1;
>     if (b === null) return -1;
>     return a - b;
>   }
>   return String(ra[i] ?? '').localeCompare(String(rb[i] ?? ''), undefined, { numeric: true });
> }
> 
> // Search, in Datagrok's two spellings: free text over every cell, or one
> // column compared against a value ("temp > 25", "city = Harare").
> function sabelaGridQuery(query, cols) {
>   const q = (query || '').trim();
>   if (!q) return null;
>   const m = q.match(/^\s*([\w ]+?)\s*(>=|<=|!=|>|<|=)\s*(.+?)\s*$/);
>   if (m) {
>     const col = cols.find((c) => c.name.toLowerCase() === m[1].toLowerCase());
>     if (col) return sabelaGridComparison(col, m[2], m[3]);
>   }
>   const needle = q.toLowerCase();
>   return (row) =>
>     row.some((v) =>
>       String(v ?? '')
>         .toLowerCase()
>         .includes(needle)
>     );
> }
> 
> function sabelaGridComparison(col, op, rhs) {
>   const i = col.index;
>   const num = Number(rhs);
>   const useNum = col.numeric && Number.isFinite(num);
>   return (row) => {
>     const raw = String(row[i] ?? '');
>     const a = useNum ? sabelaGridNum(raw) : raw.toLowerCase();
>     const b = useNum ? num : rhs.toLowerCase();
>     if (a === null) return false;
>     switch (op) {
>       case '>':
>         return a > b;
>       case '<':
>         return a < b;
>       case '>=':
>         return a >= b;
>       case '<=':
>         return a <= b;
>       case '!=':
>         return a !== b;
>       default:
>         return useNum ? a === b : a.includes(b);
>     }
>   };
> }
> 
> // ── Grid chrome: theme, stylesheet, header, distribution strip ───
> // The parts of the table view that are drawn once per render. The body and
> // its interactions live in grid-body.js.
> 
> // The notebook only hands an output iframe three colours, so the rest of the
> // palette is derived from whether the background it gave is dark or light.
> function sabelaGridTheme() {
>   const cs = getComputedStyle(document.body);
>   const m = (cs.backgroundColor || 'rgb(255,255,255)').match(/\d+/g) || [255, 255, 255];
>   const lum = (0.299 * +m[0] + 0.587 * +m[1] + 0.114 * +m[2]) / 255;
>   const dark = lum < 0.5;
>   return {
>     dark,
>     fg: cs.color || (dark ? '#e6e6ef' : '#1e1e2e'),
>     dim: dark ? '#8b8b9c' : '#8a8a99',
>     border: dark ? '#3a3a46' : '#e2e2ea',
>     rule: dark ? '#5a5a68' : '#b9b9c6',
>     zebra: dark ? 'rgba(255,255,255,0.028)' : 'rgba(0,0,0,0.022)',
>     hover: dark ? 'rgba(130,175,255,0.13)' : 'rgba(60,120,240,0.08)',
>     current: '#3aa657',
>     selected: dark ? 'rgba(255,170,70,0.20)' : 'rgba(255,165,60,0.22)',
>     scale: dark ? '130,175,255' : '60,120,240',
>   };
> }
> 
> function sabelaGridStyle(t) {
>   if (document.getElementById('sabela-grid-style')) return;
>   const s = document.createElement('style');
>   s.id = 'sabela-grid-style';
>   s.textContent = `
> .sg{font-family:inherit;font-size:11.5px;color:${t.fg};max-width:100%}
> .sg-bar{display:flex;align-items:center;gap:8px;padding:0 0 5px;color:${t.dim};font-size:11px}
> .sg-search{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.fg};
>   font-size:11px;padding:2px 9px;width:170px;cursor:text}
> .sg-search:focus{outline:none;border-color:rgba(${t.scale},0.8)}
> .sg-btn{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.dim};
>   font-size:10.5px;padding:2px 9px}
> .sg-btn[aria-pressed=true]{color:${t.fg};border-color:rgba(${t.scale},0.7)}
> .sg-scroll{overflow:auto;border-top:1px solid ${t.border};border-bottom:1px solid ${t.border};
>   max-height:420px;position:relative}
> .sg table{border-collapse:separate;border-spacing:0;table-layout:fixed;width:max-content;max-width:100%}
> .sg th,.sg td{border-right:1px solid ${t.border};padding:2px 7px;white-space:nowrap;overflow:hidden;
>   text-overflow:ellipsis;height:21px;box-sizing:border-box}
> .sg thead th{position:sticky;top:0;z-index:2;background:${t.dark ? '#20202a' : '#fbfbfd'};
>   border-bottom:1px solid ${t.rule};font-weight:500;text-align:left;user-select:none;cursor:pointer}
> .sg thead tr.sg-strip th{top:25px;z-index:1;padding:0;height:15px;cursor:default;opacity:.72;
>   border-bottom:1px solid ${t.border}}
> .sg .sg-idx{color:${t.dim};text-align:right;width:46px;position:sticky;left:0;z-index:1;
>   background:${t.dark ? '#20202a' : '#fbfbfd'};cursor:pointer}
> .sg thead .sg-idx{z-index:3}
> .sg-name{display:inline-block;max-width:calc(100% - 30px);overflow:hidden;text-overflow:ellipsis;
>   vertical-align:bottom}
> .sg-type{color:${t.dim};font-size:9px;margin-left:4px;letter-spacing:.4px}
> .sg-caret{color:rgba(${t.scale},1);margin-left:3px}
> .sg-resize{position:absolute;top:0;right:-3px;width:7px;height:100%;cursor:col-resize;z-index:4}
> .sg tbody tr:nth-child(even) td{background:${t.zebra}}
> .sg tbody tr:hover td{background:${t.hover}}
> .sg tbody tr.sg-sel td{background:${t.selected}}
> .sg tbody tr.sg-cur td{box-shadow:inset 0 -1px 0 ${t.current},inset 0 1px 0 ${t.current}}
> .sg tbody tr.sg-cur td.sg-idx{box-shadow:inset 3px 0 0 ${t.current},inset 0 -1px 0 ${t.current},
>   inset 0 1px 0 ${t.current}}
> .sg-num{text-align:right;font-variant-numeric:tabular-nums}
> .sg-chip{display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:5px;
>   vertical-align:baseline}
> .sg-foot{color:${t.dim};font-size:10.5px;padding-top:5px;display:flex;gap:10px;flex-wrap:wrap}
> .sg-stat{color:${t.dim};font-size:10px}
> `;
>   document.head.appendChild(s);
> }
> 
> var SABELA_GRID_TYPE = { num: '#', int: '#', str: 'abc', bool: '✓' };
> 
> // A column's distribution, drawn under its name: bars over the value range
> // for a number, the leading categories in proportion for anything else.
> function sabelaGridStrip(col, t) {
>   const st = col.stats;
>   const ns = 'http://www.w3.org/2000/svg';
>   const svg = document.createElementNS(ns, 'svg');
>   svg.setAttribute('preserveAspectRatio', 'none');
>   svg.setAttribute('viewBox', '0 0 100 14');
>   svg.style.cssText = 'width:100%;height:14px;display:block';
>   const add = (x, y, w, h, fill, title) => {
>     const r = document.createElementNS(ns, 'rect');
>     r.setAttribute('x', x);
>     r.setAttribute('y', y);
>     r.setAttribute('width', Math.max(0.6, w));
>     r.setAttribute('height', Math.max(0.8, h));
>     r.setAttribute('fill', fill);
>     if (title) {
>       const el = document.createElementNS(ns, 'title');
>       el.textContent = title;
>       r.appendChild(el);
>     }
>     svg.appendChild(r);
>   };
>   if (col.numeric && st.bins) {
>     const peak = Math.max(...st.bins, 1);
>     const w = 100 / st.bins.length;
>     st.bins.forEach((n, i) => {
>       if (!n) return;
>       const h = (n / peak) * 12;
>       add(i * w, 13 - h, w - 0.5, h, `rgba(${t.scale},0.5)`, n + ' rows');
>     });
>   } else if (st.categorical && st.top) {
>     let x = 0;
>     st.top.forEach((entry) => {
>       const w = (entry[1] / st.n) * 100;
>       add(x, 4, w - 0.4, 7, sabelaGridChipColor(entry[0], t), entry[0] + ' — ' + entry[1]);
>       x += w;
>     });
>     if (x < 99.5) add(x, 4, 100 - x, 7, t.border, 'other');
>   }
>   return svg;
> }
> 
> // A stable colour per category, so the same value reads the same everywhere
> // in the grid.
> function sabelaGridChipColor(v, t) {
>   let h = 0;
>   const s = String(v);
>   for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360;
>   return `hsl(${h},${t.dark ? 45 : 55}%,${t.dark ? 52 : 60}%)`;
> }
> 
> function sabelaGridHeader(state, refresh) {
>   const t = state.theme;
>   const thead = document.createElement('thead');
>   const names = document.createElement('tr');
>   const strip = document.createElement('tr');
>   strip.className = 'sg-strip';
>   const corner = document.createElement('th');
>   corner.className = 'sg-idx';
>   corner.title = 'clear selection';
>   corner.onclick = () => {
>     state.selected.clear();
>     state.current = -1;
>     refresh();
>   };
>   names.appendChild(corner);
>   const stripCorner = document.createElement('th');
>   stripCorner.className = 'sg-idx';
>   strip.appendChild(stripCorner);
>   state.cols.forEach((col) => {
>     names.appendChild(sabelaGridHeadCell(state, col, refresh));
>     const sth = document.createElement('th');
>     sth.appendChild(sabelaGridStrip(col, t));
>     strip.appendChild(sth);
>   });
>   thead.appendChild(names);
>   thead.appendChild(strip);
>   return thead;
> }
> 
> function sabelaGridHeadCell(state, col, refresh) {
>   const th = document.createElement('th');
>   th.style.position = 'relative';
>   th.title = sabelaGridSummary(col);
>   const name = document.createElement('span');
>   name.className = 'sg-name';
>   name.textContent = col.name;
>   th.appendChild(name);
>   const type = document.createElement('span');
>   type.className = 'sg-type';
>   type.textContent = SABELA_GRID_TYPE[col.kind] || '';
>   th.appendChild(type);
>   if (state.sort && state.sort.col === col.index) {
>     const caret = document.createElement('span');
>     caret.className = 'sg-caret';
>     caret.textContent = state.sort.desc ? '↓' : '↑';
>     th.appendChild(caret);
>   }
>   th.onclick = () => {
>     const on = state.sort && state.sort.col === col.index;
>     state.sort =
>       on && !state.sort.desc
>         ? { col: col.index, desc: true }
>         : on
>           ? null
>           : { col: col.index, desc: false };
>     refresh();
>   };
>   const grip = document.createElement('div');
>   grip.className = 'sg-resize';
>   grip.onclick = (e) => e.stopPropagation();
>   grip.onmousedown = (e) => sabelaGridDragWidth(e, state, col, refresh);
>   th.appendChild(grip);
>   return th;
> }
> 
> function sabelaGridDragWidth(e, state, col, refresh) {
>   e.preventDefault();
>   const startX = e.clientX;
>   const startW = col.width;
>   const move = (ev) => {
>     col.width = Math.max(46, startW + (ev.clientX - startX));
>     refresh();
>   };
>   const up = () => {
>     window.removeEventListener('mousemove', move);
>     window.removeEventListener('mouseup', up);
>   };
>   window.addEventListener('mousemove', move);
>   window.addEventListener('mouseup', up);
> }
> 
> // What a column header says about itself on hover, and what the stats line
> // under the grid repeats for every column at once.
> function sabelaGridSummary(col) {
>   const st = col.stats;
>   const miss = st.missing ? ', ' + st.missing + ' missing' : '';
>   if (col.numeric && st.n) {
>     const f = (v) => sabelaGridFormat(String(v), col, 999);
>     return `${col.name}: min ${f(st.min)}, mean ${f(st.mean)}, max ${f(st.max)}${miss}`;
>   }
>   return `${col.name}: ${st.unique || 0} unique${miss}`;
> }
> 
> // ── Grid body: rows, colour coding, selection, and the entry point ──
> // Only the rows in view are in the DOM; the rest is spacer height, so a
> // table of thousands of rows costs the same to scroll as a short one.
> // `var`, not `const`: an output iframe is reused across re-runs, so this
> // script is evaluated again in a window that already holds these names.
> 
> var SABELA_GRID_ROW_H = 21;
> var SABELA_GRID_WINDOW = 60;
> 
> function sabelaGridCellText(col, raw, width) {
>   return sabelaGridFormat(raw, col, width);
> }
> 
> // Datagrok's linear scheme: a number's cell carries its place in the column's
> // range as background weight.
> function sabelaGridPaint(td, col, raw, state) {
>   if (!state.color) return;
>   const st = col.stats;
>   if (col.numeric && st.n && st.max !== st.min) {
>     const n = sabelaGridNum(raw);
>     if (n === null) return;
>     const w = (n - st.min) / (st.max - st.min);
>     td.style.background = `rgba(${state.theme.scale},${(0.05 + 0.32 * w).toFixed(3)})`;
>   } else if (!col.numeric && st.categorical && String(raw ?? '').trim() !== '') {
>     const chip = document.createElement('span');
>     chip.className = 'sg-chip';
>     chip.style.background = sabelaGridChipColor(raw, state.theme);
>     td.insertBefore(chip, td.firstChild);
>   }
> }
> 
> function sabelaGridRow(state, n, position) {
>   const raw = state.raw.rows[n];
>   const tr = document.createElement('tr');
>   if (state.selected.has(n)) tr.classList.add('sg-sel');
>   if (state.current === n) tr.classList.add('sg-cur');
>   const idx = document.createElement('td');
>   idx.className = 'sg-idx';
>   idx.textContent = position;
>   tr.appendChild(idx);
>   state.cols.forEach((col) => {
>     const td = document.createElement('td');
>     const text = sabelaGridCellText(col, raw[col.index], col.width);
>     td.textContent = text;
>     td.title = String(raw[col.index] ?? '');
>     if (col.numeric) td.className = 'sg-num';
>     sabelaGridPaint(td, col, raw[col.index], state);
>     tr.appendChild(td);
>   });
>   tr.onclick = (e) => sabelaGridPick(state, n, e);
>   return tr;
> }
> 
> // A click makes a row current; ctrl or shift add to the selection, the way a
> // spreadsheet does.
> function sabelaGridPick(state, n, e) {
>   if (e.shiftKey && state.current >= 0) {
>     const from = state.order.indexOf(state.current);
>     const to = state.order.indexOf(n);
>     if (from >= 0 && to >= 0) {
>       const [lo, hi] = from < to ? [from, to] : [to, from];
>       for (let i = lo; i <= hi; i++) state.selected.add(state.order[i]);
>     }
>   } else if (e.ctrlKey || e.metaKey) {
>     if (!state.selected.delete(n)) state.selected.add(n);
>   } else {
>     state.selected.clear();
>   }
>   state.current = n;
>   state.repaint();
> }
> 
> function sabelaGridBody(state, tbody, scroll) {
>   const total = state.order.length;
>   const first = Math.max(0, Math.floor(scroll.scrollTop / SABELA_GRID_ROW_H) - 8);
>   const last = Math.min(total, first + SABELA_GRID_WINDOW);
>   tbody.textContent = '';
>   const spacer = (h) => {
>     const tr = document.createElement('tr');
>     const td = document.createElement('td');
>     td.colSpan = state.cols.length + 1;
>     td.style.cssText = `height:${h}px;padding:0;border:none`;
>     tr.appendChild(td);
>     return tr;
>   };
>   if (first) tbody.appendChild(spacer(first * SABELA_GRID_ROW_H));
>   for (let i = first; i < last; i++) {
>     tbody.appendChild(sabelaGridRow(state, state.order[i], state.order[i] + 1));
>   }
>   if (last < total) tbody.appendChild(spacer((total - last) * SABELA_GRID_ROW_H));
> }
> 
> // Enough width for the values and for the header's own furniture — the name,
> // its type, and the room a sort caret will want.
> function sabelaGridWidth(col, rows) {
>   let n = 0;
>   for (let i = 0; i < rows.length && i < 80; i++) {
>     n = Math.max(n, String(rows[i][col.index] ?? '').length);
>   }
>   const header = col.name.length * 7 + 38;
>   return Math.round(Math.min(260, Math.max(56, header, 16 + n * 6.8)));
> }
> 
> // Columns are laid out from their content, then opened up to whatever width
> // the cell actually gave us, so a narrow table does not sit in a wide gap.
> function sabelaGridFit(state, scroll, refresh) {
>   const avail = scroll.clientWidth - 48;
>   if (avail <= 0) return;
>   const total = state.cols.reduce((sum, c) => sum + c.width, 0);
>   if (total >= avail) return;
>   const grow = avail / total;
>   state.cols.forEach((c) => {
>     c.width = Math.min(360, Math.round(c.width * grow));
>   });
>   refresh();
> }
> 
> function sabelaGridToolbar(state, refresh) {
>   const bar = document.createElement('div');
>   bar.className = 'sg-bar';
>   const search = document.createElement('input');
>   search.className = 'sg-search';
>   search.type = 'text';
>   const numeric = state.cols.find((c) => c.numeric);
>   search.placeholder = numeric ? `search  ·  ${numeric.name} > 0` : 'search';
>   search.oninput = () => {
>     state.query = search.value;
>     refresh();
>   };
>   bar.appendChild(search);
>   const toggle = (label, key, title) => {
>     const b = document.createElement('button');
>     b.className = 'sg-btn';
>     b.type = 'button';
>     b.textContent = label;
>     b.title = title;
>     b.setAttribute('aria-pressed', String(state[key]));
>     b.onclick = () => {
>       state[key] = !state[key];
>       b.setAttribute('aria-pressed', String(state[key]));
>       refresh();
>     };
>     bar.appendChild(b);
>   };
>   toggle('color', 'color', 'colour cells by value');
>   toggle('stats', 'stats', 'show column statistics');
>   return bar;
> }
> 
> function sabelaGridFooter(state) {
>   const foot = document.createElement('div');
>   foot.className = 'sg-foot';
>   const shown = state.order.length;
>   const carried = state.raw.rows.length;
>   // The rows the page holds, and the rows the data has: a capped table shows
>   // fewer than it counts, and a search fewer still.
>   const total = state.raw.total ?? carried;
>   const rows =
>     total > carried
>       ? shown === carried
>         ? `first ${carried} of ${total} rows`
>         : `${shown} of first ${carried} · ${total} rows`
>       : shown === total
>         ? `${total} rows`
>         : `${shown} of ${total} rows`;
>   const cols = document.createElement('span');
>   cols.textContent = `${rows} × ${state.cols.length} columns`;
>   foot.appendChild(cols);
>   if (state.selected.size) {
>     const sel = document.createElement('span');
>     sel.textContent = `${state.selected.size} selected`;
>     foot.appendChild(sel);
>   }
>   if (state.stats) {
>     state.cols.forEach((col) => {
>       const s = document.createElement('span');
>       s.className = 'sg-stat';
>       s.textContent = sabelaGridSummary(col);
>       foot.appendChild(s);
>     });
>   }
>   return foot;
> }
> 
> // Turns one emitted payload into a live grid. Called once per table, with the
> // container the notebook printed.
> function sabelaGrid(container) {
>   const holder = container.querySelector('script.sabela-grid-data');
>   const raw = JSON.parse(holder.textContent);
>   const theme = sabelaGridTheme();
>   sabelaGridStyle(theme);
>   const cols = sabelaGridColumns(raw);
>   cols.forEach((col) => {
>     col.width = sabelaGridWidth(col, raw.rows);
>   });
>   const state = {
>     raw,
>     cols,
>     theme,
>     sort: null,
>     query: '',
>     color: true,
>     stats: false,
>     current: -1,
>     selected: new Set(),
>     order: raw.rows.map((_, n) => n),
>   };
>   container.className = 'sg';
>   container.textContent = '';
>   const scroll = document.createElement('div');
>   scroll.className = 'sg-scroll';
>   const table = document.createElement('table');
>   const group = document.createElement('colgroup');
>   const tbody = document.createElement('tbody');
>   scroll.appendChild(table);
> 
>   const paintBody = () => sabelaGridBody(state, tbody, scroll);
>   const refresh = () => {
>     state.order = sabelaGridOrder(raw, cols, state.sort, state.query);
>     group.textContent = '';
>     [46, ...cols.map((c) => c.width)].forEach((w) => {
>       const col = document.createElement('col');
>       col.style.width = w + 'px';
>       group.appendChild(col);
>     });
>     table.textContent = '';
>     table.appendChild(group);
>     table.appendChild(sabelaGridHeader(state, refresh));
>     table.appendChild(tbody);
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   state.repaint = () => {
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   scroll.addEventListener('scroll', paintBody);
>   container.appendChild(sabelaGridToolbar(state, refresh));
>   container.appendChild(scroll);
>   container.appendChild(sabelaGridFooter(state));
>   refresh();
>   if (scroll.clientWidth) sabelaGridFit(state, scroll, refresh);
>   else requestAnimationFrame(() => sabelaGridFit(state, scroll, refresh));
> }
> 
> // Every grid the page printed, mounted the moment its markup exists.
> function sabelaGridMountAll() {
>   document.querySelectorAll('div.sabela-grid:not([data-mounted])').forEach((el) => {
>     el.setAttribute('data-mounted', '1');
>     sabelaGrid(el);
>   });
> }
> 
> sabelaGridMountAll();</script>

```haskell
df |> D.describeColumns
```

> <!-- scripths:mime text/html -->
> <div class="sabela-grid"><script type="application/json" class="sabela-grid-data">{"cols":["Column Name","# Non-null Values","# Null Values","Type"],"rows":[["total_bedrooms","20433","207","Maybe Double"],["ocean_proximity","20640","0","Text"],["median_house_value","20640","0","Double"],["median_income","20640","0","Double"],["households","20640","0","Double"],["population","20640","0","Double"],["total_rooms","20640","0","Double"],["housing_median_age","20640","0","Double"],["latitude","20640","0","Double"],["longitude","20640","0","Double"]],"total":10}</script></div><script>// ── Grid model: typing, statistics, formatting, ordering ─────────
> // Pure data work for the table view. No DOM: everything here is a function
> // of the raw {cols, rows} payload the notebook emitted, so it can be reasoned
> // about (and tested) on its own.
> 
> // A column is numeric when every value that is present parses as one. Blank
> // cells do not vote: a column of numbers with gaps is still numeric.
> function sabelaGridKind(rows, i) {
>   let seen = 0;
>   let nums = 0;
>   let ints = 0;
>   let bools = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '' || v === 'null' || v === 'Nothing') continue;
>     seen++;
>     if (v === 'True' || v === 'False') bools++;
>     if (/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(v)) {
>       nums++;
>       if (/^[-+]?\d+$/.test(v)) ints++;
>     }
>   }
>   if (!seen) return 'str';
>   if (bools === seen) return 'bool';
>   if (nums < seen) return 'str';
>   return ints === seen ? 'int' : 'num';
> }
> 
> function sabelaGridNum(v) {
>   const t = (v ?? '').trim();
>   if (t === '' || t === 'null' || t === 'Nothing') return null;
>   const n = Number(t);
>   return Number.isFinite(n) ? n : null;
> }
> 
> // Column statistics, in the two shapes the header strip draws: a histogram
> // for numbers, the leading categories for everything else.
> function sabelaGridStats(rows, i, kind) {
>   const empty = { n: 0, missing: 0 };
>   if (kind === 'num' || kind === 'int') {
>     const vals = [];
>     let missing = 0;
>     for (const row of rows) {
>       const n = sabelaGridNum(row[i]);
>       if (n === null) missing++;
>       else vals.push(n);
>     }
>     if (!vals.length) return { ...empty, missing, kind };
>     let min = vals[0];
>     let max = vals[0];
>     let sum = 0;
>     for (const v of vals) {
>       if (v < min) min = v;
>       if (v > max) max = v;
>       sum += v;
>     }
>     const bins = new Array(24).fill(0);
>     const span = max - min;
>     for (const v of vals) {
>       const b = span === 0 ? 0 : Math.min(23, Math.floor(((v - min) / span) * 24));
>       bins[b]++;
>     }
>     return { kind, n: vals.length, missing, min, max, mean: sum / vals.length, bins };
>   }
>   const counts = new Map();
>   let missing = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '') missing++;
>     counts.set(v, (counts.get(v) || 0) + 1);
>   }
>   const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 6);
>   const n = rows.length - missing;
>   // A column whose every value differs is a key, not a set of categories:
>   // colouring it says nothing, so it is left uncoloured.
>   const categorical = counts.size > 1 && counts.size < n && counts.size <= 12;
>   return { kind, n, missing, unique: counts.size, top, categorical };
> }
> 
> // How many decimals a numeric column needs, capped so a wide column does not
> // print noise. Integers ask for none.
> function sabelaGridDecimals(rows, i, kind) {
>   if (kind === 'int' || kind === 'bool' || kind === 'str') return 0;
>   let want = 0;
>   for (const row of rows) {
>     const t = (row[i] ?? '').trim();
>     const dot = t.indexOf('.');
>     if (dot < 0 || /[eE]/.test(t)) continue;
>     want = Math.max(want, t.length - dot - 1);
>     if (want >= 4) return 4;
>   }
>   return want;
> }
> 
> // Datagrok's adaptive formatting: full precision when the column is wide,
> // degraded — then abbreviated — as it narrows.
> function sabelaGridFormat(v, col, width) {
>   if (col.kind === 'str' || col.kind === 'bool') return v ?? '';
>   const n = sabelaGridNum(v);
>   if (n === null) return v ?? '';
>   const abs = Math.abs(n);
>   if (abs !== 0 && (abs >= 1e9 || abs < 1e-4)) return n.toExponential(2);
>   let decimals = col.decimals;
>   if (width < 88) decimals = Math.min(decimals, 2);
>   if (width < 64) decimals = 0;
>   if (abs >= 1e6 && width < 110) {
>     return (n / 1e6).toFixed(decimals ? 1 : 0) + 'M';
>   }
>   return n.toLocaleString(undefined, {
>     minimumFractionDigits: decimals,
>     maximumFractionDigits: decimals,
>   });
> }
> 
> function sabelaGridColumns(raw) {
>   return raw.cols.map((name, i) => {
>     const kind = sabelaGridKind(raw.rows, i);
>     return {
>       name,
>       index: i,
>       kind,
>       numeric: kind === 'num' || kind === 'int',
>       decimals: sabelaGridDecimals(raw.rows, i, kind),
>       stats: sabelaGridStats(raw.rows, i, kind),
>       width: 0,
>     };
>   });
> }
> 
> // The order rows are shown in: the sort, then the search, both applied to the
> // original row indices so the row header keeps naming the source row.
> function sabelaGridOrder(raw, cols, sort, query) {
>   let order = raw.rows.map((_, n) => n);
>   const test = sabelaGridQuery(query, cols);
>   if (test) order = order.filter((n) => test(raw.rows[n]));
>   if (sort) {
>     const col = cols[sort.col];
>     const dir = sort.desc ? -1 : 1;
>     order.sort((a, b) => {
>       const ma = sabelaGridMissing(raw.rows[a], col);
>       const mb = sabelaGridMissing(raw.rows[b], col);
>       if (ma !== mb) return ma ? 1 : -1;
>       return dir * sabelaGridCompare(raw.rows[a], raw.rows[b], col);
>     });
>   }
>   return order;
> }
> 
> // A value that is not there sorts to the end whichever way the column is
> // sorted: it is absent, not small.
> function sabelaGridMissing(row, col) {
>   const v = row[col.index];
>   return col.numeric ? sabelaGridNum(v) === null : String(v ?? '').trim() === '';
> }
> 
> function sabelaGridCompare(ra, rb, col) {
>   const i = col.index;
>   if (col.numeric) {
>     const a = sabelaGridNum(ra[i]);
>     const b = sabelaGridNum(rb[i]);
>     if (a === null) return b === null ? 0 : 1;
>     if (b === null) return -1;
>     return a - b;
>   }
>   return String(ra[i] ?? '').localeCompare(String(rb[i] ?? ''), undefined, { numeric: true });
> }
> 
> // Search, in Datagrok's two spellings: free text over every cell, or one
> // column compared against a value ("temp > 25", "city = Harare").
> function sabelaGridQuery(query, cols) {
>   const q = (query || '').trim();
>   if (!q) return null;
>   const m = q.match(/^\s*([\w ]+?)\s*(>=|<=|!=|>|<|=)\s*(.+?)\s*$/);
>   if (m) {
>     const col = cols.find((c) => c.name.toLowerCase() === m[1].toLowerCase());
>     if (col) return sabelaGridComparison(col, m[2], m[3]);
>   }
>   const needle = q.toLowerCase();
>   return (row) =>
>     row.some((v) =>
>       String(v ?? '')
>         .toLowerCase()
>         .includes(needle)
>     );
> }
> 
> function sabelaGridComparison(col, op, rhs) {
>   const i = col.index;
>   const num = Number(rhs);
>   const useNum = col.numeric && Number.isFinite(num);
>   return (row) => {
>     const raw = String(row[i] ?? '');
>     const a = useNum ? sabelaGridNum(raw) : raw.toLowerCase();
>     const b = useNum ? num : rhs.toLowerCase();
>     if (a === null) return false;
>     switch (op) {
>       case '>':
>         return a > b;
>       case '<':
>         return a < b;
>       case '>=':
>         return a >= b;
>       case '<=':
>         return a <= b;
>       case '!=':
>         return a !== b;
>       default:
>         return useNum ? a === b : a.includes(b);
>     }
>   };
> }
> 
> // ── Grid chrome: theme, stylesheet, header, distribution strip ───
> // The parts of the table view that are drawn once per render. The body and
> // its interactions live in grid-body.js.
> 
> // The notebook only hands an output iframe three colours, so the rest of the
> // palette is derived from whether the background it gave is dark or light.
> function sabelaGridTheme() {
>   const cs = getComputedStyle(document.body);
>   const m = (cs.backgroundColor || 'rgb(255,255,255)').match(/\d+/g) || [255, 255, 255];
>   const lum = (0.299 * +m[0] + 0.587 * +m[1] + 0.114 * +m[2]) / 255;
>   const dark = lum < 0.5;
>   return {
>     dark,
>     fg: cs.color || (dark ? '#e6e6ef' : '#1e1e2e'),
>     dim: dark ? '#8b8b9c' : '#8a8a99',
>     border: dark ? '#3a3a46' : '#e2e2ea',
>     rule: dark ? '#5a5a68' : '#b9b9c6',
>     zebra: dark ? 'rgba(255,255,255,0.028)' : 'rgba(0,0,0,0.022)',
>     hover: dark ? 'rgba(130,175,255,0.13)' : 'rgba(60,120,240,0.08)',
>     current: '#3aa657',
>     selected: dark ? 'rgba(255,170,70,0.20)' : 'rgba(255,165,60,0.22)',
>     scale: dark ? '130,175,255' : '60,120,240',
>   };
> }
> 
> function sabelaGridStyle(t) {
>   if (document.getElementById('sabela-grid-style')) return;
>   const s = document.createElement('style');
>   s.id = 'sabela-grid-style';
>   s.textContent = `
> .sg{font-family:inherit;font-size:11.5px;color:${t.fg};max-width:100%}
> .sg-bar{display:flex;align-items:center;gap:8px;padding:0 0 5px;color:${t.dim};font-size:11px}
> .sg-search{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.fg};
>   font-size:11px;padding:2px 9px;width:170px;cursor:text}
> .sg-search:focus{outline:none;border-color:rgba(${t.scale},0.8)}
> .sg-btn{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.dim};
>   font-size:10.5px;padding:2px 9px}
> .sg-btn[aria-pressed=true]{color:${t.fg};border-color:rgba(${t.scale},0.7)}
> .sg-scroll{overflow:auto;border-top:1px solid ${t.border};border-bottom:1px solid ${t.border};
>   max-height:420px;position:relative}
> .sg table{border-collapse:separate;border-spacing:0;table-layout:fixed;width:max-content;max-width:100%}
> .sg th,.sg td{border-right:1px solid ${t.border};padding:2px 7px;white-space:nowrap;overflow:hidden;
>   text-overflow:ellipsis;height:21px;box-sizing:border-box}
> .sg thead th{position:sticky;top:0;z-index:2;background:${t.dark ? '#20202a' : '#fbfbfd'};
>   border-bottom:1px solid ${t.rule};font-weight:500;text-align:left;user-select:none;cursor:pointer}
> .sg thead tr.sg-strip th{top:25px;z-index:1;padding:0;height:15px;cursor:default;opacity:.72;
>   border-bottom:1px solid ${t.border}}
> .sg .sg-idx{color:${t.dim};text-align:right;width:46px;position:sticky;left:0;z-index:1;
>   background:${t.dark ? '#20202a' : '#fbfbfd'};cursor:pointer}
> .sg thead .sg-idx{z-index:3}
> .sg-name{display:inline-block;max-width:calc(100% - 30px);overflow:hidden;text-overflow:ellipsis;
>   vertical-align:bottom}
> .sg-type{color:${t.dim};font-size:9px;margin-left:4px;letter-spacing:.4px}
> .sg-caret{color:rgba(${t.scale},1);margin-left:3px}
> .sg-resize{position:absolute;top:0;right:-3px;width:7px;height:100%;cursor:col-resize;z-index:4}
> .sg tbody tr:nth-child(even) td{background:${t.zebra}}
> .sg tbody tr:hover td{background:${t.hover}}
> .sg tbody tr.sg-sel td{background:${t.selected}}
> .sg tbody tr.sg-cur td{box-shadow:inset 0 -1px 0 ${t.current},inset 0 1px 0 ${t.current}}
> .sg tbody tr.sg-cur td.sg-idx{box-shadow:inset 3px 0 0 ${t.current},inset 0 -1px 0 ${t.current},
>   inset 0 1px 0 ${t.current}}
> .sg-num{text-align:right;font-variant-numeric:tabular-nums}
> .sg-chip{display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:5px;
>   vertical-align:baseline}
> .sg-foot{color:${t.dim};font-size:10.5px;padding-top:5px;display:flex;gap:10px;flex-wrap:wrap}
> .sg-stat{color:${t.dim};font-size:10px}
> `;
>   document.head.appendChild(s);
> }
> 
> var SABELA_GRID_TYPE = { num: '#', int: '#', str: 'abc', bool: '✓' };
> 
> // A column's distribution, drawn under its name: bars over the value range
> // for a number, the leading categories in proportion for anything else.
> function sabelaGridStrip(col, t) {
>   const st = col.stats;
>   const ns = 'http://www.w3.org/2000/svg';
>   const svg = document.createElementNS(ns, 'svg');
>   svg.setAttribute('preserveAspectRatio', 'none');
>   svg.setAttribute('viewBox', '0 0 100 14');
>   svg.style.cssText = 'width:100%;height:14px;display:block';
>   const add = (x, y, w, h, fill, title) => {
>     const r = document.createElementNS(ns, 'rect');
>     r.setAttribute('x', x);
>     r.setAttribute('y', y);
>     r.setAttribute('width', Math.max(0.6, w));
>     r.setAttribute('height', Math.max(0.8, h));
>     r.setAttribute('fill', fill);
>     if (title) {
>       const el = document.createElementNS(ns, 'title');
>       el.textContent = title;
>       r.appendChild(el);
>     }
>     svg.appendChild(r);
>   };
>   if (col.numeric && st.bins) {
>     const peak = Math.max(...st.bins, 1);
>     const w = 100 / st.bins.length;
>     st.bins.forEach((n, i) => {
>       if (!n) return;
>       const h = (n / peak) * 12;
>       add(i * w, 13 - h, w - 0.5, h, `rgba(${t.scale},0.5)`, n + ' rows');
>     });
>   } else if (st.categorical && st.top) {
>     let x = 0;
>     st.top.forEach((entry) => {
>       const w = (entry[1] / st.n) * 100;
>       add(x, 4, w - 0.4, 7, sabelaGridChipColor(entry[0], t), entry[0] + ' — ' + entry[1]);
>       x += w;
>     });
>     if (x < 99.5) add(x, 4, 100 - x, 7, t.border, 'other');
>   }
>   return svg;
> }
> 
> // A stable colour per category, so the same value reads the same everywhere
> // in the grid.
> function sabelaGridChipColor(v, t) {
>   let h = 0;
>   const s = String(v);
>   for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360;
>   return `hsl(${h},${t.dark ? 45 : 55}%,${t.dark ? 52 : 60}%)`;
> }
> 
> function sabelaGridHeader(state, refresh) {
>   const t = state.theme;
>   const thead = document.createElement('thead');
>   const names = document.createElement('tr');
>   const strip = document.createElement('tr');
>   strip.className = 'sg-strip';
>   const corner = document.createElement('th');
>   corner.className = 'sg-idx';
>   corner.title = 'clear selection';
>   corner.onclick = () => {
>     state.selected.clear();
>     state.current = -1;
>     refresh();
>   };
>   names.appendChild(corner);
>   const stripCorner = document.createElement('th');
>   stripCorner.className = 'sg-idx';
>   strip.appendChild(stripCorner);
>   state.cols.forEach((col) => {
>     names.appendChild(sabelaGridHeadCell(state, col, refresh));
>     const sth = document.createElement('th');
>     sth.appendChild(sabelaGridStrip(col, t));
>     strip.appendChild(sth);
>   });
>   thead.appendChild(names);
>   thead.appendChild(strip);
>   return thead;
> }
> 
> function sabelaGridHeadCell(state, col, refresh) {
>   const th = document.createElement('th');
>   th.style.position = 'relative';
>   th.title = sabelaGridSummary(col);
>   const name = document.createElement('span');
>   name.className = 'sg-name';
>   name.textContent = col.name;
>   th.appendChild(name);
>   const type = document.createElement('span');
>   type.className = 'sg-type';
>   type.textContent = SABELA_GRID_TYPE[col.kind] || '';
>   th.appendChild(type);
>   if (state.sort && state.sort.col === col.index) {
>     const caret = document.createElement('span');
>     caret.className = 'sg-caret';
>     caret.textContent = state.sort.desc ? '↓' : '↑';
>     th.appendChild(caret);
>   }
>   th.onclick = () => {
>     const on = state.sort && state.sort.col === col.index;
>     state.sort =
>       on && !state.sort.desc
>         ? { col: col.index, desc: true }
>         : on
>           ? null
>           : { col: col.index, desc: false };
>     refresh();
>   };
>   const grip = document.createElement('div');
>   grip.className = 'sg-resize';
>   grip.onclick = (e) => e.stopPropagation();
>   grip.onmousedown = (e) => sabelaGridDragWidth(e, state, col, refresh);
>   th.appendChild(grip);
>   return th;
> }
> 
> function sabelaGridDragWidth(e, state, col, refresh) {
>   e.preventDefault();
>   const startX = e.clientX;
>   const startW = col.width;
>   const move = (ev) => {
>     col.width = Math.max(46, startW + (ev.clientX - startX));
>     refresh();
>   };
>   const up = () => {
>     window.removeEventListener('mousemove', move);
>     window.removeEventListener('mouseup', up);
>   };
>   window.addEventListener('mousemove', move);
>   window.addEventListener('mouseup', up);
> }
> 
> // What a column header says about itself on hover, and what the stats line
> // under the grid repeats for every column at once.
> function sabelaGridSummary(col) {
>   const st = col.stats;
>   const miss = st.missing ? ', ' + st.missing + ' missing' : '';
>   if (col.numeric && st.n) {
>     const f = (v) => sabelaGridFormat(String(v), col, 999);
>     return `${col.name}: min ${f(st.min)}, mean ${f(st.mean)}, max ${f(st.max)}${miss}`;
>   }
>   return `${col.name}: ${st.unique || 0} unique${miss}`;
> }
> 
> // ── Grid body: rows, colour coding, selection, and the entry point ──
> // Only the rows in view are in the DOM; the rest is spacer height, so a
> // table of thousands of rows costs the same to scroll as a short one.
> // `var`, not `const`: an output iframe is reused across re-runs, so this
> // script is evaluated again in a window that already holds these names.
> 
> var SABELA_GRID_ROW_H = 21;
> var SABELA_GRID_WINDOW = 60;
> 
> function sabelaGridCellText(col, raw, width) {
>   return sabelaGridFormat(raw, col, width);
> }
> 
> // Datagrok's linear scheme: a number's cell carries its place in the column's
> // range as background weight.
> function sabelaGridPaint(td, col, raw, state) {
>   if (!state.color) return;
>   const st = col.stats;
>   if (col.numeric && st.n && st.max !== st.min) {
>     const n = sabelaGridNum(raw);
>     if (n === null) return;
>     const w = (n - st.min) / (st.max - st.min);
>     td.style.background = `rgba(${state.theme.scale},${(0.05 + 0.32 * w).toFixed(3)})`;
>   } else if (!col.numeric && st.categorical && String(raw ?? '').trim() !== '') {
>     const chip = document.createElement('span');
>     chip.className = 'sg-chip';
>     chip.style.background = sabelaGridChipColor(raw, state.theme);
>     td.insertBefore(chip, td.firstChild);
>   }
> }
> 
> function sabelaGridRow(state, n, position) {
>   const raw = state.raw.rows[n];
>   const tr = document.createElement('tr');
>   if (state.selected.has(n)) tr.classList.add('sg-sel');
>   if (state.current === n) tr.classList.add('sg-cur');
>   const idx = document.createElement('td');
>   idx.className = 'sg-idx';
>   idx.textContent = position;
>   tr.appendChild(idx);
>   state.cols.forEach((col) => {
>     const td = document.createElement('td');
>     const text = sabelaGridCellText(col, raw[col.index], col.width);
>     td.textContent = text;
>     td.title = String(raw[col.index] ?? '');
>     if (col.numeric) td.className = 'sg-num';
>     sabelaGridPaint(td, col, raw[col.index], state);
>     tr.appendChild(td);
>   });
>   tr.onclick = (e) => sabelaGridPick(state, n, e);
>   return tr;
> }
> 
> // A click makes a row current; ctrl or shift add to the selection, the way a
> // spreadsheet does.
> function sabelaGridPick(state, n, e) {
>   if (e.shiftKey && state.current >= 0) {
>     const from = state.order.indexOf(state.current);
>     const to = state.order.indexOf(n);
>     if (from >= 0 && to >= 0) {
>       const [lo, hi] = from < to ? [from, to] : [to, from];
>       for (let i = lo; i <= hi; i++) state.selected.add(state.order[i]);
>     }
>   } else if (e.ctrlKey || e.metaKey) {
>     if (!state.selected.delete(n)) state.selected.add(n);
>   } else {
>     state.selected.clear();
>   }
>   state.current = n;
>   state.repaint();
> }
> 
> function sabelaGridBody(state, tbody, scroll) {
>   const total = state.order.length;
>   const first = Math.max(0, Math.floor(scroll.scrollTop / SABELA_GRID_ROW_H) - 8);
>   const last = Math.min(total, first + SABELA_GRID_WINDOW);
>   tbody.textContent = '';
>   const spacer = (h) => {
>     const tr = document.createElement('tr');
>     const td = document.createElement('td');
>     td.colSpan = state.cols.length + 1;
>     td.style.cssText = `height:${h}px;padding:0;border:none`;
>     tr.appendChild(td);
>     return tr;
>   };
>   if (first) tbody.appendChild(spacer(first * SABELA_GRID_ROW_H));
>   for (let i = first; i < last; i++) {
>     tbody.appendChild(sabelaGridRow(state, state.order[i], state.order[i] + 1));
>   }
>   if (last < total) tbody.appendChild(spacer((total - last) * SABELA_GRID_ROW_H));
> }
> 
> // Enough width for the values and for the header's own furniture — the name,
> // its type, and the room a sort caret will want.
> function sabelaGridWidth(col, rows) {
>   let n = 0;
>   for (let i = 0; i < rows.length && i < 80; i++) {
>     n = Math.max(n, String(rows[i][col.index] ?? '').length);
>   }
>   const header = col.name.length * 7 + 38;
>   return Math.round(Math.min(260, Math.max(56, header, 16 + n * 6.8)));
> }
> 
> // Columns are laid out from their content, then opened up to whatever width
> // the cell actually gave us, so a narrow table does not sit in a wide gap.
> function sabelaGridFit(state, scroll, refresh) {
>   const avail = scroll.clientWidth - 48;
>   if (avail <= 0) return;
>   const total = state.cols.reduce((sum, c) => sum + c.width, 0);
>   if (total >= avail) return;
>   const grow = avail / total;
>   state.cols.forEach((c) => {
>     c.width = Math.min(360, Math.round(c.width * grow));
>   });
>   refresh();
> }
> 
> function sabelaGridToolbar(state, refresh) {
>   const bar = document.createElement('div');
>   bar.className = 'sg-bar';
>   const search = document.createElement('input');
>   search.className = 'sg-search';
>   search.type = 'text';
>   const numeric = state.cols.find((c) => c.numeric);
>   search.placeholder = numeric ? `search  ·  ${numeric.name} > 0` : 'search';
>   search.oninput = () => {
>     state.query = search.value;
>     refresh();
>   };
>   bar.appendChild(search);
>   const toggle = (label, key, title) => {
>     const b = document.createElement('button');
>     b.className = 'sg-btn';
>     b.type = 'button';
>     b.textContent = label;
>     b.title = title;
>     b.setAttribute('aria-pressed', String(state[key]));
>     b.onclick = () => {
>       state[key] = !state[key];
>       b.setAttribute('aria-pressed', String(state[key]));
>       refresh();
>     };
>     bar.appendChild(b);
>   };
>   toggle('color', 'color', 'colour cells by value');
>   toggle('stats', 'stats', 'show column statistics');
>   return bar;
> }
> 
> function sabelaGridFooter(state) {
>   const foot = document.createElement('div');
>   foot.className = 'sg-foot';
>   const shown = state.order.length;
>   const carried = state.raw.rows.length;
>   // The rows the page holds, and the rows the data has: a capped table shows
>   // fewer than it counts, and a search fewer still.
>   const total = state.raw.total ?? carried;
>   const rows =
>     total > carried
>       ? shown === carried
>         ? `first ${carried} of ${total} rows`
>         : `${shown} of first ${carried} · ${total} rows`
>       : shown === total
>         ? `${total} rows`
>         : `${shown} of ${total} rows`;
>   const cols = document.createElement('span');
>   cols.textContent = `${rows} × ${state.cols.length} columns`;
>   foot.appendChild(cols);
>   if (state.selected.size) {
>     const sel = document.createElement('span');
>     sel.textContent = `${state.selected.size} selected`;
>     foot.appendChild(sel);
>   }
>   if (state.stats) {
>     state.cols.forEach((col) => {
>       const s = document.createElement('span');
>       s.className = 'sg-stat';
>       s.textContent = sabelaGridSummary(col);
>       foot.appendChild(s);
>     });
>   }
>   return foot;
> }
> 
> // Turns one emitted payload into a live grid. Called once per table, with the
> // container the notebook printed.
> function sabelaGrid(container) {
>   const holder = container.querySelector('script.sabela-grid-data');
>   const raw = JSON.parse(holder.textContent);
>   const theme = sabelaGridTheme();
>   sabelaGridStyle(theme);
>   const cols = sabelaGridColumns(raw);
>   cols.forEach((col) => {
>     col.width = sabelaGridWidth(col, raw.rows);
>   });
>   const state = {
>     raw,
>     cols,
>     theme,
>     sort: null,
>     query: '',
>     color: true,
>     stats: false,
>     current: -1,
>     selected: new Set(),
>     order: raw.rows.map((_, n) => n),
>   };
>   container.className = 'sg';
>   container.textContent = '';
>   const scroll = document.createElement('div');
>   scroll.className = 'sg-scroll';
>   const table = document.createElement('table');
>   const group = document.createElement('colgroup');
>   const tbody = document.createElement('tbody');
>   scroll.appendChild(table);
> 
>   const paintBody = () => sabelaGridBody(state, tbody, scroll);
>   const refresh = () => {
>     state.order = sabelaGridOrder(raw, cols, state.sort, state.query);
>     group.textContent = '';
>     [46, ...cols.map((c) => c.width)].forEach((w) => {
>       const col = document.createElement('col');
>       col.style.width = w + 'px';
>       group.appendChild(col);
>     });
>     table.textContent = '';
>     table.appendChild(group);
>     table.appendChild(sabelaGridHeader(state, refresh));
>     table.appendChild(tbody);
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   state.repaint = () => {
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   scroll.addEventListener('scroll', paintBody);
>   container.appendChild(sabelaGridToolbar(state, refresh));
>   container.appendChild(scroll);
>   container.appendChild(sabelaGridFooter(state));
>   refresh();
>   if (scroll.clientWidth) sabelaGridFit(state, scroll, refresh);
>   else requestAnimationFrame(() => sabelaGridFit(state, scroll, refresh));
> }
> 
> // Every grid the page printed, mounted the moment its markup exists.
> function sabelaGridMountAll() {
>   document.querySelectorAll('div.sabela-grid:not([data-mounted])').forEach((el) => {
>     el.setAttribute('data-mounted', '1');
>     sabelaGrid(el);
>   });
> }
> 
> sabelaGridMountAll();</script>

```haskell
:set -XDataKinds
:set -XTemplateHaskell
:set -XTypeApplications

$(DT.deriveSchema "Housing" df)

tdf = either (error . show) id (DT.freezeWithError @Housing df)
```

```haskell
tdf |> DT.take 5
    |> DT.thaw
```

> <!-- scripths:mime text/html -->
> <div class="sabela-grid"><script type="application/json" class="sabela-grid-data">{"cols":["longitude","latitude","housing_median_age","total_rooms","total_bedrooms","population","households","median_income","median_house_value","ocean_proximity"],"rows":[["-122.23","37.88","41.0","880.0","129.0","322.0","126.0","8.3252","452600.0","NEAR BAY"],["-122.22","37.86","21.0","7099.0","1106.0","2401.0","1138.0","8.3014","358500.0","NEAR BAY"],["-122.24","37.85","52.0","1467.0","190.0","496.0","177.0","7.2574","352100.0","NEAR BAY"],["-122.25","37.85","52.0","1274.0","235.0","558.0","219.0","5.6431000000000004","341300.0","NEAR BAY"],["-122.25","37.85","52.0","1627.0","280.0","565.0","259.0","3.8462","342200.0","NEAR BAY"]],"total":5}</script></div><script>// ── Grid model: typing, statistics, formatting, ordering ─────────
> // Pure data work for the table view. No DOM: everything here is a function
> // of the raw {cols, rows} payload the notebook emitted, so it can be reasoned
> // about (and tested) on its own.
> 
> // A column is numeric when every value that is present parses as one. Blank
> // cells do not vote: a column of numbers with gaps is still numeric.
> function sabelaGridKind(rows, i) {
>   let seen = 0;
>   let nums = 0;
>   let ints = 0;
>   let bools = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '' || v === 'null' || v === 'Nothing') continue;
>     seen++;
>     if (v === 'True' || v === 'False') bools++;
>     if (/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(v)) {
>       nums++;
>       if (/^[-+]?\d+$/.test(v)) ints++;
>     }
>   }
>   if (!seen) return 'str';
>   if (bools === seen) return 'bool';
>   if (nums < seen) return 'str';
>   return ints === seen ? 'int' : 'num';
> }
> 
> function sabelaGridNum(v) {
>   const t = (v ?? '').trim();
>   if (t === '' || t === 'null' || t === 'Nothing') return null;
>   const n = Number(t);
>   return Number.isFinite(n) ? n : null;
> }
> 
> // Column statistics, in the two shapes the header strip draws: a histogram
> // for numbers, the leading categories for everything else.
> function sabelaGridStats(rows, i, kind) {
>   const empty = { n: 0, missing: 0 };
>   if (kind === 'num' || kind === 'int') {
>     const vals = [];
>     let missing = 0;
>     for (const row of rows) {
>       const n = sabelaGridNum(row[i]);
>       if (n === null) missing++;
>       else vals.push(n);
>     }
>     if (!vals.length) return { ...empty, missing, kind };
>     let min = vals[0];
>     let max = vals[0];
>     let sum = 0;
>     for (const v of vals) {
>       if (v < min) min = v;
>       if (v > max) max = v;
>       sum += v;
>     }
>     const bins = new Array(24).fill(0);
>     const span = max - min;
>     for (const v of vals) {
>       const b = span === 0 ? 0 : Math.min(23, Math.floor(((v - min) / span) * 24));
>       bins[b]++;
>     }
>     return { kind, n: vals.length, missing, min, max, mean: sum / vals.length, bins };
>   }
>   const counts = new Map();
>   let missing = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '') missing++;
>     counts.set(v, (counts.get(v) || 0) + 1);
>   }
>   const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 6);
>   const n = rows.length - missing;
>   // A column whose every value differs is a key, not a set of categories:
>   // colouring it says nothing, so it is left uncoloured.
>   const categorical = counts.size > 1 && counts.size < n && counts.size <= 12;
>   return { kind, n, missing, unique: counts.size, top, categorical };
> }
> 
> // How many decimals a numeric column needs, capped so a wide column does not
> // print noise. Integers ask for none.
> function sabelaGridDecimals(rows, i, kind) {
>   if (kind === 'int' || kind === 'bool' || kind === 'str') return 0;
>   let want = 0;
>   for (const row of rows) {
>     const t = (row[i] ?? '').trim();
>     const dot = t.indexOf('.');
>     if (dot < 0 || /[eE]/.test(t)) continue;
>     want = Math.max(want, t.length - dot - 1);
>     if (want >= 4) return 4;
>   }
>   return want;
> }
> 
> // Datagrok's adaptive formatting: full precision when the column is wide,
> // degraded — then abbreviated — as it narrows.
> function sabelaGridFormat(v, col, width) {
>   if (col.kind === 'str' || col.kind === 'bool') return v ?? '';
>   const n = sabelaGridNum(v);
>   if (n === null) return v ?? '';
>   const abs = Math.abs(n);
>   if (abs !== 0 && (abs >= 1e9 || abs < 1e-4)) return n.toExponential(2);
>   let decimals = col.decimals;
>   if (width < 88) decimals = Math.min(decimals, 2);
>   if (width < 64) decimals = 0;
>   if (abs >= 1e6 && width < 110) {
>     return (n / 1e6).toFixed(decimals ? 1 : 0) + 'M';
>   }
>   return n.toLocaleString(undefined, {
>     minimumFractionDigits: decimals,
>     maximumFractionDigits: decimals,
>   });
> }
> 
> function sabelaGridColumns(raw) {
>   return raw.cols.map((name, i) => {
>     const kind = sabelaGridKind(raw.rows, i);
>     return {
>       name,
>       index: i,
>       kind,
>       numeric: kind === 'num' || kind === 'int',
>       decimals: sabelaGridDecimals(raw.rows, i, kind),
>       stats: sabelaGridStats(raw.rows, i, kind),
>       width: 0,
>     };
>   });
> }
> 
> // The order rows are shown in: the sort, then the search, both applied to the
> // original row indices so the row header keeps naming the source row.
> function sabelaGridOrder(raw, cols, sort, query) {
>   let order = raw.rows.map((_, n) => n);
>   const test = sabelaGridQuery(query, cols);
>   if (test) order = order.filter((n) => test(raw.rows[n]));
>   if (sort) {
>     const col = cols[sort.col];
>     const dir = sort.desc ? -1 : 1;
>     order.sort((a, b) => {
>       const ma = sabelaGridMissing(raw.rows[a], col);
>       const mb = sabelaGridMissing(raw.rows[b], col);
>       if (ma !== mb) return ma ? 1 : -1;
>       return dir * sabelaGridCompare(raw.rows[a], raw.rows[b], col);
>     });
>   }
>   return order;
> }
> 
> // A value that is not there sorts to the end whichever way the column is
> // sorted: it is absent, not small.
> function sabelaGridMissing(row, col) {
>   const v = row[col.index];
>   return col.numeric ? sabelaGridNum(v) === null : String(v ?? '').trim() === '';
> }
> 
> function sabelaGridCompare(ra, rb, col) {
>   const i = col.index;
>   if (col.numeric) {
>     const a = sabelaGridNum(ra[i]);
>     const b = sabelaGridNum(rb[i]);
>     if (a === null) return b === null ? 0 : 1;
>     if (b === null) return -1;
>     return a - b;
>   }
>   return String(ra[i] ?? '').localeCompare(String(rb[i] ?? ''), undefined, { numeric: true });
> }
> 
> // Search, in Datagrok's two spellings: free text over every cell, or one
> // column compared against a value ("temp > 25", "city = Harare").
> function sabelaGridQuery(query, cols) {
>   const q = (query || '').trim();
>   if (!q) return null;
>   const m = q.match(/^\s*([\w ]+?)\s*(>=|<=|!=|>|<|=)\s*(.+?)\s*$/);
>   if (m) {
>     const col = cols.find((c) => c.name.toLowerCase() === m[1].toLowerCase());
>     if (col) return sabelaGridComparison(col, m[2], m[3]);
>   }
>   const needle = q.toLowerCase();
>   return (row) =>
>     row.some((v) =>
>       String(v ?? '')
>         .toLowerCase()
>         .includes(needle)
>     );
> }
> 
> function sabelaGridComparison(col, op, rhs) {
>   const i = col.index;
>   const num = Number(rhs);
>   const useNum = col.numeric && Number.isFinite(num);
>   return (row) => {
>     const raw = String(row[i] ?? '');
>     const a = useNum ? sabelaGridNum(raw) : raw.toLowerCase();
>     const b = useNum ? num : rhs.toLowerCase();
>     if (a === null) return false;
>     switch (op) {
>       case '>':
>         return a > b;
>       case '<':
>         return a < b;
>       case '>=':
>         return a >= b;
>       case '<=':
>         return a <= b;
>       case '!=':
>         return a !== b;
>       default:
>         return useNum ? a === b : a.includes(b);
>     }
>   };
> }
> 
> // ── Grid chrome: theme, stylesheet, header, distribution strip ───
> // The parts of the table view that are drawn once per render. The body and
> // its interactions live in grid-body.js.
> 
> // The notebook only hands an output iframe three colours, so the rest of the
> // palette is derived from whether the background it gave is dark or light.
> function sabelaGridTheme() {
>   const cs = getComputedStyle(document.body);
>   const m = (cs.backgroundColor || 'rgb(255,255,255)').match(/\d+/g) || [255, 255, 255];
>   const lum = (0.299 * +m[0] + 0.587 * +m[1] + 0.114 * +m[2]) / 255;
>   const dark = lum < 0.5;
>   return {
>     dark,
>     fg: cs.color || (dark ? '#e6e6ef' : '#1e1e2e'),
>     dim: dark ? '#8b8b9c' : '#8a8a99',
>     border: dark ? '#3a3a46' : '#e2e2ea',
>     rule: dark ? '#5a5a68' : '#b9b9c6',
>     zebra: dark ? 'rgba(255,255,255,0.028)' : 'rgba(0,0,0,0.022)',
>     hover: dark ? 'rgba(130,175,255,0.13)' : 'rgba(60,120,240,0.08)',
>     current: '#3aa657',
>     selected: dark ? 'rgba(255,170,70,0.20)' : 'rgba(255,165,60,0.22)',
>     scale: dark ? '130,175,255' : '60,120,240',
>   };
> }
> 
> function sabelaGridStyle(t) {
>   if (document.getElementById('sabela-grid-style')) return;
>   const s = document.createElement('style');
>   s.id = 'sabela-grid-style';
>   s.textContent = `
> .sg{font-family:inherit;font-size:11.5px;color:${t.fg};max-width:100%}
> .sg-bar{display:flex;align-items:center;gap:8px;padding:0 0 5px;color:${t.dim};font-size:11px}
> .sg-search{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.fg};
>   font-size:11px;padding:2px 9px;width:170px;cursor:text}
> .sg-search:focus{outline:none;border-color:rgba(${t.scale},0.8)}
> .sg-btn{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.dim};
>   font-size:10.5px;padding:2px 9px}
> .sg-btn[aria-pressed=true]{color:${t.fg};border-color:rgba(${t.scale},0.7)}
> .sg-scroll{overflow:auto;border-top:1px solid ${t.border};border-bottom:1px solid ${t.border};
>   max-height:420px;position:relative}
> .sg table{border-collapse:separate;border-spacing:0;table-layout:fixed;width:max-content;max-width:100%}
> .sg th,.sg td{border-right:1px solid ${t.border};padding:2px 7px;white-space:nowrap;overflow:hidden;
>   text-overflow:ellipsis;height:21px;box-sizing:border-box}
> .sg thead th{position:sticky;top:0;z-index:2;background:${t.dark ? '#20202a' : '#fbfbfd'};
>   border-bottom:1px solid ${t.rule};font-weight:500;text-align:left;user-select:none;cursor:pointer}
> .sg thead tr.sg-strip th{top:25px;z-index:1;padding:0;height:15px;cursor:default;opacity:.72;
>   border-bottom:1px solid ${t.border}}
> .sg .sg-idx{color:${t.dim};text-align:right;width:46px;position:sticky;left:0;z-index:1;
>   background:${t.dark ? '#20202a' : '#fbfbfd'};cursor:pointer}
> .sg thead .sg-idx{z-index:3}
> .sg-name{display:inline-block;max-width:calc(100% - 30px);overflow:hidden;text-overflow:ellipsis;
>   vertical-align:bottom}
> .sg-type{color:${t.dim};font-size:9px;margin-left:4px;letter-spacing:.4px}
> .sg-caret{color:rgba(${t.scale},1);margin-left:3px}
> .sg-resize{position:absolute;top:0;right:-3px;width:7px;height:100%;cursor:col-resize;z-index:4}
> .sg tbody tr:nth-child(even) td{background:${t.zebra}}
> .sg tbody tr:hover td{background:${t.hover}}
> .sg tbody tr.sg-sel td{background:${t.selected}}
> .sg tbody tr.sg-cur td{box-shadow:inset 0 -1px 0 ${t.current},inset 0 1px 0 ${t.current}}
> .sg tbody tr.sg-cur td.sg-idx{box-shadow:inset 3px 0 0 ${t.current},inset 0 -1px 0 ${t.current},
>   inset 0 1px 0 ${t.current}}
> .sg-num{text-align:right;font-variant-numeric:tabular-nums}
> .sg-chip{display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:5px;
>   vertical-align:baseline}
> .sg-foot{color:${t.dim};font-size:10.5px;padding-top:5px;display:flex;gap:10px;flex-wrap:wrap}
> .sg-stat{color:${t.dim};font-size:10px}
> `;
>   document.head.appendChild(s);
> }
> 
> var SABELA_GRID_TYPE = { num: '#', int: '#', str: 'abc', bool: '✓' };
> 
> // A column's distribution, drawn under its name: bars over the value range
> // for a number, the leading categories in proportion for anything else.
> function sabelaGridStrip(col, t) {
>   const st = col.stats;
>   const ns = 'http://www.w3.org/2000/svg';
>   const svg = document.createElementNS(ns, 'svg');
>   svg.setAttribute('preserveAspectRatio', 'none');
>   svg.setAttribute('viewBox', '0 0 100 14');
>   svg.style.cssText = 'width:100%;height:14px;display:block';
>   const add = (x, y, w, h, fill, title) => {
>     const r = document.createElementNS(ns, 'rect');
>     r.setAttribute('x', x);
>     r.setAttribute('y', y);
>     r.setAttribute('width', Math.max(0.6, w));
>     r.setAttribute('height', Math.max(0.8, h));
>     r.setAttribute('fill', fill);
>     if (title) {
>       const el = document.createElementNS(ns, 'title');
>       el.textContent = title;
>       r.appendChild(el);
>     }
>     svg.appendChild(r);
>   };
>   if (col.numeric && st.bins) {
>     const peak = Math.max(...st.bins, 1);
>     const w = 100 / st.bins.length;
>     st.bins.forEach((n, i) => {
>       if (!n) return;
>       const h = (n / peak) * 12;
>       add(i * w, 13 - h, w - 0.5, h, `rgba(${t.scale},0.5)`, n + ' rows');
>     });
>   } else if (st.categorical && st.top) {
>     let x = 0;
>     st.top.forEach((entry) => {
>       const w = (entry[1] / st.n) * 100;
>       add(x, 4, w - 0.4, 7, sabelaGridChipColor(entry[0], t), entry[0] + ' — ' + entry[1]);
>       x += w;
>     });
>     if (x < 99.5) add(x, 4, 100 - x, 7, t.border, 'other');
>   }
>   return svg;
> }
> 
> // A stable colour per category, so the same value reads the same everywhere
> // in the grid.
> function sabelaGridChipColor(v, t) {
>   let h = 0;
>   const s = String(v);
>   for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360;
>   return `hsl(${h},${t.dark ? 45 : 55}%,${t.dark ? 52 : 60}%)`;
> }
> 
> function sabelaGridHeader(state, refresh) {
>   const t = state.theme;
>   const thead = document.createElement('thead');
>   const names = document.createElement('tr');
>   const strip = document.createElement('tr');
>   strip.className = 'sg-strip';
>   const corner = document.createElement('th');
>   corner.className = 'sg-idx';
>   corner.title = 'clear selection';
>   corner.onclick = () => {
>     state.selected.clear();
>     state.current = -1;
>     refresh();
>   };
>   names.appendChild(corner);
>   const stripCorner = document.createElement('th');
>   stripCorner.className = 'sg-idx';
>   strip.appendChild(stripCorner);
>   state.cols.forEach((col) => {
>     names.appendChild(sabelaGridHeadCell(state, col, refresh));
>     const sth = document.createElement('th');
>     sth.appendChild(sabelaGridStrip(col, t));
>     strip.appendChild(sth);
>   });
>   thead.appendChild(names);
>   thead.appendChild(strip);
>   return thead;
> }
> 
> function sabelaGridHeadCell(state, col, refresh) {
>   const th = document.createElement('th');
>   th.style.position = 'relative';
>   th.title = sabelaGridSummary(col);
>   const name = document.createElement('span');
>   name.className = 'sg-name';
>   name.textContent = col.name;
>   th.appendChild(name);
>   const type = document.createElement('span');
>   type.className = 'sg-type';
>   type.textContent = SABELA_GRID_TYPE[col.kind] || '';
>   th.appendChild(type);
>   if (state.sort && state.sort.col === col.index) {
>     const caret = document.createElement('span');
>     caret.className = 'sg-caret';
>     caret.textContent = state.sort.desc ? '↓' : '↑';
>     th.appendChild(caret);
>   }
>   th.onclick = () => {
>     const on = state.sort && state.sort.col === col.index;
>     state.sort =
>       on && !state.sort.desc
>         ? { col: col.index, desc: true }
>         : on
>           ? null
>           : { col: col.index, desc: false };
>     refresh();
>   };
>   const grip = document.createElement('div');
>   grip.className = 'sg-resize';
>   grip.onclick = (e) => e.stopPropagation();
>   grip.onmousedown = (e) => sabelaGridDragWidth(e, state, col, refresh);
>   th.appendChild(grip);
>   return th;
> }
> 
> function sabelaGridDragWidth(e, state, col, refresh) {
>   e.preventDefault();
>   const startX = e.clientX;
>   const startW = col.width;
>   const move = (ev) => {
>     col.width = Math.max(46, startW + (ev.clientX - startX));
>     refresh();
>   };
>   const up = () => {
>     window.removeEventListener('mousemove', move);
>     window.removeEventListener('mouseup', up);
>   };
>   window.addEventListener('mousemove', move);
>   window.addEventListener('mouseup', up);
> }
> 
> // What a column header says about itself on hover, and what the stats line
> // under the grid repeats for every column at once.
> function sabelaGridSummary(col) {
>   const st = col.stats;
>   const miss = st.missing ? ', ' + st.missing + ' missing' : '';
>   if (col.numeric && st.n) {
>     const f = (v) => sabelaGridFormat(String(v), col, 999);
>     return `${col.name}: min ${f(st.min)}, mean ${f(st.mean)}, max ${f(st.max)}${miss}`;
>   }
>   return `${col.name}: ${st.unique || 0} unique${miss}`;
> }
> 
> // ── Grid body: rows, colour coding, selection, and the entry point ──
> // Only the rows in view are in the DOM; the rest is spacer height, so a
> // table of thousands of rows costs the same to scroll as a short one.
> // `var`, not `const`: an output iframe is reused across re-runs, so this
> // script is evaluated again in a window that already holds these names.
> 
> var SABELA_GRID_ROW_H = 21;
> var SABELA_GRID_WINDOW = 60;
> 
> function sabelaGridCellText(col, raw, width) {
>   return sabelaGridFormat(raw, col, width);
> }
> 
> // Datagrok's linear scheme: a number's cell carries its place in the column's
> // range as background weight.
> function sabelaGridPaint(td, col, raw, state) {
>   if (!state.color) return;
>   const st = col.stats;
>   if (col.numeric && st.n && st.max !== st.min) {
>     const n = sabelaGridNum(raw);
>     if (n === null) return;
>     const w = (n - st.min) / (st.max - st.min);
>     td.style.background = `rgba(${state.theme.scale},${(0.05 + 0.32 * w).toFixed(3)})`;
>   } else if (!col.numeric && st.categorical && String(raw ?? '').trim() !== '') {
>     const chip = document.createElement('span');
>     chip.className = 'sg-chip';
>     chip.style.background = sabelaGridChipColor(raw, state.theme);
>     td.insertBefore(chip, td.firstChild);
>   }
> }
> 
> function sabelaGridRow(state, n, position) {
>   const raw = state.raw.rows[n];
>   const tr = document.createElement('tr');
>   if (state.selected.has(n)) tr.classList.add('sg-sel');
>   if (state.current === n) tr.classList.add('sg-cur');
>   const idx = document.createElement('td');
>   idx.className = 'sg-idx';
>   idx.textContent = position;
>   tr.appendChild(idx);
>   state.cols.forEach((col) => {
>     const td = document.createElement('td');
>     const text = sabelaGridCellText(col, raw[col.index], col.width);
>     td.textContent = text;
>     td.title = String(raw[col.index] ?? '');
>     if (col.numeric) td.className = 'sg-num';
>     sabelaGridPaint(td, col, raw[col.index], state);
>     tr.appendChild(td);
>   });
>   tr.onclick = (e) => sabelaGridPick(state, n, e);
>   return tr;
> }
> 
> // A click makes a row current; ctrl or shift add to the selection, the way a
> // spreadsheet does.
> function sabelaGridPick(state, n, e) {
>   if (e.shiftKey && state.current >= 0) {
>     const from = state.order.indexOf(state.current);
>     const to = state.order.indexOf(n);
>     if (from >= 0 && to >= 0) {
>       const [lo, hi] = from < to ? [from, to] : [to, from];
>       for (let i = lo; i <= hi; i++) state.selected.add(state.order[i]);
>     }
>   } else if (e.ctrlKey || e.metaKey) {
>     if (!state.selected.delete(n)) state.selected.add(n);
>   } else {
>     state.selected.clear();
>   }
>   state.current = n;
>   state.repaint();
> }
> 
> function sabelaGridBody(state, tbody, scroll) {
>   const total = state.order.length;
>   const first = Math.max(0, Math.floor(scroll.scrollTop / SABELA_GRID_ROW_H) - 8);
>   const last = Math.min(total, first + SABELA_GRID_WINDOW);
>   tbody.textContent = '';
>   const spacer = (h) => {
>     const tr = document.createElement('tr');
>     const td = document.createElement('td');
>     td.colSpan = state.cols.length + 1;
>     td.style.cssText = `height:${h}px;padding:0;border:none`;
>     tr.appendChild(td);
>     return tr;
>   };
>   if (first) tbody.appendChild(spacer(first * SABELA_GRID_ROW_H));
>   for (let i = first; i < last; i++) {
>     tbody.appendChild(sabelaGridRow(state, state.order[i], state.order[i] + 1));
>   }
>   if (last < total) tbody.appendChild(spacer((total - last) * SABELA_GRID_ROW_H));
> }
> 
> // Enough width for the values and for the header's own furniture — the name,
> // its type, and the room a sort caret will want.
> function sabelaGridWidth(col, rows) {
>   let n = 0;
>   for (let i = 0; i < rows.length && i < 80; i++) {
>     n = Math.max(n, String(rows[i][col.index] ?? '').length);
>   }
>   const header = col.name.length * 7 + 38;
>   return Math.round(Math.min(260, Math.max(56, header, 16 + n * 6.8)));
> }
> 
> // Columns are laid out from their content, then opened up to whatever width
> // the cell actually gave us, so a narrow table does not sit in a wide gap.
> function sabelaGridFit(state, scroll, refresh) {
>   const avail = scroll.clientWidth - 48;
>   if (avail <= 0) return;
>   const total = state.cols.reduce((sum, c) => sum + c.width, 0);
>   if (total >= avail) return;
>   const grow = avail / total;
>   state.cols.forEach((c) => {
>     c.width = Math.min(360, Math.round(c.width * grow));
>   });
>   refresh();
> }
> 
> function sabelaGridToolbar(state, refresh) {
>   const bar = document.createElement('div');
>   bar.className = 'sg-bar';
>   const search = document.createElement('input');
>   search.className = 'sg-search';
>   search.type = 'text';
>   const numeric = state.cols.find((c) => c.numeric);
>   search.placeholder = numeric ? `search  ·  ${numeric.name} > 0` : 'search';
>   search.oninput = () => {
>     state.query = search.value;
>     refresh();
>   };
>   bar.appendChild(search);
>   const toggle = (label, key, title) => {
>     const b = document.createElement('button');
>     b.className = 'sg-btn';
>     b.type = 'button';
>     b.textContent = label;
>     b.title = title;
>     b.setAttribute('aria-pressed', String(state[key]));
>     b.onclick = () => {
>       state[key] = !state[key];
>       b.setAttribute('aria-pressed', String(state[key]));
>       refresh();
>     };
>     bar.appendChild(b);
>   };
>   toggle('color', 'color', 'colour cells by value');
>   toggle('stats', 'stats', 'show column statistics');
>   return bar;
> }
> 
> function sabelaGridFooter(state) {
>   const foot = document.createElement('div');
>   foot.className = 'sg-foot';
>   const shown = state.order.length;
>   const carried = state.raw.rows.length;
>   // The rows the page holds, and the rows the data has: a capped table shows
>   // fewer than it counts, and a search fewer still.
>   const total = state.raw.total ?? carried;
>   const rows =
>     total > carried
>       ? shown === carried
>         ? `first ${carried} of ${total} rows`
>         : `${shown} of first ${carried} · ${total} rows`
>       : shown === total
>         ? `${total} rows`
>         : `${shown} of ${total} rows`;
>   const cols = document.createElement('span');
>   cols.textContent = `${rows} × ${state.cols.length} columns`;
>   foot.appendChild(cols);
>   if (state.selected.size) {
>     const sel = document.createElement('span');
>     sel.textContent = `${state.selected.size} selected`;
>     foot.appendChild(sel);
>   }
>   if (state.stats) {
>     state.cols.forEach((col) => {
>       const s = document.createElement('span');
>       s.className = 'sg-stat';
>       s.textContent = sabelaGridSummary(col);
>       foot.appendChild(s);
>     });
>   }
>   return foot;
> }
> 
> // Turns one emitted payload into a live grid. Called once per table, with the
> // container the notebook printed.
> function sabelaGrid(container) {
>   const holder = container.querySelector('script.sabela-grid-data');
>   const raw = JSON.parse(holder.textContent);
>   const theme = sabelaGridTheme();
>   sabelaGridStyle(theme);
>   const cols = sabelaGridColumns(raw);
>   cols.forEach((col) => {
>     col.width = sabelaGridWidth(col, raw.rows);
>   });
>   const state = {
>     raw,
>     cols,
>     theme,
>     sort: null,
>     query: '',
>     color: true,
>     stats: false,
>     current: -1,
>     selected: new Set(),
>     order: raw.rows.map((_, n) => n),
>   };
>   container.className = 'sg';
>   container.textContent = '';
>   const scroll = document.createElement('div');
>   scroll.className = 'sg-scroll';
>   const table = document.createElement('table');
>   const group = document.createElement('colgroup');
>   const tbody = document.createElement('tbody');
>   scroll.appendChild(table);
> 
>   const paintBody = () => sabelaGridBody(state, tbody, scroll);
>   const refresh = () => {
>     state.order = sabelaGridOrder(raw, cols, state.sort, state.query);
>     group.textContent = '';
>     [46, ...cols.map((c) => c.width)].forEach((w) => {
>       const col = document.createElement('col');
>       col.style.width = w + 'px';
>       group.appendChild(col);
>     });
>     table.textContent = '';
>     table.appendChild(group);
>     table.appendChild(sabelaGridHeader(state, refresh));
>     table.appendChild(tbody);
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   state.repaint = () => {
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   scroll.addEventListener('scroll', paintBody);
>   container.appendChild(sabelaGridToolbar(state, refresh));
>   container.appendChild(scroll);
>   container.appendChild(sabelaGridFooter(state));
>   refresh();
>   if (scroll.clientWidth) sabelaGridFit(state, scroll, refresh);
>   else requestAnimationFrame(() => sabelaGridFit(state, scroll, refresh));
> }
> 
> // Every grid the page printed, mounted the moment its markup exists.
> function sabelaGridMountAll() {
>   document.querySelectorAll('div.sabela-grid:not([data-mounted])').forEach((el) => {
>     el.setAttribute('data-mounted', '1');
>     sabelaGrid(el);
>   });
> }
> 
> sabelaGridMountAll();</script>

```haskell
:t DT.filter
```

> <!-- scripths:mime text/plain -->
> DT.filter
>   :: SabelaDF.Columnable a =>
>      DT.TExpr cols a
>      -> (a -> Bool) -> DT.TypedDataFrame cols -> DT.TypedDataFrame cols

```haskell
tdf |> DT.take 5
    |> DT.derive @"rooms_per_household" (DT.col @"total_rooms" / DT.col @"households")
    |> DT.filter (DT.col @"rooms_per_household") (>= 5)
    |> DT.thaw
```

> <!-- scripths:mime text/html -->
> <div class="sabela-grid"><script type="application/json" class="sabela-grid-data">{"cols":["longitude","latitude","housing_median_age","total_rooms","total_bedrooms","population","households","median_income","median_house_value","ocean_proximity","rooms_per_household"],"rows":[["-122.23","37.88","41.0","880.0","129.0","322.0","126.0","8.3252","452600.0","NEAR BAY","6.984126984126984"],["-122.22","37.86","21.0","7099.0","1106.0","2401.0","1138.0","8.3014","358500.0","NEAR BAY","6.238137082601054"],["-122.24","37.85","52.0","1467.0","190.0","496.0","177.0","7.2574","352100.0","NEAR BAY","8.288135593220339"],["-122.25","37.85","52.0","1274.0","235.0","558.0","219.0","5.6431000000000004","341300.0","NEAR BAY","5.8173515981735155"],["-122.25","37.85","52.0","1627.0","280.0","565.0","259.0","3.8462","342200.0","NEAR BAY","6.281853281853282"]],"total":5}</script></div><script>// ── Grid model: typing, statistics, formatting, ordering ─────────
> // Pure data work for the table view. No DOM: everything here is a function
> // of the raw {cols, rows} payload the notebook emitted, so it can be reasoned
> // about (and tested) on its own.
> 
> // A column is numeric when every value that is present parses as one. Blank
> // cells do not vote: a column of numbers with gaps is still numeric.
> function sabelaGridKind(rows, i) {
>   let seen = 0;
>   let nums = 0;
>   let ints = 0;
>   let bools = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '' || v === 'null' || v === 'Nothing') continue;
>     seen++;
>     if (v === 'True' || v === 'False') bools++;
>     if (/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(v)) {
>       nums++;
>       if (/^[-+]?\d+$/.test(v)) ints++;
>     }
>   }
>   if (!seen) return 'str';
>   if (bools === seen) return 'bool';
>   if (nums < seen) return 'str';
>   return ints === seen ? 'int' : 'num';
> }
> 
> function sabelaGridNum(v) {
>   const t = (v ?? '').trim();
>   if (t === '' || t === 'null' || t === 'Nothing') return null;
>   const n = Number(t);
>   return Number.isFinite(n) ? n : null;
> }
> 
> // Column statistics, in the two shapes the header strip draws: a histogram
> // for numbers, the leading categories for everything else.
> function sabelaGridStats(rows, i, kind) {
>   const empty = { n: 0, missing: 0 };
>   if (kind === 'num' || kind === 'int') {
>     const vals = [];
>     let missing = 0;
>     for (const row of rows) {
>       const n = sabelaGridNum(row[i]);
>       if (n === null) missing++;
>       else vals.push(n);
>     }
>     if (!vals.length) return { ...empty, missing, kind };
>     let min = vals[0];
>     let max = vals[0];
>     let sum = 0;
>     for (const v of vals) {
>       if (v < min) min = v;
>       if (v > max) max = v;
>       sum += v;
>     }
>     const bins = new Array(24).fill(0);
>     const span = max - min;
>     for (const v of vals) {
>       const b = span === 0 ? 0 : Math.min(23, Math.floor(((v - min) / span) * 24));
>       bins[b]++;
>     }
>     return { kind, n: vals.length, missing, min, max, mean: sum / vals.length, bins };
>   }
>   const counts = new Map();
>   let missing = 0;
>   for (const row of rows) {
>     const v = (row[i] ?? '').trim();
>     if (v === '') missing++;
>     counts.set(v, (counts.get(v) || 0) + 1);
>   }
>   const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 6);
>   const n = rows.length - missing;
>   // A column whose every value differs is a key, not a set of categories:
>   // colouring it says nothing, so it is left uncoloured.
>   const categorical = counts.size > 1 && counts.size < n && counts.size <= 12;
>   return { kind, n, missing, unique: counts.size, top, categorical };
> }
> 
> // How many decimals a numeric column needs, capped so a wide column does not
> // print noise. Integers ask for none.
> function sabelaGridDecimals(rows, i, kind) {
>   if (kind === 'int' || kind === 'bool' || kind === 'str') return 0;
>   let want = 0;
>   for (const row of rows) {
>     const t = (row[i] ?? '').trim();
>     const dot = t.indexOf('.');
>     if (dot < 0 || /[eE]/.test(t)) continue;
>     want = Math.max(want, t.length - dot - 1);
>     if (want >= 4) return 4;
>   }
>   return want;
> }
> 
> // Datagrok's adaptive formatting: full precision when the column is wide,
> // degraded — then abbreviated — as it narrows.
> function sabelaGridFormat(v, col, width) {
>   if (col.kind === 'str' || col.kind === 'bool') return v ?? '';
>   const n = sabelaGridNum(v);
>   if (n === null) return v ?? '';
>   const abs = Math.abs(n);
>   if (abs !== 0 && (abs >= 1e9 || abs < 1e-4)) return n.toExponential(2);
>   let decimals = col.decimals;
>   if (width < 88) decimals = Math.min(decimals, 2);
>   if (width < 64) decimals = 0;
>   if (abs >= 1e6 && width < 110) {
>     return (n / 1e6).toFixed(decimals ? 1 : 0) + 'M';
>   }
>   return n.toLocaleString(undefined, {
>     minimumFractionDigits: decimals,
>     maximumFractionDigits: decimals,
>   });
> }
> 
> function sabelaGridColumns(raw) {
>   return raw.cols.map((name, i) => {
>     const kind = sabelaGridKind(raw.rows, i);
>     return {
>       name,
>       index: i,
>       kind,
>       numeric: kind === 'num' || kind === 'int',
>       decimals: sabelaGridDecimals(raw.rows, i, kind),
>       stats: sabelaGridStats(raw.rows, i, kind),
>       width: 0,
>     };
>   });
> }
> 
> // The order rows are shown in: the sort, then the search, both applied to the
> // original row indices so the row header keeps naming the source row.
> function sabelaGridOrder(raw, cols, sort, query) {
>   let order = raw.rows.map((_, n) => n);
>   const test = sabelaGridQuery(query, cols);
>   if (test) order = order.filter((n) => test(raw.rows[n]));
>   if (sort) {
>     const col = cols[sort.col];
>     const dir = sort.desc ? -1 : 1;
>     order.sort((a, b) => {
>       const ma = sabelaGridMissing(raw.rows[a], col);
>       const mb = sabelaGridMissing(raw.rows[b], col);
>       if (ma !== mb) return ma ? 1 : -1;
>       return dir * sabelaGridCompare(raw.rows[a], raw.rows[b], col);
>     });
>   }
>   return order;
> }
> 
> // A value that is not there sorts to the end whichever way the column is
> // sorted: it is absent, not small.
> function sabelaGridMissing(row, col) {
>   const v = row[col.index];
>   return col.numeric ? sabelaGridNum(v) === null : String(v ?? '').trim() === '';
> }
> 
> function sabelaGridCompare(ra, rb, col) {
>   const i = col.index;
>   if (col.numeric) {
>     const a = sabelaGridNum(ra[i]);
>     const b = sabelaGridNum(rb[i]);
>     if (a === null) return b === null ? 0 : 1;
>     if (b === null) return -1;
>     return a - b;
>   }
>   return String(ra[i] ?? '').localeCompare(String(rb[i] ?? ''), undefined, { numeric: true });
> }
> 
> // Search, in Datagrok's two spellings: free text over every cell, or one
> // column compared against a value ("temp > 25", "city = Harare").
> function sabelaGridQuery(query, cols) {
>   const q = (query || '').trim();
>   if (!q) return null;
>   const m = q.match(/^\s*([\w ]+?)\s*(>=|<=|!=|>|<|=)\s*(.+?)\s*$/);
>   if (m) {
>     const col = cols.find((c) => c.name.toLowerCase() === m[1].toLowerCase());
>     if (col) return sabelaGridComparison(col, m[2], m[3]);
>   }
>   const needle = q.toLowerCase();
>   return (row) =>
>     row.some((v) =>
>       String(v ?? '')
>         .toLowerCase()
>         .includes(needle)
>     );
> }
> 
> function sabelaGridComparison(col, op, rhs) {
>   const i = col.index;
>   const num = Number(rhs);
>   const useNum = col.numeric && Number.isFinite(num);
>   return (row) => {
>     const raw = String(row[i] ?? '');
>     const a = useNum ? sabelaGridNum(raw) : raw.toLowerCase();
>     const b = useNum ? num : rhs.toLowerCase();
>     if (a === null) return false;
>     switch (op) {
>       case '>':
>         return a > b;
>       case '<':
>         return a < b;
>       case '>=':
>         return a >= b;
>       case '<=':
>         return a <= b;
>       case '!=':
>         return a !== b;
>       default:
>         return useNum ? a === b : a.includes(b);
>     }
>   };
> }
> 
> // ── Grid chrome: theme, stylesheet, header, distribution strip ───
> // The parts of the table view that are drawn once per render. The body and
> // its interactions live in grid-body.js.
> 
> // The notebook only hands an output iframe three colours, so the rest of the
> // palette is derived from whether the background it gave is dark or light.
> function sabelaGridTheme() {
>   const cs = getComputedStyle(document.body);
>   const m = (cs.backgroundColor || 'rgb(255,255,255)').match(/\d+/g) || [255, 255, 255];
>   const lum = (0.299 * +m[0] + 0.587 * +m[1] + 0.114 * +m[2]) / 255;
>   const dark = lum < 0.5;
>   return {
>     dark,
>     fg: cs.color || (dark ? '#e6e6ef' : '#1e1e2e'),
>     dim: dark ? '#8b8b9c' : '#8a8a99',
>     border: dark ? '#3a3a46' : '#e2e2ea',
>     rule: dark ? '#5a5a68' : '#b9b9c6',
>     zebra: dark ? 'rgba(255,255,255,0.028)' : 'rgba(0,0,0,0.022)',
>     hover: dark ? 'rgba(130,175,255,0.13)' : 'rgba(60,120,240,0.08)',
>     current: '#3aa657',
>     selected: dark ? 'rgba(255,170,70,0.20)' : 'rgba(255,165,60,0.22)',
>     scale: dark ? '130,175,255' : '60,120,240',
>   };
> }
> 
> function sabelaGridStyle(t) {
>   if (document.getElementById('sabela-grid-style')) return;
>   const s = document.createElement('style');
>   s.id = 'sabela-grid-style';
>   s.textContent = `
> .sg{font-family:inherit;font-size:11.5px;color:${t.fg};max-width:100%}
> .sg-bar{display:flex;align-items:center;gap:8px;padding:0 0 5px;color:${t.dim};font-size:11px}
> .sg-search{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.fg};
>   font-size:11px;padding:2px 9px;width:170px;cursor:text}
> .sg-search:focus{outline:none;border-color:rgba(${t.scale},0.8)}
> .sg-btn{background:transparent;border:1px solid ${t.border};border-radius:100px;color:${t.dim};
>   font-size:10.5px;padding:2px 9px}
> .sg-btn[aria-pressed=true]{color:${t.fg};border-color:rgba(${t.scale},0.7)}
> .sg-scroll{overflow:auto;border-top:1px solid ${t.border};border-bottom:1px solid ${t.border};
>   max-height:420px;position:relative}
> .sg table{border-collapse:separate;border-spacing:0;table-layout:fixed;width:max-content;max-width:100%}
> .sg th,.sg td{border-right:1px solid ${t.border};padding:2px 7px;white-space:nowrap;overflow:hidden;
>   text-overflow:ellipsis;height:21px;box-sizing:border-box}
> .sg thead th{position:sticky;top:0;z-index:2;background:${t.dark ? '#20202a' : '#fbfbfd'};
>   border-bottom:1px solid ${t.rule};font-weight:500;text-align:left;user-select:none;cursor:pointer}
> .sg thead tr.sg-strip th{top:25px;z-index:1;padding:0;height:15px;cursor:default;opacity:.72;
>   border-bottom:1px solid ${t.border}}
> .sg .sg-idx{color:${t.dim};text-align:right;width:46px;position:sticky;left:0;z-index:1;
>   background:${t.dark ? '#20202a' : '#fbfbfd'};cursor:pointer}
> .sg thead .sg-idx{z-index:3}
> .sg-name{display:inline-block;max-width:calc(100% - 30px);overflow:hidden;text-overflow:ellipsis;
>   vertical-align:bottom}
> .sg-type{color:${t.dim};font-size:9px;margin-left:4px;letter-spacing:.4px}
> .sg-caret{color:rgba(${t.scale},1);margin-left:3px}
> .sg-resize{position:absolute;top:0;right:-3px;width:7px;height:100%;cursor:col-resize;z-index:4}
> .sg tbody tr:nth-child(even) td{background:${t.zebra}}
> .sg tbody tr:hover td{background:${t.hover}}
> .sg tbody tr.sg-sel td{background:${t.selected}}
> .sg tbody tr.sg-cur td{box-shadow:inset 0 -1px 0 ${t.current},inset 0 1px 0 ${t.current}}
> .sg tbody tr.sg-cur td.sg-idx{box-shadow:inset 3px 0 0 ${t.current},inset 0 -1px 0 ${t.current},
>   inset 0 1px 0 ${t.current}}
> .sg-num{text-align:right;font-variant-numeric:tabular-nums}
> .sg-chip{display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:5px;
>   vertical-align:baseline}
> .sg-foot{color:${t.dim};font-size:10.5px;padding-top:5px;display:flex;gap:10px;flex-wrap:wrap}
> .sg-stat{color:${t.dim};font-size:10px}
> `;
>   document.head.appendChild(s);
> }
> 
> var SABELA_GRID_TYPE = { num: '#', int: '#', str: 'abc', bool: '✓' };
> 
> // A column's distribution, drawn under its name: bars over the value range
> // for a number, the leading categories in proportion for anything else.
> function sabelaGridStrip(col, t) {
>   const st = col.stats;
>   const ns = 'http://www.w3.org/2000/svg';
>   const svg = document.createElementNS(ns, 'svg');
>   svg.setAttribute('preserveAspectRatio', 'none');
>   svg.setAttribute('viewBox', '0 0 100 14');
>   svg.style.cssText = 'width:100%;height:14px;display:block';
>   const add = (x, y, w, h, fill, title) => {
>     const r = document.createElementNS(ns, 'rect');
>     r.setAttribute('x', x);
>     r.setAttribute('y', y);
>     r.setAttribute('width', Math.max(0.6, w));
>     r.setAttribute('height', Math.max(0.8, h));
>     r.setAttribute('fill', fill);
>     if (title) {
>       const el = document.createElementNS(ns, 'title');
>       el.textContent = title;
>       r.appendChild(el);
>     }
>     svg.appendChild(r);
>   };
>   if (col.numeric && st.bins) {
>     const peak = Math.max(...st.bins, 1);
>     const w = 100 / st.bins.length;
>     st.bins.forEach((n, i) => {
>       if (!n) return;
>       const h = (n / peak) * 12;
>       add(i * w, 13 - h, w - 0.5, h, `rgba(${t.scale},0.5)`, n + ' rows');
>     });
>   } else if (st.categorical && st.top) {
>     let x = 0;
>     st.top.forEach((entry) => {
>       const w = (entry[1] / st.n) * 100;
>       add(x, 4, w - 0.4, 7, sabelaGridChipColor(entry[0], t), entry[0] + ' — ' + entry[1]);
>       x += w;
>     });
>     if (x < 99.5) add(x, 4, 100 - x, 7, t.border, 'other');
>   }
>   return svg;
> }
> 
> // A stable colour per category, so the same value reads the same everywhere
> // in the grid.
> function sabelaGridChipColor(v, t) {
>   let h = 0;
>   const s = String(v);
>   for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 360;
>   return `hsl(${h},${t.dark ? 45 : 55}%,${t.dark ? 52 : 60}%)`;
> }
> 
> function sabelaGridHeader(state, refresh) {
>   const t = state.theme;
>   const thead = document.createElement('thead');
>   const names = document.createElement('tr');
>   const strip = document.createElement('tr');
>   strip.className = 'sg-strip';
>   const corner = document.createElement('th');
>   corner.className = 'sg-idx';
>   corner.title = 'clear selection';
>   corner.onclick = () => {
>     state.selected.clear();
>     state.current = -1;
>     refresh();
>   };
>   names.appendChild(corner);
>   const stripCorner = document.createElement('th');
>   stripCorner.className = 'sg-idx';
>   strip.appendChild(stripCorner);
>   state.cols.forEach((col) => {
>     names.appendChild(sabelaGridHeadCell(state, col, refresh));
>     const sth = document.createElement('th');
>     sth.appendChild(sabelaGridStrip(col, t));
>     strip.appendChild(sth);
>   });
>   thead.appendChild(names);
>   thead.appendChild(strip);
>   return thead;
> }
> 
> function sabelaGridHeadCell(state, col, refresh) {
>   const th = document.createElement('th');
>   th.style.position = 'relative';
>   th.title = sabelaGridSummary(col);
>   const name = document.createElement('span');
>   name.className = 'sg-name';
>   name.textContent = col.name;
>   th.appendChild(name);
>   const type = document.createElement('span');
>   type.className = 'sg-type';
>   type.textContent = SABELA_GRID_TYPE[col.kind] || '';
>   th.appendChild(type);
>   if (state.sort && state.sort.col === col.index) {
>     const caret = document.createElement('span');
>     caret.className = 'sg-caret';
>     caret.textContent = state.sort.desc ? '↓' : '↑';
>     th.appendChild(caret);
>   }
>   th.onclick = () => {
>     const on = state.sort && state.sort.col === col.index;
>     state.sort =
>       on && !state.sort.desc
>         ? { col: col.index, desc: true }
>         : on
>           ? null
>           : { col: col.index, desc: false };
>     refresh();
>   };
>   const grip = document.createElement('div');
>   grip.className = 'sg-resize';
>   grip.onclick = (e) => e.stopPropagation();
>   grip.onmousedown = (e) => sabelaGridDragWidth(e, state, col, refresh);
>   th.appendChild(grip);
>   return th;
> }
> 
> function sabelaGridDragWidth(e, state, col, refresh) {
>   e.preventDefault();
>   const startX = e.clientX;
>   const startW = col.width;
>   const move = (ev) => {
>     col.width = Math.max(46, startW + (ev.clientX - startX));
>     refresh();
>   };
>   const up = () => {
>     window.removeEventListener('mousemove', move);
>     window.removeEventListener('mouseup', up);
>   };
>   window.addEventListener('mousemove', move);
>   window.addEventListener('mouseup', up);
> }
> 
> // What a column header says about itself on hover, and what the stats line
> // under the grid repeats for every column at once.
> function sabelaGridSummary(col) {
>   const st = col.stats;
>   const miss = st.missing ? ', ' + st.missing + ' missing' : '';
>   if (col.numeric && st.n) {
>     const f = (v) => sabelaGridFormat(String(v), col, 999);
>     return `${col.name}: min ${f(st.min)}, mean ${f(st.mean)}, max ${f(st.max)}${miss}`;
>   }
>   return `${col.name}: ${st.unique || 0} unique${miss}`;
> }
> 
> // ── Grid body: rows, colour coding, selection, and the entry point ──
> // Only the rows in view are in the DOM; the rest is spacer height, so a
> // table of thousands of rows costs the same to scroll as a short one.
> // `var`, not `const`: an output iframe is reused across re-runs, so this
> // script is evaluated again in a window that already holds these names.
> 
> var SABELA_GRID_ROW_H = 21;
> var SABELA_GRID_WINDOW = 60;
> 
> function sabelaGridCellText(col, raw, width) {
>   return sabelaGridFormat(raw, col, width);
> }
> 
> // Datagrok's linear scheme: a number's cell carries its place in the column's
> // range as background weight.
> function sabelaGridPaint(td, col, raw, state) {
>   if (!state.color) return;
>   const st = col.stats;
>   if (col.numeric && st.n && st.max !== st.min) {
>     const n = sabelaGridNum(raw);
>     if (n === null) return;
>     const w = (n - st.min) / (st.max - st.min);
>     td.style.background = `rgba(${state.theme.scale},${(0.05 + 0.32 * w).toFixed(3)})`;
>   } else if (!col.numeric && st.categorical && String(raw ?? '').trim() !== '') {
>     const chip = document.createElement('span');
>     chip.className = 'sg-chip';
>     chip.style.background = sabelaGridChipColor(raw, state.theme);
>     td.insertBefore(chip, td.firstChild);
>   }
> }
> 
> function sabelaGridRow(state, n, position) {
>   const raw = state.raw.rows[n];
>   const tr = document.createElement('tr');
>   if (state.selected.has(n)) tr.classList.add('sg-sel');
>   if (state.current === n) tr.classList.add('sg-cur');
>   const idx = document.createElement('td');
>   idx.className = 'sg-idx';
>   idx.textContent = position;
>   tr.appendChild(idx);
>   state.cols.forEach((col) => {
>     const td = document.createElement('td');
>     const text = sabelaGridCellText(col, raw[col.index], col.width);
>     td.textContent = text;
>     td.title = String(raw[col.index] ?? '');
>     if (col.numeric) td.className = 'sg-num';
>     sabelaGridPaint(td, col, raw[col.index], state);
>     tr.appendChild(td);
>   });
>   tr.onclick = (e) => sabelaGridPick(state, n, e);
>   return tr;
> }
> 
> // A click makes a row current; ctrl or shift add to the selection, the way a
> // spreadsheet does.
> function sabelaGridPick(state, n, e) {
>   if (e.shiftKey && state.current >= 0) {
>     const from = state.order.indexOf(state.current);
>     const to = state.order.indexOf(n);
>     if (from >= 0 && to >= 0) {
>       const [lo, hi] = from < to ? [from, to] : [to, from];
>       for (let i = lo; i <= hi; i++) state.selected.add(state.order[i]);
>     }
>   } else if (e.ctrlKey || e.metaKey) {
>     if (!state.selected.delete(n)) state.selected.add(n);
>   } else {
>     state.selected.clear();
>   }
>   state.current = n;
>   state.repaint();
> }
> 
> function sabelaGridBody(state, tbody, scroll) {
>   const total = state.order.length;
>   const first = Math.max(0, Math.floor(scroll.scrollTop / SABELA_GRID_ROW_H) - 8);
>   const last = Math.min(total, first + SABELA_GRID_WINDOW);
>   tbody.textContent = '';
>   const spacer = (h) => {
>     const tr = document.createElement('tr');
>     const td = document.createElement('td');
>     td.colSpan = state.cols.length + 1;
>     td.style.cssText = `height:${h}px;padding:0;border:none`;
>     tr.appendChild(td);
>     return tr;
>   };
>   if (first) tbody.appendChild(spacer(first * SABELA_GRID_ROW_H));
>   for (let i = first; i < last; i++) {
>     tbody.appendChild(sabelaGridRow(state, state.order[i], state.order[i] + 1));
>   }
>   if (last < total) tbody.appendChild(spacer((total - last) * SABELA_GRID_ROW_H));
> }
> 
> // Enough width for the values and for the header's own furniture — the name,
> // its type, and the room a sort caret will want.
> function sabelaGridWidth(col, rows) {
>   let n = 0;
>   for (let i = 0; i < rows.length && i < 80; i++) {
>     n = Math.max(n, String(rows[i][col.index] ?? '').length);
>   }
>   const header = col.name.length * 7 + 38;
>   return Math.round(Math.min(260, Math.max(56, header, 16 + n * 6.8)));
> }
> 
> // Columns are laid out from their content, then opened up to whatever width
> // the cell actually gave us, so a narrow table does not sit in a wide gap.
> function sabelaGridFit(state, scroll, refresh) {
>   const avail = scroll.clientWidth - 48;
>   if (avail <= 0) return;
>   const total = state.cols.reduce((sum, c) => sum + c.width, 0);
>   if (total >= avail) return;
>   const grow = avail / total;
>   state.cols.forEach((c) => {
>     c.width = Math.min(360, Math.round(c.width * grow));
>   });
>   refresh();
> }
> 
> function sabelaGridToolbar(state, refresh) {
>   const bar = document.createElement('div');
>   bar.className = 'sg-bar';
>   const search = document.createElement('input');
>   search.className = 'sg-search';
>   search.type = 'text';
>   const numeric = state.cols.find((c) => c.numeric);
>   search.placeholder = numeric ? `search  ·  ${numeric.name} > 0` : 'search';
>   search.oninput = () => {
>     state.query = search.value;
>     refresh();
>   };
>   bar.appendChild(search);
>   const toggle = (label, key, title) => {
>     const b = document.createElement('button');
>     b.className = 'sg-btn';
>     b.type = 'button';
>     b.textContent = label;
>     b.title = title;
>     b.setAttribute('aria-pressed', String(state[key]));
>     b.onclick = () => {
>       state[key] = !state[key];
>       b.setAttribute('aria-pressed', String(state[key]));
>       refresh();
>     };
>     bar.appendChild(b);
>   };
>   toggle('color', 'color', 'colour cells by value');
>   toggle('stats', 'stats', 'show column statistics');
>   return bar;
> }
> 
> function sabelaGridFooter(state) {
>   const foot = document.createElement('div');
>   foot.className = 'sg-foot';
>   const shown = state.order.length;
>   const carried = state.raw.rows.length;
>   // The rows the page holds, and the rows the data has: a capped table shows
>   // fewer than it counts, and a search fewer still.
>   const total = state.raw.total ?? carried;
>   const rows =
>     total > carried
>       ? shown === carried
>         ? `first ${carried} of ${total} rows`
>         : `${shown} of first ${carried} · ${total} rows`
>       : shown === total
>         ? `${total} rows`
>         : `${shown} of ${total} rows`;
>   const cols = document.createElement('span');
>   cols.textContent = `${rows} × ${state.cols.length} columns`;
>   foot.appendChild(cols);
>   if (state.selected.size) {
>     const sel = document.createElement('span');
>     sel.textContent = `${state.selected.size} selected`;
>     foot.appendChild(sel);
>   }
>   if (state.stats) {
>     state.cols.forEach((col) => {
>       const s = document.createElement('span');
>       s.className = 'sg-stat';
>       s.textContent = sabelaGridSummary(col);
>       foot.appendChild(s);
>     });
>   }
>   return foot;
> }
> 
> // Turns one emitted payload into a live grid. Called once per table, with the
> // container the notebook printed.
> function sabelaGrid(container) {
>   const holder = container.querySelector('script.sabela-grid-data');
>   const raw = JSON.parse(holder.textContent);
>   const theme = sabelaGridTheme();
>   sabelaGridStyle(theme);
>   const cols = sabelaGridColumns(raw);
>   cols.forEach((col) => {
>     col.width = sabelaGridWidth(col, raw.rows);
>   });
>   const state = {
>     raw,
>     cols,
>     theme,
>     sort: null,
>     query: '',
>     color: true,
>     stats: false,
>     current: -1,
>     selected: new Set(),
>     order: raw.rows.map((_, n) => n),
>   };
>   container.className = 'sg';
>   container.textContent = '';
>   const scroll = document.createElement('div');
>   scroll.className = 'sg-scroll';
>   const table = document.createElement('table');
>   const group = document.createElement('colgroup');
>   const tbody = document.createElement('tbody');
>   scroll.appendChild(table);
> 
>   const paintBody = () => sabelaGridBody(state, tbody, scroll);
>   const refresh = () => {
>     state.order = sabelaGridOrder(raw, cols, state.sort, state.query);
>     group.textContent = '';
>     [46, ...cols.map((c) => c.width)].forEach((w) => {
>       const col = document.createElement('col');
>       col.style.width = w + 'px';
>       group.appendChild(col);
>     });
>     table.textContent = '';
>     table.appendChild(group);
>     table.appendChild(sabelaGridHeader(state, refresh));
>     table.appendChild(tbody);
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   state.repaint = () => {
>     paintBody();
>     const foot = container.querySelector('.sg-foot');
>     if (foot) container.replaceChild(sabelaGridFooter(state), foot);
>   };
>   scroll.addEventListener('scroll', paintBody);
>   container.appendChild(sabelaGridToolbar(state, refresh));
>   container.appendChild(scroll);
>   container.appendChild(sabelaGridFooter(state));
>   refresh();
>   if (scroll.clientWidth) sabelaGridFit(state, scroll, refresh);
>   else requestAnimationFrame(() => sabelaGridFit(state, scroll, refresh));
> }
> 
> // Every grid the page printed, mounted the moment its markup exists.
> function sabelaGridMountAll() {
>   document.querySelectorAll('div.sabela-grid:not([data-mounted])').forEach((el) => {
>     el.setAttribute('data-mounted', '1');
>     sabelaGrid(el);
>   });
> }
> 
> sabelaGridMountAll();</script>
