// ── Grid model: typing, statistics, formatting, ordering ─────────
// Pure data work for the table view. No DOM: everything here is a function
// of the raw {cols, rows} payload the notebook emitted, so it can be reasoned
// about (and tested) on its own.

// One cell as the notebook showed it, with a Haskell Maybe unwrapped: Nothing
// (or a null) is a blank, and Just x is x, minus the parentheses and quotes
// its Show added, so a Maybe column types, sorts and charts like its payload.
function sabelaGridCell(v) {
  let t = String(v ?? '').trim();
  if (t === 'Nothing' || t === 'null') return '';
  const just = t.match(/^Just\s+([\s\S]+)$/);
  if (!just) return t;
  t = just[1].trim();
  if (t.startsWith('(') && t.endsWith(')')) t = t.slice(1, -1).trim();
  if (t.startsWith('"') && t.endsWith('"') && t.length >= 2) {
    try {
      return JSON.parse(t);
    } catch {
      return t.slice(1, -1);
    }
  }
  return sabelaGridCell(t);
}

// The payload with every cell normalised, done once at ingestion so the rest
// of the model only ever sees plain values. A column's Haskell type, when the
// notebook sent one, rides along untouched under `types`.
function sabelaGridNormalize(raw) {
  return { ...raw, rows: raw.rows.map((row) => row.map(sabelaGridCell)) };
}

// A column is numeric when every value that is present parses as one. Blank
// cells do not vote: a column of numbers with gaps is still numeric.
function sabelaGridKind(rows, i) {
  let seen = 0;
  let nums = 0;
  let ints = 0;
  let bools = 0;
  for (const row of rows) {
    const v = (row[i] ?? '').trim();
    if (v === '' || v === 'null' || v === 'Nothing') continue;
    seen++;
    if (v === 'True' || v === 'False') bools++;
    if (/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(v)) {
      nums++;
      if (/^[-+]?\d+$/.test(v)) ints++;
    }
  }
  if (!seen) return 'str';
  if (bools === seen) return 'bool';
  if (nums < seen) return 'str';
  return ints === seen ? 'int' : 'num';
}

function sabelaGridNum(v) {
  const t = (v ?? '').trim();
  if (t === '' || t === 'null' || t === 'Nothing') return null;
  const n = Number(t);
  return Number.isFinite(n) ? n : null;
}

// Column statistics, in the two shapes the header strip draws: a histogram
// for numbers, the leading categories for everything else.
function sabelaGridStats(rows, i, kind) {
  const empty = { n: 0, missing: 0 };
  if (kind === 'num' || kind === 'int') {
    const vals = [];
    let missing = 0;
    for (const row of rows) {
      const n = sabelaGridNum(row[i]);
      if (n === null) missing++;
      else vals.push(n);
    }
    if (!vals.length) return { ...empty, missing, kind };
    let min = vals[0];
    let max = vals[0];
    let sum = 0;
    for (const v of vals) {
      if (v < min) min = v;
      if (v > max) max = v;
      sum += v;
    }
    const bins = new Array(24).fill(0);
    const span = max - min;
    for (const v of vals) {
      const b = span === 0 ? 0 : Math.min(23, Math.floor(((v - min) / span) * 24));
      bins[b]++;
    }
    return { kind, n: vals.length, missing, min, max, mean: sum / vals.length, bins };
  }
  const counts = new Map();
  let missing = 0;
  for (const row of rows) {
    const v = (row[i] ?? '').trim();
    if (v === '') missing++;
    counts.set(v, (counts.get(v) || 0) + 1);
  }
  const top = [...counts.entries()].sort((a, b) => b[1] - a[1]).slice(0, 6);
  const n = rows.length - missing;
  // A column whose every value differs is a key, not a set of categories:
  // colouring it says nothing, so it is left uncoloured.
  const categorical = counts.size > 1 && counts.size < n && counts.size <= 12;
  return { kind, n, missing, unique: counts.size, top, categorical };
}

// How many decimals a numeric column needs, capped so a wide column does not
// print noise. Integers ask for none.
function sabelaGridDecimals(rows, i, kind) {
  if (kind === 'int' || kind === 'bool' || kind === 'str') return 0;
  let want = 0;
  for (const row of rows) {
    const t = (row[i] ?? '').trim();
    const dot = t.indexOf('.');
    if (dot < 0 || /[eE]/.test(t)) continue;
    want = Math.max(want, t.length - dot - 1);
    if (want >= 4) return 4;
  }
  return want;
}

// Datagrok's adaptive formatting: full precision when the column is wide,
// degraded — then abbreviated — as it narrows.
function sabelaGridFormat(v, col, width) {
  if (col.kind === 'str' || col.kind === 'bool') return v ?? '';
  const n = sabelaGridNum(v);
  if (n === null) return v ?? '';
  const abs = Math.abs(n);
  if (abs !== 0 && (abs >= 1e9 || abs < 1e-4)) return n.toExponential(2);
  let decimals = col.decimals;
  if (width < 88) decimals = Math.min(decimals, 2);
  if (width < 64) decimals = 0;
  if (abs >= 1e6 && width < 110) {
    return (n / 1e6).toFixed(decimals ? 1 : 0) + 'M';
  }
  return n.toLocaleString(undefined, {
    minimumFractionDigits: decimals,
    maximumFractionDigits: decimals,
  });
}

function sabelaGridColumns(raw) {
  return raw.cols.map((name, i) => {
    const kind = sabelaGridKind(raw.rows, i);
    return {
      name,
      index: i,
      kind,
      type: (raw.types && raw.types[i]) || '',
      numeric: kind === 'num' || kind === 'int',
      decimals: sabelaGridDecimals(raw.rows, i, kind),
      stats: sabelaGridStats(raw.rows, i, kind),
      width: 0,
    };
  });
}

// The order rows are shown in: the sort, then the search, both applied to the
// original row indices so the row header keeps naming the source row.
function sabelaGridOrder(raw, cols, sort, query) {
  let order = raw.rows.map((_, n) => n);
  const test = sabelaGridQuery(query, cols);
  if (test) order = order.filter((n) => test(raw.rows[n]));
  if (sort) {
    const col = cols[sort.col];
    const dir = sort.desc ? -1 : 1;
    order.sort((a, b) => {
      const ma = sabelaGridMissing(raw.rows[a], col);
      const mb = sabelaGridMissing(raw.rows[b], col);
      if (ma !== mb) return ma ? 1 : -1;
      return dir * sabelaGridCompare(raw.rows[a], raw.rows[b], col);
    });
  }
  return order;
}

// A value that is not there sorts to the end whichever way the column is
// sorted: it is absent, not small.
function sabelaGridMissing(row, col) {
  const v = row[col.index];
  return col.numeric ? sabelaGridNum(v) === null : String(v ?? '').trim() === '';
}

function sabelaGridCompare(ra, rb, col) {
  const i = col.index;
  if (col.numeric) {
    const a = sabelaGridNum(ra[i]);
    const b = sabelaGridNum(rb[i]);
    if (a === null) return b === null ? 0 : 1;
    if (b === null) return -1;
    return a - b;
  }
  return String(ra[i] ?? '').localeCompare(String(rb[i] ?? ''), undefined, { numeric: true });
}

// Search, in Datagrok's two spellings: free text over every cell, or one
// column compared against a value ("temp > 25", "city = Harare").
function sabelaGridQuery(query, cols) {
  const q = (query || '').trim();
  if (!q) return null;
  const m = q.match(/^\s*([\w ]+?)\s*(>=|<=|!=|>|<|=)\s*(.+?)\s*$/);
  if (m) {
    const col = cols.find((c) => c.name.toLowerCase() === m[1].toLowerCase());
    if (col) return sabelaGridComparison(col, m[2], m[3]);
  }
  const needle = q.toLowerCase();
  return (row) =>
    row.some((v) =>
      String(v ?? '')
        .toLowerCase()
        .includes(needle)
    );
}

function sabelaGridComparison(col, op, rhs) {
  const i = col.index;
  const num = Number(rhs);
  const useNum = col.numeric && Number.isFinite(num);
  return (row) => {
    const raw = String(row[i] ?? '');
    const a = useNum ? sabelaGridNum(raw) : raw.toLowerCase();
    const b = useNum ? num : rhs.toLowerCase();
    if (a === null) return false;
    switch (op) {
      case '>':
        return a > b;
      case '<':
        return a < b;
      case '>=':
        return a >= b;
      case '<=':
        return a <= b;
      case '!=':
        return a !== b;
      default:
        return useNum ? a === b : a.includes(b);
    }
  };
}
