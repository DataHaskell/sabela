// ── Grid body: rows, selection, pager, and the entry point ───────
// The table shows one page of rows at a time, the way DT and marimo do; the
// page size and the page are state like the sort and the search.
// `var`, not `const`: an output iframe is reused across re-runs, so this
// script is evaluated again in a window that already holds these names.

var SABELA_GRID_PAGE_SIZES = [10, 25, 50, 100];

function sabelaGridRow(state, n, position) {
  const raw = state.raw.rows[n];
  const tr = document.createElement('tr');
  if (state.selected.has(n)) tr.classList.add('sg-sel');
  const idx = document.createElement('td');
  idx.className = 'sg-idx';
  idx.textContent = position;
  tr.appendChild(idx);
  state.cols.forEach((col) => {
    const td = document.createElement('td');
    const text = sabelaGridFormat(raw[col.index], col, 999);
    if (text === '') {
      td.className = 'sg-blank';
      td.textContent = '—';
    } else {
      td.textContent = text;
      if (col.numeric) td.className = 'sg-num';
    }
    td.title = String(raw[col.index] ?? '');
    tr.appendChild(td);
  });
  tr.onclick = (e) => sabelaGridPick(state, n, e);
  return tr;
}

// A click selects a row; ctrl or shift add to the selection, the way a
// spreadsheet does. Clicking the selected row again clears it.
function sabelaGridPick(state, n, e) {
  if (e.shiftKey && state.current >= 0) {
    const from = state.order.indexOf(state.current);
    const to = state.order.indexOf(n);
    if (from >= 0 && to >= 0) {
      const [lo, hi] = from < to ? [from, to] : [to, from];
      for (let i = lo; i <= hi; i++) state.selected.add(state.order[i]);
    }
  } else if (e.ctrlKey || e.metaKey) {
    if (!state.selected.delete(n)) state.selected.add(n);
  } else if (state.selected.size === 1 && state.selected.has(n)) {
    state.selected.clear();
  } else {
    state.selected.clear();
    state.selected.add(n);
  }
  state.current = n;
  state.repaint();
}

function sabelaGridPages(state) {
  return Math.max(1, Math.ceil(state.order.length / state.pageSize));
}

function sabelaGridBody(state, tbody) {
  const total = state.order.length;
  state.page = Math.min(state.page, sabelaGridPages(state) - 1);
  const first = state.page * state.pageSize;
  const last = Math.min(total, first + state.pageSize);
  tbody.textContent = '';
  if (!total) {
    const tr = document.createElement('tr');
    tr.className = 'sg-empty';
    const td = document.createElement('td');
    td.colSpan = state.cols.length + 1;
    td.textContent = state.query ? 'No matching rows' : 'No rows';
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }
  for (let i = first; i < last; i++) {
    tbody.appendChild(sabelaGridRow(state, state.order[i], state.order[i] + 1));
  }
}

function sabelaGridToolbar(state, refresh) {
  const bar = document.createElement('div');
  bar.className = 'sg-bar';
  if (state.slot && sabelaGridLive()) {
    sabelaGridQueryBar(state, bar);
    bar.appendChild(sabelaGridShape(state));
    return bar;
  }
  const search = document.createElement('input');
  search.className = 'sg-search';
  search.type = 'search';
  const numeric = state.cols.find((c) => c.numeric);
  search.placeholder = numeric ? `Search  ·  ${numeric.name} > 0` : 'Search';
  search.oninput = () => {
    state.query = search.value;
    state.page = 0;
    refresh();
  };
  bar.appendChild(search);
  bar.appendChild(sabelaGridShape(state));
  return bar;
}

function sabelaGridShape(state) {
  const shape = document.createElement('span');
  shape.className = 'sg-shape';
  const total = state.raw.total ?? state.raw.rows.length;
  const cols = state.cols.length;
  shape.textContent = `${total.toLocaleString()} rows × ${cols} column${cols === 1 ? '' : 's'}`;
  return shape;
}

// "1–10 of 57 rows", with the two complications spelled out when they
// arise: a search that kept fewer, and a capped table that carried fewer
// rows than the data has.
function sabelaGridCountText(state) {
  const shown = state.order.length;
  const carried = state.raw.rows.length;
  const total = state.raw.total ?? carried;
  if (!shown) return total > carried ? `0 of first ${carried} rows` : `0 of ${total} rows`;
  const a = state.page * state.pageSize + 1;
  const b = Math.min(shown, a + state.pageSize - 1);
  const span = a === b ? `${a}` : `${a}–${b}`;
  const of = shown === carried ? '' : ` (${shown} match)`;
  if (total > carried) return `${span} of first ${carried}${of} · ${total.toLocaleString()} rows`;
  return `${span} of ${shown.toLocaleString()} rows${of}`;
}

// Which page numbers the pager shows: the first and last always, the
// current page with a neighbour either side, and a gap where pages are
// skipped.
function sabelaGridPageList(page, pages) {
  if (pages <= 7) return Array.from({ length: pages }, (_, i) => i);
  const want = new Set([0, pages - 1, page - 1, page, page + 1]);
  if (page < 3) [1, 2, 3].forEach((i) => want.add(i));
  if (page > pages - 4) [pages - 4, pages - 3, pages - 2].forEach((i) => want.add(i));
  const out = [];
  let prev = -1;
  [...want]
    .filter((i) => i >= 0 && i < pages)
    .sort((a, b) => a - b)
    .forEach((i) => {
      if (prev >= 0 && i - prev > 1) out.push('gap');
      out.push(i);
      prev = i;
    });
  return out;
}

function sabelaGridPager(state) {
  const pager = document.createElement('div');
  pager.className = 'sg-pager';
  const pages = sabelaGridPages(state);
  const button = (label, target, title) => {
    const b = document.createElement('button');
    b.className = 'sg-page';
    b.type = 'button';
    b.textContent = label;
    if (title) b.title = title;
    b.disabled = target < 0 || target >= pages;
    if (target === state.page && typeof label === 'number') b.setAttribute('aria-current', 'page');
    b.onclick = () => {
      state.page = target;
      state.repaint();
    };
    pager.appendChild(b);
  };
  button('‹', state.page - 1, 'previous page');
  sabelaGridPageList(state.page, pages).forEach((p) => {
    if (p === 'gap') {
      const gap = document.createElement('span');
      gap.className = 'sg-gap';
      gap.textContent = '…';
      pager.appendChild(gap);
    } else button(p + 1, p);
  });
  button('›', state.page + 1, 'next page');
  return pager;
}

function sabelaGridFooter(state) {
  const foot = document.createElement('div');
  foot.className = 'sg-foot';
  const count = document.createElement('div');
  count.className = 'sg-count';
  const text = document.createElement('span');
  text.textContent = sabelaGridCountText(state);
  count.appendChild(text);
  if (state.selected.size) {
    const sel = document.createElement('span');
    sel.textContent = `${state.selected.size} selected`;
    count.appendChild(sel);
  }
  if (state.raw.rows.length > SABELA_GRID_PAGE_SIZES[0]) {
    const size = document.createElement('select');
    size.className = 'sg-size';
    size.title = 'rows per page';
    SABELA_GRID_PAGE_SIZES.forEach((n) => {
      const o = document.createElement('option');
      o.value = n;
      o.textContent = `${n} / page`;
      o.selected = n === state.pageSize;
      size.appendChild(o);
    });
    size.onchange = () => {
      const first = state.page * state.pageSize;
      state.pageSize = Number(size.value);
      state.page = Math.floor(first / state.pageSize);
      state.repaint();
    };
    count.appendChild(size);
  }
  foot.appendChild(count);
  if (sabelaGridPages(state) > 1) foot.appendChild(sabelaGridPager(state));
  return foot;
}

// Turns one emitted payload into a live grid. Called once per table, with the
// container the notebook printed; called again by the expression box, with
// `again` carrying the rows to show, the original payload, and the box's text.
function sabelaGrid(container, again) {
  const raw = again
    ? again.raw
    : sabelaGridNormalize(
        JSON.parse(container.querySelector('script.sabela-grid-data').textContent)
      );
  const theme = sabelaGridTheme();
  sabelaGridStyle(theme);
  const cols = sabelaGridColumns(raw);
  const state = {
    raw,
    cols,
    theme,
    container,
    slot: again ? again.slot : container.dataset.frameSlot || '',
    queryFn: again ? again.queryFn : container.dataset.frameQuery || '',
    original: again ? again.original : raw,
    expr: again ? again.expr || '' : '',
    note: again ? again.note || null : null,
    sort: null,
    query: '',
    page: 0,
    pageSize: SABELA_GRID_PAGE_SIZES[0],
    current: -1,
    selected: new Set(),
    order: raw.rows.map((_, n) => n),
  };
  container.className = 'sg';
  container.textContent = '';
  const scroll = document.createElement('div');
  scroll.className = 'sg-scroll';
  const table = document.createElement('table');
  const tbody = document.createElement('tbody');
  scroll.appendChild(table);

  const swapFooter = () => {
    const foot = container.querySelector('.sg-foot');
    if (foot) container.replaceChild(sabelaGridFooter(state), foot);
  };
  const refresh = () => {
    state.order = sabelaGridOrder(raw, cols, state.sort, state.query);
    table.textContent = '';
    table.appendChild(sabelaGridHeader(state, refresh));
    table.appendChild(tbody);
    sabelaGridBody(state, tbody);
    swapFooter();
  };
  state.repaint = () => {
    sabelaGridBody(state, tbody);
    swapFooter();
  };
  container.appendChild(sabelaGridToolbar(state, refresh));
  container.appendChild(scroll);
  container.appendChild(sabelaGridFooter(state));
  refresh();
}

// Every grid the page printed, mounted the moment its markup exists. A grid
// that took the host's palette says so, so the host can drop the plain card it
// draws behind an output whose colours it cannot know.
function sabelaGridMountAll() {
  let mounted = 0;
  document.querySelectorAll('div.sabela-grid:not([data-mounted])').forEach((el) => {
    el.setAttribute('data-mounted', '1');
    sabelaGrid(el);
    mounted++;
  });
  const themed = getComputedStyle(document.documentElement)
    .getPropertyValue('--sabela-surface')
    .trim();
  if (mounted && themed && parent !== window) {
    try {
      parent.postMessage({ type: 'sabelaGrid', themed: true }, '*');
    } catch (_e) {
      /* host not listening; the card simply stays */
    }
  }
}
