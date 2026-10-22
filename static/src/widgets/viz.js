// Chart runtime: mounts every `.sabela-viz` in the document with Vega-Lite,
// themed from the notebook palette the host writes into :root. Vega itself is
// loaded once from the binary's /vendor route, with the jsDelivr copies as a
// fallback for an export opened away from a server.
var SABELA_VEZ_LOCAL = [
  '/vendor/vega/vega.min.js',
  '/vendor/vega/vega-lite.min.js',
  '/vendor/vega/vega-embed.min.js',
];
var SABELA_VEZ_CDN = [
  'https://cdn.jsdelivr.net/npm/vega@5.30.0/build/vega.min.js',
  'https://cdn.jsdelivr.net/npm/vega-lite@5.21.0/build/vega-lite.min.js',
  'https://cdn.jsdelivr.net/npm/vega-embed@6.26.0/build/vega-embed.min.js',
];
var _sabelaVizWaiters = [];
var _sabelaVizLoading = false;

function sabelaVizEnsureVega(cb) {
  if (window.vegaEmbed) return cb(true);
  _sabelaVizWaiters.push(cb);
  if (_sabelaVizLoading) return;
  _sabelaVizLoading = true;
  var done = function (ok) {
    _sabelaVizLoading = false;
    var ws = _sabelaVizWaiters.splice(0);
    ws.forEach(function (w) {
      w(ok);
    });
  };
  var loadSeq = function (urls, i, next) {
    if (i >= urls.length) return next(true);
    var s = document.createElement('script');
    s.src = urls[i];
    s.onload = function () {
      loadSeq(urls, i + 1, next);
    };
    s.onerror = function () {
      next(false);
    };
    document.head.appendChild(s);
  };
  var sources = window.__SABELA_VEGA__ ? [] : [SABELA_VEZ_LOCAL, SABELA_VEZ_CDN];
  if (window.__SABELA_VEGA__) {
    try {
      new Function(window.__SABELA_VEGA__)();
    } catch (_e) {}
    if (window.vegaEmbed) return done(true);
    sources = [SABELA_VEZ_CDN];
  }
  var trySource = function (k) {
    if (k >= sources.length) return done(!!window.vegaEmbed);
    loadSeq(sources[k], 0, function (ok) {
      if (ok && window.vegaEmbed) done(true);
      else trySource(k + 1);
    });
  };
  trySource(0);
}

function sabelaVizVar(name, fallback) {
  var v = getComputedStyle(document.documentElement).getPropertyValue(name);
  return (v || fallback).trim();
}

// Metabase's series palette, led by the notebook's own accent.
var SABELA_VIZ_SERIES = [
  '#509ee3',
  '#88bf4d',
  '#a989c5',
  '#ef8c8c',
  '#f9d45c',
  '#f2a86f',
  '#98d9d9',
  '#7172ad',
];
function sabelaVizPalette(accent) {
  var rest = SABELA_VIZ_SERIES.filter(function (c) {
    return c.toLowerCase() !== (accent || '').toLowerCase();
  });
  return [accent].concat(rest);
}

var SABELA_VIZ_FONT = "'Lato', -apple-system, BlinkMacSystemFont, system-ui, sans-serif";

// Light or dark, judged from the surface the chart sits on.
function sabelaVizIsDark() {
  var m = /^#([0-9a-f]{6})$/i.exec(sabelaVizVar('--sabela-raised', '#ffffff'));
  if (!m) return false;
  var n = parseInt(m[1], 16);
  var lum = 0.299 * (n >> 16) + 0.587 * ((n >> 8) & 255) + 0.114 * (n & 255);
  return lum < 128;
}

// The chart's look: no axis lines or ticks, light gridlines, quiet labels,
// bold short titles, compact numbers, series in the Metabase palette.
function sabelaVizThemeConfig() {
  var fg = sabelaVizVar('--sabela-fg', '#696e7b');
  var dim = sabelaVizVar('--sabela-dim', '#949aab');
  var border = sabelaVizVar('--sabela-border', '#eeecec');
  var accent = sabelaVizVar('--sabela-accent', '#509ee3');
  var strong = sabelaVizIsDark() ? '#e8ecef' : '#4c5773';
  return {
    background: 'transparent',
    font: SABELA_VIZ_FONT,
    padding: { left: 4, right: 12, top: 6, bottom: 4 },
    view: { stroke: null },
    axis: {
      domain: false,
      ticks: false,
      gridColor: border,
      gridWidth: 1,
      labelColor: dim,
      labelFontSize: 11,
      labelPadding: 8,
      labelFont: SABELA_VIZ_FONT,
      titleColor: fg,
      titleFontSize: 12,
      titleFontWeight: 700,
      titleFont: SABELA_VIZ_FONT,
      titlePadding: 12,
    },
    axisX: { grid: false, labelAngle: 0, labelLimit: 120, labelOverlap: true },
    axisY: { labelLimit: 120 },
    axisQuantitative: { format: '~s' },
    legend: {
      orient: 'right',
      offset: 18,
      padding: 0,
      labelColor: fg,
      labelFontSize: 11,
      labelFont: SABELA_VIZ_FONT,
      titleColor: dim,
      titleFontSize: 11,
      titleFontWeight: 700,
      titleFont: SABELA_VIZ_FONT,
      symbolType: 'circle',
      symbolSize: 70,
      rowPadding: 4,
    },
    title: {
      color: strong,
      fontSize: 14,
      fontWeight: 700,
      font: SABELA_VIZ_FONT,
      anchor: 'start',
      offset: 14,
    },
    header: {
      labelColor: strong,
      titleColor: fg,
      labelFont: SABELA_VIZ_FONT,
      titleFont: SABELA_VIZ_FONT,
    },
    mark: { color: accent },
    bar: { binSpacing: 2 },
    line: { strokeWidth: 2.5, point: { filled: true, size: 36 } },
    area: { opacity: 0.35, line: true },
    point: { size: 60, filled: true, opacity: 0.85 },
    tick: { thickness: 2 },
    scale: { bandPaddingInner: 0.3, bandPaddingOuter: 0.15 },
    range: { category: sabelaVizPalette(accent) },
  };
}

// A scatter whose code says nothing about zero fits its axes to the data,
// as the library's own scatter helper does.
function sabelaVizFitScatter(vl) {
  var layers = vl.layer && vl.layer.length ? vl.layer : [vl];
  layers.forEach(function (l) {
    var m = l.mark && (typeof l.mark === 'string' ? l.mark : l.mark.type);
    if (m !== 'point' || !l.encoding) return;
    ['x', 'y'].forEach(function (ch) {
      var e = l.encoding[ch];
      if (!e || e.type !== 'quantitative' || e.bin) return;
      e.scale = e.scale || {};
      if (e.scale.zero === undefined) e.scale.zero = false;
    });
  });
}

// Many categories along x read better slanted than hidden.
function sabelaVizSlantCrowdedX(vl, rows) {
  var encs = [vl.encoding].concat(
    (vl.layer || []).map(function (l) {
      return l.encoding;
    })
  );
  encs.forEach(function (enc) {
    var x = enc && enc.x;
    if (!x || !x.field || (x.type !== 'nominal' && x.type !== 'ordinal')) return;
    var seen = {};
    rows.forEach(function (r) {
      seen[r[sabelaVizFieldName(x.field)]] = true;
    });
    var n = Object.keys(seen).length;
    var longest = Object.keys(seen).reduce(function (m, k) {
      return Math.max(m, k.length);
    }, 0);
    if (n > 8 || longest > 14) {
      x.axis = x.axis || {};
      x.axis.labelAngle = -45;
      x.axis.labelAlign = 'right';
      x.axis.labelBaseline = 'middle';
    }
  });
}

function sabelaVizReportHeight() {
  var h = document.documentElement.scrollHeight;
  try {
    parent.postMessage({ type: 'sabelaResize', height: h }, '*');
  } catch (_e) {}
  if (typeof window.__sabelaReportHeight === 'function') window.__sabelaReportHeight();
}

// A Vega-Lite field reference back to the column name: `sepal\.length` is the
// column `sepal.length`, escaped so Vega-Lite does not read nested access.
function sabelaVizFieldName(ref) {
  return String(ref).replace(/\\(.)/g, '$1');
}

// Rows arrive as text; a quantitative or temporal field is coerced so Vega
// never sorts "9" after "10".
// What the host's Design panel needs from a chart: the flat spec the
// Vega-Lite says (mark, channels, aggregates, bins), and the columns and
// kinds of the rows it carries.
function sabelaVizFlatSpec(vl) {
  var layerOf = function (l) {
    var out = { mark: l.mark && (typeof l.mark === 'string' ? l.mark : l.mark.type) };
    Object.keys(l.encoding || {}).forEach(function (ch) {
      var e = l.encoding[ch];
      if (!e) return;
      var enc = { field: e.field ? sabelaVizFieldName(e.field) : '*' };
      if (e.aggregate) enc.aggregate = e.aggregate;
      if (e.bin) enc.bin = typeof e.bin === 'object' && e.bin.maxbins ? e.bin.maxbins : 10;
      if (e.scale && e.scale.type) enc.scale = e.scale.type;
      if (e.scale && typeof e.scale.zero === 'boolean') enc.zero = e.scale.zero;
      out[ch] = ch === 'tooltip' ? [enc] : enc;
    });
    return out;
  };
  var spec = vl.layer && vl.layer.length ? layerOf(vl.layer[0]) : layerOf(vl);
  if (vl.layer && vl.layer.length > 1) spec.layers = vl.layer.slice(1).map(layerOf);
  if (typeof vl.title === 'string') spec.title = vl.title;
  var defaultSize =
    (vl.width == null || vl.width === 600) && (vl.height == null || vl.height === 400);
  if (!defaultSize) {
    spec.width = typeof vl.width === 'number' ? vl.width : 600;
    spec.height = typeof vl.height === 'number' ? vl.height : 400;
  }
  return spec;
}

function sabelaVizColumns(rows) {
  var cols = {};
  rows.forEach(function (r) {
    Object.keys(r).forEach(function (k) {
      var v = r[k];
      var kind =
        v == null
          ? null
          : typeof v === 'number'
            ? 'number'
            : typeof v === 'boolean'
              ? 'boolean'
              : 'string';
      if (kind && !cols[k]) cols[k] = kind;
      else if (kind && cols[k] && cols[k] !== kind) cols[k] = 'string';
      else if (!cols[k]) cols[k] = cols[k] || null;
    });
  });
  var names = Object.keys(cols);
  return {
    cols: names,
    types: names.map(function (n) {
      return cols[n] || '';
    }),
  };
}

function sabelaVizMount(el) {
  if (el.dataset.mounted) return;
  el.dataset.mounted = '1';
  var holder = el.querySelector('script.sabela-viz-spec');
  var view = el.querySelector('.sabela-viz-view');
  var payload;
  try {
    payload = JSON.parse(holder.textContent);
  } catch (e) {
    view.textContent = 'chart payload unreadable: ' + e.message;
    return;
  }
  var vl = payload.vegaLite;
  if (sabelaVizVar('--sabela-live', '')) delete vl.title;
  var rows = (vl.data && vl.data.values) || [];
  var flat = sabelaVizFlatSpec(vl);
  sabelaVizFitScatter(vl);
  sabelaVizSlantCrowdedX(vl, rows);
  vl.width = 'container';
  vl.autosize = { type: 'fit-x', contains: 'padding' };
  if (vl.height == null || vl.height === 400) vl.height = 300;
  view.style.width = '100%';
  var note = '';
  sabelaVizEnsureVega(function (ok) {
    if (!ok) {
      view.textContent = 'chart runtime unavailable (Vega could not be loaded)';
      sabelaVizReportHeight();
      return;
    }
    vegaEmbed(view, vl, {
      actions: false,
      renderer: 'svg',
      config: sabelaVizThemeConfig(),
      tooltip: { theme: sabelaVizIsDark() ? 'dark' : 'light' },
    })
      .then(function (res) {
        var svg = view.querySelector('svg');
        if (svg) svg.style.display = 'block';
        document.body.style.paddingBottom = '0';
        if (note) {
          var n = document.createElement('div');
          n.textContent = note;
          n.style.cssText = 'font-size:11px;opacity:.6;margin-top:4px';
          el.appendChild(n);
        }
        sabelaVizReportHeight();
        setTimeout(sabelaVizReportHeight, 80);
        if (window.ResizeObserver) new ResizeObserver(sabelaVizReportHeight).observe(view);
        try {
          parent.postMessage(
            {
              type: 'sabelaViz',
              cellId: Number(el.getAttribute('data-cell-id')),
              spec: flat,
              cols: sabelaVizColumns(rows).cols,
              types: sabelaVizColumns(rows).types,
            },
            '*'
          );
        } catch (_e) {}
      })
      .catch(function (e) {
        view.textContent = 'chart error: ' + (e && e.message ? e.message : e);
        sabelaVizReportHeight();
      });
  });
}

// A chart paints on the card it sits in, in the page's own text face, so it
// reads as part of the notebook rather than as an inset panel.
function sabelaVizAdoptSurface() {
  var surface = sabelaVizVar('--sabela-raised', '') || sabelaVizVar('--sabela-surface', '');
  if (!surface) return;
  document.body.style.background = surface;
  document.body.style.color = sabelaVizVar('--sabela-fg', '');
  document.body.style.fontFamily = SABELA_VIZ_FONT;
  try {
    parent.postMessage({ type: 'sabelaGrid', themed: true }, '*');
  } catch (_e) {}
}

function sabelaVizMountAll() {
  var els = document.querySelectorAll('div.sabela-viz');
  if (!els.length) return;
  sabelaVizAdoptSurface();
  els.forEach(sabelaVizMount);
}
