// Framing, zoom and tick maths for the scatterSelect widget. Pure functions
// over numbers; scatter.js owns the canvas, the pointer events and the
// selection. Sabela.Output.Scatter embeds this file ahead of scatter.js.

// Below this many points the tail of a distribution is not meaningful, so
// robust framing falls back to the full extent.
var SABELA_SCATTER_MIN_ROBUST = 200;

// The 1st to 99th percentile of one axis, so that a handful of outliers cannot
// squash the cloud into the middle of the canvas.
function sabelaScatterRobustRange(values, robust) {
  var n = values.length;
  if (!n) return [0, 1];
  if (!robust || n < SABELA_SCATTER_MIN_ROBUST) {
    var lo = Infinity,
      hi = -Infinity;
    for (var i = 0; i < n; i++) {
      if (values[i] < lo) lo = values[i];
      if (values[i] > hi) hi = values[i];
    }
    return [lo, hi];
  }
  var sorted = Array.prototype.slice.call(values).sort(function (a, b) {
    return a - b;
  });
  return [sorted[Math.floor(0.01 * (n - 1))], sorted[Math.ceil(0.99 * (n - 1))]];
}

// Widens a range by a fraction of its width on each side, and gives a
// degenerate range something to span.
function sabelaScatterPad(range, frac) {
  var lo = range[0],
    hi = range[1];
  if (!(hi > lo)) {
    lo -= 1;
    hi += 1;
  }
  var margin = (hi - lo) * frac;
  return [lo - margin, hi + margin];
}

// A view is data units per pixel on each axis plus the data point sitting at
// the centre of the plot box. Equal aspect is one shared units-per-pixel, which
// is what keeps distances in an embedding honest.
function sabelaScatterFit(box, xr, yr, equalAspect) {
  var ux = (xr[1] - xr[0]) / Math.max(1, box.w);
  var uy = (yr[1] - yr[0]) / Math.max(1, box.h);
  if (equalAspect) {
    ux = Math.max(ux, uy);
    uy = ux;
  }
  return { ux: ux, uy: uy, cx: (xr[0] + xr[1]) / 2, cy: (yr[0] + yr[1]) / 2 };
}

function sabelaScatterToX(view, box, x) {
  return box.l + box.w / 2 + (x - view.cx) / view.ux;
}

function sabelaScatterToY(view, box, y) {
  return box.t + box.h / 2 - (y - view.cy) / view.uy;
}

function sabelaScatterFromX(view, box, px) {
  return view.cx + (px - box.l - box.w / 2) * view.ux;
}

function sabelaScatterFromY(view, box, py) {
  return view.cy - (py - box.t - box.h / 2) * view.uy;
}

// Zooms about a pixel, keeping the data point under the cursor where it is.
function sabelaScatterZoom(view, box, px, py, factor) {
  var dx = sabelaScatterFromX(view, box, px);
  var dy = sabelaScatterFromY(view, box, py);
  var zoomed = { ux: view.ux * factor, uy: view.uy * factor, cx: 0, cy: 0 };
  zoomed.cx = dx - (px - box.l - box.w / 2) * zoomed.ux;
  zoomed.cy = dy + (py - box.t - box.h / 2) * zoomed.uy;
  return zoomed;
}

function sabelaScatterPan(view, dxPx, dyPx) {
  return {
    ux: view.ux,
    uy: view.uy,
    cx: view.cx - dxPx * view.ux,
    cy: view.cy + dyPx * view.uy,
  };
}

function sabelaScatterBounds(view, box) {
  return {
    minX: sabelaScatterFromX(view, box, box.l),
    maxX: sabelaScatterFromX(view, box, box.l + box.w),
    minY: sabelaScatterFromY(view, box, box.t + box.h),
    maxY: sabelaScatterFromY(view, box, box.t),
  };
}

// Tick values on a 1-2-5 ladder, about `target` of them across lo..hi.
function sabelaScatterTicks(lo, hi, target) {
  if (!(hi > lo)) return [lo];
  var raw = (hi - lo) / Math.max(1, target);
  var mag = Math.pow(10, Math.floor(Math.log(raw) / Math.LN10));
  var norm = raw / mag;
  var step = mag * (norm < 1.5 ? 1 : norm < 3 ? 2 : norm < 7 ? 5 : 10);
  var out = [];
  for (var v = Math.ceil(lo / step) * step; v <= hi + step * 1e-9; v += step) out.push(v);
  return out;
}

// Enough decimals to tell neighbouring ticks apart, and no negative zero.
function sabelaScatterTickLabel(v, step) {
  var decimals = Math.max(0, Math.min(6, -Math.floor(Math.log(Math.abs(step)) / Math.LN10)));
  var s = v.toFixed(decimals);
  return /^-0(\.0*)?$/.test(s) ? s.slice(1) : s;
}

// Winding test for the lasso, in pixel space.
function sabelaScatterInPoly(px, py, poly) {
  var c = false;
  for (var i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    var xi = poly[i][0],
      yi = poly[i][1],
      xj = poly[j][0],
      yj = poly[j][1];
    if (yi > py !== yj > py && px < ((xj - xi) * (py - yi)) / (yj - yi) + xi) c = !c;
  }
  return c;
}
