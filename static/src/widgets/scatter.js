// scatterSelect widget renderer. Runs inside a cell's sandboxed output
// iframe; reports the lasso selection to the editor via parent.postMessage,
// where 22-widget-bridge.js POSTs it to /api/widget. The Haskell side
// (Sabela.Output.Scatter) embeds this file and emits a sabelaScatter(cfg)
// bootstrap call carrying the per-render data. Framing and zoom maths live in
// scatter-view.js, painting in scatter-draw.js, both embedded just ahead.
function sabelaScatter(cfg) {
  var PTS = cfg.pts;
  var CVAL = cfg.cval;
  var cv = document.getElementById(cfg.elId);
  if (!cv || !PTS.length) return;
  var hasC = CVAL.length === PTS.length;
  var N = PTS.length;

  var box = { l: cfg.ylab ? 56 : 44, t: cfg.title ? 26 : 12 };
  box.w = cfg.w - box.l - (hasC ? 56 : 16);
  box.h = cfg.h - box.t - (cfg.xlab ? 42 : 28);

  var DX = new Float64Array(N),
    DY = new Float64Array(N);
  for (var i = 0; i < N; i++) {
    DX[i] = PTS[i][0];
    DY[i] = PTS[i][1];
  }

  var cmin = Infinity,
    cmax = -Infinity;
  for (var i = 0; hasC && i < N; i++) {
    if (CVAL[i] < cmin) cmin = CVAL[i];
    if (CVAL[i] > cmax) cmax = CVAL[i];
  }
  if (cmin === cmax) {
    cmin -= 1;
    cmax += 1;
  }

  // Explicit bounds are the caller's frame; otherwise the bulk of the cloud
  // sets it, so a few stragglers cannot shrink everything else to a dot.
  var xr = cfg.xb ? cfg.xb.slice() : sabelaScatterPad(sabelaScatterRobustRange(DX, true), 0.04);
  var yr = cfg.yb ? cfg.yb.slice() : sabelaScatterPad(sabelaScatterRobustRange(DY, true), 0.04);
  var home = sabelaScatterFit(box, xr, yr, !!cfg.eq);

  var plot = {
    box: box,
    view: { ux: home.ux, uy: home.uy, cx: home.cx, cy: home.cy },
    n: N,
    xs: new Float64Array(N),
    ys: new Float64Array(N),
    selected: new Set(cfg.sel),
    colorOf: function (i) {
      return hasC ? sabelaScatterRamp((CVAL[i] - cmin) / (cmax - cmin)) : cfg.color;
    },
    r: cfg.r,
    alpha: cfg.alpha,
    selColor: cfg.selColor,
    title: cfg.title,
    xlab: cfg.xlab,
    ylab: cfg.ylab,
    hasC: hasC,
    cmin: cmin,
    cmax: cmax,
    w: cfg.w,
    h: cfg.h,
  };

  // A backing store at device resolution, with every coordinate below still in
  // CSS pixels: the difference is what makes text and dots crisp on a HiDPI
  // screen rather than an upscaled 1x image.
  var dpr = Math.max(1, Math.min(3, window.devicePixelRatio || 1));
  var base = document.createElement('canvas');
  cv.width = base.width = Math.round(cfg.w * dpr);
  cv.height = base.height = Math.round(cfg.h * dpr);
  cv.style.width = cfg.w + 'px';
  cv.style.height = cfg.h + 'px';
  var ctx = cv.getContext('2d');
  var bctx = base.getContext('2d');
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  bctx.setTransform(dpr, 0, 0, dpr, 0, 0);

  function drawBase() {
    for (var i = 0; i < N; i++) {
      plot.xs[i] = sabelaScatterToX(plot.view, box, DX[i]);
      plot.ys[i] = sabelaScatterToY(plot.view, box, DY[i]);
    }
    bctx.clearRect(0, 0, cfg.w, cfg.h);
    sabelaScatterPaintAxes(bctx, plot);
    sabelaScatterPaintPoints(bctx, plot);
    sabelaScatterPaintLabels(bctx, plot);
  }

  function repaint(poly) {
    ctx.clearRect(0, 0, cfg.w, cfg.h);
    ctx.drawImage(base, 0, 0, cfg.w, cfg.h);
    if (!poly || poly.length < 2) return;
    ctx.strokeStyle = cfg.selColor;
    ctx.fillStyle = 'rgba(227,17,108,0.08)';
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.moveTo(poly[0][0], poly[0][1]);
    for (var i = 1; i < poly.length; i++) ctx.lineTo(poly[i][0], poly[i][1]);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();
  }

  // Zoom and pan arrive faster than the screen refreshes, so coalesce redraws.
  var frame = 0;
  function redraw() {
    if (frame) return;
    frame = requestAnimationFrame(function () {
      frame = 0;
      drawBase();
      repaint(null);
    });
  }

  function onPlot(i) {
    return (
      plot.xs[i] >= box.l &&
      plot.xs[i] <= box.l + box.w &&
      plot.ys[i] >= box.t &&
      plot.ys[i] <= box.t + box.h
    );
  }

  // Pointer position in CSS pixels, which is the space every draw works in
  // even when the iframe has scaled the canvas down.
  function pt(e) {
    var r = cv.getBoundingClientRect();
    return [((e.clientX - r.left) * cfg.w) / r.width, ((e.clientY - r.top) * cfg.h) / r.height];
  }

  function post(idx) {
    parent.postMessage(
      { type: 'widget', cellId: cfg.cid, name: cfg.name, value: '[' + idx.join(',') + ']' },
      '*'
    );
  }

  drawBase();
  repaint(null);

  var mode = null,
    poly = [],
    last = null;
  cv.addEventListener('contextmenu', function (e) {
    e.preventDefault();
  });
  cv.addEventListener('pointerdown', function (e) {
    if (e.button !== 0 && e.button !== 2) return;
    mode = e.button === 2 || e.shiftKey ? 'pan' : 'lasso';
    last = pt(e);
    poly = mode === 'lasso' ? [last] : [];
    cv.setPointerCapture(e.pointerId);
    e.preventDefault();
  });
  cv.addEventListener('pointermove', function (e) {
    if (!mode) return;
    var p = pt(e);
    if (mode === 'pan') {
      plot.view = sabelaScatterPan(plot.view, p[0] - last[0], p[1] - last[1]);
      last = p;
      redraw();
    } else {
      poly.push(p);
      repaint(poly);
    }
    e.preventDefault();
  });
  cv.addEventListener('pointerup', function (e) {
    if (!mode) return;
    var lassoed = mode === 'lasso' && poly.length >= 3;
    mode = null;
    if (!lassoed) {
      repaint(null);
      return;
    }
    var idx = [];
    for (var i = 0; i < N; i++) {
      if (onPlot(i) && sabelaScatterInPoly(plot.xs[i], plot.ys[i], poly)) idx.push(i);
    }
    plot.selected = new Set(idx);
    drawBase();
    repaint(null);
    post(idx);
  });
  cv.addEventListener(
    'wheel',
    function (e) {
      var p = pt(e);
      var next = sabelaScatterZoom(plot.view, box, p[0], p[1], Math.pow(1.0015, e.deltaY));
      if (next.ux < home.ux / 5000 || next.ux > home.ux * 200) return;
      plot.view = next;
      redraw();
      e.preventDefault();
    },
    { passive: false }
  );
  cv.addEventListener('dblclick', function () {
    plot.view = { ux: home.ux, uy: home.uy, cx: home.cx, cy: home.cy };
    plot.selected = new Set();
    redraw();
    post([]);
  });
}
