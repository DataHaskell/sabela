// scatterSelect widget renderer. Runs inside a cell's sandboxed output
// iframe; reports the lasso selection to the editor via parent.postMessage,
// where 22-widget-bridge.js POSTs it to /api/widget. The Haskell side
// (Sabela.Output.Scatter) embeds this file and emits a sabelaScatter(cfg)
// bootstrap call carrying the per-render data.
function sabelaScatter(cfg) {
  var PTS = cfg.pts;
  var SEL = cfg.sel;
  var CVAL = cfg.cval;
  var NAME = cfg.name;
  var CID = cfg.cid;
  var W = cfg.w,
    H = cfg.h,
    R = cfg.r,
    ALPHA = cfg.alpha;
  var COLOR = cfg.color,
    SELCOLOR = cfg.selColor;
  var TITLE = cfg.title,
    XLAB = cfg.xlab,
    YLAB = cfg.ylab;
  var XB = cfg.xb,
    YB = cfg.yb;
  var cv = document.getElementById(cfg.elId);
  if (!cv) return;
  var ctx = cv.getContext('2d');
  if (!PTS.length) {
    return;
  }
  var hasC = CVAL.length === PTS.length;
  var L = YLAB ? 52 : 40,
    Rm = hasC ? 54 : 14,
    T = TITLE ? 26 : 12,
    B = XLAB ? 40 : 26;
  var minX = Infinity,
    maxX = -Infinity,
    minY = Infinity,
    maxY = -Infinity;
  for (var i = 0; i < PTS.length; i++) {
    var p = PTS[i];
    if (p[0] < minX) minX = p[0];
    if (p[0] > maxX) maxX = p[0];
    if (p[1] < minY) minY = p[1];
    if (p[1] > maxY) maxY = p[1];
  }
  if (XB) {
    minX = XB[0];
    maxX = XB[1];
  }
  if (YB) {
    minY = YB[0];
    maxY = YB[1];
  }
  if (minX === maxX) {
    minX -= 1;
    maxX += 1;
  }
  if (minY === maxY) {
    minY -= 1;
    maxY += 1;
  }
  var cmin = Infinity,
    cmax = -Infinity;
  if (hasC) {
    for (var i = 0; i < CVAL.length; i++) {
      if (CVAL[i] < cmin) cmin = CVAL[i];
      if (CVAL[i] > cmax) cmax = CVAL[i];
    }
    if (cmin === cmax) {
      cmin -= 1;
      cmax += 1;
    }
  }
  function sx(x) {
    return L + ((x - minX) / (maxX - minX)) * (W - L - Rm);
  }
  function sy(y) {
    return H - B - ((y - minY) / (maxY - minY)) * (H - B - T);
  }
  var STOPS = [
    [68, 1, 84],
    [59, 82, 139],
    [33, 145, 140],
    [94, 201, 98],
    [253, 231, 37],
  ];
  function grad(t) {
    if (t < 0) t = 0;
    if (t > 1) t = 1;
    var s = t * 4,
      i = Math.floor(s),
      f = s - i;
    if (i >= 4) {
      i = 3;
      f = 1;
    }
    var a = STOPS[i],
      b = STOPS[i + 1];
    return (
      'rgb(' +
      Math.round(a[0] + (b[0] - a[0]) * f) +
      ',' +
      Math.round(a[1] + (b[1] - a[1]) * f) +
      ',' +
      Math.round(a[2] + (b[2] - a[2]) * f) +
      ')'
    );
  }
  function colorOf(i) {
    return hasC ? grad((CVAL[i] - cmin) / (cmax - cmin)) : COLOR;
  }
  var XS = new Float64Array(PTS.length),
    YS = new Float64Array(PTS.length);
  for (var i = 0; i < PTS.length; i++) {
    XS[i] = sx(PTS[i][0]);
    YS[i] = sy(PTS[i][1]);
  }
  var base = document.createElement('canvas');
  base.width = W;
  base.height = H;
  var bctx = base.getContext('2d');
  function drawBase(sset) {
    bctx.clearRect(0, 0, W, H);
    if (TITLE) {
      bctx.fillStyle = '#222';
      bctx.font = '600 13px sans-serif';
      bctx.textAlign = 'center';
      bctx.fillText(TITLE, W / 2, 16);
    }
    bctx.strokeStyle = '#d7d7e0';
    bctx.lineWidth = 1;
    bctx.beginPath();
    bctx.moveTo(L, T - 4);
    bctx.lineTo(L, H - B);
    bctx.lineTo(W - Rm + 4, H - B);
    bctx.stroke();
    bctx.globalAlpha = ALPHA;
    for (var i = 0; i < PTS.length; i++) {
      if (sset && sset.has(i)) continue;
      bctx.fillStyle = colorOf(i);
      bctx.fillRect(XS[i] - R, YS[i] - R, 2 * R, 2 * R);
    }
    bctx.globalAlpha = 1;
    if (sset) {
      bctx.fillStyle = SELCOLOR;
      sset.forEach(function (k) {
        var s = R + 1;
        bctx.fillRect(XS[k] - s, YS[k] - s, 2 * s, 2 * s);
      });
    }
    bctx.fillStyle = '#99a';
    bctx.font = '10px sans-serif';
    bctx.textAlign = 'start';
    bctx.fillText(String(+minX.toFixed(2)), L, H - B + 14);
    bctx.fillText(String(+maxX.toFixed(2)), W - Rm - 32, H - B + 14);
    bctx.fillText(String(+maxY.toFixed(2)), 4, T + 6);
    bctx.fillText(String(+minY.toFixed(2)), 4, H - B);
    bctx.fillStyle = '#556';
    bctx.font = '11px sans-serif';
    bctx.textAlign = 'center';
    if (XLAB) bctx.fillText(XLAB, L + (W - L - Rm) / 2, H - 6);
    if (YLAB) {
      bctx.save();
      bctx.translate(12, T + (H - B - T) / 2);
      bctx.rotate(-Math.PI / 2);
      bctx.fillText(YLAB, 0, 0);
      bctx.restore();
    }
    if (hasC) {
      var bx = W - Rm + 14,
        bw = 10,
        bh = H - B - T;
      for (var g = 0; g < bh; g++) {
        bctx.fillStyle = grad(1 - g / bh);
        bctx.fillRect(bx, T + g, bw, 1);
      }
      bctx.fillStyle = '#99a';
      bctx.font = '9px sans-serif';
      bctx.textAlign = 'start';
      bctx.fillText(String(+cmax.toFixed(1)), bx - 3, T - 3);
      bctx.fillText(String(+cmin.toFixed(1)), bx - 3, T + bh + 10);
    }
  }
  function repaint(poly) {
    ctx.clearRect(0, 0, W, H);
    ctx.drawImage(base, 0, 0);
    if (poly && poly.length > 1) {
      ctx.strokeStyle = SELCOLOR;
      ctx.fillStyle = 'rgba(227,17,108,0.08)';
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      ctx.moveTo(poly[0][0], poly[0][1]);
      for (var i = 1; i < poly.length; i++) ctx.lineTo(poly[i][0], poly[i][1]);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
    }
  }
  function inPoly(px, py, poly) {
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
  function pt(e) {
    return [e.offsetX * (cv.width / cv.clientWidth), e.offsetY * (cv.height / cv.clientHeight)];
  }
  function post(idx) {
    parent.postMessage(
      { type: 'widget', cellId: CID, name: NAME, value: '[' + idx.join(',') + ']' },
      '*'
    );
  }
  drawBase(new Set(SEL));
  repaint(null);
  var drawing = false,
    poly = [];
  cv.addEventListener('pointerdown', function (e) {
    if (e.button !== 0) return;
    drawing = true;
    poly = [pt(e)];
    cv.setPointerCapture(e.pointerId);
    e.preventDefault();
  });
  cv.addEventListener('pointermove', function (e) {
    if (!drawing) return;
    poly.push(pt(e));
    repaint(poly);
    e.preventDefault();
  });
  cv.addEventListener('pointerup', function (e) {
    if (!drawing) return;
    drawing = false;
    if (poly.length < 3) {
      repaint(null);
      return;
    }
    var idx = [];
    for (var i = 0; i < PTS.length; i++) {
      if (inPoly(XS[i], YS[i], poly)) idx.push(i);
    }
    drawBase(new Set(idx));
    repaint(null);
    post(idx);
  });
  cv.addEventListener('dblclick', function () {
    drawBase(new Set());
    repaint(null);
    post([]);
  });
}
