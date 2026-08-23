// Painting for the scatterSelect widget. Every function here takes a 2d
// context plus the plot record scatter.js builds (box, view, projected pixel
// arrays, colours, labels) and draws; none of them touch the DOM or the
// selection. Sabela.Output.Scatter embeds this file between scatter-view.js
// and scatter.js.

var SABELA_SCATTER_STOPS = [
  [68, 1, 84],
  [59, 82, 139],
  [33, 145, 140],
  [94, 201, 98],
  [253, 231, 37],
];

// The viridis ramp at t in 0..1, as a css colour.
function sabelaScatterRamp(t) {
  if (t < 0) t = 0;
  if (t > 1) t = 1;
  var s = t * 4,
    i = Math.floor(s),
    f = s - i;
  if (i >= 4) {
    i = 3;
    f = 1;
  }
  var a = SABELA_SCATTER_STOPS[i],
    b = SABELA_SCATTER_STOPS[i + 1];
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

// Gridlines, axis rules and tick labels for whatever the view currently frames.
function sabelaScatterPaintAxes(c, p) {
  var box = p.box;
  var b = sabelaScatterBounds(p.view, box);
  var xt = sabelaScatterTicks(b.minX, b.maxX, 6);
  var yt = sabelaScatterTicks(b.minY, b.maxY, 5);
  var xstep = xt.length > 1 ? xt[1] - xt[0] : 1;
  var ystep = yt.length > 1 ? yt[1] - yt[0] : 1;
  var i;
  c.strokeStyle = '#ececf2';
  c.lineWidth = 1;
  c.beginPath();
  for (i = 0; i < xt.length; i++) {
    var gx = Math.round(sabelaScatterToX(p.view, box, xt[i])) + 0.5;
    c.moveTo(gx, box.t);
    c.lineTo(gx, box.t + box.h);
  }
  for (i = 0; i < yt.length; i++) {
    var gy = Math.round(sabelaScatterToY(p.view, box, yt[i])) + 0.5;
    c.moveTo(box.l, gy);
    c.lineTo(box.l + box.w, gy);
  }
  c.stroke();
  c.strokeStyle = '#c9c9d4';
  c.beginPath();
  c.moveTo(box.l + 0.5, box.t);
  c.lineTo(box.l + 0.5, box.t + box.h + 0.5);
  c.lineTo(box.l + box.w, box.t + box.h + 0.5);
  c.stroke();
  c.fillStyle = '#8b8b9a';
  c.font = '10px sans-serif';
  c.textAlign = 'center';
  for (i = 0; i < xt.length; i++) {
    var lx = sabelaScatterToX(p.view, box, xt[i]);
    c.fillText(sabelaScatterTickLabel(xt[i], xstep), lx, box.t + box.h + 14);
  }
  c.textAlign = 'end';
  for (i = 0; i < yt.length; i++) {
    var ly = sabelaScatterToY(p.view, box, yt[i]);
    c.fillText(sabelaScatterTickLabel(yt[i], ystep), box.l - 6, ly + 3);
  }
}

// Title, axis names and the colour bar, all outside the clipped plot area.
function sabelaScatterPaintLabels(c, p) {
  var box = p.box;
  if (p.title) {
    c.fillStyle = '#222';
    c.font = '600 13px sans-serif';
    c.textAlign = 'center';
    c.fillText(p.title, p.w / 2, 16);
  }
  c.fillStyle = '#556';
  c.font = '11px sans-serif';
  c.textAlign = 'center';
  if (p.xlab) c.fillText(p.xlab, box.l + box.w / 2, p.h - 6);
  if (p.ylab) {
    c.save();
    c.translate(13, box.t + box.h / 2);
    c.rotate(-Math.PI / 2);
    c.fillText(p.ylab, 0, 0);
    c.restore();
  }
  if (!p.hasC) return;
  var bx = box.l + box.w + 18;
  for (var g = 0; g < box.h; g++) {
    c.fillStyle = sabelaScatterRamp(1 - g / box.h);
    c.fillRect(bx, box.t + g, 10, 1);
  }
  c.fillStyle = '#8b8b9a';
  c.font = '9px sans-serif';
  c.textAlign = 'start';
  c.fillText(String(+p.cmax.toFixed(1)), bx - 3, box.t - 3);
  c.fillText(String(+p.cmin.toFixed(1)), bx - 3, box.t + box.h + 10);
}

// The points themselves: unselected at the caller's alpha, selected on top and
// a shade larger, everything clipped to the plot box so zooming never spills
// over the axes.
function sabelaScatterPaintPoints(c, p) {
  var box = p.box;
  function dot(i, radius) {
    c.beginPath();
    c.arc(p.xs[i], p.ys[i], radius, 0, 6.283185307179586);
    c.fill();
  }
  c.save();
  c.beginPath();
  c.rect(box.l, box.t, box.w, box.h);
  c.clip();
  c.globalAlpha = p.alpha;
  for (var i = 0; i < p.n; i++) {
    if (p.selected.has(i)) continue;
    c.fillStyle = p.colorOf(i);
    dot(i, p.r);
  }
  c.globalAlpha = 1;
  c.fillStyle = p.selColor;
  p.selected.forEach(function (k) {
    dot(k, p.r + 1.2);
  });
  c.restore();
}
