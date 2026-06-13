# Iris Classification with a Decision Tree

## Loading the data

The classic Iris dataset ships as a small Parquet file. Each row is one flower
with four `Double` measurements and a `variety` label.

```haskell
-- cabal: build-depends: dataframe, dataframe-learn, text, random, vector
-- cabal: default-extensions: OverloadedStrings, TypeApplications, ScopedTypeVariables

import qualified DataFrame as D

df <- D.readParquet "./examples/data/iris.parquet"

displayMarkdown $ D.toMarkdown'
                $ D.take 5 df
```

> <!-- scripths:mime text/markdown -->
> | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double | variety<br>Text |
> | -----------------------|-----------------------|------------------------|-----------------------|---------------- |
> | 5.1                    | 3.5                   | 1.4                    | 0.2                   | Setosa          |
> | 4.9                    | 3.0                   | 1.4                    | 0.2                   | Setosa          |
> | 4.7                    | 3.2                   | 1.3                    | 0.2                   | Setosa          |
> | 4.6                    | 3.1                   | 1.5                    | 0.2                   | Setosa          |
> | 5.0                    | 3.6                   | 1.4                    | 0.2                   | Setosa          |

## Looking at the data

Before modelling, it helps to know the shape and types of every column.

```haskell
import DataFrame.Operators

showMarkdown = displayMarkdown . D.toMarkdown'

df |> D.describeColumns
   |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Column Name<br>Text | # Non-null Values<br>Int | # Null Values<br>Int | Type<br>Text |
> | --------------------|--------------------------|----------------------|------------- |
> | variety             | 150                      | 0                    | Text         |
> | petal.width         | 150                      | 0                    | Double       |
> | petal.length        | 150                      | 0                    | Double       |
> | sepal.width         | 150                      | 0                    | Double       |
> | sepal.length        | 150                      | 0                    | Double       |

A summary of the numeric columns shows the ranges each measurement spans.

```haskell
df |> D.summarize
   |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double |
> | ------------------|------------------------|-----------------------|------------------------|---------------------- |
> | Count             | 150.0                  | 150.0                 | 150.0                  | 150.0                 |
> | Mean              | 5.84                   | 3.06                  | 3.76                   | 1.2                   |
> | Minimum           | 4.3                    | 2.0                   | 1.0                    | 0.1                   |
> | 25%               | 5.1                    | 2.8                   | 1.6                    | 0.3                   |
> | Median            | 5.8                    | 3.0                   | 4.35                   | 1.3                   |
> | 75%               | 6.4                    | 3.3                   | 5.1                    | 1.8                   |
> | Max               | 7.9                    | 4.4                   | 6.9                    | 2.5                   |
> | StdDev            | 0.83                   | 0.44                  | 1.77                   | 0.76                  |
> | IQR               | 1.3                    | 0.5                   | 3.5                    | 1.5                   |
> | Skewness          | 0.31                   | 0.31                  | -0.27                  | -0.1                  |

## Is the dataset balanced?

A severely imbalanced dataset would make accuracy a misleading metric. The Iris
dataset is famously balanced — 50 of each species — which we can confirm with
`frequencies`.

```haskell
import qualified Data.Text as T

df |> D.frequencies (D.col @T.Text "variety")
   |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | Setosa<br>Any | Versicolor<br>Any | Virginica<br>Any |
> | ------------------|---------------|-------------------|----------------- |
> | Count             | 50            | 50                | 50               |
> | Percentage (%)    | 33.33%        | 33.33%            | 33.33%           |

## Visualizing the classes

Before building a model, we can plot petal length against petal width to get a
sense of how hard the task is. That is, how separable the three varieties really are.
The dropdown switches between the petal and sepal measurements.

```haskell
import qualified DataFrame.Functions as F
import DataFrame.Monad

pType <- display (dropdown "feature" ["petal", "sepal"] "petal")

l = D.col @Double (T.pack pType <> ".length")
w = D.col @Double (T.pack pType <> ".width")

pts = evalFrameM df $ do
        ps <- deriveM "points" (F.lift2 (,) l w)
        inspectM (D.columnAsList ps)

varieties = D.columnAsList (D.col @T.Text "variety") df

colourBy   = df |> D.derive "colourBy" (
                          F.recodeWithDefault (-1) [("Setosa", 0 :: Double),
                                                    ("Versicolor", 1),
                                                    ("Virginica", 2)] (F.col @T.Text "variety"))
                    |> D.columnAsList (F.col @Double "colourBy")

opts = defScatter
      { soAlpha   = 0.7
      , soRadius  = 5
      , soTitle   = "Iris: " <> pType <> " length vs " <> pType <> " width (colour = variety)"
      , soXLabel  = pType <> ".length"
      , soYLabel  = pType <> ".width"
      , soColorBy = colourBy
      }

sel <- display (scatterSelectWith "iris-petal-lasso" opts pts)
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/html -->
> <select onchange="parent.postMessage({type:'widget',cellId:61,name:'feature',value:this.value},'*')"><option selected>petal</option><option>sepal</option></select>
> <!-- MIME:text/html -->
> <div style='font-family:sans-serif'>
> <canvas id='sc_61_iris-petal-lasso' width='560' height='360' style='border:1px solid #e2e2ea;border-radius:6px;cursor:crosshair;max-width:100%;touch-action:none'></canvas>
> <div style='color:#889;font-size:11px;margin-top:5px'>drag to lasso-select &middot; double-click to clear &middot; 150 points</div>
> <script>
> // scatterSelect widget renderer. Runs inside a cell's sandboxed output
> // iframe; reports the lasso selection to the editor via parent.postMessage,
> // where 22-widget-bridge.js POSTs it to /api/widget. The Haskell side
> // (Sabela.Output.Scatter) embeds this file and emits a sabelaScatter(cfg)
> // bootstrap call carrying the per-render data.
> function sabelaScatter(cfg) {
>   var PTS = cfg.pts;
>   var SEL = cfg.sel;
>   var CVAL = cfg.cval;
>   var NAME = cfg.name;
>   var CID = cfg.cid;
>   var W = cfg.w,
>     H = cfg.h,
>     R = cfg.r,
>     ALPHA = cfg.alpha;
>   var COLOR = cfg.color,
>     SELCOLOR = cfg.selColor;
>   var TITLE = cfg.title,
>     XLAB = cfg.xlab,
>     YLAB = cfg.ylab;
>   var XB = cfg.xb,
>     YB = cfg.yb;
>   var cv = document.getElementById(cfg.elId);
>   if (!cv) return;
>   var ctx = cv.getContext('2d');
>   if (!PTS.length) {
>     return;
>   }
>   var hasC = CVAL.length === PTS.length;
>   var L = YLAB ? 52 : 40,
>     Rm = hasC ? 54 : 14,
>     T = TITLE ? 26 : 12,
>     B = XLAB ? 40 : 26;
>   var minX = Infinity,
>     maxX = -Infinity,
>     minY = Infinity,
>     maxY = -Infinity;
>   for (var i = 0; i < PTS.length; i++) {
>     var p = PTS[i];
>     if (p[0] < minX) minX = p[0];
>     if (p[0] > maxX) maxX = p[0];
>     if (p[1] < minY) minY = p[1];
>     if (p[1] > maxY) maxY = p[1];
>   }
>   if (XB) {
>     minX = XB[0];
>     maxX = XB[1];
>   }
>   if (YB) {
>     minY = YB[0];
>     maxY = YB[1];
>   }
>   if (minX === maxX) {
>     minX -= 1;
>     maxX += 1;
>   }
>   if (minY === maxY) {
>     minY -= 1;
>     maxY += 1;
>   }
>   var cmin = Infinity,
>     cmax = -Infinity;
>   if (hasC) {
>     for (var i = 0; i < CVAL.length; i++) {
>       if (CVAL[i] < cmin) cmin = CVAL[i];
>       if (CVAL[i] > cmax) cmax = CVAL[i];
>     }
>     if (cmin === cmax) {
>       cmin -= 1;
>       cmax += 1;
>     }
>   }
>   function sx(x) {
>     return L + ((x - minX) / (maxX - minX)) * (W - L - Rm);
>   }
>   function sy(y) {
>     return H - B - ((y - minY) / (maxY - minY)) * (H - B - T);
>   }
>   var STOPS = [
>     [68, 1, 84],
>     [59, 82, 139],
>     [33, 145, 140],
>     [94, 201, 98],
>     [253, 231, 37],
>   ];
>   function grad(t) {
>     if (t < 0) t = 0;
>     if (t > 1) t = 1;
>     var s = t * 4,
>       i = Math.floor(s),
>       f = s - i;
>     if (i >= 4) {
>       i = 3;
>       f = 1;
>     }
>     var a = STOPS[i],
>       b = STOPS[i + 1];
>     return (
>       'rgb(' +
>       Math.round(a[0] + (b[0] - a[0]) * f) +
>       ',' +
>       Math.round(a[1] + (b[1] - a[1]) * f) +
>       ',' +
>       Math.round(a[2] + (b[2] - a[2]) * f) +
>       ')'
>     );
>   }
>   function colorOf(i) {
>     return hasC ? grad((CVAL[i] - cmin) / (cmax - cmin)) : COLOR;
>   }
>   var XS = new Float64Array(PTS.length),
>     YS = new Float64Array(PTS.length);
>   for (var i = 0; i < PTS.length; i++) {
>     XS[i] = sx(PTS[i][0]);
>     YS[i] = sy(PTS[i][1]);
>   }
>   var base = document.createElement('canvas');
>   base.width = W;
>   base.height = H;
>   var bctx = base.getContext('2d');
>   function drawBase(sset) {
>     bctx.clearRect(0, 0, W, H);
>     if (TITLE) {
>       bctx.fillStyle = '#222';
>       bctx.font = '600 13px sans-serif';
>       bctx.textAlign = 'center';
>       bctx.fillText(TITLE, W / 2, 16);
>     }
>     bctx.strokeStyle = '#d7d7e0';
>     bctx.lineWidth = 1;
>     bctx.beginPath();
>     bctx.moveTo(L, T - 4);
>     bctx.lineTo(L, H - B);
>     bctx.lineTo(W - Rm + 4, H - B);
>     bctx.stroke();
>     bctx.globalAlpha = ALPHA;
>     for (var i = 0; i < PTS.length; i++) {
>       if (sset && sset.has(i)) continue;
>       bctx.fillStyle = colorOf(i);
>       bctx.fillRect(XS[i] - R, YS[i] - R, 2 * R, 2 * R);
>     }
>     bctx.globalAlpha = 1;
>     if (sset) {
>       bctx.fillStyle = SELCOLOR;
>       sset.forEach(function (k) {
>         var s = R + 1;
>         bctx.fillRect(XS[k] - s, YS[k] - s, 2 * s, 2 * s);
>       });
>     }
>     bctx.fillStyle = '#99a';
>     bctx.font = '10px sans-serif';
>     bctx.textAlign = 'start';
>     bctx.fillText(String(+minX.toFixed(2)), L, H - B + 14);
>     bctx.fillText(String(+maxX.toFixed(2)), W - Rm - 32, H - B + 14);
>     bctx.fillText(String(+maxY.toFixed(2)), 4, T + 6);
>     bctx.fillText(String(+minY.toFixed(2)), 4, H - B);
>     bctx.fillStyle = '#556';
>     bctx.font = '11px sans-serif';
>     bctx.textAlign = 'center';
>     if (XLAB) bctx.fillText(XLAB, L + (W - L - Rm) / 2, H - 6);
>     if (YLAB) {
>       bctx.save();
>       bctx.translate(12, T + (H - B - T) / 2);
>       bctx.rotate(-Math.PI / 2);
>       bctx.fillText(YLAB, 0, 0);
>       bctx.restore();
>     }
>     if (hasC) {
>       var bx = W - Rm + 14,
>         bw = 10,
>         bh = H - B - T;
>       for (var g = 0; g < bh; g++) {
>         bctx.fillStyle = grad(1 - g / bh);
>         bctx.fillRect(bx, T + g, bw, 1);
>       }
>       bctx.fillStyle = '#99a';
>       bctx.font = '9px sans-serif';
>       bctx.textAlign = 'start';
>       bctx.fillText(String(+cmax.toFixed(1)), bx - 3, T - 3);
>       bctx.fillText(String(+cmin.toFixed(1)), bx - 3, T + bh + 10);
>     }
>   }
>   function repaint(poly) {
>     ctx.clearRect(0, 0, W, H);
>     ctx.drawImage(base, 0, 0);
>     if (poly && poly.length > 1) {
>       ctx.strokeStyle = SELCOLOR;
>       ctx.fillStyle = 'rgba(227,17,108,0.08)';
>       ctx.lineWidth = 1.5;
>       ctx.beginPath();
>       ctx.moveTo(poly[0][0], poly[0][1]);
>       for (var i = 1; i < poly.length; i++) ctx.lineTo(poly[i][0], poly[i][1]);
>       ctx.closePath();
>       ctx.fill();
>       ctx.stroke();
>     }
>   }
>   function inPoly(px, py, poly) {
>     var c = false;
>     for (var i = 0, j = poly.length - 1; i < poly.length; j = i++) {
>       var xi = poly[i][0],
>         yi = poly[i][1],
>         xj = poly[j][0],
>         yj = poly[j][1];
>       if (yi > py !== yj > py && px < ((xj - xi) * (py - yi)) / (yj - yi) + xi) c = !c;
>     }
>     return c;
>   }
>   function pt(e) {
>     return [e.offsetX * (cv.width / cv.clientWidth), e.offsetY * (cv.height / cv.clientHeight)];
>   }
>   function post(idx) {
>     parent.postMessage(
>       { type: 'widget', cellId: CID, name: NAME, value: '[' + idx.join(',') + ']' },
>       '*'
>     );
>   }
>   drawBase(new Set(SEL));
>   repaint(null);
>   var drawing = false,
>     poly = [];
>   cv.addEventListener('pointerdown', function (e) {
>     if (e.button !== 0) return;
>     drawing = true;
>     poly = [pt(e)];
>     cv.setPointerCapture(e.pointerId);
>     e.preventDefault();
>   });
>   cv.addEventListener('pointermove', function (e) {
>     if (!drawing) return;
>     poly.push(pt(e));
>     repaint(poly);
>     e.preventDefault();
>   });
>   cv.addEventListener('pointerup', function (e) {
>     if (!drawing) return;
>     drawing = false;
>     if (poly.length < 3) {
>       repaint(null);
>       return;
>     }
>     var idx = [];
>     for (var i = 0; i < PTS.length; i++) {
>       if (inPoly(XS[i], YS[i], poly)) idx.push(i);
>     }
>     drawBase(new Set(idx));
>     repaint(null);
>     post(idx);
>   });
>   cv.addEventListener('dblclick', function () {
>     drawBase(new Set());
>     repaint(null);
>     post([]);
>   });
> }
> 
> sabelaScatter({elId:'sc_61_iris-petal-lasso',name:'iris-petal-lasso',cid:61,pts:[[1.4,0.2],[1.4,0.2],[1.3,0.2],[1.5,0.2],[1.4,0.2],[1.7,0.4],[1.4,0.3],[1.5,0.2],[1.4,0.2],[1.5,0.1],[1.5,0.2],[1.6,0.2],[1.4,0.1],[1.1,0.1],[1.2,0.2],[1.5,0.4],[1.3,0.4],[1.4,0.3],[1.7,0.3],[1.5,0.3],[1.7,0.2],[1.5,0.4],[1.0,0.2],[1.7,0.5],[1.9,0.2],[1.6,0.2],[1.6,0.4],[1.5,0.2],[1.4,0.2],[1.6,0.2],[1.6,0.2],[1.5,0.4],[1.5,0.1],[1.4,0.2],[1.5,0.2],[1.2,0.2],[1.3,0.2],[1.4,0.1],[1.3,0.2],[1.5,0.2],[1.3,0.3],[1.3,0.3],[1.3,0.2],[1.6,0.6],[1.9,0.4],[1.4,0.3],[1.6,0.2],[1.4,0.2],[1.5,0.2],[1.4,0.2],[4.7,1.4],[4.5,1.5],[4.9,1.5],[4.0,1.3],[4.6,1.5],[4.5,1.3],[4.7,1.6],[3.3,1.0],[4.6,1.3],[3.9,1.4],[3.5,1.0],[4.2,1.5],[4.0,1.0],[4.7,1.4],[3.6,1.3],[4.4,1.4],[4.5,1.5],[4.1,1.0],[4.5,1.5],[3.9,1.1],[4.8,1.8],[4.0,1.3],[4.9,1.5],[4.7,1.2],[4.3,1.3],[4.4,1.4],[4.8,1.4],[5.0,1.7],[4.5,1.5],[3.5,1.0],[3.8,1.1],[3.7,1.0],[3.9,1.2],[5.1,1.6],[4.5,1.5],[4.5,1.6],[4.7,1.5],[4.4,1.3],[4.1,1.3],[4.0,1.3],[4.4,1.2],[4.6,1.4],[4.0,1.2],[3.3,1.0],[4.2,1.3],[4.2,1.2],[4.2,1.3],[4.3,1.3],[3.0,1.1],[4.1,1.3],[6.0,2.5],[5.1,1.9],[5.9,2.1],[5.6,1.8],[5.8,2.2],[6.6,2.1],[4.5,1.7],[6.3,1.8],[5.8,1.8],[6.1,2.5],[5.1,2.0],[5.3,1.9],[5.5,2.1],[5.0,2.0],[5.1,2.4],[5.3,2.3],[5.5,1.8],[6.7,2.2],[6.9,2.3],[5.0,1.5],[5.7,2.3],[4.9,2.0],[6.7,2.0],[4.9,1.8],[5.7,2.1],[6.0,1.8],[4.8,1.8],[4.9,1.8],[5.6,2.1],[5.8,1.6],[6.1,1.9],[6.4,2.0],[5.6,2.2],[5.1,1.5],[5.6,1.4],[6.1,2.3],[5.6,2.4],[5.5,1.8],[4.8,1.8],[5.4,2.1],[5.6,2.4],[5.1,2.3],[5.1,1.9],[5.9,2.3],[5.7,2.5],[5.2,2.3],[5.0,1.9],[5.2,2.0],[5.4,2.3],[5.1,1.8],],sel:[],cval:[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,],w:560,h:360,r:5.0,alpha:0.7,color:'#4a9eff',selColor:'#e3116c',title:'Iris: petal length vs petal width (colour = variety)',xlab:'petal.length',ylab:'petal.width',xb:null,yb:null});
> </script>
> </div>

```haskell
import DataFrame.Operations.Subset (selectRows)

let chosen = selectRows sel df
let n      = D.nRows chosen

displayMarkdown $
    "**" ++ show n ++ "** of " ++ show (D.nRows df) ++ " flowers selected"
        ++ (if n == 0 then " — draw a lasso on the plot above to begin." else ":")

showMarkdown chosen
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> **0** of 150 flowers selected — draw a lasso on the plot above to begin.
> <!-- MIME:text/markdown -->
> | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double | variety<br>Text |
> | -----------------------|-----------------------|------------------------|-----------------------|---------------- |

## Feature engineering

Before splitting, we add a few derived features using the **typed** DataFrame
API (`DataFrame.Typed`). We `freeze` the untyped frame into a schema-checked
`TypedDataFrame`, `derive` new columns whose names and element types are verified
at compile time, then `thaw` back so the rest of the pipeline (split, fit,
score) picks them up automatically.

The three additions are classic Iris shape features: **petal area**, **sepal
area**, and the **petal aspect ratio** (length ÷ width).

```haskell
import qualified DataFrame.Typed as DT
import DataFrame.Typed (Column, TypedDataFrame)

type IrisCols =
    '[ Column "sepal.length" Double
     , Column "sepal.width"  Double
     , Column "petal.length" Double
     , Column "petal.width"  Double
     , Column "variety"      T.Text
     ]

let tdf = either (error . T.unpack) id
            (DT.freezeWithError df :: Either T.Text (TypedDataFrame IrisCols))

let featured =
        DT.thaw $ tdf
            |> DT.derive @"petal.area"  (DT.col @"petal.length" * DT.col @"petal.width")
            |> DT.derive @"sepal.area"  (DT.col @"sepal.length" * DT.col @"sepal.width")
            |> DT.derive @"petal.ratio" (DT.col @"petal.length" / DT.col @"petal.width")

displayMarkdown $ D.toMarkdown' $ D.take 5 featured
```

> <!-- scripths:mime text/markdown -->
> | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double | variety<br>Text | petal.area<br>Double | sepal.area<br>Double | petal.ratio<br>Double |
> | -----------------------|-----------------------|------------------------|-----------------------|-----------------|----------------------|----------------------|---------------------- |
> | 5.1                    | 3.5                   | 1.4                    | 0.2                   | Setosa          | 0.27999999999999997  | 17.849999999999998   | 6.999999999999999     |
> | 4.9                    | 3.0                   | 1.4                    | 0.2                   | Setosa          | 0.27999999999999997  | 14.700000000000001   | 6.999999999999999     |
> | 4.7                    | 3.2                   | 1.3                    | 0.2                   | Setosa          | 0.26                 | 15.040000000000001   | 6.5                   |
> | 4.6                    | 3.1                   | 1.5                    | 0.2                   | Setosa          | 0.30000000000000004  | 14.26                | 7.5                   |
> | 5.0                    | 3.6                   | 1.4                    | 0.2                   | Setosa          | 0.27999999999999997  | 18.0                 | 6.999999999999999     |

With the derived columns in hand, we can plot them with the **typed** charting
API (`DataFrame.Display.Web.Chart.Typed`). We re-`freeze` `featured` into a
schema that includes the new columns, then reference them by name with
`DT.col @"…"` — so a typo or a wrong element type is a compile error, not a
blank chart. The scatter of **petal area vs sepal area** shows the three
varieties pulling apart in engineered space, and the boxplot of the **petal
aspect ratio** shows Setosa standing well clear of the other two.

```haskell
import qualified DataFrame.Display.Web.Chart.Typed as CT

let charted = featured
        |> D.rename "petal.area"  "petalArea"
        |> D.rename "sepal.area"  "sepalArea"
        |> D.rename "petal.ratio" "petalRatio"

type FeaturedCols =
    '[ Column "sepal.length" Double
     , Column "sepal.width"  Double
     , Column "petal.length" Double
     , Column "petal.width"  Double
     , Column "variety"      T.Text
     , Column "petalArea"    Double
     , Column "sepalArea"    Double
     , Column "petalRatio"   Double
     ]

let ftdf = either (error . T.unpack) id
            (DT.freezeWithError charted :: Either T.Text (TypedDataFrame FeaturedCols))

displayHtml $ CT.toHtml $
    CT.chart ftdf
        |> CT.mark CT.Point
        |> CT.enc CT.X     (DT.col @"petalArea")
        |> CT.enc CT.Y     (DT.col @"sepalArea")
        |> CT.enc CT.Color (DT.col @"variety")
        |> CT.title "Engineered features — petal area vs sepal area (colour = variety)"
```

> <!-- scripths:mime text/html -->
> <div id="vis"></div>
> <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
> <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
> <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
> <script>vegaEmbed('#vis', {"$schema":"https://vega.github.io/schema/vega-lite/v5.json","data":{"values":[{"petalArea":0.27999999999999997,"sepalArea":17.849999999999998,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":14.700000000000001,"variety":"Setosa"},{"petalArea":0.26,"sepalArea":15.040000000000001,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":14.26,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":18,"variety":"Setosa"},{"petalArea":0.68,"sepalArea":21.060000000000002,"variety":"Setosa"},{"petalArea":0.42,"sepalArea":15.639999999999999,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":17,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":12.76,"variety":"Setosa"},{"petalArea":0.15000000000000002,"sepalArea":15.190000000000001,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":19.980000000000004,"variety":"Setosa"},{"petalArea":0.32000000000000006,"sepalArea":16.32,"variety":"Setosa"},{"petalArea":0.13999999999999999,"sepalArea":14.399999999999999,"variety":"Setosa"},{"petalArea":0.11000000000000001,"sepalArea":12.899999999999999,"variety":"Setosa"},{"petalArea":0.24,"sepalArea":23.2,"variety":"Setosa"},{"petalArea":0.6000000000000001,"sepalArea":25.080000000000002,"variety":"Setosa"},{"petalArea":0.52,"sepalArea":21.060000000000002,"variety":"Setosa"},{"petalArea":0.42,"sepalArea":17.849999999999998,"variety":"Setosa"},{"petalArea":0.51,"sepalArea":21.66,"variety":"Setosa"},{"petalArea":0.44999999999999996,"sepalArea":19.38,"variety":"Setosa"},{"petalArea":0.34,"sepalArea":18.36,"variety":"Setosa"},{"petalArea":0.6000000000000001,"sepalArea":18.87,"variety":"Setosa"},{"petalArea":0.2,"sepalArea":16.56,"variety":"Setosa"},{"petalArea":0.85,"sepalArea":16.83,"variety":"Setosa"},{"petalArea":0.38,"sepalArea":16.32,"variety":"Setosa"},{"petalArea":0.32000000000000006,"sepalArea":15,"variety":"Setosa"},{"petalArea":0.6400000000000001,"sepalArea":17,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":18.2,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":17.68,"variety":"Setosa"},{"petalArea":0.32000000000000006,"sepalArea":15.040000000000001,"variety":"Setosa"},{"petalArea":0.32000000000000006,"sepalArea":14.879999999999999,"variety":"Setosa"},{"petalArea":0.6000000000000001,"sepalArea":18.36,"variety":"Setosa"},{"petalArea":0.15000000000000002,"sepalArea":21.32,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":23.1,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":15.190000000000001,"variety":"Setosa"},{"petalArea":0.24,"sepalArea":16,"variety":"Setosa"},{"petalArea":0.26,"sepalArea":19.25,"variety":"Setosa"},{"petalArea":0.13999999999999999,"sepalArea":17.64,"variety":"Setosa"},{"petalArea":0.26,"sepalArea":13.200000000000001,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":17.34,"variety":"Setosa"},{"petalArea":0.39,"sepalArea":17.5,"variety":"Setosa"},{"petalArea":0.39,"sepalArea":10.35,"variety":"Setosa"},{"petalArea":0.26,"sepalArea":14.080000000000002,"variety":"Setosa"},{"petalArea":0.96,"sepalArea":17.5,"variety":"Setosa"},{"petalArea":0.76,"sepalArea":19.38,"variety":"Setosa"},{"petalArea":0.42,"sepalArea":14.399999999999999,"variety":"Setosa"},{"petalArea":0.32000000000000006,"sepalArea":19.38,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":14.719999999999999,"variety":"Setosa"},{"petalArea":0.30000000000000004,"sepalArea":19.61,"variety":"Setosa"},{"petalArea":0.27999999999999997,"sepalArea":16.5,"variety":"Setosa"},{"petalArea":6.58,"sepalArea":22.400000000000002,"variety":"Versicolor"},{"petalArea":6.75,"sepalArea":20.480000000000004,"variety":"Versicolor"},{"petalArea":7.3500000000000005,"sepalArea":21.39,"variety":"Versicolor"},{"petalArea":5.2,"sepalArea":12.649999999999999,"variety":"Versicolor"},{"petalArea":6.8999999999999995,"sepalArea":18.2,"variety":"Versicolor"},{"petalArea":5.8500000000000005,"sepalArea":15.959999999999999,"variety":"Versicolor"},{"petalArea":7.5200000000000005,"sepalArea":20.79,"variety":"Versicolor"},{"petalArea":3.3,"sepalArea":11.76,"variety":"Versicolor"},{"petalArea":5.9799999999999995,"sepalArea":19.139999999999997,"variety":"Versicolor"},{"petalArea":5.46,"sepalArea":14.040000000000001,"variety":"Versicolor"},{"petalArea":3.5,"sepalArea":10,"variety":"Versicolor"},{"petalArea":6.300000000000001,"sepalArea":17.700000000000003,"variety":"Versicolor"},{"petalArea":4,"sepalArea":13.200000000000001,"variety":"Versicolor"},{"petalArea":6.58,"sepalArea":17.689999999999998,"variety":"Versicolor"},{"petalArea":4.680000000000001,"sepalArea":16.24,"variety":"Versicolor"},{"petalArea":6.16,"sepalArea":20.77,"variety":"Versicolor"},{"petalArea":6.75,"sepalArea":16.799999999999997,"variety":"Versicolor"},{"petalArea":4.1,"sepalArea":15.66,"variety":"Versicolor"},{"petalArea":6.75,"sepalArea":13.640000000000002,"variety":"Versicolor"},{"petalArea":4.29,"sepalArea":14,"variety":"Versicolor"},{"petalArea":8.64,"sepalArea":18.880000000000003,"variety":"Versicolor"},{"petalArea":5.2,"sepalArea":17.08,"variety":"Versicolor"},{"petalArea":7.3500000000000005,"sepalArea":15.75,"variety":"Versicolor"},{"petalArea":5.64,"sepalArea":17.08,"variety":"Versicolor"},{"petalArea":5.59,"sepalArea":18.56,"variety":"Versicolor"},{"petalArea":6.16,"sepalArea":19.799999999999997,"variety":"Versicolor"},{"petalArea":6.72,"sepalArea":19.04,"variety":"Versicolor"},{"petalArea":8.5,"sepalArea":20.1,"variety":"Versicolor"},{"petalArea":6.75,"sepalArea":17.4,"variety":"Versicolor"},{"petalArea":3.5,"sepalArea":14.82,"variety":"Versicolor"},{"petalArea":4.18,"sepalArea":13.2,"variety":"Versicolor"},{"petalArea":3.7,"sepalArea":13.2,"variety":"Versicolor"},{"petalArea":4.68,"sepalArea":15.66,"variety":"Versicolor"},{"petalArea":8.16,"sepalArea":16.200000000000003,"variety":"Versicolor"},{"petalArea":6.75,"sepalArea":16.200000000000003,"variety":"Versicolor"},{"petalArea":7.2,"sepalArea":20.4,"variety":"Versicolor"},{"petalArea":7.050000000000001,"sepalArea":20.77,"variety":"Versicolor"},{"petalArea":5.720000000000001,"sepalArea":14.489999999999998,"variety":"Versicolor"},{"petalArea":5.33,"sepalArea":16.799999999999997,"variety":"Versicolor"},{"petalArea":5.2,"sepalArea":13.75,"variety":"Versicolor"},{"petalArea":5.28,"sepalArea":14.3,"variety":"Versicolor"},{"petalArea":6.4399999999999995,"sepalArea":18.299999999999997,"variety":"Versicolor"},{"petalArea":4.8,"sepalArea":15.08,"variety":"Versicolor"},{"petalArea":3.3,"sepalArea":11.5,"variety":"Versicolor"},{"petalArea":5.460000000000001,"sepalArea":15.12,"variety":"Versicolor"},{"petalArea":5.04,"sepalArea":17.1,"variety":"Versicolor"},{"petalArea":5.460000000000001,"sepalArea":16.53,"variety":"Versicolor"},{"petalArea":5.59,"sepalArea":17.98,"variety":"Versicolor"},{"petalArea":3.3000000000000003,"sepalArea":12.75,"variety":"Versicolor"},{"petalArea":5.33,"sepalArea":15.959999999999999,"variety":"Versicolor"},{"petalArea":15,"sepalArea":20.79,"variety":"Virginica"},{"petalArea":9.69,"sepalArea":15.66,"variety":"Virginica"},{"petalArea":12.39,"sepalArea":21.299999999999997,"variety":"Virginica"},{"petalArea":10.08,"sepalArea":18.27,"variety":"Virginica"},{"petalArea":12.76,"sepalArea":19.5,"variety":"Virginica"},{"petalArea":13.86,"sepalArea":22.799999999999997,"variety":"Virginica"},{"petalArea":7.6499999999999995,"sepalArea":12.25,"variety":"Virginica"},{"petalArea":11.34,"sepalArea":21.169999999999998,"variety":"Virginica"},{"petalArea":10.44,"sepalArea":16.75,"variety":"Virginica"},{"petalArea":15.25,"sepalArea":25.92,"variety":"Virginica"},{"petalArea":10.2,"sepalArea":20.8,"variety":"Virginica"},{"petalArea":10.069999999999999,"sepalArea":17.28,"variety":"Virginica"},{"petalArea":11.55,"sepalArea":20.4,"variety":"Virginica"},{"petalArea":10,"sepalArea":14.25,"variety":"Virginica"},{"petalArea":12.239999999999998,"sepalArea":16.24,"variety":"Virginica"},{"petalArea":12.19,"sepalArea":20.480000000000004,"variety":"Virginica"},{"petalArea":9.9,"sepalArea":19.5,"variety":"Virginica"},{"petalArea":14.740000000000002,"sepalArea":29.259999999999998,"variety":"Virginica"},{"petalArea":15.87,"sepalArea":20.02,"variety":"Virginica"},{"petalArea":7.5,"sepalArea":13.200000000000001,"variety":"Virginica"},{"petalArea":13.11,"sepalArea":22.080000000000002,"variety":"Virginica"},{"petalArea":9.8,"sepalArea":15.679999999999998,"variety":"Virginica"},{"petalArea":13.4,"sepalArea":21.56,"variety":"Virginica"},{"petalArea":8.82,"sepalArea":17.01,"variety":"Virginica"},{"petalArea":11.97,"sepalArea":22.11,"variety":"Virginica"},{"petalArea":10.8,"sepalArea":23.040000000000003,"variety":"Virginica"},{"petalArea":8.64,"sepalArea":17.36,"variety":"Virginica"},{"petalArea":8.82,"sepalArea":18.299999999999997,"variety":"Virginica"},{"petalArea":11.76,"sepalArea":17.919999999999998,"variety":"Virginica"},{"petalArea":9.28,"sepalArea":21.6,"variety":"Virginica"},{"petalArea":11.589999999999998,"sepalArea":20.72,"variety":"Virginica"},{"petalArea":12.8,"sepalArea":30.02,"variety":"Virginica"},{"petalArea":12.32,"sepalArea":17.919999999999998,"variety":"Virginica"},{"petalArea":7.6499999999999995,"sepalArea":17.639999999999997,"variety":"Virginica"},{"petalArea":7.839999999999999,"sepalArea":15.86,"variety":"Virginica"},{"petalArea":14.029999999999998,"sepalArea":23.1,"variety":"Virginica"},{"petalArea":13.44,"sepalArea":21.419999999999998,"variety":"Virginica"},{"petalArea":9.9,"sepalArea":19.840000000000003,"variety":"Virginica"},{"petalArea":8.64,"sepalArea":18,"variety":"Virginica"},{"petalArea":11.340000000000002,"sepalArea":21.39,"variety":"Virginica"},{"petalArea":13.44,"sepalArea":20.77,"variety":"Virginica"},{"petalArea":11.729999999999999,"sepalArea":21.39,"variety":"Virginica"},{"petalArea":9.69,"sepalArea":15.66,"variety":"Virginica"},{"petalArea":13.57,"sepalArea":21.76,"variety":"Virginica"},{"petalArea":14.25,"sepalArea":22.11,"variety":"Virginica"},{"petalArea":11.959999999999999,"sepalArea":20.1,"variety":"Virginica"},{"petalArea":9.5,"sepalArea":15.75,"variety":"Virginica"},{"petalArea":10.4,"sepalArea":19.5,"variety":"Virginica"},{"petalArea":12.42,"sepalArea":21.08,"variety":"Virginica"},{"petalArea":9.18,"sepalArea":17.700000000000003,"variety":"Virginica"}]},"encoding":{"color":{"field":"variety","type":"nominal"},"x":{"field":"petalArea","type":"quantitative"},"y":{"field":"sepalArea","type":"quantitative"}},"height":400,"mark":{"tooltip":true,"type":"point"},"title":"Engineered features — petal area vs sepal area (colour = variety)","width":600});</script>

```haskell
displayHtml $ CT.toHtml $
    CT.chart ftdf
        |> CT.mark CT.Boxplot
        |> CT.enc CT.X     (DT.col @"variety")
        |> CT.enc CT.Y     (DT.col @"petalRatio")
        |> CT.enc CT.Color (DT.col @"variety")
        |> CT.title "Engineered feature — petal aspect ratio (length / width) by variety"
```

> <!-- scripths:mime text/html -->
> <div id="vis"></div>
> <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
> <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
> <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
> <script>vegaEmbed('#vis', {"$schema":"https://vega.github.io/schema/vega-lite/v5.json","data":{"values":[{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":6.5,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":4.25,"variety":"Setosa"},{"petalRatio":4.666666666666667,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":15,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":8,"variety":"Setosa"},{"petalRatio":13.999999999999998,"variety":"Setosa"},{"petalRatio":11,"variety":"Setosa"},{"petalRatio":5.999999999999999,"variety":"Setosa"},{"petalRatio":3.75,"variety":"Setosa"},{"petalRatio":3.25,"variety":"Setosa"},{"petalRatio":4.666666666666667,"variety":"Setosa"},{"petalRatio":5.666666666666667,"variety":"Setosa"},{"petalRatio":5,"variety":"Setosa"},{"petalRatio":8.5,"variety":"Setosa"},{"petalRatio":3.75,"variety":"Setosa"},{"petalRatio":5,"variety":"Setosa"},{"petalRatio":3.4,"variety":"Setosa"},{"petalRatio":9.499999999999998,"variety":"Setosa"},{"petalRatio":8,"variety":"Setosa"},{"petalRatio":4,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":8,"variety":"Setosa"},{"petalRatio":8,"variety":"Setosa"},{"petalRatio":3.75,"variety":"Setosa"},{"petalRatio":15,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":5.999999999999999,"variety":"Setosa"},{"petalRatio":6.5,"variety":"Setosa"},{"petalRatio":13.999999999999998,"variety":"Setosa"},{"petalRatio":6.5,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":4.333333333333334,"variety":"Setosa"},{"petalRatio":4.333333333333334,"variety":"Setosa"},{"petalRatio":6.5,"variety":"Setosa"},{"petalRatio":2.666666666666667,"variety":"Setosa"},{"petalRatio":4.749999999999999,"variety":"Setosa"},{"petalRatio":4.666666666666667,"variety":"Setosa"},{"petalRatio":8,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":7.5,"variety":"Setosa"},{"petalRatio":6.999999999999999,"variety":"Setosa"},{"petalRatio":3.3571428571428577,"variety":"Versicolor"},{"petalRatio":3,"variety":"Versicolor"},{"petalRatio":3.266666666666667,"variety":"Versicolor"},{"petalRatio":3.0769230769230766,"variety":"Versicolor"},{"petalRatio":3.0666666666666664,"variety":"Versicolor"},{"petalRatio":3.4615384615384612,"variety":"Versicolor"},{"petalRatio":2.9375,"variety":"Versicolor"},{"petalRatio":3.3,"variety":"Versicolor"},{"petalRatio":3.538461538461538,"variety":"Versicolor"},{"petalRatio":2.785714285714286,"variety":"Versicolor"},{"petalRatio":3.5,"variety":"Versicolor"},{"petalRatio":2.8000000000000003,"variety":"Versicolor"},{"petalRatio":4,"variety":"Versicolor"},{"petalRatio":3.3571428571428577,"variety":"Versicolor"},{"petalRatio":2.769230769230769,"variety":"Versicolor"},{"petalRatio":3.1428571428571432,"variety":"Versicolor"},{"petalRatio":3,"variety":"Versicolor"},{"petalRatio":4.1,"variety":"Versicolor"},{"petalRatio":3,"variety":"Versicolor"},{"petalRatio":3.545454545454545,"variety":"Versicolor"},{"petalRatio":2.6666666666666665,"variety":"Versicolor"},{"petalRatio":3.0769230769230766,"variety":"Versicolor"},{"petalRatio":3.266666666666667,"variety":"Versicolor"},{"petalRatio":3.916666666666667,"variety":"Versicolor"},{"petalRatio":3.3076923076923075,"variety":"Versicolor"},{"petalRatio":3.1428571428571432,"variety":"Versicolor"},{"petalRatio":3.428571428571429,"variety":"Versicolor"},{"petalRatio":2.9411764705882355,"variety":"Versicolor"},{"petalRatio":3,"variety":"Versicolor"},{"petalRatio":3.5,"variety":"Versicolor"},{"petalRatio":3.454545454545454,"variety":"Versicolor"},{"petalRatio":3.7,"variety":"Versicolor"},{"petalRatio":3.25,"variety":"Versicolor"},{"petalRatio":3.1874999999999996,"variety":"Versicolor"},{"petalRatio":3,"variety":"Versicolor"},{"petalRatio":2.8125,"variety":"Versicolor"},{"petalRatio":3.1333333333333333,"variety":"Versicolor"},{"petalRatio":3.3846153846153846,"variety":"Versicolor"},{"petalRatio":3.1538461538461533,"variety":"Versicolor"},{"petalRatio":3.0769230769230766,"variety":"Versicolor"},{"petalRatio":3.666666666666667,"variety":"Versicolor"},{"petalRatio":3.2857142857142856,"variety":"Versicolor"},{"petalRatio":3.3333333333333335,"variety":"Versicolor"},{"petalRatio":3.3,"variety":"Versicolor"},{"petalRatio":3.230769230769231,"variety":"Versicolor"},{"petalRatio":3.5000000000000004,"variety":"Versicolor"},{"petalRatio":3.230769230769231,"variety":"Versicolor"},{"petalRatio":3.3076923076923075,"variety":"Versicolor"},{"petalRatio":2.727272727272727,"variety":"Versicolor"},{"petalRatio":3.1538461538461533,"variety":"Versicolor"},{"petalRatio":2.4,"variety":"Virginica"},{"petalRatio":2.6842105263157894,"variety":"Virginica"},{"petalRatio":2.8095238095238098,"variety":"Virginica"},{"petalRatio":3.1111111111111107,"variety":"Virginica"},{"petalRatio":2.6363636363636362,"variety":"Virginica"},{"petalRatio":3.1428571428571423,"variety":"Virginica"},{"petalRatio":2.6470588235294117,"variety":"Virginica"},{"petalRatio":3.5,"variety":"Virginica"},{"petalRatio":3.222222222222222,"variety":"Virginica"},{"petalRatio":2.44,"variety":"Virginica"},{"petalRatio":2.55,"variety":"Virginica"},{"petalRatio":2.7894736842105265,"variety":"Virginica"},{"petalRatio":2.619047619047619,"variety":"Virginica"},{"petalRatio":2.5,"variety":"Virginica"},{"petalRatio":2.125,"variety":"Virginica"},{"petalRatio":2.3043478260869565,"variety":"Virginica"},{"petalRatio":3.0555555555555554,"variety":"Virginica"},{"petalRatio":3.0454545454545454,"variety":"Virginica"},{"petalRatio":3.0000000000000004,"variety":"Virginica"},{"petalRatio":3.3333333333333335,"variety":"Virginica"},{"petalRatio":2.4782608695652177,"variety":"Virginica"},{"petalRatio":2.45,"variety":"Virginica"},{"petalRatio":3.35,"variety":"Virginica"},{"petalRatio":2.7222222222222223,"variety":"Virginica"},{"petalRatio":2.7142857142857144,"variety":"Virginica"},{"petalRatio":3.333333333333333,"variety":"Virginica"},{"petalRatio":2.6666666666666665,"variety":"Virginica"},{"petalRatio":2.7222222222222223,"variety":"Virginica"},{"petalRatio":2.6666666666666665,"variety":"Virginica"},{"petalRatio":3.6249999999999996,"variety":"Virginica"},{"petalRatio":3.2105263157894735,"variety":"Virginica"},{"petalRatio":3.2,"variety":"Virginica"},{"petalRatio":2.545454545454545,"variety":"Virginica"},{"petalRatio":3.4,"variety":"Virginica"},{"petalRatio":4,"variety":"Virginica"},{"petalRatio":2.6521739130434785,"variety":"Virginica"},{"petalRatio":2.3333333333333335,"variety":"Virginica"},{"petalRatio":3.0555555555555554,"variety":"Virginica"},{"petalRatio":2.6666666666666665,"variety":"Virginica"},{"petalRatio":2.5714285714285716,"variety":"Virginica"},{"petalRatio":2.3333333333333335,"variety":"Virginica"},{"petalRatio":2.217391304347826,"variety":"Virginica"},{"petalRatio":2.6842105263157894,"variety":"Virginica"},{"petalRatio":2.565217391304348,"variety":"Virginica"},{"petalRatio":2.2800000000000002,"variety":"Virginica"},{"petalRatio":2.2608695652173916,"variety":"Virginica"},{"petalRatio":2.6315789473684212,"variety":"Virginica"},{"petalRatio":2.6,"variety":"Virginica"},{"petalRatio":2.347826086956522,"variety":"Virginica"},{"petalRatio":2.833333333333333,"variety":"Virginica"}]},"encoding":{"color":{"field":"variety","type":"nominal"},"x":{"field":"variety","type":"nominal"},"y":{"field":"petalRatio","type":"quantitative"}},"height":400,"mark":{"tooltip":true,"type":"boxplot"},"title":"Engineered feature — petal aspect ratio (length / width) by variety","width":600});</script>

## Splitting into training and test sets

`randomSplit` is the equivalent of scikit-learn's `train_test_split`. We hold
out 30% of the data for testing and fix the random seed (42) so the split is
reproducible.

```haskell
import System.Random (mkStdGen)
import Data.Maybe
import Text.Read

n' <- display (textInput "seed" "42")

let (trainDf, testDf) = D.randomSplit (mkStdGen (fromMaybe 42 (readMaybe n'))) 0.7 df

(D.dimensions trainDf, D.dimensions testDf)
```

> <!-- scripths:mime text/html -->
> <input type='text' value='42' oninput="parent.postMessage({type:'widget',cellId:67,name:'seed',value:this.value,sel:this.selectionStart},'*')">
> ((104,5),(46,5))

## Fitting the decision tree

This is the whole "training" step. `fitDecisionTree` takes a configuration, the
target column (`variety`, a `Text` label), and the training frame. It returns an
`Expr Text`: a self-contained expression that predicts the species from the
other columns. Everything else in the frame is treated as a candidate feature.

```haskell
import qualified DataFrame.DecisionTree as DT

let model = DT.fitDecisionTree DT.defaultTreeConfig (D.col @T.Text "variety") trainDf
```

The fitted model is just data — a tree of `if`/`then`/`else` rules over the
measurement columns. Unlike the neural network's weight matrices, you can read
it directly and see exactly how the tree decides.

```haskell
putStrLn $ D.prettyPrint model
```

> <!-- scripths:mime text/plain -->
> if petal.length .<=. 2.5999999999999996
>   then "Setosa"
>   else if petal.width .<=. 1.65
>     then if petal.length .<=. 5.0
>       then "Versicolor"
>       else "Virginica"
>     else "Virginica"

## Making predictions

Because the model is an expression, we apply it to the test frame the same way
we'd add any derived column: with `D.derive`. Here we add a `predicted` column
next to the true `variety`.

```haskell
let scored = testDf |> D.derive "predicted" model

scored |> D.select ["variety", "predicted"]
       |> D.take 10
       |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | variety<br>Text | predicted<br>Text |
> | ----------------|------------------ |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |

## Measuring accuracy

We mark each test row as correct (1.0) or wrong (0.0) by comparing the true and
predicted labels with `F.eq`, then take the mean — that mean is the accuracy.

```haskell
import qualified DataFrame.Functions as F

let withCorrect =
        scored
            |> D.derive
                "is_correct"
                ( F.ifThenElse
                    (F.eq (F.col @T.Text "variety") (F.col @T.Text "predicted"))
                    (F.lit (1.0 :: Double))
                    (F.lit (0.0 :: Double))
                )

putStrLn ("Test accuracy: " ++ show (D.mean (F.col @Double "is_correct") withCorrect))
```

> <!-- scripths:mime text/plain -->
> Test accuracy: 0.9130434782608695

## Confusion matrix

The original example built a confusion matrix by hand. With a `DataFrame` we get
it by grouping on the (actual, predicted) pair and counting. The diagonal
(`variety == predicted`) holds the correct predictions; off-diagonal entries are
the mistakes.

```haskell
scored |> D.groupBy ["variety", "predicted"]
       |> D.aggregate ["count" .= F.count (F.col @T.Text "variety")]
       |> D.sortBy [D.Asc (D.col @T.Text "variety"), D.Asc (D.col @T.Text "predicted")]
       |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | variety<br>Text | predicted<br>Text | count<br>Int |
> | ----------------|-------------------|------------- |
> | Setosa          | Setosa            | 14           |
> | Versicolor      | Versicolor        | 15           |
> | Versicolor      | Virginica         | 3            |
> | Virginica       | Versicolor        | 1            |
> | Virginica       | Virginica         | 13           |

## Conclusion

Decision trees ,on a clean, well-separated
dataset like Iris, are pretty easy and efficient to run.
