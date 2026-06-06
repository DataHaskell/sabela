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

pType <- display (dropdown "feature" ["petal", "sepal"] "petal")

let pts       = zip (D.columnAsList (D.col @Double (T.pack pType <> ".length")) df)
                    (D.columnAsList (D.col @Double (T.pack pType <> ".width"))  df)

let varieties = D.columnAsList (D.col @T.Text "variety") df

let colourBy   = df |> D.derive "colourBy" (
                          F.recodeWithDefault (-1) [("Setosa", 0 :: Double),
                                                    ("Versicolor", 1),
                                                    ("Virginica", 2)] (F.col @T.Text "variety"))
                    |> D.columnAsList (F.col @Double "colourBy")

let opts = defScatter
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
> <select onchange="parent.postMessage({type:'widget',cellId:234,name:'feature',value:this.value},'*')"><option selected>petal</option><option>sepal</option></select>
> <!-- MIME:text/html -->
> <div style='font-family:sans-serif'>
> <canvas id='sc_234_iris-petal-lasso' width='560' height='360' style='border:1px solid #e2e2ea;border-radius:6px;cursor:crosshair;max-width:100%;touch-action:none'></canvas>
> <div style='color:#889;font-size:11px;margin-top:5px'>drag to lasso-select &middot; double-click to clear &middot; 150 points</div>
> <script>
> (function(){
> var PTS=[[1.4,0.2],[1.4,0.2],[1.3,0.2],[1.5,0.2],[1.4,0.2],[1.7,0.4],[1.4,0.3],[1.5,0.2],[1.4,0.2],[1.5,0.1],[1.5,0.2],[1.6,0.2],[1.4,0.1],[1.1,0.1],[1.2,0.2],[1.5,0.4],[1.3,0.4],[1.4,0.3],[1.7,0.3],[1.5,0.3],[1.7,0.2],[1.5,0.4],[1.0,0.2],[1.7,0.5],[1.9,0.2],[1.6,0.2],[1.6,0.4],[1.5,0.2],[1.4,0.2],[1.6,0.2],[1.6,0.2],[1.5,0.4],[1.5,0.1],[1.4,0.2],[1.5,0.2],[1.2,0.2],[1.3,0.2],[1.4,0.1],[1.3,0.2],[1.5,0.2],[1.3,0.3],[1.3,0.3],[1.3,0.2],[1.6,0.6],[1.9,0.4],[1.4,0.3],[1.6,0.2],[1.4,0.2],[1.5,0.2],[1.4,0.2],[4.7,1.4],[4.5,1.5],[4.9,1.5],[4.0,1.3],[4.6,1.5],[4.5,1.3],[4.7,1.6],[3.3,1.0],[4.6,1.3],[3.9,1.4],[3.5,1.0],[4.2,1.5],[4.0,1.0],[4.7,1.4],[3.6,1.3],[4.4,1.4],[4.5,1.5],[4.1,1.0],[4.5,1.5],[3.9,1.1],[4.8,1.8],[4.0,1.3],[4.9,1.5],[4.7,1.2],[4.3,1.3],[4.4,1.4],[4.8,1.4],[5.0,1.7],[4.5,1.5],[3.5,1.0],[3.8,1.1],[3.7,1.0],[3.9,1.2],[5.1,1.6],[4.5,1.5],[4.5,1.6],[4.7,1.5],[4.4,1.3],[4.1,1.3],[4.0,1.3],[4.4,1.2],[4.6,1.4],[4.0,1.2],[3.3,1.0],[4.2,1.3],[4.2,1.2],[4.2,1.3],[4.3,1.3],[3.0,1.1],[4.1,1.3],[6.0,2.5],[5.1,1.9],[5.9,2.1],[5.6,1.8],[5.8,2.2],[6.6,2.1],[4.5,1.7],[6.3,1.8],[5.8,1.8],[6.1,2.5],[5.1,2.0],[5.3,1.9],[5.5,2.1],[5.0,2.0],[5.1,2.4],[5.3,2.3],[5.5,1.8],[6.7,2.2],[6.9,2.3],[5.0,1.5],[5.7,2.3],[4.9,2.0],[6.7,2.0],[4.9,1.8],[5.7,2.1],[6.0,1.8],[4.8,1.8],[4.9,1.8],[5.6,2.1],[5.8,1.6],[6.1,1.9],[6.4,2.0],[5.6,2.2],[5.1,1.5],[5.6,1.4],[6.1,2.3],[5.6,2.4],[5.5,1.8],[4.8,1.8],[5.4,2.1],[5.6,2.4],[5.1,2.3],[5.1,1.9],[5.9,2.3],[5.7,2.5],[5.2,2.3],[5.0,1.9],[5.2,2.0],[5.4,2.3],[5.1,1.8],];
> var SEL=[];
> var CVAL=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,];
> var NAME='iris-petal-lasso';
> var CID=234;
> var W=560,H=360,R=5.0,ALPHA=0.7;
> var COLOR='#4a9eff',SELCOLOR='#e3116c';
> var TITLE='Iris: petal length vs petal width (colour = variety)',XLAB='petal.length',YLAB='petal.width';
> var XB=null,YB=null;
> var cv=document.getElementById('sc_234_iris-petal-lasso');
> if(!cv)return;
> var ctx=cv.getContext('2d');
> if(!PTS.length){return;}
> var hasC=CVAL.length===PTS.length;
> var L=YLAB?52:40,Rm=hasC?54:14,T=TITLE?26:12,B=XLAB?40:26;
> var minX=Infinity,maxX=-Infinity,minY=Infinity,maxY=-Infinity;
> for(var i=0;i<PTS.length;i++){var p=PTS[i];if(p[0]<minX)minX=p[0];if(p[0]>maxX)maxX=p[0];if(p[1]<minY)minY=p[1];if(p[1]>maxY)maxY=p[1];}
> if(XB){minX=XB[0];maxX=XB[1];}
> if(YB){minY=YB[0];maxY=YB[1];}
> if(minX===maxX){minX-=1;maxX+=1;}
> if(minY===maxY){minY-=1;maxY+=1;}
> var cmin=Infinity,cmax=-Infinity;
> if(hasC){for(var i=0;i<CVAL.length;i++){if(CVAL[i]<cmin)cmin=CVAL[i];if(CVAL[i]>cmax)cmax=CVAL[i];}if(cmin===cmax){cmin-=1;cmax+=1;}}
> function sx(x){return L+(x-minX)/(maxX-minX)*(W-L-Rm);}
> function sy(y){return (H-B)-(y-minY)/(maxY-minY)*(H-B-T);}
> var STOPS=[[68,1,84],[59,82,139],[33,145,140],[94,201,98],[253,231,37]];
> function grad(t){if(t<0)t=0;if(t>1)t=1;var s=t*4,i=Math.floor(s),f=s-i;if(i>=4){i=3;f=1;}var a=STOPS[i],b=STOPS[i+1];return 'rgb('+Math.round(a[0]+(b[0]-a[0])*f)+','+Math.round(a[1]+(b[1]-a[1])*f)+','+Math.round(a[2]+(b[2]-a[2])*f)+')';}
> function colorOf(i){return hasC?grad((CVAL[i]-cmin)/(cmax-cmin)):COLOR;}
> var XS=new Float64Array(PTS.length),YS=new Float64Array(PTS.length);
> for(var i=0;i<PTS.length;i++){XS[i]=sx(PTS[i][0]);YS[i]=sy(PTS[i][1]);}
> var base=document.createElement('canvas');base.width=W;base.height=H;
> var bctx=base.getContext('2d');
> function drawBase(sset){
>   bctx.clearRect(0,0,W,H);
>   if(TITLE){bctx.fillStyle='#222';bctx.font='600 13px sans-serif';bctx.textAlign='center';bctx.fillText(TITLE,W/2,16);}
>   bctx.strokeStyle='#d7d7e0';bctx.lineWidth=1;bctx.beginPath();bctx.moveTo(L,T-4);bctx.lineTo(L,H-B);bctx.lineTo(W-Rm+4,H-B);bctx.stroke();
>   bctx.globalAlpha=ALPHA;
>   for(var i=0;i<PTS.length;i++){if(sset&&sset.has(i))continue;bctx.fillStyle=colorOf(i);bctx.fillRect(XS[i]-R,YS[i]-R,2*R,2*R);}
>   bctx.globalAlpha=1;
>   if(sset){bctx.fillStyle=SELCOLOR;sset.forEach(function(k){var s=R+1;bctx.fillRect(XS[k]-s,YS[k]-s,2*s,2*s);});}
>   bctx.fillStyle='#99a';bctx.font='10px sans-serif';bctx.textAlign='start';
>   bctx.fillText(String(+minX.toFixed(2)),L,H-B+14);bctx.fillText(String(+maxX.toFixed(2)),W-Rm-32,H-B+14);
>   bctx.fillText(String(+maxY.toFixed(2)),4,T+6);bctx.fillText(String(+minY.toFixed(2)),4,H-B);
>   bctx.fillStyle='#556';bctx.font='11px sans-serif';bctx.textAlign='center';
>   if(XLAB)bctx.fillText(XLAB,L+(W-L-Rm)/2,H-6);
>   if(YLAB){bctx.save();bctx.translate(12,T+(H-B-T)/2);bctx.rotate(-Math.PI/2);bctx.fillText(YLAB,0,0);bctx.restore();}
>   if(hasC){var bx=W-Rm+14,bw=10,bh=H-B-T;for(var g=0;g<bh;g++){bctx.fillStyle=grad(1-g/bh);bctx.fillRect(bx,T+g,bw,1);}bctx.fillStyle='#99a';bctx.font='9px sans-serif';bctx.textAlign='start';bctx.fillText(String(+cmax.toFixed(1)),bx-3,T-3);bctx.fillText(String(+cmin.toFixed(1)),bx-3,T+bh+10);}
> }
> function repaint(poly){
>   ctx.clearRect(0,0,W,H);ctx.drawImage(base,0,0);
>   if(poly&&poly.length>1){
>     ctx.strokeStyle=SELCOLOR;ctx.fillStyle='rgba(227,17,108,0.08)';ctx.lineWidth=1.5;
>     ctx.beginPath();ctx.moveTo(poly[0][0],poly[0][1]);
>     for(var i=1;i<poly.length;i++)ctx.lineTo(poly[i][0],poly[i][1]);
>     ctx.closePath();ctx.fill();ctx.stroke();
>   }
> }
> function inPoly(px,py,poly){
>   var c=false;
>   for(var i=0,j=poly.length-1;i<poly.length;j=i++){
>     var xi=poly[i][0],yi=poly[i][1],xj=poly[j][0],yj=poly[j][1];
>     if(((yi>py)!==(yj>py))&&(px<(xj-xi)*(py-yi)/(yj-yi)+xi))c=!c;
>   }
>   return c;
> }
> function pt(e){return [e.offsetX*(cv.width/cv.clientWidth),e.offsetY*(cv.height/cv.clientHeight)];}
> function post(idx){parent.postMessage({type:'widget',cellId:CID,name:NAME,value:'['+idx.join(',')+']'},'*');}
> drawBase(new Set(SEL));repaint(null);
> var drawing=false,poly=[];
> cv.addEventListener('pointerdown',function(e){if(e.button!==0)return;drawing=true;poly=[pt(e)];cv.setPointerCapture(e.pointerId);e.preventDefault();});
> cv.addEventListener('pointermove',function(e){if(!drawing)return;poly.push(pt(e));repaint(poly);e.preventDefault();});
> cv.addEventListener('pointerup',function(e){if(!drawing)return;drawing=false;if(poly.length<3){repaint(null);return;}var idx=[];for(var i=0;i<PTS.length;i++){if(inPoly(XS[i],YS[i],poly))idx.push(i);}drawBase(new Set(idx));repaint(null);post(idx);});
> cv.addEventListener('dblclick',function(){drawBase(new Set());repaint(null);post([]);});
> })();
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
> <input type='text' value='54' oninput="parent.postMessage({type:'widget',cellId:238,name:'seed',value:this.value,sel:this.selectionStart},'*')">
> ((109,5),(41,5))

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
> if petal.length .<= 3.9
>   then if sepal.width .- petal.width .<= 1.7000000000000002
>     then "Versicolor"
>     else "Setosa"
>   else if petal.width .< 1.8
>     then if petal.length .> 5.0
>       then "Virginica"
>       else if sepal.length .>= 5.0
>         then "Versicolor"
>         else "Virginica"
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
> Test accuracy: 0.926829268292683

## Confusion matrix

The original example built a confusion matrix by hand. With a `DataFrame` we get
it by grouping on the (actual, predicted) pair and counting. The diagonal
(`variety == predicted`) holds the correct predictions; off-diagonal entries are
the mistakes.

```haskell
scored |> D.groupBy ["variety", "predicted"]
       |> D.aggregate ["count" .= F.count (F.col @T.Text "variety")]
       |> D.sortBy [D.Asc "variety", D.Asc "predicted"]
       |> showMarkdown
```

> <!-- scripths:mime text/markdown -->
> | variety<br>Text | predicted<br>Text | count<br>Int |
> | ----------------|-------------------|------------- |
> | Versicolor      | Versicolor        | 16           |
> | Virginica       | Virginica         | 10           |
> | Virginica       | Versicolor        | 1            |
> | Setosa          | Setosa            | 12           |
> | Versicolor      | Virginica         | 2            |

## Conclusion

Decision trees ,on a clean, well-separated
dataset like Iris, are pretty easy and efficient to run.
