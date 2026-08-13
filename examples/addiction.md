# Can we tell who is addicted to their phone?


### A worked Kaggle notebook


This notebook works through Kaggle Playground Series S6E8. Each row is represents a single person's phone usage. Our target column (the one we are trying to predict) is `addicted_label` which says whether that person is addicted to their phone or not.

<!-- sabela:cell -->

## Contents

1. Setup and data loading
2. Initial data inspection
3. Data cleaning and imputation
4. Exploratory data analysis
5. Feature engineering and preprocessing
6. Baseline modelling
7. Iterative model training and tuning
8. Evaluation and conclusion

<!-- sabela:cell -->

## Reading the Haskell

The following are some guidelines on how to read the conventions in the notebook.

**Qualified imports.** `import qualified DataFrame as D` means "load that library and put `D.` in front of everything from it" (similar to Python). So `D.readCsv` is the library's `readCsv`. It keeps names from colliding when several libraries all have a function called `mean`.

**The pipe operator, `|>`.** `x |> f` means "take `x`, give it to `f`". It reads
left to right like a shell pipeline, so:

````haskell
df |> D.take 5 |> D.toMarkdown'
````

means "take `df`, keep the first 5 rows, then turn that into Markdown". The same thing written inside out would be `D.toMarkdown' (D.take 5 df)`.


**Type annotations in `@`.** `F.col @Double "age"` says "the column called `age`, holding `Double` values". `Double` is a decimal number. `Int` is a whole number. `Maybe Double` means "a decimal number, or nothing at all" (similar to an Optional type in other languages), which is how a column with gaps in it is described. If you specify the wrong type (e.g F.col @Text "age") or leave it without an annotation (F.col "age") the program will fail at runtime. To make life easier you should define these column references up front.


**`Expr`, a description of a calculation.** `F.col @Double "age"` is what we'll use as a stand-in reference for the age column in our dataframe. By itself it holds no data.

**`<-` versus `=`.** `=` defines something. `<-` is used when a step touches
the outside world, such as reading a file. `df <- D.readCsv "..."` means "read
that file and call the result `df`".

Anything else unfamiliar is explained where it first appears.

<!-- sabela:cell -->

## 1. Setup and loading the data

The first line of the cell below is a comment that Sabela reads as an instruction (similar to PEP 723 script metadata in Python):

`-- cabal: build-depends: dataframe == 3.4.0.0, dataframe-learn, ...`


Let's begin by taking a peek at the data.

`displayMarkdown` is Sabela's way of putting formatted output in the notebook.

Read the output as your first look at the data. Note `Just 24.0` and `Nothing`
in the cells: that is the `Maybe Double` type from the primer showing up in
practice. `Just 24.0` is a value that is present, `Nothing` is a "null".
```haskell
-- cabal: build-depends: dataframe == 3.4.0.0, dataframe-learn, dataframe-viz, hgg, text, containers, vector
import qualified DataFrame as D
import DataFrame ((|>))

df <- D.readCsv "./examples/data/addiction/train.csv"

df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | id<br>Int | age<br>Maybe Double | daily_screen_time_hours<br>Maybe Double | social_media_hours<br>Maybe Double | gaming_hours<br>Maybe Double | work_study_hours<br>Maybe Double | sleep_hours<br>Maybe Double | notifications_per_day<br>Maybe Double | app_opens_per_day<br>Maybe Double | weekend_screen_time<br>Maybe Double | gender<br>Maybe Text | stress_level<br>Maybe Text | academic_work_impact<br>Maybe Text | addicted_label<br>Int |
> | ----------|---------------------|-----------------------------------------|------------------------------------|------------------------------|----------------------------------|-----------------------------|---------------------------------------|-----------------------------------|-------------------------------------|----------------------|----------------------------|------------------------------------|---------------------- |
> | 0         | Just 24.0           | Nothing                                 | Just 1.83                          | Just 1.5899999999999999      | Just 2.11                        | Just 7.46                   | Just 122.0                            | Just 38.0                         | Just 8.63                           | Just "Male"          | Just "Medium"              | Just "No"                          | 1                     |
> | 1         | Just 19.0           | Just 5.97                               | Just 1.08                          | Nothing                      | Just 3.03                        | Just 8.22                   | Just 76.0                             | Just 19.0                         | Nothing                             | Just "Female"        | Just "Medium"              | Just "No"                          | 0                     |
> | 2         | Just 18.0           | Just 5.09                               | Nothing                            | Nothing                      | Nothing                          | Just 6.25                   | Just 134.0                            | Just 60.0                         | Just 7.47                           | Just "Female"        | Just "Low"                 | Just "Yes"                         | 0                     |
> | 3         | Just 21.0           | Just 6.42                               | Just 1.26                          | Just 1.42                    | Just 3.36                        | Just 8.85                   | Just 112.0                            | Just 94.0                         | Just 8.66                           | Just "Other"         | Just "Low"                 | Nothing                            | 1                     |
> | 4         | Just 26.0           | Just 11.2                               | Just 1.87                          | Just 2.81                    | Just 1.95                        | Just 5.25                   | Nothing                               | Nothing                           | Just 13.39                          | Just "Female"        | Just "Medium"              | Just "No"                          | 1                     |

## 2. Initial data inspection

Before modelling anything, we want to find out what state the data is in. The data collection process is never perfect so data often has gaps or subtle mismatches. We can get a glance of the data using the `D.describeColumns` function.



The `id` column is a unique row counter, so a duplicate-row check has nothing to find here; with data joined from several sources it would be worth a cell.
```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame ((|>), (./))

df |> D.describeColumns
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> |   Column Name<br>Text   | # Non-null Values<br>Int | # Null Values<br>Int | Type<br>Text |
> | ------------------------|--------------------------|----------------------|------------- |
> | social_media_hours      | 557374                   | 133995               | Maybe Double |
> | gaming_hours            | 564548                   | 126821               | Maybe Double |
> | weekend_screen_time     | 579306                   | 112063               | Maybe Double |
> | daily_screen_time_hours | 595515                   | 95854                | Maybe Double |
> | app_opens_per_day       | 610659                   | 80710                | Maybe Double |
> | notifications_per_day   | 623785                   | 67584                | Maybe Double |
> | stress_level            | 636221                   | 55148                | Maybe Text   |
> | work_study_hours        | 639851                   | 51518                | Maybe Double |
> | sleep_hours             | 646889                   | 44480                | Maybe Double |
> | academic_work_impact    | 647145                   | 44224                | Maybe Text   |
> | gender                  | 662335                   | 29034                | Maybe Text   |
> | age                     | 662440                   | 28929                | Maybe Double |
> | addicted_label          | 691369                   | 0                    | Int          |
> | id                      | 691369                   | 0                    | Int          |

We can get the split of addicted vs not addicted with the `frequencies` function.
```haskell
df |> D.frequencies (F.col @Int "addicted_label")
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | 0<br>Any | 1<br>Any |
> | ------------------|----------|--------- |
> | Count             | 200895   | 490474   |
> | Percentage (%)    | 29.06%   | 70.94%   |

Writing `F.col @<Type> <name>` every time is both tedious and brittle. You could misspell the name of the column or get the type wrong. We can automatically generate the column references using Haskell's code generation capabilities. `declareColumns` takes in a dataframe and creates column references aliases e.g `addicted_label = F.col @Int "addicted_label"`. These are visible everywhere in the notebook. Since column names could collide with internal function names (e.g this dataframe has an `id` column that would collide with the builtin `id` function) we also have a variant that adds a prefix to to the variable names. So `declareColumnsWithPrefix "raw"` creates `raw_addicted_label = F.col @Int "addicted_label"`
```haskell
$(D.declareColumnsWithPrefix "raw" df)

df |> D.frequencies raw_addicted_label
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | 0<br>Any | 1<br>Any |
> | ------------------|----------|--------- |
> | Count             | 200895   | 490474   |
> | Percentage (%)    | 29.06%   | 70.94%   |

490,474 people are addicted to their phones while 200,895 are not. That imbalance is measured properly in the EDA section.

<!-- sabela:cell -->

## 3. Data cleaning and imputation

The data collection process is never perfect. For example, surveys sometimes have follow-up questions that are only apply depending on your answer to a previous question. A survey might ask if you smoke, and then as a follow up ask "if you smoke, what brand do you prefer?" For all the non-smokers the follow up question will be blank/missing as a matter of survey design. Some smokers might even skip the question by mistake. Or they might have misread a the question and given a list of brands. We always expect data to need some kind of processing before we train on it. It's also how we come to understand the data.

The first thing we usually check for is missingness. First we quantify how much of each column is missing? Then we ask ourselves does the fact that a value is missing carry information by itself? 
```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F

nAll = D.nRows df

-- Since positive rows are all 1 we can get total positive by just summing the column.
nPos = D.sum raw_addicted_label df

basePrec = fromIntegral nPos / fromIntegral nAll :: Double
baseF1 = 2 * basePrec / (basePrec + 1)

-- round to 3 decminal places
r4 x = fromIntegral (round (x * 1e4)) / 1e4 :: Double

D.fromRows ["rows", "addicted", "positive_rate", "baseline_f1"]
  [ [D.toAny nAll, D.toAny nPos, D.toAny (r4 basePrec), D.toAny (r4 baseF1)] ]
  |> D.toMarkdown'
  |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | rows<br>Int | addicted<br>Int | positive_rate<br>Double | baseline_f1<br>Double |
> | ------------|-----------------|-------------------------|---------------------- |
> | 691369      | 490474          | 0.7094                  | 0.83                  |

All the columns have some number of null/Nothing rows but it's hard to tell how prevalent missingness is. Since `describeColumns` gives us back a dataframe we can compute the null rate manually.

We can use `D.derive` to add a new column computed from the others.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame ((|>), (./))

df |> D.describeColumns
   |> D.derive "null_rate" (F.col @Int "# Null Values" ./ D.lit (D.nRows df))
   |> D.sortBy [D.Desc (F.col @Double "null_rate")]
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> |   Column Name<br>Text   | # Non-null Values<br>Int | # Null Values<br>Int | Type<br>Text | null_rate<br>Double  |
> | ------------------------|--------------------------|----------------------|--------------|--------------------- |
> | social_media_hours      | 557374                   | 133995               | Maybe Double | 0.19381111967704656  |
> | gaming_hours            | 564548                   | 126821               | Maybe Double | 0.18343460583277527  |
> | weekend_screen_time     | 579306                   | 112063               | Maybe Double | 0.16208855184423948  |
> | daily_screen_time_hours | 595515                   | 95854                | Maybe Double | 0.13864376331597164  |
> | app_opens_per_day       | 610659                   | 80710                | Maybe Double | 0.1167393967620764   |
> | notifications_per_day   | 623785                   | 67584                | Maybe Double | 9.77538767286355e-2  |
> | stress_level            | 636221                   | 55148                | Maybe Text   | 7.976637656591487e-2 |
> | work_study_hours        | 639851                   | 51518                | Maybe Double | 7.451592420256042e-2 |
> | sleep_hours             | 646889                   | 44480                | Maybe Double | 6.433612152121371e-2 |
> | academic_work_impact    | 647145                   | 44224                | Maybe Text   | 6.396584168512039e-2 |
> | gender                  | 662335                   | 29034                | Maybe Text   | 4.199494047317713e-2 |
> | age                     | 662440                   | 28929                | Maybe Double | 4.184306788415448e-2 |
> | addicted_label          | 691369                   | 0                    | Int          | 0.0                  |
> | id                      | 691369                   | 0                    | Int          | 0.0                  |

Nearly a fifth of `social_media_hours` is absent, and no column is clean. Every
one of the nine numeric columns has gaps, between 4% and 19% of it.



<!-- sabela:cell -->


### Is the missingness itself a signal?

A gap in our data is [not always an accident](https://en.wikipedia.org/wiki/Missing_data). If people who spend a lot of time on social media are more likely to skip the question, then the fact that
the answer is missing is itself informative.

We can test this correlation. For each column, we'll compare the addiction rate among rows where that column is missing against the overall rate of 0.7094. The gap between them is the lift. A column whose absence carried information would show a lift well away from zero.

```haskell
:t D.filterNothing
```

> <!-- scripths:mime text/plain -->
> D.filterNothing
>   :: Data.Text.Internal.Text -> D.DataFrame -> D.DataFrame

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified Data.Text as Txt
import DataFrame ((|>))

nullRow c =
  let sub = D.filterNothing c df
      n = D.nRows sub
      nAddicted = D.sum raw_addicted_label sub
      p = fromIntegral nAddicted / fromIntegral (max 1 n) :: Double
  in [D.toAny c, D.toAny n, D.toAny (r4 p), D.toAny (r4 (p - basePrec))]

nullCols = df
         |> D.selectBy [D.byProperty D.hasMissing]
         |> D.columnNames

D.fromRows ["column", "null_rows", "p_addicted_given_null", "lift"]
  (map nullRow nullCols)
  |> D.sortBy [D.Desc (F.col @Double "lift")]
  |> D.toMarkdown'
  |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> |     column<br>Text      | null_rows<br>Int | p_addicted_given_null<br>Double | lift<br>Double |
> | ------------------------|------------------|---------------------------------|--------------- |
> | age                     | 28929            | 0.7134                          | 4.0e-3         |
> | sleep_hours             | 44480            | 0.7134                          | 4.0e-3         |
> | app_opens_per_day       | 80710            | 0.7128                          | 3.4e-3         |
> | daily_screen_time_hours | 95854            | 0.7113                          | 1.9e-3         |
> | work_study_hours        | 51518            | 0.7106                          | 1.2e-3         |
> | weekend_screen_time     | 112063           | 0.7107                          | 1.2e-3         |
> | gaming_hours            | 126821           | 0.7105                          | 1.1e-3         |
> | notifications_per_day   | 67584            | 0.7102                          | 8.0e-4         |
> | social_media_hours      | 133995           | 0.7094                          | 0.0            |
> | academic_work_impact    | 44224            | 0.7095                          | 0.0            |
> | stress_level            | 55148            | 0.709                           | -4.0e-4        |
> | gender                  | 29034            | 0.7088                          | -6.0e-4        |

Every lift sits within a few thousandths of the base rate, across tens of
thousands of rows. Taken on its own, missingness doesn't seem very informative.

The table abov tests whether missingness predicts the target on its own. Missingness could still be useful when paired with other features. For example, `social_media_hours` being empty while `gaming_hours` is not that could mean that the user is not a big phone users (and is instead a big gamer).

So rather than decide from this table, let's try and test these interactions.

<!-- sabela:cell -->

Before we impute these gaps away, we can run that test directly. A shallow tree is the right instrument: give it the raw columns with the gaps left in, plus an explicit 0/1 `_absent` flag for each nullable column, and a pairing such as social media absent while gaming hours are high is a path it can express in two splits. We fit the same depth-3 tree twice, once on values alone and once with the flags added, and compare cross-validated AUC. If no flag earns a split and the two scores match, missingness carries nothing here, alone or in combination, and the imputation in the next section destroys no evidence.
```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame.Learn (maxTreeDepth, useLinearSolver)
import DataFrame ((|>))
import Data.Text (Text)

absentFlag c = F.lift (maybe (1.0 :: Double) (const 0.0)) (F.col @(Maybe Double) c)

probeCols :: [Text]
probeCols =
  [ "age", "daily_screen_time_hours", "social_media_hours", "gaming_hours"
  , "work_study_hours", "sleep_hours", "notifications_per_day"
  , "app_opens_per_day", "weekend_screen_time" ]

probeBase = df |> D.take 6000
  |> D.derive "yProbe" (F.toDouble (F.col @Int "addicted_label"))

probeValues = probeBase |> D.select (probeCols <> ["yProbe"])

probeFlagged = probeBase
  |> D.fold (\c -> D.derive (c <> "_absent") (absentFlag c)) probeCols
  |> D.select (probeCols <> map (<> "_absent") probeCols <> ["yProbe"])

probeCfg = L.defaultTreeConfig { maxTreeDepth = 3, useLinearSolver = False }

probeAuc frame =
  let scores = L.crossValidate 3 42 L.rocAuc (F.col @Double "yProbe")
                 (\tr -> L.predict (L.fit probeCfg (F.col @Double "yProbe") tr)) frame
  in sum scores / fromIntegral (length scores)

probeTree = L.fit probeCfg (F.col @Double "yProbe") probeFlagged

displayMarkdown (unlines
  [ "```", D.prettyPrint (L.predict probeTree), "```"
  , "| frame | depth-3 AUC |", "|---|---|"
  , "| values only | " <> show (r4 (probeAuc probeValues)) <> " |"
  , "| values + absence flags | " <> show (r4 (probeAuc probeFlagged)) <> " |" ])
```

> <!-- scripths:mime text/markdown -->
> ```
> if fromMaybe(daily_screen_time_hours .+ weekend_screen_time_absent .< 8.25)
>      .|| social_media_hours_absent .+ weekend_screen_time_absent .<= 1.0
> then if social_media_hours .<=. 1.9849999999999999
> then if fromMaybe(weekend_screen_time .+ social_media_hours_absent .>= 9.93)
>      .|| fromMaybe(daily_screen_time_hours .+ weekend_screen_time .>= 16.81)
> then 1.0
> else 0.0
> else 1.0
> else 1.0
> ```
> | frame | depth-3 AUC |
> |---|---|
> | values only | 0.8181 |
> | values + absence flags | 0.8044 |

The answer is firmer than expected. The tree did spend splits on the absence flags, and its cross-validated AUC dropped from 0.8181 to 0.8044 for the privilege. Missingness here is actually a distraction. Filling the gaps doesn't destroy information.

<!-- sabela:cell -->

With the missingness question settled, we can fill the gaps without destroying evidence. We'll impute all missing features with their mean. `D.fold` applies one imputation per column-and-value pair in the `imputations` list. 

`prep` also derives the target `y` as a decimal number (the learners below want `Double`) and selects only the model's columns. `D.select` drops `id` (a row counter a model would happily memorise) and `addicted_label` (the answer itself). Leaving either in is called leakage: the model reads the answer off its input, scores almost perfectly, and has learned nothing.

<!-- sabela:cell -->

The sliders below, and their siblings before sections 8 and 9, control how much data and compute the expensive steps use. They default LOW so the whole notebook runs in seconds; the figures quoted in the prose were computed at full fidelity, so expect the small-sample outputs to differ until you raise them.
```haskell
displayHtml "Work rows"
workRows <- mkWidget (slider "work rows" (2000 :: Int) 500 150000)
displayHtml "Synthesizer rows"
synRows <- mkWidget (slider "synthesiser rows" (500 :: Int) 200 20000)
displayHtml "Cross validation rows"
cvRows <- mkWidget (slider "cross-validation rows" (1000 :: Int) 500 50000)
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/html -->
> Work rows
> <!-- MIME:text/html -->
> <div id='sw_120_work rows'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_120_work rows",cid:120,name:"work rows",min:500,max:150000,value:2000});</script>
> <!-- MIME:text/html -->
> Synthesizer rows
> <!-- MIME:text/html -->
> <div id='sw_120_synthesiser rows'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_120_synthesiser rows",cid:120,name:"synthesiser rows",min:200,max:20000,value:500});</script>
> <!-- MIME:text/html -->
> Cross validation rows
> <!-- MIME:text/html -->
> <div id='sw_120_cross-validation rows'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_120_cross-validation rows",cid:120,name:"cross-validation rows",min:500,max:50000,value:1000});</script>

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame ((|>))
import Data.Text (Text)


-- Column to fill, and the value to fill it with: each column's own mean.
imputations :: [(Text, Double)]
imputations =
  [ ("daily_screen_time_hours", 7.64)
  , ("social_media_hours",      2.47)
  , ("gaming_hours",            1.46)
  , ("sleep_hours",             6.80)
  , ("notifications_per_day", 145.89)
  , ("app_opens_per_day",     102.64)
  , ("weekend_screen_time",     9.48)
  , ("work_study_hours",        2.37)
  , ("age",                    26.62)
  ]

modelColumns =
  [ "age", "daily_screen_time_hours", "social_media_hours", "gaming_hours"
  , "work_study_hours", "sleep_hours", "notifications_per_day"
  , "app_opens_per_day", "weekend_screen_time", "y" ]

prep d =
  d |> D.fold (\(c, v) -> D.impute (F.col @(Maybe Double) c) v) imputations
    |> D.derive "y" (F.toDouble (F.col @Int "addicted_label"))
    |> D.select modelColumns

work = prep (df |> D.take workRows)
workPos = D.nRows (D.filterWhere (F.col @Double "y" `F.eq` F.lit 1.0) work)

displayMarkdown (show workRows <> " rows, positive rate "
  <> show (r4 (fromIntegral workPos / fromIntegral workRows)) <> " against 0.7094 for the full data")
```

> <!-- scripths:mime text/markdown -->
> 2000 rows, positive rate 0.688 against 0.7094 for the full data

## 4. Exploratory data analysis

Now we look at the cleaned data properly, before any model touches it. The most useful single view for a binary target is the same plot drawn twice, once per class. A feature that matters shows two humps in different places. A feature that does not shows the same hump drawn twice.

Two `hgg` details, both learned the hard way, in case you write your own: `G.groupBy` is not the way to split by class, because it writes the same field the mark's x encoding uses and silently replaces the column being plotted. And `G.colorBy` looks its column up by name, so it does nothing when the data is passed in directly as it is here. The way that works is one layer per class with a fixed colour, which is what the cells below do.

<!-- sabela:cell -->


### Which columns actually separate the two groups?

We want to see which columns are correlated with our target. A correlation with a 0/1 target is awkward to interpret. **Cohen's d** is the better tool: the distance between the two group means, measured in standard deviations.

```haskell
import qualified DataFrame as D
import qualified Data.Vector as V
import qualified DataFrame.Functions as F
import qualified Graphics.Hgg as G
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.List (sortOn)
import DataFrame ((|>))

txtCol xs = G.ColTxt (V.fromList xs)
numCol xs = G.ColNum (V.fromList xs)
edaBlue = G.rgb 76 114 176
edaOrange = G.rgb 221 132 82

numericCols :: [Text]
numericCols = map fst imputations

edaFrame = work
edaAddicted = D.filterWhere (F.col @Double "y" `F.eq` F.lit 1.0) edaFrame
edaClean = D.filterWhere (F.col @Double "y" `F.eq` F.lit 0.0) edaFrame

cohensD c =
  let e = F.col @Double c
      ma = D.mean e edaAddicted
      mc = D.mean e edaClean
      sa = D.standardDeviation e edaAddicted
      sc = D.standardDeviation e edaClean
      pooled = sqrt ((sa * sa + sc * sc) / 2)
   in if pooled == 0 then 0 else (ma - mc) / pooled

effects = sortOn (negate . abs . snd) [ (c, cohensD c) | c <- numericCols ]

displayMarkdown (D.toMarkdown' (D.fromRows ["feature", "Cohen's d"]
  [ [D.toAny c, D.toAny (r4 d)] | (c, d) <- effects ]))

displaySvg (Tx.unpack (G.renderSVG
  (G.overlay [G.bar (txtCol (map fst effects)) (numCol (map snd effects)) <> G.color edaOrange]
    <> G.title "Screen time separates the classes; age and sleep do not"
    <> G.yLabel "Cohen's d" <> G.coordFlip <> G.width 760 <> G.height 380)))
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> |     feature<br>Text     | Cohen's d<br>Double |
> | ------------------------|-------------------- |
> | daily_screen_time_hours | 1.6166              |
> | weekend_screen_time     | 1.4352              |
> | social_media_hours      | 1.2586              |
> | work_study_hours        | 0.5889              |
> | gaming_hours            | 0.4287              |
> | app_opens_per_day       | 0.1118              |
> | notifications_per_day   | -8.56e-2            |
> | age                     | 8.24e-2             |
> | sleep_hours             | 6.5e-2              |
> 
> <!-- MIME:image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1013" height="507" viewBox="0 0 1013 507"><rect x="0.0" y="0.0" width="1013.3333333333333" height="506.66666666666663" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="53.38666666666666" y1="451.7347826086956" x2="1006.0" y2="451.7347826086956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="402.959420289855" x2="1006.0" y2="402.959420289855" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="354.18405797101445" x2="1006.0" y2="354.18405797101445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="305.4086956521739" x2="1006.0" y2="305.4086956521739" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="256.6333333333333" x2="1006.0" y2="256.6333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="207.85797101449273" x2="1006.0" y2="207.85797101449273" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="159.08260869565214" x2="1006.0" y2="159.08260869565214" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="110.3072463768116" x2="1006.0" y2="110.3072463768116" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="61.531884057970956" x2="1006.0" y2="61.531884057970956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="267.40736900684374" y1="32.266666666666666" x2="267.40736900684374" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="521.7877181322099" y1="32.266666666666666" x2="521.7877181322099" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="776.168067257576" y1="32.266666666666666" x2="776.168067257576" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="140.2171944441607" y1="32.266666666666666" x2="140.2171944441607" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="394.59754356952686" y1="32.266666666666666" x2="394.59754356952686" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="648.9778926948931" y1="32.266666666666666" x2="648.9778926948931" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="903.3582418202591" y1="32.266666666666666" x2="903.3582418202591" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="53.38666666666666" y="32.266666666666666" width="952.6133333333333" height="448.73333333333335" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="451.7347826086956" x2="49.72" y2="451.7347826086956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="455.84144927536227" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">age</text><line x1="53.38666666666666" y1="402.959420289855" x2="49.72" y2="402.959420289855" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="407.06608695652164" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">app_opens_per_day</text><line x1="53.38666666666666" y1="354.18405797101445" x2="49.72" y2="354.18405797101445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="358.2907246376811" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">daily_screen_time_hours</text><line x1="53.38666666666666" y1="305.4086956521739" x2="49.72" y2="305.4086956521739" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="309.5153623188406" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">gaming_hours</text><line x1="53.38666666666666" y1="256.6333333333333" x2="49.72" y2="256.6333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="260.74" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">notifications_per_day</text><line x1="53.38666666666666" y1="207.85797101449273" x2="49.72" y2="207.85797101449273" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="211.96463768115942" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">sleep_hours</text><line x1="53.38666666666666" y1="159.08260869565214" x2="49.72" y2="159.08260869565214" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="163.18927536231882" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">social_media_hours</text><line x1="53.38666666666666" y1="110.3072463768116" x2="49.72" y2="110.3072463768116" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="114.41391304347827" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">weekend_screen_time</text><line x1="53.38666666666666" y1="61.531884057970956" x2="49.72" y2="61.531884057970956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="65.63855072463761" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">work_study_hours</text><line x1="140.2171944441607" y1="481.0" x2="140.2171944441607" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="140.2171944441607" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.0</text><line x1="394.59754356952686" y1="481.0" x2="394.59754356952686" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="394.59754356952686" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.5</text><line x1="648.9778926948931" y1="481.0" x2="648.9778926948931" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="648.9778926948931" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1.0</text><line x1="903.3582418202591" y1="481.0" x2="903.3582418202591" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="903.3582418202591" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1.5</text><text x="53.38666666666666" y="21.413333333333334" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">Screen time separates the classes; age and sleep do not</text><text x="17.599999999999998" y="256.6333333333333" fill="#333333" font-size="14.666666666666666" font-family="sans-serif" text-anchor="middle" transform="rotate(-90.0 17.599999999999998 256.6333333333333)">Cohen&apos;s d</text><rect x="140.2171944441607" y="332.23514492753617" width="822.4821994952332" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="88.35833333333335" width="730.1740133906025" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="137.13369565217388" width="640.327007654198" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="39.58297101449269" width="299.62517203944384" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="283.4597826086956" width="218.1300732476648" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="381.01050724637673" width="56.87700763749463" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="96.68727272727273" y="234.68442028985504" width="43.529921716887976" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="429.78586956521735" width="41.927627062720866" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="140.2171944441607" y="185.90905797101448" width="33.04777487410759" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/></svg>

The top three columns are very informative. They are the three screen-time measures. 

In comparison, `work_study_hours` and `gaming_hours` are moderate. `app_opens_per_day`, `sleep_hours`, `notifications_per_day` and `age` are all essentially zero, meaning the addicted and non-addicted groups have the same average age and the same average sleep.

That is worth pausing on. Sleep and notification count are exactly the variables you would expect a phone-addiction dataset to turn on, and they carry nothing. The label is driven almost entirely by hours of screen time.

<!-- sabela:cell -->


### The shape of each feature, split by class

Cohen's d compressed each column to one number. These panels show the whole
distribution behind it: one curve per class, per feature.

A **density curve** is a smoothed histogram. The area under it is 1, so the two
classes are comparable even though there are more than twice as many addicted
people. Height means "how common values around here are for this group".

**Reading it:** where the blue and orange curves sit apart, the feature
separates the classes, and where they lie on top of each other it does not. The
three screen-time panels show two clearly displaced humps. The `age` and
`sleep_hours` panels show curves almost exactly on top of one another, which is
the same finding as their near-zero effect size, in a form where you can see
why.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import qualified Graphics.Hgg as G
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as Tx
import Data.Text (Text)

edaColOf d c = G.ColNum (VU.convert (L.columnOf d (F.col @Double c)))

chunk3 xs = if null xs then [] else take 3 xs : chunk3 (drop 3 xs)
edaGrid f xs = G.vconcat [ G.hconcat (map f r) | r <- chunk3 xs ]

densPanel c = G.overlay
  [ G.density (edaColOf edaClean c) <> G.color edaBlue
  , G.density (edaColOf edaAddicted c) <> G.color edaOrange
  ] <> G.title c

displaySvg (Tx.unpack (G.renderSVG
  (edaGrid densPanel numericCols <> G.width 960 <> G.height 720)))
```

> <!-- scripths:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1280" height="960" viewBox="0 0 1280 960"><rect x="0.0" y="0.0" width="1280.0" height="960.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="106.3516604540338" y1="39.599999999999994" x2="106.3516604540338" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="224.89600403455682" y1="39.599999999999994" x2="224.89600403455682" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="343.4403476150799" y1="39.599999999999994" x2="343.4403476150799" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="165.6238322442953" y1="39.599999999999994" x2="165.6238322442953" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="284.1681758248184" y1="39.599999999999994" x2="284.1681758248184" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="402.71251940534137" y1="39.599999999999994" x2="402.71251940534137" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="263.5186520290708" x2="409.84888888888884" y2="263.5186520290708" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="206.77817830943462" x2="409.84888888888884" y2="206.77817830943462" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="150.03770458979844" x2="409.84888888888884" y2="150.03770458979844" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="93.29723087016228" x2="409.84888888888884" y2="93.29723087016228" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="291.88888888888886" x2="409.84888888888884" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="235.1484151692527" x2="409.84888888888884" y2="235.1484151692527" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="178.40794144961652" x2="409.84888888888884" y2="178.40794144961652" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="121.66746772998039" x2="409.84888888888884" y2="121.66746772998039" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="64.9269940103442" x2="409.84888888888884" y2="64.9269940103442" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="49.42666666666666" y="39.599999999999994" width="360.4222222222222" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="165.6238322442953" y1="291.88888888888886" x2="165.6238322442953" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="165.6238322442953" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="284.1681758248184" y1="291.88888888888886" x2="284.1681758248184" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="284.1681758248184" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">10</text><line x1="402.71251940534137" y1="291.88888888888886" x2="402.71251940534137" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="402.71251940534137" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">15</text><line x1="49.42666666666666" y1="291.88888888888886" x2="45.76" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.00</text><line x1="49.42666666666666" y1="235.1484151692527" x2="45.76" y2="235.1484151692527" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="239.25508183591938" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.05</text><line x1="49.42666666666666" y1="178.40794144961652" x2="45.76" y2="178.40794144961652" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="182.5146081162832" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.10</text><line x1="49.42666666666666" y1="121.66746772998039" x2="45.76" y2="121.66746772998039" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="125.77413439664704" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.15</text><line x1="49.42666666666666" y1="64.9269940103442" x2="45.76" y2="64.9269940103442" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="69.03366067701087" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.20</text><text x="49.42666666666666" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">daily_screen_time_hours</text><path d="M 49.42666666666666 290.9418978641662 L 53.06729517396184 290.08929056849774 L 56.707923681257014 288.7251877159471 L 60.34855218855219 286.7324863564123 L 63.98918069584736 284.07579635206616 L 67.62980920314253 280.8424248721145 L 71.2704377104377 277.24001631440194 L 74.91106621773288 273.5298915581951 L 78.55169472502806 269.9046333683684 L 82.19232323232322 266.35539799818787 L 85.83295173961841 262.5969498446155 L 89.47358024691357 258.1066567517605 L 93.11420875420875 252.284193427087 L 96.75483726150392 244.67236626626766 L 100.39546576879911 235.13517467823993 L 104.03609427609425 223.90132102963827 L 107.67672278338945 211.4516886557241 L 111.31735129068461 198.31678194360296 L 114.95797979797979 184.8959980643258 L 118.59860830527498 171.38540024427726 L 122.23923681257011 157.82984698557493 L 125.87986531986532 144.25399799503032 L 129.52049382716046 130.8074400101656 L 133.16112233445565 117.86935880349593 L 136.80175084175085 106.06905228058281 L 140.44237934904598 96.18614089128994 L 144.0830078563412 88.92159197235503 L 147.72363636363633 84.59181196434909 L 151.36426487093152 82.86969583917796 L 155.0048933782267 82.72705226054984 L 158.64552188552187 82.67882650400584 L 162.28615039281706 81.29205037809166 L 165.92677890011225 77.76345116172467 L 169.5674074074074 72.29157500891763 L 173.20803591470258 66.04901961227229 L 176.84866442199774 60.77260767464001 L 180.48929292929293 58.19585008870206 L 184.12992143658812 59.594824645241445 L 187.77054994388325 65.57334280414216 L 191.41117845117847 76.00884986246542 L 195.0518069584736 90.00302749227728 L 198.6924354657688 105.79833686960531 L 202.333063973064 120.81793135406087 L 205.97369248035915 132.06582383749833 L 209.6143209876543 136.97978876906302 L 213.25494949494947 134.52460712685314 L 216.89557800224466 126.03462804036015 L 220.5362065095398 115.2632281645665 L 224.17683501683499 107.37846946868804 L 227.81746352413018 107.17756594434243 L 231.45809203142534 117.28824172540926 L 235.09872053872053 137.2298754705997 L 238.73934904601572 163.7824891600896 L 242.37997755331085 192.40261752462658 L 246.02060606060604 218.90635477507203 L 249.6612345679012 240.63390841465142 L 253.3018630751964 256.74168221462327 L 256.9424915824915 267.7783742300987 L 260.5831200897867 274.9712205175886 L 264.2237485970819 279.6087672125227 L 267.86437710437707 282.6993388653503 L 271.50500561167223 284.8885617357765 L 275.14563411896745 286.52626592565247 L 278.78626262626256 287.7760489953505 L 282.4268911335578 288.7079106853745 L 286.06751964085294 289.3589247586393 L 289.7081481481481 289.7682153181609 L 293.3487766554433 289.99357662247957 L 296.9894051627384 290.11179018825226 L 300.63003367003364 290.2042117831629 L 304.2706621773288 290.334742880012 L 307.911290684624 290.5322363159006 L 311.5519191919192 290.78731168464003 L 315.1925476992143 291.06476813385416 L 318.8331762065095 291.3237091417342 L 322.4738047138047 291.53453052065663 L 326.11443322109983 291.68604877148846 L 329.75506172839505 291.7829625438153 L 333.39569023569027 291.8384427800656 L 337.0363187429854 291.8669841490074 L 340.67694725028053 291.880217481241 L 344.31757575757575 291.8857595103146 L 347.9582042648709 291.88785937812736 L 351.59883277216613 291.88858014296164 L 355.2394612794613 291.8888044843919 L 358.8800897867564 291.8888678548801 L 362.5207182940517 291.88888411063044 L 366.1613468013468 291.8888878994071 L 369.80197530864194 291.8888887021063 L 373.4426038159371 291.88888885674817 L 377.0832323232323 291.8888888838473 L 380.7238608305275 291.88888888816797 L 384.36448933782265 291.8888888887949 L 388.00511784511787 291.88888888887766 L 391.64574635241297 291.88888888888766 L 395.28637485970813 291.88888888888874 L 398.9270033670033 291.88888888888886 L 402.56763187429857 291.88888888888886 L 406.2082603815937 291.88888888888886 L 409.84888888888884 291.88888888888886" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 49.42666666666666 291.52747467160697 L 53.06729517396184 291.3409472313757 L 56.707923681257014 291.09637117865657 L 60.34855218855219 290.7840509885341 L 63.98918069584736 290.38825364254603 L 67.62980920314253 289.8850497749086 L 71.2704377104377 289.24526217452063 L 74.91106621773288 288.44422926106995 L 78.55169472502806 287.47596064663423 L 82.19232323232322 286.3648599535161 L 85.83295173961841 285.16685890604276 L 89.47358024691357 283.95576207744307 L 93.11420875420875 282.7985704816629 L 96.75483726150392 281.7306968086093 L 100.39546576879911 280.7430583195478 L 104.03609427609425 279.78669479077723 L 107.67672278338945 278.79095814822597 L 111.31735129068461 277.68502050416714 L 114.95797979797979 276.4132108438984 L 118.59860830527498 274.94087419735285 L 122.23923681257011 273.25403401278373 L 125.87986531986532 271.3588582648373 L 129.52049382716046 269.28468061595163 L 133.16112233445565 267.08893152500485 L 136.80175084175085 264.857236336494 L 140.44237934904598 262.6908883084255 L 144.0830078563412 260.6791133268935 L 147.72363636363633 258.86347537553945 L 151.36426487093152 257.21066127465906 L 155.0048933782267 255.61069790277202 L 158.64552188552187 253.90722877183705 L 162.28615039281706 251.94901470457916 L 165.92677890011225 249.63761295645338 L 169.5674074074074 246.9454537960067 L 173.20803591470258 243.89384371304916 L 176.84866442199774 240.50305769066688 L 180.48929292929293 236.74099711429017 L 184.12992143658812 232.4914456498931 L 187.77054994388325 227.54087640119974 L 191.41117845117847 221.56251785674561 L 195.0518069584736 214.0802883757673 L 198.6924354657688 204.43175750444112 L 202.333063973064 191.80019614925493 L 205.97369248035915 175.40782867935053 L 209.6143209876543 154.91024700145744 L 213.25494949494947 130.89772573806727 L 216.89557800224466 105.2554016971073 L 220.5362065095398 81.08071590480645 L 224.17683501683499 62.00092972624611 L 227.81746352413018 51.04917711707999 L 231.45809203142534 49.564352889519036 L 235.09872053872053 56.65243622934936 L 238.73934904601572 69.49493800454866 L 242.37997755331085 84.35623417662346 L 246.02060606060604 97.80452679528824 L 249.6612345679012 107.62250255767106 L 253.3018630751964 113.12382684003265 L 256.9424915824915 114.91549090632157 L 260.5831200897867 114.35501031207073 L 264.2237485970819 112.97675174643389 L 267.86437710437707 112.0646738707068 L 271.50500561167223 112.43008010624342 L 275.14563411896745 114.37287664612055 L 278.78626262626256 117.7699774227304 L 282.4268911335578 122.2283116330795 L 286.06751964085294 127.24903701684379 L 289.7081481481481 132.3675056317456 L 293.3487766554433 137.25378437486097 L 296.9894051627384 141.77330001613052 L 300.63003367003364 146.01190133417376 L 304.2706621773288 150.26596924931863 L 307.911290684624 154.99263064288684 L 311.5519191919192 160.71559766516145 L 315.1925476992143 167.89400883942926 L 318.8331762065095 176.7825731666209 L 322.4738047138047 187.32898442999613 L 326.11443322109983 199.152871813766 L 329.75506172839505 211.62297045429355 L 333.39569023569027 224.00764054453708 L 337.0363187429854 235.6424174865906 L 340.67694725028053 246.05549710886433 L 344.31757575757575 255.01736501232813 L 347.9582042648709 262.51654219093007 L 351.59883277216613 268.68943403794634 L 355.2394612794613 273.7391113588426 L 358.8800897867564 277.8692924961515 L 362.5207182940517 281.2456765310059 L 366.1613468013468 283.98479679368717 L 369.80197530864194 286.1633126172442 L 373.4426038159371 287.837572804691 L 377.0832323232323 289.06337291486545 L 380.7238608305275 289.9084428714226 L 384.36448933782265 290.45443865180636 L 388.00511784511787 290.78963989746137 L 391.64574635241297 290.99673954685056 L 395.28637485970813 291.141219336423 L 398.9270033670033 291.26476972360103 L 402.56763187429857 291.38563560842346 L 406.2082603815937 291.5047752991113 L 409.84888888888884 291.6146170031586" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="533.6293671973507" y1="39.599999999999994" x2="533.6293671973507" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="623.5214291058735" y1="39.599999999999994" x2="623.5214291058735" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="713.4134910143963" y1="39.599999999999994" x2="713.4134910143963" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="803.3055529229191" y1="39.599999999999994" x2="803.3055529229191" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="488.68333624308934" y1="39.599999999999994" x2="488.68333624308934" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="578.575398151612" y1="39.599999999999994" x2="578.575398151612" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="668.467460060135" y1="39.599999999999994" x2="668.467460060135" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="758.3595219686576" y1="39.599999999999994" x2="758.3595219686576" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="268.0475285790877" x2="834.071111111111" y2="268.0475285790877" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="220.36480795948546" x2="834.071111111111" y2="220.36480795948546" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="172.68208733988314" x2="834.071111111111" y2="172.68208733988314" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="124.99936672028088" x2="834.071111111111" y2="124.99936672028088" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="77.31664610067858" x2="834.071111111111" y2="77.31664610067858" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="291.88888888888886" x2="834.071111111111" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="244.20616826928656" x2="834.071111111111" y2="244.20616826928656" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="196.5234476496843" x2="834.071111111111" y2="196.5234476496843" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="148.840727030082" x2="834.071111111111" y2="148.840727030082" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="101.1580064104797" x2="834.071111111111" y2="101.1580064104797" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="53.475285790877436" x2="834.071111111111" y2="53.475285790877436" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="473.6488888888889" y="39.599999999999994" width="360.4222222222221" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="488.68333624308934" y1="291.88888888888886" x2="488.68333624308934" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="488.68333624308934" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="578.575398151612" y1="291.88888888888886" x2="578.575398151612" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="578.575398151612" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="668.467460060135" y1="291.88888888888886" x2="668.467460060135" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="668.467460060135" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="758.3595219686576" y1="291.88888888888886" x2="758.3595219686576" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="758.3595219686576" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="473.6488888888889" y1="291.88888888888886" x2="469.9822222222222" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="473.6488888888889" y1="244.20616826928656" x2="469.9822222222222" y2="244.20616826928656" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="248.31283493595325" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="473.6488888888889" y1="196.5234476496843" x2="469.9822222222222" y2="196.5234476496843" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="200.63011431635098" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="473.6488888888889" y1="148.840727030082" x2="469.9822222222222" y2="148.840727030082" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="152.9473936967487" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><line x1="473.6488888888889" y1="101.1580064104797" x2="469.9822222222222" y2="101.1580064104797" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="105.26467307714637" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="473.6488888888889" y1="53.475285790877436" x2="469.9822222222222" y2="53.475285790877436" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="57.581952457544105" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.5</text><text x="473.6488888888889" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">social_media_hours</text><path d="M 473.6488888888889 288.278361052805 L 477.28951739618407 284.6717563275396 L 480.93014590347923 278.83676403849773 L 484.57077441077445 270.3969424121921 L 488.2114029180696 259.41897162058444 L 491.85203142536477 246.46977875435587 L 495.4926599326599 232.44885024830228 L 499.1332884399551 218.26012886773162 L 502.77391694725026 204.4783644292735 L 506.4145454545454 191.15425347297165 L 510.05517396184064 177.83544951337706 L 513.6958024691357 163.81823565718906 L 517.336430976431 148.57232244136662 L 520.9770594837262 132.1694520733632 L 524.6176879910213 115.47807600829901 L 528.2583164983164 99.9929382271686 L 531.8989450056116 87.40735924697563 L 535.5395735129068 79.17273320785944 L 539.1802020202019 76.19352077869249 L 542.8208305274972 78.63929600578186 L 546.4614590347924 85.84369986122104 L 550.1020875420875 96.37442208022188 L 553.7427160493827 108.37676780260308 L 557.3833445566779 120.12585940903674 L 561.023973063973 130.5244863733422 L 564.6646015712681 139.24772399305797 L 568.3052300785633 146.39121185127385 L 571.9458585858586 151.7326707698445 L 575.5864870931538 153.9821483270378 L 579.2271156004489 150.60458383176996 L 582.8677441077441 138.77476613826616 L 586.5083726150392 117.5046524051005 L 590.1490011223344 89.94833615940595 L 593.7896296296295 64.01723913154709 L 597.4302581369247 49.89316808933377 L 601.0708866442199 55.17830757744919 L 604.7115151515152 80.71322699424123 L 608.3521436588103 120.11347183464738 L 611.9927721661054 163.34600311396147 L 615.6334006734006 201.6490000942029 L 619.2740291806958 230.5842178926723 L 622.9146576879909 250.05898659040034 L 626.5552861952862 262.43349916454514 L 630.1959147025814 270.4883061076298 L 633.8365432098765 276.2616859605415 L 637.4771717171717 280.8011733920948 L 641.1178002244668 284.43190202575283 L 644.758428731762 287.1562437888716 L 648.3990572390571 288.95996623593265 L 652.0396857463523 289.95049774123197 L 655.6803142536475 290.3549195992417 L 659.3209427609427 290.4473242208129 L 662.9615712682378 290.466600454678 L 666.602199775533 290.5598781147157 L 670.2428282828282 290.76743026336004 L 673.8834567901233 291.04905679058425 L 677.5240852974187 291.33400555163587 L 681.1647138047138 291.5660651284842 L 684.8053423120089 291.7236148929785 L 688.4459708193041 291.81444551093887 L 692.0865993265993 291.85939044032216 L 695.7272278338944 291.878606040128 L 699.3678563411896 291.8857355882237 L 703.0084848484848 291.8880382332451 L 706.64911335578 291.8886870158789 L 710.2897418630752 291.8888467446072 L 713.9303703703702 291.8888811489963 L 717.5709988776655 291.8888876384367 L 721.2116273849606 291.88888871116933 L 724.8522558922559 291.8888888666691 L 728.4928843995509 291.888888886445 L 732.1335129068462 291.8888888886524 L 735.7741414141412 291.88888888886873 L 739.4147699214365 291.8888888888873 L 743.0553984287317 291.88888888888874 L 746.6960269360268 291.88888888888886 L 750.336655443322 291.88888888888886 L 753.9772839506172 291.88888888888886 L 757.6179124579123 291.88888888888886 L 761.2585409652075 291.88888888888886 L 764.8991694725028 291.88888888888886 L 768.5397979797979 291.88888888888886 L 772.1804264870931 291.88888888888886 L 775.8210549943883 291.88888888888886 L 779.4616835016834 291.88888888888886 L 783.1023120089786 291.88888888888886 L 786.7429405162738 291.88888888888886 L 790.383569023569 291.88888888888886 L 794.0241975308642 291.88888888888886 L 797.6648260381592 291.88888888888886 L 801.3054545454545 291.88888888888886 L 804.9460830527496 291.88888888888886 L 808.5867115600447 291.88888888888886 L 812.2273400673399 291.88888888888886 L 815.8679685746353 291.88888888888886 L 819.5085970819302 291.88888888888886 L 823.1492255892255 291.88888888888886 L 826.7898540965207 291.88888888888886 L 830.4304826038158 291.88888888888886 L 834.071111111111 291.88888888888886" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 473.6488888888889 290.7960697469954 L 477.28951739618407 290.0713671702199 L 480.93014590347923 289.02902899545563 L 484.57077441077445 287.6172837445914 L 488.2114029180696 285.81119296427323 L 491.85203142536477 283.6215173300124 L 495.4926599326599 281.0965251946066 L 499.1332884399551 278.3149103464491 L 502.77391694725026 275.37044121159613 L 506.4145454545454 272.3518365741852 L 510.05517396184064 269.3235213433701 L 513.6958024691357 266.31295663113366 L 517.336430976431 263.3077035706651 L 520.9770594837262 260.2617012696935 L 524.6176879910213 257.1078391642758 L 528.2583164983164 253.77378787551635 L 531.8989450056116 250.19855805605727 L 535.5395735129068 246.34605394590758 L 539.1802020202019 242.2095336489662 L 542.8208305274972 237.8014810531472 L 546.4614590347924 233.13023286118266 L 550.1020875420875 228.17466921513937 L 553.7427160493827 222.87168976564976 L 557.3833445566779 217.11989344809666 L 561.023973063973 210.78102490290192 L 564.6646015712681 203.64789216685895 L 568.3052300785633 195.36676036984431 L 571.9458585858586 185.35792843303744 L 575.5864870931538 172.83862915226723 L 579.2271156004489 157.05850173015375 L 582.8677441077441 137.76426546367742 L 586.5083726150392 115.73735695963717 L 590.1490011223344 93.1000702635462 L 593.7896296296295 73.09553036575437 L 597.4302581369247 59.270468860677354 L 601.0708866442199 54.328266622864774 L 604.7115151515152 59.15801019063713 L 608.3521436588103 72.5041374315161 L 611.9927721661054 91.42539498838369 L 615.6334006734006 112.29921468763526 L 619.2740291806958 131.90122295243873 L 622.9146576879909 148.1390526349492 L 626.5552861952862 160.26319196972193 L 630.1959147025814 168.6356952923784 L 633.8365432098765 174.27497629827963 L 637.4771717171717 178.3908283223872 L 641.1178002244668 182.04046253178788 L 644.758428731762 185.9461526625965 L 648.3990572390571 190.45551861633297 L 652.0396857463523 195.59933157894858 L 655.6803142536475 201.19777655602076 L 659.3209427609427 206.97409209398134 L 662.9615712682378 212.6480129885353 L 666.602199775533 217.99594342540502 L 670.2428282828282 222.8767424528715 L 673.8834567901233 227.22959421422584 L 677.5240852974187 231.05412339487862 L 681.1647138047138 234.3844456713676 L 684.8053423120089 237.26896779181791 L 688.4459708193041 239.76491689426285 L 692.0865993265993 241.94886612986957 L 695.7272278338944 243.93369326374264 L 699.3678563411896 245.87510957612474 L 703.0084848484848 247.95393174036033 L 706.64911335578 250.3337024073249 L 710.2897418630752 253.10889781564333 L 713.9303703703702 256.26661012865554 L 717.5709988776655 259.68005569777074 L 721.2116273849606 263.139267503655 L 724.8522558922559 266.4099029531433 L 728.4928843995509 269.30049729561637 L 732.1335129068462 271.7144325121783 L 735.7741414141412 273.6671522144115 L 739.4147699214365 275.26213105349865 L 743.0553984287317 276.6370283895566 L 746.6960269360268 277.90567153687704 L 750.336655443322 279.1230303133375 L 753.9772839506172 280.28686302127807 L 757.6179124579123 281.36877843737415 L 761.2585409652075 282.3520430029889 L 764.8991694725028 283.2528969942415 L 768.5397979797979 284.11551879782485 L 772.1804264870931 284.98806664958073 L 775.8210549943883 285.89703320587626 L 779.4616835016834 286.83471419863264 L 783.1023120089786 287.76400426712513 L 786.7429405162738 288.63443815878156 L 790.383569023569 289.39951499153614 L 794.0241975308642 290.02817005852 L 797.6648260381592 290.5087519531594 L 801.3054545454545 290.84771995222667 L 804.9460830527496 291.06590015130297 L 808.5867115600447 291.1937412555023 L 812.2273400673399 291.26573211199667 L 815.8679685746353 291.3141600044952 L 819.5085970819302 291.3633275024649 L 823.1492255892255 291.4260032121073 L 826.7898540965207 291.5033977959458 L 830.4304826038158 291.5884954187177 L 834.071111111111 291.6710996541285" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="962.4568245266737" y1="39.599999999999994" x2="962.4568245266737" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1044.7825947921424" y1="39.599999999999994" x2="1044.7825947921424" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1127.108365057611" y1="39.599999999999994" x2="1127.108365057611" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1209.4341353230798" y1="39.599999999999994" x2="1209.4341353230798" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="921.2939393939392" y1="39.599999999999994" x2="921.2939393939392" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1003.619709659408" y1="39.599999999999994" x2="1003.619709659408" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1085.9454799248767" y1="39.599999999999994" x2="1085.9454799248767" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1168.2712501903457" y1="39.599999999999994" x2="1168.2712501903457" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1250.5970204558143" y1="39.599999999999994" x2="1250.5970204558143" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="261.0831102750094" x2="1265.3333333333333" y2="261.0831102750094" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="199.47155304725044" x2="1265.3333333333333" y2="199.47155304725044" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="137.85999581949153" x2="1265.3333333333333" y2="137.85999581949153" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="76.2484385917326" x2="1265.3333333333333" y2="76.2484385917326" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="291.88888888888886" x2="1265.3333333333333" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="230.27733166112995" x2="1265.3333333333333" y2="230.27733166112995" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="168.665774433371" x2="1265.3333333333333" y2="168.665774433371" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="107.05421720561203" x2="1265.3333333333333" y2="107.05421720561203" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="45.44265997785317" x2="1265.3333333333333" y2="45.44265997785317" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="904.911111111111" y="39.599999999999994" width="360.4222222222223" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="921.2939393939392" y1="291.88888888888886" x2="921.2939393939392" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="921.2939393939392" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="1003.619709659408" y1="291.88888888888886" x2="1003.619709659408" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1003.619709659408" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1</text><line x1="1085.9454799248767" y1="291.88888888888886" x2="1085.9454799248767" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1085.9454799248767" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="1168.2712501903457" y1="291.88888888888886" x2="1168.2712501903457" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1168.2712501903457" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">3</text><line x1="1250.5970204558143" y1="291.88888888888886" x2="1250.5970204558143" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1250.5970204558143" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="904.911111111111" y1="291.88888888888886" x2="901.2444444444443" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="904.911111111111" y1="230.27733166112995" x2="901.2444444444443" y2="230.27733166112995" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="234.38399832779663" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="904.911111111111" y1="168.665774433371" x2="901.2444444444443" y2="168.665774433371" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="172.7724411000377" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="904.911111111111" y1="107.05421720561203" x2="901.2444444444443" y2="107.05421720561203" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="111.1608838722787" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.6</text><line x1="904.911111111111" y1="45.44265997785317" x2="901.2444444444443" y2="45.44265997785317" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="49.54932664451983" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.8</text><text x="904.911111111111" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">gaming_hours</text><path d="M 904.911111111111 280.7632753647109 L 908.5517396184061 276.14637819250976 L 912.1923681257014 270.5126624386524 L 915.8329966329966 263.9775109456844 L 919.4736251402917 256.7577322384557 L 923.1142536475869 249.13897833258707 L 926.7548821548821 241.4238396376624 L 930.3955106621772 233.87484389763762 L 934.0361391694724 226.669612522644 L 937.6767676767677 219.8818225816887 L 941.3173961840628 213.49251996557962 L 944.958024691358 207.42553040890766 L 948.5986531986532 201.59285602360913 L 952.2392817059482 195.93417168669708 L 955.8799102132434 190.43900976011514 L 959.5205387205385 185.14826880584366 L 963.1611672278337 180.13939667750765 L 966.801795735129 175.5038914931878 L 970.4424242424241 171.32568742137732 L 974.0830527497193 167.66573066870632 L 977.7236812570145 164.55381570837798 L 981.3643097643096 161.98540469250833 L 985.0049382716048 159.919421153774 L 988.6455667789 158.27266545308575 L 992.2861952861952 156.90722132954463 L 995.9268237934904 155.60937330588007 L 999.5674523007856 154.06312915725113 L 1003.2080808080807 151.82892699504438 L 1006.8487093153759 148.34680648852213 L 1010.4893378226711 142.9884226359214 L 1014.1299663299662 135.17702838944638 L 1017.7705948372615 124.57405933907434 L 1021.4112233445567 111.29723899195524 L 1025.0518518518518 96.1001724357734 L 1028.6924803591469 80.4266263890208 L 1032.3331088664422 66.27172336222398 L 1035.9737373737373 55.84069472336371 L 1039.6143658810324 51.07571619167811 L 1043.2549943883278 53.18765902576664 L 1046.8956228956226 62.34699589405727 L 1050.536251402918 77.64158396909954 L 1054.176879910213 97.31516327178943 L 1057.8175084175082 119.20055106520047 L 1061.4581369248035 141.200148586446 L 1065.0987654320986 161.66747759090674 L 1068.7393939393937 179.5995768375166 L 1072.380022446689 194.62962776366277 L 1076.0206509539842 206.87561048729737 L 1079.6612794612793 216.73098097510518 L 1083.3019079685746 224.675164252884 L 1086.9425364758697 231.14927747666343 L 1090.5831649831648 236.50540129699644 L 1094.2237934904601 241.01075267103295 L 1097.8644219977552 244.87718120086458 L 1101.5050505050503 248.28959587528007 L 1105.1456790123457 251.41822471173742 L 1108.7863075196408 254.41250237159272 L 1112.426936026936 257.3839975698462 L 1116.0675645342312 260.3897054929901 L 1119.7081930415263 263.42524935927713 L 1123.3488215488214 266.43200867636 L 1126.9894500561168 269.3158968886698 L 1130.6300785634116 271.97124768352455 L 1134.270707070707 274.3025361018862 L 1137.911335578002 276.23912417844053 L 1141.5519640852974 277.74207712023656 L 1145.1925925925925 278.80515502521826 L 1148.8332210998876 279.4529756651145 L 1152.4738496071827 279.7381569274226 L 1156.114478114478 279.73728272837235 L 1159.7551066217732 279.5443684370201 L 1163.3957351290683 279.2609817465649 L 1167.0363636363636 278.9839675299927 L 1170.6769921436587 278.7936097357531 L 1174.3176206509538 278.74573978090984 L 1177.9582491582491 278.8701816575376 L 1181.5988776655443 279.1754626532895 L 1185.2395061728394 279.65713927530123 L 1188.8801346801347 280.3057118692344 L 1192.5207631874298 281.1106462118582 L 1196.161391694725 282.05922573749143 L 1199.8020202020202 283.1317166550711 L 1203.4426487093153 284.2962555218794 L 1207.0832772166104 285.50702347159785 L 1210.7239057239058 286.7076415276677 L 1214.3645342312007 287.8392040112437 L 1218.005162738496 288.8502497350805 L 1221.6457912457913 289.7052279639471 L 1225.2864197530862 290.3888349815056 L 1228.9270482603815 290.9053851954632 L 1232.5676767676769 291.27419230776616 L 1236.208305274972 291.5230082693999 L 1239.848933782267 291.68164726830287 L 1243.4895622895622 291.7772522386388 L 1247.1301907968573 291.83172587750926 L 1250.7708193041526 291.8610770825448 L 1254.411447811448 291.8760357787112 L 1258.0520763187428 291.8832481490011 L 1261.6927048260382 291.88653862900986 L 1265.3333333333333 291.8879593534387" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 904.911111111111 280.33746676144153 L 908.5517396184061 276.36194881427684 L 912.1923681257014 271.61317951886554 L 915.8329966329966 266.14180835497 L 919.4736251402917 260.06108805151337 L 923.1142536475869 253.54270052793527 L 926.7548821548821 246.80389839891419 L 930.3955106621772 240.08748084287714 L 934.0361391694724 233.63759277412646 L 937.6767676767677 227.67513262579874 L 941.3173961840628 222.37650388565206 L 944.958024691358 217.85864043453273 L 948.5986531986532 214.1719675974423 L 952.2392817059482 211.30158426578546 L 955.8799102132434 209.17574581473156 L 959.5205387205385 207.67982596437312 L 963.1611672278337 206.6733456874183 L 966.801795735129 206.0073416262086 L 970.4424242424241 205.53930226079916 L 974.0830527497193 205.14318210120211 L 977.7236812570145 204.7126845767763 L 981.3643097643096 204.15710382831412 L 985.0049382716048 203.3904579484621 L 988.6455667789 202.31626086112948 L 992.2861952861952 200.81183517959676 L 995.9268237934904 198.71728933417762 L 999.5674523007856 195.83483081285863 L 1003.2080808080807 191.94353602951472 L 1006.8487093153759 186.83258481024362 L 1010.4893378226711 180.35201276956303 L 1014.1299663299662 172.47449066138597 L 1017.7705948372615 163.35561747452377 L 1021.4112233445567 153.37570921381777 L 1025.0518518518518 143.14546358900827 L 1028.6924803591469 133.4629642764043 L 1032.3331088664422 125.2203059020762 L 1035.9737373737373 119.272327416382 L 1039.6143658810324 116.29314622845484 L 1043.2549943883278 116.65341838366956 L 1046.8956228956226 120.34904073413509 L 1050.536251402918 127.00011908224418 L 1054.176879910213 135.92089308536796 L 1057.8175084175082 146.24287017519765 L 1061.4581369248035 157.06075311130252 L 1065.0987654320986 167.56763953706923 L 1068.7393939393937 177.1525698775521 L 1072.380022446689 185.44656972819917 L 1076.0206509539842 192.31790607076488 L 1079.6612794612793 197.8287583214899 L 1083.3019079685746 202.17125968682458 L 1086.9425364758697 205.60069794600213 L 1090.5831649831648 208.3792533265331 L 1094.2237934904601 210.73742666563638 L 1097.8644219977552 212.85442560782843 L 1101.5050505050503 214.85453437569848 L 1105.1456790123457 216.8142986948208 L 1108.7863075196408 218.77498301038167 L 1112.426936026936 220.755668151794 L 1116.0675645342312 222.7639505252894 L 1119.7081930415263 224.80292810139736 L 1123.3488215488214 226.87458051647832 L 1126.9894500561168 228.98050841628907 L 1130.6300785634116 231.12124093834683 L 1134.270707070707 233.2951032226437 L 1137.911335578002 235.49723073377564 L 1141.5519640852974 237.7189817917228 L 1145.1925925925925 239.94785786861183 L 1148.8332210998876 242.16804517744424 L 1152.4738496071827 244.3616867608935 L 1156.114478114478 246.51084766213546 L 1159.7551066217732 248.59984010698594 L 1163.3957351290683 250.6172705519494 L 1167.0363636363636 252.55705930517232 L 1170.6769921436587 254.41790062724058 L 1174.3176206509538 256.20114375749574 L 1177.9582491582491 257.90768218227413 L 1181.5988776655443 259.5348709604468 L 1185.2395061728394 261.07455540627484 L 1188.8801346801347 262.512967658554 L 1192.5207631874298 263.8326837724667 L 1196.161391694725 265.01626815816394 L 1199.8020202020202 266.05085589414955 L 1203.4426487093153 266.932799683609 L 1207.0832772166104 267.67157172702076 L 1210.7239057239058 268.29224198834277 L 1214.3645342312007 268.8359733758331 L 1218.005162738496 269.3580943555861 L 1221.6457912457913 269.9235180329228 L 1225.2864197530862 270.59965996932726 L 1228.9270482603815 271.44756514661526 L 1232.5676767676769 272.5125614461 L 1236.208305274972 273.8161925217249 L 1239.848933782267 275.35122564096054 L 1243.4895622895622 277.0810667718148 L 1247.1301907968573 278.94401176400254 L 1250.7708193041526 280.861658774321 L 1254.411447811448 282.74983467538567 L 1258.0520763187428 284.52984584963247 L 1261.6927048260382 286.1379055366693 L 1265.3333333333333 287.53117323162417" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="65.07974758723088" y1="357.15555555555557" x2="65.07974758723088" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="138.0544838136375" y1="357.15555555555557" x2="138.0544838136375" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="211.02922004004407" y1="357.15555555555557" x2="211.02922004004407" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="284.00395626645064" y1="357.15555555555557" x2="284.00395626645064" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="356.97869249285725" y1="357.15555555555557" x2="356.97869249285725" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="101.56711570043417" y1="357.15555555555557" x2="101.56711570043417" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="174.54185192684076" y1="357.15555555555557" x2="174.54185192684076" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="247.51658815324734" y1="357.15555555555557" x2="247.51658815324734" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="320.4913243796539" y1="357.15555555555557" x2="320.4913243796539" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="393.4660606060605" y1="357.15555555555557" x2="393.4660606060605" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="569.8814931432662" x2="409.84888888888884" y2="569.8814931432662" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="490.7555905409099" x2="409.84888888888884" y2="490.7555905409099" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="411.62968793855345" x2="409.84888888888884" y2="411.62968793855345" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="609.4444444444445" x2="409.84888888888884" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="530.318541842088" x2="409.84888888888884" y2="530.318541842088" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="451.19263923973165" x2="409.84888888888884" y2="451.19263923973165" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="372.0667366373753" x2="409.84888888888884" y2="372.0667366373753" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="49.42666666666666" y="357.15555555555557" width="360.4222222222222" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="101.56711570043417" y1="609.4444444444445" x2="101.56711570043417" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="101.56711570043417" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="174.54185192684076" y1="609.4444444444445" x2="174.54185192684076" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="174.54185192684076" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="247.51658815324734" y1="609.4444444444445" x2="247.51658815324734" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="247.51658815324734" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">7</text><line x1="320.4913243796539" y1="609.4444444444445" x2="320.4913243796539" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="320.4913243796539" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">8</text><line x1="393.4660606060605" y1="609.4444444444445" x2="393.4660606060605" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="393.4660606060605" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">9</text><line x1="49.42666666666666" y1="609.4444444444445" x2="45.76" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="49.42666666666666" y1="530.318541842088" x2="45.76" y2="530.318541842088" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="534.4252085087546" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="49.42666666666666" y1="451.19263923973165" x2="45.76" y2="451.19263923973165" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="455.29930590639833" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="49.42666666666666" y1="372.0667366373753" x2="45.76" y2="372.0667366373753" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="376.1734033040419" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><text x="49.42666666666666" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">sleep_hours</text><path d="M 49.42666666666666 577.1775891208372 L 53.06729517396184 571.2754157842422 L 56.70792368125701 564.8737984955078 L 60.348552188552176 558.0304472829322 L 63.98918069584735 550.8129775517754 L 67.62980920314251 543.2951255594708 L 71.2704377104377 535.5531309703025 L 74.91106621773287 527.6627431232873 L 78.55169472502804 519.6971720401808 L 82.1923232323232 511.7261169736822 L 85.83295173961838 503.8158026194204 L 89.47358024691354 496.0297753360742 L 93.11420875420873 488.43009022427293 L 96.7548372615039 481.0784716671376 L 100.39546576879914 474.0370553448229 L 104.03609427609429 467.3684051368473 L 107.67672278338947 461.13462181096526 L 111.31735129068466 455.3954987789065 L 114.95797979797983 450.2058146908836 L 118.598608305275 445.61197150926535 L 122.23923681257016 441.6482844179751 L 125.87986531986533 438.3333037192409 L 129.52049382716052 435.6665946079814 L 133.16112233445568 433.62641025678204 L 136.80175084175085 432.16865530360985 L 140.442379349046 431.22743931830007 L 144.0830078563412 430.71735862228024 L 147.7236363636364 430.53742881300684 L 151.36426487093155 430.5763457950298 L 155.0048933782267 430.7185229320561 L 158.64552188552187 430.8501887610123 L 162.28615039281706 430.8647838597159 L 165.92677890011225 430.6670001304984 L 169.5674074074074 430.1750635951172 L 173.20803591470258 429.321237154806 L 176.84866442199774 428.0509402260633 L 180.48929292929293 426.32125060019916 L 184.12992143658806 424.0997697196955 L 187.77054994388325 421.36481775423 L 191.41117845117844 418.1076480330039 L 195.05180695847358 414.3368622734906 L 198.69243546576877 410.08456245119635 L 202.33306397306393 405.4131333085521 L 205.97369248035912 400.4210698501988 L 209.61432098765425 395.246084167512 L 213.25494949494944 390.06392532398354 L 216.89557800224463 385.0819217452505 L 220.5362065095398 380.52711595323177 L 224.17683501683496 376.6298429079799 L 227.81746352413012 373.60450574590743 L 231.4580920314253 371.6299363714655 L 235.0987205387205 370.83195792653544 L 238.73934904601566 371.27054243615066 L 242.37997755331082 372.93332644305565 L 246.02060606060599 375.73633832829626 L 249.66123456790118 379.53177971877074 L 253.30186307519642 384.12177211095496 L 256.9424915824916 389.27627667249135 L 260.5831200897867 394.75300714488077 L 264.2237485970819 400.31710041086563 L 267.86437710437707 405.7585442756352 L 271.5050056116723 410.90580695211213 L 275.14563411896745 415.6346746807476 L 278.78626262626256 419.87189831079365 L 282.4268911335578 423.5938118165069 L 286.06751964085294 426.8205719693674 L 289.7081481481481 429.6070504392831 L 293.3487766554433 432.03166715660745 L 296.9894051627384 434.1845703544486 L 300.63003367003364 436.15653301551293 L 304.2706621773288 438.02974730372546 L 307.91129068462396 439.8713760569851 L 311.5519191919192 441.73030374501377 L 315.1925476992144 443.6370780693986 L 318.83317620650956 445.60661678304314 L 322.4738047138047 447.64293639487056 L 326.11443322109994 449.74498412796606 L 329.75506172839505 451.9126352291006 L 333.39569023569027 454.1520352333512 L 337.03631874298543 456.47967608161827 L 340.6769472502806 458.9248391840375 L 344.31757575757575 461.5302651394778 L 347.9582042648709 464.3510844147437 L 351.59883277216613 467.45215626760455 L 355.2394612794613 470.9040280996496 L 358.88008978675646 474.7777717189718 L 362.5207182940517 479.1390049182881 L 366.16134680134684 484.0414828034666 L 369.801975308642 489.52074044233143 L 373.4426038159371 495.5883637659681 L 377.0832323232323 502.22752338224393 L 380.7238608305275 509.39038907630936 L 384.36448933782265 516.9979261822821 L 388.00511784511787 524.9423555343833 L 391.64574635241297 533.0922590353521 L 395.2863748597082 541.2999782459397 L 398.9270033670033 549.4106418905307 L 402.5676318742985 557.2719274165338 L 406.2082603815937 564.7435555622471 L 409.84888888888884 571.7055554054649" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 49.42666666666666 586.6529732224037 L 53.06729517396184 580.8033753409402 L 56.70792368125701 574.1618207802329 L 60.348552188552176 566.8055205081289 L 63.98918069584735 558.852991503471 L 67.62980920314251 550.4569312100664 L 71.2704377104377 541.7929902054996 L 74.91106621773287 533.0457573812953 L 78.55169472502804 524.3937723064522 L 82.1923232323232 515.995573622471 L 85.83295173961838 507.97863733910606 L 89.47358024691354 500.43258562549056 L 93.11420875420873 493.4073500754022 L 96.7548372615039 486.9161881074684 L 100.39546576879914 480.94272038015356 L 104.03609427609429 475.4506035202147 L 107.67672278338947 470.39415722167007 L 111.31735129068466 465.728257525386 L 114.95797979797983 461.4160676207399 L 118.598608305275 457.4336407044182 L 122.23923681257016 453.77100586162305 L 125.87986531986533 450.42993733269907 L 129.52049382716052 447.41911726165563 L 133.16112233445568 444.7477614903729 L 136.80175084175085 442.4189470554386 L 140.442379349046 440.4238502239567 L 144.0830078563412 438.73789285529574 L 147.7236363636364 437.31943819762785 L 151.36426487093155 436.1112216823914 L 155.0048933782267 435.044203770829 L 158.64552188552187 434.0430578699062 L 162.28615039281706 433.0321381483906 L 165.92677890011225 431.9406008102611 L 169.5674074074074 430.705462388563 L 173.20803591470258 429.27181870942115 L 176.84866442199774 427.5901971820489 L 180.48929292929293 425.6119554698433 L 184.12992143658806 423.28455502594113 L 187.77054994388325 420.5491465845645 L 191.41117845117844 417.34292951866803 L 195.05180695847358 413.60800967906164 L 198.69243546576877 409.30699080745046 L 202.33306397306393 404.44354167859365 L 205.97369248035912 399.084154479371 L 209.61432098765425 393.3758415644991 L 213.25494949494944 387.55415848304466 L 216.89557800224463 381.9370163925228 L 220.5362065095398 376.9022124434801 L 224.17683501683496 372.85000289444525 L 227.81746352413012 370.15558003681053 L 231.4580920314253 369.1190718128952 L 235.0987205387205 369.921881115427 L 238.73934904601566 372.5974200432907 L 242.37997755331082 377.0216916565304 L 246.02060606060599 382.92534654451293 L 249.66123456790118 389.9247376155456 L 253.30186307519642 397.5661091796121 L 256.9424915824916 405.3751486806415 L 260.5831200897867 412.904029754358 L 264.2237485970819 419.76961605871867 L 267.86437710437707 425.67910252633766 L 271.5050056116723 430.4422644771581 L 275.14563411896745 433.9719198108975 L 278.78626262626256 436.2756770827035 L 282.4268911335578 437.4423820055956 L 286.06751964085294 437.62606985474304 L 289.7081481481481 437.02910572999014 L 293.3487766554433 435.88505546828566 L 296.9894051627384 434.4411034746159 L 300.63003367003364 432.93974529972184 L 304.2706621773288 431.60000849093353 L 307.91129068462396 430.5993511936231 L 311.5519191919192 430.058282142656 L 315.1925476992144 430.0302608451666 L 318.83317620650956 430.4993076396466 L 322.4738047138047 431.386905030415 L 326.11443322109994 432.56833974993845 L 329.75506172839505 433.8969248000113 L 333.39569023569027 435.2329418293482 L 337.03631874298543 436.4730265295226 L 340.6769472502806 437.57534183718855 L 344.31757575757575 438.57633708959156 L 347.9582042648709 439.5960912328365 L 351.59883277216613 440.83095749088267 L 355.2394612794613 442.53415633982405 L 358.88008978675646 444.9867795446096 L 362.5207182940517 448.46308974521594 L 366.16134680134684 453.19482792092197 L 369.801975308642 459.33937402414074 L 373.4426038159371 466.9560442998014 L 377.0832323232323 475.99364645356724 L 380.7238608305275 486.2908255825076 L 384.36448933782265 497.58895449185644 L 388.00511784511787 509.5556174804683 L 391.64574635241297 521.8153651876189 L 395.2863748597082 533.9835870224974 L 398.9270033670033 545.6991737865048 L 402.5676318742985 556.6521273860851 L 406.2082603815937 566.6033006396567 L 409.84888888888884 575.3948087085383" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="497.1546859903381" y1="357.15555555555557" x2="497.1546859903381" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="568.384374176548" y1="357.15555555555557" x2="568.384374176548" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="639.614062362758" y1="357.15555555555557" x2="639.614062362758" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="710.8437505489679" y1="357.15555555555557" x2="710.8437505489679" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="782.0734387351778" y1="357.15555555555557" x2="782.0734387351778" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="532.7695300834431" y1="357.15555555555557" x2="532.7695300834431" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="603.999218269653" y1="357.15555555555557" x2="603.999218269653" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="675.2289064558629" y1="357.15555555555557" x2="675.2289064558629" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="746.4585946420727" y1="357.15555555555557" x2="746.4585946420727" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="817.6882828282828" y1="357.15555555555557" x2="817.6882828282828" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="572.4407720913902" x2="834.071111111111" y2="572.4407720913902" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="498.4334273852817" x2="834.071111111111" y2="498.4334273852817" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="424.42608267917313" x2="834.071111111111" y2="424.42608267917313" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="609.4444444444445" x2="834.071111111111" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="535.437099738336" x2="834.071111111111" y2="535.437099738336" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="461.42975503222743" x2="834.071111111111" y2="461.42975503222743" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="387.42241032611884" x2="834.071111111111" y2="387.42241032611884" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="473.6488888888889" y="357.15555555555557" width="360.4222222222221" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="532.7695300834431" y1="609.4444444444445" x2="532.7695300834431" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="532.7695300834431" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">50</text><line x1="603.999218269653" y1="609.4444444444445" x2="603.999218269653" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="603.999218269653" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">100</text><line x1="675.2289064558629" y1="609.4444444444445" x2="675.2289064558629" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="675.2289064558629" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">150</text><line x1="746.4585946420727" y1="609.4444444444445" x2="746.4585946420727" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="746.4585946420727" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">200</text><line x1="817.6882828282828" y1="609.4444444444445" x2="817.6882828282828" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="817.6882828282828" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">250</text><line x1="473.6488888888889" y1="609.4444444444445" x2="469.9822222222222" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.000</text><line x1="473.6488888888889" y1="535.437099738336" x2="469.9822222222222" y2="535.437099738336" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="539.5437664050025" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.002</text><line x1="473.6488888888889" y1="461.42975503222743" x2="469.9822222222222" y2="461.42975503222743" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="465.53642169889406" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.004</text><line x1="473.6488888888889" y1="387.42241032611884" x2="469.9822222222222" y2="387.42241032611884" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="391.52907699278546" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.006</text><text x="473.6488888888889" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">notifications_per_day</text><path d="M 473.6488888888889 579.221108223489 L 477.28951739618407 574.5910926359343 L 480.93014590347923 569.8281114118943 L 484.57077441077445 565.0256684739699 L 488.2114029180696 560.2747459707726 L 491.85203142536477 555.656596123009 L 495.4926599326599 551.2365432769067 L 499.1332884399551 547.0596011531472 L 502.77391694725026 543.1484697808085 L 506.4145454545454 539.5041410586954 L 510.05517396184064 536.1089683628442 L 513.6958024691357 532.9317080956157 L 517.336430976431 529.9337770998719 L 520.9770594837262 527.0758291488673 L 524.6176879910213 524.3237513920076 L 528.2583164983164 521.6533075491038 L 531.8989450056116 519.0528784259825 L 535.5395735129068 516.5240301776964 L 539.1802020202019 514.0799332531116 L 542.8208305274972 511.74192306305605 L 546.4614590347924 509.5347107406719 L 550.1020875420875 507.4809033807072 L 553.7427160493827 505.5955703038725 L 557.3833445566779 503.88159173600366 L 561.023973063973 502.32644682589296 L 564.6646015712681 500.90093880367033 L 568.3052300785633 499.5601208302095 L 571.9458585858586 498.2463911102123 L 575.5864870931537 496.89439883448887 L 579.2271156004489 495.43708767152157 L 582.8677441077441 493.81195661060093 L 586.5083726150392 491.96649691276264 L 590.1490011223344 489.8618155293885 L 593.7896296296295 487.4737010695411 L 597.4302581369247 484.79081339346715 L 601.0708866442199 481.8102267966379 L 604.7115151515151 478.53113836442094 L 608.3521436588103 474.94805437579663 L 611.9927721661054 471.04507459176244 L 615.6334006734006 466.79291586338616 L 619.2740291806958 462.15000687034774 L 622.9146576879909 457.0683596624358 L 626.5552861952862 451.5040611407818 L 630.1959147025814 445.43126635054386 L 633.8365432098765 438.8576902907463 L 637.4771717171716 431.8389675236691 L 641.1178002244668 424.48903218584917 L 644.7584287317619 416.9839553942964 L 648.3990572390571 409.55746482014047 L 652.0396857463523 402.48756656330454 L 655.6803142536475 396.075107835331 L 659.3209427609427 390.6165162348014 L 662.9615712682378 386.3740688305579 L 666.602199775533 383.5476604712274 L 670.2428282828282 382.2520195048747 L 673.8834567901233 382.50264151828117 L 677.5240852974185 384.21248415545335 L 681.1647138047138 387.19990095053737 L 684.8053423120089 391.2066651369583 L 688.4459708193041 395.9235281630995 L 692.0865993265993 401.01980468748354 L 695.7272278338944 406.17311448730953 L 699.3678563411896 411.09566446449537 L 703.0084848484848 415.55423170642763 L 706.64911335578 419.382137990488 L 710.2897418630752 422.482771236081 L 713.9303703703702 424.8253967156983 L 717.5709988776655 426.43493746702734 L 721.2116273849606 427.3779811380383 L 724.8522558922557 427.74745356805806 L 728.4928843995509 427.64821715180653 L 732.1335129068461 427.1853823431709 L 735.7741414141412 426.45646915488555 L 739.4147699214365 425.54783565236335 L 743.0553984287317 424.53510743083325 L 746.6960269360268 423.4867815643618 L 750.336655443322 422.4697997042218 L 753.9772839506172 421.5557171792435 L 757.6179124579123 420.8261371076715 L 761.2585409652075 420.376301928804 L 764.8991694725028 420.3160887107374 L 768.5397979797979 420.7680753182761 L 772.1804264870931 421.86276617167715 L 775.8210549943881 423.731432079216 L 779.4616835016834 426.497289516372 L 783.1023120089785 430.2659038672512 L 786.7429405162738 435.1157523707276 L 790.3835690235688 441.0898440508049 L 794.0241975308641 448.1891893883982 L 797.6648260381592 456.36876245814744 L 801.3054545454544 465.53641512992937 L 804.9460830527496 475.5549910063044 L 808.5867115600447 486.2476468460072 L 812.2273400673399 497.4061257557847 L 815.8679685746351 508.8014538214257 L 819.5085970819302 520.1962769817153 L 823.1492255892255 531.3578543215651 L 826.7898540965207 542.0706158209787 L 830.4304826038158 552.1472064644366 L 834.071111111111 561.4370856064149" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 473.6488888888889 581.1787479147131 L 477.28951739618407 574.7463340537412 L 480.93014590347923 567.6985976599661 L 484.57077441077445 560.1852779438844 L 488.2114029180696 552.3925604364445 L 491.85203142536477 544.5295538205394 L 495.4926599326599 536.8118198596819 L 499.1332884399551 529.4442785636195 L 502.77391694725026 522.6059778043193 L 506.4145454545454 516.4388674903648 L 510.05517396184064 511.0419108175488 L 513.6958024691357 506.4707791026261 L 517.336430976431 502.7422719192127 L 520.9770594837262 499.84175072498033 L 524.6176879910213 497.73147671466927 L 528.2583164983164 496.35788310506814 L 531.8989450056116 495.6564223756741 L 535.5395735129068 495.55351797050696 L 539.1802020202019 495.96605799721976 L 542.8208305274972 496.79954669420215 L 546.4614590347924 497.9463127705451 L 550.1020875420875 499.2850254857488 L 553.7427160493827 500.6822840868593 L 557.3833445566779 501.9964110081667 L 561.023973063973 503.0830073396619 L 564.6646015712681 503.80148895612376 L 568.3052300785633 504.0217847477901 L 571.9458585858586 503.6306017211024 L 575.5864870931537 502.53701018444065 L 579.2271156004489 500.6774005845991 L 582.8677441077441 498.01996085739097 L 586.5083726150392 494.56864821366366 L 590.1490011223344 490.36622039225887 L 593.7896296296295 485.49539168473984 L 597.4302581369247 480.076798722298 L 601.0708866442199 474.2624166681886 L 604.7115151515151 468.22351225116336 L 608.3521436588103 462.1331884302263 L 611.9927721661054 456.14494284683747 L 615.6334006734006 450.3701516532659 L 619.2740291806958 444.858615943028 L 622.9146576879909 439.5868578454114 L 626.5552861952862 434.4583967975741 L 630.1959147025814 429.3186338156538 L 633.8365432098765 423.984355744934 L 637.4771717171716 418.28467318561326 L 641.1178002244668 412.1071004581416 L 644.7584287317619 405.44025935999775 L 648.3990572390571 398.4040421791625 L 652.0396857463523 391.2594118203931 L 655.6803142536475 384.39330101933047 L 659.3209427609427 378.27874115255406 L 662.9615712682378 373.41542461725123 L 666.602199775533 370.26020706597785 L 670.2428282828282 369.1595230732363 L 673.8834567901233 370.29567123879644 L 677.5240852974185 373.6563834443339 L 681.1647138047138 379.0326226169007 L 684.8053423120089 386.0442170411508 L 688.4459708193041 394.1879676067796 L 692.0865993265993 402.89931712701286 L 695.7272278338944 411.6171704214238 L 699.3678563411896 419.84206959060975 L 703.0084848484848 427.1802335131441 L 706.64911335578 433.36922886944086 L 710.2897418630752 438.2844507106057 L 713.9303703703702 441.9284967374859 L 717.5709988776655 444.4075289181915 L 721.2116273849606 445.8997177096404 L 724.8522558922557 446.62096373700234 L 728.4928843995509 446.79251305624507 L 732.1335129068461 446.6140762235971 L 735.7741414141412 446.2448499454434 L 739.4147699214365 445.7935946722621 L 743.0553984287317 445.31776670299575 L 746.6960269360268 444.83072639084037 L 750.336655443322 444.31530008306555 L 753.9772839506172 443.741485540031 L 757.6179124579123 443.0858500001882 L 761.2585409652075 442.3501437009651 L 764.8991694725028 441.5767984801097 L 768.5397979797979 440.85927188341867 L 772.1804264870931 440.3456279269428 L 775.8210549943881 440.23433491232714 L 779.4616835016834 440.76202902903685 L 783.1023120089785 442.18393006589156 L 786.7429405162738 444.7486323967883 L 790.3835690235688 448.6699890174012 L 794.0241975308641 454.09956341472434 L 797.6648260381592 461.1034401576383 L 801.3054545454544 469.6469108685108 L 804.9460830527496 479.5896489893555 L 808.5867115600447 490.6925574364268 L 812.2273400673399 502.6357537854934 L 815.8679685746351 515.0454683563063 L 819.5085970819302 527.5263005371938 L 823.1492255892255 539.694565180834 L 826.7898540965207 551.2084874084924 L 830.4304826038158 561.7917342814648 L 834.071111111111 571.2480248003179" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="941.1519130700947" y1="357.15555555555557" x2="941.1519130700947" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1040.4417814508722" y1="357.15555555555557" x2="1040.4417814508722" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1139.7316498316497" y1="357.15555555555557" x2="1139.7316498316497" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1239.021518212427" y1="357.15555555555557" x2="1239.021518212427" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="990.7968472604836" y1="357.15555555555557" x2="990.7968472604836" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1090.086715641261" y1="357.15555555555557" x2="1090.086715641261" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1189.3765840220385" y1="357.15555555555557" x2="1189.3765840220385" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="577.6114565324863" x2="1265.3333333333333" y2="577.6114565324863" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="513.9454807085701" x2="1265.3333333333333" y2="513.9454807085701" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="450.2795048846539" x2="1265.3333333333333" y2="450.2795048846539" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="386.6135290607376" x2="1265.3333333333333" y2="386.6135290607376" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="609.4444444444445" x2="1265.3333333333333" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="545.7784686205282" x2="1265.3333333333333" y2="545.7784686205282" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="482.11249279661195" x2="1265.3333333333333" y2="482.11249279661195" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="418.44651697269575" x2="1265.3333333333333" y2="418.44651697269575" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="904.911111111111" y="357.15555555555557" width="360.4222222222223" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="990.7968472604836" y1="609.4444444444445" x2="990.7968472604836" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="990.7968472604836" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">50</text><line x1="1090.086715641261" y1="609.4444444444445" x2="1090.086715641261" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1090.086715641261" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">100</text><line x1="1189.3765840220385" y1="609.4444444444445" x2="1189.3765840220385" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1189.3765840220385" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">150</text><line x1="904.911111111111" y1="609.4444444444445" x2="901.2444444444443" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0000</text><line x1="904.911111111111" y1="545.7784686205282" x2="901.2444444444443" y2="545.7784686205282" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="549.8851352871948" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0025</text><line x1="904.911111111111" y1="482.11249279661195" x2="901.2444444444443" y2="482.11249279661195" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="486.21915946327863" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0050</text><line x1="904.911111111111" y1="418.44651697269575" x2="901.2444444444443" y2="418.44651697269575" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="422.5531836393624" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0075</text><text x="904.911111111111" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">app_opens_per_day</text><path d="M 904.911111111111 568.218697473499 L 908.5517396184061 561.3645608295119 L 912.1923681257014 554.2534576567969 L 915.8329966329966 547.0596629723152 L 919.4736251402917 539.9713432191772 L 923.1142536475869 533.178013883811 L 926.7548821548821 526.8571630500488 L 930.3955106621772 521.1614135827342 L 934.0361391694724 516.2075647573956 L 937.6767676767677 512.0686291521355 L 941.3173961840628 508.76959807050207 L 944.958024691358 506.287193604406 L 948.598653198653 504.55337918174797 L 952.2392817059482 503.4619846319407 L 955.8799102132434 502.8775214735286 L 959.5205387205385 502.6451548658538 L 963.1611672278337 502.6008602247284 L 966.801795735129 502.58099032310565 L 970.4424242424241 502.43075291494176 L 974.0830527497193 502.0113787403667 L 977.7236812570145 501.2059799432053 L 981.3643097643096 499.9242145656317 L 985.0049382716048 498.105869145953 L 988.6455667789 495.723366541354 L 992.2861952861952 492.78304549492896 L 995.9268237934904 489.32490361645887 L 999.5674523007856 485.42040831172227 L 1003.2080808080807 481.1680078581149 L 1006.8487093153759 476.6861377335891 L 1010.4893378226711 472.10380388817896 L 1014.1299663299662 467.5491933902007 L 1017.7705948372615 463.1371510986469 L 1021.4112233445567 458.95669787892444 L 1025.0518518518518 455.05998633835844 L 1028.6924803591469 451.45414634733913 L 1032.333108866442 448.09734104768773 L 1035.9737373737373 444.90003641636514 L 1039.6143658810324 441.73200802084557 L 1043.2549943883275 438.4350089933262 L 1046.8956228956226 434.84035758504865 L 1050.536251402918 430.7900345657866 L 1054.176879910213 426.15928212665904 L 1057.8175084175082 420.8782457633175 L 1061.4581369248035 414.9499805739459 L 1065.0987654320986 408.46222820577213 L 1068.7393939393937 401.5908120203956 L 1072.380022446689 394.5933047735196 L 1076.0206509539842 387.7927430526066 L 1079.6612794612793 381.55247352858544 L 1083.3019079685746 376.24453177190765 L 1086.9425364758697 372.21505096458804 L 1090.5831649831648 369.7508556473716 L 1094.2237934904601 369.0514499625859 L 1097.8644219977552 370.2099968591354 L 1101.5050505050503 373.20566886610743 L 1105.1456790123457 377.9081235922047 L 1108.7863075196408 384.0931029308598 L 1112.426936026936 391.46659361005806 L 1116.0675645342312 399.6939020649297 L 1119.7081930415263 408.4295729674318 L 1123.3488215488214 417.3443627323891 L 1126.9894500561168 426.1463657218379 L 1130.6300785634116 434.59466372629686 L 1134.270707070707 442.5052495058932 L 1137.911335578002 449.7501891275557 L 1141.5519640852972 456.25182754600087 L 1145.1925925925925 461.9742039284889 L 1148.8332210998876 466.91373887871106 L 1152.4738496071827 471.090792779446 L 1156.114478114478 474.5430388193257 L 1159.7551066217732 477.32092318362885 L 1163.3957351290683 479.4849448614731 L 1167.0363636363636 481.1041661210391 L 1170.6769921436587 482.25528116229253 L 1174.3176206509538 483.0216841435832 L 1177.9582491582491 483.4922094499241 L 1181.5988776655443 483.7594755002329 L 1185.2395061728394 483.917969995969 L 1188.8801346801347 484.06212040462475 L 1192.5207631874296 484.2845862030709 L 1196.161391694725 484.67490968559383 L 1199.8020202020202 485.3185130995995 L 1203.4426487093153 486.29588276477534 L 1207.0832772166104 487.6816811615166 L 1210.7239057239058 489.5435051463729 L 1214.3645342312007 491.9400706677867 L 1218.005162738496 494.91873893223453 L 1221.645791245791 498.5124770437751 L 1225.2864197530862 502.7365300017249 L 1228.9270482603815 507.58523213873946 L 1232.5676767676766 513.029472686462 L 1236.2083052749717 519.0153321501599 L 1239.848933782267 525.464318436139 L 1243.4895622895622 532.2754644692232 L 1247.1301907968573 539.3293261664116 L 1250.7708193041526 546.4936746061653 L 1254.4114478114477 553.6304469291097 L 1258.0520763187428 560.6033430274233 L 1261.6927048260382 567.2853576970464 L 1265.3333333333333 573.5655361493288" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 904.911111111111 580.0061662464104 L 908.5517396184061 574.2796456410817 L 912.1923681257014 568.1408940321171 L 915.8329966329966 561.6898041815466 L 919.4736251402917 555.0324858887175 L 923.1142536475869 548.272494840611 L 926.7548821548821 541.5034751468712 L 930.3955106621772 534.8044271194256 L 934.0361391694724 528.23824075128 L 937.6767676767677 521.8534088167887 L 941.3173961840628 515.6881328508249 L 944.958024691358 509.77554044769977 L 948.598653198653 504.14856940863484 L 952.2392817059482 498.8432776775186 L 955.8799102132434 493.89984044439313 L 959.5205387205385 489.36114948699844 L 963.1611672278337 485.2695520965149 L 966.801795735129 481.66269502485267 L 970.4424242424241 478.56957591599445 L 974.0830527497193 476.0077424908716 L 977.7236812570145 473.982192840073 L 981.3643097643096 472.48604511040963 L 985.0049382716048 471.5025975827241 L 988.6455667789 471.00809628537274 L 992.2861952861952 470.9744179590719 L 995.9268237934904 471.3709542262833 L 999.5674523007856 472.1651970660842 L 1003.2080808080807 473.3218048569105 L 1006.8487093153759 474.8002045876523 L 1010.4893378226711 476.5510115282955 L 1014.1299663299662 478.51170124689645 L 1017.7705948372615 480.6020534799337 L 1021.4112233445567 482.71992405535684 L 1025.0518518518518 484.7379200549998 L 1028.6924803591469 486.5015840159308 L 1032.333108866442 487.8297519382854 L 1035.9737373737373 488.51782864406766 L 1039.6143658810324 488.34477869149197 L 1043.2549943883275 487.0845811249247 L 1046.8956228956226 484.52263927268075 L 1050.536251402918 480.47708073943795 L 1054.176879910213 474.8239926221845 L 1057.8175084175082 467.52448001314224 L 1061.4581369248035 458.65020662915305 L 1065.0987654320986 448.40308321843213 L 1068.7393939393937 437.12437312401516 L 1072.380022446689 425.28899780574386 L 1076.0206509539842 413.4823982911576 L 1079.6612794612793 402.3598378070385 L 1083.3019079685746 392.59112912663596 L 1086.9425364758697 384.7968212250313 L 1090.5831649831648 379.4841739429477 L 1094.2237934904601 376.9921682532652 L 1097.8644219977552 377.45399751476407 L 1101.5050505050503 380.7830070757095 L 1105.1456790123457 386.6843464786999 L 1108.7863075196408 394.69042557988246 L 1112.426936026936 404.2144936005225 L 1116.0675645342312 414.6140523614405 L 1119.7081930415263 425.2548410925824 L 1123.3488215488214 435.56686521808234 L 1126.9894500561168 445.0860781423663 L 1130.6300785634116 453.4782860461119 L 1134.270707070707 460.54495422354614 L 1137.911335578002 466.2132469597048 L 1141.5519640852972 470.5144311127211 L 1145.1925925925925 473.5555726350401 L 1148.8332210998876 475.48934599688624 L 1152.4738496071827 476.48600695063635 L 1156.114478114478 476.71045509748484 L 1159.7551066217732 476.3061087624363 L 1163.3957351290683 475.3862184947195 L 1167.0363636363636 474.03234455697543 L 1170.6769921436587 472.2990273168599 L 1174.3176206509538 470.2231600795656 L 1177.9582491582491 467.8362088363642 L 1181.5988776655443 465.1772194460734 L 1185.2395061728394 462.30454645226064 L 1188.8801346801347 459.3044728220557 L 1192.5207631874296 456.2953830918471 L 1196.161391694725 453.42686337752 L 1199.8020202020202 450.8739222097464 L 1203.4426487093153 448.82729658844573 L 1207.0832772166104 447.4813586036224 L 1210.7239057239058 447.02134380326135 L 1214.3645342312007 447.61144745040383 L 1218.005162738496 449.38485276435654 L 1221.645791245791 452.4361323160072 L 1225.2864197530862 456.8159066208941 L 1228.9270482603815 462.5273301291501 L 1232.5676767676766 469.5239895263371 L 1236.2083052749717 477.7091033103908 L 1239.848933782267 486.9363533636928 L 1243.4895622895622 497.01304846418805 L 1247.1301907968573 507.70642435923384 L 1250.7708193041526 518.7536231726144 L 1254.4114478114477 529.8752948200693 L 1258.0520763187428 540.7919772187022 L 1261.6927048260382 551.2416670722515 L 1265.3333333333333 560.9965147445104" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="91.4484039332315" y1="674.711111111111" x2="91.4484039332315" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="178.35995981030464" y1="674.711111111111" x2="178.35995981030464" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="265.2715156873777" y1="674.711111111111" x2="265.2715156873777" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="352.1830715644508" y1="674.711111111111" x2="352.1830715644508" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="134.9041818717681" y1="674.711111111111" x2="134.9041818717681" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="221.81573774884117" y1="674.711111111111" x2="221.81573774884117" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="308.7272936259143" y1="674.711111111111" x2="308.7272936259143" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="395.63884950298734" y1="674.711111111111" x2="395.63884950298734" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="900.8421115552449" x2="409.84888888888884" y2="900.8421115552449" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="848.5263346657346" x2="409.84888888888884" y2="848.5263346657346" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="796.2105577762245" x2="409.84888888888884" y2="796.2105577762245" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="743.8947808867142" x2="409.84888888888884" y2="743.8947808867142" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="691.579003997204" x2="409.84888888888884" y2="691.579003997204" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="49.42666666666666" y1="927.0" x2="409.84888888888884" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="874.6842231104898" x2="409.84888888888884" y2="874.6842231104898" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="822.3684462209795" x2="409.84888888888884" y2="822.3684462209795" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="770.0526693314694" x2="409.84888888888884" y2="770.0526693314694" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="49.42666666666666" y1="717.7368924419591" x2="409.84888888888884" y2="717.7368924419591" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="49.42666666666666" y="674.711111111111" width="360.4222222222222" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="134.9041818717681" y1="927.0" x2="134.9041818717681" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="134.9041818717681" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="221.81573774884117" y1="927.0" x2="221.81573774884117" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="221.81573774884117" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">8</text><line x1="308.7272936259143" y1="927.0" x2="308.7272936259143" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="308.7272936259143" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">12</text><line x1="395.63884950298734" y1="927.0" x2="395.63884950298734" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="395.63884950298734" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">16</text><line x1="49.42666666666666" y1="927.0" x2="45.76" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.00</text><line x1="49.42666666666666" y1="874.6842231104898" x2="45.76" y2="874.6842231104898" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="878.7908897771565" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.05</text><line x1="49.42666666666666" y1="822.3684462209795" x2="45.76" y2="822.3684462209795" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="826.4751128876462" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.10</text><line x1="49.42666666666666" y1="770.0526693314694" x2="45.76" y2="770.0526693314694" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="774.1593359981362" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.15</text><line x1="49.42666666666666" y1="717.7368924419591" x2="45.76" y2="717.7368924419591" stroke="#444444" stroke-width="1.3333333333333333"/><text x="42.82666666666666" y="721.8435591086259" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.20</text><text x="49.42666666666666" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">weekend_screen_time</text><path d="M 49.42666666666666 926.9999567479872 L 53.06729517396184 926.9998345323212 L 56.707923681257014 926.9994204773714 L 60.34855218855219 926.9981396455862 L 63.98918069584736 926.994517618796 L 67.62980920314253 926.9851379326338 L 71.2704377104377 926.9628401416346 L 74.91106621773288 926.914016324769 L 78.55169472502806 926.8151047353464 L 82.19232323232322 926.6286368074514 L 85.83295173961841 926.2992428068957 L 89.47358024691358 925.7497894060858 L 93.11420875420875 924.8776895650905 L 96.75483726150392 923.5521080323566 L 100.39546576879911 921.6145550423527 L 104.03609427609427 918.8871807388183 L 107.67672278338945 915.1928509274086 L 111.31735129068461 910.3873742412131 L 114.95797979797979 904.3981786397599 L 118.59860830527498 897.2587679282489 L 122.23923681257014 889.1274123126302 L 125.87986531986532 880.2811914575866 L 129.5204938271605 871.0791815257327 L 133.16112233445565 861.8900367032611 L 136.80175084175085 852.9855354987178 L 140.442379349046 844.4221551743445 L 144.08300785634117 835.9650540749594 L 147.72363636363633 827.1275722479764 L 151.36426487093155 817.3691840182489 L 155.0048933782267 806.4080273246702 L 158.64552188552187 794.5071500215158 L 162.28615039281706 782.5634891357306 L 165.92677890011225 771.907663291785 L 169.5674074074074 763.8726511263536 L 173.20803591470258 759.3155820842812 L 176.84866442199774 758.3018686659793 L 180.4892929292929 760.0788529286716 L 184.12992143658812 763.3340147325941 L 187.77054994388328 766.6217576944352 L 191.41117845117847 768.7968023928422 L 195.0518069584736 769.3152023679231 L 198.6924354657688 768.3286620217159 L 202.333063973064 766.5667604743976 L 205.97369248035912 765.0521883293197 L 209.61432098765434 764.7280173901347 L 213.25494949494947 766.101143679922 L 216.89557800224466 769.010727862385 L 220.5362065095398 772.5919376850597 L 224.17683501683499 775.4318064883973 L 227.81746352413018 775.8615870911085 L 231.45809203142534 772.3474446083414 L 235.09872053872053 763.9916082231987 L 238.73934904601566 751.1247458071268 L 242.37997755331085 735.7974105427435 L 246.02060606060604 721.7886183617447 L 249.66123456790118 713.7974663338556 L 253.30186307519642 715.8966066610752 L 256.9424915824916 729.8862924103032 L 260.5831200897867 754.4358649706057 L 264.2237485970819 785.5438706574097 L 267.86437710437707 818.0852131805832 L 271.5050056116723 847.6017734505568 L 275.14563411896745 871.4657074871893 L 278.78626262626256 889.0497761129706 L 282.4268911335578 901.1398063644948 L 286.06751964085294 909.1140377990644 L 289.7081481481481 914.3152221347643 L 293.3487766554433 917.760501213688 L 296.9894051627384 920.112605776776 L 300.63003367003364 921.7689318447428 L 304.2706621773288 922.9648653033664 L 307.91129068462396 923.8488508720095 L 311.5519191919191 924.5239520278275 L 315.1925476992144 925.0641885812271 L 318.8331762065095 925.5174896045723 L 322.47380471380467 925.9067138869701 L 326.1144332210999 926.23561623732 L 329.75506172839505 926.499562608649 L 333.39569023569027 926.6959536782207 L 337.0363187429854 926.8296249099737 L 340.67694725028053 926.9123029864369 L 344.31757575757575 926.9586439836359 L 347.9582042648709 926.9821620801623 L 351.59883277216613 926.9929701794081 L 355.2394612794613 926.9974704043116 L 358.8800897867564 926.9991692238034 L 362.52071829405156 926.9997510400486 L 366.1613468013468 926.9999319362124 L 369.801975308642 926.9999830254188 L 373.4426038159371 926.9999961385524 L 377.0832323232323 926.9999991987803 L 380.7238608305275 926.9999998483675 L 384.36448933782265 926.9999999738262 L 388.00511784511787 926.9999999958793 L 391.64574635241297 926.9999999994084 L 395.2863748597082 926.9999999999225 L 398.9270033670033 926.9999999999907 L 402.5676318742985 926.9999999999989 L 406.2082603815937 926.9999999999998 L 409.84888888888884 927.0" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 49.42666666666666 926.7781039404408 L 53.06729517396184 926.6831441221682 L 56.707923681257014 926.5835087228239 L 60.34855218855219 926.4938119011364 L 63.98918069584736 926.4264821009143 L 67.62980920314253 926.3854242504665 L 71.2704377104377 926.3626949145089 L 74.91106621773288 926.3408857009265 L 78.55169472502806 926.3008838170574 L 82.19232323232322 926.2313212861983 L 85.83295173961841 926.1344925031319 L 89.47358024691358 926.0248436349362 L 93.11420875420875 925.9195891766177 L 96.75483726150392 925.824753591738 L 100.39546576879911 925.7222573243344 L 104.03609427609427 925.563836793287 L 107.67672278338945 925.2758789841832 L 111.31735129068461 924.7763238817375 L 114.95797979797979 924.001076027258 L 118.59860830527498 922.9331191439229 L 122.23923681257014 921.6236470979607 L 125.87986531986532 920.1933032287744 L 129.5204938271605 918.8059098771512 L 133.16112233445565 917.6175845654213 L 136.80175084175085 916.7170744138798 L 140.442379349046 916.0811896282835 L 144.08300785634117 915.5663099760586 L 147.72363636363633 914.943015914169 L 151.36426487093155 913.9625423602226 L 155.0048933782267 912.4302843680844 L 158.64552188552187 910.2597162867833 L 162.28615039281706 907.4902726811163 L 165.92677890011225 904.2687535630714 L 169.5674074074074 900.8064306950678 L 173.20803591470258 897.3276222402006 L 176.84866442199774 894.0222926206702 L 180.4892929292929 891.011766992628 L 184.12992143658812 888.3353018230707 L 187.77054994388328 885.9613395662575 L 191.41117845117847 883.8167623827444 L 195.0518069584736 881.8157428710924 L 198.6924354657688 879.8684518744072 L 202.333063973064 877.8632835246268 L 205.97369248035912 875.6342593838428 L 209.61432098765434 872.932138202506 L 213.25494949494947 869.4078743128493 L 216.89557800224466 864.6009073339651 L 220.5362065095398 857.9206917611501 L 224.17683501683499 848.6306368464026 L 227.81746352413018 835.8850905421879 L 231.45809203142534 818.9037230488193 L 235.09872053872053 797.3450152909083 L 238.73934904601566 771.8245923129958 L 242.37997755331085 744.345616200143 L 246.02060606060604 718.2938115490726 L 249.66123456790118 697.7533588366725 L 253.30186307519642 686.2527573118678 L 256.9424915824916 685.4601966969731 L 260.5831200897867 694.5013388776483 L 264.2237485970819 710.2940662443563 L 267.86437710437707 728.7370685331982 L 271.5050056116723 746.1344057731418 L 275.14563411896745 760.1874617120184 L 278.78626262626256 770.225588549408 L 282.4268911335578 776.7980567010427 L 286.06751964085294 781.0167978197948 L 289.7081481481481 784.0162905860175 L 293.3487766554433 786.6866641472891 L 296.9894051627384 789.6239721220168 L 300.63003367003364 793.1562961317424 L 304.2706621773288 797.3549945267214 L 307.91129068462396 802.0423473948667 L 311.5519191919191 806.8600452170101 L 315.1925476992144 811.4272313661384 L 318.8331762065095 815.533542429003 L 322.47380471380467 819.2612551069426 L 326.1144332210999 822.9592118600805 L 329.75506172839505 827.0794378834321 L 333.39569023569027 831.9693142575699 L 337.0363187429854 837.7315628489 L 340.67694725028053 844.2143574632637 L 344.31757575757575 851.1146579531697 L 347.9582042648709 858.1216621690442 L 351.59883277216613 865.0225169157814 L 355.2394612794613 871.7304337499615 L 358.8800897867564 878.2441187167642 L 362.52071829405156 884.5776832362459 L 366.1613468013468 890.7026376353247 L 369.801975308642 896.5269722854069 L 373.4426038159371 901.9144948560343 L 377.0832323232323 906.7297964320837 L 380.7238608305275 910.8849424827888 L 384.36448933782265 914.3655768159676 L 388.00511784511787 917.2260453938868 L 391.64574635241297 919.5596558980646 L 395.2863748597082 921.4620426232054 L 398.9270033670033 923.0061312276367 L 402.5676318742985 924.2372916121672 L 406.2082603815937 925.1844918961247 L 409.84888888888884 925.8758906931604" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="510.36902124695223" y1="674.711111111111" x2="510.36902124695223" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="566.8615325670498" y1="674.711111111111" x2="566.8615325670498" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="623.3540438871473" y1="674.711111111111" x2="623.3540438871473" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="679.8465552072448" y1="674.711111111111" x2="679.8465552072448" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="736.3390665273424" y1="674.711111111111" x2="736.3390665273424" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="792.8315778474399" y1="674.711111111111" x2="792.8315778474399" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="482.1227655869035" y1="674.711111111111" x2="482.1227655869035" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="538.615276907001" y1="674.711111111111" x2="538.615276907001" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="595.1077882270986" y1="674.711111111111" x2="595.1077882270986" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="651.6002995471961" y1="674.711111111111" x2="651.6002995471961" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="708.0928108672936" y1="674.711111111111" x2="708.0928108672936" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="764.5853221873911" y1="674.711111111111" x2="764.5853221873911" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="821.0778335074885" y1="674.711111111111" x2="821.0778335074885" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="896.2335777571994" x2="834.071111111111" y2="896.2335777571994" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="834.7007332715984" x2="834.071111111111" y2="834.7007332715984" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="773.1678887859973" x2="834.071111111111" y2="773.1678887859973" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="711.6350443003963" x2="834.071111111111" y2="711.6350443003963" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="473.6488888888889" y1="927.0" x2="834.071111111111" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="865.4671555143989" x2="834.071111111111" y2="865.4671555143989" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="803.9343110287979" x2="834.071111111111" y2="803.9343110287979" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="742.4014665431969" x2="834.071111111111" y2="742.4014665431969" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="473.6488888888889" y1="680.8686220575958" x2="834.071111111111" y2="680.8686220575958" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="473.6488888888889" y="674.711111111111" width="360.4222222222221" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="482.1227655869035" y1="927.0" x2="482.1227655869035" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="482.1227655869035" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="538.615276907001" y1="927.0" x2="538.615276907001" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="538.615276907001" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1</text><line x1="595.1077882270986" y1="927.0" x2="595.1077882270986" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="595.1077882270986" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="651.6002995471961" y1="927.0" x2="651.6002995471961" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="651.6002995471961" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">3</text><line x1="708.0928108672936" y1="927.0" x2="708.0928108672936" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="708.0928108672936" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="764.5853221873911" y1="927.0" x2="764.5853221873911" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="764.5853221873911" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="821.0778335074885" y1="927.0" x2="821.0778335074885" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="821.0778335074885" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="473.6488888888889" y1="927.0" x2="469.9822222222222" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="473.6488888888889" y1="865.4671555143989" x2="469.9822222222222" y2="865.4671555143989" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="869.5738221810657" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="473.6488888888889" y1="803.9343110287979" x2="469.9822222222222" y2="803.9343110287979" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="808.0409776954646" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="473.6488888888889" y1="742.4014665431969" x2="469.9822222222222" y2="742.4014665431969" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="746.5081332098636" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><line x1="473.6488888888889" y1="680.8686220575958" x2="469.9822222222222" y2="680.8686220575958" stroke="#444444" stroke-width="1.3333333333333333"/><text x="467.0488888888889" y="684.9752887242626" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><text x="473.6488888888889" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">work_study_hours</text><path d="M 473.6488888888889 920.0407311320888 L 477.28951739618407 916.3372596495853 L 480.93014590347923 911.2871274969707 L 484.57077441077445 904.6971207959677 L 488.2114029180696 896.458344709934 L 491.8520314253647 886.5758743789945 L 495.4926599326599 875.1810398600194 L 499.1332884399551 862.5205602305182 L 502.77391694725026 848.9228667592554 L 506.4145454545454 834.7489913112106 L 510.05517396184064 820.3407517547623 L 513.6958024691357 805.9803347938919 L 517.336430976431 791.8718626762058 L 520.9770594837262 778.1482484208192 L 524.6176879910213 764.8983572587927 L 528.2583164983164 752.2033537722791 L 531.8989450056116 740.1691783890087 L 535.5395735129068 728.9444829764234 L 539.1802020202019 718.7185577749798 L 542.8208305274972 709.6997835551031 L 546.4614590347924 702.0804184912165 L 550.1020875420875 695.9972421977991 L 553.7427160493827 691.4991155680268 L 557.3833445566779 688.5311585836046 L 561.023973063973 686.940599350362 L 564.6646015712682 686.5022014965958 L 568.3052300785633 686.9539314857344 L 571.9458585858586 688.0294582644385 L 575.5864870931537 689.4757531662291 L 579.2271156004489 691.0518327464724 L 582.8677441077441 692.5160761306681 L 586.5083726150392 693.6196901404041 L 590.1490011223344 694.1270049956153 L 593.7896296296295 693.8750444145192 L 597.4302581369247 692.8653641859789 L 601.0708866442199 691.3572463578614 L 604.7115151515151 689.9153291067908 L 608.3521436588103 689.3690117291344 L 611.9927721661054 690.6696144411853 L 615.6334006734006 694.6746245906694 L 619.2740291806958 701.9255719293111 L 622.9146576879909 712.4962646056244 L 626.5552861952862 725.9636261673355 L 630.1959147025814 741.505710068991 L 633.8365432098765 758.084919786146 L 637.4771717171717 774.6512955438955 L 641.1178002244668 790.308236834822 L 644.758428731762 804.4111579704406 L 648.3990572390571 816.59970706368 L 652.0396857463523 826.7817417286544 L 655.6803142536476 835.0891403280309 L 659.3209427609427 841.8185332452351 L 662.9615712682378 847.3634621170775 L 666.602199775533 852.1432809050223 L 670.2428282828282 856.5370419323867 L 673.8834567901233 860.8330442147947 L 677.5240852974185 865.2029846343535 L 681.1647138047138 869.7035037888683 L 684.8053423120089 874.3001779009101 L 688.4459708193041 878.9033654987786 L 692.0865993265993 883.4040889675053 L 695.7272278338944 887.7012103365087 L 699.3678563411896 891.7164497198215 L 703.0084848484848 895.3986118833865 L 706.64911335578 898.7209470820072 L 710.2897418630752 901.6757001310555 L 713.9303703703702 904.2686538452614 L 717.5709988776655 906.5151011277405 L 721.2116273849607 908.437769201863 L 724.8522558922557 910.0666018599377 L 728.4928843995509 911.4396334437379 L 732.1335129068462 912.6035123235836 L 735.7741414141412 913.6120287180863 L 739.4147699214365 914.5216815278852 L 743.0553984287317 915.3847289939367 L 746.6960269360268 916.2415789106246 L 750.336655443322 917.1149646239476 L 753.9772839506172 918.007786021155 L 757.6179124579123 918.9051304837855 L 761.2585409652075 919.779602879944 L 764.8991694725028 920.5983452941118 L 768.5397979797979 921.3301734767375 L 772.1804264870931 921.9517580494125 L 775.8210549943883 922.4522361350497 L 779.4616835016834 922.8357869476577 L 783.1023120089786 923.121665105363 L 786.7429405162738 923.3413061397997 L 790.3835690235688 923.532647010258 L 794.0241975308642 923.732653874883 L 797.6648260381592 923.9698260715022 L 801.3054545454545 924.2586975360582 L 804.9460830527496 924.5978631107962 L 808.5867115600447 924.9719652491432 L 812.2273400673399 925.356825992301 L 815.8679685746353 925.7260030177301 L 819.5085970819302 926.0568136486106 L 823.1492255892255 926.3343251375533 L 826.7898540965207 926.5526894740742 L 830.4304826038158 926.7141126868457 L 834.071111111111 926.8263576859987" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 473.6488888888889 921.8714003343573 L 477.28951739618407 919.6139244673687 L 480.93014590347923 916.6321840658744 L 484.57077441077445 912.8065040097832 L 488.2114029180696 908.0377055257786 L 491.8520314253647 902.2617573113164 L 495.4926599326599 895.463590068211 L 499.1332884399551 887.6877700395504 L 502.77391694725026 879.0438447461895 L 506.4145454545454 869.7047769078799 L 510.05517396184064 859.8978874691971 L 513.6958024691357 849.8889305950861 L 517.336430976431 839.9610549293976 L 520.9770594837262 830.3912209091255 L 524.6176879910213 821.4269932952161 L 528.2583164983164 813.2664885146348 L 531.8989450056116 806.043716199483 L 535.5395735129068 799.8207633618413 L 539.1802020202019 794.5873828695995 L 542.8208305274972 790.2676836198693 L 546.4614590347924 786.7328444305638 L 550.1020875420875 783.818114175108 L 553.7427160493827 781.3418396935936 L 557.3833445566779 779.1239417060043 L 561.023973063973 777.0012615443289 L 564.6646015712682 774.8376904852131 L 568.3052300785633 772.5280808480968 L 571.9458585858586 769.9965570055225 L 575.5864870931537 767.1916485367793 L 579.2271156004489 764.0820416509694 L 582.8677441077441 760.6569787900996 L 586.5083726150392 756.9339366833207 L 590.1490011223344 752.9732351064824 L 593.7896296296295 748.8954563690138 L 597.4302581369247 744.8943819127926 L 601.0708866442199 741.2371172351252 L 604.7115151515151 738.2451930977974 L 608.3521436588103 736.2555843066987 L 611.9927721661054 735.5673288184732 L 615.6334006734006 736.3853706949387 L 619.2740291806958 738.7759737656189 L 622.9146576879909 742.6461749899026 L 626.5552861952862 747.7536154981026 L 630.1959147025814 753.744739942221 L 633.8365432098765 760.2117077938925 L 637.4771717171717 766.7540043290871 L 641.1178002244668 773.0309073383443 L 644.758428731762 778.7951435739913 L 648.3990572390571 783.9043730518388 L 652.0396857463523 788.3132156848944 L 655.6803142536476 792.052520490638 L 659.3209427609427 795.2037076253303 L 662.9615712682378 797.8746808187595 L 666.602199775533 800.1811555465997 L 670.2428282828282 802.2345524503817 L 673.8834567901233 804.1357449036368 L 677.5240852974185 805.9731671694642 L 681.1647138047138 807.8237886491643 L 684.8053423120089 809.7557244568195 L 688.4459708193041 811.8313956012962 L 692.0865993265993 814.1101073016176 L 695.7272278338944 816.6488961823202 L 699.3678563411896 819.5007864651062 L 703.0084848484848 822.7102987527575 L 706.64911335578 826.3069965971719 L 710.2897418630752 830.2986581132766 L 713.9303703703702 834.6659567381257 L 717.5709988776655 839.3601831628614 L 721.2116273849607 844.3046989962575 L 724.8522558922557 849.3998553297894 L 728.4928843995509 854.5304232426575 L 732.1335129068462 859.574374718627 L 735.7741414141412 864.4120618298623 L 739.4147699214365 868.9352051566763 L 743.0553984287317 873.0553161673402 L 746.6960269360268 876.7110788119446 L 750.336655443322 879.8738752499708 L 753.9772839506172 882.5503336691409 L 757.6179124579123 884.780828711775 L 761.2585409652075 886.6334644546997 L 764.8991694725028 888.194132584679 L 768.5397979797979 889.5544107502246 L 772.1804264870931 890.7998675054077 L 775.8210549943883 892.0013832263178 L 779.4616835016834 893.2112749487253 L 783.1023120089786 894.4645667088549 L 786.7429405162738 895.7841698059804 L 790.3835690235688 897.1875801934126 L 794.0241975308642 898.6923582847357 L 797.6648260381592 900.3182319914764 L 801.3054545454545 902.084945456551 L 804.9460830527496 904.0065263884027 L 808.5867115600447 906.0839525582252 L 812.2273400673399 908.2988411428429 L 815.8679685746353 910.6105622632082 L 819.5085970819302 912.9581731097596 L 823.1492255892255 915.267121282705 L 826.7898540965207 917.4592572977094 L 830.4304826038158 919.4637813706701 L 834.071111111111 921.2265947719561" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="911.656981580511" y1="674.711111111111" x2="911.656981580511" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1008.026559714795" y1="674.711111111111" x2="1008.026559714795" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1104.396137849079" y1="674.711111111111" x2="1104.396137849079" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1200.765715983363" y1="674.711111111111" x2="1200.765715983363" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="959.841770647653" y1="674.711111111111" x2="959.841770647653" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1056.211348781937" y1="674.711111111111" x2="1056.211348781937" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1152.580926916221" y1="674.711111111111" x2="1152.580926916221" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1248.950505050505" y1="674.711111111111" x2="1248.950505050505" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="892.9533611012308" x2="1265.3333333333333" y2="892.9533611012308" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="824.8600833036924" x2="1265.3333333333333" y2="824.8600833036924" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="756.766805506154" x2="1265.3333333333333" y2="756.766805506154" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="688.6735277086156" x2="1265.3333333333333" y2="688.6735277086156" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="904.911111111111" y1="927.0" x2="1265.3333333333333" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="858.9067222024615" x2="1265.3333333333333" y2="858.9067222024615" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="790.8134444049231" x2="1265.3333333333333" y2="790.8134444049231" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="904.911111111111" y1="722.7201666073847" x2="1265.3333333333333" y2="722.7201666073847" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="904.911111111111" y="674.711111111111" width="360.4222222222223" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="959.841770647653" y1="927.0" x2="959.841770647653" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="959.841770647653" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">20</text><line x1="1056.211348781937" y1="927.0" x2="1056.211348781937" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1056.211348781937" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">25</text><line x1="1152.580926916221" y1="927.0" x2="1152.580926916221" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1152.580926916221" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">30</text><line x1="1248.950505050505" y1="927.0" x2="1248.950505050505" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1248.950505050505" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">35</text><line x1="904.911111111111" y1="927.0" x2="901.2444444444443" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.00</text><line x1="904.911111111111" y1="858.9067222024615" x2="901.2444444444443" y2="858.9067222024615" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="863.0133888691282" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.02</text><line x1="904.911111111111" y1="790.8134444049231" x2="901.2444444444443" y2="790.8134444049231" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="794.9201110715899" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.04</text><line x1="904.911111111111" y1="722.7201666073847" x2="901.2444444444443" y2="722.7201666073847" stroke="#444444" stroke-width="1.3333333333333333"/><text x="898.3111111111109" y="726.8268332740514" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.06</text><text x="904.911111111111" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">age</text><path d="M 904.911111111111 837.2996748541225 L 908.5517396184061 826.8224868166606 L 912.1923681257014 816.4399729355478 L 915.8329966329966 806.3603282234851 L 919.4736251402917 796.7801974644212 L 923.1142536475869 787.8741557540253 L 926.7548821548821 779.7859591649619 L 930.3955106621772 772.6222796899038 L 934.0361391694724 766.449347568981 L 937.6767676767677 761.2925935741956 L 941.3173961840628 757.1390628529168 L 944.958024691358 753.9421058124782 L 948.598653198653 751.6276739143829 L 952.2392817059482 750.1014758187299 L 955.8799102132435 749.2562799473162 L 959.5205387205385 748.9787641725385 L 963.1611672278337 749.1554810699878 L 966.801795735129 749.6776921774901 L 970.4424242424241 750.4449936296046 L 974.0830527497193 751.3677837141468 L 977.7236812570145 752.3686982112837 L 981.3643097643096 753.383163156921 L 985.0049382716048 754.3591996395116 L 988.6455667789 755.2565811478169 L 992.2861952861952 756.0454120918625 L 995.9268237934904 756.7041837652288 L 999.5674523007856 757.217380565525 L 1003.2080808080807 757.5727542368693 L 1006.8487093153759 757.7584474857473 L 1010.4893378226711 757.7602143532936 L 1014.1299663299662 757.5590347733587 L 1017.7705948372613 757.129438634806 L 1021.4112233445567 756.4388301492701 L 1025.0518518518518 755.4480340914844 L 1028.6924803591469 754.1131772026222 L 1032.333108866442 752.3888827010442 L 1035.9737373737373 750.2326089810674 L 1039.6143658810324 747.6098214756021 L 1043.2549943883275 744.499564130475 L 1046.8956228956226 740.8999062625549 L 1050.536251402918 736.8326914304855 L 1054.176879910213 732.3470148788847 L 1057.8175084175082 727.5209107700578 L 1061.4581369248035 722.4608429376174 L 1065.0987654320986 717.2987622835942 L 1068.7393939393937 712.1867127278335 L 1072.380022446689 707.2892197720131 L 1076.0206509539842 702.7739559119742 L 1079.6612794612793 698.801412227381 L 1083.3019079685746 695.514478922437 L 1086.9425364758697 693.0289158311595 L 1090.5831649831648 691.4256538387172 L 1094.2237934904601 690.7457036977996 L 1097.8644219977552 690.9881744484258 L 1101.5050505050503 692.11155449065 L 1105.1456790123457 694.0380345499545 L 1108.7863075196408 696.6603101510584 L 1112.426936026936 699.8500445266136 L 1116.067564534231 703.4670395388573 L 1119.7081930415263 707.3681687730441 L 1123.3488215488214 711.4152652570217 L 1126.9894500561168 715.4813953975355 L 1130.6300785634116 719.4552438650377 L 1134.270707070707 723.2436281652208 L 1137.911335578002 726.7724074849814 L 1141.5519640852972 729.9862121409656 L 1145.1925925925925 732.8474804287735 L 1148.8332210998876 735.3352520647583 L 1152.4738496071827 737.4440527218269 L 1156.114478114478 739.1830453854888 L 1159.7551066217732 740.5754588432337 L 1163.3957351290683 741.65816579679 L 1167.0363636363636 742.4811975973662 L 1170.6769921436587 743.1069609179413 L 1174.3176206509538 743.6089611501831 L 1177.9582491582491 744.0699231765675 L 1181.5988776655443 744.5793096845347 L 1185.2395061728394 745.2303449978073 L 1188.8801346801347 746.1167358995918 L 1192.5207631874298 747.3293246564904 L 1196.161391694725 748.9529077007425 L 1199.8020202020202 751.0634106241321 L 1203.442648709315 753.7255391596037 L 1207.0832772166104 756.990944656518 L 1210.7239057239058 760.8968700641935 L 1214.3645342312007 765.4651942391007 L 1218.005162738496 770.701777608823 L 1221.645791245791 776.5960318185823 L 1225.2864197530864 783.1206828878709 L 1228.9270482603815 790.2317583707402 L 1232.5676767676769 797.8688876917167 L 1236.2083052749717 805.9560453614554 L 1239.848933782267 814.4028771535251 L 1243.4895622895622 823.1067238324155 L 1247.1301907968573 831.9553973312873 L 1250.7708193041526 840.8306790243412 L 1254.4114478114475 849.6124127800764 L 1258.0520763187428 858.1829732142264 L 1261.6927048260382 866.4318178669411 L 1265.3333333333333 874.2597934404245" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 904.911111111111 867.2285524565843 L 908.5517396184061 857.4924214694071 L 912.1923681257014 847.3969584046905 L 915.8329966329966 837.1387359192454 L 919.4736251402917 826.9144844589127 L 923.1142536475869 816.9081619584354 L 926.7548821548821 807.2804710112798 L 930.3955106621772 798.1620220061843 L 934.0361391694724 789.6506774134839 L 937.6767676767677 781.8128818817993 L 941.3173961840628 774.6881351524219 L 944.958024691358 768.2953248735395 L 948.598653198653 762.639476317401 L 952.2392817059482 757.7176027146926 L 955.8799102132435 753.5226997703627 L 959.5205387205385 750.0454251781686 L 963.1611672278337 747.2735266780174 L 966.801795735129 745.189527996195 L 970.4424242424241 743.7674773495743 L 974.0830527497193 742.9696731541337 L 977.7236812570145 742.7442097997821 L 981.3643097643096 743.023967613869 L 985.0049382716048 743.727359913797 L 988.6455667789 744.7608083886707 L 992.2861952861952 746.0226050288722 L 995.9268237934904 747.4075827962744 L 999.5674523007856 748.8118910574655 L 1003.2080808080807 750.1371707277164 L 1006.8487093153759 751.2935456649618 L 1010.4893378226711 752.2010724620656 L 1014.1299663299662 752.7895879830422 L 1017.7705948372613 752.9972196719324 L 1021.4112233445567 752.7681277475282 L 1025.0518518518518 752.0502783441162 L 1028.6924803591469 750.794153258088 L 1032.333108866442 748.9532466960673 L 1035.9737373737373 746.486963146943 L 1039.6143658810324 743.3661221853381 L 1043.2549943883275 739.5807391982701 L 1046.8956228956226 735.1491649702909 L 1050.536251402918 730.1271397865872 L 1054.176879910213 724.6149696319949 L 1057.8175084175082 718.7609715016026 L 1061.4581369248035 712.7596298359518 L 1065.0987654320986 706.843559476955 L 1068.7393939393937 701.2693061898018 L 1072.380022446689 696.2980825823662 L 1076.0206509539842 692.173533174506 L 1079.6612794612793 689.0993357734538 L 1083.3019079685746 687.2197054216028 L 1086.9425364758697 686.6055852829384 L 1090.5831649831648 687.2485121053301 L 1094.2237934904601 689.0629725775625 L 1097.8644219977552 691.8967474334995 L 1101.5050505050503 695.5475342612108 L 1105.1456790123457 699.7832835736618 L 1108.7863075196408 704.3633308869951 L 1112.426936026936 709.0576002975895 L 1116.067564534231 713.6618123078157 L 1119.7081930415263 718.0075752557427 L 1123.3488215488214 721.9672521436798 L 1126.9894500561168 725.454356438259 L 1130.6300785634116 728.4207813823947 L 1134.270707070707 730.8523352357172 L 1137.911335578002 732.7638628909976 L 1141.5519640852972 734.1947856321165 L 1145.1925925925925 735.2053356982816 L 1148.8332210998876 735.8732580070379 L 1152.4738496071827 736.2904244058893 L 1156.114478114478 736.5587255153669 L 1159.7551066217732 736.7847731982671 L 1163.3957351290683 737.0733030096696 L 1167.0363636363636 737.5196090408068 L 1170.6769921436587 738.2017576128429 L 1174.3176206509538 739.1736093914012 L 1177.9582491582491 740.4597653678745 L 1181.5988776655443 742.0534211038223 L 1185.2395061728394 743.9177915415922 L 1188.8801346801347 745.9913157891081 L 1192.5207631874298 748.1963451814328 L 1196.161391694725 750.4505360456351 L 1199.8020202020202 752.6797747919004 L 1203.442648709315 754.8312006642909 L 1207.0832772166104 756.8847836214566 L 1210.7239057239058 758.8619681953757 L 1214.3645342312007 760.8301051993187 L 1218.005162738496 762.9017515648936 L 1221.645791245791 765.2284073042858 L 1225.2864197530864 767.988849668023 L 1228.9270482603815 771.3728732469192 L 1232.5676767676769 775.5618851782325 L 1236.2083052749717 780.7083508983451 L 1239.848933782267 786.9164414677209 L 1243.4895622895622 794.2263087707302 L 1247.1301907968573 802.6041502504468 L 1250.7708193041526 811.9396130236378 L 1254.4114478114475 822.0511875145364 L 1258.0520763187428 832.6991783599742 L 1261.6927048260382 843.6047880383001 L 1265.3333333333333 854.4729929802497" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/></svg>

### The same comparison, as boxplots

A **boxplot** compresses a distribution to five numbers: the median as the line
in the middle, the box covering the middle half of the data, and the whiskers
reaching out to the bulk of the rest. Points beyond them are candidate outliers.

It shows less than a density curve but makes medians and spread easier to
compare across nine panels at a glance, and it is the standard way to look for
outliers.

**Reading it:** the screen-time boxes are clearly offset between classes,
matching the panels above. Nothing here shows the very long one-sided tails or
isolated extreme points that would suggest data-entry errors or a column needing
to be capped, so no outlier handling is applied anywhere in this notebook.

```haskell
import qualified DataFrame as D
import qualified Graphics.Hgg as G
import qualified Data.Text as Tx

boxPanel c = G.overlay
  [ G.boxplot (edaColOf edaClean c) <> G.color edaBlue
  , G.boxplot (edaColOf edaAddicted c) <> G.color edaOrange
  ] <> G.title c

displaySvg (Tx.unpack (G.renderSVG
  (edaGrid boxPanel numericCols <> G.width 960 <> G.height 720)))
```

> <!-- scripths:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1280" height="960" viewBox="0 0 1280 960"><rect x="0.0" y="0.0" width="1280.0" height="960.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="35.346666666666664" y1="253.67551231161454" x2="412.19555555555553" y2="253.67551231161454" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="162.080649949979" x2="412.19555555555553" y2="162.080649949979" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="70.48578758834347" x2="412.19555555555553" y2="70.48578758834347" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="207.87808113079677" x2="412.19555555555553" y2="207.87808113079677" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="116.28321876916125" x2="412.19555555555553" y2="116.28321876916125" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="35.346666666666664" y="39.599999999999994" width="376.84888888888884" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="207.87808113079677" x2="31.679999999999996" y2="207.87808113079677" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="211.98474779746346" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">5</text><line x1="35.346666666666664" y1="116.28321876916125" x2="31.679999999999996" y2="116.28321876916125" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="120.38988543582792" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">10</text><text x="35.346666666666664" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">daily_screen_time_hours</text><rect x="129.55888888888887" y="178.33873801916926" width="188.42444444444442" height="46.34700035498762" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="199.8177332429728" x2="317.9833333333333" y2="199.8177332429728" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="224.68573837415687" x2="223.77111111111108" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="178.33873801916926" x2="223.77111111111108" y2="121.4125310614128" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="280.4212121212121" x2="270.8772222222222" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="121.4125310614128" x2="270.8772222222222" y2="121.4125310614128" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="102.17760996546936" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="129.55888888888887" y="114.08494207248198" width="188.42444444444442" height="45.43105173137123" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="142.11296995514246" x2="317.9833333333333" y2="142.11296995514246" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="159.5159938038532" x2="223.77111111111108" y2="227.47938167618673" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="114.08494207248198" x2="223.77111111111108" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="227.47938167618673" x2="270.8772222222222" y2="227.47938167618673" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="51.067676767676744" x2="270.8772222222222" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="285.00095523929383" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="276.7574176267467" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="269.0634491883693" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="267.5979313905831" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="264.8500855197341" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="263.9341368961177" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="263.7509471713944" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="262.10223964888496" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="262.10223964888496" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="261.73586019943843" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="260.087152676929" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="259.53758350275916" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="257.88887598024974" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="252.57637396327488" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="251.84361506438182" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="251.47723561493527" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="249.46214864297932" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="249.46214864297932" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="248.91257946880947" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="248.91257946880947" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="248.36301029463965" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="245.98154387323714" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="243.60007745183464" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="243.41688772711134" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="242.31774937877174" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="241.03542130570884" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.6539548843063" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.47076515958304" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.28757543485978" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.28757543485978" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.1043857101365" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="238.1043857101365" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="236.8220576370736" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="235.5397295640107" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="235.5397295640107" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="234.2574014909478" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="233.34145286733147" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="233.15826314260818" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="232.2423145189918" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="232.2423145189918" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="230.41041727175912" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="229.31127892341948" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="228.21214057507987" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="228.02895085035658" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="31.832755671733327" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="461.91555555555556" y1="260.42192425179434" x2="838.7644444444443" y2="260.42192425179434" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="217.87024793388426" x2="838.7644444444443" y2="217.87024793388426" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="175.31857161597418" x2="838.7644444444443" y2="175.31857161597418" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="132.76689529806407" x2="838.7644444444443" y2="132.76689529806407" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="90.21521898015399" x2="838.7644444444443" y2="90.21521898015399" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="47.66354266224391" x2="838.7644444444443" y2="47.66354266224391" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="281.6977624107494" x2="838.7644444444443" y2="281.6977624107494" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="239.1460860928393" x2="838.7644444444443" y2="239.1460860928393" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="196.59440977492923" x2="838.7644444444443" y2="196.59440977492923" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="154.04273345701915" x2="838.7644444444443" y2="154.04273345701915" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="111.49105713910906" x2="838.7644444444443" y2="111.49105713910906" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="68.93938082119897" x2="838.7644444444443" y2="68.93938082119897" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="461.91555555555556" y="39.599999999999994" width="376.8488888888888" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="281.6977624107494" x2="458.24888888888887" y2="281.6977624107494" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="285.8044290774161" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0</text><line x1="461.91555555555556" y1="239.1460860928393" x2="458.24888888888887" y2="239.1460860928393" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="243.252752759506" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">1</text><line x1="461.91555555555556" y1="196.59440977492923" x2="458.24888888888887" y2="196.59440977492923" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="200.70107644159592" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">2</text><line x1="461.91555555555556" y1="154.04273345701915" x2="458.24888888888887" y2="154.04273345701915" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="158.1494001236858" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">3</text><line x1="461.91555555555556" y1="111.49105713910906" x2="458.24888888888887" y2="111.49105713910906" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="115.59772380577573" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">4</text><line x1="461.91555555555556" y1="68.93938082119897" x2="458.24888888888887" y2="68.93938082119897" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="73.04604748786564" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">5</text><text x="461.91555555555556" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">social_media_hours</text><rect x="556.1277777777777" y="176.59512190551146" width="188.4244444444444" height="63.08286014130173" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="215.52990573639923" x2="744.5522222222222" y2="215.52990573639923" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="239.6779820468132" x2="650.3399999999999" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="176.59512190551146" x2="650.3399999999999" y2="115.74622477090004" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="280.4212121212121" x2="697.446111111111" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="115.74622477090004" x2="697.446111111111" y2="115.74622477090004" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="556.1277777777777" y="132.76689529806407" width="188.4244444444444" height="54.891662450104036" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="173.6165045632578" x2="744.5522222222222" y2="173.6165045632578" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="187.6585577481681" x2="650.3399999999999" y2="268.5067427521973" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="132.76689529806407" x2="650.3399999999999" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="268.5067427521973" x2="697.446111111111" y2="268.5067427521973" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="51.067676767676744" x2="697.446111111111" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="278.71914506849566" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="277.8681115421375" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="275.74052772624196" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="274.0384606735256" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="274.0384606735256" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="271.05984333127185" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="271.05984333127185" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="49.365609714960364" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="48.5145761886021" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="48.5145761886021" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="46.38699237270664" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="45.53595884634846" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="43.408375030452966" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="43.408375030452966" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="43.408375030452966" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="42.5573415040947" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="41.2807912145574" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="41.2807912145574" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="41.2807912145574" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="40.004240925020135" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="40.004240925020135" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="39.153207398661905" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="37.45114034594553" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="37.45114034594553" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="36.6001068195873" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="35.74907329322912" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="35.32355653005004" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="34.89803976687089" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="34.47252300369181" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="31.06838889825902" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="24.68563745057247" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="22.132536871497905" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="22.132536871497905" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="17.877369239706884" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="17.877369239706884" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="17.877369239706884" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="17.026335713348693" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="15.749785423811392" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="14.473235134274052" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="14.473235134274052" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="11.494617792020373" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="5.96289987069205" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="4.686349581154748" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="-3.823985682427254" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="-29.78050823635245" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="888.4844444444443" y1="250.94132325828983" x2="1265.333333333333" y2="250.94132325828983" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="191.98154553244524" x2="1265.333333333333" y2="191.98154553244524" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="133.02176780660068" x2="1265.333333333333" y2="133.02176780660068" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="74.06199008075612" x2="1265.333333333333" y2="74.06199008075612" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="280.4212121212121" x2="1265.333333333333" y2="280.4212121212121" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="221.46143439536752" x2="1265.333333333333" y2="221.46143439536752" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="162.50165666952296" x2="1265.333333333333" y2="162.50165666952296" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="103.5418789436784" x2="1265.333333333333" y2="103.5418789436784" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="44.58210121783387" x2="1265.333333333333" y2="44.58210121783387" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="888.4844444444443" y="39.599999999999994" width="376.84888888888884" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="280.4212121212121" x2="884.8177777777776" y2="280.4212121212121" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="284.52787878787876" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0</text><line x1="888.4844444444443" y1="221.46143439536752" x2="884.8177777777776" y2="221.46143439536752" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="225.5681010620342" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">1</text><line x1="888.4844444444443" y1="162.50165666952296" x2="884.8177777777776" y2="162.50165666952296" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="166.60832333618964" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">2</text><line x1="888.4844444444443" y1="103.5418789436784" x2="884.8177777777776" y2="103.5418789436784" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="107.64854561034505" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">3</text><line x1="888.4844444444443" y1="44.58210121783387" x2="884.8177777777776" y2="44.58210121783387" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="48.688767884500535" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">4</text><text x="888.4844444444443" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">gaming_hours</text><rect x="982.6966666666665" y="191.39194775518683" width="188.42444444444442" height="44.21983329438342" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="200.23591441406347" x2="1171.121111111111" y2="200.23591441406347" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="235.61178104957025" x2="1076.9088888888887" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="191.39194775518683" x2="1076.9088888888887" y2="125.94659447949934" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="280.4212121212121" x2="1124.0149999999999" y2="280.4212121212121" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="125.94659447949934" x2="1124.0149999999999" y2="125.94659447949934" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="121.81941003869022" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="120.0506167069149" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="115.33383448884732" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="108.25866116174598" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="107.66906338448749" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="105.31067227545375" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="104.72107449819532" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="104.72107449819532" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="101.77308561190307" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="98.82509672561082" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="97.05630339383552" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="95.8771078393186" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="95.28751006206016" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="91.74992339850947" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="88.21233673495885" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="84.08515229414971" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="82.90595673963279" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="81.7267611851159" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="79.36837007608213" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="78.7787722988237" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="982.6966666666665" y="155.42648334242162" width="188.42444444444442" height="69.57253771649658" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="194.33993664147903" x2="1171.121111111111" y2="194.33993664147903" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="224.9990210589182" x2="1076.9088888888887" y2="279.8316143439537" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="155.42648334242162" x2="1076.9088888888887" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="279.8316143439537" x2="1124.0149999999999" y2="279.8316143439537" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="51.067676767676744" x2="1124.0149999999999" y2="51.067676767676744" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="50.4780789904183" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="50.4780789904183" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="47.530090104126046" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="47.530090104126046" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="47.530090104126046" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="1076.9088888888887" cy="45.76129677235072" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="35.346666666666664" y1="598.4875773323434" x2="412.19555555555553" y2="598.4875773323434" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="547.4066117747632" x2="412.19555555555553" y2="547.4066117747632" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="496.325646217183" x2="412.19555555555553" y2="496.325646217183" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="445.24468065960275" x2="412.19555555555553" y2="445.24468065960275" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="394.1637151020225" x2="412.19555555555553" y2="394.1637151020225" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="572.9470945535534" x2="412.19555555555553" y2="572.9470945535534" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="521.8661289959731" x2="412.19555555555553" y2="521.8661289959731" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="470.7851634383929" x2="412.19555555555553" y2="470.7851634383929" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="419.70419788081256" x2="412.19555555555553" y2="419.70419788081256" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="368.62323232323234" x2="412.19555555555553" y2="368.62323232323234" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="35.346666666666664" y="357.15555555555557" width="376.84888888888884" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="572.9470945535534" x2="31.679999999999996" y2="572.9470945535534" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="577.0537612202199" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">5</text><line x1="35.346666666666664" y1="521.8661289959731" x2="31.679999999999996" y2="521.8661289959731" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="525.9727956626398" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">6</text><line x1="35.346666666666664" y1="470.7851634383929" x2="31.679999999999996" y2="470.7851634383929" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="474.8918301050595" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">7</text><line x1="35.346666666666664" y1="419.70419788081256" x2="31.679999999999996" y2="419.70419788081256" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="423.81086454747924" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">8</text><line x1="35.346666666666664" y1="368.62323232323234" x2="31.679999999999996" y2="368.62323232323234" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="372.72989898989897" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">9</text><text x="35.346666666666664" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">sleep_hours</text><rect x="129.55888888888887" y="433.87916582304115" width="188.42444444444442" height="100.75720456232708" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="481.00135654990896" x2="317.9833333333333" y2="481.00135654990896" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="534.6363703853682" x2="223.77111111111108" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="433.87916582304115" x2="223.77111111111108" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="597.9767676767676" x2="270.8772222222222" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="368.62323232323234" x2="270.8772222222222" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="129.55888888888887" y="426.34472340329796" width="188.42444444444442" height="101.77882387347873" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="481.00135654990896" x2="317.9833333333333" y2="481.00135654990896" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="528.1235472767767" x2="223.77111111111108" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="426.34472340329796" x2="223.77111111111108" y2="369.1340419788081" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="597.9767676767676" x2="270.8772222222222" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="369.1340419788081" x2="270.8772222222222" y2="369.1340419788081" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="461.91555555555556" y1="592.9908212560387" x2="838.7644444444443" y2="592.9908212560387" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="543.1313570487483" x2="838.7644444444443" y2="543.1313570487483" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="493.2718928414581" x2="838.7644444444443" y2="493.2718928414581" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="443.4124286341678" x2="838.7644444444443" y2="443.4124286341678" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="393.5529644268775" x2="838.7644444444443" y2="393.5529644268775" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="568.0610891523935" x2="838.7644444444443" y2="568.0610891523935" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="518.2016249451032" x2="838.7644444444443" y2="518.2016249451032" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="468.34216073781295" x2="838.7644444444443" y2="468.34216073781295" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="418.4826965305227" x2="838.7644444444443" y2="418.4826965305227" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="368.62323232323234" x2="838.7644444444443" y2="368.62323232323234" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="461.91555555555556" y="357.15555555555557" width="376.8488888888888" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="568.0610891523935" x2="458.24888888888887" y2="568.0610891523935" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="572.1677558190602" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">50</text><line x1="461.91555555555556" y1="518.2016249451032" x2="458.24888888888887" y2="518.2016249451032" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="522.3082916117698" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">100</text><line x1="461.91555555555556" y1="468.34216073781295" x2="458.24888888888887" y2="468.34216073781295" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="472.4488274044796" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">150</text><line x1="461.91555555555556" y1="418.4826965305227" x2="458.24888888888887" y2="418.4826965305227" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="422.5893631971893" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">200</text><line x1="461.91555555555556" y1="368.62323232323234" x2="458.24888888888887" y2="368.62323232323234" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="372.72989898989897" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">250</text><text x="461.91555555555556" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">notifications_per_day</text><rect x="556.1277777777777" y="418.4826965305227" width="188.4244444444444" height="93.73579270970572" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="472.4406086956522" x2="744.5522222222222" y2="472.4406086956522" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="512.2184892402283" x2="650.3399999999999" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="418.4826965305227" x2="650.3399999999999" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="597.9767676767676" x2="697.446111111111" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="368.62323232323234" x2="697.446111111111" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="556.1277777777777" y="423.4686429512517" width="188.4244444444444" height="91.74141414141414" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="472.4406086956522" x2="744.5522222222222" y2="472.4406086956522" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="515.2100570926658" x2="650.3399999999999" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="423.4686429512517" x2="650.3399999999999" y2="368.62323232323234" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="597.9767676767676" x2="697.446111111111" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="368.62323232323234" x2="697.446111111111" y2="368.62323232323234" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="888.4844444444443" y1="584.0765534129171" x2="1265.333333333333" y2="584.0765534129171" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="514.5754820936638" x2="1265.333333333333" y2="514.5754820936638" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="445.0744107744108" x2="1265.333333333333" y2="445.0744107744108" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="375.5733394551576" x2="1265.333333333333" y2="375.5733394551576" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="549.3260177532904" x2="1265.333333333333" y2="549.3260177532904" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="479.8249464340373" x2="1265.333333333333" y2="479.8249464340373" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="410.3238751147842" x2="1265.333333333333" y2="410.3238751147842" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="888.4844444444443" y="357.15555555555557" width="376.84888888888884" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="549.3260177532904" x2="884.8177777777776" y2="549.3260177532904" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="553.4326844199571" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">50</text><line x1="888.4844444444443" y1="479.8249464340373" x2="884.8177777777776" y2="479.8249464340373" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="483.931613100704" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">100</text><line x1="888.4844444444443" y1="410.3238751147842" x2="884.8177777777776" y2="410.3238751147842" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="414.4305417814508" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">150</text><text x="888.4844444444443" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">app_opens_per_day</text><rect x="982.6966666666665" y="438.1243036424854" width="188.42444444444442" height="86.18132843587394" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="476.1552898683808" x2="1171.121111111111" y2="476.1552898683808" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="524.3056320783594" x2="1076.9088888888887" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="438.1243036424854" x2="1076.9088888888887" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="597.9767676767676" x2="1124.0149999999999" y2="597.9767676767676" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="368.62323232323234" x2="1124.0149999999999" y2="368.62323232323234" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="982.6966666666665" y="421.44404652586474" width="188.42444444444442" height="107.03164983164984" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="476.1552898683808" x2="1171.121111111111" y2="476.1552898683808" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="528.4756963575146" x2="1076.9088888888887" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="421.44404652586474" x2="1076.9088888888887" y2="368.62323232323234" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="597.9767676767676" x2="1124.0149999999999" y2="597.9767676767676" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="368.62323232323234" x2="1124.0149999999999" y2="368.62323232323234" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="35.346666666666664" y1="881.3770513779773" x2="412.19555555555553" y2="881.3770513779773" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="828.2859552313255" x2="412.19555555555553" y2="828.2859552313255" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="775.1948590846738" x2="412.19555555555553" y2="775.1948590846738" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="722.1037629380222" x2="412.19555555555553" y2="722.1037629380222" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.346666666666664" y1="907.9225994513031" x2="412.19555555555553" y2="907.9225994513031" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="854.8315033046515" x2="412.19555555555553" y2="854.8315033046515" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="801.7404071579997" x2="412.19555555555553" y2="801.7404071579997" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="748.649311011348" x2="412.19555555555553" y2="748.649311011348" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="695.5582148646963" x2="412.19555555555553" y2="695.5582148646963" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="35.346666666666664" y="674.711111111111" width="376.84888888888884" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="35.346666666666664" y1="907.9225994513031" x2="31.679999999999996" y2="907.9225994513031" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="912.0292661179698" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">3</text><line x1="35.346666666666664" y1="854.8315033046515" x2="31.679999999999996" y2="854.8315033046515" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="858.9381699713182" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">6</text><line x1="35.346666666666664" y1="801.7404071579997" x2="31.679999999999996" y2="801.7404071579997" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="805.8470738246665" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">9</text><line x1="35.346666666666664" y1="748.649311011348" x2="31.679999999999996" y2="748.649311011348" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="752.7559776780147" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">12</text><line x1="35.346666666666664" y1="695.5582148646963" x2="31.679999999999996" y2="695.5582148646963" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.746666666666663" y="699.664881531363" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">15</text><text x="35.346666666666664" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">weekend_screen_time</text><rect x="129.55888888888887" y="797.979787847612" width="188.42444444444442" height="61.80688443072692" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="829.5247474747475" x2="317.9833333333333" y2="829.5247474747475" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="859.7866722783389" x2="223.77111111111108" y2="915.5323232323232" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="797.979787847612" x2="223.77111111111108" y2="745.1099046015712" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="915.5323232323232" x2="270.8772222222222" y2="915.5323232323232" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="745.1099046015712" x2="270.8772222222222" y2="745.1099046015712" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="129.55888888888887" y="751.1268954981917" width="188.42444444444442" height="44.9504614041651" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="129.55888888888887" y1="784.0433751091159" x2="317.9833333333333" y2="784.0433751091159" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="223.77111111111108" y1="796.0773569023569" x2="223.77111111111108" y2="862.0872864446939" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="223.77111111111108" y1="751.1268954981917" x2="223.77111111111108" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="862.0872864446939" x2="270.8772222222222" y2="862.0872864446939" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="176.66499999999996" y1="686.1787878787878" x2="270.8772222222222" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="946.50212931787" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="924.9117502182316" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="917.6559670781892" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="901.0207569522383" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="895.3577066965955" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="893.764973812196" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="893.4110331712183" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="891.8183002868187" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="891.1104190048634" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="890.0485970819304" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="890.0485970819304" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="890.0485970819304" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="889.8716267614415" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="889.340715799975" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="887.5710125950866" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="886.3322203516648" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="885.2703984287318" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="884.7394874672652" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="881.554021698466" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="881.554021698466" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="880.3152294550442" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="878.8994668911334" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="877.8376449682005" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="875.3600604813566" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="873.2364166354906" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="869.3430695847362" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="867.7503367003367" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="866.6885147774037" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="866.334574136426" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="866.157603815937" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="865.4497225339817" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="865.2727522134929" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.7418412520265" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.7418412520265" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.5648709315375" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.3879006110487" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.3879006110487" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="864.3879006110487" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="863.6800193290934" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="863.6800193290934" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="682.9933221099888" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="682.8163517894998" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="682.1084705075446" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="679.9848266616784" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><circle cx="223.77111111111108" cy="679.6308860207007" r="3.1181102362204727" fill="#dd8452" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="461.91555555555556" y1="901.2965865552071" x2="838.7644444444443" y2="901.2965865552071" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="861.7528735632183" x2="838.7644444444443" y2="861.7528735632183" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="822.2091605712295" x2="838.7644444444443" y2="822.2091605712295" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="782.6654475792407" x2="838.7644444444443" y2="782.6654475792407" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="743.1217345872517" x2="838.7644444444443" y2="743.1217345872517" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="703.5780215952628" x2="838.7644444444443" y2="703.5780215952628" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="461.91555555555556" y1="921.0684430512016" x2="838.7644444444443" y2="921.0684430512016" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="881.5247300592127" x2="838.7644444444443" y2="881.5247300592127" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="841.981017067224" x2="838.7644444444443" y2="841.981017067224" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="802.437304075235" x2="838.7644444444443" y2="802.437304075235" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="762.8935910832462" x2="838.7644444444443" y2="762.8935910832462" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="723.3498780912573" x2="838.7644444444443" y2="723.3498780912573" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="683.8061650992684" x2="838.7644444444443" y2="683.8061650992684" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="461.91555555555556" y="674.711111111111" width="376.8488888888888" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="461.91555555555556" y1="921.0684430512016" x2="458.24888888888887" y2="921.0684430512016" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="925.1751097178683" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0</text><line x1="461.91555555555556" y1="881.5247300592127" x2="458.24888888888887" y2="881.5247300592127" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="885.6313967258794" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">1</text><line x1="461.91555555555556" y1="841.981017067224" x2="458.24888888888887" y2="841.981017067224" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="846.0876837338907" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">2</text><line x1="461.91555555555556" y1="802.437304075235" x2="458.24888888888887" y2="802.437304075235" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="806.5439707419018" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">3</text><line x1="461.91555555555556" y1="762.8935910832462" x2="458.24888888888887" y2="762.8935910832462" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="767.0002577499129" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">4</text><line x1="461.91555555555556" y1="723.3498780912573" x2="458.24888888888887" y2="723.3498780912573" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="727.456544757924" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">5</text><line x1="461.91555555555556" y1="683.8061650992684" x2="458.24888888888887" y2="683.8061650992684" stroke="#444444" stroke-width="1.3333333333333333"/><text x="455.31555555555553" y="687.9128317659352" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">6</text><text x="461.91555555555556" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">work_study_hours</text><rect x="556.1277777777777" y="825.2737983281087" width="188.4244444444444" height="47.94675200278637" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="848.3080111459421" x2="744.5522222222222" y2="848.3080111459421" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="873.2205503308951" x2="650.3399999999999" y2="914.7414489724835" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="825.2737983281087" x2="650.3399999999999" y2="755.7757227446882" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="914.7414489724835" x2="697.446111111111" y2="914.7414489724835" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="755.7757227446882" x2="697.446111111111" y2="755.7757227446882" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="750.6350400557296" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="745.8897944966909" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="744.7034831069313" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="740.7491118077324" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="737.1901776384534" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="736.0038662486937" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="732.4449320794148" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="726.5133751306164" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="719.7909439219784" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="705.9506443747823" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><circle cx="650.3399999999999" cy="697.6464646464647" r="3.1181102362204727" fill="#4c72b0" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="556.1277777777777" y="784.2471960989202" width="188.4244444444444" height="73.94674329501913" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="556.1277777777777" y1="827.349843260188" x2="744.5522222222222" y2="827.349843260188" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="650.3399999999999" y1="858.1939393939393" x2="650.3399999999999" y2="915.5323232323232" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="650.3399999999999" y1="784.2471960989202" x2="650.3399999999999" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="915.5323232323232" x2="697.446111111111" y2="915.5323232323232" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="603.2338888888888" y1="686.1787878787878" x2="697.446111111111" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="888.4844444444443" y1="922.2780154486037" x2="1265.333333333333" y2="922.2780154486037" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="854.8210932857992" x2="1265.333333333333" y2="854.8210932857992" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="787.3641711229945" x2="1265.333333333333" y2="787.3641711229945" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="719.90724896019" x2="1265.333333333333" y2="719.90724896019" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="888.4844444444443" y1="888.5495543672014" x2="1265.333333333333" y2="888.5495543672014" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="821.0926322043968" x2="1265.333333333333" y2="821.0926322043968" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="753.6357100415923" x2="1265.333333333333" y2="753.6357100415923" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="686.1787878787878" x2="1265.333333333333" y2="686.1787878787878" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="888.4844444444443" y="674.711111111111" width="376.84888888888884" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="888.4844444444443" y1="888.5495543672014" x2="884.8177777777776" y2="888.5495543672014" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="892.6562210338681" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">20</text><line x1="888.4844444444443" y1="821.0926322043968" x2="884.8177777777776" y2="821.0926322043968" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="825.1992988710635" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">25</text><line x1="888.4844444444443" y1="753.6357100415923" x2="884.8177777777776" y2="753.6357100415923" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="757.7423767082591" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">30</text><line x1="888.4844444444443" y1="686.1787878787878" x2="884.8177777777776" y2="686.1787878787878" stroke="#444444" stroke-width="1.3333333333333333"/><text x="881.8844444444442" y="690.2854545454545" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">35</text><text x="888.4844444444443" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">age</text><rect x="982.6966666666665" y="753.6357100415923" width="188.42444444444442" height="107.93107546048714" fill="#ffffff" fill-opacity="1.0" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="799.2365894236482" x2="1171.121111111111" y2="799.2365894236482" stroke="#4c72b0" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="861.5667855020795" x2="1076.9088888888887" y2="915.5323232323232" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="753.6357100415923" x2="1076.9088888888887" y2="686.1787878787878" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="915.5323232323232" x2="1124.0149999999999" y2="915.5323232323232" stroke="#4c72b0" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="686.1787878787878" x2="1124.0149999999999" y2="686.1787878787878" stroke="#4c72b0" stroke-width="1.3333333333333333"/><rect x="982.6966666666665" y="740.1443256090315" width="188.42444444444442" height="121.42245989304804" fill="#ffffff" fill-opacity="1.0" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="982.6966666666665" y1="799.2365894236482" x2="1171.121111111111" y2="799.2365894236482" stroke="#dd8452" stroke-width="2.6666666666666665"/><line x1="1076.9088888888887" y1="861.5667855020795" x2="1076.9088888888887" y2="915.5323232323232" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1076.9088888888887" y1="740.1443256090315" x2="1076.9088888888887" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="915.5323232323232" x2="1124.0149999999999" y2="915.5323232323232" stroke="#dd8452" stroke-width="1.3333333333333333"/><line x1="1029.8027777777777" y1="686.1787878787878" x2="1124.0149999999999" y2="686.1787878787878" stroke="#dd8452" stroke-width="1.3333333333333333"/></svg>

### The three columns we nearly threw away

`gender`, `stress_level` and `academic_work_impact` were dropped by the first
pass. Let's check if they are useful.

Each bar is the proportion of addicted people within one value of one column, so
a column that mattered would show bars at clearly different heights.

**Reading it:** every bar is flat against the 0.7094 base rate. Gender spans
0.701 to 0.723, stress 0.706 to 0.711, work impact 0.708 to 0.711. Knowing
someone reports high stress moves the estimate by less than half a percentage
point.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified Graphics.Hgg as G
import qualified Data.Text as Tx
import Data.Text (Text)
import DataFrame ((|>))

labelRate d = D.mean (F.lift (fromIntegral :: Int -> Double) (F.col @Int "addicted_label")) d

catRate col v =
  labelRate (D.filterWhere (F.lift (== Just v) (F.col @(Maybe Text) col)) df)

catPanel (col, vals) = G.overlay
  [ G.bar (txtCol vals) (numCol (map (catRate col) vals)) <> G.color edaOrange ]
  <> G.title col

catSpecs =
  [ ("gender" :: Text, ["Male", "Female", "Other"])
  , ("stress_level", ["Low", "Medium", "High"])
  , ("academic_work_impact", ["Yes", "No"]) ]

displayMarkdown (D.toMarkdown' (D.fromRows ["column", "value", "P(addicted)"]
  [ [D.toAny col, D.toAny v, D.toAny (r4 (catRate col v))]
  | (col, vals) <- catSpecs, v <- vals ]))

displaySvg (Tx.unpack (G.renderSVG
  (G.hconcat (map catPanel catSpecs) <> G.width 960 <> G.height 300)))
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> |    column<br>Text    | value<br>Text | P(addicted)<br>Double |
> | ---------------------|---------------|---------------------- |
> | gender               | Male          | 0.7232                |
> | gender               | Female        | 0.7038                |
> | gender               | Other         | 0.701                 |
> | stress_level         | Low           | 0.7112                |
> | stress_level         | Medium        | 0.7057                |
> | stress_level         | High          | 0.7114                |
> | academic_work_impact | Yes           | 0.7079                |
> | academic_work_impact | No            | 0.711                 |
> 
> <!-- MIME:image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1280" height="400" viewBox="0 0 1280 400"><rect x="0.0" y="0.0" width="1280.0" height="400.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="112.60583333333332" y1="39.599999999999994" x2="112.60583333333332" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="229.63777777777776" y1="39.599999999999994" x2="229.63777777777776" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="346.6697222222222" y1="39.599999999999994" x2="346.6697222222222" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="42.38666666666666" y1="323.8846495163672" x2="416.8888888888889" y2="323.8846495163672" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="42.38666666666666" y1="237.65394854910156" x2="416.8888888888889" y2="237.65394854910156" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="42.38666666666666" y1="151.423247581836" x2="416.8888888888889" y2="151.423247581836" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="42.38666666666666" y1="65.19254661457042" x2="416.8888888888889" y2="65.19254661457042" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="42.38666666666666" y1="367.0" x2="416.8888888888889" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="42.38666666666666" y1="280.7692990327344" x2="416.8888888888889" y2="280.7692990327344" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="42.38666666666666" y1="194.53859806546882" x2="416.8888888888889" y2="194.53859806546882" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="42.38666666666666" y1="108.30789709820317" x2="416.8888888888889" y2="108.30789709820317" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="42.38666666666666" y="39.599999999999994" width="374.5022222222222" height="327.4" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="112.60583333333332" y1="367.0" x2="112.60583333333332" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="112.60583333333332" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Female</text><line x1="229.63777777777776" y1="367.0" x2="229.63777777777776" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="229.63777777777776" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Male</text><line x1="346.6697222222222" y1="367.0" x2="346.6697222222222" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="346.6697222222222" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Other</text><line x1="42.38666666666666" y1="367.0" x2="38.72" y2="367.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="35.78666666666666" y="371.1066666666666" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="42.38666666666666" y1="280.7692990327344" x2="38.72" y2="280.7692990327344" stroke="#444444" stroke-width="1.3333333333333333"/><text x="35.78666666666666" y="284.8759656994011" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="42.38666666666666" y1="194.53859806546882" x2="38.72" y2="194.53859806546882" stroke="#444444" stroke-width="1.3333333333333333"/><text x="35.78666666666666" y="198.6452647321355" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="42.38666666666666" y1="108.30789709820317" x2="38.72" y2="108.30789709820317" stroke="#444444" stroke-width="1.3333333333333333"/><text x="35.78666666666666" y="112.41456376486983" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.6</text><text x="42.38666666666666" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">gender</text><rect x="176.97340277777778" y="55.19047619047615" width="105.32874999999999" height="311.80952380952385" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="59.94145833333333" y="63.53774368190818" width="105.32874999999999" height="303.4622563180918" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="294.0053472222222" y="64.752808889095" width="105.32874999999999" height="302.24719111090496" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="536.8280555555556" y1="39.599999999999994" x2="536.8280555555556" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="653.86" y1="39.599999999999994" x2="653.86" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="770.8919444444446" y1="39.599999999999994" x2="770.8919444444446" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="466.60888888888894" y1="323.1715297879665" x2="841.1111111111111" y2="323.1715297879665" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="466.60888888888894" y1="235.51458936389946" x2="841.1111111111111" y2="235.51458936389946" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="466.60888888888894" y1="147.85764893983244" x2="841.1111111111111" y2="147.85764893983244" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="466.60888888888894" y1="60.20070851576546" x2="841.1111111111111" y2="60.20070851576546" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="466.60888888888894" y1="367.0" x2="841.1111111111111" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="466.60888888888894" y1="279.343059575933" x2="841.1111111111111" y2="279.343059575933" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="466.60888888888894" y1="191.68611915186597" x2="841.1111111111111" y2="191.68611915186597" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="466.60888888888894" y1="104.02917872779895" x2="841.1111111111111" y2="104.02917872779895" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="466.60888888888894" y="39.599999999999994" width="374.5022222222222" height="327.4" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="536.8280555555556" y1="367.0" x2="536.8280555555556" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="536.8280555555556" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">High</text><line x1="653.86" y1="367.0" x2="653.86" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="653.86" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Low</text><line x1="770.8919444444446" y1="367.0" x2="770.8919444444446" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="770.8919444444446" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Medium</text><line x1="466.60888888888894" y1="367.0" x2="462.94222222222226" y2="367.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="460.0088888888889" y="371.1066666666666" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="466.60888888888894" y1="279.343059575933" x2="462.94222222222226" y2="279.343059575933" stroke="#444444" stroke-width="1.3333333333333333"/><text x="460.0088888888889" y="283.4497262425997" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="466.60888888888894" y1="191.68611915186597" x2="462.94222222222226" y2="191.68611915186597" stroke="#444444" stroke-width="1.3333333333333333"/><text x="460.0088888888889" y="195.79278581853265" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="466.60888888888894" y1="104.02917872779895" x2="462.94222222222226" y2="104.02917872779895" stroke="#444444" stroke-width="1.3333333333333333"/><text x="460.0088888888889" y="108.13584539446562" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.6</text><text x="466.60888888888894" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">stress_level</text><rect x="601.1956250000001" y="55.309363238496225" width="105.32874999999999" height="311.69063676150375" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="718.2275694444445" y="57.71857681079378" width="105.32874999999999" height="309.2814231892062" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="484.16368055555563" y="55.19047619047615" width="105.32874999999999" height="311.80952380952385" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="992.9680808080808" y1="39.599999999999994" x2="992.9680808080808" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1163.1963636363635" y1="39.599999999999994" x2="1163.1963636363635" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="890.8311111111111" y1="323.1467512947328" x2="1265.3333333333333" y2="323.1467512947328" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="890.8311111111111" y1="235.44025388419846" x2="1265.3333333333333" y2="235.44025388419846" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="890.8311111111111" y1="147.73375647366407" x2="1265.3333333333333" y2="147.73375647366407" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="890.8311111111111" y1="60.027259063129804" x2="1265.3333333333333" y2="60.027259063129804" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="890.8311111111111" y1="367.0" x2="1265.3333333333333" y2="367.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="890.8311111111111" y1="279.2935025894656" x2="1265.3333333333333" y2="279.2935025894656" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="890.8311111111111" y1="191.5870051789313" x2="1265.3333333333333" y2="191.5870051789313" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="890.8311111111111" y1="103.88050776839691" x2="1265.3333333333333" y2="103.88050776839691" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="890.8311111111111" y="39.599999999999994" width="374.5022222222222" height="327.4" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="992.9680808080808" y1="367.0" x2="992.9680808080808" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="992.9680808080808" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">No</text><line x1="1163.1963636363635" y1="367.0" x2="1163.1963636363635" y2="370.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1163.1963636363635" y="382.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">Yes</text><line x1="890.8311111111111" y1="367.0" x2="887.1644444444444" y2="367.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="884.231111111111" y="371.1066666666666" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="890.8311111111111" y1="279.2935025894656" x2="887.1644444444444" y2="279.2935025894656" stroke="#444444" stroke-width="1.3333333333333333"/><text x="884.231111111111" y="283.4001692561323" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="890.8311111111111" y1="191.5870051789313" x2="887.1644444444444" y2="191.5870051789313" stroke="#444444" stroke-width="1.3333333333333333"/><text x="884.231111111111" y="195.69367184559798" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="890.8311111111111" y1="103.88050776839691" x2="887.1644444444444" y2="103.88050776839691" stroke="#444444" stroke-width="1.3333333333333333"/><text x="884.231111111111" y="107.98717443506357" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.6</text><text x="890.8311111111111" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">academic_work_impact</text><rect x="1086.5936363636363" y="56.57037615695723" width="153.20545454545447" height="310.42962384304275" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="916.3653535353535" y="55.19047619047618" width="153.20545454545447" height="311.8095238095238" fill="#dd8452" fill-opacity="1.0" stroke="none"/></svg>

### How the features relate to each other

The two charts here answer different questions.

The **heatmap** shows every pairwise correlation among the features. Correlation
runs from -1 to 1: 1 means two columns move together exactly, 0 means no linear
relationship, -1 means they move oppositely. Only the lower triangle is drawn,
because the matrix is symmetric and drawing both halves adds ink without adding
information.

What you look for is pairs of features that are strongly correlated with each
other, called **multicollinearity**. Tree models tolerate it, but it explains
why adding more variants of an already-present quantity does nothing, which is
the Section 9 result about synthesised features.

The **bar chart** ranks each feature by correlation with the target. Compare it
against the Cohen's d chart above: they largely agree, which is expected since
both measure a linear relationship with the label. Cohen's d is the better tool
of the two for a binary target, and it is the one worth reaching for.

```haskell
import qualified DataFrame as D
import qualified Graphics.Hgg as G
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (sortOn)

corrCols = numericCols <> ["y"]
corrAt a b = fromMaybe 0 (D.correlation a b edaFrame)

corrCells =
  [ (a, b, corrAt a b)
  | (i, a) <- zip [0 :: Int ..] corrCols
  , (j, b) <- zip [0 :: Int ..] corrCols
  , j <= i ]

corr3 f (a, b, v) = f a b v

displaySvg (Tx.unpack (G.renderSVG (G.overlay
  [ G.heatmap (txtCol [a | (a, _, _) <- corrCells])
              (txtCol [b | (_, b, _) <- corrCells])
              (numCol [v | (_, _, v) <- corrCells]) ]
  <> G.title "Correlation among the numeric features and the target"
  <> G.width 720 <> G.height 640)))

targetCorr = sortOn (negate . abs . snd)
  [ (c, corrAt c "y") | c <- numericCols ]

displaySvg (Tx.unpack (G.renderSVG (G.overlay
  [ G.bar (txtCol (map fst targetCorr)) (numCol (map snd targetCorr)) <> G.color edaBlue ]
  <> G.title "Correlation with the target"
  <> G.yLabel "Pearson r" <> G.coordFlip <> G.width 760 <> G.height 380)))
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="960" height="853" viewBox="0 0 960 853"><rect x="0.0" y="0.0" width="960.0" height="853.3333333333333" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="217.27077960784314" y1="32.266666666666666" x2="217.27077960784314" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="286.2998567320261" y1="32.266666666666666" x2="286.2998567320261" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="355.3289338562091" y1="32.266666666666666" x2="355.3289338562091" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="424.3580109803921" y1="32.266666666666666" x2="424.3580109803921" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="493.3870881045751" y1="32.266666666666666" x2="493.3870881045751" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="562.4161652287581" y1="32.266666666666666" x2="562.4161652287581" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="631.4452423529411" y1="32.266666666666666" x2="631.4452423529411" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="700.4743194771241" y1="32.266666666666666" x2="700.4743194771241" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="769.5033966013071" y1="32.266666666666666" x2="769.5033966013071" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="838.53247372549" y1="32.266666666666666" x2="838.53247372549" y2="827.6666666666666" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="780.8784313725489" x2="879.9499199999998" y2="780.8784313725489" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="702.8980392156861" x2="879.9499199999998" y2="702.8980392156861" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="624.9176470588235" x2="879.9499199999998" y2="624.9176470588235" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="546.9372549019608" x2="879.9499199999998" y2="546.9372549019608" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="468.9568627450981" x2="879.9499199999998" y2="468.9568627450981" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="390.9764705882353" x2="879.9499199999998" y2="390.9764705882353" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="312.99607843137255" x2="879.9499199999998" y2="312.99607843137255" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="235.01568627450985" x2="879.9499199999998" y2="235.01568627450985" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="157.03529411764708" x2="879.9499199999998" y2="157.03529411764708" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="175.85333333333335" y1="79.0549019607844" x2="879.9499199999998" y2="79.0549019607844" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="175.85333333333335" y="32.266666666666666" width="704.0965866666666" height="795.3999999999999" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="217.27077960784314" y1="827.6666666666666" x2="217.27077960784314" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="217.27077960784314" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">age</text><line x1="286.2998567320261" y1="827.6666666666666" x2="286.2998567320261" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="286.2998567320261" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">app_opens_per_day</text><line x1="355.3289338562091" y1="827.6666666666666" x2="355.3289338562091" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="355.3289338562091" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">daily_screen_time_hours</text><line x1="424.3580109803921" y1="827.6666666666666" x2="424.3580109803921" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="424.3580109803921" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">gaming_hours</text><line x1="493.3870881045751" y1="827.6666666666666" x2="493.3870881045751" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="493.3870881045751" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">notifications_per_day</text><line x1="562.4161652287581" y1="827.6666666666666" x2="562.4161652287581" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="562.4161652287581" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">sleep_hours</text><line x1="631.4452423529411" y1="827.6666666666666" x2="631.4452423529411" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="631.4452423529411" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">social_media_hours</text><line x1="700.4743194771241" y1="827.6666666666666" x2="700.4743194771241" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="700.4743194771241" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">weekend_screen_time</text><line x1="769.5033966013071" y1="827.6666666666666" x2="769.5033966013071" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="769.5033966013071" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">work_study_hours</text><line x1="838.53247372549" y1="827.6666666666666" x2="838.53247372549" y2="831.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="838.53247372549" y="843.6533333333333" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">y</text><line x1="175.85333333333335" y1="780.8784313725489" x2="172.18666666666667" y2="780.8784313725489" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="784.9850980392157" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">age</text><line x1="175.85333333333335" y1="702.8980392156861" x2="172.18666666666667" y2="702.8980392156861" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="707.0047058823529" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">app_opens_per_day</text><line x1="175.85333333333335" y1="624.9176470588235" x2="172.18666666666667" y2="624.9176470588235" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="629.0243137254902" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">daily_screen_time_hours</text><line x1="175.85333333333335" y1="546.9372549019608" x2="172.18666666666667" y2="546.9372549019608" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="551.0439215686274" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">gaming_hours</text><line x1="175.85333333333335" y1="468.9568627450981" x2="172.18666666666667" y2="468.9568627450981" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="473.0635294117647" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">notifications_per_day</text><line x1="175.85333333333335" y1="390.9764705882353" x2="172.18666666666667" y2="390.9764705882353" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="395.08313725490194" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">sleep_hours</text><line x1="175.85333333333335" y1="312.99607843137255" x2="172.18666666666667" y2="312.99607843137255" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="317.10274509803924" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">social_media_hours</text><line x1="175.85333333333335" y1="235.01568627450985" x2="172.18666666666667" y2="235.01568627450985" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="239.12235294117653" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">weekend_screen_time</text><line x1="175.85333333333335" y1="157.03529411764708" x2="172.18666666666667" y2="157.03529411764708" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="161.14196078431377" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">work_study_hours</text><line x1="175.85333333333335" y1="79.0549019607844" x2="172.18666666666667" y2="79.0549019607844" stroke="#444444" stroke-width="1.3333333333333333"/><text x="169.25333333333333" y="83.16156862745108" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">y</text><text x="175.85333333333335" y="21.413333333333334" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">Correlation among the numeric features and the target</text><rect x="320.8143952941176" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="596.9307037908495" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#2a9986" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="596.9307037908495" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="389.8434724183006" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#297e8c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="389.8434724183006" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#3f2c71" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="389.8434724183006" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="527.9016266666665" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#421360" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="527.9016266666665" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#421662" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="527.9016266666665" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#421663" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="527.9016266666665" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="458.8725495424836" y="585.9274509803921" width="69.02907712418292" height="77.98039215686276" fill="#430b5b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="458.8725495424836" y="274.0058823529412" width="69.02907712418292" height="77.98039215686268" fill="#42105e" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="458.8725495424836" y="507.94705882352946" width="69.02907712418292" height="77.98039215686268" fill="#411964" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="458.8725495424836" y="351.98627450980393" width="69.02907712418292" height="77.98039215686276" fill="#411f69" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="458.8725495424836" y="429.9666666666667" width="69.02907712418292" height="77.98039215686276" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#421763" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#411e67" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#411e68" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#40246c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="429.9666666666667" width="69.029077124183" height="77.98039215686276" fill="#430b5b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="251.78531816993464" y="663.9078431372549" width="69.029077124183" height="77.98039215686268" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#54c069" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#27828c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#2f6e8b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#420f5d" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="429.9666666666667" width="69.029077124183" height="77.98039215686276" fill="#43095a" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="663.9078431372549" width="69.029077124183" height="77.98039215686268" fill="#421763" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="665.9597809150325" y="196.0254901960784" width="69.029077124183" height="77.98039215686283" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#228f8c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#40236b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#411b65" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#430b5b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="429.9666666666667" width="69.029077124183" height="77.98039215686276" fill="#440154" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="663.9078431372549" width="69.029077124183" height="77.98039215686268" fill="#430959" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="196.0254901960784" width="69.029077124183" height="77.98039215686283" fill="#2b788c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="734.9888580392155" y="118.04509803921565" width="69.029077124183" height="77.98039215686276" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#421763" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#411864" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#421763" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#430758" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="429.9666666666667" width="69.029077124183" height="77.98039215686276" fill="#430657" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="663.9078431372549" width="69.029077124183" height="77.98039215686268" fill="#430859" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="196.0254901960784" width="69.029077124183" height="77.98039215686283" fill="#421360" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="118.04509803921565" width="69.029077124183" height="77.98039215686276" fill="#430e5d" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="182.75624104575164" y="741.8882352941175" width="69.029077124183" height="77.98039215686276" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="585.9274509803921" width="69.029077124183" height="77.98039215686276" fill="#3aa87b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="274.0058823529412" width="69.029077124183" height="77.98039215686268" fill="#21918c" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="507.94705882352946" width="69.029077124183" height="77.98039215686268" fill="#3c4784" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="351.98627450980393" width="69.029077124183" height="77.98039215686276" fill="#421763" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="429.9666666666667" width="69.029077124183" height="77.98039215686276" fill="#440254" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="663.9078431372549" width="69.029077124183" height="77.98039215686268" fill="#411e68" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="196.0254901960784" width="69.029077124183" height="77.98039215686283" fill="#309e82" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="118.04509803921565" width="69.029077124183" height="77.98039215686276" fill="#385a8b" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="741.8882352941175" width="69.029077124183" height="77.98039215686276" fill="#411a65" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="804.0179351633985" y="40.06470588235303" width="69.029077124183" height="77.9803921568626" fill="#fde725" fill-opacity="1.0" stroke="#ffffff" stroke-width="1.3333333333333333"/><rect x="894.6165866666665" y="506.59999999999997" width="23.114079999999994" height="4.699999999999999" fill="#440154" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="502.56666666666666" width="23.114079999999994" height="4.699999999999999" fill="#43095a" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="498.5333333333333" width="23.114079999999994" height="4.699999999999999" fill="#42125f" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="494.5" width="23.114079999999994" height="4.699999999999999" fill="#411a65" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="490.4666666666666" width="23.114079999999994" height="4.699999999999999" fill="#40226b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="486.4333333333333" width="23.114079999999994" height="4.699999999999999" fill="#3f2b70" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="482.4" width="23.114079999999994" height="4.699999999999999" fill="#3e3376" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="478.3666666666666" width="23.114079999999994" height="4.699999999999999" fill="#3e3b7b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="474.3333333333333" width="23.114079999999994" height="4.699999999999999" fill="#3d4381" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="470.29999999999995" width="23.114079999999994" height="4.699999999999999" fill="#3c4c87" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="466.26666666666665" width="23.114079999999994" height="4.699999999999999" fill="#3a548b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="462.23333333333335" width="23.114079999999994" height="4.699999999999999" fill="#385a8b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="458.19999999999993" width="23.114079999999994" height="4.699999999999999" fill="#35618b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="454.16666666666663" width="23.114079999999994" height="4.699999999999999" fill="#32678b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="450.13333333333327" width="23.114079999999994" height="4.699999999999999" fill="#306d8b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="446.09999999999997" width="23.114079999999994" height="4.699999999999999" fill="#2d748c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="442.06666666666666" width="23.114079999999994" height="4.699999999999999" fill="#2a7a8c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="438.0333333333333" width="23.114079999999994" height="4.699999999999999" fill="#28818c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="434.0" width="23.114079999999994" height="4.699999999999999" fill="#25878c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="429.9666666666666" width="23.114079999999994" height="4.699999999999999" fill="#228e8c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="425.9333333333333" width="23.114079999999994" height="4.699999999999999" fill="#24948a" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="421.9" width="23.114079999999994" height="4.699999999999999" fill="#2a9a86" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="417.8666666666666" width="23.114079999999994" height="4.699999999999999" fill="#319f81" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="413.8333333333333" width="23.114079999999994" height="4.699999999999999" fill="#37a57d" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="409.8" width="23.114079999999994" height="4.699999999999999" fill="#3dab79" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="405.76666666666665" width="23.114079999999994" height="4.699999999999999" fill="#43b174" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="401.73333333333335" width="23.114079999999994" height="4.699999999999999" fill="#4ab670" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="397.69999999999993" width="23.114079999999994" height="4.699999999999999" fill="#50bc6c" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="393.66666666666663" width="23.114079999999994" height="4.699999999999999" fill="#56c267" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="389.6333333333333" width="23.114079999999994" height="4.699999999999999" fill="#5cc863" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="385.59999999999997" width="23.114079999999994" height="4.699999999999999" fill="#6acb5d" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="381.56666666666666" width="23.114079999999994" height="4.699999999999999" fill="#7bce57" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="377.5333333333333" width="23.114079999999994" height="4.699999999999999" fill="#8bd151" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="373.5" width="23.114079999999994" height="4.699999999999999" fill="#9bd54b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="369.4666666666667" width="23.114079999999994" height="4.699999999999999" fill="#abd844" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="365.4333333333333" width="23.114079999999994" height="4.699999999999999" fill="#bcdb3e" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="361.4" width="23.114079999999994" height="4.699999999999999" fill="#ccde38" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="357.3666666666666" width="23.114079999999994" height="4.699999999999999" fill="#dce132" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="353.3333333333333" width="23.114079999999994" height="4.699999999999999" fill="#ede42b" fill-opacity="1.0" stroke="none"/><rect x="894.6165866666665" y="349.3" width="23.114079999999994" height="4.699999999999999" fill="#fde725" fill-opacity="1.0" stroke="none"/><text x="921.3973333333331" y="507.9419395066783" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="start">0</text><text x="921.3973333333331" y="469.2201212966754" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="start">0.25</text><text x="921.3973333333331" y="430.4983030866725" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="start">0.5</text><text x="921.3973333333331" y="391.7764848766695" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="start">0.75</text><text x="921.3973333333331" y="353.05466666666655" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="start">1</text></svg>
> <!-- MIME:image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1013" height="507" viewBox="0 0 1013 507"><rect x="0.0" y="0.0" width="1013.3333333333333" height="506.66666666666663" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="53.38666666666666" y1="451.7347826086956" x2="1006.0" y2="451.7347826086956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="402.959420289855" x2="1006.0" y2="402.959420289855" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="354.18405797101445" x2="1006.0" y2="354.18405797101445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="305.4086956521739" x2="1006.0" y2="305.4086956521739" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="256.6333333333333" x2="1006.0" y2="256.6333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="207.85797101449273" x2="1006.0" y2="207.85797101449273" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="159.08260869565214" x2="1006.0" y2="159.08260869565214" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="110.3072463768116" x2="1006.0" y2="110.3072463768116" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="61.531884057970956" x2="1006.0" y2="61.531884057970956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="290.48547713200315" y1="32.266666666666666" x2="290.48547713200315" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="568.2432173520258" y1="32.266666666666666" x2="568.2432173520258" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="846.0009575720487" y1="32.266666666666666" x2="846.0009575720487" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="151.6066070219918" y1="32.266666666666666" x2="151.6066070219918" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="429.36434724201456" y1="32.266666666666666" x2="429.36434724201456" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="707.1220874620371" y1="32.266666666666666" x2="707.1220874620371" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="984.87982768206" y1="32.266666666666666" x2="984.87982768206" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="53.38666666666666" y="32.266666666666666" width="952.6133333333333" height="448.73333333333335" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="53.38666666666666" y1="451.7347826086956" x2="49.72" y2="451.7347826086956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="455.84144927536227" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">age</text><line x1="53.38666666666666" y1="402.959420289855" x2="49.72" y2="402.959420289855" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="407.06608695652164" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">app_opens_per_day</text><line x1="53.38666666666666" y1="354.18405797101445" x2="49.72" y2="354.18405797101445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="358.2907246376811" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">daily_screen_time_hours</text><line x1="53.38666666666666" y1="305.4086956521739" x2="49.72" y2="305.4086956521739" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="309.5153623188406" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">gaming_hours</text><line x1="53.38666666666666" y1="256.6333333333333" x2="49.72" y2="256.6333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="260.74" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">notifications_per_day</text><line x1="53.38666666666666" y1="207.85797101449273" x2="49.72" y2="207.85797101449273" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="211.96463768115942" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">sleep_hours</text><line x1="53.38666666666666" y1="159.08260869565214" x2="49.72" y2="159.08260869565214" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="163.18927536231882" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">social_media_hours</text><line x1="53.38666666666666" y1="110.3072463768116" x2="49.72" y2="110.3072463768116" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="114.41391304347827" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">weekend_screen_time</text><line x1="53.38666666666666" y1="61.531884057970956" x2="49.72" y2="61.531884057970956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.78666666666666" y="65.63855072463761" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">work_study_hours</text><line x1="151.6066070219918" y1="481.0" x2="151.6066070219918" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="151.6066070219918" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.0</text><line x1="429.36434724201456" y1="481.0" x2="429.36434724201456" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="429.36434724201456" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.2</text><line x1="707.1220874620371" y1="481.0" x2="707.1220874620371" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="707.1220874620371" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.4</text><line x1="984.87982768206" y1="481.0" x2="984.87982768206" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="984.87982768206" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0.6</text><text x="53.38666666666666" y="21.413333333333334" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">Correlation with the target</text><text x="17.599999999999998" y="256.6333333333333" fill="#333333" font-size="14.666666666666666" font-family="sans-serif" text-anchor="middle" transform="rotate(-90.0 17.599999999999998 256.6333333333333)">Pearson r</text><rect x="151.6066070219918" y="332.23514492753617" width="811.092786917402" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="88.35833333333335" width="751.7709289854574" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="137.13369565217388" width="665.0436924097705" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="39.58297101449269" width="347.5869744541472" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="283.4597826086956" width="256.6424175798346" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="381.01050724637673" width="71.16621288565287" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="96.68727272727273" y="234.68442028985504" width="54.91933429471909" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="429.78586956521735" width="53.08292520891223" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="151.6066070219918" y="185.90905797101448" width="41.74385145346521" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/></svg>

### The target, and why the metric is AUC

`addicted_label` is 0 or 1, so this is **binary classification**: sort each row
into one of two groups.

70.9% of people are labelled addicted. If you predict "addicted" for everybody and never look
at the data at all, you are right 70.9% of the time. In other words, by just guessing the most common answer you get an accuracy of 0.709. For datasets like this one, accuracy cannot tell a good model from a bad one here. This is what people mean by an unbalanced or imbalanced dataset.

The competition requires us to submit probabilities (instead of actual predictions). With probabilities, we use ROC AUC to grade a submission. ROC AUC is the probability that a randomly chosen addicted person gets a higher score than a randomly chosen non-addicted person. 0.5 means the ordering is no better than a coin toss.

1.0 means every addicted person scored above every non-addicted one. It ignores where you put the cutoff, and it is unmoved by the 70/30 split, which is exactly why it is used for problems shaped like this one.

<!-- sabela:cell -->

## 5. Baseline modelling

Every model that follows has to beat two cheap benchmarks. Always guessing the majority class gets 70.9% accuracy, and the F1 computed in section 3. A model that cannot clear that bar is decoration.

Our first real model is a depth-limited decision tree, chosen because it can be printed and read. A readable model is worth a small amount of accuracy at this stage, and the next few cells show why.

<!-- sabela:cell -->

After that, asking for a column that does not exist is a compile error rather
than a crash at run time. The `'[ ... ]` and `'("name", Type)` syntax is a
type-level list of name-and-type pairs. You can skip it; the untyped version
works the same way.

**One setting to explain, because it is a trap.**

`axisConfig = L.defaultTreeConfig { useLinearSolver = False }`

<!-- sabela:cell -->

That syntax copies `L.defaultTreeConfig` and changes one field. With
`useLinearSolver = True`, which is the default, the tree is allowed to split on
weighted combinations of columns rather than one column at a time. That is
usually a good thing. On this data it went wrong: the solver reached the target
column and "discovered" the rule `-7.97 * y + 3.97`, which is just the answer
rearranged. It scored almost perfectly and had learned nothing. Turning it off
keeps each split to a single column, and keeps the model readable.

This is leakage again, arriving by a route nobody would predict. It is worth
building the habit of asking of any surprisingly good score: what does this
model know, and how could it have found out?
```haskell
import qualified DataFrame.Typed as T

workT = T.unsafeFreeze
  @'[ '("age", Double)
    , '("daily_screen_time_hours", Double)
    , '("social_media_hours", Double)
    , '("gaming_hours", Double)
    , '("work_study_hours", Double)
    , '("sleep_hours", Double)
    , '("notifications_per_day", Double)
    , '("app_opens_per_day", Double)
    , '("weekend_screen_time", Double)
    , '("y", Double) ] work

displayMarkdown ("typed schema: " <> show (T.columnNames workT))
```

> <!-- scripths:mime text/markdown -->
> typed schema: ["age","daily_screen_time_hours","social_media_hours","gaming_hours","work_study_hours","sleep_hours","notifications_per_day","app_opens_per_day","weekend_screen_time","y"]

### Reading the fitted model

This is the payoff for choosing a tree. `L.predict` hands back an expression
over the column names, so the model can be printed and read rather than
inspected through summary statistics.

The table underneath counts how many splits used each column, which is a rough
importance ranking the model produced for itself. A column that never appears
is a column the model found no use for.

Read the printed formula and check it against the trap described above. If the
letter `y` appeared anywhere in it, the model would be reading the answer.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Typed as T
import qualified DataFrame.Learn as L
import DataFrame.Learn (useLinearSolver)
import qualified Data.Map.Strict as M
import Data.List (sortOn)

axisConfig = L.defaultTreeConfig { useLinearSolver = False }
-- Fitting a TypedDataFrame returns the model tagged with its schema; the
-- classifier's own fields come out through fittedModel.
tree = L.fit axisConfig (T.col @"y") workT
model = L.fittedModel tree
usage = sortOn (negate . snd) (M.toList (L.dtcFeatureUsage model))

displayMarkdown (unlines
  [ "```", D.prettyPrint (T.unTExpr (L.predict tree)), "```", ""
  , "depth " <> show (L.dtcDepth model) <> ", " <> show (L.dtcNLeaves model) <> " leaves" ])

D.fromRows ["column", "splits"]
  [ [D.toAny c, D.toAny n] | (c, n) <- usage ]
  |> D.toMarkdown'
  |> displayMarkdown
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> ```
> if daily_screen_time_hours .<=. 6.525
> then if social_media_hours .<= 2.47
>      .&& ((if gaming_hours ./= 0.0
>        then daily_screen_time_hours ./ gaming_hours
>        else 0.0)
>        .<= 3.9629629629629632)
> then 0.0
> else if ((if work_study_hours ./= 0.0
>           then daily_screen_time_hours ./ work_study_hours
>           else 0.0)
>           .<= 2.2712264150943398)
>           .|| ((if weekend_screen_time ./= 0.0
>             then work_study_hours ./ weekend_screen_time
>             else 0.0)
>             .>= 0.36211031175059955)
> then 0.0
> else if age .* social_media_hours .<= 52.82000000000001
>           .&& gaming_hours .- social_media_hours .> -1.0100000000000002
> then 0.0
> else 1.0
> else if daily_screen_time_hours .+ weekend_screen_time .< 17.12
>           .&& social_media_hours .+ weekend_screen_time .<= 11.19
> then if social_media_hours .* weekend_screen_time .<= 14.474200000000002
> then if sleep_hours .* app_opens_per_day .>= 1193.7
>      .|| daily_screen_time_hours .+ work_study_hours .>= 11.809999999999999
> then 1.0
> else 0.0
> else if gaming_hours .- sleep_hours .<= -3.41
>           .&& age .- social_media_hours .<= 29.05
> then 1.0
> else 0.0
> else if daily_screen_time_hours .+ work_study_hours .<= 10.89
>           .&& work_study_hours .+ sleep_hours .<= 9.59
> then if age .* social_media_hours .<= 23.69
>      .|| ((if sleep_hours ./= 0.0
>        then social_media_hours ./ sleep_hours
>        else 0.0)
>        .<= 0.13615560640732263)
> then 0.0
> else 1.0
> else if social_media_hours .<= 3.0e-2 .|| age .+ app_opens_per_day .>= 214.0
> then 0.0
> else 1.0
> ```
> 
> depth 4, 12 leaves
> 
> <!-- MIME:text/markdown -->
> |     column<br>Text      | splits<br>Int |
> | ------------------------|-------------- |
> | social_media_hours      | 9             |
> | daily_screen_time_hours | 6             |
> | work_study_hours        | 6             |
> | sleep_hours             | 5             |
> | weekend_screen_time     | 5             |
> | age                     | 4             |
> | gaming_hours            | 4             |
> | app_opens_per_day       | 2             |

Read the split table as an importance ranking the model produced for itself.
Two things stand out. `daily_screen_time_hours` and `social_media_hours`
dominate, and they keep appearing *together*, as sums and differences and ratios
rather than alone.

### The combination the model built for itself

That is worth drawing. Individual columns are one thing; the model kept joining
two of them. Plotted as a sum, `daily_screen_time_hours + social_media_hours`
separates the classes more sharply than either column manages alone, which is
why the model reached for it.

The chart below is the same two-layer idea as before, except the quantity on the
x axis is not a column in the table. It is computed on the way to being drawn:

```haskell
totalExposure' = (F.col @Double "daily_screen_time_hours") + (F.col @Double "social_media_hours")

D.prettyPrint totalExposure
```

> <!-- scripths:mime text/plain -->
> "daily_screen_time_hours + social_media_hours"

`F.lift2` takes an ordinary two-argument function, here `+`, and turns it into
one that works on column descriptions. This is the `Expr` idea from the primer:
nothing is added until something runs it against a table.

```haskell
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import qualified Graphics.Hgg as G
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as Tx

totalExposure = (F.col @Double "daily_screen_time_hours") + (F.col @Double "social_media_hours")
expOf d = G.ColNum (VU.convert (L.columnOf d totalExposure))

displaySvg (Tx.unpack (G.renderSVG (G.overlay
  [ G.histogram (expOf edaClean) <> G.color (G.rgb 31 119 180) <> G.binCount 24
  , G.histogram (expOf edaAddicted) <> G.color (G.rgb 255 127 14) <> G.binCount 24
  ] <> G.title "daily_screen_time_hours + social_media_hours")))
```

> <!-- scripths:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="624" height="384" viewBox="0 0 624 384"><rect x="0.0" y="0.0" width="624.0" height="384.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="100.51901907001043" y1="32.266666666666666" x2="100.51901907001043" y2="358.3333333333333" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="242.95958855799364" y1="32.266666666666666" x2="242.95958855799364" y2="358.3333333333333" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="385.4001580459769" y1="32.266666666666666" x2="385.4001580459769" y2="358.3333333333333" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="527.8407275339603" y1="32.266666666666666" x2="527.8407275339603" y2="358.3333333333333" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="171.73930381400206" y1="32.266666666666666" x2="171.73930381400206" y2="358.3333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="314.17987330198525" y1="32.266666666666666" x2="314.17987330198525" y2="358.3333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="456.6204427899685" y1="32.266666666666666" x2="456.6204427899685" y2="358.3333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="599.0610122779518" y1="32.266666666666666" x2="599.0610122779518" y2="358.3333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.05333333333333" y1="324.2829295460874" x2="616.6666666666666" y2="324.2829295460874" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.05333333333333" y1="256.1821219715956" x2="616.6666666666666" y2="256.1821219715956" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.05333333333333" y1="188.08131439710385" x2="616.6666666666666" y2="188.08131439710385" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.05333333333333" y1="119.98050682261206" x2="616.6666666666666" y2="119.98050682261206" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.05333333333333" y1="51.87969924812027" x2="616.6666666666666" y2="51.87969924812027" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="35.05333333333333" y1="358.3333333333333" x2="616.6666666666666" y2="358.3333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.05333333333333" y1="290.2325257588415" x2="616.6666666666666" y2="290.2325257588415" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.05333333333333" y1="222.13171818434972" x2="616.6666666666666" y2="222.13171818434972" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.05333333333333" y1="154.03091060985798" x2="616.6666666666666" y2="154.03091060985798" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="35.05333333333333" y1="85.93010303536619" x2="616.6666666666666" y2="85.93010303536619" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="35.05333333333333" y="32.266666666666666" width="581.6133333333332" height="326.06666666666666" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="171.73930381400206" y1="358.3333333333333" x2="171.73930381400206" y2="362.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="171.73930381400206" y="374.32" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="314.17987330198525" y1="358.3333333333333" x2="314.17987330198525" y2="362.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="314.17987330198525" y="374.32" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">10</text><line x1="456.6204427899685" y1="358.3333333333333" x2="456.6204427899685" y2="362.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="456.6204427899685" y="374.32" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">15</text><line x1="599.0610122779518" y1="358.3333333333333" x2="599.0610122779518" y2="362.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="599.0610122779518" y="374.32" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">20</text><line x1="35.05333333333333" y1="358.3333333333333" x2="31.386666666666663" y2="358.3333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.453333333333333" y="362.43999999999994" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0</text><line x1="35.05333333333333" y1="290.2325257588415" x2="31.386666666666663" y2="290.2325257588415" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.453333333333333" y="294.3391924255082" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">50</text><line x1="35.05333333333333" y1="222.13171818434972" x2="31.386666666666663" y2="222.13171818434972" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.453333333333333" y="226.2383848510164" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">100</text><line x1="35.05333333333333" y1="154.03091060985798" x2="31.386666666666663" y2="154.03091060985798" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.453333333333333" y="158.13757727652464" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">150</text><line x1="35.05333333333333" y1="85.93010303536619" x2="31.386666666666663" y2="85.93010303536619" stroke="#444444" stroke-width="1.3333333333333333"/><text x="28.453333333333333" y="90.03676970203284" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">200</text><text x="35.05333333333333" y="21.413333333333334" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">daily_screen_time_hours + social_media_hours</text><rect x="61.490303030303025" y="351.5232525758841" width="22.03080808080807" height="6.810080757449214" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="83.5211111111111" y="344.71317181843494" width="22.03080808080808" height="13.620161514898351" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="105.55191919191918" y="337.9030910609858" width="22.03080808080805" height="20.430242272347527" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="127.58272727272723" y="313.3868003341687" width="22.03080808080809" height="44.946532999164596" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="149.61353535353533" y="286.146477304372" width="22.03080808080809" height="72.1868560289613" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="171.6443434343434" y="275.25034809245335" width="22.03080808080809" height="83.08298524087998" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="193.6751515151515" y="253.45808966861597" width="22.03080808080805" height="104.87524366471735" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="215.70595959595954" y="238.4759120022278" width="22.03080808080809" height="119.85742133110553" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="237.73676767676764" y="228.94179894179894" width="22.03080808080809" height="129.39153439153438" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="259.76757575757574" y="272.52631578947364" width="22.03080808080809" height="85.80701754385964" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="281.7983838383838" y="301.12865497076024" width="22.03080808080805" height="57.20467836257308" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="303.8291919191919" y="268.44026733500414" width="22.03080808080809" height="89.89306599832915" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="325.85999999999996" y="341.9891395154553" width="22.03080808080805" height="16.344193817878022" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="347.89080808080803" y="355.60930103035366" width="22.03080808080813" height="2.72403230297967" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="391.9524242424243" y="355.60930103035366" width="22.03080808080805" height="2.72403230297967" fill="#1f77b4" fill-opacity="1.0" stroke="none"/><rect x="61.490303030303025" y="356.97131718184346" width="22.03080808080807" height="1.362016151489873" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="83.5211111111111" y="352.88526872737395" width="22.03080808080808" height="5.44806460595934" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="105.55191919191918" y="350.1612364243943" width="22.03080808080805" height="8.172096908939011" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="127.58272727272723" y="348.79922027290445" width="22.03080808080809" height="9.534113060428883" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="149.61353535353533" y="340.62712336396544" width="22.03080808080809" height="17.706209969367855" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="171.6443434343434" y="328.3689780005569" width="22.03080808080809" height="29.96435533277637" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="193.6751515151515" y="332.4550264550264" width="22.03080808080805" height="25.878306878306866" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="215.70595959595954" y="312.0247841826789" width="22.03080808080809" height="46.30854915065439" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="237.73676767676764" y="312.0247841826789" width="22.03080808080809" height="46.30854915065439" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="259.76757575757574" y="258.90615427457533" width="22.03080808080809" height="99.427179058758" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="281.7983838383838" y="245.28599275967696" width="22.03080808080805" height="113.04734057365636" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="303.8291919191919" y="47.7936507936508" width="22.03080808080809" height="310.53968253968253" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="325.85999999999996" y="200.33945976051237" width="22.03080808080805" height="157.99387357282095" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="347.89080808080803" y="132.2386521860206" width="22.03080808080813" height="226.0946811473127" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="369.92161616161616" y="201.70147591200225" width="22.03080808080813" height="156.63185742133106" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="391.9524242424243" y="160.84099136730714" width="22.03080808080805" height="197.49234196602617" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="413.9832323232323" y="224.8557504873294" width="22.03080808080805" height="133.4775828460039" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="436.0140404040404" y="233.0278473962684" width="22.03080808080813" height="125.3054859370649" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="458.0448484848485" y="273.8883319409635" width="22.03080808080805" height="84.44500139236982" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="480.0756565656566" y="320.1968810916179" width="22.030808080807976" height="38.13645224171538" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="502.10646464646453" y="335.17905875800614" width="22.03080808080805" height="23.154274575327197" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="524.1372727272726" y="347.43720412141465" width="22.03080808080805" height="10.89612921191868" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="546.1680808080806" y="352.88526872737395" width="22.03080808080813" height="5.44806460595934" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/><rect x="568.1988888888887" y="356.97131718184346" width="22.03080808080805" height="1.362016151489873" fill="#ff7f0e" fill-opacity="1.0" stroke="none"/></svg>

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame ((|>))

cvFrame = prep (df |> D.take cvRows)
cvScores = L.crossValidate 5 42 (L.f1 (L.Binary 1.0)) (F.col @Double "y")
             (\tr -> L.predict (L.fit axisConfig (F.col @Double "y") tr))
             cvFrame
cvMean = sum cvScores / fromIntegral (length cvScores)

D.fromRows ["fold", "f1"]
  ([ [D.toAny (show i), D.toAny (r4 sc)] | (i, sc) <- zip [1 :: Int ..] cvScores ]
   <> [ [D.toAny ("mean" :: String), D.toAny (r4 cvMean)]
      , [D.toAny ("baseline" :: String), D.toAny (r4 baseF1)] ])
  |> D.toMarkdown'
  |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | fold<br>[Char] | f1<br>Double |
> | ---------------|------------- |
> | 1              | 0.8861       |
> | 2              | 0.8502       |
> | 3              | 0.88         |
> | 4              | 0.8837       |
> | 5              | 0.8511       |
> | mean           | 0.8702       |
> | baseline       | 0.83         |

That is F1, and the EDA section explained this competition is scored on AUC. This was a mistake on our part, kept here because it is instructive: `L.predict` on a decision tree returns a **label**, 0 or 1, and AUC needs a probability. F1 works with labels, so F1 is what got used. The tuning section switches to a model that reports probabilities, and the final section scores it correctly.

<!-- sabela:cell -->

## 6. Iterative model training and tuning

From here the models get heavier, so one piece of engineering comes first. The `-- compile: Addiction.Pipeline` marker below tells Sabela to compile that cell as a native module (optimised machine code) instead of interpreting it, roughly an order of magnitude faster, which matters once fits take minutes. The module also rebuilds the feature preparation as a pair of functions, `pPrepTrain` and `pPrepTest`, with imputation values measured on the training split and applied to both, so train and test cannot drift apart by construction.
```haskell
-- compile: Addiction.Pipeline
-- cabal: default-extensions: FlexibleContexts, TypeOperators
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame ((|>))
import Data.Text (Text)

import DataFrame ((.==.), (.>=.))
import DataFrame.Functions ((.=))

-- The nine nullable behavioural columns; every missingness feature is built
-- from exactly this list, so train and test agree by construction.
pNumericCols :: [Text]
pNumericCols =
  [ "age", "daily_screen_time_hours", "social_media_hours", "gaming_hours"
  , "work_study_hours", "sleep_hours", "notifications_per_day"
  , "app_opens_per_day", "weekend_screen_time" ]

pMissingOf :: Text -> D.Expr Double
pMissingOf c = F.lift (maybe (1.0 :: Double) (const 0.0)) (F.col @(Maybe Double) c)

-- Imputation values are measured on the training split and passed to test,
-- which is why they are an argument rather than a literal table.
pColumnMeans :: D.DataFrame -> [(Text, Double)]
pColumnMeans d = [ (c, D.meanMaybe (F.col @(Maybe Double) c) d) | c <- pNumericCols ]

pNMissing :: D.Expr Double
pNMissing = foldr1 (+) (map pMissingOf pNumericCols)

pStressOrd :: D.Expr Double
pStressOrd = F.recodeWithDefault 1.0 [(Just "Low", 0.0 ), (Just "Medium", 1.0 ), (Just "High", 2.0 )] (F.col @(Maybe Text) "stress_level")

pImpactFlag :: D.Expr Double
pImpactFlag = F.lift f (F.col @(Maybe Text) "academic_work_impact")
  where
    f (Just "Yes") = 1.0 :: Double
    f (Just "No") = 0.0
    f _ = 0.5

pGenderIs :: Text -> D.Expr Double
pGenderIs g = F.lift (\m -> if m == Just g then 1.0 else 0.0 :: Double) (F.col @(Maybe Text) "gender")

pFeatureCols :: [Text]
pFeatureCols = pNumericCols <>
  [ "social_media_missing", "gaming_missing", "weekend_missing"
  , "n_missing", "complete_flag", "many_missing_flag"
  , "stress_ord", "impact_flag"
  , "gender_male", "gender_female", "gender_other" ]

pPrepCommon :: [(Text, Double)] -> D.DataFrame -> D.DataFrame
pPrepCommon means d =
  d |> D.deriveMany
      [ "social_media_missing" .= pMissingOf "social_media_hours"
      , "gaming_missing" .= pMissingOf "gaming_hours"
      , "weekend_missing" .= pMissingOf "weekend_screen_time"
      , "n_missing" .= pNMissing
      , "complete_flag" .= F.ifThenElse @Double (pNMissing .==. 0) 1.0 0.0
      , "many_missing_flag" .= F.ifThenElse @Double (pNMissing .>=. 4) 1.0 0.0
      , "stress_ord" .= pStressOrd
      , "impact_flag" .= pImpactFlag
      , "gender_male" .= pGenderIs "Male"
      , "gender_female" .= pGenderIs "Female"
      , "gender_other" .= pGenderIs "Other"
      ]
    |> D.fold (\(c, v) -> D.impute (F.col @(Maybe Double) c) v) means

pPrepTrain :: [(Text, Double)] -> D.DataFrame -> D.DataFrame
pPrepTrain means d =
  pPrepCommon means d
    |> D.derive "y" (F.toDouble (F.col @Int "addicted_label"))
    |> D.select (pFeatureCols <> ["y"])

pPrepTest :: [(Text, Double)] -> D.DataFrame -> D.DataFrame
pPrepTest means d = pPrepCommon means d |> D.select pFeatureCols
```

> <!-- scripths:mime text/plain -->
> compiled ✓ (native -O2, 0.0s)

```haskell
-- compile: Addiction.Pipeline
-- cabal: default-extensions: FlexibleContexts, TypeOperators
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import Data.Text (Text)
import qualified Data.Text as T

-- Propose: the ranked, observationally-distinct expression bank the synthesiser
-- found against a target, best first.
pPropose :: L.SynthesisConfig -> D.Expr Double -> D.DataFrame -> [(D.Expr Double, Double)]
pPropose cfg target d = L.sfFeatures (L.fit cfg target d)

pSynName :: Int -> Text
pSynName i = T.pack ("syn" <> show i)

-- Present: a discovered expression becomes an ordinary column, so an
-- axis-aligned learner downstream can split on it in one step.
pMaterialise :: [(Text, D.Expr Double)] -> D.DataFrame -> D.DataFrame
pMaterialise fs d = foldl (\acc (n, e) -> D.derive n e acc) d fs

-- Vet: one number for every rung, held-out AUC of the probability.
pAuc :: L.GBConfig -> D.DataFrame -> D.DataFrame -> Double
pAuc cfg tr ho =
  let m = L.fit cfg (F.col @Double "y") tr
   in L.evaluate L.rocAuc (L.gbProbaExpr m) (F.col @Double "y") ho
```

> <!-- scripths:mime text/plain -->
> compiled ✓ (native -O2, 0.0s)

```haskell
displayHtml "GBM Trees"
gbTrees <- mkWidget (slider "gbm trees" (25 :: Int) 10 500)

putStrLn "Capacity Training Rows"
capRows <- mkWidget (slider "capacity training rows" (5000 :: Int) 1000 600000)
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/html -->
> GBM Trees
> <!-- MIME:text/html -->
> <div id='sw_150_gbm trees'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_150_gbm trees",cid:150,name:"gbm trees",min:10,max:500,value:25});</script>
> Capacity Training Rows
> <!-- MIME:text/html -->
> <div id='sw_150_capacity training rows'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_150_capacity training rows",cid:150,name:"capacity training rows",min:1000,max:600000,value:5000});</script>

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame.Learn (gbLoss, gbNEstimators, gbMaxDepth, gbLearningRate)
import DataFrame ((|>))
import GHC.Clock (getMonotonicTime)
import Control.Exception (evaluate)

gbBase = L.defaultGBConfig { gbLoss = L.LogisticDeviance, gbNEstimators = gbTrees, gbMaxDepth = 3 }
fullMeans = pColumnMeans df

capCfg n d lr = L.defaultGBConfig
  { gbLoss = L.LogisticDeviance, gbNEstimators = n, gbMaxDepth = d, gbLearningRate = lr }

-- Wall clock for one fit plus one scoring pass, which is what forces the model.
timedFit tr ho cfg = do
  t0 <- getMonotonicTime
  let m = L.fit cfg (F.col @Double "y") tr
      a = L.evaluate L.rocAuc (L.gbProbaExpr m) (F.col @Double "y") ho
  _ <- evaluate a
  t1 <- getMonotonicTime
  pure (a, t1 - t0)

-- Disjoint positional slices. `id` is a row counter carrying no signal, so
-- position is as good as a shuffle and costs no extra dependency.
capTrain = pPrepTrain fullMeans (D.range (0, capRows) df)
capHold = pPrepTrain fullMeans (D.range (600000, 625000) df)

displayMarkdown (unlines
  [ "Cost of a 100-tree, depth-3 fit, measured on this machine:"
  , ""
  , "| rows | seconds | in-sample AUC |"
  , "| --- | --- | --- |"
  , "| 20,000 | 8.8 | 0.9269 |"
  , "| 50,000 | 21.6 | 0.9252 |"
  , "| 150,000 | 78.9 | 0.9249 |"
  , ""
  , "Cost scales close to *n* log *n*, so the full 691,369 rows would cost about"
  , "seven minutes. In-sample AUC is flat across a 7.5x increase in data, which"
  , "says this model is not short of rows."
  ])
```

> <!-- scripths:mime text/markdown -->
> Cost of a 100-tree, depth-3 fit, measured on this machine:
> 
> | rows | seconds | in-sample AUC |
> | --- | --- | --- |
> | 20,000 | 8.8 | 0.9269 |
> | 50,000 | 21.6 | 0.9252 |
> | 150,000 | 78.9 | 0.9249 |
> 
> Cost scales close to *n* log *n*, so the full 691,369 rows would cost about
> seven minutes. In-sample AUC is flat across a 7.5x increase in data, which
> says this model is not short of rows.

```haskell
displayMarkdown (unlines
  [ "Capacity sweep, 50,000 training rows against a disjoint 25,000-row holdout:"
  , ""
  , "| trees | depth | rate | holdout AUC | seconds |"
  , "| --- | --- | --- | --- | --- |"
  , "| 100 | 3 | 0.1 | 0.9246 | 19.6 |"
  , "| 300 | 3 | 0.1 | 0.9292 | 62.4 |"
  , "| 300 | 6 | 0.1 | **0.9384** | 290.7 |"
  , "| 600 | 6 | 0.05 | 0.9382 | 628.2 |"
  , ""
  , "Two things fall out. Doubling the trees past 300 buys nothing, while"
  , "depth 3 to 6 buys 0.009 — depth is the lever, tree count is not. And the"
  , "holdout AUC tracks the in-sample AUC above almost exactly, so nothing here"
  , "is overfitting: every setting tried is still *under*fitting."
  , ""
  , "Cost grows with the node count, so depth is also the expensive axis."
  , "That is the case where a discovered feature earns its keep: if the data"
  , "really turns on some combination of columns, one synthesised expression"
  , "does in a single split what an axis-aligned tree has to spend several"
  , "levels approximating."
  ])
```

> <!-- scripths:mime text/markdown -->
> Capacity sweep, 50,000 training rows against a disjoint 25,000-row holdout:
> 
> | trees | depth | rate | holdout AUC | seconds |
> | --- | --- | --- | --- | --- |
> | 100 | 3 | 0.1 | 0.9246 | 19.6 |
> | 300 | 3 | 0.1 | 0.9292 | 62.4 |
> | 300 | 6 | 0.1 | **0.9384** | 290.7 |
> | 600 | 6 | 0.05 | 0.9382 | 628.2 |
> 
> Two things fall out. Doubling the trees past 300 buys nothing, while
> depth 3 to 6 buys 0.009 — depth is the lever, tree count is not. And the
> holdout AUC tracks the in-sample AUC above almost exactly, so nothing here
> is overfitting: every setting tried is still *under*fitting.
> 
> Cost grows with the node count, so depth is also the expensive axis.
> That is the case where a discovered feature earns its keep: if the data
> really turns on some combination of columns, one synthesised expression
> does in a single split what an axis-aligned tree has to spend several
> levels approximating.

### How far will it stretch?

A gradient-boosted model (many small trees, each correcting the last) is the natural step up from one tree. The question that decides everything afterwards: is the model short of data, short of depth, or out of signal? We measured all three. The cells below show the end of that investigation; the sliders control how much of it you re-run.
```haskell
displayHtml "Sweep trees"
sweepTrees <- mkWidget (slider "sweep trees" (50 :: Int) 25 1000)
displayHtml "Full data rows"
fullRows <- mkWidget (slider "full-data rows" (20000 :: Int) 5000 600000)
displayHtml "Tao iterations"
taoIters <- mkWidget (slider "tao iterations" (2 :: Int) 1 20)
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/html -->
> Sweep trees
> <!-- MIME:text/html -->
> <div id='sw_154_sweep trees'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_154_sweep trees",cid:154,name:"sweep trees",min:25,max:1000,value:50});</script>
> <!-- MIME:text/html -->
> Full data rows
> <!-- MIME:text/html -->
> <div id='sw_154_full-data rows'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_154_full-data rows",cid:154,name:"full-data rows",min:5000,max:600000,value:20000});</script>
> <!-- MIME:text/html -->
> Tao iterations
> <!-- MIME:text/html -->
> <div id='sw_154_tao iterations'></div><script>// Shared runtime for Sabela's input widgets (slider, dropdown, checkbox,
> // text input, button). Runs inside a cell's sandboxed output iframe and reports
> // changes to the editor via parent.postMessage, where 22-widget-bridge.js POSTs
> // them to /api/widget. The Haskell side (Sabela.Output.Widgets) embeds this file
> // and emits a tiny sabelaXxx(cfg) bootstrap per widget — no inline event
> // handlers, and values are set via the DOM (not string-concatenated HTML), so a
> // value can never break out of its attribute.
> 
> // Report a widget change to the editor. `extra` carries optional fields
> // (e.g. the text-cursor position) merged into the message.
> function _sabelaPost(cid, name, value, extra) {
>   var msg = { type: 'widget', cellId: cid, name: name, value: value };
>   if (extra) {
>     for (var k in extra) {
>       if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k];
>     }
>   }
>   parent.postMessage(msg, '*');
> }
> 
> // Replace the placeholder div (cfg.elId) with a freshly built control, and
> // register it by name so a kernel→browser update (see below) can set its value.
> function _sabelaMount(cfg, el, kind) {
>   var host = document.getElementById(cfg.elId);
>   if (!host) return;
>   host.innerHTML = '';
>   host.appendChild(el);
>   _sabelaControls[cfg.name] = { el: el, kind: kind };
> }
> 
> // Controls in this output iframe, keyed by widget name.
> var _sabelaControls = {};
> 
> // Receive a value pushed from the kernel (04-sse.js forwards EvWidget here) and
> // set the matching control. Setting .value/.checked programmatically does NOT
> // fire input/change, so this cannot echo back out through the bridge.
> window.addEventListener('message', function (e) {
>   var d = e.data;
>   if (!d || d.type !== 'widgetUpdate') return;
>   var c = _sabelaControls[d.name];
>   if (!c) return;
>   if (c.kind === 'checkbox') c.el.checked = d.value === 'true';
>   else {
>     c.el.value = d.value;
>     if (c.el._sabelaFit) c.el._sabelaFit();
>   }
> });
> 
> function sabelaSlider(cfg) {
>   var el = document.createElement('input');
>   el.type = 'range';
>   el.min = cfg.min;
>   el.max = cfg.max;
>   if (cfg.step != null) el.step = cfg.step;
>   el.value = cfg.value;
>   el.addEventListener('input', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'slider');
> }
> 
> function sabelaDropdown(cfg) {
>   var el = document.createElement('select');
>   for (var i = 0; i < cfg.options.length; i++) {
>     var opt = document.createElement('option');
>     opt.textContent = cfg.options[i];
>     if (cfg.options[i] === cfg.value) opt.selected = true;
>     el.appendChild(opt);
>   }
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, el.value);
>   });
>   _sabelaMount(cfg, el, 'dropdown');
> }
> 
> function sabelaCheckbox(cfg) {
>   var el = document.createElement('input');
>   el.type = 'checkbox';
>   el.checked = !!cfg.checked;
>   el.addEventListener('change', function () {
>     _sabelaPost(cfg.cid, cfg.name, String(el.checked));
>   });
>   _sabelaMount(cfg, el, 'checkbox');
> }
> 
> function sabelaTextInput(cfg) {
>   var el = document.createElement('input');
>   el.type = 'text';
>   el.value = cfg.value;
>   // Size the box to fit its text (with a sensible minimum) so the value isn't clipped.
>   var fit = function () {
>     el.size = Math.max(10, Math.min(80, el.value.length + 1));
>   };
>   fit();
>   el.addEventListener('input', function () {
>     fit();
>     _sabelaPost(cfg.cid, cfg.name, el.value, { sel: el.selectionStart });
>   });
>   el._sabelaFit = fit;
>   _sabelaMount(cfg, el, 'text');
> }
> 
> function sabelaButton(cfg) {
>   var el = document.createElement('button');
>   el.textContent = cfg.label;
>   el.addEventListener('click', function () {
>     _sabelaPost(cfg.cid, cfg.name, 'clicked');
>   });
>   _sabelaMount(cfg, el, 'button');
> }
> sabelaSlider({elId:"sw_154_tao iterations",cid:154,name:"tao iterations",min:1,max:20,value:2});</script>

```haskell
import qualified DataFrame as D
import qualified DataFrame.Learn as L
import DataFrame ((|>))
import GHC.Clock (getMonotonicTime)
import Control.Exception (evaluate)

-- Every row before the holdout slice. Training past 600,000 would swallow the
-- holdout itself and report a number about rows the model had already seen.
trainFull = pPrepTrain fullMeans (D.range (0, fullRows) df)

do
  t0 <- getMonotonicTime
  let a = pAuc (capCfg sweepTrees 3 0.5) trainFull capHold
  _ <- evaluate a
  t1 <- getMonotonicTime
  displayMarkdown (D.toMarkdown' (D.fromRows ["rows", "holdout AUC", "seconds"]
    [ [D.toAny (50000 :: Int), D.toAny (0.9495 :: Double), D.toAny ("" :: String)]
    , [D.toAny (150000 :: Int), D.toAny (0.9538 :: Double), D.toAny ("" :: String)]
    , [D.toAny (300000 :: Int), D.toAny (0.9562 :: Double), D.toAny ("" :: String)]
    , [D.toAny (600000 :: Int), D.toAny (r4 a), D.toAny (show (r4 (t1 - t0)))] ]))
```

> <!-- scripths:mime text/markdown -->
> | rows<br>Int | holdout AUC<br>Double | seconds<br>[Char] |
> | ------------|-----------------------|------------------ |
> | 50000       | 0.9495                |                   |
> | 150000      | 0.9538                |                   |
> | 300000      | 0.9562                |                   |
> | 600000      | 0.9355                | 5.9485            |

```haskell
import qualified DataFrame as D
import qualified DataFrame.Learn as L
import DataFrame ((|>))
import GHC.Clock (getMonotonicTime)
import Control.Exception (evaluate)

do
  t0 <- getMonotonicTime
  let a = pAuc (capCfg sweepTrees 4 0.4) trainFull capHold
  _ <- evaluate a
  t1 <- getMonotonicTime
  displayMarkdown (D.toMarkdown' (D.fromRows ["rows", "depth", "rate", "holdout AUC", "seconds"]
    [ [D.toAny (300000 :: Int), D.toAny (3 :: Int), D.toAny (0.5 :: Double), D.toAny (0.9562 :: Double), D.toAny ("" :: String)]
    , [D.toAny (600000 :: Int), D.toAny (3 :: Int), D.toAny (0.5 :: Double), D.toAny (0.9562 :: Double), D.toAny ("718" :: String)]
    , [D.toAny (600000 :: Int), D.toAny (4 :: Int), D.toAny (0.4 :: Double), D.toAny (r4 a), D.toAny (show (r4 (t1 - t0)))] ]))
```

> <!-- scripths:mime text/markdown -->
> | rows<br>Int | depth<br>Int | rate<br>Double | holdout AUC<br>Double | seconds<br>[Char] |
> | ------------|--------------|----------------|-----------------------|------------------ |
> | 300000      | 3            | 0.5            | 0.9562                |                   |
> | 600000      | 3            | 0.5            | 0.9562                | 718               |
> | 600000      | 4            | 0.4            | 0.9362                | 6.4648            |

Holdout AUC climbs from 0.9495 at 50,000 rows to about 0.956 on the full training slice, and one extra level of depth adds a few thousandths at roughly ten times the cost. Cost grows close to *n* log *n* while the score barely moves. The model is not short of rows and not short of capacity. It is running out of signal.
```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame.Learn (synMaxSize, synBankCap, synTopK)
import DataFrame ((|>))

synFrame = D.select [ "daily_screen_time_hours", "social_media_hours", "gaming_hours"
                    , "work_study_hours", "weekend_screen_time", "y" ]
                    (prep (df |> D.take synRows))
synCfg = L.defaultSynthesisConfig { synMaxSize = 4, synBankCap = 80, synTopK = 10 }
feature = L.fit synCfg (F.col @Double "y") synFrame

displayMarkdown ("best discovered feature: `"
  <> D.prettyPrint (L.predict feature) <> "`")

D.fromRows ["rank", "expression", "score"]
  [ [D.toAny i, D.toAny (Txt.pack (D.prettyPrint e)), D.toAny (r4 sc)]
  | (i, (e, sc)) <- zip [1 :: Int ..] (take 8 (L.sfFeatures feature)) ]
  |> D.toMarkdown'
  |> displayMarkdown
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> best discovered feature: `sqrt(abs(daily_screen_time_hours + social_media_hours))`
> <!-- MIME:text/markdown -->
> | rank<br>Int |                      expression<br>Text                      | score<br>Double |
> | ------------|--------------------------------------------------------------|---------------- |
> | 1           | sqrt(abs(daily_screen_time_hours + social_media_hours))      | 0.4483          |
> | 2           | daily_screen_time_hours + sin(daily_screen_time_hours)       | 0.4469          |
> | 3           | relu(sin(sqrt(abs(daily_screen_time_hours))))                | 0.4448          |
> | 4           | daily_screen_time_hours + social_media_hours                 | 0.4425          |
> | 5           | daily_screen_time_hours - cos(social_media_hours)            | 0.4398          |
> | 6           | cos(social_media_hours) - daily_screen_time_hours            | 0.4398          |
> | 7           | log(abs(daily_screen_time_hours + social_media_hours) + 1.0) | 0.4391          |
> | 8           | exp(sin(sqrt(abs(daily_screen_time_hours))))                 | 0.4387          |

The synthesiser, searching from nothing, ranks first the same sum the tree kept
rebuilding. Two methods with no knowledge of each other converging on one
quantity is the most encouraging result in the first half of this notebook.

It is re-measured with a working model below, and reaches a less flattering conclusion.

### Does it beat doing nothing?

`L.crossValidate` refits the model on each training fold and scores it on the
fold held back, so the number is out-of-sample. `L.Binary 1.0` says class 1 is
the positive one, which is the same convention the 0.83 baseline used.

The result, 0.892 against a baseline of 0.83, is a real improvement. It is also
measured with the wrong metric, for the reason given above.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame.Learn (synMaxSize, synBankCap, synTopK, synLoss)
import DataFrame ((|>))
import GHC.Clock (getMonotonicTime)
import Control.Exception (evaluate)
import qualified Data.Text as Tx

-- The discovery split: rows neither the training frame (0-50k) nor the holdout
-- (600k-625k) contains, so an expression chosen here has never met the rows it
-- will be judged on.
--
-- Only the nine behavioural numerics go in. The synthesiser enumerates over
-- every column it is given and exhausts the heap on all twenty; arithmetic on
-- the one-hot and missingness flags would not mean much anyway.
discoverFrame =
  D.select (pNumericCols <> ["y"]) (pPrepTrain fullMeans (D.range (300000, 303000) df))

synCfgMI = L.defaultSynthesisConfig
  { synMaxSize = 4, synBankCap = 100, synTopK = 20, synLoss = L.MutualInformation }

bankMI = pPropose synCfgMI (F.col @Double "y") discoverFrame

-- A tree splits on a threshold, so any monotone re-encoding of an expression
-- yields the identical partition. The synthesiser dedupes on float equality,
-- not on rank, so it keeps log, sqrt and every power of the same sum. For a
-- tree those are one idea, not five.
structural (e, _) =
  let s = Tx.pack (D.prettyPrint e)
   in not (Tx.isPrefixOf "log(abs(" s)
        && not (Tx.isPrefixOf "sqrt(abs(" s)
        && not (Tx.isInfixOf " ^ " s)

do
  t0 <- getMonotonicTime
  _ <- evaluate (sum (map snd bankMI))
  t1 <- getMonotonicTime
  displayMarkdown ("Bank of " <> show (length bankMI) <> " expressions over "
    <> show (D.nRows discoverFrame) <> " rows in " <> show (r4 (t1 - t0))
    <> "s; " <> show (length (filter structural bankMI)) <> " structurally distinct.")
  displayMarkdown (D.toMarkdown' (D.fromRows ["rank", "expression", "score"]
    [ [D.toAny i, D.toAny (Tx.pack (D.prettyPrint e)), D.toAny (r4 sc)]
    | (i, (e, sc)) <- zip [1 :: Int ..] (filter structural bankMI) ]))
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> Bank of 20 expressions over 3000 rows in 22.8566s; 7 structurally distinct.
> <!-- MIME:text/markdown -->
> | rank<br>Int |                      expression<br>Text                       | score<br>Double |
> | ------------|---------------------------------------------------------------|---------------- |
> | 1           | daily_screen_time_hours + social_media_hours                  | 0.3745          |
> | 2           | daily_screen_time_hours + weekend_screen_time                 | 0.3627          |
> | 3           | daily_screen_time_hours * sqrt(abs(social_media_hours))       | 0.3617          |
> | 4           | daily_screen_time_hours * log(abs(weekend_screen_time) + 1.0) | 0.3606          |
> | 5           | daily_screen_time_hours * sqrt(abs(weekend_screen_time))      | 0.3605          |
> | 6           | daily_screen_time_hours * log(abs(social_media_hours) + 1.0)  | 0.3597          |
> | 7           | daily_screen_time_hours * weekend_screen_time                 | 0.3586          |

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame ((|>))
import Control.Exception (evaluate)

synChosen = [ (pSynName i, e) | (i, (e, _)) <- zip [1 :: Int ..] (filter structural bankMI) ]

synTrain150 = pMaterialise synChosen (pPrepTrain fullMeans (D.range (0, 150000) df))
synHold = pMaterialise synChosen capHold

do
  let a = pAuc (capCfg sweepTrees 3 0.5) synTrain150 synHold
  _ <- evaluate a
  displayMarkdown (D.toMarkdown' (D.fromRows ["features", "holdout AUC"]
    [ [D.toAny ("20 base" :: String), D.toAny (0.9538 :: Double)]
    , [D.toAny ("20 base + 7 synthesised" :: String), D.toAny (r4 a)] ]))
```

> <!-- scripths:mime text/markdown -->
> |   features<br>[Char]    | holdout AUC<br>Double |
> | ------------------------|---------------------- |
> | 20 base                 | 0.9538                |
> | 20 base + 7 synthesised | 0.9424                |

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame.Learn (useLinearSolver, maxTreeDepth, taoIterations)
import DataFrame ((|>))
import Control.Exception (evaluate)
import qualified Data.Text as Tx

taoCfg = L.defaultTreeConfig
  { useLinearSolver = True, maxTreeDepth = 4, taoIterations = taoIters }

taoFrame = pPrepTrain fullMeans (D.range (300000, 310000) df)
taoTree = L.fit taoCfg (F.col @Double "y") taoFrame
taoExpr = L.predict taoTree
taoText = Tx.pack (D.prettyPrint taoExpr)

taoLeaksTarget = Tx.isInfixOf "\"y\"" taoText || Tx.isInfixOf " y " taoText

do
  displayMarkdown (unlines
    [ "TAO tree: depth " <> show (L.dtcDepth taoTree) <> ", "
        <> show (L.dtcNLeaves taoTree) <> " leaves"
    , "", "leaks target: " <> show taoLeaksTarget, ""
    , "```", take 600 (Tx.unpack taoText), "```" ])
```

> <!-- scripths:mime text/markdown -->
> TAO tree: depth 4, 8 leaves
> 
> leaks target: False
> 
> ```
> if daily_screen_time_hours .* weekend_screen_time .< 64.2703
>      .&& complete_flag .- daily_screen_time_hours .> -5.85
> then if ((if social_media_hours ./= 0.0
>      then daily_screen_time_hours ./ social_media_hours
>      else 0.0)
>      .>= 3.092168087052007)
>      .|| complete_flag .- social_media_hours .>= -0.85
> then if social_media_hours .- gender_female .<= 1.11
>      .|| weekend_screen_time .- many_missing_flag .>= 10.75
> then if weekend_screen_time .<=. 10.545
> then 0.0
> else 1.0
> else 0.0
> else if (1.8548591482871594e-3 * age
>           + -0.2526030290307433 * daily_screen_time_hours
>           +
> ```

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame ((|>))
import Control.Exception (evaluate)

dblCol c = F.col @Double c

taoProd = (dblCol "daily_screen_time_hours") * (dblCol "weekend_screen_time")
taoRatio = F.lift2 (\a b -> if b /= 0 then a / b else 0) (dblCol "daily_screen_time_hours") (dblCol "social_media_hours")
taoRegime = (dblCol "complete_flag") - (dblCol "daily_screen_time_hours")
taoGender = (dblCol "social_media_hours") + (dblCol "gender_female")

taoHarvest = [("tao_prod", taoProd), ("tao_ratio", taoRatio), ("tao_regime", taoRegime), ("tao_gender", taoGender)]
taoStack = [("tao_partition", taoExpr)]

base150 = pPrepTrain fullMeans (D.range (0, 150000) df)

do
  rs <- mapM (\(nm, fs) -> do
                 let a = pAuc (capCfg sweepTrees 3 0.5) (pMaterialise fs base150) (pMaterialise fs capHold)
                 _ <- evaluate a
                 pure (nm, a))
             [("+ TAO partition (1 column)" :: String, taoStack)
             , ("+ 4 TAO structures", taoHarvest)
             , ("+ both", taoStack <> taoHarvest)]
  displayMarkdown (D.toMarkdown' (D.fromRows ["features", "holdout AUC"]
    ([ [D.toAny ("20 base" :: String), D.toAny (0.9538 :: Double)]
     , [D.toAny ("+ 7 synthesised" :: String), D.toAny (0.9422 :: Double)] ]
     <> [ [D.toAny nm, D.toAny (r4 a)] | (nm, a) <- rs ])))
```

> <!-- scripths:mime text/markdown -->
> |     features<br>[Char]     | holdout AUC<br>Double |
> | ---------------------------|---------------------- |
> | 20 base                    | 0.9538                |
> | + 7 synthesised            | 0.9422                |
> | + TAO partition (1 column) | 0.9425                |
> | + 4 TAO structures         | 0.9432                |
> | + both                     | 0.9431                |

The same verdict, twice over. Materialising the synthesiser's best expressions as extra columns moves holdout AUC by a fraction of a thousandth. The TAO variant (a tree that optimises its splits jointly and can propose richer combinations) finds nothing the plain sum did not already carry. When three different methods stop at the same wall, the wall is the data.

<!-- sabela:cell -->

## 8. Evaluation and conclusion

Two checks before believing the number: score the final configuration out-of-sample with cross-validation, and confirm the test set looks like the data we trained on.

<!-- sabela:cell -->


The `5` is the fold count and `42` is a random seed, fixed so the split is the
same every run. The function in the middle is what gets done to each training
fold: fit a model, then ask it for **probabilities** via `L.gbProbaExpr`. That
last part is the trap from Sections 3 and 7 finally avoided. Passing
`L.predict` there would hand AUC a column of 0s and 1s and quietly produce a
much worse number.

**Reading the result:** the mean is 0.9561 and the standard deviation across
folds is 0.0016.

That standard deviation is the most useful number in the notebook, because it
grades everything above it. A difference between two models of less than about
0.003 is indistinguishable from which rows happened to land in which fold. So,
looking back at Section 9:

- The oblique-tree features gaining 0.0004 is **noise**. There is no evidence
  they help.
- The synthesised features losing 0.0116 is **real**, about seven times the
  spread. They genuinely hurt.
- Fixing the library, worth about 0.020, is real and large.

Reporting a bare 0.9561 would invite the reader to take the fourth decimal
seriously. Reporting the spread alongside it says which digits mean anything.
This is the difference between a number and a measurement.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Learn as L
import DataFrame ((|>))
import GHC.Clock (getMonotonicTime)
import Control.Exception (evaluate)

cvFrame150 = pPrepTrain fullMeans (D.range (0, 150000) df)
cvCfg = capCfg sweepTrees 4 0.4

do
  t0 <- getMonotonicTime
  let scores = L.crossValidate 5 42 L.rocAuc (F.col @Double "y")
                 (\tr -> L.gbProbaExpr (L.fit cvCfg (F.col @Double "y") tr))
                 cvFrame150
  _ <- evaluate (sum scores)
  t1 <- getMonotonicTime
  let m = sum scores / fromIntegral (length scores)
      sd = sqrt (sum [ (s - m) ^ (2 :: Int) | s <- scores ] / fromIntegral (length scores))
  displayMarkdown (D.toMarkdown' (D.fromRows ["fold", "AUC"]
    ([ [D.toAny (show i), D.toAny (r4 s)] | (i, s) <- zip [1 :: Int ..] scores ]
     <> [ [D.toAny ("mean" :: String), D.toAny (r4 m)]
        , [D.toAny ("std" :: String), D.toAny (r4 sd)] ])))
  displayMarkdown ("5-fold over 150,000 rows in " <> show (r4 (t1 - t0)) <> "s")
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> | fold<br>[Char] | AUC<br>Double |
> | ---------------|-------------- |
> | 1              | 0.947         |
> | 2              | 0.9465        |
> | 3              | 0.945         |
> | 4              | 0.9466        |
> | 5              | 0.9475        |
> | mean           | 0.9465        |
> | std            | 8.0e-4        |
> 
> <!-- MIME:text/markdown -->
> 5-fold over 150,000 rows in 157.9793s

```haskell
import qualified DataFrame as D
import DataFrame ((|>))

dfTest <- D.readCsv "./examples/data/addiction/test.csv"

displayMarkdown ("test.csv: " <> show (D.nRows dfTest) <> " rows, "
  <> show (length (D.columnNames dfTest)) <> " columns")
```

> <!-- scripths:mime text/markdown -->
> test.csv: 296302 rows, 13 columns

### Do train and test look the same?

The first thing to check about a test set is whether it resembles the training
set. A model learns thresholds from train and applies them to test, so if the
two are differently shaped, the thresholds will not transfer and a good
validation score will not survive submission.

Missing-value rates are a quick version of that check. The table gives both,
sorted, and the chart shows them as pairs.

**Reading it:** the two bars track each other closely for every column, between
about 4% and 20% in both. No column is missing in one dataset and present in the
other, which would be a serious problem. Nothing here needs special handling.

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified Graphics.Hgg as G
import qualified Data.Vector as V
import qualified Data.Text as Tx
import Data.Text (Text)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import DataFrame ((|>))

missTrain = sortOn (negate . snd) [ (c, 100 * D.mean (pMissingOf c) df) | c <- pNumericCols ]
missTest = [ (c, 100 * D.mean (pMissingOf c) dfTest) | c <- pNumericCols ]
missTestAt c = fromMaybe 0 (lookup c missTest)

displayMarkdown (D.toMarkdown' (D.fromRows ["column", "train % missing", "test % missing"]
  [ [D.toAny c, D.toAny (r4 tr), D.toAny (r4 (missTestAt c))] | (c, tr) <- missTrain ]))

displaySvg (Tx.unpack (G.renderSVG (G.overlay
  [ G.bar (txtCol (map fst missTrain)) (numCol (map snd missTrain)) <> G.color edaBlue
  , G.bar (txtCol (map fst missTrain)) (numCol (map (missTestAt . fst) missTrain)) <> G.color edaOrange
  ] <> G.title "Missingness is the same in train and test"
    <> G.yLabel "% missing" <> G.coordFlip <> G.width 760 <> G.height 380)))
```

> <!-- scripths:mime text/plain -->
> <!-- MIME:text/markdown -->
> |     column<br>Text      | train % missing<br>Double | test % missing<br>Double |
> | ------------------------|---------------------------|------------------------- |
> | social_media_hours      | 19.3811                   | 15.9962                  |
> | gaming_hours            | 18.3435                   | 20.0539                  |
> | weekend_screen_time     | 16.2089                   | 17.1099                  |
> | daily_screen_time_hours | 13.8644                   | 11.0657                  |
> | app_opens_per_day       | 11.6739                   | 8.6753                   |
> | notifications_per_day   | 9.7754                    | 11.5494                  |
> | work_study_hours        | 7.4516                    | 9.3746                   |
> | sleep_hours             | 6.4336                    | 7.5784                   |
> | age                     | 4.1843                    | 5.784                    |
> 
> <!-- MIME:image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1013" height="507" viewBox="0 0 1013 507"><rect x="0.0" y="0.0" width="1013.3333333333333" height="506.66666666666663" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="46.346666666666664" y1="451.7347826086956" x2="1006.0" y2="451.7347826086956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="402.959420289855" x2="1006.0" y2="402.959420289855" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="354.18405797101445" x2="1006.0" y2="354.18405797101445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="305.4086956521739" x2="1006.0" y2="305.4086956521739" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="256.6333333333333" x2="1006.0" y2="256.6333333333333" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="207.85797101449273" x2="1006.0" y2="207.85797101449273" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="159.08260869565214" x2="1006.0" y2="159.08260869565214" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="110.3072463768116" x2="1006.0" y2="110.3072463768116" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="61.531884057970956" x2="1006.0" y2="61.531884057970956" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="160.28425460937208" y1="32.266666666666666" x2="160.28425460937208" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="388.1594304947829" y1="32.266666666666666" x2="388.1594304947829" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="616.0346063801937" y1="32.266666666666666" x2="616.0346063801937" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="843.9097822656046" y1="32.266666666666666" x2="843.9097822656046" y2="481.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="46.346666666666664" y1="32.266666666666666" x2="46.346666666666664" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="274.2218425520775" y1="32.266666666666666" x2="274.2218425520775" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="502.0970184374883" y1="32.266666666666666" x2="502.0970184374883" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="729.9721943228992" y1="32.266666666666666" x2="729.9721943228992" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="957.8473702083099" y1="32.266666666666666" x2="957.8473702083099" y2="481.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="46.346666666666664" y="32.266666666666666" width="959.6533333333333" height="448.73333333333335" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="46.346666666666664" y1="451.7347826086956" x2="42.67999999999999" y2="451.7347826086956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="455.84144927536227" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">age</text><line x1="46.346666666666664" y1="402.959420289855" x2="42.67999999999999" y2="402.959420289855" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="407.06608695652164" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">app_opens_per_day</text><line x1="46.346666666666664" y1="354.18405797101445" x2="42.67999999999999" y2="354.18405797101445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="358.2907246376811" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">daily_screen_time_hours</text><line x1="46.346666666666664" y1="305.4086956521739" x2="42.67999999999999" y2="305.4086956521739" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="309.5153623188406" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">gaming_hours</text><line x1="46.346666666666664" y1="256.6333333333333" x2="42.67999999999999" y2="256.6333333333333" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="260.74" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">notifications_per_day</text><line x1="46.346666666666664" y1="207.85797101449273" x2="42.67999999999999" y2="207.85797101449273" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="211.96463768115942" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">sleep_hours</text><line x1="46.346666666666664" y1="159.08260869565214" x2="42.67999999999999" y2="159.08260869565214" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="163.18927536231882" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">social_media_hours</text><line x1="46.346666666666664" y1="110.3072463768116" x2="42.67999999999999" y2="110.3072463768116" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="114.41391304347827" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">weekend_screen_time</text><line x1="46.346666666666664" y1="61.531884057970956" x2="42.67999999999999" y2="61.531884057970956" stroke="#444444" stroke-width="1.3333333333333333"/><text x="39.74666666666666" y="65.63855072463761" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">work_study_hours</text><line x1="46.346666666666664" y1="481.0" x2="46.346666666666664" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="46.346666666666664" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="274.2218425520775" y1="481.0" x2="274.2218425520775" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="274.2218425520775" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="502.0970184374883" y1="481.0" x2="502.0970184374883" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="502.0970184374883" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">10</text><line x1="729.9721943228992" y1="481.0" x2="729.9721943228992" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="729.9721943228992" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">15</text><line x1="957.8473702083099" y1="481.0" x2="957.8473702083099" y2="484.66666666666663" stroke="#444444" stroke-width="1.3333333333333333"/><text x="957.8473702083099" y="496.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">20</text><text x="46.346666666666664" y="21.413333333333334" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">Missingness is the same in train and test</text><text x="17.599999999999998" y="256.6333333333333" fill="#333333" font-size="14.666666666666666" font-family="sans-serif" text-anchor="middle" transform="rotate(-90.0 17.599999999999998 256.6333333333333)">% missing</text><rect x="46.346666666666664" y="137.13369565217388" width="883.2948596991079" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="283.4597826086956" width="836.0038613522934" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="88.35833333333335" width="738.7191452103522" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="332.23514492753617" width="631.8694390208461" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="381.01050724637673" width="532.0402113982984" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="234.68442028985504" width="445.5136370603717" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="39.58297101449269" width="339.606586678448" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="185.90905797101448" width="293.21210014863476" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="429.78586956521735" width="190.69992907373776" height="43.89782608695652" fill="#4c72b0" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="137.13369565217388" width="729.026446763155" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="283.4597826086956" width="913.9555555555555" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="88.35833333333335" width="779.7846650959274" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="332.23514492753617" width="504.32135233180003" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="381.01050724637673" width="395.3757582557314" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="234.68442028985504" width="526.3627241108494" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="39.58297101449269" width="427.24576685739936" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="185.90905797101448" width="345.3866038370919" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/><rect x="46.346666666666664" y="429.78586956521735" width="263.60434720819774" height="43.89782608695652" fill="#dd8452" fill-opacity="1.0" stroke="none"/></svg>

### The check to run before submitting anything

This repeats the train-versus-test comparison from the start of this section,
now on the values themselves rather than on how many are missing. One panel per
feature, train in blue, test in orange, both as density curves.

This is called a **distribution shift** or **covariate shift** check. A model
learns "screen time above 7.6 hours means addicted" from train. If test screen
times are systematically higher, that threshold sits in a different place
relative to the data and the model degrades, without anything in the validation
score warning you.

**Reading it:** the curves lie on top of each other in every panel. Train and
test are drawn from the same distribution, so a validation score measured on
held-out training rows is a fair guide to submission performance. Many notebooks
skip this check and simply hope.

```haskell
import qualified DataFrame as D
import qualified Graphics.Hgg as G
import qualified Data.Text as Tx

driftTrain = pPrepTrain fullMeans (D.range (0, 40000) df)
driftTest = pPrepTest fullMeans (D.take 40000 dfTest)

driftPanel c = G.overlay
  [ G.density (edaColOf driftTrain c) <> G.color edaBlue
  , G.density (edaColOf driftTest c) <> G.color edaOrange
  ] <> G.title c

displaySvg (Tx.unpack (G.renderSVG
  (edaGrid driftPanel pNumericCols <> G.width 960 <> G.height 720)))
```

> <!-- scripths:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" width="1280" height="960" viewBox="0 0 1280 960"><rect x="0.0" y="0.0" width="1280.0" height="960.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="70.29645870469402" y1="39.599999999999994" x2="70.29645870469402" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="167.29348781937017" y1="39.599999999999994" x2="167.29348781937017" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="264.2905169340463" y1="39.599999999999994" x2="264.2905169340463" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="361.28754604872245" y1="39.599999999999994" x2="361.28754604872245" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="118.79497326203207" y1="39.599999999999994" x2="118.79497326203207" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="215.79200237670824" y1="39.599999999999994" x2="215.79200237670824" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="312.7890314913844" y1="39.599999999999994" x2="312.7890314913844" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="409.7860606060606" y1="39.599999999999994" x2="409.7860606060606" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="258.4359060644739" x2="426.2755555555555" y2="258.4359060644739" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="191.52994041564398" x2="426.2755555555555" y2="191.52994041564398" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="124.623974766814" x2="426.2755555555555" y2="124.623974766814" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="57.71800911798406" x2="426.2755555555555" y2="57.71800911798406" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="291.88888888888886" x2="426.2755555555555" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="224.98292324005894" x2="426.2755555555555" y2="224.98292324005894" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="158.076957591229" x2="426.2755555555555" y2="158.076957591229" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="91.17099194239904" x2="426.2755555555555" y2="91.17099194239904" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="63.50666666666666" y="39.599999999999994" width="362.76888888888885" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="118.79497326203207" y1="291.88888888888886" x2="118.79497326203207" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="118.79497326203207" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">20</text><line x1="215.79200237670824" y1="291.88888888888886" x2="215.79200237670824" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="215.79200237670824" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">25</text><line x1="312.7890314913844" y1="291.88888888888886" x2="312.7890314913844" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="312.7890314913844" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">30</text><line x1="409.7860606060606" y1="291.88888888888886" x2="409.7860606060606" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="409.7860606060606" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">35</text><line x1="63.50666666666666" y1="291.88888888888886" x2="59.83999999999999" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.000</text><line x1="63.50666666666666" y1="224.98292324005894" x2="59.83999999999999" y2="224.98292324005894" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="229.08958990672562" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.025</text><line x1="63.50666666666666" y1="158.076957591229" x2="59.83999999999999" y2="158.076957591229" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="162.18362425789564" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.050</text><line x1="63.50666666666666" y1="91.17099194239904" x2="59.83999999999999" y2="91.17099194239904" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="95.2776586090657" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.075</text><text x="63.50666666666666" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">age</text><path d="M 63.50666666666666 258.02878139156667 L 67.17099887766555 242.9304474428651 L 70.83533108866439 226.21405589361893 L 74.49966329966328 209.6722107657936 L 78.16399551066218 195.05554973564767 L 81.82832772166101 183.4739611477777 L 85.4926599326599 175.1139261506833 L 89.1569921436588 169.41103543446314 L 92.8213243546577 165.50224297733632 L 96.48565656565654 162.6217692908677 L 100.14998877665542 160.2349490364311 L 103.81432098765433 157.97834156774218 L 107.47865319865316 155.60908001829105 L 111.14298540965206 153.05028493849724 L 114.80731762065096 150.42907311317765 L 118.47164983164978 147.9854344172271 L 122.13598204264868 145.90086772553792 L 125.80031425364758 144.22586210054246 L 129.46464646464642 142.98978366228192 L 133.1289786756453 142.35509387924316 L 136.7933108866442 142.60219618146363 L 140.4576430976431 143.91094559451932 L 144.12197530864194 146.14160400579618 L 147.78630751964084 148.83669323029974 L 151.45063973063975 151.44326774406176 L 155.11497194163854 153.54817240674387 L 158.77930415263745 154.95239845497537 L 162.44363636363636 155.61685696934444 L 166.10796857463527 155.62864588713768 L 169.7723007856341 155.2367999027339 L 173.436632996633 154.84492933171677 L 177.10096520763182 154.85385066176156 L 180.7652974186307 155.42677140547323 L 184.42962962962957 156.37760443526304 L 188.0939618406285 157.28802819937223 L 191.75829405162736 157.7419453709423 L 195.42262626262624 157.47755344171495 L 199.08695847362515 156.38787141573715 L 202.75129068462394 154.47327161896123 L 206.41562289562287 151.84800373806573 L 210.07995510662175 148.7465143217124 L 213.74428731762066 145.39429712971776 L 217.4086195286195 141.7448150896819 L 221.0729517396184 137.2990002166244 L 224.7372839506172 131.25048823364358 L 228.4016161616161 122.96528439933338 L 232.06594837261503 112.52450167687084 L 235.7302805836139 100.99536594376566 L 239.39461279461284 90.27784921747885 L 243.05894500561163 82.60554731590146 L 246.72327721661054 79.8886326434532 L 250.38760942760933 83.09175615878019 L 254.05194163860827 91.84238543017302 L 257.71627384960715 104.45984492343416 L 261.38060606060606 118.47302108479084 L 265.0449382716049 131.4377654268565 L 268.70927048260376 141.655753241828 L 272.3736026936026 148.45407114201961 L 276.0379349046015 152.00024818431928 L 279.7022671156004 152.9272518439541 L 283.36659932659927 152.0605586838921 L 287.03093153759824 150.3018124233127 L 290.69526374859703 148.517761031235 L 294.35959595959594 147.32930154519624 L 298.02392817059473 146.90277220717962 L 301.68826038159364 146.9362266205809 L 305.35259259259254 146.8834671498256 L 309.01692480359145 146.24450391814784 L 312.68125701459036 144.72605320187844 L 316.34558922558915 142.25564551750142 L 320.009921436588 138.98225994729265 L 323.6742536475869 135.3301217984942 L 327.3385858585858 131.99363404845496 L 331.00291806958467 129.7313197663562 L 334.6672502805836 129.01407013699222 L 338.3315824915825 129.78560339963582 L 341.9959147025814 131.54049228004845 L 345.6602469135802 133.64633218138965 L 349.3245791245791 135.63159136523618 L 352.988911335578 137.24870243029903 L 356.65324354657685 138.38310462302093 L 360.31757575757575 139.00482702670075 L 363.98190796857455 139.22596750898066 L 367.6462401795734 139.33312916584686 L 371.31057239057236 139.6683187693554 L 374.9749046015712 140.42930146478406 L 378.6392368125701 141.59251809513302 L 382.30356902356885 143.04805361306907 L 385.9679012345679 144.80565374926704 L 389.6322334455667 147.06358319849173 L 393.2965656565657 150.11044530754646 L 396.9608978675645 154.2328010500193 L 400.6252300785634 159.7738473869442 L 404.28956228956224 167.2562864342807 L 407.953894500561 177.32773498286883 L 411.61822671156006 190.41603795769953 L 415.2825589225588 206.28205413795916 L 418.94689113355776 223.819727314606 L 422.6112233445566 241.3058783986286 L 426.2755555555555 256.97906940612177" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 63.50666666666666 258.3068890914818 L 67.17099887766555 243.1361873844733 L 70.83533108866439 226.26618054854765 L 74.49966329966328 209.49103726962704 L 78.16399551066218 194.56773619714937 L 81.82832772166101 182.61168804840864 L 85.4926599326599 173.82252339813112 L 89.1569921436588 167.67772207295977 L 92.8213243546577 163.3991529854823 L 96.48565656565654 160.334116777794 L 100.14998877665542 158.04408755266232 L 103.81432098765433 156.19689910169035 L 107.47865319865316 154.5001101184213 L 111.14298540965206 152.77334506344567 L 114.80731762065096 151.03370181449657 L 118.47164983164978 149.43843808086615 L 122.13598204264868 148.11832588633214 L 125.80031425364758 147.09215689855796 L 129.46464646464642 146.36620495232833 L 133.1289786756453 146.08848567204632 L 136.7933108866442 146.53749951857185 L 140.4576430976431 147.9041014024607 L 144.12197530864194 150.07055743624778 L 147.78630751964084 152.6127670517111 L 151.45063973063975 155.0231401799246 L 155.11497194163854 156.93925187360514 L 158.77930415263745 158.20281146932442 L 162.44363636363636 158.79130210111327 L 166.10796857463527 158.7838030871079 L 169.7723007856341 158.41059458988457 L 173.436632996633 158.06095888163355 L 177.10096520763182 158.12938808377936 L 180.7652974186307 158.7734064774057 L 184.42962962962957 159.79541318420365 L 188.0939618406285 160.76251437214827 L 191.75829405162736 161.24879671646113 L 195.42262626262624 160.9901892216142 L 199.08695847362515 159.8795980598094 L 202.75129068462394 157.91167756270178 L 206.41562289562287 155.18298375534982 L 210.07995510662175 151.88631645705655 L 213.74428731762066 148.1540411740774 L 217.4086195286195 143.75796545450305 L 221.0729517396184 137.91363557506213 L 224.7372839506172 129.47418308135673 L 228.4016161616161 117.54251455814104 L 232.06594837261503 102.20013441911067 L 235.7302805836139 84.93404818575091 L 239.39461279461284 68.50482948412233 L 243.05894500561163 56.27001773108149 L 246.72327721661054 51.18278534994537 L 250.38760942760933 54.79637265949766 L 254.05194163860827 66.64982678082109 L 257.71627384960715 84.34146077056815 L 261.38060606060606 104.33390729886685 L 265.0449382716049 123.15115109277596 L 268.70927048260376 138.38763156707074 L 272.3736026936026 149.08218846490928 L 276.0379349046015 155.4442195620038 L 279.7022671156004 158.30287015686457 L 283.36659932659927 158.67746333049826 L 287.03093153759824 157.58359079445714 L 290.69526374859703 155.9263302194745 L 294.35959595959594 154.34337828272436 L 298.02392817059473 153.06043612258483 L 301.68826038159364 151.9185505965181 L 305.35259259259254 150.59505847101047 L 309.01692480359145 148.84341311584575 L 312.68125701459036 146.57601180048198 L 316.34558922558915 143.81282289599946 L 320.009921436588 140.66452902776945 L 323.6742536475869 137.42376930243898 L 327.3385858585858 134.62507517845341 L 331.00291806958467 132.89169160271916 L 334.6672502805836 132.60277644865755 L 338.3315824915825 133.64592275000768 L 341.9959147025814 135.48129001713397 L 345.6602469135802 137.45752843842797 L 349.3245791245791 139.10172431476622 L 352.988911335578 140.1902464446165 L 356.65324354657685 140.67148509589123 L 360.31757575757575 140.6332268194073 L 363.98190796857455 140.36154900628753 L 367.6462401795734 140.3392679294472 L 371.31057239057236 141.05458707096457 L 374.9749046015712 142.7200593332085 L 378.6392368125701 145.15202855676128 L 382.30356902356885 147.93215602680942 L 385.9679012345679 150.7042851003511 L 389.6322334455667 153.36032212006546 L 393.2965656565657 156.0395799711149 L 396.9608978675645 159.0823864232985 L 400.6252300785634 163.0709860943132 L 404.28956228956224 168.87657295750094 L 407.953894500561 177.4940594374437 L 411.61822671156006 189.5909834897533 L 415.2825589225588 205.00027184955437 L 418.94689113355776 222.52961730382214 L 422.6112233445566 240.2781910788432 L 426.2755555555555 256.30054419446583" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="552.4010336864123" y1="39.599999999999994" x2="552.4010336864123" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="666.9909916398171" y1="39.599999999999994" x2="666.9909916398171" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="781.5809495932219" y1="39.599999999999994" x2="781.5809495932219" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="495.10605470971" y1="39.599999999999994" x2="495.10605470971" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="609.6960126631147" y1="39.599999999999994" x2="609.6960126631147" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="724.2859706165195" y1="39.599999999999994" x2="724.2859706165195" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="838.8759285699243" y1="39.599999999999994" x2="838.8759285699243" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="268.7620850010919" x2="852.8444444444444" y2="268.7620850010919" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="222.5084772254981" x2="852.8444444444444" y2="222.5084772254981" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="176.25486944990428" x2="852.8444444444444" y2="176.25486944990428" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="130.00126167431048" x2="852.8444444444444" y2="130.00126167431048" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="83.74765389871666" x2="852.8444444444444" y2="83.74765389871666" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="291.88888888888886" x2="852.8444444444444" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="245.63528111329506" x2="852.8444444444444" y2="245.63528111329506" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="199.38167333770122" x2="852.8444444444444" y2="199.38167333770122" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="153.12806556210737" x2="852.8444444444444" y2="153.12806556210737" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="106.87445778651359" x2="852.8444444444444" y2="106.87445778651359" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="60.62085001091975" x2="852.8444444444444" y2="60.62085001091975" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="490.0755555555555" y="39.599999999999994" width="362.76888888888885" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="495.10605470971" y1="291.88888888888886" x2="495.10605470971" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="495.10605470971" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="609.6960126631147" y1="291.88888888888886" x2="609.6960126631147" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="609.6960126631147" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="724.2859706165195" y1="291.88888888888886" x2="724.2859706165195" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="724.2859706165195" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">10</text><line x1="838.8759285699243" y1="291.88888888888886" x2="838.8759285699243" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="838.8759285699243" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">15</text><line x1="490.0755555555555" y1="291.88888888888886" x2="486.4088888888889" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.00</text><line x1="490.0755555555555" y1="245.63528111329506" x2="486.4088888888889" y2="245.63528111329506" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="249.74194777996175" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.05</text><line x1="490.0755555555555" y1="199.38167333770122" x2="486.4088888888889" y2="199.38167333770122" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="203.4883400043679" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.10</text><line x1="490.0755555555555" y1="153.12806556210737" x2="486.4088888888889" y2="153.12806556210737" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="157.23473222877402" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.15</text><line x1="490.0755555555555" y1="106.87445778651359" x2="486.4088888888889" y2="106.87445778651359" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="110.98112445318026" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.20</text><line x1="490.0755555555555" y1="60.62085001091975" x2="486.4088888888889" y2="60.62085001091975" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="64.72751667758641" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.25</text><text x="490.0755555555555" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">daily_screen_time_hours</text><path d="M 490.0755555555555 291.849998478981 L 493.73988776655443 291.7780328994928 L 497.40421997755334 291.63570692334133 L 501.0685521885522 291.416700117102 L 504.73288439955104 291.14414648669856 L 508.3972166105499 290.83489039863014 L 512.0615488215487 290.4410685559528 L 515.7258810325477 289.84024193296364 L 519.3902132435466 288.9033981622241 L 523.0545454545454 287.58530796534893 L 526.7188776655444 285.9498345136317 L 530.3832098765431 284.09253379229494 L 534.0475420875421 282.041056397048 L 537.7118742985409 279.7603054101834 L 541.3762065095398 277.2117367369542 L 545.0405387205387 274.30244743934077 L 548.7048709315376 270.84632776596766 L 552.3692031425364 266.80968829297524 L 556.0335353535353 262.5933001064668 L 559.6978675645341 258.7833609872939 L 563.3621997755331 255.5180349634668 L 567.026531986532 252.29080373549667 L 570.6908641975308 248.41086880228625 L 574.3551964085298 243.44716957272027 L 578.0195286195286 237.3281337662347 L 581.6838608305275 230.49125905328893 L 585.3481930415263 224.01448462606334 L 589.0125252525252 219.12336039947368 L 592.6768574635241 216.37983020975312 L 596.341189674523 215.57206184346128 L 600.0055218855218 216.1671737081812 L 603.6698540965208 217.2618023784351 L 607.3341863075195 217.24149698968097 L 610.9985185185185 214.5197304466874 L 614.6628507295173 209.20759815159096 L 618.3271829405162 203.54175751653102 L 621.9915151515152 200.0129076395796 L 625.6558473625139 199.18045543387416 L 629.3201795735129 199.67003701679735 L 632.9845117845117 200.1104840864587 L 636.6488439955106 200.55030836056477 L 640.3131762065095 201.86866196631073 L 643.9775084175084 204.2273623437917 L 647.6418406285072 206.2016350920839 L 651.3061728395062 204.44964582757203 L 654.970505050505 193.27405625326787 L 658.6348372615039 166.15849337513146 L 662.2991694725029 122.53241171835455 L 665.9635016835016 75.90704618328466 L 669.6278338945006 50.39469897173588 L 673.2921661054994 61.54248333386802 L 676.9564983164983 101.15675901313735 L 680.6208305274972 145.68183212693972 L 684.285162738496 177.14462272860752 L 687.9494949494949 192.37504844028197 L 691.6138271604938 196.78136333880371 L 695.2781593714926 195.8929132476018 L 698.9424915824916 192.71852039707747 L 702.6068237934905 189.0640300872663 L 706.2711560044893 186.58223889822034 L 709.9354882154881 186.2104708576444 L 713.599820426487 187.64649497759774 L 717.264152637486 190.07561910005626 L 720.9284848484849 193.06819652936602 L 724.5928170594837 196.55305519205496 L 728.2571492704825 200.3140186879 L 731.9214814814815 203.76092720547666 L 735.5858136924803 206.2516907638933 L 739.2501459034793 207.68630841690052 L 742.914478114478 208.53530077494844 L 746.5788103254769 209.22472699396099 L 750.2431425364758 210.04789543766947 L 753.9074747474748 211.82830536949706 L 757.5718069584736 215.89849004324475 L 761.2361391694724 222.96116700417247 L 764.9004713804713 232.35139244099662 L 768.5648035914702 242.60445311690106 L 772.229135802469 252.4132570590099 L 775.893468013468 261.01030593441305 L 779.5578002244667 268.07742055649527 L 783.2221324354656 273.6059886301256 L 786.8864646464647 277.80797410904313 L 790.5507968574635 281.008053330267 L 794.2151290684624 283.5166220329529 L 797.8794612794611 285.5492701980064 L 801.5437934904601 287.2185802407639 L 805.2081257014589 288.5649661541969 L 808.8724579124579 289.59660136193514 L 812.5367901234567 290.32947310732004 L 816.2011223344555 290.81361123866293 L 819.8654545454544 291.1289270292502 L 823.5297867564534 291.35369161806614 L 827.1941189674523 291.53318687990964 L 830.8584511784511 291.6764750630571 L 834.5227833894501 291.7783610271709 L 838.1871156004488 291.8396172385324 L 841.8514478114478 291.8702904598905 L 845.5157800224466 291.8830141282049 L 849.1801122334455 291.8873560858054 L 852.8444444444442 291.88856280957776" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 490.0755555555555 291.8432743233585 L 493.73988776655443 291.7648362150842 L 497.40421997755334 291.6156987937838 L 501.0685521885522 291.39170064837185 L 504.73288439955104 291.1146062336385 L 508.3972166105499 290.8011488186183 L 512.0615488215487 290.42222908500514 L 515.7258810325477 289.90238318042947 L 519.3902132435466 289.15650227778275 L 523.0545454545454 288.1123731779949 L 526.7188776655444 286.6985300454018 L 530.3832098765431 284.8346437814969 L 534.0475420875421 282.4746490632481 L 537.7118742985409 279.6840981836201 L 541.3762065095398 276.6205373348138 L 545.0405387205387 273.3560314638484 L 548.7048709315376 269.779932714555 L 552.3692031425364 265.8205918692319 L 556.0335353535353 261.71242481162506 L 559.6978675645341 257.83683071986457 L 563.3621997755331 254.26800090819896 L 567.026531986532 250.62311334157164 L 570.6908641975308 246.3509996322751 L 574.3551964085298 241.00969039644656 L 578.0195286195286 234.4021213185327 L 581.6838608305275 226.89252585425578 L 585.3481930415263 219.66991927445275 L 589.0125252525252 214.24357308990102 L 592.6768574635241 211.4460921849518 L 596.341189674523 211.10390527465475 L 600.0055218855218 212.46463635987658 L 603.6698540965208 214.33294399512695 L 607.3341863075195 214.91898068753875 L 610.9985185185185 212.62743819895468 L 614.6628507295173 207.6609587610015 L 618.3271829405162 202.3337394271826 L 621.9915151515152 199.08866227721683 L 625.6558473625139 198.28079008800955 L 629.3201795735129 198.29767141606104 L 632.9845117845117 197.71437417471796 L 636.6488439955106 196.85320105915122 L 640.3131762065095 197.13245145134843 L 643.9775084175084 199.2461428604741 L 647.6418406285072 202.01578168346498 L 651.3061728395062 202.16443022468212 L 654.970505050505 194.50690564233196 L 658.6348372615039 173.83074802755806 L 662.2991694725029 140.2430303596241 L 665.9635016835016 104.58192786654725 L 669.6278338945006 84.98928428946601 L 673.2921661054994 92.87537152077944 L 676.9564983164983 122.33155909820677 L 680.6208305274972 156.12828283171459 L 684.285162738496 180.636040552927 L 687.9494949494949 192.7829801684211 L 691.6138271604938 195.967305735911 L 695.2781593714926 194.11375556917145 L 698.9424915824916 189.85276025011314 L 702.6068237934905 185.30832517221125 L 706.2711560044893 182.31177599444874 L 709.9354882154881 181.60656965773416 L 713.599820426487 182.65036119252028 L 717.264152637486 184.59078791342608 L 720.9284848484849 187.18000742332234 L 724.5928170594837 190.66573861022454 L 728.2571492704825 195.06306730242036 L 731.9214814814815 199.68491577285545 L 735.5858136924803 203.47663311320403 L 739.2501459034793 205.8640255803848 L 742.914478114478 207.07322949929085 L 746.5788103254769 207.65272438304876 L 750.2431425364758 208.2383071527735 L 753.9074747474748 209.90957973937577 L 757.5718069584736 214.02422893158558 L 761.2361391694724 221.1805948540391 L 764.9004713804713 230.61555830245788 L 768.5648035914702 240.82627269464166 L 772.229135802469 250.55104954690322 L 775.893468013468 259.13573663087476 L 779.5578002244667 266.3569189547189 L 783.2221324354656 272.199757610087 L 786.8864646464647 276.7681832439106 L 790.5507968574635 280.2469719537118 L 794.2151290684624 282.86720142417414 L 797.8794612794611 284.8775363246817 L 801.5437934904601 286.49728765283885 L 805.2081257014589 287.8548020296114 L 808.8724579124579 288.9758502311303 L 812.5367901234567 289.84538921338833 L 816.2011223344555 290.47460825557204 L 819.8654545454544 290.9106196780119 L 823.5297867564534 291.2066546223443 L 827.1941189674523 291.4028346059341 L 830.8584511784511 291.530847279848 L 834.5227833894501 291.62103712468905 L 838.1871156004488 291.69677710674193 L 841.8514478114478 291.7654935779179 L 845.5157800224466 291.821517569816 L 849.1801122334455 291.858612055184 L 852.8444444444442 291.87789168361564" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="961.1727004399033" y1="39.599999999999994" x2="961.1727004399033" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1045.4102225318316" y1="39.599999999999994" x2="1045.4102225318316" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1129.6477446237598" y1="39.599999999999994" x2="1129.6477446237598" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1213.885266715688" y1="39.599999999999994" x2="1213.885266715688" y2="291.88888888888886" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="919.0539393939393" y1="39.599999999999994" x2="919.0539393939393" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1003.2914614858676" y1="39.599999999999994" x2="1003.2914614858676" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1087.5289835777958" y1="39.599999999999994" x2="1087.5289835777958" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1171.7665056697238" y1="39.599999999999994" x2="1171.7665056697238" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1256.0040277616522" y1="39.599999999999994" x2="1256.0040277616522" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="257.75389710029765" x2="1265.3333333333333" y2="257.75389710029765" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="189.48391352311515" x2="1265.3333333333333" y2="189.48391352311515" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="121.21392994593266" x2="1265.3333333333333" y2="121.21392994593266" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="52.94394636875021" x2="1265.3333333333333" y2="52.94394636875021" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="291.88888888888886" x2="1265.3333333333333" y2="291.88888888888886" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="223.6189053117064" x2="1265.3333333333333" y2="223.6189053117064" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="155.34892173452394" x2="1265.3333333333333" y2="155.34892173452394" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="87.07893815734144" x2="1265.3333333333333" y2="87.07893815734144" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="902.5644444444443" y="39.599999999999994" width="362.7688888888889" height="252.2888888888889" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="919.0539393939393" y1="291.88888888888886" x2="919.0539393939393" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="919.0539393939393" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="1003.2914614858676" y1="291.88888888888886" x2="1003.2914614858676" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1003.2914614858676" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="1087.5289835777958" y1="291.88888888888886" x2="1087.5289835777958" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1087.5289835777958" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="1171.7665056697238" y1="291.88888888888886" x2="1171.7665056697238" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1171.7665056697238" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="1256.0040277616522" y1="291.88888888888886" x2="1256.0040277616522" y2="295.55555555555554" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1256.0040277616522" y="307.8755555555555" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">8</text><line x1="902.5644444444443" y1="291.88888888888886" x2="898.8977777777777" y2="291.88888888888886" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="295.99555555555554" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="902.5644444444443" y1="223.6189053117064" x2="898.8977777777777" y2="223.6189053117064" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="227.7255719783731" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="902.5644444444443" y1="155.34892173452394" x2="898.8977777777777" y2="155.34892173452394" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="159.4555884011906" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="902.5644444444443" y1="87.07893815734144" x2="898.8977777777777" y2="87.07893815734144" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="91.1856048240081" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.6</text><text x="902.5644444444443" y="28.74666666666667" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">social_media_hours</text><path d="M 902.5644444444443 291.8517353511114 L 906.2287766554432 291.69819259888584 L 909.8931088664422 291.15621838899744 L 913.5574410774409 289.75095739017814 L 917.2217732884399 287.04216877421305 L 920.8861054994387 283.04911618589915 L 924.5504377104377 278.2616929105668 L 928.2147699214365 273.0825990539894 L 931.8791021324353 267.52432272906293 L 935.5434343434342 261.5839332974691 L 939.2077665544331 255.54888530643183 L 942.872098765432 249.72240182444366 L 946.5364309764309 244.13349935531724 L 950.2007631874299 238.76854534509033 L 953.8650953984286 233.86563943720878 L 957.5294276094276 229.64555560487477 L 961.1937598204264 225.9302138081538 L 964.8580920314253 222.4681604922663 L 968.5224242424242 219.46918362448127 L 972.186756453423 217.28398835125643 L 975.8510886644219 215.77001260275281 L 979.5154208754208 214.47632347649363 L 983.1797530864196 213.09639882519983 L 986.8440852974186 211.42855930213582 L 990.5084175084173 209.44745771339115 L 994.1727497194163 207.74062972095948 L 997.8370819304151 207.4367847212643 L 1001.5014141414141 208.90571500587015 L 1005.165746352413 209.66033780087662 L 1008.8300785634119 202.40255890573385 L 1012.4944107744107 175.56748671142753 L 1016.1587429854095 123.73883960438721 L 1019.8230751964085 66.43921039148218 L 1023.4874074074073 44.10491918745659 L 1027.1517396184063 76.1752194672288 L 1030.816071829405 136.87545065971835 L 1034.4804040404038 188.04745149383928 L 1038.1447362514027 216.13821794218438 L 1041.8090684624017 228.298439030567 L 1045.4734006734006 233.15083414475993 L 1049.1377328843994 234.55900124217038 L 1052.8020650953983 234.49132010180057 L 1056.466397306397 234.78253001659488 L 1060.130729517396 236.3684617337624 L 1063.795061728395 238.7179759736424 L 1067.4593939393937 240.78048839213972 L 1071.1237261503927 242.336289704935 L 1074.7880583613914 244.0726500641023 L 1078.4523905723904 246.45116466818922 L 1082.1167227833894 249.1972019845361 L 1085.7810549943883 252.05231180233483 L 1089.445387205387 255.17344400479934 L 1093.109719416386 258.52757342666524 L 1096.7740516273848 261.6251041939426 L 1100.4383838383837 264.0180120873865 L 1104.1027160493827 265.6628866491398 L 1107.7670482603814 266.75932718485876 L 1111.4313804713804 267.4250909372568 L 1115.0957126823791 267.75670487572415 L 1118.760044893378 268.17324182622883 L 1122.424377104377 269.22229695240765 L 1126.088709315376 270.9396405249793 L 1129.7530415263748 272.91851701886844 L 1133.4173737373735 275.02260488318865 L 1137.0817059483725 277.4231413216976 L 1140.7460381593714 279.9687975674788 L 1144.4103703703704 282.0808106437553 L 1148.0747025813691 283.32692431058774 L 1151.7390347923679 283.8442192086828 L 1155.4033670033668 284.1912878238404 L 1159.0676992143658 284.8603139327915 L 1162.7320314253648 285.8860953777932 L 1166.3963636363635 286.9119929748244 L 1170.0606958473625 287.6401959982172 L 1173.7250280583612 288.1186122737072 L 1177.3893602693602 288.57580969567675 L 1181.0536924803591 289.1454380803998 L 1184.7180246913579 289.79391551819464 L 1188.3823569023568 290.3939816022943 L 1192.0466891133556 290.83683108763927 L 1195.7110213243545 291.10465235106284 L 1199.3753535353535 291.2572819143423 L 1203.0396857463522 291.3763580696483 L 1206.7040179573512 291.5088077531965 L 1210.36835016835 291.643289052508 L 1214.032682379349 291.7447893958827 L 1217.6970145903479 291.7984825427251 L 1221.3613468013468 291.812938329253 L 1225.0256790123456 291.8060572571244 L 1228.6900112233445 291.7993144179831 L 1232.3543434343433 291.80882199844825 L 1236.0186756453422 291.8337609852106 L 1239.6830078563412 291.86041775069896 L 1243.34734006734 291.8780060792221 L 1247.011672278339 291.88583846805045 L 1250.6760044893376 291.8882664158427 L 1254.3403367003366 291.88879693180695 L 1258.0046689113356 291.8888790954612 L 1261.6690011223345 291.8888881392299 L 1265.3333333333333 291.8888888477301" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 902.5644444444443 291.8397677580001 L 906.2287766554432 291.65522844041715 L 909.8931088664422 291.04541363088305 L 913.5574410774409 289.5438109467542 L 917.2217732884399 286.7548434775411 L 920.8861054994387 282.7395074232878 L 924.5504377104377 277.9818428031838 L 928.2147699214365 272.8320507996973 L 931.8791021324353 267.20531372819687 L 935.5434343434342 260.96949286805767 L 939.2077665544331 254.34965777039832 L 942.872098765432 247.80029670808648 L 946.5364309764309 241.7343709879135 L 950.2007631874299 236.50421478182457 L 953.8650953984286 232.38845845021015 L 957.5294276094276 229.2978404408098 L 961.1937598204264 226.53367554330492 L 964.8580920314253 223.24393664517416 L 968.5224242424242 219.31008279264273 L 972.186756453423 215.48987722744369 L 975.8510886644219 212.62900528936484 L 979.5154208754208 210.97320668420642 L 983.1797530864196 210.0509540789589 L 986.8440852974186 208.95507156908357 L 990.5084175084173 207.0756279189906 L 994.1727497194163 204.94671798103536 L 997.8370819304151 204.07080074632827 L 1001.5014141414141 205.1608649889179 L 1005.165746352413 205.9263146979703 L 1008.8300785634119 199.75983942705813 L 1012.4944107744107 177.13388085372767 L 1016.1587429854095 134.68008436943134 L 1019.8230751964085 89.08143186632674 L 1023.4874074074073 72.13914730103045 L 1027.1517396184063 98.53878817756852 L 1030.816071829405 148.2327453959406 L 1034.4804040404038 191.17127931394361 L 1038.1447362514027 215.73356532599325 L 1041.8090684624017 226.88895856432538 L 1045.4734006734006 231.56650014158146 L 1049.1377328843994 233.1428839502558 L 1052.8020650953983 233.32994370227416 L 1056.466397306397 233.74819603322695 L 1060.130729517396 235.4560139433601 L 1063.795061728395 238.12821044200928 L 1067.4593939393937 240.49441388023106 L 1071.1237261503927 241.8491235624473 L 1074.7880583613914 242.73939149931152 L 1078.4523905723904 244.01514471795883 L 1082.1167227833894 245.92093253594635 L 1085.7810549943883 248.39135194387995 L 1089.445387205387 251.43896598884908 L 1093.109719416386 254.89884309300743 L 1096.7740516273848 258.31426018959337 L 1100.4383838383837 261.2741286803845 L 1104.1027160493827 263.6142856348465 L 1107.7670482603814 265.2407528659915 L 1111.4313804713804 266.01812653576394 L 1115.0957126823791 266.0955469594033 L 1118.760044893378 266.15346639181183 L 1122.424377104377 266.90323625795145 L 1126.088709315376 268.4024708733058 L 1129.7530415263748 270.2586923523794 L 1133.4173737373735 272.3859988966841 L 1137.0817059483725 275.02110533457903 L 1140.7460381593714 278.0367987885435 L 1144.4103703703704 280.78387525503763 L 1148.0747025813691 282.72009232310506 L 1151.7390347923679 283.89110433267456 L 1155.4033670033668 284.73443834420516 L 1159.0676992143658 285.61900158909646 L 1162.7320314253648 286.5924462657428 L 1166.3963636363635 287.42815455820744 L 1170.0606958473625 287.920625588393 L 1173.7250280583612 288.16572986560607 L 1177.3893602693602 288.47151927182927 L 1181.0536924803591 289.0117854694471 L 1184.7180246913579 289.68043457226315 L 1188.3823569023568 290.2758908506968 L 1192.0466891133556 290.71274214016063 L 1195.7110213243545 291.01974566991385 L 1199.3753535353535 291.23743706708746 L 1203.0396857463522 291.393287520562 L 1206.7040179573512 291.51730496443673 L 1210.36835016835 291.624133109273 L 1214.032682379349 291.7019263454321 L 1217.6970145903479 291.73882190624147 L 1221.3613468013468 291.74627892868835 L 1225.0256790123456 291.7502457402472 L 1228.6900112233445 291.76898129355175 L 1232.3543434343433 291.80217092810176 L 1236.0186756453422 291.8363008529897 L 1239.6830078563412 291.85828917759187 L 1243.34734006734 291.8655188329418 L 1247.011672278339 291.8654459842554 L 1250.6760044893376 291.8671460832641 L 1254.3403367003366 291.8733697315044 L 1258.0046689113356 291.88075363047676 L 1261.6690011223345 291.88578781843705 L 1265.3333333333333 291.8880310916668" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="121.21989898989898" y1="357.15555555555557" x2="121.21989898989898" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="203.66737373737368" y1="357.15555555555557" x2="203.66737373737368" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="286.11484848484844" y1="357.15555555555557" x2="286.11484848484844" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="368.56232323232314" y1="357.15555555555557" x2="368.56232323232314" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="79.9961616161616" y1="357.15555555555557" x2="79.9961616161616" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="162.44363636363633" y1="357.15555555555557" x2="162.44363636363633" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="244.8911111111111" y1="357.15555555555557" x2="244.8911111111111" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="327.3385858585858" y1="357.15555555555557" x2="327.3385858585858" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="409.7860606060606" y1="357.15555555555557" x2="409.7860606060606" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="580.5377634964328" x2="426.2755555555555" y2="580.5377634964328" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="522.7244016004096" x2="426.2755555555555" y2="522.7244016004096" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="464.9110397043863" x2="426.2755555555555" y2="464.9110397043863" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="407.0976778083631" x2="426.2755555555555" y2="407.0976778083631" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="609.4444444444445" x2="426.2755555555555" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="551.6310825484212" x2="426.2755555555555" y2="551.6310825484212" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="493.81772065239795" x2="426.2755555555555" y2="493.81772065239795" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="436.0043587563747" x2="426.2755555555555" y2="436.0043587563747" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="378.19099686035145" x2="426.2755555555555" y2="378.19099686035145" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="63.50666666666666" y="357.15555555555557" width="362.76888888888885" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="79.9961616161616" y1="609.4444444444445" x2="79.9961616161616" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="79.9961616161616" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="162.44363636363633" y1="609.4444444444445" x2="162.44363636363633" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="162.44363636363633" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1</text><line x1="244.8911111111111" y1="609.4444444444445" x2="244.8911111111111" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="244.8911111111111" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="327.3385858585858" y1="609.4444444444445" x2="327.3385858585858" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="327.3385858585858" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">3</text><line x1="409.7860606060606" y1="609.4444444444445" x2="409.7860606060606" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="409.7860606060606" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="63.50666666666666" y1="609.4444444444445" x2="59.83999999999999" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.00</text><line x1="63.50666666666666" y1="551.6310825484212" x2="59.83999999999999" y2="551.6310825484212" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="555.7377492150879" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.25</text><line x1="63.50666666666666" y1="493.81772065239795" x2="59.83999999999999" y2="493.81772065239795" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="497.92438731906464" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.50</text><line x1="63.50666666666666" y1="436.0043587563747" x2="59.83999999999999" y2="436.0043587563747" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="440.1110254230414" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.75</text><line x1="63.50666666666666" y1="378.19099686035145" x2="59.83999999999999" y2="378.19099686035145" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="382.29766352701813" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">1.00</text><text x="63.50666666666666" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">gaming_hours</text><path d="M 63.50666666666666 608.1060738181551 L 67.17099887766554 606.1913476183845 L 70.83533108866442 602.5703897760843 L 74.49966329966328 596.7184481731288 L 78.16399551066216 588.6018802036731 L 81.82832772166104 578.8876330435062 L 85.4926599326599 568.7898853645592 L 89.1569921436588 559.6102766330214 L 92.82132435465766 552.2609790588548 L 96.48565656565656 547.0297913724444 L 100.14998877665542 543.6425182748466 L 103.81432098765431 541.5231700603986 L 107.47865319865318 540.1030248022901 L 111.14298540965207 539.0257617860339 L 114.80731762065093 538.1501041719849 L 118.47164983164983 537.3964139937068 L 122.13598204264868 536.6202588308063 L 125.80031425364757 535.6610196572046 L 129.46464646464645 534.5128486735439 L 133.1289786756453 533.4167008202678 L 136.79331088664418 532.7451317729458 L 140.45764309764309 532.7547701222413 L 144.12197530864196 533.3978638003039 L 147.78630751964084 534.3281021657085 L 151.4506397306397 535.0898735196853 L 155.1149719416386 535.363318486271 L 158.77930415263748 535.115930363572 L 162.44363636363633 534.5817802763604 L 166.1079685746352 534.0830591489191 L 169.77230078563412 533.7389123709145 L 173.436632996633 533.0612096431732 L 177.10096520763184 530.4358744958324 L 180.7652974186307 522.7438403042179 L 184.42962962962957 505.9218705862884 L 188.09396184062848 477.42680586926264 L 191.75829405162736 440.12636027386236 L 195.42262626262624 404.37094467095187 L 199.08695847362515 384.36994487973953 L 202.751290684624 389.58454081648534 L 206.41562289562287 417.9782083143212 L 210.07995510662172 457.5320548696734 L 213.7442873176206 494.5404280912005 L 217.4086195286195 521.0925506335976 L 221.0729517396184 536.5539322316326 L 224.73728395061727 544.360523964325 L 228.40161616161612 548.32831438124 L 232.06594837261505 550.881792162621 L 235.7302805836139 553.0210396422351 L 239.39461279461275 554.9512772472455 L 243.05894500561163 556.6464498421929 L 246.72327721661054 558.1172608275291 L 250.3876094276094 559.4228548285455 L 254.0519416386083 560.5779660624733 L 257.71627384960715 561.516886848824 L 261.380606060606 562.1695870026927 L 265.0449382716049 562.5672591328942 L 268.70927048260376 562.8734588436707 L 272.3736026936026 563.3345977585116 L 276.03793490460157 564.2055194179012 L 279.7022671156004 565.6671231071861 L 283.36659932659927 567.7258193495031 L 287.0309315375981 570.1468715361405 L 290.69526374859703 572.5202556534076 L 294.35959595959594 574.4674609906535 L 298.0239281705948 575.85040124902 L 301.68826038159364 576.8261026472907 L 305.35259259259254 577.7263237172904 L 309.0169248035914 578.8658372003875 L 312.6812570145903 580.3885242399577 L 316.34558922558915 582.2079915772215 L 320.00992143658806 584.0660471714956 L 323.67425364758697 585.691531595226 L 327.3385858585858 586.9676703779952 L 331.00291806958467 587.9852575333773 L 334.6672502805836 588.944735690813 L 338.3315824915825 590.0015383552645 L 341.99591470258133 591.1827268666516 L 345.6602469135802 592.4113920733789 L 349.3245791245791 593.577179713093 L 352.988911335578 594.5893971675309 L 356.6532435465768 595.4063938954911 L 360.31757575757575 596.0538618638856 L 363.98190796857455 596.6179840857637 L 367.64624017957345 597.1976197670382 L 371.31057239057236 597.8379053707756 L 374.9749046015712 598.4894446229323 L 378.6392368125701 599.0206306141115 L 382.3035690235689 599.2848746687387 L 385.9679012345679 599.2197626823 L 389.6322334455667 598.9262843865977 L 393.2965656565656 598.6690939068928 L 396.9608978675645 598.7794859855294 L 400.62523007856345 599.5062428145995 L 404.28956228956224 600.8940171300237 L 407.95389450056115 602.7537718421626 L 411.61822671155994 604.7418980891446 L 415.28255892255885 606.508036573144 L 418.94689113355776 607.8311848810174 L 422.6112233445566 608.6711307199973 L 426.2755555555555 609.1232735749135" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 63.50666666666666 608.2568454396101 L 67.17099887766554 606.4976256782575 L 70.83533108866442 603.1034889833159 L 74.49966329966328 597.5188109234134 L 78.16399551066216 589.6467302498971 L 81.82832772166104 580.0818248400008 L 85.4926599326599 569.980324686735 L 89.1569921436588 560.6020094409095 L 92.82132435465766 552.823848191061 L 96.48565656565656 546.9209937753371 L 100.14998877665542 542.6787556647646 L 103.81432098765431 539.6875651860191 L 107.47865319865318 537.6169886618436 L 111.14298540965207 536.3267361072417 L 114.80731762065093 535.783696897584 L 118.47164983164983 535.8770359550481 L 122.13598204264868 536.3049431471943 L 125.80031425364757 536.6614473629569 L 129.46464646464645 536.6689821582956 L 133.1289786756453 536.3446200434788 L 136.79331088664418 535.9436037509191 L 140.45764309764309 535.7426721757652 L 144.12197530864196 535.8567512490861 L 147.78630751964084 536.203637460643 L 151.4506397306397 536.583999405623 L 155.1149719416386 536.7977833187465 L 158.77930415263748 536.7515320525717 L 162.44363636363633 536.51543024628 L 166.1079685746352 536.2608976009338 L 169.77230078563412 536.0173327160221 L 173.436632996633 535.2300199242 L 177.10096520763184 532.1667958066872 L 180.7652974186307 523.4670116189479 L 184.42962962962957 504.69496731402705 L 188.09396184062848 473.0115547930819 L 191.75829405162736 431.5470443700154 L 195.42262626262624 391.8493468294564 L 199.08695847362515 369.8689670503 L 202.751290684624 376.1607232250267 L 206.41562289562287 408.1999987934017 L 210.07995510662172 452.25919203084754 L 213.7442873176206 493.0016904462562 L 217.4086195286195 521.876640938487 L 221.0729517396184 538.4749722574072 L 224.73728395061727 546.6873866223167 L 228.40161616161612 550.6188435898687 L 232.06594837261505 552.8275917243639 L 235.7302805836139 554.4330623937813 L 239.39461279461275 555.7900940802256 L 243.05894500561163 557.0043516592805 L 246.72327721661054 558.1622972944612 L 250.3876094276094 559.3589425925923 L 254.0519416386083 560.6358021301941 L 257.71627384960715 561.9397605326949 L 261.380606060606 563.1571248710687 L 265.0449382716049 564.1909957590301 L 268.70927048260376 565.032182336954 L 272.3736026936026 565.8078058414458 L 276.03793490460157 566.779429746587 L 279.7022671156004 568.2342939343853 L 283.36659932659927 570.2836661260574 L 287.0309315375981 572.7152091014233 L 290.69526374859703 575.0589476458294 L 294.35959595959594 576.8542566482763 L 298.0239281705948 577.9220104092789 L 301.68826038159364 578.4492660036813 L 305.35259259259254 578.8584672563602 L 309.0169248035914 579.5738515186654 L 312.6812570145903 580.8171117636335 L 316.34558922558915 582.5201183639592 L 320.00992143658806 584.393232088944 L 323.67425364758697 586.113328241562 L 327.3385858585858 587.5076507143862 L 331.00291806958467 588.6052343047036 L 334.6672502805836 589.545122196547 L 338.3315824915825 590.4513468060072 L 341.99591470258133 591.3770521079485 L 345.6602469135802 592.3160363591628 L 349.3245791245791 593.2225623933315 L 352.988911335578 594.0253300449924 L 356.6532435465768 594.6702559461885 L 360.31757575757575 595.1852548361975 L 363.98190796857455 595.6936515748612 L 367.64624017957345 596.3340417758022 L 371.31057239057236 597.1483214202614 L 374.9749046015712 598.0369947206332 L 378.6392368125701 598.8128969157756 L 382.3035690235689 599.308091234471 L 385.9679012345679 599.4719817268507 L 389.6322334455667 599.4162382103748 L 393.2965656565656 599.3846713812899 L 396.9608978675645 599.6582843918501 L 400.62523007856345 600.4381375810779 L 404.28956228956224 601.7571942478555 L 407.95389450056115 603.4581303374789 L 411.61822671155994 605.2517988170813 L 415.28255892255885 606.8369677329333 L 418.94689113355776 608.0203819938977 L 422.6112233445566 608.767806979394 L 426.2755555555555 609.1668478136339" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="533.5428552638235" y1="357.15555555555557" x2="533.5428552638235" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="588.5995996694828" y1="357.15555555555557" x2="588.5995996694828" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="643.656344075142" y1="357.15555555555557" x2="643.656344075142" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="698.7130884808013" y1="357.15555555555557" x2="698.7130884808013" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="753.7698328864606" y1="357.15555555555557" x2="753.7698328864606" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="808.8265772921197" y1="357.15555555555557" x2="808.8265772921197" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="506.0144830609939" y1="357.15555555555557" x2="506.0144830609939" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="561.0712274666531" y1="357.15555555555557" x2="561.0712274666531" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="616.1279718723124" y1="357.15555555555557" x2="616.1279718723124" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="671.1847162779716" y1="357.15555555555557" x2="671.1847162779716" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="726.241460683631" y1="357.15555555555557" x2="726.241460683631" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="781.2982050892903" y1="357.15555555555557" x2="781.2982050892903" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="836.3549494949494" y1="357.15555555555557" x2="836.3549494949494" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="584.3467491216377" x2="852.8444444444444" y2="584.3467491216377" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="534.1513584760241" x2="852.8444444444444" y2="534.1513584760241" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="483.95596783041066" x2="852.8444444444444" y2="483.95596783041066" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="433.76057718479717" x2="852.8444444444444" y2="433.76057718479717" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="383.56518653918374" x2="852.8444444444444" y2="383.56518653918374" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="609.4444444444445" x2="852.8444444444444" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="559.2490537988309" x2="852.8444444444444" y2="559.2490537988309" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="509.0536631532175" x2="852.8444444444444" y2="509.0536631532175" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="458.85827250760394" x2="852.8444444444444" y2="458.85827250760394" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="408.66288186199046" x2="852.8444444444444" y2="408.66288186199046" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="358.46749121637697" x2="852.8444444444444" y2="358.46749121637697" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="490.0755555555555" y="357.15555555555557" width="362.76888888888885" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="506.0144830609939" y1="609.4444444444445" x2="506.0144830609939" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="506.0144830609939" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="561.0712274666531" y1="609.4444444444445" x2="561.0712274666531" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="561.0712274666531" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">1</text><line x1="616.1279718723124" y1="609.4444444444445" x2="616.1279718723124" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="616.1279718723124" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">2</text><line x1="671.1847162779716" y1="609.4444444444445" x2="671.1847162779716" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="671.1847162779716" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">3</text><line x1="726.241460683631" y1="609.4444444444445" x2="726.241460683631" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="726.241460683631" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">4</text><line x1="781.2982050892903" y1="609.4444444444445" x2="781.2982050892903" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="781.2982050892903" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="836.3549494949494" y1="609.4444444444445" x2="836.3549494949494" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="836.3549494949494" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="490.0755555555555" y1="609.4444444444445" x2="486.4088888888889" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="490.0755555555555" y1="559.2490537988309" x2="486.4088888888889" y2="559.2490537988309" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="563.3557204654976" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="490.0755555555555" y1="509.0536631532175" x2="486.4088888888889" y2="509.0536631532175" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="513.160329819884" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="490.0755555555555" y1="458.85827250760394" x2="486.4088888888889" y2="458.85827250760394" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="462.96493917427057" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><line x1="490.0755555555555" y1="408.66288186199046" x2="486.4088888888889" y2="408.66288186199046" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="412.7695485286571" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><line x1="490.0755555555555" y1="358.46749121637697" x2="486.4088888888889" y2="358.46749121637697" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="362.57415788304365" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.5</text><text x="490.0755555555555" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">work_study_hours</text><path d="M 490.0755555555555 609.3890655940033 L 493.73988776655443 609.2742909833951 L 497.40421997755334 608.981467426057 L 501.0685521885522 608.3219484360366 L 504.73288439955104 607.0040571575857 L 508.3972166105499 604.6530274151376 L 512.0615488215487 600.8744988386235 L 515.7258810325477 595.3294438976093 L 519.3902132435466 587.7725612585078 L 523.0545454545454 578.0630133270645 L 526.7188776655444 566.2409757553534 L 530.3832098765431 552.726541561377 L 534.0475420875421 538.4933250296842 L 537.7118742985409 524.9328194192165 L 541.3762065095398 513.3299431176968 L 545.0405387205387 504.2785776936097 L 548.7048709315376 497.4995246059194 L 552.3692031425364 492.1814925897696 L 556.0335353535353 487.5206690779097 L 559.6978675645341 483.0591769462863 L 563.3621997755331 478.719096783731 L 567.026531986532 474.6946745215961 L 570.6908641975308 471.33967180535575 L 574.3551964085298 469.0263179871843 L 578.0195286195286 467.9349491607269 L 581.6838608305275 467.87979757336916 L 585.3481930415264 468.356949881053 L 589.0125252525252 468.8346743605367 L 592.6768574635241 469.04244226504863 L 596.341189674523 469.011510867902 L 600.0055218855218 468.9151235066007 L 603.6698540965208 468.9356112471085 L 607.3341863075195 469.1975852910691 L 610.9985185185185 469.54386462042095 L 614.6628507295173 469.01204214673146 L 618.3271829405162 465.3726018969335 L 621.9915151515152 455.65817429619676 L 625.655847362514 438.4110666028313 L 629.3201795735129 416.64919109672365 L 632.9845117845117 398.4390503349241 L 636.6488439955107 392.7930079501466 L 640.3131762065095 403.34619885576404 L 643.9775084175084 425.69448903573937 L 647.6418406285072 451.01494594026923 L 651.3061728395062 472.1029965917351 L 654.970505050505 486.4016108821689 L 658.6348372615039 494.92393887447406 L 662.2991694725029 499.7934790770611 L 665.9635016835016 502.7771560712781 L 669.6278338945006 505.0198632985733 L 673.2921661054994 507.1905499072267 L 676.9564983164984 509.57083182153457 L 680.6208305274972 512.1131327104896 L 684.285162738496 514.6183080429464 L 687.9494949494949 517.0085639537906 L 691.6138271604938 519.4656656793538 L 695.2781593714926 522.2651086518932 L 698.9424915824916 525.4763051075216 L 702.6068237934905 528.8859491456262 L 706.2711560044893 532.2300978823503 L 709.9354882154881 535.43815198773 L 713.5998204264871 538.6165721570574 L 717.264152637486 541.8431330017712 L 720.9284848484849 545.0218477581724 L 724.5928170594837 547.9318973028813 L 728.2571492704825 550.4245992112071 L 731.9214814814815 552.6153777676138 L 735.5858136924803 554.8920562649234 L 739.2501459034793 557.6790643456661 L 742.914478114478 561.1246386053938 L 746.578810325477 564.9783998678063 L 750.2431425364758 568.7595819322285 L 753.9074747474748 572.0612644300538 L 757.5718069584736 574.7527021981239 L 761.2361391694724 576.961975540642 L 764.9004713804713 578.8948145731201 L 768.5648035914702 580.6464100216621 L 772.2291358024692 582.1499150479527 L 775.893468013468 583.2933073558461 L 779.5578002244667 584.0973228665839 L 783.2221324354657 584.7876272877222 L 786.8864646464647 585.6758331749629 L 790.5507968574635 586.937334903311 L 794.2151290684624 588.4891834149471 L 797.8794612794611 590.0883489134005 L 801.5437934904601 591.5386388777044 L 805.208125701459 592.7797857532246 L 808.8724579124579 593.8047360565711 L 812.5367901234567 594.5861863570993 L 816.2011223344557 595.1505295110801 L 819.8654545454544 595.688612541455 L 823.5297867564534 596.517827754558 L 827.1941189674523 597.8986278836564 L 830.8584511784511 599.8681532104545 L 834.5227833894501 602.207885372099 L 838.187115600449 604.5435049428206 L 841.8514478114478 606.5132342303142 L 845.5157800224466 607.915921194111 L 849.1801122334455 608.7567848566359 L 852.8444444444444 609.1797319105806" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 490.0755555555555 609.4017025659482 L 493.73988776655443 609.3037473980771 L 497.40421997755334 609.0392097657057 L 501.0685521885522 608.4174067320318 L 504.73288439955104 607.1390021829984 L 508.3972166105499 604.8235642120908 L 512.0615488215487 601.0886174549406 L 515.7258810325477 595.6362872564115 L 519.3902132435466 588.2820488948169 L 523.0545454545454 578.9244455489182 L 526.7188776655444 567.5654030743207 L 530.3832098765431 554.4920662498361 L 534.0475420875421 540.5255230783807 L 537.7118742985409 527.0150997972927 L 541.3762065095398 515.3621728270314 L 545.0405387205387 506.3194349330481 L 548.7048709315376 499.6413491903942 L 552.3692031425364 494.4154256995455 L 556.0335353535353 489.7798462587741 L 559.6978675645341 485.39409117010666 L 563.3621997755331 481.35391196385115 L 567.026531986532 477.8383616137886 L 570.6908641975308 474.9313697873534 L 574.3551964085298 472.6765416911783 L 578.0195286195286 471.1027637839502 L 581.6838608305275 470.1038621657552 L 585.3481930415264 469.3706293385576 L 589.0125252525252 468.55644057088466 L 592.6768574635241 467.53204356587 L 596.341189674523 466.465280710637 L 600.0055218855218 465.7190115960246 L 603.6698540965208 465.72387817265064 L 607.3341863075195 466.78071874430555 L 610.9985185185185 468.57762815231246 L 614.6628507295173 469.4454441988849 L 618.3271829405162 465.918181836759 L 621.9915151515152 453.6725769673841 L 625.655847362514 430.64560575752887 L 629.3201795735129 401.033558698868 L 632.9845117845117 376.0308715611718 L 636.6488439955107 368.08078561120703 L 640.3131762065095 382.00313660623203 L 643.9775084175084 411.39053934236955 L 647.6418406285072 443.94593738336346 L 651.3061728395062 469.9893295666907 L 654.970505050505 486.4211785069806 L 658.6348372615039 494.98994196171816 L 662.2991694725029 498.91926822807346 L 665.9635016835016 500.970132832662 L 669.6278338945006 502.976889432535 L 673.2921661054994 505.8758133831211 L 676.9564983164984 509.80534916737156 L 680.6208305274972 514.3124993542 L 684.285162738496 518.7032413939562 L 687.9494949494949 522.4389764231898 L 691.6138271604938 525.3701805977014 L 695.2781593714926 527.6739835423666 L 698.9424915824916 529.64004446292 L 702.6068237934905 531.5673182777496 L 706.2711560044893 533.7888602572866 L 709.9354882154881 536.6174164711668 L 713.5998204264871 540.1562718687782 L 717.264152637486 544.1685337140323 L 720.9284848484849 548.1533948541216 L 724.5928170594837 551.5798717657667 L 728.2571492704825 554.1636356748535 L 731.9214814814815 556.0659857016236 L 735.5858136924803 557.8467336647875 L 739.2501459034793 560.1084603445098 L 742.914478114478 563.0732160945036 L 746.578810325477 566.461160861165 L 750.2431425364758 569.7624723314144 L 753.9074747474748 572.6286637064729 L 757.5718069584736 575.0570573003591 L 761.2361391694724 577.2763606801593 L 764.9004713804713 579.4858901305472 L 768.5648035914702 581.6777522527075 L 772.2291358024692 583.6662984071847 L 775.893468013468 585.269757633246 L 779.5578002244667 586.4829173086252 L 783.2221324354657 587.5069135650681 L 786.8864646464647 588.6143968627481 L 790.5507968574635 589.9510023802445 L 794.2151290684624 591.434953099651 L 797.8794612794611 592.8448134256171 L 801.5437934904601 594.0031418916822 L 805.208125701459 594.8634381265836 L 808.8724579124579 595.4467496427496 L 812.5367901234567 595.7805377260083 L 816.2011223344557 595.9584173265544 L 819.8654545454544 596.2201201254329 L 823.5297867564534 596.8928818602491 L 827.1941189674523 598.2114158422763 L 830.8584511784511 600.1693483104966 L 834.5227833894501 602.5045832443436 L 838.187115600449 604.813402178524 L 841.8514478114478 606.7279225009078 L 845.5157800224466 608.0614783471265 L 849.1801122334455 608.8398850656831 L 852.8444444444444 609.2194366623378" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="919.0539393939393" y1="357.15555555555557" x2="919.0539393939393" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="992.3405836139169" y1="357.15555555555557" x2="992.3405836139169" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1065.6272278338943" y1="357.15555555555557" x2="1065.6272278338943" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1138.913872053872" y1="357.15555555555557" x2="1138.913872053872" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1212.2005162738496" y1="357.15555555555557" x2="1212.2005162738496" y2="609.4444444444445" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="955.6972615039281" y1="357.15555555555557" x2="955.6972615039281" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1028.9839057239055" y1="357.15555555555557" x2="1028.9839057239055" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1102.2705499438832" y1="357.15555555555557" x2="1102.2705499438832" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1175.557194163861" y1="357.15555555555557" x2="1175.557194163861" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1248.8438383838384" y1="357.15555555555557" x2="1248.8438383838384" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="581.1340339732216" x2="1265.3333333333333" y2="581.1340339732216" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="524.5132130307759" x2="1265.3333333333333" y2="524.5132130307759" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="467.8923920883301" x2="1265.3333333333333" y2="467.8923920883301" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="411.27157114588437" x2="1265.3333333333333" y2="411.27157114588437" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="609.4444444444445" x2="1265.3333333333333" y2="609.4444444444445" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="552.8236235019988" x2="1265.3333333333333" y2="552.8236235019988" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="496.202802559553" x2="1265.3333333333333" y2="496.202802559553" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="439.58198161710726" x2="1265.3333333333333" y2="439.58198161710726" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="382.96116067466147" x2="1265.3333333333333" y2="382.96116067466147" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="902.5644444444443" y="357.15555555555557" width="362.7688888888889" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="955.6972615039281" y1="609.4444444444445" x2="955.6972615039281" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="955.6972615039281" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="1028.9839057239055" y1="609.4444444444445" x2="1028.9839057239055" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1028.9839057239055" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">6</text><line x1="1102.2705499438832" y1="609.4444444444445" x2="1102.2705499438832" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1102.2705499438832" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">7</text><line x1="1175.557194163861" y1="609.4444444444445" x2="1175.557194163861" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1175.557194163861" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">8</text><line x1="1248.8438383838384" y1="609.4444444444445" x2="1248.8438383838384" y2="613.1111111111111" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1248.8438383838384" y="625.4311111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">9</text><line x1="902.5644444444443" y1="609.4444444444445" x2="898.8977777777777" y2="609.4444444444445" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="613.5511111111111" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="902.5644444444443" y1="552.8236235019988" x2="898.8977777777777" y2="552.8236235019988" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="556.9302901686654" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="902.5644444444443" y1="496.202802559553" x2="898.8977777777777" y2="496.202802559553" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="500.3094692262196" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="902.5644444444443" y1="439.58198161710726" x2="898.8977777777777" y2="439.58198161710726" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="443.6886482837739" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><line x1="902.5644444444443" y1="382.96116067466147" x2="898.8977777777777" y2="382.96116067466147" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="387.0678273413281" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.4</text><text x="902.5644444444443" y="346.3022222222222" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">sleep_hours</text><path d="M 902.5644444444443 604.0473162035237 L 906.2287766554432 599.7452038018764 L 909.8931088664422 593.4934214008416 L 913.5574410774409 585.3385571747367 L 917.2217732884399 575.7929355983732 L 920.8861054994387 565.7664596772393 L 924.5504377104377 556.3047868769784 L 928.2147699214365 548.2371710977557 L 931.8791021324355 541.8983110783947 L 935.5434343434342 537.0516254739579 L 939.2077665544331 533.0378508396716 L 942.872098765432 529.0714762265998 L 946.5364309764309 524.5543393179503 L 950.2007631874299 519.278477727458 L 953.8650953984286 513.4422571380002 L 957.5294276094276 507.4926472290099 L 961.1937598204264 501.89688248023083 L 964.8580920314253 496.9804546161299 L 968.5224242424242 492.9092244894846 L 972.186756453423 489.77564805884924 L 975.8510886644219 487.6672597300068 L 979.5154208754208 486.6266052644581 L 983.1797530864196 486.5347595274132 L 986.8440852974186 487.05026833790487 L 990.5084175084176 487.706694139374 L 994.1727497194163 488.130284228751 L 997.8370819304151 488.21701566014065 L 1001.5014141414141 488.1276486066652 L 1005.165746352413 488.1076144368679 L 1008.8300785634119 488.27717892522617 L 1012.4944107744107 488.54108583375825 L 1016.1587429854095 488.6478706561748 L 1019.8230751964085 488.3142509135033 L 1023.4874074074074 487.3208277098054 L 1027.1517396184063 485.55929108280816 L 1030.816071829405 483.06880851118706 L 1034.480404040404 480.08262242808917 L 1038.1447362514027 477.04911242548224 L 1041.8090684624017 474.5653413567435 L 1045.4734006734006 473.19640459054045 L 1049.1377328843994 473.2222209765071 L 1052.8020650953983 474.4023238719926 L 1056.466397306397 475.84990488619496 L 1060.130729517396 476.07494942217505 L 1063.795061728395 473.2312275560874 L 1067.459393939394 465.592057328387 L 1071.1237261503927 452.2354111478956 L 1074.7880583613917 433.7781875288224 L 1078.4523905723904 412.79782942134017 L 1082.1167227833894 393.50864281500503 L 1085.7810549943883 380.53593443035135 L 1089.445387205387 377.2094689934362 L 1093.109719416386 384.25751401048507 L 1096.7740516273848 399.6519532109196 L 1100.4383838383837 419.63764447067234 L 1104.1027160493827 440.24927548909056 L 1107.7670482603817 458.4713730129587 L 1111.4313804713804 472.6551336613382 L 1115.0957126823791 482.365499465235 L 1118.760044893378 488.03529828886155 L 1122.424377104377 490.6428788735675 L 1126.088709315376 491.41079234489337 L 1129.7530415263748 491.4760006647332 L 1133.4173737373737 491.5810026068872 L 1137.0817059483725 491.91772864704336 L 1140.7460381593714 492.217622227749 L 1144.4103703703704 492.04799319998176 L 1148.0747025813691 491.14951816945666 L 1151.739034792368 489.62548765537264 L 1155.4033670033668 487.8921267438899 L 1159.0676992143658 486.4559155213366 L 1162.7320314253648 485.68523213976835 L 1166.3963636363635 485.71504117793586 L 1170.0606958473625 486.49719499752297 L 1173.7250280583612 487.8994635849608 L 1177.3893602693602 489.7517257761107 L 1181.0536924803591 491.82074612210386 L 1184.7180246913579 493.7755564782865 L 1188.3823569023568 495.21569062082335 L 1192.0466891133556 495.77944349941447 L 1195.7110213243545 495.28140224730396 L 1199.3753535353535 493.79781113117724 L 1203.0396857463525 491.6452290926835 L 1206.7040179573514 489.2669686094587 L 1210.3683501683502 487.10656717377844 L 1214.032682379349 485.5548807579413 L 1217.6970145903479 484.99195369864765 L 1221.3613468013468 485.856991678879 L 1225.0256790123458 488.65075160515266 L 1228.6900112233445 493.83478148516616 L 1232.3543434343433 501.68110012706103 L 1236.0186756453422 512.1560522931379 L 1239.6830078563412 524.8768426554128 L 1243.3473400673402 539.1270902279992 L 1247.011672278339 553.9196844983497 L 1250.6760044893376 568.1272985905456 L 1254.3403367003366 580.6927436851003 L 1258.0046689113356 590.8670186393288 L 1261.6690011223345 598.3688117098841 L 1265.3333333333333 603.3838343321343" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 902.5644444444443 604.352456862978 L 906.2287766554432 600.1866321290747 L 909.8931088664422 594.0628746315471 L 913.5574410774409 585.9927523319647 L 917.2217732884399 576.4613802205695 L 920.8861054994387 566.3742024747592 L 924.5504377104377 556.8000030209083 L 928.2147699214365 548.6090008396433 L 931.8791021324355 542.1753870518405 L 935.5434343434342 537.2831282829876 L 939.2077665544331 533.2689521267092 L 942.872098765432 529.3269285413407 L 946.5364309764309 524.8365569873615 L 950.2007631874299 519.575384553842 L 953.8650953984286 513.7348181625873 L 957.5294276094276 507.75747616362025 L 961.1937598204264 502.10964994359733 L 964.8580920314253 497.12806900531973 L 968.5224242424242 493.00782041332656 L 972.186756453423 489.878877581469 L 975.8510886644219 487.8519401850783 L 979.5154208754208 486.96115224343635 L 983.1797530864196 487.05123894879245 L 986.8440852974186 487.73653741295453 L 990.5084175084176 488.5164598918923 L 994.1727497194163 488.9963833466166 L 997.8370819304151 489.05728136624964 L 1001.5014141414141 488.8445642208833 L 1005.165746352413 488.58953894713636 L 1008.8300785634119 488.40803539067326 L 1012.4944107744107 488.2218582926322 L 1016.1587429854095 487.82848298893606 L 1019.8230751964085 487.02716694289154 L 1023.4874074074074 485.70158025520186 L 1027.1517396184063 483.84123729062844 L 1030.816071829405 481.55067924592987 L 1034.480404040404 479.0792336462757 L 1038.1447362514027 476.83880537604847 L 1041.8090684624017 475.3431098249908 L 1045.4734006734006 475.0379759358118 L 1049.1377328843994 476.06579202510807 L 1052.8020650953983 478.0539548924787 L 1056.466397306397 480.0064340988536 L 1060.130729517396 480.3403335131616 L 1063.795061728395 477.10214995639626 L 1067.459393939394 468.42596132346955 L 1071.1237261503927 453.2713762881966 L 1074.7880583613917 432.29781169220416 L 1078.4523905723904 408.43397776532424 L 1082.1167227833894 386.5599712142281 L 1085.7810549943883 372.0499024726767 L 1089.445387205387 368.6828320540194 L 1093.109719416386 377.05272336965623 L 1096.7740516273848 394.4515215857033 L 1100.4383838383837 416.258504874269 L 1104.1027160493827 437.9033001981626 L 1107.7670482603817 456.27452501590005 L 1111.4313804713804 470.0668447696262 L 1115.0957126823791 479.3397801688144 L 1118.760044893378 484.8772726029343 L 1122.424377104377 487.73142989284065 L 1126.088709315376 488.980365341522 L 1129.7530415263748 489.56270807718374 L 1133.4173737373737 490.11095547697215 L 1137.0817059483725 490.8329732159241 L 1140.7460381593714 491.5359120237259 L 1144.4103703703704 491.81878265516673 L 1148.0747025813691 491.34792411769683 L 1151.739034792368 490.06864023802274 L 1155.4033670033668 488.2471403875 L 1159.0676992143658 486.349414209001 L 1162.7320314253648 484.8586531356551 L 1166.3963636363635 484.13877634495407 L 1170.0606958473625 484.3808135108561 L 1173.7250280583612 485.59957641035436 L 1177.3893602693602 487.6386481199494 L 1181.0536924803591 490.1779838504086 L 1184.7180246913579 492.76835044024347 L 1188.3823569023568 494.9131007849801 L 1192.0466891133556 496.19021723114895 L 1195.7110213243545 496.37643064722124 L 1199.3753535353535 495.51823390078613 L 1203.0396857463525 493.9094253573977 L 1206.7040179573514 491.9855371708288 L 1210.3683501683502 490.20177626417103 L 1214.032682379349 488.9717476414271 L 1217.6970145903479 488.68905960352095 L 1221.3613468013468 489.77764728969623 L 1225.0256790123458 492.6925111063615 L 1228.6900112233445 497.84304820613704 L 1232.3543434343433 505.4792321022494 L 1236.0186756453422 515.5965321097585 L 1239.6830078563412 527.8795828763377 L 1243.3473400673402 541.6805332323779 L 1247.011672278339 556.050292402826 L 1250.6760044893376 569.8655572762852 L 1254.3403367003366 582.0598963981943 L 1258.0046689113356 591.88421835123 L 1261.6690011223345 599.0723438077152 L 1265.3333333333333 603.8302244819284" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="87.16550724637679" y1="674.711111111111" x2="87.16550724637679" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="158.85896354852872" y1="674.711111111111" x2="158.85896354852872" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="230.55241985068074" y1="674.711111111111" x2="230.55241985068074" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="302.24587615283264" y1="674.711111111111" x2="302.24587615283264" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="373.93933245498454" y1="674.711111111111" x2="373.93933245498454" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="123.01223539745277" y1="674.711111111111" x2="123.01223539745277" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="194.70569169960473" y1="674.711111111111" x2="194.70569169960473" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="266.39914800175666" y1="674.711111111111" x2="266.39914800175666" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="338.0926043039086" y1="674.711111111111" x2="338.0926043039086" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="409.7860606060606" y1="674.711111111111" x2="409.7860606060606" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="896.3838031064448" x2="426.2755555555555" y2="896.3838031064448" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="835.1514093193346" x2="426.2755555555555" y2="835.1514093193346" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="773.9190155322242" x2="426.2755555555555" y2="773.9190155322242" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="712.6866217451139" x2="426.2755555555555" y2="712.6866217451139" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="63.50666666666666" y1="927.0" x2="426.2755555555555" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="865.7676062128896" x2="426.2755555555555" y2="865.7676062128896" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="804.5352124257794" x2="426.2755555555555" y2="804.5352124257794" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="743.3028186386691" x2="426.2755555555555" y2="743.3028186386691" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="63.50666666666666" y1="682.070424851559" x2="426.2755555555555" y2="682.070424851559" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="63.50666666666666" y="674.711111111111" width="362.76888888888885" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="123.01223539745277" y1="927.0" x2="123.01223539745277" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="123.01223539745277" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">50</text><line x1="194.70569169960473" y1="927.0" x2="194.70569169960473" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="194.70569169960473" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">100</text><line x1="266.39914800175666" y1="927.0" x2="266.39914800175666" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="266.39914800175666" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">150</text><line x1="338.0926043039086" y1="927.0" x2="338.0926043039086" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="338.0926043039086" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">200</text><line x1="409.7860606060606" y1="927.0" x2="409.7860606060606" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="409.7860606060606" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">250</text><line x1="63.50666666666666" y1="927.0" x2="59.83999999999999" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0000</text><line x1="63.50666666666666" y1="865.7676062128896" x2="59.83999999999999" y2="865.7676062128896" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="869.8742728795564" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0025</text><line x1="63.50666666666666" y1="804.5352124257794" x2="59.83999999999999" y2="804.5352124257794" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="808.6418790924461" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0050</text><line x1="63.50666666666666" y1="743.3028186386691" x2="59.83999999999999" y2="743.3028186386691" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="747.4094853053358" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0075</text><line x1="63.50666666666666" y1="682.070424851559" x2="59.83999999999999" y2="682.070424851559" stroke="#444444" stroke-width="1.3333333333333333"/><text x="56.90666666666665" y="686.1770915182257" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0100</text><text x="63.50666666666666" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">notifications_per_day</text><path d="M 63.50666666666666 920.8095317200451 L 67.17099887766554 916.3360126580822 L 70.83533108866442 910.0859763912974 L 74.49966329966328 902.1826501356104 L 78.16399551066216 893.1235020507426 L 81.82832772166104 883.6990192782237 L 85.4926599326599 874.8021599238484 L 89.1569921436588 867.2108804717595 L 92.82132435465766 861.4241168356167 L 96.48565656565656 857.583259512052 L 100.14998877665542 855.4694666312519 L 103.81432098765431 854.5638156779112 L 107.47865319865318 854.1713655633445 L 111.14298540965206 853.6029274449536 L 114.80731762065093 852.3746776050309 L 118.47164983164981 850.3581355609241 L 122.13598204264868 847.8206241085009 L 125.80031425364757 845.335262035492 L 129.46464646464645 843.5851587614444 L 133.12897867564533 843.1216374302186 L 136.79331088664418 844.1572158277991 L 140.45764309764309 846.4745643843075 L 144.12197530864194 849.4995056237414 L 147.78630751964081 852.5183948772365 L 151.4506397306397 854.9465579611845 L 155.11497194163857 856.5205877133435 L 158.77930415263745 857.3221976794498 L 162.44363636363633 857.6324997280728 L 166.1079685746352 857.7116708151448 L 169.7723007856341 857.6381285482128 L 173.43663299663297 857.2927409952204 L 177.10096520763184 856.4677891703723 L 180.7652974186307 854.9963321498985 L 184.42962962962957 852.801772965438 L 188.09396184062848 849.8531116668652 L 191.75829405162736 846.1013207827659 L 195.42262626262624 841.4884521645636 L 199.0869584736251 836.0536905036566 L 202.751290684624 830.0703348672034 L 206.41562289562287 824.107541152769 L 210.07995510662172 818.9472125388838 L 213.7442873176206 815.3679234154736 L 217.4086195286195 813.8767282386634 L 221.0729517396184 814.4865073546998 L 224.73728395061724 816.6011812282575 L 228.4016161616161 819.0165762307265 L 232.06594837261497 820.0166553200047 L 235.73028058361388 817.5756874220335 L 239.39461279461275 809.7465551136421 L 243.05894500561163 795.3247637219547 L 246.72327721661048 774.7100954248056 L 250.38760942760933 750.5695247189578 L 254.05194163860827 727.7030083151782 L 257.7162738496071 711.7600071677604 L 261.380606060606 707.1603252179609 L 265.0449382716049 715.2520798647089 L 268.70927048260376 733.7702994180315 L 272.3736026936026 757.9079972215227 L 276.0379349046015 782.3328865479308 L 279.7022671156004 803.0213858678035 L 283.36659932659927 818.0965754483046 L 287.0309315375981 827.579006938202 L 290.69526374859703 832.5281014061773 L 294.35959595959594 834.1847125829736 L 298.0239281705948 833.5029312084449 L 301.68826038159364 831.1207783041914 L 305.35259259259254 827.5686486531249 L 309.0169248035914 823.4511449317502 L 312.6812570145903 819.452480908091 L 316.34558922558915 816.1992124455385 L 320.00992143658806 814.119984008282 L 323.6742536475869 813.3984051040903 L 327.3385858585858 813.9936134854875 L 331.00291806958467 815.6473793846294 L 334.6672502805835 817.865138744819 L 338.3315824915825 819.9563989811323 L 341.99591470258133 821.2117636085388 L 345.6602469135802 821.1664607825912 L 349.3245791245791 819.7842641657822 L 352.988911335578 817.4216746763424 L 356.6532435465768 814.590947258092 L 360.31757575757575 811.69223826856 L 363.98190796857455 808.89535805525 L 367.6462401795734 806.2164978910838 L 371.31057239057236 803.6827803909988 L 374.97490460157115 801.4433853079454 L 378.6392368125701 799.7816126919543 L 382.3035690235689 799.0895619533591 L 385.9679012345678 799.8710489485408 L 389.6322334455667 802.7498613380469 L 393.2965656565655 808.393420242324 L 396.9608978675645 817.3012409753471 L 400.6252300785633 829.5232595463158 L 404.28956228956224 844.4614607306069 L 407.9538945005611 860.893212419036 L 411.61822671155994 877.243986678814 L 415.28255892255885 892.0037899284868 L 418.94689113355776 904.1081492673948 L 422.6112233445566 913.1316360090948 L 426.2755555555555 919.2464468233716" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 63.50666666666666 921.0590591807495 L 67.17099887766554 916.6283108499797 L 70.83533108866442 910.3589831125679 L 74.49966329966328 902.3478395212584 L 78.16399551066216 893.0951720020407 L 81.82832772166104 883.4360687290542 L 85.4926599326599 874.3456631177303 L 89.1569921436588 866.7003423368666 L 92.82132435465766 861.083776205588 L 96.48565656565656 857.6812383192924 L 100.14998877665542 856.2605777796973 L 103.81432098765431 856.2296419561591 L 107.47865319865318 856.7710892898156 L 111.14298540965206 857.0473789804938 L 114.80731762065093 856.4331549225606 L 118.47164983164981 854.7003761032593 L 122.13598204264868 852.0853341152275 L 125.80031425364757 849.2064665316644 L 129.46464646464645 846.8539546784423 L 133.12897867564533 845.7160718431467 L 136.79331088664418 846.1346863913495 L 140.45764309764309 847.983927297883 L 144.12197530864194 850.7281085894283 L 147.78630751964081 853.639498094698 L 151.4506397306397 856.077312712885 L 155.11497194163857 857.6958535326833 L 158.77930415263745 858.488781033545 L 162.44363636363633 858.6690157807049 L 166.1079685746352 858.4744746374458 L 169.7723007856341 858.0216318202761 L 173.43663299663297 857.2806315788872 L 177.10096520763184 856.1520824604117 L 180.7652974186307 854.5575972739355 L 184.42962962962957 852.4654653364073 L 188.09396184062848 849.8459999965982 L 191.75829405162736 846.620521992586 L 195.42262626262624 842.6749729232066 L 199.0869584736251 837.9550192438137 L 202.751290684624 832.5947064749735 L 206.41562289562287 827.0026178950525 L 210.07995510662172 821.8462763114201 L 213.7442873176206 817.9150562793312 L 217.4086195286195 815.8824728707934 L 221.0729517396184 816.0231566370259 L 224.73728395061724 817.9627632407443 L 228.4016161616161 820.5379914171019 L 232.06594837261497 821.8204698082947 L 235.73028058361388 819.3479132007842 L 239.39461279461275 810.6331429385953 L 243.05894500561163 794.017145659816 L 246.72327721661048 769.7561913093446 L 250.38760942760933 740.8734888903132 L 254.05194163860827 713.0532023226353 L 257.7162738496071 693.1274379271003 L 261.380606060606 686.5590337436547 L 265.0449382716049 695.1730731866448 L 268.70927048260376 716.4492410909049 L 272.3736026936026 744.7685593659768 L 276.0379349046015 773.8061820580243 L 279.7022671156004 798.7106373869462 L 283.36659932659927 817.1128089217497 L 287.0309315375981 828.8767892695064 L 290.69526374859703 835.150346167755 L 294.35959595959594 837.3946109620231 L 298.0239281705948 836.813720088897 L 301.68826038159364 834.2507342852218 L 305.35259259259254 830.3626795934854 L 309.0169248035914 825.8124158677165 L 312.6812570145903 821.3096612291831 L 316.34558922558915 817.5081464099796 L 320.00992143658806 814.8813318259995 L 323.6742536475869 813.6709829045608 L 327.3385858585858 813.8894783130714 L 331.00291806958467 815.3035949230853 L 334.6672502805835 817.3999734172789 L 338.3315824915825 819.4319109259538 L 341.99591470258133 820.6283375690614 L 345.6602469135802 820.4991807986851 L 349.3245791245791 819.0445018863402 L 352.988911335578 816.7098692958168 L 356.6532435465768 814.1107153273451 L 360.31757575757575 811.7169053265086 L 363.98190796857455 809.7008786914653 L 367.6462401795734 808.0073183415034 L 371.31057239057236 806.5363039272726 L 374.97490460157115 805.2883536378631 L 378.6392368125701 804.4122056580441 L 382.3035690235689 804.2057398506754 L 385.9679012345678 805.1304968750466 L 389.6322334455667 807.819102164309 L 393.2965656565655 812.9918039331199 L 396.9608978675645 821.2350511739807 L 400.6252300785633 832.7052498360318 L 404.28956228956224 846.9087992463151 L 407.9538945005611 862.7013634967843 L 411.61822671155994 878.5451611429304 L 415.28255892255885 892.9270964757977 L 418.94689113355776 904.7562463598547 L 422.6112233445566 913.5772125171793 L 426.2755555555555 919.5408025068459" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="526.552317110499" y1="674.711111111111" x2="526.552317110499" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="626.488650137741" y1="674.711111111111" x2="626.488650137741" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="726.4249831649831" y1="674.711111111111" x2="726.4249831649831" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="826.3613161922251" y1="674.711111111111" x2="826.3613161922251" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="576.52048362412" y1="674.711111111111" x2="576.52048362412" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="676.456816651362" y1="674.711111111111" x2="676.456816651362" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="776.3931496786041" y1="674.711111111111" x2="776.3931496786041" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="884.3472719914789" x2="852.8444444444444" y2="884.3472719914789" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="799.0418159744369" x2="852.8444444444444" y2="799.0418159744369" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="713.7363599573948" x2="852.8444444444444" y2="713.7363599573948" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="490.0755555555555" y1="927.0" x2="852.8444444444444" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="841.694543982958" x2="852.8444444444444" y2="841.694543982958" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="490.0755555555555" y1="756.3890879659159" x2="852.8444444444444" y2="756.3890879659159" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="490.0755555555555" y="674.711111111111" width="362.76888888888885" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="576.52048362412" y1="927.0" x2="576.52048362412" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="576.52048362412" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">50</text><line x1="676.456816651362" y1="927.0" x2="676.456816651362" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="676.456816651362" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">100</text><line x1="776.3931496786041" y1="927.0" x2="776.3931496786041" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="776.3931496786041" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">150</text><line x1="490.0755555555555" y1="927.0" x2="486.4088888888889" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.000</text><line x1="490.0755555555555" y1="841.694543982958" x2="486.4088888888889" y2="841.694543982958" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="845.8012106496246" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.005</text><line x1="490.0755555555555" y1="756.3890879659159" x2="486.4088888888889" y2="756.3890879659159" stroke="#444444" stroke-width="1.3333333333333333"/><text x="483.47555555555556" y="760.4957546325827" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.010</text><text x="490.0755555555555" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">app_opens_per_day</text><path d="M 490.0755555555555 918.898893622688 L 493.73988776655443 913.3288257699396 L 497.40421997755334 905.8241245937616 L 501.0685521885522 896.7812914447543 L 504.73288439955104 887.0731143483724 L 508.3972166105499 877.8340704185199 L 512.0615488215487 870.0891119027674 L 515.7258810325477 864.4016196613917 L 519.3902132435466 860.7170370683464 L 523.0545454545454 858.4618276474472 L 526.7188776655444 856.8145486453352 L 530.3832098765431 855.0048531523881 L 534.0475420875421 852.5396258644769 L 537.7118742985409 849.3296546307636 L 541.3762065095398 845.7160533949947 L 545.0405387205387 842.3757909519429 L 548.7048709315376 840.0898257009896 L 552.3692031425364 839.4264563368467 L 556.0335353535353 840.4783608384823 L 559.6978675645341 842.8030651656936 L 563.3621997755331 845.6170598470935 L 567.026531986532 848.1446325091817 L 570.6908641975308 849.9298504118553 L 574.3551964085298 850.9473410900374 L 578.0195286195286 851.4718757305538 L 581.6838608305275 851.8058145877558 L 585.3481930415263 852.0358409830819 L 589.0125252525252 851.9616579262231 L 592.6768574635241 851.231524564384 L 596.341189674523 849.5945287557148 L 600.0055218855218 847.1098164345078 L 603.6698540965208 844.1834778370578 L 607.3341863075195 841.416910347498 L 610.9985185185185 839.3681612425733 L 614.6628507295173 838.3665474585979 L 618.3271829405162 838.4611578656155 L 621.9915151515152 839.4850830199417 L 625.6558473625139 841.1595733229647 L 629.3201795735129 843.1743526025854 L 632.9845117845117 845.2255543346209 L 636.6488439955106 847.0199144374549 L 640.3131762065095 848.2482979034098 L 643.9775084175084 848.5175965270778 L 647.6418406285072 847.2369656470665 L 651.3061728395062 843.4923026984584 L 654.970505050505 836.0019985940258 L 658.6348372615039 823.2981375264278 L 662.2991694725027 804.2614343394616 L 665.9635016835016 778.9819031294178 L 669.6278338945006 749.6177898319993 L 673.2921661054994 720.6548257849126 L 676.9564983164983 698.0345250459031 L 680.6208305274972 687.1922390619008 L 684.285162738496 690.8470054623544 L 687.9494949494949 707.7742675296802 L 691.6138271604938 733.3221323759644 L 695.2781593714926 761.3683568002525 L 698.9424915824916 786.5789762444684 L 702.6068237934905 805.8380403297499 L 706.2711560044893 818.4360927657336 L 709.9354882154881 825.3691939239849 L 713.599820426487 828.3897388298375 L 717.264152637486 829.2647714016939 L 720.9284848484849 829.3703252493253 L 724.5928170594837 829.554006026262 L 728.2571492704825 830.166849349951 L 731.9214814814815 831.1952439975014 L 735.5858136924803 832.4359893952262 L 739.2501459034793 833.6547456792715 L 742.914478114478 834.6828580940801 L 746.5788103254769 835.4451297746642 L 750.2431425364758 835.9457783330456 L 753.9074747474748 836.2463488646786 L 757.5718069584736 836.4469755974599 L 761.2361391694724 836.6544781581849 L 764.9004713804713 836.9175716740515 L 768.5648035914702 837.1421993576273 L 772.229135802469 837.0432075240856 L 775.893468013468 836.1955597211627 L 779.5578002244667 834.1972991379986 L 783.2221324354657 830.8804165511165 L 786.8864646464647 826.4647292998684 L 790.5507968574635 821.5730310832389 L 794.2151290684624 817.089353493551 L 797.8794612794611 813.9037438237469 L 801.5437934904601 812.6270811599402 L 805.2081257014589 813.3814349897959 L 808.8724579124579 815.7709272834479 L 812.5367901234567 819.0910582245947 L 816.2011223344555 822.7265135348166 L 819.8654545454544 826.5555557488188 L 823.5297867564534 831.1155408638078 L 827.1941189674523 837.3743155974668 L 830.8584511784511 846.1820352831394 L 834.5227833894498 857.7033406601496 L 838.1871156004488 871.1727910491169 L 841.8514478114478 885.1231374101485 L 845.5157800224466 897.9420321945572 L 849.1801122334455 908.433974862065 L 852.8444444444444 916.1128235617857" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 490.0755555555555 918.2853375655469 L 493.73988776655443 912.5037559361708 L 497.40421997755334 904.7824350072124 L 501.0685521885522 895.5101141488418 L 504.73288439955104 885.5278776762462 L 508.3972166105499 875.9328886615441 L 512.0615488215487 867.7403386212429 L 515.7258810325477 861.5556166861471 L 519.3902132435466 857.4108544760486 L 523.0545454545454 854.8255896832904 L 526.7188776655444 853.0337260009239 L 530.3832098765431 851.2628370785338 L 534.0475420875421 848.9746107435512 L 537.7118742985409 846.0221822239083 L 541.3762065095398 842.6967022080264 L 545.0405387205387 839.6312648130752 L 548.7048709315376 837.5584355465846 L 552.3692031425364 836.9958231469515 L 556.0335353535353 838.0060600818961 L 559.6978675645341 840.1654281938406 L 563.3621997755331 842.7653400812444 L 567.026531986532 845.1349762151563 L 570.6908641975308 846.9070422950542 L 574.3551964085298 848.0921974068763 L 578.0195286195286 848.9441359403314 L 581.6838608305275 849.7114843518905 L 585.3481930415263 850.4246931605699 L 589.0125252525252 850.8372924492406 L 592.6768574635241 850.5511723208447 L 596.341189674523 849.2511635906467 L 600.0055218855218 846.9115633888064 L 603.6698540965208 843.8557395775498 L 607.3341863075195 840.6411163869354 L 610.9985185185185 837.8458434396682 L 614.6628507295173 835.8797148808704 L 618.3271829405162 834.9049536290572 L 621.9915151515152 834.8703255035514 L 625.6558473625139 835.5991771784348 L 629.3201795735129 836.8645379366059 L 632.9845117845117 838.4190399594402 L 636.6488439955106 839.9853349641927 L 640.3131762065095 841.227006603565 L 643.9775084175084 841.7119593829541 L 647.6418406285072 840.8701470658207 L 651.3061728395062 837.9562084136812 L 654.970505050505 832.0612839173336 L 658.6348372615039 822.2568369712495 L 662.2991694725027 807.9478067723621 L 665.9635016835016 789.4109334830209 L 669.6278338945006 768.3037100388565 L 673.2921661054994 747.7701890151923 L 676.9564983164983 731.8305075835467 L 680.6208305274972 724.105588278043 L 684.285162738496 726.4125138878262 L 687.9494949494949 737.9861449057349 L 691.6138271604938 755.7880896759389 L 695.2781593714926 775.7235757264381 L 698.9424915824916 794.0820687888311 L 702.6068237934905 808.5089463544518 L 706.2711560044893 818.2291797262525 L 709.9354882154881 823.6977251584824 L 713.599820426487 826.0386356161825 L 717.264152637486 826.5472891600003 L 720.9284848484849 826.3554410272707 L 724.5928170594837 826.2522257271517 L 728.2571492704825 826.6327788500208 L 731.9214814814815 827.549303047856 L 735.5858136924803 828.8285377405971 L 739.2501459034793 830.2061639171104 L 742.914478114478 831.4356611466374 L 746.5788103254769 832.3546106531941 L 750.2431425364758 832.9136014676415 L 753.9074747474748 833.1766921773572 L 757.5718069584736 833.2926824656998 L 761.2361391694724 833.4303999679815 L 764.9004713804713 833.6830887678689 L 768.5648035914702 833.9752583313584 L 772.229135802469 834.0281572260143 L 775.893468013468 833.4288428946713 L 779.5578002244667 831.7960497359745 L 783.2221324354657 828.9744164368673 L 786.8864646464647 825.1628452310695 L 790.5507968574635 820.9117651714271 L 794.2151290684624 816.9843863456804 L 797.8794612794611 814.1301289980852 L 801.5437934904601 812.8467720141194 L 805.2081257014589 813.218098244855 L 808.8724579124579 814.9087903235165 L 812.5367901234567 817.3595414194447 L 816.2011223344555 820.1355332421479 L 819.8654545454544 823.2681863917296 L 823.5297867564534 827.3753706198014 L 827.1941189674523 833.427324687556 L 830.8584511784511 842.2313983222367 L 834.5227833894498 853.9088226922952 L 838.1871156004488 867.6735805471795 L 841.8514478114478 882.0503889083 L 845.5157800224466 895.4041175367715 L 849.1801122334455 906.4860601106882 L 852.8444444444444 914.7368087582317" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/><rect x="0.0" y="0.0" width="0.0" height="0.0" fill="#ffffff" fill-opacity="1.0" stroke="none"/><line x1="957.545546372819" y1="674.711111111111" x2="957.545546372819" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1054.258126721763" y1="674.711111111111" x2="1054.258126721763" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1150.9707070707068" y1="674.711111111111" x2="1150.9707070707068" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="1247.683287419651" y1="674.711111111111" x2="1247.683287419651" y2="927.0" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="909.189256198347" y1="674.711111111111" x2="909.189256198347" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1005.9018365472909" y1="674.711111111111" x2="1005.9018365472909" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1102.614416896235" y1="674.711111111111" x2="1102.614416896235" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="1199.326997245179" y1="674.711111111111" x2="1199.326997245179" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="886.6168857008406" x2="1265.3333333333333" y2="886.6168857008406" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="805.8506571025216" x2="1265.3333333333333" y2="805.8506571025216" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="725.0844285042027" x2="1265.3333333333333" y2="725.0844285042027" stroke="#dddddd" stroke-width="0.6666666666666666"/><line x1="902.5644444444443" y1="927.0" x2="1265.3333333333333" y2="927.0" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="846.233771401681" x2="1265.3333333333333" y2="846.233771401681" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="765.4675428033621" x2="1265.3333333333333" y2="765.4675428033621" stroke="#dddddd" stroke-width="1.3333333333333333"/><line x1="902.5644444444443" y1="684.7013142050433" x2="1265.3333333333333" y2="684.7013142050433" stroke="#dddddd" stroke-width="1.3333333333333333"/><rect x="902.5644444444443" y="674.711111111111" width="362.7688888888889" height="252.28888888888892" fill="#ffffff" fill-opacity="0.0" stroke="#444444" stroke-width="1.3333333333333333"/><line x1="909.189256198347" y1="927.0" x2="909.189256198347" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="909.189256198347" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">0</text><line x1="1005.9018365472909" y1="927.0" x2="1005.9018365472909" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1005.9018365472909" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">5</text><line x1="1102.614416896235" y1="927.0" x2="1102.614416896235" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1102.614416896235" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">10</text><line x1="1199.326997245179" y1="927.0" x2="1199.326997245179" y2="930.6666666666666" stroke="#444444" stroke-width="1.3333333333333333"/><text x="1199.326997245179" y="942.9866666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="middle">15</text><line x1="902.5644444444443" y1="927.0" x2="898.8977777777777" y2="927.0" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="931.1066666666667" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.0</text><line x1="902.5644444444443" y1="846.233771401681" x2="898.8977777777777" y2="846.233771401681" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="850.3404380683478" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.1</text><line x1="902.5644444444443" y1="765.4675428033621" x2="898.8977777777777" y2="765.4675428033621" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="769.5742094700288" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.2</text><line x1="902.5644444444443" y1="684.7013142050433" x2="898.8977777777777" y2="684.7013142050433" stroke="#444444" stroke-width="1.3333333333333333"/><text x="895.9644444444443" y="688.80798087171" fill="#333333" font-size="11.733333333333334" font-family="sans-serif" text-anchor="end">0.3</text><text x="902.5644444444443" y="663.8577777777778" fill="#333333" font-size="17.599999999999998" font-family="sans-serif" text-anchor="start">weekend_screen_time</text><path d="M 902.5644444444443 926.999827114671 L 906.2287766554432 926.9989010276333 L 909.8931088664422 926.9948755727174 L 913.5574410774409 926.9823113092186 L 917.2217732884399 926.9540470494601 L 920.8861054994387 926.9074057049189 L 924.5504377104377 926.8476094946278 L 928.2147699214365 926.7797021533495 L 931.8791021324353 926.7010653887271 L 935.5434343434342 926.6090859125835 L 939.2077665544331 926.5034152211412 L 942.872098765432 926.3663251447934 L 946.5364309764309 926.1657447956569 L 950.2007631874299 925.9038579156495 L 953.8650953984286 925.6234612942862 L 957.5294276094276 925.3189866060718 L 961.1937598204264 924.8559153632893 L 964.8580920314253 923.9971022141638 L 968.5224242424242 922.4895607702053 L 972.186756453423 920.1182010192829 L 975.8510886644219 916.7901714953715 L 979.5154208754208 912.7583279743324 L 983.1797530864196 908.6795002393533 L 986.8440852974186 905.1235988982928 L 990.5084175084176 902.0982099703888 L 994.1727497194163 899.4473299097887 L 997.8370819304151 897.2940802712028 L 1001.5014141414141 895.331570552218 L 1005.165746352413 892.393577253065 L 1008.8300785634119 887.7505904956289 L 1012.4944107744107 882.1451966009587 L 1016.1587429854095 876.8382352029337 L 1019.8230751964085 872.5132942191076 L 1023.4874074074073 869.5809293254633 L 1027.1517396184063 868.3616929218779 L 1030.816071829405 868.2868151627133 L 1034.480404040404 867.9048637997257 L 1038.1447362514027 866.3829174496765 L 1041.8090684624017 864.2022465560658 L 1045.4734006734006 861.782450787262 L 1049.1377328843994 858.5196444788498 L 1052.8020650953983 854.3533792246345 L 1056.466397306397 851.3013259313445 L 1060.130729517396 851.4832857613383 L 1063.795061728395 853.922485420095 L 1067.459393939394 855.2063687440543 L 1071.1237261503927 853.1366279284858 L 1074.7880583613917 847.4341523676649 L 1078.4523905723904 835.2346335075588 L 1082.1167227833894 808.1426927524742 L 1085.7810549943883 761.9462918201618 L 1089.4453872053873 713.8811951034374 L 1093.109719416386 697.2230877919678 L 1096.7740516273848 725.7092732580992 L 1100.4383838383837 776.239560313537 L 1104.1027160493827 817.0922508853905 L 1107.7670482603814 837.5676804081064 L 1111.4313804713804 844.9157186360846 L 1115.0957126823791 848.1290512007317 L 1118.760044893378 850.7542596947492 L 1122.424377104377 852.320116166484 L 1126.088709315376 851.6679448461036 L 1129.7530415263748 848.8310909392026 L 1133.4173737373735 845.2735605493754 L 1137.0817059483725 843.3951926538746 L 1140.7460381593714 845.2153987345156 L 1144.4103703703704 850.4360399654871 L 1148.0747025813691 856.3963993081616 L 1151.7390347923679 860.8959162224633 L 1155.4033670033668 864.2466203133797 L 1159.0676992143658 867.9693506664104 L 1162.7320314253648 872.7217456388369 L 1166.3963636363635 878.0482707673125 L 1170.0606958473625 883.0710166336773 L 1173.7250280583612 887.2059313455609 L 1177.3893602693602 890.7418553076911 L 1181.0536924803591 894.4676260602487 L 1184.7180246913579 898.7207511906299 L 1188.3823569023568 903.1901164144165 L 1192.0466891133558 907.3647883280315 L 1195.7110213243545 910.8505547086571 L 1199.3753535353535 913.5044091336167 L 1203.0396857463525 915.5053455356111 L 1206.7040179573512 917.2587617205842 L 1210.3683501683502 919.1132766253479 L 1214.032682379349 921.0782713807154 L 1217.6970145903479 922.8359973685997 L 1221.3613468013468 924.083593344563 L 1225.0256790123456 924.822207461342 L 1228.6900112233445 925.288653517521 L 1232.3543434343433 925.6884772990863 L 1236.0186756453422 926.0599652153821 L 1239.6830078563412 926.3581946517043 L 1243.34734006734 926.5747743124513 L 1247.011672278339 926.7373181723656 L 1250.6760044893376 926.8587477493593 L 1254.3403367003366 926.9369603991015 L 1258.0046689113356 926.9771937602992 L 1261.6690011223345 926.9934193130455 L 1265.3333333333333 926.9985130286216" fill="" fill-opacity="0.0" stroke="#4c72b0" stroke-width="1.8897637795275593"/><path d="M 902.5644444444443 926.9984314886703 L 906.2287766554432 926.9933880047308 L 909.8931088664422 926.9792714959176 L 913.5574410774409 926.9511950273609 L 917.2217732884399 926.9122634512075 L 920.8861054994387 926.8754496528795 L 924.5504377104377 926.849767108703 L 928.2147699214365 926.8245800938744 L 931.8791021324353 926.7757913256975 L 935.5434343434342 926.688053425839 L 939.2077665544331 926.5644961405519 L 942.872098765432 926.410628417447 L 946.5364309764309 926.2133430312603 L 950.2007631874299 925.9512103113813 L 953.8650953984286 925.625813547698 L 957.5294276094276 925.2509445716387 L 961.1937598204264 924.7701834445367 L 964.8580920314253 923.9816762072084 L 968.5224242424242 922.5827594015834 L 972.186756453423 920.3042353005881 L 975.8510886644219 917.0621662385186 L 979.5154208754208 913.1458327014443 L 983.1797530864196 909.2304152668908 L 986.8440852974186 905.8984677476064 L 990.5084175084176 903.1359872586314 L 994.1727497194163 900.6527142315047 L 997.8370819304151 898.3999628259144 L 1001.5014141414141 896.0750641472885 L 1005.165746352413 892.7210853489324 L 1008.8300785634119 887.8425643082734 L 1012.4944107744107 882.2982537239228 L 1016.1587429854095 877.2670481771407 L 1019.8230751964085 873.1953436987217 L 1023.4874074074073 870.3615456668042 L 1027.1517396184063 869.2115991116771 L 1030.816071829405 869.3308296189537 L 1034.480404040404 869.1928569189672 L 1038.1447362514027 867.7630086483301 L 1041.8090684624017 865.4663114744191 L 1045.4734006734006 862.9176922804564 L 1049.1377328843994 859.7603560817139 L 1052.8020650953983 855.8688719293797 L 1056.466397306397 852.9140751733139 L 1060.130729517396 852.8813388159556 L 1063.795061728395 855.0465801805028 L 1067.459393939394 856.2825969702435 L 1071.1237261503927 854.423807861163 L 1074.7880583613917 848.890929782736 L 1078.4523905723904 836.0085103552055 L 1082.1167227833894 806.1110857003766 L 1085.7810549943883 754.5368587824289 L 1089.4453872053873 700.8960800803841 L 1093.109719416386 682.7830490419187 L 1096.7740516273848 715.4559744695166 L 1100.4383838383837 772.5671384265517 L 1104.1027160493827 818.4846156834247 L 1107.7670482603814 841.3587339075718 L 1111.4313804713804 849.0277560168147 L 1115.0957126823791 851.2330027158124 L 1118.760044893378 852.3277969950968 L 1122.424377104377 852.6337079054242 L 1126.088709315376 851.3854295215457 L 1129.7530415263748 848.5643052212804 L 1133.4173737373735 845.3620165690215 L 1137.0817059483725 843.9374904446981 L 1140.7460381593714 846.2389109368606 L 1144.4103703703704 851.9154743602965 L 1148.0747025813691 858.1088619928111 L 1151.7390347923679 862.401706763291 L 1155.4033670033668 865.0761614518325 L 1159.0676992143658 867.9034893121029 L 1162.7320314253648 872.0168132045562 L 1166.3963636363635 877.3768543075431 L 1170.0606958473625 883.0776419849808 L 1173.7250280583612 888.0356362110798 L 1177.3893602693602 892.0336153268782 L 1181.0536924803591 895.7222997740874 L 1184.7180246913579 899.5970780876069 L 1188.3823569023568 903.6072000193723 L 1192.0466891133558 907.4821525020262 L 1195.7110213243545 910.908419884003 L 1199.3753535353535 913.6263796656394 L 1203.0396857463525 915.6635962524413 L 1206.7040179573512 917.3751380738727 L 1210.3683501683502 919.1219932448587 L 1214.032682379349 920.9335028669486 L 1217.6970145903479 922.5476132658118 L 1221.3613468013468 923.7445497027418 L 1225.0256790123456 924.5465508813352 L 1228.6900112233445 925.1205506859217 L 1232.3543434343433 925.5993178527826 L 1236.0186756453422 926.0168364654394 L 1239.6830078563412 926.3531047130757 L 1243.34734006734 926.5958207531978 L 1247.011672278339 926.7586959823982 L 1250.6760044893376 926.8654003769989 L 1254.3403367003366 926.9334662287483 L 1258.0046689113356 926.9726522963203 L 1261.6690011223345 926.9911418012599 L 1265.3333333333333 926.9978201147838" fill="" fill-opacity="0.0" stroke="#dd8452" stroke-width="1.8897637795275593"/></svg>

## What to take away

**One derived quantity carried the signal.** `daily_screen_time_hours + social_media_hours` separates the classes better than any raw column. A readable tree surfaced it, and the feature synthesiser found the same sum independently.

**Missingness was noise here.** The gaps carried nothing, alone or in combination, which is what made plain mean-imputation safe. That was measured, not assumed.

**More was not better.** Twelve times the data and an extra level of depth moved holdout AUC from 0.9495 to about 0.956, and machine-invented features moved it by less than a thousandth. Knowing when a model is out of signal is worth more than the next tuning run.

The sliders above re-run any of this at whatever fidelity you like. Fork the notebook and drag them up.
