# Plotting in Sabela

We're gonna go over some common plots. We'll be using the granite library to create SVG plots.
```haskell
-- cabal: build-depends: text, granite

import Granite.Svg
import qualified Data.Text as T

```

For bar charts we specify the bar values and the lengths. 
```haskell
:set -XOverloadedStrings

displaySvg $ T.unpack (bars [("Q1",12),("Q2",18),("Q3",9),("Q4",15)] defPlot {plotTitle="Sales"})

```

> <!-- sabela:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 790 396" width="790" height="396" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="370" y="26" text-anchor="middle" fill="#222" font-size="14">Sales</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="70" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="66" y2="34" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="38" text-anchor="end" fill="#555" font-size="11">18.0</text>
> <line x1="70" y1="34" x2="670" y2="34" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="194.50" x2="66" y2="194.50" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="198.50" text-anchor="end" fill="#555" font-size="11">9.0</text>
> <line x1="70" y1="194.50" x2="670" y2="194.50" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="354" x2="66" y2="354" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="358" text-anchor="end" fill="#555" font-size="11">0.0</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#eee" stroke-width="0.50"/>
> <text x="145" y="370" text-anchor="middle" fill="#555" font-size="11">Q1</text>
> <text x="295" y="370" text-anchor="middle" fill="#555" font-size="11">Q2</text>
> <text x="445" y="370" text-anchor="middle" fill="#555" font-size="11">Q3</text>
> <text x="595" y="370" text-anchor="middle" fill="#555" font-size="11">Q4</text>
> <rect x="92.50" y="140.67" width="105" height="213.33" fill="#3498db" rx="2"/>
> <rect x="242.50" y="34.00" width="105" height="320.00" fill="#9b59b6" rx="2"/>
> <rect x="392.50" y="194.00" width="105" height="160.00" fill="#1abc9c" rx="2"/>
> <rect x="542.50" y="87.33" width="105" height="266.67" fill="#2ecc71" rx="2"/>
> <rect x="685" y="39" width="12" height="12" fill="#3498db"/>
> <text x="701" y="49" text-anchor="start" fill="#555" font-size="11">Q1</text>
> <rect x="685" y="59" width="12" height="12" fill="#9b59b6"/>
> <text x="701" y="69" text-anchor="start" fill="#555" font-size="11">Q2</text>
> <rect x="685" y="79" width="12" height="12" fill="#1abc9c"/>
> <text x="701" y="89" text-anchor="start" fill="#555" font-size="11">Q3</text>
> <rect x="685" y="99" width="12" height="12" fill="#2ecc71"/>
> <text x="701" y="109" text-anchor="start" fill="#555" font-size="11">Q4</text>
> </svg>

For stacked bar charts the API is similar but we have tuples of category and count.
```haskell
displaySvg $ T.unpack (stackedBars [ ("Q1", [("Hardware", 120), ("Software", 200), ("Services", 80)])
                                , ("Q2", [("Hardware", 135), ("Software", 220), ("Services", 95)])
                                , ("Q3", [("Hardware", 110), ("Software", 240), ("Services", 110)])
                                , ("Q4", [("Hardware", 145), ("Software", 260), ("Services", 125)])
                                ] defPlot {plotTitle="Quarterly Revenue Breakdown"})

```

> <!-- sabela:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 790 396" width="790" height="396" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="370" y="26" text-anchor="middle" fill="#222" font-size="14">Quarterly Revenue Breakdown</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="70" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="66" y2="34" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="38" text-anchor="end" fill="#555" font-size="11">530.0</text>
> <line x1="70" y1="34" x2="670" y2="34" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="194.50" x2="66" y2="194.50" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="198.50" text-anchor="end" fill="#555" font-size="11">265.0</text>
> <line x1="70" y1="194.50" x2="670" y2="194.50" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="354" x2="66" y2="354" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="358" text-anchor="end" fill="#555" font-size="11">0.0</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#eee" stroke-width="0.50"/>
> <text x="145" y="370" text-anchor="middle" fill="#555" font-size="11">Q1</text>
> <text x="295" y="370" text-anchor="middle" fill="#555" font-size="11">Q2</text>
> <text x="445" y="370" text-anchor="middle" fill="#555" font-size="11">Q3</text>
> <text x="595" y="370" text-anchor="middle" fill="#555" font-size="11">Q4</text>
> <rect x="85" y="281.55" width="120" height="72.45" fill="#3498db" rx="1"/>
> <rect x="85" y="160.79" width="120" height="120.75" fill="#9b59b6" rx="1"/>
> <rect x="85" y="112.49" width="120" height="48.30" fill="#1abc9c" rx="1"/>
> <rect x="235" y="272.49" width="120" height="81.51" fill="#3498db" rx="1"/>
> <rect x="235" y="139.66" width="120" height="132.83" fill="#9b59b6" rx="1"/>
> <rect x="235" y="82.30" width="120" height="57.36" fill="#1abc9c" rx="1"/>
> <rect x="385" y="287.58" width="120" height="66.42" fill="#3498db" rx="1"/>
> <rect x="385" y="142.68" width="120" height="144.91" fill="#9b59b6" rx="1"/>
> <rect x="385" y="76.26" width="120" height="66.42" fill="#1abc9c" rx="1"/>
> <rect x="535" y="266.45" width="120" height="87.55" fill="#3498db" rx="1"/>
> <rect x="535" y="109.47" width="120" height="156.98" fill="#9b59b6" rx="1"/>
> <rect x="535" y="34" width="120" height="75.47" fill="#1abc9c" rx="1"/>
> <rect x="685" y="39" width="12" height="12" fill="#3498db"/>
> <text x="701" y="49" text-anchor="start" fill="#555" font-size="11">Hardware</text>
> <rect x="685" y="59" width="12" height="12" fill="#9b59b6"/>
> <text x="701" y="69" text-anchor="start" fill="#555" font-size="11">Software</text>
> <rect x="685" y="79" width="12" height="12" fill="#1abc9c"/>
> <text x="701" y="89" text-anchor="start" fill="#555" font-size="11">Services</text>
> </svg>

No one likes pie charts but we have them anyway.
```haskell
displaySvg $ T.unpack (pie [("Alpha",0.35),("Beta",0.25),("Gamma",0.20),("Delta",0.20)] defPlot{widthChars=46,heightChars=18,legendPos=LegendRight,plotTitle="Share"})

```

> <!-- sabela:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 650 364" width="650" height="364" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="300" y="26" text-anchor="middle" fill="#222" font-size="14">Share</text>
> <path d="M 300 178 L 422.40 178 A 122.40 122.40 0 0 0 228.06 78.98 Z" fill="#3498db" stroke="white" stroke-width="2"/>
> <path d="M 300 178 L 228.06 78.98 A 122.40 122.40 0 0 0 200.98 249.94 Z" fill="#9b59b6" stroke="white" stroke-width="2"/>
> <path d="M 300 178 L 200.98 249.94 A 122.40 122.40 0 0 0 337.82 294.41 Z" fill="#1abc9c" stroke="white" stroke-width="2"/>
> <path d="M 300 178 L 337.82 294.41 A 122.40 122.40 0 0 0 422.40 178.00 Z" fill="#2ecc71" stroke="white" stroke-width="2"/>
> <rect x="545" y="39" width="12" height="12" fill="#3498db"/>
> <text x="561" y="49" text-anchor="start" fill="#555" font-size="11">Alpha</text>
> <rect x="545" y="59" width="12" height="12" fill="#9b59b6"/>
> <text x="561" y="69" text-anchor="start" fill="#555" font-size="11">Beta</text>
> <rect x="545" y="79" width="12" height="12" fill="#1abc9c"/>
> <text x="561" y="89" text-anchor="start" fill="#555" font-size="11">Gamma</text>
> <rect x="545" y="99" width="12" height="12" fill="#2ecc71"/>
> <text x="561" y="109" text-anchor="start" fill="#555" font-size="11">Delta</text>
> </svg>

If you fancy yourself a statistician you can try out box plots.
```haskell
displaySvg $ T.unpack $ boxPlot [ ("Class A", [78, 82, 85, 88, 90, 92, 85, 87, 89, 91, 76, 94, 88])
                            , ("Class B", [70, 75, 72, 80, 85, 78, 82, 77, 79, 81, 74, 83])
                            , ("Class C", [88, 92, 95, 90, 93, 89, 91, 94, 96, 87, 90, 92])
                            , ("Class D", [65, 70, 72, 68, 75, 80, 73, 71, 69, 74, 77, 76])
                            ] defPlot {plotTitle="Test Score Distribution by Class"}

```

> <!-- sabela:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 790 396" width="790" height="396" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="370" y="26" text-anchor="middle" fill="#222" font-size="14">Test Score Distribution by Class</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="70" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="66" y2="34" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="38" text-anchor="end" fill="#555" font-size="11">105.6</text>
> <line x1="70" y1="34" x2="670" y2="34" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="194.50" x2="66" y2="194.50" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="198.50" text-anchor="end" fill="#555" font-size="11">82.0</text>
> <line x1="70" y1="194.50" x2="670" y2="194.50" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="354" x2="66" y2="354" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="358" text-anchor="end" fill="#555" font-size="11">58.5</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#eee" stroke-width="0.50"/>
> <text x="145" y="370" text-anchor="middle" fill="#555" font-size="11">Class A</text>
> <text x="295" y="370" text-anchor="middle" fill="#555" font-size="11">Class B</text>
> <text x="445" y="370" text-anchor="middle" fill="#555" font-size="11">Class C</text>
> <text x="595" y="370" text-anchor="middle" fill="#555" font-size="11">Class D</text>
> <line x1="145" y1="235.10" x2="145" y2="173.96" stroke="#3498db" stroke-width="1.50"/>
> <line x1="122.50" y1="235.10" x2="167.50" y2="235.10" stroke="#3498db" stroke-width="1.50"/>
> <line x1="145" y1="139.99" x2="145" y2="112.81" stroke="#3498db" stroke-width="1.50"/>
> <line x1="122.50" y1="112.81" x2="167.50" y2="112.81" stroke="#3498db" stroke-width="1.50"/>
> <rect x="100" y="139.99" width="90" height="33.97" fill="#3498db" fill-opacity="0.3" stroke="#3498db" stroke-width="1.5" rx="2"/>
> <line x1="100" y1="153.58" x2="190" y2="153.58" stroke="#3498db" stroke-width="2.50"/>
> <line x1="295" y1="275.87" x2="295" y2="241.90" stroke="#9b59b6" stroke-width="1.50"/>
> <line x1="272.50" y1="275.87" x2="317.50" y2="275.87" stroke="#9b59b6" stroke-width="1.50"/>
> <line x1="295" y1="194.34" x2="295" y2="173.96" stroke="#9b59b6" stroke-width="1.50"/>
> <line x1="272.50" y1="173.96" x2="317.50" y2="173.96" stroke="#9b59b6" stroke-width="1.50"/>
> <rect x="250" y="194.34" width="90" height="47.56" fill="#9b59b6" fill-opacity="0.3" stroke="#9b59b6" stroke-width="1.5" rx="2"/>
> <line x1="250" y1="214.72" x2="340" y2="214.72" stroke="#9b59b6" stroke-width="2.50"/>
> <line x1="445" y1="160.37" x2="445" y2="139.99" stroke="#1abc9c" stroke-width="1.50"/>
> <line x1="422.50" y1="160.37" x2="467.50" y2="160.37" stroke="#1abc9c" stroke-width="1.50"/>
> <line x1="445" y1="112.81" x2="445" y2="99.22" stroke="#1abc9c" stroke-width="1.50"/>
> <line x1="422.50" y1="99.22" x2="467.50" y2="99.22" stroke="#1abc9c" stroke-width="1.50"/>
> <rect x="400" y="112.81" width="90" height="27.18" fill="#1abc9c" fill-opacity="0.3" stroke="#1abc9c" stroke-width="1.5" rx="2"/>
> <line x1="400" y1="126.40" x2="490" y2="126.40" stroke="#1abc9c" stroke-width="2.50"/>
> <line x1="595" y1="309.84" x2="595" y2="275.87" stroke="#2ecc71" stroke-width="1.50"/>
> <line x1="572.50" y1="309.84" x2="617.50" y2="309.84" stroke="#2ecc71" stroke-width="1.50"/>
> <line x1="595" y1="235.10" x2="595" y2="207.93" stroke="#2ecc71" stroke-width="1.50"/>
> <line x1="572.50" y1="207.93" x2="617.50" y2="207.93" stroke="#2ecc71" stroke-width="1.50"/>
> <rect x="550" y="235.10" width="90" height="40.76" fill="#2ecc71" fill-opacity="0.3" stroke="#2ecc71" stroke-width="1.5" rx="2"/>
> <line x1="550" y1="255.49" x2="640" y2="255.49" stroke="#2ecc71" stroke-width="2.50"/>
> <rect x="685" y="39" width="12" height="12" fill="#3498db"/>
> <text x="701" y="49" text-anchor="start" fill="#555" font-size="11">Class A</text>
> <rect x="685" y="59" width="12" height="12" fill="#9b59b6"/>
> <text x="701" y="69" text-anchor="start" fill="#555" font-size="11">Class B</text>
> <rect x="685" y="79" width="12" height="12" fill="#1abc9c"/>
> <text x="701" y="89" text-anchor="start" fill="#555" font-size="11">Class C</text>
> <rect x="685" y="99" width="12" height="12" fill="#2ecc71"/>
> <text x="701" y="109" text-anchor="start" fill="#555" font-size="11">Class D</text>
> </svg>

The humble line plot.
```haskell
displaySvg $ T.unpack $ lineGraph [ ("Product A", [(1, 100), (2, 120), (3, 115), (4, 140), (5, 155), (6, 148)])
                              , ("Product B", [(1, 80), (2, 85), (3, 95), (4, 92), (5, 110), (6, 125)])
                              , ("Product C", [(1, 60), (2, 62), (3, 70), (4, 85), (5, 82), (6, 90)])
                              ] defPlot {plotTitle="Monthly Sales Trends"}

```

> <!-- sabela:mime image/svg+xml -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 790 396" width="790" height="396" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="370" y="26" text-anchor="middle" fill="#222" font-size="14">Monthly Sales Trends</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="70" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="70" y1="34" x2="66" y2="34" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="38" text-anchor="end" fill="#555" font-size="11">159.8</text>
> <line x1="70" y1="34" x2="670" y2="34" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="194.50" x2="66" y2="194.50" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="198.50" text-anchor="end" fill="#555" font-size="11">107.5</text>
> <line x1="70" y1="194.50" x2="670" y2="194.50" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="354" x2="66" y2="354" stroke="#aaa" stroke-width="1"/>
> <text x="62" y="358" text-anchor="end" fill="#555" font-size="11">55.2</text>
> <line x1="70" y1="354" x2="670" y2="354" stroke="#eee" stroke-width="0.50"/>
> <line x1="70" y1="354" x2="70" y2="358" stroke="#aaa" stroke-width="1"/>
> <text x="70" y="370" text-anchor="middle" fill="#555" font-size="11">0.7</text>
> <line x1="70" y1="34" x2="70" y2="354" stroke="#eee" stroke-width="0.50"/>
> <line x1="370.50" y1="354" x2="370.50" y2="358" stroke="#aaa" stroke-width="1"/>
> <text x="370.50" y="370" text-anchor="middle" fill="#555" font-size="11">3.5</text>
> <line x1="370.50" y1="34" x2="370.50" y2="354" stroke="#eee" stroke-width="0.50"/>
> <line x1="670" y1="354" x2="670" y2="358" stroke="#aaa" stroke-width="1"/>
> <text x="670" y="370" text-anchor="middle" fill="#555" font-size="11">6.3</text>
> <line x1="670" y1="34" x2="670" y2="354" stroke="#eee" stroke-width="0.50"/>
> <polyline points="97.27,216.97 206.36,155.72 315.45,171.03 424.55,94.48 533.64,48.55 642.73,69.98" fill="none" stroke="#3498db" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="97.27,278.21 206.36,262.90 315.45,232.28 424.55,241.46 533.64,186.34 642.73,140.41" fill="none" stroke="#9b59b6" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <polyline points="97.27,339.45 206.36,333.33 315.45,308.83 424.55,262.90 533.64,272.09 642.73,247.59" fill="none" stroke="#1abc9c" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <rect x="685" y="39" width="12" height="12" fill="#3498db"/>
> <text x="701" y="49" text-anchor="start" fill="#555" font-size="11">Product A</text>
> <rect x="685" y="59" width="12" height="12" fill="#9b59b6"/>
> <text x="701" y="69" text-anchor="start" fill="#555" font-size="11">Product B</text>
> <rect x="685" y="79" width="12" height="12" fill="#1abc9c"/>
> <text x="701" y="89" text-anchor="start" fill="#555" font-size="11">Product C</text>
> </svg>

