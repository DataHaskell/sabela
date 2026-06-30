```haskell
-- cabal: build-depends: dataframe ==2.1.0.3, dataframe-hasktorch ==0.2.0.1, hasktorch, text, random, granite
-- cabal: source-repository-package: https://github.com/kutyel/granite.git 0474178eeee4a9e4b4f732ebbf18fe9c231c99b0 
-- cabal: default-extensions: BangPatterns, NumericUnderscores, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications
import qualified DataFrame as D

raw <- D.readCsv "./examples/data/top5_attackers.csv"
df = D.parseDefaults D.defaultParseOptions raw
D.dimensions df
```

> <!-- scripths:mime text/plain -->
> (933,20)

Let's first have a look at the available data:

```haskell
import DataFrame.Operators

df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | season<br>Maybe Text | league<br>Maybe Text  |    team<br>Maybe Text     |       player<br>Maybe Text       | nationality<br>Maybe Text | position<br>Maybe Text | age<br>Double | minutes<br>Double | minutes_90s<br>Double | goals<br>Double | assists<br>Double | goals_assists<br>Double | goals_per90<br>Double | assists_per90<br>Double | goals_assists_per90<br>Double | xg<br>Double | xg_assist<br>Double | npxg<br>Double | shots<br>Double | shots_on_target<br>Double |
> | ---------------------|-----------------------|---------------------------|----------------------------------|---------------------------|------------------------|---------------|-------------------|-----------------------|-----------------|-------------------|-------------------------|-----------------------|-------------------------|-------------------------------|--------------|---------------------|----------------|-----------------|-------------------------- |
> | Just "2018-2019"     | Just "Premier League" | Just "Tottenham"          | Just "Georges-K\233vin N'Koudou" | Just "CMR"                | Just "FW"              | 23.0          | 5.0               | 0.1                   | 0.0             | 1.0               | 1.0                     | 0.0                   | 18.0                    | 18.0                          | 0.0          | 0.5                 | 0.0            | 0.0             | 0.0                       |
> | Just "2018-2019"     | Just "La Liga"        | Just "Villarreal"         | Just "Nicola Sansone"            | Just "ITA"                | Just "FW,MF"           | 26.0          | 37.0              | 0.4                   | 1.0             | 1.0               | 2.0                     | 2.43                  | 2.43                    | 4.86                          | 0.0          | 0.0                 | 0.0            | 1.0             | 1.0                       |
> | Just "2018-2019"     | Just "La Liga"        | Just "Atl\233tico Madrid" | Just "Borja Garc\233s"           | Just "ESP"                | Just "FW"              | 18.0          | 20.0              | 0.2                   | 1.0             | 0.0               | 1.0                     | 4.5                   | 0.0                     | 4.5                           | 0.5          | 0.0                 | 0.5            | 3.0             | 1.0                       |
> | Just "2018-2019"     | Just "Ligue 1"        | Just "Saint-\201tienne"   | Just "Makhtar Gueye"             | Just "SEN"                | Just "FW,MF"           | 20.0          | 50.0              | 0.6                   | 1.0             | 1.0               | 2.0                     | 1.8                   | 1.8                     | 3.6                           | 0.2          | 0.4                 | 0.2            | 2.0             | 1.0                       |
> | Just "2018-2019"     | Just "Bundesliga"     | Just "Hannover 96"        | Just "Benjamin Had\382i\263"     | Just "BIH"                | Just "FW,MF"           | 19.0          | 25.0              | 0.3                   | 0.0             | 1.0               | 1.0                     | 0.0                   | 3.6                     | 3.6                           | 0.0          | 0.0                 | 0.0            | 0.0             | 0.0                       |

# Who is the best striker? A z-score story

The viral version of this chart is a few lines of `pandas` + `numpy`: take every
attacker in the top-5 leagues (here the 2018-19 season), look at goals + assists
per 90 minutes, and measure how many standard deviations each player sits above
the average. The freaks of the distribution — the ones living out at 4σ, 5σ —
are the elite. Spoiler: Messi is alone out there.

We tell the same story in Haskell: `dataframe` for the data, **hasktorch**
tensors for the mean / standard deviation / z-scores, and
[`granite`](https://github.com/mchav/granite)'s brand-new `gauss` helper — a
one-call bell-curve-with-markers chart — for the plot.

## Qualify the sample

Per-90 rates are meaningless for someone who played four minutes — one lucky
goal would read as an absurd rate. So we keep only players with a real sample:
at least ten full matches (`minutes_90s >= 10`, i.e. 900+ minutes), then pull
goals-plus-assists-per-90 out as a plain list of `Double`s.

```haskell
import qualified DataFrame.Functions as F
import Data.Text (Text)

qualified = df |> D.filter (F.col @Double "minutes_90s") (>= 10)

ga90 = D.columnAsList (F.col @Double "goals_assists_per90") qualified :: [Double]

(D.dimensions qualified, Prelude.take 5 ga90)
```

> <!-- scripths:mime text/plain -->
> ((513,20),[1.63,1.54,1.38,1.34,1.24])

## The stats, in hasktorch

This is the part the image does with `numpy`. We move the column into a hasktorch
tensor and let Torch compute the mean and (unbiased) standard deviation. A
z-score is then just `(value - μ) / σ` — and because tensors broadcast, the whole
column standardises in a single elementwise expression, no loop required.

```haskell
import Torch

vals :: Tensor
vals = asTensor (map realToFrac ga90 :: [Float])

mu = asValue @Float (mean vals)
sd = asValue @Float (std vals)

zTensor = Torch.div (Torch.sub vals (mean vals)) (std vals)
zScores = asValue @[Float] zTensor :: [Float]

putStrLn ("average G+A/90 = " <> show mu <> "  |  std dev = " <> show sd)
```

> <!-- scripths:mime text/plain -->
> average G+A/90 = 0.4231189  |  std dev = 0.2426023

## The outliers

We attach the z-scores back onto the frame and sort descending. These are the
players furthest out on the right tail — the strikers everyone argues about.

```haskell
withZ = qualified |> D.insert "z" (map realToFrac zScores :: [Double])

leaders = withZ |> D.sortBy [D.Desc (F.col @Double "z")] |> D.take 8

leaders |> D.select ["player", "team", "goals_assists_per90", "z"]
        |> D.toMarkdown'
        |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> |  player<br>Maybe Text   |   team<br>Maybe Text   | goals_assists_per90<br>Double |    z<br>Double     |
> | ------------------------|------------------------|-------------------------------|------------------- |
> | Just "Lionel Messi"     | Just "Barcelona"       | 1.63                          | 4.974730491638184  |
> | Just "Kylian Mbapp\233" | Just "Paris S-G"       | 1.54                          | 4.603753089904785  |
> | Just "Neymar"           | Just "Paris S-G"       | 1.38                          | 3.944237470626831  |
> | Just "Paco Alc\225cer"  | Just "Dortmund"        | 1.34                          | 3.7793588638305664 |
> | Just "Edinson Cavani"   | Just "Paris S-G"       | 1.24                          | 3.367161512374878  |
> | Just "Dries Mertens"    | Just "Napoli"          | 1.08                          | 2.707645893096924  |
> | Just "Sergio Ag\252ero" | Just "Manchester City" | 1.06                          | 2.625206232070923  |
> | Just "Iago Aspas"       | Just "Celta Vigo"      | 1.04                          | 2.542766809463501  |

## The picture

Now the plot. The original chart is one image: the right-skewed hump every
attacker lives inside, with a handful of marquee names laddered up the right
edge, and Messi alone out past 5σ. `granite` ships a helper built for exactly
this shape — `gauss` — so we don't draw it by hand. We just hand it the
population and the names we want called out; it fits the bell curve, labels the
x-axis in σ units, and drops a lollipop on each marker (highlighting the
furthest-out one). First we pick the marquee players out of the data:

```haskell
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

marquee = ["Messi", "Ronaldo", "Mbappé", "Neymar", "Lewandowski", "Salah"] :: [Text]
isMarquee p = Prelude.any (`T.isInfixOf` p) marquee

stars =
    withZ
        |> D.filter (F.col @(Maybe Text) "player") (maybe False isMarquee)
        |> D.sortBy [D.Desc (F.col @Double "z")]

starName = map (fromMaybe "?") (D.columnAsList (F.col @(Maybe Text) "player") stars) :: [Text]
starGa90 = D.columnAsList (F.col @Double "goals_assists_per90") stars :: [Double]

markers = zip starName starGa90 :: [(Text, Double)]
markers
```

> <!-- scripths:mime text/plain -->
> [("Lionel Messi",1.63),("Kylian Mbapp\233",1.54),("Neymar",1.38),("Cristiano Ronaldo",0.97),("Robert Lewandowski",0.88),("Mohamed Salah",0.83)]

That's the whole input: `ga90` (every qualified attacker — the population that
fixes μ and σ) and `markers` (the names to call out, as `(label, value)`).
`gauss` re-derives the same mean and standard deviation we computed in hasktorch
and turns the lot into one chart — no manual bins, no layers, no laddering.

```haskell
import qualified Granite.Svg as G

chart =
    G.gauss ga90 markers
        G.defPlot
            { G.widthChars = 70
            , G.heightChars = 20
            , G.plotTitle = "Goals + assists per 90 — every qualified attacker, top-5 leagues"
            }

displayHtml (T.unpack chart)
```

> <!-- scripths:mime text/html -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 890 396" width="890" height="396" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <text x="420" y="26" text-anchor="middle" fill="#222" font-size="14">Goals + assists per 90 — every qualified attacker, top-5 leagues</text>
> <path d="M 70 354 L 70 343.40 L 73.33 341.36 L 76.67 339.07 L 80.00 336.54 L 83.33 333.75 L 86.67 330.71 L 90.00 327.42 L 93.33 323.88 L 96.67 320.07 L 100.00 316.00 L 103.33 311.64 L 106.67 306.99 L 110.00 302.01 L 113.33 296.69 L 116.67 291.00 L 120.00 284.90 L 123.33 278.39 L 126.67 271.44 L 130.00 264.04 L 133.33 256.19 L 136.67 247.91 L 140.00 239.23 L 143.33 230.17 L 146.67 220.80 L 150.00 211.17 L 153.33 201.35 L 156.67 191.43 L 160.00 181.49 L 163.33 171.61 L 166.67 161.90 L 170.00 152.43 L 173.33 143.28 L 176.67 134.53 L 180.00 126.23 L 183.33 118.44 L 186.67 111.20 L 190.00 104.53 L 193.33 98.44 L 196.67 92.94 L 200.00 88.02 L 203.33 83.65 L 206.67 79.83 L 210.00 76.52 L 213.33 73.70 L 216.67 71.34 L 220.00 69.43 L 223.33 67.95 L 226.67 66.88 L 230.00 66.23 L 233.33 66.00 L 236.67 66.18 L 240.00 66.79 L 243.33 67.82 L 246.67 69.27 L 250.00 71.14 L 253.33 73.41 L 256.67 76.07 L 260.00 79.08 L 263.33 82.42 L 266.67 86.04 L 270.00 89.91 L 273.33 93.97 L 276.67 98.18 L 280.00 102.51 L 283.33 106.93 L 286.67 111.40 L 290.00 115.90 L 293.33 120.43 L 296.67 124.98 L 300.00 129.55 L 303.33 134.16 L 306.67 138.82 L 310.00 143.55 L 313.33 148.37 L 316.67 153.30 L 320.00 158.38 L 323.33 163.60 L 326.67 168.98 L 330.00 174.53 L 333.33 180.24 L 336.67 186.09 L 340.00 192.06 L 343.33 198.09 L 346.67 204.16 L 350.00 210.19 L 353.33 216.14 L 356.67 221.93 L 360.00 227.50 L 363.33 232.80 L 366.67 237.79 L 370.00 242.42 L 373.33 246.67 L 376.67 250.53 L 380.00 254.02 L 383.33 257.15 L 386.67 259.95 L 390.00 262.48 L 393.33 264.78 L 396.67 266.91 L 400.00 268.93 L 403.33 270.88 L 406.67 272.81 L 410.00 274.77 L 413.33 276.78 L 416.67 278.87 L 420.00 281.05 L 423.33 283.32 L 426.67 285.68 L 430.00 288.11 L 433.33 290.61 L 436.67 293.17 L 440.00 295.75 L 443.33 298.34 L 446.67 300.94 L 450.00 303.52 L 453.33 306.06 L 456.67 308.57 L 460.00 311.04 L 463.33 313.45 L 466.67 315.81 L 470.00 318.11 L 473.33 320.34 L 476.67 322.52 L 480.00 324.62 L 483.33 326.66 L 486.67 328.62 L 490.00 330.51 L 493.33 332.32 L 496.67 334.05 L 500.00 335.69 L 503.33 337.25 L 506.67 338.73 L 510.00 340.11 L 513.33 341.41 L 516.67 342.62 L 520.00 343.75 L 523.33 344.78 L 526.67 345.74 L 530.00 346.61 L 533.33 347.39 L 536.67 348.10 L 540.00 348.72 L 543.33 349.26 L 546.67 349.73 L 550.00 350.12 L 553.33 350.44 L 556.67 350.69 L 560.00 350.88 L 563.33 351.01 L 566.67 351.09 L 570.00 351.12 L 573.33 351.12 L 576.67 351.09 L 580.00 351.03 L 583.33 350.94 L 586.67 350.85 L 590.00 350.74 L 593.33 350.63 L 596.67 350.52 L 600.00 350.41 L 603.33 350.30 L 606.67 350.21 L 610.00 350.13 L 613.33 350.07 L 616.67 350.02 L 620.00 350.00 L 623.33 350.01 L 626.67 350.04 L 630.00 350.10 L 633.33 350.18 L 636.67 350.29 L 640.00 350.41 L 643.33 350.55 L 646.67 350.70 L 650.00 350.85 L 653.33 351.00 L 656.67 351.13 L 660.00 351.26 L 663.33 351.36 L 666.67 351.44 L 670.00 351.49 L 673.33 351.52 L 676.67 351.53 L 680.00 351.51 L 683.33 351.48 L 686.67 351.43 L 690.00 351.37 L 693.33 351.31 L 696.67 351.24 L 700.00 351.18 L 703.33 351.13 L 706.67 351.09 L 710.00 351.06 L 713.33 351.05 L 716.67 351.07 L 720.00 351.10 L 723.33 351.16 L 726.67 351.24 L 730.00 351.33 L 733.33 351.45 L 736.67 351.58 L 740.00 351.73 L 743.33 351.89 L 746.67 352.06 L 750.00 352.24 L 753.33 352.41 L 756.67 352.58 L 760.00 352.75 L 763.33 352.91 L 766.67 353.07 L 770.00 353.21 L 770.00 354 Z" fill="#dfe4e8" fill-opacity="0.6"/>
> <line x1="269.74" y1="34" x2="269.74" y2="354" stroke="#cfd6dc" stroke-width="1" stroke-dasharray="3 4"/>
> <circle cx="89.27" cy="345.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="99.59" cy="328.05" r="1.30" fill="#aeb4bb"/>
> <circle cx="90.34" cy="340.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="100.66" cy="330.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="104.75" cy="349.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="110.23" cy="305.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="110.98" cy="319.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="114.43" cy="330.54" r="1.30" fill="#aeb4bb"/>
> <circle cx="121.30" cy="305.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="123.36" cy="327.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="127.39" cy="342.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="134.54" cy="270.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="133.79" cy="293.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="133.04" cy="304.69" r="1.30" fill="#aeb4bb"/>
> <circle cx="137.71" cy="317.50" r="1.30" fill="#aeb4bb"/>
> <circle cx="136.95" cy="351.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="141.19" cy="234.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="144.22" cy="247.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="142.72" cy="281.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="148.78" cy="301.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="146.63" cy="313.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="145.88" cy="325.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="145.13" cy="341.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="151.51" cy="209.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="153.90" cy="213.53" r="1.30" fill="#aeb4bb"/>
> <circle cx="152.40" cy="242.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="158.35" cy="267.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="159.10" cy="269.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="156.31" cy="288.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="154.81" cy="322.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="156.69" cy="347.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="161.83" cy="169.96" r="1.30" fill="#aeb4bb"/>
> <circle cx="163.58" cy="194.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="162.83" cy="196.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.92" cy="216.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="169.42" cy="250.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.99" cy="263.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.24" cy="273.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="165.51" cy="289.90" r="1.30" fill="#aeb4bb"/>
> <circle cx="166.26" cy="309.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.01" cy="313.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="167.65" cy="342.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="173.26" cy="162.17" r="1.30" fill="#aeb4bb"/>
> <circle cx="172.51" cy="178.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="178.24" cy="192.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="178.99" cy="201.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="179.74" cy="217.92" r="1.30" fill="#aeb4bb"/>
> <circle cx="175.67" cy="238.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="174.92" cy="241.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="175.83" cy="257.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="177.33" cy="295.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="177.33" cy="317.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="173.42" cy="334.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="174.17" cy="350.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="183.69" cy="125.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.94" cy="133.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="182.19" cy="144.92" r="1.30" fill="#aeb4bb"/>
> <circle cx="189.31" cy="172.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.10" cy="190.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="184.60" cy="225.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.15" cy="236.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="186.90" cy="248.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="188.51" cy="264.29" r="1.30" fill="#aeb4bb"/>
> <circle cx="187.76" cy="281.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="187.01" cy="294.03" r="1.30" fill="#aeb4bb"/>
> <circle cx="184.49" cy="313.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="180.92" cy="339.62" r="1.30" fill="#aeb4bb"/>
> <circle cx="180.17" cy="341.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="192.62" cy="111.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="198.88" cy="143.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.63" cy="164.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="195.78" cy="176.85" r="1.30" fill="#aeb4bb"/>
> <circle cx="195.03" cy="189.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.28" cy="204.49" r="1.30" fill="#aeb4bb"/>
> <circle cx="196.47" cy="223.29" r="1.30" fill="#aeb4bb"/>
> <circle cx="197.22" cy="235.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="198.19" cy="244.18" r="1.30" fill="#aeb4bb"/>
> <circle cx="196.69" cy="268.33" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.06" cy="283.87" r="1.30" fill="#aeb4bb"/>
> <circle cx="194.81" cy="302.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="190.60" cy="314.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.85" cy="337.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="199.10" cy="348.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="202.30" cy="107.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="208.45" cy="120.34" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.20" cy="130.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.95" cy="146.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="204.71" cy="170.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.54" cy="224.01" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.87" cy="226.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="207.12" cy="245.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="206.37" cy="263.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="204.38" cy="279.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="205.13" cy="291.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="209.53" cy="319.34" r="1.30" fill="#aeb4bb"/>
> <circle cx="208.78" cy="338.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="201.97" cy="350.78" r="1.30" fill="#aeb4bb"/>
> <circle cx="212.73" cy="77.88" r="1.30" fill="#aeb4bb"/>
> <circle cx="211.98" cy="93.70" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.52" cy="116.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="215.14" cy="146.01" r="1.30" fill="#aeb4bb"/>
> <circle cx="214.39" cy="169.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="216.36" cy="171.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.11" cy="191.61" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.86" cy="208.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="217.55" cy="224.27" r="1.30" fill="#aeb4bb"/>
> <circle cx="216.80" cy="237.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="213.95" cy="247.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="215.45" cy="282.82" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.96" cy="286.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="219.21" cy="301.99" r="1.30" fill="#aeb4bb"/>
> <circle cx="211.54" cy="314.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="212.29" cy="339.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="213.04" cy="340.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="221.66" cy="87.28" r="1.30" fill="#aeb4bb"/>
> <circle cx="229.84" cy="122.61" r="1.30" fill="#aeb4bb"/>
> <circle cx="220.59" cy="133.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.82" cy="146.23" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.07" cy="162.23" r="1.30" fill="#aeb4bb"/>
> <circle cx="227.43" cy="192.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="226.48" cy="238.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="224.27" cy="241.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="225.02" cy="257.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="225.77" cy="271.34" r="1.30" fill="#aeb4bb"/>
> <circle cx="228.89" cy="297.17" r="1.30" fill="#aeb4bb"/>
> <circle cx="222.61" cy="334.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="223.36" cy="350.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.09" cy="77.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="231.34" cy="89.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="239.41" cy="102.62" r="1.30" fill="#aeb4bb"/>
> <circle cx="230.16" cy="117.59" r="1.30" fill="#aeb4bb"/>
> <circle cx="234.50" cy="149.19" r="1.30" fill="#aeb4bb"/>
> <circle cx="233.75" cy="157.41" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.00" cy="169.11" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.75" cy="181.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="237.66" cy="197.23" r="1.30" fill="#aeb4bb"/>
> <circle cx="236.91" cy="215.68" r="1.30" fill="#aeb4bb"/>
> <circle cx="234.59" cy="251.05" r="1.30" fill="#aeb4bb"/>
> <circle cx="235.34" cy="262.25" r="1.30" fill="#aeb4bb"/>
> <circle cx="230.07" cy="274.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="238.57" cy="308.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.18" cy="321.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.93" cy="330.69" r="1.30" fill="#aeb4bb"/>
> <circle cx="232.48" cy="341.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="241.02" cy="96.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="240.48" cy="115.70" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.93" cy="128.36" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.18" cy="140.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="243.43" cy="155.15" r="1.30" fill="#aeb4bb"/>
> <circle cx="247.32" cy="174.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="248.07" cy="187.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="246.59" cy="220.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="244.91" cy="248.05" r="1.30" fill="#aeb4bb"/>
> <circle cx="245.66" cy="266.96" r="1.30" fill="#aeb4bb"/>
> <circle cx="249.75" cy="279.49" r="1.30" fill="#aeb4bb"/>
> <circle cx="249.00" cy="287.51" r="1.30" fill="#aeb4bb"/>
> <circle cx="248.25" cy="299.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="243.25" cy="326.96" r="1.30" fill="#aeb4bb"/>
> <circle cx="251.45" cy="78.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="259.30" cy="92.92" r="1.30" fill="#aeb4bb"/>
> <circle cx="250.05" cy="117.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="254.61" cy="140.40" r="1.30" fill="#aeb4bb"/>
> <circle cx="253.86" cy="157.74" r="1.30" fill="#aeb4bb"/>
> <circle cx="257.64" cy="187.87" r="1.30" fill="#aeb4bb"/>
> <circle cx="258.39" cy="198.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="257.02" cy="214.92" r="1.30" fill="#aeb4bb"/>
> <circle cx="256.27" cy="235.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="254.48" cy="239.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="255.23" cy="255.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="259.43" cy="293.92" r="1.30" fill="#aeb4bb"/>
> <circle cx="257.93" cy="316.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="253.57" cy="351.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="269.62" cy="105.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="261.12" cy="137.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="264.29" cy="151.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="263.54" cy="160.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="267.21" cy="176.53" r="1.30" fill="#aeb4bb"/>
> <circle cx="267.96" cy="197.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="268.71" cy="199.80" r="1.30" fill="#aeb4bb"/>
> <circle cx="266.70" cy="215.75" r="1.30" fill="#aeb4bb"/>
> <circle cx="265.95" cy="229.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="265.55" cy="254.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="266.30" cy="275.49" r="1.30" fill="#aeb4bb"/>
> <circle cx="269.11" cy="292.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="268.36" cy="308.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="262.39" cy="321.47" r="1.30" fill="#aeb4bb"/>
> <circle cx="263.14" cy="331.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="263.89" cy="347.43" r="1.30" fill="#aeb4bb"/>
> <circle cx="279.94" cy="121.57" r="1.30" fill="#aeb4bb"/>
> <circle cx="270.69" cy="129.61" r="1.30" fill="#aeb4bb"/>
> <circle cx="274.72" cy="141.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="273.97" cy="153.93" r="1.30" fill="#aeb4bb"/>
> <circle cx="277.53" cy="187.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="278.28" cy="200.30" r="1.30" fill="#aeb4bb"/>
> <circle cx="276.38" cy="234.08" r="1.30" fill="#aeb4bb"/>
> <circle cx="275.63" cy="246.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="275.12" cy="262.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="275.87" cy="280.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="276.62" cy="293.03" r="1.30" fill="#aeb4bb"/>
> <circle cx="278.79" cy="302.08" r="1.30" fill="#aeb4bb"/>
> <circle cx="272.71" cy="339.39" r="1.30" fill="#aeb4bb"/>
> <circle cx="280.49" cy="112.05" r="1.30" fill="#aeb4bb"/>
> <circle cx="280.26" cy="124.70" r="1.30" fill="#aeb4bb"/>
> <circle cx="284.40" cy="158.63" r="1.30" fill="#aeb4bb"/>
> <circle cx="282.90" cy="192.81" r="1.30" fill="#aeb4bb"/>
> <circle cx="287.85" cy="205.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="288.60" cy="217.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="286.81" cy="232.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="286.06" cy="251.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="285.31" cy="264.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="285.44" cy="272.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="286.19" cy="284.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="288.47" cy="312.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="287.72" cy="330.95" r="1.30" fill="#aeb4bb"/>
> <circle cx="283.03" cy="343.61" r="1.30" fill="#aeb4bb"/>
> <circle cx="290.58" cy="146.53" r="1.30" fill="#aeb4bb"/>
> <circle cx="294.08" cy="176.28" r="1.30" fill="#aeb4bb"/>
> <circle cx="293.33" cy="186.52" r="1.30" fill="#aeb4bb"/>
> <circle cx="292.58" cy="202.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="298.17" cy="223.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="296.49" cy="242.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="295.74" cy="255.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="295.76" cy="282.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="296.51" cy="302.51" r="1.30" fill="#aeb4bb"/>
> <circle cx="298.90" cy="315.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="298.15" cy="336.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="300.15" cy="147.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="300.90" cy="156.45" r="1.30" fill="#aeb4bb"/>
> <circle cx="301.65" cy="172.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="303.76" cy="194.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="307.74" cy="212.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="308.49" cy="227.01" r="1.30" fill="#aeb4bb"/>
> <circle cx="309.24" cy="250.71" r="1.30" fill="#aeb4bb"/>
> <circle cx="306.17" cy="252.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="305.42" cy="273.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="305.33" cy="290.84" r="1.30" fill="#aeb4bb"/>
> <circle cx="306.08" cy="307.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="306.83" cy="320.77" r="1.30" fill="#aeb4bb"/>
> <circle cx="308.58" cy="330.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="307.83" cy="347.23" r="1.30" fill="#aeb4bb"/>
> <circle cx="311.22" cy="166.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="313.44" cy="198.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="312.69" cy="221.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="318.06" cy="237.32" r="1.30" fill="#aeb4bb"/>
> <circle cx="318.81" cy="245.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="319.56" cy="261.22" r="1.30" fill="#aeb4bb"/>
> <circle cx="315.85" cy="277.48" r="1.30" fill="#aeb4bb"/>
> <circle cx="315.65" cy="301.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="318.26" cy="341.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="320.79" cy="170.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="321.54" cy="192.31" r="1.30" fill="#aeb4bb"/>
> <circle cx="323.87" cy="204.86" r="1.30" fill="#aeb4bb"/>
> <circle cx="323.12" cy="217.55" r="1.30" fill="#aeb4bb"/>
> <circle cx="322.37" cy="232.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="328.38" cy="251.56" r="1.30" fill="#aeb4bb"/>
> <circle cx="326.28" cy="272.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="325.53" cy="284.26" r="1.30" fill="#aeb4bb"/>
> <circle cx="324.78" cy="296.95" r="1.30" fill="#aeb4bb"/>
> <circle cx="325.97" cy="312.16" r="1.30" fill="#aeb4bb"/>
> <circle cx="326.72" cy="330.95" r="1.30" fill="#aeb4bb"/>
> <circle cx="328.69" cy="343.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="331.86" cy="204.97" r="1.30" fill="#aeb4bb"/>
> <circle cx="332.80" cy="237.64" r="1.30" fill="#aeb4bb"/>
> <circle cx="338.70" cy="257.24" r="1.30" fill="#aeb4bb"/>
> <circle cx="339.45" cy="269.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="335.96" cy="283.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="335.21" cy="303.33" r="1.30" fill="#aeb4bb"/>
> <circle cx="334.46" cy="315.82" r="1.30" fill="#aeb4bb"/>
> <circle cx="336.29" cy="336.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="342.18" cy="220.60" r="1.30" fill="#aeb4bb"/>
> <circle cx="343.23" cy="235.68" r="1.30" fill="#aeb4bb"/>
> <circle cx="342.48" cy="260.05" r="1.30" fill="#aeb4bb"/>
> <circle cx="348.27" cy="263.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="349.02" cy="284.91" r="1.30" fill="#aeb4bb"/>
> <circle cx="349.77" cy="302.54" r="1.30" fill="#aeb4bb"/>
> <circle cx="345.64" cy="319.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="344.14" cy="345.03" r="1.30" fill="#aeb4bb"/>
> <circle cx="351.75" cy="231.42" r="1.30" fill="#aeb4bb"/>
> <circle cx="352.50" cy="248.17" r="1.30" fill="#aeb4bb"/>
> <circle cx="352.16" cy="273.03" r="1.30" fill="#aeb4bb"/>
> <circle cx="359.34" cy="305.15" r="1.30" fill="#aeb4bb"/>
> <circle cx="355.32" cy="331.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="354.57" cy="339.50" r="1.30" fill="#aeb4bb"/>
> <circle cx="362.82" cy="263.51" r="1.30" fill="#aeb4bb"/>
> <circle cx="361.84" cy="284.83" r="1.30" fill="#aeb4bb"/>
> <circle cx="368.91" cy="297.38" r="1.30" fill="#aeb4bb"/>
> <circle cx="369.66" cy="310.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="360.41" cy="325.96" r="1.30" fill="#aeb4bb"/>
> <circle cx="372.39" cy="253.21" r="1.30" fill="#aeb4bb"/>
> <circle cx="373.02" cy="267.07" r="1.30" fill="#aeb4bb"/>
> <circle cx="372.27" cy="280.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="371.52" cy="294.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="379.98" cy="328.89" r="1.30" fill="#aeb4bb"/>
> <circle cx="375.43" cy="350.76" r="1.30" fill="#aeb4bb"/>
> <circle cx="382.71" cy="259.65" r="1.30" fill="#aeb4bb"/>
> <circle cx="382.70" cy="282.20" r="1.30" fill="#aeb4bb"/>
> <circle cx="381.20" cy="317.98" r="1.30" fill="#aeb4bb"/>
> <circle cx="389.55" cy="333.28" r="1.30" fill="#aeb4bb"/>
> <circle cx="380.30" cy="347.44" r="1.30" fill="#aeb4bb"/>
> <circle cx="393.03" cy="276.40" r="1.30" fill="#aeb4bb"/>
> <circle cx="392.38" cy="291.58" r="1.30" fill="#aeb4bb"/>
> <circle cx="391.63" cy="300.66" r="1.30" fill="#aeb4bb"/>
> <circle cx="399.12" cy="317.10" r="1.30" fill="#aeb4bb"/>
> <circle cx="399.87" cy="325.02" r="1.30" fill="#aeb4bb"/>
> <circle cx="390.62" cy="341.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="402.06" cy="290.09" r="1.30" fill="#aeb4bb"/>
> <circle cx="401.31" cy="307.07" r="1.30" fill="#aeb4bb"/>
> <circle cx="409.44" cy="325.25" r="1.30" fill="#aeb4bb"/>
> <circle cx="411.74" cy="298.19" r="1.30" fill="#aeb4bb"/>
> <circle cx="410.99" cy="320.14" r="1.30" fill="#aeb4bb"/>
> <circle cx="419.76" cy="333.79" r="1.30" fill="#aeb4bb"/>
> <circle cx="410.51" cy="347.28" r="1.30" fill="#aeb4bb"/>
> <circle cx="422.17" cy="296.37" r="1.30" fill="#aeb4bb"/>
> <circle cx="421.42" cy="312.00" r="1.30" fill="#aeb4bb"/>
> <circle cx="420.08" cy="351.88" r="1.30" fill="#aeb4bb"/>
> <circle cx="431.10" cy="309.82" r="1.30" fill="#aeb4bb"/>
> <circle cx="430.40" cy="342.72" r="1.30" fill="#aeb4bb"/>
> <circle cx="441.53" cy="305.73" r="1.30" fill="#aeb4bb"/>
> <circle cx="440.78" cy="331.54" r="1.30" fill="#aeb4bb"/>
> <circle cx="449.97" cy="351.41" r="1.30" fill="#aeb4bb"/>
> <circle cx="451.21" cy="316.06" r="1.30" fill="#aeb4bb"/>
> <circle cx="450.46" cy="331.46" r="1.30" fill="#aeb4bb"/>
> <circle cx="460.89" cy="329.35" r="1.30" fill="#aeb4bb"/>
> <circle cx="460.14" cy="341.04" r="1.30" fill="#aeb4bb"/>
> <circle cx="479.82" cy="352.82" r="1.30" fill="#aeb4bb"/>
> <circle cx="499.93" cy="340.97" r="1.30" fill="#aeb4bb"/>
> <polyline points="70,343.40 73.33,341.36 76.67,339.07 80.00,336.54 83.33,333.75 86.67,330.71 90.00,327.42 93.33,323.88 96.67,320.07 100.00,316.00 103.33,311.64 106.67,306.99 110.00,302.01 113.33,296.69 116.67,291.00 120.00,284.90 123.33,278.39 126.67,271.44 130.00,264.04 133.33,256.19 136.67,247.91 140.00,239.23 143.33,230.17 146.67,220.80 150.00,211.17 153.33,201.35 156.67,191.43 160.00,181.49 163.33,171.61 166.67,161.90 170.00,152.43 173.33,143.28 176.67,134.53 180.00,126.23 183.33,118.44 186.67,111.20 190.00,104.53 193.33,98.44 196.67,92.94 200.00,88.02 203.33,83.65 206.67,79.83 210.00,76.52 213.33,73.70 216.67,71.34 220.00,69.43 223.33,67.95 226.67,66.88 230.00,66.23 233.33,66.00 236.67,66.18 240.00,66.79 243.33,67.82 246.67,69.27 250.00,71.14 253.33,73.41 256.67,76.07 260.00,79.08 263.33,82.42 266.67,86.04 270.00,89.91 273.33,93.97 276.67,98.18 280.00,102.51 283.33,106.93 286.67,111.40 290.00,115.90 293.33,120.43 296.67,124.98 300.00,129.55 303.33,134.16 306.67,138.82 310.00,143.55 313.33,148.37 316.67,153.30 320.00,158.38 323.33,163.60 326.67,168.98 330.00,174.53 333.33,180.24 336.67,186.09 340.00,192.06 343.33,198.09 346.67,204.16 350.00,210.19 353.33,216.14 356.67,221.93 360.00,227.50 363.33,232.80 366.67,237.79 370.00,242.42 373.33,246.67 376.67,250.53 380.00,254.02 383.33,257.15 386.67,259.95 390.00,262.48 393.33,264.78 396.67,266.91 400.00,268.93 403.33,270.88 406.67,272.81 410.00,274.77 413.33,276.78 416.67,278.87 420.00,281.05 423.33,283.32 426.67,285.68 430.00,288.11 433.33,290.61 436.67,293.17 440.00,295.75 443.33,298.34 446.67,300.94 450.00,303.52 453.33,306.06 456.67,308.57 460.00,311.04 463.33,313.45 466.67,315.81 470.00,318.11 473.33,320.34 476.67,322.52 480.00,324.62 483.33,326.66 486.67,328.62 490.00,330.51 493.33,332.32 496.67,334.05 500.00,335.69 503.33,337.25 506.67,338.73 510.00,340.11 513.33,341.41 516.67,342.62 520.00,343.75 523.33,344.78 526.67,345.74 530.00,346.61 533.33,347.39 536.67,348.10 540.00,348.72 543.33,349.26 546.67,349.73 550.00,350.12 553.33,350.44 556.67,350.69 560.00,350.88 563.33,351.01 566.67,351.09 570.00,351.12 573.33,351.12 576.67,351.09 580.00,351.03 583.33,350.94 586.67,350.85 590.00,350.74 593.33,350.63 596.67,350.52 600.00,350.41 603.33,350.30 606.67,350.21 610.00,350.13 613.33,350.07 616.67,350.02 620.00,350.00 623.33,350.01 626.67,350.04 630.00,350.10 633.33,350.18 636.67,350.29 640.00,350.41 643.33,350.55 646.67,350.70 650.00,350.85 653.33,351.00 656.67,351.13 660.00,351.26 663.33,351.36 666.67,351.44 670.00,351.49 673.33,351.52 676.67,351.53 680.00,351.51 683.33,351.48 686.67,351.43 690.00,351.37 693.33,351.31 696.67,351.24 700.00,351.18 703.33,351.13 706.67,351.09 710.00,351.06 713.33,351.05 716.67,351.07 720.00,351.10 723.33,351.16 726.67,351.24 730.00,351.33 733.33,351.45 736.67,351.58 740.00,351.73 743.33,351.89 746.67,352.06 750.00,352.24 753.33,352.41 756.67,352.58 760.00,352.75 763.33,352.91 766.67,353.07 770.00,353.21" fill="none" stroke="#5b6770" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
> <line x1="70" y1="354" x2="770" y2="354" stroke="#aaa" stroke-width="1"/>
> <line x1="83.69" y1="354" x2="83.69" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="83.69" y="372" text-anchor="middle" fill="#777" font-size="11">-2σ</text>
> <line x1="176.72" y1="354" x2="176.72" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="176.72" y="372" text-anchor="middle" fill="#777" font-size="11">-1σ</text>
> <line x1="362.76" y1="354" x2="362.76" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="362.76" y="372" text-anchor="middle" fill="#777" font-size="11">1σ</text>
> <line x1="455.78" y1="354" x2="455.78" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="455.78" y="372" text-anchor="middle" fill="#777" font-size="11">2σ</text>
> <line x1="548.81" y1="354" x2="548.81" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="548.81" y="372" text-anchor="middle" fill="#777" font-size="11">3σ</text>
> <line x1="641.83" y1="354" x2="641.83" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="641.83" y="372" text-anchor="middle" fill="#777" font-size="11">4σ</text>
> <line x1="734.85" y1="354" x2="734.85" y2="359" stroke="#aaa" stroke-width="1"/>
> <text x="734.85" y="372" text-anchor="middle" fill="#777" font-size="11">5σ</text>
> <text x="269.74" y="372" text-anchor="middle" fill="#999" font-size="11">average</text>
> <text x="269.74" y="386" text-anchor="middle" fill="#bbb" font-size="10">0.42</text>
> <line x1="425.75" y1="354" x2="425.75" y2="50" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="425.75" cy="354" r="3" fill="#3a86c8"/>
> <text x="425.75" y="46" text-anchor="middle" fill="#3a86c8" font-size="11">Mohamed Salah 1.7σ</text>
> <line x1="444.92" y1="354" x2="444.92" y2="65" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="444.92" cy="354" r="3" fill="#3a86c8"/>
> <text x="444.92" y="61" text-anchor="middle" fill="#3a86c8" font-size="11">Robert Lewandowski 1.9σ</text>
> <line x1="479.43" y1="354" x2="479.43" y2="80" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="479.43" cy="354" r="3" fill="#3a86c8"/>
> <text x="479.43" y="76" text-anchor="middle" fill="#3a86c8" font-size="11">Cristiano Ronaldo 2.3σ</text>
> <line x1="636.64" y1="354" x2="636.64" y2="50" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="636.64" cy="354" r="3" fill="#3a86c8"/>
> <text x="636.64" y="46" text-anchor="middle" fill="#3a86c8" font-size="11">Neymar 3.9σ</text>
> <line x1="697.99" y1="354" x2="697.99" y2="65" stroke="#3a86c8" stroke-width="1"/>
> <circle cx="697.99" cy="354" r="3" fill="#3a86c8"/>
> <text x="697.99" y="61" text-anchor="end" fill="#3a86c8" font-size="11">Kylian Mbappé 4.6σ</text>
> <line x1="732.50" y1="354" x2="732.50" y2="50" stroke="#ff2e88" stroke-width="1.80"/>
> <circle cx="732.50" cy="354" r="6" fill="#ff2e88"/>
> <text x="732.50" y="46" text-anchor="end" fill="#ff2e88" font-size="13">Lionel Messi 5.0σ</text>
> </svg>
