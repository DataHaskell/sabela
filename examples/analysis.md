# DataFrame Example


We're going to create two dataframes and join them. First we have to install
the dataframe library.


```haskell
-- cabal: build-depends: base, dataframe, text
-- cabal: default-extensions: TemplateHaskell, TypeApplications, OverloadedStrings, DataKinds

import qualified DataFrame as D
import Data.Text (Text)

```




```haskell
import qualified DataFrame.Functions as F

df = D.fromNamedColumns [("key", D.fromList ["K0" :: Text, "K1", "K2", "K3"]), ("A", D.fromList ["A0", "A1", "A2", "A3"])]
other = D.fromNamedColumns [("key", D.fromList ["K0" :: Text, "K1", "K2"]), ("B", D.fromList ["B0", "B1", "B2"]), ("C", D.fromList [1:: Int,2,3])]

$(F.declareColumns df)
$(F.declareColumns other)

```



```haskell
import DataFrame ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

df |> D.innerJoin [F.name key] other
   |> D.derive "joint" (F.lift2 (<>) a b)
   |> D.toMarkdownTable
   |> T.unpack
   |> displayMarkdown
```

```haskell
import qualified DataFrame.Display.Web.Plot as P

P.HtmlPlot p <- P.plotHistogram (F.name c) other

displayHtml (T.unpack p)
```

That's it!
