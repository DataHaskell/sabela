# Sabela Example

This is an example of Sabela, a reactive notebook system for Haskell.


Let's start with something boring - regular evaluation:



```haskell
print (5 + 5)
```

The notebook is reactive - meaning cells are automatically rerun when their
dependencies change.


Run the following three cells then come back, change the value of `x` from 10
to 15. The value of `x + y` should automatically update.


```haskell
x = 10
```

```haskell
y = 10
```


```haskell
x + y
```

We can even install dependencies on the fly similar to how cabal scripts
manage/install dependencies.


```haskell
-- Main.ghci
-- cabal: build-depends: base, dataframe, text, time
-- cabal: default-extensions: TemplateHaskell, TypeApplications, OverloadedStrings, DataKinds

import qualified DataFrame as D
import Data.Text (Text)

df = D.fromNamedColumns [("key", D.fromList ["K0" :: Text, "K1", "K2", "K3"]), ("A", D.fromList ["A0", "A1", "A2", "A3"])]
```

We can also run remote scripts (use with caution). This remote script, for example, sets up all the boilerplate
you need to run dataframes (imports, defaults, language extensions etc).

We can also run import code inline as we do with GHCi.



```haskell
:! curl -s --output dataframe.script https://raw.githubusercontent.com/mchav/ihaskell-dataframe/refs/heads/main/rc.hs
:script dataframe.script

:set prompt ""

$(F.declareColumns df)

other = D.fromNamedColumns [("key", D.fromList ["K0", "K1", "K2"]), ("B", D.fromList ["B0", "B1", "B2"])]
```

```haskell
import qualified Data.Text.IO as TIO

displayMarkdown $ T.unpack $ D.toMarkdownTable $ D.innerJoin [F.name key] df other
```

That's it!
