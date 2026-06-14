# California Housing

This tutorial is taken from the book...
```haskell
-- cabal: build-depends: filepath
import System.FilePath

downloadRoot = "https://raw.githubusercontent.com/ageron/handson-ml2/master"
housingPath = "datasets" </> "housing"
housingUrl = downloadRoot </> housingPath </> "housing.tgz"
```

```haskell
-- cabal: build-depends: http-conduit, zlib
import Codec.Compression.GZip
import Network.HTTP.Simple


req <- parseRequest housingUrl
response <- httpLBS req

body = decompress (getResponseBody response)
```

```haskell
-- cabal: build-depends: bytestring
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8


startOfCsv = C8.unpack (C8.dropWhile (/= 'l') (C8.toStrict body))
```

```haskell
-- cabal: build-depends: dataframe == 2.1.0.3
import qualified DataFrame as D
import DataFrame.Operators

df <- fmap (either error id) (D.fromCsv startOfCsv)

df |> D.take 10
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | longitude<br>Text | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Maybe Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double | ocean_proximity<br>Text |
> | ------------------|--------------------|------------------------------|-----------------------|--------------------------------|----------------------|----------------------|-------------------------|------------------------------|------------------------ |
> | -122.23           | 37.88              | 41.0                         | 880.0                 | Just 129.0                     | 322.0                | 126.0                | 8.3252                  | 452600.0                     | NEAR BAY                |
> | -122.22           | 37.86              | 21.0                         | 7099.0                | Just 1106.0                    | 2401.0               | 1138.0               | 8.3014                  | 358500.0                     | NEAR BAY                |
> | -122.24           | 37.85              | 52.0                         | 1467.0                | Just 190.0                     | 496.0                | 177.0                | 7.2574                  | 352100.0                     | NEAR BAY                |
> | -122.25           | 37.85              | 52.0                         | 1274.0                | Just 235.0                     | 558.0                | 219.0                | 5.6431000000000004      | 341300.0                     | NEAR BAY                |
> | -122.25           | 37.85              | 52.0                         | 1627.0                | Just 280.0                     | 565.0                | 259.0                | 3.8462                  | 342200.0                     | NEAR BAY                |
> | -122.25           | 37.85              | 52.0                         | 919.0                 | Just 213.0                     | 413.0                | 193.0                | 4.0368                  | 269700.0                     | NEAR BAY                |
> | -122.25           | 37.84              | 52.0                         | 2535.0                | Just 489.0                     | 1094.0               | 514.0                | 3.6591                  | 299200.0                     | NEAR BAY                |
> | -122.25           | 37.84              | 52.0                         | 3104.0                | Just 687.0                     | 1157.0               | 647.0                | 3.12                    | 241400.0                     | NEAR BAY                |
> | -122.26           | 37.84              | 42.0                         | 2555.0                | Just 665.0                     | 1206.0               | 595.0                | 2.0804                  | 226700.0                     | NEAR BAY                |
> | -122.25           | 37.84              | 52.0                         | 3549.0                | Just 707.0                     | 1551.0               | 714.0                | 3.6912000000000003      | 261100.0                     | NEAR BAY                |
