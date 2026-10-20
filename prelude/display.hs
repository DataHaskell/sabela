{- | Type-directed display: what GHCi's @-interactive-print@ calls for a cell's
trailing expression, so a value renders itself in the best form it has. Every
name resolves through a qualified alias, and the names it binds are kept few:
a prompt binding shadows a user's import of the same name, unhideably.
-}
class Display a where
    display :: a -> SabelaBase.IO ()

{- | The fallback that keeps every notebook that already works working: a
value with no richer instance prints exactly as GHCi printed it before.
-}
instance {-# OVERLAPPABLE #-} (SabelaBase.Show a) => Display a where
    display = SabelaBase.print

instance {-# OVERLAPPING #-} Display SabelaBase.String where
    display = SabelaBase.print

{- | A widget shows itself: a cell whose last expression is an 'Input' renders
the control, as 'showInput' does, instead of failing for want of a 'Show'.
-}
instance Display (Input a) where
    display = showInput

{- | A value that carries the MIME type it wants to be shown as. Built by
'md', 'html', 'svg', 'latex', 'json', 'png', 'jpeg' and 'table'.
-}
data Rich = Rich SabelaBase.String SabelaBase.String

instance Display Rich where
    display (Rich mime content) = displayMime_ mime content

md, html, svg, latex, json :: SabelaBase.String -> Rich
md = Rich "text/markdown"
html = Rich "text/html"
svg = Rich "image/svg+xml"
latex = Rich "text/latex"
json = Rich "application/json"

-- | Base64 image data, shown inline.
png, jpeg :: SabelaBase.String -> Rich
png = Rich "image/png;base64"
jpeg = Rich "image/jpeg;base64"

-- | Headers and already-rendered rows, for tabular data with no record behind it.
table :: [SabelaBase.String] -> [[SabelaBase.String]] -> Rich
table hs rs = tableOf hs (SabelaBase.take _sabelaRowCap rs) (SabelaBase.length rs)

{- | A table whose rows have already been capped, told how many rows the data
it came from actually has — for a source that can count without being read.
-}
tableOf ::
    [SabelaBase.String] -> [[SabelaBase.String]] -> SabelaBase.Int -> Rich
tableOf hs shown total = tableOfTyped hs [] shown total

{- | The capped table with its columns' type names, which the grid shows under
each column name; an empty list leaves the grid to infer them.
-}
tableOfTyped ::
    [SabelaBase.String] ->
    [SabelaBase.String] ->
    [[SabelaBase.String]] ->
    SabelaBase.Int ->
    Rich
tableOfTyped hs tys shown total = html (_sabelaTableHtml hs tys shown total)

{- | One row of a table. The generic default reads a single-constructor
record's field names and values, so @deriving (Generic, Tabular)@ is the whole
opt-in; the catch-all instance reports no columns, which is what keeps every
other list printing as it always did.
-}
class Tabular r where
    tabHeaders :: r -> [SabelaBase.String]
    default tabHeaders ::
        (SabelaGRow (SabelaGen.Rep r), SabelaGen.Generic r) => r -> [SabelaBase.String]
    tabHeaders x = _sabelaGHeaders (SabelaGen.from x)
    tabCells :: r -> [SabelaBase.String]
    default tabCells ::
        (SabelaGRow (SabelaGen.Rep r), SabelaGen.Generic r) => r -> [SabelaBase.String]
    tabCells x = _sabelaGCells (SabelaGen.from x)
    tabTypes :: r -> [SabelaBase.String]
    default tabTypes ::
        (SabelaGRow (SabelaGen.Rep r), SabelaGen.Generic r) => r -> [SabelaBase.String]
    tabTypes x = _sabelaGTypes (SabelaGen.from x)

instance {-# OVERLAPPABLE #-} (SabelaBase.Show r) => Tabular r where
    tabHeaders _ = []
    tabCells x = [SabelaBase.show x]
    tabTypes _ = []

{- | A list is a table when its element type opted in, and is printed exactly
as before when it did not.
-}
instance {-# OVERLAPPING #-} (SabelaBase.Show r, Tabular r) => Display [r] where
    display rs = case rs of
        [] -> SabelaBase.print rs
        (r0 : _)
            | SabelaBase.null (tabHeaders r0) -> SabelaBase.print rs
            | SabelaBase.otherwise ->
                display
                    ( tableOfTyped
                        (_sabelaNumberBlanks (tabHeaders r0))
                        (tabTypes r0)
                        (SabelaBase.map tabCells (SabelaBase.take _sabelaRowCap rs))
                        (SabelaBase.length rs)
                    )

{- | How many rows a table carries into the page. The grid pages through
them, so this bounds the payload rather than the render.
-}
_sabelaRowCap :: SabelaBase.Int
_sabelaRowCap = 1000

{- | Positional headers for a single-constructor type whose fields are
unnamed, so @data P = P Int Int@ still tabulates.
-}
_sabelaNumberBlanks :: [SabelaBase.String] -> [SabelaBase.String]
_sabelaNumberBlanks =
    SabelaBase.zipWith
        (\i h -> if SabelaBase.null h then SabelaBase.show (i :: SabelaBase.Int) else h)
        [1 ..]

{- | The markup a table prints: the rows as data, and the runtime that turns
them into the grid. The data goes in a JSON script tag rather than the markup
so a value containing angle brackets stays a value.
-}
_sabelaTableHtml ::
    [SabelaBase.String] ->
    [SabelaBase.String] ->
    [[SabelaBase.String]] ->
    SabelaBase.Int ->
    SabelaBase.String
_sabelaTableHtml = _sabelaTableHtmlWith "" ""

{- | The same markup for a table printed from a frame the kernel keeps: the
slot key goes on the grid so its expression box can name the frame.
-}
_sabelaTableHtmlWith ::
    SabelaBase.String ->
    SabelaBase.String ->
    [SabelaBase.String] ->
    [SabelaBase.String] ->
    [[SabelaBase.String]] ->
    SabelaBase.Int ->
    SabelaBase.String
_sabelaTableHtmlWith query slot hs tys rs total =
    SabelaBase.concat
        [ "<div class=\"sabela-grid\""
        , if SabelaBase.null slot
            then ""
            else
                " data-frame-slot=\""
                    SabelaBase.++ slot
                    SabelaBase.++ "\" data-frame-query=\""
                    SabelaBase.++ query
                    SabelaBase.++ "\""
        , ">"
        , "<script type=\"application/json\" class=\"sabela-grid-data\">"
        , _sabelaGridJson hs tys rs total
        , "</script></div><script>"
        , _sabelaGridJs
        , "\nsabelaGridMountAll();</script>"
        ]

{- | The payload the grid reads: the columns, the rows carried into the page,
and how many rows the data has in total — which is not the same number when a
table was capped.
-}
_sabelaGridJson ::
    [SabelaBase.String] ->
    [SabelaBase.String] ->
    [[SabelaBase.String]] ->
    SabelaBase.Int ->
    SabelaBase.String
_sabelaGridJson hs tys rs total =
    SabelaBase.concat
        [ "{\"cols\":"
        , _sabelaJsonList (SabelaBase.map _sabelaJsonString hs)
        , if SabelaBase.null tys
            then ""
            else
                ",\"types\":"
                    SabelaBase.++ _sabelaJsonList (SabelaBase.map _sabelaJsonString tys)
        , ",\"rows\":"
        , _sabelaJsonList
            (SabelaBase.map (\r -> _sabelaJsonList (SabelaBase.map _sabelaJsonString r)) rs)
        , ",\"total\":"
        , SabelaBase.show total
        , "}"
        ]

_sabelaJsonList :: [SabelaBase.String] -> SabelaBase.String
_sabelaJsonList xs =
    SabelaBase.concat ["[", SabelaBase.intercalate "," xs, "]"]

{- | A JSON string. @<@ leaves as an escape so no value can close the script
tag it is written into, and a control character leaves as one so the payload
stays parseable.
-}
_sabelaJsonString :: SabelaBase.String -> SabelaBase.String
_sabelaJsonString s =
    SabelaBase.concat ["\"", SabelaBase.concatMap esc s, "\""]
  where
    esc c = case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '<' -> "\\u003c"
        _
            | SabelaBase.isControl c -> _sabelaUnicodeEscape c
            | SabelaBase.otherwise -> [c]

_sabelaUnicodeEscape :: SabelaBase.Char -> SabelaBase.String
_sabelaUnicodeEscape c =
    "\\u" SabelaBase.++ SabelaBase.map digit ([12, 8, 4, 0] :: [SabelaBase.Int])
  where
    n = SabelaBase.ord c
    digit shift =
        "0123456789abcdef"
            SabelaBase.!! SabelaBase.mod (SabelaBase.div n (2 SabelaBase.^ shift)) 16

{- | One table cell's text. A string field lands in its column unquoted;
everything else falls back to its 'Show'.
-}
class SabelaTableCell a where
    _sabelaCellText :: a -> SabelaBase.String
    _sabelaCellType :: a -> SabelaBase.String

instance
    {-# OVERLAPPABLE #-}
    (Data.Typeable.Typeable a, SabelaBase.Show a) =>
    SabelaTableCell a
    where
    _sabelaCellText = SabelaBase.show
    _sabelaCellType x = SabelaBase.show (Data.Typeable.typeOf x)

instance {-# OVERLAPPING #-} SabelaTableCell SabelaBase.String where
    _sabelaCellText s = s
    _sabelaCellType _ = "String"

{- | An absent value is a blank cell; a present one is its own cell text. The
type name never forces the value, so it is read off any row, blank or not.
-}
instance
    {-# OVERLAPPING #-}
    (SabelaTableCell a) =>
    SabelaTableCell (SabelaBase.Maybe a)
    where
    _sabelaCellText = SabelaBase.maybe "" _sabelaCellText
    _sabelaCellType m =
        "Maybe "
            SabelaBase.++ _sabelaParenType (_sabelaCellType (SabelaBase.fromJust m))

-- | A type name as an argument: parenthesised when it has arguments itself.
_sabelaParenType :: SabelaBase.String -> SabelaBase.String
_sabelaParenType t
    | ' ' `SabelaBase.elem` t = "(" SabelaBase.++ t SabelaBase.++ ")"
    | SabelaBase.otherwise = t

class SabelaGRow f where
    _sabelaGHeaders :: f p -> [SabelaBase.String]
    _sabelaGCells :: f p -> [SabelaBase.String]
    _sabelaGTypes :: f p -> [SabelaBase.String]

instance (SabelaGRow f) => SabelaGRow (SabelaGen.D1 c f) where
    _sabelaGHeaders (SabelaGen.M1 x) = _sabelaGHeaders x
    _sabelaGCells (SabelaGen.M1 x) = _sabelaGCells x
    _sabelaGTypes (SabelaGen.M1 x) = _sabelaGTypes x

instance (SabelaGRow f) => SabelaGRow (SabelaGen.C1 c f) where
    _sabelaGHeaders (SabelaGen.M1 x) = _sabelaGHeaders x
    _sabelaGCells (SabelaGen.M1 x) = _sabelaGCells x
    _sabelaGTypes (SabelaGen.M1 x) = _sabelaGTypes x

instance (SabelaGRow f, SabelaGRow g) => SabelaGRow (f SabelaGen.:*: g) where
    _sabelaGHeaders (a SabelaGen.:*: b) =
        _sabelaGHeaders a SabelaBase.++ _sabelaGHeaders b
    _sabelaGCells (a SabelaGen.:*: b) = _sabelaGCells a SabelaBase.++ _sabelaGCells b
    _sabelaGTypes (a SabelaGen.:*: b) = _sabelaGTypes a SabelaBase.++ _sabelaGTypes b

instance
    (SabelaGen.Selector c, SabelaTableCell a) =>
    SabelaGRow (SabelaGen.S1 c (SabelaGen.K1 i a))
    where
    _sabelaGHeaders s = [SabelaGen.selName s]
    _sabelaGCells (SabelaGen.M1 (SabelaGen.K1 a)) = [_sabelaCellText a]
    _sabelaGTypes (SabelaGen.M1 (SabelaGen.K1 a)) = [_sabelaCellType a]

instance SabelaGRow SabelaGen.U1 where
    _sabelaGHeaders _ = []
    _sabelaGCells _ = []
    _sabelaGTypes _ = []

{- | What GHCi calls for a cell's trailing expression. Private, so a user cell
that binds a @display@ of its own cannot unseat the printer.
-}
_sabelaPrint :: (Display a) => a -> SabelaBase.IO ()
_sabelaPrint = display
