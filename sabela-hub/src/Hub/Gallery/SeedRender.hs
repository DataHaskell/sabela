{-# LANGUAGE OverloadedStrings #-}

{- | Render a notebook's Markdown to the gallery's self-contained "Warm Paper"
notebook page, and brand a pre-built dashboard export. Port of the rendering
half of the former @sabela-hub\/scripts\/seed-gallery.py@.

The Markdown subset matches what the seeder needs: fenced code blocks, quoted
output blocks (@\> \<!-- scripths:mime … --\>@), headings, and paragraphs with
inline bold\/code\/link\/image.
-}
module Hub.Gallery.SeedRender (
    inline,
    renderBody,
    page,
    brandDashboard,
) where

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Inline markdown
-- ---------------------------------------------------------------------------

-- | Escape, then images, links, bold, code (in that order, as the Python did).
inline :: Text -> Text
inline =
    T.pack
        . codeSpans
        . boldSpans
        . links
        . images
        . T.unpack
        . escapeHtml

-- | @html.escape(quote=False)@: ampersands and angle brackets only.
escapeHtml :: Text -> Text
escapeHtml =
    T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

{- | Match a @(open … close)@ span. @nonEmpty@ requires the captured body to be
non-empty (the regex @+@); when 'False' an empty body is allowed (@*@).
-}
spanBetween :: Char -> Char -> Bool -> String -> Maybe (String, String)
spanBetween _ close nonEmpty s = case break (== close) s of
    (body, _ : rest)
        | nonEmpty && null body -> Nothing
        | otherwise -> Just (body, rest)
    (_, []) -> Nothing

images :: String -> String
images [] = []
images ('!' : '[' : rest)
    | Just (alt, r1) <- spanBetween '[' ']' False rest
    , '(' : r2 <- r1
    , Just (src, r3) <- spanBetween '(' ')' True r2 =
        "<img alt=\"" ++ alt ++ "\" src=\"" ++ src ++ "\">" ++ images r3
images (c : cs) = c : images cs

links :: String -> String
links [] = []
links ('[' : rest)
    | Just (txt, r1) <- spanBetween '[' ']' True rest
    , '(' : r2 <- r1
    , Just (href, r3) <- spanBetween '(' ')' True r2 =
        "<a href=\"" ++ href ++ "\">" ++ txt ++ "</a>" ++ links r3
links (c : cs) = c : links cs

boldSpans :: String -> String
boldSpans = delimited "**" "<strong>" "</strong>"

codeSpans :: String -> String
codeSpans = delimited "`" "<code>" "</code>"

-- | Wrap each @open … open@ (same delimiter, non-empty body) pair in tags.
delimited :: String -> String -> String -> String -> String
delimited delim openTag closeTag = go
  where
    go s = case stripPrefix' delim s of
        Just afterOpen -> case breakOn' delim afterOpen of
            Just (body, rest)
                | not (null body) -> openTag ++ body ++ closeTag ++ go rest
            _ -> head' s : go (tail' s)
        Nothing -> case s of
            [] -> []
            (c : cs) -> c : go cs
    head' (c : _) = c
    head' [] = ' '
    tail' (_ : cs) = cs
    tail' [] = []

stripPrefix' :: String -> String -> Maybe String
stripPrefix' [] s = Just s
stripPrefix' (p : ps) (c : cs) | p == c = stripPrefix' ps cs
stripPrefix' _ _ = Nothing

-- | Split at the first occurrence of @delim@; 'Nothing' if absent.
breakOn' :: String -> String -> Maybe (String, String)
breakOn' delim = go []
  where
    go _ [] = Nothing
    go acc s@(c : cs) = case stripPrefix' delim s of
        Just rest -> Just (reverse acc, rest)
        Nothing -> go (c : acc) cs

-- ---------------------------------------------------------------------------
-- Body
-- ---------------------------------------------------------------------------

{- | Render a notebook's Markdown to the gallery's notebook-cell HTML (the
inner @\<main\>@ body). Line-oriented, matching the Python @render_body@.
-}
renderBody :: Text -> Text
renderBody md = T.intercalate "\n" (go (T.splitOn "\n" md))
  where
    go [] = []
    go (line : rest)
        | T.null (T.strip line) || isCellMarker line = go rest
        | "```" `T.isPrefixOf` line =
            codeBlock (T.toLower (T.strip (T.drop 3 line))) rest
        | ">" `T.isPrefixOf` line = quoteBlock (line : rest)
        | Just (lvl, txt) <- heading line =
            [headingHtml lvl txt | lvl > 1] ++ go rest
        | otherwise = paragraph (line : rest)

    codeBlock lang ls =
        let (buf, after) = break ("```" `T.isPrefixOf`) ls
            code = escapeHtml (T.intercalate "\n" buf)
            cls = if lang == "haskell" then "language-haskell" else "nohighlight"
         in ("<pre class=\"code\"><code class=\"" <> cls <> "\">" <> code <> "</code></pre>")
                : go (drop 1 after)

    quoteBlock ls =
        let (qs, after) = span (">" `T.isPrefixOf`) ls
            buf = map dequote qs
            firstNonBlank = headOr "" (filter (not . T.null . T.strip) buf)
            content = T.intercalate "\n" (filter (not . isComment) buf)
            block =
                if "scripths:mime text/html" `T.isInfixOf` firstNonBlank
                    then "<div class=\"out\">" <> content <> "</div>"
                    else "<pre class=\"out\">" <> escapeHtml content <> "</pre>"
         in [block | not (T.null (T.strip content))] ++ go after

    paragraph ls =
        let (para, after) = span isProse ls
         in ("<p>" <> inline (T.unwords para) <> "</p>") : go after

    isProse l =
        not (T.null (T.strip l))
            && not (any (`T.isPrefixOf` l) ["```", ">", "#"])
            && not (isCellMarker l)

dequote :: Text -> Text
dequote l = case T.uncons (T.drop 1 l) of
    Just (c, cs) | c == ' ' || c == '\t' -> cs
    _ -> T.drop 1 l

isComment :: Text -> Bool
isComment = T.isPrefixOf "<!--" . T.strip

{- | A line that is wholly an HTML comment — the @sabela:cell@ separator, the
@scripths:@ version banner, or any other tooling marker. Skipped in prose flow.
-}
isCellMarker :: Text -> Bool
isCellMarker l = "<!--" `T.isPrefixOf` s && "-->" `T.isSuffixOf` s
  where
    s = T.strip l

-- | Parse an ATX heading: 1–6 @#@ then whitespace then the text.
heading :: Text -> Maybe (Int, Text)
heading line
    | n >= 1 && n <= 6 && hasSpace = Just (n, T.dropWhile isSp afterHashes)
    | otherwise = Nothing
  where
    n = T.length (T.takeWhile (== '#') line)
    afterHashes = T.drop n line
    hasSpace = maybe False (isSp . fst) (T.uncons afterHashes)
    isSp c = c == ' ' || c == '\t'

headingHtml :: Int -> Text -> Text
headingHtml lvl txt =
    "<h" <> tlvl <> ">" <> inline txt <> "</h" <> tlvl <> ">"
  where
    tlvl = T.pack (show lvl)

headOr :: a -> [a] -> a
headOr d [] = d
headOr _ (x : _) = x

-- ---------------------------------------------------------------------------
-- Page shell + dashboard branding
-- ---------------------------------------------------------------------------

-- | A self-contained notebook page in the gallery's Warm Paper palette.
page :: Text -> Text -> Text -> Text -> Text
page title author slug body =
    T.concat
        [ pageHead title
        , "<header class=\"top\">\n"
        , "<a class=\"brand\" href=\"/gallery\"><span class=\"lam\">&lambda;</span> Sabela</a>\n"
        , "<nav class=\"acts\">\n"
        , "<a class=\"gal\" href=\"/gallery\">Gallery</a>\n"
        , "<a class=\"dl\" href=\"/_hub/source/" <> slug <> "\">Download</a>\n"
        , "<form method=\"post\" action=\"/_hub/fork/" <> slug <> "\">"
        , "<button class=\"fork\" type=\"submit\">Fork &#9656;</button></form>\n"
        , "</nav>\n</header>\n<main>\n"
        , "<h1>" <> escapeHtml title <> "</h1>\n"
        , "<p class=\"byline\">by " <> escapeHtml author <> "</p>\n"
        , body
        , "\n</main>\n"
        , pageScripts
        ]

pageHead :: Text -> Text
pageHead title =
    T.concat
        [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">\n"
        , "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
        , "<title>" <> escapeHtml title <> "</title>\n"
        , "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n"
        , "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-light.min.css\">\n"
        , "<style>\n"
        , pageCss
        , "</style></head><body>\n"
        ]

pageScripts :: Text
pageScripts =
    T.concat
        [ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js\"></script>\n"
        , "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js\"></script>\n"
        , "<script>hljs.highlightAll();</script>\n"
        , "</body></html>"
        ]

pageCss :: Text
pageCss =
    T.unlines
        [ "@import url('https://fonts.googleapis.com/css2?family=Geist:wght@400;500;600&family=Fraunces:opsz,wght@9..144,500;9..144,600&family=JetBrains+Mono:wght@400;500&display=swap');"
        , ":root{--bg:#f7f3ec;--cell:#fff;--panel:#f0eadf;--fg:#2e3440;--dim:#6f6a5d;--accent:#c2674a;--border:#e7ddcf;--mono:'JetBrains Mono',ui-monospace,monospace}"
        , "*{box-sizing:border-box}"
        , "body{margin:0;background:radial-gradient(60% 44% at 86% -10%,rgba(208,135,111,.18),transparent 72%),var(--bg);color:var(--fg);font-family:Geist,system-ui,sans-serif;line-height:1.6}"
        , "a{color:var(--accent)}"
        , ".top{display:flex;align-items:center;justify-content:space-between;padding:0 clamp(1.2rem,5vw,3rem);height:58px;border-bottom:1px solid var(--border);background:var(--panel);position:sticky;top:0}"
        , ".brand{font-weight:600;font-size:1.1rem;text-decoration:none;color:var(--fg)}.brand .lam{color:var(--accent);font-family:var(--mono)}"
        , ".acts{display:flex;gap:.7rem;align-items:center;font-family:var(--mono);font-size:.8rem}"
        , ".acts .gal{color:var(--dim)}.acts .gal:hover{color:var(--fg)}"
        , ".acts .dl{color:var(--dim);border:1px solid var(--border);border-radius:6px;padding:.3rem .7rem;background:var(--cell);text-decoration:none}"
        , ".acts .dl:hover{color:var(--fg);border-color:var(--dim)}"
        , ".acts form{margin:0}"
        , ".acts .fork{color:#fff;background:var(--accent);border:none;border-radius:6px;padding:.38rem .9rem;font:inherit;font-weight:600;cursor:pointer}"
        , ".acts .fork:hover{opacity:.9}"
        , "main{max-width:46rem;margin:0 auto;padding:clamp(1.6rem,4vw,3rem) clamp(1.2rem,5vw,2rem) 5rem}"
        , "h1,h2,h3{font-family:Fraunces,Georgia,serif;letter-spacing:-.01em;line-height:1.2}"
        , "h1{font-size:clamp(1.9rem,4vw,2.6rem);margin:.2rem 0 .3rem}"
        , "h2{font-size:1.5rem;margin:2.2rem 0 .6rem}"
        , ".byline{font-family:var(--mono);font-size:.8rem;color:var(--accent);margin:0 0 2rem}"
        , "p{margin:.9rem 0}"
        , "code{font-family:var(--mono);font-size:.86em;background:var(--panel);padding:.08em .35em;border-radius:4px}"
        , "pre.code{background:#fff;border:1px solid var(--border);border-left:3px solid var(--accent);border-radius:8px;padding:1rem 1.2rem;overflow:auto;box-shadow:0 4px 14px rgba(70,52,34,.06)}"
        , "pre.code code{background:none;padding:0;font-size:.82rem;line-height:1.5}"
        , "pre.code code.hljs{background:transparent;padding:0}"
        , ".out{border-left:3px solid #6f9355;background:#fff;border-radius:0 8px 8px 0;padding:.8rem 1.1rem;margin:.4rem 0 1.4rem}"
        , "pre.out{font-family:var(--mono);font-size:.8rem;color:var(--dim);white-space:pre-wrap}"
        , "img{max-width:100%;border-radius:8px}"
        ]

{- | Inject the warm-lambda brand mark into a pre-built dashboard export's
header. Idempotent: a dashboard already carrying the brand is left untouched.
-}
brandDashboard :: Text -> Text
brandDashboard html
    | "db-brand" `T.isInfixOf` html = html
    | otherwise = replaceFirst "</head>" (css <> "</head>") withMark
  where
    mark = "<span class=\"db-brand\"><span class=\"lam\">\955</span> Sabela</span>"
    withMark =
        replaceFirst
            "<h1 id=\"dashboard-title\""
            (mark <> "<h1 id=\"dashboard-title\"")
            html
    css =
        "<style>.db-brand{font-family:var(--font-mono);font-weight:600;"
            <> "font-size:15px;color:var(--fg-heading);white-space:nowrap}"
            <> ".db-brand .lam{color:#c2674a}</style>"

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle repl hay
    | T.null rest = hay
    | otherwise = before <> repl <> T.drop (T.length needle) rest
  where
    (before, rest) = T.breakOn needle hay
