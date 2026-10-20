{-# LANGUAGE OverloadedStrings #-}

module Hub.Pages (
    loginHtml,
    loginPage,
    startingHtml,
    startingPage,
    shareNotFoundHtml,
    textResponse,
    jsonResponse,
    jsonError,
) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

import Hub.Brand (brandMark, faviconTag)
import Network.Wai

textResponse :: Status -> Text -> Response
textResponse s msg =
    responseLBS
        s
        [(hContentType, "text/plain")]
        (BL.fromStrict (TE.encodeUtf8 msg))

jsonResponse :: Status -> Value -> Response
jsonResponse st = responseLBS st [(hContentType, "application/json")] . encode

jsonError :: Status -> Text -> Response
jsonError st msg = jsonResponse st (object ["error" .= msg])

loginPage :: Response
loginPage =
    responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (BL.fromStrict (TE.encodeUtf8 loginHtml))

startingPage :: Response
startingPage =
    responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (BL.fromStrict (TE.encodeUtf8 startingHtml))

themeStyle :: Text
themeStyle =
    T.concat
        [ "@import url('https://fonts.googleapis.com/css2?family=Fira+Sans:wght@400;500&family=JetBrains+Mono:wght@400;500&display=swap');"
        , "*{margin:0;padding:0;box-sizing:border-box}"
        , ":root{--bg:#fbfbfb;--cell:#fbfbfb;--panel:#f2f2f2;--fg:#181818;--dim:#808080;--accent:#fc1785;--mauve:#fb6620;--blue:#1586d2;--border:#e8e8e8;--green:#22a21f;--radius:0;--mono:'JetBrains Mono',ui-monospace,monospace;--sans:'Fira Sans',-apple-system,BlinkMacSystemFont,system-ui,sans-serif}"
        , "body{font-family:var(--sans);color:var(--fg);min-height:100vh;line-height:1.5;background:var(--bg)}"
        , "a{color:var(--accent)}"
        , ".brand{display:flex;align-items:center;font-weight:500;font-size:1rem}"
        ]

shareNotFoundHtml :: Text
shareNotFoundHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">" <> faviconTag
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Not found \183 Sabela</title>"
        , "<style>"
        , themeStyle
        , "body{display:grid;place-items:center;text-align:center;padding:32px}"
        , ".brand{justify-content:center;margin-bottom:20px}"
        , "h1{font-size:56px;font-weight:500;color:var(--accent);margin-bottom:6px}"
        , "p{color:var(--dim)}"
        , "</style></head>"
        , "<body><div>"
        , "<div class=\"brand\">" <> brandMark <> "Sabela</div>"
        , "<h1>404</h1>"
        , "<p>This shared notebook doesn't exist or was unpublished.</p>"
        , "<p style=\"margin-top:14px\"><a href=\"/gallery\">Browse the gallery</a></p>"
        , "</div></body></html>"
        ]

startingHtml :: Text
startingHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">" <> faviconTag
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela \183 Starting</title>"
        , "<style>"
        , themeStyle
        , "body{display:flex;align-items:center;justify-content:center;padding:32px}"
        , ".card{background:var(--panel);border:1px solid var(--border);padding:32px;width:380px;max-width:100%;text-align:center}"
        , ".card .brand{justify-content:center;margin-bottom:16px}"
        , "h1{font-size:22px;font-weight:500;margin-bottom:8px}"
        , "p{color:var(--dim);font-size:14px;margin-bottom:16px}"
        , ".spinner{display:inline-block;width:20px;height:20px;border:2px solid var(--border);border-top-color:var(--accent);border-radius:50%;animation:spin 1s linear infinite}"
        , "@keyframes spin{to{transform:rotate(360deg)}}"
        , "</style></head>"
        , "<body><div class=\"card\">"
        , "<div class=\"brand\" style=\"justify-content:center;margin-bottom:16px\">"
            <> brandMark
            <> "Sabela</div>"
        , "<p>Starting your notebook environment...</p>"
        , "<div class=\"spinner\"></div>"
        , "<p id=\"status\" style=\"margin-top:16px;font-size:12px\">This usually takes a few seconds.</p>"
        , "<script>"
        , "  async function poll() {"
        , "    try {"
        , "      const r = await fetch('/_hub/health');"
        , "      if (r.ok) {"
        , "        const r2 = await fetch('/api/notebook');"
        , "        if (r2.ok) { window.location.reload(); return; }"
        , "      }"
        , "    } catch(e) {}"
        , "    setTimeout(poll, 5000);"
        , "  }"
        , "  setTimeout(poll, 5000);"
        , "</script>"
        , "</div></body></html>"
        ]

loginHtml :: Text
loginHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">" <> faviconTag
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela</title>"
        , "<style>"
        , themeStyle
        , "body{display:grid;place-items:center;padding:32px}"
        , ".hero{width:100%;max-width:980px;display:grid;grid-template-columns:1.05fr .95fr;gap:56px;align-items:center}"
        , ".eyebrow{color:var(--dim);font-family:var(--mono);font-size:.74rem;margin:14px 0 16px}"
        , "h1{font-size:40px;line-height:1.1;font-weight:500;margin-bottom:16px}"
        , "h1 .accent{color:var(--accent)}"
        , ".lede{color:var(--dim);font-size:16px;line-height:1.55;max-width:30em;margin-bottom:28px}"
        , ".google-btn{display:inline-flex;align-items:center;gap:10px;padding:8px 18px;background:transparent;color:var(--fg);border:1px solid var(--fg);font-size:14px;font-weight:500;text-decoration:none;cursor:pointer}"
        , ".google-btn:hover{background:var(--panel)}"
        , ".google-btn svg{width:16px;height:16px}"
        , ".features{display:flex;flex-wrap:wrap;gap:18px;margin-top:30px;color:var(--dim);font-family:var(--mono);font-size:.74rem}"
        , ".art{width:100%;height:auto}"
        , "@media (max-width:880px){.hero{grid-template-columns:1fr;gap:40px;max-width:460px}h1{font-size:34px}.art{order:2}}"
        , "</style></head>"
        , "<body><main class=\"hero\">"
        , "<section class=\"copy\">"
        , "<div class=\"brand\">" <> brandMark <> "Sabela</div>"
        , "<p class=\"eyebrow\">A DataHaskell project</p>"
        , "<h1>Reactive notebooks<br>for <span class=\"accent\">Haskell</span></h1>"
        , "<p class=\"lede\">Write Haskell and Markdown in one file. Change a cell"
        , "  and the cells that depend on it re-run automatically, keeping code"
        , "  and output in sync.</p>"
        , "<a href=\"/_hub/login\" class=\"google-btn\">"
        , "  <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path fill=\"#4285F4\" d=\"M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92a5.06 5.06 0 0 1-2.2 3.32v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.1z\"/><path fill=\"#34A853\" d=\"M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z\"/><path fill=\"#FBBC05\" d=\"M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z\"/><path fill=\"#EA4335\" d=\"M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z\"/></svg>"
        , "  Sign in with Google"
        , "</a>"
        , "<div class=\"features\"><span>Reactive execution</span>"
        , "  <span>Haskell \183 Python</span>"
        , "  <span>Charts, tables, LaTeX</span></div>"
        , "</section>"
        , "<aside>"
        , "  <svg class=\"art\" viewBox=\"0 0 460 300\" role=\"img\" aria-label=\"A Sabela notebook: a Haskell cell computing the mean of a list, rendering a bar chart\">"
        , "    <rect x=\"0.5\" y=\"0.5\" width=\"459\" height=\"299\" fill=\"#fbfbfb\" stroke=\"#e8e8e8\"/>"
        , "    <rect x=\"0.5\" y=\"0.5\" width=\"459\" height=\"33\" fill=\"#f2f2f2\" stroke=\"#e8e8e8\"/>"
        , "    <rect x=\"0.5\" y=\"0.5\" width=\"140\" height=\"33\" fill=\"#fbfbfb\" stroke=\"#e8e8e8\"/>"
        , "    <text x=\"16\" y=\"22\" fill=\"#181818\" font-family=\"'Fira Sans', system-ui, sans-serif\" font-size=\"13\">mean.md</text>"
        , "    <text x=\"444\" y=\"22\" fill=\"#808080\" font-family=\"'Fira Sans', system-ui, sans-serif\" font-size=\"12\" text-anchor=\"end\">Run all</text>"
        , "    <g font-family=\"'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace\" font-size=\"14\">"
        , "      <text x=\"16\" y=\"66\" fill=\"#808080\">1</text><text x=\"40\" y=\"66\" fill=\"#181818\">xs = [1..100]</text>"
        , "      <text x=\"16\" y=\"90\" fill=\"#808080\">2</text><text x=\"40\" y=\"90\" fill=\"#181818\">mean xs</text>"
        , "    </g>"
        , "    <line x1=\"0\" y1=\"108\" x2=\"460\" y2=\"108\" stroke=\"#e8e8e8\"/>"
        , "    <rect x=\"0\" y=\"108\" width=\"460\" height=\"192\" fill=\"#f2f2f2\"/>"
        , "    <text x=\"40\" y=\"134\" fill=\"#22a21f\" font-family=\"'JetBrains Mono', ui-monospace, monospace\" font-size=\"14\">50.5</text>"
        , "    <g fill=\"#fc1785\">"
        , "      <rect x=\"44\" y=\"266\" width=\"22\" height=\"16\"/>"
        , "      <rect x=\"76\" y=\"254\" width=\"22\" height=\"28\"/>"
        , "      <rect x=\"108\" y=\"238\" width=\"22\" height=\"44\"/>"
        , "      <rect x=\"140\" y=\"220\" width=\"22\" height=\"62\"/>"
        , "      <rect x=\"172\" y=\"202\" width=\"22\" height=\"80\"/>"
        , "      <rect x=\"204\" y=\"188\" width=\"22\" height=\"94\"/>"
        , "      <rect x=\"236\" y=\"190\" width=\"22\" height=\"92\"/>"
        , "      <rect x=\"268\" y=\"208\" width=\"22\" height=\"74\"/>"
        , "      <rect x=\"300\" y=\"226\" width=\"22\" height=\"56\"/>"
        , "      <rect x=\"332\" y=\"244\" width=\"22\" height=\"38\"/>"
        , "      <rect x=\"364\" y=\"258\" width=\"22\" height=\"24\"/>"
        , "      <rect x=\"396\" y=\"268\" width=\"22\" height=\"14\"/>"
        , "    </g>"
        , "    <line x1=\"40\" y1=\"282.5\" x2=\"430\" y2=\"282.5\" stroke=\"#808080\"/>"
        , "  </svg>"
        , "</aside>"
        , "</main></body></html>"
        ]
