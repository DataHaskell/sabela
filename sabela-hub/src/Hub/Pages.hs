{-# LANGUAGE OverloadedStrings #-}

{- | Embedded HTML pages and small WAI response builders. This is a leaf module
(no @Hub.*@ dependencies): putting the response builders here lets both
'Hub.Auth' and 'Hub.Shares.Api' share them without a cycle. The pages share the
Warm Paper palette and fonts used by the gallery ("Hub.Gallery.Style") so the
whole site reads as one product.
-}
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

{- | Shared Warm Paper base: the gallery font import, palette tokens, page
ground (warm radial gradient), and the brand mark. Goes first in every
@<style>@ so its @\@import@ is the leading rule.
-}
themeStyle :: Text
themeStyle =
    T.concat
        [ "@import url('https://fonts.googleapis.com/css2?family=Geist:wght@400;500;600&family=Fraunces:opsz,wght@9..144,500;9..144,600&family=JetBrains+Mono:wght@400;500&display=swap');"
        , "*{margin:0;padding:0;box-sizing:border-box}"
        , ":root{--bg:#f7f3ec;--cell:#ffffff;--panel:#f0eadf;--fg:#2e3440;--dim:#6f6a5d;--accent:#c2674a;--border:#e7ddcf;--green:#6f9355;--mono:'JetBrains Mono',ui-monospace,monospace}"
        , "body{font-family:'Geist',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;color:var(--fg);min-height:100vh;line-height:1.5;background:radial-gradient(60% 44% at 86% -10%,rgba(208,135,111,.20),transparent 72%),radial-gradient(52% 42% at -8% 112%,rgba(216,166,87,.18),transparent 74%),var(--bg)}"
        , "a{color:var(--accent)}"
        , ".brand{display:flex;align-items:center;gap:8px;font-weight:600;font-size:1.05rem}.brand .lam{color:var(--accent);font-family:var(--mono)}"
        ]

shareNotFoundHtml :: Text
shareNotFoundHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Not found \183 Sabela</title>"
        , "<style>"
        , themeStyle
        , "body{display:grid;place-items:center;text-align:center;padding:32px}"
        , ".brand{justify-content:center;margin-bottom:20px}"
        , "h1{font-family:'Fraunces',Georgia,serif;font-size:56px;font-weight:600;color:var(--accent);margin-bottom:6px}"
        , "p{color:var(--dim)}"
        , "</style></head>"
        , "<body><div>"
        , "<div class=\"brand\"><span class=\"lam\">\955</span> Sabela</div>"
        , "<h1>404</h1>"
        , "<p>This shared notebook doesn't exist or was unpublished.</p>"
        , "<p style=\"margin-top:14px\"><a href=\"/gallery\">Browse the gallery</a></p>"
        , "</div></body></html>"
        ]

startingHtml :: Text
startingHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela \183 Starting</title>"
        , "<style>"
        , themeStyle
        , "body{display:flex;align-items:center;justify-content:center;padding:32px}"
        , ".card{background:var(--cell);border:1px solid var(--border);border-radius:14px;padding:40px;width:380px;max-width:100%;text-align:center;box-shadow:0 12px 32px rgba(70,52,34,.10)}"
        , ".card .brand{justify-content:center;margin-bottom:16px}"
        , "h1{font-family:'Fraunces',Georgia,serif;font-size:22px;font-weight:600;margin-bottom:8px}"
        , "p{color:var(--dim);font-size:14px;margin-bottom:16px}"
        , ".spinner{display:inline-block;width:24px;height:24px;border:3px solid var(--border);border-top-color:var(--accent);border-radius:50%;animation:spin 1s linear infinite}"
        , "@keyframes spin{to{transform:rotate(360deg)}}"
        , "</style></head>"
        , "<body><div class=\"card\">"
        , "<div class=\"brand\"><span class=\"lam\">\955</span> Sabela</div>"
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
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela</title>"
        , "<style>"
        , themeStyle
        , "body{display:grid;place-items:center;padding:32px}"
        , ".hero{width:100%;max-width:980px;display:grid;grid-template-columns:1.05fr .95fr;gap:56px;align-items:center}"
        , ".eyebrow{color:var(--dim);font-family:var(--mono);font-size:.74rem;letter-spacing:.04em;margin:14px 0 16px}"
        , "h1{font-family:'Fraunces',Georgia,serif;font-size:44px;line-height:1.08;letter-spacing:-.01em;font-weight:600;margin-bottom:16px}"
        , "h1 .accent{color:var(--accent)}"
        , ".lede{color:var(--dim);font-size:16px;line-height:1.55;max-width:30em;margin-bottom:28px}"
        , ".google-btn{display:inline-flex;align-items:center;gap:10px;padding:11px 22px;background:var(--accent);color:#fff;border:none;border-radius:8px;font-size:15px;font-weight:600;text-decoration:none;cursor:pointer;transition:box-shadow .15s,transform .15s}"
        , ".google-btn:hover{box-shadow:0 8px 22px rgba(194,103,74,.30);transform:translateY(-1px)}"
        , ".google-btn svg{width:18px;height:18px;background:#fff;border-radius:3px;padding:1px}"
        , ".features{display:flex;flex-wrap:wrap;gap:18px;margin-top:30px;color:var(--dim);font-family:var(--mono);font-size:.74rem}"
        , ".art{width:100%;height:auto;filter:drop-shadow(0 18px 40px rgba(70,52,34,.18))}"
        , "@media (max-width:880px){.hero{grid-template-columns:1fr;gap:40px;max-width:460px}h1{font-size:34px}.art{order:2}}"
        , "</style></head>"
        , "<body><main class=\"hero\">"
        , "<section class=\"copy\">"
        , "<div class=\"brand\"><span class=\"lam\">\955</span> Sabela</div>"
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
        , "    <rect x=\"0\" y=\"0\" width=\"460\" height=\"300\" rx=\"14\" fill=\"#ffffff\" stroke=\"#e7ddcf\"/>"
        , "    <circle cx=\"24\" cy=\"26\" r=\"5\" fill=\"#ff5f56\"/><circle cx=\"42\" cy=\"26\" r=\"5\" fill=\"#ffbd2e\"/><circle cx=\"60\" cy=\"26\" r=\"5\" fill=\"#27c93f\"/>"
        , "    <rect x=\"372\" y=\"16\" width=\"64\" height=\"20\" rx=\"10\" fill=\"#f0eadf\"/>"
        , "    <text x=\"404\" y=\"30\" fill=\"#6f6a5d\" font-family=\"monospace\" font-size=\"11\" text-anchor=\"middle\">haskell</text>"
        , "    <line x1=\"0\" y1=\"48\" x2=\"460\" y2=\"48\" stroke=\"#e7ddcf\"/>"
        , "    <g font-family=\"'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace\" font-size=\"15\">"
        , "      <text x=\"24\" y=\"80\" fill=\"#2e3440\">xs = [1..100]</text>"
        , "      <text x=\"24\" y=\"106\" fill=\"#2e3440\">mean xs</text>"
        , "      <text x=\"24\" y=\"132\" fill=\"#6f9355\">\8658 50.5</text>"
        , "    </g>"
        , "    <rect x=\"24\" y=\"152\" width=\"412\" height=\"126\" rx=\"10\" fill=\"#f7f3ec\" stroke=\"#e7ddcf\"/>"
        , "    <g fill=\"#c2674a\">"
        , "      <rect x=\"44\" y=\"250\" width=\"22\" height=\"16\" rx=\"3\"/>"
        , "      <rect x=\"76\" y=\"238\" width=\"22\" height=\"28\" rx=\"3\"/>"
        , "      <rect x=\"108\" y=\"222\" width=\"22\" height=\"44\" rx=\"3\"/>"
        , "      <rect x=\"140\" y=\"204\" width=\"22\" height=\"62\" rx=\"3\"/>"
        , "      <rect x=\"172\" y=\"186\" width=\"22\" height=\"80\" rx=\"3\"/>"
        , "      <rect x=\"204\" y=\"172\" width=\"22\" height=\"94\" rx=\"3\"/>"
        , "      <rect x=\"236\" y=\"174\" width=\"22\" height=\"92\" rx=\"3\"/>"
        , "      <rect x=\"268\" y=\"192\" width=\"22\" height=\"74\" rx=\"3\"/>"
        , "      <rect x=\"300\" y=\"210\" width=\"22\" height=\"56\" rx=\"3\"/>"
        , "      <rect x=\"332\" y=\"228\" width=\"22\" height=\"38\" rx=\"3\"/>"
        , "      <rect x=\"364\" y=\"242\" width=\"22\" height=\"24\" rx=\"3\"/>"
        , "      <rect x=\"396\" y=\"252\" width=\"22\" height=\"14\" rx=\"3\"/>"
        , "    </g>"
        , "  </svg>"
        , "</aside>"
        , "</main></body></html>"
        ]
