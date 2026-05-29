{-# LANGUAGE OverloadedStrings #-}

{- | Embedded HTML pages and small WAI response builders. This is a leaf module
(no @Hub.*@ dependencies): putting the response builders here lets both
'Hub.Auth' and 'Hub.Shares.Api' share them without a cycle.
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

shareNotFoundHtml :: Text
shareNotFoundHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\"><head><meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Not found \183 Sabela</title>"
        , "<style>"
        , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        , "    background:#0f1117; color:#e6edf3; min-height:100vh; margin:0;"
        , "    display:grid; place-items:center; }"
        , "  h1 { font-size:52px; margin:0 0 8px; color:#58a6ff; }"
        , "  p { color:#8b949e; }"
        , "</style></head>"
        , "<body><div style=\"text-align:center\">"
        , "  <h1>404</h1>"
        , "  <p>This shared notebook doesn't exist or was unpublished.</p>"
        , "</div></body></html>"
        ]

startingHtml :: Text
startingHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , ""
        , "<title>Sabela - Starting</title>"
        , "<style>"
        , "  * { margin: 0; padding: 0; box-sizing: border-box; }"
        , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        , "    background: #0f1117; color: #e1e4e8; display: flex; align-items: center;"
        , "    justify-content: center; min-height: 100vh; }"
        , "  .card { background: #1c1f26; border: 1px solid #2d3139; border-radius: 12px;"
        , "    padding: 40px; width: 380px; text-align: center; }"
        , "  h1 { font-size: 24px; margin-bottom: 8px; }"
        , "  p { color: #8b949e; font-size: 14px; margin-bottom: 16px; }"
        , "  .spinner { display: inline-block; width: 24px; height: 24px;"
        , "    border: 3px solid #2d3139; border-top-color: #58a6ff;"
        , "    border-radius: 50%; animation: spin 1s linear infinite; }"
        , "  @keyframes spin { to { transform: rotate(360deg); } }"
        , "</style>"
        , "</head>"
        , "<body>"
        , "<div class=\"card\">"
        , "  <h1>Sabela</h1>"
        , "  <p>Starting your notebook environment...</p>"
        , "  <div class=\"spinner\"></div>"
        , "  <p id=\"status\" style=\"margin-top:16px;font-size:12px;\">This may take a couple of minutes.</p>"
        , "  <script>"
        , "    async function poll() {"
        , "      try {"
        , "        const r = await fetch('/_hub/health');"
        , "        if (r.ok) {"
        , "          const r2 = await fetch('/api/notebook');"
        , "          if (r2.ok) { window.location.reload(); return; }"
        , "        }"
        , "      } catch(e) {}"
        , "      setTimeout(poll, 5000);"
        , "    }"
        , "    setTimeout(poll, 5000);"
        , "  </script>"
        , "</div>"
        , "</body>"
        , "</html>"
        ]

loginHtml :: Text
loginHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela</title>"
        , "<style>"
        , "  * { margin: 0; padding: 0; box-sizing: border-box; }"
        , "  :root { --bg:#0f1117; --panel:#161a22; --border:#2a2f3a; --text:#e6edf3;"
        , "    --muted:#8b949e; --accent:#58a6ff; --green:#3fb950; }"
        , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        , "    background: var(--bg); color: var(--text); min-height: 100vh;"
        , "    display: grid; place-items: center; padding: 32px;"
        , "    background-image:"
        , "      radial-gradient(60% 50% at 80% 6%, rgba(88,166,255,.16), transparent 70%),"
        , "      radial-gradient(48% 42% at 6% 96%, rgba(63,185,80,.10), transparent 70%); }"
        , "  .hero { width: 100%; max-width: 980px; display: grid;"
        , "    grid-template-columns: 1.05fr 0.95fr; gap: 56px; align-items: center; }"
        , "  .brand { display: flex; align-items: center; gap: 9px; font-weight: 600;"
        , "    margin-bottom: 8px; }"
        , "  .brand .mark { color: var(--accent); font-size: 18px; }"
        , "  .eyebrow { color: var(--muted); font-size: 13px; letter-spacing: .3px;"
        , "    margin-bottom: 18px; }"
        , "  h1 { font-size: 44px; line-height: 1.08; letter-spacing: -.5px;"
        , "    margin-bottom: 16px; }"
        , "  h1 .accent { color: var(--accent); }"
        , "  .lede { color: var(--muted); font-size: 16px; line-height: 1.55;"
        , "    max-width: 30em; margin-bottom: 28px; }"
        , "  .google-btn { display: inline-flex; align-items: center; gap: 10px;"
        , "    padding: 11px 22px; background: #fff; color: #3c4043;"
        , "    border: 1px solid #dadce0; border-radius: 8px; font-size: 15px;"
        , "    font-weight: 500; text-decoration: none; cursor: pointer;"
        , "    transition: box-shadow .2s, transform .2s; }"
        , "  .google-btn:hover { box-shadow: 0 4px 14px rgba(0,0,0,.35);"
        , "    transform: translateY(-1px); }"
        , "  .google-btn svg { width: 18px; height: 18px; }"
        , "  .features { display: flex; flex-wrap: wrap; gap: 18px; margin-top: 30px;"
        , "    color: var(--muted); font-size: 13px; }"
        , "  .art { width: 100%; height: auto;"
        , "    filter: drop-shadow(0 18px 40px rgba(0,0,0,.45)); }"
        , "  @media (max-width: 880px) {"
        , "    .hero { grid-template-columns: 1fr; gap: 40px; max-width: 460px; }"
        , "    h1 { font-size: 34px; } .art { order: 2; } }"
        , "</style>"
        , "</head>"
        , "<body>"
        , "<main class=\"hero\">"
        , "  <section class=\"copy\">"
        , "    <div class=\"brand\"><span class=\"mark\">\9670</span> Sabela</div>"
        , "    <p class=\"eyebrow\">A DataHaskell project</p>"
        , "    <h1>Reactive notebooks<br>for <span class=\"accent\">Haskell</span></h1>"
        , "    <p class=\"lede\">Write Haskell and Markdown in one file. Change a cell"
        , "      and the cells that depend on it re-run automatically, keeping code"
        , "      and output in sync.</p>"
        , "    <a href=\"/_hub/login\" class=\"google-btn\">"
        , "      <svg viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path fill=\"#4285F4\" d=\"M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92a5.06 5.06 0 0 1-2.2 3.32v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.1z\"/><path fill=\"#34A853\" d=\"M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z\"/><path fill=\"#FBBC05\" d=\"M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z\"/><path fill=\"#EA4335\" d=\"M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z\"/></svg>"
        , "      Sign in with Google"
        , "    </a>"
        , "    <div class=\"features\"><span>Reactive execution</span>"
        , "      <span>Haskell \183 Python</span>"
        , "      <span>Charts, tables, LaTeX</span></div>"
        , "  </section>"
        , "  <aside>"
        , "    <svg class=\"art\" viewBox=\"0 0 460 300\" role=\"img\" aria-label=\"A Sabela notebook: a Haskell cell computing the mean of a list, rendering a bar chart\">"
        , "      <rect x=\"0\" y=\"0\" width=\"460\" height=\"300\" rx=\"14\" fill=\"#161a22\" stroke=\"#2a2f3a\"/>"
        , "      <circle cx=\"24\" cy=\"26\" r=\"5\" fill=\"#ff5f56\"/><circle cx=\"42\" cy=\"26\" r=\"5\" fill=\"#ffbd2e\"/><circle cx=\"60\" cy=\"26\" r=\"5\" fill=\"#27c93f\"/>"
        , "      <rect x=\"372\" y=\"16\" width=\"64\" height=\"20\" rx=\"10\" fill=\"#1f2530\"/>"
        , "      <text x=\"404\" y=\"30\" fill=\"#8b949e\" font-family=\"monospace\" font-size=\"11\" text-anchor=\"middle\">haskell</text>"
        , "      <line x1=\"0\" y1=\"48\" x2=\"460\" y2=\"48\" stroke=\"#2a2f3a\"/>"
        , "      <g font-family=\"ui-monospace, SFMono-Regular, Menlo, monospace\" font-size=\"15\">"
        , "        <text x=\"24\" y=\"80\" fill=\"#e6edf3\">xs = [1..100]</text>"
        , "        <text x=\"24\" y=\"106\" fill=\"#e6edf3\">mean xs</text>"
        , "        <text x=\"24\" y=\"132\" fill=\"#3fb950\">\8658 50.5</text>"
        , "      </g>"
        , "      <rect x=\"24\" y=\"152\" width=\"412\" height=\"126\" rx=\"10\" fill=\"#0f1117\" stroke=\"#2a2f3a\"/>"
        , "      <g fill=\"#58a6ff\">"
        , "        <rect x=\"44\" y=\"250\" width=\"22\" height=\"16\" rx=\"3\"/>"
        , "        <rect x=\"76\" y=\"238\" width=\"22\" height=\"28\" rx=\"3\"/>"
        , "        <rect x=\"108\" y=\"222\" width=\"22\" height=\"44\" rx=\"3\"/>"
        , "        <rect x=\"140\" y=\"204\" width=\"22\" height=\"62\" rx=\"3\"/>"
        , "        <rect x=\"172\" y=\"186\" width=\"22\" height=\"80\" rx=\"3\"/>"
        , "        <rect x=\"204\" y=\"172\" width=\"22\" height=\"94\" rx=\"3\"/>"
        , "        <rect x=\"236\" y=\"174\" width=\"22\" height=\"92\" rx=\"3\"/>"
        , "        <rect x=\"268\" y=\"192\" width=\"22\" height=\"74\" rx=\"3\"/>"
        , "        <rect x=\"300\" y=\"210\" width=\"22\" height=\"56\" rx=\"3\"/>"
        , "        <rect x=\"332\" y=\"228\" width=\"22\" height=\"38\" rx=\"3\"/>"
        , "        <rect x=\"364\" y=\"242\" width=\"22\" height=\"24\" rx=\"3\"/>"
        , "        <rect x=\"396\" y=\"252\" width=\"22\" height=\"14\" rx=\"3\"/>"
        , "      </g>"
        , "    </svg>"
        , "  </aside>"
        , "</main>"
        , "</body>"
        , "</html>"
        ]
