{-# LANGUAGE OverloadedStrings #-}

{- | The HTML pages for the @siza@ CLI device-authorization flow, split from
'Hub.CliAuth' for the module-size cap.

The authorize page is served with @no-store@ + anti-framing headers (it carries
a live CSRF nonce and is a one-click-sensitive approval surface). Only the CSRF
nonce is embedded (an HTML-escaped @data-@ attribute read from @dataset@, never
interpolated into JS source). The user code is deliberately NOT placed in the
page — neither rendered nor in a @data-@ attribute — so a logged-in victim lured
to a crafted @?code=@ link has nothing to type and cannot approve a request they
never initiated. The approver must supply the code out-of-band from their own
terminal; 'Hub.CliAuth.handleCliApprove' validates the typed code server-side.
-}
module Hub.CliAuth.Page (
    authorizePage,
    noticePage,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hub.Gallery.Chrome (htmlEscape)
import Network.HTTP.Types
import Network.Wai (Response, responseLBS)

-- | The authorize page for a pending request, carrying only its CSRF nonce.
authorizePage :: Text -> Response
authorizePage csrf =
    securePage status200 $
        T.concat
            [ shellHead "Authorize siza CLI"
            , "<h1>Authorize the siza CLI</h1>"
            , "<p>A terminal on your machine is asking to drive your notebook. "
            , "If you didn't start <code>siza login</code>, close this page \x2014 "
            , "do not enter a code someone else gave you.</p>"
            , "<p>To approve, type the code shown <strong>in your terminal</strong>:</p>"
            , "<p><input id=confirm autocomplete=off autocapitalize=characters "
            , "spellcheck=false placeholder='enter the code'></p>"
            , "<p><button id=go disabled data-csrf=\""
            , htmlEscape csrf
            , "\">Approve access</button></p>"
            , "<p id=ok hidden>Approved \x2014 return to your terminal.</p>"
            , "<script>"
            , approveScript
            , "</script></body></html>"
            ]

-- | A minimal notice page (missing/expired code), with the same secure headers.
noticePage :: Status -> Text -> Response
noticePage st msg =
    securePage
        st
        (shellHead "siza CLI" <> "<p>" <> htmlEscape msg <> "</p></body></html>")

{- | The script: enable Approve once a code is typed, then POST the typed code
plus the CSRF nonce (read from the button's @dataset@, not spliced into source).
The code is never in the page, so it must come from the terminal; the server
validates the typed value against the pending request.
-}
approveScript :: Text
approveScript =
    T.concat
        [ "var go=document.getElementById('go'),inp=document.getElementById('confirm');"
        , "inp.addEventListener('input',function(){go.disabled=inp.value.trim().length===0;});"
        , "go.addEventListener('click',async function(){"
        , "  var code=inp.value.trim().toUpperCase(); if(!code)return; go.disabled=true;"
        , "  var r=await fetch('/_hub/cli-auth/approve',{method:'POST',"
        , "    headers:{'content-type':'application/json'},"
        , "    body:JSON.stringify({userCode:code,csrf:go.dataset.csrf})});"
        , "  if(r.ok){document.getElementById('ok').hidden=false;go.hidden=true;inp.disabled=true;}"
        , "  else{go.disabled=false;alert('Authorization failed or expired. Re-run siza login.');}"
        , "});"
        ]

shellHead :: Text -> Text
shellHead title =
    T.concat
        [ "<!doctype html><html lang=en><head><meta charset=utf-8>"
        , "<meta name=viewport content='width=device-width,initial-scale=1'>"
        , "<title>"
        , htmlEscape title
        , "</title><style>"
        , "body{font-family:system-ui,sans-serif;max-width:34rem;margin:4rem auto;"
        , "padding:0 1.5rem;color:#2e3440;background:#fdfaf5}h1{font-size:1.4rem}"
        , "input{font-size:1rem;padding:.5rem;width:14rem;"
        , "letter-spacing:.1em}button{font-size:1rem;padding:.7rem 1.4rem;border:0;border-radius:.5rem;"
        , "background:#5e81ac;color:#fff;cursor:pointer}button:disabled{background:#9aa5b1;cursor:default}"
        , "#ok{color:#3b7a57;font-weight:600}</style></head><body>"
        ]

-- | An auth-approval page: never cached, never framed.
securePage :: Status -> Text -> Response
securePage st body =
    responseLBS
        st
        [ (hContentType, "text/html; charset=utf-8")
        , ("Cache-Control", "no-store")
        , ("X-Frame-Options", "DENY")
        , ("Content-Security-Policy", "frame-ancestors 'none'")
        ]
        (BL.fromStrict (TE.encodeUtf8 body))
