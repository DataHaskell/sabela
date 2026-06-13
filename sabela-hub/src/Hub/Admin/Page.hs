{-# LANGUAGE OverloadedStrings #-}

{- | The server-rendered admin curation page at @\/_hub\/admin@ (inline JS →
the JSON endpoints in "Hub.Admin.Api"). Reflected titles are rendered with
@textContent@, never @innerHTML@, so the client side has no DOM-XSS sink.
-}
module Hub.Admin.Page (
    adminPage,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseLBS)

{- | The admin page. @contact@ (if set) drives the request-access note; when
unset, a banner reminds the operator the gallery CTA is disabled.
-}
adminPage :: Maybe Text -> Response
adminPage contact =
    responseLBS
        status200
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("X-Frame-Options", "SAMEORIGIN")
        , ("Content-Security-Policy", "frame-ancestors 'self'")
        ]
        (BL.fromStrict (TE.encodeUtf8 (pageHtml (bannerFor contact))))

pageHtml :: Text -> Text
pageHtml banner =
    T.unlines
        [ "<!doctype html><html lang=en><head><meta charset=utf-8>"
        , "<meta name=viewport content='width=device-width,initial-scale=1'>"
        , "<title>Sabela gallery — admin</title>"
        , styleTag
        , "</head><body>"
        , "<header><h1>Gallery curation</h1>"
        , "<span id=dangle class=badge hidden></span></header>"
        , banner
        , "<p class=sop>SOP: email the author and get a yes <b>before</b> featuring."
        , " Re-published notebooks lose their slug and must be re-linked.</p>"
        , "<section><h2>Collections</h2>"
        , "<form id=newcol><input name=title placeholder='Collection title' required>"
        , "<input name=description placeholder='Description'>"
        , "<button>Create</button></form><div id=cols></div></section>"
        , "<section><h2>Shares</h2><div id=shares></div></section>"
        , scriptTag
        , "</body></html>"
        ]

bannerFor :: Maybe Text -> Text
bannerFor (Just _) = ""
bannerFor Nothing =
    "<p class=warn>Conversion CTA disabled — set HUB_ADMIN_CONTACT.</p>"

styleTag :: Text
styleTag =
    "<style>\
    \body{font:15px/1.5 system-ui,sans-serif;max-width:60rem;margin:1.5rem auto;padding:0 1rem;color:#1b1a17}\
    \header{display:flex;align-items:center;gap:.6rem}\
    \h1{font-size:1.4rem}h2{font-size:1.1rem;margin-top:1.5rem}\
    \.badge{background:#bf616a;color:#fff;border-radius:999px;padding:.1rem .6rem;font-size:.8rem}\
    \.warn{background:#fef2f2;border:1px solid #f3c0c0;padding:.5rem;border-radius:6px}\
    \.sop{color:#555;font-size:.9rem}\
    \table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #e7e1f0;padding:.4rem;text-align:left;font-size:.9rem}\
    \button{cursor:pointer}input{padding:.3rem;margin-right:.3rem}\
    \.on{color:#5e81ac;font-weight:600}.dangling{color:#bf616a}\
    \</style>"

scriptTag :: Text
scriptTag =
    "<script>\
    \const J=(u,m,b)=>fetch(u,{method:m||'GET',headers:b?{'Content-Type':'application/json'}:{},body:b?JSON.stringify(b):undefined}).then(r=>r.ok?r.json().catch(()=>({})):Promise.reject(r.status));\
    \function el(t,txt){const e=document.createElement(t);if(txt!=null)e.textContent=txt;return e}\
    \async function load(){const d=await J('/_hub/admin/shares');renderShares(d);renderDangle(d.dangling||[]);renderCols(d.shares||[])}\
    \function renderDangle(dl){const b=document.getElementById('dangle');if(dl.length){b.hidden=false;b.textContent=dl.length+' missing — re-link'}else b.hidden=true}\
    \function renderShares(d){const root=document.getElementById('shares');root.textContent='';const t=el('table');const hr=el('tr');['Title','Owner','Mode','Featured','Tags',''].forEach(h=>hr.appendChild(el('th',h)));t.appendChild(hr);\
    \(d.shares||[]).forEach(s=>{const tr=el('tr');tr.appendChild(el('td',s.title));tr.appendChild(el('td',s.owner));tr.appendChild(el('td',s.mode));const f=el('td',s.featured?'yes':'no');if(s.featured)f.className='on';tr.appendChild(f);\
    \const tg=el('td');const ti=el('input');ti.value=(s.tags||[]).join(',');ti.size=14;const tb=el('button','tags');tb.onclick=()=>J('/_hub/admin/tags/'+s.slug,'PUT',{tags:ti.value.split(',').map(x=>x.trim()).filter(Boolean)}).then(load).catch(e=>alert('tags: '+e));tg.appendChild(ti);tg.appendChild(tb);tr.appendChild(tg);\
    \const ac=el('td');const b=el('button',s.featured?'unfeature':'feature');b.onclick=()=>(s.featured?J('/_hub/admin/feature/'+s.slug,'DELETE'):J('/_hub/admin/feature','POST',{slug:s.slug})).then(load).catch(e=>alert(e));ac.appendChild(b);tr.appendChild(ac);t.appendChild(tr)});root.appendChild(t)}\
    \function renderCols(shares){const root=document.getElementById('cols');root.textContent='';}\
    \document.getElementById('newcol').onsubmit=e=>{e.preventDefault();const f=e.target;J('/_hub/admin/collection','POST',{title:f.title.value,description:f.description.value}).then(()=>{f.reset();load()}).catch(x=>alert(x))};\
    \load();\
    \</script>"
