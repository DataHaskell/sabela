{-# LANGUAGE OverloadedStrings #-}

-- | The gallery pages' inline stylesheet (Warm Paper palette + notebook cells).
module Hub.Gallery.Style (styleBlock) where

import Data.Text (Text)

-- | Nord palette + notebook-cell language, matched to the editor's default.
styleBlock :: Text
styleBlock =
    "<style>"
        <> "@import url('https://fonts.googleapis.com/css2?family=Geist:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap');"
        <> ":root{--bg:#f7f3ec;--cell:#ffffff;--panel:#f0eadf;--fg:#2e3440;--dim:#6f6a5d;--accent:#c2674a;--mauve:#9a6f96;--border:#e7ddcf;--radius:8px;--mono:'JetBrains Mono',ui-monospace,monospace}"
        <> "*{box-sizing:border-box}body{margin:0;background:radial-gradient(60% 44% at 86% -10%,rgba(208,135,111,.20),transparent 72%),radial-gradient(52% 42% at -8% 112%,rgba(216,166,87,.18),transparent 74%),var(--bg);color:var(--fg);font-family:Geist,-apple-system,system-ui,sans-serif;line-height:1.5}"
        <> "a{color:inherit;text-decoration:none}"
        <> ".top{display:flex;align-items:center;justify-content:space-between;padding:0 clamp(1.2rem,5vw,3rem);height:58px;border-bottom:1px solid var(--border);background:var(--panel);position:sticky;top:0}"
        <> ".brand{font-weight:600;font-size:1.1rem}.brand .lam{color:var(--accent);font-family:var(--mono)}"
        <> ".hnav{display:flex;gap:1.4rem;align-items:center;font-family:var(--mono);font-size:.8rem;color:var(--dim)}.hnav a:hover{color:var(--fg)}"
        <> ".signin{color:var(--bg);background:var(--accent);padding:.35rem .8rem;border-radius:6px;font-weight:500}"
        <> ".masthead{padding:clamp(2rem,5vw,3.2rem) clamp(1.2rem,5vw,3rem) 1rem;max-width:48rem}"
        <> ".kick{font-family:var(--mono);font-size:.72rem;color:var(--accent)}"
        <> ".masthead h1{font-size:clamp(1.7rem,3.5vw,2.3rem);font-weight:600;letter-spacing:-.02em;margin:.5rem 0 .5rem;background:linear-gradient(95deg,#bb842c,#c2674a 52%,#bf616a);-webkit-background-clip:text;background-clip:text;color:transparent}"
        <> ".masthead p{color:var(--dim);font-size:1.02rem}"
        <> ".chips{display:flex;flex-wrap:wrap;gap:.45rem;padding:.4rem clamp(1.2rem,5vw,3rem) 1.4rem}"
        <> ".chip{font-family:var(--mono);font-size:.74rem;color:var(--dim);border:1px solid var(--border);border-radius:999px;padding:.18rem .65rem;background:var(--cell)}"
        <> ".chip:hover{color:var(--fg)}.chip.on{background:var(--accent);color:var(--bg);border-color:var(--accent);font-weight:500}"
        <> ".feed{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:1rem;padding:0 clamp(1.2rem,5vw,3rem) 3rem}"
        <> ".cell{display:flex;flex-direction:column;gap:.5rem;background:var(--cell);border:1px solid var(--border);border-top:3px solid var(--c,var(--accent));border-radius:var(--radius);padding:1.1rem 1.3rem 1.2rem;box-shadow:0 4px 14px rgba(70,52,34,.07);transition:transform .15s,border-color .15s,box-shadow .15s}"
        <> ".cell:nth-child(6n+1){--c:#c2674a}.cell:nth-child(6n+2){--c:#bb842c}.cell:nth-child(6n+3){--c:#6f9355}.cell:nth-child(6n+4){--c:#bf616a}.cell:nth-child(6n+5){--c:#4f8a99}.cell:nth-child(6n+6){--c:#9a6f96}"
        <> ".cell:hover{transform:translateY(-4px);border-color:var(--c,var(--accent));box-shadow:0 14px 30px rgba(70,52,34,.14)}"
        <> ".cell.spotlight{grid-column:1/-1;background:linear-gradient(120deg,rgba(194,103,74,.08),var(--cell) 58%)}"
        <> ".crow{display:flex;justify-content:space-between;font-family:var(--mono);font-size:.66rem;letter-spacing:.06em}"
        <> ".num{color:var(--dim)}.kind{text-transform:uppercase;font-weight:600;color:var(--c,var(--accent))}"
        <> ".ctitle{text-decoration:none;color:inherit}.ctitle:hover h3{color:var(--c,var(--accent))}"
        <> ".cell h3{font-size:1.25rem;font-weight:600;letter-spacing:-.01em;margin:.1rem 0;transition:color .15s}"
        <> ".cell.spotlight h3{font-size:1.7rem}"
        <> ".byline{font-family:var(--mono);font-size:.72rem;color:var(--c,var(--accent));margin:0}"
        <> ".desc{color:var(--dim);font-size:.93rem;margin:0;flex:1}.meta{color:var(--dim);font-family:var(--mono);font-size:.72rem;margin:0}"
        <> ".cardtags{display:flex;gap:.5rem;flex-wrap:wrap;margin:0}.tg{font-family:var(--mono);font-size:.7rem;color:var(--c,var(--accent))}"
        <> ".cfoot{display:flex;gap:.6rem;align-items:center;border-top:1px solid var(--border);padding-top:.7rem;margin-top:.2rem}"
        <> ".open{font-family:var(--mono);font-size:.74rem;color:var(--dim)}"
        <> ".act{font-family:var(--mono);font-size:.74rem;margin:0}"
        <> ".act{color:var(--dim);border:1px solid var(--border);border-radius:6px;padding:.25rem .6rem;text-decoration:none;background:none;cursor:pointer}"
        <> ".act:hover{color:var(--c,var(--accent));border-color:var(--c,var(--accent))}"
        <> "form.act{display:inline}form.act button{font:inherit;color:inherit;background:none;border:none;cursor:pointer;padding:0}"
        <> ".reader{width:100%;height:80vh;border:1px solid var(--border);border-radius:var(--radius);background:#fff}"
        <> ".reader-nav{display:flex;justify-content:space-between;align-items:center;font-family:var(--mono);padding:1rem clamp(1.2rem,5vw,3rem)}"
        <> ".cta{padding:2rem clamp(1.2rem,5vw,3rem);border-top:1px solid var(--border);background:var(--panel);color:var(--dim);font-size:.9rem}.cta a{color:var(--accent);font-family:var(--mono);font-size:.82rem}"
        <> "</style>"
