{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery.Style (styleBlock) where

import Data.Text (Text)

{- | The public gallery's stylesheet: the editor's Summer scheme, flat and
square, Fira Sans for prose and JetBrains Mono for labels.
-}
styleBlock :: Text
styleBlock =
    "<style>"
        <> "@import url('https://fonts.googleapis.com/css2?family=Fira+Sans:wght@400;500&family=JetBrains+Mono:wght@400;500&display=swap');"
        <> ":root{--bg:#fbfbfb;--cell:#fbfbfb;--panel:#f2f2f2;--fg:#181818;--dim:#808080;--accent:#fc1785;--mauve:#fb6620;--blue:#1586d2;--border:#e8e8e8;--green:#22a21f;--radius:0;--mono:'JetBrains Mono',ui-monospace,monospace;--sans:'Fira Sans',-apple-system,BlinkMacSystemFont,system-ui,sans-serif}"
        <> "*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--fg);font-family:var(--sans);line-height:1.5}"
        <> "a{color:inherit;text-decoration:none}"
        <> ".top{display:flex;align-items:center;justify-content:space-between;padding:0 clamp(1.2rem,5vw,3rem);height:34px;border-bottom:1px solid var(--border);background:var(--panel);position:sticky;top:0}"
        <> ".brand{font-weight:500;font-size:.95rem}"
        <> ".hnav{display:flex;gap:1.2rem;align-items:center;font-size:.85rem;color:var(--dim)}.hnav a:hover{color:var(--fg)}"
        <> ".signin{color:var(--fg);border:1px solid var(--fg);padding:.15rem .7rem}.signin:hover{background:var(--fg);color:var(--bg)}"
        <> ".masthead{padding:clamp(2rem,5vw,3.2rem) clamp(1.2rem,5vw,3rem) 1rem;max-width:48rem}"
        <> ".kick{font-family:var(--mono);font-size:.72rem;color:var(--accent)}"
        <> ".masthead h1{font-size:clamp(1.6rem,3vw,2.1rem);font-weight:500;margin:.5rem 0 .5rem;color:var(--fg)}"
        <> ".masthead p{color:var(--dim);font-size:1rem}"
        <> ".chips{display:flex;flex-wrap:wrap;gap:.4rem;padding:.4rem clamp(1.2rem,5vw,3rem) 1.4rem}"
        <> ".chip{font-family:var(--mono);font-size:.74rem;color:var(--dim);border:1px solid var(--border);padding:.15rem .6rem;background:var(--cell)}"
        <> ".chip:hover{color:var(--fg);border-color:var(--dim)}.chip.on{background:var(--fg);color:var(--bg);border-color:var(--fg)}"
        <> ".feed{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:1rem;padding:0 clamp(1.2rem,5vw,3rem) 3rem}"
        <> ".cell{display:flex;flex-direction:column;gap:.5rem;background:var(--cell);border:1px solid var(--border);border-left:2px solid var(--c,var(--accent));padding:1rem 1.2rem 1.1rem}"
        <> ".cell:nth-child(6n+1){--c:#fc1785}.cell:nth-child(6n+2){--c:#fb6620}.cell:nth-child(6n+3){--c:#22a21f}.cell:nth-child(6n+4){--c:#d9262a}.cell:nth-child(6n+5){--c:#1586d2}.cell:nth-child(6n+6){--c:#808080}"
        <> ".cell:hover{border-color:var(--dim);border-left-color:var(--c,var(--accent))}"
        <> ".cell.spotlight{grid-column:1/-1;background:var(--panel)}"
        <> ".crow{display:flex;justify-content:space-between;font-family:var(--mono);font-size:.68rem}"
        <> ".num{color:var(--dim)}.kind{color:var(--c,var(--accent))}"
        <> ".ctitle{text-decoration:none;color:inherit}.ctitle:hover h3{color:var(--c,var(--accent))}"
        <> ".cell h3{font-size:1.15rem;font-weight:500;margin:.1rem 0}"
        <> ".cell.spotlight h3{font-size:1.5rem}"
        <> ".byline{font-family:var(--mono);font-size:.72rem;color:var(--c,var(--accent));margin:0}"
        <> ".desc{color:var(--dim);font-size:.92rem;margin:0;flex:1}.meta{color:var(--dim);font-family:var(--mono);font-size:.72rem;margin:0}"
        <> ".cardtags{display:flex;gap:.5rem;flex-wrap:wrap;margin:0}.tg{font-family:var(--mono);font-size:.7rem;color:var(--c,var(--accent))}"
        <> ".cfoot{display:flex;gap:.6rem;align-items:center;border-top:1px solid var(--border);padding-top:.7rem;margin-top:.2rem}"
        <> ".open{font-family:var(--mono);font-size:.74rem;color:var(--dim)}"
        <> ".act{font-family:var(--mono);font-size:.74rem;margin:0}"
        <> ".act{color:var(--dim);border:1px solid var(--border);padding:.2rem .6rem;text-decoration:none;background:none;cursor:pointer}"
        <> ".act:hover{color:var(--c,var(--accent));border-color:var(--c,var(--accent))}"
        <> "form.act{display:inline}form.act button{font:inherit;color:inherit;background:none;border:none;cursor:pointer;padding:0}"
        <> ".reader{width:100%;height:80vh;border:1px solid var(--border);background:var(--cell)}"
        <> ".reader-nav{display:flex;justify-content:space-between;align-items:center;font-family:var(--mono);font-size:.8rem;padding:1rem clamp(1.2rem,5vw,3rem)}"
        <> ".cta{padding:1.6rem clamp(1.2rem,5vw,3rem);border-top:1px solid var(--border);background:var(--panel);color:var(--dim);font-size:.9rem}.cta a{color:var(--accent);font-family:var(--mono);font-size:.82rem}"
        <> "</style>"
