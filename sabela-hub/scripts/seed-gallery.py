#!/usr/bin/env python3
"""Seed a local demo gallery from repo example notebooks.

Writes the share records (index.html + meta + source.md) and the gallery
curation files (index / attribution / tags) that Hub.Share and
Hub.Gallery.Store read on startup, so a fresh hub pointed at the demo data
root shows a curated, attributed, featured gallery with no admin clicks.

Usage:  python3 scripts/seed-gallery.py [DATA_ROOT]   (default: ./demo-data)
Then:   HUB_SHARES_DIR=$ROOT/shares HUB_GALLERY_DIR=$ROOT/gallery \\
          HUB_USERS_DIR=$ROOT/users HUB_BACKEND=docker cabal run sabela-hub
"""

import html
import os
import re
import sys

REPO = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", ".."))

# slug must be lowercase hex (Hub.Share.validSlug / isLowerHex).
CURATION = [
    {
        "slug": "c56a0001",
        "file": "examples/CSG.md",
        "title": "What is Constructive Solid Geometry?",
        "author": "Joe Warren",
        "tags": ["geometry", "graphics", "3d"],
        # The notebook's saved model-viewer outputs reference /api/asset (a
        # live-editor endpoint the static gallery can't serve), so a dashboard
        # export renders empty 3D viewers and drops the code. Render the markdown
        # directly (code blocks shown) and repoint the viewers at the author's
        # committed public GLBs so the diagrams appear. Models are remapped
        # positionally to this list, in document order.
        "assets": {
            "base": "https://raw.githubusercontent.com/joe-warren/sabela/0b87c7bc183323cc38db5e5a2f20a500473a425a/waterfall/",
            # Document order (the committed GLBs are named per a different run, so
            # the list is ordered by shape, identified from each model's geometry):
            # cylinder, scaled, rotated, cross, 3D-cross, cube, sphere,
            # rounded-cube, final shape.
            "models": [
                "563306", "271686", "795000", "796533", "492778",
                "747853", "396714", "469117", "559254",
            ],
        },
    },
    {
        "slug": "b1ef0001",
        "file": "examples/bluefin.md",
        "title": "A tour of Bluefin",
        "author": "Tom Ellis",
        "tags": ["effects", "tutorial"],
        "dashboard": "scripts/dashboards/bluefin.html",
    },
    {
        "slug": "ca1f0001",
        "file": "examples/CaliforniaHousing.md",
        "title": "California Housing: From Exploration to Linear Regression",
        "author": "DataHaskell",
        "tags": ["regression", "dataframe", "hasktorch"],
        # Rich snapshot: the editor's /api/export/dashboard output (what the hub
        # publish flow stores), so every output renders faithfully. Regenerate
        # with: curl -s localhost:3000/api/export/dashboard > scripts/dashboards/california.html
        "dashboard": "scripts/dashboards/california.html",
    },
    {
        "slug": "f12a0001",
        "file": "examples/frp-tutorial.md",
        "title": "Functional Reactive Programming in Sabela",
        "author": "DataHaskell",
        "tags": ["frp", "animation", "tutorial"],
        # Notebook-mode export (code + the self-contained requestAnimationFrame
        # players), generated from a run of examples/frp-tutorial.md.
        "dashboard": "scripts/dashboards/frp.html",
    },
    {
        "slug": "c0de0001",
        "file": "examples/tutorial-python-integration.md",
        "title": "Haskell and Python in one notebook",
        "author": "DataHaskell",
        "tags": ["python", "interop", "matplotlib"],
        # Notebook-mode export (code + the matplotlib PNG outputs), generated
        # from a run of examples/tutorial-python-integration.md.
        "dashboard": "scripts/dashboards/python.html",
    },
]

OWNER = "curators@sabela.dev"
CREATED_AT = "2026-06-12T00:00:00Z"


def inline(text):
    """Render inline markdown (escape, then images, links, bold, code)."""
    s = html.escape(text, quote=False)
    s = re.sub(
        r"!\[([^\]]*)\]\(([^)]+)\)",
        r'<img alt="\1" src="\2">',
        s,
    )
    s = re.sub(r"\[([^\]]+)\]\(([^)]+)\)", r'<a href="\2">\1</a>', s)
    s = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", s)
    s = re.sub(r"`([^`]+)`", r"<code>\1</code>", s)
    return s


def render_body(md):
    """Render a notebook's markdown to the gallery's notebook-cell HTML."""
    lines = md.split("\n")
    out, i = [], 0
    while i < len(lines):
        line = lines[i]
        if line.strip() == "" or re.match(r"^<!--\s*sabela:cell\s*-->", line):
            i += 1
            continue
        if line.startswith("```"):
            lang = line[3:].strip().lower()
            i += 1
            buf = []
            while i < len(lines) and not lines[i].startswith("```"):
                buf.append(lines[i])
                i += 1
            i += 1  # closing fence
            code = html.escape("\n".join(buf), quote=False)
            cls = "language-haskell" if lang == "haskell" else "nohighlight"
            out.append(f'<pre class="code"><code class="{cls}">{code}</code></pre>')
            continue
        if line.startswith(">"):
            buf = []
            while i < len(lines) and lines[i].startswith(">"):
                buf.append(re.sub(r"^>\s?", "", lines[i]))
                i += 1
            dequoted = "\n".join(buf)
            first = next((b for b in buf if b.strip()), "")
            if "scripths:mime text/html" in first:
                rich = "\n".join(
                    b for b in buf if not b.strip().startswith("<!--")
                )
                out.append(f'<div class="out">{rich}</div>')
            else:
                out.append(
                    '<pre class="out">'
                    + html.escape(dequoted, quote=False)
                    + "</pre>"
                )
            continue
        m = re.match(r"^(#{1,6})\s+(.*)$", line)
        if m:
            lvl = len(m.group(1))
            # the page header already shows the notebook's H1 title.
            if lvl > 1:
                out.append(f"<h{lvl}>{inline(m.group(2))}</h{lvl}>")
            i += 1
            continue
        para = []
        while (
            i < len(lines)
            and lines[i].strip()
            and not lines[i].startswith(("```", ">", "#"))
            and not re.match(r"^<!--\s*sabela:cell\s*-->", lines[i])
        ):
            para.append(lines[i])
            i += 1
        out.append(f'<p>{inline(" ".join(para))}</p>')
    return "\n".join(out)


def page(title, author, slug, body):
    """A self-contained notebook page in the gallery's Warm Paper palette."""
    return f"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{html.escape(title)}</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-light.min.css">
<style>
@import url('https://fonts.googleapis.com/css2?family=Geist:wght@400;500;600&family=Fraunces:opsz,wght@9..144,500;9..144,600&family=JetBrains+Mono:wght@400;500&display=swap');
:root{{--bg:#f7f3ec;--cell:#fff;--panel:#f0eadf;--fg:#2e3440;--dim:#6f6a5d;--accent:#c2674a;--border:#e7ddcf;--mono:'JetBrains Mono',ui-monospace,monospace}}
*{{box-sizing:border-box}}
body{{margin:0;background:radial-gradient(60% 44% at 86% -10%,rgba(208,135,111,.18),transparent 72%),var(--bg);color:var(--fg);font-family:Geist,system-ui,sans-serif;line-height:1.6}}
a{{color:var(--accent)}}
.top{{display:flex;align-items:center;justify-content:space-between;padding:0 clamp(1.2rem,5vw,3rem);height:58px;border-bottom:1px solid var(--border);background:var(--panel);position:sticky;top:0}}
.brand{{font-weight:600;font-size:1.1rem;text-decoration:none;color:var(--fg)}}.brand .lam{{color:var(--accent);font-family:var(--mono)}}
.acts{{display:flex;gap:.7rem;align-items:center;font-family:var(--mono);font-size:.8rem}}
.acts .gal{{color:var(--dim)}}.acts .gal:hover{{color:var(--fg)}}
.acts .dl{{color:var(--dim);border:1px solid var(--border);border-radius:6px;padding:.3rem .7rem;background:var(--cell);text-decoration:none}}
.acts .dl:hover{{color:var(--fg);border-color:var(--dim)}}
.acts form{{margin:0}}
.acts .fork{{color:#fff;background:var(--accent);border:none;border-radius:6px;padding:.38rem .9rem;font:inherit;font-weight:600;cursor:pointer}}
.acts .fork:hover{{opacity:.9}}
main{{max-width:46rem;margin:0 auto;padding:clamp(1.6rem,4vw,3rem) clamp(1.2rem,5vw,2rem) 5rem}}
h1,h2,h3{{font-family:Fraunces,Georgia,serif;letter-spacing:-.01em;line-height:1.2}}
h1{{font-size:clamp(1.9rem,4vw,2.6rem);margin:.2rem 0 .3rem}}
h2{{font-size:1.5rem;margin:2.2rem 0 .6rem}}
.byline{{font-family:var(--mono);font-size:.8rem;color:var(--accent);margin:0 0 2rem}}
p{{margin:.9rem 0}}
code{{font-family:var(--mono);font-size:.86em;background:var(--panel);padding:.08em .35em;border-radius:4px}}
pre.code{{background:#fff;border:1px solid var(--border);border-left:3px solid var(--accent);border-radius:8px;padding:1rem 1.2rem;overflow:auto;box-shadow:0 4px 14px rgba(70,52,34,.06)}}
pre.code code{{background:none;padding:0;font-size:.82rem;line-height:1.5}}
pre.code code.hljs{{background:transparent;padding:0}}
.out{{border-left:3px solid #6f9355;background:#fff;border-radius:0 8px 8px 0;padding:.8rem 1.1rem;margin:.4rem 0 1.4rem}}
pre.out{{font-family:var(--mono);font-size:.8rem;color:var(--dim);white-space:pre-wrap}}
img{{max-width:100%;border-radius:8px}}
</style></head><body>
<header class="top">
<a class="brand" href="/gallery"><span class="lam">&lambda;</span> Sabela</a>
<nav class="acts">
<a class="gal" href="/gallery">Gallery</a>
<a class="dl" href="/_hub/source/{slug}">Download</a>
<form method="post" action="/_hub/fork/{slug}"><button class="fork" type="submit">Fork &#9656;</button></form>
</nav>
</header>
<main>
<h1>{html.escape(title)}</h1>
<p class="byline">by {html.escape(author)}</p>
{body}
</main>
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js"></script>
<script>hljs.highlightAll();</script>
</body></html>"""


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text)


def rewrite_assets(md, cfg):
    """Repoint a notebook's /api/asset output references (a live-editor
    endpoint the static gallery can't serve) at public committed assets, so
    the inlined dashboard outputs render. Model viewers are remapped
    positionally to cfg['models']; other waterfall assets keep their name."""
    base, models = cfg["base"], cfg.get("models", [])
    counter = {"i": 0}

    def repl_model(_m):
        i = counter["i"]
        counter["i"] += 1
        name = models[i] if i < len(models) else _m.group(0)
        return base + "models/" + name + ".glb"

    md = re.sub(r"/api/asset\?path=[^\s\"'<>]*?models/\d+\.glb", repl_model, md)
    return re.sub(r"/api/asset\?path=(?:examples/data/)?waterfall/", base, md)


def brand_dashboard(html):
    """Inject the warm-lambda brand mark (matching the hub front page) into a
    pre-built dashboard export's header. Idempotent: dashboards exported from a
    template that already carries the brand are left untouched."""
    if "db-brand" in html:
        return html
    mark = '<span class="db-brand"><span class="lam">λ</span> Sabela</span>'
    html = html.replace(
        '<h1 id="dashboard-title"', mark + '<h1 id="dashboard-title"', 1
    )
    css = (
        "<style>.db-brand{font-family:var(--font-mono);font-weight:600;"
        "font-size:15px;color:var(--fg-heading);white-space:nowrap}"
        ".db-brand .lam{color:#c2674a}</style>"
    )
    return html.replace("</head>", css + "</head>", 1)


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else os.path.join(REPO, "sabela-hub", "demo-data")
    shares = os.path.join(root, "shares")
    gallery = os.path.join(root, "gallery")
    os.makedirs(os.path.join(gallery, "collections"), exist_ok=True)

    index_lines, attr_lines, tag_lines = [], [], []
    for c in CURATION:
        src_path = os.path.join(REPO, c["file"])
        with open(src_path) as f:
            md = f.read()
        sdir = os.path.join(shares, c["slug"])
        write(os.path.join(sdir, "source.md"), md)
        if "dashboard" in c:
            with open(os.path.join(REPO, "sabela-hub", c["dashboard"])) as f:
                index_html = brand_dashboard(f.read())
        else:
            rendered = rewrite_assets(md, c["assets"]) if "assets" in c else md
            index_html = page(c["title"], c["author"], c["slug"], render_body(rendered))
        write(os.path.join(sdir, "index.html"), index_html)
        write(
            os.path.join(sdir, "meta"),
            f"owner={OWNER}\nmode=dashboard\ncreatedAt={CREATED_AT}\ntitle={c['title']}\n",
        )
        index_lines.append(f"share={c['slug']}")
        attr_lines.append(f"{c['slug']}={c['author']}")
        tag_lines.append(f"{c['slug']}={','.join(c['tags'])}")
        print(f"seeded {c['slug']}  {c['title']}  (by {c['author']})")

    write(os.path.join(gallery, "index"), "\n".join(index_lines) + "\n")
    write(os.path.join(gallery, "attribution"), "\n".join(attr_lines) + "\n")
    write(os.path.join(gallery, "tags"), "\n".join(tag_lines) + "\n")
    print(f"\ngallery seeded at {root}")


if __name__ == "__main__":
    main()
