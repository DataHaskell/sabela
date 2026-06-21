// ── Shims for the shared MIME renderer (mime-render.js) ───────────────────
//
// The shared renderer calls a few globals that, in the editor, live in other
// partials (19-theme.js, 17-render-prose.js). The export page does not load
// those, so we provide minimal stand-ins with the same names. `_activeTextInput`
// is editor-only state; here it stays a permanently-null global so the
// renderer's focus-restore branch is simply skipped.
var _activeTextInput = null;

// Base style injected into every text/html output iframe — mirrors the editor's
// iframeBaseStyle so HTML/SVG widgets look the same in a share. Reads theme
// CSS vars when present, with editor-matching fallbacks.
function iframeBaseStyle() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => (cs.getPropertyValue(n) || d).trim();
  const bg = v('--output-html-bg', '#ffffff');
  const fg = v('--output-html-fg', '#1e1e2e');
  const accent = v('--accent', '#89b4fa');
  return (
    `<style>body{background:${bg};color:${fg};` +
    `font-family:'JetBrains Mono',ui-monospace,monospace;font-size:13px;` +
    `margin:0;padding:4px 8px;font-variant-ligatures:none;` +
    `font-feature-settings:"calt" 0,"liga" 0}` +
    `input,select,button{accent-color:${accent};font-family:inherit;` +
    `font-size:13px;cursor:pointer}</style>`
  );
}

// KaTeX-before-marked pass for text/markdown output. The export page loads
// marked + katex from CDN; if katex is absent this is a safe no-op that returns
// the source unchanged for marked to handle.
function renderMathInMarkdown(src) {
  let s = src || '';
  if (typeof katex === 'undefined') return s;
  s = s.replace(/\$\$([\s\S]+?)\$\$/g, (m, tex) => {
    try {
      return katex.renderToString(tex.trim(), { displayMode: true, throwOnError: false });
    } catch (_e) {
      return m;
    }
  });
  s = s.replace(/(^|[^\\$])\$([^\$\n]+?)\$(?!\d)/g, (m, pre, tex) => {
    try {
      return pre + katex.renderToString(tex, { displayMode: false, throwOnError: false });
    } catch (_e) {
      return m;
    }
  });
  return s;
}
