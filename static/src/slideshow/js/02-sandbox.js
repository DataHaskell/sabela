// ── Static-export iframe: opaque-origin sandbox + in-document CSP ──
// In a shared/offline export we still run a script-drawn widget so its plot
// renders, but sandbox='allow-scripts' (NO allow-same-origin) gives the frame an
// opaque origin that cannot read this page (the embedded notebook JSON), cookies,
// or storage; the meta-CSP allows charting libs from trusted CDNs (cdnjs,
// jsdelivr) but keeps connect-src closed, so a plot can draw yet can't
// exfiltrate the page. 'unsafe-eval' is required because Vega compiles its
// dataflow expressions at runtime via the Function() constructor; it adds
// little risk here since 'unsafe-inline' already runs arbitrary script and the
// opaque origin + closed connect-src block any reads or exfiltration. Because
// the frame is cross-origin we cannot read its height, so it reports its own
// via postMessage.
const STATIC_CSP =
  "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; script-src 'unsafe-inline' 'unsafe-eval' https://cdnjs.cloudflare.com https://cdn.jsdelivr.net; style-src 'unsafe-inline'; img-src data:; base-uri 'none'; form-action 'none'; frame-src 'none'\">";
let __staticFrameSeq = 0;
function staticSrcdoc(content, kind, id) {
  const style = kind === 'widget' ? widgetIframeStyle() : iframeContentStyle();
  const reporter =
    '<scr' +
    "ipt>(function(){function r(){try{parent.postMessage({__sabelaH:document.documentElement.scrollHeight,__sabelaId:'" +
    id +
    "'},'*')}catch(e){}}window.__sabelaReportHeight=r;requestAnimationFrame(function(){r();setTimeout(r,60)})})();</scr" +
    'ipt>';
  return STATIC_CSP + style + vegaPreamble(content) + content + reporter;
}
// A chart in a static export loads Vega from the page's own copy, written in
// by the exporter, rather than from the network.
function vegaPreamble(content) {
  if (!content.includes('class="sabela-viz"') || typeof window.__SABELA_VEGA__ !== 'string')
    return '';
  return '<scr' + 'ipt>' + window.__SABELA_VEGA__ + '</scr' + 'ipt>';
}
function mountStaticFrame(container, content, kind) {
  const id = 'sframe' + __staticFrameSeq++;
  const iframe = document.createElement('iframe');
  iframe.id = id;
  iframe.setAttribute('sandbox', 'allow-scripts');
  if (kind === 'widget') iframe.style.pointerEvents = 'none';
  iframe.style.height = (kind === 'widget' ? 60 : 80) + 'px';
  iframe.dataset.iframeKind = kind;
  iframe.dataset.lastContent = content;
  iframe.dataset.staticFrame = '1';
  iframe.srcdoc = staticSrcdoc(content, kind, id);
  container.appendChild(iframe);
}
window.addEventListener('message', (e) => {
  const d = e.data;
  if (d && typeof d.__sabelaH === 'number' && d.__sabelaId) {
    const el = document.getElementById(d.__sabelaId);
    if (el && el.dataset.staticFrame) el.style.height = Math.max(32, d.__sabelaH) + 'px';
  }
  if (d && d.type === 'sabelaResize' && typeof d.height === 'number') {
    document.querySelectorAll('iframe').forEach((f) => {
      if (f.contentWindow === e.source)
        f.style.height = Math.max(60, Math.ceil(d.height) + 8) + 'px';
    });
  }
});
function iframeContentStyle() {
  const isDark = currentTheme() === 'dark';
  const bg = isDark ? '#2e2e32' : '#fbfbfb';
  const fg = isDark ? '#e1e1e6' : '#181818';
  const fgHead = isDark ? '#e1e1e6' : '#181818';
  const muted = isDark ? '#252529' : '#f2f2f2';
  const border = isDark ? '#202024' : '#e8e8e8';
  const accent = isDark ? '#93ddfa' : '#fc1785';
  const altRow = muted;
  return `<style>:root{${gridPaletteVars(isDark, bg, muted, fg, border, accent)}}body{background:${bg};color:${fg};font-family:'Fira Sans',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;font-size:15px;margin:0;padding:14px 18px;line-height:1.55}img{max-width:100%}a{color:${accent}}table{border-collapse:collapse;width:100%;border:1px solid ${border};margin:14px 0;font-size:14px}th,td{padding:10px 14px;text-align:left;border-bottom:1px solid ${border}}tr:last-child td{border-bottom:none}th{background:${muted};color:${fgHead};font-weight:500}tr:nth-child(even) td{background:${altRow}}input,select,button{font-family:inherit;font-size:14px;accent-color:${accent}}</style>`;
}
// The palette the embedded grid runtime reads (--sabela-*), so a table in a
// slide carries the deck's theme rather than one derived from its background.
function gridPaletteVars(isDark, surface, raised, fg, border, accent) {
  return [
    `--sabela-surface:${surface}`,
    `--sabela-raised:${raised}`,
    `--sabela-fg:${fg}`,
    `--sabela-dim:${isDark ? '#97979c' : '#808080'}`,
    `--sabela-border:${border}`,
    `--sabela-accent:${accent}`,
    `--sabela-accent-rgb:${isDark ? '147,221,250' : '252,23,133'}`,
  ].join(';');
}

function widgetIframeStyle() {
  const isDark = currentTheme() === 'dark';
  const fg = isDark ? '#e1e1e6' : '#181818';
  const fgDim = isDark ? '#97979c' : '#808080';
  const accent = isDark ? '#93ddfa' : '#fc1785';
  return `<style>body{background:transparent;color:${fg};font-family:'JetBrains Mono',ui-monospace,monospace;font-size:14px;margin:0;padding:8px 14px;line-height:1.2;font-variant-ligatures:none;font-feature-settings:"calt" 0,"liga" 0}input,select{font-family:inherit;font-size:14px;accent-color:${accent};vertical-align:middle}button{font-family:inherit;font-size:14px;cursor:pointer;accent-color:${accent};vertical-align:middle}label{font-weight:500;font-size:13px;color:${fgDim};display:inline-block;margin-right:8px;vertical-align:middle}</style>`;
}
if (window.matchMedia) {
  window.matchMedia('(prefers-color-scheme: light)').addEventListener('change', (e) => {
    if (!window.__sabelaThemePinned) applyTheme(e.matches ? 'light' : 'dark', { persist: false });
  });
}
applyTheme(currentTheme(), { persist: false });
