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
    "'},'*')}catch(e){}}requestAnimationFrame(function(){r();setTimeout(r,60)})})();</scr" +
    'ipt>';
  return STATIC_CSP + style + content + reporter;
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
});
function iframeContentStyle() {
  const isDark = currentTheme() === 'dark';
  const bg = isDark ? '#1e1e2e' : '#ffffff';
  const fg = isDark ? '#cdd6f4' : '#0a0a0a';
  const fgHead = isDark ? '#f5f5ff' : '#000000';
  const muted = isDark ? '#262637' : '#fafafa';
  const border = isDark ? '#313244' : '#e4e4e7';
  const accent = isDark ? '#89b4fa' : '#0066ff';
  const altRow = isDark ? 'rgba(137,180,250,0.05)' : 'rgba(0,102,255,0.04)';
  return `<style>body{background:${bg};color:${fg};font-family:'Geist',-apple-system,BlinkMacSystemFont,system-ui,sans-serif;font-size:15px;margin:0;padding:14px 18px;line-height:1.6;-webkit-font-smoothing:antialiased}img{max-width:100%}a{color:${accent}}table{border-collapse:separate;border-spacing:0;width:100%;border:1px solid ${border};border-radius:8px;overflow:hidden;margin:14px 0;font-size:14px}th,td{padding:10px 14px;text-align:left;border-bottom:1px solid ${border}}tr:last-child td{border-bottom:none}th{background:${muted};color:${fgHead};font-weight:600}tr:nth-child(even) td{background:${altRow}}input,select,button{font-family:inherit;font-size:14px;accent-color:${accent}}</style>`;
}
function widgetIframeStyle() {
  const isDark = currentTheme() === 'dark';
  const fg = isDark ? '#cdd6f4' : '#0a0a0a';
  const fgDim = isDark ? '#9399b2' : '#71717a';
  const accent = isDark ? '#89b4fa' : '#0066ff';
  return `<style>body{background:transparent;color:${fg};font-family:'JetBrains Mono',ui-monospace,monospace;font-size:14px;margin:0;padding:8px 14px;line-height:1.2;font-variant-ligatures:none;font-feature-settings:"calt" 0,"liga" 0}input,select{font-family:inherit;font-size:14px;accent-color:${accent};vertical-align:middle}button{font-family:inherit;font-size:14px;cursor:pointer;accent-color:${accent};vertical-align:middle}label{font-weight:600;font-size:13px;color:${fgDim};display:inline-block;margin-right:8px;vertical-align:middle}</style>`;
}
if (window.matchMedia) {
  window.matchMedia('(prefers-color-scheme: light)').addEventListener('change', (e) => {
    if (!window.__sabelaThemePinned) applyTheme(e.matches ? 'light' : 'dark', { persist: false });
  });
}
applyTheme(currentTheme(), { persist: false });
