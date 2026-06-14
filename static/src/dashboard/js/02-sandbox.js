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
// Initial sync (renders iframes with the active theme).
applyTheme(currentTheme(), { persist: false });
