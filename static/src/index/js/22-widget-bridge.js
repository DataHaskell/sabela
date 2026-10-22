// ── Widget postMessage bridge ─────────────────────────────────────
const _widgetDebounceTimers = new Map();
let _activeTextInput = null;
window.addEventListener('message', (e) => {
  const d = e.data;
  if (d && d.type === 'sabelaGrid' && d.themed) {
    markThemedOutput(e.source);
    return;
  }
  if (d && d.type === 'frameQuery') {
    relayFrameQuery(e.source, d);
    return;
  }
  if (d && d.type === 'sabelaResize') {
    resizeOutputFrame(e.source, d.height);
    return;
  }
  if (d && d.type === 'sabelaViz') {
    if (typeof noteVizSpec === 'function') noteVizSpec(d);
    return;
  }
  if (d && d.type === 'widget') {
    if (d.sel !== undefined) _activeTextInput = { cellId: String(d.cellId), sel: d.sel };
    const key = d.cellId + ':' + d.name;
    clearTimeout(_widgetDebounceTimers.get(key));
    _widgetDebounceTimers.set(
      key,
      setTimeout(() => {
        _widgetDebounceTimers.delete(key);
        fetch('/api/widget', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ wuCellId: d.cellId, wuName: d.name, wuValue: d.value }),
        });
      }, 300)
    );
  }
});

// A grid that adopted the notebook palette paints its own surface, so the card
// behind it would show as a ring. Matched by window, since a sandboxed frame
// gives no other handle on which output spoke.
function markThemedOutput(source) {
  document.querySelectorAll('.cell-output.mime-html iframe').forEach((f) => {
    if (f.contentWindow === source) f.parentElement.classList.add('themed');
  });
}

// A chart that drew after its frame was sized asks for the room it needs.
function resizeOutputFrame(source, height) {
  if (typeof height !== 'number') return;
  document.querySelectorAll('.cell-output.mime-html iframe').forEach((f) => {
    if (f.contentWindow === source) f.style.height = Math.max(60, Math.ceil(height) + 8) + 'px';
  });
}

// A table's expression box asks the kernel to re-shape its frame; the reply
// goes back to the frame that asked, matched by the id it chose.
function relayFrameQuery(source, d) {
  fetch('/api/frame/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      fqSlot: d.slot,
      fqExpr: d.expr,
      fqQuery: d.query || null,
      fqCols: d.cols || null,
      fqTypes: d.types || null,
    }),
  })
    .then(async (r) => (r.ok ? r.json() : { fqOutputs: [], fqError: await r.text() }))
    .catch((err) => ({ fqOutputs: [], fqError: String(err) }))
    .then((res) => {
      try {
        source.postMessage(
          { type: 'frameResult', id: d.id, outputs: res.fqOutputs, error: res.fqError },
          '*'
        );
      } catch (_e) {
        /* the output was re-rendered meanwhile */
      }
    });
}
