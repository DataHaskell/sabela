// ── MhsEngine: ONE persistent interactive MicroHs (mhsi) session ─────────
//
// MicroHs 0.16.5.0 (augustss's web-mhs) is a classic global-Module emscripten
// build. The old design re-instantiated it per run (`mhs -r <module>`), which
// re-loads/links the base library EVERY run (~2s). Profiling: instantiation is
// 49ms; the ~2s is base processing per run.
//
// Instead we boot ONE long-lived `mhsi` REPL (Module.arguments=[]) in a single
// kept-alive iframe. Base loads ONCE; then each input evaluates in ~90ms. We feed
// input via Module._set_input_char and read output from the FS.init stdout
// callback (the iframe is same-origin, so the parent reads window.__so directly).
//
//     MhsEngine.load()           -> Promise<void>            (boot/warm; memoised)
//     MhsEngine.available        -> boolean                  (session ready)
//     MhsEngine.feed(lines)      -> Promise<{ out, err }>    (run REPL inputs)
//     MhsEngine.reset()          -> Promise<void>            (:clear all defs)
//
// feed() NEVER throws for ordinary Haskell errors — they come back in `err`
// (mhsi prints `*** Exception …` and keeps the session alive). It rejects only if
// the engine never boots / a feed times out (caller shows a Fork nudge).

const MHS_BOOT_TIMEOUT_MS = 60000; // cold start: download + instantiate + base
const MHS_FEED_TIMEOUT_MS = 60000; // a single feed (covers the first-eval warmup)

const MhsEngine = (function () {
  let frame = null;
  let bootPromise = null;
  let ready = false;
  let feedId = 0;
  let chain = Promise.resolve(); // serialise feeds so they never interleave

  // Rendered-but-invisible frame. It MUST stay schedulable (real area, near-zero
  // opacity, top z-index) or Chrome throttles the idle session and feeds stall.
  function ensureFrame() {
    if (frame && frame.isConnected) return frame;
    frame = document.createElement('iframe');
    frame.style.cssText =
      'position:fixed;right:0;bottom:0;width:200px;height:120px;opacity:0.01;' +
      'border:0;pointer-events:none;z-index:2147483647';
    document.body.appendChild(frame);
    return frame;
  }

  // The session document: boot mhsi interactive, mirror stdout/stderr into
  // window.__so/__se (the parent reads them same-origin), no exit hooks.
  function sessionDoc() {
    return [
      '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body><script>',
      '(function(){',
      '  window.__so="";window.__se="";',
      '  function so(c){if(c!==null)window.__so+=String.fromCharCode(c);}',
      '  function se(c){if(c!==null)window.__se+=String.fromCharCode(c);}',
      '  window.Module={arguments:[],preRun:[function(){',
      '    Module.FS.init(function(){return null;},so,se);',
      '    Module.FS.chdir(' + JSON.stringify(MHS_EMBED.home) + ');',
      '  }],print:function(){},printErr:function(){}};',
      '  var s=document.createElement("script");s.src=' + JSON.stringify(MHS_EMBED.url) + ';',
      '  s.onerror=function(){window.__se+="failed to load MicroHs embed\\n";};',
      '  document.body.appendChild(s);',
      '})();',
      '<' + '/script></body></html>',
    ].join('\n');
  }

  function load() {
    if (bootPromise) return bootPromise;
    bootPromise = new Promise((resolve, reject) => {
      const f = ensureFrame();
      f.srcdoc = sessionDoc();
      const poll = setInterval(() => {
        const w = f.contentWindow;
        // ready when the prompt has printed and the input hook exists
        if (
          w &&
          w.__so &&
          w.__so.indexOf('> ') >= 0 &&
          w.Module &&
          typeof w.Module._set_input_char === 'function'
        ) {
          clearInterval(poll);
          clearTimeout(to);
          ready = true;
          resolve();
        }
      }, 30);
      const to = setTimeout(() => {
        clearInterval(poll);
        reject(new Error('MicroHs session boot timed out'));
      }, MHS_BOOT_TIMEOUT_MS);
    });
    return bootPromise;
  }

  function sendLine(w, s) {
    for (let i = 0; i < s.length; i++) w.Module._set_input_char(s.charCodeAt(i));
    w.Module._set_input_char(10);
  }

  function waitFor(pred, ms) {
    return new Promise((resolve, reject) => {
      const poll = setInterval(() => {
        if (pred()) {
          clearInterval(poll);
          clearTimeout(to);
          resolve();
        }
      }, 15);
      const to = setTimeout(() => {
        clearInterval(poll);
        reject(new Error('MicroHs feed timed out'));
      }, ms);
    });
  }

  // Strip the echoed input (lines after a `> ` prompt) and blanks; route mhsi
  // error lines to `err`, everything else to `out`.
  function parseRegion(region) {
    const out = [];
    const err = [];
    for (const line of region.split('\n')) {
      if (line.startsWith('> ') || line.length === 0) continue;
      if (/\*\*\* Exception|(^|\s)error:|no location:/.test(line)) err.push(line);
      else out.push(line);
    }
    return { out: out.join('\n'), err: err.join('\n') };
  }

  // Feed REPL input lines, bracketed by printed sentinels so the captured region
  // excludes prompts/echoes cleanly. Resolves with the cell's output.
  function feedNow(lines) {
    return (async () => {
      await load();
      const w = frame.contentWindow;
      const id = ++feedId;
      const S = '@@SWS' + id + '@@';
      const E = '@@SWE' + id + '@@';
      const mark = w.__so.length;
      sendLine(w, 'putStrLn "' + S + '"');
      for (const l of lines) if (l.trim().length) sendLine(w, l);
      sendLine(w, 'putStrLn "' + E + '"');
      // The PRINTED sentinel is the token followed by a newline; the echo is the
      // token followed by a quote, so `E + "\n"` matches only the print.
      await waitFor(() => w.__so.indexOf(E + '\n', mark) >= 0, MHS_FEED_TIMEOUT_MS);
      const buf = w.__so;
      const sStart = buf.indexOf(S + '\n', mark);
      const from = sStart >= 0 ? sStart + (S + '\n').length : mark;
      const to = buf.indexOf(E + '\n', from);
      return parseRegion(buf.slice(from, to));
    })();
  }

  function feed(lines) {
    chain = chain.then(
      () => feedNow(lines),
      () => feedNow(lines)
    );
    return chain;
  }

  async function reset() {
    await load();
    return feed([':clear']);
  }

  return {
    load,
    feed,
    reset,
    get available() {
      return ready;
    },
  };
})();
