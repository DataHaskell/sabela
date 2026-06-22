// ── MhsEngine: ONE persistent interactive MicroHs (mhsi) session ─────────
//
// MicroHs 0.16.5.0 (augustss's web-mhs) is a classic global-Module emscripten
// build. The old design re-instantiated it per run (`mhs -r <module>`), which
// re-loads/links the base library EVERY run (~2s). Profiling: instantiation is
// 49ms; the ~2s is base processing per run.
//
// Instead we boot ONE long-lived `mhsi` REPL (Module.arguments=[]) in a single
// kept-alive iframe. Base loads ONCE; then each input evaluates in ~90ms.
//
// The session runs in a child iframe whose origin may be OPAQUE: the gallery
// collection reader frames a share with `sandbox` and no `allow-same-origin`,
// so the parent CANNOT touch the child's `window` (`window.__so` / `Module`)
// directly. Every byte of output and every input line therefore crosses via
// `postMessage`, which is allowed cross-origin. The child mirrors stdout/stderr
// to the parent and accepts input lines back.
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

  // The session iframe's cumulative stdout/stderr, mirrored here over
  // postMessage because the child may be a distinct opaque origin (the reader's
  // sandbox), which makes a direct `frame.contentWindow.__so` read a
  // SecurityError. The child posts the FULL string on change, so these always
  // hold the latest cumulative output.
  let soBuf = '';
  let seBuf = '';
  let childReady = false;

  window.addEventListener('message', (e) => {
    if (!frame || e.source !== frame.contentWindow) return;
    const d = e.data;
    if (!d || typeof d !== 'object' || typeof d.sws !== 'string') return;
    if (d.sws === 'out') soBuf = d.v;
    else if (d.sws === 'err') seBuf = d.v;
    else if (d.sws === 'ready') {
      if (typeof d.so === 'string') soBuf = d.so;
      childReady = true;
    }
  });

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

  // The session document: boot mhsi interactive, stream stdout/stderr to the
  // parent via postMessage (it can't read our window cross-origin), accept input
  // lines the parent posts back, and announce readiness. No exit hooks.
  function sessionDoc() {
    return [
      '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body><script>',
      '(function(){',
      '  var so="",se="",soP=false,seP=false;',
      '  function flush(k,v){parent.postMessage({sws:k,v:v},"*");}',
      '  function so_(c){if(c!==null){so+=String.fromCharCode(c);' +
        'if(!soP){soP=true;setTimeout(function(){soP=false;flush("out",so);},0);}}}',
      '  function se_(c){if(c!==null){se+=String.fromCharCode(c);' +
        'if(!seP){seP=true;setTimeout(function(){seP=false;flush("err",se);},0);}}}',
      '  window.addEventListener("message",function(e){',
      '    if(e.source!==window.parent)return;var d=e.data;',
      '    if(!d||d.sws!=="input"||typeof d.s!=="string")return;',
      '    if(!(window.Module&&Module._set_input_char))return;',
      '    for(var i=0;i<d.s.length;i++)Module._set_input_char(d.s.charCodeAt(i));',
      '    Module._set_input_char(10);',
      '  });',
      '  window.Module={arguments:[],preRun:[function(){',
      '    Module.FS.init(function(){return null;},so_,se_);',
      '    Module.FS.chdir(' + JSON.stringify(MHS_EMBED.home) + ');',
      '  }],print:function(){},printErr:function(){}};',
      '  var rdy=setInterval(function(){',
      '    if(window.Module&&typeof Module._set_input_char==="function"&&so.indexOf("> ")>=0){',
      '      clearInterval(rdy);parent.postMessage({sws:"ready",so:so},"*");}',
      '  },30);',
      '  var s=document.createElement("script");s.src=' + JSON.stringify(MHS_EMBED.url) + ';',
      '  s.onerror=function(){se+="failed to load MicroHs embed\\n";flush("err",se);};',
      '  document.body.appendChild(s);',
      '})();',
      '<' + '/script></body></html>',
    ].join('\n');
  }

  function load() {
    if (bootPromise) return bootPromise;
    bootPromise = new Promise((resolve, reject) => {
      const f = ensureFrame();
      soBuf = '';
      seBuf = '';
      childReady = false;
      f.srcdoc = sessionDoc();
      const poll = setInterval(() => {
        if (childReady) {
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

  // One REPL input line to the session (the child appends the newline).
  function sendLine(s) {
    frame.contentWindow.postMessage({ sws: 'input', s: s }, '*');
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
      const id = ++feedId;
      const S = '@@SWS' + id + '@@';
      const E = '@@SWE' + id + '@@';
      const mark = soBuf.length;
      sendLine('putStrLn "' + S + '"');
      for (const l of lines) if (l.trim().length) sendLine(l);
      sendLine('putStrLn "' + E + '"');
      // The PRINTED sentinel is the token followed by a newline; the echo is the
      // token followed by a quote, so `E + "\n"` matches only the print.
      await waitFor(() => soBuf.indexOf(E + '\n', mark) >= 0, MHS_FEED_TIMEOUT_MS);
      const buf = soBuf;
      const sStart = buf.indexOf(S + '\n', mark);
      const from = sStart >= 0 ? sStart + (S + '\n').length : mark;
      const end = buf.indexOf(E + '\n', from);
      return parseRegion(buf.slice(from, end));
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
