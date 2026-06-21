// ── Fork nudge: route unsupported cells to the full toolchain ────────────

// Does this cell need full Sabela (a Hackage import MicroHs can't satisfy)?
// Checked up front, before compiling, on the cell's own source.
function cellNeedsFork(code) {
  if (CABAL_META.test(code)) return true;
  return UNSUPPORTED_IMPORTS.some((re) => re.test(code));
}

// Does this compiler stderr look like an unsupported-feature/missing-package
// miss (vs an ordinary type error the author can fix in-browser)?
function stderrNeedsFork(stderr) {
  if (!stderr) return false;
  return UNSUPPORTED_STDERR.some((re) => re.test(stderr));
}

// A per-cell "needs full Sabela" prompt: a POST form to /_hub/fork/<slug>. The
// collection reader iframe already grants allow-forms, so the submit works
// inside it. Opens in a new tab to match the share banner's fork button.
function forkNudge(slug) {
  const wrap = document.createElement('div');
  wrap.className = 'swasm-fork';
  const msg = document.createElement('span');
  msg.className = 'swasm-fork-msg';
  msg.textContent = 'This needs full Sabela — ';
  const form = document.createElement('form');
  form.method = 'post';
  form.action = '/_hub/fork/' + encodeURIComponent(slug);
  form.target = '_blank';
  form.style.display = 'inline';
  const btn = document.createElement('button');
  btn.type = 'submit';
  btn.className = 'swasm-fork-btn';
  btn.textContent = 'Fork to run ▸';
  form.appendChild(btn);
  wrap.appendChild(msg);
  wrap.appendChild(form);
  return wrap;
}
