// ── Run orchestration: drive the persistent mhsi REPL, dependency-scoped ──
//
// Touching cell K marks it active and re-runs the AFFECTED set (K + its transitive
// dependents that are active). The REPL accumulates definitions (re-feeding a name
// ADDS a clause rather than replacing it), so each recompute `:clear`s and re-feeds
// the affected cells' dependency closure in order — bounded by what those cells
// actually need, not the whole notebook. Base stays loaded, so each feed is ~90ms.

const SwasmState = { controls: [], slug: '', active: new Set() };

// Live source of every cell, from its editable textarea.
function liveCells() {
  return SwasmState.controls.map((c, i) => ({ index: i, code: c.textarea.value }));
}

// Coalesced recompute: a run in flight collects further roots and reruns once.
let swasmBusy = false;
let swasmPending = new Set();
function recompute(idx) {
  if (idx !== undefined) {
    SwasmState.active.add(idx);
    swasmPending.add(idx);
  }
  if (swasmBusy || swasmPending.size === 0) return;
  swasmBusy = true;
  const roots = swasmPending;
  swasmPending = new Set();
  runNotebook(roots).finally(() => {
    swasmBusy = false;
    if (swasmPending.size) recompute();
  });
}

async function runNotebook(roots) {
  const live = liveCells();
  const slug = SwasmState.slug;
  const { revDeps, fwdDeps } = buildDeps(live);

  // affected = roots + their transitive dependents that are active.
  const affected = new Set(roots);
  for (const r of roots)
    for (const a of affectedFrom(r, revDeps)) if (SwasmState.active.has(a)) affected.add(a);

  const forkSet = new Set([...affected].filter((i) => cellNeedsFork(live[i].code)));

  // closure = the affected cells + their transitive upstream deps (the definitions
  // they need in scope), minus fork cells. Fed in source order after a :clear.
  const closure = new Set();
  const stack = [...affected];
  while (stack.length) {
    const i = stack.pop();
    if (forkSet.has(i) || closure.has(i)) continue;
    closure.add(i);
    for (const dep of fwdDeps.get(i) || []) stack.push(dep);
  }
  const order = [...closure].sort((a, b) => a - b);

  setRunning(true);
  for (const i of affected) {
    if (forkSet.has(i)) showForkOnly(SwasmState.controls[i], slug);
    else markCellRunning(SwasmState.controls[i]);
  }

  // Only :clear when re-feeding a definition (else the REPL keeps the old clause).
  // A closure of pure expressions needs no clear — keeping it ~one feed (~90ms).
  const needReset = order.some((i) => cellDefs(live[i].code).size > 0);
  try {
    await MhsEngine.load();
    if (needReset) await MhsEngine.reset();
  } catch (_e) {
    affected.forEach((i) => forkSet.has(i) || showEngineUnavailable(SwasmState.controls[i], slug));
    setRunning(false);
    return;
  }

  for (const i of order) {
    let res;
    try {
      res = await MhsEngine.feed(cellToReplLines(live[i].code));
    } catch (_e) {
      if (affected.has(i)) showEngineUnavailable(SwasmState.controls[i], slug);
      continue;
    }
    if (affected.has(i)) renderCell(SwasmState.controls[i], res, slug);
  }
  setRunning(false);
}

// Render one re-run cell's REPL result. A missing-module error → Fork nudge;
// otherwise text output and/or the error text, with the error border.
function renderCell(ctrl, res, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  if (stderrNeedsFork(res.err)) {
    showForkOnly(ctrl, slug);
    return;
  }
  const outputs = res.out.trim() ? [{ oiMime: 'text/plain', oiOutput: res.out }] : [];
  ctrl.root.classList.toggle('has-error', !!res.err.trim());
  renderCellResult(ctrl.out, outputs, res.err);
}

function markCellRunning(ctrl) {
  ctrl.status.textContent = MhsEngine.available ? 'running…' : 'starting Haskell…';
  ctrl.root.classList.add('running');
}

function setRunning(on) {
  SwasmState.controls.forEach((c) => {
    c.runBtn.disabled = on;
    if (!on) c.root.classList.remove('running');
  });
}

// A cell MicroHs can't run shows only the Fork nudge.
function showForkOnly(ctrl, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  ctrl.out.innerHTML = '';
  ctrl.out.appendChild(forkNudge(slug));
}

// The engine could not boot (404 / timeout) — degrade the cell to a Fork nudge.
function showEngineUnavailable(ctrl, slug) {
  ctrl.status.textContent = '';
  ctrl.root.classList.remove('running');
  ctrl.out.innerHTML = '';
  const note = document.createElement('div');
  note.className = 'cell-output';
  note.style.cssText = 'margin-bottom:6px';
  note.textContent = "Couldn't start the in-browser Haskell engine.";
  ctrl.out.appendChild(note);
  ctrl.out.appendChild(forkNudge(slug));
}
