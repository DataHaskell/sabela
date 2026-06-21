// ── Bootstrap: wire everything up on DOMContentLoaded ────────────────────

function initWasmRun() {
  const { source, slug } = readSourceIsland();
  const cells = parseHaskellCells(source);
  if (cells.length === 0) return; // nothing runnable (prose-only share)

  injectRunStyles();
  const controls = mountCellControls(cells);

  SwasmState.controls = controls;
  SwasmState.slug = slug;

  // Reactive: a Run click re-runs the cell + its active dependents; so does an
  // edit, after a short quiet period. Only affected cells re-run (dependency
  // scoped), each ~90ms against the warm persistent session.
  let editTimer = null;
  controls.forEach((ctrl, idx) => {
    ctrl.runBtn.addEventListener('click', () => {
      if (editTimer) clearTimeout(editTimer);
      recompute(idx);
    });
    ctrl.textarea.addEventListener('input', () => {
      if (editTimer) clearTimeout(editTimer);
      editTimer = setTimeout(() => recompute(idx), 700);
    });
  });

  // Warm the persistent MicroHs session in the background once the page is idle,
  // so the first Run is usually already past the ~2s boot/warmup. Reader pages
  // that are never run just pay an idle background load.
  const warm = () => MhsEngine.load().catch(() => {});
  if (typeof requestIdleCallback === 'function') requestIdleCallback(warm, { timeout: 3000 });
  else setTimeout(warm, 2000);
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initWasmRun);
} else {
  initWasmRun();
}
