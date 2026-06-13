// ── Actions ──────────────────────────────────────────────────────
async function runCell(cellId) {
  if (editors[cellId])
    await api('PUT', `cell/${cellId}`, { ucSource: editors[cellId].getValue() }).catch(() => {});
  api('POST', `run/${cellId}`).catch(console.error);
}

async function runAll() {
  // Save all editor content without triggering execution
  for (const [id, cm] of Object.entries(editors))
    await api('PUT', `cell/${parseInt(id)}/source`, { ucSource: cm.getValue() }).catch(() => {});
  api('POST', 'run-all').catch(console.error);
  setStatus('Running all cells...', 'running');
}

async function resetNotebook() {
  api('POST', 'reset').then(render).catch(console.error);
}

async function interruptKernel() {
  setStatus('Interrupting…', 'running');
  await api('POST', 'interrupt').catch(console.error);
}

async function restartKernel() {
  hideCrashBanner();
  setStatus('Restarting kernel...', 'running');
  // Flush editor drafts first so the restarted kernel runs what's on screen.
  for (const [id, cm] of Object.entries(editors))
    await api('PUT', `cell/${parseInt(id)}/source`, { ucSource: cm.getValue() }).catch(() => {});
  await api('POST', 'restart-kernel').catch(console.error);
}

function showCrashBanner() {
  let banner = document.getElementById('crash-banner');
  if (!banner) {
    banner = document.createElement('div');
    banner.id = 'crash-banner';
    banner.style.cssText =
      'background:#b91c1c;color:#fff;padding:8px 16px;text-align:center;cursor:pointer;font-weight:600;';
    banner.textContent = 'Kernel crashed — click to restart';
    banner.onclick = restartKernel;
    document.body.prepend(banner);
  }
  banner.style.display = 'block';
}

function hideCrashBanner() {
  const banner = document.getElementById('crash-banner');
  if (banner) banner.style.display = 'none';
}

async function addCell(afterId, type, lang) {
  const l = lang || 'Haskell';
  await api('POST', 'cell', {
    icAfter: afterId,
    icType: type,
    icLang: l,
    icSource: type === 'CodeCell' ? '' : 'New text',
  });
  const nb = await api('GET', 'notebook');
  render(nb);
}

async function clearCellOutput(cellId) {
  api('POST', `clear/${cellId}`).catch(console.error);
}

// The `-- compile` directive in the cell text is the single source of truth;
// this button is sugar over an ordinary edit (insert/remove the line).
async function toggleCellCompile(cellId) {
  const cm = editors[cellId];
  if (!cm) return;
  const lines = cm.getValue().split('\n');
  let next;
  if (lines.some((l) => COMPILE_DIRECTIVE_RE.test(l))) {
    next = lines.filter((l) => !COMPILE_DIRECTIVE_RE.test(l)).join('\n');
  } else {
    let at = 0;
    while (at < lines.length && /^--\s*cabal:/.test(lines[at])) at++;
    lines.splice(at, 0, '-- compile');
    next = lines.join('\n');
  }
  await api('PUT', `cell/${cellId}`, { ucSource: next }).catch(() => {});
  const nb = await api('GET', 'notebook');
  render(nb);
  api('POST', `run/${cellId}`).catch(console.error);
}

async function deleteCell(cellId) {
  const nb = await api('DELETE', `cell/${cellId}`);
  delete editors[cellId];
  render(nb);
}

function editProse(cellId) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  el.classList.add('editing');
  const ta = el.querySelector('.prose-edit');
  ta.style.height = 'auto';
  ta.style.height = Math.max(80, ta.scrollHeight) + 'px';
  ta.focus();
}

async function setCellLang(cellId, lang) {
  const cell = await api('PUT', `cell/${cellId}/lang`, lang);
  const nb = await api('GET', 'notebook');
  render(nb);
}

async function finishEditProse(cellId, textarea) {
  const el = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (!el) return;
  el.classList.remove('editing');
  await api('PUT', `cell/${cellId}`, { ucSource: textarea.value });
  const prose = el.querySelector('.prose-rendered');
  prose.innerHTML = renderProseHTML(textarea.value);
  renderMath(prose);
}

// ── Platform-aware keyboard shortcut hints ──────────────────────
const IS_MAC = /Mac|iPhone|iPad|iPod/.test(navigator.platform || navigator.userAgent || '');
function kbd(...parts) {
  // Mac joins symbols without separator (⌘⇧K); others use Ctrl+Shift+K.
  if (IS_MAC) {
    return parts
      .map((p) => (p === 'mod' ? '⌘' : p === 'shift' ? '⇧' : p === 'enter' ? '↵' : p.toUpperCase()))
      .join('');
  }
  return parts
    .map((p) =>
      p === 'mod' ? 'Ctrl' : p === 'shift' ? 'Shift' : p === 'enter' ? 'Enter' : p.toUpperCase()
    )
    .join('+');
}
