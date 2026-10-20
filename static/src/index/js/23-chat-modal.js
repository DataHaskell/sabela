// ── Chat (AI Assistant) ──────────────────────────────────────────
let chatStreaming = false;
let currentAssistantBubble = null;

let aiConfigured = false;

async function updateChatContext() {
  const el = document.getElementById('chat-context');
  if (!el) return;
  // Check if AI is configured + refresh model info
  try {
    const resp = await fetch('/api/config/ai');
    const cfg = await resp.json();
    aiConfigured = cfg.configured;
    aiCurrentModel = cfg.model || null;
    aiModels = cfg.models || aiModels;
    updateModelBadge();
  } catch (e) {
    aiConfigured = false;
  }
  if (!notebook) return;
  const codeCells = notebook.nbCells.filter((c) => c.cellType === 'CodeCell').length;
  el.textContent = `${notebook.nbTitle} — ${codeCells} code cells`;
}

let aiModels = [];
let aiOllamaModels = [];
let aiOllamaReachable = false;
let aiCurrentModel = null;
const CUSTOM_MODEL_VALUE = '__custom__';

async function openAIModal() {
  const overlay = document.getElementById('ai-modal-overlay');
  overlay.style.display = 'flex';
  const keyInput = document.getElementById('ai-modal-key');
  const keyHint = document.getElementById('ai-modal-key-hint');
  const title = document.getElementById('ai-modal-title');
  const desc = document.getElementById('ai-modal-desc');
  const err = document.getElementById('ai-modal-error');
  keyInput.value = '';
  err.style.display = 'none';
  try {
    const resp = await fetch('/api/config/ai');
    const info = await resp.json();
    aiModels = info.models || [];
    aiOllamaModels = info.ollamaModels || [];
    aiOllamaReachable = !!info.ollamaReachable;
    aiCurrentModel = info.model || null;
    document.getElementById('ai-modal-provider').value = info.provider || 'ollama';
    document.getElementById('ai-modal-toollimit').value = info.toolLimit ?? '';
    document.getElementById('ai-modal-numctx').value = info.numCtx ?? '';
    onProviderChange();
    if (info.configured) {
      title.textContent = 'AI settings';
      desc.textContent = 'Change the provider or model. An Anthropic key is kept until replaced.';
      keyHint.textContent = '(leave blank to keep current)';
    } else {
      title.textContent = 'Connect to AI';
      desc.textContent =
        'Pick a local Ollama model, or switch to Anthropic and paste an API key. Stored in your workspace only.';
      keyHint.textContent = '';
    }
  } catch (e) {
    document.getElementById('ai-modal-provider').value = 'ollama';
    onProviderChange();
  }
  setTimeout(() => keyInput.focus(), 50);
}

// The API key is only needed for Anthropic; Ollama is a local server. The
// model list follows the provider: the tags Ollama has pulled, or the Claude
// models known by name.
function onProviderChange() {
  const provider = document.getElementById('ai-modal-provider').value;
  const isOllama = provider === 'ollama';
  document.getElementById('ai-modal-key-wrap').style.display = isOllama ? 'none' : '';
  // num_ctx is an Ollama concept; Anthropic has a fixed large context window.
  document.getElementById('ai-modal-numctx-wrap').style.display = isOllama ? '' : 'none';
  document.getElementById('ai-modal-custom').placeholder = isOllama ? 'family:tag' : 'claude-...';
  if (isOllama) {
    const installed = aiOllamaModels.map((id) => ({ id, label: id, description: 'installed' }));
    const current = aiCurrentModel && !aiCurrentModel.startsWith('claude') ? aiCurrentModel : null;
    populateModelSelect(installed, current || (installed[0] && installed[0].id), {
      customDesc: 'Any tag from `ollama list`',
      empty: aiOllamaReachable
        ? 'No models pulled yet. Run: ollama pull gpt-oss:20b'
        : 'Ollama is not reachable. Start it, or set OLLAMA_HOST.',
    });
  } else {
    const current = aiCurrentModel && aiCurrentModel.startsWith('claude') ? aiCurrentModel : null;
    populateModelSelect(aiModels, current, {
      customDesc: 'Type any model id supported by your Anthropic account',
    });
  }
}

function populateModelSelect(models, current, opts) {
  const sel = document.getElementById('ai-modal-model');
  sel.innerHTML = '';
  let matched = false;
  if (!models.length && opts && opts.empty) {
    const note = document.createElement('option');
    note.value = '';
    note.disabled = true;
    note.selected = true;
    note.textContent = opts.empty;
    note.dataset.desc = opts.empty;
    sel.appendChild(note);
  }
  for (const m of models) {
    const opt = document.createElement('option');
    opt.value = m.id;
    opt.textContent = m.label === m.id ? m.id : m.label + ' — ' + m.id;
    opt.dataset.desc = m.description || '';
    if (m.id === current) {
      opt.selected = true;
      matched = true;
    }
    sel.appendChild(opt);
  }
  const customOpt = document.createElement('option');
  customOpt.value = CUSTOM_MODEL_VALUE;
  customOpt.textContent = 'Custom…';
  customOpt.dataset.desc = (opts && opts.customDesc) || '';
  sel.appendChild(customOpt);
  if (current && !matched) {
    customOpt.selected = true;
    document.getElementById('ai-modal-custom').value = current;
  } else if (!models.length) {
    document.getElementById('ai-modal-custom').value = '';
  }
  onModelChange();
}

function onModelChange() {
  const sel = document.getElementById('ai-modal-model');
  const opt = sel.options[sel.selectedIndex];
  const descEl = document.getElementById('ai-modal-model-desc');
  const customWrap = document.getElementById('ai-modal-custom-wrap');
  descEl.textContent = opt && opt.dataset.desc ? opt.dataset.desc : '';
  if (sel.value === CUSTOM_MODEL_VALUE) {
    customWrap.classList.add('shown');
  } else {
    customWrap.classList.remove('shown');
  }
}

function closeAIModal() {
  document.getElementById('ai-modal-overlay').style.display = 'none';
}

async function submitAIModal() {
  const keyInput = document.getElementById('ai-modal-key');
  const sel = document.getElementById('ai-modal-model');
  const customInput = document.getElementById('ai-modal-custom');
  const errEl = document.getElementById('ai-modal-error');
  const provider = document.getElementById('ai-modal-provider').value;
  const key = keyInput.value.trim();
  let model = sel.value;
  if (model === CUSTOM_MODEL_VALUE) model = customInput.value.trim();

  if (provider !== 'ollama' && !aiConfigured && !key) {
    errEl.textContent = 'Please enter an API key';
    errEl.style.display = 'block';
    return;
  }
  if (!model) {
    errEl.textContent = 'Please pick or enter a model';
    errEl.style.display = 'block';
    return;
  }
  const body = { model, provider };
  if (key) body.apiKey = key;
  const toolLimit = parseInt(document.getElementById('ai-modal-toollimit').value, 10);
  if (Number.isFinite(toolLimit) && toolLimit > 0) body.toolLimit = toolLimit;
  if (provider === 'ollama') {
    const numCtx = parseInt(document.getElementById('ai-modal-numctx').value, 10);
    if (Number.isFinite(numCtx) && numCtx > 0) body.numCtx = numCtx;
  }
  try {
    const resp = await fetch('/api/config/ai', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });
    const result = await resp.json();
    if (result.configured) {
      aiConfigured = true;
      aiCurrentModel = result.model || model;
      closeAIModal();
      updateChatContext();
      updateModelBadge();
    } else {
      errEl.textContent = result.error || 'Failed to configure';
      errEl.style.display = 'block';
    }
  } catch (e) {
    errEl.textContent = 'Connection failed';
    errEl.style.display = 'block';
  }
}

function updateModelBadge() {
  const btn = document.getElementById('chat-model-btn');
  if (!btn) return;
  btn.textContent = aiCurrentModel ? shortModelLabel(aiCurrentModel) : 'model';
}

function shortModelLabel(mid) {
  if (mid.includes('haiku')) return 'haiku';
  if (mid.includes('opus')) return 'opus';
  if (mid.includes('sonnet')) return 'sonnet';
  const tag = mid.replace(/:latest$/, '');
  return tag.length > 22 ? tag.slice(0, 20) + '…' : tag;
}

// Enter to submit in the modal
document.addEventListener('keydown', (e) => {
  if (e.key === 'Enter' && document.getElementById('ai-modal-overlay').style.display === 'flex') {
    e.preventDefault();
    submitAIModal();
  }
  if (e.key === 'Escape' && document.getElementById('ai-modal-overlay').style.display === 'flex') {
    closeAIModal();
  }
});
