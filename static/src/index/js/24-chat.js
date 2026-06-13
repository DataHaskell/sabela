async function sendChatMessage() {
  if (!aiConfigured) {
    openAIModal();
    return;
  }
  const input = document.getElementById('chat-input');
  const text = input.value.trim();
  if (!text || chatStreaming) return;
  input.value = '';
  // Show user message
  const container = document.getElementById('chat-messages');
  const userDiv = document.createElement('div');
  userDiv.className = 'chat-msg user';
  userDiv.textContent = text;
  container.appendChild(userDiv);
  container.scrollTop = container.scrollHeight;
  // Start streaming
  chatStreaming = true;
  currentAssistantBubble = null;
  document.getElementById('chat-send').style.display = 'none';
  document.getElementById('chat-cancel').style.display = 'inline-block';
  // Send to backend
  try {
    await fetch('/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ crMessage: text }),
    });
  } catch (e) {
    showChatError('Failed to send message: ' + e.message);
    finishChatTurn();
  }
}

function appendChatTextDelta(text) {
  const container = document.getElementById('chat-messages');
  if (!currentAssistantBubble) {
    currentAssistantBubble = document.createElement('div');
    currentAssistantBubble.className = 'chat-msg assistant';
    currentAssistantBubble._rawText = '';
    container.appendChild(currentAssistantBubble);
  }
  currentAssistantBubble._rawText += text;
  // Render markdown
  if (typeof marked !== 'undefined') {
    currentAssistantBubble.innerHTML = marked.parse(currentAssistantBubble._rawText);
    renderMath(currentAssistantBubble);
  } else {
    currentAssistantBubble.textContent = currentAssistantBubble._rawText;
  }
  container.scrollTop = container.scrollHeight;
}

function showChatToolCall(toolCallId, toolName, input) {
  const container = document.getElementById('chat-messages');
  const div = document.createElement('div');
  div.className = 'chat-tool-indicator active';
  div.id = 'tool-' + toolCallId;
  const label = toolName.replace(/_/g, ' ');
  div.textContent = label;
  container.appendChild(div);
  container.scrollTop = container.scrollHeight;
  // Start new assistant bubble for text after tools
  currentAssistantBubble = null;
}

function showChatToolResult(toolCallId, result) {
  const el = document.getElementById('tool-' + toolCallId);
  if (el) {
    el.classList.remove('active');
    el.textContent = el.textContent.replace('...', '') + ' done';
  }
}

// Surface proposals that arrive while the chat drawer is hidden (e.g.
// siza/CLI propose_edit): open the chat tab without the API-key modal.
function revealChatPanel() {
  const panel = document.getElementById('right-panel');
  if (panel.classList.contains('collapsed') || activeTab !== 'chat') {
    panel.classList.remove('collapsed');
    switchTab('chat', { quiet: true });
  }
}

function showChatEditProposal(editId, cellId, oldSource, newSource) {
  revealChatPanel();
  const container = document.getElementById('chat-messages');
  const div = document.createElement('div');
  div.className = 'chat-edit-proposal';
  div.innerHTML = `
    <div style="font-size:10px;color:var(--fg-dim);margin-bottom:6px">Proposed edit for cell ${cellId}</div>
    <pre style="color:var(--red);font-size:11px">${escapeHtml(oldSource.substring(0, 500))}</pre>
    <pre style="color:var(--green);font-size:11px">${escapeHtml(newSource.substring(0, 500))}</pre>
    <div class="chat-edit-actions">
      <button class="accept" onclick="acceptEdit(${editId}, ${cellId})">Accept</button>
      <button class="revert" onclick="revertEdit(${editId}, ${cellId})">Revert</button>
    </div>
  `;
  container.appendChild(div);
  container.scrollTop = container.scrollHeight;
  // Mark cell as proposed
  const cellEl = document.querySelector(`.cell[data-id="${cellId}"]`);
  if (cellEl) cellEl.classList.add('ai-proposed');
  currentAssistantBubble = null;
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

async function acceptEdit(editId, cellId) {
  try {
    await fetch(`/api/chat/edit/${editId}/accept`, { method: 'POST' });
    // Clear the editor draft so render() picks up the server's new source
    if (editors[cellId]) {
      delete editors[cellId];
    }
    const nb = await api('GET', 'notebook');
    render(nb);
  } catch (e) {
    showChatError('Accept failed: ' + e.message);
  }
}

async function revertEdit(editId, cellId) {
  try {
    await fetch(`/api/chat/edit/${editId}/revert`, { method: 'POST' });
    const cellEl = document.querySelector(`.cell[data-id="${cellId}"]`);
    if (cellEl) cellEl.classList.remove('ai-proposed');
  } catch (e) {
    showChatError('Revert failed: ' + e.message);
  }
}

function showChatError(message) {
  const container = document.getElementById('chat-messages');
  const div = document.createElement('div');
  div.className = 'chat-error';
  div.textContent = message;
  container.appendChild(div);
  container.scrollTop = container.scrollHeight;
}

function finishChatTurn(label) {
  chatStreaming = false;
  currentAssistantBubble = null;
  document.getElementById('chat-send').style.display = 'inline-block';
  document.getElementById('chat-cancel').style.display = 'none';
}

async function cancelChat() {
  try {
    await fetch('/api/chat/cancel', { method: 'POST' });
  } catch (e) {}
}

async function newChatConversation() {
  try {
    await fetch('/api/chat/clear', { method: 'POST' });
  } catch (e) {}
  document.getElementById('chat-messages').innerHTML = '';
  document
    .querySelectorAll('.cell.ai-proposed')
    .forEach((el) => el.classList.remove('ai-proposed'));
  resetSessionUsage();
}

// Chat input keyboard handling
document.addEventListener('DOMContentLoaded', () => {
  const chatInput = document.getElementById('chat-input');
  if (chatInput) {
    chatInput.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        sendChatMessage();
      }
    });
  }
});
