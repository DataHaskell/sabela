// ── Output classification / merging (shared with dashboard) ──────
function classifyOutput(item) {
  if (
    item.oiMime === 'text/html' &&
    item.oiOutput &&
    item.oiOutput.includes('class="sabela-viz"')
  ) {
    return 'content';
  }
  if (
    item.oiMime === 'text/html' &&
    item.oiOutput &&
    item.oiOutput.includes('parent.postMessage')
  ) {
    return 'widget';
  }
  return 'content';
}
function mergeHtmlOutputs(outputs) {
  const merged = [];
  for (const o of outputs) {
    const isWidget = classifyOutput(o) === 'widget';
    const last = merged[merged.length - 1];
    if (
      !isWidget &&
      o.oiMime === 'text/html' &&
      last &&
      last.oiMime === 'text/html' &&
      classifyOutput(last) !== 'widget'
    ) {
      last.oiOutput += '\n' + o.oiOutput;
    } else {
      merged.push({ oiMime: o.oiMime, oiOutput: o.oiOutput });
    }
  }
  return merged;
}

// ── Render a content output ──────────────────────────────────────
function renderOutput(container, content, mime) {
  if (mime === 'text/html') {
    if (isStatic) {
      mountStaticFrame(container, content, 'content');
      return;
    }
    const iframe = document.createElement('iframe');
    iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin');
    iframe.style.visibility = 'hidden';
    container.appendChild(iframe);
    requestAnimationFrame(() => {
      const doc = iframe.contentDocument || iframe.contentWindow?.document;
      if (!doc) return;
      iframe.dataset.lastContent = content;
      doc.open();
      iframe.onload = () => {
        iframe.style.height = Math.max(60, doc.body.scrollHeight + 20) + 'px';
        iframe.style.visibility = '';
      };
      doc.write(iframeContentStyle() + content);
      doc.close();
    });
  } else if (mime === 'text/markdown') {
    const div = document.createElement('div');
    div.className = 'mime-markdown';
    div.innerHTML = renderMarkdownWithMath(content);
    wrapTables(div);
    highlightWithin(div);
    container.appendChild(div);
  } else if (mime === 'image/svg+xml') {
    const div = document.createElement('div');
    div.className = 'mime-svg';
    div.innerHTML = content;
    container.appendChild(div);
  } else if (mime.startsWith('image/') && mime.includes('base64')) {
    const div = document.createElement('div');
    div.className = 'mime-image';
    const mimeClean = mime.replace(';base64', '');
    div.innerHTML = '<img src="data:' + mimeClean + ';base64,' + content.trim() + '" />';
    container.appendChild(div);
  } else if (mime === 'application/json') {
    const div = document.createElement('div');
    div.className = 'mime-json';
    try {
      div.textContent = JSON.stringify(JSON.parse(content), null, 2);
    } catch {
      div.textContent = content;
    }
    container.appendChild(div);
  } else if (mime === 'text/latex') {
    const div = document.createElement('div');
    div.className = 'mime-latex';
    try {
      katex.render(content, div, { displayMode: true, throwOnError: false });
    } catch {
      div.textContent = content;
    }
    container.appendChild(div);
  } else {
    const div = document.createElement('div');
    div.className = 'mime-plain';
    div.textContent = content;
    container.appendChild(div);
  }
}

function renderWidgetOutput(container, output) {
  if (isStatic) {
    // Script-drawn widgets (e.g. the scatter <canvas>) need their script to run to
    // render at all — do it in the opaque-origin, network-dead sandbox (see
    // mountStaticFrame), inert via pointer-events:none. Form controls have no such
    // need and stay as greyed-out, script-stripped placeholders.
    if (/<canvas|<script/i.test(output.oiOutput)) {
      mountStaticFrame(container, output.oiOutput, 'widget');
      return;
    }
    const wrapper = document.createElement('div');
    wrapper.className = 'static-widget';
    let cleaned = output.oiOutput
      .replace(/\s*on\w+="[^"]*"/g, '')
      .replace(/<(input|select|button)/gi, '<$1 disabled');
    wrapper.innerHTML = cleaned;
    container.appendChild(wrapper);
    return;
  }
  const iframe = document.createElement('iframe');
  iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin');
  iframe.style.visibility = 'hidden';
  container.appendChild(iframe);
  requestAnimationFrame(() => {
    const doc = iframe.contentDocument || iframe.contentWindow?.document;
    if (!doc) return;
    iframe.dataset.lastContent = output.oiOutput;
    iframe.dataset.iframeKind = 'widget';
    doc.open();
    iframe.onload = () => {
      iframe.style.height = Math.max(32, doc.body.scrollHeight) + 'px';
      iframe.style.visibility = '';
    };
    doc.write(widgetIframeStyle() + output.oiOutput);
    doc.close();
  });
}

// Re-write iframe docs so they reflow at the current width / theme.
function rerenderVisibleIframes() {
  const slide = slideEls[current];
  if (!slide) return;
  slide.querySelectorAll('iframe[data-last-content]').forEach((f) => {
    if (f.dataset.staticFrame) {
      f.srcdoc = staticSrcdoc(f.dataset.lastContent, f.dataset.iframeKind || 'content', f.id);
      return;
    }
    try {
      const doc = f.contentDocument || f.contentWindow?.document;
      if (!doc) return;
      const isWidget = f.dataset.iframeKind === 'widget';
      f.style.visibility = 'hidden';
      doc.open();
      f.onload = () => {
        f.style.height =
          Math.max(isWidget ? 32 : 60, doc.body.scrollHeight + (isWidget ? 0 : 20)) + 'px';
        f.style.visibility = '';
      };
      doc.write((isWidget ? widgetIframeStyle() : iframeContentStyle()) + f.dataset.lastContent);
      doc.close();
    } catch {}
  });
}
