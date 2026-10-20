// ── Info panel rendering ─────────────────────────────────────────
// Turns the raw text of a lookup (GHCi `:info`, then `:doc` after a
// "--- Documentation ---" rule) into a signature block, a "defined in"
// line, a folded instance list and the Haddock prose rendered as Markdown.

const INFO_DOC_RULE = '\n--- Documentation ---\n';
const HS_KEYWORDS =
  /^(class|instance|type|data|newtype|where|forall|family|deriving|import|qualified|as|hiding|module|role|pattern|infixl|infixr|infix)$/;
const HS_TOKEN_RE =
  /(--.*$)|("(?:[^"\\]|\\.)*")|((?:[A-Z][\w']*\.)*[A-Za-z_][\w']*)|(::|=>|->|<-|[=|\\@~!#$%^&*+./<>?-]+)|(\d+(?:\.\d+)?)/gm;

// Package-qualified and internal module prefixes that GHCi prints but a
// reader never wants: `ghc-internal-9.1202.0:GHC.Internal.Data.Foldable.fold`.
function cleanGhcNames(s) {
  return s
    .replace(/\b[a-z][\w-]*?-\d[\d.]*:(?=[A-Z])/g, '')
    .replace(/\b(?:GHC\.Internal\.)?(?:[A-Z][\w']*\.)+(?=[a-z_][\w']*\b|\([^\s()]+\))/g, (q) =>
      /^Sabela[A-Za-z]*\.|^GHC\.Internal\./.test(q) || q.split('.').length > 2 ? '' : q
    )
    .replace(/\bSabela[A-Za-z]*\./g, '');
}

// GHCi wraps long lines at an arbitrary column; continuation lines are
// indented deeper than any declaration, so they are joined back on.
function joinWrapped(lines) {
  const out = [];
  for (const line of lines) {
    const indent = line.match(/^[ \t]*/)[0].length;
    const isDefined = /^\s*--\s*Defined/.test(line);
    if (out.length && indent > 2 && !isDefined && !/^\s*$/.test(line)) {
      out[out.length - 1] += ' ' + line.trim();
    } else {
      out.push(line.replace(/\s+$/, ''));
    }
  }
  return out;
}

function hlToken(m, com, str, ident, op, num) {
  const t = escapeHtml(m);
  if (com) return `<span class="hl-comment">${t}</span>`;
  if (str) return `<span class="hl-string">${t}</span>`;
  if (ident) {
    if (HS_KEYWORDS.test(m)) return `<span class="hl-keyword">${t}</span>`;
    if (/^[A-Z]/.test(m)) return `<span class="hl-type">${t}</span>`;
    return t;
  }
  if (op) return `<span class="hl-op">${t}</span>`;
  if (num) return `<span class="hl-num">${t}</span>`;
  return t;
}

function hlHaskell(src) {
  let out = '';
  let last = 0;
  for (const m of src.matchAll(HS_TOKEN_RE)) {
    out += escapeHtml(src.slice(last, m.index)) + hlToken(...m.slice(0, 6));
    last = m.index + m[0].length;
  }
  return out + escapeHtml(src.slice(last));
}

// Splits `:info` text into declaration lines, instance lines and the first
// "Defined in" location. Each instance keeps its own location in a title.
function parseInfoSection(text) {
  const lines = joinWrapped(cleanGhcNames(text).split('\n'));
  const decl = [];
  const instances = [];
  let definedIn = null;
  let last = null;
  for (const line of lines) {
    const def = line.match(/^\s*--\s*Defined (?:in|at) [‘']?([^’']+)[’']?\s*$/);
    if (def) {
      const where = def[1].startsWith('<interactive>') ? 'this notebook' : def[1];
      if (last === 'instance') instances[instances.length - 1].where = where;
      else if (!definedIn) definedIn = where;
      continue;
    }
    if (/^instance\b/.test(line)) {
      instances.push({ text: line.replace(/^instance\s+/, ''), where: '' });
      last = 'instance';
    } else if (/^type instance \S*Rep1? /.test(line) || (last === 'skip' && /^\s/.test(line))) {
      last = 'skip';
    } else if (line.trim() && last !== 'skip') {
      decl.push(line);
      last = 'decl';
    }
  }
  return { decl: decl.join('\n'), instances, definedIn };
}

// Haddock markup to Markdown: 'ident' and @code@ become code spans,
// "Module" a code span, /emph/ emphasis, bird tracks and >>> examples a
// fenced block, and `==== __Examples__` a heading.
function haddockToMarkdown(docText) {
  const lines = [];
  for (const raw of docText.split('\n')) {
    if (/^\s*--\s*[A-Z][\w ]* defined in [‘']/.test(raw)) continue;
    if (!/^\s*--/.test(raw)) continue;
    lines.push(raw.replace(/^\s*--(?: \|)?(?: |$)/, ''));
  }
  const out = [];
  let i = 0;
  while (i < lines.length) {
    if (/^\s*>/.test(lines[i])) {
      out.push('```');
      while (i < lines.length && lines[i].trim()) out.push(lines[i++].replace(/^\s*> ?(?!>)/, ''));
      out.push('```');
      continue;
    }
    if (/^\s*@\s*$/.test(lines[i])) {
      out.push('```');
      i++;
      while (i < lines.length && !/^\s*@\s*$/.test(lines[i])) out.push(lines[i++]);
      i++;
      out.push('```');
      continue;
    }
    out.push(haddockInline(lines[i++]));
  }
  return out.join('\n');
}

function haddockInline(line) {
  const h = line.match(/^\s*(=+)\s+(.*)$/);
  if (h) return `${'#'.repeat(Math.min(4, h[1].length + 2))} ${h[2].replace(/^__(.*)__$/, '$1')}`;
  return line
    .replace(/\\\((.+?)\\\)/g, '$$$1$$')
    .replace(/@([^@\n]+)@/g, (_, c) => '`' + c.replace(/'([^'\s]+)'/g, '$1') + '`')
    .replace(
      /(^|[\s(])'((?:[A-Z][\w']*\.)*[A-Za-z_][\w']*|\([^\s()]+\))'(?=[\s.,;:)!?]|$)/g,
      '$1`$2`'
    )
    .replace(/"([A-Z][\w']*(?:\.[A-Z][\w']*)+)"/g, '`$1`')
    .replace(/(^|\s)\/([^/\n]+?)\/(?=[\s.,;:)!?]|$)/g, '$1*$2*')
    .replace(/<(https?:\/\/[^\s>]+)\s+([^>]+)>/g, '[$2]($1)')
    .replace(/^(\s*)\((\d+)\)\s/, '$1$2. ');
}

function infoNote(container, text) {
  const note = document.createElement('div');
  note.className = 'info-note';
  note.innerHTML = formatGhcInline(cleanGhcNames(text.trim()));
  container.appendChild(note);
}

function renderInfoResult(container, text, name) {
  container.innerHTML = '';
  const [infoText, docText] = text.split(INFO_DOC_RULE);
  if (
    /^(not in scope|no ghci session|<interactive>|variable not in scope)/i.test(infoText.trim())
  ) {
    infoNote(container, infoText);
    return;
  }
  const { decl, instances, definedIn } = parseInfoSection(infoText);
  if (decl) {
    const sig = document.createElement('pre');
    sig.className = 'info-sig';
    sig.innerHTML = hlHaskell(decl);
    container.appendChild(sig);
  }
  if (definedIn) {
    const where = document.createElement('div');
    where.className = 'info-where';
    where.innerHTML = `defined in <code>${escapeHtml(definedIn)}</code>`;
    container.appendChild(where);
  }
  if (docText && docText.trim()) {
    const doc = document.createElement('div');
    doc.className = 'info-doc';
    doc.innerHTML = marked.parse(haddockToMarkdown(docText));
    container.appendChild(doc);
    if (typeof renderMathInElement === 'function') {
      renderMathInElement(doc, {
        delimiters: [{ left: '$', right: '$', display: false }],
        throwOnError: false,
      });
    }
  }
  if (instances.length) {
    const det = document.createElement('details');
    det.className = 'info-instances';
    det.innerHTML =
      `<summary>${instances.length} instance${instances.length === 1 ? '' : 's'}</summary>` +
      instances
        .map(
          (i) =>
            `<div class="info-instance" title="${escapeHtml(i.where)}">${hlHaskell(i.text)}</div>`
        )
        .join('');
    container.appendChild(det);
  }
  if (!container.childElementCount) infoNote(container, `No information found for ${name}.`);
}
