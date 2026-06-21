// ── Cell → REPL input + dependency tracking ──────────────────────────────
//
// The runner drives ONE persistent `mhsi` REPL (see 03-mhs-engine.js). Each cell
// is fed as REPL input — definitions accumulate, expressions print. Dependency
// tracking decides which cells to (re)feed so an edit only re-runs its dependents.

// A cell is a declaration if it binds/imports/declares rather than being a bare
// expression. (Kept for classification by other partials.)
function isDeclarationCell(code) {
  const lines = code.split('\n').filter((l) => l.trim() && !/^\s*--/.test(l));
  if (lines.length === 0) return true; // empty → nothing to print
  if (/^\s*import\b/m.test(code)) return true;
  if (/^(data|newtype|type|class|instance|infix|infixl|infixr)\b/m.test(code)) return true;
  // A binding/signature at column 0 (e.g. `foo = …`, `foo :: …`, `foo x = …`).
  if (/^[A-Za-z_(][^\n]*?(::|=)/m.test(lines[0]) && !/^\s/.test(lines[0])) return true;
  return false;
}

// A ghci-style `let x = e` (no `in`): goes into main's do-block, not module
// scope (`let x = e` is illegal at the top level), and stays in scope for every
// later cell's statement.
function isLetCell(code) {
  return /^\s*let\b/.test(code) && !/\bin\b/.test(code);
}

// ── Dependency tracking (mirrors src/Sabela/Topo.hs) ─────────────────────────
// A cell depends on whichever cell first defines a name it uses; editing a cell
// only re-runs its transitive dependents, not the whole notebook.

// Top-level names a cell defines: bindings/signatures at column 0, data/newtype
// (+ their constructors), type/class names, and ghci `let x = …`.
function cellDefs(code) {
  const defs = new Set();
  for (const raw of code.split('\n')) {
    const line = raw.replace(/--.*$/, '');
    let m;
    if ((m = line.match(/^\s*(?:data|newtype)\s+([A-Z][\w']*)/))) {
      defs.add(m[1]);
      const rhs = line.slice(line.indexOf('=') + 1);
      if (line.includes('=')) for (const c of rhs.matchAll(/\b([A-Z][\w']*)\b/g)) defs.add(c[1]);
    } else if ((m = line.match(/^\s*(?:type|class)\s+([A-Z][\w']*)/))) {
      defs.add(m[1]);
    } else if ((m = line.match(/^\s*let\s+([a-z_][\w']*)\s*=/))) {
      defs.add(m[1]);
    } else if ((m = line.match(/^([a-z_][\w']*)\s*(?:::|[^=]*=(?!=))/))) {
      defs.add(m[1]);
    }
  }
  return defs;
}

// Every identifier a cell references (comments stripped).
function cellUses(code) {
  const uses = new Set();
  for (const m of code.replace(/--.*$/gm, '').matchAll(/[A-Za-z_][\w']*/g)) uses.add(m[0]);
  return uses;
}

// Dependency graph in BOTH directions. First cell to define a name owns it
// (first-wins, like Topo's defMap). revDeps: definer → its dependents (downstream,
// for "what to re-run on edit"); fwdDeps: cell → cells it depends on (upstream,
// for "what must be fed first" in the stateful REPL).
function buildDeps(cells) {
  const defMap = new Map();
  cells.forEach((c, i) => {
    for (const name of cellDefs(c.code)) if (!defMap.has(name)) defMap.set(name, i);
  });
  const revDeps = new Map();
  const fwdDeps = new Map();
  cells.forEach((c, i) => {
    for (const name of cellUses(c.code)) {
      const dk = defMap.get(name);
      if (dk !== undefined && dk !== i) {
        if (!revDeps.has(dk)) revDeps.set(dk, new Set());
        revDeps.get(dk).add(i);
        if (!fwdDeps.has(i)) fwdDeps.set(i, new Set());
        fwdDeps.get(i).add(dk);
      }
    }
  });
  return { revDeps, fwdDeps };
}

// Transitive dependents of `root` (including root itself).
function affectedFrom(root, revDeps) {
  const seen = new Set([root]);
  const queue = [root];
  while (queue.length) {
    for (const d of revDeps.get(queue.shift()) || []) {
      if (!seen.has(d)) {
        seen.add(d);
        queue.push(d);
      }
    }
  }
  return seen;
}

// Turn a cell's source into REPL input lines. The mhsi REPL is line-oriented and
// rejects a single definition split across physical lines (layout), but each
// column-0 clause is its own input and multi-clause defs accumulate — so:
//   - group indented continuation lines into their column-0 head (collapse layout,
//     which handles guards/where on separate lines),
//   - keep distinct column-0 lines as separate inputs (multi-clause functions),
//   - rewrite ghci `let x = e` to `x = e` (the REPL wants a bare DEFN).
// Known gaps: `do`/`case` blocks split across lines need `;` (collapse with a
// space breaks them) — such a cell errors visibly without killing the session.
function cellToReplLines(code) {
  const units = [];
  for (const raw of code.split('\n')) {
    const line = raw.replace(/\s+$/, '');
    if (!line.trim() || /^\s*--/.test(line)) continue;
    if (/^\s/.test(line) && units.length) units[units.length - 1] += ' ' + line.trim();
    else units.push(line.trim());
  }
  return units.map((u) => {
    const m = u.match(/^let\s+(.+)$/);
    return m && !/\bin\b/.test(u) ? m[1] : u;
  });
}
