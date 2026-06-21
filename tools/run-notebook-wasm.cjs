#!/usr/bin/env node
// Run a Sabela notebook's Haskell cells through the real MicroHs WASM engine
// (mhs-embed.js) — no server, no GHCi. Mirrors the in-browser runtime's cell
// assembly (static/src/wasm-run/js/04-program.js): declaration cells are hoisted
// to module scope, bare-expression cells are printed, each result delimited by
// SABELA_MARKER lines so stdout splits back into per-cell output.
//
//   node tools/run-notebook-wasm.cjs <source.md> [--cells N] [--engine path]
//
// The engine artifact (mhs-embed.js) is resolved from --engine, else
// static/mhs-embed.js, else /tmp/mhs-embed.js.

const fs = require('node:fs');
const path = require('node:path');

function arg(flag) {
  const i = process.argv.indexOf(flag);
  return i >= 0 ? process.argv[i + 1] : null;
}

const src = process.argv[2];
if (!src || src.startsWith('--')) {
  console.error('usage: node tools/run-notebook-wasm.cjs <source.md> [--cells N] [--engine path]');
  process.exit(2);
}
const limit = arg('--cells') ? parseInt(arg('--cells'), 10) : Infinity;
const engine =
  arg('--engine') ||
  [path.resolve(__dirname, '../static/mhs-embed.js'), '/tmp/mhs-embed.js'].find((p) =>
    fs.existsSync(p)
  );
if (!engine || !fs.existsSync(engine)) {
  console.error('mhs-embed.js not found. Pass --engine <path> or place it at static/mhs-embed.js');
  process.exit(2);
}

// ── parse + assemble (mirrors 04-program.js) ────────────────────────────────
const md = fs.readFileSync(src, 'utf8');
const cells = [...md.matchAll(/```(?:haskell|hs)\s*\n([\s\S]*?)```/g)]
  .map((m) => m[1].replace(/\s+$/, ''))
  .filter((c) => c.trim());

const isDecl = (code) => {
  const lines = code.split('\n').filter((l) => l.trim() && !/^\s*--/.test(l));
  if (!lines.length) return true;
  if (/^\s*import\b/m.test(code)) return true;
  if (/^(data|newtype|type|class|instance|infix|infixl|infixr)\b/m.test(code)) return true;
  if (/^[A-Za-z_(][^\n]*?(::|=)/m.test(lines[0]) && !/^\s/.test(lines[0])) return true;
  return false;
};
const hsString = (s) => '"' + s.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
const marker = (n) => '---SABELA_MARKER_' + n + '---';
const printStmt = (e) =>
  /^\s*(do\b|putStr|putStrLn|print\b|mapM_|forM_)/.test(e) ? e : 'print (' + e + ')';
const indent = (s) =>
  s
    .split('\n')
    .map((l) => '  ' + l)
    .join('\n');

const n = Math.min(cells.length, limit);
const imports = [];
const decls = [];
const body = [];
for (let i = 0; i < n; i++) {
  body.push('  putStrLn ' + hsString(marker(i)));
  if (/^\s*let\b/.test(cells[i]) && !/\bin\b/.test(cells[i])) {
    // ghci-style `let x = e` → a do-let in main: legal here and in scope for
    // every later cell's statement (top-level scope would reject it).
    body.push(indent(cells[i].trim()));
  } else if (isDecl(cells[i])) {
    for (const line of cells[i].split('\n')) {
      if (/^\s*import\b/.test(line)) imports.push(line.trim());
      else decls.push(line);
    }
  } else {
    body.push(indent(printStmt(cells[i].trim())));
  }
}
const program = [
  'module Main where',
  '',
  imports.join('\n'),
  '',
  decls.join('\n'),
  '',
  'main :: IO ()',
  'main = do',
  body.join('\n'),
  '  putStrLn ' + hsString(marker(n)),
  '',
].join('\n');

// ── run through the engine (the locked recipe) ──────────────────────────────
process.argv = [process.argv[0], './this.program', '-r', 'Main.hs'];
const out = [];
const push = (c) => {
  if (c !== null) out.push(String.fromCharCode(c));
};
const Module = {
  preRun: [
    function () {
      Module.FS.init(() => null, push, push);
      Module.FS.chdir('/home/web_user');
      Module.FS.writeFile('Main.hs', program);
    },
  ],
  print: (t) => out.push(t + '\n'),
  printErr: (t) => out.push('[err] ' + t + '\n'),
};

process.on('exit', () => {
  const text = out.join('').replace(/^Warning: cannot find config file:.*\n/m, '');
  const chunks = text.split(/---SABELA_MARKER_\d+---\n?/);
  console.log(`\n${path.basename(src)} — ${n}/${cells.length} cells via MicroHs WASM\n`);
  for (let i = 0; i < n; i++) {
    const result = (chunks[i + 1] || '').trim();
    const code = cells[i].replace(/\n/g, ' ⏎ ').slice(0, 48);
    console.log(`  [${String(i).padStart(3)}] ${code.padEnd(50)} => ${result || '·'}`);
  }
});

const embedSrc = fs.readFileSync(engine, 'utf8');
const fn = new Function(
  'require',
  'module',
  'exports',
  '__dirname',
  '__filename',
  'process',
  'Module',
  embedSrc
);
fn(require, { exports: {} }, {}, path.dirname(engine), engine, process, Module);
