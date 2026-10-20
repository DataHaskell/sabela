// Typing, formatting and ordering for the table view, checked without a
// browser: everything in grid-model.js is a function of its payload.
//
//   node tools/grid-model-test.mjs
import { readFileSync } from 'node:fs';

const source = readFileSync(
  new URL('../static/src/widgets/grid-model.js', import.meta.url),
  'utf8'
);
const names = [
  'sabelaGridKind',
  'sabelaGridColumns',
  'sabelaGridFormat',
  'sabelaGridOrder',
  'sabelaGridStats',
  'sabelaGridNormalize',
];
const model = new Function(`${source}\nreturn {${names.join(',')}};`)();

const failures = [];
const eq = (name, got, want) => {
  const a = JSON.stringify(got);
  const b = JSON.stringify(want);
  if (a !== b) failures.push(`${name}\n  expected ${b}\n  got      ${a}`);
};

const rows = [
  ['Bulawayo', '27.5', 'True', '3'],
  ['Harare', '24.125', 'False', '-1'],
  ['Gweru', '', 'True', '12'],
];
const raw = { cols: ['city', 'temp', 'wet', 'n'], rows };
const cols = model.sabelaGridColumns(raw);

eq('a text column is text', cols[0].kind, 'str');
eq('a decimal column is a number', cols[1].kind, 'num');
eq('True and False make a boolean column', cols[2].kind, 'bool');
eq('whole numbers make an integer column', cols[3].kind, 'int');
eq('a blank does not vote on the type', cols[1].stats.missing, 1);
eq('an all-distinct text column is a key, not categories', cols[0].stats.categorical, false);
eq(
  'a repeating text column is categorical',
  model.sabelaGridColumns({
    cols: ['k'],
    rows: [['a'], ['b'], ['a']],
  })[0].stats.categorical,
  true
);
eq('decimals come from the widest value', cols[1].decimals, 3);

eq('a number carries its column decimals', model.sabelaGridFormat('27.5', cols[1], 200), '27.500');
eq('a narrow column drops the decimals', model.sabelaGridFormat('27.5', cols[1], 60), '28');
eq('a blank stays blank', model.sabelaGridFormat('', cols[1], 200), '');
eq('an integer keeps its shape', model.sabelaGridFormat('12', cols[3], 200), '12');

const order = (sort, query) => model.sabelaGridOrder(raw, cols, sort, query);
eq('unsorted is source order', order(null, ''), [0, 1, 2]);
eq('a numeric sort orders by value', order({ col: 1, desc: false }, ''), [1, 0, 2]);
eq('a descending sort reverses it', order({ col: 1, desc: true }, ''), [0, 1, 2]);
eq('missing values stay last descending', order({ col: 1, desc: true }, '').at(-1), 2);
eq('missing values sort last', order({ col: 1, desc: false }, '').at(-1), 2);
eq('free text matches any cell', order(null, 'harare'), [1]);
eq('a column expression compares numerically', order(null, 'temp > 25'), [0]);
eq('an expression on a missing value drops the row', order(null, 'temp < 100'), [0, 1]);
eq('an unknown column falls back to free text', order(null, 'nope > 1'), []);

const maybes = model.sabelaGridNormalize({
  cols: ['score', 'label', 'nested'],
  rows: [
    ['Just 27.5', 'Just "Harare"', 'Just (Just 3)'],
    ['Nothing', 'Nothing', 'Just (-4)'],
    ['Just (-1.25)', 'Just "a \\"b\\""', 'null'],
  ],
});
eq('Nothing is a blank cell', maybes.rows[1][0], '');
eq('Just unwraps to its payload', maybes.rows[0][0], '27.5');
eq('a shown string loses its quotes', maybes.rows[0][1], 'Harare');
eq('an escaped string is unescaped', maybes.rows[2][1], 'a "b"');
eq('a parenthesised negative is bare', maybes.rows[0][2], '3');
eq('a nested Just unwraps fully', maybes.rows[1][2], '-4');
const maybeCols = model.sabelaGridColumns(maybes);
eq('a Maybe Double column is numeric', maybeCols[0].kind, 'num');
eq('a Maybe Int column is an integer column', maybeCols[2].kind, 'int');
eq('a Maybe column counts its Nothings as missing', maybeCols[0].stats.missing, 1);
eq('a Maybe column still gets a histogram', maybeCols[0].stats.bins.length > 0, true);
eq('a column without a sent type has none', maybeCols[0].type, '');
const typed = model.sabelaGridColumns(
  model.sabelaGridNormalize({
    cols: ['score', 'name'],
    types: ['Maybe Double', 'Text'],
    rows: [['Just 1.5', 'a']],
  })
);
eq('a sent type names the column', typed[0].type, 'Maybe Double');
eq('a sent type survives normalisation', typed[1].type, 'Text');
eq('a sent type does not change the inferred kind', typed[0].kind, 'num');
eq(
  'a Maybe column sorts by its payload',
  model.sabelaGridOrder(maybes, maybeCols, { col: 0, desc: false }, ''),
  [2, 0, 1]
);

if (failures.length) {
  console.error(`✗ ${failures.length} grid-model check(s) failed:\n\n${failures.join('\n\n')}`);
  process.exit(1);
}
console.log('✓ grid-model: all checks passed');
