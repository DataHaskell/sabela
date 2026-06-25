export const meta = {
  name: 'siza-live-eval',
  description: 'Drive gpt-oss:20b through siza on dataset/plot/repair tasks, capture transcripts, analyse',
  phases: [
    { title: 'Run', detail: 'one task per agent, serial — single local model' },
    { title: 'Analyse', detail: 'read each transcript, extract what the model did' },
    { title: 'Report', detail: 'synthesise sessions/REPORT.md' },
  ],
}

const INPUT = typeof args === 'string' ? JSON.parse(args) : args || {}
const REPO = INPUT.repo
const SESSIONS = INPUT.sessions
const RUNNER = `${REPO}/eval/neuro-symbolic/run-one.sh`
const DEADLINE = INPUT.deadline || 360
const TOOL_TIMEOUT = INPUT.toolTimeout || 300
const MODEL = INPUT.model || 'gpt-oss:20b'
const TASKS = INPUT.tasks

const RUN_SCHEMA = {
  type: 'object',
  properties: {
    task: { type: 'string' },
    passed: { type: 'boolean' },
    verdict: { type: 'string' },
    turns: { type: 'integer' },
    toolCalls: { type: 'integer' },
    seconds: { type: 'number' },
    stopped: { type: 'string' },
    transcriptExists: { type: 'boolean' },
    transcriptBytes: { type: 'integer' },
    rawResult: { type: 'string' },
    error: { type: 'string' },
  },
  required: ['task', 'passed', 'stopped', 'transcriptExists'],
  additionalProperties: false,
}

const ANALYSIS_SCHEMA = {
  type: 'object',
  properties: {
    task: { type: 'string' },
    passed: { type: 'boolean' },
    setDependency: { type: 'boolean' },
    dependencies: { type: 'string' },
    browsedApi: { type: 'boolean' },
    usedIntendedIdiom: { type: 'boolean' },
    fellBackToBadHaskell: { type: 'boolean' },
    repaired: { type: 'boolean' },
    repairRounds: { type: 'integer' },
    failureMode: { type: 'string' },
    summary: { type: 'string' },
  },
  required: ['task', 'setDependency', 'usedIntendedIdiom', 'summary'],
  additionalProperties: false,
}

phase('Run')
const runs = []
for (const t of TASKS) {
  const cmd = `SIZA_EVAL_MODEL=${MODEL} SIZA_EVAL_DEADLINE_SECS=${DEADLINE} SABELA_TOOL_TIMEOUT=${TOOL_TIMEOUT} bash ${RUNNER} ${t.id} ${SESSIONS} ${t.port}`
  const r = await agent(
    `Run exactly ONE shell command with the Bash tool, set its timeout to 590000, and do not run anything else before it:\n\n    ${cmd}\n\n` +
    `This starts a fresh Sabela server, points gpt-oss:20b at it for task "${t.id}" (${t.kind}), grades the outcome, kills the server, and writes a session transcript to ${SESSIONS}/${t.id}.md. ` +
    `The command prints ONE JSON line to stdout with fields task, passed, verdict, turns, tool_calls, seconds, stopped, final, evidence. ` +
    `After it returns, run \`wc -c ${SESSIONS}/${t.id}.md\` to confirm the transcript was written. ` +
    `Then return: the parsed fields (task, passed, verdict, turns, toolCalls, seconds, stopped), whether the transcript file exists (transcriptExists) and its byte size (transcriptBytes), the raw JSON line (rawResult), and any error text in error (empty string if none). If the JSON line never appeared, set passed=false, stopped to your best guess, and put what went wrong in error.`,
    { label: `run:${t.id}`, phase: 'Run', schema: RUN_SCHEMA },
  )
  runs.push(r ? { ...r, kind: t.kind } : { task: t.id, kind: t.kind, passed: false, stopped: 'agent-died', transcriptExists: false, error: 'execution agent returned null' })
  const last = runs[runs.length - 1]
  log(`${t.id}: ${last.passed ? 'PASS' : 'fail'} (stopped=${last.stopped})`)
}

phase('Analyse')
const analyses = await parallel(
  runs.map((r) => () =>
    agent(
      `Read the session transcript at ${SESSIONS}/${r.task}.md with the Read tool. It records a gpt-oss:20b model driving a reactive Haskell notebook through tools on task "${r.task}" (${r.kind}). ` +
      `Assess CONCRETELY, quoting the transcript where it helps:\n` +
      `- setDependency: did it add a \`-- cabal:\` build-depends line? put the packages in dependencies.\n` +
      `- browsedApi: did it inspect the real API (a ghci_query browse call, or a discover tool message) before writing against it?\n` +
      `- usedIntendedIdiom: did it use the notebook idiom (DataFrame for data, Granite + displaySvg for plots, top-level bindings), as opposed to fellBackToBadHaskell (main = do, writeFile, manual CSV parsing, an invented plot API)?\n` +
      `- repaired / repairRounds: did it hit compile errors and fix them, and roughly how many rounds; note whether the diagnostics or health-gate message helped.\n` +
      `- failureMode: if it failed, the precise reason (empty string if it passed).\n` +
      `- summary: 3-5 sentences on what the model actually did.\n` +
      `Harness result for context: passed=${r.passed}, stopped=${r.stopped}, turns=${r.turns}.`,
      { label: `analyse:${r.task}`, phase: 'Analyse', schema: ANALYSIS_SCHEMA },
    ).then((a) => (a ? { ...a, kind: r.kind } : null)),
  ),
)

phase('Report')
const clean = analyses.filter(Boolean)
const report = await agent(
  `Write a concise markdown report to ${SESSIONS}/REPORT.md with the Write tool, summarising this live gpt-oss:20b run over ${runs.length} siza notebook tasks (dataset-loading, plotting, and fixing broken code).\n\n` +
  `Per-task run results (JSON): ${JSON.stringify(runs)}\n\n` +
  `Per-task transcript analyses (JSON): ${JSON.stringify(clean)}\n\n` +
  `Structure the report as: (1) a summary table with columns task, passed, stopped, set-dep, browsed-API, used-idiom, fell-back, repair-rounds; (2) one short paragraph per task on what the model actually did and how it ended; (3) an overall verdict on whether the install -> discover -> repair loop is working, what idioms the model reached for, and where it breaks. Be honest and specific about failures; do not inflate. ` +
  `After writing the file, return a 6-8 line plain summary for the operator.`,
  { label: 'synthesis', phase: 'Report' },
)

return { runs, analyses: clean, report }
