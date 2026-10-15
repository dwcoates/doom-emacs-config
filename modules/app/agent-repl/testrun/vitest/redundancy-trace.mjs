// redundancy-trace.mjs -- the vitest half of `testrun redundancy vitest`
// (testrun/internal/redundancy/vitestmeasure): a setup file that records,
// per test, the istanbul statements it reached and how long it ran.
//
// The istanbul provider counts inside each instrumented module, into the
// global coverage store; this diffs that store's statement counters around
// every test and appends one JSON line per test to the file
// AGENT_REPL_REDUNDANCY_TRACE names:
//
//   {"file": "<test file>", "name": "<describe > ... > test>",
//    "state": "pass"|"fail"|"skip", "ms": <duration>, "hit": ["<src>:<id>"]}
//
// It must run with --isolate (bin/coverage-honesty.mjs: an environment
// shared across files snapshots a module's counters against the first file
// only) and --maxWorkers=1, so no two tests' counters interleave.
import { afterEach, beforeEach } from "vitest";
import { appendFileSync } from "node:fs";
import { fullName } from "./redundancy-names.mjs";

const out = process.env.AGENT_REPL_REDUNDANCY_TRACE;
if (!out) {
  throw new Error("redundancy-trace.mjs: AGENT_REPL_REDUNDANCY_TRACE names no trace file");
}

// The istanbul provider's store (@vitest/coverage-istanbul's coverage
// variable). Its absence means the run is not instrumented, which would
// record every test as reaching nothing: refused, never recorded.
const STORE = "__VITEST_COVERAGE__";

function store() {
  const s = globalThis[STORE];
  if (!s) {
    throw new Error(`redundancy-trace.mjs: no ${STORE}; run with --coverage.enabled --coverage.provider=istanbul`);
  }
  return s;
}

function snapshot() {
  const snap = {};
  for (const [file, data] of Object.entries(store())) {
    snap[file] = { ...data.s };
  }
  return snap;
}

let before = {};
let started = 0;

beforeEach(() => {
  before = snapshot();
  started = performance.now();
});

afterEach((ctx) => {
  const ms = performance.now() - started;
  const hit = [];
  for (const [file, data] of Object.entries(store())) {
    const was = before[file] || {};
    for (const [id, n] of Object.entries(data.s)) {
      if (n > (was[id] || 0)) hit.push(`${file}:${id}`);
    }
  }
  const task = ctx.task;
  const state = task.result?.state ?? "pass";
  appendFileSync(out, JSON.stringify({ file: task.file.filepath, name: fullName(task), state, ms, hit }) + "\n");
});
