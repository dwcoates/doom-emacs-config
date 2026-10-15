// redundancy-trace.mjs -- the vitest half of `testrun redundancy vitest`
// (testrun/internal/redundancy/vitestmeasure): a setup file that records,
// per test, the istanbul statements it reached, the files it read and how
// long it ran.
//
// The istanbul provider counts inside each instrumented module, into the
// global coverage store; this diffs that store's statement counters around
// every test and appends one JSON line per test to the file
// AGENT_REPL_REDUNDANCY_TRACE names:
//
//   {"file": "<test file>", "name": "<describe > ... > test>",
//    "state": "pass"|"fail"|"skip", "ms": <duration>, "hit": ["<src>:<id>"],
//    "read": ["<absolute path>"]}
//
// "read" is every file the test read through node:fs, so the measurement
// finds the tests that read a source Stryker mutates (redundancy-skip.mjs).
//
// It must run with --isolate (bin/coverage-honesty.mjs: an environment
// shared across files snapshots a module's counters against the first file
// only) and --maxWorkers=1, so no two tests' counters interleave.
import { afterEach, beforeEach } from "vitest";
import fs, { appendFileSync } from "node:fs";
import { syncBuiltinESMExports } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";
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

// THE READS ARE RECORDED ON node:fs ITSELF, once per worker process: the
// module is shared by every isolated test file the worker runs, so the
// wrappers live on it and record into the process, and each setup file run
// only resets the current test's set. syncBuiltinESMExports carries the
// wrappers to every `import { readFileSync } from "node:fs"`.
const READS = Symbol.for("agent-repl.redundancy.reads");

function readPath(p) {
  if (typeof p === "string") return path.resolve(p);
  if (p instanceof URL && p.protocol === "file:") return fileURLToPath(p);
  return null;
}

if (!process[READS]) {
  process[READS] = { current: null };
  const record = (p) => {
    const file = readPath(p);
    if (file && process[READS].current) process[READS].current.add(file);
  };
  for (const [owner, name] of [[fs, "readFileSync"], [fs, "readFile"], [fs.promises, "readFile"]]) {
    const real = owner[name];
    owner[name] = function (p, ...rest) {
      record(p);
      return real.call(this, p, ...rest);
    };
  }
  syncBuiltinESMExports();
}

let before = {};
let started = 0;

beforeEach(() => {
  before = snapshot();
  process[READS].current = new Set();
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
  const read = [...process[READS].current];
  process[READS].current = null;
  appendFileSync(out, JSON.stringify({ file: task.file.filepath, name: fullName(task), state, ms, hit, read }) + "\n");
});
