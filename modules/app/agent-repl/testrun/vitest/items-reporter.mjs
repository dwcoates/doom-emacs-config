// items-reporter.mjs — the test runner's (testrun) per-file cost reporter for
// one vitest chunk.
//
// A file's cost is EVERYTHING the file adds to its chunk: preparing it,
// loading its environment, its setup files, collecting it (importing its
// whole module graph, which under coverage and isolation dominates) and
// running its tests. vitest's JSON reporter times only the last of those, so
// the planner would read the rest as per-chunk overhead and split the suite
// wrongly. This reporter writes { "<absolute file>": seconds, ... } to the
// path in AGENT_REPL_TESTRUN_ITEMS when the run finishes.
//
// It also writes every individual test's time, for the metrics store
// (testrun/internal/testtimes), to the path in AGENT_REPL_TESTRUN_TESTS:
// [{ "file", "name" (its suites and its own name, joined by " > "), "state"
// (pass, fail, skip or todo), "seconds" }, ...].

import { writeFileSync } from "node:fs";

/** Every test under TASKS, its suites' names before its own, into OUT. */
function collectTests(file, tasks, path, out) {
  for (const task of tasks ?? []) {
    const name = [...path, task.name];
    if (task.type === "suite") {
      collectTests(file, task.tasks, name, out);
      continue;
    }
    const state = task.result?.state ?? task.mode;
    out.push({ file, name: name.join(" > "), state, seconds: (task.result?.duration ?? 0) / 1000 });
  }
}

export default class ItemsReporter {
  onFinished(files) {
    const target = process.env.AGENT_REPL_TESTRUN_ITEMS;
    if (!target) {
      throw new Error("items-reporter: AGENT_REPL_TESTRUN_ITEMS names no output file");
    }
    const testsTarget = process.env.AGENT_REPL_TESTRUN_TESTS;
    if (!testsTarget) {
      throw new Error("items-reporter: AGENT_REPL_TESTRUN_TESTS names no output file");
    }
    const out = {};
    for (const f of files ?? []) {
      const ms =
        (f.prepareDuration ?? 0) +
        (f.environmentLoad ?? 0) +
        (f.setupDuration ?? 0) +
        (f.collectDuration ?? 0) +
        (f.result?.duration ?? 0);
      out[f.filepath] = ms / 1000;
    }
    writeFileSync(target, JSON.stringify(out));
    const tests = [];
    for (const f of files ?? []) collectTests(f.filepath, f.tasks, [], tests);
    writeFileSync(testsTarget, JSON.stringify(tests));
  }
}
