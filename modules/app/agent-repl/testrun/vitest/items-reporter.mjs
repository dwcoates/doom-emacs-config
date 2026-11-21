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
// (pass, fail, skip or todo), "seconds", "start" (the unix seconds it started
// at, 0 for a test that never ran) }, ...]. The start places the test in its
// chunk's CPU over time, for its share (testtimes.AttributeCPU).

import { writeFileSync } from "node:fs";

/** A test case's state as the runner names it (testtimes.ParseVitest). */
function stateOf(testCase) {
  const state = testCase.result().state;
  switch (state) {
    case "passed":
      return "pass";
    case "failed":
      return "fail";
    case "skipped":
      return testCase.options.mode === "todo" ? "todo" : "skip";
    case "pending":
      // A test still pending when the run ended never ran.
      return "skip";
  }
  throw new Error(`items-reporter: ${testCase.fullName} ended in the unknown state ${state}`);
}

export default class ItemsReporter {
  onTestRunEnd(testModules) {
    const target = process.env.AGENT_REPL_TESTRUN_ITEMS;
    if (!target) {
      throw new Error("items-reporter: AGENT_REPL_TESTRUN_ITEMS names no output file");
    }
    const testsTarget = process.env.AGENT_REPL_TESTRUN_TESTS;
    if (!testsTarget) {
      throw new Error("items-reporter: AGENT_REPL_TESTRUN_TESTS names no output file");
    }
    const out = {};
    const tests = [];
    for (const m of testModules ?? []) {
      const d = m.diagnostic();
      out[m.moduleId] =
        (d.prepareDuration + d.environmentSetupDuration + d.setupDuration + d.collectDuration + d.duration) / 1000;
      for (const testCase of m.children.allTests()) {
        const t = testCase.diagnostic();
        tests.push({
          file: m.moduleId,
          name: testCase.fullName,
          state: stateOf(testCase),
          seconds: (t?.duration ?? 0) / 1000,
          start: (t?.startTime ?? 0) / 1000,
        });
      }
    }
    writeFileSync(target, JSON.stringify(out));
    writeFileSync(testsTarget, JSON.stringify(tests));
  }
}
