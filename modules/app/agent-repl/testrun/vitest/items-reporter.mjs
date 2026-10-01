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

import { writeFileSync } from "node:fs";

export default class ItemsReporter {
  onFinished(files) {
    const target = process.env.AGENT_REPL_TESTRUN_ITEMS;
    if (!target) {
      throw new Error("items-reporter: AGENT_REPL_TESTRUN_ITEMS names no output file");
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
  }
}
