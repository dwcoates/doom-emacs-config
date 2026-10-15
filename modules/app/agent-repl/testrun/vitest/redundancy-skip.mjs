// redundancy-skip.mjs -- the Stryker half of `testrun redundancy vitest`
// (testrun/internal/redundancy/vitestmeasure): a setup file that skips, in
// Stryker's runs, every test the traced runs saw read a source file Stryker
// mutates. Stryker mutates in place, so such a test reads the mutant
// switches' text, not the source: it would fail the dry run, and under each
// mutant it would "kill" by text, not behavior. The measurement pins those
// tests instead.
//
// AGENT_REPL_REDUNDANCY_SKIP names a JSON file of [{"file", "name"}], the
// test file's absolute path and its fullName.
import { beforeEach } from "vitest";
import { readFileSync } from "node:fs";
import { fullName } from "./redundancy-names.mjs";

const list = process.env.AGENT_REPL_REDUNDANCY_SKIP;
if (!list) {
  throw new Error("redundancy-skip.mjs: AGENT_REPL_REDUNDANCY_SKIP names no skip list");
}
const skip = new Set(JSON.parse(readFileSync(list, "utf8")).map((t) => `${t.file}\0${t.name}`));

beforeEach((ctx) => {
  if (skip.has(`${ctx.task.file.filepath}\0${fullName(ctx.task)}`)) {
    ctx.skip();
  }
});
