// Refuses a run that did not come through bin/background.sh: tests only ever
// run at background priority (see that script and require-background.mjs).
import "../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
import unit from "./vitest.config";

/**
 * THE RULE SUITE'S CONFIG (testrun's `webapp-rules` suite): the tests in
 * `rules/`, each reading the webapp's own source (`src/`, its stylesheets,
 * the page shell, the test code) to hold it to a structural rule. Every branch
 * runs this suite before its merge is admitted to the queue, so it holds
 * only rules and stays cheap; `rules/source-reads.test.ts` fails a
 * source-reading test anywhere else.
 *
 * It is the unit config (its environment, setup files, aliases and bounds)
 * with its own files: the unit config excludes `rules/`, and this one runs
 * nothing else. Its tests run no source, so a coverage run never instruments
 * them (testrun's `roster.Suite.Config`).
 */
export default defineConfig({
  ...unit,
  test: {
    ...unit.test,
    include: ["rules/**/*.test.ts"],
    exclude: ["**/node_modules/**", "**/dist/**"],
  },
});
