// layer-server.mjs -- ONE WARM VITEST FOR EVERY WEBAPP-LAYER AREA OF AN e2e RUN.
//
// A cold vitest child per area paid vitest's own start, the app's transform
// and its module graph's evaluation before running ~1-2s of tests: measured
// over twelve areas, 169s of child Duration was 22s of tests and 144s of
// transform plus collect. This driver creates vitest ONCE and runs each
// area's file on request, so every run after the first reuses the warm vite
// server and the un-isolated worker.
//
// e2e/WEBAPP-LAYER-SPEC.md's one-area-one-world-one-file ruling holds: each Go
// area still builds its OWN world and asks for exactly ONE file, which runs
// against that world alone. The world is handed in through vitest's
// provide/inject (`webappLayerWorld`), never through this process's
// environment, because the environment is shared by every run.
//
// PROTOCOL. Requests arrive on stdin, one JSON object per line:
//   {"file": "<name>.layer.test.ts", "world": {daemonUrl, workspaceId, workspaceDir, webappBuild}}
// Each answer is ONE stdout line prefixed RESULT_PREFIX, carrying
//   {"file", "ok", "tests", "failures": [...]}
// Everything else vitest prints is ordinary output for the requesting area's
// log. Runs are strictly one at a time: a request is read only after the
// previous answer is written.
//
// LIFETIME. stdin closing ends the process: the Go test binary owns the pipe,
// so a binary that exits for any reason -- or dies -- takes this driver with
// it, and no warm vitest can outlive its run.

import { createInterface } from "node:readline";
import { resolve } from "node:path";
import { createVitest } from "vitest/node";

const RESULT_PREFIX = "WEBAPP-LAYER-RESULT ";
// READY_PREFIX marks the one line the driver prints once its warm-up ran:
// {"ok", "warmMs", "failures"}. No area is sent before it.
const READY_PREFIX = "WEBAPP-LAYER-READY ";
const LAYER_DIR = resolve("test/webapp-layer");

const vitest = await createVitest("test", {
  config: "vitest.webapp-layer.config.ts",
  watch: false,
  run: true,
});
const project = vitest.getCoreWorkspaceProject();

/** Every failed test's name and first error, for the area's report. */
function failuresOf(task, out = []) {
  for (const child of task.tasks ?? []) failuresOf(child, out);
  if (task.result?.state === "fail") {
    const err = task.result.errors?.[0];
    out.push(`${task.name}: ${err?.message ?? "failed with no error message"}`);
  }
  return out;
}

/** The number of tests a file ran, so a file that ran none is never a pass. */
function testsOf(task) {
  if (task.type === "test") return 1;
  return (task.tasks ?? []).reduce((n, child) => n + testsOf(child), 0);
}

// THE WARM-UP RUNS FIRST, before any request is read: vite transforms and the
// worker evaluates the module graph every area imports (warmup.layer.test.ts),
// so the first area's run costs what every later one does instead of also
// paying the cold start against its own bound.
const warmStarted = Date.now();
const warmupPath = resolve(LAYER_DIR, "warmup.layer.test.ts");
await vitest.start([warmupPath]);
const warmed = vitest.state.getFiles([warmupPath])[0];
const warmFailures = warmed === undefined ? ["vitest ran no warm-up file"] : failuresOf(warmed);
const warmOk = warmed?.result?.state === "pass" && warmFailures.length === 0;
process.stdout.write(READY_PREFIX + JSON.stringify({ ok: warmOk, warmMs: Date.now() - warmStarted, failures: warmFailures }) + "\n");
if (!warmOk) {
  await vitest.close();
  process.exit(1);
}

const lines = createInterface({ input: process.stdin });
for await (const line of lines) {
  if (line.trim() === "") continue;
  const request = JSON.parse(line);
  const path = resolve(LAYER_DIR, request.file);
  project.provide("webappLayerWorld", request.world);
  await vitest.rerunFiles([path]);
  const file = vitest.state.getFiles([path])[0];
  const answer = { file: request.file, ok: false, tests: 0, failures: [] };
  if (file === undefined) {
    answer.failures.push(`vitest ran no file for ${request.file}`);
  } else {
    answer.tests = testsOf(file);
    answer.failures = failuresOf(file);
    if (file.result?.state === "fail" && answer.failures.length === 0) {
      const err = file.result.errors?.[0];
      answer.failures.push(`the file failed: ${err?.message ?? "with no error message"}`);
    }
    if (answer.tests === 0) answer.failures.push(`${request.file} ran no tests`);
    answer.ok = file.result?.state === "pass" && answer.failures.length === 0;
  }
  process.stdout.write(RESULT_PREFIX + JSON.stringify(answer) + "\n");
}
await vitest.close();
