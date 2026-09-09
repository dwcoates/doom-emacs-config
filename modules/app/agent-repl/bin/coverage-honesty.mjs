#!/usr/bin/env node
/**
 * coverage-honesty.mjs -- fail if this package's per-file coverage numbers are
 * not a measurement.
 *
 * WHY THIS EXISTS, and it is a bug that was shipped, not a precaution.
 *
 * `@vitest/coverage-v8@2.1.9` collects a RAW V8 coverage per test-file window
 * and merges those with `mergeProcessCovs` BEFORE remapping them through the
 * source maps (see `@vitest/coverage-v8/dist/provider.js`, the
 * `mergeAndTransformCoverage` path). A module compiled in more than one window
 * therefore appears under more than one script entry, and the merge keeps one
 * contributor's ranges rather than the sum: measured on this repo,
 * webapp/src/scroll.ts read 100% statements when test/scroll.test.ts ran alone
 * and 47.71% with test/feed added, and eleven webapp files plus five shim files
 * moved between two runs that differed only in test-FILE order. A number that
 * moves without the code moving is not a measurement, so the provider was
 * changed to istanbul, which instruments at transform time and counts inside
 * the module rather than reconstructing attribution afterwards.
 *
 * `--isolate` does NOT repair the v8 provider (it was measured with isolation
 * on), and it IS required by the istanbul provider: with one environment shared
 * across a worker's files, a module instantiated once has its counters snapshot
 * and reset against the first file, and later files report zero for it --
 * src/main.ts, src/feed/cards/hook.ts and src/panels/refused.ts all read 0%
 * un-isolated against 100%, 100% and 98.64% isolated. So the coverage script
 * pays for isolation and `npm test` does not.
 *
 * This script is what stops a later provider bump reintroducing any of that
 * silently. It checks two invariants against the package's REAL `npm run
 * coverage` command, never a reconstruction of it:
 *
 *   ORDER-INDEPENDENCE -- three runs, one in the natural order and two with
 *   `--sequence.shuffle.files` under different seeds, must agree on every
 *   file's covered and total counts for lines, statements, branches and
 *   functions. This is strictly stronger than repeating the same run three
 *   times, which cannot see an attribution that depends on which files share a
 *   process.
 *
 *   MONOTONICITY -- a probe source file's totals must be identical whether the
 *   whole suite runs or only the probe's own test file, and the full run must
 *   cover at least what that one test file covers alone. Adding test files can
 *   only ever reveal more of a module, never less; the v8 merge broke exactly
 *   this, and it is stated as a subset rather than an equality because a probe
 *   may legitimately share its module with another test file.
 *
 * Usage (from a package directory), with each probe naming a test-file filter
 * and the source file it is the principal exercise of:
 *   node <module-root>/bin/coverage-honesty.mjs \
 *     --probe test/scroll.test.ts=src/scroll.ts \
 *     --probe test/feed/cards/hook.test.ts=src/feed/cards/hook.ts
 */
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

const METRICS = ["lines", "statements", "branches", "functions"];
const SHUFFLE_SEEDS = [20260909, 951753];

function parseArgs(argv) {
  const probes = [];
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] !== "--probe") {
      throw new Error(`unrecognized argument: ${argv[i]}`);
    }
    const spec = argv[++i];
    if (spec === undefined) throw new Error("--probe needs a <test>=<src> pair");
    const eq = spec.indexOf("=");
    if (eq <= 0 || eq === spec.length - 1) {
      throw new Error(`--probe wants <test file>=<src file>, got: ${spec}`);
    }
    probes.push({ test: spec.slice(0, eq), src: spec.slice(eq + 1) });
  }
  if (probes.length === 0) throw new Error("at least one --probe is required");
  return probes;
}

/**
 * Run the package's own `npm run coverage`, so this check can never drift from
 * the command whose numbers are reported. A failing run is fatal: a coverage
 * report from a red suite says nothing about attribution.
 */
function runCoverage(label, extraArgs) {
  const dir = mkdtempSync(join(tmpdir(), "coverage-honesty-"));
  const args = ["run", "coverage", "--", `--coverage.reportsDirectory=${dir}`,
    "--coverage.reporter=json-summary", ...extraArgs];
  const res = spawnSync("npm", args, { stdio: ["ignore", "pipe", "pipe"], encoding: "utf8" });
  if (res.error) {
    rmSync(dir, { recursive: true, force: true });
    throw new Error(`${label}: could not run npm: ${res.error.message}`);
  }
  if (res.status !== 0) {
    process.stderr.write(res.stdout ?? "");
    process.stderr.write(res.stderr ?? "");
    rmSync(dir, { recursive: true, force: true });
    throw new Error(`${label}: the coverage run failed (exit ${res.status}); fix the suite first`);
  }
  let summary;
  try {
    summary = JSON.parse(readFileSync(join(dir, "coverage-summary.json"), "utf8"));
  } catch (err) {
    throw new Error(`${label}: no readable coverage-summary.json in ${dir}: ${err.message}`);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
  const byFile = new Map();
  for (const [key, value] of Object.entries(summary)) {
    if (key !== "total") byFile.set(key, value);
  }
  if (byFile.size === 0) throw new Error(`${label}: the coverage report named no files`);
  return byFile;
}

function fileEntry(byFile, srcPath, label) {
  const absolute = resolve(process.cwd(), srcPath);
  const entry = byFile.get(absolute);
  if (entry === undefined) {
    throw new Error(`${label}: ${srcPath} is absent from the coverage report; ` +
      `pick a probe file that carries executable statements`);
  }
  return entry;
}

function compare(a, b, labelA, labelB) {
  const differences = [];
  for (const key of new Set([...a.keys(), ...b.keys()])) {
    const x = a.get(key);
    const y = b.get(key);
    if (x === undefined || y === undefined) {
      differences.push(`${key}: reported by ${x === undefined ? labelB : labelA} only`);
      continue;
    }
    for (const metric of METRICS) {
      if (x[metric].covered !== y[metric].covered || x[metric].total !== y[metric].total) {
        differences.push(`${key}: ${metric} ${x[metric].covered}/${x[metric].total} in ` +
          `${labelA} against ${y[metric].covered}/${y[metric].total} in ${labelB}`);
      }
    }
  }
  return differences;
}

function main() {
  const probes = parseArgs(process.argv.slice(2));

  const runs = [{ label: "natural order", byFile: runCoverage("natural order", []) }];
  for (const seed of SHUFFLE_SEEDS) {
    const label = `file order seed ${seed}`;
    runs.push({
      label,
      byFile: runCoverage(label, ["--sequence.shuffle.files", `--sequence.seed=${seed}`]),
    });
  }

  const failures = [];
  for (let i = 1; i < runs.length; i++) {
    const differences = compare(runs[0].byFile, runs[i].byFile, runs[0].label, runs[i].label);
    if (differences.length > 0) {
      failures.push(`per-file counts moved with test-file order (${differences.length} file(s)):\n  ` +
        differences.join("\n  "));
    }
  }

  for (const probe of probes) {
    const label = `probe ${probe.test}`;
    const alone = runCoverage(label, [probe.test]);
    const soloEntry = fileEntry(alone, probe.src, `${label} alone`);
    const fullEntry = fileEntry(runs[0].byFile, probe.src, "the full run");
    for (const metric of METRICS) {
      if (soloEntry[metric].total !== fullEntry[metric].total) {
        failures.push(`${probe.src}: the full run counts ${fullEntry[metric].total} ` +
          `${metric} but ${probe.test} alone counts ${soloEntry[metric].total}; ` +
          `one module cannot have two sizes`);
        continue;
      }
      if (soloEntry[metric].covered > fullEntry[metric].covered) {
        failures.push(`${probe.src}: ${metric} reads ` +
          `${fullEntry[metric].covered}/${fullEntry[metric].total} in the full run but ` +
          `${soloEntry[metric].covered}/${soloEntry[metric].total} when only ${probe.test} ` +
          `runs; running MORE tests lost coverage`);
      }
    }
  }

  if (failures.length > 0) {
    process.stderr.write("coverage-honesty: this package's per-file numbers are not a measurement.\n\n");
    for (const failure of failures) process.stderr.write(`${failure}\n\n`);
    process.stderr.write("Read the header of bin/coverage-honesty.mjs: this is the v8-provider\n" +
      "merge defect, or its shape, coming back. Do not adjust the check to pass.\n");
    process.exit(1);
  }

  process.stdout.write(`coverage-honesty: ${runs.length} runs agree on all ` +
    `${runs[0].byFile.size} files, and ${probes.length} probe file(s) lose nothing ` +
    `when the rest of the suite joins them.\n`);
}

try {
  main();
} catch (err) {
  process.stderr.write(`coverage-honesty: ${err.message}\n`);
  process.exit(1);
}
