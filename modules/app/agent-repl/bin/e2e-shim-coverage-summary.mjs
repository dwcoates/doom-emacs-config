// Summarize the shim's own source coverage out of a c8 json-summary report.
//
// c8 remaps the v8 coverage of the esbuild BUNDLE through its source map, so
// the report carries one entry per original file: our TypeScript under
// agent-shim/**/src, plus every dependency esbuild inlined. Only ours is code
// this suite measures, so those are what this sums.
//
// Usage: node e2e-shim-coverage-summary.mjs <c8-reports-dir>
import { readFileSync } from "node:fs";
import path from "node:path";

const reportsDir = process.argv[2];
if (!reportsDir) {
  console.error("usage: e2e-shim-coverage-summary.mjs <c8-reports-dir>");
  process.exit(2);
}

const summary = JSON.parse(
  readFileSync(path.join(reportsDir, "coverage-summary.json"), "utf8"),
);

let covered = 0;
let total = 0;
let files = 0;
for (const [file, entry] of Object.entries(summary)) {
  if (file === "total") continue;
  if (!file.includes("/agent-shim/") || file.includes("/node_modules/")) continue;
  files += 1;
  covered += entry.statements.covered;
  total += entry.statements.total;
}

if (total === 0) {
  console.error(
    "no agent-shim source survived the remap: the bundle's source map is the only thing that attributes the bundle back to src/**/*.ts",
  );
  process.exit(1);
}

const pct = ((covered / total) * 100).toFixed(1);
console.log(`${pct}% (${covered}/${total} statements over ${files} files)`);
