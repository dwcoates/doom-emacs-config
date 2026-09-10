/**
 * scripts/run-smoke.mjs — bundle and run the dist smoke.
 *
 * WHY A WRAPPER RATHER THAN THE esbuild CLI. The committed protobuf stubs live
 * outside this package (proto/gen/ts) and import `@bufbuild/protobuf` by bare
 * specifier; esbuild resolves those by walking up from the STUB's directory,
 * where no node_modules exists. `nodePaths` fixes it and is only available
 * through the JS API — the same reason `build.mjs` exists and the same option it
 * passes.
 */
import { build } from "esbuild";
import path from "node:path";
import { pathToFileURL, fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const pkg = path.join(here, "..");
const outfile = path.join(pkg, "dist", "dist-smoke.mjs");

await build({
  entryPoints: [path.join(here, "dist-smoke.ts")],
  outfile,
  bundle: true,
  platform: "node",
  format: "esm",
  target: "node20",
  nodePaths: [path.join(pkg, "node_modules")],
  logLevel: "warning",
});

await import(pathToFileURL(outfile).href);
