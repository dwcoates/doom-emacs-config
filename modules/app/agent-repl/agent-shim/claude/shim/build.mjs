// build.mjs — bundle the shim into a single self-contained file with esbuild.
//
// WHY A BUNDLE (the runtime-resolution problem this solves):
//   The committed protobuf TS stubs live OUTSIDE this package, at
//   proto/gen/ts. They `import { ... } from "@bufbuild/protobuf"`, but Node's
//   runtime resolver walks up from the stub's OWN directory (proto/gen/ts),
//   where no node_modules exists, so a plain `tsc` emit cannot run: the stub
//   cannot find @bufbuild/protobuf at runtime. (tsc also emits under a deep
//   rootDir-mirrored path — dist/agent-shim/claude/shim/src/main.js — not the
//   dist/main.js the daemon spawns.) esbuild fixes both: it INLINES
//   @bufbuild/protobuf (resolved via nodePaths from THIS package's
//   node_modules) and emits ONE file at dist/main.js — exactly the entry the
//   daemon (daemon.el) and the e2e harness already spawn, so neither needs a
//   path change.
//
//   The Claude Agent SDK is kept EXTERNAL: it is heavy, drives a spawned
//   `claude` child, and is only ever dynamically imported at runtime, where it
//   resolves from this package's node_modules.
//
//   @connectrpc/connect and @connectrpc/connect-node are the OPPOSITE case and
//   are BUNDLED (they are absent from `external` on purpose): they are plain
//   static dependencies of the transport the shim serves and dials, they are
//   small, and the daemon spawns `dist/main.js` by path — a bundle that left
//   them external would resolve them only while this package's node_modules
//   sits beside the output, which is exactly the runtime-resolution problem
//   the bundle exists to remove.
import { build } from "esbuild";
import { fileURLToPath } from "node:url";
import path from "node:path";

const dir = path.dirname(fileURLToPath(import.meta.url));

// SHIM_BUILD_OUTFILE redirects the bundle somewhere other than dist/main.js.
// The daemon e2e harness sets it to build a FRESH bundle into its own temp dir
// on every run, so the suite can never be exercising a stale (or absent —
// dist/ is gitignored) checked-out bundle. Unset, the output is unchanged.
const outfile = process.env.SHIM_BUILD_OUTFILE || path.join(dir, "dist/main.js");

// SHIM_BUILD_SOURCEMAP=1 emits an external source map beside the bundle. It
// exists for ONE caller: the e2e suite's coverage build, where v8 reports
// coverage against the bundle and only the map attributes those ranges back
// to src/**/*.ts. It is OFF by default and nothing in the deploy path sets
// it, so the production bundle's bytes — and with them the build identity
// bin/build-frontend.sh stamps — are exactly what they were.
const sourcemap = process.env.SHIM_BUILD_SOURCEMAP === "1";

await build({
  entryPoints: [path.join(dir, "src/main.ts")],
  outfile,
  bundle: true,
  platform: "node",
  format: "esm",
  target: "node20",
  sourcemap,
  external: ["@anthropic-ai/claude-agent-sdk"],
  // Resolve bare imports (notably @bufbuild/protobuf, imported by the
  // out-of-package proto stubs) from THIS package's node_modules.
  nodePaths: [path.join(dir, "node_modules")],
  // THE BUILD IDENTITY IS NOT BAKED. It is read from the SPAWN ENV at
  // runtime (`process.env.SHIM_BUILD_SHA`, src/build-identity.ts), because a
  // shim outlives its daemon and the daemon must be able to tell what the
  // process it started actually IS. An esbuild `define` here would substitute
  // the expression at bundle time, so the value the daemon exported when it
  // spawned the process would be silently ignored and every shim would report
  // the sha of whichever build happened to be bundled.
  //
  // bin/build-frontend.sh still computes the content hash (lowercase hex
  // SHA-256) of this bundle's bytes once and writes it to dist/.built-sha; the
  // daemon exports that same hash into the shim's environment, so bundle
  // stamp and reported identity agree by construction, and the daemon's
  // deploy compares it against a freshly built bundle's own hash to decide
  // whether a running shim is stale. src/main.ts REFUSES TO START without it,
  // so an unset value is a loud startup failure rather than a fabricated
  // identity.
  banner: {
    js: "// AUTO-GENERATED single-file bundle (esbuild); edit src/ and rebuild via `npm run build`.",
  },
  logLevel: "info",
});
