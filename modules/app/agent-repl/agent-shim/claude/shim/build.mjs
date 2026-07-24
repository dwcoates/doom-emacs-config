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
import { build } from "esbuild";
import { fileURLToPath } from "node:url";
import path from "node:path";

const dir = path.dirname(fileURLToPath(import.meta.url));

await build({
  entryPoints: [path.join(dir, "src/main.ts")],
  outfile: path.join(dir, "dist/main.js"),
  bundle: true,
  platform: "node",
  format: "esm",
  target: "node20",
  external: ["@anthropic-ai/claude-agent-sdk"],
  // Resolve bare imports (notably @bufbuild/protobuf, imported by the
  // out-of-package proto stubs) from THIS package's node_modules.
  nodePaths: [path.join(dir, "node_modules")],
  banner: {
    js: "// AUTO-GENERATED single-file bundle (esbuild); edit src/ and rebuild via `npm run build`.",
  },
  logLevel: "info",
});
