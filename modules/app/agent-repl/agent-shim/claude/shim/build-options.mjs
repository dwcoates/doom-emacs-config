// build-options.mjs — the one esbuild configuration every Node bundle of this
// package is built with: the shim itself (build.mjs) and the suite's fake
// store run as a process (test/fakes/build-store-server.mjs).
//
// WHY A BUNDLE: the committed protobuf TS stubs live OUTSIDE this package, at
// proto/gen/ts, and import "@bufbuild/protobuf", which Node's runtime resolver
// would look for beside the stub, where no node_modules exists. esbuild
// INLINES it, resolving bare imports from THIS package's node_modules
// (nodePaths), and emits one file. The full account is in build.mjs.
import { fileURLToPath } from "node:url";
import path from "node:path";
import { esbuildRegistryGuard } from "../../../proto/ts/registry-guard.mjs";

const dir = path.dirname(fileURLToPath(import.meta.url));

/** The shared options for bundling `entry` into the single file `outfile`. */
export function bundleOptions(entry, outfile) {
  return {
    entryPoints: [entry],
    outfile,
    bundle: true,
    platform: "node",
    format: "esm",
    target: "node20",
    // Resolve bare imports (notably @bufbuild/protobuf, imported by the
    // out-of-package proto stubs) from THIS package's node_modules.
    nodePaths: [path.join(dir, "node_modules")],
    // protobuf-es re-walks every booted dependency's closure for each file it
    // boots; the guard cuts ~55ms from every start (proto/ts/registry-guard.mjs).
    plugins: [esbuildRegistryGuard()],
  };
}
