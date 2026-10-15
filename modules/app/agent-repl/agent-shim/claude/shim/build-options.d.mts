// build-options.d.mts — the types of build-options.mjs, so a TypeScript
// caller (test/build-options.test.ts) reads the one bundle configuration
// typed rather than as an unresolved call.
import type { BuildOptions } from "esbuild";

/** The shared options for bundling `entry` into the single file `outfile`. */
export function bundleOptions(entry: string, outfile: string): BuildOptions;
