// registry-guard.mjs -- a build-time fix for protobuf-es's descriptor boot.
//
// THE DEFECT (@bufbuild/protobuf 2.13.0, unchanged in 2.16.0). Every generated
// `*_pb` file boots its descriptor at import through `fileDesc`, which builds
// a fresh registry and, in `createFileRegistry`'s `recurseDeps`, calls
// `registry.addFile(dep, false, true)` for every already-booted dependency.
// `addFile` with `withDeps` re-adds that dependency's types AND recurses into
// all of ITS dependencies, with no check for a file it already added, so each
// file re-walks the whole closure below it. MEASURED with `node --cpu-prof`:
// the shim's bundle spent ~55ms of a 142ms start in that walk; booting the
// webapp's 153 files took 0.20s of CPU against 0.13s with this guard.
//
// THE GUARD. `addFile(file, false, true)` is idempotent per registry: once a
// file's types and its whole dependency closure are in, adding it again adds
// nothing new. So the registry remembers the files it fully added and returns
// at once for one it has. A file first added WITHOUT its types or deps is not
// in that set, so a later full add still does the full work.
//
// APPLIED AT BUILD, NOT IN node_modules: the shim's esbuild bundle and the
// webapp's vite build rewrite `registry.js` as they load it. A library whose
// source no longer matches is a build FAILURE, never a silent no-op, so an
// upgrade cannot quietly drop the fix (or quietly keep a stale one).

/** The file the guard rewrites, in either of the package's module formats. */
export const REGISTRY_FILE = /[\\/]@bufbuild[\\/]protobuf[\\/]dist[\\/](esm|cjs)[\\/]registry\.js$/;

const ADD_FILE = /addFile\((\w+), skipTypes, withDeps\) \{\n(\s*)files\.set\(\1\.proto\.name, \1\);/;
const BASE_REGISTRY = /function createBaseRegistry\(\) \{\n/;

/**
 * Answers registry.js's source with the guard applied. Throws when the source
 * is not the shape the guard was written against.
 */
export function guardRegistrySource(source) {
  if (source.includes("fullyAdded")) {
    throw new Error("protobuf registry guard: registry.js already mentions fullyAdded; refusing to apply the guard twice or over an upstream change");
  }
  const addFile = source.match(ADD_FILE);
  if (addFile === null) {
    throw new Error("protobuf registry guard: registry.js has no `addFile(file, skipTypes, withDeps)` of the guarded shape; re-measure the boot and update or drop the guard");
  }
  if (!BASE_REGISTRY.test(source)) {
    throw new Error("protobuf registry guard: registry.js has no `function createBaseRegistry()`; re-measure the boot and update or drop the guard");
  }
  const [whole, file, indent] = addFile;
  const guarded =
    `addFile(${file}, skipTypes, withDeps) {\n` +
    `${indent}if (!skipTypes && withDeps) {\n` +
    `${indent}    if (fullyAdded.has(${file})) {\n` +
    `${indent}        return;\n` +
    `${indent}    }\n` +
    `${indent}    fullyAdded.add(${file});\n` +
    `${indent}}\n` +
    `${indent}files.set(${file}.proto.name, ${file});`;
  return source
    .replace(whole, guarded)
    .replace(BASE_REGISTRY, "function createBaseRegistry() {\n    const fullyAdded = new Set();\n");
}

/** The guard as an esbuild plugin (the shim's bundle). */
export function esbuildRegistryGuard() {
  return {
    name: "protobuf-registry-guard",
    setup(build) {
      build.onLoad({ filter: REGISTRY_FILE }, async (args) => {
        const { readFile } = await import("node:fs/promises");
        return { contents: guardRegistrySource(await readFile(args.path, "utf8")), loader: "js" };
      });
    },
  };
}

/** The guard as a vite plugin (the webapp's build). */
export function viteRegistryGuard() {
  return {
    name: "protobuf-registry-guard",
    enforce: "pre",
    transform(code, id) {
      if (!REGISTRY_FILE.test(id.split("?")[0])) return null;
      return { code: guardRegistrySource(code), map: null };
    },
  };
}
