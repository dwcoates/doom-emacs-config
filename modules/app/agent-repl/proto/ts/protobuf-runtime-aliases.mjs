// protobuf-runtime-aliases.mjs -- the resolve aliases that pin every
// `@bufbuild/protobuf` import, the generated stubs' under ../gen/ts included,
// to ONE runtime copy: the importing package's own node_modules.
//
// ONE RUNTIME COPY. The generated code sits outside every package that loads
// it, so a bare `@bufbuild/protobuf` import from it resolves from ITS
// directory first: to proto/node_modules whenever proto's codegen has been
// installed there. protobuf-es compares descriptors by identity, so a second
// copy makes a generated message unusable through a Connect client ("different
// @bufbuild/protobuf instance"), and under a confined test run the second copy
// is a read outside the unit's inputs (the shim's integration chunks failed
// on proto/node_modules/@bufbuild/protobuf/dist/esm/codegenv2/index.js,
// 2026-11-20). Every subpath a package imports must be listed -- a missing one
// resolves through the normal algorithm and can land on the second copy --
// and `@bufbuild/protobuf` itself comes LAST so the longer subpaths match
// first.
import { fileURLToPath } from "node:url";

const SUBPATHS = [
  ["@bufbuild/protobuf/codegenv2", "dist/esm/codegenv2/index.js"],
  ["@bufbuild/protobuf/reflect", "dist/esm/reflect/index.js"],
  ["@bufbuild/protobuf/wkt", "dist/esm/wkt/index.js"],
  ["@bufbuild/protobuf/wire", "dist/esm/wire/index.js"],
  ["@bufbuild/protobuf", "dist/esm/index.js"],
];

/** The aliases for the package whose directory is PACKAGE_URL (a file: URL ending in "/"). */
export function protobufRuntimeAliases(packageUrl) {
  if (!String(packageUrl).endsWith("/")) {
    throw new Error(`protobuf-runtime-aliases: ${packageUrl} is no package directory URL`);
  }
  return SUBPATHS.map(([find, file]) => ({
    find,
    replacement: fileURLToPath(new URL(`node_modules/@bufbuild/protobuf/${file}`, packageUrl)),
  }));
}
