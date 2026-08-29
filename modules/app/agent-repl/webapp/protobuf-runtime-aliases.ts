import { fileURLToPath } from "node:url";

/**
 * Runtime aliases for generated stubs whose source sits above this package.
 *
 * ONE RUNTIME COPY. The generated code under `../proto/gen/ts` and the
 * `@connectrpc/*` packages both import `@bufbuild/protobuf`; protobuf-es
 * compares descriptors by identity, so two resolved copies would make a
 * generated message unusable through a Connect client ("different
 * @bufbuild/protobuf instance"). Pinning every subpath to this package's own
 * `node_modules` copy is what keeps that single.
 *
 * Every subpath the app imports must be listed — a missing one resolves
 * through the normal algorithm and can land on a second copy. `@bufbuild/protobuf`
 * itself comes LAST so the longer subpath prefixes match first.
 */
export const protobufRuntimeAliases = [
  ["@bufbuild/protobuf/codegenv2", "./node_modules/@bufbuild/protobuf/dist/esm/codegenv2/index.js"],
  ["@bufbuild/protobuf/reflect", "./node_modules/@bufbuild/protobuf/dist/esm/reflect/index.js"],
  ["@bufbuild/protobuf/wkt", "./node_modules/@bufbuild/protobuf/dist/esm/wkt/index.js"],
  ["@bufbuild/protobuf/wire", "./node_modules/@bufbuild/protobuf/dist/esm/wire/index.js"],
  ["@bufbuild/protobuf", "./node_modules/@bufbuild/protobuf/dist/esm/index.js"],
].map(([find, relative]) => ({ find, replacement: fileURLToPath(new URL(relative, import.meta.url)) }));
