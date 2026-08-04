import { fileURLToPath } from "node:url";

/** Runtime aliases for generated stubs whose source sits above this package. */
export const protobufRuntimeAliases = [
  ["@bufbuild/protobuf/codegenv2", "./node_modules/@bufbuild/protobuf/dist/esm/codegenv2/index.js"],
  ["@bufbuild/protobuf/wkt", "./node_modules/@bufbuild/protobuf/dist/esm/wkt/index.js"],
  ["@bufbuild/protobuf", "./node_modules/@bufbuild/protobuf/dist/esm/index.js"],
].map(([find, relative]) => ({ find, replacement: fileURLToPath(new URL(relative, import.meta.url)) }));
