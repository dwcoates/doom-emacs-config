import { protobufRuntimeAliases as aliasesFor } from "../../../proto/ts/protobuf-runtime-aliases.mjs";

/**
 * This package's runtime aliases: every `@bufbuild/protobuf` import, the
 * generated stubs' above this package included, pinned to this package's own
 * copy (proto/ts/protobuf-runtime-aliases.mjs has why).
 */
export const protobufRuntimeAliases = aliasesFor(new URL(".", import.meta.url));
