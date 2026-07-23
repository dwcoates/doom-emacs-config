import { defineConfig } from "vitest/config";
import { fileURLToPath } from "node:url";

// The generated protobuf TS stubs live at proto/gen/ts, outside this
// package. Vite's dev-server fs guard blocks files outside the project
// root by default; allow the agent-repl subtree so the relatively-imported
// stubs load. Runtime bare-import resolution (@bufbuild/protobuf) still
// comes from this package's node_modules, which vite anchors at the root.
const agentReplRoot = fileURLToPath(new URL("../../", import.meta.url));

export default defineConfig({
  server: { fs: { allow: [agentReplRoot] } },
});
