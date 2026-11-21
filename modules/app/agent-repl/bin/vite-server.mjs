// vite-server.mjs -- what the vite server of every vitest and vite config in
// this module may serve (`server.fs`): the agent-repl module root, this
// file's parent directory.
//
// The packages import from beyond their own directories: the generated
// protobuf stubs (proto/gen/ts) and the shared images (images/, as `?raw` and
// `?inline`). Vite refuses a file outside `server.fs.allow`, and from vite 6 on
// it refuses a query-suffixed import (`?raw`, `?inline`) outside it too.
import { fileURLToPath } from "node:url";

export const agentReplViteServer = { fs: { allow: [fileURLToPath(new URL("..", import.meta.url))] } };
