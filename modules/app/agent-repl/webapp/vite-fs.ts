import { fileURLToPath } from "node:url";

/**
 * WHAT THE VITE SERVER MAY SERVE (`server.fs`), one definition imported by
 * every config in this package (the build config and every test project).
 *
 * The webapp imports from beyond its own directory: the generated protobuf
 * stubs (`../proto/gen/ts`) and the shared images (`../images`, as `?raw` and
 * `?inline`). Vite refuses a file outside `server.fs.allow`, and from vite 6 on
 * it refuses a query-suffixed import (`?raw`, `?inline`) outside it too, so the
 * agent-repl module root is allowed, as the shim's config allows it.
 * test/vite-cache.test.ts holds every config to this.
 */
export const viteServer = { fs: { allow: [fileURLToPath(new URL("..", import.meta.url))] } };
