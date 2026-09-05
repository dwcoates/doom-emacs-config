import { defineConfig } from "vite";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";
import { viteCacheDir } from "./vite-cache";

/**
 * Production build config. Tests read vitest.config.ts (which wins when both
 * files exist), so this file only shapes `vite build`.
 *
 * The heavy vendor libraries (xterm, highlight.js, markdown-it) are split into
 * their own chunks so no single chunk trips Vite's 500 kB size advisory and so
 * the browser can cache each independently of the app code.
 */
export default defineConfig({
  // OUT OF `node_modules`, WHICH IS A READ-ONLY SYMLINK IN THE E2E SANDBOX
  // AND A GARBAGE-COLLECTED SHARED TREE ON THE HOST. Vite's default
  // `cacheDir` is `node_modules/.vite`; see vite-cache.ts for the whole
  // account, and test/vite-cache.test.ts for the check that keeps every
  // config in this package on it.
  cacheDir: viteCacheDir,
  resolve: { alias: protobufRuntimeAliases },
  build: {
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (!id.includes("node_modules")) return undefined;
          if (id.includes("@xterm")) return "xterm";
          if (id.includes("highlight.js")) return "highlight";
          if (id.includes("markdown-it")) return "markdown";
          return undefined;
        },
      },
    },
  },
});
