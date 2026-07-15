import { defineConfig } from "vite";

/**
 * Production build config. Tests read vitest.config.ts (which wins when both
 * files exist), so this file only shapes `vite build`.
 *
 * The heavy vendor libraries (xterm, highlight.js, markdown-it) are split into
 * their own chunks so no single chunk trips Vite's 500 kB size advisory and so
 * the browser can cache each independently of the app code.
 */
export default defineConfig({
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
