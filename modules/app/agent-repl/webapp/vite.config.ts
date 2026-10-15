import { defineConfig, type Plugin } from "vite";
import { appLayerChunks, type ModuleGraph } from "./app-layers";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";
import { viteCacheDir } from "./vite-cache";
import { viteRegistryGuard } from "../proto/ts/registry-guard.mjs";

/**
 * Production build config. Tests read vitest.config.ts (which wins when both
 * files exist), so this file only shapes `vite build`.
 *
 * The heavy vendor libraries (xterm, highlight.js, markdown-it) are split into
 * their own chunks so no single chunk trips Vite's 500 kB size advisory and so
 * the browser can cache each independently of the app code.
 *
 * THE WIRE CODE IS SPLIT TOO. The generated protobuf code under
 * proto/gen/ts/ (every message schema plus its embedded file descriptor) is
 * the single largest thing the app imports, and with the protobuf-es runtime
 * and the Connect client it put the entry chunk over the advisory on its own.
 * All three are needed at boot (the app opens its daemon streams as it
 * mounts), so lazy-loading them buys nothing; they get chunks of their own
 * instead, and the generated code's chunk changes only when a .proto does.
 * xterm stays a lazy import (src/login/terminal.ts), loaded only by the login
 * terminal. The limit itself is never raised: test/vite-config.test.ts builds
 * and fails on any warning or any chunk over the default.
 *
 * THE APP'S OWN CODE IS LAYERED (app-layers.ts): packed into `app-<n>` chunks
 * that each import only lower layers, so the app code grows into more chunks
 * rather than one past the advisory.
 */

/**
 * The layer budget, in bytes of pre-minification code: about half of what
 * minifies to the 500 kB advisory, so a layer is far under it.
 */
const APP_LAYER_BUDGET = 400_000;

/** Whether a module id is the webapp's own source. */
const isApp = (id: string): boolean => id.includes("/webapp/src/");

/**
 * The layer chunk of each app module, computed once per build from the whole
 * module graph, and dropped when the next build starts.
 */
let appChunks: Map<string, string> | null = null;
const appLayersPlugin: Plugin = {
  name: "agent-repl-app-layers",
  buildStart() {
    appChunks = null;
  },
};

export default defineConfig({
  // OUT OF `node_modules`, WHICH IS A READ-ONLY SYMLINK IN THE E2E SANDBOX
  // AND A GARBAGE-COLLECTED SHARED TREE ON THE HOST. Vite's default
  // `cacheDir` is `node_modules/.vite`; see vite-cache.ts for the whole
  // account, and test/vite-cache.test.ts for the check that keeps every
  // config in this package on it.
  cacheDir: viteCacheDir,
  resolve: { alias: protobufRuntimeAliases },
  // protobuf-es re-walks every booted dependency's closure for each file it
  // boots; the guard cuts the page's proto boot from ~0.20s to ~0.13s of CPU
  // (proto/ts/registry-guard.mjs).
  plugins: [viteRegistryGuard(), appLayersPlugin],
  build: {
    rollupOptions: {
      output: {
        manualChunks(id, graph) {
          if (id.includes("/proto/gen/ts/")) return "proto";
          if (isApp(id)) {
            appChunks ??= appLayerChunks(graph as ModuleGraph, isApp, APP_LAYER_BUDGET);
            return appChunks.get(id);
          }
          if (!id.includes("node_modules")) return undefined;
          if (id.includes("@bufbuild/protobuf")) return "protobuf";
          if (id.includes("@connectrpc")) return "connect";
          if (id.includes("@xterm")) return "xterm";
          if (id.includes("highlight.js")) return "highlight";
          if (id.includes("markdown-it")) return "markdown";
          return undefined;
        },
      },
    },
  },
});
