// Refuses a run that did not come through bin/background.sh: tests only ever
// run at background priority (see that script and require-background.mjs).
import "../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";
import { viteCacheDir } from "./vite-cache";

/**
 * The REAL-WEBKIT suite's config (`npm run test:webkit`), separate from the
 * unit one.
 *
 * These tests launch Playwright's headless WebKit -- the engine the Emacs
 * webview runs -- over the real stylesheet and real modules, for behavior
 * jsdom cannot lay out: scroll anchoring under `content-visibility: auto`
 * (test/webkit/anchoring.webkit.test.ts). They run in node, drive the browser
 * themselves, and take seconds rather than milliseconds, so they stay out of
 * `npm test`.
 */
export default defineConfig({
  cacheDir: viteCacheDir,
  resolve: { alias: protobufRuntimeAliases },
  test: {
    environment: "node",
    include: ["test/webkit/**/*.webkit.test.ts"],
    // One file, one browser: nothing is gained by a second worker.
    maxWorkers: 1,
    minWorkers: 1,
    // MEASURED, ~3x the healthy max (webapp/AGENTS.md "Wait/timeout bounds").
    // The whole browser run -- bundle, launch, the 120-step pass -- is the
    // `beforeAll`, observed at 29.4s at most; the tests only read its result.
    hookTimeout: 90_000,
    testTimeout: 850,
  },
});
