import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";

/**
 * Vitest stubs CSS imports out to an empty module by default, which would
 * silently empty the `?raw` stylesheet import that test/styles.test.ts asserts
 * against. Processing CSS keeps that import carrying the real source.
 *
 * Vite's own build reads vite.config.ts, so this file is test-only.
 */
export default defineConfig({
  resolve: { alias: protobufRuntimeAliases },
  test: {
    css: true,
    setupFiles: ["./test/setup.ts"],
    // The integration suite has its own config (vitest.integration.config.ts):
    // it boots the app against a real loopback Connect server, so it must not
    // ride along in the fast unit run. The webapp e2e layer
    // (vitest.webapp-layer.config.ts) is excluded for a stronger reason: it
    // needs the REAL daemon the Go e2e world spawns, and refuses to run
    // without it, so riding along here would fail every unit run.
    exclude: ["**/node_modules/**", "**/dist/**", "test/integration/**", "test/webapp-layer/**"],
    // TIGHT ON PURPOSE. Everything here is mocked/fake-timered — no real I/O,
    // no daemon — so a healthy run's slowest test is under 100ms (measured:
    // 88.9ms). Vitest's own defaults (5000ms/10000ms) would let a genuinely
    // hung test burn 50-100x that before failing. ~3x the observed max, so
    // real variance has headroom without masking a hang. If a test needs more,
    // it gets its own `{ timeout: ... }` with a one-line reason, not a raised
    // global.
    testTimeout: 300,
    hookTimeout: 300,
    coverage: {
      provider: "v8",
      all: true,
      include: ["src/**/*.ts"],
      exclude: ["src/**/*.d.ts", "src/**/generated/**"],
      reporter: ["text", "json", "json-summary", "html"],
      reportsDirectory: "coverage",
      // WHY: Establish the baseline before coverage-closing work enforces 90%.
    },
  },
});
